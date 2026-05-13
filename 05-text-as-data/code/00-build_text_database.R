###
# 00 - Build Text Database from TOTA XML Files
# Data Science for Economists - afternoon application
# March 2026
###

# Common file for everyone: run/source this file to generate the same clean text database.
# The XML parsing is deliberately hidden inside functions so that the lab can
# focus on text analysis rather than file ingestion.
#
# Output:
#   data/tota_texts.rds
#   data/tota_texts.csv
#
# Based on trade-agreement text data from the TOTA project:
# https://github.com/mappingtreaties/tota

if (!require("pacman")) install.packages("pacman")
library(pacman)
p_load(tidyverse)
p_load(xml2)

# -----------------------------------------------------------------------------
# Helper: download TOTA if it is not already present
# -----------------------------------------------------------------------------

download_tota_if_needed <- function(input_dir = "input", force_download = FALSE) {
  dir.create(input_dir, showWarnings = FALSE, recursive = TRUE)

  tota_dir <- file.path(input_dir, "tota-master")
  zip_path <- file.path(input_dir, "tota.zip")

  if (!dir.exists(tota_dir) || force_download) {
    message("Downloading TOTA data from GitHub...")
    url <- "https://github.com/mappingtreaties/tota/archive/refs/heads/master.zip"
    download.file(url, zip_path, mode = "wb")
    unzip(zip_path, exdir = input_dir)
  }

  invisible(tota_dir)
}

# -----------------------------------------------------------------------------
# Helper: read one XML file and return one row
# -----------------------------------------------------------------------------

read_one_tota_xml <- function(path) {
  tryCatch({
    treaty_xml <- read_xml(path)

    get_meta <- function(field) {
      # local-name() makes the function robust to XML namespaces.
      node <- xml_find_first(treaty_xml, sprintf("//*[local-name()='%s']", field))

      if (!inherits(node, "xml_missing")) {
        out <- xml_text(node)
      } else {
        out <- NA_character_
      }

      if (length(out) == 0 || is.na(out[1]) || out[1] == "") {
        NA_character_
      } else {
        str_squish(as.character(out[1]))
      }
    }

    get_parties <- function() {
      # In the TOTA XML, parties_original often has one child node per party.
      # xml_text() on the parent would paste them together as "ZAFZWE".
      # We therefore read the child/leaf nodes and collapse them with a separator.
      party_nodes <- xml_find_all(
        treaty_xml,
        "//*[local-name()='parties_original']//*[not(*)]"
      )

      party_values <- party_nodes |>
        xml_text() |>
        str_squish()

      party_values <- party_values[!is.na(party_values) & party_values != ""] |>
        unique()

      if (length(party_values) > 0) {
        paste(party_values, collapse = "; ")
      } else {
        # Fallback for files where parties_original is stored as plain text.
        get_meta("parties_original")
      }
    }

    # Metadata fields differ slightly across files. These are useful when present.
    date_signed <- get_meta("date_signed")
    date_entry_into_force <- get_meta("date_entry_into_force")
    parties <- get_parties()
    title <- get_meta("name")

    # Main treaty text. In TOTA, substantive content is stored in article nodes.
    article_nodes <- xml_find_all(treaty_xml, "//*[local-name()='article']")

    article_text <- article_nodes |>
      xml_text() |>
      str_squish()

    article_text <- article_text[nchar(article_text) > 0]
    full_text <- paste(article_text, collapse = " ") |>
      str_squish()

    tibble(
      agreement_id = tools::file_path_sans_ext(basename(path)),
      source_file = basename(path),
      title = title,
      parties = parties,
      date_signed = date_signed,
      date_entry_into_force = date_entry_into_force,
      year = suppressWarnings(as.integer(str_sub(date_signed, 1, 4))),
      n_articles = length(article_text),
      text = full_text,
      n_chars = nchar(full_text),
      n_words = str_count(full_text, "\\S+")
    )
  }, error = function(e) {
    warning("Could not read ", basename(path), ": ", conditionMessage(e))
    tibble(
      agreement_id = tools::file_path_sans_ext(basename(path)),
      source_file = basename(path),
      title = NA_character_,
      parties = NA_character_,
      date_signed = NA_character_,
      date_entry_into_force = NA_character_,
      year = NA_integer_,
      n_articles = NA_integer_,
      text = NA_character_,
      n_chars = NA_integer_,
      n_words = NA_integer_
    )
  })
}

# -----------------------------------------------------------------------------
# Main function students call
# -----------------------------------------------------------------------------

build_tota_text_database <- function(input_dir = "input",
                                     output_dir = "data",
                                     force_rebuild = FALSE,
                                     force_download = FALSE,
                                     save_csv = TRUE) {

  dir.create(output_dir, showWarnings = FALSE, recursive = TRUE)

  rds_path <- file.path(output_dir, "tota_texts.rds")
  csv_path <- file.path(output_dir, "tota_texts.csv")

  if (file.exists(rds_path) && !force_rebuild) {
    message("Loading existing clean text database: ", rds_path)
    return(readRDS(rds_path))
  }

  tota_dir <- download_tota_if_needed(input_dir = input_dir,
                                      force_download = force_download)

  xml_dir <- file.path(tota_dir, "xml")
  xml_files <- list.files(xml_dir, pattern = "\\.xml$", full.names = TRUE)
  xml_files <- sort(xml_files)

  if (length(xml_files) == 0) {
    stop("No XML files found in ", xml_dir)
  }

  message("Building clean text database from ", length(xml_files), " XML files...")

  tota_texts <- map_dfr(xml_files, read_one_tota_xml) |>
    filter(!is.na(text), nchar(text) > 100) |>
    arrange(year, parties, source_file)

  saveRDS(tota_texts, rds_path)

  if (save_csv) {
    write_csv(tota_texts, csv_path)
  }

  message("Saved clean text database to:")
  message("  - ", rds_path)
  if (save_csv) message("  - ", csv_path)
  message("Rows: ", nrow(tota_texts), " agreements")

  return(tota_texts)
}

# -----------------------------------------------------------------------------
# Run this file directly to build the database
# -----------------------------------------------------------------------------

# If you click "Source" on this file, the database is built automatically.
# If another script sources this file, the function can be called from there too.

if (sys.nframe() == 0) {
  tota_texts <- build_tota_text_database(force_rebuild = TRUE)
  print(glimpse(tota_texts))
}
