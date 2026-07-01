###
# 05 - Mini-LLM: Character-Level Transformer on ECB Speeches
# Inspired by Karpathy's nanochat -- understand LLMs by building one
# 260226
###

if (!require("pacman")) install.packages("pacman"); library(pacman)
p_load(data.table)
p_load(magrittr)
p_load(torch)

# 0 - settings ----

dir.create("temp", showWarnings = FALSE, recursive = TRUE)
set.seed(42)

# 1 - prepare the training corpus ----

# We train a character-level language model: it reads one character at a time
# and learns to predict the next one. No tokenizer, no vocabulary tricks --
# just raw characters. This is the simplest possible setup that still produces
# a working transformer.

ecb_speeches <- fread("temp/ecb_speeches_cleaned.csv")

# Subsample: a tiny model cannot learn patterns from 50M+ characters.
# 300 speeches (~2M chars) gives the model enough repetition to pick up
# common words and phrases within a reasonable training time.
ecb_sample <- ecb_speeches[sample(.N, min(300L, .N))]

corpus <- paste(ecb_sample$contents, collapse = "\n\n")
cat("Corpus length:", format(nchar(corpus), big.mark = ","), "characters\n")

# Build character vocabulary: every unique character gets an integer index.
# R torch embeddings are 1-indexed, so indices run from 1 to vocab_size.
chars <- sort(unique(strsplit(corpus, "")[[1]]))
vocab_size <- length(chars)
cat("Vocabulary size:", vocab_size, "unique characters\n")

char_to_idx <- setNames(seq_along(chars), chars)
idx_to_char <- setNames(chars, seq_along(chars))

encode <- function(s) unname(char_to_idx[strsplit(s, "")[[1]]])
decode <- function(idx) paste(idx_to_char[as.character(idx)], collapse = "")

# Encode the entire corpus into a single long integer tensor.
# This is what the model trains on: one giant sequence of character indices.
data <- torch_tensor(encode(corpus), dtype = torch_long())
cat("Encoded tensor length:", format(length(data), big.mark = ","), "\n")

# Train/val split (90/10) -- we monitor validation loss to detect overfitting
n <- length(data)
train_data <- data[1:floor(n * 0.9)]
val_data   <- data[(floor(n * 0.9) + 1):n]

# 2 - hyperparameters ----

# These control the model size and training duration.
# The model is deliberately small so it trains in minutes on a laptop CPU.

block_size  <- 256L    # context window: how many characters the model sees at once
batch_size  <- 32L     # how many independent sequences per gradient step
n_embd      <- 128L    # dimensionality of the internal representations
n_head      <- 4L      # number of parallel attention heads (each gets n_embd/n_head dims)
n_layer     <- 4L      # number of stacked transformer blocks
dropout     <- 0.1     # fraction of activations randomly zeroed during training (regularisation)
max_iters   <- 3000L   # total gradient update steps
eval_every  <- 500L    # how often to estimate train/val loss
lr          <- 1e-3    # Adam learning rate
device      <- if (cuda_is_available()) "cuda" else "cpu"

cat("Device:", device, "\n")

# 3 - data loader ----

# Each training step, we sample random chunks of `block_size` characters.
# x = input characters, y = the same sequence shifted one position right.
# The model's job: given x[1..t], predict y[t] = x[t+1] for every position t.

get_batch <- function(split) {
  d  <- if (split == "train") train_data else val_data
  ix <- sample.int(length(d) - block_size, batch_size)
  x  <- torch_stack(lapply(ix, function(i) d[i:(i + block_size - 1L)]))
  y  <- torch_stack(lapply(ix, function(i) d[(i + 1L):(i + block_size)]))
  list(x = x$to(device = device), y = y$to(device = device))
}

# 4 - model architecture ----

# The architecture below is identical to GPT -- just much smaller.
# GPT-3 has 96 layers, 96 heads, 12288 embedding dims, and 175B parameters.
# Ours has 4 layers, 4 heads, 128 embedding dims, and ~900K parameters.

# -- Single attention head --
# Each head learns to focus on different aspects of the context.
# It computes query/key/value projections, then uses dot-product attention
# with a causal mask so position t can only attend to positions <= t.

attention_head <- nn_module(
  initialize = function(head_size) {
    self$key   <- nn_linear(n_embd, head_size, bias = FALSE)
    self$query <- nn_linear(n_embd, head_size, bias = FALSE)
    self$value <- nn_linear(n_embd, head_size, bias = FALSE)
    self$dropout <- nn_dropout(dropout)
    # lower-triangular mask: prevents attending to future characters
    self$register_buffer("tril",
      torch_tril(torch_ones(block_size, block_size)))
  },
  forward = function(x) {
    B <- x$shape[1]; TT <- x$shape[2]; C <- x$shape[3]
    k <- self$key(x)
    q <- self$query(x)
    # scaled dot-product attention: similarity scores between all position pairs
    w <- torch_matmul(q, k$transpose(-2, -1)) * C^(-0.5)
    # mask out future positions (causal / autoregressive constraint)
    w <- w$masked_fill(self$tril[1:TT, 1:TT] == 0, -Inf)
    w <- nnf_softmax(w, dim = -1)
    w <- self$dropout(w)
    v <- self$value(x)
    torch_matmul(w, v)
  }
)

# -- Multi-head attention --
# Run several attention heads in parallel, concatenate their outputs,
# then project back to n_embd dimensions. Different heads can specialise
# on different patterns (e.g. one for spacing, one for common bigrams).

multi_head_attention <- nn_module(
  initialize = function(n_head, head_size) {
    self$heads <- nn_module_list(
      lapply(1:n_head, function(i) attention_head(head_size))
    )
    self$proj <- nn_linear(n_embd, n_embd)
    self$dropout <- nn_dropout(dropout)
  },
  forward = function(x) {
    out <- torch_cat(lapply(self$heads, function(h) h(x)), dim = -1)
    self$dropout(self$proj(out))
  }
)

# -- Feed-forward block --
# A simple two-layer MLP applied independently to each position.
# The hidden layer is 4x wider than n_embd (standard GPT convention).
# This is where the model stores "knowledge" learned from the data.

feed_forward <- nn_module(
  initialize = function() {
    self$net <- nn_sequential(
      nn_linear(n_embd, 4L * n_embd),
      nn_relu(),
      nn_linear(4L * n_embd, n_embd),
      nn_dropout(dropout)
    )
  },
  forward = function(x) self$net(x)
)

# -- Transformer block --
# One block = multi-head attention + feed-forward, each wrapped with
# layer normalisation and a residual (skip) connection.
# LayerNorm stabilises training; residual connections let gradients
# flow through deep networks without vanishing.

transformer_block <- nn_module(
  initialize = function() {
    head_size <- as.integer(n_embd / n_head)
    self$sa  <- multi_head_attention(n_head, head_size)
    self$ffn <- feed_forward()
    self$ln1 <- nn_layer_norm(n_embd)
    self$ln2 <- nn_layer_norm(n_embd)
  },
  forward = function(x) {
    x <- x + self$sa(self$ln1(x))   # attend, then add back (residual)
    x <- x + self$ffn(self$ln2(x))  # transform, then add back
    x
  }
)

# -- Full model --
# Token embedding: maps each character index to a learned 128-dim vector.
# Position embedding: tells the model WHERE each character sits in the window.
# After stacking transformer blocks, a final linear layer projects back
# to vocab_size logits = unnormalised probabilities over the next character.

mini_gpt <- nn_module(
  initialize = function() {
    self$token_emb <- nn_embedding(vocab_size, n_embd)
    self$pos_emb   <- nn_embedding(block_size, n_embd)
    self$blocks    <- nn_sequential(
      !!!lapply(1:n_layer, function(i) transformer_block())
    )
    self$ln_f    <- nn_layer_norm(n_embd)
    self$lm_head <- nn_linear(n_embd, vocab_size)
  },
  forward = function(idx, targets = NULL) {
    B <- idx$shape[1]; TT <- idx$shape[2]
    tok_emb <- self$token_emb(idx)
    pos_emb <- self$pos_emb(torch_arange(1, TT, device = idx$device, dtype = torch_long()))
    x <- tok_emb + pos_emb
    x <- self$blocks(x)
    x <- self$ln_f(x)
    logits <- self$lm_head(x)

    if (is.null(targets)) {
      return(list(logits = logits, loss = NULL))
    }
    # cross-entropy loss: how wrong were the predictions?
    B <- logits$shape[1]; TT <- logits$shape[2]; C <- logits$shape[3]
    logits_flat  <- logits$view(c(B * TT, C))
    targets_flat <- targets$view(c(B * TT))
    loss <- nnf_cross_entropy(logits_flat, targets_flat)
    list(logits = logits, loss = loss)
  },
  generate = function(idx, max_new_tokens, temperature = 1.0) {
    # Autoregressive generation: predict one character, append it, repeat.
    # Temperature < 1 makes the distribution peakier (more conservative);
    # temperature > 1 makes it flatter (more creative / more random).
    for (i in seq_len(max_new_tokens)) {
      # crop context to the last block_size characters (model's maximum window)
      idx_cond <- idx[, max(1, idx$shape[2] - block_size + 1):idx$shape[2]]
      out <- self$forward(idx_cond)
      logits <- out$logits[, idx_cond$shape[2], ]  # logits at the last position
      logits <- logits / temperature
      probs <- nnf_softmax(logits, dim = -1)
      idx_next <- torch_multinomial(probs$squeeze(1), num_samples = 1)
      idx <- torch_cat(list(idx, idx_next$unsqueeze(1)), dim = 2)
    }
    idx
  }
)

# 5 - training loop ----

model <- mini_gpt()
model$to(device = device)
n_params <- sum(sapply(model$parameters, function(p) p$numel()))
cat("Model parameters:", format(n_params, big.mark = ","), "\n")

optimizer <- optim_adam(model$parameters, lr = lr)

# Estimate loss on several random batches (avoids noisy single-batch estimates)
estimate_loss <- function(eval_iters = 50L) {
  model$eval()
  losses <- list(train = c(), val = c())
  for (split in c("train", "val")) {
    batch_losses <- numeric(eval_iters)
    for (k in seq_len(eval_iters)) {
      batch <- get_batch(split)
      out <- model(batch$x, batch$y)
      batch_losses[k] <- out$loss$item()
    }
    losses[[split]] <- mean(batch_losses)
  }
  model$train()
  losses
}

cat("\nTraining...\n\n")

for (iter in seq_len(max_iters)) {
  if (iter %% eval_every == 1 || iter == max_iters) {
    losses <- estimate_loss()
    cat(sprintf("Step %4d | train loss: %.4f | val loss: %.4f\n",
                iter, losses$train, losses$val))
  }

  batch <- get_batch("train")
  out <- model(batch$x, batch$y)

  optimizer$zero_grad()
  out$loss$backward()
  optimizer$step()
}

# 6 - generate text ----

cat("\n── Generated ECB-style text ──────────────────────────────────────\n\n")

model$eval()
seed <- torch_tensor(matrix(encode("The inflation"), nrow = 1), dtype = torch_long())
seed <- seed$to(device = device)

with_no_grad({
  generated <- model$generate(seed, max_new_tokens = 500L, temperature = 0.8)
})

cat(decode(as.integer(generated$cpu())), "\n")
cat("\n─────────────────────────────────────────────────────────────────\n")

# cleanup
rm(model, optimizer, data, train_data, val_data, ecb_speeches, ecb_sample)
gc()
