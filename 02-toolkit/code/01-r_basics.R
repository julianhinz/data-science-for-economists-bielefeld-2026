###
# 01 - R Basics
# 260226
###

if (!require("pacman")) install.packages("pacman"); library(pacman)
p_load(magrittr)

# 1 - arithmetic ----

1 + 2
6 * 7
2^10
100 / 3
100 %/% 3  # integer division
100 %% 3   # modulo (remainder)

sqrt(144)
log(exp(1))
abs(-42)

# 2 - types and coercion ----

class(42)         # "numeric"
class(42L)        # "integer"
class("hello")    # "character"
class(TRUE)       # "logical"
class(1 + 2i)     # "complex"

# coercion hierarchy: logical < integer < double < character
as.numeric(TRUE)   # 1
as.character(42)   # "42"
as.integer(3.7)    # 3

# careful with floating-point comparison
0.1 + 0.2 == 0.3        # FALSE!
all.equal(0.1 + 0.2, 0.3)  # TRUE

# 3 - logic and evaluation ----

TRUE & FALSE   # AND
TRUE | FALSE   # OR
!TRUE          # NOT

# comparison
5 > 3
5 == 5
5 != 4

# matching
"a" %in% c("a", "b", "c")
"z" %in% c("a", "b", "c")

# 4 - assignment ----

x = 10       # preferred in this course
y <- 20      # also fine, more traditional
x + y

# 5 - vectors ----

# create
v = c(1, 2, 3, 4, 5)
v = 1:5           # shorthand
v = seq(0, 1, by = 0.2)
v = rep(0, times = 10)

# inspect
length(v)
head(v, 3)
tail(v, 2)

# vectorised operations
v = 1:5
v * 2
v + v
sum(v)
mean(v)
cumsum(v)

# indexing
v[1]           # first element (R is 1-indexed!)
v[c(2, 4)]    # second and fourth
v[-1]          # everything except the first
v[v > 3]       # logical subsetting

# named vectors
named_v = c(a = 1, b = 2, c = 3)
named_v["b"]

# 6 - matrices ----

m = matrix(1:6, nrow = 2, ncol = 3)
m

# indexing: [row, col]
m[1, 2]      # row 1, col 2
m[1, ]       # entire row 1
m[, 3]       # entire col 3

# operations
t(m)            # transpose
m %*% t(m)      # matrix multiplication
dim(m)
nrow(m)
ncol(m)

# 7 - data frames ----

df = data.frame(
  country = c("DEU", "FRA", "USA", "CHN", "BRA"),
  gdp     = c(4.2, 2.9, 25.5, 17.7, 2.1),
  pop     = c(83, 67, 331, 1412, 214)
)
df

# inspect
str(df)
summary(df)
nrow(df)
names(df)

# indexing
df[1, ]        # first row
df[, "gdp"]    # column by name
df$country     # column with $
df[df$gdp > 3, ]  # filter rows

# 8 - lists ----

my_list = list(
  name    = "Germany",
  code    = "DEU",
  values  = c(1.1, 2.2, 3.3),
  nested  = data.frame(x = 1:3, y = 4:6)
)

# access
my_list$name        # by name
my_list[["code"]]   # by name (alternative)
my_list[[3]]        # by position
my_list[[3]][2]     # second element of third component

# 9 - functions ----

# define a simple function
gdp_per_capita = function(gdp_trillions, pop_millions) {
  gdp_pc = (gdp_trillions * 1e12) / (pop_millions * 1e6)
  return(gdp_pc)
}

gdp_per_capita(4.2, 83)

# functions with default arguments
greet = function(name, greeting = "Hello") {
  paste(greeting, name)
}
greet("world")
greet("world", greeting = "Hola")

# anonymous functions (lambda)
sapply(1:5, function(x) x^2)
sapply(1:5, \(x) x^2)  # shorthand (R 4.1+)

# 10 - control flow ----

# if / else
value = 42
if (value > 0) {
  cat("positive\n")
} else if (value == 0) {
  cat("zero\n")
} else {
  cat("negative\n")
}

# for loop
for (i in 1:5) {
  cat("Iteration:", i, "\n")
}

# while loop
counter = 1
while (counter <= 3) {
  cat("Counter:", counter, "\n")
  counter = counter + 1
}

# apply family (vectorised alternative to loops)
sapply(1:5, sqrt)
lapply(1:3, function(i) rep(i, i))

# 11 - pipes ----

# pipes pass the left-hand side as the first argument of the right-hand side
# magrittr pipe %>% -- requires library(magrittr) or library(tidyverse)
# native pipe |>     -- built-in since R 4.1+; you'll see both in the wild

library(magrittr)

# without pipe
head(subset(df, gdp > 3), 2)

# with pipe
df %>%
  subset(gdp > 3) %>%
  head(2)

# pipes shine with multi-step transformations
c(16, 9, 25, 4) %>%
  sqrt() %>%
  sort() %>%
  rev()

# 12 - packages ----

# install once, load every session
# install.packages("data.table")
# library(data.table)

# pacman: install-if-missing + load in one step (already loaded at top)
p_load(data.table)
p_load(ggplot2)

# quick data.table preview
dt = as.data.table(df)
dt[gdp > 3]                       # filter
dt[, gdp_pc := gdp / pop * 1e6]   # add column
dt[, .(mean_gdp = mean(gdp))]     # summarise

# chaining with pipes
dt[gdp > 2] %>%
  .[order(-gdp)] %>%
  .[, .(country, gdp)]

# quick ggplot preview
p = ggplot(dt, aes(x = pop, y = gdp)) +
  geom_point(size = 3) +
  geom_text(aes(label = country), vjust = -1) +
  labs(title = "GDP vs Population",
       x = "Population (millions)",
       y = "GDP (trillions USD)") +
  theme_minimal()
print(p)

# 13 - getting help ----

?mean          # help page for a function
help(mean)     # same thing
example(mean)  # run examples from help page

# search across all installed packages
??regression

# 14 - cleanup ----

rm(x, y, v, named_v, m, df, my_list, value, counter, dt, p)
gc()
