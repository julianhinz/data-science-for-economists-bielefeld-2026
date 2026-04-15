###
# 01 - examples
# 260415
# - first R examples for the data science for economists course
### 

if (!require("pacman")) install.packages("pacman"); library(pacman)
p_load(ggplot2)

# create a simple data frame
data <- data.frame(
  x = 1:10,
  y = c(2, 3, 5, 7, 11, 13, 17, 19, 23, 29)
)
