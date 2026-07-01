###
# 00 - ML Basics with the iris dataset
# 260610
###

# Gentle on-ramp for the machine-learning lecture.  We use the classic
# iris dataset to walk through:
#
# 1. Train / test split
# 2. k-Nearest Neighbours classification
# 3. Decision tree
# 4. Confusion matrix and accuracy
# 5. Principal Component Analysis (PCA)
# 6. k-means clustering
# 7. Elbow method for choosing k

if (!require("pacman")) install.packages("pacman"); library(pacman)
p_load(class)
p_load(rpart)
p_load(rpart.plot)
p_load(data.table)
p_load(magrittr)
p_load(ggplot2)
p_load(patchwork)

# 0 - settings ----
setwd("09-machine-learning")
dir.create("output/figures", showWarnings = FALSE, recursive = TRUE)
set.seed(1234)

# 1 - explore data ----

data(iris)
iris_dt <- as.data.table(iris)
str(iris_dt)
summary(iris_dt)

# Two views of the data
plot_sepal <- ggplot() +
  geom_point(data = iris_dt,
  aes(x = Sepal.Length, y = Sepal.Width, color = Species), size = 2, alpha = 0.8) +
  theme_minimal() +
  labs(title = "Sepal length vs width")

plot_petal <- ggplot() +
  geom_point(data = iris_dt,
             aes(x = Petal.Length, y = Petal.Width, color = Species), size = 2, alpha = 0.8) +
  theme_minimal() +
  labs(title = "Petal length vs width")

plot = (plot_sepal + plot_petal) + plot_layout(guides = "collect") & theme(legend.position = "bottom")

ggsave("output/figures/260610_iris_features.png", plot = plot,
       width = 10, height = 4, dpi = 150)

# Question: which pair of features separates the species better?

# 2 - train / test split ----

n         <- nrow(iris_dt)
train_idx <- sample(seq_len(n), size = 0.5 * n)
train_dt  <- iris_dt[train_idx]
test_dt   <- iris_dt[-train_idx]

# 3 - k-Nearest Neighbours ----

# k = 1: very flexible, low bias, high variance
knn_1 <- knn(train = train_dt[, 1:4],
             test  = test_dt[, 1:4],
             cl    = train_dt$Species,
             k     = 1)

# k = 20: smoother boundary, higher bias, lower variance
knn_20 <- knn(train = train_dt[, 1:4],
              test  = test_dt[, 1:4],
              cl    = train_dt$Species,
              k     = 20)

# Confusion matrices
table(truth = test_dt$Species, predicted = knn_1)
table(truth = test_dt$Species, predicted = knn_20)

# Accuracy
mean(knn_1  == test_dt$Species)
mean(knn_20 == test_dt$Species)

# Exercise: loop over k = 1, 3, 5, ..., 49 and plot test accuracy vs k.
#           Where does the U-shape kick in?

# 4 - decision tree ----

tree_fit <- rpart(Species ~ ., data = train_dt, method = "class")
rpart.plot(tree_fit)

# Predict and evaluate
tree_pred <- predict(tree_fit, newdata = test_dt, type = "class")
table(truth = test_dt$Species, predicted = tree_pred)
mean(tree_pred == test_dt$Species)

# Interpretation: each split is a yes/no question on one feature.
#                 The tree prints the rules in plain language.

# 5 - PCA ----

iris_scaled <- scale(iris_dt[, 1:4])

pca <- prcomp(iris_scaled)
summary(pca)

# Variance explained
var_explained <- pca$sdev^2 / sum(pca$sdev^2)
data.table(PC = paste0("PC", seq_along(var_explained)),
           variance = var_explained,
           cumulative = cumsum(var_explained))

# PCA scatter
pca_dt <- as.data.table(pca$x)
pca_dt[, Species := iris_dt$Species]

ggplot(pca_dt, aes(PC1, PC2, color = Species)) +
  geom_point(size = 2, alpha = 0.8) +
  theme_minimal() +
  labs(title = "PCA of iris",
       x = paste0("PC1 (", round(100 * var_explained[1]), "% var)"),
       y = paste0("PC2 (", round(100 * var_explained[2]), "% var)"))

ggsave("output/figures/260610_iris_pca.png",
       width = 6, height = 5, dpi = 150)

# Interpretation: the first two PCs capture ~95% of the variation.
#                 Species separate cleanly even without using labels.

# 6 - k-means clustering ----

km <- kmeans(iris_scaled, centers = 3, nstart = 25)

cluster_dt <- copy(pca_dt)
cluster_dt[, Cluster := factor(km$cluster)]

ggplot(cluster_dt, aes(PC1, PC2, color = Cluster, shape = Species)) +
  geom_point(size = 2.5, alpha = 0.8) +
  theme_minimal() +
  labs(title = "k-means on iris (k = 3), shown in PCA space")

ggsave("output/figures/260610_iris_kmeans.png",
       width = 6.5, height = 5, dpi = 150)

# How well do the clusters match the species?
table(truth = iris_dt$Species, cluster = km$cluster)

# 7 - elbow method ----

k_grid <- 1:10
wcss <- sapply(k_grid, function(k) {
  kmeans(iris_scaled, centers = k, nstart = 25)$tot.withinss
})

elbow_dt <- data.table(k = k_grid, wcss = wcss)

ggplot(elbow_dt, aes(k, wcss)) +
  geom_line() +
  geom_point(size = 2) +
  scale_x_continuous(breaks = k_grid) +
  theme_minimal() +
  labs(title = "Elbow method for choosing k",
       x = "Number of clusters k",
       y = "Within-cluster sum of squares")

ggsave("output/figures/260610_iris_elbow.png",
       width = 6, height = 4, dpi = 150)

# Question: where is the elbow?  Does it match the three species?

# 8 - cleanup ----

rm(list = ls())
gc()
