###
# growth LASSO
# 260617
###

if (!require("pacman")) install.packages("pacman"); library(pacman)
p_load(hdm)
p_load(glmnet)
p_load(data.table)
p_load(ggplot2)

# create folders
getwd()
setwd("09-machine-learning")
dir.create("output/figures", showWarnings = FALSE, recursive = TRUE)
dir.create("input", showWarnings = FALSE, recursive = TRUE)

# recreate Barro and Lee (2010) dataset
data(GrowthData)
?GrowthData
data_growthdata = GrowthData

# make initial GDP per capita in logs
data_growthdata$log_gdpsh465 = log(data_growthdata$gdpsh465)
data_growthdata$gdpsh465 = NULL

# plot growth vs initial GDP
fit_unconditional <- lm(Outcome ~ log_gdpsh465, data = data_growthdata)
summary(fit_unconditional)

plot_growth_initial_gdp = ggplot() +
    geom_point(data = data_growthdata, aes(x = log_gdpsh465, y = Outcome)) +
    geom_smooth(data = data_growthdata, aes(x = log_gdpsh465, y = Outcome), method = "lm") +
    theme_minimal() +
    labs(x = "Log of GDP per capita in 1960",
         y = "Average annual growth rate of GDP per capita (1960-2000)",
         title = "Growth vs initial GDP")

ggsave("output/figures/260617_growth_initial_gdp.png", plot = plot_growth_initial_gdp,
       width = 6, height = 4, dpi = 150)


# conditional growth regression
fit_conditional <- lm(Outcome ~ ., data = data_growthdata)
summary(fit_conditional)


# LASSO regression

# select outcome, initial GDP and covariates
setDT(data_growthdata)
y = data_growthdata$Outcome
d = data_growthdata$log_gdpsh465
X = as.matrix(data_growthdata[, -c("Outcome", "log_gdpsh465")])

# run LASSO regression
fit_lasso <- cv.glmnet(cbind(d, X), y, alpha = 1)
coef(fit_lasso)
fit_lasso$beta
