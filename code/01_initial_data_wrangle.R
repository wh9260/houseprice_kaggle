# Load libraries

library(tidyverse)
library(MASS)
library(ggplot2)
library(scales)

# Load data

rm(list = ls())

df <- read.csv("./data/train.csv")

# Summary stats for numerical features

df_numerical <- df[names(df)[sapply(df, is.numeric)]]

summary(df_numerical)

# Summary stats for categorical features

df_categorial <- df[setdiff(names(df), names(df)[sapply(df, is.numeric)])]

# Explore dependent variable. Should it be normalised?

sale_price_norm_fit <- fitdistr(df_numerical$SalePrice, "normal")

para <- sale_price_norm_fit$estimate

sale_price_norm_fit_mean <- para[1]
sale_price_norm_fit_sd <- para[2]

norm_line$x <- seq()

ggplot(df_numerical, aes(x = SalePrice)) +
    geom_histogram(aes(y = after_stat(density)), position = "identity", bins = 50, alpha = 0.5) +
    scale_x_continuous(labels = comma) +
    scale_y_continuous(labels = comma) +
    geom_density(alpha = 0.6) +
    geom_vline(aes(xintercept = sale_price_norm_fit_mean), linetype = "dashed") +
    stat_function(fun = dnorm, args = list(mean = sale_price_norm_fit_mean, sd = sale_price_norm_fit_sd))

                   