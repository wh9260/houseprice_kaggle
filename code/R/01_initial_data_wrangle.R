# Load libraries

library(tidyverse)
library(MASS)
library(ggplot2)
library(scales)
library(forcats)

# Load data

rm(list = ls())

setwd("C:/Users/will/Google Drive/PhD/04 - Coding/00 - Learning/01 - Python/houseprice_kaggle/data/")

df <- read.csv("train.csv")

# Save copy of data for R use only

save(df, file = "train_R.csv")

# Summary stats for numerical features

df_names_categorical <- c(names(df)[!sapply(df, is.numeric)],"MSSubClass")

df_names_numerical <- setdiff(names(df), df_names_categorical)

df_categorial <- df[df_names_categorical]

df_numerical <- df[df_names_numerical]

# Summary stats for categorical features

summary(df_numerical)

#Explore dependent variable. Should it be normalised?

sale_price_norm_fit <- fitdistr(df_numerical$SalePrice, "normal")

para <- sale_price_norm_fit$estimate

sale_price_norm_fit_mean <- para[1]
sale_price_norm_fit_sd <- para[2]

norm_line$x <- seq()

ggplot(df_numerical, aes(x = SalePrice)) +
    geom_histogram(aes(y = after_stat(density)), position = "identity", bins = 100,alpha = 0.5) +
    scale_x_continuous(labels = comma) +
    scale_y_continuous(labels = comma) +
    geom_density(alpha = 0.6) +
    geom_vline(aes(xintercept = sale_price_norm_fit_mean), linetype = "dashed") +
    stat_function(fun = dnorm, args = list(mean = sale_price_norm_fit_mean, sd = sale_price_norm_fit_sd))

# Create a Q-Q plot using ggplot
ggplot(df, aes(sample = SalePrice)) +
    stat_qq() +
    stat_qq_line(color = "red", linetype = "dashed", size = 1) +
    ggtitle("Q-Q Plot of SalePrice") +
    theme_minimal() +
    scale_y_continuous(labels = comma)  # Change axis to comma format


#Start to look at patterns and correlations. A series of questions...

#1. Distribution of dwelling types and their relation to sale prices?
#2. Does zoning impact sale price?
#3. Does street and alley access types effect on sale price?
#4. What is the Average sale price by property shape?
#5. Is there a Correlation between Property Age and Sale Price
#6. Is there a Correlation between Living Area and Sale Price
#7. Does price change year to year?

#1. Distribution of dwelling types and their relation to sale prices?

# Count instances of the categorical variable (e.g., 'BldgType')
df_BldgType_counts <- df_categorial %>%
    count(BldgType)

df_BldgType_counts <- df_counts %>%
    mutate(BldgType = fct_reorder(BldgType, n, .desc = TRUE))

# Plot the counts using ggplot2
ggplot(df_BldgType_counts, aes(x = BldgType, y = n)) +
    geom_col(fill = "skyblue") +  # Use geom_col() for pre-counted data
    geom_text(aes(label = n), vjust = -0.5, size = 3) +  # Add values on top of the bars
    ggtitle("Counts of BldgType") +
    xlab("BldgType") +
    ylab("Count") +
    theme_minimal() +
    theme(plot.title = element_text(hjust = 0.5))  # Center the title

#Find mean sale price for each BldgType

mean_sales_prices <- df %>%
    group_by(BldgType) %>%
    summarise(MeanSalePrice = mean(SalePrice, na.rm = TRUE))

mean_sales_prices <- mean_sales_prices %>%
    mutate(BldgType = fct_reorder(BldgType, MeanSalePrice, .desc = TRUE))

# Plot the mean sale prices by BldgType
ggplot(mean_sales_prices, aes(x = BldgType, y = MeanSalePrice)) +
    geom_col(fill = "skyblue") +  # Use geom_col() for pre-counted data
    geom_text(aes(label = round(MeanSalePrice, 0)), vjust = -0.5, size = 3) +  # Add mean price labels
    ggtitle("Mean Sale Price by Building Type") +
    xlab("Building Type") +
    ylab("Mean Sale Price") +
    theme_minimal() +
    theme(plot.title = element_text(hjust = 0.5))  # Center the title







                   