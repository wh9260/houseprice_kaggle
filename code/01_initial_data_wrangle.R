# Load libraries

library(tidyverse)

# Load data

df <- read.csv("./data/train.csv")

# Summary stats for numerical features

df_numerical <- df[names(df)[sapply(df, is.numeric)]]

# Summary stats for categorical features

df_categorial <- df[setdiff(names(df), names(df)[sapply(df, is.numeric)])]



