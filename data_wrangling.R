Sys.setLanguage(lang = "en")
library(mice)
library(tidyverse)

# Data import
colnames = c("id", "Clump_Thickness", "Uniformity_of_Cell_Size",
             "Uniformity_of_Cell_Shape", "Marginal_Adhesion",
             "Single_Epithelial_Cell_Size", "Bare_Nuclei",
             "Bland_Chromatin", "Normal_Nucleoli", "Mitoses",
             "Class")
df_raw = read.table("datasets/breast-cancer-wisconsin.data",
                    col.names = colnames,
                    sep = ",",
                    na.strings = "?")

# Inspection
head(df_raw)

# Change Class to categorical variable
df = df_raw %>%
  mutate(Class = as.factor(Class))

# Check Missing value
colSums(is.na(df_raw))

# Missing value
df_inputed = mice(df,
                  m = 10,
                  method = "pmm",
                  maxit = 10,
                  seed = 1)
df_complete = complete(df_inputed,1)
colSums(is.na(df_complete))

# Train/Validation Split
set.seed(2023)
n = nrow(df_complete)
ind = sample(n, 0.8 * n, F)
train = df_complete[ind,]
val = df_complete[-ind,]

# Write out
write.csv(train,
          file = "datasets/train.csv",
          row.names = FALSE)
write.csv(val,
          file = "datasets/val.csv",
          row.names = FALSE)

