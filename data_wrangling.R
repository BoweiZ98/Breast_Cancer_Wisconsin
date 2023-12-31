Sys.setLanguage(lang = "en")
library(mice)
library(tidyverse)
# install.packages("gtsummary")
library(gtsummary)

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
  mutate(Class = as.factor(if_else(Class == 2, 0, 1))) %>%
  select(-id)

# Dist. of df
df %>%
  keep(is.numeric) %>%
  gather %>%
  ggplot(aes(value)) +
  facet_wrap(~ key, scales = "free") +
  geom_histogram()

# Summary table
df %>% tbl_summary(b = Class)

# Check Missing value
colSums(is.na(df_raw))
md.pattern(df)

# Missing value
df_inputed = mice(df,
                  m = 10,
                  method = "pmm",
                  maxit = 10,
                  seed = 1)
df_complete = complete(df_inputed,1)
colSums(is.na(df_complete))

# Dist. of df_complete
df_complete %>%
  keep(is.numeric) %>%
  gather %>%
  ggplot(aes(value)) +
  facet_wrap(~ key, scales = "free") +
  geom_histogram()

# Summary Table After imputation
df_complete %>% tbl_summary(b = Class)

# Train/Validation Split
set.seed(2023)
n = nrow(df_complete)
ind = sample(n, 0.8 * n, F)
train = df_complete[ind,]
val = df_complete[-ind,]

# Summary table
train %>% tbl_summary(b = Class)
val %>% tbl_summary(b = Class)

# Write out
write.csv(train,
          file = "datasets/train.csv",
          row.names = FALSE)
write.csv(val,
          file = "datasets/val.csv",
          row.names = FALSE)

