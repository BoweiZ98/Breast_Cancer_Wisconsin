library(tidyverse)

data <- data.frame(
  Method = c("Logistic Regression", "LASSO", "Rdige", "5NN", "Random Forest"),
  Accuracy = c(0.9714,0.9643, 0.9643, 0.9786, 0.9786),
  Upper_CI = c(0.9285,0.9186, 0.9186, 0.9387, 0.9387),
  Lower_CI = c(0.9922, 0.9883, 0.9883, 0.9956, 0.9956)
)

ggplot(data, aes(x = Method, y = Accuracy)) +
  geom_boxplot(width = 0.2) +
  labs(title = "Accuracy Boxplot",
       x = "Method Used",
       y = "Accuracy") +
  geom_errorbar(
    aes(ymin = Lower_CI, ymax = Upper_CI),
    width = 0.2,
    position = position_dodge(0.75)
  ) +
  ylim(0.8,1) +
  theme_bw()
