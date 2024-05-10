install.packages("class")
install.packages("dplyr")
install.packages("ggplot2")
install.packages("reshape2")
install.packages("vcd")
install.packages("caret")

library(class)
library(dplyr)
library(ggplot2)
library(reshape2)
library(vcd)
library(caret)

milkQuality<- read.csv("D:/milknew.csv ",
                       header= TRUE, sep=",")
milkQuality

str(milkQuality)

colSums(is.na(milkQuality))


milkQuality$Grade<- as.numeric(factor(milkQuality$Grade))
gradeInNumeric <- c("high" = 3, "low" = 1, "medium" = 2)

milkQuality$Grade <- gradeInNumeric[milkQuality$Grade]
milkQuality


min_max_normalize <- function(x) {
  (x - min(x)) / (max(x) - min(x))
}

n_milkQuality <- min_max_normalize(milkQuality)
print(n_milkQuality)


cor_matrix <- cor(n_milkQuality)
print(cor_matrix)

melted_data <- melt(cor_matrix)

ggplot(data = melted_data, aes(x = Var1, y = Var2, fill = value)) +
  geom_tile() +
  geom_text(aes(label = round(value, 2)), color = "black", size = 3) +
  scale_fill_gradient(low = "yellow", high = "red") +
  theme_minimal() +
  labs(title = "Correlation Heatmap with Matrix Values")


n_milkQuality <- n_milkQuality %>% select(-pH)
n_milkQuality <- n_milkQuality %>% select(-Taste)
n_milkQuality


features <- n_milkQuality[, 1:5]
labels <- n_milkQuality[, 6]


normalized_features <- scale(features)

set.seed(123)

train_index <- sample(1:nrow(n_milkQuality), 0.7 * nrow(n_milkQuality))  # 70% for training
train_features <- normalized_features[train_index, ]
train_labels <- labels[train_index]
test_features <- normalized_features[-train_index, ]
test_labels <- labels[-train_index]

k <- 5 
knn_model <- knn(train_features, test_features, train_labels, k)

accuracy <- sum(knn_model == test_labels) / length(test_labels)
print(paste("Accuracy:", accuracy))


cat("Number of samples in training set:", length(train_labels), "\n")
cat("Number of samples in test set:", length(test_labels), "\n")


ctrl <- trainControl(method = "cv", number = 10)
knn_model_cv <- train(
  x = train_features,
  y = train_labels,
  method = "knn",
  trControl = ctrl,
  tuneGrid = expand.grid(k = k)
)
print(knn_model_cv)
best_k <- knn_model_cv$bestTune$k
knn_model <- knn(train_features, test_features, train_labels, best_k)
accuracy <- sum(knn_model == test_labels) / length(test_labels)
print(paste("Accuracy (Test Set):", accuracy))


test_labels <- factor(test_labels, levels = levels(knn_model))

conf_matrix <- confusionMatrix(data = knn_model, reference = test_labels)
print(conf_matrix)



precision <- conf_matrix$table[2, 2] / sum(conf_matrix$table[, 2])
recall <- conf_matrix$table[2, 2] / sum(conf_matrix$table[2, ])

cat("Precision:", precision, "\n")
cat("Recall:", recall, "\n")

