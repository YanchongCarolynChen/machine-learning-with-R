# KNN
library(dplyr)
library(caret)

rm(list = ls())
gc()

iris[, -ncol(iris)] <- scale(iris[, -ncol(iris)])
iris$Species <- as.numeric(iris$Species)

p <- 0.75
samples <- sample(nrow(iris), nrow(iris)*p)
iris_train <- iris[samples, ]
# iris_train$Species <- as.character(iris_train$Species)
iris_test <- iris[-samples, ]
# iris_test$Species <- as.character(iris_test$Species)

train_target <- iris_train$Species
test_target <- iris_test$Species

train_data <- iris_train[, -which(names(iris_train) == "Species")]
test_data <- iris_test[, -which(names(iris_test) == "Species")]

knn_classifier <- function(test_data, test_target, train_data, train_target, k) {
  centr_matrix = unlist(rep(test_data, times = nrow(train_data)), use.names = F) %>% 
    matrix(byrow = T, ncol = length(names(train_data)))
  diff = as.matrix(train_data) - centr_matrix
  sqrd_distance = apply(diff^2, 1, sum)
  distance = as.numeric(sqrd_distance^0.5)
  
  sorted_distance_indices = rank(distance)
  class_counter = c()
  for (i in 1:k){
    target_sort = train_target[sorted_distance_indices == i]
    class_counter = c(class_counter, target_sort)
  }
  max_count = plyr::count(class_counter) %>% 
    arrange(-freq) %>% .[1, 1]
  return(max_count)
}

knn_classifier(test_data = test_data[1, ],
               test_target = test_target,
               train_data = train_data,
               train_target = train_target,
               k = 5)

irisClassifyTest <- function(test_data, train_data, train_target, test_target, k = 5) {
  m = nrow(test_data)
  w = ncol(test_data)
  error_count = 0.0
  test_predict = c()
  for (i in 1:m) {
    classifier_result = knn_classifier(test_data = test_data[i, ],
                                       train_data = train_data,
                                       train_target = train_target,
                                       k = k)
    if (classifier_result != test_target[i]) {
      error_count = error_count + 1.0
    }
    test_predict = c(test_predict, classifier_result)
  }
  test_data[["test_predict"]] = test_predict
  test_data[["class"]] = test_target
  target_names = c("setosa", "versicolor", "virginica")
  print(confusionMatrix(factor(test_predict, labels = target_names), factor(test_target, labels = target_names)))
  # print(confusionMatrix(factor(test_predict), factor(test_target)))
  confusion_matrix = table(test_predict, test_target)
  dimnames(confusion_matrix) <- list(target_names, target_names)
  cat(sprintf("in datingClassTest, the total error rate is: %f", error_count/m), sep = "\n")
  cat(sprintf("in datingClassTest, error count: %d", error_count, sep = "\n"))
  return(list(test_data = test_data, confusion_matrix = confusion_matrix))
}

require(e1071)
result <- irisClassifyTest(test_data = test_data,
                          train_data = train_data,
                          train_target = train_target,
                          test_target = test_target)

result$test_data
result$confusion_matrix


