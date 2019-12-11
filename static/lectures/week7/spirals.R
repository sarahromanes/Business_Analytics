library(tidyverse)
library(MASS)
library(e1071)
library(gridExtra)
library(randomForest)

# Generate the data

spirals <-mlbench.spirals(300,1.5,0.05)
data <- as.data.frame(cbind(spirals$x, spirals$classes))
colnames(data) <-  c("X1", "X2", "class")
data$class <- as.factor(data$class)
data_p <- as.data.frame(expand.grid(X1 = seq(-1.5,1.5,0.05), X2 = seq(-1.5,1.5,0.05)))

# Generate RF plot

data_RF <- randomForest(class ~ X1 + X2, data = data)
data_p$region_RF <- predict(data_RF, data_p)

p1 <- ggplot() +
  geom_point(data = data_p, aes(x = X1, y = X2, color = region_RF), alpha = 0.2) +
  geom_point(data = data, aes(x = X1, y = X2, color = class, shape = class), size = 2.5) +
  geom_contour(data = data_p, aes(x= X1, y=X2, z= as.numeric(region_RF)), breaks=c(1.5), color="black", size=1) +
  scale_color_brewer("", palette="Dark2") +
  theme_bw() + theme(aspect.ratio=1, legend.position="none") +
  ggtitle("Random Forest")

# Generate SVM plot 


data_SVM <- svm(class ~ X1 + X2, data = data, kernel = "radial", cost = 10)
data_p$region_SVM <- predict(data_SVM, data_p)

p2 <- ggplot() +
  geom_point(data = data_p, aes(x = X1, y = X2, color = region_SVM), alpha = 0.2) +
  geom_point(data = data, aes(x = X1, y = X2, color = class, shape = class), size = 2.5) +
  geom_contour(data = data_p,  aes(x= X1, y=X2, z= as.numeric(region_SVM)), breaks=c(1.5), color="black", size=1) +
  scale_color_brewer("", palette="Dark2") +
  theme_bw() + theme(aspect.ratio=1, legend.position="none") +
  ggtitle("SVM")


data_lda <- lda(class ~ X1 + X2, data = data)
data_p$region_lda <- predict(data_lda, data_p)$class

p3 <- ggplot() +
  geom_point(data = data_p, aes(x = X1, y = X2, color = region_lda), alpha = 0.2) +
  geom_point(data = data, aes(x = X1, y = X2, color = class, shape = class), size = 2.5) +
  geom_contour(data = data_p,  aes(x= X1, y=X2, z= as.numeric(region_lda)), breaks=c(1.5), color="black", size=1) +
  scale_color_brewer("", palette="Dark2") +
  theme_bw() + theme(aspect.ratio=1, legend.position="none") +
  ggtitle("LDA")


grid.arrange(p1, p2, p3, ncol=3)
####################################################################

