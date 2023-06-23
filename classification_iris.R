# install the packages
install.packages("caret")
install.packages("C50")
install.packages("ellipse")
install.packages("randomForest")
install.packages("RWeka")
install.packages("gridExtra")
install.packages("GGally")

library("caret")
library("C50")
library("ellipse")
library("randomForest")
library("RWeka")
library("ggplot2")
library("gridExtra")
library("GGally")

data("iris")
head(iris)

dim(iris)
str(iris)
sapply(iris, function(x) sum(is.na(x)))
summary(iris)
sd(iris$Sepal.Length)
sd(iris$Sepal.Width)
sd(iris$Petal.Length)
sd(iris$Petal.Width)

x <- iris[,1:4]
y <- iris[,5]

par(mfrow=c(1,4))
for(i in 1:4) {
  boxplot(x[,i], main=names(iris)[i])
}

par(mfrow=c(2,2))
hist(iris$Sepal.Length,
     main = " ",
     xlab = "Sepal Length",
     ylab = "Frequency")
hist(iris$Sepal.Width,
     main = " ",
     xlab = "Sepal Width",
     ylab = "Frequency")
hist(iris$Petal.Length,
     main = " ",
     xlab = "Petal Length",
     ylab = "Frequency")
hist(iris$Petal.Width,
     main = " ",
     xlab = "Petal Width",
     ylab = "Frequency")

plots <- list(
  ggplot(iris, aes(x=Sepal.Length, fill=Species)) +
    geom_histogram(aes(y=after_stat(density)), alpha=.5, position="identity", bins=30) +
    geom_density(alpha=.2) +
    xlab("Sepal Length") +
    ylab("Density") +
    scale_fill_manual(values=c("#00AFBB", "#E7B800", "#FC4E07")),
  ggplot(iris, aes(x=Sepal.Width, fill=Species)) +
    geom_histogram(aes(y=after_stat(density)), alpha=.5, position="identity", bins=30) +
    geom_density(alpha=.2) +
    xlab("Sepal Width") +
    ylab("Density") +
    scale_fill_manual(values=c("#00AFBB", "#E7B800", "#FC4E07")),
  ggplot(iris, aes(x=Petal.Length, fill=Species)) +
    geom_histogram(aes(y=after_stat(density)), alpha=.5, position="identity", bins=30) +
    geom_density(alpha=.2) +
    xlab("Petal Length") +
    ylab("Density") +
    scale_fill_manual(values=c("#00AFBB", "#E7B800", "#FC4E07")),
  ggplot(iris, aes(x=Petal.Width, fill=Species)) +
    geom_histogram(aes(y=after_stat(density)), alpha=.5, position="identity", bins=30) +
    geom_density(alpha=.2) +
    xlab("Petal Width") +
    ylab("Density") +
    scale_fill_manual(values=c("#00AFBB", "#E7B800", "#FC4E07"))
)
grid.arrange(grobs=plots, ncol=2)

ggpairs(iris, columns=1:4, aes(color=Species))

featurePlot(x=iris[,1:4], y=iris[,5], plot="box", scales=list(x=list(relation="free"),
            y=list(relation="free")), auto.key=list(columns=3))

ggplot(iris, aes(x=Species, fill=Species)) + geom_bar()



# Correlation panel
panel.cor <- function(x, y){
  usr <- par("usr"); on.exit(par(usr))
  par(usr = c(0, 1, 0, 1))
  r <- round(cor(x, y), digits=2)
  txt <- paste0("R = ", r)
  cex.cor <- 0.8/strwidth(txt)
  text(0.5, 0.5, txt, cex = cex.cor * r)
}
# Customize upper panel
upper.panel<-function(x, y){
  points(x,y, pch = 19, col = iris$Species)
}
# Create the plots
pairs(iris[,1:4], 
      lower.panel = panel.cor,
      upper.panel = upper.panel)



# set a random seed for result reproducibility
set.seed(Sys.time())
# compute the partition index
index <- createDataPartition(iris$Species, p=.70, list=FALSE)
# define the training set
df.train <- iris[index,]
df.train
dim(df.train)
summary(df.train)
# define the testing set
df.test <- iris[-index,]
df.test
dim(df.test)
summary(df.test)

# DATA EXPLORATION

# exploring data
x <- df.train[ , 1:4]
y <- df.train[ , 5]

par(mfrow=c(1,4))
for(i in 1:4) {
  boxplot(x[,i], main=names(df.train)[i])
}

# https://machinelearningmastery.com/data-visualization-with-the-caret-r-package/

featurePlot(x=x, y=y, plot="pairs", auto.key=list(columns=3))
featurePlot(x=x, y=y, plot="ellipse", auto.key=list(columns=3))
featurePlot(x=x, y=y, plot="density", scales=list(x=list(relation="free"), y=list(relation="free")), auto.key=list(columns=3))
featurePlot(x=x, y=y, plot="box", scales=list(x=list(relation="free"), y=list(relation="free")), auto.key=list(columns=3))


# DECISION TREE CLASSIFIER
# training phase
C45.model <- J48(Species~., data = df.train)
C45.model
summary(C45.model)
plot(C45.model)
# testing phase
C45.pred <- predict(C45.model, df.test)
C45.pred
summary(C45.pred)
confusionMatrix(data = C45.pred, reference = df.test$Species, mode = "everything")
confusionMatrix(data = C45.pred, reference = df.test$Species)
# training phase
C50.model <- C5.0(Species~., data = df.train)
C50.model
summary(C50.model)
plot(C50.model)
# testing phase
C50.pred <- predict(C50.model, df.test)
C50.pred
summary(C50.pred)
confusionMatrix(data = C50.pred, reference = df.test$Species, mode = "everything")

# RANDOM FOREST IMPLEMENTATION
# training phase
rf.model <- randomForest(Species ~., data = df.train, importance = TRUE)
rf.model
varImpPlot(rf.model, main="")
# testing phase
rf.pred <- predict(rf.model, df.test)
rf.pred
confusionMatrix(data = rf.pred, reference = df.test$Species, mode = "everything")
confusionMatrix(data = rf.pred, reference = df.test$Species)

install.packages("party")
library("party")
x <- ctree(Species ~ ., data=df.train)
plot(x, type="simple")

install.packages("e1071")
library("e1071")

svm.model <- svm(Species ~., data = df.train, kernel = "linear")
svm.model
summary(svm.model)
plot(svm.model, df.train)
svm.pred <- predict(svm.model, df.test)
svm.pred
confusionMatrix(data = svm.pred, reference = df.test$Species)

support_vectors <- svm.model$SV

# Print the support vectors
print(support_vectors)

# Get the coefficients of the hyperplane
hyperplane_coefs <- svm.model$coefs

# Print the coefficients of the hyperplane
print(hyperplane_coefs)


