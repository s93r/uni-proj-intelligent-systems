install.packages("tidyverse")
install.packages("ggplot2")
install.packages("ggcorrplot")
install.packages("cowplot")
install.packages("tree")
install.packages("ranger")
install.packages("caret")
install.packages("leaps")
install.packages("Metrics")
install.packages("MASS")

library(ggcorrplot)
library(tidyverse)
library(ggplot2)
library(cowplot)
library(Metrics)
library(ranger)
library(glmnet)
library(caret)
library(leaps)
library(tree)
library(MASS)

fires.data <- read.csv("C:\\RData\\forestfires.csv", header=T)
head(fires.data)

str(fires.data)
summary(fires.data)
sd(fires.data$FFMC)
sd(fires.data$DMC)
sd(fires.data$DC)
sd(fires.data$ISI)
sd(fires.data$temp)
sd(fires.data$RH)
sd(fires.data$wind)
sd(fires.data$rain)
sd(fires.data$area)

fires.data$X <- factor(fires.data$X)
fires.data$Y <- factor(fires.data$Y)
fires.data$month <- factor(fires.data$month, levels = c("jan","feb","mar","apr","may","jun","jul","aug","sep","oct","nov","dec"))
fires.data$day <- factor(fires.data$day, levels = c("mon","tue","wed","thu","fri","sat","sun"))

point_alpha <- 0.4
line_color <- "brown3"

set.seed(42)
train_nrow <- floor(0.8 * nrow(fires.data))
train_idx <- sample(seq_len(nrow(fires.data)), size = train_nrow)

fires <- fires.data[train_idx,]
cat("Training set size: ", nrow(fires))
fires.test <- fires.data[-train_idx,]
cat("Testing set size: ", nrow(fires.test))

# data exploration

par(mfrow=c(1,9))
boxplot(fires$FFMC, xlab = "FFMC")
boxplot(fires$DMC, xlab = "DMC")
boxplot(fires$DC, xlab = "DC")
boxplot(fires$ISI, xlab = "ISI")
boxplot(fires$temp, xlab = "temp")
boxplot(fires$RH, xlab = "RH")
boxplot(fires$wind, xlab = "wind")
boxplot(fires$rain, xlab = "rain")
boxplot(fires$area, xlab = "area")

par(mfrow=c(3,3))
hist(fires$FFMC, main = "", xlab = "FFMC", ylab = "")
hist(fires$DMC, main = "", xlab = "DMC", ylab = "")
hist(fires$DC, main = "", xlab = "DC", ylab = "")
hist(fires$ISI, main = "", xlab = "ISI", ylab = "")
hist(fires$temp, main = "", xlab = "temp", ylab = "")
hist(fires$RH, main = "", xlab = "RH", ylab = "")
hist(fires$wind, main = "", xlab = "wind", ylab = "")
hist(fires$rain, main = "", xlab = "rain", ylab = "")
hist(fires$area, main = "", xlab = "area", ylab = "")

# spatial data
coord_counts <- merge(as.data.frame(table(fires[, 1:2])), expand.grid(X=as.factor(c(1:9)), Y=as.factor(c(1:9))), by=c("X", "Y"), all=TRUE)
ggplot() +
  geom_raster(data=coord_counts, aes(x=X, y=Y, fill=Freq)) +
  scale_fill_gradient(low="white", high="brown3", na.value = "white", name="Count") +
  scale_x_discrete(position = "top") +
  scale_y_discrete(limits=factor(9:1)) +
  theme(plot.title = element_text(hjust = 0.5))

# burned area
small_big_count <- data.frame(
  factor(c("small (<100m^2)", "big (>100m^2)"), levels=c("small (<100m^2)", "big (>100m^2)")),
  c(sum(fires$area == 0), sum(fires$area > 0))
)
colnames(small_big_count) <- c("area", "count")
ggplot(data=small_big_count, aes(x=area, y=count)) +
  geom_bar(stat="identity", width=0.5) +
  theme(plot.title = element_text(hjust = 0.5))

ggplot() +
  geom_histogram(data=fires, mapping=aes(x=area), binwidth=30) +
  theme(plot.title = element_text(hjust = 0.5))

ggplot() +
  geom_histogram(data=fires, mapping=aes(x=log(area+1)), binwidth=0.2) +
  theme(plot.title = element_text(hjust = 0.5))


fires.big <- fires[fires$area > 0, ]
ggplot(data=fires) +
  geom_jitter(aes(x=X, y=Y, color=log(area+1)), alpha=0.8) +
  scale_color_gradient(low="blue3", high="brown1", na.value="lightblue4", name="ln(area+1)", lim=c(min(log(fires.big$area+1)), max(log(fires.big$area+1)))) +
  scale_x_discrete(position = "top") +
  scale_y_discrete(limits=factor(9:1)) +
  theme_bw() +
  theme(plot.title = element_text(hjust = 0.5),
        plot.background = element_blank(),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank())

#temporal data
moty_order <- factor(fires$month, c("jan", "feb", "mar", "apr", "may", "jun", "jul", "aug", "sep", "oct", "nov", "dec"))
areas_month_plot <- ggplot(data=fires) +
  geom_jitter(mapping=aes(x=moty_order, y=log(1+area)), width=0.1, alpha=0.4) +
  theme(plot.title = element_text(hjust = 0.5)) +
  labs(x="month of the year")
count_month_plot <- ggplot(data=fires) +
  geom_bar(mapping=aes(x=moty_order)) +
  theme(plot.title = element_text(hjust = 0.5)) +
  labs(x="month of the year")
plot(areas_month_plot)
plot(count_month_plot)


fires$isweekend <- factor(ifelse(fires$day %in% c("mon", "tue", "wed", "thu"), 0, 1))
fires.test$isweekend <- factor(ifelse(fires.test$day %in% c("mon", "tue", "wed", "thu"), 0, 1))

areas_weekend_plot <- ggplot(data=fires) +
  geom_jitter(mapping=aes(x=isweekend, y=log(1+area)), width=0.1, alpha=0.4) +
  theme(plot.title = element_text(hjust = 0.5)) +
  labs(x="is weekend?")
count_weekend_plot <- ggplot(data=fires) +
  geom_bar(mapping=aes(x=isweekend), width=0.5) +
  theme(plot.title = element_text(hjust = 0.5)) +
  labs(x="is weekend?")
plot(areas_weekend_plot)
plot(count_weekend_plot)

# weather data
nbins <- 30
plot_grid(nrow=2, ncol=2,
          ggplot(data=fires) + geom_histogram(mapping=aes(x=FFMC), bins=nbins),
          ggplot(data=fires) + geom_histogram(mapping=aes(x=DMC), bins=nbins),
          ggplot(data=fires) + geom_histogram(mapping=aes(x=DC), bins=nbins),
          ggplot(data=fires) + geom_histogram(mapping=aes(x=ISI), bins=nbins))
plot_grid(nrow=2, ncol=2,
          ggplot(data=fires) + geom_histogram(mapping=aes(x=temp), bins=nbins),
          ggplot(data=fires) + geom_histogram(mapping=aes(x=RH), bins=nbins),
          ggplot(data=fires) + geom_histogram(mapping=aes(x=wind), bins=nbins),
          ggplot(data=fires) + geom_histogram(mapping=aes(x=rain), bins=nbins))

rain_count <- data.frame(c("zero", "non-zero"), c(nrow(subset(fires, rain==0)), nrow(subset(fires, rain>0))))
colnames(rain_count) <- c("rain", "count")
ggplot(data=rain_count, aes(x=rain, y=count)) +
  geom_bar(stat="identity", width=0.5) +
  theme(plot.title = element_text(hjust = 0.5))

# correlation
cm <- cor(fires[, c(5,6,7,8,9,10,11,13)])
ggcorrplot(cm, type="lower", lab=TRUE)

# DATA ANALYSIS

# linear regression
set.seed(Sys.time())
complete.lm <- lm(area ~ ., data=fires)
summary(complete.lm)
par(mfrow=c(2,2))
plot(complete.lm)

confint(complete.lm)

# performance metrics
loginv <- function(x) {
  output <- exp(x) - 1
  output[output < 0] <- 0.
  return(output)
}
mae <- function(truth, preds) {
  mae <- mean(abs(truth - preds))
  return(mae)
}
rmse <- function(truth, preds) {
  rmse <- sqrt(mean((truth - preds)^2))
  return(rmse)
}
complete.preds <- predict(complete.lm, newdata=fires.test)
ggplot(fires.test, aes(x=fires.test$area, y=complete.preds)) +
  geom_smooth(method="lm",se=FALSE,color="red") +
  geom_point(alpha=0.4) +
  xlab("actual area values") +
  ylab("predicted area values") +
  theme(text = element_text(size = 12))
print(data.frame("MAE" = mae(fires.test$area,complete.preds),
                 "RMSE" = rmse(fires.test$area,complete.preds),
                 "RAE" = rae(fires.test$area,complete.preds),
                 row.names=c("complete.lm")))

# ridge regression
set.seed(Sys.time())

y <- fires$area
x <- data.matrix(fires)

ridge <- glmnet(x,y,alpha=0)
summary(ridge)
cv_model <- cv.glmnet(x,y,alpha=0)
best_lambda <- cv_model$lambda.min
best_lambda
plot(cv_model)
best_model <- glmnet(x,y,alpha=0,lambda=best_lambda)
summary(best_model)
coef(best_model)
best_model
plot(ridge,xvar="lambda")
y_predicted <- predict(cv_model, s = best_lambda, newx=data.matrix(fires.test))

ggplot(fires.test, aes(x=fires.test$area, y=y_predicted)) +
  geom_smooth(method="lm",se=FALSE,color="red") +
  geom_point(alpha=0.4) +
  xlab("actual area values") +
  ylab("predicted area values") +
  theme(text = element_text(size = 12))

rmse_ridge <- RMSE(y_predicted, fires.test$area)
rae_ridge <- rae(y_predicted,fires.test$area)
mae_ridge <- mae(y_predicted,fires.test$area)
rmse_ridge
rae_ridge
mae_ridge


#lasso
set.seed(Sys.time())
y <- fires$area
x <- data.matrix(fires)

lasso <- glmnet(x,y,alpha=1)
summary(lasso)
cv_model <- cv.glmnet(x,y,alpha=1)
best_lambda <- cv_model$lambda.min
best_lambda
plot(cv_model)
best_model <- glmnet(x,y,alpha=1,lambda=best_lambda)
coef(best_model)
plot(lasso,xvar="lambda")
y_predicted <- predict(cv_model, s = best_lambda, newx=data.matrix(fires.test))

ggplot(fires.test, aes(x=fires.test$area, y=y_predicted)) +
  geom_smooth(method="lm",se=FALSE,color="red") +
  geom_point(alpha=0.4) +
  xlab("actual area values") +
  ylab("predicted area values") +
  theme(text = element_text(size = 12))

rmse_lasso <- RMSE(y_predicted, fires.test$area)
mae_lasso <- mae(y_predicted, fires.test$area)
rae_lasso <- rae(y_predicted, fires.test$area)
rmse_lasso
mae_lasso
rae_lasso
