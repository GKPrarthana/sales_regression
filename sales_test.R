head(house_test)
colnames(house_test)
str(house_test)

#add age
library(lubridate)
house_test$age<-year(house_test$date)-house_test$yr_built

#check missing/duplicated
colSums(is.na(house_test))
sum(duplicated(house_test))
#all clear

#set waterfront and view as factor(categorical) vector
house_test$waterfront<-factor(house_test$waterfront)
house_test$view<-factor(house_test$view)
str(house_test$waterfront,house_test$view)

#drop unnecessary columns
data_cleaned<-house_test[,-c(1:2,16:19)]
View(data_cleaned)

#explore dataset
hist(data_cleaned$price)
plot(data_cleaned$price)
boxplot(data_cleaned$price)

#split data into train and test
set.seed(1234)
index<-sample(nrow(data_cleaned),.70*nrow(data_cleaned))
train_data<-data_cleaned[index,]
test_data<-data_cleaned[-index,]
dim(train_data)
dim(test_data)

#check correlation
View(train_data)
numeric_cols <- sapply(train_data, is.numeric)
correlation <- cor(train_data[, numeric_cols],
                   method = c("pearson"))
library(corrplot)
corrplot(correlation,method = "number")

#scatterplot matrix
pairs(price~sqft_living+grade+sqft_above+sqft_living15,
      data = train_data)
#plots
plot(train_data$sqft_living,train_data$price)
abline(lm(train_data$price~train_data$sqft_living), col="red") # regression line (y~x)
lines(lowess(train_data$sqft_living,train_data$price), col="blue") # lowess line (x,y)

plot(train_data$grade,train_data$price)
abline(lm(train_data$price~train_data$grade), col="red")
lines(lowess(train_data$grade,train_data$price), col="blue")
#not obviously linear

plot(train_data$sqft_above,train_data$price)
abline(lm(train_data$price~train_data$sqft_above), col="red")
lines(lowess(train_data$sqft_above,train_data$price), col="blue")

plot(train_data$sqft_living15,train_data$price)
abline(lm(train_data$price~train_data$sqft_living15), col="red")
lines(lowess(train_data$sqft_living15,train_data$price), col="blue")

#p-values
highly_correlated_pairs <- which(upper.tri(correlation, diag = TRUE)
                                 & abs(correlation) > 0.7, arr.ind = TRUE)
highly_correlated_pairs
train_data1<-train_data[,-c(4:5,13)]
View(train_data1)

model1<-lm(price~.,data = train_data1)
summary(model1)

p_values<-summary(model1)$coefficients[,4]
p_values
#all are significant

#check outliers
boxplot(train_data1$price,
        main = "Boxplot of House Prices",
        xlab="Bedrooms", ylab="Price")
outliers<-boxplot(train_data1$price,plot=FALSE)$out
outlier_data<-train_data1[which(train_data1$price %in% outliers),]
train_data1_noout<-train_data1[-which(train_data1$price %in% outliers),]
View(outlier_data)
View(train_data1_noout)

model2<-lm(price~.,data = train_data1_noout)
summary(model2)

plot(train_data1$bedrooms, train_data1$price, main="With Outliers", xlab="bedrooms", ylab="price", cex=2)
abline(lm(price ~ bedrooms, data=train_data1), col="green",lwd=3)

plot(train_data1_noout$bedrooms, train_data1_noout$price, main="Outliers removed", xlab="bedrooms", ylab="price", cex=2)
abline(lm(price ~ bedrooms, data=train_data1_noout), col="red", lwd=3)

#----------------------ANOVA------------
one_way <- aov(price ~ condition, data = train_data1)
summary(one_way)

#--------------------Multiple regression-------
lm<-lm(price~., data=train_data1)
summary(lm)

library(MASS)
step <- stepAIC(lm, direction="both")

step$anova

library(caret)
train_model<-train(price~.,data=train_data1, method="lm")
summary(train_model)
# display the R-squared value
train_model_rsq <- summary(train_model)$r.squared
train_model_rsq
# show the bootstrap results
train_model

#influential points
cooksd <- cooks.distance(lm)
mean(cooksd)
plot(cooksd, main="Influential Obs by Cooks distance")
abline(h = 4*mean(cooksd, na.rm=T), col="green") 

influential <- as.numeric(names(cooksd)[(cooksd > 4*mean(cooksd, na.rm=T))])  
head(train_data1[influential, ])

influential_data <- train_data1[influential, ]
library(dplyr)
# inner join outliers_data and influential_data
influential_outliers <- inner_join(outlier_data,influential_data)
# remove outliers_data from train_data but keep the influential_data
train_data2 <- rbind(train_data1,influential_outliers)
View(train_data2)
# relabel the index of train_data2
row.names(train_data2) <- NULL

lm1<-lm(price~.,data = train_data2)
summary(lm1)
summary(lm)
#RSE(lm) < RSE(lm1) --> use lm

plot(lm1)
library(car)
vif(lm1)
#no multicollinearity

#check accuracy
fitted_values<-fitted(lm)
length(fitted(lm))
length(train_data1$price)
pred=lm$fitted.values
actual_fitted<-data.frame(actual=train_data1$price, predicted=pred)
abs_diff = mean(abs(actual_fitted$actual-actual_fitted$predicted)/actual_fitted$actual)
accuracy=1-abs_diff
accuracy

pred_test=predict(newdata=test_data, lm)
actual_fitted_test=data.frame(actual=test_data$price, predicted=pred_test)
abs_diff_test = mean(abs(actual_fitted_test$actual-actual_fitted_test$predicted)/actual_fitted_test$actual)
accuracy=1-abs_diff_test
accuracy

#accuracy
#test < train 🤞