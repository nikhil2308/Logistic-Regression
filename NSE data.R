# ** OBJECTIVE ** 
#1. Import the csv file with past 3 months history for infy share trend
scrip.data <- read.csv("C:/Users/nikhil/OneDrive/imarticus/data/idea_90.csv")

#Same as above, created new column for High - Low
scrip.data$Movement <- scrip.data$Open.Price - scrip.data$Close.Price
scrip.data$Up_Down <- ifelse(scrip.data$Movement > 0,1,0)

#*********Model 1*****************

train_subset<- scrip.data[c(4:7,17)]

model1 <- glm(train_subset$Up_Down ~ ., family = binomial(link = 'logit'), data = train_subset[1:4])

summary(model1)

anova(model1, test = 'Chisq')

log_predict1 <- predict(model1,newdata = train_subset[1:4], type = "response")
log_predict1 <- ifelse(log_predict1 > 0.5,1,0)
log_predict1
scrip.data$Up_Down_Pred1_.5 <- log_predict1

#*********Model 2*****************
train_subset<- scrip.data[c(4,6,7,17)]

model2 <- glm(train_subset$Up_Down ~ ., family = binomial(link = 'logit'), data = train_subset[1:3])

summary(model2)

anova(model2, test = 'Chisq')

log_predict2 <- predict(model2,newdata = train_subset[1:3], type = "response")
log_predict2 <- ifelse(log_predict2 > 0.5,1,0)
log_predict2

scrip.data$Up_Down_Pred2_.5 <- log_predict2

scrip.data$Up_Down_Pred2_.6 <- ifelse(log_predict2 > 0.6,1,0)


#*********Model 3*****************
train_subset<- scrip.data[c(4,6,17)]

model3 <- glm(train_subset$Up_Down ~ ., family = binomial(link = 'logit'), data = train_subset[1:2])

summary(model3)

anova(model3, test = 'Chisq')

log_predict3 <- predict(model3,newdata = train_subset[1:3], type = "response")
log_predict3 <- ifelse(log_predict3 > 0.5,1,0)
log_predict3
scrip.data$Up_Down_Pred3_.5 <- log_predict3

#COMPARE ALL 3 MODELS
anova(model1, model2, model3, test = 'Chisq')





