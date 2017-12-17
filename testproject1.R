library(neuralnet)
library(caret)
library(forecast)
library(TTR)
library(quantmod)

google <- read.csv("raw.csv")

any(is.na(google))
dim(google)
names(google)
boxplot(google)

google <- google[complete.cases(google), ]

#transformation

#input
EMA15 <- EMA(x = google$Close, n = 15)
google$newEMA15 <- google$Close - EMA15
google$RDP5 <- Delt(google$Close, k = 5) * 100
google$RDP10 <- Delt(google$Close, k = 10) * 100
google$RDP15 <- Delt(google$Close, k = 15) * 100
google$RDP20 <- Delt(google$Close, k = 20) *100
google <- google[complete.cases(google), ]

#output
google$EMAi3 <- EMA(x = google$Close, n = 3)
google$EMAi5 <- EMA(x = google$Tclose.5, n = 3)

google <- google[complete.cases(google), ]

google$RDPout5 <- Delt(x1 = google$EMAi3, x2 = google$EMAi5) * 100




google1 <- google[complete.cases(google),]
any(is.na(google1))
dim(google1)



#train and test without scale
g_train <- google1[1:144, ]
g_test <- google1[145:204, ]


#algorithm to normalize the data
normalize <- function(x){
  return((x - min(x)) / (max(x) - min(x)))
} 

gnorm <- subset(google1, select = -Date)#we cant normalize the date so we need to exclude it
head(gnorm)

google1_norm <- as.data.frame(lapply(gnorm, normalize))
summary(google1_norm$Delt.0.arithmetic)

#dividing the data into training and testing set
google1_train <- google1_norm[1:144, ]
google1_test <- google1_norm[145:204, ]

names(google1_norm)

#fitting the model using neural network


google1_model <- neuralnet(Delt.0.arithmetic ~ newEMA15 +
                             Delt.5.arithmetic +
                            Delt.10.arithmetic + Delt.15.arithmetic +
                            Delt.20.arithmetic, 
                          data = google1_train, hidden = c(1,10),
                          learningrate = 0.005, algorithm = 'rprop+')

model_results <- compute(google1_model, google1_test[19:23])

#denormalizing the data
predict_testNN = (model_results$net.result * 
                    (max(google1$Delt.0.arithmetic) - 
                       min(google1$Delt.0.arithmetic))) + 
                              min(google1$Delt.0.arithmetic)

plot(google1_test$Delt.0.arithmetic, model_results$net.result,col='blue', pch=16, 
     ylab = "predicted price NN", xlab = "real price")
abline(0,1, col = "magenta")

predicted_strength <- model_results$net.result
cor(predicted_strength, google1_test$Delt.0.arithmetic)

d2 <- (1/60-1)*sum(model_results$net.result - mean(google1_test$Delt.0.arithmetic))
nmse <- sum((model_results$net.result - google1_test$Delt.0.arithmetic)^2)/(60*d2)





