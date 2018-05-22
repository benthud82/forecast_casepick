 data = read.csv("rstattest.csv", header = TRUE)
 set.seed(500)
 index <- sample(1:nrow(data),round(0.75*nrow(data)))
 train <- data[index,]
 test <- data[-index,]
 lm.fit <- glm(BOXES~., data=train)
 pr.lm <- predict(lm.fit,test)
 MSE.lm <- sum((pr.lm - test$BOXES)^2)/nrow(test)
 maxs <- apply(data, 2, max)
 mins <- apply(data, 2, min)
 scaled <- as.data.frame(scale(data, center = mins, scale = maxs - mins))
 train_ <- scaled[index,]
 test_ <- scaled[-index,]
 n <- names(train_)
 f <- as.formula(paste("BOXES ~", paste(n[!n %in% "BOXES"], collapse = " + ")))
 nn <- neuralnet(f,data=train_,hidden=c(2),linear.output=F)
 pr.nn <- compute(nn,test_[,1:10])
 pr.nn_ <- pr.nn$net.result*(max(data$BOXES)-min(data$BOXES))+min(data$BOXES)
 test.r <- (test_$BOXES)*(max(data$BOXES)-min(data$BOXES))+min(data$BOXES)
 MSE.nn <- sum((test.r - pr.nn_)^2)/nrow(test_)
 print(paste(MSE.lm,MSE.nn))
 plot(test$BOXES,pr.nn_,col='red',main='Real vs predicted NN',pch=18,cex=0.7)
 abline(0,1,lwd=2)
 plot(test$BOXES,pr.lm,col='blue',main='Real vs predicted lm',pch=18, cex=0.7)
 abline(0,1,lwd=2)

//To Predict future outcomes
 preddata = read.csv("preddata.csv", header = TRUE)
 maxs <- apply(preddata, 2, max)
 mins <- apply(preddata, 2, min)
 scaled <- as.data.frame(scale(preddata, center = mins, scale = maxs - mins))
 pr.nn <- compute(nn,scaled[,1:10])
 pr.nn_ <- pr.nn$net.result*(max(data)-min(data))+min(data)
 pr.nn_