setwd("/Users/ayacha/Desktop/cs 584 - ass 3")

data<-read.table("iris.data.txt", sep=',')

dim(data)

data = cbind(   8/(1+ exp( -1*as.matrix(data[,1]) ) )  , data)
data = cbind(   4/(1+ exp( -1*as.matrix(data[,2]) ) )  , data)


test_idx <- seq(41, 60)
testingSet <- data[test_idx,]
trainingSet <- data[-test_idx,]


x = trainingSet[-7]
xtest = testingSet[-7]
y = trainingSet[7]

x = cbind(matrix(1,80,1), x)
xtest = cbind(matrix(1,20,1), xtest)

theta = c(.00101, .000141,.002201,.00201,.00101,.00201,.00101)

m = dim(x)[1]

for(j in 1:50)
{
  things = 0
  for(i in 1:80)
  {
    things = things + ( ( (1/  (1+ exp( -1*as.matrix(x[i,]) %*% as.matrix(theta) ) )   )  - (as.numeric(y[i,])-1)  )  * x[i,])
  }
  theta = as.matrix(theta) - 0.1 * t(things)
}


testYpredictions = (1/ (1+ exp( -1*as.matrix(xtest) %*% as.matrix(theta) ) ) )

#Training error
trainYpredictions = (1/ (1+ exp( -1*as.matrix(x) %*% as.matrix(theta) ) ) )

yhat <- ifelse(testYpredictions > 0.5, "Iris-setosa", "Iris-versicolor")
confusionMatrix=table(yhat, testingSet$V5)





                          
                                  