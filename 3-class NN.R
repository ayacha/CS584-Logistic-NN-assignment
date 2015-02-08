setwd("/Users/ayacha/Desktop/cs 584 - ass 3")

data<-read.table("iris.data 2.txt", sep=',')

dim(data)

test_idx <- seq(41, 60)
test_idx = c(test_idx, seq(91, 110))
testingSet <- data[test_idx,]
trainingSet <- data[-test_idx,]


x = trainingSet[-5]
xtest = testingSet[-5]
y = trainingSet[5]

x = cbind(matrix(1,110,1), x)
xtest = cbind(matrix(1,40,1), xtest)

v1 = as.matrix(c(.00101, .000141,.002201) )

v2 = as.matrix(c(.00101, .000141,.002201) )


w1 = as.matrix(c(.00101, .000141,.002201,.00201,.00101) )

w2 = as.matrix(c(.00101, .000141,.002201,.00201,.00101) )


z = function (x)
{
  result = 1
  result = rbind (result, 1+ exp( -1*t(w1) %*% x))  
  result = rbind (result, 1+ exp( -1*t(w2) %*% x))
  return (result)  
}



m = dim(x)[1]

for(j in 1:50)
{
  things = as.matrix(c(0,0,0))
  for(i in 1:110)
  {
    things = things +   as.numeric( ( t(v1) %*% z(t(x[i,])) ) -  (as.numeric(y[i,])-1) ) * z(t(x[i,]) )  
  }
  v1 = v1 - 0.1 * things
  
  things = as.matrix(c(0,0,0))
  for(i in 1:110)
  {
    things = things +   as.numeric( ( t(v2) %*% z(t(x[i,])) ) -  (as.numeric(y[i,])-1) ) * z(t(x[i,]) )  
  }
  v2 = v2 - 0.1 * things
  
  
  things = as.matrix(c(0,0,0,0,0))

  for(i in 1:110)
  {
    things = as.matrix(things) +   as.numeric( ( t(v1) %*% z(t(x[i,])) ) -  (as.numeric(y[i,])-1) ) * v1[1]* z(t(x[i,]) )[1] * (1-z(t(x[i,]) )[1])  *x[i,]    
    things = as.matrix(things) +   as.numeric( ( t(v2) %*% z(t(x[i,])) ) -  (as.numeric(y[i,])-1) ) * v2[1]* z(t(x[i,]) )[1] * (1-z(t(x[i,]) )[1])  *x[i,]       
  }
  w1 = w1 - 0.1 * t(things)
    
  
  things = as.matrix(c(0,0,0,0,0))  
    for(i in 1:110)
    {
      things = as.matrix(things) +   as.numeric( ( t(v1) %*% z(t(x[i,])) ) -  (as.numeric(y[i,])-1) ) * v1[2]* z(t(x[i,]) )[2] * (1-z(t(x[i,]) )[2])  *x[i,]    
      things = as.matrix(things) +   as.numeric( ( t(v2) %*% z(t(x[i,])) ) -  (as.numeric(y[i,])-1) ) * v2[2]* z(t(x[i,]) )[2] * (1-z(t(x[i,]) )[2])  *x[i,]       
      
      
    }
  w2 = w2 - 0.1 * t(things)
  
  
}



testYpredictions1 = t(v1) %*% z(t(xtest))

#Training error
#trainYpredictions = (1/ (1+ exp( -1*as.matrix(x) %*% as.matrix(theta) ) ) )

yhat <- ifelse(testYpredictions1 > 0.5, "Iris-setosa", "Iris-versicolor")
confusionMatrix=table(yhat, testingSet$V5)


