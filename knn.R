data <- read.csv("k_example_data.txt",stringsAsFactors = TRUE)
data <- na.omit(data)

#use only regression
data1 <- data[-c(1, 2, 3, 6, 12, 16, 18)] 
label <- data$violent_crime_rate

#normalize vectors
normalize <- function(x){
  return((x - min(x))/(max(x) - min(x)))
}
data_n <- as.data.frame(lapply(data1, normalize))

#train-test split
train <- sample(1:nrow(data_n), nrow(data_n)/2)
train.labels <- label[train]
test.labels <- label[-train]

library(class)
#run a series of k values
i=1
k.optm=1
for (i in 1:50){
  data_test_pred <- knn(train = data_n[train, ], test = data_n[-train, ], cl = train.labels, k = i)
  tab <- cbind(data_test_pred, test.labels)
  k.optm[i] <- sqrt(mean((tab[,1] - tab[,2])^2))
  k=i
  cat(k,'=',k.optm[i],'')
}
#Accuracy plot
plot(k.optm, type="b", xlab="K- Value",ylab="RMSE")

#use k=sqrt(n) in training set
k = sqrt(length(train))
data_test_pred <- knn(train = data_n[train, ], test = data_n[-train, ], cl = train.labels, k = k)
RMSE <- sqrt(mean((tab[,1] - tab[,2])^2))
tab <- cbind(data_test_pred, test.labels)
tab