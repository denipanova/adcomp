
#setwd("/home/didi/BGSE/semester2/adcomp/week4")
train<-read.csv("MNIST_training.csv", header = FALSE)
#setwd("/home/didi/BGSE/semester2/adcomp/adcomp")
source("kNNNoDist.R")
source("DistanceMatrix.R")
source("kNN.R")

if (!require("class")) install.packages("class");
library("class")
#Take random small sample in order to validate
s_indeces<-sample.int(6000, size = 1000)
s<- train[s_indeces,]

#split the data into two
tr<-sample.int(1000, size=300)
val<-setdiff(1:1000,tr)
training_set<- s[tr,]
training_x<- training_set[,-1]
training_y<- training_set[,1]

validation_set<- s[val,]
validation_x<-validation_set[,-1]
validation_y<-validation_set[,1]

kMeasures<- c(1,3,5,7,9,11,15,17,23,25,35,45,55,83,101,151 )
distMeasure<-c(1,2,3)


accuracyMatrix<-matrix(NA,length(kMeasures), length(distMeasure))

for (j in 1:length(distMeasure)){ 
 distMatrix<-DistanceMatrix(validation_x,training_x, p=distMeasure[j])
 for (i in 1:length(kMeasures)){
   prediction <- kNNNoDist(distMatrix, training_y, k=kMeasures[i])
   accuracyMatrix[i,j] <- mean(prediction$predLabels == validation_y)
 }  
    
}
#optimal is k=1, p=2

test<-read.csv("MNIST_test.csv", header = FALSE)
memory<- train[,-1]
labels<- train[,1]

mat<-DistanceMatrix()
predictions<-kNN(features=test,labels = labels, train_set = memory, k=1, p=2, type = "predict")

mnistlab<-as.vector(predictions$predLabels)

write.csv(mnistlab,"MNIST_predictions.csv", row.names = FALSE)

