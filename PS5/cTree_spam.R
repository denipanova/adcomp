

dataset <- read.csv("spambase.data", header=FALSE)

data <- dataset[c(1:100, 4501:4601),]
formula <- V58 ~ V1+V2+V3+V10+V56
depth <- 5
minPoints <-1   

ctr_new<-cTree(formula,dataset,depth,minPoints)
