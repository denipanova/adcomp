#*********************  
# ADABOOST FUNCTION 
# for prediction
#*********************

## Extend the ADABOOST function so that it 
## applies the alreadt learned classifiers to test data set 

predictionAdaBoost <- function(trees, alpha,data,noTrees) {
  ## Packages
  if (!require("assertthat")) install.packages("assertthat"); 
  library(assertthat)
  if (!require("rpart")) install.packages("rpart"); 
  library(rpart)
  
  ## Assert that the given attributes are correct
  assert_that(is.data.frame(data))
  
  ## Apply all of the above weak classifiers to the data 
  G_data<- as.data.frame(matrix(NA,nrow=nrow(data),ncol=noTrees))
  for (i in 1:noTrees){
    G_data[,i]<-predict(trees[[i]], data, type="class")
  }
  ## Compute the final classifier
  L<- levels(G_data[,1])
  #create matrix for each observation having the two labels
  prediction<-matrix(NA, nrow(data), length(L))
  colnames(prediction)<-L
  
  for (i in L){
    prediction[,i]<- rowSums( (G_data == i) %*% alpha[1:noTrees] )
  }
  
  predLabels<- apply(prediction,1, function(x) L[which.max(x)])
  
  return(list(predLabels=as.numeric(predLabels)))
  
}  

