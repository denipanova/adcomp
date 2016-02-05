kNNNoDist <- function(distMatrix, labels, k){
  
  # test the inputs
  if (!require("assertthat")) install.packages("assertthat");
  library(assertthat);
  if (!require("plyr")) install.packages("plyr");
  library(plyr);
  not_empty(labels); 
  are_equal(nrow(distMatrix),length(labels));
  #check that labels are integers 
  is.numeric(distMatrix)
  is.count(k); 
 
  #define
  noObs <- nrow(distMatrix)

  
  # Sort the distances in increasing numerical order and pick the first 
  # k elements
  neighbors <- apply(distMatrix, 1, order) #1-apply to rows  
  
  # Compute and return the most frequent class in the k nearest neighbors
  prob <- rep(NA, noObs)
  predictedClasses<- rep(NA,noObs)
  for (obs in 1:noObs) {
    freq <- plyr::count(labels[neighbors[1:k,obs]])
    maxfreq<- max(freq$freq)
    lab<- freq[maxfreq==freq$freq,"x"]
    predictedClasses[obs]<-lab[sample(length(lab),1)] #sample(5,1)
    #take at rondom one of the biggest mode 
    prob[obs]<- (maxfreq/k)*100
  }
  

  
  # return the results
  return(list(predLabels = predictedClasses, 
              prob = prob))
  
}