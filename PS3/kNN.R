kNN <- function(features, labels, train_set = NULL, k = 1, p = 2, type='train'){
  
  # test the inputs
  if (!require("assertthat")) install.packages("assertthat");
  library(assertthat);
  if (!require("plyr")) install.packages("plyr");
  library(plyr);
  not_empty(features); 
  not_empty(labels); 
  are_equal(nrow(features),length(labels));
  #check that labels are integers 
  is.numeric(features)
  is.count(k); 
  assert_that(p %in% c(1, 2, Inf))
  if (type == "train") {
    assert_that(nrow(features) == length(labels))
  }
  is.string(type)
  assert_that(type %in% c("train", "predict"))
  if (type == "predict") {
    assert_that(not_empty(train_set) & 
                  ncol(train_set) == ncol(features) & 
                  nrow(train_set) == length(labels))
  }
  
  
  # Compute the distance between each point and all others 
  noObs <- nrow(features)
  noVars <- ncol(features)
  
  #distance matrix
  if (type == "train") {
    distMatrix <- matrix(NA, noObs, noObs)
    for (obs in 1:noObs) {
      
      # getting the probe for the current observation
      probe <- as.numeric(features[obs,])
      probeExpanded <- matrix(probe, nrow = noObs, ncol = noVars, byrow = TRUE)
      
      # computing distances between the probe and exemplars in the
      # training X
      if (p %in% c(1,2)) {
        distMatrix[obs, ] <- (rowSums((abs(features -  probeExpanded))^p) )^(1/p)
      } else if (p==Inf) {
        distMatrix[obs, ] <- apply(abs(features - probeExpanded), 1, max)
      }  
    }
  } else if (type == "predict") {
    notrain_set <- nrow(train_set) 
    distMatrix <- matrix(NA, noObs, notrain_set)
    for (obs in 1:noObs) {
      
      # getting the probe for the current observation
      probe <- as.numeric(features[obs,])
      probeExpanded <- matrix(probe, nrow = notrain_set, ncol = ncol(features), byrow = TRUE)
      
      # computing distances between the probe and exemplars in the train_set
      if (p %in% c(1,2)) {
        distMatrix[obs, ] <- (rowSums((abs(train_set - probeExpanded))^p) )^(1/p)
      } else if (p==Inf) {
        distMatrix[obs, ] <- apply(abs(train_set - probeExpanded), 1, max)
      }  
    }
  }
  
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
  
  # examine the performance
  if (type == "train") {
    errorCount <- table(predictedClasses, labels)
    accuracy <- mean(predictedClasses == labels)
  } else if (type == "predict") {
    errorCount <- NA
    accuracy <- NA
  }
  
  # return the results
  return(list(predLabels = predictedClasses, 
              prob = prob,
              accuracy = accuracy,
              errorCount = errorCount))
 
  
}