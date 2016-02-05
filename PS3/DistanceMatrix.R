DistanceMatrix <- function(features, train_set, p = 2){
  
  # test the inputs
  if (!require("assertthat")) install.packages("assertthat");
  library(assertthat);
  if (!require("plyr")) install.packages("plyr");
  library(plyr);
  not_empty(features); 
  #check that labels are integers 
  is.numeric(features)
  assert_that(p %in% c(1, 2, 3))
  assert_that(not_empty(train_set) & 
                  ncol(train_set) == ncol(features))
  
  
  
  # Compute the distance between each point and all others 
  noObs <- nrow(features)
  noVars <- ncol(features)
  
  #distance matrix
  notrain_set <- nrow(train_set) 
  distMatrix <- matrix(NA, noObs, notrain_set)
  for (obs in 1:noObs) {
    
    # getting the probe for the current observation
    probe <- as.numeric(features[obs,])
    probeExpanded <- matrix(probe, nrow = notrain_set, ncol = ncol(features), byrow = TRUE)
    
    # computing distances between the probe and exemplars in the train_set
    if (p %in% c(1,2)) {
      distMatrix[obs, ] <- (rowSums((abs(train_set - probeExpanded))^p) )^(1/p)
    } else if (p==3) {
      distMatrix[obs, ] <- apply(abs(train_set - probeExpanded), 1, max)
    }  
  }
return(distMatrix)
}

  