
#Define Treshold function for multiple classes and multiple 
# variables
findThreshold <- function(boundary,X_data,Y_data,depth, minPoints, costFnc) {
    
    indices<-rep(TRUE,nrow(X_data))
    
    varNames<-colnames(X_data)
    
    for (var in varNames){
        ind<-(X_data[,var]>=boundary[1,var] & X_data[,var]<=boundary[2,var])
    indices<- indices & ind
    }
    
    X<-X_data[indices,] 
    Y<-Y_data[indices]
    
    if (length(Y)>minPoints) {
    #keeping track of best cut by dimention
    thresholdDim<-rep(NA,length(varNames))
    errorDim<-rep(NA,length(varNames))
    
    #looping through all variables
    for (var in varNames){
        x<-sort(unique(X[,var]))
        nIter <- length(x)-1
        errors <- rep(NA, nIter)
        thresholds <- rep(NA, nIter)
        #splitLabels <- matrix(NA, ncol=2, nrow=nIter)
        
        #loop through all posiible cut points in the current dim
        
        for (i in 1:(nIter)) {
            
            # locate a potential threshold, a split between two points
            potThres <- mean(x[i:(i+1)])
            
            # define purity 
            
            left <-(X[,var]<=potThres)
            right <-(X[,var]>=potThres)
            
            purity_left <- costFnc(as.vector(table(Y[left]))/length(Y[left])) 
            purity_right <- costFnc(as.vector(table(Y[right]))/length(Y[right])) 
            errors[i]<-purity_left + purity_right
            thresholds[i]<-potThres
        }
        #define the best posiible cut for each variable
    errorDim[var]<-min(errors)
    thresholdDim[var]<-thresholds[which.min(errors)]
    }

    bestThres<-thresholdDim[which.min(errorDim)]
    #which is this variable
    bestVar<-names(bestThres)
    
    #calculate the purity in each bucket 
    left_best <-(X[,bestVar]<=bestThres)
    right_best <-(X[,bestVar]>=bestThres)
    
    purity_left_best <- costFnc(as.vector(table(Y[left_best]))/length(Y[left_best])) 
    purity_right_best <- costFnc(as.vector(table(Y[right_best]))/length(Y[right_best])) 
    
    #we redefine the boundaries
    #create two new buckets to
    #replace the already existing one
    boundary1<-boundary
    boundary1[2,bestVar]<-bestThres
    boundary2<-boundary
    boundary2[1,bestVar]<-bestThres
    
    return(list(boundary1,boundary2,purity_left_best,purity_right_best))
    } else { 
        purity_old <- costFnc(as.vector(table(Y))/length(Y))
        return(list(boundary, boundary, purity_old, purity_old))
    }
}

#Define entropy functions 
#NOTE: they are DIFFERENT than 
#the ones provided in class 
ME <- function(prob) {
    MissError <- 1 - max(prob)
    return(MissError) 
}

Gini <- function(prob) {
    Gini <- sum(prob*(1-prob))
    return(Gini)
}

Entropy <- function(prob) {
    CrossEntropy <- - sum(prob*log(prob))
    return(CrossEntropy)
}


### Here is the cTree function
cTree <- function(formula, data, depth, minPoints, costFnc=Entropy) {
    
    #Packages
    if (!require("assertthat")) install.packages("assertthat"); library(assertthat)
    if (!require("formula.tools")) install.packages("formula.tools"); library(formula.tools)
    if (!require("plyr")) install.packages("plyr"); library(plyr)
    
    #check inputs 
    assert_that(class(formula)=="formula")
    not_empty(names(data))
    assert_that(is.data.frame(data))
    
    
    # redefine data
    X.names <- get.vars(rhs(formula))
    Y.names <- get.vars(lhs(formula))
    dim <- length(X.names)
    X_data <- data[,X.names]
    Y_data <- data[,Y.names] #number of elements in list = number of nodes

    
     
    #define the first purity and boundary on the whole data set
    purity<-costFnc(as.vector(table(Y_data))/length(Y_data))
    #purity <- lapply(Y_data, FUN=function(x) costFnc(as.vector(table(x))/length(x)) )
    boundaries <- list(sapply(X_data, FUN=function(x) c( min(x), max(x) ) ) )  #creates a list of one element (a matrix)
    
    
    # define recursive function
    recursive  <- function(purity, boundaries, X_data,Y_data,depth, minPoints, costFnc) {
        #define current debth length = current number of nodes
        
        BoundariesList <- lapply(boundaries, FUN= function(x) findThreshold(x,X_data,Y_data,depth, minPoints, costFnc))
        print(BoundariesList)
       
        #define purity decrease measure
        purity_decrease<-rep(NA,length(boundaries))
        for (i in 1:length(boundaries)){
            #compute the decrease for each node
            purity_decrease[i]<- purity[i] - ( BoundariesList[[i]][[3]] +  BoundariesList[[i]][[4]] )
        }
        
        #compute the max decrease change
        best_node<- which.max(purity_decrease) 
        
        #take the relevant boundaries and purities
        new_boundaries<-BoundariesList[[best_node]][1:2]
        new_purity<-unlist(BoundariesList[[best_node]][3:4])
        print("new_boundaries")
        print(new_boundaries)
        print("new_purity")
        print(new_purity)
        #replace the unnecessary features with the ones from the new nodes 
        if (length(boundaries)==1){
            boundaries<-(new_boundaries)
            purity<-(new_purity)
        }else{
            boundaries<-c(new_boundaries,boundaries[-best_node])
            purity<-c(new_purity, purity[-best_node])    
        }
        print("B")
        print(boundaries)
        print("P")
        print(purity)
        
        #check the new number of nodes
        no_nodes<-length(boundaries)
        #define termination stage
        if ( no_nodes==depth ) return(list(boundaries=boundaries, purity=purity))
        
        #recursive stage if the is-statement is false
        print(".")
        T<- recursive(purity,boundaries, X_data, Y_data,depth, minPoints, costFnc)
        
        
        return(T)
    }
    
    results <- recursive(purity, boundaries, X_data, Y_data,depth, minPoints, costFnc)
    final_boundaries <- results[[1]]
    
    # initialise final dataframe to be returned
    #add id to the dataset to keep track of the order
    data$id <- seq(1,nrow(data))
    
    data_final <- data[FALSE,]
    data_final$predLabel <- integer()
    data_final$prob <- double()
    
    
    #fill up final df, bucket at a time
    for (element in 1:length(final_boundaries)) {
        
        indices_final<-rep(TRUE,nrow(X_data))
        boundary<-final_boundaries[[element]]
        
        for (var in X.names){
            ind<-(data[,var]>= boundary[1,var] & data[,var]<=boundary[2,var])
            indices_final<- indices_final & ind
        }
        
        new_data <-data[indices_final,]
        print(head(new_data,2))
         
        # add predicted label and prob to X
        Y_new_data<-unlist(new_data[,Y.names])
        print(head(Y_new_data))
        table_prob<-(table(Y_new_data))/length(Y_new_data)
     
        new_data$prob <- max(as.vector(table_prob))
        
        new_data$predLabel <- as.integer(names(table_prob[table_prob==max(as.vector(table_prob))]))
        
        
        data_final <- rbind(data_final, new_data)
    }
    
    data_final<-data_final[order(data_final$id),]
    
    return(list(prob=data_final$prob,predLabels=data_final$predLabel,boundaries=final_boundaries))
    
    
    
}

        
        
        
        
        
        
        
        
        
        
        
        
        
       