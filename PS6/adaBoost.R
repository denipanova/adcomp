#*********************  
# ADABOOST FUNCTION
#*********************

#Inputs: formula, data (train data), depth (integer), number of trees (integer), test set
#Outputs: predicted labels of data points (list) to the training data 




adaBoost<-function(formula,data,depth,noTrees){
    
    ## required packages
    if (!require("assertthat")) install.packages("assertthat"); 
    library(assertthat)
    if (!require("formula.tools")) install.packages("formula.tools"); 
    library(formula.tools)
    if (!require("rpart")) install.packages("rpart"); 
    library(rpart)
    
    ## assert that the inputs are in the correct format
    assert_that(class(formula)=="formula") 
    not_empty(data)
    assert_that(is.data.frame(data))
    assert_that(is.count(depth))
    assert_that(is.count(noTrees))
    
    ## redefine data, using formula package
    X_names <- get.vars(formula,data)[-1]
    Y_names <- get.vars(formula,data)[1]
    #save it as factors
    data[,Y_names]<-factor(data[,Y_names])
    
    #number of observations
    n<-nrow(data)
    
    #specify empty vectors 
    W <-rep(1/n,n)
    G<-as.data.frame(matrix(NA,n,noTrees))
    alpha <- rep(NA,noTrees)
    # to save the trees we grow,
    # we initialize an empty list
    trees <- list()
    
    weakG<-function(formula,data,W, depth){
      #redefine the environment in order to find the W vector 
      environment(formula) <- environment()
      rpart(formula=formula, 
            data=data, 
            weights = W, 
            control = rpart.control(maxdepth=depth))
      
      }
    
  
    #start the algorithm 
    for(i in 1:noTrees) {
      
      #initialize with weak classifier
      trees[[i]]<-weakG(formula,data,W,depth)
      
      #use it to predict the first one
      #use class type in order to predict factor
      G[,i]<-predict(trees[[i]], data,  type="class" )
      
      #calculate misclassification ocuurances
      wrong <- (data[,Y_names] != G[,i])
      err <- sum(wrong*W) / sum(W)
      
      alpha[i] <- log((1-err) / err)

      #redefine w 
      W <- W * exp(alpha[i]*wrong)
    
    }
   
    ## Compute the final classifier 
    
    L<- levels(data[,Y_names])
    #create matrix for each observation having the two labels 
    prediction<- matrix(NA,n,length(L))
    colnames(prediction)<-L
    #calculate majority vote for both labels 
    for (i in L){
      prediction[,i]<-((G == L) %*% alpha)
    }
    #Take the maximim 
    predLabels = apply(G, MARGIN = 1, 
                       FUN = function(x) L[which.max(x)])
    
    return(list(predLabels=as.numeric(predLabels),trees=trees,alpha=alpha))

    
}


    
      