# ----
# Hrvoje's plain 2D XOR 
# ----

genXOR <- function(noObs=50, seed=1111, saveData=TRUE, savePlot=TRUE) {
  
  # load the required libraries
  library(assertthat)
  library(mvtnorm)
  library(ggplot2)
  
  # check the inputs
  assert_that(is.scalar(noObs) && is.double(noObs))
  assert_that(is.scalar(seed) && is.double(seed))
  assert_that(is.scalar(saveData) && is.logical(saveData))
  assert_that(is.scalar(savePlot) && is.logical(savePlot))
  
  # defining a function for generating bivariate normal data
  genBVN <- function(n = 1, muXY = c(0,1), sigmaXY = diag(2)) {
    rdraws <- rmvnorm(n, mean = muXY, sigma = sigmaXY)
    return(rdraws)
  }
  
  # generate XOR data and add some simple names
  set.seed(seed)
  class1 <- rbind(genBVN(noObs, c(1,1), diag(2)),
                  genBVN(noObs, c(10,10), diag(2)) )
  class2 <- rbind(genBVN(noObs, c(1,10), diag(2)),
                  genBVN(noObs, c(10,1), diag(2)) )
  dataset <- rbind(cbind(class1, 0), cbind(class2, 1))
  dataset <- as.data.frame(dataset)
  colnames(dataset) <- c("x1", "x2", "y")
  
  # save the dataset in a CSV format
  if (saveData) {
    write.csv(dataset, file="dataset.csv", row.names = FALSE)
  }
  
  # save the plot 
  if (savePlot) {
    
    # generating the plot
    dataPlot <- 
      ggplot(data = dataset, 
             aes(x = x1, y = x2, color = factor(y))) + 
      geom_point(size = 2, shape = 4) +
      xlab("Dimension 1") +
      ylab("Dimension 2") +
      theme_bw() +
      theme(text = element_text(family = "Helvetica")) +
      scale_color_manual("Class", 
                         values = c("0" = "blue", "1" = "red"))
    
    # saving
    cairo_pdf("dataPlot.pdf")
    print(dataPlot) 
    dev.off()
  }
  
  return(dataset)
}

#Generate data with the function

dataset<-genXOR(noObs=100,seed=1111, savePlot = FALSE)

#plot the data
#   ggplot(data = dataset, 
#          aes(x = x1, y = x2, colour = factor(y))) + 
#     geom_point(size = 2, shape = 4) +
#     xlab("X1") +
#     ylab("X2") +
#     scale_color_manual("Class", 
#                        values = c("0" = "blue", "1" = "red"))

#Write the dataset in csv file

predict_5nn<-kNN(dataset[,1:2],dataset[,3],train_set = NULL, k = 5, p = 2, type='train')
pred<-cbind(dataset,predict_5nn$predLabels, predict_5nn$prob) 
colnames(pred)<-c("x1","x2","y","predLabels","prob")
write.csv(pred,"predictions.csv")

#Decision Boundaries

#define points for grid 
x1 <- seq(min(dataset$x1), max(dataset$x1), by=0.2)
x2 <- seq(min(dataset$x2), max(dataset$x2), by=0.2)
#create grid with x1 and x2
sample <- expand.grid(x1=x1, x2=x2)

# let's use the kNN function to predict 
#the sample labels
predSample <- kNN(features=sample, labels=dataset[,3], train_set=dataset[,1:2], k=1, p=2, type="predict") 
predSampleLab <- predSample$predLabels

# spread out predicted classes in the grid pattern 
prob <- matrix(predSampleLab, length(x1), length(x2))

plotFunc<-function() {
  ggplot(data=sample, aes(x=x1, y=x2, z=predSampleLab)) + 
    stat_contour(bins=1) +
    scale_alpha_continuous(guide=FALSE)+
    geom_point(aes(x=x1, y=x2, colour=as.factor(predSampleLab), alpha=0.3)) +
    geom_point(data=dataset, size=4, shape=7, aes(x=x1, y=x2, z=y, colour=as.factor(dataset$y), alpha=1))+ 
    
    labs(colour="LABELS")+
    ggtitle("DATA AND DECISION BOUNDARIES")
  ggsave("plot.pdf")
}
  
      
plotFunc()
    

