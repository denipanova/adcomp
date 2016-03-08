## packages 
if (!require("gbm")) install.packages("gbm"); library(gbm)
if (!require("dplyr")) install.packages("dplyr");library(dplyr)
if (!require("reshape2")) install.packages("reshape2"); library(reshape2)
if (!require("ggplot2")) install.packages("ggplot2");library(ggplot2)


## load the data

data <- read.csv("/home/didi/BGSE/semester2/adcomp/adcomp/spambase.data", 
                 header=FALSE)

## source the function 
#source("adaBoost.R")
#source("predictionAdaBoost.R")

## split the data train and test data
set.seed(1234)
test_inx<-sample.int(nrow(data),0.4*nrow(data))
test<-data[test_inx,]
train<-data[-test_inx,]

#specify the other coefficients
#formula<-V58 ~ V1 + V2 + V57
depth <- 5
#noTrees<-5


# DO adaBoost with the package 
noIterations <- 200
boost <-gbm(V58~.,
            distribution = "adaboost",
            data = train,
            n.trees = 200,
            interaction.depth = depth,
            shrinkage = 1,
            bag.fraction = 1)

## extracting training and test errors for the GBM adaboost

  
GBMtrainError <- GBMtestError <- rep(NA, noIterations)
for (i in 1:noIterations) {
  GBMtrainError[i] <- mean((predict(boost, train, n.trees = i) > 0) != train$V58)
  GBMtestError[i] <- mean((predict(boost, test, n.trees = i) > 0) != test$V58)
}

## extracting training and test errors for my adaboost

MYtrainError <- MYtestError <- rep(NA, noIterations)
# initialize the function
MYboost<-adaBoost(V58~.,train,depth,200)

for (i in 1:noIterations) {
  MYtrainError[i] <- mean( predictionAdaBoost(MYboost$trees,
                                              MYboost$alpha,
                                              train,
                                              i)$predLabels != train$V58 )
  MYtestError[i]  <- mean( predictionAdaBoost(MYboost$trees,
                                              MYboost$alpha,
                                              test,
                                              i)$predLabels != test$V58 )
}



# combining errors in a data frame
errorsDF <- data.frame(Iterations = 1:noIterations,
                     TrainGBM = GBMtrainError,
                     TestGBM = GBMtestError,
                     TrainMY = MYtrainError,
                     TestMY = MYtestError) %>%
  melt(id.vars = "Iterations")

# plotting everything

plotBoost <-
  ggplot(data = errorsDF,
         aes(x = Iterations, y = value, color = variable)) +
  geom_line() +
  scale_y_continuous( "Misclassification error") +
  #theme_bw()+
  ggtitle("Test and Train errors for GBM and MYAdaBoost functions")

ggsave("adaBoost.pdf",plotBoost)
