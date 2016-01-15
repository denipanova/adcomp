

#necessary packages
if (!require("ggplot2")) install.packages("ggplot2")
library(ggplot2)
if (!require("mvtnorm")) install.packages("mtvnorm")
library(mvtnorm)


#create sigma matrix
sigmaXY <- function(rho, sdX, sdY) {
  covTerm <- rho * sdX * sdY
  VCmatrix <- matrix(c(sdX^2, covTerm, covTerm, sdY^2), 
                     2, 2, byrow = TRUE)
  return(VCmatrix)
}

#function generating multivariate random numbers, given the previous sigma matrix 
genBVN <- function(n = 1, seed = NA, muXY=c(0,1), sigmaXY=diag(2)) {
  if(!is.na(seed)) set.seed(seed)
  rdraws <- rmvnorm(n, mean = muXY, sigma = sigmaXY)
  return(rdraws)
}


# create a function to generate loan data for Accepted,Denied,Undecided 
loanData <- function(noApproved, noDenied, noUndecided, muApproved, muDenied, muUndecided, sdApproved, 
                        sdDenied, sdUndecided, rhoApproved, rhoDenied, rhoUndecided, seed=1111) {
  
  sigmaApproved <- sigmaXY(rho=rhoApproved, sdX=sdApproved[1], sdY=sdApproved[2])
  sigmaDenied <- sigmaXY(rho=rhoDenied, sdX=sdDenied[1], sdY=sdDenied[2])
  sigmaUndecided <- sigmaXY(rho=rhoUndecided, sdX=sdUndecided[1], sdY=sdUndecided[2])
  
  approved <- genBVN(noApproved, muApproved, sigmaApproved, seed = seed)
  denied <- genBVN(noDenied, muDenied, sigmaDenied, seed = seed+1)
  undecided <- genBVN(noUndecided, muUndecided, sigmaUndecided, seed = seed+2)
  
    
  loanDf <- as.data.frame(rbind(approved,denied,undecided))
  deny <- c(rep("Approved", noApproved), rep("Denied", noDenied), rep("Undecided", noUndecided))
  target= c(rep(0, noApproved), rep(1, noDenied), rep(2,noUndecided))
  loanDf <- data.frame(loanDf, deny, target)
  colnames(loanDf) <- c("PIratio", "solvency", "deny", "target")
  return(loanDf)
}

#Synthetic Data
noApproved <- 50; noDenied <- 50; noUndecided <- 50
loanDf <- loanData(noApproved, noDenied, noUndecided, c(4, 180), c(13, 80) , c(7, 130), 
                   c(2,20), c(2,30), c(2,25), -0.5, 0.3, 0.05 )

#add dummy variables to the data frame 
loanDf <- cbind(loanDf, 
                target1 = c(rep(1, noApproved), rep(0, noDenied),rep(0,noUndecided)),
                target2 = c(rep(0, noApproved), rep(1, noDenied), rep(0,noUndecided)),
                target3 = c(rep(0, noApproved), rep(0, noDenied), rep(1,noUndecided))) 


# create X and Y in order to solve W 
X <- as.matrix(cbind(ind=rep(1, nrow(loanDf)), 
                     loanDf[,c("PIratio", "solvency")])) #add intercept
Y <- as.matrix(loanDf[,c("target1","target2","target3")]) #only the dummy variables 
w <- solve(t(X)%*%X) %*% t(X) %*% Y

# compute predictions
predictions <- X %*% w
colnames(predictions)<-c("ProbOfApproved","ProbOfDenied","ProbOfUndecided") # for clarity change the name of the columns


# classify according to the argmax criterion
approved <- (predictions==apply(predictions, 1, max))[,1]
denied <- (predictions==apply(predictions, 1, max))[,2]

predictedLabels <- ifelse(denied, "Denied", ifelse(approved,"Approved","Undecided"))

predictedData<- cbind(loanDf,predictions,predictedLabels)

#export to csv file 
write.csv(predictedData,file = "predictions.csv")

#Create the decision boundaries

#line1 A vs D
int1<- (w[1,2]-w[1,1])/(w[2,1]-w[2,2])
sl1<- (w[3,2]-w[3,1])/(w[2,1]-w[2,2])

#line2 A vs U
int2<- (w[1,3]-w[1,1])/(w[2,1]-w[2,3])
sl2<- (w[3,3]-w[3,1])/(w[2,1]-w[2,3])
#line D vs U
int3<- (w[1,2]-w[1,3])/(w[2,3]-w[2,2])
sl3<- (w[3,2]-w[3,3])/(w[2,3]-w[2,2])

#constructing x and y for each line 
x <- seq(min(loanDf["solvency"]), max(loanDf["solvency"]), 
         length.out = nrow(loanDf))
y1 <- int1+sl1*x
y2 <- int2+sl2*x
y3 <- int3+sl3*x

#Creating the half-lines/planes 
xCheck1<-as.matrix(cbind(1,y1,x)) #because we have solvency as the first variable
predictionCheck1<-xCheck1%*%w
blank1<-x
blank1[predictionCheck1[,1]<= predictionCheck1[,3]] <-NA #here the first and second column are equal,so we check with the 3rd one

xCheck2<-as.matrix(cbind(1,y2,x)) 
predictionCheck2<-xCheck2%*%w
blank2<-x
blank2[predictionCheck2[,1]<= predictionCheck2[,2]] <-NA

xCheck3<-as.matrix(cbind(1,y3,x)) 
predictionCheck3<-xCheck3%*%w
blank3<-x
blank3[predictionCheck3[,2]<= predictionCheck3[,1]] <-NA

# constructing the boundaty lines as functions

boundaryDf1 <- data.frame(PIratio=y1, solvency=blank1, 
                         deny=rep("Boundary1", length(x)))

boundaryDf2 <- data.frame(PIratio=y2, solvency=blank2, 
                          deny=rep("Boundary2", length(x)))
boundaryDf3 <- data.frame(PIratio=y3, solvency=blank3, 
                          deny=rep("Boundary3", length(x)))
plotDiscFnc <- function() {
  ggplot(data = loanDf, 
         aes(x = solvency, y = PIratio, colour=deny)) + 
    geom_point() +
    xlab("solvency") +
    ylab("PI ratio") +
    geom_line(data=boundaryDf1) + geom_line(data=boundaryDf2)+ geom_line(data=boundaryDf3) +
    scale_color_manual("Target", 
                       values = c("Boundary1" = "violet", "Boundary2" = "black","Boundary3" = "green", 
                                  "Approved" = "blue", "Denied" = "red","Undecided"="orange")) +
    
   ggsave("discFunction3C.pdf") #save the graph 
}

plotDiscFnc()













