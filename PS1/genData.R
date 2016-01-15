
N = 2000;
degrees = 570;
location = 90;
blend = 0.2;
  



genData<-function(N,degrees,location,blend,saveData=TRUE, savePlot=TRUE){
  
  # Generate two-spiral data 
  # idea of the problematic dataset: http://www.benmargolis.com/compsci/ai/two_spirals_problem.htm
  # N - number of observations
  # degrees - length of the spiral 
  # location - how far away from the origin
  # blend<-blending together

  #necessary packages
  if (!require("ggplot2")) install.packages("ggplot2")
  library(ggplot2)
  if (!require("extrafont")) install.packages("extrafont")
  library(extrafont)
  
    
  
  #define some variables
  degrees2rad<-(2*pi)/360 #convert degrees to radiant
  location<- location*degrees2rad #how far away from 00 the spiral starts
  
  N1<- floor(N/2)
  N2<- N-N1
  
  #spiral 1 
  #we indicate it by 0 in V3
  n<- as.vector(location+sqrt(runif(N1))*degrees*degrees2rad)
  d1<- t(rbind(-1*n*cos(n)+runif(N1)*blend, sin(n)*n+runif(N1)*blend, rep(0,N1)))
  
  #the second spiral we indicate by 1 in V3
  n<- as.vector(location+sqrt(runif(N1))*degrees*degrees2rad)
  d2<-t(rbind(n*cos(n)+runif(N1)*blend, -1*sin(n)*n+runif(N1)*blend, rep(1,N1))) 
  
  #combine the data 
  data<- as.data.frame(rbind(d1,d2))
  
  #create pdf
  if(saveData) {
    write.csv(data, "dataset.csv")
  }
  #create pdf plot
  
    ggplot(data = data, aes(x = V1, y = V2, 
                                   colour=V3)) + 
    scale_colour_continuous(guide = FALSE) +
    geom_point() +
    ggtitle("Synthetic data set which behaves in spiral manner") +
    xlab("x") +
    ylab("y") 
   if(savePlot) {
    ggsave("dataPlot.pdf")
    }  
 
return(data)
}

