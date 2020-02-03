# jeff borrowed this code from here:
# https://rdrr.io/bioc/SamSPECTRAL/src/R/kneepointDetection.R

y = c(18,9,4,2,1.8,1.7,1.6,1.5,1.4,1.3)
x <- 1:length(y)
plot(x,y)

abline(v = 3)



for (i in 4:(length(y) - 4)) {
  # i <- 4
  print(y[i+1] + y[i-1] - 2 * y[i])
  #tmp <- (y[i+3] - y[i-3])/5
  #print(tmp)
  #if ((tmp < -50) & (tmp > -40))
  #  print(paste(i, tmp))
}

y <- sort(c(40, 187, 1873, 9363, 24344, 18726, 8716, 10985, 5040, 5390, 
                       4060, 2830, 3187, 2407, 2856, 2002, 1593, 1200, 1689, 1026, 911, 
                       937, 1296, 2519, 1310, 932, 600, 754, 505, 606, 392, 491, 433, 
                       448, 343, 347, 245, 379, 193, 434, 173, 448, 358, 347, 343, 367, 
                       376, 189, 423, 322, 309, 287, 244, 253, 166, 355, 328, 220, 230, 
                       169, 357, 396, 115, 151, 179, 142, 166, 185, 151, 136, 229, 179, 
                       111, 366, 319, 183, 99, 141, 109, 159, 154, 129, 110, 167, 117, 
                       108, 111, 172, 136, 243, 226, 174, 161, 136, 102, 192, 132, 98, 
                       121, 110, 127, 138, 131, 156, 154, 152, 116, 101, 473, 271, 142, 
                       100, 188, 219, 139, 143, 148, 294, 221, 194), decreasing = T)

kneepointDetection(y, TRUE)

kneepointDetection <- function(vect, PlotFlag=FALSE){
  # OrthagonalResiduals=FALSE
  #Finds the change point using piece wise linear regressions. See the Rd document for details. 
  n<-length(vect);
  Vect<-vect;
  
  
  
  a<-as.data.frame(cbind(1:n, Vect[1:n]));
  l<-lm(a[,2]~a[,1], data=a);
  MinError=100000000;
  MinIndex=1;
  for (i in 2:(n-2)){
    #first line
    # We fix the first line to be always horizontal (Y=1).
    a<-as.data.frame(cbind(1:i, Vect[1:i]))
    l1<-lm(a[,2]~a[,1], data=a)
    #error 1
    e1<-sum(abs(1-a[,2]))
    
    #second line  
    a<-as.data.frame(cbind((i+1):n, Vect[(i+1):n]))
    l<-lm(a[,2]~a[,1], data=a)
    l2<-l
    #error 2
    e2<-sum(abs(l$residuals))
    Error=e1+e2;
    if (MinError>Error){
      MinError=Error;
      MinIndex=i;
    }
    
    if (PlotFlag){
      
      a<-as.data.frame(cbind(1:length(Vect), Vect))
      plot(a, xlim=c(0,n), ylim=c(0,max(Vect)), axes=FALSE, xlab='Iteration', ylab='Distance');
      par(new=TRUE);
      plot(a[MinIndex,], pch=19, col='green', xlim=c(0,n), ylim=c(0,max(Vect)), axes=FALSE, xlab='Iteration', ylab='Distance')
      par(new=TRUE);
      plot(a[i,], pch=19, col='red', xlim=c(0,n), ylim=c(0,max(Vect)), axes=FALSE, xlab='Iteration', ylab='Distance')
      axis(1);axis(2);
      #title(sprintf('SSD: %.3f',Error))
      title(sub='Red: Current Change Point       Green: Best Change Point');
      abline(l1)
      abline(l2)
     
    }
  }
  if (PlotFlag){
    system('convert -delay 40 tmpfigs/* animation.gif')
  }
  
  i<-MinIndex
  a<-as.data.frame(cbind(1:i, Vect[1:i]))
  l1<-lm(a[,2]~a[,1], data=a)
  Error=mean(abs(l$residuals));
  a<-as.data.frame(cbind((i+1):n, Vect[(i+1):n]))
  l2<-lm(a[,2]~a[,1], data=a)
  a<-as.data.frame(cbind(1:n, Vect))
  
  return(list(MinIndex=MinIndex, l1=l1 ,l2=l2));
}# End kneepointDetection <-function(vect, PlotFlag=FALSE)
