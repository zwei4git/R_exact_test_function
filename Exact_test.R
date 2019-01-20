
library(combinat)

############### function
exact_test <- function(x, mu, g1){
  require(combinat)
  b<-permn(length(x))
  c<-lapply(b, `[`,1:g1)
  d<-unique(lapply(unique(c),sort))
  
  for(i in 1:length(d)){
    e[i]<-mean(x[d[[i]]])-mean(x[-d[[i]]])
  }
  
  f<-c("  number of combinations"=as.integer(length(e)),
       "  number of extreme values"=as.integer(length(e[abs(e)>= abs(mu)])),
       "P"=length(e[abs(e)>=abs(mu)])/length(e))
  return(f)
}



#exapmle:  mean of first 4 is -9, 2 groups so number in g1=4
t<-c(12,10,6,3,17,20,19,11)
exact_test(t,-9,4)









