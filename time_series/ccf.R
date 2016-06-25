#ccf

library(TSA)
library(urca)

data(electricity)
data("milk")
#time(milk)
electri=window(electricity,start=c(1994,1))

me.dif=ts.intersect(diff(diff(milk,12)),diff(diff(log(electri),12)))
prewhiten(as.numeric(me.dif[,1]),as.numeric(me.dif[,2]))



