#electricity_季节趋势

library(TSA)
library(urca)

data(electricity)

par(mfrow=c(1,2))
plot(electricity)
acf(as.vector(electricity),lag.max=36)   #查看平稳性-非平稳
par(mfrow=c(1,1))

####差分除去趋势，确定d的阶数
ele1=diff(electricity)

par(mfrow=c(1,2))
plot(ele1) 
acf(as.vector(ele1),lag.max=36)          #季节趋势
par(mfrow=c(1,1))
#pacf(as.vector(ele1),lag.max=36)

####消除季节波动 确定D的阶数
ele2=diff(diff(electricity),lag=12)

par(mfrow=c(1,2))
plot(ele2) 
acf(as.vector(ele2),lag.max=36)          #截尾
par(mfrow=c(1,1))
#pacf(as.vector(ele2),lag.max=36)


#平稳检验-ele2
m1=ur.df(ele2,type='none')
summary(m1)    #无单位根



####模型识别
par(mfrow=c(2,1))
acf(as.vector(ele2),lag.max=36)      #截尾(2,1)
pacf(as.vector(ele2),lag.max=36)     #拖尾的
par(mfrow=c(1,1))

####模型估计
m1.ele2=arima(electricity,order=c(0,1,3),seasonal=list(order=c(0,1,1),period=12))
m1.ele2

m2.ele2=arima(electricity,order=c(0,1,2),seasonal=list(order=c(0,1,1),period=12))
m2.ele2

m2=arima(electricity,order=c(1,1,1),seasonal=list(order=c(0,1,1),period=12))
m2

###模型诊断
plot(m1.ele2$resid)
acf(as.vector(m1.ele2$resid),lag.max=72)   #白噪声
pacf(as.vector(m1.ele2$resid),lag.max=72)   #白噪声
#平稳检验
Box.test(m1.ele2$resid)   
#Box.test图
Box_test_plot=function(m){
  B=NULL
  for(i in 1:20){
    B=c(B,Box.test(m$residuals,lag=i,type = 'Ljung-Box')$p.value)
  }
  plot(B)
  abline(h=.05,lty=2)
}
Box_test_plot(m2.ele2)

qqnorm(m1.ele2$resid)           #正态性
qqline(m1.ele2$resid)


####预测
win.graph(width=4.875, height=3,pointsize=8)
plot(electricity)
par(mfrow=c(2,1))
plot(m1.ele2,n1=c(1973,1),n.ahead=24,col='red',xlab='Year',type='o')
plot(m1.ele2,n1=c(2000,1),n.ahead=24,col='red',xlab='Year',type='o')


#predict(m1.ele2,n1=c(2003,1),n.ahead=24)  #预测值


