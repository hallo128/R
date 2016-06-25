#练习hare

library(TSA)
data("hare")


#图-----确定选择ar(2)
par(mfrow=c(3,1))
plot(hare)  
acf(hare)
pacf(hare)
par(mfrow=c(1,1))

#------估计AR
#已确定模型
out1.ar=ar(hare,aic=F,order.max =2,method = 'yw')#ar(2)
#自动识别AR阶数
out2.ar=ar(hare,method = 'mle')  #极大似然
p=out2.ar$order
p
out3.ar=ar(hare,method = 'ols')  #最小二乘
p=out3.ar$order
p


#------估计ARIMA
#极大似然有AIC值
out5.ar=arima(hare,order=c(2,0,0),method = 'ML')#极大似然
out6.ar=arima(hare,order=c(6,0,0),method = 'ML')#极大似然


#模型拟合值
f6=fitted.Arima(out6.ar)  #p=6
f2=fitted.Arima(out5.ar)  #p=2
plot(hare)
lines(f2,col='red')
lines(f6,col='blue')



#极大似然与条件最小二乘
out=arima(hare,order=c(2,0,0),method = 'CSS-ML')
out



#-----------------------参数估计
############确定阶数
m1=ar(gnp,method = 'mle')           #AR模型
p=m1$order                          #用AIC确定阶数p
m2=arima(gnp,order =c(p,0,0))       #ARIMA模型(p,d,q)
m2

#m2$coef   #前p个系数
x=1
for(i in 1:p){
  x=x-m2$coef[i]
}
c=x*m2$coef[p+1]                    #常数项

sqrt(m2$sigma2)                     #标准差

