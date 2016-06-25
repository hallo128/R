
###读入数据（只能一条数据，可以事前处理，也可以事后处理�?
#1
#data=read.csv("./时间序列/hj.csv",header=F)[,1]

#2
data1=read.csv("./时间序列/177.csv",header=F)
data=NULL
for(i in 1:length(data1)){
  data=c(data,data1[,i])
}
data                                  #变为一个向量数�?


#########
#时间序列的数据尽量不要出现缺失，否则在之后的计算也会算入缺失�?
#对向量data进行缺失值处�?
rm_na=function(data){
  num_na=sum(is.na(data))           #判断是否有缺失值，如果有，则剔�?
  if(num_na!=0){
    wh=which(is.na(data))  #缺失所在行
    wh1=NULL               #缺失行的排序颠�?
    for(i in 1:num_na){
      wh1[i]=wh[num_na-i+1]
    }
    for(i in 1:num_na) {
      data=data[-wh1[i]]       #删除缺失�?
    }
  }
  data
}

data=rm_na(data)
###########



#改变为时间序列数�?
hj=ts(data,start = c(1980,1),frequency = 12)  


####################1求长期趋势Tt
library(TSA)          #与season()函数有关

#季节平均模型(计算长期趋势)
time=time(hj)       #提取模型的时间，要求数据位序列类型数�?
model=lm(hj~time)
#plot(hj,type='o')    #画出趋势�?
#abline(model,col='blue')


#长期趋势
Tt=ts(fitted(model),start = c(1980,1),freq=12) 


####################2求季节趋势St
Month=season(hj)                   #提取季节因素
#加法模型
model1=lm(residuals(model)~Month-1)  #-1不再有截距项
St=ts(fitted(model1),start = c(1980,1),freq=12)
Tas=Tt+St


####################2求季节趋势St
#乘法模型
newhj=hj/fitted(model)
model2=lm(newhj~Month-1)  #-1不再有截距项
St1=ts(fitted(model2),start = c(1980,1),freq=12)
Tas1=Tt*St1


###################画图
#加法
plot(hj,type='o')           #画出真实趋势�?
#points(Tas,type='o',col='red')
lines(Tas,col='red')
title('真实值与加法模型的趋势比�?')
legend("topleft",lwd = 1,legend = c('row','add'),col=c('black','red'))

#乘法
plot(hj,type='o')           #画出真实趋势�?
#points(Tas1,type='o',col='blue')
lines(Tas1,col='blue')
title('真实值与乘法模型的趋势比�?')
legend("topleft",lwd = 1,legend = c('row','mult'),col=c('black','blue'))

#全部
plot(hj,type='o')           #画出真实趋势�?
lines(Tas1,col='blue')
lines(Tas,col='red')
title('真实值与加法、乘法模型的趋势比较')
legend("topleft",lwd = 1,legend = c('row','add','mult'),col=c('black','red','blue'))


s1=summary(model)
s1$coef[,1]
s2=summary(model1)
s2$coef[,1]
s3=summary(model2)
s3$coef[,1]

#sin���⻬(�ӷ�)
perfun=harmonic(hj,1)
model3=lm(residuals(model)~perfun)
St=ts(fitted(model3),start = c(1980,1),freq=12)
Tas=Tt+St

