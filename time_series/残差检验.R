setwd("d:/")

###读入数据（只能一条数据，可以事前处理，也可以事后处理）
data=read.csv("hj.csv",header=F)[,1]


#改变为时间序列数据
hj=ts(data,start = c(1980,1),frequency = 12)  


####################1求长期趋势Tt
library(TSA)          #与season()函数有关

#季节平均模型(计算长期趋势)
time=time(hj)       #提取模型的时间，要求数据位序列类型数据
model=lm(hj~time)
#plot(hj,type='o')    #画出趋势图
#abline(model,col='blue')

#长期趋势
Tt=ts(fitted(model),start = c(1980,1),freq=12) 


####################2求季节趋势St
Month=season(hj)                   #提取季节因素
####加法模型
model1=lm(residuals(model)~Month-1)  #-1不再有截距项
St=ts(fitted(model1),start = c(1980,1),freq=12)
Tas=Tt+St
#残差
St_res1=model1$residuals

#平稳性检验
plot(St_res1,type='o')

#正态分布
#1QQ图
qqnorm(St_res1)  
qqline(St_res1)
#2(H0：正态)
shapiro.test(St_res1)
#3(H0：正态)
jarque.bera.test(St_res1)
#4直方图
hist(St_res1)
#5箱型图
boxplot(St_res1)

#########独立性检验
###游程检验(变量必须为因子)————随机性检验
#H0:独立
runs.test(factor(sign(St_res1)))

###########相关性检验
###1样本自相关函数（H0:rho(k)=0）
acf(St_res1)
#接受域，k阶无相关性。拒绝域，k阶有相关性
###2相关性(H0:不相关)
Box.test(St_res1,lag=3,type='Ljung-Box')#前3个残差
Box.test(St_res1,lag=3,type='Box-Pierce')#前3个残差










####################2求季节趋势St
#乘法模型
newhj=hj/fitted(model)
model2=lm(newhj~Month-1)  #-1不再有截距项
St1=ts(fitted(model2),start = c(1980,1),freq=12)
Tas1=Tt*St1
#残差
St_res2=model2$residuals


#全部
plot(hj,type='o')           #画出真实趋势图




s1=summary(model)
s1$coef[,1]
s2=summary(model1)
s2$coef[,1]
s3=summary(model2)
s3$coef[,1]









