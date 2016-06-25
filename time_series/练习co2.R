#练习co2

library(TSA)         #与season()函数,tempdub有关
data(co2)
plot(co2)
acf(co2)

####################1求长期趋势Tt
#季节平均模型(计算长期趋势)
time=time(co2)       #提取模型的时间
model=lm(co2~time)
#长期趋势
Tt=ts(fitted(model),start = c(1994,1),freq=12) 
plot(Tt)


#---------------残差建模
#2求季节趋势St
Month=season(co2)                   #提取季节因素
#加法模型
model1=lm(residuals(model)~Month-1)  #-1不再有截距项
St=ts(fitted(model1),start =c(1994,1),freq=12)
Tas=Tt+St                        #确定性趋势

plot(Tas)
###########随机部分et
et=co2-Tas

#原始数据与确定趋势图
par(mfrow=c(3,1))
plot(co2,main='原始数据图')
plot(Tas,main='拟合趋势图')
plot(et,main='随机残差图')
par(mfrow=c(1,1))


#---------------对建模后的残差进行白噪声检验
########画图 
par(mfrow=c(2,1))
pacf(et)                #偏自相关函数 
acf(et)
par(mfrow=c(1,1))
############

#检验残差的序列自相关acf(H0:不相关)
Box.test(et,lag=12,type ='Ljung' )   #序列相关

#单位根检验
m2.3=ur.df(et,type = c("trend"), lags = 1)#线性函数
summary(m2.3)            #拒绝，无单位根（平稳）


#AR(2)拟合残差


