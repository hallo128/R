#jd数据预处理

setwd("F:/python/京东数据爬虫")

data=read.csv('手机数据.csv')


#---------------删除不完整行
p=16
n=nrow(data)
n_rm=NULL
if(ncol(data)!=16){
  for(i in 1:n){
    if (!(is.na(data[i,17]))) n_rm=append(n_rm,i)
    if (is.na(data[i,16])) n_rm=append(n_rm,i)
  }
}

data1=data[-n_rm,][,1:p]  #统一为16个属性
data3=data1    #保留初步读入的数据


#数据约规
source('F:/python/京东数据爬虫/数据约规.R', encoding = 'UTF-8')



#--------------剔除缺失
#检查没有缺失的个数
sum(complete.cases(data1))

#------缺失数据
data_NA=data1[!complete.cases(data1),]   #缺失-机身内存
#data_NA=rbind(data_NA,data1[3,])        #缺失-分辨率
#--机身内存，用同等价位的手机填补，或者用运行内存与机身内存的关系填补


#----完整的数据
data2=data1[complete.cases(data1),]#[-3,]     


#---重命名
names(data2)<-c("id","brand","version","year","CPU","ROM","RAM",
                "screen_size","pixel","b_pixel","f_pixel","b_capacity",
                "volume","weigth","price","evaluate","sys")
data_use=data2[c("sys","CPU","ROM","RAM","pixel","b_pixel","f_pixel","b_capacity",
                 "evaluate","year","brand",
                 "screen_size","volume","weigth",
                 "price")]
attach(data_use)     #实际进行运算的数据

#------------去重复


#d1=data_use[,-9]    #去除评价数
#s=F                    #对单条
#for(i in 1:ncol(d1)){
#  if(d1[1,][i]!=d1[2,][i]) break
#  s=T
#}




