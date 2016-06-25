#数据约规（针对data1）

#----------------分辨率
library(stringr)
ll=data1$分辨率
ll1=str_trim(ll)   #去除前后空格

ll2=str_extract_all(ll1,'\\d{1,4}')
a=NULL
for(i in 1:length(ll2)){
  #a[i]=paste(ll2[[i]][1],ll2[[i]][2],sep='x')
  a[i]=as.numeric(ll2[[i]][1])*as.numeric(ll2[[i]][2])
}                     #a=更新的分辨率格式
#a1=as.factor(a)       #转换为因子
#data1$分辨率<-a1
data1$分辨率<-a       #转换数值型

#----------------上市年份
ll=data1$上市年份
ll1=str_trim(ll)   #去除前后空格

ll2=str_extract_all(ll1,'\\d{1,4}')
a=NULL
for(i in 1:length(ll2)){
  a[i]=ll2[[i]][1]
}                     #a=更新的上市年份格式
a1=as.numeric(a)
data1$上市年份<-a1

#----------------机身尺寸
ll=data1$机身尺寸.mm.
ll1=str_trim(ll)   #去除前后空格

ll2=str_extract_all(ll1,'\\d{1,3}\\.?\\d{0,3}' )    #只保留数值
a=NULL
for(i in 1:length(ll2)){
  num=c(as.numeric(ll2[[i]][1]),as.numeric(ll2[[i]][2]),as.numeric(ll2[[i]][3]))
  a[i]=num[1]*num[2]*num[3]
}                     #a=更新的分辨率格式
data1$机身尺寸.mm.<-a      #添加了新的变量


#----------------机身内存
ll=data1$机身内存
ll1=str_trim(ll)   #去除前后空格

ll2=str_extract_all(ll1,'\\d{1,3}')
a=NULL
for(i in 1:length(ll2)){
  a[i]=ll2[[i]][1]
}                     
a1=as.numeric(a)
data1$机身内存<-a1

#----------------运行内存
ll=data1$运行内存
ll1=str_trim(ll)   #去除前后空格

ll2=str_extract_all(ll1,'\\d{1,3}')
a=NULL
for(i in 1:length(ll2)){
  a[i]=ll2[[i]][1]
}                     
a1=as.numeric(a)
data1$运行内存<-a1

#----------------屏幕尺寸
ll=data1$屏幕尺寸
ll1=str_trim(ll)   #去除前后空格

ll2=str_extract_all(ll1,'\\d.\\d')
a=NULL
for(i in 1:length(ll2)){
  a[i]=ll2[[i]][1]
}                     
a1=as.numeric(a)
data1$屏幕尺寸<-a1

#----------------后置摄像头
ll=data1$后置摄像头
ll1=str_trim(ll)   #去除前后空格

ll2=str_extract_all(ll1,'\\d{1,4}')
a=NULL
for(i in 1:length(ll2)){
  a[i]=ll2[[i]][1]
}                     
a1=as.numeric(a)
data1$后置摄像头<-a1

#----------------前置摄像头
ll=data1$前置摄像头
ll1=str_trim(ll)   #去除前后空格

ll2=str_extract_all(ll1,'\\d{1,4}')
a=NULL
for(i in 1:length(ll2)){
  a[i]=ll2[[i]][1]
}                     
a1=as.numeric(a)
data1$前置摄像头<-a1


#----------------电池容量
ll=data1$电池容量.mAh.
ll1=str_trim(ll)   #去除前后空格

ll2=str_extract(ll1,'\\d{1,4}')
a=NULL
for(i in 1:length(ll2)){
  a[i]=ll2[[i]][1]
}                     
a1=as.numeric(a)
data1$电池容量.mAh.<-a1

#----------------机身重量
ll=data1$机身重量.g.
ll1=str_trim(ll)   #去除前后空格

ll2=str_extract(ll1,'\\d{1,3}')
a=NULL
for(i in 1:length(ll2)){
  a[i]=ll2[[i]][1]
}                     
a1=as.numeric(a)
data1$机身重量.g.<-a1



#-------------CPU品牌
ll=data1$CPU品牌
ll1=str_trim(ll)   #去除前后空格
a=NULL
for(i in 1:length(ll1)){
  if(str_detect(ll1[i],pattern = 'Qualcomm|骁龙')){a[i]='骁龙'}
  else if (str_detect(ll1[i],pattern = '高通')){a[i]='高通'}
  else if (str_detect(ll1[i],pattern = '苹果')){a[i]='苹果'}
  else if (str_detect(ll1[i],pattern = '三星')){a[i]='三星'}
  else {a[i]='其他'}
} 
a1=as.factor(a)       #转换为因子
data1$CPU品牌<-a1



#-------------品牌
ll=data1$品牌
ll1=str_trim(ll)   #去除前后空格
#l2=ll1
pp=c('三星','华为','魅族','Apple','锤子','OPPO','vivo','小米')
for(i in 1:length(pp)){
  ll1[str_detect(ll1,pattern = pp[i])]<-pp[i]
}
a1=as.factor(ll1)       #转换为因子
data1$品牌<-a1
#ll1[str_detect(ll1,pattern = '锤子')]<-'锤子'


#---------添加系统类型
data1$'系统类型'='安卓'
for(i in 1:nrow(data1)){
  if(data1$品牌[i]=='Apple') data1$'系统类型'[i]='ios'
}
data1$'系统类型'=as.factor(data1$'系统类型')

#---------价格（改为数值型）
data1$价格=as.numeric(as.character(data1$价格))

#---------评价数（改为数值型）
data1$评价数=as.numeric(as.character(data1$评价数))

