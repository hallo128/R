#自动推算129

##########读入样本数据
setwd("F:/R/sample/估计/模拟数据")
data_all=read.csv('530600补抽22.csv')    #---------
p_data=ncol(data_all)

###########每次选取指标放入最后一列
#data_all[,p_data+1]=data_all$从业人员期末人数    
idx=read.table('指标.txt')
#s='从业人员期末人数';ss='从业人员期末人数' ;year=100
s=as.character(idx[,1][18])                   #------指标i[,1][i]

#----------------------------分开指标与年份
library(stringr)
s1=str_trim(s)   #去除前后空格
year=as.numeric(str_extract_all(s1,'\\d{1,4}'))  #年份year
d1=str_split(s1,pattern = paste0(as.character(year),'年'))[[1]][2]
ss=str_split(d1,pattern ='\\.人\\.')[[1]][1]            #指标ss


for(i in 1:p_data){
  if (names(data_all)[i]==s)  {data_all[,p_data+1]=data_all[,i]}
}


#####选取不重复
choose_only=function(d){
  index=duplicated(d)
  d=d[!index]             #不重复的编号
  d=d[!(is.na(d))]
}

#地州大的编号
shi=data_all$行政区划代码_地州
shi=choose_only(shi)  
#区号
qu=data_all$行政区划代码_县
qu=choose_only(qu)
length(qu)



        #代码对应的市
for(i in 1:length(shi)){
  shi_dm=shi[i]
  
  shi_quhao=choose_only(data_all[which(data_all$行政区划代码_地州==shi_dm),]$行政区划代码_县)
  data_shi=data_all[which(data_all$行政区划代码_地州==shi_dm),]    #每个市的数据
  setwd("F:/R/sample/估计")
  #分区
  data_Yst=data.frame()
  YB=0
  for(i in shi_quhao){
    quhao= i
    data=data_shi[which(data_shi$行政区划代码_县== i),]
    
    #分密集与非
    source('F:/R/sample/估计/分密集与非.R', encoding = 'UTF-8')
    
    #分层与层估计信息
    source('F:/R/sample/估计/分层.R', encoding = 'UTF-8')
    
    #区一级  #层data_yh(hat_y,s_hat_y)
    Yst=sum(data_yh$hat_y)
    s_Yst=sum(data_yh$s_hat_y)
    
    #-------------均值估计
    Nqu=sum(Nh)
    y=Yst/Nqu   #均值
    s_y=s_Yst/(Nqu*Nqu)
    #----------
    
    if(Yst==YB)
      data_l=data.frame('地区代码'=quhao,'年份'=year,'指标'=ss,'总量'=0,'总量的方差'=0,'总量的标准差'=0,'均值'=y,'均值的方差'=s_y,'均值的标准差'=sqrt(s_y))       #全部是0表示出现了错误，与上一次完全相同
    else
      data_l=data.frame('地区代码'=quhao,'年份'=year,'指标'=ss,'总量'=Yst,'总量的方差'=s_Yst,'总量的标准差'=sqrt(s_Yst),'均值'=y,'均值的方差'=s_y,'均值的标准差'=sqrt(s_y))
    
    #市里的所有区信息
    data_Yst=rbind(data_Yst,data_l)
    
    YB=Yst          #保留上一个时刻的值，比较是否进行了更新
  }
  data_Yst
  
  ######输出每个区对应的估计值
  setwd("F:/R/sample/估计/估计值")
  write.table(data_Yst, file = paste0('区估计值','.txt'),
              append = T, sep = ",",col.names = F,row.names=F)    #逗号分隔
  
}


###进行市的估计
source('F:/R/sample/估计/市信息.R', encoding = 'UTF-8')

