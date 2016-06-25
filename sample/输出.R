#输出

#source('F:/R/sample/确定分层.R', encoding = 'UTF-8')
#num_level=select_num_level(dd)
level_all=data.frame('区号'=quhao,
                     '输出层数'=num_level,
                     '样本量'=sample_tol[num_level],
                     '总量'=tolnum ,
                     '抽样比'=rito[num_level])
###分层的信息
setwd("F:/R/sample/层信息11")
write.table(level_all, file = paste0('输出层数信息11','.txt'),
            append = T,col.names = F)
##分层具体信息函数
qu_xx=function(h_h){
  h1_breakpoint=c(yy1[h_h,1:h_h-1],max(xx1$zu))
  h0_breakpoint=c(yy0[h_h,1:h_h-1],max(xx0$zu))
  hhh=c(1:h_h,c(1:h_h)+h_h)
  h_breakpoint=c(h1_breakpoint,h0_breakpoint)#人数分位点
  Nhhh=c(numh1[h_h,1:h_h],numh0[h_h,1:h_h])  #每层大小
  Wh=c(ww1[h_h,1:h_h],ww0[h_h,1:h_h])  #每层大小
  qu_xx=data.frame('区号'=quhao,
                   '层数'=hhh,
                   '人数分位点'=h_breakpoint,
                   '每层大小_Nh'=Nhhh,
                   '每层权重_Wh'=Wh)
  qu_xx
}  
xxx_cen=qu_xx(num_level)
setwd("F:/R/sample/层信息11")
write.csv(xxx_cen, file = paste0(quhao,'.csv'),row.names =F)
num_level


##运行程序
  ##确定分层数
affirm_h=num_level
if(affirm_h>1){
  source('F:/R/sample/抽取样本.R', encoding = 'UTF-8')
  #sample_qu  #抽取的全部样本
}

if(affirm_h==1){
  sample_qu=data
}

#写入保存
setwd("F:/R/sample/sample_choose11")
write.csv(sample_qu,file =paste0(quhao,'.csv'),row.names=F)     
#####-----


setwd("F:/R/sample/层信息11")
n_ppp=data.frame('区号'=quhao,
                 '样本量'=n_affirmh,
                 '备用样本量'=n_reserve,
                 '全抽的总量'=n_all,
                 '总量'=tolnum)
write.table(n_ppp, file = paste0('样本数11','.txt'),
            append = T,col.names = F,row.names = F)

setwd("F:/R/sample")
