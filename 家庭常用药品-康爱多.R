#抓取康爱多-家庭常备药
library(XML)

#getrootNode 抓取静态网址并为xpath抓取数据做准备
getrootNode=function(URL){
  html_bd <- htmlParse(URL,encoding="UTF-8")
  rootNode_bd <- xmlRoot(html_bd)
  rootNode_bd
} 

#按页抓取各药品细节页面地址
for (j in 1:25){
  #生成对应页码url
  URL <- paste0("http://www.360kad.com/Category_122/Index.aspx?page=",j)
  
  #提取页面信息
  root <- getrootNode(URL)
  
  #获取药品信息html
  druglist <- getNodeSet(root,"//p[@class='Ypic']/a")
  
  #初始化数据集
  drugurl <- data.frame(matrix(NA,length(druglist),1))
  colnames(drugurl)<-c("drugurl")
  
  #提取网址
  for (i in 1:length(druglist)){
    drugurl$drugurl[i] <- paste0("http://www.360kad.com",xmlGetAttr(druglist[[i]],'href'))
  }
  
  #整理结果
  if(j==1){drug<-drugurl}else{drug<-rbind(drug,drugurl)}
  
  #监控进度
  cat(j,"\n")
}
rm(drugurl)
rm(druglist)
#write.csv(drug,".../基本药/druglist.csv")
#drug <- read.csv(".../基本药/druglist.csv",stringsAsFactors =F)

#提取每个药品的信息
for(i in 1:nrow(drug)){
  #抓取网页信息
  root <- getrootNode(drug$drugurl[i])
  
  #提取药品名称
  drug$name[i]<-xpathSApply(root,"//div[@class='prem-proname clearfix']/h1",xmlValue)
  
  #提取通用名/批文/企业
  a <- xpathSApply(root,"//div[@class='dtl-inf-top']/div/div[@class='dtl-inf-r']",xmlValue)
  drug$通用名[i] <- a[1]
  drug$批准文号[i] <- a[2]
  drug$生产企业[i] <- a[3]
  
  #进度监控
  cat(i,"\n")
  
  #系统休眠
  if(i%%10==0){
    cat("sleeping...\n")
    Sys.sleep(3)}
}

#write.csv(drug,".../基本药/drug.csv")
