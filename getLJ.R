library(XML)

#getrootNode 抓取静态网址并为xpath抓取数据做准备
getrootNode=function(URL){
  html_bd <- htmlParse(URL,encoding="UTF-8")
  rootNode_bd <- xmlRoot(html_bd)
  rootNode_bd
} 

#GetTpage 抓取当前自如网上海租房信息总页数，并返回每一页的URL
GetTpage=function(URL){
  root1<-getrootNode(URL)
  Tpage<-xpathSApply(root1,"//div[@class='pages']/span[2]",xmlValue)
  Tpage<-as.numeric(substr(Tpage,2,nchar(Tpage)-1))
  pageurl<-paste0("http://sh.ziroom.com/z/nl/?p=",1:Tpage)
  pageurl
}

#GetNumurl 抓取出租房url
GetNumurl=function(rootnode){
  root<-getNodeSet(rootnode,"//li[@class='clearfix']/div/h3/a")

  detailurl <-c()
  for(j in 1:length(root)){
    detailurl[j]<-paste0("http://sh.ziroom.com/",xmlGetAttr(root[[j]],'href'))
  }
 
  detailurl<-as.data.frame(detailurl,stringsAsFactors=FALSE)
  colnames(detailurl)=c('url')
  
  detailurl
}

#GetHouse用于获取房屋信息
GetHouse=function(rootnode,url){
  infor<-data.frame(matrix(NA,1,19))
  colnames(infor)=c('url','region','subregion','community','title','subtitle','price','punit','label','area',
                    'orientation','housetype','leasetype','floor','transportation','housenum','housesurround',
                    'housetraffic','houseallocation')
  infor$url<-url
  
  area<-xpathSApply(rootnode,"//div[@class='node_infor area']/a",xmlValue)[-1] 
  
  infor$region<-substr(area[1],1,nchar(area[1])-2)
  infor$leasetype<-substr(area[1],nchar(area[1])-1,nchar(area[1]))
  infor$subregion<-substr(area[2],1,nchar(area[2])-4)
  infor$community<-substr(area[3],1,nchar(area[3])-4)
  
  
  pass<-xpathSApply(rootnode,"//div[@class='room_name']/h2",xmlValue)
  infor$title<-gsub("([\n ])", "", pass)
  
  pass<-xpathSApply(rootnode,"//div[@class='room_name']/p",xmlValue)
  pass<-unlist(strsplit(gsub("([\n ])", "", pass),"￥"))
  infor$subtitle<-pass[1]
  infor$price<-as.numeric(substr(pass[2],1,nchar(pass[2])-4))
  infor$punit<-substr(pass[2],nchar(pass[2])-2,nchar(pass[2])-1)
  
  pass<-xpathSApply(rootnode,"//p[@class='room_tags clearfix']",xmlValue)
  infor$label<-gsub("([\n])", ",", gsub("([\t ])", "", pass))

  pass<-xpathSApply(rootnode,"//ul[@class='detail_room']/li[1]",xmlValue)
  infor$area<-as.numeric(gsub(" ", "", substr(pass,4,nchar(pass)-1)))

  pass<-xpathSApply(rootnode,"//ul[@class='detail_room']/li[2]",xmlValue)
  infor$orientation<-gsub(" ", "", substr(pass,4,nchar(pass)))

  pass<-xpathSApply(rootnode,"//ul[@class='detail_room']/li[3]",xmlValue)
  pass<-strsplit(pass,"\n")[[1]][1]
  infor$housetype<-gsub(" ", "", substr(pass,4,nchar(pass)))

  pass<-xpathSApply(rootnode,"//ul[@class='detail_room']/li[4]",xmlValue)
  infor$floor<-gsub(" ", "", substr(pass,4,nchar(pass)))

  pass<-xpathSApply(rootnode,"//ul[@class='detail_room']/li[5]/span",xmlValue)
  if(length(pass)>0){infor$交通<-gsub("\n",",",gsub("([\t ])", "", pass))}
  
  pass<-xpathSApply(rootnode,"//h3[@class='fb']",xmlValue)
  infor$housenum<-substr(gsub("([\n ])", "",pass),4,nchar(pass))
  
  pass<-xpathSApply(rootnode,"//div[@class='aboutRoom gray-6']/p[1]",xmlValue)
  infor$housesurround<-gsub("\n","",substr(pass,4,nchar(pass)))

  pass<-xpathSApply(rootnode,"//div[@class='aboutRoom gray-6']/p[2]",xmlValue)
  infor$housetraffic<-gsub("\n","",substr(pass,4,nchar(pass)))

  a<-getNodeSet(rootnode,"//ul[@class='configuration clearfix']/li")

  for (j in 1:9){
    b<-xmlGetAttr(a[[j]],"class")
    if(j==1){
      pass<-as.character(length(grep("have",b)))
    }else{
      pass<-paste0(pass,as.character(length(grep("have",b))))
    }
  }
  infor$houseallocation<-pass
  
  infor
}

#SaveData用于存储数据,type用于判断是否需要列名
SaveData=function(NumURL,name,type=F){
  name<-paste0(name,".txt")
  write.table(NumURL,file=name,sep=";",append=T,row.names = F,col.names=type,quote=F)
}

#GetData用于提取数据
GetData=function(name){
  name<-paste0(name,".txt")
  read.table(name,header=T,sep=";",stringsAsFactors = F)
}



#############################################################
######主程序1：提取各出租房页面URL
############################################################

setwd("...\\租房信息\\data")

homeURL="http://sh.ziroom.com/z/nl/"#自如网综合页URL
pageurl<-GetTpage(homeURL)

time<-gsub("[_ :]","",Sys.time())
name1<-paste0("homeURL ",time)
name2<-paste0("houseDetail ",time)

nn=length(pageurl)
for (i in 1:nn){
  rootnode<-getrootNode(pageurl[i])
  NumURL<-GetNumurl(rootnode)
  if(i==1){
    SaveData(NumURL,name1,type = T)
  }else{
    SaveData(NumURL,name1)
  }
  #cat(i,"\n")
}

#############################################################
######主程序2：提取每个出租房详细信息
############################################################
NumURL<-GetData(name1)
NumURL<-unique(NumURL)

nn<-nrow(NumURL)
for (i in 1:nn){
  rootnode<-getrootNode(NumURL[i,])
  housedata<-GetHouse(rootnode,NumURL[i,])
  if(i==1){
    SaveData(housedata,name2,type = T)
  }else{
    SaveData(housedata,name2)
  }
}


