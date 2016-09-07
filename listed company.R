library(RCurl)
library(dplyr)
library(rjson)
library(XML)

#############################抓取股票代码#############################
URL <- "http://quote.eastmoney.com/stocklist.html"
htmldetail<-getURL(URL,.encoding = "gb2312")
name <- strsplit(htmldetail,"\r\n")[[1]]
#提取页面中包含股票代码的信息
name <- name[grep("<li><a target=\\\"_blank\\\" href=\\\"http://quote.eastmoney.com/",name)]
#提取股票代码信息
n <- length(name)
total <- vector(mode = "character",length = n)
tradadd <- vector(mode = "character",length = n)
stkdata=data.frame(matrix(NA,n,6))
colnames(stkdata)=c('股票','股票代码','交易所标识','注册地址','经度','维度')
for(i in 1:n) {
  n_str <- regexpr("\\\">",name[i])
  n_end <- regexpr("</a>",name[i])
  total[i] <- substr(name[i],n_str[1]+2,n_end[1]-1)
  stkdata$交易所标识 <- substr(name[i],69,70)
}
stkdata$股票 <- substr(total,1,nchar(total)-8)
stkdata$股票代码 <- substr(total,nchar(total)-6,nchar(total)-1)
write.csv(stkdata,file="baidu_geocoding.csv")
rm(total,n_str,n_end,tradadd)
##############################抓取地址

URL <- paste0("http://f10.eastmoney.com/f10_v2/CompanySurvey.aspx?code=",stkdata$交易所标识,stkdata$股票代码,"#jbzl-0")
for (i in 1:length(URL) ){
  #Sys.sleep(runif(1,1,2))
  doc <- htmlParse(URL[i],encoding="UTF-8")
  rootNode <- xmlRoot(doc)
  tables <- readHTMLTable(doc,header=F,which=1)
  tables_df <- tbl_df(tables)
  stkdata$注册地址[i] <- as.character(filter(tables_df,V3=="注册地址")$V4)
  cat(i,"\n")
}
stkdata <- cbind(paste0(stkdata$交易所标识,stkdata$股票代码),stkdata)
colnames(stkdata)=c('unicode','股票','股票代码','交易所标识','注册地址','经度','维度')

stkdata_df <- tbl_df(stkdata)
a<-filter(stkdata_df,substr(stkdata_df$股票代码,1,3)=="600")
b<-filter(stkdata_df,substr(stkdata_df$股票代码,1,3)=="601")
c<-filter(stkdata_df,substr(stkdata_df$股票代码,1,3)=="000")
d<-filter(stkdata_df,substr(stkdata_df$股票代码,1,3)=="603")
e<-filter(stkdata_df,substr(stkdata_df$股票代码,1,3)=="002")
f<-filter(stkdata_df,substr(stkdata_df$股票代码,1,3)=="300")
stkdata_a <- rbind(a,b,c,d,e,f)
rm(a,b,c,d,e,f,)

write.csv(stkdata,file="f:/stkdataall.csv")
write.csv(stkdata_a,file="f:/stkdata_a.csv")

#############################抓取经纬度
stkdata <- read.csv("...\\test.csv")
AK <- "Your baidu AK"
add <- stkdata$RegAddr
n<-length(add)

#列表循环-begin#

for (i in 1:length(add)) {
  #建立地址转换网址
  url <- paste("http://api.map.baidu.com/geocoder/v2/?ak=",AK,"&output=json&address=",add[i], sep = "")
  url_string <- URLencode(url)
  
  # 捕获连接对象
  connect <- url(url_string)
  
  # 处理json对象
  temp_geo <- fromJSON(paste(readLines(connect,warn = F), collapse = ""))
  if(is.null(temp_geo$result$location$lat)){next}
  stkdata$lat[i]<-temp_geo$result$location$lat
  stkdata$lng[i]<-temp_geo$result$location$lng
  cat(i,"\n")
}
#列表循环-end#
stkdata_a<-read.csv(file="baidu_geocoding3.csv")
#####自动运行-end#####

#查看数据
e <- which(is.na(stkdata$lat))
for (i in e){
  url <- paste("http://api.map.baidu.com/geocoder/v2/?ak=",AK,"&output=json&address=",add[i], sep = "")
  url_string <- URLencode(url)
  
  # 捕获连接对象
  connect <- url(url_string)
  
  # 处理json对象
  temp_geo <- fromJSON(paste(readLines(connect,warn = F), collapse = ""))
  if(is.null(temp_geo$result$location$lat)){next}
  stkdata$lat[i]<-temp_geo$result$location$lat
  stkdata$lng[i]<-temp_geo$result$location$lng
  cat(i,"\n")
}



#导出数据
write.csv(stkdata,file="...\\test_return.csv")








