library(tidyverse)
library(rvest)

#part a
#Scraping data from the link
html <- read_html("https://www.moneyworks4me.com/best-index/nse-stocks/top-nifty50-companies-list/")
M.cap <- html %>% html_elements(".company-ellipses a") %>% html_text()
M.cap
cmp <- html %>% html_elements(".stick+ td span") %>% html_text()
priceChange <- html %>% html_elements("td:nth-child(4) span") %>% html_text()
MarketCap <- html %>% html_elements("td:nth-child(5)") %>% html_text()
MarkepCap <- MarketCap[1:50]
week_52_high <- html %>% html_elements("td:nth-child(6) > span") %>% html_text()
week_52_low <- html %>% html_elements("td:nth-child(7) > span") %>% html_text()
Roe <- html %>% html_elements("td:nth-child(8)") %>% html_text() 
Roe <- Roe[1:50]
p_by_e <- html %>% html_elements("td:nth-child(9)") %>% html_text() 
p_by_BV <- html %>% html_elements("td:nth-child(10)") %>% html_text()
EV_by_EBITDA <- html %>% html_elements("td:nth-child(11)") %>% html_text()
FIVEYSales_Gr <- html %>% html_elements("td:nth-child(12)") %>% html_text()
FIVEYProfit_Gr <- html %>% html_elements("td:nth-child(13)") %>% html_text() 
#creating data frame
tabledata1 <- data.frame(M.cap,cmp,priceChange,MarkepCap,week_52_high,week_52_low,Roe,p_by_e,p_by_BV,EV_by_EBITDA,FIVEYSales_Gr,FIVEYProfit_Gr)

#part b
#hdfc life insurance
#scraping data
DATE <- c(0)
for(i in 1:10)
{
  DATE[i] <- paste("Mar",12+i)
}
DATE <- append(DATE,'TTM')
html <- read_html("https://www.moneyworks4me.com/indianstocks/large-cap/bfsi/insurance/hdfc-life-insurance/company-info")
Sales <- html %>% html_nodes("tbody~ tbody tr:nth-child(1) td+ td") %>% html_text()
YoY_Gr_Rt1 <- html %>% html_nodes("tbody~ tbody tr:nth-child(2) td+ td")%>%html_text()
Adj_EPS <- html%>%html_nodes("tbody~ tbody tr:nth-child(3) td+ td")%>%html_text()
YoY_Gr_Rt2 <- html%>%html_nodes("tbody~ tbody tr:nth-child(4) span")%>%html_text()
BVPS <- html%>%html_nodes("tbody~ tbody tr:nth-child(5) td+ td")%>%html_text()
Adj_Net_Profit <- html%>%html_nodes("tbody~ tbody tr:nth-child(6) td+ td")%>%html_text()
Cash_flow_from_Ops <- html%>%html_nodes("tr:nth-child(7) td+ td , tr:nth-child(7) .stick+ td")%>%html_text()
Debt_CF_from_Ops <- html%>%html_nodes("tr:nth-child(8) td+ td span")%>%html_text()
Return_on_Equity <- html%>%html_nodes("#tenyearxraysa .col-12 tr:nth-child(1) td+ td span")%>%html_text()
OP_Profit_Mgn <- html%>%html_nodes(".col-12.mt-4 tr:nth-child(2) td+ td , .col-12.mt-4 tr:nth-child(2) .stick+ td")%>%html_text()
Net_Profit_Mgn <- html%>%html_nodes(".col-12.mt-4 tr:nth-child(3) td+ td")%>%html_text()
Debt_to_Equity <- html%>%html_nodes(".col-12.mt-4 tr+ tr td+ td span")%>%html_text()
Working_cap_days <- html%>%html_nodes(".mt-4 tr:nth-child(5) td+ td , .mt-4 tr:nth-child(5) .stick+ td")%>%html_text()
Cash_conv_cycle <- html%>%html_nodes(".mt-4 tr:nth-child(6) td+ td , .mt-4 tr:nth-child(6) .stick+ td")%>%html_text()
Hdfc_Life_Insurance <- data.frame(DATE,Sales,YoY_Gr_Rt1,Adj_EPS,YoY_Gr_Rt2,BVPS,Adj_Net_Profit,Cash_flow_from_Ops,Debt_CF_from_Ops,Return_on_Equity,OP_Profit_Mgn,Net_Profit_Mgn,Debt_to_Equity,Working_cap_days,Cash_conv_cycle,row.names = TRUE)
Hdfc_Life_Insurance <- t(Hdfc_Life_Insurance)


#part c
tennis <- function(p)
{
  a = 0
  b = 0
  for(i in 1:5)
  {
    h <- runif(n=1,min=0,max=1)
    if(h<=p)
      a=a+1
    b=b+1
    if(a==3)
      break
  }
  return(b)
}
var <- c(0)
for(i in 1:1000)
{
  var[i] <- tennis(0.7)
}
var
mean(var) #No of games played on average

#part d
a <- sample(x=0:1,size=1,prob=c(1/3,2/3))
a
t <- 0
for(i in 1:1000)
{
  a <- sample(x=0:1,size=1,prob=c(1/3,2/3))
  t <- t+a
}
ans <- t/1000
ans #finding average

#part e
html <- read_html("https://editorial.rottentomatoes.com/guide/best-netflix-movies-to-watch-right-now/")
Ranking <- c(1:100)
Name <- html %>% html_nodes(".article_movie_title a") %>% html_text()
Tomato_score <- html %>% html_elements(".tMeterScore") %>% html_text()
Year <- html %>% html_nodes(".start-year") %>% html_text()
tabledata2 <- data.frame(Ranking,Name,Tomato_score,Year)


