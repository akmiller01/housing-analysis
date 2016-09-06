source("C:/git/housing-analysis/api_plugin.R")

library(ggplot2)

df1 <- GetPriceDF("600 Roosevelt Blvd., Apt 404","22044")

df2 <- GetPriceDF("13123 Lazy Glen Ct.","20171")

df3 <- GetPriceDF("3859 Appaloosa Drive","22192")

data <- rbind(df1,df2,df3)

p <- ggplot(data,aes(x=date,y=price,color=street)) + geom_line()
p
