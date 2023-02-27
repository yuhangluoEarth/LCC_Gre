library(ggplot2)
library(dplyr)
library(reshape2)
library(MatchIt)
library(readxl)
library(foreign)
library(openxlsx)
setwd('...')
data.CN.Point <- read.dbf('...')
data.CN.Point.change <- filter(data.CN.Point,(loc_maxlcc>0)|(loc_minlcc>0))%>%transform(change=1)
data.CN.Point.notchange <- filter(data.CN.Point,((loc_maxlcc==0)&(loc_minlcc==0))&((maxlcc <0.002)&(abs(minlcc) <0.002)))%>%transform(change=0)
data.CN.Point <- rbind(data.CN.Point.change,data.CN.Point.notchange)
data.CN.Env1 <- read.dbf('...')
data.CN.Env2 <- read.dbf('...')
data.CN.Env <- rbind(data.CN.Env1,data.CN.Env2)
rm(data.CN.Env1)
rm(data.CN.Env2)
data.CN.Point <- inner_join(data.CN.Point,data.CN.Env,by = "pointid")
data.CN.Point <- filter(data.CN.Point,lai_mean >0)
rownames(data.CN.Point) <- data.CN.Point$pointid

m.out <- matchit(change ~ prec_yr_sl + temp_yr_sl + elevation + slope + highway_di + population + lai_mean + maxlc_Av_v,
                 data = data.CN.Point,replace = TRUE,exact = c("Eco_ID","maxlc_Av_l"),
                 method = "nearest",ratio=3,caliper=0.25) #Or ratio=1,caliper=NULL

summ <- summary(m.out)
write.xlsx(data.frame(summ$sum.all),"...",rowNames = TRUE)
write.xlsx(data.frame(summ$sum.matched),"...",rowNames = TRUE)
write.xlsx(data.frame(summ$reduction),"...",rowNames = TRUE)
write.xlsx(data.frame(summ$nn),"...",rowNames = TRUE)
m.data.i <- data.frame(m.out$match.matrix)
numb <- ceiling(nrow(m.data.i)/1000000)
m.data.i <- transform(m.data.i,pointid = rownames(m.data.i))


pdf(file = 'histogram.pdf',height = 7, width = 8)
plot(m.out, type = "histogram")
dev.off()
pdf(file = 'love_plot.pdf',height = 7, width = 8)
plot(summary(m.out, interactions = F),
     var.order = "unmatched")
dev.off()
