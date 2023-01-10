---
  title: "社發處"
author: "蔡佳泓"
date: '3/1/2022'
output:
  bookdown::html_document2:
  toc: TRUE
---
rm(list=ls()) 

library(knitr)
library(dplyr)
library(kableExtra)
library(showtext);library(here);library(janitor)
library(ggplot2); #library(emoGG);
library(ggthemes)
#library(ggstatsplot);library(TSstudio);library(nycflights13)
showtext.auto(enable = TRUE)
#font_add("SimSun","Songti.ttc","YouYuan")
knitr::opts_chunk$set(echo = TRUE, warning=FALSE, message=FALSE,results='asis',
                      fig.width = 8, fig.height=5, collapse = TRUE,
                      latex.options.color='blue', fig.align = "center")
options(knitr.table.format = "html", digits=4)


# Example

df <- read.csv('testdata1.csv', header=T, sep=',')
p <- ggplot(df, aes(x=Occu, y=Value))
png('test.png')
p + geom_bar(stat = "identity", aes(fill=Occu)) +
  facet_wrap(~Var, ncol = 1) +
  theme(text = element_text(family = 'YouYuan'))
dev.off()

#路徑設定或讀取
getwd()

setwd("C:/Users/user/Desktop")

# 盒型圖
## 
file <- here::here('PP2297E53.sav')
df <- sjlabelled::read_spss(file)
df <- df %>% select(Q1:Q12)

#function
myrecode1 <- function(x){
  ifelse(x<=10, x, NA)
}
dat<-df %>% mutate_at(c("Q1","Q2","Q3","Q4","Q5","Q6","Q7","Q8","Q9","Q10", "Q11","Q12"), myrecode1)
#dat %>% dplyr::summarise_all(.funs=c(mean='mean',
#                SD='sd',median='median'))

stargazer::stargazer(dat, median = T)
stargazer::stargazer(dat, type='html', median = T)
stargazer::stargazer(dat, type='text', median = T)

dat <- reshape2::melt(dat)
dat <- na.omit(dat)
dat$variable<-car::recode(dat$variable, "'Q1'='少子化';'Q2'='老人長照';'Q3'='偏鄉教育';
          'Q4'='非典型就業';'Q5'='薪資成長緩慢';'Q6'='高房價';'Q7'='貧富差距';
          'Q8'='企業機器自動化';
          'Q9'='網路詐騙';'Q10'='極端氣候';'Q11'='產業轉型及失業';'Q12'='假消息'")
dat$var<-factor(dat$variable, levels=c('少子化','老人長照','偏鄉教育',
                                       '非典型就業','薪資成長緩慢','高房價',
                                       '貧富差距',   '企業機器自動化',
                                       '網路詐騙','極端氣候','產業轉型及失業','假消息'))

## Box Plot

p <- ggplot2::ggplot(dat, aes(var, value, fill=var)) +
  geom_boxplot(alpha=0.7, outlier.colour = NULL) +
  stat_summary(geom = 'crossbar', width=0.8, fatten=1, colour='white',
               fun.data=function(x){return(c(y=mean(x),ymin=mean(x),
                                             ymax=mean(x)))})+
  theme_minimal()+
  theme(text = element_text(family = 'YouYuan', size=13),
        legend.key = element_blank(), legend.position = 'none',
        legend.title = element_blank()) +
  labs(x='',y='')

png('boxplot1.png', res = 150, width=1600, height=1000)
p
dev.off()
