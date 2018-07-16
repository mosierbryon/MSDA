# 
# 
# 
# title: group 10 project, summer 2018
# authors: Bryon Mosier, Hejin Shin, Veronica Stephens
# date: 07/15/2018
# 
# ----------------------------------------------------------------------------- 
# load libraries
library(dplyr)
library(caret)
library(ggplot2)
library(lubridate)
library(pls)

# ----------------------------------------------------------------------------- 
# read csv: unemployment length 
df_length <- read.csv('https://raw.githubusercontent.com/mosierbryon/MSDA/master/mean_unemployment_length.csv')
names(df_length) <- c('date','mean_unemp_weeks')
df_length <- df_length %>% mutate(date=as.Date(date,format='%m/%d/%Y'))

# scatter plot: date and mean weekly unemployment
a <- ggplot(df_length,aes(x=date,y=mean_unemp_weeks)) + geom_point(stat='identity') + theme_bw()
a


# read csv: unemployment rate
df_rate <- read.csv('https://raw.githubusercontent.com/mosierbryon/MSDA/master/Unemployment.csv')

# alter date to match mm-dd-yyyy format
df_rate <- df_rate %>% mutate(Month=as.character(Month),
                              month_num=ifelse(Month=='Jan',1,
                                        ifelse(Month=='Feb',2,
                                        ifelse(Month=='Mar',3,
                                        ifelse(Month=='Apr',4,
                                        ifelse(Month=='May',5,
                                        ifelse(Month=='Jun',6,
                                        ifelse(Month=='Jul',7,
                                        ifelse(Month=='Aug',8,
                                        ifelse(Month=='Sep',9,
                                        ifelse(Month=='Oct',10,
                                        ifelse(Month=='Nov',11,
                                        ifelse(Month=='Dec',12,
                                               'error')))))))))))))
# create date column in order to join 
df_rate <- df_rate %>% mutate(day=01, date=as.Date(paste(month_num,day,Year,sep='/'),
                                                    format='%m/%d/%Y')) %>% 
                       select(date,Unemployment_Rate,rate_women)

# combine dfs
df_rate <- inner_join(df_length,df_rate,by=c('date'='date'))  
df_rate <- df_rate %>% select(date,everything())

# read csv: fred data
df_fred <- read.csv('https://raw.githubusercontent.com/mosierbryon/MSDA/master/fred_mo_1919_2018_180715.csv')
names(df_fred)[1] <- c('date')
df_fred <- df_fred %>% mutate(date=as.Date(date,format='%Y-%m-%d'), year=year(date))

df <- left_join(df_fred,df_rate,by=c('date','date'))
df <- df %>% select(date,year,everything()) %>% filter(date >= '1948-01-01', date<'2018-06-01') %>% 
              select(-PSAVERT)

# ----------------------------------------------------------------------------- 
# initial models: pls, ...

# trainControl: control the computational nuances of the train function {caret}
myTimeControl <- trainControl(method = "timeslice",
                              initialWindow = 36,
                              horizon = 12,
                              fixedWindow = TRUE)

# names(df)
plsFitTime <- train(mean_unemp_weeks ~ date + Unemployment_Rate + rate_women +
                      INDPRO + CIVPART + CPIAUCSL + PAYEMS,
                    data = df,
                    method = "pls",
                    preProc = c("center", "scale"),
                    trControl = myTimeControl)
plsFitTime 
# Rsquared values:
# 47.18%, pred = date + year
# 41.71%, pred = date + Unemployment_Rate
# 40.62%, pred = date + Unemployment_Rate + rate_women














# ----------------------------------------------------------------------------- 
# for reference
# 
# 
## initial models
## source: https://stackoverflow.com/questions/24758218/time-series-data-spliting-and-model-evaluation?lq=1
# data(economics)
# df <- economics
# 
# # trainControl: control the computational nuances of the train function {caret}
# myTimeControl <- trainControl(method = "timeslice",
#                               initialWindow = 36,
#                               horizon = 12,
#                               fixedWindow = TRUE)
# # original: timeSlices <- createTimeSlices(1:nrow(df),initialWindow = 36, horizon = 12, fixedWindow = TRUE)
# 
# plsFitTime <- train(unemploy ~ pce + pop + psavert,
#                     data = economics,
#                     method = "pls",
#                     preProc = c("center", "scale"),
#                     trControl = myTimeControl)
# plsFitTime





