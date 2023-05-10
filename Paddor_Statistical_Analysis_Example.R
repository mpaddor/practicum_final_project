##Business Practicum Project----
library(stringr)
library(tidyverse)
library(dplyr)

#Downloading Datasets---
rsi<-read.csv('C:/Users/maylu/OneDrive/Desktop/Practicum Project/Racial_and_Social_Equity_Composite_Index (1).csv')
hospitals<-read.csv('C:/Users/maylu/Downloads/Hospitals_edited.csv')
insurance<-read.csv('C:/Users/maylu/OneDrive/Desktop/Practicum Project/Census Tract_Full Data_data.csv')

#Cleaning RSI----
##rsi dataset
rsi$NAME10<-str_remove_all(rsi$NAME10, ".00")
summary(rsi)
#Remove NA because there is only 1, less than 5% of data
is.na(rsi)
rsi<-na.omit(rsi)
#Remove non-numeric data and creating percentage & datasets 
rsi.pct<-rsi[-c(135),-c(1:2,4,5,7,9,11,13,15,23:29,31,33,35,37:39)]
rsi.ptl<-rsi[-c(135),-c(1:2,4,5,6,8,10,12,14,16:22,31,33,35,37:39)]

#Cleaning insurance 
insurance$X..Without.Health.Insurance<-str_remove_all(insurance$X..Without.Health.Insurance, "%")
insurance$X..Without.Health.Insurance<-as.numeric(insurance$X..Without.Health.Insurance)

#Exploratory Data Analysis----
summary(rsi.pct)
# asthma: min 8.0%, mean 8.93%, max: 11.4%
# diabetes: min 1.2%, mean 6.84%, max 17.77%
# mental health not good: min 7.1%, mean 9.97%, max 16.2%

##Insurance: Finding averages of people w/o insurance across race 
w<-insurance%>%
  filter(Race=="White Alone")%>%
  summarize(mean(X..Without.Health.Insurance))
#avg: 4.94697

b<-insurance%>%
  filter(Race=="Black / African American Alone")%>%
  summarize(mean(X..Without.Health.Insurance))
#avg: 8.968504

a<-insurance%>%
  filter(Race=="Asian Alone")%>%
  summarize(mean(X..Without.Health.Insurance))
#avg:4.651515

o<-insurance%>%
  filter(Race=="Some Other Race Alone")%>%
  summarize(mean(X..Without.Health.Insurance))
#avg: 16.48739

r<-insurance%>%
  filter(Race=="2+ Races")%>%
  summarize(mean(X..Without.Health.Insurance))
#AVG: 6.666667

n<-insurance%>%
  filter(Race=="Native Hawaiian & Other Pacific Islander Alone")%>%
  summarize(mean(X..Without.Health.Insurance))
#Avg: 3.244444

ai<-insurance%>%
  filter(Race=="American Indian & Alaska Native Alone")%>%
  summarize(mean(X..Without.Health.Insurance))
#19.31579
#Replaced NA Values in Excel with means

#Cluster Analysis----

#PTL
##Normalize Data
summary(rsi.ptl)
rsi.ptl.df<-rsi.ptl[,-c(1:7,9,12:16)]
rsi.ptl.norm<- sapply(rsi.ptl.df, scale)
row.names(rsi.ptl.norm) <- row.names(rsi.ptl)

#Creating Cluster using euclidean & complete method 
d2 = dist(rsi.ptl.norm, "euclidean")
hcl2<-hclust(d2,method="complete")
hcl2
plot(hcl2)
plot(hcl2, hang =-1, ann=TRUE)

#get cluster 1 and 2 from hcl
memb2<-cutree(hcl2,h=4) #h= gives a cutoff for number of height 
#print to see which census tracts are assigned with clusters 
memb2
#creating table with cluster results as a column
ptl.df2<-rsi.ptl%>%cbind(memb2,deparse.level=1) %>%view()
#number of census tract areas per cluster 
table(memb2) 
summary(memb2)

#Creating Heatmap
heatmap(rsi.ptl.norm, colv = NA, hclustfun = hclust)
view(ptl.df2)

##Possible Variables---- 
# -PCT of PPL of Color 
# -PCT English Less Than Very Well 
# -PCT Foreign Born 
# -Poverty 
# -Less than Bachelor Degree 
# -No Leisure Physical Activity 
# -PCT Obese 
# -PCT Disabilities
# -Socioeconomic Percentiles
# -Health Percentile

##Multiple Linear Regression ----

#Fixing Independent: mental health not good, Adult no leisure physical activity
log(rsi.ptl$PTL_ADULTMENTALHEALTHNOTGOOD)
rsi.ptl<-rsi.ptl[-c(132),] #133 rows
log(rsi.ptl$PTL_ADULTNOLEISUREPHYSACTIVITY)
rsi.ptl<-rsi.ptl[-c(117),] #132 rows

#fixing dependent column
log(rsi.ptl$PTL_ADULT_WITH_ASTHMA)
rsi.ptl.a<-rsi.ptl[-c(39),] #131

log(rsi.ptl$PTL_ADULT_DIAGNOSED_DIABETES)
rsi.ptl.d<-rsi.ptl[-c(94),] #131

##Asthma
cor(rsi.ptl$PTL_PEOPLE_OF_COLOR, rsi.ptl$PTL_ADULT_WITH_ASTHMA)
#0.6713643
cor(rsi.ptl$PTL_ENGLISH_LESSTHAN_VERY_WELL, rsi.ptl$PTL_ADULT_WITH_ASTHMA)
#0.6028536
cor(rsi.ptl$PTL_FOREIGN_BORN, rsi.ptl$PTL_ADULT_WITH_ASTHMA)
#0.4818459
cor(rsi.ptl$PTL_POP_INC_UNDER_200_POVERTY, rsi.ptl$PTL_ADULT_WITH_ASTHMA)
#0.7799835
cor(rsi.ptl$PTL_LESS_BACHELOR_DEGREE, rsi.ptl$PTL_ADULT_WITH_ASTHMA)
#0.7038015
cor(rsi.ptl$PTL_ADULTNOLEISUREPHYSACTIVITY, rsi.ptl$PTL_ADULT_WITH_ASTHMA)
#0.7788867
cor(rsi.ptl$PTL_ADULT_DIAGNOSED_DIABETES, rsi.ptl$PTL_ADULT_WITH_ASTHMA)
#0.4339155
cor(rsi.ptl$PTL_ADULT_OBESE, rsi.ptl$PTL_ADULT_WITH_ASTHMA)
#0.673499
cor(rsi.ptl$PTL_ADULTMENTALHEALTHNOTGOOD, rsi.ptl$PTL_ADULT_WITH_ASTHMA)
#0.9194263
cor(rsi.ptl$PTL_ADULT_WITH_DISABILITIES, rsi.ptl$PTL_ADULT_WITH_ASTHMA)
#0.4523253
cor(rsi.ptl$PTL_LOW_LIFE_EXPECT_AT_BIRTH, rsi.ptl$PTL_ADULT_WITH_ASTHMA)
#0.5142009
cor(rsi.ptl$SOCIOECONOMIC_PERCENTILE, rsi.ptl$PTL_ADULT_WITH_ASTHMA)
#0.8125134
cor(rsi.ptl$HEALTH_PERCENTILE, rsi.ptl$PTL_ADULT_WITH_ASTHMA)
#0.8280946

model1 <- lm(log(rsi.ptl.a$PTL_ADULT_WITH_ASTHMA)~
               rsi.ptl.a$PTL_FOREIGN_BORN+
               log(rsi.ptl.a$PTL_ADULTMENTALHEALTHNOTGOOD)+
               log(rsi.ptl.a$PTL_ADULTNOLEISUREPHYSACTIVITY)+
               rsi.ptl.a$PTL_POP_INC_UNDER_200_POVERTY+
               rsi.ptl.a$PTL_ADULT_OBESE+
               rsi.ptl.a$PTL_LESS_BACHELOR_DEGREE)
summary(model1)
#sig var: foreign_born, mental health, leisure/physical,socioeconomic_percentile

##Diabetes
cor(rsi.ptl$PTL_PEOPLE_OF_COLOR, rsi.ptl$PTL_ADULT_DIAGNOSED_DIABETES)
#0.5500539
cor(rsi.ptl$PTL_ENGLISH_LESSTHAN_VERY_WELL, rsi.ptl$PTL_ADULT_DIAGNOSED_DIABETES)
#0.5876599
cor(rsi.ptl$PTL_FOREIGN_BORN, rsi.ptl$PTL_ADULT_DIAGNOSED_DIABETES)
#0.4607185
cor(rsi.ptl$PTL_POP_INC_UNDER_200_POVERTY, rsi.ptl$PTL_ADULT_DIAGNOSED_DIABETES)
#0.373661
cor(rsi.ptl$PTL_LESS_BACHELOR_DEGREE, rsi.ptl$PTL_ADULT_DIAGNOSED_DIABETES)
#0.7305333
cor(rsi.ptl$PTL_ADULTNOLEISUREPHYSACTIVITY, rsi.ptl$PTL_ADULT_DIAGNOSED_DIABETES)
#0.8037525
cor(rsi.ptl$PTL_ADULT_WITH_ASTHMA, rsi.ptl$PTL_ADULT_DIAGNOSED_DIABETES)
#0.4339155
cor(rsi.ptl$PTL_ADULT_OBESE, rsi.ptl$PTL_ADULT_DIAGNOSED_DIABETES)
#0.7026671
cor(rsi.ptl$PTL_ADULTMENTALHEALTHNOTGOOD, rsi.ptl$PTL_ADULT_DIAGNOSED_DIABETES)
#0.4585303
cor(rsi.ptl$PTL_ADULT_WITH_DISABILITIES, rsi.ptl$PTL_ADULT_DIAGNOSED_DIABETES)
#0.6848682
cor(rsi.ptl$PTL_LOW_LIFE_EXPECT_AT_BIRTH, rsi.ptl$PTL_ADULT_DIAGNOSED_DIABETES)
#0.4071639
cor(rsi.ptl$SOCIOECONOMIC_PERCENTILE, rsi.ptl$PTL_ADULT_DIAGNOSED_DIABETES)
#0.5957086
cor(rsi.ptl$HEALTH_PERCENTILE, rsi.ptl$PTL_ADULT_DIAGNOSED_DIABETES)
#0.764672

model2 <- lm(log(rsi.ptl.d$PTL_ADULT_DIAGNOSED_DIABETES)~
               log(rsi.ptl.d$PTL_ADULTMENTALHEALTHNOTGOOD)+
               log(rsi.ptl.d$PTL_ADULTNOLEISUREPHYSACTIVITY)+
               rsi.ptl.d$PTL_POP_INC_UNDER_200_POVERTY+
               rsi.ptl.d$PTL_ADULT_OBESE+
               rsi.ptl.d$PTL_LESS_BACHELOR_DEGREE)
summary(model2)

#sig var: mental health, leisure/physical 

##Mental Health
##Diabetes
cor(rsi.ptl$PTL_PEOPLE_OF_COLOR, rsi.ptl$PTL_ADULTMENTALHEALTHNOTGOOD)
#0.7977803
cor(rsi.ptl$PTL_ENGLISH_LESSTHAN_VERY_WELL, rsi.ptl$PTL_ADULTMENTALHEALTHNOTGOOD)
#0.7256239
cor(rsi.ptl$PTL_FOREIGN_BORN, rsi.ptl$PTL_ADULTMENTALHEALTHNOTGOOD)
#0.6463905
cor(rsi.ptl$PTL_POP_INC_UNDER_200_POVERTY, rsi.ptl$PTL_ADULTMENTALHEALTHNOTGOOD)
#0.8804195
cor(rsi.ptl$PTL_LESS_BACHELOR_DEGREE, rsi.ptl$PTL_ADULTMENTALHEALTHNOTGOOD)
#0.8127267
cor(rsi.ptl$PTL_ADULTNOLEISUREPHYSACTIVITY, rsi.ptl$PTL_ADULTMENTALHEALTHNOTGOOD)
#0.8605336
cor(rsi.ptl$PTL_ADULT_WITH_ASTHMA, rsi.ptl$PTL_ADULTMENTALHEALTHNOTGOOD)
#0.9194263
cor(rsi.ptl$PTL_ADULT_OBESE, rsi.ptl$PTL_ADULTMENTALHEALTHNOTGOOD)
#0.6222263
cor(rsi.ptl$PTL_ADULT_DIAGNOSED_DIABETES, rsi.ptl$PTL_ADULTMENTALHEALTHNOTGOOD)
#0.4585303
cor(rsi.ptl$PTL_ADULT_WITH_DISABILITIES, rsi.ptl$PTL_ADULTMENTALHEALTHNOTGOOD)
#0.5166168
cor(rsi.ptl$PTL_LOW_LIFE_EXPECT_AT_BIRTH, rsi.ptl$PTL_ADULTMENTALHEALTHNOTGOOD)
#0.5166451
cor(rsi.ptl$SOCIOECONOMIC_PERCENTILE, rsi.ptl$PTL_ADULTMENTALHEALTHNOTGOOD)
#0.9267606
cor(rsi.ptl$HEALTH_PERCENTILE, rsi.ptl$PTL_ADULTMENTALHEALTHNOTGOOD)
#0.8481273

model3 <- lm(rsi.ptl$PTL_ADULTMENTALHEALTHNOTGOOD~
                rsi.ptl$PTL_FOREIGN_BORN+
                log(rsi.ptl$PTL_ADULTNOLEISUREPHYSACTIVITY)+
                rsi.ptl$PTL_ADULT_DIAGNOSED_DIABETES+
                rsi.ptl$PTL_ADULT_WITH_ASTHMA+
                rsi.ptl$PTL_POP_INC_UNDER_200_POVERTY+
                rsi.ptl$PTL_ADULT_OBESE)
summary(model3)
#sig var: diabetes, asthma, leisure/physical, socioeconomic ptl

#Health Percentile 
cor(rsi.ptl$PTL_PEOPLE_OF_COLOR, rsi.ptl$HEALTH_PERCENTILE)
#0.7010267
cor(rsi.ptl$PTL_ENGLISH_LESSTHAN_VERY_WELL, rsi.ptl$HEALTH_PERCENTILE)
#0.6573145
cor(rsi.ptl$PTL_FOREIGN_BORN, rsi.ptl$HEALTH_PERCENTILE)
#0.5174591
cor(rsi.ptl$PTL_POP_INC_UNDER_200_POVERTY, rsi.ptl$HEALTH_PERCENTILE)
#0.7114281
cor(rsi.ptl$PTL_LESS_BACHELOR_DEGREE, rsi.ptl$HEALTH_PERCENTILE)
#0.9339718
cor(rsi.ptl$PTL_ADULTNOLEISUREPHYSACTIVITY, rsi.ptl$HEALTH_PERCENTILE)
#0.8605336
cor(rsi.ptl$PTL_ADULT_WITH_ASTHMA, rsi.ptl$HEALTH_PERCENTILE)
#0.8197053
cor(rsi.ptl$PTL_ADULT_OBESE, rsi.ptl$HEALTH_PERCENTILE)
#0.8563307
cor(rsi.ptl$PTL_ADULT_DIAGNOSED_DIABETES, rsi.ptl$HEALTH_PERCENTILE)
#0.7711629
cor(rsi.ptl$PTL_ADULT_WITH_DISABILITIES, rsi.ptl$HEALTH_PERCENTILE)
#0.7336185
cor(rsi.ptl$PTL_LOW_LIFE_EXPECT_AT_BIRTH, rsi.ptl$HEALTH_PERCENTILE)
#0.7336185
cor(rsi.ptl$SOCIOECONOMIC_PERCENTILE, rsi.ptl$HEALTH_PERCENTILE)
#0.8795926
cor(rsi.ptl$PTL_ADULTMENTALHEALTHNOTGOOD, rsi.ptl$HEALTH_PERCENTILE)
#0.8407671
model4 <- lm(rsi.ptl$HEALTH_PERCENTILE~
               rsi.ptl$PTL_FOREIGN_BORN+
               +rsi.ptl$PTL_ADULT_DIAGNOSED_DIABETES+
               rsi.ptl$PTL_ADULT_WITH_ASTHMA+
               rsi.ptl$PTL_POP_INC_UNDER_200_POVERTY+
               rsi.ptl$PTL_ADULT_OBESE+
               rsi.ptl$PTL_LESS_BACHELOR_DEGREE)
summary(model4)
#sig var: foreign born, diabetes, asthma, mental health, leisure/physical,socioeconomic

#Checking VIF
library(car)

vif(model1)
#create vector of VIF values
vif_values <- vif(model1)
#create horizontal bar chart to display each VIF value
barplot(vif_values, main = "VIF Values", horiz = TRUE, col = "steelblue")
#add vertical line at 5
abline(v = 5, lwd = 3, lty = 2)

vif(model2)
#create vector of VIF values
vif_values2 <- vif(model2)
#create horizontal bar chart to display each VIF value
barplot(vif_values2, main = "VIF Values", horiz = TRUE, col = "green")
#add vertical line at 5
abline(v = 5, lwd = 3, lty = 2)

vif(model3)
#create vector of VIF values
vif_values3 <- vif(model3)
#create horizontal bar chart to display each VIF value
barplot(vif_values3, main = "VIF Values", horiz = TRUE, col = "steelblue")
#add vertical line at 5
abline(v = 5, lwd = 3, lty = 2)

vif(model4)
#create vector of VIF values
vif_values4 <- vif(model4)
#create horizontal bar chart to display each VIF value
barplot(vif_values4, main = "VIF Values", horiz = TRUE, col = "steelblue")
#add vertical line at 5
abline(v = 5, lwd = 3, lty = 2)

#Graphing Models----
##Model 1
plot(density(model1$residuals))
#fairly normally distributed
ggplot(data=rsi.ptl, aes(x=PTL_ADULT_WITH_ASTHMA))+geom_histogram()
#the prevalence of asthma is skewed left with more than half tracts having a ptl 
#of asthma less than 50% 
ggplot(data=rsi.ptl, aes(NAME10, PTL_ADULT_WITH_ASTHMA)) + geom_point(colour = "red", size = 3)

#Model2
plot(model2$residuals)
plot(density(model2$residuals))
#fairly normally distributed with more outliers on the left side
ggplot(data=rsi.ptl, aes(x=rsi.ptl$PTL_ADULT_DIAGNOSED_DIABETES))+geom_histogram()
#fairly normally distributed in the amount of counties per percentile of asthma
ggplot(data=rsi.ptl, aes(NAME10, PTL_ADULT_DIAGNOSED_DIABETES)) + geom_point(colour = "red", size = 3)
#more drastic pattern in tracts with percentiles of diabetes

#Model3
plot(model3$residuals)
plot(density(model3$residuals))
#Normal distribution 
ggplot(data=rsi.ptl, aes(x=PTL_ADULTMENTALHEALTHNOTGOOD))+geom_histogram()
#fairly normally distributed in the amount of counties per percentile of not good mental health 
ggplot(data=rsi.ptl, aes(NAME10, PTL_ADULTMENTALHEALTHNOTGOOD)) + geom_point(colour = "red", size = 3)
#less pattern in distribution

#Determining Tracts with highest and lowest chronic condition prevalence percentiles---
#Asthma
#Tract 118 = highest asthma, 0.992 
#Tract 107.02 = second asthma diabetes,0.992
#Tract 53.01 = third highest asthma, 0.984
top.asthma<-rsi.ptl%>%
                  select(NAME10, PTL_ADULT_WITH_ASTHMA,PTL_ADULTMENTALHEALTHNOTGOOD,
                   PTL_ADULT_DIAGNOSED_DIABETES)%>%
                   top_n(34, PTL_ADULT_WITH_ASTHMA)

low.asthma<-rsi.ptl%>%
  select(NAME10, PTL_ADULT_WITH_ASTHMA,PTL_ADULTMENTALHEALTHNOTGOOD,
         PTL_ADULT_DIAGNOSED_DIABETES)%>%
  top_n(-34, PTL_ADULT_WITH_ASTHMA)


#Diabetes
#Tract 91 = highest diabetes, 0.992 
#Tract 110.01 = second highest diabetes,0.992
#Tract 118 = third highest diabetes, 0.984
top.diabetes<-rsi.ptl%>%
  select(NAME10, PTL_ADULT_DIAGNOSED_DIABETES, PTL_ADULT_WITH_ASTHMA,PTL_ADULTMENTALHEALTHNOTGOOD)%>%
  top_n(34, PTL_ADULT_DIAGNOSED_DIABETES)

low.diabetes<-rsi.ptl%>%
  select(NAME10, PTL_ADULT_DIAGNOSED_DIABETES, PTL_ADULT_WITH_ASTHMA,PTL_ADULTMENTALHEALTHNOTGOOD)%>%
  top_n(-34, PTL_ADULT_DIAGNOSED_DIABETES)

#Mental Health Not Good
#Tract 53.01 = highest bad mental health, 1.0
#Tract 118 = second highest bad mental health, 0.992
#Tract 53.02 = third highest bad mental health, 0.984
top.mental<-rsi.ptl%>%
  select(NAME10,PTL_ADULTMENTALHEALTHNOTGOOD, PTL_ADULT_DIAGNOSED_DIABETES, PTL_ADULT_WITH_ASTHMA)%>%
  top_n(34, PTL_ADULTMENTALHEALTHNOTGOOD)

low.mental<-rsi.ptl%>%
  select(NAME10,PTL_ADULTMENTALHEALTHNOTGOOD, PTL_ADULT_DIAGNOSED_DIABETES, PTL_ADULT_WITH_ASTHMA)%>%
  top_n(-34, PTL_ADULTMENTALHEALTHNOTGOOD)

##Creating tables that share the first and fourth quantile----
high.burden<-inner_join(top.mental,top.asthma,NAME10=NAME10)%>%
              inner_join(top.diabetes,NAME10=NAME10)

low.burden<-inner_join(low.mental,low.asthma,NAME10=NAME10)%>%
  inner_join(low.diabetes,NAME10=NAME10)

#Hospital Data---
##matching datasets
class(hospitals$Cenus.Tract)
class(ptl.df2$NAME10)

hospitals$Cenus.Tract<-as.character(hospitals$Cenus.Tract)
hospitals$Cenus.Tract<-str_remove_all(hospitals$Cenus.Tract, ".00")

#Table that shows hospitals & cluster number

table1<-hospitals%>% 
  select(FACILITY,Cenus.Tract)

class(table1$Cenus.Tract)

table1$Cenus.Tract<-as.character(table1$Cenus.Tract)

table<-full_join(table1, ptl.df2, by=c("Cenus.Tract"="NAME10")) %>% view()

table2<-full_join(table1, high.burden, by=c("Cenus.Tract"="NAME10"))
#showed that there is one hospital in the high.burden tracts

table3<-full_join(table1, low.burden, by=c("Cenus.Tract"="NAME10"))
#showed there are no hospitals in the low.burden tracts

#Insurance: Finding avg. w/o insurance rates by the highest prevalence areas----
#Joining to High Burden tables
ai.table<-insurance%>%
  filter(Race=="American Indian & Alaska Native Alone")%>%
  select(X..Without.Health.Insurance,Census.Tract)

ai.table$Census.Tract<-as.character(ai.table$Census.Tract)

ai.tracts<-
  full_join(ai.table,high.burden,by = c("Census.Tract"="NAME10")) %>%
  na.omit(ai.tracts)
mean(ai.tracts$X..Without.Health.Insurance) #17.30921

w.table<-insurance%>%
  filter(Race=="White Alone")

w.table$Census.Tract<-as.character(w.table$Census.Tract)

w.tracts<-
  full_join(w.table,high.burden,by = c("Census.Tract"="NAME10")) %>%
  na.omit(ai.tracts)
mean(w.tracts$X..Without.Health.Insurance) #7.875


b.table<-insurance%>%
  filter(Race=="Black / African American Alone")

b.table$Census.Tract<-as.character(b.table$Census.Tract)

b.tracts<-
  full_join(b.table,high.burden,by = c("Census.Tract"="NAME10")) %>%
  na.omit(b.tracts)
mean(b.tracts$X..Without.Health.Insurance) #13.3125

a.table<-insurance%>%
  filter(Race=="Asian Alone")

a.table$Census.Tract<-as.character(a.table$Census.Tract)

a.tracts<-
  full_join(a.table,high.burden,by = c("Census.Tract"="NAME10")) %>%
  na.omit(a.tracts)
mean(a.tracts$X..Without.Health.Insurance) #8

o.table<-insurance%>%
  filter(Race=="Some Other Race Alone")

o.table$Census.Tract<-as.character(o.table$Census.Tract)

o.tracts<-
  full_join(o.table,high.burden,by = c("Census.Tract"="NAME10")) %>%
  na.omit(o.tracts)
mean(o.tracts$X..Without.Health.Insurance) #22.1875

r.table<-insurance%>%
  filter(Race=="2+ Races")

r.table$Census.Tract<-as.character(r.table$Census.Tract)

r.tracts<-
  full_join(r.table,high.burden,by = c("Census.Tract"="NAME10")) %>%
  na.omit(r.tracts)
mean(r.tracts$X..Without.Health.Insurance) #16.0625

n.table<-insurance%>%
  filter(Race=="Native Hawaiian & Other Pacific Islander Alone")

n.table$Census.Tract<-as.character(n.table$Census.Tract)

n.tracts<-
  full_join(n.table,high.burden,by = c("Census.Tract"="NAME10")) %>%
  na.omit(n.tracts)
mean(n.tracts$X..Without.Health.Insurance) #4.091666

