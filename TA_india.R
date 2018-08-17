# Terrorism Analysis in India
# Author:- Darpan Purwar

# clearing all variables
rm(list = ls())

#loading library
library(data.table)
library(tidyverse)
library(stringr)
library(magrittr)
library(readxl)
library(hashmap)
library(kableExtra)
library(magrittr)
library(treemap)
library(leaflet)
library(corrplot)
library(plyr)
library(randomForest)

# seting the directory

# reading the input file
df_main <- fread('Terror_India.csv')

# showing the random 15 rows of data in html format
df_main[10000:11000,c("Date","provstate","attacktype1_txt","targtype1_txt","gname","nkill","nwound")] %>% kable() %>% kable_styling(bootstrap_options = "striped", font_size = 10,full_width = F, position = "left")

# Terror Attack per Year
df_main %>% filter(!is.na(iyear)) %>% group_by(iyear) %>% summarise(nat = length(city)) %>% ungroup() -> df_A

# plotting graph of number of attack per year
ggplot(data = df_A, mapping = aes(x= iyear, y= nat))+
  geom_area(fill = "Red")+geom_point()+theme_bw()+theme(legend.position="right")+labs(x= "Year", y= "Number of attacks")+
  ggtitle(" Terror Attack by Year ") +
  theme(plot.title = element_text(hjust = 0.5))

# Killed per Year
df_main %>% filter(!is.na(nkill)) %>% group_by(iyear) %>% summarise(nk = sum(nkill)) %>% ungroup() -> df_k

# plotting graph of Killed per year
ggplot(data = df_k, mapping = aes(x= iyear, y= nk))+
  geom_line()+geom_point()+theme_bw()+theme(legend.position="right")+labs(x= "Year", y= "Killed")+
  ggtitle(" Killed per Year ") +
  theme(plot.title = element_text(hjust = 0.5))

# Plotting attacks per Year
# df_main %>% filter(nkill > 0) -> dfk
com_ak <- merge(df_A,df_k, by ="iyear")
treemap(com_ak, 
        index=c("iyear"), 
        type = "value",
        vSize = "nat", 
        vColor="nk",
        palette = "Reds",  
        # algorithm = "pivot-by-size",
        title="Attack & Killings (1976-2016)", 
        # range = c(1,10000),
        fontsize.title = 15,
        # fontsize.labels=c(15,12),
        title.legend = "Number of killed"
        # sortID = "nkills"
)


# Killed per State
df_main %>% filter(!is.na(iyear)) %>% group_by(provstate) %>% summarise(sa = length(iday)) %>% ungroup() -> df_s
df_main %>% filter(!is.na(nkill)) %>% group_by(provstate) %>% summarise(sk = sum(nkill)) %>% ungroup() -> df_s1
  
com_state <- merge(df_s,df_s1,by = "provstate")
treemap(com_state, 
        index=c("provstate"), 
        type = "value",
        vSize = "sa", 
        vColor="sk",
        palette = "RdBu",  
        # algorithm = "pivot-by-size",
        title="Attack & Killings in India", 
        # range = c(1,10000),
        fontsize.title = 15,
        title.legend = "Number of killed"
        # sortID = "nkills"
)
# removing useless data drames( clearning the memory)
rm(com_ak,com_state,df_A,df_k,df_s,df_s1,df_temp)

# Attack type
df_main %>% filter(!is.na(iyear)) %>% group_by(attacktype1_txt) %>% summarise(nk = length(iday)) %>% ungroup() -> df_temp

# Plotting the grpah

ggplot(data = df_temp, mapping = aes(x= reorder(attacktype1_txt,nk),y = nk))+
  geom_bar(stat = "identity",position = "dodge", color = "black")+
  theme_bw(base_size = 20)+coord_flip()+
  labs(x= "Attack Type", y ="Frequency")+
  ggtitle(" Type of Terror Attack ") +
  theme(plot.title = element_text(hjust = 0.5))

# Target chose by Terrorist 
df_main %>% filter(!is.na(iyear)) %>% group_by(targtype1_txt) %>% summarise(nk = length(iday)) %>% ungroup() -> df_temp

# Plotting the graph

ggplot(data = df_temp, mapping = aes(x= reorder(targtype1_txt,nk),y = nk))+
  geom_bar(stat = "identity",position = "dodge", fill= " light green", color = "black")+
  theme_bw(base_size = 20)+coord_flip()+
  labs(x= "Targets", y ="Frequency")+
  ggtitle(" Terror Targets ") +
  theme(plot.title = element_text(hjust = 0.5))

# Weapon used in Attack
df_main %>% filter(!is.na(iyear)) %>% group_by(weaptype1_txt) %>% summarise(nk = length(iday))%>% top_n(n=5) %>% ungroup() -> df_temp

# Plotting the graph

ggplot(data = df_temp, mapping = aes(x= reorder(weaptype1_txt,nk),y = nk))+
  geom_bar(stat = "identity",position = "dodge", fill= " Orange", color = "black")+
  theme_bw(base_size = 20)+coord_flip()+
  labs(x= "Weapons", y ="Frequency")+
  ggtitle(" Weapon used in Terror Attack ") +
  theme(plot.title = element_text(hjust = 0.5))


# Attack by Terror Group
df_main %>% filter(!is.na(iyear)) %>% group_by(gname) %>% summarise(nk = length(iday))%>% top_n(n=20) %>% ungroup() -> df_temp

ggplot(data = df_temp, mapping = aes(x= reorder(gname,nk),y = nk))+
  geom_bar(stat = "identity",position = "dodge", fill= " Blue", color = "black")+
  theme_bw(base_size = 20)+coord_flip()+
  labs(x= "Terror Group", y ="Frequency")+
  ggtitle(" Terrorist Groups ") +
  theme(plot.title = element_text(hjust = 0.5))


# Attack by City
df_main %>% filter(!is.na(iyear)) %>% group_by(city) %>% summarise(nk = length(iday)) %>% top_n(n=10)%>% ungroup() -> df_temp

ggplot(data = df_temp, mapping = aes(x= reorder(city,nk),y = nk))+
  geom_bar(stat = "identity",position = "dodge", fill= " purple", color = "black")+
  theme_bw(base_size = 20)+coord_flip()+
  labs(x= "City", y ="Frequency")+
  ggtitle(" Terror Attack by City ") +
  theme(plot.title = element_text(hjust = 0.5))


leaflet(data = df_main) %>%
  addTiles() %>%
  addMarkers(lat=df_main$latitude, lng=df_main$longitude, clusterOptions = markerClusterOptions(),
             popup= paste("<strong>Date: </strong>", df_main$iday,"/",df_main$imonth,"/", df_main$iyear,
                          "<br><br><strong>Place: </strong>", df_main$city,"-",df_main$country_txt,
                          "<br><strong>Killed: </strong>", df_main$nkill
             ))
df_cor <- df_main[,c("nkill","nwound")]
df_cor <- na.omit(df_cor)
res <- cor(df_cor)
corrplot(res,type = "upper", order = "hclust", 
         tl.col = "black", tl.srt = 45)
rm(df_temp)

# prediction part
# Implementing Random forest

df_temp <- df_main[, c("iday","iyear","imonth","provstate","attacktype1_txt","targtype1_txt","gname","weaptype1_txt","nkill","success","nwound")]
# Removing unknown and missing values
df_temp %>% filter(!is.na(nkill),!is.na(nwound),provstate != "Unknown",attacktype1_txt != "Unknown",targtype1_txt != "Unknown",provstate != "Unknown",gname != "Unknown",weaptype1_txt != "Unknown",nkill>0)-> df_data; rm(df_temp)
df_data  <- na.omit(df_data)
df_data <- filter(df_data,provstate != "Unknown")
# removing gname who attacked only 1 times before 2003
qw<-data.frame(count(df_data,gname))
colnames(qw)[c(1,2)]=c("g","f")
qw %>% filter(f < 2 ) -> r1
rm_list <- r1[,1]
df1 <- df_data[df_data$gname %in% rm_list,]
df2 <- filter(df1,iyear<2003)
rm_list  <- df2[,"gname"]
df_data <- df_data[!df_data$gname %in%rm_list ,]

# converting in to Factor 
df_data$weaptype1_txt <- as.factor(df_data$weaptype1_txt)
df_data$provstate <- as.factor(df_data$provstate)
df_data$gname <- as.factor(df_data$gname)
df_data$targtype1_txt <- as.factor(df_data$targtype1_txt)
df_data$provstate <- as.factor(df_data$provstate)
df_data$iyear <- as.factor(df_data$iyear)
df_data$iday <- as.factor(df_data$iday)
df_data$imonth <- as.factor(df_data$imonth)
# partition in train and test data
test <- sample(1:nrow(df_data),size = .3*nrow(df_data))
df_test <- df_data[test,]
frmla <- success ~ imonth+provstate
df_train <- df_data[-test,]
rm(df_cor,df_k,df1)

# decision tree being used to predict weapon type
set.seed(222)
library(evtree)
fitEv		<-	evtree(weaptype1_txt ~ iday+imonth+nkill, data=df_train)
plot(fitEv)

# implementing random forest
rf <- randomForest(provstate ~ iday+ imonth+nkill+weaptype1_txt ,data=df_train)
print(rf)
library(caret)
p1<- predict(rf,df_train)
head(p1)
head(df_train$weaptype1_txt)
confusionMatrix(p1,df_train$provstate)
plot(rf)

# Regression applied to predict the number of killed people
lm_killed <- lm(nkill~ nwound, data = df_train)
summary(lm_killed)