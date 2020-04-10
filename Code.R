library("readxl")
df <- read_excel("D:/Big Data Analytics/Assignment/school-profile-2018.xlsx", sheet = "School Profile")
head(df)
colnames(df)[7] <- "Sector"
library(dplyr)
Gov<-filter(df,Sector=='Catholic')
Gov
install.packages("ggplot2")
library(ggplot2)
platform = df %>% group_by(State) %>% summarise(Count = n())
p1 = ggplot(aes(x = State , y = Count , fill=Count) , data=platform) +
  geom_bar(colour='black',stat='identity') +
  theme_bw()+
  theme(axis.text.x = element_text(angle=45,hjust=1) , 
        plot.title = element_text(hjust=0.5))+  ## title center
  ggtitle('Platform Count')+
  scale_fill_distiller(palette = 'RdYlBu') +
  ylab('Count')




grid.arrange(p1, ncol = 1)
p1
#####
plat = df %>% group_by(Geolocation) %>% summarise(Count = n())
p2 = ggplot(aes(x = Geolocation , y = Count , fill=Count) , data=plat) +
  geom_bar(colour='black',stat='identity') +
  theme_bw()+
  theme(axis.text.x = element_text(angle=45,hjust=1) , 
        plot.title = element_text(hjust=0.5))+  ## title center
  ggtitle('Platform Count')+
  scale_fill_distiller(palette = 'RdYlBu') +
  ylab('Count')

p2

library(reshape2)
na.omit(df)
platform_sales = df %>% group_by(df$`State`) %>% summarise(x = sum(df$`Full Time Equivalent Enrolments`),
                                                         y = sum(df$`Girls Enrolments`),
                                                         z = sum(df$`Boys Enrolments`),
                                                         t = sum(df$`Total Enrolments`))

platform_sales = melt(platform_sales)
platform_sales
names(platform_sales) = c('Platform','SaleType','Sale')
ggplot(data = platform_sales,aes(x = Platform ,y = Sale , fill = SaleType)) + 
  geom_bar(colour='black',stat='identity',position='dodge') + 
  theme_bw()+
  theme(axis.text.x = element_text(angle=45,hjust=1),
        plot.title = element_text(hjust=0.5))+
  ggtitle('Platform Sales') +
  scale_fill_brewer(palette = 'YlGnBu')
summary(df)
St <- df %>% group_by(State)

Ax = df %>% group_by(`School Sector`) %>% summarise(Count = n())
Ax
p3 = ggplot(aes(x =df$`School Sector` , y = Count , fill=Count) , data=Ax) +
  geom_bar(colour='black',stat='identity') +
  theme_bw()+
  theme(axis.text.x = element_text(angle=45,hjust=1) , 
        plot.title = element_text(hjust=0.5))+  ## title center
  ggtitle('Platform Count')+
  scale_fill_distiller(palette = 'RdYlBu') +
  ylab('Count')
p3
####error
As=df%>% group_by('School Sector'=='Catholic')
library(dplyr)
meta_citplus <- df %>%
  filter(df$`School Sector` == "Government")
plot(df$`Girls Enrolments`, type= "b")
meta_citplus
go = meta_citplus %>% group_by(State) %>% summarise(Count = n())
D = ggplot(aes(x = State , y = Count , fill=Count) , data=go) +
  geom_bar(colour='black',stat='identity') +
  theme_bw()+
  theme(axis.text.x = element_text(angle=45,hjust=1) , 
        plot.title = element_text(hjust=0.5))+  ## title center
  ggtitle('Government School City Wise')+
  scale_fill_distiller(palette = 'RdYlBu') +
  ylab('Count')
D
####################
met <- df %>%
  filter(df$`School Sector` == "Catholic")
met
ca = met %>% group_by(State) %>% summarise(Count = n())
k = ggplot(aes(x = State , y = Count , fill=Count) , data=ca) +
  geom_bar(colour='black',stat='identity') +
  theme_bw()+
  theme(axis.text.x = element_text(angle=45,hjust=1) , 
        plot.title = element_text(hjust=0.5))+  ## title center
  ggtitle('Catholic School City Wise ')+
  scale_fill_distiller(palette = 'RdYlBu') +
  ylab('Count')
k
#############################3
#Select NSW State for the K means 
NSW <- df %>%
  filter(df$`State` == "NSW")
NSW
###Analysis of State NSW 
na.omit(NSW)
Tp= NSW %>% group_by(`Geolocation`) %>% summarise(Count = n())
Y = ggplot(aes(x = NSW$`Geolocation` , y = Count , fill=Count) , data=Tp) +
  geom_bar(colour='black',stat='identity') +
  theme_bw()+
  theme(axis.text.x = element_text(angle=45,hjust=1) , 
        plot.title = element_text(hjust=0.5))+  ## title center
  ggtitle('Catholic School City Wise ')+
  scale_fill_distiller(palette = 'RdYlBu') +
  ylab('Count')
Y
dat<- df %>% select(6,17:31)
head(dat)
##To conclude all the numerical data
##Heat Map
#heatmap(heatmap(as.matrix(data)))
matrix<-data.matrix(dat)
matrix
nba_heatmap <- heatmap(matrix, Rowv=NA, Colv=NA, col = cm.colors(256), scale="column", margins=c(5,10))
corre<-df %>% select(17:31)
corre
corre<-data.matrix(corre)
correl<-cor(corre)
correl
library(corrplot)
corrplot(correl)
k_mea<-df %>% select(17:31)
library("readxl")
df1 <- read_excel("D:/Big Data Analytics/Assignment/school-profile-2018.xlsx", sheet = "School Profile")
library(hablar)
library(dplyr)
df1 %>% rationalize()
na.omit(df1)
k<- df1 %>% select(17:30)
df2<-scale(k)
df2 %>% rationalize()
na.omit(df2)
library(NbClust)
set.seed(1234)
nc <- NbClust(df2, min.nc=2, max.nc=15, method="kmeans")

