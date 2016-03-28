library(ggplot2)
T1<-read.csv(file="~/Desktop/DA/Assign6/Data/Chocolate.csv",header = T)
ggplot(T1,aes(x=Chocolate,y=Nobel))+geom_point(aes(colour=Country),alpha=0.5,position = 'jitter')+labs(x='consumption of chocolate per capita (kilogram per year)',y="Nobel Laureattes (per 10.million)",title='Correlation')
Retention.reg <- lm(T1$Nobel ~ T1$Chocolate, data = T1) 
summary(Retention.reg) 
cor(T1$Chocolate,T1$Nobel)
T1<-T1[T1$Country!="Switzerland",]
cor(T1$Chocolate,T1$Nobel)

T2<-read.csv(file="~/Desktop/DA/Assign6/Data/Alcohol.csv",header = T)
ggplot(T2,aes(x=Alcohol,y=Nobel))+geom_point(aes(colour=Country),alpha=0.5,position = 'jitter')+labs(x='consumption of alcohol capita (liter per year)',y="Nobel Laureattes (per 10.million)",title='Correlation')
Retention.reg <- lm(T2$Nobel ~ T2$Alcohol, data = T2) 
summary(Retention.reg) 
cor(T2$Alcohol,T2$Nobel)

T3<-read.csv(file="~/Desktop/DA/Assign6/Data/Cigarette.csv",header = T)
ggplot(T3,aes(x=Cigarette,y=Nobel))+geom_point(aes(colour=Country),alpha=0.5,position = 'jitter')+labs(x='consumption of cigarette capita (number per year)',y="Nobel Laureattes (per 10.million)",title='Correlation')
Retention.reg <- lm(T3$Nobel ~ T3$Cigarette, data = T3) 
summary(Retention.reg) 
cor(T3$Cigarette,T3$Nobel)

T4<-read.csv(file="~/Desktop/DA/Assign6/Data/Tea.csv",header = T)
ggplot(T4,aes(x=Tea,y=Nobel))+geom_point(aes(colour=Country),alpha=0.5,position = 'jitter')+labs(x='consumption of tea capita (kilogram per year)',y="Nobel Laureattes (per 10.million)",title='Correlation')
Retention.reg <- lm(T4$Nobel ~ T4$Tea, data = T4) 
summary(Retention.reg) 
cor(T4$Tea,T4$Nobel)

