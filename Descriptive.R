#Our working directory
setwd("D:/Research/HIV_BD Research")

#Required packages
library(readxl)
library(dplyr)
library(ggplot2)
library(car)
library(carData)
library(reshape2)

#Upload data
data<-read_excel("data_HIV_BD.xlsx", sheet="Normalized data")

##Box plots
df1<-data[4:15]#socio-demographic
df2<-data[16:21]#socio-economic
df3<-data[22:23]#Human development
boxplot(df1,col="blue")
boxplot(df2,col="turquoise")
boxplot(df3,col="darkred")

#correlation heatmap
#Data select
data1<-data[2:23]
str(data1)

#correlation
corr.data<-cor(data1)

#melt the correlation
melted_corr <- melt(corr.data)

#heatmap
ggplot(melted_corr, aes(x=Var1, y=Var2, fill=value)) +
  geom_tile(color = "white") +               # white grid lines
  scale_fill_gradient2(low = "blue", mid = "white", high = "red", 
                       midpoint = 0, limit = c(-1,1), space = "Lab",
                       name="Correlation") +
  theme_minimal() +                           # clean theme
  theme(axis.text.x = element_text(angle = 45, vjust = 1, hjust=1),
        axis.title = element_blank(),
        panel.grid = element_blank(),
        plot.title = element_text(hjust = 0.5, size=16)) +
  geom_text(aes(label = round(value,2)), color = "black", size = 3) +
  ggtitle("")
