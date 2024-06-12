library(tidyverse)

## Plotting multiple with dummy data

x<-c(0:10)
y<-x
y_1<-exp(x)
y_2=x^3

df<-as.data.frame(cbind(x,y,y_1,y_2))
 ## Think of x as your timestamp

ggplot()+geom_line(data = df,aes(x=x,y=y),col="black",linetype=3)+geom_line(data = df,aes(x=x,y=y_1),
                                                                 col="red")+
  geom_line(data = df,aes(x=x,y=y_2),col="green",linewidth=5)
