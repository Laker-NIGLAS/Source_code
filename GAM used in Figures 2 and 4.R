
########Generalized additive model analysis##########


####The scatter distribution analyzed by GAM in Figure 2

library(gam)####用gam函数统计的参数更少且图形没有阴影是虚线

Data <- as.data.frame(read.csv(file.choose()))

gam1 <- gam(ETT ~ s(ATT),data=Data)

summary(gam1)

Data1 <- na.omit(Data)

gam2 <- gam(HTT ~ s(ATT),data=Data1)

summary(gam2)

gam3 <- gam(HTT ~ s(ETT),data=Data1)

summary(gam3)

plot(gam1,se=T,col=c('red'),xlim=c(-2, 2.5),ylim=c(-5, 5), shade = TRUE, residuals = TRUE)
plot(gam2,se=T,col=c('red'),xlim=c(-1.5, 1.5),ylim=c(-4, 3), shade = TRUE, residuals = TRUE)
plot(gam3,se=T,col=c('red'),xlim=c(-2, 3),ylim=c(-4, 3), shade = TRUE, residuals = TRUE)


####The scatter distribution analyzed by GAM in Figure 4

library(gam)

Data2 <- as.data.frame(read.csv(file.choose()))

gam4 <- gam(ETT_ATT ~ s(Secchi),data=Data2)

summary(gam4)

Data3 <- na.omit(Data2)

gam5 <- gam(HTT_ATT ~ s(Secchi),data=Data3)

summary(gam5)

plot(gam4,se=T,col=c('red'),xlim=c(-0.5, 1.4),ylim=c(-3.5, 3.5), shade = TRUE, residuals = TRUE)
plot(gam5,se=T,col=c('red'),xlim=c(-0.5, 1.4),ylim=c(-3.9, 2.99), shade = TRUE, residuals = TRUE)


