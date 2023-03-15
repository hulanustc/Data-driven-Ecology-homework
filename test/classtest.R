rm(list = ls())#清空输出
library(datasets)
library(ggplot2)


# Create the data for the chart
H <- c(7,12,28,3,41)
M <- c("Mar","Apr","May","Jun","Jul")

# Plot the bar chart 
barplot(H,names.arg=M,xlab="Month",ylab="Revenue",col="blue",
main="Revenue chart",border="red")



# Obtain the data set "mtcars"
input <- mtcars[,c('mpg','cyl')]

# Plot the chart.
boxplot(mpg ~ cyl, data = mtcars, 
   xlab = "Number of Cylinders",
  ylab = "Miles Per Gallon", 
   main = "Mileage Data",
   notch = TRUE, 
   varwidth = TRUE, 
   col = c("green","yellow","purple"),
   names = c("High","Medium","Low")
)



v <- c(9,13,21,8,36,22,12,41,31,33,19)

# Create the histogram.
hist(v,xlab = "Weight",col = "green",border = "red", xlim = c(0,40), ylim = c(0,5), breaks = 5)


# Create the data for the chart.
v <- c(7,12,28,3,41)
t <- c(14,7,6,19,3)

# Plot the bar chart.
plot(v,type = "o",col = "red", xlab = "Month", ylab = "Rain fall", 
   main = "Rain fall chart")

lines(t, type = "o", col = "blue")



input <- mtcars[,c('wt','mpg')]

# Plot the chart for cars with weight between 2.5 to 5 and mileage between 15 and 30.
plot(x = input$wt,y = input$mpg,
   xlab = "Weight",
   ylab = "Milage",
   xlim = c(2.5,5),
   ylim = c(15,30),     
   main = "Weight vs Milage"
)


# Create a vector. 
x <- c(12,7,3,4.2,18,2,54,-21,8,-5)

# Find Mean.
result.mean <- mean(x)
print(result.mean)

# Create the vector.
x <- c(12,7,3,4.2,18,2,54,-21,8,-5)

# Find the median.
median.result <- median(x)
print(median.result)








##example

library(ade4)
data(doubs,package="ade4")
env<-doubs$env
spe<-doubs$fish #美元符号打不出来用大写的S代替了
spa<-doubs$xy #美元符号打不出来用大写的S代替了

spe[1:5,1:10] #spe的一到五行和一到十列
head(spe) #spe的前六行
nrow(spe)
ncol(spe)
dim(spe) #spe的行数，列数，维度
colnames(spe)
rownames(spe)#提取列名，行名
summary(spe)#对列变量进行描述性统计

range(spe)
range(spe) #多度范围
ab<-table(unlist(spe))
ab
barplot(ab,las=1,xlab = "Abundance class",ylab = "Frequency",col = gray(5:0/5))
#每种多度值的数量并绘制柱状图

sum(spe=0)
sum(spe=0)/(nrow(spe)*ncol(spe))#多度为0所占的比例

plot(spa, asp=1, type="n", main="样方位置",xlab="x坐标 (km)", ylab="y坐标 (km)")
lines(spa, col="light blue")
text(spa, row.names(spa), cex=0.8, col="red")
text(70, 10, "上游", cex=1.2, col="red")
text(20, 120, "下游", cex=1.2, col="red")

par(mfrow=c(2,2))
plot(spa, asp=1, col="brown", cex=speSAbbr, main="欧鳊",
     xlab="x坐标 (km)", ylab="y坐标 (km)")
lines(spa, col="light blue")
plot(spa, asp=1, col="brown", cex=speSCogo, main="茴鱼",
     xlab="x坐标 (km)", ylab="y坐标 (km)")
lines(spa, col="light blue")
plot(spa, asp=1, col="brown", cex=speSBaba, main="鲃鱼",
     xlab="x坐标 (km)", ylab="y坐标 (km)")
lines(spa, col="light blue")
plot(spa, asp=1, col="brown", cex=speSNeba, main="褐鳟",
     xlab="x坐标 (km)", ylab="y坐标 (km)")
lines(spa, col="light blue")

spe.pres <- apply(spe > 0, 2, sum) #按列求和
sort(spe.pres) #升序排列
spe.relf <- 100*spe.pres/nrow(spe) #频数百分比
round(sort(spe.relf), 1) #一位小数
par(mfrow=c(1,2)) # 将绘图窗口垂直一分为二
hist(spe.pres, main="物种出现数", right=FALSE, las=1,xlab="出现数", ylab="物种数量" ,breaks=seq(0,30,by=5), col="bisque")
hist(spe.relf, main="物种相对频度", right=FALSE, las=1, xlab="出现率(%)", ylab="物种数量",breaks=seq(0, 100, by=10), col="bisque")


sit.pres <- apply(spe > 0, 1, sum)
sort(sit.pres)
par(mfrow=c(1,2))
plot(sit.pres,type="s", las=1, col="gray",
     main="物种丰富度-上下游的梯度",
     xlab="样方沿着河流的位置", ylab="物种丰富度")
text(sit.pres, row.names(spe), cex=.8, col="red")#不同样方的物种丰富度
plot(spa, asp=1, main="物种丰富度地图", pch=21, col="white",
     bg="brown", cex=5*sit.pres/max(sit.pres), xlab="x坐标 (km)",
     ylab="y坐标 (km)")
lines(spa, col="light blue")#物种丰富度地图

library(vegan)
N0 <- rowSums(spe > 0)
H <- diversity(spe)
N1 <- exp(H)
N2 <- diversity(spe, "inv")
J <- H/log(N0)
E1 <- N1/N0
E2 <- N2/N0
(divers <- data.frame(N0, H, N1, N2, E1, E2, J)) # 输出值分别是物种丰富度、Shannon熵指数、Shannon多样性指数、Simpson多样性指数、Pielou均匀度、Shannon均匀度（Hill比率）、Simpson均匀度











