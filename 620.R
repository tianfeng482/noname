一、
1. 1）getwd()     2) setwd("D:/Rtest")     3) list.files()   4)       5) rm(x1);rm(x2)
2. 1) is.character()   2) as.numeric()    3)x <- c(Rtest1.png, Rtest2.png, ...,Rtest1000.png)
3. 1) rnorm(100, 2, 3)  2) 2) y1=x[5<=x<=10]  3) which(5<=x & x<=10)
4. 1) header=True     2)  sep=';'  3) skip=2
5. 1) x = paste("D:/Rtest",list.files('D:/Rtest'),sep="")  2)  y = matrix(NaN, nrow = 50, ncol = 10)
       3)   for (i in 1:10) {
            y[, i] = read.csv(file=x[i])
          }
6. 1) col = "red", lwd=2       2) xlab = "Rname", ylab = "Rscore"  3) test(x=3, y=2, "This is Rtest!")

二、
1. 
# 编写函数 对于单点
Rtest_f <- function(x){
  if(1<x & x<5)
    {return(2*x^2+1)}
  else
    return(3*x)
}
# 取x
x = seq(-10, 10, 0.5)
# sapply将单点函数映射到向量中
y = sapply(x, Rtest_f)
# 作图
plot(x, y, type = "o")


2. 
y = c(70, 67, 55, 52, 51, 52, 51, 60, 64)
x = c(1616.3, 1610.9, 1440.0, 1440.7, 1423.3, 1471.3, 1421.8, 1547.1, 1533.0)
# 建立最小二乘回归
model = lm(y~x)
# 查看细节
model$coefficients
# 最后结果
y = -78.94220079 + 0.09126505 *x

3.
x = c(10.4, 10.6, 10.1, 10.4, 10.5, 10.3, 10.3, 10.2, 10.9, 10.6, 10.8, 10.5, 10.7, 10.2, 10.7)
# 正态性检验
qqnorm(x)
qqline(x)
# 数据是近似正态的
# 相当于是z检验，方差已知
# 利用包BSDA中的函数
install.packages("BSDA")
library(BSDA)
z.test(x, mu=10.5, sigma.x = 0.15)
# 结果p值为0.6056，接受原假设，说明工作正常



4. 
