rm(list = ls())

library(ggplot2)
T <- function(v){
  ##构造T函数
  len = length(v)
  c_all = matrix(c(1:(2*(len-1))),nrow = 2)
  m = matrix(c(1:(len-1)),nrow = 1)
  for (i in 1:(len-1)){
    c1 = sum(v[1:i])/i
    c2 = sum(v[(i+1):len])/(len-i)
    c_all[1,i] = c1
    c_all[2,i] = c2
    mse1 = rep(1,i)*c1
    mse2 = rep(1,(len-i))*c2
    
    m[i] = sum((v[1:i]-mse1)**2)+
      sum((v[(i+1):len]-mse2)**2)
  } 
  index = which(m==min(m))
  flag <- c_all[,index]
  result1 <- rep(1,index)*flag[1]
  result2 <-rep(1, (len-index))*flag[2]
  result = c(result1, result2)
  return(result)
}




#数据读取：没有csv所以自己照书敲的
df = matrix(c(1,2,3,4,5,6,7,8,9,10,5.56,5.70,5.91,6.4,6.8,7.05,8.9,8.7,9,9.05), 
            nrow= 10,ncol = 2)
l = nrow(df)
colnames(df) <- c("x", "y")
df <- as.data.frame(df)

LR = 0.1

prediction <- rep(0,10)
df$pred_1 <- mean(df$y)
df <- cbind(df[1],prediction,df[2:ncol(df)])

df$resd_1 <- df$y-df$pred_1
df$T_1 <- T(df$y)
df$f_1 <- df$T_1
df$SSE_1 <- sum((df$f_1-df$y)**2)
df$prediction <- df$T_1 
df$resd_2 <- df$y - df$prediction


sse_flag =  df$SSE_1[1]
##if(FALSE){}:多行注释
if(FALSE){
  nrounds <- 11
  for(i in 2:nrounds){
    df[[paste0("T_",i)]] <- T(df[[paste0("resd_",i)]])
    df[[paste0("f_",i)]] <- df[[paste0("f_",(i-1))]]+df[[paste0("T_",i)]] 
    df[[paste0("SSE_",i)]] <- sum((df[[paste0("f_",i)]]-df$y)**2)
    df$prediction <- df$prediction + df[[paste0("T_",i)]]
    df[[paste0("resd_",i+1)]]  <- df$y -df$prediction
    
  }
}
##

i=2
while(sse_flag > 0.17){
  df[[paste0("T_",i)]] <- T(df[[paste0("resd_",i)]])
  df[[paste0("f_",i)]] <- df[[paste0("f_",(i-1))]]+df[[paste0("T_",i)]] 
  df[[paste0("SSE_",i)]] <- sum((df[[paste0("f_",i)]]-df$y)**2)
  sse_flag <- df[[paste0("SSE_",i)]][1]
  df$prediction <- df$prediction + df[[paste0("T_",i)]]
  df[[paste0("resd_",i+1)]]  <- df$y -df$prediction
  i = i+1
}
runtime = i-1
SSE_data <- df$SSE_1
SSE_data = as.data.frame(SSE_data)
for(i in 2:runtime){
  SSE_data[[paste0("SSE_",i)]] = df[[paste0("SSE_",i)]]
}
SSE_table <- SSE_data[1,]








