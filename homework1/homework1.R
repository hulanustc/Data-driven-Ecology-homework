rm(list = ls())#清空输出

#从"ade4"数据集中读取doubs数据

library(dplyr)
library("ade4")
library(tidyverse)

F1 <- function(env_tb){
  #3.1
  #One of the columns is dfs. 
  #It indicates the distance from sources. 
  #Extract and remain the data of the dfs with more than 1000 km.
  env_tb1<-env_tb[which(env_tb$dfs>1000),]
  
  #3.2
  #Only interested in these columns: 
  #site, dfs, slo, flo, pH, nit, oxy.
  #Select these columns for further analysis.
  index1 <- which(colnames(env_tb)=="site")
  index2 <- which(colnames(env_tb)=="dfs")
  index3 <- which(colnames(env_tb)=="slo")
  index4 <- which(colnames(env_tb)=="flo")
  index5 <- which(colnames(env_tb)=="pH")
  index6 <- which(colnames(env_tb)=="nit")
  index7 <- which(colnames(env_tb)=="oxy")
  index <- c(index1,index2,index3,index4,index5,index6,index7)
  env_tb2<-env_tb[,index]
  return(env_tb2)
}

F2 <- function(env_tb2){
  #3.3
  #Some column names are not intuitive.
  #Rename them as follows: 
  #dfs to distsour, slo to slope,flo to flowrate,nit to nitrogen, oxy to oxygen.
  index_new <- colnames(env_tb2)
  index_new[which(index_new=="dfs")] <- "distour"
  index_new[which(index_new=="slo")] <- "slop"
  index_new[which(index_new=="flo")] <- "flowrate"
  index_new[which(index_new=="nit")] <- "nitrogen"
  index_new[which(index_new=="oxy")] <- "oxygen"
  env_tb3 <- env_tb2
  colnames(env_tb3) <- index_new 
  
  return(env_tb3)
}

F3 <- function(env_tb3){
  #3.4
  #Order the data.
  #Arrange the data first by slope in ascending order, 
  #and then by pH in descending order.
  env_tb4 <- env_tb3[order(env_tb3$slop, decreasing = FALSE),]
  env_final <- env_tb4[order(env_tb4$pH, decreasing = TRUE),]
  return(env_final)
}



#1. 
#Loading libraries of tidyverse and ade4, 
#as well as the doubs data into R, 
#and checking what the data looks like and the class of the data. 
data(doubs, package = "ade4")
?doubs
class(doubs)
doubs
length(doubs)
env <- doubs$env
class(env)
env_row <- nrow(env)
env_col <- ncol(env)



#2.
#Turning the row names into a column called site, 
#then convert the data frame to a tibble, named it env_tb
site <- rownames(env)
env_new <- cbind(site, env)
env_tb <- tibble(env_new)




#3.
#Concatenating several steps with %>% pipe, 
#and name the final variable as env_final.

env_final <- (((env_tb %>% F1()) %>% F2()) %>% F3())
