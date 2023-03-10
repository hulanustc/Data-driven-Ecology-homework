rm(list = ls())#清空输出
library(datasets)
library(ggplot2)
help("datasets")

?CO2
#Plant
#an ordered factor with levels Qn1 < Qn2 < Qn3 < ... < Mc1 
#giving a unique identifier for each plant.

#Type
#a factor with levels Quebec Mississippi giving the origin of the plant

#Treatment
#a factor with levels nonchilled chilled

#conc
#a numeric vector of ambient carbon dioxide concentrations (mL/L).

#uptake
#a numeric vector of carbon dioxide uptake rates (\mu\mbox{mol}/m^2μmol/m2sec).

data(CO2, package = "datasets")
data_CO2 = CO2
data_CO2$conc = as.character(CO2$conc)

#绘制散点图
plot(CO2$uptake,type = "b")

#绘制堆叠柱状图
ggplot( data_CO2, aes( x = conc , weight = uptake, fill = Plant)) +
  geom_bar( position = "stack") +theme(panel.border = element_blank())+
  theme(axis.text.x = element_text(angle=-60)) 