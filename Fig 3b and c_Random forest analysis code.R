#Choose data file
otu <- as.data.frame(read.csv(file.choose()))

otu1 <- na.omit(otu)

library(randomForest)
library(rfPermute)

from_reg1 <- ETT_ATT ~ Area + Area.depth + Max_depth + Volume + Res_time + 
  Elevation + Wshd + Agriculture + Development + Water + Forest + 
  Wetland + Grass + Shrubland + WS + TSP + SR + LR + H + LE + SuAT +
  SpAT + FaAT + WiAT  + Humidity + Secchi_trend

from_reg1

set.seed(123)

otu_forest1 <- randomForest(from_reg1, data = otu1, importance = TRUE, ntree = 500)   

otu_forest1

###Importance of variables in explaining the ETT-ATT explored using random forest analysis
set.seed(123)
otu_rfP1 <- rfPermute(from_reg1, data = otu1, importance = TRUE, ntree = 500, nrep = 1000, num.cores = 1)
otu_rfP1
#The mean squared error (MSE)
importance_otu.scale1 <- data.frame(importance(otu_rfP1, scale = TRUE), check.names = FALSE)
importance_otu.scale1

library(A3)
#model.fn=randomForest 调用随机森林的方法进行运算
#p.acc=0.001 表示基于 1000 次随机置换获得对 p 值的估计，p.acc 值越小代表置换次数越多，运算也就越慢，因此如果对全模型 p 值不是很迫切的话还是慎用
#model.args 用于传递参数给 randomForest()，因此里面的参数项根据 randomForest() 的参数项而定，具体可 ?randomForest
#其它详情可 ?a3 查看帮助 
set.seed(123)
otu_forest.pval <- a3(from_reg1, data = otu1, model.fn = randomForest, p.acc = 0.001, model.args = list(importance = TRUE, ntree = 500))
otu_forest.pval



####计算HTT_ATT
otu <- as.data.frame(read.csv(file.choose()))
otu2 <- na.omit(otu)

from_reg2 <- HTT_ATT ~ Area + Max_depth + Area.depth + Volume + Res_time + 
  Elevation + Wshd + Agriculture + Development + Forest + Water + 
  Wetland + Grass + Shrubland + WS + TSP + SR + LR + H + LE + 
  SuAT + SpAT + FaAT + WiAT + Humidity + Secchi_trend

from_reg2

set.seed(123)

otu_forest2 <- randomForest(from_reg2, data = otu2, importance = TRUE, ntree = 500)   

otu_forest2

###Importance of variables in explaining the HTT-ATT explored using random forest analysis
set.seed(123)
otu_rfP2 <- rfPermute(from_reg2, data = otu2, importance = TRUE, ntree = 500, nrep = 1000, num.cores = 1)
otu_rfP2
#The mean squared error (MSE)
importance_otu.scale2 <- data.frame(importance(otu_rfP2, scale = TRUE), check.names = FALSE)
importance_otu.scale2

####A3 PACKAGE
set.seed(123)
otu_forest.pval <- a3(from_reg2, data = otu2, model.fn = randomForest, p.acc = 0.001, model.args = list(importance = TRUE, ntree = 500))
otu_forest.pval

