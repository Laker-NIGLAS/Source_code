##Variation partitioning analysis of the relative contributions of lake geomorphology (lake), land use, climate, and trophic variables to the response of lake water temperatures to air temperature


library(vegan)
library(tidyverse)
#弹出窗口自己选文件

dat <- as.data.frame(read.csv(file.choose()))
dat1 <- na.omit(dat)

TAS1 <- dat1[,28]
LakeC1 <- dat1[,2:8]
Land1 <- dat1[,9:15]
Meto1 <- dat1[,16:26]
Trophic1 <- dat1[,27]

mod1 <- varpart(TAS1, LakeC1, Land1, Meto1, Trophic1) ###群落数据进行mod <- varpart(mite, mite.env, mite.pcnm, transfo="hel")
mod1

plot(mod1)

####Removed the insignificant lake trophic varibale (Secchi) to reanalysis the data
TAS2 <- dat[,28]
LakeC2 <- dat[,2:8]
Land2 <- dat[,9:15]
Meto2 <- dat[,16:26]

mod2 <- varpart(TAS2, LakeC2, Land2, Meto2) ###群落数据进行mod <- varpart(mite, mite.env, mite.pcnm, transfo="hel")
mod2

plot(mod2)

###################ETT-ATT################
####筛选变量######
# suppressMessages will hide message from loading package, it makes the output clearer suppressMessages(library(vegan))

# Create the full model
Factors1 <- dat[,2:26]

rda_full1 <- rda(TAS2~., data = Factors1)

# Create the zero model
rda_null1 <- rda(TAS2~1, data = Factors1)

# backward selection
# trace = 0 prevent ordistep print the selection progress from outputing to the console
# it makes the output clearer.
rda_back1 <- ordistep(rda_full1, direction = 'backward',trace = 0)

# forward selection
rda_frwd1 <- ordistep(rda_null1, formula(rda_full1), direction = 'forward',trace = 0)

# bothward selection 
rda_both1 <- ordistep(rda_null1, formula(rda_full1), direction = 'both',trace = 0)

rda_back1
rda_frwd1
rda_both1

# Divide the environmental data.frame by different categories.
Land3 <- Factors1 %>% select(Agriculture)
Meto3 <- Factors1 %>% select(WS, SR, SuAT, SpAT, Humidity)
Lake3 <- Factors1 %>% select(Area.depth, Elevation)

mod3 <- varpart(TAS2, Lake3, Land3, Meto3)
mod3

plot(mod3)


##VPA analysis followed up on significance tests
##Start by combining the environmental factors
mite.total1 <- data.frame(Lake3, Land3, Meto3, TAS2)

#X1
formula_X11 <- formula(TAS2 ~ Area.depth + Elevation + Condition(Agriculture) + 
                        Condition(WS) + Condition(SR) + Condition(SuAT) + Condition(SpAT) + Condition(Humidity))    
#X2
formula_X21 <- formula(TAS2 ~ Condition(Area.depth) + Condition(Elevation) + Agriculture + 
                        Condition(WS) + Condition(SR) + Condition(SuAT) + Condition(SpAT) + Condition(Humidity))
#X3
formula_X31 <- formula(TAS2 ~ Condition(Area.depth) + Condition(Elevation) + Condition(Agriculture) + 
                        WS + SR + SuAT + SpAT + Humidity)

##X1 and X2
formula_X1X21 <- formula(TAS2 ~ Area.depth:Elevation:Agriculture)

#X1 and X3
formula_X1X31 <- formula(TAS2 ~ Area.depth:Elevation:WS:SR:SuAT:SpAT:Humidity)

#X2 and X3
formula_X2X31 <- formula(TAS2 ~ Agriculture:WS:SR:SuAT:SpAT:Humidity)                         
#X1, X2 and X3
formula_X1X2X31 <- formula(TAS2 ~ Area.depth:Elevation:Agriculture:WS:SR:SuAT:SpAT:Humidity)     

##The partial RDA was used to test 999 displacements, and finally the significance of the model was obtained
anova(rda(formula_X11, data=mite.total1))
anova(rda(formula_X21, data=mite.total1))
anova(rda(formula_X31, data=mite.total1))
anova(rda(formula_X1X21, data=mite.total1))
anova(rda(formula_X1X31, data=mite.total1))
anova(rda(formula_X2X31, data=mite.total1))
anova(rda(formula_X1X2X31, data=mite.total1))


###################HTT-ATT################
dat2 <- as.data.frame(read.csv(file.choose()))
dat3 <- na.omit(dat2)


TAS4 <- dat3[,28]
LakeC4 <- dat3[,2:8]
Land4 <- dat3[,9:15]
Meto4 <- dat3[,16:26]
Trophic4 <- dat3[,27]

Factors4 <- dat3[,2:27]

mod4 <- varpart(TAS4, LakeC4, Land4, Meto4, Trophic4) ###群落数据进行mod <- varpart(mite, mite.env, mite.pcnm, transfo="hel")
mod4

plot(mod4)

####筛选变量######
# suppressMessages will hide message from loading package, it makes the output clearer suppressMessages(library(vegan))

# Create the full model
rda_full2 <- rda(TAS4~., data = Factors4)

# Create the zero model
rda_null2 <- rda(TAS4~1, data = Factors4)

# backward selection
# trace = 0 prevent ordistep print the selection progress from outputing to the console
# it makes the output clearer.
rda_back2 <- ordistep(rda_full2, direction = 'backward',trace = 0)

# forward selection
rda_frwd2 <- ordistep(rda_null2, formula(rda_full2), direction = 'forward',trace = 0)

# bothward selection 
rda_both2 <- ordistep(rda_null2, formula(rda_full2), direction = 'both',trace = 0)

rda_back2
rda_frwd2
rda_both2

# Divide the environmental data.frame by different categories.

Land5 <- Factors4 %>% select(Forest)
Meto5 <- Factors4 %>% select(SuAT,WiAT)
Trophic5 <- Factors4 %>% select(Secchi_trend)

mod5 <- varpart(TAS4, Land5, Meto5, Trophic5) ###群落数据进行mod <- varpart(mite, mite.env, mite.pcnm, transfo="hel")
mod5

plot(mod5)

##VPA分析后续的显著性检验
##首先将两个环境因子合并
mite.total2 <- data.frame(Land5, Meto5, Trophic5, TAS4)

##描述partial RDA的公式
#X1
formula_X12 <- formula(TAS4 ~ Forest + Condition(SuAT) + Condition(WiAT) + Condition(Secchi_trend))    
#X2
formula_X22 <- formula(TAS4 ~ Condition(Forest) + SuAT + WiAT + Condition(Secchi_trend))
#X3
formula_X32 <- formula(TAS4 ~ Condition(Forest) + Condition(SuAT) + Condition(WiAT) + Secchi_trend)
#X1和X2
formula_X1X22 <- formula(TAS4 ~ Forest:SuAT:WiAT)

#X1和X3
formula_X1X32 <- formula(TAS4 ~ Forest:Secchi_trend)

#X2和X3
formula_X2X32 <- formula(TAS4 ~ SuAT:WiAT:Secchi_trend)

#X1, X2 and X3
formula_X1X2X32 <- formula(TAS4 ~ Forest:SuAT:WiAT:Secchi_trend)   

##利用partial RDA进行999次的置换检验，最后得出模型的显著性
anova(rda(formula_X12, data=mite.total2))
anova(rda(formula_X22, data=mite.total2))
anova(rda(formula_X32, data=mite.total2))
anova(rda(formula_X1X22, data=mite.total2))
anova(rda(formula_X1X32, data=mite.total2))
anova(rda(formula_X2X32, data=mite.total2))
anova(rda(formula_X1X2X32, data=mite.total2))


