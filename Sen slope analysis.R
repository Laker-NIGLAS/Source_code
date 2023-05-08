##Trend°üµÄSen slope analysis
library(plyr)
library(trend)

#Input data
dat <- read.csv(file.choose())

#############################################


library(trend)
dat <- na.omit(dat)

res <- c()
for (ii in unique(dat$lake_id)) {
  dat.ii <- dat[dat$lake_id==ii,]
  p <- sens.slope(dat.ii$humidity)
  res <- rbind(res, data.frame(ii,p$p.value,p$estimates))
}
names(res) <- c("Lake name", "p.value", "Sen's slope")

write.csv(res, 'Humidity trend.csv', row.names = FALSE)
