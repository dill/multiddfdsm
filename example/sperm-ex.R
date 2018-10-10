# sperm whale example

library(ggplot2)
library(mrds)
library(Distance)
library(dsm)

load("spermwhale.RData")
rm(dist)

# split the sperm whale data according to survey
obs1 <- subset(obs, Survey=="en04395")
obs2 <- subset(obs, Survey=="GU0403")

#par(mfrow=c(1,2))
#hist(obs1$distance)
#hist(obs2$distance)

# fit detection functions
df1 <- ds(obs1, truncation=6000)
df2 <- ds(obs2, truncation=8000)

#plot(df1)
#plot(df2)

segs$ddfobj <- 1
segs$ddfobj[segs$Survey=="GU0403"] <- 2

# build a dsm?
bigdsm <- dsm(count~s(x,y), list(df1, df2), segs, obs, family=tw())

# check predictions
predgrid$p2 <- predict(bigdsm, predgrid, off.set=predgrid$off.set)
pp <- ggplot(predgrid) +
  geom_tile(aes(x=x,y=y,fill=p2)) +
  scale_fill_viridis_c(limits=c(0, 3.5))
print(pp)


## compare

dfa <- ds(obs, truncation=8000)
adsm <- dsm(count~s(x,y), dfa, segs, obs, family=tw())

predgrid$p1 <- predict(adsm, predgrid, off.set=predgrid$off.set)
pp <- ggplot(predgrid) +
  geom_tile(aes(x=x,y=y,fill=p1)) +
  scale_fill_viridis_c(limits=c(0, 3.5))
print(pp)

## what if we don't use a detection function?
df_dum <- dummy_ddf(obs2$object, obs2$size, 5000)
bigdsm_strip <- dsm(count~s(x,y), list(df1, df_dum), segs, obs, family=tw())


predgrid$p3 <- predict(bigdsm_strip, predgrid, off.set=predgrid$off.set)
pp <- ggplot(predgrid) +
  geom_tile(aes(x=x,y=y,fill=p3)) +
  scale_fill_viridis_c(limits=c(0, 3.5))
print(pp)


# okay what about variance

# our model with 2 detection functions
vp <- dsm_varprop(bigdsm, predgrid)
# delta method to check
vg <- dsm.var.gam(bigdsm, predgrid, predgrid$off.set)

# all-in-one model
vp2 <- dsm_varprop(adsm, predgrid)
vg2 <- dsm.var.gam(adsm, predgrid, predgrid$off.set)


