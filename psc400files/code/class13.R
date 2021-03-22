
### Class Code from Mar 22

## some more on Obama 08-12

rm(list=ls(all=TRUE))

pres12 <- read.csv("~/Dropbox/Teaching/2021_PSC_400/classes/week_6/data/pres12.csv")

pres08 <- read.csv("~/Dropbox/Teaching/2021_PSC_400/classes/week_6/data/pres08.csv")

colnames(pres08) <- c("state.name", "state", "Obama08", "McCain08", "EV08")

colnames(pres12) <- c("state", "Obama12", "Romney12", "EV12")

pres <- merge(pres08, pres12, by="state")

plot(pres$Obama08, pres$Obama12, pch=16, xlab="Obama Vote Share 2008", ylab="Obama Vote Share 2012")


# explicitly add intercept
reg1 <- lm(Obama12 ~ 1 + Obama08, data=pres)
summary(reg1)

abline(reg1, col="red", lwd=1.5)


# intercept-only model
reg0 <- lm(Obama12 ~ 1, data=pres)
summary(reg0)

abline(reg0, col="blue", lwd=1.5)
# we predict much better using Obama08. Reduction in sum of sq errors frmo blue to red is R2

mean(pres$Obama12)






## female politicians and policy outcomes

rm(list=ls(all=TRUE))

womendata <- read.csv("~/Dropbox/Teaching/2021_PSC_400/classes/week_6/data/women.csv")

table(womendata$reserved)

mean(womendata$female[womendata$reserved==1])
mean(womendata$female[womendata$reserved==0])


reg1 <- lm(water ~ 1 + reserved, data=womendata)
summary(reg1)


mean(womendata$water[womendata$reserved==1]) - mean(womendata$water[womendata$reserved==0])



reg2 <- lm(irrigation ~ 1 + reserved, data=womendata)
summary(reg2)

mean(womendata$irrigation[womendata$reserved==1]) - mean(womendata$irrigation[womendata$reserved==0])




reg3 <- lm(water ~ 1, data=womendata)
summary(reg3)

mean(womendata$water)



summary(reg1)

mean(womendata$water[womendata$reserved==0])
mean(womendata$water[womendata$reserved==1])





## immigration attitudes

rm(list=ls(all=TRUE))

immidata <- read.csv("~/Dropbox/Teaching/2021_PSC_400/classes/week_7/data/immig.csv")

# 0=decrease a great deal, 1=increase a great deal (larger values=more suport for immigration)
prop.table(table(immidata$h1bvis.supp))

mean(immidata$h1bvis.supp, na.rm=T)

# 0=low implicit prejudice, 1=high implicit prejudice
hist(immidata$impl.prejud)



m.prejud <- lm(h1bvis.supp ~ impl.prejud, data=immidata)
summary(m.prejud)


m.female <- lm(h1bvis.supp ~ female, data=immidata)
summary(m.female)


m.both <- lm(h1bvis.supp ~ impl.prejud + female, data=immidata)
summary(m.both)



m.full <- lm(h1bvis.supp ~ impl.prejud + female + age + employed + nontech.whitcol + tech.whitcol, data=immidata)
summary(m.full)



