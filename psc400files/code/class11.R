
### Class Code from Mar 15


## UN Assembly voting

rm(list=ls(all=TRUE))

unvoting <- read.csv("~/Dropbox/Teaching/2021_PSC_400/classes/week_5/data/unvoting.csv")

idealpoint.med <- tapply(unvoting$idealpoint, unvoting$Year, median, na.rm=T)
plot(names(idealpoint.med), idealpoint.med, type="l", lwd=3, xlab="Year", ylab="Median Idealpoint")

PctAgreeUS.med <- tapply(unvoting$PctAgreeUS, unvoting$Year, median, na.rm=T)
plot(names(PctAgreeUS.med), PctAgreeUS.med, type="l", lwd=3, xlab="Year", ylab="Median Percentage of Votes with US")

plot(idealpoint.med, PctAgreeUS.med, pch=16, xlab="Median Idealpoint", ylab="Median Percentage of Votes with US")

cor(idealpoint.med, PctAgreeUS.med)

cor(idealpoint.med, PctAgreeUS.med, use="complete.obs")



## Facial competence and vote share

rm(list=ls(all=TRUE))

facedata <- read.csv("~/Dropbox/Teaching/2021_PSC_400/classes/week_6/data/face.csv")

facedata$d.share <- facedata$d.votes/(facedata$d.votes + facedata$r.votes)

plot(facedata$d.comp, facedata$d.share, pch=16, xlab="Competence Score for Democrats", ylab="Democratic Vote Share")

cor(facedata$d.comp, facedata$d.share)



reg1 <- lm(d.share ~ d.comp, data=facedata)
reg1

coef(reg1)
coef(reg1)[1]
coef(reg1)[2]


plot(facedata$d.comp, facedata$d.share, pch=16, xlab="Competence Score for Democrats", ylab="Democratic Vote Share")
abline(reg1, col="red", lwd=3)


summary(reg1)



### ELection results in 08 and 12


rm(list=ls(all=TRUE))

pres12 <- read.csv("~/Dropbox/Teaching/2021_PSC_400/classes/week_6/data/pres12.csv")

pres08 <- read.csv("~/Dropbox/Teaching/2021_PSC_400/classes/week_6/data/pres08.csv")

colnames(pres08) <- c("state.name", "state", "Obama08", "McCain08", "EV08")

colnames(pres12) <- c("state", "Obama12", "Romney12", "EV12")

pres <- merge(pres08, pres12, by="state")


