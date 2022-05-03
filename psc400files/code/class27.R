
### PSC 400, Spring 2022
### Week 14, Monday 5/2



### MORE ON GRAPHS

rm(list=ls(all=TRUE))

library(grDevices)

data <- read.csv("~/Dropbox/Teaching/2022_1_PSC_400/classes/week_4/data/qog_bas_cs_jan22.csv")


data$regimetype <- NA
data$regimetype[data$p_polity2>=6] <- "Democracy"
data$regimetype[data$p_polity2<=-6] <- "Autocracy"
data$regimetype[data$p_polity2<=5 & data$p_polity2>=-5] <- "Mixed"


# now look at bci_bci and rsf_pfi (press freedom)

plot(data$rsf_pfi, data$bci_bci, pch=16)

# now look at bci_bci and rsf_pfi for dem, aut, mixed
plot(data$rsf_pfi, data$bci_bci, pch=16, type="n")
points(data$rsf_pfi[data$regimetype=="Democracy"], data$bci_bci[data$regimetype=="Democracy"], pch=16, col="#66c2a5")
points(data$rsf_pfi[data$regimetype=="Autocracy"], data$bci_bci[data$regimetype=="Autocracy"], pch=16, col="#fc8d62")
points(data$rsf_pfi[data$regimetype=="Mixed"], data$bci_bci[data$regimetype=="Mixed"], pch=16, col="#8da0cb")

legend("topleft", legend=c("Democracy", "Autocracy", "Mixed"), pch=c(16, 16, 16), col=c("#66c2a5", "#fc8d62", "#8da0cb"), bty="n")

grid()



plot(data$rsf_pfi, data$bci_bci, pch=16, type="n")
grid(lwd=2)
points(data$rsf_pfi[data$regimetype=="Democracy"], data$bci_bci[data$regimetype=="Democracy"], pch=16, col="#66c2a5")
points(data$rsf_pfi[data$regimetype=="Autocracy"], data$bci_bci[data$regimetype=="Autocracy"], pch=16, col="#fc8d62")
points(data$rsf_pfi[data$regimetype=="Mixed"], data$bci_bci[data$regimetype=="Mixed"], pch=16, col="#8da0cb")

legend("topleft", legend=c("Democracy", "Autocracy", "Mixed"), pch=c(16, 16, 16), col=c("#66c2a5", "#fc8d62", "#8da0cb"), bty="n")


plot(data$rsf_pfi, data$bci_bci, pch=16, type="n")
rect(par("usr")[1], par("usr")[3], par("usr")[2], par("usr")[4], col = "#f7f7f7")
grid(col="white", lwd=3)
points(data$rsf_pfi[data$regimetype=="Democracy"], data$bci_bci[data$regimetype=="Democracy"], pch=16, col="#66c2a5")
points(data$rsf_pfi[data$regimetype=="Autocracy"], data$bci_bci[data$regimetype=="Autocracy"], pch=16, col="#fc8d62")
points(data$rsf_pfi[data$regimetype=="Mixed"], data$bci_bci[data$regimetype=="Mixed"], pch=16, col="#8da0cb")

legend("topleft", legend=c("Democracy", "Autocracy", "Mixed"), pch=c(16, 16, 16), col=c("#66c2a5", "#fc8d62", "#8da0cb"), bty="n")


