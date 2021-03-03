
### Class Code from Mar 3


rm(list=ls(all=TRUE))

# codebook: https://www.qogdata.pol.gu.se/data/codebook_bas_jan21.pdf

data <- read.csv("~/Dropbox/Teaching/2021_PSC_400/classes/week_3/data/qog.csv")


# explore life expectancy
hist(data$wdi_lifexp, xlab="Life Expectancy at Birth", main="Life Expectancy at Birth")
hist(data$wdi_lifexp, xlab="Life Expectancy at Birth", main="Life Expectancy at Birth", freq=F)

plot(density(data$wdi_lifexp, na.rm=T), lwd=3, xlab="Life Expectancy at Birth", main="Life Expectancy at Birth")
hist(data$wdi_lifexp, add=T, freq=F)
abline(v=mean(data$wdi_lifexp, na.rm=T), lwd=3, lty=2)



# difference between male and female life expectancy
data$wdi_lifexp_diff <- data$wdi_lifexpf - data$wdi_lifexpm
hist(data$wdi_lifexp_diff, freq=F)

data$wdi_lifexp_avg <- (data$wdi_lifexpf + data$wdi_lifexpm)/2
hist(data$wdi_lifexp_avg, freq=F)




# CPI
hist(data$ti_cpi)

data$ti_cpi_max10 <- data$ti_cpi/10

data$ti_cpi_reverse <- (data$ti_cpi * (-1)) + 100

# rank =  lowest score gets 1, second lowest 2, etc. na.last = "keep" ensures that NAs in data$ti_cpi also become NA in new variable
# so this creates variable that ranks the least corrupt country first
data$ti_cpi_rank <- rank(data$ti_cpi_reverse, na.last = "keep")

data$ti_cpi_rank[data$cname=="United States"]




# GDP per capita
plot(density(data$mad_gdppc, na.rm=T))
data$mad_gdppc_log <- log(data$mad_gdppc)
	# be careful: if you have 0's in original variable, log(0) is NA. Instead use log(variable + 1)
plot(density(data$mad_gdppc_log, na.rm=T))




# regimetype
table(data$ht_regtype1)

# first way: ifelse
data$dem <- ifelse(data$ht_regtype1==100, "Democracy", "Not Democracy")
data$dem <- as.factor(data$dem)

# another way, works equally well
data$dem <- "Not Democracy"
data$dem[data$ht_regtype1==100] <- "Democracy"


