### PSC 400, Spring 2024
### Week 5, Tuesday 2/20




rm(list=ls(all=TRUE))

data <- read.csv("~/Dropbox/Teaching/2022_1_PSC_400/classes/week_4/data/qog_bas_cs_jan22.csv")
# codebook: https://www.qogdata.pol.gu.se/data/codebook_bas_jan24.pdf



# CPI
hist(data$ti_cpi)
hist(data$ti_cpi, breaks=20)



data$ti_cpi_max10 <- data$ti_cpi/10

data$ti_cpi_reverse <- (data$ti_cpi * (-1)) + 100

# rank =  lowest score gets 1, second lowest 2, etc. na.last = "keep" ensures that NAs in data$ti_cpi also become NA in new variable
# so this creates variable that ranks the least corrupt country first
data$ti_cpi_rank <- rank(data$ti_cpi_reverse, na.last = "keep")

data$ti_cpi_rank[data$cname=="United States of America (the)"]




# GDP per capita
plot(density(data$mad_gdppc, na.rm=T))
data$mad_gdppc_log <- log(data$mad_gdppc)
	# be careful: if you have 0's in original variable, log(0) is NA. Instead use log(variable + 1)
plot(density(data$mad_gdppc_log, na.rm=T))




# # colonial origin
table(data$ht_colonial)


# first way: ifelse
data$colonized <- ifelse(data$ht_colonial==0, 0, 1)

# another way, works equally well
data$colonized <- NA
data$colonized[data$ht_colonial==0] <- 0
data$colonized[data$ht_colonial==1] <- 1
data$colonized[data$ht_colonial==2] <- 1
data$colonized[data$ht_colonial==3] <- 1
data$colonized[data$ht_colonial==4] <- 1
data$colonized[data$ht_colonial==5] <- 1
data$colonized[data$ht_colonial==6] <- 1
data$colonized[data$ht_colonial==7] <- 1
data$colonized[data$ht_colonial==8] <- 1
data$colonized[data$ht_colonial==9] <- 1
data$colonized[data$ht_colonial==10] <- 1


# a bit quicker
data$colonized <- NA
data$colonized[data$ht_colonial==0] <- 0
data$colonized[data$ht_colonial==1 | data$ht_colonial==2 | data$ht_colonial==3 | data$ht_colonial==4 | data$ht_colonial==5 | data$ht_colonial==6 | data$ht_colonial==7 | data$ht_colonial==8 | data$ht_colonial==9 | data$ht_colonial==10] <- 1

# or, more easily in this case
data$colonized <- NA
data$colonized[data$ht_colonial==0] <- 0
data$colonized[data$ht_colonial>0] <- 1




data$colonized2 <- ifelse(data$ht_colonial==0, "not colonized", "colonized")



# relationship between democracy and COVID fatality rate

data$covid_fatality <- data$jht_ccd/data$jht_ccc
#data$covid_fatality[data$jht_ccc==0] <- NA

data$p_polity2_bin <- ifelse(data$p_polity2>=6, 1, 0)

summary(data$covid_fatality[data$p_polity2_bin==1])
summary(data$covid_fatality[data$p_polity2_bin==0])

boxplot(covid_fatality ~ p_polity2_bin, data=data)

plot(data$p_polity2, data$covid_fatality)

