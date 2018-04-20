library(haven)
library(SASxport)
library(dplyr)
library(foreign)
library(survey)
library(car)

#### Load dataset in the environment ###
me0015_m <- read_sas("C:/BRFSS_MIKE/me0015_m.sas7bdat")
llcp2015 <- read_sas("C:/BRFSS_MIKE/llcp2015.sas7bdat")
FlightDelay <- read_sas("C:/BRFSS_MIKE/FlightDelays.csv")
bcl_data <- read_sas("C:/BRFSS_MIKE/bcl-data.csv")

tempdataset <- me0015_m
datap1 <- me0015_m

datap <- subset(datap1,select = c(`_STSTR`, `_LLCPWT`,`_FINALQ1`,`_FINALWT`,`_LCPWTV1`,`_LCPWTV2`,`_LANDWT`, `_psu`, `GENHLTH`,`_AGEG5YR`, `year`, `county`, `SEX`,`iyear`,`_AGE65YR`,`_AGEG_`,`PHYSHLTH`,`POORHLTH`))

dat2015 <- dplyr::filter(datap1, year == '2015')
mydesign2015 <- svydesign(id = ~`_psu`, strata = ~`_STSTR` , weight = ~`_LLCPWT`,data = dat2015, nest = TRUE )
dat2014 <- dplyr::filter(datap1, year == '2014')
mydesign2014 <- svydesign(id = ~`_psu`, strata = ~`_STSTR` , weight = ~`_LLCPWT`,data = dat2014, nest = TRUE )
dat2013 <- dplyr::filter(datap1, year == '2013')
mydesign2013 <- svydesign(id = ~`_psu`, strata = ~`_STSTR` , weight = ~`_LLCPWT`,data = dat2013, nest = TRUE )
dat2012 <- dplyr::filter(datap1, year == '2012')
mydesign2012 <- svydesign(id = ~`_psu`, strata = ~`_STSTR` , weight = ~`_LLCPWT`,data = dat2012, nest = TRUE )
dat2011 <- dplyr::filter(me0015_m, year == '2011')
mydesign2011 <- svydesign(id = ~`_psu`, strata = ~`_STSTR` , weight = ~`_LLCPWT`,data = dat2011, nest = TRUE )
dat2010 <- dplyr::filter(me0015_m, year == '2010')


#Format data general health
GENHELTH <- factor(dat2015$GENHLTH, levels=c(1, 2, 3, 4,5,7,9),labels=c("Excellent", "Very good", "Good","Fair","Poor","Don't know/Not sure","Refused"))
# Format data county
COUNTY <- factor(dat2015$county, levels=c(1,2,3,4,5,6,7,8,9,10,11,12,13,14,15,16),labels=c("Androscoggin", "Aroostook", "Cumberland","Franklin","Hancock","Kennebec","Knox","Lincoln","Oxford","Penobscot","Piscataquis","Sagadahoc","Somerset","Waldo","Washington","York"))
# Format Gender
GENDER <- factor(dat2015$SEX, levels=c(1, 2),labels=c("Male", "Female"))
# Format Age
AGE <- factor(dat2015$`_AGEG5YR`, levels=c(1, 2, 3, 4,5,6,7,8,9,10,11,12,13,14),labels=c("18-24", "25-29", "30-34","35-39","40-44","45-49","50-54","55-59","60-64","65-69","70-74","75-79","80+","Refused"))


# Survey Means


mydesign2 <- svydesign(id = ~`_psu`, strata = ~`_STSTR` ,	weight = ~`_LLCPWT`,data = dat2015, nest = TRUE )
