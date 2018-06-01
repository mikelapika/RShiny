

#brfss_design <- svydesign(id = ~`_psu`, strata = ~`_STSTR` ,	weight = ~`_LLCPWT`,data = brfss_15, nest = TRUE )

# total unweigted freq

# variable to analyze in this case GENHLTH will always be Var1, so we will replace Var1 by GENHLTH.
totfreq <- data.frame(addmargins( table(brfss_15$GENHLTH)))

####
#creaate sum column to help merge the total freq with freqency
totfreq$SEX <- "NA"
totfreq$SEX[totfreq$SEX == 'NA'] <- 'Sum'

#####
colnames(totfreq)  <- c('GENHLTH','Frequency','SEX')
totfreq$GENHLTH  <- factor(totfreq$GENHLTH , levels = c("Excellent","Very good", "Good", "Fair","Poor","Sum"))
totfreq <- data.frame(totfreq[order(totfreq$GENHLTH),])


# when you do a 2by2 cross table, the Var1 column name will be the name of the variable you are using and Var2 the option you choose
# unweighted frequencies by sex
frqsex <- data.frame(table(brfss_15$GENHLTH,brfss_15$SEX))
#frqsex <- data.frame(addmargins(table(brfss_15$GENHLTH,brfss_15$SEX)))
colnames(frqsex)  <- c('GENHLTH','SEX','Frequency')
frqsex <- data.frame(frqsex[order(frqsex$GENHLTH),])


 ## unweighted frequencies with totals
frqsex2 <- data.frame(addmargins(table(brfss_15$GENHLTH,brfss_15$SEX)))
colnames(frqsex2)  <- c('GENHLTH','SEX','Frequency')
frqsex2 <- data.frame(frqsex2[order(frqsex2$GENHLTH),])


# weighted frequency
weight.fr <- data.frame(svyby(~SEX, ~GENHLTH, brfss_design , svytotal,na.rm=TRUE))

###
Wght_freq <- data.frame(gather(weight.fr,se.SEXMale,se.SEXFemale,SEXMale,SEXFemale))
colnames(Wght_freq) <- c("GENHLTH","SEX","Wght_Frequency")


Std_Wght_freq <- data.frame(gather(weight.fr,SEXMale,SEXFemale,se.SEXMale,se.SEXFemale))
colnames(Std_Wght_freq) <- c("GENHLTH","SEX","Std_Err_Wght_Freq")

# weight freq order
Wght_freq$GENHLTH  <- factor(Wght_freq$GENHLTH , levels = c("Excellent","Very good", "Good", "Fair","Poor","Sum"))
Wght_freq$SEX  <- as.factor(Wght_freq$SEX)

#std weight freq order
Std_Wght_freq$GENHLTH  <- factor(Std_Wght_freq$GENHLTH , levels = c("Excellent","Very good", "Good", "Fair","Poor","Sum"))
Std_Wght_freq$SEX  <- as.factor(Std_Wght_freq$SEX )

levels(Wght_freq$SEX)[levels(Wght_freq$SEX)=="SEXMale"] <- "Male"
levels(Wght_freq$SEX)[levels(Wght_freq$SEX)=="SEXFemale"] <- "Female"
levels(Std_Wght_freq$SEX)[levels(Std_Wght_freq$SEX)=="se.SEXMale"] <- "Male"
levels(Std_Wght_freq$SEX)[levels(Std_Wght_freq$SEX)=="se.SEXFemale"] <- "Female"

# merge weighted freq and std weighted freq
mix_weight_freq <- data.frame(merge(Wght_freq,Std_Wght_freq, by=c("GENHLTH","SEX")))

# merge weighted freq and std weighted with his total

final_weight_freq <- rbind(mix_weight_freq,total_wght_freq)
final_weight_freq$GENHLTH  <- factor(final_weight_freq$GENHLTH , levels = c("Excellent","Very good", "Good", "Fair","Poor","Sum"))
final_weight_freq <- data.frame(final_weight_freq[order(final_weight_freq$GENHLTH),]) 

####
# Row percent and std err row percent
row_prct <- data.frame(svyby(~SEX , ~ GENHLTH, brfss_design,svymean, na.rm = TRUE))

Row_Percent <- data.frame(gather(row_prct,se.SEXMale,se.SEXFemale,SEXMale,SEXFemale))
colnames(Row_Percent) <- c("GENHLTH","SEX","Row_Percent")


Std_Err_Rw_Prct <- data.frame(gather(row_prct,SEXMale,SEXFemale,se.SEXMale,se.SEXFemale))
colnames(Std_Err_Rw_Prct) <- c("GENHLTH","SEX","Std_Err_Row_Perct")

# weight freq order
Row_Percent$GENHLTH  <- factor(Row_Percent$GENHLTH , levels = c("Excellent","Very good", "Good", "Fair","Poor","Sum"))
Row_Percent$SEX  <- as.factor(Row_Percent$SEX)

#std weight freq order
Std_Err_Rw_Prct$GENHLTH  <- factor(Std_Err_Rw_Prct$GENHLTH , levels = c("Excellent","Very good", "Good", "Fair","Poor","Sum"))
Std_Err_Rw_Prct$SEX  <- as.factor(Std_Err_Rw_Prct$SEX )

levels(Row_Percent$SEX)[levels(Row_Percent$SEX)=="SEXMale"] <- "Male"
levels(Row_Percent$SEX)[levels(Row_Percent$SEX)=="SEXFemale"] <- "Female"
levels(Std_Err_Rw_Prct$SEX)[levels(Std_Err_Rw_Prct$SEX)=="se.SEXMale"] <- "Male"
levels(Std_Err_Rw_Prct$SEX)[levels(Std_Err_Rw_Prct$SEX)=="se.SEXFemale"] <- "Female"

# Multiply by 100 to get the value in percentage
Row_Percent$Row_Percent <- Row_Percent$Row_Percent * 100
Std_Err_Rw_Prct$Std_Err_Row_Perct <- Std_Err_Rw_Prct$Std_Err_Row_Perct * 100
 

#########


#####   ################
# Percent and std_err percent (to work on it, split the column name)
#prct <- data.frame( svymean(~interaction(GENHLTH, SEX), design = brfss_design, na.rm = TRUE))
# percent and std err percent after refactor
rslt <- data.frame(svymean(~interaction(GENHLTH, SEX), design = brfss_design, na.rm = TRUE)) #percent
# create column name to extract the row name
rslt$ConfInt <- rownames(rslt) 
# separate row name into GENELTH and SEX
rslt2 <- separate(data = rslt, col = ConfInt, into = c("GENHLTH", "SEX"), sep = "\\.") #
rslt3 <- separate(data = rslt2, col = GENHLTH, into = c("CI", "GENHLTH"), sep = "\\)")
#rslt3 <- as.factor(rslt3)
rslt3 <- rslt3[ , -c(3)]
colnames(rslt3)  <- c('Percent','Std_Err_Percent','GENHLTH','SEX')
rslt3$GENHLTH  <- factor(rslt3$GENHLTH , levels = c("Excellent","Very good", "Good", "Fair","Poor","Sum"))
rslt3$Percent <- rslt3$Percent * 100
rslt3$Std_Err_Percent <- rslt3$Std_Err_Percent * 100
prct_std_prct <- data.frame(rslt3[order(rslt3$GENHLTH),]) 
##################################################################

# make the variable follow order for weight freq and std freq
Wght_freq <- data.frame(Wght_freq[order(Wght_freq$GENHLTH),])

Std_Wght_freq <- data.frame(Std_Wght_freq[order(Std_Wght_freq$GENHLTH),])

Row_Percent <- data.frame(Row_Percent[order(Row_Percent$GENHLTH),])

Std_Err_Rw_Prct <- data.frame(Std_Err_Rw_Prct[order(Std_Err_Rw_Prct$GENHLTH),])

#weighted Frequency Totals and std error weighted frequency totals
Var_order <- c("Excellent","Very good", "Good", "Fair","Poor","Sum1","Sum2")
total_wght_freq <- svytotal(~GENHLTH+~SEX, brfss_design, na.rm=TRUE) 
total_wght_freq <- data.frame(Var_order,total_wght_freq)
colnames(total_wght_freq) <- c("GENHLTH","Wght_Frequency","Std_Err_Wght_Freq")
total_wght_freq$Wght_Frequency <- round(total_wght_freq$Wght_Frequency,0)
total_wght_freq$Std_Err_Wght_Freq <- round(total_wght_freq$Std_Err_Wght_Freq,0)
#total_Std_Wght_freq$Wght_Frequency <- round(total_wght_freq$Wght_Frequency,0)
total_wght_freq <- data.frame(total_wght_freq)
#total_Std_Wght_freq <- data.frame(total_wght_freq)
# create sex colum to help merge with frequency
total_wght_freq$SEX <- "NA"
total_wght_freq$SEX[total_wght_freq$SEX == 'NA'] <- 'Sum'
total_wght_freq <- data.frame(total_wght_freq)

# Total percent and std error percent 
total_prct <- svymean(~GENHLTH+SEX, brfss_design, na.rm=TRUE)
total_prct <- data.frame(Var_order,total_prct)
colnames(total_prct) <- c("GENHLTH","Percent","Std_Err_Percent")
total_prct$Percent <- total_prct$Percent * 100
total_prct$Std_Err_Percent <- total_prct$Std_Err_Percent * 100
total_prct <- data.frame(total_prct)

###############################################
# confidence interval limit 95%

# 95% Confidence Limits for Percent (to work on it, split the column name)

Conf_int <- svymean(~interaction(GENHLTH, SEX), design = brfss_design, na.rm = TRUE) #percent
# conf_int

Conf_int <- data.frame(confint(svymean(~interaction(GENHLTH, SEX), design = brfss_design, na.rm = TRUE)))
# create column name to extract the row name
Conf_int$ConfInt <- rownames(Conf_int) 
# separate row name into GENELTH and SEX
Conf_int2 <- separate(data = Conf_int, col = ConfInt, into = c("GENHLTH", "SEX"), sep = "\\.") #
Conf_int3 <- separate(data = Conf_int2, col = GENHLTH, into = c("CI", "GENHLTH"), sep = "\\)")
#rslt3 <- as.factor(rslt3)
Conf_int3 <- Conf_int3[ , -c(3)]
colnames(Conf_int3)  <- c('Conf_Limits_Prcnt_2.5','Conf_Limits_Prcnt_97.5','GENHLTH','SEX')
Conf_int3$GENHLTH  <- factor(Conf_int3$GENHLTH , levels = c("Excellent","Very good", "Good", "Fair","Poor","Sum"))
Conf_int3$Conf_Limits_Prcnt_2.5 <- Conf_int3$Conf_Limits_Prcnt_2.5 * 100
Conf_int3$Conf_Limits_Prcnt_97.5 <- Conf_int3$Conf_Limits_Prcnt_97.5 * 100
Conf_int_prct <- data.frame(Conf_int3[order(Conf_int3$GENHLTH),]) 

#####
# confidence limit total
# 95% Confidence Limits for Percent (to work on it, split the column name)

Conf_intx <- data.frame(confint(svymean(~interaction(GENHLTH), design = brfss_design, na.rm = TRUE)))
# create column name to extract the row name
Conf_intx$ConfIntx <- rownames(Conf_intx) 
# separate row name into GENELTH 
Conf_intx2 <- separate(data = Conf_intx, col = ConfIntx, into = c("CIX", "GENHLTH"), sep = "\\)") #

Conf_intx2 <- Conf_intx2[ , -c(3)]
colnames(Conf_intx2)  <- c('Conf_Limits_Prcnt_2.5','Conf_Limits_Prcnt_97.5','GENHLTH')
Conf_intx2$GENHLTH  <- factor(Conf_intx2$GENHLTH , levels = c("Excellent","Very good", "Good", "Fair","Poor","Sum"))
Conf_intx2$Conf_Limits_Prcnt_2.5 <- Conf_intx2$Conf_Limits_Prcnt_2.5 * 100
Conf_intx2$Conf_Limits_Prcnt_97.5 <- Conf_intx2$Conf_Limits_Prcnt_97.5 * 100
Conf_intx_prct <- data.frame(Conf_intx2[order(Conf_intx2$GENHLTH),]) 

#############################################################
# 95 % confidence limit for Row percent
rw_prct_CI <- data.frame(confint(svyby(~SEX , ~ GENHLTH, brfss_design,svymean, na.rm = TRUE)))
rw_prct_CI$ConfInt <- rownames(rw_prct_CI) 
# separate row name into GENELTH and SEX
rw_prct_CI2 <- separate(data = rw_prct_CI, col = ConfInt, into = c("GENHLTH", "SEX"), sep = "\\:") #
colnames(rw_prct_CI2)  <- c('Conf_Limits_Row_Prcnt_2.5','Conf_Limits_Row_Prcnt_97.5','GENHLTH','SEX')

rw_prct_CI2$GENHLTH  <- factor(rw_prct_CI2$GENHLTH , levels = c("Excellent","Very good", "Good", "Fair","Poor","Sum"))
rw_prct_CI2$SEX  <- factor(rw_prct_CI2$SEX , levels = c("SEXMale","SEXFemale"))
levels(rw_prct_CI2$SEX)[levels(rw_prct_CI2$SEX)=="SEXMale"] <- "Male"
levels(rw_prct_CI2$SEX)[levels(rw_prct_CI2$SEX)=="SEXFemale"] <- "Female"
rw_prct_CI2$Conf_Limits_Row_Prcnt_2.5 <- rw_prct_CI2$Conf_Limits_Row_Prcnt_2.5 * 100
rw_prct_CI2$Conf_Limits_Row_Prcnt_97.5 <- rw_prct_CI2$Conf_Limits_Row_Prcnt_97.5 * 100
rw_Conf_int_prct <- data.frame(rw_prct_CI2[order(rw_prct_CI2$GENHLTH),])

#############################################################
# merge total of unweigted freq and weighted freq and std error
alltotal1 <- data.frame( merge(totfreq, total_wght_freq, by=c("GENHLTH"), all = TRUE)) # with all=true display all value even sum
# meerge with 95% confidence interval total
alltotal2 <- data.frame( merge(alltotal1, Conf_intx_prct, by=c("GENHLTH"), all = TRUE))
# total include percent and std err percent
alltotal <- data.frame( merge(alltotal2, total_prct, by=c("GENHLTH"), all = TRUE))

# create the variable with missign value 
alltotal$SEX <- "NA"
alltotal$SEX[alltotal$SEX == 'NA'] <- 'Sum'
# replace NA value by Total
alltotal$SEX[(alltotal$SEX)=="NA"] <- "Sum"

# create the variable with value for row percent and std err row percent and 95% confidence interval and 95 % confidence row percent
alltotal$Row_Percent <- "NA"
alltotal$Row_Percent[alltotal$Row_Percent == 'NA'] <- '100.00'
alltotal$Std_Err_Row_Perct <- "NA"
alltotal$Std_Err_Row_Perct[alltotal$Std_Err_Row_Perct == 'NA'] <- ''
# 95% confidence limit total
#alltotal$Conf_Limits_Prcnt_2.5 <- "NA"
#alltotal$Conf_Limits_Prcnt_2.5[alltotal$Conf_Limits_Prcnt_2.5 == 'NA'] <- ''
#alltotal$Conf_Limits_Prcnt_97.5 <- "NA"
#alltotal$Conf_Limits_Prcnt_97.5[alltotal$Conf_Limits_Prcnt_97.5 == 'NA'] <- ''
# 95% row percent total 
alltotal$Conf_Limits_Row_Prcnt_2.5 <- "NA"
alltotal$Conf_Limits_Row_Prcnt_2.5[alltotal$Conf_Limits_Row_Prcnt_2.5 == 'NA'] <- ''
alltotal$Conf_Limits_Row_Prcnt_97.5 <- "NA"
alltotal$Conf_Limits_Row_Prcnt_97.5[alltotal$Conf_Limits_Row_Prcnt_97.5 == 'NA'] <- ''
alltotal$GENHLTH  <- factor(alltotal$GENHLTH , levels = c("Excellent","Very good", "Good", "Fair","Poor","Sum1","Sum2","Sum"))
alltotal <- data.frame(alltotal[order(alltotal$GENHLTH),]) 



# merge weighted frequency and std weight freq table, row percent and std err row percent
weight.complet <- data.frame(merge(Wght_freq, Std_Wght_freq, by=c("GENHLTH","SEX")))  
rw.percent.complet <- data.frame(merge(Row_Percent, Std_Err_Rw_Prct, by=c("GENHLTH","SEX")))
#weight.complet
# merge frequency with wght freq and std err wght freq  prct_std_prct  , rw.percent.complet
mixtable <- data.frame(merge(frqsex2, weight.complet, by=c("GENHLTH","SEX")) )

# merge the data we got with row percent and std percent
mixtable2 <- data.frame(merge(mixtable,prct_std_prct, by=c("GENHLTH","SEX")) )


# merge the data we get with 95% confidence limits for percent
   
mixtable3 <- data.frame(merge(mixtable2,Conf_int_prct, by=c("GENHLTH","SEX")) )

# merge the data we get with row percent and std err row percent
mixtable4 <- data.frame(merge(mixtable3,rw.percent.complet, by=c("GENHLTH","SEX")) )

# merge the data we get with 95% confidence limit row percent and std err row percent
mixtable5 <- data.frame(merge(mixtable4,rw_Conf_int_prct, by=c("GENHLTH","SEX")) )

#
# make the dataset to follow order
mixtable5$GENHLTH  <- factor(mixtable5$GENHLTH , levels = c("Excellent","Very good", "Good", "Fair","Poor","Sum"))
mixtable5 <- data.frame(mixtable5[order(mixtable5$GENHLTH),])
#mixtable

# merge all totals with mix table
mixdata <- mixtable5

df3 <- data.frame(mixdata) #All data without total
#df4 <- data.frame(alltotal1) #All total freq (weighted and unweigted, perct and std err prctg)
df4 <- data.frame(alltotal) #All total freq (weighted and unweigted)

#df4 <- df4[c("GENHLTH","SEX", "Frequency", "Wght_Frequency", "Std_Err_Wght_Freq")]
# mix total for all freq and unweigted freq and std error

#identical(names(df3), names(df4) )
#names(df3) <- names(df4)

tab_mix <- rbind(df3, df4)
#tab_mix
# factor the result in order
tab_mix$GENHLTH  <- factor(tab_mix$GENHLTH , levels = c("Excellent","Very good", "Good", "Fair","Poor","Sum1","Sum2","Sum"))

tab_mix <- data.frame(tab_mix[order(tab_mix$GENHLTH),])
tab_mix
