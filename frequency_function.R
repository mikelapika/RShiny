#brfss_design <- svydesign(id = ~`_psu`, strata = ~`_STSTR` ,	weight = ~`_LLCPWT`,data = brfss_15, nest = TRUE )

# total unweigted freq

# variable to analyze in this case GENHLTH will always be Var1, so we will replace Var1 by GENHLTH.
totfreqx <- data.frame(addmargins( table(brfss_15$GENHLTH)))

#colnames(totfreqx)  <- c('GENHLTH','Frequency')
totfreqx$Var1  <- factor(totfreqx$Var1 , levels = c("Excellent","Very good", "Good", "Fair","Poor","Sum"))
totfreqx <- data.frame(totfreqx[order(totfreqx$Var1),])

#

#weighted Frequency Totals and std error weighted frequency totals
Var1 <- c("Excellent","Very good", "Good", "Fair","Poor")
total_wght_freqx <- data.frame(svytotal(~GENHLTH, brfss_design, na.rm=TRUE)) 
#colnames(total_wght_freqx) <- c('Wght_Freq','Std_Err_Wght_Freq')
total_wght_freqx$total <- round(total_wght_freqx$total,0)
total_wght_freqx$SE <- round(total_wght_freqx$SE,0)
total_wght_freqx <- data.frame(Var1,total_wght_freqx)
#colnames(total_wght_freqx) <- c('GENHLTH','Wght_Freq','Std_Err_Wght_Freq')
total_wght_freqx <- data.frame(total_wght_freqx)

# Total percent and std error percent 
total_prctx <- svymean(~GENHLTH, brfss_design, na.rm=TRUE)
total_prctx <- data.frame(Var1,total_prctx)
#colnames(total_prctx) <- c("GENHLTH","Percent","Std_Err_Percent")
total_prctx$mean <- total_prctx$mean * 100
total_prctx$SE <- total_prctx$SE * 100

total_prctx <- data.frame(total_prctx)


###############################################
# confidence interval limit 95%

# 95% Confidence Limits for Percent (to work on it, split the column name)

Conf_intx <- data.frame(confint(svymean(~interaction(GENHLTH), design = brfss_design, na.rm = TRUE)))
# create column name to extract the row name
Conf_intx$ConfIntx <- rownames(Conf_intx) 
# separate row name into GENELTH 
Conf_intx2 <- separate(data = Conf_intx, col = ConfIntx, into = c("CINT", "Var1"), sep = "\\)") #

Conf_intx2 <- Conf_intx2[ , -c(3)]
#colnames(Conf_intx2)  <- c('Conf_Limits_Prcnt_2.5','Conf_Limits_Prcnt_97.5','GENHLTH')
Conf_intx2$Var1  <- factor(Conf_intx2$Var1 , levels = c("Excellent","Very good", "Good", "Fair","Poor","Sum"))
Conf_intx2$X2.5.. <- Conf_intx2$X2.5.. * 100
Conf_intx2$X97.5.. <- Conf_intx2$X97.5.. * 100
Conf_intx_prct <- data.frame(Conf_intx2[order(Conf_intx2$Var1),]) 

# merge all table one by one weigted freq and unweighted freq
tblx1 <- data.frame(merge(totfreqx,total_wght_freqx, by=c("Var1")) )

# merge the data we get with percent and std err percent
tblx2 <- data.frame(merge(tblx1,total_prctx, by=c("Var1")) )

# merge the data we get with 95% conf limit percent 
tblx3 <- data.frame(merge(tblx2,Conf_intx_prct, by=c("Var1")) )

# factor the result in order
tblx3$Var1  <- factor(tblx3$Var1 , levels = c("Excellent","Very good", "Good", "Fair","Poor","Total"))

tblx3 <- data.frame(tblx3[order(tblx3$Var1),])
# rename the column name
colnames(tblx3)  <- c('GENHLTH','Frequency','Wght_Freq','Std_Err_Wght_Freq','Percent','Std_Err_Percent','Conf_Limits_Prcnt_2.5','Conf_Limits_Prcnt_97.5')
tblx3

# create the variable with total 
total1 <- colSums(tblx3[ , 2,drop = FALSE])
total2 <- colSums(tblx3[ , 3,drop = FALSE])
total3 <- colSums(tblx3[ , 4,drop = FALSE])
total4 <- colSums(tblx3[ , 5,drop = FALSE])

# create row with total 
xct <- data.frame(GENHLTH = "Total", Frequency = total1,Wght_Freq=total2,Std_Err_Wght_Freq=total3,Percent=total4,
                  Std_Err_Percent="",Conf_Limits_Prcnt_2.5="",Conf_Limits_Prcnt_97.5=""   )

# remove the row name inside the dataframe
rownames(xct) <- c()
# merge the table and the total variable row we create
tblx4 <- rbind(tblx3,xct)
tblx4 <- data.frame(tblx4)
tblx4

