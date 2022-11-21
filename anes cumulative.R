library(readstata13) 
##### ANES cumulative #####
df <-  read.dta13("anes_timeseries_cdf_stata_20220916.dta",  
                  missing.type=TRUE, generate.factors=TRUE)

# df <-  read.dta13("D:\\dataset\\ANES\\ANES Cumulative Dataset\\1948-2020\\anes_timeseries_cdf_stata_20220916.dta",  
#                   missing.type=TRUE, generate.factors=TRUE)



#1: Party Identification of Respondent
table(df$VCF0302)
df$partyid1 <- car::recode(df$VCF0302, "5=1; 1=2; 2=3; 9=NA; 8=NA")
table(df$partyid1)
summary(df$partyid1)

#2: Which Major Party is More Conservative
table(df$VCF0502a)
df$socialpc <- car::recode(df$VCF0502a, "5=3; 5=NA; 0=NA")
table(df$socialpc)
summary(df$socialpc)

#3: R's Vote for President
table(df$VCF0705)
df$votep1 <- car::recode(df$VCF0705, "0=NA")
table(df$votep1)
summary(df$votepres1)

#4: R's Vote for President (Including 3rd parties)
table(df$VCF0706)

df$votep2 <- car::recode(df$VCF0706, "7=NA; 0=NA")
table(df$votep2)
summary(df$votepres2)

#5: R's Vote for House
table(df$VCF0736)
df$voteh <- car::recode(df$VCF0736, "5=2; 7=3; 0=NA")
table(df$voteh)
summary(df$voteh)

#6:If Compelled to Choose Liberal or Conservative
table(df$VCF0824)
df$ideo1 <- car::recode(df$VCF0824, "3=2; 5=3; 7=NA; 8=4; 9=NA; 0=NA")
table(df$ideo1)
summary(df$ideo1)

#7:Is Religion Important to Respondent
table(df$VCF0846)
df$demoreli <- car::recode(df$VCF0846, "8=NA; 0=NA")
summary(df$demoreli)

#8: Liberal-Conservative: 7 Point Scale
table(df$VCF0849 )
df$ideo2  <- car::recode(df$VCF0849, "3=2; 5=3; 6=NA; 9=NA; 0=NA")
table(df$ideo2)
summary(df$iseo2)

#9: Should Gays/Lesbians Be Able to Adopt Children
table(df$VCF0878)
df$gayadopt <- car::recode(df$VCF0878, "5=2; 8=3; 9=NA")
table(df$gayadopt)
summary(df$gayadopt)

#10: Congressional District of Residence
table(df$VCF0900)
df$democd <- car::recode(df$VCF0900, "00=NA; -9=NA")
table(df$democd)
summary(df$democd)

#11: Type of U.S. House Race
table(df$VCF0902)
df$election1 <- car::recode(df$VCF0902, "99=NA; 00=NA")
table(df$election1)
summary(df$election1)

#12: Is House Incumbent Running
table(df$VCF0903)
df$election2 <- car::recode(df$VCF0903, "9=NA")
table(df$election2)
summary(df$election2)

#13: Is House Incumbent Opposed 
table(df$VCF0904)
df$election3 <- car::recode(df$VCF0904, "0=3; 9=NA")
table(df$election3)
summary(df$election3)

#14: Number of Candidates in U.S. House Race
table(df$VCF0905)
df$election4 <- car::recode(df$VCF0905, "9=NA")
table(df$election4)
summary(df$election4)

#15: Nonvoter Preference- Presidential Cand
table(df$VCF9023)
df$votepres3 <- car::recode(df$VCF9023, "0=NA; 9=NA; 7=4")
table(df$votepres3)
summary(df$votepres3)

#16: Vote for Governor- Party
table(df$VCF9025)
df$votegov <- car::recode(df$VCF9025, "9=NA; 0=NA")
table(df$votegov)
summary(df$votegov)

#17: Vote in Previous Presidential Election - Party 
table(df$VCF9027)
df$votepres4 <- car::recode(df$VCF9027, "5=3; 3=NA; 9=NA; 0=NA")
table(df$votepres4)
summary(df$votepres4)

#18: Thermometer - Senate Democratic Candidate
table(df$VCF9056)
df$ftsenatedem <- car::recode(df$VCF9056, "990=NA; 996=NA; 997=NA; 998=NA; 999=NA")
table(df$ftsenatedem)
summary(df$ftsenatedem)

#19: Thermometer - Senate Republican Candidate
table(df$VCF9057)
df$ftsenaterep <- car::recode(df$VCF9057, "990=NA; 996=NA; 997=NA; 998=NA; 999=NA")
table(df$ftsenaterep)
summary(df$ftsenaterep)

#20: Thermometer - Senator in State with Senate Race
table(df$VCF9060)
df$ftsenate <- car::recode(df$VCF9060, "996=NA; 997=NA; 998=NA; 999=NA")
table(df$ftsenate)
summary(df$ftsenate)

#21: Strength Approve/Disapprove Running U.S. House Incumbent
table(df$VCF9069)
df$fthouse <- car::recode(df$VCF9069, "8=3; 9=NA; 0=NA")
table(df$fthouse)
summary(df$fthouse)

#22: Which party would do a better job handling the nation’s economy
table(df$VCF9205)
df$econ1 <- car::recode(df$VCF9205, "3=2; 7=3; 2=4; -8=NA; -9=NA")
table(df$econ1)
summary(df$econ1)





# 23: which party would better handle economy
table(df$VCF9205)
df$partyecon <- car::recode(df$VCF9205, "-9=NA; -8=NA; 7=NA")
table(df$partyecon)
summary(df$partyecon)

# 24: approval of current Pres's foreign relations
table(df$VCF9217)
df$presfp <- car::recode(df$VCF9217, "-8=NA; -9=NA")
table(df$presfp)
summary(df$presfp)

# 25: approval of current Pres's health care
table(df$VCF9218)
df$preshc <- car::recode(df$VCF9218, "-8=NA; -9=NA")
table(df$preshc)
summary(df$preshc)

# 26: unemployment trends
table(df$VCF9225)
df$unemp <- car::recode(df$VCF9225, "-8=NA; -9=NA")
table(df$unemp)
summary(df$unemp)

# 27: level of unemployment has gotten much/somewhat better/worse
table(df$VCF9226)
df$unempupdown <- car::recode(df$VCF9226, "-8=NA; -9=NA")
table(df$unempupdown)
summary(df$unempupdown)

# 28: 20 yr income gap
table(df$VCF9227)
df$incomegap <- car::recode(df$VCF9227, "-8=NA; -9=NA")
table(df$incomegap)
summary(df$incomegap)

# 29: 20 yr income gap much/somewhat larger/smaller
table(df$VCF9228)
df$incomegaplevel <-car::recode(df$VCF9228, "-8=NA; -9=NA")
table(df$incomegaplevel)
summary(df$incomegaplevel)

# 30: unemployment projection (12 months)
table(df$VCF9229)
df$unempproj <- car::recode(df$VCF9229, "-8=NA; -9=NA")
table(df$unempproj)
summary(df$unempproj)

# 31: favor/oppose foreign import taxes
table(df$VCF9231)
df$imports <- car::recode(df$VCF9231, "-7=NA; -8=NA; -9=NA")
table(df$imports)
summary(df$imports)

# 32: democratic Pres candidate position on abortion
table(df$VCF9234)
df$demabortion <- car::recode(df$VCF9234, "-9=NA; -8=NA")
table(df$demabortion)
summary(df$demabortion)

# 33: republican Pres candidate position on abortion
table(df$VCF9235)
df$repabortion <- car::recode(df$VCF9235, "-9=NA; -8=NA")
table(df$repabortion)
summary(df$repabortion)

# 34: strongly/not strongly favor/oppose death penalty for murder convictions
table(df$VCF9237)
df$deathp <- car::recode(df$VCF9237, "-8=NA; -9=NA; 4=3; 5=4")
table(df$deathp)
summary(df$deathp)

# 35: should gov't make it easier/harder to obtain a gun
table(df$VCF9238)
df$gunaccess <- car::recode(df$VCF9238, "-8=NA; -9=NA")
table(df$gunaccess)
summary(df$gunaccess)

# 36: gun control importance
table(df$VCF9239)
df$gunimp <- car::recode(df$VCF9239, "-8=NA; -9=NA")
table(df$gunimp)
summary(df$gunimp)

# 37: left-right scale placement for respondent 0-10
table(df$VCF9240)
df$scaleten <- car::recode(df$VCF9240, "-9=NA; -8=NA")
table(df$scaleten)
summary(df$scaleten)

# 38: left-right scale placement for Democratic respondent 0-10
table(df$VCF9241)
df$demscaleten <- car::recode(df$VCF9241, "-9=NA; -8=NA")
table(df$demscaleten)
summary(df$demscaleten)

# 39: left-right scale placement for Republican respondent 0-10
table(df$VCF9242)
df$repscaleten <- car::recode(df$VCF9242, "-9:-7=NA")
table(df$repscaleten)
summary(df$repscaleten)

# 40: is respondent a born-again Christian
table(df$VCF9243)
df$christ <- car::recode(df$VCF9243, "-9=NA; -8=NA")
table(df$christ)
summary(df$christ)

# 41: how often respondent trusts people
table(df$VCF9244)
df$trust <- car::recode(df$VCF9244, "-9=NA; -8=NA")
table(df$trust)
summary(df$trust)

# 42: vote makes a big/small difference
table(df$VCF9250)
df$vote <- car::recode(df$VCF9250, "-9=NA; -8=NA")
table(df$vote)
summary(df$vote)

# 43: respondent voted in state Pres primary/caucus
table(df$VCF9265)

df$presprim <- car::recode(df$VCF9265, "-8=NA; -9=NA")
table(df$presprim)
summary(df$presprim)

# 44: (hispanic respondents) most political info in english/spanish
table(df$VCF9266)
df$hisplang <- car::recode(df$VCF9266, "-9=NA; -8=NA")
table(df$hisplang)
summary(df$hisplang)



# # gender: male-1, female-0
# table(df$VCF0104)
# df$gender <- car::recode(df$VCF0104, "0=NA; 1=1; 2=0; 3=NA")
# table(df$gender)
# summary(df$gender)
# # race: white-1, nonwhite-0
# df$race <- car::recode(df$VCF0105a, "1=1; 2:7=0; 9=NA")
# summary(df$race)
# table(df$race)
# 
# #age: 18 and above
# df$age <- car::recode(df$VCF0101, "17=NA")
# table(df$age)
# summary(df$age)
# # edu: 1-8 grades or less, 6-BA and above
# table(df$VCF0140)
# df$edu <- car::recode(df$VCF0140, "8:9=NA")
# table(df$edu)
# summary(df$edu)
# # ideology
# df$ideo <- car::recode(df$VCF0803, "9=NA; 0=NA")
# table(df$ideo)
# summary(df$ideo)




# ###### GSS ######
# gss <- read.dta13("D:\\dataset\\GSS\\GSS_stata_7221\\gss7221_r2.dta",  
#                   missing.type=TRUE, generate.factors=TRUE)
# table(gss$partyid)
# table(gss$partyid)
# class(gss$partyid) # check the type of the variable
# 
# gss$partyid_1 <- as.numeric(gss$partyid)
# table(gss$partyid_1)
# 
# gss$pid7 <- car::recode(as.numeric(gss$partyid), "8=NA") # convert a factor to numeric 
# table(gss$pid7)
# 
# gss$pid7 <- car::recode(gss$partyid, " 'strong democrat'=1; 'not very strong democrat'=2; 
#                         'independent, close to democrat'=3;
#                         'independent (neither, no response)'=4;
#                         'independent, close to republican'=5;
#                         'not very strong republican'=6;
#                         'strong republican'=7; 
#                         'other party'=NA
#                         ")
# table(gss$pid7)
# class(gss$pid7)
# 
# 
# gss$pid7_num <- as.numeric(gss$pid7)
# class(gss$pid7_num)

