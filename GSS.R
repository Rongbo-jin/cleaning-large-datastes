library(readstata13) # load packages


gss <- read.dta13("gss7221_r2.dta")
# gss <- read.dta13("D:\\dataset\\GSS\\GSS_stata_7221\\gss7221_r2.dta")


# 46: Last week were you working full time, part time, going to school, keeping house, or what?
table(gss$wrkstat)
gss$laborforce <- car::recode(as.numeric(gss$wrkstat), "-99=NA; -97=NA")
table(gss$laborforce)

# 47: ASTROLOGICAL SIGN OF RESPONDENT
table(gss$zodiac)
gss$rzodiac <- car::recode(as.numeric(gss$zodiac), "99=NA; 98=NA; 100=NA")
table(gss$rzodiac)

# 48: IF WORKING, FULL OR PART TIME: How many hours did you work last week, at all jobs?
table(gss$hrs1)
gss$hrswork1 <- car::recode(gss$hrs1, "99=NA; 98=NA; 97=NA; 100=NA")
table(gss$hrswork1)

# 49:IF WITH A JOB, BUT NOT AT WORK: How many hours a week do you usually work, at all jobs? 
table(gss$hrs2)
gss$hrswork2 <- car::recode(gss$hrs2, "99=NA; 98=NA; 97=NA; 100=NA")
table(gss$hrswork2)

# 50: IF RETIRED, IN SCHOOL, KEEPING HOUSE, OR OTHER: Did you ever work for as long as one year?
table(gss$evwork)
gss$everwork <- car::recode(as.numeric(gss$evwork), "-99=NA; -98=NA; -97=NA; -100=NA")
table(gss$everwork)

# 51: (Are/Were) you self employed or (do/did) you work for someone else?
table(gss$wrkslf)
gss$slfemploy <- car::recode(as.numeric(gss$wrkslf), "-99=NA; -98=NA; -97=NA; -100=NA")
table(gss$slfemploy)
class(gss$slfemploy)

# 52: RESPONDENT'S OCCUPATION
table(gss$occ10)
gss$rocc <- car::recode(gss$occ10, "'Minimum'=1; '1st Quartile'=2; 'Median'=3; 'Mean'=4; '3rd Quartile'=5; 'Maximum'=6; 'Standard deviation'=7")
table(gss$rocc)
class(gss$rocc)

# 53: PRESTIGE OF RESPONDENT'S OCCUPATION
table(gss$prestg10)
gss$prestg1 <- car::recode(gss$prestg10, "-100=NA")
table(gss$prestg1)
class(gss$prestg1)

# 54: R'S OCCUPATIONAL PRESTIGE SCORE USING THRESHOLD METHOD 
table(gss$prestg105plus)
gss$prestg2 <- car::recode(gss$prestg105plus, "-80=NA; -100=NA")
table(gss$prestg2)
class(gss$prestg2)

# 55: RESPONDENT'S INDUSTRY
table(gss$indus10)
gss$industry <- car::recode(gss$indus10, "-95=NA; -97=NA; -99=NA; -100=NA")
table(gss$industry)
class(gss$industry)

# 56: RESPONDENT'S EDUCATION
table(gss$educ)
gss$education <- car::recode(gss$educ, "-99=NA; -98=NA")
table(gss$education)
class(gss$education)

# 57: RESPONDENT'S DEGREE
table(gss$degree)
gss$tpdegree <- car::recode(as.numeric(gss$degree),"-99=NA; -98=NA; -97=NA")
table(gss$tpdegree)
class(gss$tpdegree)

# 58: CODE RESPONDENT'S SEX
table(gss$sex)
gss$rsex <- car::recode(as.numeric(gss$sex), "-99=NA; -100=NA; -97=NA")
table(gss$rsex)
class(gss$rsex)

# 59: What race do you consider yourself?
table(gss$race)
gss$rrace <- car::recode(as.numeric(gss$race), "-100=NA")
table(gss$rrace)
class(gss$rrace)

# 60: Were you born in this country?
table(gss$born)
gss$usborn <- car::recode(as.numeric(gss$born), "-99=NA; -98=NA; -97=NA; -100=NA")
table(gss$usborn)
class(gss$usborn)

# 61: In which of these groups did your total family income, from all sources, fall last year before taxes, that is?
table(gss$income)
gss$famincome1 <- car::recode(as.numeric(gss$income), "-90=NA; -99=NA; -98=NA; -97=NA; -100=NA")

table(gss$famincome1)
class(gss$famincome1)

# 62: Inflation-adjusted family income.
table(gss$coninc)
gss$famincome2 <- car::recode(as.numeric(gss$coninc),"'Minimum'=1; '1st Quartile'=2; 'Median'=3; 'Mean'=4; '3rd Quartile'=5; 'Maximum'=6; 'Standard deviation'=7")
table(gss$famincome2)
class(gss$famincome2)

# 63: Inflation-adjusted personal income.
table(gss$conrinc)
gss$perincome <- car::recode(as.numeric(gss$conrinc),"'Minimum'=1; '1st Quartile'=2; 'Median'=3; 'Mean'=4; '3rd Quartile'=5; 'Maximum'=6; 'Standard deviation'=7")
table(gss$perincome)
class(gss$perincome)


# 64: REGION OF INTERVIEW
table(gss$region)
gss$regioninterv <- car::recode(as.numeric(gss$region),"'new england'=1; 'middle atlantic'=2; 'east north central'=3; 'west north central'=4; 'south atlantic'=5; 'east south atlantic'=6; 'west south central'=7; 'mountain'=8; 'pacific'=9")
table(gss$regioninterv)
class(gss$regioninterv)

# 65: Generally speaking, do you usually think of yourself as a Republican, Democrat, Independent, or what?
gss$partyid <- as.numeric(gss$partyid)
table(gss$partyid)
gss$partyid <- car::recode(as.numeric(gss$partyid), "8=NA") 
table(gss$partyid)
gss$ptid <- car::recode(gss$partyid, " 'strong democrat'=1; 'not very strong democrat'=2; 
                        'independent, close to democrat'=3;
                        'independent (neither, no response)'=4;
                        'independent, close to republican'=5;
                        'not very strong republican'=6;
                        'strong republican'=7; 
                        'other party'=NA
                        ")
table(gss$ptid)

# 66: In 2016, you remember that Hillary Clinton ran for President on the Democratic ticket against Donald Trump for the Republicans. Do you remember for sure whether or not you voted in that election?
table(gss$vote16)
gss$vt216 <- car::recode(as.numeric(gss$vote16), "-97=NA; -98=NA; -99=NA; -100=NA")
table(gss$vt216)
class(gss$vt216)


# 67: Did you vote for Hillary Clinton or Donald Trump?
table(gss$pres16)
gss$prs216 <- car::recode(as.numeric(gss$pres16), "4=NA; -70=NA; -99=NA; -98=NA; -97=NA; -100=NA")
table(gss$prs216)
class(gss$prs216)


# 68: Ideology, 7 point scale
table(gss$polviews)
gss$ideo7 <- car::recode(as.numeric(gss$polviews), "-99=NA; -98=NA; -97=NA; -100=NA")
table(gss$ideo7)
class(gss$ideo7)


# 69: Government spending on: Space exploration program
table(gss$natspac)
gss$gsspacep <- car::recode(as.numeric(gss$natspac), "-99=NA; -98=NA; -97=NA; -100=NA")
table(gss$gsspacep)
class(gss$gsspacep)


# 70: Government spending on: Space exploration
table(gss$natspacy)
gss$gsspace <- car::recode(as.numeric(gss$natspacy), "-99=NA; -98=NA; -97=NA; -100=NA")
table(gss$gsspace)
class(gss$gsspace)


# 71: Government spending on: Foreign aid
table(gss$nataid)
gss$gsfaid <- car::recode(as.numeric(gss$nataid), "-99=NA; -98=NA; -97=NA; -100=NA")
table(gss$gsfaid)
class(gss$gsfaid)


# 72: Government spending on: Assistance to other countries 
table(gss$nataidy)
gss$gsafaid <- car::recode(as.numeric(gss$nataidy), "-99=NA; -98=NA; -97=NA; -100=NA")
table(gss$gsafaid)
class(gss$gsafaid)


# 73: Government spending on: The military, armaments, and defense
table(gss$natarms)
gss$gsmad <- car::recode(as.numeric(gss$natarms), "-99=NA; -98=NA; -97=NA; -100=NA")
table(gss$gsmad)
class(gss$gsmad)

# 74: Government spending on: National defense
table(gss$natarmsy)
gss$gsnd <- car::recode(as.numeric(gss$natarmsy), "-99=NA; -98=NA; -97=NA; -100=NA")
table(gss$gsnd)
class(gss$gsnd)


# 75: Government spending on: Assistance for childcare
table(gss$natchld)
gss$gsachild <- car::recode(as.numeric(gss$natchld), "-99=NA; -98=NA; -97=NA; -100=NA")
table(gss$gsachild)
class(gss$gsachild)


# 76: Government spending on: Solving the problems of big cities
table(gss$natcity)
gss$gssprobbc <- car::recode(as.numeric(gss$natcity), "-99=NA; -98=NA; -97=NA; -100=NA")
table(gss$gssprobbc)
class(gss$gssprobbc)

# 77: Government spending on: Assistance to big cities
table(gss$natcityy)
gss$gsassistbc <- car::recode(as.numeric(gss$natcityy), "-99=NA; -98=NA; -97=NA; -100=NA")
table(gss$gsassistbc)
class(gss$gsassistbc)

# 78: Government spending on: Halting the rising crime rate
table(gss$natcrime)
gss$gshaltrc <- car::recode(as.numeric(gss$natcrime), "-99=NA; -98=NA; -97=NA; -100=NA")
table(gss$gshaltrc)
class(gss$gshaltrc)

# 79: Government spending on: Law enforcement
table(gss$natcrimy)
gss$gslawenf <- car::recode(as.numeric(gss$natcrimy), "-99=NA; -98=NA; -97=NA; -100=NA")
table(gss$gslawenf)
class(gss$gslawenf)


# 80: Government spending on: Dealing with drug addiction
table(gss$natdrug)
gss$gsdrugadd <- car::recode(as.numeric(gss$natdrug), "-99=NA; -98=NA; -97=NA; -100=NA")
table(gss$gsdrugadd)
class(gss$gsdrugadd)


# 81: Government spending on: Drug rehabilitation
table(gss$natdrugy)
gss$gsdrugrehab <- car::recode(as.numeric(gss$natdrugy), "-99=NA; -98=NA; -97=NA; -100=NA")
table(gss$gsdrugrehab)
class(gss$gsdrugrehab)


# 82: Government spending on: Improving the nation's education system
table(gss$nateduc)
gss$gsineducs <- car::recode(as.numeric(gss$nateduc), "-99=NA; -98=NA; -97=NA; -100=NA")
table(gss$gsineducs)
class(gss$gsineducs)


# 83: Government spending on: Education
table(gss$nateducy)
gss$gseduc <- car::recode(as.numeric(gss$nateduc), "-99=NA; -98=NA; -97=NA; -100=NA")
table(gss$gseduc)
class(gss$gseduc)


# 84: Government spending on: Developing alternative energy sources
table(gss$natenrgy)
gss$gsdaltes <- car::recode(as.numeric(gss$natenrgy), "-99=NA; -98=NA; -97=NA; -100=NA")
table(gss$gsdaltes)
class(gss$gsdaltes)


# 85: Government spending on: Improving and protecting the environment
table(gss$natenvir)
gss$gsipenvir <- car::recode(as.numeric(gss$natenvir), "-99=NA; -98=NA; -97=NA; -100=NA")
table(gss$gsipenvir)
class(gss$gsipenvir)


# 86: Government spending on: The environment
table(gss$natenviy)
gss$gsenvir <- car::recode(as.numeric(gss$natenviy), "-99=NA; -98=NA; -97=NA; -100=NA")
table(gss$gsenvir)
class(gss$gsenvir)


# 87: Government spending on: Welfare
table(gss$natfare)
gss$gswelfare <- car::recode(as.numeric(gss$natfare), "-99=NA; -98=NA; -97=NA; -100=NA")
table(gss$gswelfare)
class(gss$gswelfare)


# 88: Government spending on: Assistance to the poor
table(gss$natfarey)
gss$gsastpoor <- car::recode(as.numeric(gss$natfarey), "-99=NA; -98=NA; -97=NA; -100=NA")
table(gss$gsastpoor)
class(gss$gsastpoor)


# 89: Government spending on: Improving and protecting the nation's health
table(gss$natheal)
gss$gsipnh <- car::recode(as.numeric(gss$natheal), "-99=NA; -98=NA; -97=NA; -100=NA")
table(gss$gsipnh)
class(gss$gsipnh)


# 90: Government spending on: Health
table(gss$nathealy)
gss$gshealth <- car::recode(as.numeric(gss$nathealy), "-99=NA; -98=NA; -97=NA; -100=NA")
table(gss$gshealth)
class(gss$gshealth)


# 91: Government spending on: Mass Transportation
table(gss$natmass)
gss$gsmsstrnport <- car::recode(as.numeric(gss$natmass), "-99=NA; -98=NA; -97=NA; -100=NA")
table(gss$gsmsstrnport)
class(gss$gsmsstrnport)


# 92: Government spending on: Parks and recreation
table(gss$natpark)
gss$gsparkrec <- car::recode(as.numeric(gss$natpark), "-99=NA; -98=NA; -97=NA; -100=NA")
table(gss$gsparkrec)
class(gss$gsparkrec)


# 93: Government spending on: Improving the conditions of Blacks
table(gss$natrace)
gss$gsimprvblk <- car::recode(as.numeric(gss$natrace), "-99=NA; -98=NA; -97=NA; -100=NA")
table(gss$gsimprvblk)
class(gss$gsimprvblk)


# 94: Government spending on: Assistance to Blacks
table(gss$nateracey)
gss$gsasstblk <- car::recode(as.numeric(gss$natracey), "-99=NA; -98=NA; -97=NA; -100=NA")
table(gss$gsasstblk)
class(gss$gsasstblk)

# 95: fund highways and bridges: 1 - too little, 2 - about right, 3 - too much
table(gss$natroad)
gss$bridges <- car::recode(gss$natroad,"'too little'=1; 'about right'=2; 'too much'=3")
table(gss$bridges)

# 96: fund scientific research: 1 - too little, 2 - about right, 3 - too much
table(gss$natsci)
gss$sci <- car::recode(gss$natsci,"'too little'=1; 'about right'=2; 'too much'=3")
table(gss$sci)

# 97: fund social security: 1 - too little, 2 - about right, 3 - too much
table(gss$natsoc)
gss$ss <- car::recode(gss$natsoc,"'too little'=1; 'about right'=2; 'too much'=3")
table(gss$ss)

# 98: govt should reduce income gap (1-7): 1 - yes, 7 - no
table(gss$eqwlth)

# 99: taxes: 1 - too high, 2 - about right, 3 - too low
table(gss$tax)
gss$taxrate <- car::recode(gss$tax, "'too high'=1; 'about right'=2; 'too low'=3; 'r pays no income tax (vol.)'=NA")
table(gss$taxrate)

# 100: bad allowed to speak: 1 - yes, 2 - no
table(gss$spkath)
gss$speak <- car::recode(gss$spkath, "'yes, allowed to speak'=1; 'not allowed'=2")
table(gss$speak)

# 101: bad teach college: 1 - yes, 2 - no
table(gss$colath)
gss$college <- car::recode(gss$colath, "4=1; 5=2")
table(gss$college)

# 102: bad library book removed: 1 - no, 2 - yes
table(gss$libath)
gss$book <- car::recode(gss$libath, "'remove'=2; 'not remove'=1")
table(gss$book)

# 103: black inferior speak?: 1 - yes, 2 - no
table(gss$spkrac)
gss$racist <- car::recode(gss$spkrac, "'yes, allowed to speak'=1; 'not allowed'=2")
table(gss$racist)

# 104: black inferior teach?: 1 - yes, 2 - no
table(gss$colrac)
gss$raccol <- car::recode(gss$colrac, " 4=1; 5=2")
table(gss$raccol)

# 105: black inferior library book removed?: 1 - no, 2 - yes
table(gss$librac)
gss$racbook <- car::recode(gss$librac, "'remove'=2; 'not remove'=1")
table(gss$racbook)

# 106: communist speak?: 1 - yes, 2 - no
table(gss$spkcom)
gss$comspeak <- car::recode(gss$spkcom, "'yes, allowed to speak'=1; 'not allowed'=2" )
table(gss$comspeak)

# 107: communist prof fired : 1 - no, 2 - yes
table(gss$colcom)
gss$comfire <- car::recode(gss$colcom, "'yes, fired'=2; 'not fired'=1")
table(gss$comfire)

# 108: communist book removed? 1 - no, 2 - yes
table(gss$libcom)
gss$combook <- car::recode(gss$libcom, "'not remove'=1; 'remove'=2")
table(gss$combook)

# 109: military speak: 1 - yes, 2 - no
table(gss$spkmil)
gss$milspeak <- car::recode(gss$spkmil, "'yes, allowed to speak'=1; 'not allowed'=2")
table(gss$milspeak)

# 110: military teach college: 1 - yes, 2 - no
table(gss$colmil)
gss$milcol <- car::recode(gss$colmil, "4=1; 5=2")
table(gss$milcol)

# 111: military book removed: 1 - no, 2 - yes
table(gss$libmil)
gss$milbook <- car::recode(gss$libmil, "'not remove'=1; 'remove'=2")
table(gss$milbook)

# 112: homo speak: 1 - yes, 2 - no
table(gss$spkhomo)
gss$homospeak <- car::recode(gss$spkhomo, "'yes, allowed to speak'=1; 'not allowed'=2")
table(gss$homospeak)

# 113: homo teach college: 1  - yes, 2 - no
table(gss$colhomo)
gss$homocol <- car::recode(gss$colhomo, "4=1; 5=2")
table(gss$homocol)

# 114: homo book removed?: 1 - no, 2 - yes
table(gss$libhomo)
gss$homobook <- car::recode(gss$libhomo, "'remove'=2; 'not remove'=1")
table(gss$homobook)

# 115: muslim hate speech: 1 - yes, 2 - no
table(gss$spkmslm)
gss$mslmspeak <- car::recode(gss$spkmslm, "'yes, allowed'=1; 'not allowed'=2")
table(gss$mslmspeak)

# 116: muslim college teach: 1 - yes, 2 - no
table(gss$colmslm)
gss$mslmcol <- car::recode(gss$colmslm, "4=1; 5=2")
table(gss$mslmcol)

# 117: muslim book removed: 1 - no, 2 - yes
table(gss$libmslm)
gss$mslmbook <- car::recode(gss$libmslm, "'remove'=2; 'not remove'=1")
table(gss$mslmbook)

# 118: support death penalty: 1 - yes, 2 - no
table(gss$cappun)
gss$deathp <- car::recode(gss$cappun, "'favor'=1; 'oppose'=2")
table(gss$deathp)

# 119: police permit to buy gun: 1 - yes, 2 - no
table(gss$gunlaw)
gss$gunpermit <- car::recode(gss$gunlaw, "'favor'=1; 'oppose'=2")
table(gss$gunpermit)

# 120: religion: 1 - any christian, 2- jewish, 3- hinduism, 4- buddhism, 5-islam, 6- other, 7-none
table(gss$relig)
gss$religion <- car::recode(gss$relig, "'protestant'=1; 'catholic'=1; 'jewish'=2; 'none'=7; 'other'=6; 'other eastern religions'=6; 'buddhism'=4; 'hinduism'=3;'orthodox-christian'=1; 'christian'=1; 'native american'=6; 'inter-nondenominational'=6; 'muslim/islam'=5")
table(gss$religion) 

# 121: religious strength: 0-no religion, 1- not very strong, 2-somewhat strong, 3-strong
table(gss$relitenv)
gss$religstrong <- car::recode(gss$relitenv, "'strong'=3; 'somewhat strong'=2; 'not very strong'=1; 'no religion'=0")
table(gss$religstrong)

# 122: lord's prayer/bible use in schools? 1 - approve, 2 - disapprove
table(gss$prayernv)
gss$religschool <- car::recode(gss$prayernv, "'approve'=1; 'disapprove'=2")
table(gss$religschool)

# 123: lord's prayer/bible use in schools? (version 2): 1 - approve, 2 - disapprove
table(gss$prayerv)
gss$religschool2 <- car::recode(gss$prayerv, "'approve'=1; 'disapprove'=2")
table(gss$religschool2) 

# 124: overall current happiness: 1 - very happy, 2 - pretty happy, 3 - not too happy
table(gss$happy)
gss$happiness <- car::recode(gss$happy, "'very happy'=1; 'pretty happy'=2; 'not too happy'=3")
table(gss$happiness)

# 125: confidence in SCOTUS? 1- a great deal, 2 - only some, 3 - hardly any
table(gss$conjudge)
gss$courtcon <- car::recode(gss$conjudge, "'a great deal'=1; 'only some'=2; 'hardly any'=3")
table(gss$courtcon)

# 126: confidence in Congress? 1- a great deal, 2 - only some, 3 - hardly any
table(gss$conlegis)
gss$legcon <- car::recode(gss$conlegis, "'a great deal'=1; 'only some'=2; 'hardly any'=3")
table(gss$legcon)

# 127: confidence in military? 1- a great deal, 2 - only some, 3 - hardly any
table(gss$conarmy)
gss$milcon <- car::recode(gss$conarmy, "'a great deal'=1; 'only some'=2; 'hardly any'=3")
table(gss$milcon)

# 128: confidence in financial industry? 1- a great deal, 2 - only some, 3 - hardly any
table(gss$confinan)
gss$moncon <- car::recode(gss$confinan, "'a great deal'=1; 'only some'=2; 'hardly any'=3")
table(gss$moncon)

# 129: social class? 1- upper class, 2-middle class, 3-working class, 4- lower class
table(gss$class)
gss$socclass <- car::recode(gss$class, "'lower class'=4; 'working class'=3; 'middle class'=2; 'upper class'=1; 'no class'=NA")
table(gss$socclass)

# 130: societal rank: 1-top, 10-bottom
table(gss$rank)

# 131: how satisfied w/ financial situation? 1- pretty well satisfied, 2-more or less satisfied, 3-not satisfied at all
table(gss$satfin)
gss$financesat <- car::recode(gss$satfin, "'pretty well satisfied'=1; 'more or less satisfied'=2; 'not satisfied at all'=3")
table(gss$financesat)

# 132: financial situation changed? 1 - better, 0-stayed same, -1 - worse
table(gss$finalter)
gss$finchange <- car::recode(gss$finalter, "'better'=1; 'worse'=-1; 'stayed same'=0")
table(gss$finchange)

# 133: relative financial status? 2- far above average, 1- above average, 0-average, -1- below average, -2- far below average
table(gss$finrela)
gss$financerel <- car::recode(gss$finrela, "'far below average'=-2; 'below average'=-1; 'average'=0; 'above average'=1; 'far above average'=2")
table(gss$financerel)

# 134: abortion if defect? 1 - yes, 2 - no
table(gss$abdefect)
gss$abordef <- car::recode(gss$abdefect, "'yes'=1; 'no'=2")
table(gss$abordef)

# 135: abortion if no more kids? 1-yes, 2-no
table(gss$abnomore)
gss$aborno <- car::recode(gss$abnomore, "'yes'=1; 'no'=2")
table(gss$aborno)

# 136: abortion if health in danger? 1-yes, 2-no
table(gss$abhlth)
gss$aborhlth <- car::recode(gss$abhlth, "'yes'=1; 'no'=2")
table(gss$aborhlth)

# 137: abortion if cannot afford kid? 1-yes, 2-no
table(gss$abpoor)
gss$aborpoor <- car::recode(gss$abpoor, "'yes'=1; 'no'=2")
table(gss$aborpoor)

# 138: abortion if rape? 1-yes, 2-no
table(gss$abrape)
gss$aborrape <- car::recode(gss$abrape, "'yes'=1; 'no'=2")
table(gss$aborrape)

# 139: abortion if single and doesn't want to marry man? 1-yes, 2-no
table(gss$absingle)
gss$aborsingle <- car::recode(gss$absingle, "'yes'=1; 'no'=2")
table(gss$aborsingle)

# 140: abortion for any reason? 1-yes, 2-no
table(gss$abany)
gss$aborany <- car::recode(gss$abany, "'yes'=1; 'no'=2")
table(gss$aborany)

# 141: pill ok ages 14-16? 1-agree, 2-disagree
table(gss$pillok)
gss$pillteen <- car::recode(gss$pillok, "'strongly agree'=1; 'agree'=1; 'strongly disagree'=2; 'disagree'=2 ")
table(gss$pillteen)

# 142: sex education in public schools? 1-favor, 2-oppose
table(gss$sexeduc)
gss$sexed <- car::recode(gss$sexeduc, "'favor'=1; 'oppose'=2; 'depends on age/grade (vol.)'=NA")
table(gss$sexed)


