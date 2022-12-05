library(readstata13) # load packages

cces <- readRDS("cumulative_2006-2021.rds")
# cces <- readRDS("D:\\dataset\\CCES\\cumulative_2006-2021.rds")




#182 Validated registered party
table(cces$vv_party_gen)
cces$partyreg <- car::recode(cces$vv_party_gen, "'Democratic Party'=1; 
                     'Republican Party'=2; 'Independent Party'=3; 'Libertarian Party'=4;
                             'Green Party'=5; 'Constitution Party'=6; 'Reform Party'=7; 'Socialist Party'=8;
                           'Wor'=9; 'Other'=9; 'Cns'=9; 'Declined to State'=NA; 'Unknown'=NA;
                             'No Record of Party Registration'=NA; 'No Party Affiliation'=NA")
table(cces$partyreg)
summary(cces$partyreg)


#183 Validated registered Primary party
table(cces$vv_party_prm)
cces$partyregp <- car::recode(cces$vv_party_prm, "'Democratic Party'=1; 'Republican Party'=2; 'Independent Party'=3; 'Libertarian Party'=4; 'Green Party'=5; 'Other'=6; 'No Party Affiliation'=NA; 'No Record of Party Registration'=NA; ")
table(cces$partyregp)
summary(cces$partyregp)

#184 Validated turnout General Election
table(cces$vv_turnout_gvm)
cces$turngen <- car::recode(cces$vv_turnout_gvm, "'Voted'=1; 'No Record of Voting'=2; 'No Voter File'=2")
table(cces$turngen)
summary(cces$turngen)


#185 Validated turnout Primary Election (Congressional)
table(cces$vv_turnout_pvm)
cces$turnpc <- car::recode(cces$vv_turnout_pvm, "'Voted'=1; 'No Record of Voting'=2; 'No Voter File'=2")
table(cces$turnpc)
summary(cces$turnpc)


#186 Self-reported turnout (pre-election wave) "2020: Do you intend to vote in the 2020 general election on November 3rd?”
table(cces$intent_turnout_self)
cces$voteplanpre <- car::recode(cces$intent_turnout_self, "'I already voted (early or absentee)'=1; 'Plan to vote early'=2; 'Yes, definitely'=3; 'Probably'=4; 'Undecided'=5; 'No'=6")
table(cces$voteplanpre)
summary(cces$voteplanpre)


#187 Self-reported turnout (post-election wave)
table(cces$voted_turnout_self)
cces$voteplanpost <- car::recode(cces$voted_turnout_self, "'Yes'=1; 'No'=2")
table(cces$voteplanpost)
summary(cces$voteplanpost)


#188 Partisan identity (3 point)
table(cces$pid3) 
cces$pid3p <- car::recode(cces$pid3, "4=NA; 5=NA")
table(cces$pid3p)
summary(cces$pid3p)

#189 Partisan identity (7 point)
table(cces$pid7)
cces$pid7p <- car::recode(cces$pid7, "8=NA; 9=NA")
table(cces$pid7p)
summary(cces$pid7p)

#190 Partisan identity (including leaners)
table(cces$pid3_leaner)
cces$pid3pil <- car::recode(cces$pid3_leaner, "8=NA")
table(cces$pid3pil)
summary(cces$pid3pil)

#191 Ideology (5 point)
table(cces$ideo5)
cces$ideo5p <- car::recode(cces$ideo5, "'Very Liberal'=1; 'Liberal'=2; 'Moderate'=3; 'Conservative'=4; 'Very Conservative'=5; 'Not Sure'=NA")
table(cces$ideo5p)
summary(cces$ideo5p)

# 192: family's annual income over past year: 1- <10k, 2- 10-20k, 3- 20-30k, 4- 30-40k, 5- 40-50k, 6- 50-60k, 7- 60-70k, 8- 70-80k, 9- 80-100k, 10- 100-120k, 11- 120-150k, 12- 150k+
table(cces$faminc)
cces$famincome <- car::recode(cces$faminc, "'Less than 10k'=1; '10k - 20k'=2; '20k - 30k'=3;'30k - 40k'=4;'40k - 50k'=5; '50k - 60k'=6;'60k - 70k'=7;'70k - 80k'=8;'80k - 100k'=9;'100k - 120k'=10; '120k - 150k'=11;'150k+'=12; 'Prefer not to say'=NA; 'Skipped'=NA")
table(cces$famincome)

# 193: current employment status: 1- full time, 2- part time, 3- retired, 4- not employed
table(cces$employ)
cces$employment <- car::recode(cces$employ, "'Full-Time'=1; 'Part-Time'=2; 'Temporarily Laid Off'=4; 'Unemployed'=4; 'Retired'=3; 'Permanently Disabled'=4; 'Homemaker'=4; 'Student'=4; 'Other'=NA")
table(cces$employment)

# 194: does respondent have any of the listed health insurance options? 1- yes, 2-no
table(cces$no_healthins)
cces$healthins <- car::recode(cces$no_healthins, "'Yes'=1; 'No'=2")
table(cces$healthins)

# 195: respondent ever a member of a union? 1-yes, currently, 2-yes, formerly, 3-no, never
table(cces$union)

# 196: anyone in respondent's household ever a member of a union? 1-yes, currently, 2-yes, formerly, 3-no, never, 4-unsure
table(cces$union_hh)

# 197: retrospective look at economy: 2- gotten much better, 1- gotten better/somewhat better, 0- stayed about the same, -1- gotten worse/somewhat worse, -2- gotten much worse
table(cces$economy_retro)
cces$econretro <- car::recode(cces$economy_retro, "1=2;2=1;3=0;4=-1;5=-2;6=NA")
table(cces$econretro)

# 198: how often does R follow what's going on in govt in the news: 1- hardly at all, 2-only now and then, 3-some of the time, 4-most of the time
table(cces$newsint)
cces$news <- car::recode(cces$newsint, "1=4; 2=3; 3=2; 4=1; 7=NA")
table(cces$news)

# 199: does R approve of Pres's way of doing job: 1- strongly approve, 2- approve/somewhat approve, 3-neither approve nor disapprove, 4-disapprove/somewhat disapprove, 5-strongly disapprove
table(cces$approval_pres)
cces$presapp <- car::recode(cces$approval_pres, "3=4;4=5;5=NA;6=3")
table(cces$presapp)

# 200: does R approve of House Rep's way of doing job: 1- strongly approve, 2- approve/somewhat approve, 3-neither approve nor disapprove, 4-disapprove/somewhat disapprove, 5-strongly disapprove, 6- never heard of person
table(cces$approval_rep)
cces$repapp <- car::recode(cces$approval_rep, "'Strongly Approve'=1; 'Approve / Somewhat Approve'=2; 'Disapprove / Somewhat Disapprove'=4; 'Strongly Disapprove'=5; 'Never Heard / Not Sure'=NA; 'Neither Approve nor Disapprove'=3; 'Never Heard of this Person'=6")
table(cces$repapp)

# 201: does R approve of Sen1's way of doing job: 1- strongly approve, 2- approve/somewhat approve, 3-neither approve nor disapprove, 4-disapprove/somewhat disapprove, 5-strongly disapprove, 6- never heard of person
table(cces$approval_sen1)
cces$sen1app <- car::recode(cces$approval_sen1, "'Strongly Approve'=1; 'Approve / Somewhat Approve'=2; 'Disapprove / Somewhat Disapprove'=4; 'Strongly Disapprove'=5; 'Never Heard / Not Sure'=NA; 'Neither Approve nor Disapprove'=3; 'Never Heard of this Person'=6")
table(cces$sen1app)

# 202: does R approve of Governor's way of doing job: 1- strongly approve, 2- approve/somewhat approve, 3-neither approve nor disapprove, 4-disapprove/somewhat disapprove, 5-strongly disapprove
table(cces$approval_gov)
cces$govapp <- car::recode(cces$approval_gov, "3=4;4=5;5=NA;6=3")
table(cces$govapp)

