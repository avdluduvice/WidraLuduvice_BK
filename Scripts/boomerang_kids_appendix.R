# -----------------------------------------------------------------------------------------
# Title: Boomerang Kids in the Pandemic: How High-Income Families Are Their Own Safety Net
# Purpose: Reproduce the charts and tables from the appendix
# File: Script for running full appendix replication
# Authors: Rachel Widra and André Victor D. Luduvice
# Cleveland, January 30th, 2023
# -----------------------------------------------------------------------------------------

###### Library packages and read in data ######

#set working directory
wd<-gsub(x=rstudioapi::getActiveDocumentContext()$path, pattern = "boomerang_kids_appendix.R", replacement = "")
setwd(wd)

#run two lines below in the case of running the appendix without running the main code
#Chart_Path <- paste0(wd, "Charts/")
#dir.create(Chart_Path)

# Install and library required packages
check.packages <- function(pkg){
  new.pkg <- pkg[!(pkg %in% installed.packages()[, "Package"])]
  if (length(new.pkg))
    install.packages(new.pkg, dependencies = TRUE)
  sapply(pkg, require, character.only = TRUE)
}

# list of packages used
required_packages <- c("tidyr", "data.table", "lubridate", 
                     "ggplot2", "dplyr", "purrr", "readxl", "ipumsr")

# run function to install and library required packages
check.packages(required_packages)

# set colors
color1 = "#599871"   #green
color2 = "#2875a8"   #blue
color3 = "#e67a17"   #orange
color4 = "#581f54"   #maroon

if (!require("ipumsr")) stop("Reading IPUMS data into R requires the ipumsr package. It can be installed using the following command: install.packages('ipumsr')")
# read in cps data
ddi <- read_ipums_ddi("boomerang_kids_data.xml")
data <- as.data.table(read_ipums_micro(ddi))

# create date variable
data[, date:=ymd(paste(YEAR, MONTH, "15", sep="-"))]
data[, CPSIDP:=as.character(CPSIDP)]
data[, CPSID:=as.character(CPSID)]

####### Separate ASEC and monthly data
asec <- subset(data, ASECFLAG==1)
monthly_files <- subset(data, ASECFLAG!=1 | is.na(ASECFLAG))

#### show percent of young adults living at home ####
# subset to older young adults
ya <- subset(monthly_files, AGE>=24 & AGE<=29)
ya[, flag:=1]
# calculate total young adults per month using the person weights
ya[, total:=sum(WTFINL), by="date"]

# remove NAs from variables for line number of parents
ya[is.na(PELNPAR2), PELNPAR2:=0]
ya[is.na(PELNPAR1), PELNPAR1:=0]
ya[is.na(PELNDAD), PELNDAD:=0]
ya[is.na(PELNMOM), PELNMOM:=0]

# if line number for any parents is not zero, create flag for young adult living with parent
ya[PELNPAR2!=0|PELNPAR1!=0|PELNDAD!=0|PELNMOM!=0, parents_flag:=1]
# use person wieghts to calculate how many people live with their
# parents each month
ya[, parents_total:=sum(WTFINL), by=c("date", "parents_flag")]
# calculate the percent that live with parents each month
ya[, pct_live_at_home:=parents_total/total]
ya[, date:=as.POSIXct(date)]

##### Figure 5 - Percent of young adults (24 to 30) who live with parents ####
ggplot()+
  geom_hline(yintercept = seq(26,31, by=1),size=0.2, linetype=2)+
  geom_line(ya[parents_flag==1], mapping=aes(x=date, y=100*pct_live_at_home),size=1.75, color=color2)+
  theme_classic()+
  theme(axis.title.x = element_blank(),
        axis.title.y = element_text(size=20, color="black"),
        axis.text.y=element_text(size=20, color="black"),
        axis.text.x=element_text(size=20, color="black"),
        strip.background = element_blank(),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.background = element_rect(colour = "black", size=1),
        plot.title=element_text(size=20, color="black"),
        legend.key.size = unit(0, 'cm'),
        legend.text=element_text( size=10),
        legend.text.align = 1,
        legend.title=element_blank(),
        legend.key = element_rect(colour = "transparent", fill="transparent"),
        legend.background = element_rect(linetype = 1, size = 0.5, colour = 1),
        legend.justification = c(0,1),
        legend.position=c(0,1))+
  scale_y_continuous( limits=c(26,31.5), breaks=seq(26,31, by=1))+
  scale_x_datetime(date_breaks = "1 year", date_labels="%Y") +
  ggtitle("Figure 5: Percent of young adults 24 to 30 who live with at least one parent")+
  labs( y="Percent")

ggsave(paste0(Chart_Path, "Figure_5_older_perc_live_w_parents.png"), width = 12, height=5, unit="in")


###### Identify boomerang kids in monthly files #####

# identify the entry date for the parents' household by finding the minimum date or each household id
# remove dashes so date should match the first six digits of the household id for households that 
# entered during the period 
monthly_files[, hh_first_date:=gsub("-", "", substr(min(date), 1, 7)), by=CPSID]
# find the entry date for each person
monthly_files[, pers_first_date:=gsub("-", "", substr(min(date), 1, 7)), by=CPSIDP]
# remove NAs in all variables related to line number of parents, replace with zero
monthly_files[is.na(PELNPAR2), PELNPAR2:=0]
monthly_files[is.na(PELNPAR1), PELNPAR1:=0]
monthly_files[is.na(PELNDAD), PELNDAD:=0]
monthly_files[is.na(PELNMOM), PELNMOM:=0]
# if any line number of any parent is not zero, flag row as living with parent
monthly_files[PELNPAR2!=0|PELNPAR1!=0|PELNDAD!=0|PELNMOM!=0, parents_flag:=1]

# Decision rule for boomerang kid: 
# 1.If a young adult lives with at least one parent (parents flag is 1)
# 2. The entry date of the person is more recent than the entry date of the household
# 3. Age is between 24-30
monthly_files[parents_flag==1 & pers_first_date>hh_first_date & 
                AGE>=24 & AGE<30 & date>="2020-03-01", boomerang_kid:=1]

# create a list of person IDs for all boomerang kids
boomerang_kid_xwalk <- subset(monthly_files, boomerang_kid==1,select="CPSIDP")

# subset to just boomerang kids
boomerang_kids <- subset(monthly_files, boomerang_kid==1)

###### calculate length of unemployment for unemployed boomerang kids 
# order boomerang kids by person id and date
ordered<-boomerang_kids[order(CPSIDP, date)]
ordered[,flag:=1]
ordered[DURUNEMP==999,DURUNEMP:=NA]
# create a bunch of lagged demographic variables so we can check that demographics stay the same
# for each person across months
ordered$recent_age<-c(NA, ordered$AGE[1:(length(ordered$AGE)-1)])
ordered$recent_sex<-c(NA, ordered$SEX[1:(length(ordered$SEX)-1)])
ordered$recent_race<-c(NA, ordered$RACE[1:(length(ordered$RACE)-1)])
# for each persons first month in the survey set their recent demograpics equal to their
# current demographics
ordered[pers_first_date==gsub("-", "", substr(date, 1, 7)), 
        c("recent_age", "recent_sex", "recent_race"):=list(AGE, SEX, RACE)]

# check that demographics match from month to month for each person. Sex and race should stay the same
# age should be within one year
ordered[SEX==recent_sex & RACE==recent_race & (AGE==recent_age |AGE==recent_age+1), dems:=1]
# if demographics no not match from month to month, set dems variable equal to zero
ordered[is.na(dems), dems:=0]
# create a list of person ids with demographic mismatches
people_mismatch<-subset(ordered, dems==0, select="CPSIDP")
# subset to only boomerang kids during the pandemic that match demographics across months 
ordered<-subset(ordered, !(CPSIDP %chin%people_mismatch$CPSIDP) & date>="2020-03-01" & boomerang_kid==1)

# create list of person id for valid pandemic boomerang kids
boomerang_kids_xwalk <- unique(subset(ordered, select="CPSIDP"))

##### Calculate income quintiles #####
# subset ASEC data to one row per household per date, remove households with missing income
income_quints <- unique(subset(asec, !is.na(HHINCOME) & HHINCOME!=999999999), by=c("YEAR", "CPSID"))

# create list of all household incomes, split by year
income_quints_year <- split(income_quints$HHINCOME, income_quints$YEAR)

# calculate quintile cut offs for each years
quints_list <- lapply(income_quints_year,function(x) quantile(x, probs = (0:5)/5))
quints_list <- lapply(quints_list,as.data.table)
# make into a table with each quintile cutoff for each year
quints_table <- as.data.table(rbindlist(quints_list, idcol="YEAR"))
# fill in table
quints_table[, quintile:=rep(0:5, 5)]
setnames(quints_table, "V1", "quintile_max")
# make a column for each quintile
quints_wide <- spread(quints_table, quintile, quintile_max)
quints_wide$YEAR <- as.numeric(quints_wide$YEAR)

# merge table of quintile cutoffs onto household data
data_qints <- merge(income_quints, quints_wide, by="YEAR")
# determine which income quintile each household is in each year
data_qints[HHINCOME<=`1`, income_quintile:=1]
data_qints[HHINCOME<=`2` &HHINCOME>`1`, income_quintile:=2]
data_qints[HHINCOME<=`3` &HHINCOME>`2`, income_quintile:=3]
data_qints[HHINCOME<=`4` &HHINCOME>`3`, income_quintile:=4]
data_qints[HHINCOME<=`5` &HHINCOME>`4`, income_quintile:=5]

# create crosswak of family IDs and income
family_income_xwalk <- unique(subset(data_qints, select=c("YEAR", "CPSID","HHINCOME", "income_quintile" )), by=c("YEAR", "CPSID"))

##### merge income quintiles with boomerang kids ####
# merge annual family income onto boomerang kids monthly data, keeping only a boomerang kid's first
# observation. 
boomerang_inc <- subset(merge(ordered, family_income_xwalk, by=c("YEAR", "CPSID")),pers_first_date==gsub(x=substr(date, 1,7), pattern="-", ""))

###### boomerang kids by income #####
# calculate the percent of boomeang kids in each income quintile
boomerang_inc[, flag:=1]
# count all boomerang kids
boomerang_inc[,total:=sum(flag)]
# count boomerang kids by income quintile
boomerang_inc[,inc_total:=sum(flag), by="income_quintile"]
# calculate the percent of boomerang kids in each income quintile
boomerang_inc[,inc_pct:=100*sum(flag)/total, by=c("income_quintile")]

###### nonboomerang kids #####
# subset to young adults in the pandemic not living with a parent
ya_ordered <- subset(monthly_files, date>="2020-03-01" & PELNPAR2==0 & PELNPAR1==0 & AGE>=24 & AGE<=29)
# order by person id and date
ya_ordered <- ya_ordered[order(CPSIDP, date)]

##### Combined Income Bar chart #####
# merge young adults living separately from their parents with their
# household income
nonboomerang_inc <- unique(merge(ya_ordered, family_income_xwalk, by=c("YEAR", "CPSID")), by="CPSIDP")
nonboomerang_inc[,flag:=1]
# count young adults by income quintile
nonboomerang_inc[, inc_total:=sum(flag), by="income_quintile"]
# calculate percent of nonboomerang young adults in each
# income quintile
nonboomerang_inc[, inc_pct:=100*inc_total/sum(flag)]
# create text variables for graph titles
boomerang_inc$variable <- "Boomerang kids"
nonboomerang_inc$variable <- "Young adults not living with parents"
# bind boomerang and nonboomerang data
income_data<-rbind(boomerang_inc, nonboomerang_inc, fill=T)
# Create variable for character income quintiles 
income_data[income_quintile==1, quintile:="First"]
income_data[income_quintile==2, quintile:="Second"]
income_data[income_quintile==3, quintile:="Third"]
income_data[income_quintile==4, quintile:="Fourth"]
income_data[income_quintile==5, quintile:="Fifth"]
income_data[, quintile:=factor(quintile, 
                               levels = c("First", "Second", "Third", "Fourth", "Fifth"))]
# create text variable for income ranges of each quintile
# put line breaks in text for a clean x-axis
income_data[income_quintile==1, income_range:="< $27,000"]
income_data[income_quintile==2, income_range:="$27,000 to\n $52,000"]
income_data[income_quintile==3, income_range:="$52,000 to\n $84,000"]
income_data[income_quintile==4, income_range:="$84,000 to\n $140,000"]
income_data[income_quintile==5, income_range:="$140,000+"]
income_data[, income_range:=factor(income_range, 
                                   levels=c("< $27,000",
                                            "$27,000 to\n $52,000",
                                            "$52,000 to\n $84,000",
                                            "$84,000 to\n $140,000",
                                            "$140,000+"))]

##### Figure 6 - Percent of young adults by income quintiles
ggplot(unique(income_data, by=c("quintile", "variable")), aes(x=income_range,y=inc_pct, fill=variable))+
  geom_bar(position="dodge", stat="identity")+
  theme_classic()+
  theme(axis.title = element_text(size=15, color="black"),
        axis.text.y=element_text(size=20, color="black"),
        axis.text.x=element_text(size=20, color="black"),
        strip.background = element_blank(),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.background = element_rect(colour = "black", size=1),
        plot.title=element_text(size=20, color="black"),
        legend.key.size = unit(.5, 'cm'),
        legend.text=element_text( size=15),
        legend.text.align = 1,
        legend.title=element_blank(),
        legend.key = element_rect(colour = "transparent", fill="transparent"),
        legend.background = element_rect(linetype = 1, size = 0.5, colour = 1),
        legend.justification = c(0,1),
        legend.position=c(0,1))+
  scale_y_continuous()+
  scale_fill_manual(values=c(color3, color2))+
  ggtitle("Figure 6: Percent of young adults in each income quintile.")+
  labs(x="Household income", y="Percent of young adults in quintile")

ggsave(paste0(Chart_Path, "Figure_6_older_ya_income.png"), width = 12, height=6, unit="in")

# combine detailed labor force categories into broad categories 
# merge all young adults with income
# bind datasets boomerang and nonboomerang kids
all_ya_ordered <- rbind(ordered, ya_ordered, fill=T)
# subset to each persons' first observation date
all_ya_unique <- subset(all_ya_ordered, pers_first_date==gsub(x=substr(date, 1,7), pattern="-", "") )
all_ya_unique[,flag:=1]

# merge in family income levels
unique_boomerang_inc <- subset(merge(all_ya_unique, family_income_xwalk, by="CPSID"),boomerang_kid==1)
unique_boomerang_inc[,flag:=1]


##### labor force status ####
ya_ordered[,date:=as.Date(date)]
ya_ordered[, variable:="All young adults"]
ordered[, variable:="Boomerang kids"]

# create unique data sets of boomerang kids and young adults not living with parents
# only keeping the first observation of each person
unique_boomerang <- subset(ordered, pers_first_date==gsub(x=substr(date, 1,7), pattern="-", "") )
unique_ya <- subset(ya_ordered, pers_first_date==gsub(x=substr(date, 1,7), pattern="-", "") )
# bind boomerang and nonboomerang unique datasets together
all_ya <- unique(rbind(unique_ya, unique_boomerang, fill=TRUE ), by="CPSIDP")
all_ya[, flag:=1]
#merge all young adults and income 
all_ya_inc <- merge(all_ya, family_income_xwalk, by="CPSID")
# replace NAs in boomerang_kids variable with 0
all_ya_inc[is.na(boomerang_kid), boomerang_kid:=0]
# if employed, set new employment variable to "Employed"
all_ya_inc[EMPSTAT %in% c(1,10:12), emp:="Employed" ]
# if unemployed, set new employment varaible to "Unemployed"
all_ya_inc[EMPSTAT %in% c(20:22), emp:="Unemployed" ]
# if currently enrolled in school, or not in the labor force and major activity was
# attending school, set new emp varaible to "In school"
all_ya_inc[SCHLCOLL %in% 1:4 | NILFACT==3, emp:="In school" ]
# everyone not currently employed, unemployed, or in school, set to "Not in labor force"
all_ya_inc[is.na(emp), emp:="Not in labor force" ]
# sum young adults by boomerang status and income quintile
all_ya_inc[, total:=sum(flag, na.rm=T), by=c("variable", "income_quintile")]
# sum young adults by boomerang status, labor force status, and income quintile
all_ya_inc[, emp_count:=sum(flag, na.rm=T), by=c("variable", "emp", "income_quintile")]
# calculate the percent of boomerang and nonboomerang kids in each income quintile that
# are in each labor force status
all_ya_inc[, pct_emp:=100*emp_count/total]
# label income quintiles as a character 
all_ya_inc[income_quintile==1, quintile:="First"]
all_ya_inc[income_quintile==2, quintile:="Second"]
all_ya_inc[income_quintile==3, quintile:="Third"]
all_ya_inc[income_quintile==4, quintile:="Fourth"]
all_ya_inc[income_quintile==5, quintile:="Fifth"]
all_ya_inc[, quintile:=factor(quintile, 
                              levels = c("First", "Second", "Third", "Fourth", "Fifth"))]

##### Figure 7 - Employment status bar chart
ggplot(unique(all_ya_inc, by=c("variable", "emp", "quintile")), aes(x=quintile, y=pct_emp, fill=emp))+
  geom_bar(stat="identity", position="dodge")+
  facet_wrap(.~variable, )+
  theme_classic()+
  theme(axis.title = element_text(size=15, color="black"),
        axis.text.y=element_text(size=15, color="black"),
        axis.text.x=element_text(size=15, color="black"),
        strip.background = element_blank(),
        strip.text = element_text(size=15),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.background = element_rect(colour = "black", size=1),
        plot.title=element_text(size=20, color="black"),
        legend.key.size = unit(.5, 'cm'),
        legend.text=element_text( size=15),
        legend.text.align = 1,
        legend.title=element_blank(),
        legend.key = element_rect(colour = "transparent", fill="transparent"),
        legend.background = element_rect(linetype = 1, size = 0.5, colour = 1),
        legend.position="right")+
  scale_y_continuous()+
  scale_fill_manual(values=c(color1, color2, color3, color4))+
  ggtitle("Figure 7: Percent of young adults in each labor force status by income quintile.")+
  labs(x="Household income quintile", y="Percent of young adults in income quintile")

ggsave(paste0(Chart_Path, "Figure_7_older_boomerang_vs_ya_empstat_by_inc.png"), width = 12, height=5, unit="in")

###### Young adults living at home who are students #####
ya<-subset(monthly_files, AGE>=18 & AGE<=29)
ya[, flag:=1]
# calculate total young adults per month using the person weights
ya[, total:=sum(WTFINL), by="date"]

# remove NAs from variables for line number of parents
ya[is.na(PELNPAR2), PELNPAR2:=0]
ya[is.na(PELNPAR1), PELNPAR1:=0]
ya[is.na(PELNDAD), PELNDAD:=0]
ya[is.na(PELNMOM), PELNMOM:=0]

# if line number for any parents is not zero, create flag for young adult living with parent
ya[PELNPAR2!=0|PELNPAR1!=0|PELNDAD!=0|PELNMOM!=0, parents_flag:=1]
ya[, parents_total:=sum(WTFINL), by=c("date", "parents_flag")]
ya[, pct_live_at_home:=parents_total/total]
ya[, date:=as.POSIXct(date)]

# if a person says they are in school or if their labor force status is NILF, student create
# variable to flag they are a student
ya[EMPSTAT==33 | SCHLCOLL %in% c(1:4), in_school:=1]
ya[is.na(in_school), in_school:=0]
ya[, in_school_total:=sum(WTFINL), by=c("date", "in_school", "parents_flag")]
ya[, pct_in_school:=in_school_total/parents_total]

# create small data set with one row per month of the percent of ya living at home who are students
ya_plot_data<-unique(ya[in_school==1 & parents_flag==1], by="date")

ya_plot_data_lag<-copy(ya_plot_data)
# in years prior to 2021, add one to the year
ya_plot_data_lag[YEAR<2021, YEAR:=YEAR+1]
# in 2021 add 2 to the year so 2019 will be the base year
ya_plot_data_lag[YEAR==2021, YEAR:=YEAR+2]
# merge lagged data 
lagged_data<-merge(ya_plot_data, ya_plot_data_lag, by=c("YEAR", "MONTH"), suffixes = c("", "_lag"))
# calculate percent change 
lagged_data[, yoy_in_school_change:=100*(pct_in_school-pct_in_school_lag)/pct_in_school_lag]

##### Figure 8 - YoY change in young adults living at home who are students ####
ggplot()+
  geom_hline(yintercept = seq(-8,4, by=4),size=0.2, linetype=2)+
  geom_line(lagged_data, mapping=aes(x=date, y=yoy_in_school_change),size=1.75, color=color2)+
  theme_classic()+
  theme(axis.title = element_text(size=20, color="black"),
        axis.text.y=element_text(size=20, color="black"),
        axis.text.x=element_text(size=20, color="black"),
        strip.background = element_blank(),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.background = element_rect(colour = "black", size=1),
        plot.title=element_text(size=20, color="black"),
        legend.key.size = unit(0, 'cm'),
        legend.text=element_text( size=10),
        legend.text.align = 1,
        legend.title=element_blank(),
        legend.key = element_rect(colour = "transparent", fill="transparent"),
        legend.background = element_rect(linetype = 1, size = 0.5, colour = 1),
        legend.justification = c(0,1),
        legend.position=c(0,1))+
  scale_y_continuous()+
  scale_x_datetime(date_breaks = "1 year", date_labels="%Y") +
  labs(title="Percent change in young adults living with parents who are students",
       subtitle = "YoY % change, 2021 base year is 2019", y="Percent Change", x="")

ggsave(paste0(Chart_Path, "Figure_8_yoy_perc_student_live_w_parents.png"), width = 12, height=5, unit="in")

##### Pre-pandemic baseline #####

####### percent that become employed in each occupation #####
# subset to all young adults expect the previously identified IDs with demographic mismatches
all_ya_ordered<-subset(monthly_files, AGE>=17 & AGE<=30 & !(CPSIDP %chin% people_mismatch$CPSIDP))[order(CPSIDP,date)]
all_ya_ordered[DURUNEMP==999, DURUNEMP:=NA]
# find the first and last date we observe each person
all_ya_ordered[, date:=ymd(date)]
all_ya_ordered[, pers_last_date:=max(date), by="CPSIDP"]
all_ya_ordered[, pers_first_date:=NULL]
all_ya_ordered[, pers_first_date:=min(date), by="CPSIDP"]
# create a forward looking variable with each persons employment status in the next month 
all_ya_ordered$next_emp<-c(all_ya_ordered$EMPSTAT[2:length(all_ya_ordered$EMPSTAT)],NA)
all_ya_ordered[pers_last_date==date, 
               next_emp:=EMPSTAT]

# If a person is unemployed and next month they are employed, flag new employment
all_ya_ordered[EMPSTAT %in% c(21,22) & next_emp %in% c(10:12), unemp_to_emp:=1]

# create backwards looking variable for employment
all_ya_ordered$recent_emp<-c(NA, all_ya_ordered$EMPSTAT[1:(length(all_ya_ordered$EMPSTAT)-1)])
all_ya_ordered[date==pers_first_date, recent_emp:=EMPSTAT]

# if a person goes from employed to NILF or unemployed, flag a new nonemployment spell
all_ya_ordered[recent_emp %in% c(10:12) & EMPSTAT %in% c(21:22, 30:36), new_unemployment_flag:=1]
# if a person enters the survey unemployed, flag a new unemployment
all_ya_ordered[date==pers_first_date & EMPSTAT %in% c(21:22),new_unemployment_flag:=1]

# For people who have a nonemployment spell after their first employment
# I can measure the length of the nonemployment. Include them as new
# employments if they become reemployed.
# This might introduce downward bias in the length of nonemployment
# since I am only including NILF spells that are necessarily very short.
# # Find first date of employment for every person
all_ya_ordered[EMPSTAT %in% 10:12, emp_date:=date]
all_ya_ordered[, first_emp_date:=min(emp_date, na.rm=T), by="CPSIDP"]

# If a person is NILF to employed and they have been employed before,
# flag new employment
all_ya_ordered[EMPSTAT %in% 30:36 & next_emp %in% c(10:12) & date>first_emp_date,
               unemp_to_emp:=1]

# when a person becomes nonemployed, save the date when they were most recently
# employed
all_ya_ordered[!(next_emp %in% 10:12) & EMPSTAT %in% 10:12, last_employment_flag:=1]
# create a new variable of the start date of each unemployment spell for each person
all_ya_ordered[last_employment_flag==1, last_employment_date:=date]
# fill start of nonemployment spell downwards by person
all_ya_ordered<-all_ya_ordered %>% group_by(CPSIDP) %>% fill(last_employment_date, .direction = "down") %>% as.data.table()
##### Count duration of nonemployment for NLF 
# for unemployed and NILF, calculate how long it has been since the start of the nonemployment spell in weeks
all_ya_ordered[EMPSTAT %in% c(20:22, 30:36), dur_nonemp:=round(interval(last_employment_date, date)/weeks(1))]
all_ya_ordered[EMPSTAT %in% c(20:22, 30:36) & is.na(last_employment_date), dur_nonemp:=round(interval(pers_first_date, date)/weeks(1))]

# for people who entered unemployed, 
#save their length of unemployment before they entered the survey and add
# it to the duration they were in the survey before becoming employed 
all_ya_ordered[date==pers_first_date & EMPSTAT %in% 21:22, enter_unemp:=1]
all_ya_ordered[enter_unemp==1, enter_unemp_weeks:=DURUNEMP]
all_ya_ordered[is.na(enter_unemp_weeks),enter_unemp_weeks:=0]
all_ya_ordered[, enter_unemp_weeks:=max(enter_unemp_weeks, na.rm=T), by="CPSIDP"]
all_ya_ordered[dur_nonemp>DURUNEMP | is.na(DURUNEMP), duration:=dur_nonemp]
all_ya_ordered[enter_unemp_weeks>0, duration:=dur_nonemp+enter_unemp_weeks]
all_ya_ordered[is.na(duration), duration:=DURUNEMP]

#If a person enters NILF and stays NILF for longer than 26 weeks
# flag that nonemployment spell
all_ya_ordered[new_unemployment_flag==1, unemp_date:=date]
all_ya_ordered[,first_unemp_flag:=min(unemp_date, na.rm=T), by="CPSIDP"]
all_ya_ordered[dur_nonemp>=26 & date<first_unemp_flag, new_unemployment_flag:=1]


# Create  recent occupation variable that is filled downwards so 
# it could includes very old employments(cookie sellers)
# these will be used in our occupational risk analysis
all_ya_ordered$recent_occ<-c(NA, all_ya_ordered$OCC[1:(length(all_ya_ordered$OCC)-1)])
all_ya_ordered[date==pers_first_date, recent_occ:=OCC]
all_ya_ordered[OCC==0, OCC:=NA]
all_ya_ordered<-all_ya_ordered %>% group_by(CPSIDP) %>% fill(OCC, .direction = "down") %>% as.data.table()
all_ya_ordered$recent_occ<-c(NA, all_ya_ordered$OCC[1:(length(all_ya_ordered$OCC)-1)])
all_ya_ordered[date==pers_first_date, recent_occ:=OCC]
all_ya_ordered[date==pers_first_date & EMPSTAT %in% 30:36, enter_nilf:=1]
all_ya_ordered$recent_weight<-c(NA, all_ya_ordered$WTFINL[1:(length(all_ya_ordered$WTFINL)-1)])
all_ya_ordered[date==pers_first_date, recent_weight:=WTFINL]

# subset to before the pandemic
pandemic_ya_ordered <- subset(all_ya_ordered, date<"2020-03-01" & AGE>=18 & AGE<=29)

pandemic_ya_ordered[unemp_to_emp==1 , short_unemp:=ifelse(duration<26,1,0)]
pandemic_ya_ordered[WNFTLOOK==20, short_unemp:=0]
pandemic_ya_ordered[is.na(parents_flag) , parents_flag:=0]

# create a new variable of the start date of each unemployment spell for each person
pandemic_ya_ordered[new_unemployment_flag==1, new_unemployment_date:=date]

# find the last date that a person becomes unemployed
pandemic_ya_ordered[, last_unemployment_date:=max(new_unemployment_date, na.rm=T), by="CPSIDP"]
# find the last date that a person become reemployed
pandemic_ya_ordered[unemp_to_emp==1,last_unemp_to_emp:=max(date,na.rm=T), by=CPSIDP]
pandemic_ya_ordered[,last_unemp_to_emp:=max(last_unemp_to_emp,na.rm=T), by=CPSIDP]
pandemic_ya_ordered[,pers_last_date:=NULL]
pandemic_ya_ordered[,pers_last_date:=max(date, na.rm=T), by=CPSIDP]

##### Remove new unemployment spells that we do not see the end of
# and that the end of would be more than 26 weeks in the future.
# This introduces downward bias on the length of nonemployment
# becasue we are keeping spells that end prior to the 26 weeks mark
# and dropping those of indefinite length .
# We could remove all spells that start less than 26 weeks before the 
# person leaves the survey but it would substantially decrease our sample
# but does more than double the differential between the percent of people
# living with versus without their parents that become reemployed.
# We could also consider using a shorter week cutoff. 
pandemic_ya_ordered[last_unemp_to_emp<new_unemployment_date &new_unemployment_flag==1 &
                      (ymd(new_unemployment_date)+ weeks(26))>pers_last_date, 
                    new_unemployment_flag:=0]
pandemic_ya_ordered[new_unemployment_flag==1]

# what percent of reentrants whose length nilf we can't see?
pandemic_ya_ordered[unemp_to_emp==1 & duration<26 & enter_unemp==1 & WHYUNEMP==5]
pandemic_ya_ordered[unemp_to_emp==1 & duration<26]

##### start risk index calculation #####
# Online link: https://www2.census.gov/programs-surveys/demo/guidance/industry-occupation/2018-occupation-code-list-and-crosswalk.xlsx
in_xwalk <- read_excel("2018-occupation-code-list-and-crosswalk.xlsx", sheet="2018 Census Occ Code List", skip = 4)
xwalk <- subset(in_xwalk,!is.na(`2018 Census Code`), select=2:4)
colnames(xwalk) <- c("title", "OCC", "soc_code")


#### COVID Occupational Risk Exposure Index Replicated from Beland, Brodeur, and Wright ####
# read in the onet work activities survey results
# Online link: https://www.onetcenter.org/dictionary/25.0/excel/work_activities.html
in_work_activities <- as.data.table(read_excel("onet_work_activities.xlsx"))

# read in the onet work context survey results
# Online link: https://www.onetcenter.org/dictionary/25.0/excel/work_context.html
in_work_context <- as.data.table(read_excel("onet_work_context.xlsx"))


# subset O*NET work context survey to just the two most relevant questions
onet_categories_full <- subset(in_work_context, `Element Name` %chin% c( "Physical Proximity", "Exposed to Disease or Infections",
                                                                       "Deal With External Customers", "Face-to-Face Discussions" ),
                             select=c("O*NET-SOC Code", "Title", "Element Name", "Scale Name", "Category", "Data Value", "N"))

# change the column names to be easier to work with
colnames(onet_categories_full) <- c("occ_code", "job_title","variable", "scale","category", "value", "N" )

# save the maximum score of the question in a new columns, scale_max.Right now scales are reported as "Context (Categories 1-max #)". Use substring to just save the max #
onet_categories_full[, scale_max:=as.numeric(substr(scale, 23,23))]
onet_categories_full[, scale_max:=max(scale_max, na.rm = TRUE), by=c("occ_code", "variable")]

# subset to the rows with the average score for the occupation
onet_categories <- subset(onet_categories_full, scale=="Context")

# create consistently scaled scores for each of the work contexts we will 
# include in the overall risk index
onet_categories[variable=="Physical Proximity", proximity_lessthanarmslength:=(value-1)/(scale_max-1)]

onet_categories[variable=="Exposed to Disease or Infections", disease_onceaweek:=(value-1)/(scale_max-1)]

onet_categories[variable=="Face-to-Face Discussions" , facetoface_everyday:=(value-1)/(scale_max-1)]

onet_categories[variable=="Deal With External Customers" , externalcustomers_veryimportant:=(value-1)/(scale_max-1)]

# subset the work activities survey to just the activity we want to include
# in our overall risk index
onet_activities_full <- as.data.table(subset(in_work_activities, `Element Name`=="Assisting and Caring for Others" & 
                                             `Scale Name`=="Importance",
                                           select=c("O*NET-SOC Code", "Title", "Element Name", "Scale Name", 
                                                    "Data Value", "N")))

# change the column names to be easier to work with
colnames(onet_activities_full) <- c("occ_code", "job_title","variable", "scale", "value", "N" )

# scale survey responses
onet_activities_full[, care_for_others:=(value-1)/(5-1)]

# subset just to the occupation code and the care for others activity index
care_for_others <- subset(onet_activities_full, select=c("occ_code", "care_for_others"))

# subset to the occupation code and the work context indices we created
onet_context_categories_flags <- subset(onet_categories, select=c(occ_code, job_title,
                                                                facetoface_everyday, proximity_lessthanarmslength,
                                                                disease_onceaweek, externalcustomers_veryimportant, N))
# merge O*NET work context and work activities indices
onet_categories_flags <- merge(onet_context_categories_flags, care_for_others, by="occ_code")

# gather into long format
onet_categories_long <- as.data.table(gather(onet_categories_flags, key="variable", value="flag", c(-"occ_code", -"job_title", -"N")))
onet_categories_long[is.na(flag), flag:=0]
# save the maximum index value for each occupation-context/activity pair
onet_categories_long[, flag:=max(flag, na.rm = TRUE), by=c("occ_code", "variable")]
# save the maximum number of respondents for each occupation
onet_categories_long[, n:=max(N, na.rm = TRUE), by=c("occ_code")]
# remove duplicate occupation- context/activity pairs
onet_categories_flagged <- unique(onet_categories_long, by=c("occ_code", "variable"))
# spread the data into wide format
onet_categories_risky <- as.data.table(spread(onet_categories_flagged, key = variable, value=flag))

# create index of how important face-to-face interaction with external 
# customers is in each occupation 
onet_categories_risky[, externalcustomers_veryimportant_and_face_to_face:=sqrt(externalcustomers_veryimportant * facetoface_everyday), by="occ_code"]

# create overall occupational risk index as the mean of the individual indices
onet_categories_risky[, high_risk:=mean(c(proximity_lessthanarmslength,
                                          care_for_others, disease_onceaweek, externalcustomers_veryimportant_and_face_to_face)),
                      by="occ_code"]


# prepare the O*NET data to be merged by removing extra columns.
onet_merge <- subset(onet_categories_risky, select=c("occ_code",  "high_risk",  "n"))

# remove duplicate rows
indices_merge <- unique(onet_merge)
indices_merge[, soc_code:=substr(occ_code, 1,7)]
risk_xwalk <- merge(xwalk, indices_merge, by='soc_code')

pandemic_ya_ordered[, OCC:=as.character(OCC)]
pandemic_ya_ordered[, parents_char:=ifelse(parents_flag==1, "Live with parents", "Do not live with parents")]

risk_xwalk <- as.data.table(risk_xwalk)
risk_xwalk[, high_risk:=weighted.mean(high_risk, w = n, na.rm=T), by="OCC"]
risk_xwalk_small <- unique(risk_xwalk, by="OCC")
emp_risk_table <- merge(pandemic_ya_ordered, risk_xwalk_small, by="OCC")
emp_risk_table[, health_care:=ifelse((substr(soc_code,1,2) %chin% c("29", "31")),
                                     "Healthcare",
                                     "Not healthcare")]

###### Re-employment and income #####


# become employed within 26 weeks - Table 2 (appendix 4), row 1
prop.test(c(sum(pandemic_ya_ordered[parents_flag==1]$short_unemp, na.rm=T),
            sum(pandemic_ya_ordered[parents_flag==0]$short_unemp, na.rm=T)),
          c(sum(pandemic_ya_ordered[parents_flag==1]$new_unemployment_flag, na.rm=T),
            sum(pandemic_ya_ordered[parents_flag==0]$new_unemployment_flag, na.rm=T)))
# NOTE: The percent of young adults living with parents who are unemployed 
# and become employed within 26 weeks is 64.80%, not 64.73% as written in the published Commentary

# duration of nonemployment - Table 2 (appendix 4), row 2
t.test(pandemic_ya_ordered[ unemp_to_emp==1 & parents_flag==1]$duration, 
       pandemic_ya_ordered[unemp_to_emp==1 &  parents_flag==0]$duration)

# Set a threshold to be considered "high risk" and flag people in those occupations
emp_risk_table[, risk_flag:=ifelse(high_risk<=.7, 0,1)]
# if the current occupation code does not match the most recent occupation code
# and the person is listed as employed, flag that they changed occupations
emp_risk_table[OCC!=recent_occ & EMPSTAT %in% 10:12, occ_change:=1]
# save unique employment spells by person and occupation, among job changers
unique_employments<-unique(emp_risk_table[EMPSTAT %in% 10:12],by=c("OCC", "CPSIDP"))

# employed in a high risk occupation - Table 2 (appendix 4), row 3
t.test(unique_employments[EMPSTAT %in% 10:12 & parents_flag==1]$risk_flag, 
       unique_employments[EMPSTAT %in% 10:12 & parents_flag==0]$risk_flag)
# NOTE: The percent of young adults living with parents who are employed 
# and working in a high risk occupation is 6.35%, not 6.36% as written in the published Commentary

# t.test if employed people switch into high risk occupation - Table 2 (appendix 4), row 4
t.test(unique_employments[ occ_change==1 & parents_flag==1]$risk_flag, 
       unique_employments[occ_change==1 & parents_flag==0]$risk_flag)
