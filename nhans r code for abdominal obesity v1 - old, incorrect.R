
rm(list=ls())

my.library <- function() {
        library(survey)
        library(tidyverse)    
        library(ggplot2)
        library(NHANES)
        library(Hmisc)
}

my.library()
getwd()
setwd('/Users/Joon/Box/Research box/Active Projects/Abdominal obesity NHANES/data/2017-2018')

#trying to write a function but it does not work... I think just a line is missing and I don't know what that is. 
getdata <- function(directory) {
        filelist <- list.files(path = directory)
        values <- data.frame()
        for (i in 1:length(filelist)) {
                imported <- sasxport.get(filelist[i])
                imported1 <- sasxport.get(filelist[i+1])
                values <- full_join(imported,imported1, by = 'seqn')
        }
        
}

getdata('/Users/Joon/Box/Research box/Active Projects/Abdominal obesity NHANES/data/2017-2018') #does not work

#import data manually...
nhanes_1 <- sasxport.get(list.files()[1])
nhanes_2 <- sasxport.get(list.files()[2])
nhanes_3 <- sasxport.get(list.files()[3])
nhanes_4 <- sasxport.get(list.files()[4])
nhanes_5 <- sasxport.get(list.files()[5])
nhanes_6 <- sasxport.get(list.files()[6])
nhanes_7 <- sasxport.get(list.files()[7])
nhanes_8 <- sasxport.get(list.files()[8])
nhanes_9 <- sasxport.get(list.files()[9])
nhanes_10 <- sasxport.get(list.files()[10])
nhanes_11 <- sasxport.get(list.files()[11])
nhanes_12 <- sasxport.get(list.files()[12])
nhanes_13 <- sasxport.get(list.files()[13])
nhanes_14 <- sasxport.get(list.files()[14])
nhanes_15 <- sasxport.get(list.files()[15])
nhanes_16 <- sasxport.get(list.files()[16])
nhanes_17 <- sasxport.get(list.files()[17])
nhanes_18 <- sasxport.get(list.files()[18])
nhanes_19 <- sasxport.get(list.files()[19])
nhanes_20 <- sasxport.get(list.files()[20])
nhanes_21 <- sasxport.get(list.files()[21])
nhanes_22 <- sasxport.get(list.files()[22])
nhanes_23 <- sasxport.get(list.files()[23])
nhanes_24 <- sasxport.get(list.files()[24])
nhanes_25 <- sasxport.get(list.files()[25])
nhanes_26 <- sasxport.get(list.files()[26])
nhanes_27 <- sasxport.get(list.files()[27])
nhanes_28 <- sasxport.get(list.files()[28])
nhanes_29 <- sasxport.get(list.files()[29])
nhanes_30 <- sasxport.get(list.files()[30])
nhanes_31 <- sasxport.get(list.files()[31])
nhanes_32 <- sasxport.get(list.files()[32])
nhanes_33 <- sasxport.get(list.files()[33])
nhanes_34 <- sasxport.get(list.files()[34])
nhanes_35 <- sasxport.get(list.files()[35])
nhanes_36 <- sasxport.get(list.files()[36])

nhanes.combined <- full_join(nhanes_1,nhanes_2,by='seqn')
nhanes.combined <- full_join(nhanes.combined,nhanes_3,by='seqn')
nhanes.combined <- full_join(nhanes.combined,nhanes_4,by='seqn')
nhanes.combined <- full_join(nhanes.combined,nhanes_5,by='seqn')
nhanes.combined <- full_join(nhanes.combined,nhanes_6,by='seqn')
nhanes.combined <- full_join(nhanes.combined,nhanes_7,by='seqn')
nhanes.combined <- full_join(nhanes.combined,nhanes_8,by='seqn')
nhanes.combined <- full_join(nhanes.combined,nhanes_9,by='seqn')
nhanes.combined <- full_join(nhanes.combined,nhanes_10,by='seqn')
nhanes.combined <- full_join(nhanes.combined,nhanes_11,by='seqn')
nhanes.combined <- full_join(nhanes.combined,nhanes_12,by='seqn')
nhanes.combined <- full_join(nhanes.combined,nhanes_13,by='seqn')
nhanes.combined <- full_join(nhanes.combined,nhanes_14,by='seqn')
nhanes.combined <- full_join(nhanes.combined,nhanes_15,by='seqn')
nhanes.combined <- full_join(nhanes.combined,nhanes_16,by='seqn')
nhanes.combined <- full_join(nhanes.combined,nhanes_17,by='seqn')
nhanes.combined <- full_join(nhanes.combined,nhanes_18,by='seqn')
nhanes.combined <- full_join(nhanes.combined,nhanes_19,by='seqn')
nhanes.combined <- full_join(nhanes.combined,nhanes_20,by='seqn')
nhanes.combined <- full_join(nhanes.combined,nhanes_21,by='seqn')
nhanes.combined <- full_join(nhanes.combined,nhanes_22,by='seqn')
nhanes.combined <- full_join(nhanes.combined,nhanes_23,by='seqn')
nhanes.combined <- full_join(nhanes.combined,nhanes_24,by='seqn')
nhanes.combined <- full_join(nhanes.combined,nhanes_25,by='seqn')
nhanes.combined <- full_join(nhanes.combined,nhanes_26,by='seqn')
nhanes.combined <- full_join(nhanes.combined,nhanes_27,by='seqn')
nhanes.combined <- full_join(nhanes.combined,nhanes_28,by='seqn')
nhanes.combined <- full_join(nhanes.combined,nhanes_29,by='seqn')
nhanes.combined <- full_join(nhanes.combined,nhanes_30,by='seqn')
nhanes.combined <- full_join(nhanes.combined,nhanes_31,by='seqn')
nhanes.combined <- full_join(nhanes.combined,nhanes_32,by='seqn')
nhanes.combined <- full_join(nhanes.combined,nhanes_33,by='seqn')
nhanes.combined <- full_join(nhanes.combined,nhanes_34,by='seqn')
nhanes.combined <- full_join(nhanes.combined,nhanes_35,by='seqn')
nhanes.combined <- full_join(nhanes.combined,nhanes_36,by='seqn')

write.csv(nhanes.combined, file='nhanes.1718.csv')

summary(nhanes.1718$bmxwaist)
summary(nhanes.1718$seqn)

#we will select : select(seqn, bmxwaist,bmxbmi,ridageyr,riagendr,urxpreg,ridreth1,lbdhdd,lbdhddsi,bpxsy1,bpxsy2,bpxsy3,bpxsy4,wtint2yr,wtmec2yr,sdmvpsu,sdmvstra,bpxdi1,bpxdi2,bpxdi3,bpxdi4,bpq020,bpq030,bpq040a,bpq050a,lbxglu,diq050,diq070,lbxgh)

#get the raw file
wc.nhanes <- read.csv('nhanes_1718_raw.csv')
table(is.na(wc.nhanes$bmxwaist))

#filter age >= 20 + #making age category
wc.nhanes <- wc.nhanes %>% 
        filter(ridageyr>=20) %>% 
        mutate(age.cat = ifelse(ridageyr>=20 & ridageyr<40, 1,
                                ifelse(ridageyr>=40 & ridageyr<60,2,3))) %>% 
        mutate_at(vars(age.cat),factor)
hist(wc.nhanes$ridageyr) 
table(is.na(wc.nhanes$ridageyr))       
summary(wc.nhanes$age.cat)

#filter pregnant subjects (cannot use filter function in dplyr because filter automatically removes NA)
table(wc.nhanes$urxpreg)
table(wc.nhanes$ridexprg)

wc.nhanes[!(wc.nhanes$urxpreg)==1,] -> wc.nhanes
wc.nhanes[!(wc.nhanes$ridexprg)==1,] -> wc.nhanes

table(wc.nhanes.test$urxpreg)
rm(wc.nhanes.test)
#creating race variable 
wc.nhanes <- wc.nhanes %>% 
        mutate(race=ridreth1) %>% 
        mutate_at(vars(race),factor)

##creating sex variable 
wc.nhanes <- wc.nhanes %>% 
        mutate(sex=riagendr) %>% 
        mutate_at(vars(sex),factor)

#recode age 
wc.nhanes <- wc.nhanes %>% 
        mutate(age=riagendr) %>% 
        mutate(age=as.factor(age))

#creating abdominal obesity 
table(is.na(wc.nhanes$bmxwaist))

wc.nhanes.test <- wc.nhanes %>% 
        drop_na(bmxwaist,riagendr) %>% 
        mutate(abd_obesity =ifelse((bmxwaist>102 & riagendr==1) | (bmxwaist>88 & riagendr==2),1,0)) %>% 
        mutate_at(vars(abd_obesity), factor)

#recode BMI
wc.nhanes <- wc.nhanes %>% 
        mutate(bmi = bmxbmi)

#creating obesity_cat variable 
wc.nhanes %>% 
        drop_na(bmxbmi) %>% 
        mutate(obesity_cat=ifelse(bmxbmi<18.5, 1,
                                  ifelse(bmxbmi>=18.5 & bmxbmi<25, 2,
                                  ifelse(bmxbmi>=25 & bmxbmi<30, 3,4)))) %>% 
        mutate_at(vars(obesity_cat),factor)-> wc.nhanes

#exclude individuals with BMI < 18.5 (group 1))
wc.nhanes %>% 
        filter(!(obesity_cat==1)) -> wc.nhanes

#htn
wc.nhanes %>% 
        drop_na(bpxsy1,bpxsy2,bpxsy3,bpxsy4) %>% 
        summarise(mean.sbp = sum(bpxsy1,bpxsy2,bpxsy3,bpxsy4)/4) 



table(wc.nhanes$bpxsy1)
