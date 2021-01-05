
rm(list=ls())

my.library <- function() {
        library(survey)
        library(tidyverse)    
        library(ggplot2)
        library(NHANES)
        library(Hmisc)
        library(broom)
}

my.library()
getwd()
setwd('/Users/Joon/Box/Research box/Active Projects/Abdominal obesity NHANES/data/2017-2018/core')

#import data manually...
nhanes_demo <- sasxport.get('DEMO_J.XPT')
nhanes_bp <- sasxport.get('BPX_J.XPT')
nhanes_bpq <- sasxport.get('BPQ_J.XPT')
nhanes_hb <- sasxport.get('GHB_J.XPT')
nhanes_bodymeasure <- sasxport.get('BMX_J.XPT')
nhanes_glucose <- sasxport.get('GLU_J.XPT')
nhanes_hdl <- sasxport.get('HDL_J.XPT')
nhanes_totalchol <- sasxport.get('TCHOL_J.XPT')
nhanes_preglab <- sasxport.get('UCPREG_J.XPT')
nhanes_dmq <- sasxport.get('DIQ_J.XPT')

nhanes.combined <- left_join(nhanes_demo,nhanes_bp,by='seqn')
nhanes.combined <- left_join(nhanes.combined,nhanes_bpq,by='seqn')
nhanes.combined <- left_join(nhanes.combined,nhanes_hb,by='seqn')
nhanes.combined <- left_join(nhanes.combined,nhanes_bodymeasure,by='seqn')
nhanes.combined <- left_join(nhanes.combined,nhanes_glucose,by='seqn')
nhanes.combined <- left_join(nhanes.combined,nhanes_preglab,by='seqn')


class(nhanes.combined$seqn)
class(nhanes_hdl$seqn)

nhanes.combined %>% 
    mutate(seqn=as.character(seqn)) -> nhanes.combined

nhanes_hdl %>% 
    mutate(seqn=as.character(seqn)) -> nhanes_hdl

nhanes_totalchol %>% 
    mutate(seqn=as.character(seqn)) -> nhanes_totalchol

nhanes_dmq %>% 
    mutate(seqn=as.character(seqn)) -> nhanes_dmq

nhanes.combined <- left_join(nhanes.combined,nhanes_hdl,by='seqn')
nhanes.combined <- left_join(nhanes.combined,nhanes_totalchol,by='seqn')
nhanes.combined <- left_join(nhanes.combined,nhanes_dmq,by='seqn')

write.csv(nhanes.combined, file='nhanes.1718.csv',row.names = F)

#we will select : select(seqn, bmxwaist,bmxbmi,ridageyr,riagendr,urxpreg,ridreth1,lbdhdd,lbdhddsi,bpxsy1,bpxsy2,bpxsy3,bpxsy4,wtint2yr,wtmec2yr,sdmvpsu,sdmvstra,bpxdi1,bpxdi2,bpxdi3,bpxdi4,bpq020,bpq030,bpq040a,bpq050a,lbxglu,diq050,diq070,lbxgh)

#get the raw file
rm(list=ls())
setwd('/Users/Joon/Box/Research box/Active Projects/Abdominal obesity NHANES/nhanes_r/NHANS_abdominal_obesity')
wc.nhanes <- read.csv('nhanes.1718.raw.csv')
wc.nhanes <- as.data.frame(wc.nhanes)
str(wc.nhanes)
sum(wc.nhanes$wtmec2yr)

#change all integer to number for now
wc.nhanes %>% 
    mutate_if(is.integer,as.numeric) -> wc.nhanes

class(wc.nhanes)
table(is.na(wc.nhanes$bmxwaist))
table(is.na(wc.nhanes$seqn))
#filter age >= 20 + #making age category
hist(wc.nhanes$ridageyr) 
hist(nhanes_demo$ridageyr)
table(is.na(wc.nhanes$ridageyr))   

nhanes_demo %>% 
        filter(ridageyr<20) %>% 
        summarise(tot = n())

wc.nhanes <- wc.nhanes %>% 
        filter(ridageyr>=20) %>% 
        mutate(age.cat = ifelse(ridageyr>=20 & ridageyr<40, 1,
                                ifelse(ridageyr>=40 & ridageyr<60,2,3))) %>% 
        mutate_at(vars(age.cat),factor)
    
summary(wc.nhanes$age.cat)

#filter pregnant subjects (needs to be specific to keep NA)
table(wc.nhanes$urxpreg)
table(is.na(wc.nhanes$urxpreg))
table(wc.nhanes$ridexprg)
table(is.na(wc.nhanes$ridexprg))

wc.nhanes %>% 
    filter(is.na(urxpreg) | urxpreg !='1') %>% 
    filter(is.na(ridexprg) | ridexprg != '1') -> wc.nhanes

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
        mutate(age=ridageyr) 

#creating abdominal obesity 
table(is.na(wc.nhanes$bmxwaist))
table(is.na(nhanes_bodymeasure$bmxwaist))

wc.nhanes <- wc.nhanes %>% 
        drop_na(bmxwaist,age) %>% 
        mutate(abd_obesity =ifelse((bmxwaist>102 & sex==1) | (bmxwaist>88 & sex==2),1,0)) %>% 
        mutate_at(vars(abd_obesity), factor)
table(wc.nhanes$abd_obesity)

#recode BMI
wc.nhanes <- wc.nhanes %>% 
        mutate(bmi = bmxbmi)

#creating obesity_cat variable and remove subjects < 18.5
table(is.na(wc.nhanes$bmxbmi))

wc.nhanes %>% 
        drop_na(bmxbmi) %>% 
        mutate(obesity_cat=ifelse(bmxbmi<18.5, 1,
                                  ifelse(bmxbmi>=18.5 & bmxbmi<25, 2,
                                  ifelse(bmxbmi>=25 & bmxbmi<30, 3,4)))) %>%
        filter(obesity_cat!=1 & obesity_cat!=4) %>% 
        mutate_at(vars(obesity_cat),factor)-> wc.nhanes

table(wc.nhanes$obesity_cat)

# creating htn.cat variable 
table(is.na(wc.nhanes$bpxsy1))
table(is.na(wc.nhanes$bpxsy2))
table(is.na(wc.nhanes$bpxsy3))
table(is.na(wc.nhanes$bpxsy4))
summary(wc.nhanes$bpxsy4)
hist(wc.nhanes$bpxsy4)
table(wc.nhanes$bpq050a)

table(is.na(wc.nhanes$bpxsy1))
table(wc.nhanes$bpxsy1>130)

wc.nhanes <- wc.nhanes %>% 
    mutate(htn1 = if_else(bpxsy1>130,1,0,missing=0)) 
wc.nhanes <-wc.nhanes %>% 
    mutate(htn2 = if_else(bpxsy2>130,1,0,missing=0)) 
wc.nhanes <-wc.nhanes %>% 
    mutate(htn3 = if_else(bpxsy3>130,1,0,missing=0)) 
wc.nhanes <-wc.nhanes %>% 
    mutate(htn4 = if_else(bpxsy4>130,1,0,missing=0))

wc.nhanes <- wc.nhanes %>% 
    mutate(htn5 = if_else(bpxdi1>80,1,0,missing=0)) 
wc.nhanes <-wc.nhanes %>% 
    mutate(htn6 = if_else(bpxdi2>80,1,0,missing=0)) 
wc.nhanes <-wc.nhanes %>% 
    mutate(htn7 = if_else(bpxdi3>80,1,0,missing=0)) 
wc.nhanes <-wc.nhanes %>% 
    mutate(htn8 = if_else(bpxdi4>80,1,0,missing=0))


table(wc.nhanes$htn1)
table(wc.nhanes$htn2)
table(wc.nhanes$htn3)
table(wc.nhanes$htn4)
table(wc.nhanes$htn5)
table(wc.nhanes$htn6)
table(wc.nhanes$htn7)
table(wc.nhanes$htn8)
table(wc.nhanes$bpq050a)

wc.nhanes %>% 
    mutate(htn.cat = if_else((htn1==1 | htn2 ==1 | htn3==1 | htn4 ==1 | htn5 ==1 | htn6 ==1 | htn7 ==1| htn8 ==1| bpq050a==1), 1,0,missing=0)) -> wc.nhanes

table(wc.nhanes$htn.cat)

#create low.hdl value 
summary(wc.nhanes$lbdhdd)

wc.nhanes <- wc.nhanes %>% 
    drop_na(lbdhdd,age) %>% 
    mutate(low.hdl =ifelse((lbdhdd<40 & sex==1) | (lbdhdd<50 & sex==2),1,0)) %>% 
    mutate_at(vars(low.hdl), factor)
table(wc.nhanes$low.hdl)

#create dm value 
summary(wc.nhanes$lbxgh) #a1c
table(wc.nhanes$lbxgh>=6.5)
summary(wc.nhanes$lbxglu) #fasting glucose
table(wc.nhanes$lbxglu>=126)
table(wc.nhanes$diq050)
table(wc.nhanes$diq070)

wc.nhanes %>% 
    mutate(dm.cat = if_else((lbxgh>=6.5 | lbxglu >=126 | diq050==1 | diq070==1 ),1,0,missing=0)) -> wc.nhanes
table(wc.nhanes$dm.cat)
colnames(wc.nhanes)

ao.nhanes <- wc.nhanes %>% 
    select(seqn,wtint2yr, wtmec2yr, sdmvpsu, sdmvstra, sddsrvyr,ridstatr, age,age.cat,sex,race,bmi,abd_obesity,obesity_cat,htn.cat,low.hdl,dm.cat)

write.csv(ao.nhanes, file='nhanes.1718.project.csv',row.names = F)

##############################################################
#main analysis
rm(list=ls())

ao.nhanes <- read.csv('nhanes.1718.project.csv')
colnames(ao.nhanes)
nhanes.ao.design <- svydesign(data=ao.nhanes,
                           strata = ~sdmvstra,
                           id=~sdmvpsu, nest=T,
                           weights = ~wtmec2yr)

#check weight 
ao.nhanes %>% 
    summarise(total.weight = sum(wtmec2yr))

#change some variables as factors 
ao.nhanes %>% 
    mutate_each(funs(as.factor), c("age.cat", 'sex', 'race', 'abd_obesity', 'obesity_cat', 'htn.cat', 'low.hdl','dm.cat')) -> ao.nhanes
str(ao.nhanes)    
glimpse(ao.nhanes)

summary(ao.nhanes$race)

#ao survey table 
ao.tab <- svytable(~abd_obesity, design=nhanes.ao.design)
class(ao.tab)
ao.tab
ao.tab %>% 
    as.data.frame() %>% 
    mutate(prop=Freq/sum(Freq)) -> ao.prop.tab #trying to see how I can get the CI 
ao.prop.tab

?svyciprop
install.packages('PropCIs')
library(PropCIs)
class(ao.tab[1])

exactci(ao.tab[2],(ao.tab[1]+ao.tab[2]),conf.level=0.95) #got the CI by using Clopper–Pearson exact method. I think this is wrong. This method does not consider survey design
svyciprop(~abd_obesity, nhanes.ao.design,method='logit') #this is good but I think likelihood is correct 
svyciprop(~abd_obesity, nhanes.ao.design,method='likelihood')#this is correct. 
options(digits = 10) #change default decimal points 


#AO by sex
sex.ao.tab = svytable(~abd_obesity+sex, design=nhanes.ao.design)
sex.ao.tab
sex.ao.prop.tab = sex.ao.tab %>% 
    as.data.frame() %>% 
    group_by(sex) %>% 
    mutate(prop.ao = Freq/sum(Freq))
sex.ao.prop.tab

#create a function to make it look simple
out_tab <- function(t) {
    x <- 
        cbind(
            coef(t),
            SE(t),
            confint(t)
        ) %>% 
        format(big.mark = ",", digits = 4) %>% 
        as.data.frame
    names(x) <- c("Total", "SE", "2.5%", "97.5%")
    return(x)
}

out_tab1 <- function(t) {
    x <- 
        cbind(
            coef(t),
            confint(t)
        ) %>% 
        format(big.mark = ",", digits = 3) %>% 
        as.data.frame
    names(x) <- c("Total","2.5%", "97.5%")
    return(x)
}

svyby(formula = ~abd_obesity,
      by = ~sex, 
      design=nhanes.ao.design,
      FUN=svytotal, na.rm=T) %>% 
    out_tab1

svyby(formula = ~abd_obesity,
      by = ~sex, 
      design=nhanes.ao.design,
      FUN=svymean, na.rm=T) %>% 
    out_tab1 

confint(svyby(formula = ~abd_obesity,
              by = ~sex, 
              design=nhanes.ao.design,
              FUN=svymean, na.rm=T))

#chi-square 
obesity.sex.chis <- svychisq(~abd_obesity+sex, 
         design=nhanes.ao.design,
         statistic = 'Chisq')
tidy(obesity.sex.chis)                 

#AO by age cat
svyby(formula = ~abd_obesity,
      by = ~age.cat, 
      design=nhanes.ao.design,
      FUN=svymean, na.rm=T) %>% 
    out_tab1 

svychisq(~abd_obesity+age.cat, 
         design=nhanes.ao.design,
         statistic = 'Chisq')

#AO by age cat
svyby(formula = ~abd_obesity,
      by = ~race, 
      design=nhanes.ao.design,
      FUN=svymean, na.rm=T) %>% 
    out_tab1 

svychisq(~abd_obesity+race, 
         design=nhanes.ao.design,
         statistic = 'Chisq')

#AO by obesity cat
svyby(formula = ~abd_obesity,
      by = ~obesity_cat, 
      design=nhanes.ao.design,
      FUN=svymean, na.rm=T) %>% 
    out_tab1 

svychisq(~abd_obesity+obesity_cat, 
         design=nhanes.ao.design,
         statistic = 'Chisq')

#fit a multiple regression model
#relevel first 
relevel(weight$sex,1) # need to level first , this is an example code
elsq1ch_brr <- update( elsq1ch_brr , F1HIMATH = relevel(F1HIMATH,"PreAlg or Less") ) #something like this 

#htn
colnames(ao.nhanes)
table(ao.nhanes$age.cat)
table(ao.nhanes$low.hdl)
levels(ao.nhanes$age.cat)
table(ao.nhanes$race)
table(ao.nhanes$sex)
table(ao.nhanes$obesity_cat)
table(ao.nhanes$htn.cat)
table(ao.nhanes$abd_obesity)

ao.nhanes$age.cat <- relevel(ao.nhanes$age.cat, "1")
ao.nhanes$abd_obesity <- relevel(ao.nhanes$abd_obesity, '0')

#overall htn model
htn.logit.model.unadjust <- svyglm(htn.cat~abd_obesity, design = nhanes.ao.design, family='binomial')
summary(htn.logit.model.unadjust)
round(exp(coef(htn.logit.model.unadjust)),2)
round(exp(cbind(OR=coef(htn.logit.model.unadjust),confint(htn.logit.model.unadjust))),2)

#adjusted model 
htn.logit.model.adjust <- svyglm(htn.cat~factor(abd_obesity) + factor(age.cat) + factor(race) +factor(sex), design = nhanes.ao.design, family='binomial')
summary(htn.logit.model.adjust)
round(exp(coef(htn.logit.model.adjust)),2)
round(exp(cbind(OR=coef(htn.logit.model.adjust),confint(htn.logit.model.adjust))),2)

#what is this then??
htn.logit.model.test <- svyglm(htn.cat~abd_obesity*age.cat + abd_obesity*race +abd_obesity*sex, design = nhanes.ao.design, family='binomial')
summary(htn.logit.model.test)
round(exp(coef(htn.logit.model.test)),2)
round(exp(cbind(OR=coef(htn.logit.model.test),confint(htn.logit.model.test))),2)

#htn interaction test age
htn.logit.model1 <- svyglm(htn.cat~factor(age.cat)*factor(abd_obesity)+ factor(race) +factor(sex), design = nhanes.ao.design, family='binomial')
summary(htn.logit.model1)
round(exp(coef(htn.logit.model1)),2)

#htn interaction test sex
htn.logit.model2 <- svyglm(htn.cat~factor(sex)*factor(abd_obesity)+ factor(race) +factor(age.cat), design = nhanes.ao.design, family='binomial')
summary(htn.logit.model2)
round(exp(coef(htn.logit.model2)),2)
round(exp(cbind(OR=coef(htn.logit.model2),confint(htn.logit.model2))),2)

#htn interaction test race
htn.logit.model3 <- svyglm(htn.cat~factor(race)*factor(abd_obesity)+ fac1tor(sex) +factor(age.cat), design = nhanes.ao.design, family='binomial')
summary(htn.logit.model3)
round(exp(coef(htn.logit.model3)),2)
round(exp(cbind(OR=coef(htn.logit.model3),confint(htn.logit.model3))),2)

#I think this means nothing at this point ..
htn.logit.model.what <- svyglm(htn.cat~abd_obesity*factor(age.cat) + abd_obesity*factor(race) +abd_obesity*factor(sex), design = nhanes.ao.design, family='binomial')
summary(htn.logit.model.what)
#get the OR
exp(htn.logit.model$coefficients)
round(exp(coef(htn.logit.model)),2)

#DM
#overall dm model
dm.logit.model.unadjust <- svyglm(dm.cat~abd_obesity, design = nhanes.ao.design, family='binomial')
summary(dm.logit.model.unadjust)
round(exp(coef(dm.logit.model.unadjust)),2)
round(exp(cbind(OR=coef(dm.logit.model.unadjust),confint(dm.logit.model.unadjust))),2)

#adjusted model 
dm.logit.model.adjust <- svyglm(dm.cat~factor(abd_obesity) + factor(age.cat) + factor(race) +factor(sex), design = nhanes.ao.design, family='binomial')
summary(dm.logit.model.adjust)
round(exp(coef(dm.logit.model.adjust)),2)
round(exp(cbind(OR=coef(dm.logit.model.adjust),confint(dm.logit.model.adjust))),2)

#dm interaction test age
dm.logit.model1 <- svyglm(dm.cat~factor(age.cat)*factor(abd_obesity)+ factor(race) +factor(sex), design = nhanes.ao.design, family='binomial')
summary(dm.logit.model1)
round(exp(coef(dm.logit.model1)),2)
round(exp(cbind(OR=coef(dm.logit.model1),confint(dm.logit.model1))),2)

#dm interaction test sex
dm.logit.model2 <- svyglm(dm.cat~factor(sex)*factor(abd_obesity)+ factor(race) +factor(age.cat), design = nhanes.ao.design, family='binomial')
summary(dm.logit.model2)
round(exp(coef(dm.logit.model2)),2)
round(exp(cbind(OR=coef(dm.logit.model2),confint(dm.logit.model2))),2)

#dm interaction test race
dm.logit.model3 <- svyglm(dm.cat~factor(race)*factor(abd_obesity)+ factor(sex) +factor(age.cat), design = nhanes.ao.design, family='binomial')
summary(dm.logit.model3)
round(exp(coef(dm.logit.model3)),2)
round(exp(cbind(OR=coef(dm.logit.model3),confint(dm.logit.model3))),2)

#low hdl
#overall lowhdl model
lowhdl.logit.model.unadjust <- svyglm(low.hdl~abd_obesity, design = nhanes.ao.design, family='binomial')
summary(lowhdl.logit.model.unadjust)
round(exp(cbind(OR=coef(lowhdl.logit.model.unadjust),confint(lowhdl.logit.model.unadjust))),2)


#adjusted model 
lowhdl.logit.model.adjust <- svyglm(low.hdl~factor(abd_obesity) + factor(age.cat) + factor(race) +factor(sex), design = nhanes.ao.design, family='binomial')
summary(lowhdl.logit.model.adjust)
round(exp(cbind(OR=coef(lowhdl.logit.model.adjust),confint(lowhdl.logit.model.adjust))),2)

#lowhdl interaction test age
lowhdl.logit.model1 <- svyglm(low.hdl~factor(age.cat)*factor(abd_obesity)+ factor(race) +factor(sex), design = nhanes.ao.design, family='binomial')
summary(lowhdl.logit.model1)
round(exp(cbind(OR=coef(lowhdl.logit.model1),confint(lowhdl.logit.model1))),2)

#lowhdl interaction test sex
lowhdl.logit.model2 <- svyglm(low.hdl~factor(sex)*factor(abd_obesity)+ factor(race) +factor(age.cat), design = nhanes.ao.design, family='binomial')
summary(lowhdl.logit.model2)
round(exp(cbind(OR=coef(lowhdl.logit.model2),confint(lowhdl.logit.model2))),2)

#lowhdl interaction test race
lowhdl.logit.model3 <- svyglm(low.hdl~factor(race)*factor(abd_obesity)+ factor(sex) +factor(age.cat), design = nhanes.ao.design, family='binomial')
summary(lowhdl.logit.model3)

round(exp(cbind(OR=coef(lowhdl.logit.model3),confint(lowhdl.logit.model3))),2)
