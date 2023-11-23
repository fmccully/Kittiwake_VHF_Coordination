
### VHF Kittiwake coordination study code 

####----Starting Setup----####

## @knitr setup

library(dplyr)
library(ggplot2)
library(lme4)
library(MuMIn)
library(tidyverse)
library(knitr)
library(car)
library(MASS)
library(sjPlot)
library(lmerTest)
library(blme)
library(data.table)
library(magrittr)
library(Amelia)
library(factoextra)
library(lmtest)

setwd("M:/Documents/R Practice/Kittiwakes/VHF data/Two years data")

## @knitr rolling_means_24

####----PART 1- 100 repeats of analyses----####

####----Rolling means of partner behaviour----####

total_df<-read.csv("total_df_orig.csv") # original file uploaded


# Subsetting to variables of interest for ease
kit_dat <-total_df[ ,c("ring", "start_time", "dur_hr")]
kit_dat$date <- as.Date(kit_dat$start_time,"%d/%m/%Y")
total_df$date <- as.Date(total_df$start_time, "%d/%m/%Y")


# Define window size - e.g. set  1 for daily, 7 for weekly
ws <- 1 # previous 24 hours rolling mean

# Set loop parameters
kit_rings <- unique(kit_dat$ring)
kit_list <- vector(mode = "list", length = length(kit_rings))

for (i in 1:length(kit_rings)) {
  
  mydf <- subset(kit_dat, ring == kit_rings[i])
  mydf <- mydf[order(mydf$start_time),]
  
  roll_df <- setDT(mydf)[SJ(start = seq(min(date), max(date), 1))[, end := start + ws - 1], 
                         on = .(date >= start, date <= end),
                         .(mean_dur = mean(dur_hr)), by = .EACHI]
  
  roll_df[,1] <- NULL # retain date for end of week 
  roll_df$ring <- kit_rings[i]
  
  colnames(roll_df) <- c("date", "partner_rolling_1_day_mean", "ring")
  
  kit_list[[i]] <- roll_df
  
}

# Bind up data
partner_durs_1 <- do.call("rbind", kit_list)

# merge so that represents partner behaviour i.e. with p_ring
kit_df2<-merge(x = total_df, y = partner_durs_1, by.x=c('date', 'p_ring'), by.y=c('date', 'ring'), all = TRUE) #merges each bird's score to their PARTNER 
#if their partner made a trip on that date
kit_df3<-kit_df2[!is.na(kit_df2$dur_hr),]



#Subsetting to variables of interest for ease
kit_dat <-kit_df3[ ,c("ring", "start_time", "dur_hr")]
kit_dat$date <- as.Date(kit_dat$start_time, "%d/%m/%Y")
kit_df3$date <- as.Date(kit_df3$start_time, "%d/%m/%Y")

## @knitr rolling_mean_2_7

# Define window size 
ws <- 2 # previous 48 hours rolling mean

# Set loop parameters
kit_rings <- unique(kit_dat$ring)
kit_list <- vector(mode = "list", length = length(kit_rings))

for (i in 1:length(kit_rings)) {
  
  mydf <- subset(kit_dat, ring == kit_rings[i])
  mydf <- mydf[order(mydf$start_time),]
  
  roll_df <- setDT(mydf)[SJ(start = seq(min(date), max(date), 1))[, end := start + ws - 1], 
                         on = .(date >= start, date <= end),
                         .(mean_dur = mean(dur_hr)), by = .EACHI]
  
  roll_df[,1] <- NULL # retain date for end of week 
  roll_df$ring <- kit_rings[i]
  
  colnames(roll_df) <- c("date", "partner_rolling_2_day", "ring")
  
  kit_list[[i]] <- roll_df
  
}

# Bind up data
partner_durs_2 <- do.call("rbind", kit_list)

# merge so that represents partner behaviour i.e. with p_ring
kit_df4<-merge(x = kit_df3, y = partner_durs_2, by.x=c('date', 'p_ring'), by.y=c('date', 'ring'), all = TRUE)
kit_df5<-kit_df4[!is.na(kit_df4$dur_hr),]

#3 day
#Subsetting to variables of interest for ease
kit_dat <-kit_df5[ ,c("ring", "start_time", "dur_hr")]
kit_dat$date <- as.Date(kit_dat$start_time, "%d/%m/%Y")
kit_df5$date <- as.Date(kit_df5$start_time, "%d/%m/%Y")


# Define window size 
ws <- 3 # rolling mean for previous 72 hours

# Set loop parameters
kit_rings <- unique(kit_dat$ring)
kit_list <- vector(mode = "list", length = length(kit_rings))

for (i in 1:length(kit_rings)) {
  
  mydf <- subset(kit_dat, ring == kit_rings[i])
  mydf <- mydf[order(mydf$start_time),]
  
  roll_df <- setDT(mydf)[SJ(start = seq(min(date), max(date), 1))[, end := start + ws - 1], 
                         on = .(date >= start, date <= end),
                         .(mean_dur = mean(dur_hr)), by = .EACHI]
  
  roll_df[,1] <- NULL # retain date for end of week 
  roll_df$ring <- kit_rings[i]
  
  colnames(roll_df) <- c("date", "partner_rolling_3_day", "ring")
  
  kit_list[[i]] <- roll_df
  
}

# Bind up data
partner_durs_3 <- do.call("rbind", kit_list)

# merge so that represents partner behaviour i.e. with p_ring
kit_df6<-merge(x = kit_df5, y = partner_durs_3, by.x=c('date', 'p_ring'), by.y=c('date', 'ring'), all = TRUE) 
kit_df7<-kit_df6[!is.na(kit_df6$dur_hr),]


### could be repeated 4 more times changing defined period 
#to 4-7 to get rolling means over 7 days (see Supplementary Material) 

## @knitr pca_file

#PCA
kit_df7$trip_ID<-paste(kit_df7$ring, kit_df7$nest, kit_df7$num_start, sep= "_")
kit_co_PCA<-kit_df7
row.names(kit_co_PCA)<-kit_co_PCA$trip_ID

kit_co_PCA<-kit_co_PCA[ ,c("partner_rolling_1_day_mean", "partner_rolling_2_day", "partner_rolling_3_day")]

####---- IMPUTATION LOOP STARTS HERE----####

## @knitr imputations
# Amelia II Imputations 

#EVERYTHING AFTER THIS POINT (UNTIL  SPECIFED END POINT) 
#IS REPEATED 100 TIMES

n_perms<-100 # number of repeats of analysis
for (i in 1:n_perms ) {

  setwd("M:/Documents/R Practice/Kittiwakes/VHF data/Two years data")

  # generate 5 new scores for missing data

kit_pca2<-amelia(kit_co_PCA, m = 5, ts = NULL, cs = NULL)
write.amelia(obj = kit_pca2, file.stem = "outdata") # 5 outputs, one per each set of scores generated- 
#these will be averaged to create  a final score per trip

outdata1<-read.csv("outdata1.csv")
outdata2<-read.csv("outdata2.csv")
outdata3<-read.csv("outdata3.csv")
outdata4<-read.csv("outdata4.csv")
outdata5<-read.csv("outdata5.csv")

pca_all1<-full_join(outdata1,outdata2) # join 5 scores together
pca_all2<-full_join(pca_all1,outdata3)
pca_all3<-full_join(pca_all2,outdata4)
pca_all4<-full_join(pca_all3,outdata5)

pca_all<-pca_all4

pca_all$trip_id<-as.factor(pca_all$X)
pca_all$X<-NULL
pca_all22<-drop_na(pca_all) # will eventually lose 44 trips where 
#there was no information to create imputed data 

# calculate averages of 5 figures per time period trip created with Amelia II package
ave_co<-as.data.frame(pca_all22%>%group_by(trip_id)%>%summarise(average=mean(partner_rolling_1_day_mean)))
ave_co<-rename(ave_co, c("day1"="average"))

ave_co2<-as.data.frame(pca_all22%>%group_by(trip_id)%>%summarise(average=mean(partner_rolling_2_day)))
ave_co2<-rename(ave_co2, c("day2"="average"))

ave_co3<-as.data.frame(pca_all22%>%group_by(trip_id)%>%summarise(average=mean(partner_rolling_3_day)))
ave_co3<-rename(ave_co3, c("day3"="average"))


#merge and tidy
ave_all1<-merge(ave_co, ave_co2, by.x = "trip_id", by.y="trip_id", all = TRUE)
ave_all2<-merge(ave_all1, ave_co3, by.x = "trip_id", by.y="trip_id", all = TRUE)


row.names(ave_all2)<-ave_all2$trip_id
ave_all2$trip_id<-NULL

## @knitr pca

###----3 day PCA----####

# PCA of partner behaviour/ coordination variable 

kit_pca<- prcomp(ave_all2, scale = TRUE)
kit_pca$rotation <- -1*kit_pca$rotation

# eigenvalues and variance explained 
get_eig(kit_pca) # only PC1 has eigen value >1

# weightings
kit_pca # weightings for all averages similar to each other 
kit_pca$x <- -1*kit_pca$x

# get PCA scores

kw_pca_scores_df<-as.data.frame(kit_pca$x)

## @knitr merge_pca

# merge with dataset to make 'PCA partner trip duration'
kw_pca_scores_df$trip_id<-row.names(kw_pca_scores_df)
kw_pca_scores_df$PC2<-NULL
kw_pca_scores_df$PC3<-NULL
kw_pca_scores_df$PC4<-NULL
kw_pca_scores_df$PC5<-NULL
kw_pca_scores_df$PC6<-NULL
kw_pca_scores_df$PC7<-NULL
kit_df16<-merge(kit_df7, kw_pca_scores_df, by.x = "trip_ID", by.y="trip_id", all = TRUE)

#get riud of extra variables
kit_df16$partner_rolling_1_day_mean<-NULL
kit_df16$partner_rolling_2_day<-NULL 
kit_df16$partner_rolling_3_day<-NULL  
kit_df16$partner_rolling_4_day<-NULL 
kit_df16$partner_rolling_5_day<-NULL  
kit_df16$partner_rolling_6_day<-NULL 
kit_df16$partner_rolling_7_day<-NULL 

kit_df16<-rename(kit_df16, c("part_beh"="PC1"))

total_dfx<-kit_df16


##@knitr save_pca
# save this repeat's version of the weighting and eigenvalue to report medians later

setwd("M:/Documents/R Practice/Kittiwakes/VHF data/Permutations/PCA/weights")
kit_pca2<-as.data.frame(kit_pca[["rotation"]])
kit_pca2$perm_no<-i
write.csv(kit_pca2, paste0(i, "_PCA_weights.csv"))

setwd("M:/Documents/R Practice/Kittiwakes/VHF data/Permutations/PCA/eigen")
eigen<-as.data.frame(get_eig(kit_pca))
eigen$perm_no<-i
write.csv(eigen, paste0(i, "_PCA_eigen.csv"))

####----adding hatch date----####

## @knitr hatch_date

#add no days since hatching for each pair 

b2<-subset(total_dfx, nest == "B2")
hatch_date<-as.Date("10/07/2021","%d/%m/%Y")
b2$hatch_days  <- difftime(b2$date,hatch_date)
b2$hatch_days<-b2$hatch_days/86400
b2$hatch_days<-as.integer(b2$hatch_days)

c5<-subset(total_dfx, nest == "C5")
hatch_date<-as.Date("07/07/2021","%d/%m/%Y",tz = "Europe/Oslo")
c5$hatch_days  <- difftime(c5$date,hatch_date)
c5$hatch_days<-c5$hatch_days/86400
c5$hatch_days<-as.integer(c5$hatch_days)

c3<-subset(total_dfx, nest == "C3")
hatch_date<-as.Date("11/07/2022","%d/%m/%Y")
c3$hatch_days  <- difftime(c3$date,hatch_date)
c3$hatch_days<-as.integer(c3$hatch_days)

c6<-subset(total_dfx, nest == "C6")
hatch_date<-as.Date("04/07/2021","%d/%m/%Y")
c6$hatch_days  <- difftime(c6$date,hatch_date)
c6$hatch_days<-c6$hatch_days/86400
c6$hatch_days<-as.integer(c6$hatch_days)

k1<-subset(total_dfx, nest == "K1")
hatch_date<-as.Date("13/07/2022","%d/%m/%Y")
k1$hatch_days  <- difftime(k1$date,hatch_date)
k1$hatch_days<-k1$hatch_days/86400
k1$hatch_days<-as.integer(k1$hatch_days)

d4<-subset(total_dfx, nest == "D4")
hatch_date<-as.Date("16/07/2022","%d/%m/%Y")
d4$hatch_days  <- difftime(d4$date,hatch_date)
d4$hatch_days<-d4$hatch_days/86400
d4$hatch_days<-as.integer(d4$hatch_days)

d2<-subset(total_dfx, nest == "D2")
hatch_date<-as.Date("03/07/2021","%d/%m/%Y")
d2$hatch_days  <- difftime(d2$date,hatch_date)
d2$hatch_days<-d2$hatch_days/86400
d2$hatch_days<-as.integer(d2$hatch_days)

c2<-subset(total_dfx, nest == "C2")
hatch_date<-as.Date("14/07/2022","%d/%m/%Y")
c2$hatch_days  <- difftime(c2$date,hatch_date) # this pair doesn't need /86400
c2$hatch_days<-as.integer(c2$hatch_days)

f5<-subset(total_dfx, nest == "F5")
hatch_date<-as.Date("12/07/2022","%d/%m/%Y")
f5$hatch_days  <- difftime(f5$date,hatch_date)
f5$hatch_days<-f5$hatch_days/86400
f5$hatch_days<-as.integer(f5$hatch_days)

K3<-subset(total_dfx, nest == "K3")
hatch_date<-as.Date("17/07/2022","%d/%m/%Y")
K3$hatch_days  <- difftime(K3$date,hatch_date)
K3$hatch_days<-K3$hatch_days/86400
K3$hatch_days<-as.integer(K3$hatch_days)

K5<-subset(total_dfx, nest == "K5")
hatch_date<-as.Date("14/07/2022","%d/%m/%Y")
K5$hatch_days  <- difftime(K5$date,hatch_date)
K5$hatch_days<-K5$hatch_days/86400
K5$hatch_days<-as.integer(K5$hatch_days)

l4<-subset(total_dfx, nest == "l4")
hatch_date<-as.Date("14/07/2022","%d/%m/%Y")
l4$hatch_days  <- difftime(l4$date,hatch_date)
l4$hatch_days<-l4$hatch_days/86400
l4$hatch_days<-as.integer(l4$hatch_days)

llb1<-subset(total_dfx, nest == "llb1")
hatch_date<-as.Date("16/07/2022","%d/%m/%Y")
llb1$hatch_days  <- difftime(llb1$date,hatch_date)
llb1$hatch_days<-llb1$hatch_days/86400
llb1$hatch_days<-as.integer(llb1$hatch_days)

g3<-subset(total_dfx, nest == "G3")
hatch_date<-as.Date("15/07/2022","%d/%m/%Y")
g3$hatch_days  <- difftime(g3$date,hatch_date)
g3$hatch_days<-g3$hatch_days/86400
g3$hatch_days<-as.integer(g3$hatch_days)

h2<-subset(total_dfx, nest == "H2")
hatch_date<-as.Date("12/07/2022","%d/%m/%Y")
h2$hatch_days  <- difftime(h2$date,hatch_date)
h2$hatch_days<-h2$hatch_days/86400
h2$hatch_days<-as.integer(h2$hatch_days)

i3<-subset(total_dfx, nest == "I3")
hatch_date<-as.Date("14/07/2022","%d/%m/%Y")
i3$hatch_days  <- difftime(i3$date,hatch_date)# this pair doesn't need /86400
i3$hatch_days<-as.integer(i3$hatch_days)

i5<-subset(total_dfx, nest == "I5")
hatch_date<-as.Date("06/07/2021","%d/%m/%Y")
i5$hatch_days  <- difftime(i5$date,hatch_date)
i5$hatch_days<-as.integer(i5$hatch_days)# this pair doesn't need /86400

i4<-subset(total_dfx, nest == "I4")
hatch_date<-as.Date("14/07/2022","%d/%m/%Y")
i4$hatch_days  <- difftime(i4$date,hatch_date)
i4$hatch_days<-as.integer(i4$hatch_days)# this pair doesn't need /86400

g5<-subset(total_dfx, nest == "G5")
hatch_date<-as.Date("06/07/2021","%d/%m/%Y")
g5$hatch_days  <- difftime(g5$date,hatch_date)
g5$hatch_days<-as.integer(g5$hatch_days)# this pair doesn't need /86400

d5<-subset(total_dfx, nest == "D5")
hatch_date<-as.Date("06/07/2021","%d/%m/%Y")
d5$hatch_days  <- difftime(d5$date,hatch_date)
d5$hatch_days<-as.integer(d5$hatch_days)# this pair doesn't need /86400

f3<-subset(total_dfx, nest == "F3")
hatch_date<-as.Date("14/07/2022","%d/%m/%Y")
f3$hatch_days  <- difftime(f3$date,hatch_date)
f3$hatch_days<-as.integer(f3$hatch_days)# this pair doesn't need /86400

#join all data together 
total_df2<-full_join(f3, d2)
total_df3<-full_join(total_df2, d4)
total_df4<-full_join(total_df3, k1)
total_df5<-full_join(total_df4, c6)
total_df6<-full_join(total_df5, c5)
total_df7<-full_join(total_df6, c3)
total_df8<-full_join(total_df7, K3)
total_df9<-full_join(total_df8, K5)
total_df10<-full_join(total_df9, l4)
total_df11<-full_join(total_df10, llb1)
total_df12<-full_join(total_df11, f5)
total_df13<-full_join(total_df12, h2)
total_df14<-full_join(total_df13, i3)
total_df15<-full_join(total_df14, b2)
total_df16<-full_join(total_df15, c2)
total_df17<-full_join(total_df16, g3)
total_df18<-full_join(total_df17, d5)
total_df19<-full_join(total_df18, g5)
total_df20<-full_join(total_df19, i4)
total_df21<-full_join(total_df20, i5)

total_df<-total_df21
total_df<-total_df[!is.na(total_df$part_beh),]  # remove 44 trips with no coordination variable

## @knitr transform_scale

####--- Transforming and scaling----####

#transform the dependent variable 'focal bird trip duration' and 
#'PCA partner trip duration' fixed effect
#'
total_df$dur_hrs_t<-asinh(total_df$dur_hr)
total_df$part_beh_t<-asinh(total_df$part_beh)
total_df$part_beh_t_sc<-scale(total_df$part_beh_t)
total_df$dur_hrs_t_sc<-scale(total_df$dur_hrs_t)

#scale hatch days
total_df$hatch_sc<-scale(total_df$hatch_days)

#check variable types 
total_df$year<-as.factor(total_df$year)
total_df$sex<-as.factor(total_df$sex)
total_df$nest<-as.factor(total_df$nest)

## @knitr data_subsets

#produce data subsets for models and save them

incu4<-subset(total_df, breed_stage == "incu")
brood4<-subset(total_df, breed_stage == "brood")

setwd("M:/Documents/R Practice/Kittiwakes/VHF data/Permutations/main data/incu")
incu4$perm_no<-i
write.csv(incu4, paste0(i, "_incu_data.csv"))

setwd("M:/Documents/R Practice/Kittiwakes/VHF data/Permutations/main data/brood")
brood4$perm_no<-i
write.csv(brood4, paste0(i, "_brood_data.csv"))

####----Global models----####

## @knitr global_models_incu

#run the global incubation model
incu_mod<-blmer(dur_hrs_t_sc ~  
                  sex+
                  year+
                  part_beh_t_sc+
                  part_beh_t_sc: sex+
                  hatch_sc+
                  (1|ring) + 
                  (1|nest) + 
                  (1+part_beh_t_sc||nest),
                data = incu4, na.action=na.fail, REML=FALSE,
                control=lmerControl (optimizer= "Nelder_Mead",
                                     optCtrl = list()))
summary(incu_mod)

## @knitr global_models_brood

#run the global brooding model
brood_mod<-blmer(dur_hrs_t_sc ~  
                   sex+
                   year+
                   part_beh_t_sc+
                   part_beh_t_sc: sex+
                   hatch_sc+
                   (1|ring)+
                   (1|nest)+
                   (1+part_beh_t_sc||nest),
                 data = brood4, na.action=na.fail, REML=FALSE,
                 control=lmerControl (optimizer= "Nelder_Mead",
                                      optCtrl = list()))
summary(brood_mod)

## @knitr global_models_cc

#run the global coordination comparison model
tot_mod<- blmer(dur_hrs_t_sc ~
                  part_beh_t_sc+
                  year+
                  sex+
                  breed_stage+
                  part_beh_t_sc:breed_stage+
                  (1|ring)+
                  (1|nest)+
                  (1+part_beh_t_sc||nest),
                data = total_df, na.action=na.fail, REML=FALSE,
                control=lmerControl (optimizer= "Nelder_Mead", 
                                     optCtrl = list()))

summary(tot_mod)

## @knitr incu_mod_sel

####----Incubation model selection and saving information----####

#Model selection
incu_mod2<-dredge(incu_mod, extra = alist(deviance)) # produces all possible models
m1_incu_sub<-subset(incu_mod2, delta<2) ##subsets models delta <2
nest_incu <- subset(m1_incu_sub, !nested(.))# removes more complicated models


## @knitr incu_prep

# prep for downloading csv fiules so medians of the 100 repeats can be calulcated later
# this code renames variables so that the csv files of the outputs can be downloaded properly
incu_coeffs <- as.data.frame(coeffs(incu_mod2))
colnames(incu_coeffs) [colnames(incu_coeffs) == 'part_beh_t_sc:sexMale'] <- 'part_beh_t_sc_sex'
colnames(incu_mod2)[colnames(incu_mod2) == 'part_beh_t_sc:sex'] <- 'part_beh_t_sc_sex'

incu_mod2$sex <- incu_coeffs$sexMale
incu_mod2$year <- incu_coeffs$year2022
incu_mod2$part_beh_t_sc_sex <- incu_coeffs$part_beh_t_sc_sex

nest_incu_coeffs <- as.data.frame(coeffs(nest_incu))
nest_incu<-as.data.frame(nest_incu)

colnames(nest_incu_coeffs) [colnames(nest_incu_coeffs) == 'part_beh_t_sc:sexMale'] <- 'part_beh_t_sc_sex'
colnames(nest_incu)[colnames(nest_incu) == 'part_beh_t_sc:sex'] <- 'part_beh_t_sc_sex'

nest_incu$sex <- nest_incu_coeffs$sexMale
nest_incu$year <- nest_incu_coeffs$year2022
nest_incu$part_beh_t_sc_sex <- nest_incu_coeffs$part_beh_t_sc_sex

## @knitr write_incu

#write the required incubation data files from the analysis

setwd("M:/Documents/R Practice/Kittiwakes/VHF data/Permutations/Original incubation/full")
m1_incu_models<-as.data.frame(incu_mod2)
m1_incu_models$perm_no<-i
write.csv(m1_incu_models, paste0(i, "_incu_models.csv"))


setwd("M:/Documents/R Practice/Kittiwakes/VHF data/Permutations/Original incubation/nest")
nest_incu<-as.data.frame(nest_incu)
nest_incu$perm_no<-i
write.csv(nest_incu, paste0(i, "_nest_incu.csv"))

####----Brooding model selection and saving information----####

## @knitr brood_mod_sel

#brooding model selection

brood_mod2<-dredge(brood_mod, extra = alist(deviance))
m1_brood_sub<-subset(brood_mod2, delta<2) ##subsets models delta <2
nest_brood <- subset(m1_brood_sub, !nested(.))

## @knitr brood_prep

# prep variable names for saving
brood_coeffs <- as.data.frame(coeffs(brood_mod2))
colnames(brood_coeffs) [colnames(brood_coeffs) == 'part_beh_t_sc:sexMale'] <- 'part_beh_t_sc_sex'
colnames(brood_mod2)[colnames(brood_mod2) == 'part_beh_t_sc:sex'] <- 'part_beh_t_sc_sex'

brood_mod2$sex <- brood_coeffs$sexMale
brood_mod2$year <- brood_coeffs$year2022
brood_mod2$part_beh_t_sc_sex <- brood_coeffs$part_beh_t_sc_sex

nest_brood_coeffs <- as.data.frame(coeffs(nest_brood))
colnames(nest_brood_coeffs) [colnames(nest_brood_coeffs) == 'part_beh_t_sc:sexMale'] <- 'part_beh_t_sc_sex'
colnames(nest_brood)[colnames(nest_brood) == 'part_beh_sc:sex'] <- 'part_beh_sc_sex'

nest_brood$sex <- nest_brood_coeffs$sexMale
nest_brood$year <- nest_brood_coeffs$year2022
nest_brood$part_beh_sc_sex <- nest_brood_coeffs$part_beh_sc_sex

## @knitr save_brood

# save brooding anaysis files

setwd("M:/Documents/R Practice/Kittiwakes/VHF data/Permutations/Original brooding/full")
brood_models<-as.data.frame(brood_mod2)
brood_models$perm_no<-i
write.csv(brood_models, paste0(i, "_brood_models.csv"))



setwd("M:/Documents/R Practice/Kittiwakes/VHF data/Permutations/Original brooding/nest")
#m1_brood_sub<-as.data.frame(m1_brood_sub)
#m1_brood_sub$perm_no<-i
#write.csv(m1_brood_sub, paste0(i, "_m1_brood_sub.csv"))

nest_brood
nest_brood<-as.data.frame(nest_brood)
nest_brood$perm_no<-i
write.csv(nest_brood, paste0(i, "_nest_brood_sub.csv"))

####----Coordination comparison model selection and saving information----####

## @knitr model_sel_cc

#model selection for coordination comparison model
tot_mod_dr<-dredge(tot_mod, extra = alist(deviance))
in_mod<-subset(tot_mod_dr, delta<2) ##subsets models delta <2
nest_int <- subset(in_mod, !nested(.))

## @knitr cc_prep

#prep coordination compariosn files for saving
cc_coeffs <- as.data.frame(coeffs(tot_mod_dr))
colnames(cc_coeffs) [colnames(cc_coeffs) == 'breed_stageincu:part_beh_t_sc'] <- 'breed_stage_part_beh'
colnames(tot_mod_dr)[colnames(tot_mod_dr) == 'breed_stage:part_beh_t_sc'] <- 'breed_stage_part_beh'

tot_mod_dr$sex <- cc_coeffs$sexMale
tot_mod_dr$breed_stage <- cc_coeffs$breed_stageincu
tot_mod_dr$year <- cc_coeffs$year2022
tot_mod_dr$breed_stage_part_beh <- cc_coeffs$breed_stage_part_beh

nest_int_coeffs <- as.data.frame(coeffs(nest_int))
nest_int<-as.data.frame(nest_int)

colnames(nest_int_coeffs) [colnames(nest_int_coeffs) == 'breed_stageincu:part_beh_t_sc'] <- 'breed_stage_part_beh'
colnames(nest_int)[colnames(nest_int) == 'breed_stage:part_beh_t_sc'] <- 'breed_stage_part_beh'

nest_int$sex <- nest_int_coeffs$sexMale
nest_int$breed_stage <- nest_int_coeffs$breed_stageincu
nest_int$year <- nest_int_coeffs$year2022
nest_int$breed_stage_part_beh <- nest_int_coeffs$breed_stage_part_beh

## @knitr cc_save

#save this repeat's information from coordination comparison model 
setwd("M:/Documents/R Practice/Kittiwakes/VHF data/Permutations/Cooridnation comparison/full")
tot_mod_dr<-as.data.frame(tot_mod_dr)
tot_mod_dr$perm_no<-i
write.csv(tot_mod_dr, paste0(i, "_int_mod.csv")) 


setwd("M:/Documents/R Practice/Kittiwakes/VHF data/Permutations/Cooridnation comparison/nest")
nest_int<-as.data.frame(nest_int)
nest_int$perm_no<-i
write.csv(nest_int, paste0(i, "_nest_comp.csv"))

####--- R squareds from global models----####

## @knitr save_r_sq

#Save the conditional and marginal R square values from the global incubation
#brooding and coordination comparison models (from this repeat)

setwd("M:/Documents/R Practice/Kittiwakes/VHF data/Permutations/global R squared/incu")
incu_rs<-as.data.frame(r.squaredGLMM(incu_mod))
incu_rs$perm_no<-i
write.csv(incu_rs, paste0(i, "_incu_rs.csv"))

setwd("M:/Documents/R Practice/Kittiwakes/VHF data/Permutations/global R squared/brood")
brood_rs<-as.data.frame(r.squaredGLMM(brood_mod))
brood_rs$perm_no<-i
write.csv(brood_rs, paste0(i, "_brood_rs.csv"))

setwd("M:/Documents/R Practice/Kittiwakes/VHF data/Permutations/global R squared/cc")
tot_rs<-as.data.frame(r.squaredGLMM(tot_mod))
tot_rs$perm_no<-i
write.csv(tot_rs, paste0(i, "_cc_rs.csv"))

###--- Importance of pair slopes- model fit comparison----####

## @knitr slope_sig

# generate the incubation and brooding models without the random slope to asess
#if their addition to the model improved the fit

setwd("M:/Documents/R Practice/Kittiwakes/VHF data/Permutations/slope comp/incu")
incu_mod3<-blmer(dur_hrs_t_sc ~  
                   sex+
                   year+
                   part_beh_t_sc+
                   part_beh_t_sc: sex+
                   hatch_sc+
                   (1|ring) + 
                   (1|nest),  
                 #(1+part_beh_t_sc|nest), # random slope from original model removed
                 data = incu4, na.action=na.fail, REML=FALSE,
                 control=lmerControl (optimizer= "Nelder_Mead",
                                      optCtrl = list()))
## @ comp_incu_mod

# compare new model without slope to original model with slope via 
#likelihood ratio test and prepare this repeat's information for saving

incu_slop_comp<-lrtest(incu_mod, incu_mod3)


incu_slop_comp<-as.data.frame(incu_slop_comp)
incu_slop_comp$perm_no<-i

# save information from this repeat
write.csv(incu_slop_comp, paste0(i, "_incu_slop_comp.csv"))

## @knitr brood_comp_slope

#repeat process with brooding model

setwd("M:/Documents/R Practice/Kittiwakes/VHF data/Permutations/slope comp/brood")
brood_mod3<-blmer(dur_hrs_t_sc ~  
                    sex+
                    year+
                    part_beh_t_sc+
                    part_beh_t_sc: sex+
                    hatch_sc+
                    (1|ring)+
                    (1|nest),
                  #(1+part_beh_t_sc||nest), #random slope removed in new version
                  data = brood4, na.action=na.fail, REML=FALSE,
                  control=lmerControl (optimizer= "Nelder_Mead",
                                       optCtrl = list()))

#likelihood ratio test new an old version of model
brood_slop_comp<-lrtest(brood_mod, brood_mod3)

# information from this repeat saved
brood_slop_comp<-as.data.frame(brood_slop_comp)
brood_slop_comp$perm_no<-i
write.csv(brood_slop_comp, paste0(i, "_brood_slop_comp.csv"))

####----Within-pair coordination strength model----####

## @knitr within_pair_mods

# This code relates to testing the within-pair relationship between
# coordination strength in incubation and brooding

# It also extracts information used in creation of Figure 1

# first rerun the global models so they work better in SJ plot
# global incubation model
incu_mod<-blmer(dur_hrs_t_sc ~  
                  sex+
                  year+
                  part_beh_t_sc+
                  part_beh_t_sc: sex+
                  hatch_sc+
                  (1|ring) + 
                  (1|nest) + 
                  (1+part_beh_t_sc||nest),
                data = incu4, na.action=na.fail, REML=FALSE,
                control=lmerControl (optimizer= "Nelder_Mead",
                                     optCtrl = list()))

# extracts slope coefficient (coordination strength) for each pair
ran_in<-coef(incu_mod)
incu_coefs2<-ran_in[["nest"]]
incu_coefs2<-subset(incu_coefs2, select = c( "(Intercept)", "part_beh_t_sc"))
incu_coefs2$nest<-row.names(incu_coefs2)

# extracts predictions for sample level coordination strength
inDat <- plot_model(incu_mod, type = "pred", terms = c("part_beh_t_sc"))
inDat <- data.frame(inDat$data)
inDat$part_beh_t_sc<-inDat$x
inDat$dur_hrs_t_sc<-inDat$predicted

## @knitr brood_slope_within

# process is repeated for global brooding model

brood_mod<-blmer(dur_hrs_t_sc ~  
                   sex+
                   year+
                   part_beh_t_sc+
                   part_beh_t_sc: sex+
                   hatch_sc+
                   (1|ring)+
                   (1|nest)+
                   (1+part_beh_t_sc||nest),
                 data = brood4, na.action=na.fail, REML=FALSE,
                 control=lmerControl (optimizer= "Nelder_Mead",
                                      optCtrl = list()))
# slope extracted for each pair
ran_br<-coef(brood_mod)
brood_coefs2<-ran_br[["nest"]]
brood_coefs2<-subset(brood_coefs2, select = c( "(Intercept)", "part_beh_t_sc"))
brood_coefs2$nest<-row.names(brood_coefs2)

# sample level prediction extracted
brDat <- plot_model(brood_mod, type = "pred", terms = c("part_beh_t_sc"))
brDat <- data.frame(brDat$data)
brDat$part_beh_t_sc<-brDat$x
brDat$dur_hrs_t_sc<-brDat$predicted

## @knitr rename_var

#variables renamed for later use
incu_coefs2$incu_coeff<-incu_coefs2$part_beh_t_sc
brood_coefs2$brood_coeff<-brood_coefs2$part_beh_t_sc

# and merged into a single dataset
slope_coeffs<-merge(incu_coefs2, brood_coefs2, by.x = "nest", by.y="nest", all =TRUE) # will delete pairs with no brooding data


## @knitr linear_mod

# linear model testing the relationship between pairs' coordination 
#strength in incubation and brooding

coeff_comp<-lm(brood_coeff ~ incu_coeff, data = slope_coeffs)
summary<-summary(coeff_comp)

## @knitr within_pair_info

#Model information extracted and prepped for later use
#coefficients, R squared information

coeff_g<-plot_model(coeff_comp, type = "pred", terms = c("incu_coeff") )
coefDat <- data.frame(coeff_g$data)
coefDat$incu_coeff<-coefDat$x
coefDat$brood_coeff<-coefDat$predicted

setwd("M:/Documents/R Practice/Kittiwakes/VHF data/Permutations/within pair")
coefDat<-as.data.frame(coefDat)
coeff<-as.data.frame(summary[["coefficients"]])
adrsq<-as.data.frame(summary[["r.squared"]])
rsq<-as.data.frame(summary[["adj.r.squared"]])
inDat<-as.data.frame(inDat)
brDat<-as.data.frame(brDat)
coefDat$perm_no<-i
coeff$perm_no<-i
adrsq$perm_no<-i
rsq$perm_no<-i
inDat$perm_no<-i
brDat$perm_no<-i
slope_coeffs$perm_no<-i

## @knitr linear_mod_info

# this repeat's model information saved for later use

setwd("M:/Documents/R Practice/Kittiwakes/VHF data/Permutations/within pair/model/rsq")
write.csv(adrsq, paste0(i, "_within_pair_adrsq.csv"))

setwd("M:/Documents/R Practice/Kittiwakes/VHF data/Permutations/within pair/model/results")
write.csv(coeff, paste0(i, "_within_pair_model_results.csv"))

write.csv(rsq, paste0(i, "_within_pair_rsq.csv"))
setwd("M:/Documents/R Practice/Kittiwakes/VHF data/Permutations/within pair/graph/pop slope/incu")

write.csv(inDat, paste0(i, "_within_pair_graph_data_in.csv"))

setwd("M:/Documents/R Practice/Kittiwakes/VHF data/Permutations/within pair/graph/within pair slope")

write.csv(coefDat, paste0(i, "_within_pair_graph_slope.csv"))

setwd("M:/Documents/R Practice/Kittiwakes/VHF data/Permutations/within pair/graph/pop slope/brood")

write.csv(brDat, paste0(i, "_within_pair_graph_data_br.csv"))

setwd("M:/Documents/R Practice/Kittiwakes/VHF data/Permutations/within pair/graph/pair slope")
write.csv(slope_coeffs, paste0(i, "_within_pair_brandincu_coeff.csv"))
}

####---- END OF LOOP ----#### 

#process will begin again and run 100 times so that the analyses 
#are repeated with 100 sets of different imputed data to ensure results are stable

####---- Part 2- Uploading 100 files for results----####

## @knitr upload_setup


library(plyr) # only needed once the 100 files are generated, 
#do not load before this point or loop of 100 repeats will not run

####----PCA results----####

## @knitr upload_pca

# upload information from the 100 versions of the PCA

setwd("M:/Documents/R Practice/Kittiwakes/VHF data/Permutations/PCA")

## @knitr create-path
# creates a path to the downloaded data directory
raw_data_path <- here::here("weights")

# create path to data
PCA_path <-fs::path("weights")

# create vector of paths to all individual files 
#in the directory
PCA_paths<-fs::dir_ls(PCA_path)

# output vector (list) to store the tables containing 
#the read in data
PCA_df_list <- vector("list", length(PCA_paths))

# creating index vector 
seq_along(PCA_paths)

## @knitr read-in-for-loop
for(i in seq_along(PCA_paths)){
  PCA_df_list[[i]] <- read.table(PCA_paths[[i]],
                                  header=TRUE, 
                                  sep =",")
}


## @knitr weights_pca_median
# calculating meidans for reporting
# create one file with all information on weightings from 100 repeats
PCA_weights<-do.call(what=rbind.fill, args = PCA_df_list)

PCA_weights$X<-as.factor(PCA_weights$X)
PCA_weights%>%dplyr::group_by(X)%>%dplyr::summarize(med=median(PC1))# median weightings from 100 PCA repeats

## @knitr upload_eigen

# repeat process but this time to determine the meidan eigenvalue from 100 repeats
#of the PCA
setwd("M:/Documents/R Practice/Kittiwakes/VHF data/Permutations/PCA")

## @knitr create-path
# creates a path to the downloaded data directory
raw_data_path <- here::here("eigen")

# create path to data
eigen_path <-fs::path("eigen")

# create vector of paths to all individual files 
#in the directory
eigen_paths<-fs::dir_ls(eigen_path)

# output vector (list) to store the tables containing 
#the read in data
eigen_df_list <- vector("list", length(eigen_paths))

# creating index vector 
seq_along(eigen_paths)

## @knitr read-in-for-loop
for(i in seq_along(eigen_paths)){
  eigen_df_list[[i]] <- read.table(eigen_paths[[i]],
                                 header=TRUE, 
                                 sep =",")
}


PCA_eigen<-do.call(what=rbind.fill, args = eigen_df_list)
PCA_eigen<-subset(PCA_eigen, X == "Dim.1")


PCA_eigen%>%dplyr::summarize(med=median(eigenvalue)) # median eigenvalue
PCA_eigen%>%dplyr::summarize(med=median(variance.percent)) # median % variance explained 


####--- R squared of global models results----####

## @knitr upload_r squared_incu

# to determine median r squared from global incubation model
setwd("M:/Documents/R Practice/Kittiwakes/VHF data/Permutations/global R squared")

## @knitr create-path
# creates a path to the downloaded data directory
raw_data_path2 <- here::here("incu")

# create path to data
inrs_path2 <-fs::path("incu")

# create vector of paths to all individual files 
#in the directory
inrs_paths2<-fs::dir_ls(inrs_path2)

# output vector (list) to store the tables containing 
#the read in data
inrs_df_list <- vector("list", length(inrs_paths2))

# creating index vector 
seq_along(inrs_paths2)

## @knitr read-in-for-loop
for(i in seq_along(inrs_paths2)){
  inrs_df_list[[i]] <- read.table(inrs_paths2[[i]],
                                   header=TRUE, 
                                   sep =",")
}

inrs<-do.call(what=rbind.fill, args = inrs_df_list)
inrs%>%dplyr::summarize(med=median(R2m)) # median marginal r squared incubation model
inrs%>%dplyr::summarize(med=median(R2c)) # median conditional r squared incubation model

## @knitr upload_r squared_brood

# to determine median r squared from global brooding model

setwd("M:/Documents/R Practice/Kittiwakes/VHF data/Permutations/global R squared")

## @knitr create-path
# creates a path to the downloaded data directory
raw_data_path2 <- here::here("brood")

# create path to data
brrs_path2 <-fs::path("brood")

# create vector of paths to all individual files 
#in the directory
brrs_paths2<-fs::dir_ls(brrs_path2)

# output vector (list) to store the tables containing 
#the read in data
brrs_df_list <- vector("list", length(brrs_paths2))

# creating index vector 
seq_along(brrs_paths2)

## @knitr read-in-for-loop
for(i in seq_along(brrs_paths2)){
  brrs_df_list[[i]] <- read.table(brrs_paths2[[i]],
                                  header=TRUE, 
                                  sep =",")
}

brrs<-do.call(what=rbind.fill, args = brrs_df_list)
brrs%>%dplyr::summarize(med=median(R2m)) # median marginal r squared brooding model
brrs%>%dplyr::summarize(med=median(R2c)) # median conditional r squared brooding model

## @knitr upload_rsq_cc

# Upload the R squared information from the global coordination comparison model

setwd("M:/Documents/R Practice/Kittiwakes/VHF data/Permutations/global R squared")

## @knitr create-path
# creates a path to the downloaded data directory
raw_data_path2 <- here::here("cc")

# create path to data
ccrs_path2 <-fs::path("cc")

# create vector of paths to all individual files 
#in the directory
ccrs_paths2<-fs::dir_ls(ccrs_path2)

# output vector (list) to store the tables containing 
#the read in data
ccrs_df_list <- vector("list", length(ccrs_paths2))

# creating index vector 
seq_along(ccrs_paths2)

## @knitr read-in-for-loop
for(i in seq_along(ccrs_paths2)){
  ccrs_df_list[[i]] <- read.table(ccrs_paths2[[i]],
                                  header=TRUE, 
                                  sep =",")
}

ccrs<-do.call(what=rbind.fill, args = ccrs_df_list)
ccrs%>%dplyr::summarize(med=median(R2m)) # median marginal r squared for coordination comparison model
ccrs%>%dplyr::summarize(med=median(R2c)) # median conditional r squared for coordination comparison model

###---- Inucbation model results----####


## @knitr upload_incu_results

# uploading and extracting median results from 100 repeats of incubation analysis
setwd("M:/Documents/R Practice/Kittiwakes/VHF data/Permutations/Original incubation")

## @knitr create-path
# creates a path to the downloaded data directory
raw_data_path <- here::here("nested")

# create path to data
kitt_path <-fs::path("nest")

# create vector of paths to all individual files 
#in the directory
kitt_paths<-fs::dir_ls(kitt_path)

# output vector (list) to store the tables containing 
#the read in data
incu_df_list <- vector("list", length(kitt_paths))

# creating index vector 
seq_along(kitt_paths)

## @knitr read-in-for-loop
for(i in seq_along(kitt_paths)){
  incu_df_list[[i]] <- read.table(kitt_paths[[i]],
                                  header=TRUE, 
                                  sep =",")
}

all_incu_df<-do.call(what=rbind.fill, args = incu_df_list)

## @knitr incu_retain

#establish how many modesl were retianed in each repeat

table(all_incu_df$X)# % time each numbered model retained

incu_no_mods<-as.data.frame(table(all_incu_df$perm_no)) 
table(incu_no_mods$Freq)#100% of repeats retained 2 models

## @knitr incu_med_cal

# organise variables for median calculation
incu_no_mods$Var1<-as.factor(incu_no_mods$Var1)
incu_no_mods$perm_no<-as.factor(incu_no_mods$Var1)
all_incu_df$perm_no<-as.factor(all_incu_df$perm_no)

all_incu_df2<-merge(all_incu_df,incu_no_mods, x.by = "perm_no", y.by = "perm_no")


all_incu_df3<-subset(all_incu_df2, Freq == 2) # code used in case other scenarios produced different number of models
#will just retain most common outcome

# to separate medians by model number
all_incu_df3$model_number<-all_incu_df3$X 
all_incu_df3$model_number<-as.factor(all_incu_df3$model_number)

# figures for each retained model
all_incu_df3%>%dplyr::group_by(model_number)%>%dplyr::summarize(med=median(X.Intercept.)) # median intercept
all_incu_df3%>%dplyr::group_by(model_number)%>%dplyr::summarize(med=median(part_beh_t_sc)) # median coefficient for 'PCA partner trip duration'
all_incu_df3%>%group_by(model_number)%>%dplyr::summarise(med=median(sex))# median coefficient for 'sex'
all_incu_df3%>%dplyr::group_by(model_number)%>%dplyr::summarize(med=median(year)) # median coefficient for 'year'
all_incu_df3%>%group_by(model_number)%>%dplyr::summarise(med=median(deviance))%>%dplyr::mutate_if(is.numeric, format, 2) #median deviance
all_incu_df3%>%dplyr::group_by(model_number)%>%dplyr::summarize(med=median(delta)) # median AIC delta
all_incu_df3%>%group_by(model_number)%>%dplyr::summarise(med=median(AICc))%>%dplyr::mutate_if(is.numeric, format, 2) #median AICc

## @knitr full_incu

# repeat process with full set of models
#to get model Akaike weights

setwd("M:/Documents/R Practice/Kittiwakes/VHF data/Permutations/Original incubation")

## @knitr create-path
# creates a path to the downloaded data directory
raw_data_path <- here::here("full")

# create path to data
kitt_path <-fs::path("full")

# create vector of paths to all individual files 
#in the directory
kitt_paths<-fs::dir_ls(kitt_path)

# output vector (list) to store the tables containing 
#the read in data
incu_df_list <- vector("list", length(kitt_paths))

# creating index vector 
seq_along(kitt_paths)

## @knitr read-in-for-loop
for(i in seq_along(kitt_paths)){
  incu_df_list[[i]] <- read.table(kitt_paths[[i]],
                                  header=TRUE, 
                                  sep =",")
}

all_incu_df<-do.call(what=rbind.fill, args = incu_df_list)


all_incu_df2<-merge(all_incu_df,incu_no_mods, x.by = "perm_no", y.by = "perm_no")

all_incu_df3<-subset(all_incu_df2, Freq == 2)
all_incu_df_11<-subset(all_incu_df3, X == 11) # numbers of retained models from above
median(all_incu_df_11$weight) # median akaike weight of model

all_incu_df_15<-subset(all_incu_df3, X == 15) # numbers of retained models from above
 median(all_incu_df_15$weight) # median akaike weight of model
 
 ####---- Brooding model results----####

 ## @knitr brood_results
 
 # process above in repeated again this time for the brooding results

setwd("M:/Documents/R Practice/Kittiwakes/VHF data/Permutations/Original brooding")

## @knitr create-path
# creates a path to the downloaded data directory
raw_data_path2 <- here::here("nest")

# create path to data
kitt_path2 <-fs::path("nest")

# create vector of paths to all individual files 
#in the directory
kitt_paths2<-fs::dir_ls(kitt_path2)

# output vector (list) to store the tables containing 
#the read in data
brood_df_list <- vector("list", length(kitt_paths2))

# creating index vector 
seq_along(kitt_paths2)

## @knitr read-in-for-loop
for(i in seq_along(kitt_paths2)){
  brood_df_list[[i]] <- read.table(kitt_paths2[[i]],
                                  header=TRUE, 
                                  sep =",")
}


## @knitr brooding_mod_no

# number of modeos retained following brooding analysis

all_brood_df<-do.call(what=rbind.fill, args = brood_df_list)
table(all_brood_df$X)# 95% time 2 models retained

brood_no_mods<-as.data.frame(table(all_brood_df$perm_no))  #95% of the repeats retained 2 models, 
#model 10 always retained, model 12 retained 95% of time
brood_no_mods$perm_no<-as.factor(brood_no_mods$Var1)
all_brood_df2<-merge(all_brood_df,brood_no_mods, x.by = "perm_no", y.by = "perm_no")

# data prepped for median calculation
all_brood_df3<-subset(all_brood_df2, Freq == 2) # check for extra models, remove 5% of repeats where only one model retained

all_brood_df3$model_number<-all_brood_df3$X
all_brood_df3$model_number<-as.factor(all_brood_df3$model_number)

## @knitr brooding_meds_cal

#medians for brooding analysis calculated

# separated by model number
all_brood_df3%>%dplyr::group_by(model_number)%>%dplyr::summarize(med=median(X.Intercept.)) #median intercept for brooding analysis
all_brood_df3%>%dplyr::group_by(model_number)%>%dplyr::summarize(med=median(part_beh_t_sc)) #median coefficient for 'PCA partner trip duration' for brooding analysis
all_brood_df3%>%group_by(model_number)%>%dplyr::summarise(med=median(hatch_sc)) #median coefficient for 'days since hatching' for brooding analysis
all_brood_df3%>%dplyr::group_by(model_number)%>%dplyr::summarize(med=median(year)) #median coefficient for 'year' for brooding analysis
all_brood_df3%>%group_by(model_number)%>%dplyr::summarise(med=median(deviance))%>%dplyr::mutate_if(is.numeric, format, 2) #median deviance for brooding analysis
all_brood_df3%>%dplyr::group_by(model_number)%>%dplyr::summarize(med=median(delta)) #median AICc delta for brooding analysis
all_brood_df3%>%group_by(model_number)%>%dplyr::summarise(med=median(AICc))%>%dplyr::mutate_if(is.numeric, format, 2) #median AICc for brooding analysis

## @knitr brooding_weights

## Akaike weights determined from full list of brooding models, as before

setwd("M:/Documents/R Practice/Kittiwakes/VHF data/Permutations/Original brooding")

## @knitr create-path
# creates a path to the downloaded data directory
raw_data_path2 <- here::here("full")

# create path to data
kitt_path2 <-fs::path("full")

# create vector of paths to all individual files 
#in the directory
kitt_paths2<-fs::dir_ls(kitt_path2)

# output vector (list) to store the tables containing 
#the read in data
brood_df_list <- vector("list", length(kitt_paths2))

# creating index vector 
seq_along(kitt_paths2)

## @knitr read-in-for-loop
for(i in seq_along(kitt_paths2)){
  brood_df_list[[i]] <- read.table(kitt_paths2[[i]],
                                   header=TRUE, 
                                   sep =",")
}

all_brood_df<-do.call(what=rbind.fill, args = brood_df_list)

all_brood_df2<-merge(all_brood_df,brood_no_mods, x.by = "perm_no", y.by = "perm_no")

all_brood_df3<-subset(all_brood_df2, Freq == 2)
all_brood_df_10<-subset(all_brood_df3, X == 10) 
median(all_brood_df_10$weight)# median Akaike weight of model 10 (model numbers determined above)

all_brood_df_12<-subset(all_brood_df3, X == 12)
median(all_brood_df_12$weight) #median Akaike weight of model 10 (model numbers determined above)

####---- Coordination comparison model results----####

## @knitr upload_cc

## Process is repeated again to determine median results for the coordination comparison model
#100 repeats uploaded

setwd("M:/Documents/R Practice/Kittiwakes/VHF data/Permutations/Cooridnation comparison")

## @knitr create-path
# creates a path to the downloaded data directory
raw_data_path3 <- here::here("nest")

# create path to data
kitt_path3 <-fs::path("nest")

# create vector of paths to all individual files 
#in the directory
kitt_paths3<-fs::dir_ls(kitt_path3)

# output vector (list) to store the tables containing 
#the read in data
cc_df_list <- vector("list", length(kitt_paths3))

# creating index vector 
seq_along(kitt_paths3)

## @knitr read-in-for-loop
for(i in seq_along(kitt_paths3)){
  cc_df_list[[i]] <- read.table(kitt_paths3[[i]],
                                   header=TRUE, 
                                   sep =",")}



all_cc_df<-do.call(what=rbind.fill, args = cc_df_list)

## @knitr cc_mod_no

# number of models retained determined
cc_no_mods<-as.data.frame(table(all_cc_df$perm_no))
table(cc_no_mods$Freq)# 1 model 97% of repeats, 2 models 2%of repeats, 3 models 1% of repeats

#prep data for median calculation
cc_no_mods$perm_no<-as.factor(cc_no_mods$Var1)
all_cc_df2<-merge(all_cc_df,cc_no_mods, x.by = "perm_no", y.by = "perm_no")

all_cc_df3<-subset(all_cc_df2, Freq == 1) # check for extra models, keep only repeats where 1 model retained (most common outcome)

## @knitr cc_results_med

# only one model so no need to separate by model number unlike previously
median(all_cc_df3$X.Intercept.) # median intercept for cc analysis
median(all_cc_df3$breed_stage) # median coefficient for 'breeding stage' for cc analysis
median(all_cc_df3$part_beh_t_sc) # median coefficient for 'PCA partner trip duration' for cc analysis
median(all_cc_df3$year) # median coefficient for 'year' for cc analysis
median(all_cc_df3$breed_stage_part_beh) # NOTE NOT RETAINED- NO INTERACTION 
median(all_cc_df3$AICc) # median AICc for cc analysis
median(all_cc_df3$delta) # median Delta AIC for cc analysis
median(all_cc_df3$deviance) # median deviance for cc analysis

## @knitr cc_full_mods

#upload full cc models for Akaike weights
setwd("M:/Documents/R Practice/Kittiwakes/VHF data/Permutations/Cooridnation comparison")

## @knitr create-path
# creates a path to the downloaded data directory
raw_data_path3 <- here::here("full")

# create path to data
kitt_path3 <-fs::path("full")

# create vector of paths to all individual files 
#in the directory
kitt_paths3<-fs::dir_ls(kitt_path3)

# output vector (list) to store the tables containing 
#the read in data
cc_df_list <- vector("list", length(kitt_paths3))

# creating index vector 
seq_along(kitt_paths3)

## @knitr read-in-for-loop
for(i in seq_along(kitt_paths3)){
  cc_df_list[[i]] <- read.table(kitt_paths3[[i]],
                                header=TRUE, 
                                sep =",")}

#calculate median Akaike weight for single most commonly retained model
all_cc_df<-do.call(what=rbind, args = cc_df_list)
all_cc_df2<-merge(all_cc_df,cc_no_mods, x.by = "perm_no", y.by = "perm_no")
all_cc_df2<-subset(all_cc_df2, Freq == 1)
all_cc_df2<-subset(all_cc_df2, X == 12) # model 12 target model
median(all_cc_df2$weight)# median Akaike weight

####----Building Figure 1----####

## @knitr figure1_upload

#this code creates the two graphs which are combined to make figure 1
# upload the model information saves during part 1, first for incubation
# start with individual pair slopes of coordination strength 

#MUST BE RUN FOR INCUBATION FIRST AS FILES CREATED THAT THE BROODING GRAPH ALSO USES

setwd("M:/Documents/R Practice/Kittiwakes/VHF data/Permutations/within pair/graph")

## @knitr create-path
# creates a path to the downloaded data directory
raw_data_path4 <- here::here("pair slope")

# create path to data
kitt_path4 <-fs::path("pair slope")

# create vector of paths to all individual files 
#in the directory
kitt_paths4<-fs::dir_ls(kitt_path4)

# output vector (list) to store the tables containing 
#the read in data
cc_df_list <- vector("list", length(kitt_paths4))

# creating index vector 
seq_along(kitt_paths4)

## @knitr read-in-for-loop
for(i in seq_along(kitt_paths4)){
  cc_df_list[[i]] <- read.table(kitt_paths4[[i]],
                                header=TRUE, 
                                sep =",")}


all_pair_slopes<-do.call(what=rbind, args = cc_df_list)


## @knitr incu_slope_med

# calculate the medians of these slope coeeficient across 100 repeats
incu_int<-as.data.frame(all_pair_slopes%>%dplyr::group_by(nest)%>%dplyr::summarise(`(Intercept)`=median(X.Intercept..x)))
incu_coeff<-as.data.frame(all_pair_slopes%>%dplyr::group_by(nest)%>%dplyr::summarise(part_beh_t_sc=median(incu_coeff)))
brood_int<-as.data.frame(all_pair_slopes%>%dplyr::group_by(nest)%>%dplyr::summarise(`(Intercept)`=median(X.Intercept..y)))
brood_coeff<-as.data.frame(all_pair_slopes%>%dplyr::group_by(nest)%>%dplyr::summarise(part_beh_t_sc=median(brood_coeff)))

#and create new dataframes
pair_slopes_incu<-merge(incu_int, incu_coeff, by.x = "nest", by.y = "nest", all = TRUE) 
pair_slopes_brood<-merge(brood_int, brood_coeff, by.x = "nest", by.y = "nest", all = TRUE)  


## @knitr pop_incu_slope

# then upload overall coeffcient slope for that breeding stage
setwd("M:/Documents/R Practice/Kittiwakes/VHF data/Permutations/within pair/graph/pop slope")

## @knitr create-path
# creates a path to the downloaded data directory
raw_data_path4 <- here::here("incu")

# create path to data
kitt_path4 <-fs::path("incu")

# create vector of paths to all individual files 
#in the directory
kitt_paths4<-fs::dir_ls(kitt_path4)

# output vector (list) to store the tables containing 
#the read in data
pop_slope_list <- vector("list", length(kitt_paths4))

# creating index vector 
seq_along(kitt_paths4)

## @knitr read-in-for-loop
for(i in seq_along(kitt_paths4)){
  pop_slope_list[[i]] <- read.table(kitt_paths4[[i]],
                                header=TRUE, 
                                sep =",")}


pop_slopes<-do.call(what=rbind, args = pop_slope_list)                            
                     
## @knitr incu_med_pop_slope

# calculate the median slopes, confidence intervals for incubation
incu_DV<-as.data.frame(pop_slopes%>%dplyr::group_by(X)%>%dplyr::summarise(dur_hrs_t_sc=median(dur_hrs_t_sc)))
incu_conf.low<-as.data.frame(pop_slopes%>%dplyr::group_by(X)%>%dplyr::summarise(conf.low=median(conf.low)))
incu_conf.high<-as.data.frame(pop_slopes%>%dplyr::group_by(X)%>%dplyr::summarise(conf.high=median(conf.high)))
incu_IV<-as.data.frame(pop_slopes%>%dplyr::group_by(X)%>%dplyr::summarise(part_beh_t_sc=median(part_beh_t_sc)))

#merge files
incu_pop_slope<-merge(incu_DV, incu_IV, by.x = "X", by.y = "X", all = TRUE) 
incu_pop_slope<-merge(incu_pop_slope, incu_conf.low, by.x = "X", by.y = "X", all = TRUE)  
incu_pop_slope<-merge(incu_pop_slope, incu_conf.high, by.x = "X", by.y = "X", all = TRUE)                  
                             
## @knitr incu_main_data

#upload main data for graph
setwd("M:/Documents/R Practice/Kittiwakes/VHF data/Permutations/main data")

## @knitr create-path
# creates a path to the downloaded data directory
raw_data_path6 <- here::here("incu")

# create path to data
kitt_path6 <-fs::path("incu")

# create vector of paths to all individual files 
#in the directory
kitt_paths6<-fs::dir_ls(kitt_path6)

# output vector (list) to store the tables containing 
#the read in data
main_data_incu_list <- vector("list", length(kitt_paths6))

# creating index vector 
seq_along(kitt_paths6)

## @knitr read-in-for-loop
for(i in seq_along(kitt_paths6)){
  main_data_incu_list[[i]] <- read.table(kitt_paths6[[i]],
                                header=TRUE, 
                                sep =",")}


## @knitr prep_incu_graph

#prep and merge data for graph
main_data_incu<-do.call(what=rbind, args = main_data_incu_list)
trip_part_meds<-as.data.frame(main_data_incu%>%dplyr::group_by(trip_ID)%>%dplyr::summarise(part_beh_t_sc=median(part_beh_t_sc)))
trip_hours<-as.data.frame(main_data_incu%>%dplyr::group_by(trip_ID)%>%dplyr::summarise(dur_hrs_t_sc=median(dur_hrs_t_sc)))# will take only value as no medium needed

incu_graph_data<-merge(trip_hours, trip_part_meds, by.x = "trip_ID", by.y ="trip_ID")
incu_graph_data$nest<-sapply(strsplit(incu_graph_data$trip_ID, split='_', fixed=TRUE), '[', 2)


## @knitr incu_graph_build

#build coordination graph for incubation breeding stage
incu_pairs_graph<-incu_graph_data%>% ggplot(aes( x = part_beh_t_sc, y = dur_hrs_t_sc))+
  geom_point(data= incu_graph_data, size = 2, show.legend = FALSE,)+
  scale_color_manual(values=c("grey50", "black"))+
  geom_ribbon (data = incu_pop_slope, fill= "grey", alpha=0.3,aes(ymin=conf.low,ymax=conf.high))+
  geom_abline(data = pair_slopes_incu, aes(intercept=`(Intercept)`, slope = part_beh_t_sc))+
  geom_line(data = incu_pop_slope, aes(x = part_beh_t_sc, y = dur_hrs_t_sc),  colour = "red", linewidth = 1)+
  #facet_wrap(.~nest)+ can be used to view each nest 
  xlab("Transformed and scaled PCA partner trip duration")+
  ylab("Transformed and scaled focal bird's foraging trip duration")+
  theme_bw()+
  theme(axis.text = element_text(size = 15))+
  theme(axis.title= element_text(size=18))+
  scale_x_continuous(expand = c(0, 0))

incu_pairs_graph

## @knitr upload_brood_main

#upload main brooding data

setwd("M:/Documents/R Practice/Kittiwakes/VHF data/Permutations/main data")

## @knitr create-path
# creates a path to the downloaded data directory
raw_data_path6 <- here::here("brood")

# create path to data
kitt_path6 <-fs::path("brood")

# create vector of paths to all individual files 
#in the directory
kitt_paths6<-fs::dir_ls(kitt_path6)

# output vector (list) to store the tables containing 
#the read in data
main_data_brood_list <- vector("list", length(kitt_paths6))

# creating index vector 
seq_along(kitt_paths6)

## @knitr read-in-for-loop
for(i in seq_along(kitt_paths6)){
  main_data_brood_list[[i]] <- read.table(kitt_paths6[[i]],
                                         header=TRUE, 
                                         sep =",")}

## @knitr prep_brood_graph

# prep for building graph
main_data_brood<-do.call(what=rbind, args = main_data_brood_list)
trip_part_meds_brood<-as.data.frame(main_data_brood%>%dplyr::group_by(trip_ID)%>%dplyr::summarise(part_beh_t_sc=median(part_beh_t_sc)))
trip_hours_brood<-as.data.frame(main_data_brood%>%dplyr::group_by(trip_ID)%>%dplyr::summarise(dur_hrs_t_sc=median(dur_hrs_t_sc)))# will take only value as no medium needed

brood_graph_data<-merge(trip_hours_brood, trip_part_meds_brood, by.x = "trip_ID", by.y ="trip_ID")


## @knitr upload_brood_popslope

#upload data to calculate median coordination slope for breeding stage
setwd("M:/Documents/R Practice/Kittiwakes/VHF data/Permutations/within pair/graph/pop slope")

## @knitr create-path
# creates a path to the downloaded data directory
raw_data_path4 <- here::here("brood")

# create path to data
kitt_path4 <-fs::path("brood")

# create vector of paths to all individual files 
#in the directory
kitt_paths4<-fs::dir_ls(kitt_path4)

# output vector (list) to store the tables containing 
#the read in data
brood_pop_slope_list <- vector("list", length(kitt_paths4))

# creating index vector 
seq_along(kitt_paths4)

## @knitr read-in-for-loop
for(i in seq_along(kitt_paths4)){
  brood_pop_slope_list[[i]] <- read.table(kitt_paths4[[i]],
                                    header=TRUE, 
                                    sep =",")}


brood_pop_slopes<-do.call(what=rbind, args = brood_pop_slope_list)                            

## @knitr brood_slope_med

#calculate medians for slopes and confidence intervals

brood_DV<-as.data.frame(brood_pop_slopes%>%dplyr::group_by(X)%>%dplyr::summarise(dur_hrs_t_sc=median(dur_hrs_t_sc)))
brood_conf.low<-as.data.frame(brood_pop_slopes%>%dplyr::group_by(X)%>%dplyr::summarise(conf.low=median(conf.low)))
brood_conf.high<-as.data.frame(brood_pop_slopes%>%dplyr::group_by(X)%>%dplyr::summarise(conf.high=median(conf.high)))
brood_IV<-as.data.frame(brood_pop_slopes%>%dplyr::group_by(X)%>%dplyr::summarise(part_beh_t_sc=median(part_beh_t_sc)))

#merge and prepfor graph
brood_pop_slope<-merge(brood_DV, brood_IV, by.x = "X", by.y = "X", all = TRUE) 
brood_pop_slope<-merge(brood_pop_slope, brood_conf.low, by.x = "X", by.y = "X", all = TRUE)  
brood_pop_slope<-merge(brood_pop_slope, brood_conf.high, by.x = "X", by.y = "X", all = TRUE) 

#label nests
brood_graph_data$nest<-sapply(strsplit(brood_graph_data$trip_ID, split='_', fixed=TRUE), '[', 2)

## @knitr build_brood_graph

# Build brooding coordination graph 

brood_pairs_graph<-brood_graph_data%>% ggplot(aes( x = part_beh_t_sc, y = dur_hrs_t_sc))+
  geom_point(data= brood_graph_data, size = 2, show.legend = FALSE,)+
  #facet_wrap(.~nest)+ # can be used to veiw each nest 
  scale_color_manual(values=c("grey50", "black"))+
  geom_ribbon (data = brood_pop_slope, fill= "grey", alpha=0.3,aes(ymin=conf.low,ymax=conf.high))+
  geom_abline(data = pair_slopes_brood, aes(intercept=`(Intercept)`, slope = part_beh_t_sc))+
  geom_line(data = brood_pop_slope, aes(x = part_beh_t_sc, y = dur_hrs_t_sc),  colour = "red", linewidth = 1)+
  facet_wrap(.~nest)+
  xlab("Transformed and scaled PCA partner trip duration")+
  ylab("Transformed and scaled focal bird's foraging trip duration")+
  theme_bw()+
  theme(axis.text = element_text(size = 15))+
  theme(axis.title= element_text(size=18))+
  scale_x_continuous(expand = c(0, 0))

brood_pairs_graph

# graphs then combined outside R to complete Figure 1

####---- Importance of random slope results----####


# results of the liklihood ratio test comparing model fit with and without random slopes
## @knitr upload_random_slope_results_incu

#Incubation comparison
setwd("M:/Documents/R Practice/Kittiwakes/VHF data/Permutations/slope comp")

## @knitr create-path
# creates a path to the downloaded data directory
raw_data_path7 <- here::here("incu")

# create path to data
kitt_path7 <-fs::path("incu")

# create vector of paths to all individual files 
#in the directory
kitt_paths7<-fs::dir_ls(kitt_path7)

# output vector (list) to store the tables containing 
#the read in data
slope_comp_list <- vector("list", length(kitt_paths7))

# creating index vector 
seq_along(kitt_paths7)

## @knitr read-in-for-loop
for(i in seq_along(kitt_paths7)){
  slope_comp_list[[i]] <- read.table(kitt_paths7[[i]],
                                     header=TRUE, 
                                     sep =",")}


slope_comp_df<-do.call(what=rbind.fill, args = slope_comp_list) 

slope_comp_df%>%dplyr::group_by(X)%>%dplyr::summarise(med=median(Pr..Chisq.)) ### provides P value from likelihood ratio test
#neither model significantly better

## @knitr brood_slope_comp

#repeat for brooding models

setwd("M:/Documents/R Practice/Kittiwakes/VHF data/Permutations/slope comp")

## @knitr create-path
# creates a path to the downloaded data directory
raw_data_path7 <- here::here("brood")

# create path to data
kitt_path7 <-fs::path("brood")

# create vector of paths to all individual files 
#in the directory
kitt_paths7<-fs::dir_ls(kitt_path7)

# output vector (list) to store the tables containing 
#the read in data
slope_comp_list <- vector("list", length(kitt_paths7))

# creating index vector 
seq_along(kitt_paths7)

## @knitr read-in-for-loop
for(i in seq_along(kitt_paths7)){
  slope_comp_list[[i]] <- read.table(kitt_paths7[[i]],
                                          header=TRUE, 
                                          sep =",")}


slope_comp_df<-do.call(what=rbind.fill, args = slope_comp_list) 

slope_comp_df%>%dplyr::group_by(X)%>%dplyr::summarise(med=median(Pr..Chisq.)) ### provides P value from likelihood ratio test
#neither model significantly better

####---- Within pair coordination strength results----####

## @knitr r_sq_within

# Results of within-pair incu vs brooding coordination strength comparison

setwd("M:/Documents/R Practice/Kittiwakes/VHF data/Permutations/within pair/model")

## @knitr create-path
# creates a path to the downloaded data directory
raw_data_path8 <- here::here("rsq")

# create path to data
kitt_path8 <-fs::path("rsq")

# create vector of paths to all individual files 
#in the directory
kitt_paths8<-fs::dir_ls(kitt_path8)

# output vector (list) to store the tables containing 
#the read in data
lm_list <- vector("list", length(kitt_paths8))

# creating index vector 
seq_along(kitt_paths8)

## @knitr read-in-for-loop
for(i in seq_along(kitt_paths8)){
  lm_list[[i]] <- read.table(kitt_paths8[[i]],
                                     header=TRUE, 
                                     sep =",")}


lm_df<-do.call(what=rbind.fill, args = lm_list) 

## @knitr model_results_lm

# for linear model comparing incubation and brooding coordination strength within pairs....
lm_df%>%dplyr::group_by(X)%>%dplyr::summarise(med=median(Pr...t..))# median p value across 100 repeats
lm_df%>%dplyr::group_by(X)%>%dplyr::summarise(med=median(t.value)) # median t value across 100 repeats
lm_df%>%dplyr::group_by(X)%>%dplyr::summarise(med=median(summary...r.squared...)) # median r squared across 100 repeats
lm_df_sig<-subset(lm_df, Pr...t.. <0.05) # check none are significant


                             ####----End of Script----####
