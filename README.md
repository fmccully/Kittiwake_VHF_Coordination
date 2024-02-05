# Kittiwake_VHF_Coordination
Data and Code for study on kittiwake coordinated parental care

GENERAL INFORMATION: total_df_orig.csv

Dataset description: Details on 697 foraging trips made by breeding black-legged kittiwakes (Rissa tridactyla) across 2 years. Trip durations were gathered from very high frequency (VHF) loggers monitoring 19 pairs. Ten pairs had both members tagged, nine pairs had only a single member tagged and their partner's behaviour was estimated from the tagged birds behaviour.  

Current use: To test for coordination (the matching of foraging trip durations) within pairs.

Geographic location of data collection: Svalbard, 78° 41' 53.1780'' N and 15° 43' 25.3812'' E.

Information about funding sources that supported the collection of the data: Supported by programmes MOSJ (https://mosj.no/) and SEAPOP (https://seapop.no/). We are grateful for funding provided by NERC (grant numbers NE/S00713X/1 for FMcC and FMck and NE/L002450/1 for SH) and the Research Council of Norway’s Arctic Field Grant (310627, Research in Svalbard (RiS) ID: 11366 for FMcC, and 333121, RiS ID: 11820 for FMck).

DATA & FILE OVERVIEW

File List: The original data file (total_df_orig.csv)

Zip file containt the results generated from the 3 day rolling means (Permutations 3 day.zip) 

Zip file containing the results generated from the 7 day rolling means (Permutations 7 day.zip) 

The r script for the cleaning and analysis of the data (VHF_kittiwakes_RMarkdown.R)

METHODOLOGICAL INFORMATION

Trip durations: 19 pairs of kittiwakes were tagged with VHF loggers across 2 years (2021 and 2022). The loggers emitted a coded signal which was detected by a receiver at the colony. This allowed us to determine the birds' presence/absence and record trip durations.

Methods for processing the data: 
Trip durations: If a bird’s VHF signal was detected within a five-minute period, the bird was assumed to be within detection range of the receiver for the entirety of that five-minute period. The term ‘nest shift’ was used to describe the period of time the focal bird was present at the colony (and therefore assumed to be attending the nest).‘Focal bird trip duration’ was defined as the length of a bird’s partner’s nest shift and was used as the response variable in this study.

Instrument- or software-specific information needed to interpret the data: Data processed in R, packages used: 
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

DATA-SPECIFIC INFORMATION FOR: total_df_orig.csv

Number of variables: 20

Number of cases/rows: 697

Variable List: 
x: row number

ring: focal bird unique ID number

p_ring: unique ID number of focal bird’s partner

year: 2021 or 2022

nest: nest number

nest_index: trip ID. nest number_ trip number for that nest

biurd_id: nest number_ bird one or two

sex: sex of focal bird (M/F)

num_start: numerical start time of trip duration

num_end: numerical end time of trip duration

start_time: start time and date of trip duration

end-time: end time and date of trip duration

dur_seconds: duration of trip  in seconds

dur_mins: duration of trip in minutes

dur_hr: duration trip in hours

breed_stage: whether trip took place in incuabtion or brooding

e_hatch_date: estimated hatching date for that nest

l_status: loggered or non logger bird (logger or partner)

startdate_new: date of start of trip duration (date only)

no_days: number of days since June 1st

DATA-SPECIFIC INFORMATION FOR: Permutations 3 day.zip and Permutations 7 day.zip

These files contain the results of the analyses run on the 3 day rolling means and the seven day rolling means respectively. the list outlining which results are in which files are outlined below. This layout is identical between the two zip files but the results they contain will differ.

Coordination comparison: results from the 100 iterations of the coordination comparison model, full = all possible models, nest= models according to model selection criteria

global R squared: R squared for the global brooding (brood), coordination comparison (cc) and incubation (incu) models

main data: the files produced by the 100 iterations of the data cleaning process for the brooding (brood) and incubation (incu) models

original brooding: results from the 100 iterations of the brooding model, full = all possible models, nest= models according to model selection criteria

original incubation: results from the 100 iterations of the incubation model, full = all possible models, nest= models according to model selection criteria

PCA: results from the 100 PCAs including eigenvalues (eigen) and weightings (weights)

slope comp: results from test comparing importance of pair slopes in the brooding (brood) and incubation (incu) models

within pair: data for testing within pair changes in coordination strength. Used for visualising data
graph > pair slope> individual pairs slopes
graph> pop slope> overall strength of coordination in brooding (brood) and incubation (incu)
within pair slope: predicted slopes for graph

model>results: results comparing within pair partners in coordination between breeding stages
model>rsq: adjusted r squared for these models

