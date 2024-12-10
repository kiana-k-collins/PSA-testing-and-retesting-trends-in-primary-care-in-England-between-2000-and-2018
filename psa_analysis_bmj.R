library(data.table)
library(popEpi)
library(dplyr)
library(ggplot2)
library(tidyr)
library(dplyr)
library(epitools)

# samp_all_men is has individual line for each patid with a yob, gender, enter and exit date, region, imd, family history, ethnicity, prostate cancer diagnosis
#this table is retrieved from SQL and based on CPRD data and is subject to a full licence agreement, which does not permit data sharing outside of the research team
#calculate person years from population data 
# data is loaded - convert to data.table
setDT(samp_all_men)

# Convert date fields from character to Date type (replace with actual date format if different)
samp_all_men[, kc_start_date2 := as.Date(kc_start_date2, "%Y-%m-%d")]
samp_all_men[, kc_end_date2 := as.Date(kc_end_date2, "%Y-%m-%d")]

# Calculate follow-up time in years
samp_all_men[, follow_up_years := as.numeric(kc_end_date2 - kc_start_date2)/365.25]

samp_all_men[, birthday := as.IDate(paste0(yob, '-07-01'))]

#change names 
setnames(samp_all_men, old = c("birthday", "kc_start_date2", "kc_end_date2"), 
         new = c("bi_date", "dg_date", "ex_date"))

### Adding a dummy 'status' column with all zeros
samp_all_men$status <- 0  

#PSA testing data is also pulled from SQL. this has multiple patids if patients have multiple psa tests, psa test date, and psa value and whether or not the patient had a symptom in the 90 days before the test(1/0) for each symptom. 
# all data management was done seperately on SQL 
#calculate number of psa tests from psa data 
# Ensure samp_psa_tests is a data table
setDT(samp_psa_tests)

# Convert psa_date to year and count tests
samp_psa_tests[, psa_year := year(as.Date(psa_date, "%Y-%m-%d"))]

##GET BASELINE CHARACTERISTICS
#baseline patients with at least one psa test 
one_psa_pats <- unique(samp_psa_tests[, .(patid)]) #1521116
one_psa_cohort <- samp_all_men[patid %in% one_psa_pats$patid]
#group_4_cohort is the sub analysis cohort 
group_4_cohort <- samp_all_men[patid %in% group_4$patid]
two_psa_pats <- unique(NInt_samp_psa_tests_clean[, .(patid)])

two_psa_cohort <- samp_all_men[patid %in% two_psa_pats$patid]                       

#total number of people 
samp_all_men %>% summarise(count = n_distinct(patid))
one_psa_cohort %>% summarise(count = n_distinct(patid))
two_psa_cohort %>% summarise(count = n_distinct(patid))

#total number of practices 
samp_all_men %>% summarise(count = n_distinct(pracid))
one_psa_cohort %>% summarise(count = n_distinct(pracid))
two_psa_cohort %>% summarise(count = n_distinct(pracid))
group_4_cohort %>% summarise(count = n_distinct(pracid))

#total number of follow up years 
samp_all_men %>% summarise(total_follow_up_years = sum(follow_up_years, na.rm = TRUE))
one_psa_cohort %>% summarise(total_follow_up_years = sum(follow_up_years, na.rm = TRUE))
two_psa_cohort %>% summarise(total_follow_up_years = sum(follow_up_years, na.rm = TRUE))
group_4_cohort %>% summarise(total_follow_up_years = sum(follow_up_years, na.rm = TRUE))

#from here on substitute group_4_cohort to one_psa_cohort and the two_psa_cohort to get all baseline

#median followup time and IQR 
samp_all_men %>% summarise(
  median_follow_up_years = median(follow_up_years, na.rm = TRUE),
  Q1 = quantile(follow_up_years, 0.25, na.rm = TRUE),
  Q3 = quantile(follow_up_years, 0.75, na.rm = TRUE)
)

two_psa_cohort %>% summarise(
  median_follow_up_years = median(follow_up_years, na.rm = TRUE),
  Q1 = quantile(follow_up_years, 0.25, na.rm = TRUE),
  Q3 = quantile(follow_up_years, 0.75, na.rm = TRUE)
)

#max follow up 
samp_all_men %>% summarise(max_follow_up_years = max(follow_up_years, na.rm = TRUE))
one_psa_cohort %>% summarise(max_follow_up_years = max(follow_up_years, na.rm = TRUE))

#number of patients with at least 1 psa test 
samp_psa_tests %>% summarise(count = n_distinct(patid))

#add in prostate_cacner to samp_psa_tests 
samp_psa_tests <- merge(samp_psa_tests, samp_all_men[, .(patid, prostate_cancer)], by = "patid", all.x = TRUE)

#find distinct patients in samp_psa_tests which have prostate cancer 
samp_psa_tests[, uniqueN(patid[prostate_cancer == "Yes"])]
two_psa_cohort[, uniqueN(patid[prostate_cancer == "Yes"])]

#FH 
samp_all_men[, uniqueN(patid[fh == "yes"])]

#add in family history to samp_psa_tests 
samp_psa_tests <- merge(samp_psa_tests, samp_all_men[, .(patid, fh)], by = "patid", all.x = TRUE)

samp_psa_tests[, uniqueN(patid[fh == "yes"])]

#baseline ethnicity
baseline_eth <- samp_all_men %>%
  group_by(eth1) %>%
  summarise(count = n()) %>%
  mutate(percentage = (count / sum(count)) * 100)

baseline_eth3 <- two_psa_cohort %>%
  group_by(eth1) %>%
  summarise(count = n()) %>%
  mutate(percentage = (count / sum(count)) * 100)

#baseline region
baseline_region <- samp_all_men %>%
  group_by(region_label) %>%
  summarise(count = n()) %>%
  mutate(percentage = (count / sum(count)) * 100)

baseline_region3 <- two_psa_cohort %>%
  group_by(region_label) %>%
  summarise(count = n()) %>%
  mutate(percentage = (count / sum(count)) * 100)

#baseline imd creating IMD into quintiles it was originally in 20 levels. 
samp_all_men <- samp_all_men %>%
  mutate(imd = case_when(
    imd %in% 1:4   ~ 1,
    imd %in% 5:8   ~ 2,
    imd %in% 9:12  ~ 3,
    imd %in% 13:16 ~ 4,
    imd %in% 17:20 ~ 5,
    TRUE ~ as.numeric(NA) # Handles cases outside 1-20, if any
  ))

baseline_imd <- samp_all_men %>%
  group_by(imd) %>%
  summarise(count = n()) %>%
  mutate(percentage = (count / sum(count)) * 100)

baseline_imd3 <- two_psa_cohort %>%
  group_by(imd) %>%
  summarise(count = n()) %>%
  mutate(percentage = (count / sum(count)) * 100)

#baseline age at entry
baseline_age <- samp_all_men %>%
  mutate(age = year(dg_date) - yob,  # Calculate age based on a reference year (year entered the cohort) and year of birth
         # Define age categories based on calculated age
         age_category = case_when(
           age < 18 ~ "<18",
           age >= 18 & age <= 29 ~ "18-29",
           age >= 30 & age <= 39 ~ "30-39",
           age >= 40 & age <= 49 ~ "40-49",
           age >= 50 & age <= 59 ~ "50-59",
           age >= 60 & age <= 69 ~ "60-69",
           age >= 70 & age <= 79 ~ "70-79",
           age >= 80 & age <= 89 ~ "80-89",
           age >= 90 ~ "90+"
         )) %>%
  group_by(age_category) %>%
  summarise(num_people = n_distinct(patid)) %>%
  ungroup() %>%  
  mutate(percentage = (num_people / sum(num_people)) * 100) %>%
  arrange(age_category)

baseline_age3 <- two_psa_cohort %>%
  mutate(age = year(dg_date) - yob,  
         # Define age categories based on calculated age
         age_category = case_when(
           age < 18 ~ "<18",
           age >= 18 & age <= 29 ~ "18-29",
           age >= 30 & age <= 39 ~ "30-39",
           age >= 40 & age <= 49 ~ "40-49",
           age >= 50 & age <= 59 ~ "50-59",
           age >= 60 & age <= 69 ~ "60-69",
           age >= 70 & age <= 79 ~ "70-79",
           age >= 80 & age <= 89 ~ "80-89",
           age >= 90 ~ "90+"
         )) %>%
  group_by(age_category) %>%
  summarise(num_people = n_distinct(patid)) %>%
  ungroup() %>%  
  mutate(percentage = (num_people / sum(num_people)) * 100) %>%
  arrange(age_category)

#make age range for age at psa test in samp_psa_test 
samp_psa_tests[, age_category := fcase(
  age_psa1 < 18, "<18",
  age_psa1 >= 18 & age_psa1 <= 29, "18-29",
  age_psa1 >= 30 & age_psa1 <= 39, "30-39",
  age_psa1 >= 40 & age_psa1 <= 49, "40-49",
  age_psa1 >= 50 & age_psa1 <= 59, "50-59",
  age_psa1 >= 60 & age_psa1 <= 69, "60-69",
  age_psa1 >= 70 & age_psa1 <= 79, "70-79",
  age_psa1 >= 80 & age_psa1 <= 89, "80-89",
  age_psa1 >= 90, "90+"
)]

#number of psa tests by age 
samp_psa_tests[, .N, by = age_category]

rm(baseline_age, baseline_eth, baseline_imd, baseline_region)

##PSA TESTING RATES OVERTIME OVERALL ###################
##########################################################
##MAKE POPULATION AGE RANGES 
#population
expand_age <- lexpand(samp_all_men, birth = bi_date, entry = dg_date, exit = ex_date,
                      per = seq(2000,2019,1), age = c(0:100,Inf),
                      aggre = list(year = per, age = age))

#make age ranges 
expand_age <- expand_age %>%
  mutate(age_range = case_when(
    age < 18 ~ "<18",
    age >= 18 & age <= 29 ~ "18-29",
    age >= 30 & age <= 39 ~ "30-39",
    age >= 40 & age <= 49 ~ "40-49",
    age >= 50 & age <= 59 ~ "50-59",
    age >= 60 & age <= 69 ~ "60-69",
    age >= 70 & age <= 79 ~ "70-79",
    age >= 80 & age <= 89 ~ "80-89",
    age >= 90 ~ "90+"
  ))

expand_age <- expand_age[, .(TotalPyrs = sum(pyrs)), by = .(year, age_range)]

expand_age <- expand_age[age_range != "<18"]


##MAKE PSA TESTING DATA 
samp_psa_tests <- merge(samp_psa_tests, samp_all_men[, .(patid)], by = "patid", all.x = TRUE)

dt_final2 <- samp_psa_tests[, .N, by = .(age_category, psa_year)]

names(dt_final2) <- c("age_range", "year", "Count")

all <- merge(expand_age, dt_final2, by = c("age_range", "year"), all.x = TRUE)

# Replace NA in Count with 0
all[is.na(all$Count), "Count"] <- 0

##get standard population 
teststdpop_2018 <- testexpand_age[year == 2018, .(age_range, stdpop = TotalPyrs)]

# Merge stdpop back to the original table, matching on region and age_range
all <- merge(all, stdpop_2018, by = c("age_range"), all.x = TRUE)

group_by<- c(all$year)

age_adjusted_rate_overall <- all %>%
  group_by(year) %>%
  summarise(age_adjust = list(ageadjust.direct(count = Count,   #count of events
                                               pop = TotalPyrs,          #person years of DFpop
                                               rate = NULL,                
                                               stdpop = stdpop,            
                                               conf.level = 0.95))) %>%
  mutate(age_adjust = map(age_adjust, as.data.frame.list)) %>%
  unnest(cols = c(age_adjust))

##make rates per 1000 person years 
age_adjusted_rate_overall <- age_adjusted_rate_overall %>%
  mutate(
    crude.rate = crude.rate * 1000,
    adj.rate = adj.rate * 1000,
    lci = lci * 1000,
    uci = uci * 1000
  )

ggplot(age_adjusted_rate_overall, aes(x = year)) +
  geom_line(aes(y = crude.rate, color = "Crude Rate")) +
  geom_line(aes(y = adj.rate, color = "Adjusted Rate")) +
  labs(title = "Crude Rate vs Adjusted Rate over Years", x = "Year", y = "Rate") +
  scale_color_manual(values = c("Crude Rate" = "blue", "Adjusted Rate" = "red")) +
  theme_minimal()

#above is by year and this is overall 
overall <- all %>%
  summarise(age_adjust = list(ageadjust.direct(count = Count,   #count of events
                                               pop = TotalPyrs,         
                                               rate = NULL,                
                                               stdpop = stdpop,         
                                               conf.level = 0.95))) %>%
  mutate(age_adjust = map(age_adjust, as.data.frame.list)) %>%
  unnest(cols = c(age_adjust))

overall <- overall %>%
  mutate(
    crude.rate = crude.rate * 1000,
    adj.rate = adj.rate * 1000,
    lci = lci * 1000,
    uci = uci * 1000
  )

##AGE adjusted RATES BY REGION 
R_expand_age <- lexpand(samp_all_men, birth = bi_date, entry = dg_date, exit = ex_date,
                        per = seq(2000,2019,1), age = c(0:100,Inf),
                        aggre = list(year = per, age = age, region = region_label))

#make age ranges 
R_expand_age <- R_expand_age %>%
  mutate(age_range = case_when(
    age < 18 ~ "<18",
    age >= 18 & age <= 29 ~ "18-29",
    age >= 30 & age <= 39 ~ "30-39",
    age >= 40 & age <= 49 ~ "40-49",
    age >= 50 & age <= 59 ~ "50-59",
    age >= 60 & age <= 69 ~ "60-69",
    age >= 70 & age <= 79 ~ "70-79",
    age >= 80 & age <= 89 ~ "80-89",
    age >= 90 ~ "90+"
  ))

R_expand_age <- R_expand_age[, .(TotalPyrs = sum(pyrs)), by = .(year, age_range, region)]

R_expand_age <- R_expand_age[age_range != "<18"]

##PSA tests

R_samp_psa_tests <- merge(samp_psa_tests, samp_all_men[, .(patid, region_label)], by = "patid", all.x = TRUE)

R_dt_final <- R_samp_psa_tests[, .N, by = .(age_category, psa_year, region_label)]

names(R_dt_final) <- c("age_range", "year", "region", "count")

R_all <- merge(R_expand_age, R_dt_final, by = c("age_range", "year" , "region"), all.x = TRUE)

# Replace NA in Count with 0
R_all[is.na(R_all$count), "count"] <- 0

##get standard population 
R_stdpop_2018 <- R_all[year == 2018, .(stdpop = sum(TotalPyrs)), by = .(age_range, region)]

# Merge stdpop back to the original table, matching on region and age_range
R_all <- merge(R_all, R_stdpop_2018, by = c("age_range", "region"), all.x = TRUE)

##overall 
library(purrr)
overall_rate_region_age <- R_all %>%
  group_by(region) %>%
  summarise(age_adjust = list(ageadjust.direct(count = count,   #count of events
                                               pop = TotalPyrs,          #person years of DFpop
                                               rate = NULL,                
                                               stdpop = stdpop,            #standard population (European standard population per age_cat)
                                               conf.level = 0.95))) %>%
  mutate(age_adjust = map(age_adjust, as.data.frame.list)) %>%
  unnest(cols = c(age_adjust))


##by year 
rate_region_age <- R_all %>%
  group_by(region, year) %>%
  summarise(age_adjust = list(ageadjust.direct(count = count,   #count of events
                                               pop = TotalPyrs,          #person years of DFpop
                                               rate = NULL,                
                                               stdpop = stdpop,            #standard population (European standard population per age_cat)
                                               conf.level = 0.95))) %>%
  mutate(age_adjust = map(age_adjust, as.data.frame.list)) %>%
  unnest(cols = c(age_adjust))


rate_region_age <- na.omit(rate_region_age, cols = "region")

rate_region_age <- rate_region_age %>%
  mutate(
    crude.rate = crude.rate * 1000,
    adj.rate = adj.rate * 1000,
    lci = lci * 1000,
    uci = uci * 1000
  )

regions_adj <-ggplot(rate_region_age, aes(x = year, y = adj.rate, group = region, color = region)) +
  geom_line() +  # Draw lines for adjusted rates, with colors based on region
  labs(title = "Adjusted Rate over Years by Region",
       x = "Year", y = "Adjusted Rate",
       color = "Region") +  # Label for the legend
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1),  # Improve readability of x-axis labels
        legend.position = "bottom")  # Move legend to bottom
print(regions_adj)


##rate by above/below ref thresh 

expand_age <- lexpand(samp_all_men, birth = bi_date, entry = dg_date, exit = ex_date,
                      per = seq(2000, 2019, 1), age = c(0:100, Inf),
                      aggre = list(year = per, age = age))



# Separate counts for tests above and below threshold
dt_final2_above <- samp_psa_tests[above_ref_thresh == 1, .N, by = .(age_category, psa_year)]
dt_final2_below <- samp_psa_tests[above_ref_thresh == 0, .N, by = .(age_category, psa_year)]

names(dt_final2_above) <- c("age_range", "year", "Count_above")
names(dt_final2_below) <- c("age_range", "year", "Count_below")

# Merge counts with person-years
all_above <- merge(expand_age, dt_final2_above, by = c("age_range", "year"), all.x = TRUE)
all_below <- merge(expand_age, dt_final2_below, by = c("age_range", "year"), all.x = TRUE)

# Replace NA in Count with 0
all_above[is.na(all_above$Count_above), "Count_above"] <- 0
all_below[is.na(all_below$Count_below), "Count_below"] <- 0

# Merge standard population
all_above <- merge(all_above, stdpop_2018, by = "age_range", all.x = TRUE)
all_below <- merge(all_below, stdpop_2018, by = "age_range", all.x = TRUE)

age_adjusted_rate_above <- all_above %>%
  group_by(year) %>%
  summarise(age_adjust = list(ageadjust.direct(count = Count_above,   
                                               pop = TotalPyrs,          
                                               rate = NULL,                
                                               stdpop = stdpop,            
                                               conf.level = 0.95))) %>%
  mutate(age_adjust = map(age_adjust, as.data.frame.list)) %>%
  unnest(cols = c(age_adjust))

age_adjusted_rate_below <- all_below %>%
  group_by(year) %>%
  summarise(age_adjust = list(ageadjust.direct(count = Count_below,   
                                               pop = TotalPyrs,          
                                               rate = NULL,                
                                               stdpop = stdpop,            
                                               conf.level = 0.95))) %>%
  mutate(age_adjust = map(age_adjust, as.data.frame.list)) %>%
  unnest(cols = c(age_adjust))

# Convert rates to per 1000 person-years
age_adjusted_rate_above <- age_adjusted_rate_above %>%
  mutate(
    crude.rate = crude.rate * 1000,
    adj.rate = adj.rate * 1000,
    lci = lci * 1000,
    uci = uci * 1000
  )

age_adjusted_rate_below <- age_adjusted_rate_below %>%
  mutate(
    crude.rate = crude.rate * 1000,
    adj.rate = adj.rate * 1000,
    lci = lci * 1000,
    uci = uci * 1000
  )

age_adjusted_rate_above$group <- 'Above Threshold'
age_adjusted_rate_below$group <- 'Below Threshold'

# Combine the data frames
rate_threshold <- rbind(age_adjusted_rate_above, age_adjusted_rate_below)

threshold_adj <- ggplot(rate_threshold, aes(x = year, y = adj.rate, group = group, color = group)) +
  geom_line() +  # Draw lines for adjusted rates, with colors based on group
  labs(title = "Adjusted PSA Testing Rate over Years by Threshold",
       x = "Year", y = "Adjusted Rate per 1000 Person-Years",
       color = "Threshold") +  # Label for the legend
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1),  # Improve readability of x-axis labels
        legend.position = "bottom")  # Move legend to bottom

# Print the plot
print(threshold_adj)


#'BY IMD 
IMD_expand_age <- lexpand(samp_all_men, birth = bi_date, entry = dg_date, exit = ex_date,
                          per = seq(2000,2019,1), age = c(0:100,Inf),
                          aggre = list(year = per, age = age, imd = imd))

#make age ranges 
IMD_expand_age <- IMD_expand_age %>%
  mutate(age_range = case_when(
    age < 18 ~ "<18",
    age >= 18 & age <= 29 ~ "18-29",
    age >= 30 & age <= 39 ~ "30-39",
    age >= 40 & age <= 49 ~ "40-49",
    age >= 50 & age <= 59 ~ "50-59",
    age >= 60 & age <= 69 ~ "60-69",
    age >= 70 & age <= 79 ~ "70-79",
    age >= 80 & age <= 89 ~ "80-89",
    age >= 90 ~ "90+"
  ))

IMD_expand_age <- IMD_expand_age[, .(TotalPyrs = sum(pyrs)), by = .(year, age_range, imd)]

IMD_expand_age <- IMD_expand_age[age_range != "<18"]

IMD_samp_psa_tests <- merge(samp_psa_tests, samp_all_men[, .(patid, imd)], by = "patid", all.x = TRUE)

IMD_dt_final <- IMD_samp_psa_tests[, .N, by = .(age_category, psa_year, imd)]

names(IMD_dt_final) <- c("age_range", "year", "imd", "count")



IMD_test <- merge(IMD_expand_age, IMD_dt_final, by = c("age_range", "year" , "imd"), all.x = TRUE)

# Replace NA in Count with 0
IMD_test[is.na(IMD_test$count), "count"] <- 0

##get standard population 
IMD_test_stdpop_2018 <- IMD_test[year == 2018, .(stdpop = sum(TotalPyrs)), by = .(age_range, imd)]

# Merge stdpop back to the original table, matching on region and age_range
IMD_test <- merge(IMD_test, IMD_test_stdpop_2018, by = c("age_range", "imd"), all.x = TRUE)


#overall 
overall_imd_age_rate <- IMD_test %>%
  group_by(imd) %>%
  summarise(age_adjust = list(ageadjust.direct(count = count,   #count of events
                                               pop = TotalPyrs,          #person years of DFpop
                                               rate = NULL,                
                                               stdpop = stdpop,            #standard population (European standard population per age_cat)
                                               conf.level = 0.95))) %>%
  mutate(age_adjust = map(age_adjust, as.data.frame.list)) %>%
  unnest(cols = c(age_adjust))

#by year 

imd_age_rate <- IMD_test %>%
  group_by(year, imd) %>%
  summarise(age_adjust = list(ageadjust.direct(count = count,   #count of events
                                               pop = TotalPyrs,          #person years of DFpop
                                               rate = NULL,                
                                               stdpop = stdpop,            #standard population (European standard population per age_cat)
                                               conf.level = 0.95))) %>%
  mutate(age_adjust = map(age_adjust, as.data.frame.list)) %>%
  unnest(cols = c(age_adjust))


imd_age_rate <- imd_age_rate %>%
  mutate(
    crude.rate = crude.rate * 1000,
    adj.rate = adj.rate * 1000,
    lci = lci * 1000,
    uci = uci * 1000
  )

imd_age_rate <- na.omit(imd_age_rate, cols = "imd")

IMD_adj <-ggplot(imd_age_rate, aes(x = year, y = adj.rate, group = imd, color = imd)) +
  geom_line() +  # Draw lines for adjusted rates, with colors based on region
  labs(title = "Adjusted Rate over Years by IMD",
       x = "Year", y = "Adjusted Rate",
       color = "imd") +  # Label for the legend
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1),  # Improve readability of x-axis labels
        legend.position = "bottom")  # Move legend to bottom

print(IMD_adj)

#ETHNICUTY RATES 
eth_expand_age <- lexpand(samp_all_men, birth = bi_date, entry = dg_date, exit = ex_date,
                          per = seq(2000,2019,1), age = c(0:100,Inf),
                          aggre = list(year = per, age = age, eth1 = eth1))
#make age ranges 
eth_expand_age <- eth_expand_age %>%
  mutate(age_range = case_when(
    age < 18 ~ "<18",
    age >= 18 & age <= 29 ~ "18-29",
    age >= 30 & age <= 39 ~ "30-39",
    age >= 40 & age <= 49 ~ "40-49",
    age >= 50 & age <= 59 ~ "50-59",
    age >= 60 & age <= 69 ~ "60-69",
    age >= 70 & age <= 79 ~ "70-79",
    age >= 80 & age <= 89 ~ "80-89",
    age >= 90 ~ "90+"
  ))

eth_expand_age <- eth_expand_age[, .(TotalPyrs = sum(pyrs)), by = .(year, age_range, eth1)]

eth_expand_age <- eth_expand_age[age_range != "<18"]

eth_samp_psa_tests <- merge(samp_psa_tests, samp_all_men[, .(patid, eth1)], by = "patid", all.x = TRUE)

eth_dt_final <- eth_samp_psa_tests[, .N, by = .(age_category, psa_year, eth1)]

names(eth_dt_final) <- c("age_range", "year", "eth1", "count")

eth_test <- merge(eth_expand_age, eth_dt_final, by = c("age_range", "year" , "eth1"), all.x = TRUE)

names(eth_test) <- c("age_range", "year", "ethnicity", "TotalPyrs", "count")

# Replace NA in Count with 0
eth_test[is.na(eth_test$count), "count"] <- 0

##get standard population 
eth_test_stdpop_2018 <- eth_test[year == 2018, .(stdpop = sum(TotalPyrs)), by = .(age_range, ethnicity)]

# Merge stdpop back to the original table, matching on region and age_range
eth_test <- merge(eth_test, eth_test_stdpop_2018, by = c("age_range", "ethnicity"), all.x = TRUE)

#overall 
overall_eth_age_rate <- eth_test %>%
  group_by(ethnicity) %>%
  summarise(age_adjust = list(ageadjust.direct(count = count,   #count of events
                                               pop = TotalPyrs,          #person years of DFpop
                                               rate = NULL,                
                                               stdpop = stdpop,            #standard population (European standard population per age_cat)
                                               conf.level = 0.95))) %>%
  mutate(age_adjust = map(age_adjust, as.data.frame.list)) %>%
  unnest(cols = c(age_adjust))

#by year 
eth_age_rate <- eth_test %>%
  group_by(year, ethnicity) %>%
  summarise(age_adjust = list(ageadjust.direct(count = count,   #count of events
                                               pop = TotalPyrs,          #person years of DFpop
                                               rate = NULL,                
                                               stdpop = stdpop,            #standard population (European standard population per age_cat)
                                               conf.level = 0.95))) %>%
  mutate(age_adjust = map(age_adjust, as.data.frame.list)) %>%
  unnest(cols = c(age_adjust))

eth_age_rate<- eth_age_rate %>%
  mutate(
    crude.rate = crude.rate * 1000,
    adj.rate = adj.rate * 1000,
    lci = lci * 1000,
    uci = uci * 1000
    
  )

eth_age_rate <- na.omit(eth_age_rate, cols = "ethnicity")
eth_adj <-ggplot(eth_age_rate, aes(x = year, y = adj.rate, group = ethnicity, color = ethnicity)) +
  geom_line() +  # Draw lines for adjusted rates, with colors based on region
  labs(title = "Adjusted Rate over Years by Ethnicity",
       x = "Year", y = "Adjusted Rate",
       color = "ethnicity") +  # Label for the legend
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1),  # Improve readability of x-axis labels
        legend.position = "bottom")  # Move legend to bottom

print (eth_adj)

##BY AGE 
samp_psa_tests2 <- merge(samp_psa_tests, samp_all_men[, .(patid)], by = "patid", all.x = TRUE)

dt_final2 <- samp_psa_tests2[, .N, by = .(age_category, psa_year)]

names(dt_final2) <- c("age_range", "year", "count")

age_psa <- merge(expand_age, dt_final2, by = c("age_range", "year"), all.x = TRUE)

# Replace NA in Count with 0
age_psa[is.na(age_psa$Count), "Count"] <- 0

##get standard population 
age_psa_stdpop_2018 <- age_psa[year == 2018, .(age_range, stdpop = TotalPyrs)]

# Merge stdpop back to the original table, matching on region and age_range
age_psa <- merge(age_psa, age_psa_stdpop_2018, by = c("age_range"), all.x = TRUE)

age_rates <- age_psa %>%
  group_by(year, age_range) %>%
  summarise(age_adjust = list(ageadjust.direct(count = count,   #count of events
                                               pop = TotalPyrs,          #person years of DFpop
                                               rate = NULL,                
                                               stdpop = stdpop,            #standard population (European standard population per age_cat)
                                               conf.level = 0.95))) %>%
  mutate(age_adjust = map(age_adjust, as.data.frame.list)) %>%
  unnest(cols = c(age_adjust))

age_rates<- age_rates%>%
  mutate(
    crude.rate = crude.rate * 1000,
    adj.rate = adj.rate * 1000,
    lci = lci * 1000,
    uci = uci * 1000
  )

rm(age_psa, age_psa_stdpop_2020, dt_final2, eth_dt_final, eth_test, eth_test_stdpop_2020, IMD_samp_psa_tests, IMD_test, R_stdpop_2020, R_samp_psa_tests)
rm(samp_psa_tests2, stdpop_2020, R_dt_final, IMD_dt_final, eth_samp_psa_tests, IMD_test_stdpop_2020, regions_adj)
rm(age_adjusted_rate_overall, age_rates, all, eth_adj, eth_age_rate, IMD_adj, imd_age_rate, rate_region_age, R_all)


############################## NB MODELS for liklihood of psa testing ############################# 

#get number of tests per person overall 
nb_model_test_counts <- samp_psa_tests[, .(psa_test_count = .N), by = .(patid)]

y_nb_model_test_counts <- samp_psa_tests[, .(psa_test_count = .N), by = .(patid, psa_year)]

# Merge test counts into samp_all_men
nb_model_full_data <- merge(samp_all_men, nb_model_test_counts, by = "patid", all.x = TRUE)

y_nb_model_full_data <- merge(samp_all_men, y_nb_model_test_counts, by = "patid", all.x = TRUE)

# Replace NA in psa_test_count with 0 (for those with no tests)
nb_model_full_data[is.na(psa_test_count), psa_test_count := 0]

#make region a factor 
nb_model_full_data$region_label <- as.factor(nb_model_full_data$region_label)

#Make south east the reference 
nb_model_full_data$region_label  <- relevel(factor(nb_model_full_data$region_label ), ref = "South East")

# make imd categorical 
nb_model_full_data$imd <- as.factor(nb_model_full_data$imd)

#make ethnicity a factor and white as the reference 
nb_model_full_data$eth1 <- as.factor(nb_model_full_data$eth1)

nb_model_full_data$eth1 <- relevel(nb_model_full_data$eth1, ref = "White")

#add age  in 
# Calculate age at the start of the study 
library(lubridate)

nb_model_full_data$age_at_start <- year(as.Date(nb_model_full_data$kc_start_date)) - nb_model_full_data$yob

nb_model_full_data <- nb_model_full_data%>%
  mutate(age_range = case_when(
    age_at_start < 18 ~ "<18",
    age_at_start >= 18 & age_at_start <= 29 ~ "18-29",
    age_at_start >= 30 & age_at_start <= 39 ~ "30-39",
    age_at_start >= 40 & age_at_start <= 49 ~ "40-49",
    age_at_start >= 50 & age_at_start <= 59 ~ "50-59",
    age_at_start >= 60 & age_at_start <= 69 ~ "60-69",
    age_at_start >= 70 & age_at_start <= 79 ~ "70-79",
    age_at_start >= 80 & age_at_start <= 89 ~ "80-89",
    age_at_start >= 90 ~ "90+"
  ))

nb_model_full_data$age_range <- as.factor(nb_model_full_data$age_range)
nb_model_full_data$age_range <- relevel(nb_model_full_data$age_range, ref = "60-69")

###########################################

#####UNIVARIATE MODELS########## NB cant do by psa value since its an overall count of tests
library(MASS)
uni_nb_region <- glm.nb(psa_test_count ~ region_label + offset(log(follow_up_years)), data = nb_model_full_data)

summary(uni_nb_region)

uni_nb_eth <- glm.nb(psa_test_count ~ eth1 + offset(log(follow_up_years)), data = nb_model_full_data)

summary(uni_nb_eth)

uni_nb_imd <- glm.nb(psa_test_count ~ imd + offset(log(follow_up_years)), data = nb_model_full_data)

summary(uni_nb_imd)

uni_nb_age <- glm.nb(psa_test_count ~ age_range + offset(log(follow_up_years)), data = nb_model_full_data)

summary(uni_nb_age)

uni_nb_pc <- glm.nb(psa_test_count ~ prostate_cancer + offset(log(follow_up_years)), data = nb_model_full_data)

summary(uni_nb_pc)

##fh
uni_nb_fh <- glm.nb(psa_test_count ~ fh + offset(log(follow_up_years)), data = nb_model_full_data)

summary(uni_nb_fh)

##get PSA ever above ref for uni model 
patients_above_ref <- samp_psa_tests[above_ref_thresh == 1, unique(patid)]
nb_model_full_data[, ever_above_ref := ifelse(patid %in% patients_above_ref, 1, 0)]

uni_nb_psa <- glm.nb(psa_test_count ~ ever_above_ref + offset(log(follow_up_years)), data = nb_model_full_data)

summary(uni_nb_psa)

###ever had symptom before a test 
psa_symptoms_merged <- samp_psa_tests %>%
  left_join(symptoms, by = c("patid", "psa_date"))

# Create an indicator for the presence of symptoms within 90 days before PSA test
psa_symptoms_merged <- psa_symptoms_merged %>%
  mutate(symptom_present = ifelse(!is.na(symptom), 1, 0))

# Summarize data to identify if any PSA test for each patient had a symptom present
symptom_summary <- psa_symptoms_merged %>%
  group_by(patid) %>%
  summarize(symptom_present = as.integer(any(symptom_present == 1))) %>%
  ungroup()


# Ensure all patients are included, with 0 for those with no PSA tests or no symptoms

uni_nb_symptom <- glm.nb(psa_test_count ~ symptom_present + offset(log(follow_up_years)), data = nb_model_full_data)
summary(uni_nb_symptom)

####### MUTLI VAIRABLE MODEL####### without selection

#3:08pm - 3:22pm
multi_nb <- glm.nb(psa_test_count ~ region_label + age_range + eth1 + imd + 
                     prostate_cancer + ever_above_ref + fh + symptom_present + offset(log(follow_up_years)), data = nb_model_full_data)

summary(multi_nb)


###INTERVALS#### first create the intervals 

samp_psa_tests[, days_between_tests := as.numeric(gsub("[^0-9.-]", "", days_between_tests))]
samp_psa_tests[, months_between_tests := as.numeric(gsub("[^0-9.-]", "", months_between_tests))]
samp_psa_tests[, years_between_tests := as.numeric(gsub("[^0-9.-]", "", years_between_tests))]
NInt_samp_psa_tests <- samp_psa_tests


# Ensure T_samp_psa_tests is a data.table
NInt_samp_psa_tests <- samp_psa_tests
setDT(NInt_samp_psa_tests)
NInt_samp_psa_tests[, psa_date := as.Date(psa_date, format="%Y-%m-%d")]

# Order by patid and psa_date
setorder(NInt_samp_psa_tests, patid, psa_date)

# Calculate the difference in days between tests for each patid
NInt_samp_psa_tests[, days_between_tests := {
  differences <- as.numeric(diff(psa_date))
  c(differences, NA_real_)  # Append NA at the end for each patid
}, by = patid]

#NInt_samp_psa_tests[, days_between_tests := c(NA, diff(psa_date)), by = patid]
NInt_samp_psa_tests[, months_between_tests := days_between_tests / 30.44]
NInt_samp_psa_tests[, years_between_tests := days_between_tests / 365.25]

NInt_samp_psa_tests[samp_all_men, prostate_cancer := i.prostate_cancer, on = .(patid)]

#remove nas for intervals 
NInt_samp_psa_tests_clean <- NInt_samp_psa_tests[!is.na(NInt_samp_psa_tests$months_between_tests), ]

##add in region ethnicity etc 
NInt_samp_psa_tests_clean <- merge(NInt_samp_psa_tests_clean, 
                                   samp_all_men[, c("patid", "region_label", "imd", "eth1")], 
                                   by = "patid", 
                                   all.x = TRUE)

##make age_range

NInt_samp_psa_tests_clean <- NInt_samp_psa_tests_clean%>%
  mutate(age_range = case_when(
    age_psa1 < 18 ~ "<18",
    age_psa1 >= 18 & age_psa1 <= 29 ~ "18-29",
    age_psa1 >= 30 & age_psa1 <= 39 ~ "30-39",
    age_psa1 >= 40 & age_psa1 <= 49 ~ "40-49",
    age_psa1 >= 50 & age_psa1 <= 59 ~ "50-59",
    age_psa1 >= 60 & age_psa1 <= 69 ~ "60-69",
    age_psa1 >= 70 & age_psa1 <= 79 ~ "70-79",
    age_psa1 >= 80 & age_psa1 <= 89 ~ "80-89",
    age_psa1 >= 90 ~ "90+"
  ))
##remove na make variables factors and set the reference ranges
Nint_samp_psa_multi <- NInt_samp_psa_tests_clean[!is.na(eth1) & !is.na(region_label) & !is.na(imd)]

#make unknowns unknown instead of deleting 
NInt_samp_psa_tests_clean[, imd := as.character(imd)]
NInt_samp_psa_tests_clean[is.na(imd), imd := "unknown"]
NInt_samp_psa_tests_clean[is.na(region_label), region_label := "unknown"]

Nint_samp_psa_multi$region_label <- as.factor(Nint_samp_psa_multi$region_label)
Nint_samp_psa_multi$region_label <- relevel(Nint_samp_psa_multi$region_label, ref = "South East")

Nint_samp_psa_multi[, imd := as.factor(imd)]

Nint_samp_psa_multi$eth1 <- as.factor(Nint_samp_psa_multi$eth1)

Nint_samp_psa_multi$eth1 <- relevel(Nint_samp_psa_multi$eth1, ref = "White")

Nint_samp_psa_multi$age_range <- as.factor(Nint_samp_psa_multi$age_range)

Nint_samp_psa_multi$age_range <- relevel(Nint_samp_psa_multi$age_range, ref = "60-69")


##ADD IN SYMPTOMS 
##loop for other symptoms 
symptom_list <- c("Fatigue", "UWL", "LUTSOverall", "Haematuria", "BonePain", "BackPain" , "ED")
setDT(merged_data_all)
setDT(symptoms)
merged_data_all <- copy(Nint_samp_psa_multi)
merged_data_all <- copy(NInt_samp_psa_tests_clean)


# Loop through each symptom
for (symptom_name in symptom_list) {
  # Step 1: Filter the symptom table for the specific symptom
  filtered_symptom_all <- symptoms[symptom == symptom_name, .(patid, psa_date, indicator = 1)]
  
  # Step 2: Perform a left join on the specified columns
  merged_data_all <- merge(merged_data_all, filtered_symptom_all, 
                           by = c("patid", "psa_date"), 
                           all.x = TRUE)
  
  # Step 3: Create the indicator column for the current symptom
  merged_data_all[, (symptom_name) := ifelse(is.na(indicator), 0, 1)]
  
  # Step 4: Remove the temporary indicator column
  merged_data_all[, indicator := NULL]
  merged_data_all <- unique(merged_data_all)
}

###############################
##########RUN MODELS########## (mixed effects)

#make region a factor 
group_4$region_label <- as.factor(group_4$region_label)

#Make south east the reference 
group_4$region_label  <- relevel(factor(group_4$region_label ), ref = "South East")

#make imd categorical 
merged_data_all$imd <- as.factor(merged_data_all$imd)

#make ethnicity a factor and white as the reference 
merged_data_all$eth1 <- as.factor(merged_data_all$eth1)

merged_data_all$eth1 <- relevel(merged_data_all$eth1, ref = "White")

group_4$eth1 <- as.factor(group_4$eth1)

group_4$region_label <- as.factor(group_4$region_label)

group_4$region_label <- relevel(group_4$region_label, ref = "South East")
merged_data_all$age_range <- as.factor(merged_data_all$age_range)

merged_data_all$age_range <- relevel(merged_data_all$age_range, ref = "60-69")


# Step 2: Relevel eth1 to set "White" as the reference category
group_4$eth1 <- relevel(group_4$eth1, ref = "White")

#log transform months_between_tests so it has a normal distribution 
merged_data_all$log_months_between_tests <- log(merged_data_all$months_between_tests)

head(merged_data_all)

library(glmmTMB)
library(nlme)
##region 
lme_model_uni_region <- lme(
  fixed = log_months_between_tests ~ region_label, 
  random = ~ 1 | patid, 
  data = merged_data_all, 
  control = lmeControl(opt = "optim")
)
summary(lme_model_uni_region)


##UWL
lme_model_uni_UWL <- lme(
  fixed = log_months_between_tests ~ UWL, 
  random = ~ 1 | patid, 
  data = merged_data_all, 
  control = lmeControl(opt = "optim")
)
summary(lme_model_uni_UWL)

##ethnicty
lme_model_uni_eth <- lme(
  fixed = log_months_between_tests ~ eth1, 
  random = ~ 1 | patid, 
  data = merged_data_all, 
  control = lmeControl(opt = "optim")
)

summary(lme_model_uni_eth)
##imd
lme_model_uni_imd <- lme(
  fixed = log_months_between_tests ~ imd, 
  random = ~ 1 | patid, 
  data = merged_data_all, 
  control = lmeControl(opt = "optim")
)
summary(lme_model_uni_imd)

merged_data_all <- merge(merged_data_all, samp_all_men[, .(patid, fh)], by = "patid", all.x = TRUE)
##fh
lme_model_uni_fh <- lme(
  fixed = log_months_between_tests ~ fh, 
  random = ~ 1 | patid, 
  data = merged_data_all, 
  control = lmeControl(opt = "optim")
)
summary(lme_model_uni_fh)

#age range 
lme_model_uni_age <- lme(
  fixed = log_months_between_tests ~ age_range, 
  random = ~ 1 | patid, 
  data = merged_data_all, 
  control = lmeControl(opt = "optim")
)
summary(lme_model_uni_age)

lme_model_uni_fatigue <- lme(
  fixed = log_months_between_tests ~ Fatigue, 
  random = ~ 1 | patid, 
  data = merged_data_all, 
  control = lmeControl(opt = "optim")
)

summary(lme_model_uni_fatigue)

lme_model_uni_haem <- lme(
  fixed = log_months_between_tests ~ Haematuria, 
  random = ~ 1 | patid, 
  data = merged_data_all, 
  control = lmeControl(opt = "optim")
)

summary(lme_model_uni_haem)

lme_model_uni_bonepain <- lme(
  fixed = log_months_between_tests ~ BonePain, 
  random = ~ 1 | patid, 
  data = merged_data_all, 
  control = lmeControl(opt = "optim")
)

summary(lme_model_uni_bonepain)

lme_model_uni_backpain <- lme(
  fixed = log_months_between_tests ~ BackPain, 
  random = ~ 1 | patid, 
  data = merged_data_all, 
  control = lmeControl(opt = "optim")
)

summary(lme_model_uni_backpain)

lme_model_uni_ed <- lme(
  fixed = log_months_between_tests ~ ED, 
  random = ~ 1 | patid, 
  data = merged_data_all, 
  control = lmeControl(opt = "optim")
)

summary(lme_model_uni_ed)

lme_model_uni_LUTS <- lme(
  fixed = log_months_between_tests ~ LUTSOverall, 
  random = ~ 1 | patid, 
  data = merged_data_all, 
  control = lmeControl(opt = "optim")
)

summary(lme_model_uni_LUTS)

lme_model_uni_value <- lme(
  fixed = log_months_between_tests ~ above_ref_thresh, 
  random = ~ 1 | patid, 
  data = merged_data_all, 
  control = lmeControl(opt = "optim")
)

summary(lme_model_uni_value)

lme_model_uni_prostate_cancer <- lme(
  fixed = log_months_between_tests ~ prostate_cancer, 
  random = ~ 1 | patid, 
  data = merged_data_all, 
  control = lmeControl(opt = "optim")
)


##multi model 11:20am
lme_model_multi <- lme(
  fixed = log_months_between_tests ~ region_label + age_range + eth1 + imd + fh + 
    above_ref_thresh + prostate_cancer + Fatigue + BonePain + BackPain + 
    UWL + Haematuria + ED + LUTSOverall, 
  random = ~ 1 | patid, 
  data = merged_data_all, 
  control = lmeControl(opt = "optim")
)
summary(lme_model_multi)

##get median intervals and IQR for symptoms - in additional file 1 

##no symptoms 
merged_data_all[Fatigue == 0 & UWL == 0  & BackPain == 0 & ED == 0 & LUTSOverall == 0
                & BonePain == 0 & Haematuria == 0, .(
                  median_months = median(months_between_tests, na.rm = TRUE),
                  Q1 = quantile(months_between_tests, 0.25, na.rm = TRUE),
                  Q3 = quantile(months_between_tests, 0.75, na.rm = TRUE)
                )]

##overall interval 
merged_data_all_50[, .(N= .N,
                       median_months = median(months_between_tests, na.rm = TRUE),
                       Q1 = quantile(months_between_tests, 0.25, na.rm = TRUE),
                       Q3 = quantile(months_between_tests, 0.75, na.rm = TRUE)
)]
##total distinct pats with at least one interval 
uniqueN(merged_data_all_50$patid)

# Define a function to calculate statistics
calculate_stats <- function(data, group_col) {
  data[, .(
    Group = group_col,
    GroupValue = get(group_col),
    N_patients = uniqueN(patid),
    N_tests = .N,
    median_months = median(months_between_tests, na.rm = TRUE),
    Q1 = quantile(months_between_tests, 0.25, na.rm = TRUE),
    Q3 = quantile(months_between_tests, 0.75, na.rm = TRUE)
  ), by = group_col]
}

# Calculate statistics for each group
stats_eth1 <- calculate_stats(merged_data_all, "eth1")
stats_fh <-calculate_stats(merged_data_all, "fh")
stats_region <- calculate_stats(merged_data_all, "region_label")
stats_age <-calculate_stats(merged_data_all, "age_range")

stats_imd <- calculate_stats(merged_data_all_70, "imd")
stats_psa <- calculate_stats(merged_data_all, "above_ref_thresh")
stats_pc <- calculate_stats(merged_data_all, "prostate_cancer")
stats_uwl <- calculate_stats(merged_data_all, "UWL")
stats_LUTS <- calculate_stats(merged_data_all, "LUTSOverall")
stats_bonepain <- calculate_stats(merged_data_all, "BonePain")
stats_backpain <- calculate_stats(merged_data_all, "BackPain")
stats_haem <- calculate_stats(merged_data_all, "Haematuria")
stats_fatigue <- calculate_stats(merged_data_all, "Fatigue")
stats_ed <- stats_fatigue <- calculate_stats(merged_data_all, "ED")



# Combine the results into one long table
combined_stats <- rbind(
  stats_region, stats_age, stats_eth1, stats_imd, stats_psa, 
  stats_ed, stats_fatigue, stats_haem, stats_backpain, 
  stats_bonepain, stats_LUTS, stats_uwl, fill = TRUE
)
#get into excel 
write.xlsx(combined_stats, file = "median_intervals.xlsx")

#count number of PSA tests that occured <6 months, <1 year, <1.5 years, <2 years
merged_data_all %>%
  filter(months_between_tests < 30) %>%
  nrow()
#<6 months 
(650660/2742240)*100

#<12 months
(1279488 /2742240)*100

#<18 months 
(1724453/2742240)*100

#<24 months 
(1950933/2742240)*100

#<30 months 
(2115229/2742240)*100

##find all the patients who never have a PSA over the threshold and no cancer diagnosis

group_4 <- merged_data_all[prostate_cancer == "No"]
dis<- unique(group_4[, .(patid)]) #746809
rm(dis)

invalid_patids <- merged_data_all[above_ref_thresh == 1, unique(patid)]
group_4 <- merged_data_all[!patid %in% invalid_patids & prostate_cancer == "No"]
dis_group4<- unique(group_4[, .(patid)]) #605,354

#check duplicates 
group_4[duplicated(group_4)]

#find months for patients in group_4 
group_4 %>%
  filter(months_between_tests < 24) %>%
  nrow()

#<6 
(151771/1351400)*100
#<12 
(427619/1351400)*100
#<18 
(681719/1351400)*100
#<24 
(824275/1351400)*100
#<30
(932589/1351400)*100
##get medians for group_4

##overall interval 
group_4[, .(N= .N,
            median_months = median(months_between_tests, na.rm = TRUE),
            Q1 = quantile(months_between_tests, 0.25, na.rm = TRUE),
            Q3 = quantile(months_between_tests, 0.75, na.rm = TRUE)
)]
uniqueN(group_4$patid)

# Define a function to calculate statistics
g4_calculate_stats <- function(data, group_col) {
  data[, .(
    Group = group_col,
    GroupValue = get(group_col),
    N_patients = uniqueN(patid),
    N_tests = .N,
    median_months = median(months_between_tests, na.rm = TRUE),
    Q1 = quantile(months_between_tests, 0.25, na.rm = TRUE),
    Q3 = quantile(months_between_tests, 0.75, na.rm = TRUE)
  ), by = group_col]
}

# Calculate statistics for each group
g4_stats_eth1 <- g4_calculate_stats(group_4, "eth1")
g4_stats_region <- g4_calculate_stats(group_4, "region_label")
g4_stats_age <-g4_calculate_stats(group_4, "age_range")

g4_stats_imd <- g4_calculate_stats(group_4, "imd")
g4_stats_psa <- g4_calculate_stats(group_4, "above_ref_thresh")
g4_stats_pc <- g4_calculate_stats(group_4, "prostate_cancer")
g4_stats_uwl <- g4_calculate_stats(group_4, "UWL")
g4_stats_LUTS <- g4_calculate_stats(group_4, "LUTSOverall")
g4_stats_bonepain <- g4_calculate_stats(group_4, "BonePain")
g4_stats_backpain <- g4_calculate_stats(group_4, "BackPain")
g4_stats_haem <- g4_calculate_stats(group_4, "Haematuria")
g4_stats_fatigue <- g4_calculate_stats(group_4, "Fatigue")
g4_stats_ed <- g4_calculate_stats(group_4, "ED")
g4_stats_fh <- g4_calculate_stats(group_4, "fh")


# Combine the results into one long table
g4_combined_stats <- rbind(
  g4_stats_region, g4_stats_age, g4_stats_eth1, g4_stats_imd, g4_stats_psa, 
  g4_stats_ed, g4_stats_fatigue, g4_stats_haem, g4_stats_backpain, 
  g4_stats_bonepain, g4_stats_LUTS, g4_stats_uwl, fill = TRUE
)

write.xlsx(g4_combined_stats, file = "g4_median_intervals.xlsx")

##RUN LME models for group4 
##########RUN MODELS##########
library(glmmTMB)
library(nlme)
##region 
g4_lme_model_uni_region <- lme(
  fixed = log_months_between_tests ~ region_label, 
  random = ~ 1 | patid, 
  data = group_4, 
  control = lmeControl(opt = "optim")
)
summary(g4_lme_model_uni_region)

##UWL
g4_lme_model_uni_UWL <- lme(
  fixed = log_months_between_tests ~ UWL, 
  random = ~ 1 | patid, 
  data = group_4, 
  control = lmeControl(opt = "optim")
)

summary(g4_lme_model_uni_UWL)

##ethnicty
g4_lme_model_uni_eth <- lme(
  fixed = log_months_between_tests ~ eth1, 
  random = ~ 1 | patid, 
  data = group_4, 
  control = lmeControl(opt = "optim")
)

summary(g4_lme_model_uni_eth)
##imd
g4_lme_model_uni_imd <- lme(
  fixed = log_months_between_tests ~ imd, 
  random = ~ 1 | patid, 
  data = group_4, 
  control = lmeControl(opt = "optim")
)

summary(g4_lme_model_uni_imd)

#age range 
g4_lme_model_uni_age <- lme(
  fixed = log_months_between_tests ~ age_range, 
  random = ~ 1 | patid, 
  data = group_4, 
  control = lmeControl(opt = "optim")
)

summary(g4_lme_model_uni_age)

g4_lme_model_uni_fatigue <- lme(
  fixed = log_months_between_tests ~ Fatigue, 
  random = ~ 1 | patid, 
  data = group_4, 
  control = lmeControl(opt = "optim")
)

summary(g4_lme_model_uni_fatigue)

g4_lme_model_uni_haem <- lme(
  fixed = log_months_between_tests ~ Haematuria, 
  random = ~ 1 | patid, 
  data = group_4, 
  control = lmeControl(opt = "optim")
)

summary(g4_lme_model_uni_haem)

g4_lme_model_uni_bonepain <- lme(
  fixed = log_months_between_tests ~ BonePain, 
  random = ~ 1 | patid, 
  data = group_4, 
  control = lmeControl(opt = "optim")
)

summary(g4_lme_model_uni_bonepain)

g4_lme_model_uni_backpain <- lme(
  fixed = log_months_between_tests ~ BackPain, 
  random = ~ 1 | patid, 
  data = group_4, 
  control = lmeControl(opt = "optim")
)

summary(g4_lme_model_uni_backpain)

g4_lme_model_uni_ed <- lme(
  fixed = log_months_between_tests ~ ED, 
  random = ~ 1 | patid, 
  data = group_4, 
  control = lmeControl(opt = "optim")
)

summary(g4_lme_model_uni_ed)

g4_lme_model_uni_LUTS <- lme(
  fixed = log_months_between_tests ~ LUTSOverall, 
  random = ~ 1 | patid, 
  data = group_4, 
  control = lmeControl(opt = "optim")
)

summary(g4_lme_model_uni_LUTS)

##these two dont work beucase g4 doesnt have any patients with prostate cancer or high PSA 
g4_lme_model_uni_value <- lme(
  fixed = log_months_between_tests ~ above_ref_thresh, 
  random = ~ 1 | patid, 
  data = group_4, 
  control = lmeControl(opt = "optim")
)

summary(g4_lme_model_uni_value)

g4_lme_model_uni_prostate_cancer <- lme(
  fixed = months_between_tests ~ prostate_cancer, 
  random = ~ 1 | patid, 
  data = group_4, 
  control = lmeControl(opt = "optim")
)

#fh 
g4_lme_model_uni_fh <- lme(
  fixed = log_months_between_tests ~ fh, 
  random = ~ 1 | patid, 
  data = group_4, 
  control = lmeControl(opt = "optim")
)
summary(g4_lme_model_uni_fh)


##multi model 7:32pm
g4_lme_model_multi <- lme(
  fixed = log_months_between_tests ~ region_label + age_range + eth1 + imd + fh +
    + Fatigue + BonePain + BackPain + 
    UWL + Haematuria + ED + LUTSOverall, 
  random = ~ 1 | patid, 
  data = group_4, 
  control = lmeControl(opt = "optim")
)

summary(g4_lme_model_multi)



###### get group 4 baseline 

NInt_samp_psa_tests[samp_all_men, prostate_cancer := i.prostate_cancer, on = .(patid)]
#need only people who have an interval so they have more than 1 test 

#remove nas for intervals 
NInt_samp_psa_tests <- NInt_samp_psa_tests[!is.na(NInt_samp_psa_tests$days_between_tests), ]

group_4 <- NInt_samp_psa_tests[prostate_cancer == "No"]
dis<- unique(group_4[, .(patid)]) #747625
rm(dis)

invalid_patids <- NInt_samp_psa_tests[above_ref_thresh == 1, unique(patid)]
group_4 <- NInt_samp_psa_tests[!patid %in% invalid_patids & prostate_cancer == "No"]
dis_group4<- unique(group_4[, .(patid)]) #605,354 - 606021

#check duplicates 
group_4[duplicated(group_4)]

#find months for patients in group_4 
group_4 %>%
  filter(months_between_tests < 30) %>%
  nrow()

##get samp all men for only the patients in dis group 4 
g4_baseline <- samp_all_men %>%
  filter(patid %in% dis_group4$patid)


#rates for family history 
#####FAMILY HISTORY 
fh_expand_age <- lexpand(samp_all_men, birth = bi_date, entry = dg_date, exit = ex_date,
                         per = seq(2000,2019,1), age = c(0:100,Inf),
                         aggre = list(year = per, age = age, fh = fh))
#make age ranges 
fh_expand_age <- fh_expand_age %>%
  mutate(age_range = case_when(
    age < 18 ~ "<18",
    age >= 18 & age <= 29 ~ "18-29",
    age >= 30 & age <= 39 ~ "30-39",
    age >= 40 & age <= 49 ~ "40-49",
    age >= 50 & age <= 59 ~ "50-59",
    age >= 60 & age <= 69 ~ "60-69",
    age >= 70 & age <= 79 ~ "70-79",
    age >= 80 & age <= 89 ~ "80-89",
    age >= 90 ~ "90+"
  ))

fh_expand_age <- fh_expand_age[, .(TotalPyrs = sum(pyrs)), by = .(year, age_range, fh)]

fh_expand_age <- fh_expand_age[age_range != "<18"]

fh_samp_psa_tests <- merge(samp_psa_tests, samp_all_men[, .(patid, fh)], by = "patid", all.x = TRUE)

fh_dt_final <- fh_samp_psa_tests[, .N, by = .(age_category, psa_year, fh.x)]

names(fh_dt_final) <- c("age_range", "year", "fh", "count")

fh_test <- merge(fh_expand_age, fh_dt_final, by = c("age_range", "year" , "fh"), all.x = TRUE)

names(fh_test) <- c("age_range", "year", "fh", "TotalPyrs", "count")

# Replace NA in Count with 0
fh_test[is.na(fh_test$count), "count"] <- 0

##get standard population 
fh_test_stdpop_2018 <- fh_test[year == 2018, .(stdpop = sum(TotalPyrs)), by = .(age_range, fh)]

# Merge stdpop back to the original table, matching on region and age_range
fh_test <- merge(fh_test, fh_test_stdpop_2018, by = c("age_range", "fh"), all.x = TRUE)

#overall 
overall_fh_age_rate <- fh_test %>%
  group_by(fh) %>%
  summarise(age_adjust = list(ageadjust.direct(count = count,   #count of events
                                               pop = TotalPyrs,          #person years of DFpop
                                               rate = NULL,                
                                               stdpop = stdpop,          
                                               conf.level = 0.95))) %>%
  mutate(age_adjust = map(age_adjust, as.data.frame.list)) %>%
  unnest(cols = c(age_adjust))

#by year 
fh_age_rate <- fh_test %>%
  group_by(year, fh) %>%
  summarise(age_adjust = list(ageadjust.direct(count = count,   #count of events
                                               pop = TotalPyrs,          #person years of DFpop
                                               rate = NULL,                
                                               stdpop = stdpop,           
                                               conf.level = 0.95))) %>%
  mutate(age_adjust = map(age_adjust, as.data.frame.list)) %>%
  unnest(cols = c(age_adjust))

fh_age_rate<- fh_age_rate %>%
  mutate(
    crude.rate = crude.rate * 1000,
    adj.rate = adj.rate * 1000,
    lci = lci * 1000,
    uci = uci * 1000
    
  )

fh_age_rate <- na.omit(fh_age_rate, cols = "fh")
fh_adj <-ggplot(fh_age_rate, aes(x = year, y = adj.rate, group = fh, color = fh)) +
  geom_line() +  # Draw lines for adjusted rates, with colors based on region
  labs(title = "Adjusted Rate over Years by Ethnicity",
       x = "Year", y = "Adjusted Rate",
       color = "fh") +  # Label for the legend
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1),  # Improve readability of x-axis labels
        legend.position = "bottom")  # Move legend to bottom

print (fh_adj)

group_4 %>%
  filter(fh == 'yes') %>%                # Filter for fh = 'yes'
  summarise(unique_patients = n_distinct(patid))


###symptoms 

symptoms_test$psa_date <- as.Date(symptoms_test$psa_date)
symptoms_test$symptom_date <- as.Date(symptoms_test$symptom_date)
symptoms_test$year <- as.integer(format(symptoms_test$psa_date, "%Y"))


summary_symp <- symptoms_test %>%
  group_by(year, symptom_group) %>%
  summarise(tests_count = n())

ggplot(summary_symp, aes(x = year, y = tests_count, color = symptom_group)) +
  geom_line() +
  labs(title = "PSA Testing Over Time by Symptom Group",
       x = "Year",
       y = "Number of Tests") +
  theme_minimal()

library(dplyr)

tests_summary <- setNames(tests_summary, c("year", "total_tests"))
# Assuming tests_summary and symptoms are your data frames
merged_df <- tests_summary %>%
  left_join(summary_symp, by = "year")

merged_df <- merged_df %>%
  mutate(proportion_with_symptoms = tests_count / total_tests)

ggplot(merged_df, aes(x = year, y = proportion_with_symptoms, color = symptom_group)) +
  geom_line() +
  geom_point() + # Add points for each data point
  labs(title = "Proportion of Tests with Symptoms Over Time, by Symptom Group",
       x = "Year",
       y = "Proportion with Symptoms") +
  theme_minimal() +
  scale_color_brewer(palette = "Set1") # Use a col


symptoms1 <- merge(symptoms, 
                   samp_all_men[, c("patid", "region_label", "imd", "eth1")], 
                   by = "patid", 
                   all.x = TRUE)



#get intervals 
setorder(symptoms1, patid, psa_date)

setDT(symptoms1)
# Calculate the difference in days between tests for each patid
symptoms1[, days_between_tests := c(NA, diff(psa_date)), by = patid]
symptoms1[, months_between_tests := days_between_tests / 30.44]
symptoms1[, years_between_tests := days_between_tests / 365.25]



lm_model_sym <- lm(months_between_tests ~ above_ref_thresh +symptom_group, data = symptoms1)
summary(lm_model_sym)



lme_model_sym <- lme(fixed = months_between_tests ~ above_ref_thresh + symptom_group , random = ~ 1 | patid, data = symptoms1, control = lmeControl(opt = "optim"))

summary(lme_model_eth)


#liklihoood psa testing if above_Ref_tresh every had a psa test above 

# Step 1: Summarize the samp_psa_tests data to identify if any test was above the threshold
above_ref_thresh <- samp_psa_tests[, .(ever_above_ref_thresh = as.integer(any(above_ref_thresh == 1))), by = .(patid)]

nb_model_full_data <- merge(nb_model_full_data, above_ref_thresh, by = "patid", all.x = TRUE)

# Replace NA in ever_above_ref_thresh with 0 (for those with no tests)
nb_model_full_data[is.na(ever_above_ref_thresh), ever_above_ref_thresh := 0]

# Fit the negative binomial model
uni_nb_above_ref <- glm.nb(psa_test_count ~ ever_above_ref_thresh + offset(log(follow_up_years)), data = nb_model_full_data)
summary(uni_nb_above_ref)


#liklihoood psa testing if symptom present   

# Step 1: Summarize the samp_psa_tests data to identify if any test was above the threshold
symptom_yes <- samp_psa_tests[, .(symptom_present = as.integer(any(above_ref_thresh == 1))), by = .(patid)]

nb_model_full_data <- merge(nb_model_full_data, above_ref_thresh, by = "patid", all.x = TRUE)

# Replace NA in ever_above_ref_thresh with 0 (for those with no tests)
nb_model_full_data[is.na(ever_above_ref_thresh), ever_above_ref_thresh := 0]

# Fit the negative binomial model
uni_nb_above_ref <- glm.nb(psa_test_count ~ ever_above_ref_thresh + offset(log(follow_up_years)), data = nb_model_full_data)
summary(uni_nb_above_ref)




####needs to be if they have ever had a symptom to work 

psa_symptoms_merged <- samp_psa_tests %>%
  left_join(symptoms, by = c("patid", "psa_date"))

# Create an indicator for the presence of symptoms within 90 days before PSA test
psa_symptoms_merged <- psa_symptoms_merged %>%
  mutate(symptom_present = ifelse(!is.na(symptom), 1, 0))

# Summarize data to identify if any PSA test for each patient had a symptom present
symptom_summary <- psa_symptoms_merged %>%
  group_by(patid) %>%
  summarize(symptom_present = as.integer(any(symptom_present == 1))) %>%
  ungroup()

# Assuming nb_model_full_data is already loaded and structured properly
# Example structure for nb_model_full_data: patid, follow_up_years, psa_test_count, other variables...

# Step 2: Merge the summarized data with the full dataset
nb_model_full_data <- nb_model_full_data %>%
  left_join(symptom_summary, by = "patid")

# Replace NA in symptom_present with 0 (for those with no tests or no symptoms)
nb_model_full_data <- nb_model_full_data %>%
  mutate(symptom_present = ifelse(is.na(symptom_present), 0, symptom_present))

# Fit the negative binomial model
uni_nb_symptom <- glm.nb(psa_test_count ~ symptom_present + offset(log(follow_up_years)), data = nb_model_full_data)
summary(uni_nb_symptom)




########
psa_symptoms_merged <- samp_psa_tests %>%
  left_join(symptoms, by = c("patid", "psa_date"))

# Create an indicator for the presence of symptoms within 90 days before PSA test
psa_symptoms_merged <- psa_symptoms_merged %>%
  mutate(symptom_present = ifelse(!is.na(symptom), 1, 0))

# Summarize data to identify if any PSA test for each patient had a symptom present
symptom_summary <- psa_symptoms_merged %>%
  group_by(patid) %>%
  summarize(symptom_present = as.integer(any(symptom_present == 1))) %>%
  ungroup()


# Ensure all patients are included, with 0 for those with no PSA tests or no symptoms

uni_nb_symptom <- glm.nb(psa_test_count ~ symptom_present + offset(log(follow_up_years)), data = nb_model_full_data)
summary(uni_nb_symptom)




##TESTING rates SYMTPOMS 
psa_symptoms_merged <- samp_psa_tests %>%
  left_join(symptoms, by = c("patid", "psa_date"))

# Create an indicator for the presence of symptoms within 90 days before PSA test
psa_symptoms_merged <- psa_symptoms_merged %>%
  mutate(symptom_present = ifelse(!is.na(symptom), 1, 0))

# Separate counts for tests above and below threshold
sym_yes <- psa_symptoms_merged[symptom_present == 1, .N, by = .(age_category, psa_year)]
sym_no <- psa_symptoms_merged[symptom_present == 0, .N, by = .(age_category, psa_year)]

names(sym_yes) <- c("age_range", "year", "symptoms_yes")
names(sym_no) <- c("age_range", "year", "symptoms_no")

# Merge counts with person-years
sym_yes <- merge(expand_age, sym_yes, by = c("age_range", "year"), all.x = TRUE)
sym_no <- merge(expand_age, sym_no, by = c("age_range", "year"), all.x = TRUE)

# Replace NA in Count with 0
sym_yes[is.na(sym_yes$sym_yes), "sym_yes"] <- 0
sym_no[is.na(sym_no1$sym_no), "sym_no"] <- 0

# Merge standard population
all_yes <- merge(sym_yes1, stdpop_2018, by = "age_range", all.x = TRUE)
all_no <- merge(sym_no1, stdpop_2018, by = "age_range", all.x = TRUE)

age_adjusted_rate_yes <- all_yes %>%
  group_by(year) %>%
  summarise(age_adjust = list(ageadjust.direct(count = symptoms_yes,   
                                               pop = TotalPyrs,          
                                               rate = NULL,                
                                               stdpop = stdpop,            
                                               conf.level = 0.95))) %>%
  mutate(age_adjust = map(age_adjust, as.data.frame.list)) %>%
  unnest(cols = c(age_adjust))

age_adjusted_rate_no <- all_no %>%
  group_by(year) %>%
  summarise(age_adjust = list(ageadjust.direct(count = symptoms_no,   
                                               pop = TotalPyrs,          
                                               rate = NULL,                
                                               stdpop = stdpop,            
                                               conf.level = 0.95))) %>%
  mutate(age_adjust = map(age_adjust, as.data.frame.list)) %>%
  unnest(cols = c(age_adjust))

# Convert rates to per 1000 person-years
age_adjusted_rate_yes <- age_adjusted_rate_yes %>%
  mutate(
    crude.rate = crude.rate * 1000,
    adj.rate = adj.rate * 1000,
    lci = lci * 1000,
    uci = uci * 1000
  )

age_adjusted_rate_no <- age_adjusted_rate_no %>%
  mutate(
    crude.rate = crude.rate * 1000,
    adj.rate = adj.rate * 1000,
    lci = lci * 1000,
    uci = uci * 1000
  )

age_adjusted_rate_yes$symptom <- 'Yes'
age_adjusted_rate_no$symptom <- 'No'

# Combine the data frames
rate_sym <- rbind(age_adjusted_rate_yes, age_adjusted_rate_no)

sym_adj <- ggplot(rate_sym, aes(x = year, y = adj.rate, group = symptom1, color = symptom1)) +
  geom_line() +  # Draw lines for adjusted rates, with colors based on group
  labs(title = "Adjusted PSA Testing Rate over Years by Threshold",
       x = "Year", y = "Adjusted Rate per 1000 Person-Years",
       color = "Threshold") +  # Label for the legend
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1),  # Improve readability of x-axis labels
        legend.position = "bottom")  # Move legend to bottom

# Print the plot
print(sym_adj)


# Summarize data to create indicators for each symptom
symptom_summary <- psa_symptoms_merged %>%
  group_by(patid, psa_date) %>%
  summarize(
    symptom_present = max(symptom_present, na.rm = TRUE)
  ) %>%
  ungroup()


samp_psa_tests <- samp_psa_tests %>%
  mutate(year = year(psa_date))

# Merge with symptom summary to get symptom_present for each test
samp_psa_tests <- samp_psa_tests %>%
  left_join(symptom_summary, by = c("patid", "psa_date"))

psa_test_counts <- samp_psa_tests %>%
  group_by(year, age_category, symptom_present) %>%
  summarize(psa_test_count = n()) %>%
  ungroup()

psa_test_counts <- psa_test_counts %>%
  rename(age_range = age_category)

rate_data <- expand_age %>%
  left_join(psa_test_counts, by = c("year", "age_range")) %>%
  mutate(psa_test_count = replace_na(psa_test_count, 0))


rate_data <- rate_data %>%
  group_by(symptom_present, year) %>%
  summarize(
    total_pyrs = sum(TotalPyrs),
    psa_test_count = sum(psa_test_count)
  ) %>%
  mutate(rate_per_pyr = psa_test_count / total_pyrs * 1000)

ge_adjusted_rate_symptoms <- rate_data %>%
  group_by(symptom_present, year) %>%
  summarize(
    total_pyrs = sum(total_pyrs),
    psa_test_count = sum(psa_test_count)
  ) %>%
  ungroup() %>%
  mutate(age_adjust = list(ageadjust.direct(
    count = psa_test_count,
    pop = total_pyrs,
    rate = NULL,
    stdpop = stdpop_2018$stdpop,
    conf.level = 0.95
  ))) %>%
  mutate(age_adjust = map(age_adjust, as.data.frame.list)) %>%
  unnest(cols = c(age_adjust)) %>%
  mutate(
    crude.rate = crude.rate * 1000,
    adj.rate = adj.rate * 1000,
    lci = lci * 1000,
    uci = uci * 1000
  )

symptom_plot <- ggplot(ge_adjusted_rate_symptoms, aes(x = year, y = adj.rate, color = as.factor(symptom_present))) +
  geom_line() +
  labs(title = "PSA Testing Rates Over Time by Symptom Presence",
       x = "Year", y = "Adjusted Rate per 1000 Person-Years",
       color = "Symptom Present") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1),
        legend.position = "bottom")

print(symptom_plot)















