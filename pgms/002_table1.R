rm(list=ls())
gc()

library(dplyr)
library(magrittr)
library(lulab.utils)

# 1. set the working directory
raw_input_dir= "./data/raw_input/tidy data/"
full_model_data_dir= "./data/CID_full_model_data/"
full_model_results_dir= './output/CID_full_model_results/'

base_hpv= readRDS(file = paste0(full_model_data_dir,'base_hpv.rds'))
base_hpv %>% dim() #5889


# 2. data cleaning
base_hpv$ID %>% unique() %>% length() #remove id, 5889
base_hpv$bh %>% unique() %>% length() #1425 unique values
base_hpv$class %>% table() #1414

# class
dat1= base_hpv %>% 
  group_by(bh) %>% 
  arrange(bh,class) %>% 
  mutate(class= row_number()-1)
  # .$class %>% table()
  # .$bh %>% unique() %>% length() # 1425
# dat1$Sample_T %>% table() #remove
# dat1$time_ord_class %>% table() #remove

# 3. data processing
dat1$Sample_T %>% is.na() %>% sum() #16
dat1$time_ord_class %>% is.na() %>% sum() #5876
dat1$y_birth %>% is.na() %>% sum() #0

mapply(check_cha,names(dat1)[c(29,69)], MoreArgs = list(dat1))
mapply(check_cha,names(dat1)[c(2)], MoreArgs = list(dat1))

dat2= dat1 %>% 
  mutate(
    y_birth= if_else(y_birth %in% c(''),NA,y_birth %>% as.numeric()),
  ) %>% 
  # .$y_birth %>% table() %>% names()
  rowwise() %>%
  mutate(
    age= 
    ifelse(is.na(y_birth) | is.na(Sample_T),
      NA, 
      lubridate::year(Sample_T)-y_birth)) %>% 
  select(-all_of(c('y_birth',
  # 'Sample_T',
  "time_ord_class")))
  # .$age %>% 
  #   # is.na() %>% 
  #   table() %>% 
  # sum() #29


mapply(check_cha,names(dat2)[c(2:5)], MoreArgs = list(dat1))
dat2 %<>% 
  mutate(
    huji= huji %>% factor(., levels = c("Local resident","Non-local resident")),
    ethnic= ethnic %>% factor(., levels = c("Han","Non-han")),
    edu= ifelse(edu== "Bachelor\xa1\xafs degree and above","Bachelor’s degree and above",edu) %>% 
      factor(.,
        levels = c("High school or below", "Some college", "Bachelor’s degree and above"),
        ordered = TRUE
      ),
    employment= employment %>% factor(., levels = c("Employed","Unemployed"))
  )
dat2 %<>% ungroup()
mapply(check_cha,names(dat2)[c(27:28)], MoreArgs = list(dat1))
dat2 %<>% 
  mutate(
    income= income %>% factor(., levels = c("<=1000","1001~5000","5001~10000",">=100001"),ordered = TRUE),
    age_first_sex= if_else(age_first_sex %in% c(''),NA,age_first_sex) %>% as.numeric,
    sex_ori= sex_ori %>% factor(., levels = c("Gay","Bisexual or other")),
    sex_par= sex_par %>% factor(., levels = c("Men only","Both men and women")),
    sex_par_1y= if_else(sex_par== "Men only", "Men only", sex_par_1y) %>% 
      factor(., levels = c("Men only","Both men and women")),
    anal_sex_6m= anal_sex_6m %>% factor(., levels= c("Yes","No")),
    Sex_inter= Sex_inter %>% factor(., levels= c("Mainly insertive", "Mainly receptive", "Insertive and receptive"),ordered=TRUE),
    par_nu_6m= if_else(par_nu_6m== '', NA, par_nu_6m) %>% as.numeric,
    condom_yon= if_else(condom_yon== '', 'Unknown', condom_yon) %>% 
      if_else(anal_sex_6m== "No", 'No anal_sex_6m', .) %>% 
      factor(., levels= c("Yes","No",'No anal_sex_6m'
        # ,'Unknown'
        ),ordered=TRUE),
    condom_freq_6m= if_else(condom_freq_6m=='','Unknown',condom_freq_6m) %>% 
      if_else(anal_sex_6m== "No", 'No anal_sex_6m', .) %>% 
      if_else(condom_yon== 'No','Never',.) %>% 
      if_else(condom_yon== 'Unknown','Unknown condom_yon',.) %>% 
      factor(., levels=c("Always","Sometimes","Never",'No anal_sex_6m'
        # ,'Unknown','Unknown condom_yon'
        )),
    commercialsex_yon= if_else(commercialsex_yon=='','Unknown',commercialsex_yon) %>% 
      factor(., levels= c("No","Yes",'Unknown'),ordered=TRUE),
    commercial_ms_co_yon= if_else(commercial_ms_co_yon== '', 'Unknown',commercial_ms_co_yon) %>% 
      if_else(commercialsex_yon== 'No','No commercialsex_yon',.) %>% 
      if_else(commercialsex_yon== 'Unknown','Unknown commercialsex_yon',.) %>% 
      factor(., levels= c('No commercialsex_yon','Unknown commercialsex_yon',"Yes","No"
        # ,'Unknown'
        ),ordered=TRUE),
    commercial_ms_co_freq= if_else(commercial_ms_co_freq=='', 'Unknown',commercial_ms_co_freq) %>% 
      if_else(commercialsex_yon== 'No','No commercialsex_yon',.) %>% 
      if_else(commercialsex_yon== 'Unknown','Unknown commercialsex_yon',.) %>% 
      if_else(commercial_ms_co_yon=='No', 'Never', .) %>% 
      if_else(commercial_ms_co_yon=='Unknown', 'Unknown commercial_ms_co_yon', .) %>% 
      factor(., levels= c('No commercialsex_yon','Unknown commercialsex_yon',
        # 'No commercial_ms_co_yon',
        # 'Unknown commercial_ms_co_yon',
        "Always","Sometimes","Never"
        # ,    
        # 'Unknown'
        ),ordered=TRUE),
    sex_women_yon= if_else(sex_par_1y== "Men only", "No", sex_women_yon) %>% 
      factor(., levels= c("No","Yes")),
    womensex_co_yon= if_else(womensex_co_yon== '', 'Unknown',womensex_co_yon) %>% 
      if_else(sex_women_yon== 'No', 'No sex_women_yon',.) %>%
      factor(., levels= c('No sex_women_yon',"Yes","No"
      # ,'Unknown'
      ),ordered=TRUE),
    womensex_co_freq= if_else(womensex_co_freq== '', 'Unknown',womensex_co_freq) %>%
      if_else(sex_women_yon=='No','No sex_women_yon',.) %>% 
      if_else(womensex_co_yon=='No','Never',.) %>%
      if_else(womensex_co_yon=='Unknown','Unknown womensex_co_yon',.) %>%
      factor(., levels=c("Always","Sometimes","Never",
        # 'No womensex_co_yon',
        'No sex_women_yon'
        # ,'Unknown','Unknown womensex_co_yon'
        )),
    substance= substance %>% factor(., levels= c("No","Yes")),
    vct= if_else(vct== '', 'Unknown',vct) %>% factor(., levels= c("Yes","No",'Unknown')),
    circumcise= if_else(circumcise== '', 'Unknown',circumcise) %>% factor(., levels= c("No","Yes",'Unknown')),
    smoking= smoking %>% factor(., levels= c("Never","Sometimes","Everyday")),
    drunking= drunking %>% factor(., levels= c("Never","Sometimes","Everyday")),
    HIV_x= HIV_x %>% factor(., levels= c("0","1")),
    class= class %>% as.numeric
  )

dat2 %>% dim #5889
dat2$ID %>% unique() %>% length() #5889
dat2$bh %>% unique() %>% length() #1425
dat3= dat2 %>% 
  select(c(29,28,27,68,2:26,30:66))
dat3$HIV_x %>% is.na() %>% sum() #0

# dat3 %>% 
#   filter(age< age_first_sex, !is.na(age), !is.na(age_first_sex)) %>% 
#   select(age,age_first_sex)
#   # nrow #32

# 4. table1
table1_dat= dat3 %>% 
  filter(class==0) %>%
  select(c(3:29))
Table1(table1_dat, xcol= names(table1_dat)[2:(ncol(table1_dat))], ycol= names(table1_dat)[1], result_dir= full_model_results_dir)


# 5. clean up hpv data
saveRDS(dat2, file = paste0(full_model_data_dir,'dat2.rds'))
saveRDS(dat3, file = paste0(full_model_data_dir,'dat3.rds'))
save(list = ls(), file = paste0(full_model_data_dir,'002_table1_20240822.RData'))
