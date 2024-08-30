
rm(list=ls())
gc()

library(dplyr)
library(magrittr)
# install.packages("devtools")
# remove.packages(c('rlang','fastmap','digest','xfun','cli','Rcpp','stringi','backports','openxlsx'))
# install.packages(c('Rcpp'))
# remove.packages(c('lulab.utils'))

# install:
# devtools::install_github("Leslie-Lu/lulab.utils")
# or
install.packages("lulab.utils", repos = c("https://leslie-lu.r-universe.dev", "https://cloud.r-project.org"))
library(lulab.utils)

# 1. set the working directory
raw_input_dir= "./data/raw_input/tidy data/"
full_model_data_dir= "./data/CID_full_model_data/"
full_model_results_dir= './output/CID_full_model_results/'

# 2. load the baseline data
base_que_old= readr::read_csv(paste0(raw_input_dir, 'base_que_240613v2.csv'))
base_que= openxlsx::read.xlsx(paste0(raw_input_dir, 'matched_data_cleaned.xlsx'))

base_que1= base_que %>% 
  select(all_of(
    c('bh',
      'y_birth','huji','ethnic','edu','employment',
      'income','age_first_sex','sex_ori','sex_par',
      'sex_par_1y','anal_sex_6m','Sex_inter',
      'par_nu_6m',
      'condom_yon','condom_freq_6m','commercialsex_yon',
      'commercial_ms_co_yon','commercial_ms_co_freq',
      'sex_women_yon','womensex_co_yon','womensex_co_freq',
      'substance','vct','circumcise','smoking','drunking',"HIV_x","class"
    ) 
  )) %>% 
  rename('ID'= 'bh')

complementary_que= openxlsx::read.xlsx(paste0(raw_input_dir,'医科大HPV队列调查随访问卷_20240601202741.xlsx'),startRow = 2)
complementary_que$huji= NA
complementary_que$ethnic= NA
complementary_que$edu= NA
complementary_que$employment= NA
complementary_que$income= NA
complementary_que$age_first_sex= NA
complementary_que$sex_ori= NA
complementary_que$sex_par= NA
complementary_que$commercialsex_yon= NA
complementary_que$vct= NA
complementary_que$circumcise= NA
complementary_que$smoking= NA
complementary_que$drunking= NA
complementary_que$HIV_x= NA
complementary_que$class= NA
complementary_que1= complementary_que %>% 
  select(all_of(
    c(
      "X2",'X4',
      'huji','ethnic','edu','employment','income','age_first_sex','sex_ori','sex_par',
      'X5','X8','X7','X9','X11','X12',
      'commercialsex_yon',
      'X15','X16','X17','X18','X19','X20',
      'vct','circumcise','smoking','drunking',"HIV_x","class"
    )
  ))
names(complementary_que1)= names(base_que1)


fol_que= readr::read_csv(paste0(raw_input_dir,'fol_que_240613.csv'))
fol_que$huji= NA
fol_que$ethnic= NA
fol_que$edu= NA
fol_que$employment= NA
fol_que$income= NA
fol_que$age_first_sex= NA
fol_que$sex_ori= NA
fol_que$sex_par= NA
fol_que$commercialsex_yon= NA
fol_que$commercial_ms_co_yon= NA
fol_que$commercial_ms_co_freq= NA
fol_que$womensex_co_yon= NA
fol_que$womensex_co_freq= NA
fol_que$vct= NA
fol_que$circumcise= NA
fol_que$smoking= NA
fol_que$drunking= NA
fol_que$HIV_x= NA
fol_que$class= NA
fol_que1= fol_que %>% 
  select(all_of(
    c(
      'bh',
      'y_birth',
      'huji','ethnic','edu','employment',
      'income','age_first_sex','sex_ori','sex_par',
      'sex_par_1y','anal_sex_6m','Sex_inter',
      'par_nu_6m',
      'condom_yon','condom_freq_6m',
      'commercialsex_yon',
      'commercial_ms_co_yon','commercial_ms_co_freq',
      'sex_women_yon',
      'womensex_co_yon','womensex_co_freq',
      'substance',
      'vct','circumcise','smoking','drunking',"HIV_x","class"
    )
  ))
names(fol_que1)= names(base_que1)


base_que1 %<>% apply(2, as.character) %>% as.data.frame()
complementary_que1 %<>% apply(2, as.character) %>% as.data.frame()
fol_que1 %<>% apply(2, as.character) %>% as.data.frame()
base_que1 %<>% bind_rows(complementary_que1) %>% 
  bind_rows(fol_que1)
base_que1 %<>% distinct()

# check for duplicates of ID
base_que1$ID %<>% as.character() 
base_que1= base_que1 %>% 
  filter(!is.na(ID))
base_que1 %>% nrow() #7496 records, 8563
base_que1$ID %>% unique() %>% length() #7505 records, 7245
base_que1$ID %<>% stringr::str_trim(., side= 'both')
# base_que1 %>% 
#   filter(
#     ID %in% c(
#       base_que1 %>% 
#         group_by(ID) %>% 
#         summarise(n= n()) %>%
#         filter(n>1) %>%
#         .$ID
#     )
#   ) %>% 
#   arrange(ID) %>% 
#   # nrow() #2637 records
#   # .$ID %>% unique() %>% length() #1307 individuals, 1314
#   # filter(ID %in% c('31069','31186','41069','51069','61069','101205','101301','141148')) %>%
#   readr::write_csv(paste0(full_model_data_dir,'ID_duplicates.csv'))
#   # readr::write_csv(paste0(full_model_data_dir,'ID_duplicates_20240806.csv'))

# base_que1 %>% 
#   filter(
#     ID %in% c(
#       base_que1 %>% 
#         group_by(ID) %>% 
#         summarise(n= n()) %>%
#         filter(n>1) %>%
#         .$ID
#     )
#   ) %>% 
#   arrange(ID) %>% 
#   # nrow() #2637 records
#   # .$ID %>% unique() %>% length() #1307 individuals, 1314
#   filter(HIV_x %in% c('1','0')) %>%
#   # filter(ID %in% c('31069','31186','41069','51069','61069','101205','101301','141148')) %>%
#   # readr::write_csv(paste0(full_model_data_dir,'ID_duplicates.csv'))
#   readr::write_csv(paste0(full_model_data_dir,'ID_duplicates_20240806.csv'))


TT_ID= readr::read_csv(paste0(raw_input_dir, '改ID20240806.csv'),col_names = TRUE)
TT_ID %<>% apply(2, as.character) %>% as.data.frame()
TT_ID %>% dim() #1307 records, 6
names(TT_ID)= names(base_que1)
TT_ID$ID %>% unique() %>% length() #1307 individuals, 6
base_que1 %<>% 
  filter(
    !(ID %in% c(
      base_que1 %>% 
        group_by(ID) %>% 
        summarise(n= n()) %>%
        filter(n>1) %>%
        .$ID
    ))
  ) %>% 
  bind_rows(TT_ID)
base_que1 %>% nrow() #7505 records, 5939
base_que1$ID %>% unique() %>% length() #7505 individuals, 5939


# 3. remove duplicate individuals in baseline table
hpv= readr::read_csv(paste0(raw_input_dir, 'hpv_240607.csv'))
hpv1= hpv %>% 
  select(
    -all_of(
      c("...1")
    )
  )
hpv1$ID %<>% as.character() %>% stringr::str_trim(., side= 'both')
hpv1$bh %<>% as.character %>% stringr::str_trim(., side= 'both')
# check for duplicate hpv results on the same days
# hpv1 %>% filter(
#   ID %in% (
#     hpv1 %>% 
#       group_by(ID) %>% 
#       summarise(n= n()) %>% 
#       filter(n>1 & !is.na(ID)) %>% 
#       .$ID
#   )
# ) %>% 
#   arrange(ID) %>% 
#   openxlsx::write.xlsx(paste0(full_model_data_dir,'hpv_id_duplicate3.xlsx'))
# replace these records with selected records by TT
hpv2= hpv1 %>% filter(
  !(ID %in% (
    hpv1 %>% 
      group_by(ID) %>% 
      summarise(n= n()) %>% 
      filter(n>1 & !is.na(ID)) %>% 
      .$ID
  ))
)
TT1= openxlsx::read.xlsx(paste0(raw_input_dir,'D_hpv_id_duplicate.xlsx'), sheet= 1)
hpv2 %<>% bind_rows(TT1)
# remove id of na
hpv3= hpv2 %>% 
  filter(!is.na(ID) & !is.na(bh))
hpv3$Sample_T %<>% as.character() %>% stringr::str_trim(., side= 'both')
hpv3$Sample_T %<>% lubridate::mdy()
# use map_lgl to check if all values in a row are NA
na_all_hpv= hpv3[4:40] %>% 
  asplit(., 1) %>%
  purrr::map_lgl(~ all(.x %>% is.na()))
hpv3= hpv3[!na_all_hpv,]
# individuals with only one record of HPV result
bh_one_follow_up= hpv3 %>% 
  group_by(bh) %>% 
  summarise(n= n()) %>% 
  filter(n==1) %>% 
  .$bh
hpv3$class[hpv3$bh %in% bh_one_follow_up]= 0
# check time order of class and sample_t
bh_missing_sample_t= hpv3$bh[hpv3$Sample_T %>% is.na()] %>% unique()
hpv4= hpv3 %>% 
  filter(!(bh %in% bh_one_follow_up)) %>% 
  filter(!(bh %in% bh_missing_sample_t)) %>%
  group_by(bh) %>% 
  arrange(bh,Sample_T) %>%
  mutate(
    # time_ord_sample_t= Sample_T > lag(Sample_T,default = Sample_T[1]),
    # time_ord_class= class > lag(class, default = class[1])
    class= row_number()-1
  ) %>% 
  ungroup()
# hpv3 %>% 
#   filter(!(bh %in% bh_one_follow_up)) %>%
#   filter(bh %in% bh_missing_sample_t) %>% 
#   group_by(bh) %>%
#   arrange(bh,class) %>%
#   mutate(
#     # time_ord_sample_t= Sample_T > lag(Sample_T,default = Sample_T[1]),
#     time_ord_class= class > lag(class, default = class[1]),
#     time_ord_class= if_else(row_number()==1, TRUE, time_ord_class)
#   ) %>% 
#   ungroup() %>%
#   filter(bh %in% c('1227','1898')) %>% 
#   # filter(time_ord_class==FALSE) %>%
#   # select(all_of(c('bh','ID','class','Sample_T'))) %>% 
#   # as.data.frame()
#   openxlsx::write.xlsx(paste0(full_model_data_dir,'class错误但sample时间缺失无法判断.xlsx'))
hpv4_1= hpv3 %>% 
    filter(!(bh %in% bh_one_follow_up)) %>%
    filter(bh %in% bh_missing_sample_t) %>%
    filter(!(bh %in% c('1227','1898')))
TT2= openxlsx::read.xlsx(paste0(raw_input_dir,'TT_class错误但sample时间缺失无法判断.xlsx'), sheet= 1, detectDates = TRUE)
hpv5= hpv4 %>% 
  bind_rows(hpv4_1) %>%
  bind_rows(TT2) %>% 
  bind_rows(hpv3 %>% filter(bh %in% bh_one_follow_up))


# check variables -----

mapply(check_cha,names(hpv5)[1], MoreArgs = list(hpv5))
hpv5[4:40]= sapply(hpv5[4:40],function(x) x= if_else(x>1 & !is.na(x), 1, x)) %>% 
  tibble::as_tibble()
mapply(check_cha,names(hpv5)[4:8], MoreArgs = list(hpv5))
mapply(check_cha,names(hpv5)[9:13], MoreArgs = list(hpv5))
mapply(check_cha,names(hpv5)[14:18], MoreArgs = list(hpv5))
mapply(check_cha,names(hpv5)[19:23], MoreArgs = list(hpv5))
mapply(check_cha,names(hpv5)[24:28], MoreArgs = list(hpv5))
mapply(check_cha,names(hpv5)[29:33], MoreArgs = list(hpv5))
mapply(check_cha,names(hpv5)[34:38], MoreArgs = list(hpv5))
mapply(check_cha,names(hpv5)[39:40], MoreArgs = list(hpv5))
hpv6= hpv5 
hpv6 %>%
  # openxlsx::write.xlsx(paste0(full_model_data_dir,'hpv_clean.xlsx'))
  saveRDS(., file = paste0(full_model_data_dir,'hpv_clean.rds'))
hpv6 %>% 
  filter(!(bh %in% bh_one_follow_up)) %>%
  # openxlsx::write.xlsx(paste0(full_model_data_dir,'hpv_clean_not_for_prevalance.xlsx'))
  saveRDS(., file = paste0(full_model_data_dir,'hpv_clean_not_for_prevalance.rds'))
hpv6 %>%
  filter(class==0) %>%
  # openxlsx::write.xlsx(paste0(full_model_data_dir,'hpv_clean_for_prevalance.xlsx'))
  saveRDS(., file = paste0(full_model_data_dir,'hpv_clean_for_prevalance.rds'))
hpv6 %>% 
  select(all_of(c('bh','ID','class'))) %>% 
  distinct() %>% 
  openxlsx::write.xlsx(paste0(full_model_data_dir,'complete_bh_ID_class.xlsx'))

hpv6$ID %>% unique() %>% length() #5889 individuals, 7023
hpv6$bh %>% unique() %>% length() #1744 individuals, 1461
hpv6$class %>% table() #1744



# 4. inner join with HPV data
complete_bh_ID_class= hpv6 %>% 
  # select(all_of(c('bh','ID','class'))) %>% 
  distinct()
complete_bh_ID_class %>% dim() #7023
base_que1$ID %<>% as.character() 
base_que2= base_que1 %>% 
  filter(!is.na(ID))
base_que2 %>% 
  filter(
    ID %>% 
      stringi::stri_enc_isutf8() %>% `!`()
  ) %>% 
  # nrow()
  readr::write_csv(paste0(full_model_data_dir,'ID含有非utf8字符.csv'))
base_que2$ID[
  base_que2$ID %>% 
    stringi::stri_enc_isutf8() %>% `!`() %>% which()
]= c(
  # '03619',
  rep('del',9))
base_que2 %<>% 
  filter(
    ID!= 'del'
  )
base_que2$ID %<>% stringr::str_trim(., side= 'both')


# # 20240806: update the matching method, no need to add zero to the ID
# # the first matching method: add zero to the ID
# complete_bh_ID_class1= complete_bh_ID_class %>% 
#   # base_hpv_1 %>% 
#   # inner_join(base_hpv_2 %>% mutate(ID= paste0('0',ID)), by= 'ID') %>% 
#   # .$ID
#   # filter(!(ID %in% c('31186', '31295', '41568'))) %>% 
#   mutate(ID= paste0('0',ID))
# base_hpv_1= base_que2 %>% 
#   inner_join(complete_bh_ID_class1, by= 'ID')
# base_hpv_1 %>% dim() #5324 records, 403
# base_hpv_1$ID %>% unique() %>% length() #5324 records
# base_hpv_1$bh %>% unique() %>% length() #1461 individuals
# base_que2 %>% nrow() #7496 records
# hpv6 %>% nrow() #7023 records


base_que2 %<>% select(-class)
# the second matching method: remove zero from the ID
complete_bh_ID_class2= complete_bh_ID_class
base_hpv_2= base_que2 %>% 
  inner_join(complete_bh_ID_class2, by= 'ID')
base_hpv_2 %>% dim() #1358 records, 5920
base_hpv_2$ID %>% unique() %>% length() #1358 records, 5920
base_hpv_2$bh %>% unique() %>% length() #315 individuals, 1427
# 5324+1358 # 6682 records
# check on 20240806:
base_hpv_2 %>% 
  select(bh, HIV_x) %>% 
  distinct() %>% 
  group_by(HIV_x) %>% 
  summarise(n= n()) %>%
  arrange(desc(n))

# base_hpv= base_hpv_1 %>% 
#   bind_rows(base_hpv_2)
base_hpv= base_hpv_2
base_hpv %<>% 
  filter((HIV_x %in% c('1','0')))
  # select(bh, HIV_x) %>% 
  # distinct() %>% 
  # group_by(HIV_x) %>% 
  # summarise(n= n()) %>%
  # arrange(desc(n))
base_hpv %>% nrow() #6682 records, 5889
base_hpv$bh %>% unique() %>% length() #1462 individuals, 1425
base_hpv$ID %>% unique() %>% length() #6682 records, 5889
base_hpv %>% 
  group_by(bh) %>% 
  summarise(n= n()) %>%
  filter(n>1) %>%
  nrow() #1060 individuals, 965
base_hpv %>% 
  group_by(bh) %>% 
  summarise(n= n()) %>%
  .$n %>% table()
base_hpv$class %>% table() #1414

# # unmatched records
# base_que2 %>%
#   anti_join(complete_bh_ID_class1, by= 'ID') %>%
#   anti_join(complete_bh_ID_class2, by= 'ID') %>%
#   apply(2, as.character) %>%
#   as.data.frame() %>% 
#   # nrow() #814 records
#   .$ID %>% unique() %>% length() #814 individuals= 7496-5324-1358
#   # readr::write_csv(paste0(full_model_data_dir,'in_base_not_in_hpv.csv'))
# hpv6 %>% 
#   anti_join(base_que2, by= 'ID') %>% #5665 records= 7023-1358
#   mutate(ID= paste0('0',ID)) %>%
#   anti_join(base_que2, by= 'ID') %>% #341 records= 5665-5324
#   .$bh %>% unique() %>% length() #337 individuals
#   # readr::write_csv(paste0(full_model_data_dir,'in_hpv_not_in_base.csv'))

# to be continued, 20240806
save(list = ls(), file = paste0(full_model_data_dir,'001_data_processing_20240806.RData'))
saveRDS(base_hpv, file = paste0(full_model_data_dir,'base_hpv.rds'))






