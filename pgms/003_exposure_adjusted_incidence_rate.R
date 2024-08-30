
rm(list=ls())
gc()

library(dplyr)
library(magrittr)
library(slider)
library(lulab.utils)


# 1. set the working directory
raw_input_dir= "./data/raw_input/tidy data/"
full_model_data_dir= "./data/CID_full_model_data/"
full_model_results_dir= './output/CID_full_model_results/'

hpv_np= readRDS(paste0(full_model_data_dir, 'dat2.rds')) #not for prevalence
hpv_np %<>% 
  select(c(1,29,28,27,67,30:66))

# mapply(check_cha,names(hpv_np)[1:3])
mapply(check_cha,names(hpv_np)[c(4:10)+2], MoreArgs = list(hpv_np))
mapply(check_cha,names(hpv_np)[c(11:20)+2], MoreArgs = list(hpv_np))
mapply(check_cha,names(hpv_np)[c(21:30)+2], MoreArgs = list(hpv_np))
mapply(check_cha,names(hpv_np)[c(31:40)+2], MoreArgs = list(hpv_np))


hpv_np %<>% 
  rowwise() %>% 
  mutate(
    hr_risk_hpv= ifelse((HPV16==1 | HPV18==1 | HPV31==1 | HPV33==1 | HPV35==1 | HPV39==1 | HPV45==1 | HPV51==1 | HPV52==1 | HPV56==1 | HPV58==1 | HPV59==1 | HPV68==1),1,0) %>% 
      ifelse((HPV16%>% is.na & HPV18%>% is.na & HPV31%>% is.na & HPV33%>% is.na & HPV35%>% is.na & HPV39%>% is.na & HPV45%>% is.na & HPV51%>% is.na & HPV52%>% is.na & HPV56%>% is.na & HPV58%>% is.na & HPV59%>% is.na & HPV68%>% is.na),NA,.),
    lr_risk_hpv= ifelse((HPV6==1 | HPV11==1 | HPV26==1 | HPV34==1 | HPV40==1 | HPV42==1 | HPV43==1 | HPV44==1 | HPV53==1 | HPV54==1 | HPV55==1 | HPV57==1 | HPV61==1 | HPV66==1 | HPV67==1 | HPV69==1 | HPV70==1 | HPV71==1 | HPV72==1 | HPV73==1 | HPV81==1 | HPV82==1 | HPV83==1 | HPV84==1),1,0) %>% 
      ifelse((HPV6%>% is.na & HPV11%>% is.na & HPV26%>% is.na & HPV34%>% is.na & HPV40%>% is.na & HPV42%>% is.na & HPV43%>% is.na & HPV44%>% is.na & HPV53%>% is.na & HPV54%>% is.na & HPV55%>% is.na & HPV57%>% is.na & HPV61%>% is.na & HPV66%>% is.na & HPV67%>% is.na & HPV69%>% is.na & HPV70%>% is.na & HPV71%>% is.na & HPV72%>% is.na & HPV73%>% is.na & HPV81%>% is.na & HPV82%>% is.na & HPV83%>% is.na & HPV84%>% is.na),NA,.),
    HPV_9v= ifelse((HPV16==1 | HPV18==1 | HPV31==1 | HPV33==1 | HPV45==1 | HPV52==1 | HPV58==1 | HPV6==1 | HPV11==1),1,0) %>% 
      ifelse((HPV16%>% is.na & HPV18%>% is.na & HPV31%>% is.na & HPV33%>% is.na & HPV45%>% is.na & HPV52%>% is.na & HPV58%>% is.na & HPV6%>% is.na & HPV11%>% is.na),NA,.),
    HPV_4v= ifelse((HPV6==1 | HPV11==1 | HPV16==1 | HPV18==1),1,0) %>% 
      ifelse((HPV6%>% is.na & HPV11%>% is.na & HPV16%>% is.na & HPV18%>% is.na),NA,.),
    HPV_16_18= ifelse((HPV16==1 | HPV18==1),1,0) %>% 
      ifelse((HPV16%>% is.na & HPV18%>% is.na),NA,.),
    HPV_6_11= ifelse((HPV6==1 | HPV11==1),1,0) %>% 
      ifelse((HPV6%>% is.na & HPV11%>% is.na),NA,.),
    HPV_any= ifelse((hr_risk_hpv==1 | lr_risk_hpv==1),1,0) %>% 
      ifelse((hr_risk_hpv%>% is.na & lr_risk_hpv%>% is.na),NA,.)
  )


ad_infec= hpv_np %>% select(bh) %>% distinct()
ad_infec %<>% 
  mutate(
    HPV_any= NA,
    hr_risk_hpv= NA,
    lr_risk_hpv= NA,
    HPV_9v= NA,
    HPV_4v= NA,
    HPV_16_18= NA,
    HPV_6_11= NA,
    # high-risk accoding to the JMV article
    HPV16= NA,
    HPV18= NA,
    HPV31= NA,
    HPV33= NA,
    HPV35= NA,
    HPV39= NA,
    HPV45= NA,
    HPV51= NA,
    HPV52= NA,
    HPV56= NA,
    HPV58= NA,
    HPV59= NA,
    HPV68= NA,
    # low-risk accoding to the JMV article
    HPV6= NA,
    HPV11= NA,
    HPV26= NA,
    HPV34= NA,
    HPV40= NA,
    HPV42= NA,
    HPV43= NA,
    HPV44= NA,
    HPV53= NA,
    HPV54= NA,
    HPV55= NA,
    HPV57= NA,
    HPV61= NA,
    HPV66= NA,
    HPV67= NA,
    HPV69= NA,
    HPV70= NA,
    HPV71= NA,
    HPV72= NA,
    HPV73= NA,
    HPV81= NA,
    HPV82= NA,
    HPV83= NA,
    HPV84= NA,
    # other HPV types
    # HPV_9v= NA,
    # HPV_4v= NA,
    # HPV_2v= NA,
    inc_fl= NA_character_,
    clear_fl= NA_character_,
    ongo_fl= NA_character_,
    duration_fl= NA_character_
  )
hpv_genotypes= names(ad_infec)[9:45]
ad_infec %<>% 
  tidyr::pivot_longer(
    cols= -c(bh,inc_fl,clear_fl,ongo_fl,duration_fl),
    names_to= 'genotype'
  ) %>% 
  select(-value) %>% 
  tidyr::pivot_longer(
    cols= -c(bh,genotype),
    names_to= 'paramcd',
    values_to= 'avalc'
  )
ad_infec %>% dim() #250800
1425*44*4 #250800



pattern_sli= function(data){
  if(length(data)==1){
    before= data %>% paste0('-',.)
  }else{
    before= data %>% paste0(.,collapse = '-')
  }
  return(before)
}

EAIR= function(data, hpv){
  hpv_arg= rlang::sym(hpv)
  data %>% 
  group_by(bh) %>% 
  arrange(class) %>% 
  select(bh,class,!!hpv_arg,Sample_T) %>% 
  filter(!is.na(!!hpv_arg) & !is.na(Sample_T)) %>%
  mutate(
    class= row_number()-1,
    class_fl= max(class),
  ) %>% 
  mutate(
    pattern= if_else(class_fl>=1,
                     slide_index_chr(!!hpv_arg, class, ~pattern_sli(.x),.before= 1,.complete = FALSE),
                     'not_applicable'
                     ),
    inc= case_when(
      pattern== 'not_applicable' ~ 'not_applicable',
      pattern== '-0' ~ 'not_infected_at_baseline',
      pattern== '-1' ~ 'infected_at_baseline',
      pattern== '0-1' ~ 'Y',
      .default = 'N'
    ),
    clear= case_when(
      pattern== 'not_applicable' ~ 'not_applicable',
      pattern== '1-0' ~ 'Y',
      .default = 'N'
    ),
    ongo= case_when(
      pattern== 'not_applicable' ~ 'not_applicable',
      pattern== '1-1' ~ 'Y',
      .default = 'N'
    ),
    duration= 
      if(all(class_fl<1)){
        'not_applicable'
      }else{
        ((Sample_T-Sample_T[1]+1)/365.25) %>% as.character()
      }
  ) %>% 
  select(bh,!!hpv_arg,inc,clear,ongo,duration,class_fl) %>% 
  mutate(
    # individuals are required to be infected within our data, which means the inc_fl must occurred in our cohort
    # individuals who infected before baseline are not included in the time at risk, otherwise he/she has the new infection in our cohort
    inc_fl= 
      if(all(inc== 'not_applicable')){
        # only one row:
        'not_applicable'
      }else if(inc[1]== 'not_infected_at_baseline' & all(inc[-1]== 'N')){
        'n_N'
      }else if(inc[1]== 'not_infected_at_baseline' & any(inc[-1]== 'Y')){
        'n_Y'
      }else if(all(class_fl==1) & inc[1]== 'infected_at_baseline'){
        # only two rows:
        'not_applicable'
      }else if(all(class_fl>1) & inc[1]== 'infected_at_baseline' & clear[2]=='Y' & all(inc[-1]== 'N')){
        # only three rows:
        'i_0_N'
      }else if(all(class_fl>1) & inc[1]== 'infected_at_baseline' & clear[2]=='Y' & any(inc[-1]== 'Y')){
        'i_0_Y'
      }else if(all(class_fl>1) & inc[1]== 'infected_at_baseline' & ongo[2]== 'Y' & all(clear=='N')){
        'not_applicable'
      }else if(all(class_fl>1) & inc[1]== 'infected_at_baseline' & ongo[2]== 'Y' & sum(clear=='Y')==1 & clear[class_fl[1]+1]=='Y'){
        'not_applicable'
      }else if(all(class_fl>1) & inc[1]== 'infected_at_baseline' & ongo[2]== 'Y' & sum(clear=='Y')>1){
        'i_1_0_Y'
      }else if(all(class_fl>1) & inc[1]== 'infected_at_baseline' & ongo[2]== 'Y' & sum(clear=='Y')>=1 & clear[class_fl[1]+1]=='N' & all(inc[-1]== 'N')){
        'i_1_0_N'
      }else if(all(class_fl>1) & inc[1]== 'infected_at_baseline' & ongo[2]== 'Y' & sum(clear=='Y')>=1 & clear[class_fl[1]+1]=='N' & any(inc[-1]== 'Y')){
        'i_1_0_Y'
      }else{
        'not_considered'
      }
    ,
    inc_dur_fl= 
      if(all(inc_fl== 'not_applicable')){
        'not_applicable'
      }else if(all(inc_fl== 'n_N')){
        duration[(class_fl[1]+1)]
      }else if(all(inc_fl== 'n_Y')){
        duration[which(inc== 'Y')[1]]
      }else if(all(inc_fl== 'i_0_N') | all(inc_fl== 'i_1_0_N')){
        as.character(as.numeric(duration[(class_fl[1]+1)])- as.numeric(duration[which(clear== 'Y')[1]]) + 1/365.25)
      }else if(all(inc_fl== 'i_0_Y') | all(inc_fl== 'i_1_0_Y')){
        as.character(as.numeric(duration[which(inc== 'Y')[1]])- as.numeric(duration[which(clear== 'Y')[1]]) + 1/365.25)
      }else {'not_considered'}
    ,
    # individuals are not required to be infected within our data, which means the clear_fl just occurred in our cohort whenever the infection occurred
    clear_fl= 
      if(all(clear== 'not_applicable')){
        'not_applicable'
      }else if(inc[1]== 'not_infected_at_baseline' & all(inc[-1]== 'N')){
        'not_applicable'
      }else if(all(class_fl>=1) & inc[1]== 'not_infected_at_baseline' & sum(inc[-1]== 'Y')==1 & inc[class_fl[1]+1]== 'Y'){
        'not_applicable'
      }else if(all(class_fl>=1) & inc[1]== 'not_infected_at_baseline' & sum(inc[-1]== 'Y')>1 & inc[class_fl[1]+1]== 'Y'){
        'n_1_Y'
      }else if(all(class_fl>=1) & inc[1]== 'not_infected_at_baseline' & sum(inc[-1]== 'Y')>=1 & inc[class_fl[1]+1]== 'N' & all(clear== 'N')){
        'n_1_N'
      }else if(all(class_fl>=1) & inc[1]== 'not_infected_at_baseline' & sum(inc[-1]== 'Y')>=1 & inc[class_fl[1]+1]== 'N' & any(clear== 'Y')){
        'n_1_Y'
      }else if(all(class_fl==1) & inc[1]== 'infected_at_baseline' & all(clear== 'N')){
        'i_N'
      }else if(all(class_fl==1) & inc[1]== 'infected_at_baseline' & any(clear== 'Y')){
        'i_Y'
      }else if(all(class_fl>1) & inc[1]== 'infected_at_baseline' & clear[2]=='Y' & all(inc[-1]== 'N')){
        'i_Y'
      }else if(all(class_fl>1) & inc[1]== 'infected_at_baseline' & clear[2]=='Y' & any(inc[-1]== 'Y')){
        'i_Y'
      }else if(all(class_fl>1) & inc[1]== 'infected_at_baseline' & ongo[2]== 'Y' & all(clear=='N')){
        'i_N'
      }else if(all(class_fl>1) & inc[1]== 'infected_at_baseline' & ongo[2]== 'Y' & sum(clear=='Y')==1 & clear[class_fl[1]+1]=='Y'){
        'i_Y'
      }else if(all(class_fl>1) & inc[1]== 'infected_at_baseline' & ongo[2]== 'Y' & sum(clear=='Y')>1){
        'i_Y'
      }else if(all(class_fl>1) & inc[1]== 'infected_at_baseline' & ongo[2]== 'Y' & sum(clear=='Y')>=1 & clear[class_fl[1]+1]=='N' & all(inc[-1]== 'N')){
        'i_Y'
      }else if(all(class_fl>1) & inc[1]== 'infected_at_baseline' & ongo[2]== 'Y' & sum(clear=='Y')>=1 & clear[class_fl[1]+1]=='N' & any(inc[-1]== 'Y')){
        'i_Y'
      }else {
        'not_considered'
      }
    ,
    clear_dur_fl= 
      if(all(clear_fl== 'not_applicable')){
        'not_applicable'
      }else if(all(clear_fl== 'n_1_N')){
         as.character(as.numeric(duration[(class_fl[1]+1)])- as.numeric(duration[which(inc== 'Y')[1]]) + 1/365.25)
      }else if(all(clear_fl== 'n_1_Y')){
         as.character(as.numeric(duration[which(clear== 'Y')[1]])- as.numeric(duration[which(inc== 'Y')[1]]) + 1/365.25)
      }else if(all(clear_fl== 'i_N')){
         duration[(class_fl[1]+1)]
      }else if(all(clear_fl== 'i_Y')){
         duration[which(clear== 'Y')[1]]
      }else{
        'not_considered'
      }
    ,
    # individuals are not required to be infected within our data, which means the ongo_fl just occurred in our cohort whenever the infection occurred
    ongo_fl= 
      if(all(ongo== 'not_applicable')) {
        'not_applicable'
      }else if(inc[1]== 'not_infected_at_baseline' & all(inc[-1]== 'N')){
        'not_applicable'
      }else if(all(class_fl>=1) & inc[1]== 'not_infected_at_baseline' & sum(inc[-1]== 'Y')==1 & inc[class_fl[1]+1]== 'Y'){
        'not_applicable'
      }else if(all(class_fl>1) & inc[1]== 'not_infected_at_baseline' & any(inc[-1]== 'Y') & any(ongo== 'Y')){
        'n_Y'
      }else if(all(class_fl>1) & inc[1]== 'not_infected_at_baseline' & any(inc[-1]== 'Y') & all(ongo== 'N')){
        'n_N'
      }else if(inc[1]== 'infected_at_baseline' & all(ongo== 'N')){
        'i_N'
      }else if(inc[1]== 'infected_at_baseline' & any(ongo== 'Y')){
        'i_Y'
      }else{
        'not_considered'
      }
    ,
    ongo_dur_fl= 
      if(all(ongo_fl== 'not_applicable')){
        'not_applicable'
      }else if(all(ongo_fl== 'n_Y')) {
         as.character(as.numeric(duration[which(ongo== 'Y')[1]])- as.numeric(rev(duration[2:(which(ongo== 'Y')[1])])[which(rev(inc[2:(which(ongo== 'Y')[1])])== 'Y')[1]]) + 1/365.25)
      }else if(all(ongo_fl== 'i_Y')) {
         duration[which(ongo== 'Y')[1]]
      }else if(all(ongo_fl== 'n_N')){
         as.character(as.numeric(duration[which(clear== 'Y')[1]])- as.numeric(duration[which(inc== 'Y')[1]]) + 1/365.25)
      }else if(all(ongo_fl== 'i_N')){
         duration[which(clear== 'Y')[1]]
      }else{
        'not_considered'
      }
    ,
    ongo_inf_fl= 
      if(all(ongo_fl== 'not_applicable')){
        'not_applicable'
      }else if(all(ongo_fl== 'n_N')){
        'not_applicable'
      }else if(all(ongo_fl== 'i_N')){
        'not_applicable'
      }else if(all(ongo_fl== 'n_Y') & sum(ongo== 'Y')==1){
        as.character(as.numeric(duration[which(ongo== 'Y')[1]])- as.numeric(rev(duration[2:(which(ongo== 'Y')[1])])[which(rev(inc[2:(which(ongo== 'Y')[1])])== 'Y')[1]]) + 1/365.25)
      }else if(all(ongo_fl== 'n_Y') & sum(ongo== 'Y')>1 & any(ongo[which(ongo=='Y')[1]:(class_fl[1]+1)]=='N')){
        as.character(as.numeric(duration[which(ongo=='Y')[1]:(class_fl[1]+1)][which(ongo[which(ongo=='Y')[1]:(class_fl[1]+1)]=='N')[1]-1])- as.numeric(rev(duration[2:(which(ongo== 'Y')[1])])[which(rev(inc[2:(which(ongo== 'Y')[1])])== 'Y')[1]]) + 1/365.25)
      }else if(all(ongo_fl== 'n_Y') & sum(ongo== 'Y')>1 & all(ongo[which(ongo=='Y')[1]:(class_fl[1]+1)]=='Y')){
        as.character(as.numeric(duration[(class_fl[1]+1)])- as.numeric(rev(duration[2:(which(ongo== 'Y')[1])])[which(rev(inc[2:(which(ongo== 'Y')[1])])== 'Y')[1]]) + 1/365.25)
      }else if(all(ongo_fl== 'i_Y') & all(inc[2:(which(ongo== 'Y')[1])]== 'N') & all(ongo[which(ongo=='Y')[1]:(class_fl[1]+1)]=='Y')){
        duration[(class_fl[1]+1)]
      }else if(all(ongo_fl== 'i_Y') & all(inc[2:(which(ongo== 'Y')[1])]== 'N') & any(ongo[which(ongo=='Y')[1]:(class_fl[1]+1)]=='N')){
        duration[which(ongo=='Y')[1]:(class_fl[1]+1)][which(ongo[which(ongo=='Y')[1]:(class_fl[1]+1)]=='N')[1]-1]
      }else if(all(ongo_fl== 'i_Y') & any(inc[2:(which(ongo== 'Y')[1])]== 'Y') & all(ongo[which(ongo=='Y')[1]:(class_fl[1]+1)]=='Y')){
        as.character(as.numeric(duration[(class_fl[1]+1)])- as.numeric(rev(duration[2:(which(ongo== 'Y')[1])])[which(rev(inc[2:(which(ongo== 'Y')[1])])== 'Y')[1]]) + 1/365.25)
      }else if(all(ongo_fl== 'i_Y') & any(inc[2:(which(ongo== 'Y')[1])]== 'Y') & any(ongo[which(ongo=='Y')[1]:(class_fl[1]+1)]=='N')){
        as.character(as.numeric(duration[which(ongo=='Y')[1]:(class_fl[1]+1)][which(ongo[which(ongo=='Y')[1]:(class_fl[1]+1)]=='N')[1]-1])- as.numeric(rev(duration[2:(which(ongo== 'Y')[1])])[which(rev(inc[2:(which(ongo== 'Y')[1])])== 'Y')[1]]) + 1/365.25)
      }else {
        'not_considered'
      }
  ) %>%
  select(bh,!!hpv_arg,inc_fl,inc_dur_fl,clear_fl,clear_dur_fl,ongo_fl,ongo_dur_fl,ongo_inf_fl) %>% 
  filter(row_number()==1) %>% 
  tidyr::pivot_longer(
    cols= -c(bh,inc_fl,inc_dur_fl,clear_fl,clear_dur_fl,ongo_fl,ongo_dur_fl,ongo_inf_fl),
    names_to= 'genotype'
  ) %>%
  select(-value) %>% 
  tidyr::pivot_longer(
    cols= -c(bh,genotype),
    names_to= 'paramcd',
    values_to= 'avalc'
  ) %>% 
  ungroup(bh)
}

not_for_prevalence= hpv_np %>% 
  group_by(bh) %>%
  arrange(bh,class) %>%
  mutate(
    n= n(),
  ) %>% 
  filter(n>1) %>% 
  ungroup()
  # nrow
  # .$bh %>% unique() %>% length() # 965

hiv_info= not_for_prevalence %>% 
  select(bh,HIV_x) %>% 
  distinct()
outcome_data= purrr::map_dfr(hpv_genotypes, ~EAIR(not_for_prevalence, .x))
outcome_data %>% nrow() #275576 rows, 249935
not_for_prevalence %>% select(bh) %>% distinct() %>% nrow() #1064 individuals, 965
# 1064 * 37 * 7 #275576 rows
965*37*7 # 249935
outcome_data %>% head
outcome_data$avalc %>% stringr::str_detect('not_considered') %>% sum() # 0

  

#figure 1
aa= hpv_np
fig1_dat= aa %>% 
  select(-ID) %>% 
  dplyr::mutate(
    id= bh,
  ) %>% 
  select(-all_of(c('bh',"Sample_T")))

fig1_dat1= fig1_dat %>% 
  filter(class==0) %>% 
  select(c(47,2,3:46)) %>% 
  tidyr::pivot_longer(
    cols= c(3:46),
    names_to = 'HPV_type',
    values_to = 'value'
  ) 

fig1_1= fig1_dat1 %>% 
  filter(HIV_x=='0') %>% 
  group_by(HPV_type) %>% 
  summarise(
    NA_N= sum(is.na(value)),
    pos= sum(value, na.rm = TRUE),
    N= n()-NA_N,
    prevalence= pos/N
  ) %>%
  dplyr::mutate(
    LCL= prevalence-qnorm(.975)*sqrt(prevalence*(1-prevalence)/N),
    UCL= prevalence+qnorm(.975)*sqrt(prevalence*(1-prevalence)/N),
    prevalence= prevalence*100 %>% round(.,digits = 2),
    LCL= LCL*100 %>% round(.,digits = 2),
    UCL= UCL*100 %>% round(.,digits = 2)
  ) %>% 
  dplyr::arrange(desc(prevalence)) %>% ungroup()
fig1_1

fig1_2= fig1_dat1 %>%
  filter(HIV_x=='1') %>%
  group_by(HPV_type) %>% 
  summarise(
    NA_N= sum(is.na(value)),
    pos= sum(value,na.rm = TRUE),
    N= n()- NA_N,
    prevalence= pos/N
  ) %>% 
  dplyr::mutate(
    LCL= prevalence-qnorm(.975)*sqrt(prevalence*(1-prevalence)/N),
    UCL= prevalence+qnorm(.975)*sqrt(prevalence*(1-prevalence)/N),
    prevalence= prevalence*100 %>% round(.,digits = 2),
    LCL= LCL*100 %>% round(.,digits = 2),
    UCL= UCL*100 %>% round(.,digits = 2)
  ) %>% 
  dplyr::arrange(desc(prevalence)) %>% ungroup()
fig1_2

fig1_1$Subpopulation= 'HIV- (N= 1,294)'
fig1_2$Subpopulation= 'HIV+ (N= 131)'
fig1_2$prevalence= -fig1_2$prevalence
fig1_2$LCL= dplyr::if_else(fig1_2$LCL<0, 0, fig1_2$LCL)
fig1_2$LCL= -fig1_2$LCL
fig1_2$UCL= -fig1_2$UCL
fig1_2$UCL= if_else(fig1_2$UCL==0, -0.001, fig1_2$UCL)
fig1_1$LCL= dplyr::if_else(fig1_1$LCL<0, 0, fig1_1$LCL)
fig1_1$UCL= if_else(fig1_1$UCL==0, 0.001, fig1_1$UCL)

fig1_2 %<>% 
  mutate(
    HPV_type= dplyr::case_when(
      HPV_type== 'HPV_any' ~ 'Any HPV',
      HPV_type== 'hr_risk_hpv' ~ 'HR-HPV',
      HPV_type== 'lr_risk_hpv' ~ 'LR-HPV',
      HPV_type== 'HPV_9v' ~ '9v-HPV',
      HPV_type== 'HPV_4v' ~ '4v-HPV',
      HPV_type== 'HPV_16_18' ~ 'HPV 16/18',
      HPV_type== 'HPV_6_11' ~ 'HPV 6/11',
      .default= HPV_type
    )
  )
fig1_1 %<>% 
  mutate(
    HPV_type= dplyr::case_when(
      HPV_type== 'HPV_any' ~ 'Any HPV',
      HPV_type== 'hr_risk_hpv' ~ 'HR-HPV',
      HPV_type== 'lr_risk_hpv' ~ 'LR-HPV',
      HPV_type== 'HPV_9v' ~ '9v-HPV',
      HPV_type== 'HPV_4v' ~ '4v-HPV',
      HPV_type== 'HPV_16_18' ~ 'HPV 16/18',
      HPV_type== 'HPV_6_11' ~ 'HPV 6/11',
      .default= HPV_type
    )
  )
total= fig1_2 %>% dplyr::bind_rows(fig1_1)

total %<>% filter(
  HPV_type %in% c('Any HPV','HR-HPV','LR-HPV','9v-HPV','4v-HPV','HPV 16/18','HPV 6/11')
) %>% 
  mutate(HPV_type= factor(HPV_type, levels= c('Any HPV','HR-HPV','LR-HPV','9v-HPV','4v-HPV','HPV 16/18','HPV 6/11')))
total$Subpopulation %<>% factor(., levels = c('HIV+ (N= 131)','HIV- (N= 1,294)'))
total$HPV_type %<>% factor(., levels= rev(c('Any HPV','HR-HPV','LR-HPV','9v-HPV','4v-HPV','HPV 16/18','HPV 6/11')))

library(ggplot2)
library(ggsci)

col= pal_jama("default")(7)
col_light= pal_jama("default", alpha = 0.6)(7)

total %<>% 
  mutate(
    HPV_colors= dplyr::case_when(
      Subpopulation== 'HIV+ (N= 131)' & HPV_type== 'Any HPV' ~ col[1],
      Subpopulation== 'HIV+ (N= 131)' & HPV_type== 'HR-HPV' ~ col[2],
      Subpopulation== 'HIV+ (N= 131)' & HPV_type== 'LR-HPV' ~ col[3],
      Subpopulation== 'HIV+ (N= 131)' & HPV_type== '9v-HPV' ~ col[4],
      Subpopulation== 'HIV+ (N= 131)' & HPV_type== '4v-HPV' ~ col[5],
      Subpopulation== 'HIV+ (N= 131)' & HPV_type== 'HPV 16/18' ~ col[6],
      Subpopulation== 'HIV+ (N= 131)' & HPV_type== 'HPV 6/11' ~ col[7],
      Subpopulation== 'HIV- (N= 1,294)' & HPV_type== 'Any HPV' ~ col_light[1],
      Subpopulation== 'HIV- (N= 1,294)' & HPV_type== 'HR-HPV' ~ col_light[2],
      Subpopulation== 'HIV- (N= 1,294)' & HPV_type== 'LR-HPV' ~ col_light[3],
      Subpopulation== 'HIV- (N= 1,294)' & HPV_type== '9v-HPV' ~ col_light[4],
      Subpopulation== 'HIV- (N= 1,294)' & HPV_type== '4v-HPV' ~ col_light[5],
      Subpopulation== 'HIV- (N= 1,294)' & HPV_type== 'HPV 16/18' ~ col_light[6],
      Subpopulation== 'HIV- (N= 1,294)' & HPV_type== 'HPV 6/11' ~ col_light[7]
    )
  )

p= total %>% 
  ggplot(aes(x = HPV_type, y = prevalence, fill = HPV_colors))+
  geom_bar(stat = "identity", position = "identity",width = .15)+
  geom_errorbar(aes(ymin = LCL, ymax = UCL), width = 0.07, color = "#585858") + 
  geom_text(aes(label = if_else(abs(prevalence)>0.01, sprintf('%.2f%%', abs(prevalence)), '<0.01%'), y= UCL+UCL/abs(UCL)*3), color = "black") +  
  geom_hline(yintercept = 0, color = "#3f3d3d") + 
  annotate("text", x = 1.25, y = 30, label = "HIV- (N= 1,294)", hjust = -0.1, vjust = -1,
    size= 7,
     color = "black") +  
  annotate("text", x = 1.25, y = -60, label = "HIV+ (N= 131)", 
    hjust = 0.1, vjust = -1, size=7,
    color = "black") + 
  scale_y_continuous(labels = function(x) paste0(abs(x), "%"), limits = c(-74,55),breaks = seq(-65,55,5))+
  coord_flip()+
  theme_classic()+
  theme(
    panel.grid.major.y = element_blank(),
    panel.grid.minor.y = element_blank(),
    panel.grid.minor.x= element_blank(),
    panel.grid.major.x= element_blank(),
    title = element_text(size = 15),
    axis.title.x = element_text(margin = margin(t=15), size = 20),
    axis.title.y = element_text(size = 20),
    axis.text.x = element_text(size = 12),
    axis.text.y = element_text(size = 15),
  )+
  ylab('HPV prevalance')+
  xlab('Genotypes')+
  guides(fill=guide_none())+
  scale_fill_manual(values = c(
    col_light[1],col[1],
    col_light[2],col[2],
    col_light[3],col[3],
    col_light[4],col[4],
    col_light[5],col[5],
    col_light[6],col[6],
    col_light[7],col[7]
  ))
ggsave(file.path(full_model_results_dir, 'Fig1.pdf'),
       p, width = 15, height = 10, units = "in", dpi = 800)


# for table2
base_dat= readRDS(paste0(full_model_data_dir, 'dat2.rds'))
base_dat %<>% 
  filter(class==0) %>% 
  select(all_of(c('bh','HIV_x','class',"edu","income","commercialsex_yon","substance")))
# openxlsx::write.xlsx(total, file.path(full_model_results_dir, 'Fig1_dat.xlsx'), asTable = TRUE)
# for sas table2
base_dat %<>% rename(id= bh)
#not ajusted
s_fig1_dat1= fig1_dat %>% 
  filter(class==0) %>% 
  select(c(2,3:39)) %>% 
  tidyr::pivot_longer(
    cols= c(2:38),
    names_to = 'HPV_type',
    values_to = 'value'
  )  %>% 
  group_by(HIV_x,HPV_type) %>%
  summarise(
    NA_N= sum(is.na(value)),
    pos= sum(value, na.rm = TRUE),
    N= n()-NA_N,
    neg= N-pos,
  ) %>% 
  select(-all_of(c('NA_N','N'))) %>% 
  tidyr::pivot_longer(
    cols= c(pos,neg),
    names_to = 'outcome',
    values_to = 'count'
  ) %>% ungroup() %>% 
  mutate(id= row_number(),outcome= if_else(outcome== 'pos', '1', '0') %>% as.numeric)
s_fig1_dat1
openxlsx::write.xlsx(s_fig1_dat1, file.path(full_model_results_dir, 'Fig1_dat_sas_not_adj.xlsx'), asTable = TRUE)
#adjusted
s_fig1_dat1_adj= fig1_dat %>% 
  filter(class==0) %>% 
  inner_join(base_dat, by= c('id','HIV_x','class')) %>% 
  select(c(2,3:39,48:51)) %>% 
  tidyr::pivot_longer(
    cols= c(2:38),
    names_to = 'HPV_type',
    values_to = 'value'
  )  %>% 
  group_by(HIV_x,HPV_type, edu, income, commercialsex_yon,substance) %>%
  summarise(
    NA_N= sum(is.na(value)),
    pos= sum(value, na.rm = TRUE),
    N= n()-NA_N,
    neg= N-pos,
  ) %>% 
  select(-all_of(c('NA_N','N'))) %>% 
  tidyr::pivot_longer(
    cols= c(pos,neg),
    names_to = 'outcome',
    values_to = 'count'
  ) %>% ungroup() %>% 
  mutate(id= row_number(),outcome= if_else(outcome== 'pos', '1', '0') %>% as.numeric)
s_fig1_dat1_adj
openxlsx::write.xlsx(s_fig1_dat1_adj, file.path(full_model_results_dir, 'Fig1_dat_sas_adj.xlsx'), asTable = TRUE)
save(list= ls(), file= file.path(full_model_results_dir, '003_exposure_adjusted_incidence_rate_20240822.RData'))

# run 004.sas


# 20240826
# load(file.path(full_model_results_dir, '003_exposure_adjusted_incidence_rate_20240822.RData'))
# check for sas table 3
table3_dat_1= outcome_data %>% 
  filter(paramcd %in% c('inc_dur_fl','clear_dur_fl','ongo_dur_fl','ongo_inf_fl'))
table3_dat_2= outcome_data %>% 
  filter(!(paramcd %in% c('inc_dur_fl','clear_dur_fl','ongo_dur_fl','ongo_inf_fl')))
table3_dat_2 %>% 
  # filter(paramcd=='ongo_inf_fl') %>%
  .$avalc %>% table() %>% names()
table3_dat_3= table3_dat_2 %>% 
  mutate(
    outcome= case_when(
      avalc %in% c(
        "i_0_N","i_1_0_N","n_N","i_N","n_1_N"
      )~ '0',
      avalc %in% c(
        "i_0_Y","i_1_0_Y","n_Y","i_Y","n_1_Y"
      )~ '1',
      avalc %in% c(
        "not_applicable"
      )~ NA,
      .default= 'not_considered'
    )
)

table3_dat_3$outcome %<>% as.numeric()
table3_dat_3 %<>% select(-avalc)
table3_dat_3 %>% dim #107115

table3_dat_1 %<>% 
  mutate(avalc= if_else(avalc== 'not_applicable', NA, avalc) %>% as.numeric %>% `*`(12)) %>%
  tidyr::pivot_wider(
    names_from= paramcd,
    values_from= avalc
  )
table3_dat_1_part1= table3_dat_1 %>% select(-c(ongo_inf_fl))
table3_dat_1_part1 %>% dim #35705
35705*3 #107115
table3_dat_1_part1 %<>% 
  tidyr::pivot_longer(
    cols= c(inc_dur_fl,clear_dur_fl,ongo_dur_fl),
    names_to= 'paramcd',
    values_to= 'pys'
  )
table3_dat_3 %>% dim #107115
table3_dat_1_part1 %>% dim #107115

table3_dat_1_part1 %<>% 
  mutate(
    paramcd= case_when(
      paramcd== 'inc_dur_fl' ~ 'inc_fl',
      paramcd== 'clear_dur_fl' ~ 'clear_fl',
      paramcd== 'ongo_dur_fl' ~ 'ongo_fl',
      .default= 'not_considered'
    )
  )
table3_dat_sas= table3_dat_3 %>% 
  inner_join(hiv_info, by= 'bh') %>% 
  inner_join(table3_dat_1_part1, by= c('bh','genotype','paramcd')) %>% 
  group_by(HIV_x,genotype,paramcd) %>%
  summarise(
    NA_N= sum(is.na(outcome)),
    pos= sum(outcome, na.rm = TRUE),
    N= n()-NA_N,
    neg= N-pos,
    sum_pys= sum(pys, na.rm = TRUE),
  ) %>% 
  select(-all_of(c('NA_N','N'))) %>% 
  tidyr::pivot_longer(
    cols= c(pos,neg),
    names_to = 'outcome',
    values_to = 'count'
  ) %>% ungroup() %>% 
  rename(pys= sum_pys) %>% 
  mutate(id= row_number(),outcome= if_else(outcome== 'pos', '1', '0') %>% as.numeric)
openxlsx::write.xlsx(table3_dat_sas, file.path(full_model_results_dir, 'Table3_dat_sas_not_adj.xlsx'), asTable = TRUE)
save(list= ls(), file= file.path(full_model_results_dir, '20240823_003_exposure_adjusted_incidence_rate.RData'))

# 20240826
base_dat %<>% select(-class)
table3_dat_sas_adj= table3_dat_3 %>% 
  inner_join(hiv_info, by= 'bh') %>% 
  inner_join(table3_dat_1_part1, by= c('bh','genotype','paramcd')) %>% 
  rename(id= bh) %>% 
  inner_join(base_dat, by= c('id','HIV_x')) %>% 
  group_by(HIV_x,genotype,paramcd,edu, income, commercialsex_yon,substance) %>%
  summarise(
    NA_N= sum(is.na(outcome)),
    pos= sum(outcome, na.rm = TRUE),
    N= n()-NA_N,
    neg= N-pos,
    sum_pys= sum(pys, na.rm = TRUE),
  ) %>% 
  select(-all_of(c('NA_N','N'))) %>% 
  tidyr::pivot_longer(
    cols= c(pos,neg),
    names_to = 'outcome',
    values_to = 'count'
  ) %>% ungroup() %>% 
  rename(pys= sum_pys) %>% 
  mutate(id= row_number(),outcome= if_else(outcome== 'pos', '1', '0') %>% as.numeric)
table3_dat_sas_adj %>% head()
openxlsx::write.xlsx(table3_dat_sas_adj, file.path(full_model_results_dir, 'Table3_dat_sas_adj.xlsx'), asTable = TRUE)
