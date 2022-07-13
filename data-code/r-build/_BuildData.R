
# Meta --------------------------------------------------------------------

## Author:        Ian McCarthy
## Date Created:  9/14/2020
## Date Edited:   3/16/2021
## Description:   Clean hospital, fips, and combined hospital-level datasets

# Clean data --------------------------------------------------------------

## create data of claims variables only
claims.only <- ch.data %>%
  mutate(Comp_Any=(Comp_Any>0),
         month=month(fst_admtdt),
         hcci_price=ifelse(Type=="Inpatient",wtd_price,calc_allwd)) %>%
  dplyr::select(aha_hnpi, Year, Surgery, hcci_price, gdr, los, tot_mem_cs, Comp_Any,
                any_readmit_30, any_readmit_60, any_readmit_90, z_patid, 
                fst_admtdt, last_dischdt, mdc, z_group_id, drg, mbr_zip_5_cd,
                ends_with("_ccc"), ccc_flag, month, hosp_zip, Type, prod)

## create data of full claims (used to identify highly specialized procedures)
claims.full <- ch.data.full %>%
  mutate(Comp_Any=(Comp_Any>0),
         month=month(fst_admtdt),
         hcci_price=ifelse(Type=="Inpatient",wtd_price,calc_allwd)) %>%
  dplyr::select(aha_hnpi, Year, hcci_price, gdr, los, tot_mem_cs, Comp_Any,
                any_readmit_30, any_readmit_60, any_readmit_90, z_patid, 
                fst_admtdt, last_dischdt, mdc, z_group_id, drg, mbr_zip_5_cd,
                ends_with("_ccc"), starts_with("Surg_"), ccc_flag, month, hosp_zip, Type, prod)


## create data of unique hospital/year observations
hosp.only <- ch.data %>%
  group_by(aha_hnpi,Year) %>%
  dplyr::select(aha_hnpi, Year, bdtot, teaching=teaching_hospital1,
                CH_Tier, fips, nonprofit, system, starts_with("labor_"), urban, hrrcode, 
                starts_with("capital_"), ends_with("_discharges"), hosp_zip) %>%
  dplyr::mutate(hosp_obs_count=row_number())

hosp.only <- hosp.only %>%
  group_by(aha_hnpi, Year) %>%
  arrange(aha_hnpi, Year, bdtot) %>%
  filter(hosp_obs_count==1)

hosp.only <- hosp.only %>% 
  group_by(aha_hnpi) %>%
  mutate_at(c("bdtot","labor_nurse", "labor_phys","labor_residents",
              "labor_other","total_discharges", "mcaid_discharges","mcare_discharges",
              "capital_imaging","capital_caresetting", "capital_services"), 
            list(miss=~ ifelse(is.na(.),1,0))) %>%
  mutate_at(c("bdtot","labor_nurse", "labor_phys","labor_residents",
              "labor_other","total_discharges", "mcaid_discharges","mcare_discharges", 
              "capital_imaging","capital_caresetting", "capital_services"),               
            list(~ ifelse(is.na(.),mean(.,na.rm=TRUE),.))) %>%
  ungroup()

hosp.only <- hosp.only %>%
  group_by(aha_hnpi) %>%
  arrange(aha_hnpi, Year) %>%
  fill(CH_Tier, fips, teaching, nonprofit, system, urban, hrrcode, hosp_zip, .direction="downup")

hosp.only <- hosp.only %>%
  mutate(CH_TierAll=case_when(CH_Tier==1 ~ "Tier A",
                              CH_Tier==2 ~ "Tier B",
                              CH_Tier==3 ~ "Tier C",
                              CH_Tier==4 ~ "Tier D",
                              is.na(CH_Tier) ~ "Unassigned",
                              TRUE ~ "Missing"),
         CH_TierAlt=case_when(CH_Tier==1 ~ "Tier A",
                              CH_Tier==2 ~ "Tier B",
                              CH_Tier==3 ~ "NCH",
                              CH_Tier==4 ~ "NCH",
                              is.na(CH_Tier) ~ "NCH"))

hosp.specialty <- ch.data.full %>%
  group_by(aha_hnpi, Year) %>%
  summarize(ecmo=sum(Surg_ECMO, na.rm=TRUE),
            chs=sum(Surg_CHS, na.rm=TRUE),
            transplant=sum(Surg_Transplant, na.rm=TRUE),
            brain=sum(Surg_Brain, na.rm=TRUE)) %>%
  mutate(specialty = case_when(
    ecmo>0 ~ 1,
    chs>0 ~ 1,
    transplant>0 ~ 1,
    brain>0 ~ 1,
    TRUE ~ 0))

hosp.specialty <- hosp.specialty %>%
  group_by(aha_hnpi) %>%
  arrange(aha_hnpi, Year) %>%
  mutate(ever_specialty=cumsum(specialty)) %>%
  ungroup() %>%
  mutate(specialty=ifelse(ever_specialty>0,1,0)) %>%
  select(-ever_specialty)

hosp.only <- hosp.only %>%
  left_join(hosp.specialty, by=c("aha_hnpi","Year"))


## create data of unique fips/year observations
fips.only <- ch.data %>%
  group_by(fips,Year) %>%
  dplyr::select(fips, Year, totalpop, 
                starts_with("race_"), starts_with("age_"),
                starts_with("income_"), starts_with("educ_"),
                emp_fulltime, totalhhi) %>%
  dplyr::mutate(fips_obs_count=row_number())

fips.only <- fips.only %>%
  group_by(fips, Year) %>%
  arrange(fips, Year, totalpop) %>%
  filter(fips_obs_count==1)


fips.only <- fips.only %>% 
  group_by(fips) %>%
  mutate_at(c("totalpop", "age_18to34", "age_35to64", "age_65plus", 
              "race_black", "race_asian", "race_other",
              "income_25to50", "income_50to75", "income_75to100",
              "income_100to150", "income_150plus", "educ_gradeschool",
              "educ_highschool", "educ_hsgrad", "educ_graduate", "emp_fulltime",
              "totalhhi"), 
            list(miss=~ ifelse(is.na(.),1,0))) %>%
  mutate_at(c("totalpop", "age_18to34", "age_35to64", "age_65plus", 
              "race_black", "race_asian", "race_other",
              "income_25to50", "income_50to75", "income_75to100",
              "income_100to150", "income_150plus", "educ_gradeschool",
              "educ_highschool", "educ_hsgrad", "educ_graduate", "emp_fulltime",
              "totalhhi"), 
            list(~ ifelse(is.na(.),mean(.,na.rm=TRUE),.))) %>%
  ungroup()

fips.specialty <- ch.data.full %>%
  group_by(fips, Year) %>%
  summarize(ecmo_total=sum(Surg_ECMO, na.rm=TRUE),
            chs_total=sum(Surg_CHS, na.rm=TRUE),
            transplant_total=sum(Surg_Transplant, na.rm=TRUE),
            brain_total=sum(Surg_Brain, na.rm=TRUE))
  
fips.only <- fips.only %>%
  left_join(fips.specialty, by=c("fips","Year"))
    

# Final claims-level dataset ----------------------------------------------

ch.data.final <- claims.only %>%
  left_join(hosp.only %>% select(-hosp_zip),by=c("aha_hnpi","Year")) %>%
  left_join(fips.only,by=c("fips","Year"))

ch.data.final <- ch.data.final %>%
  dplyr::select(aha_hnpi, Year, CH_TierAll, CH_TierAlt, nonprofit, system, teaching, hrrcode, urban,
                bdtot, labor_nurse, labor_residents, labor_phys, labor_other, total_discharges,
                mcaid_discharges, mcare_discharges, totalpop, age_18to34,
                age_35to64, age_65plus, race_black, race_asian, race_other,
                income_25to50, income_50to75, income_75to100, income_100to150, income_150plus,
                educ_gradeschool, educ_highschool, educ_hsgrad, educ_graduate, emp_fulltime,
                ends_with("_miss"), Surgery, hcci_price, gdr, los, Comp_Any,
                any_readmit_30, any_readmit_60, any_readmit_90, fips,
                tot_mem_cs, z_patid, fst_admtdt, last_dischdt, mdc, Type, prod,
                z_group_id, drg, mbr_zip_5_cd, Surgery, ends_with("_ccc"), ccc_flag, month, hosp_zip,
                specialty, brain, ecmo, transplant, chs, brain_total, ecmo_total, transplant_total, chs_total) %>%
  mutate(female=(gdr==2))
