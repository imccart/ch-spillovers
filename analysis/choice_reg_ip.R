
# Meta --------------------------------------------------------------------

## Author:        Ian McCarthy
## Date Created:  9/4/2019
## Date Edited:   10/19/2021
## Description:   Hospital choice model for inpatient stays
## Notes:         Check market variable. "market" denotes community detection

# Predicted prices and quality outcomes -----------------------------------

mean.price.ip <- ch.data.final %>%
  filter(Type=="Inpatient") %>%
  group_by(Year, aha_hnpi, Surgery) %>%
  summarize(mean_price=mean(hcci_price, rm.na=TRUE)) %>% ungroup()

min.price.ip <- claims.only %>%
  filter(Type=="Inpatient") %>%
  group_by(aha_hnpi, Year) %>%
  summarize(min_price=min(hcci_price, na.rm=TRUE))

reg.price.ip <- felm(hcci_price ~ los + gdr + Surgery + factor(Year) + ccc_flag + factor(month) | aha_hnpi,
                      data=claims.only %>% filter(Type=="Inpatient"))
reg.price.coeff.ip <- reg.price.ip$coefficients
price.fe.ip <- as_tibble(getfe(reg.price.ip))
price.fe.ip <- price.fe.ip %>%
  dplyr::select(fe=effect, aha_hnpi=idx) %>%
  mutate(aha_hnpi=as.character(aha_hnpi))

reg.comp.ip <- felm(Comp_Any ~ los + gdr + Surgery + factor(Year) + ccc_flag + factor(month) | aha_hnpi,
                     data=claims.only %>% filter(Type=="Inpatient"))
reg.comp.coeff.ip <- reg.comp.ip$coefficients
comp.fe.ip <- as_tibble(getfe(reg.comp.ip))
comp.fe.ip <- comp.fe.ip %>%
  dplyr::select(fe=effect, aha_hnpi=idx) %>%
  mutate(aha_hnpi=as.character(aha_hnpi))

reg.readmit.ip <- felm(any_readmit_90 ~ los + gdr + Surgery + factor(Year) + ccc_flag + factor(month) | aha_hnpi,
                        data=claims.only %>% filter(Type=="Inpatient"))
reg.readmit.coeff.ip <- reg.readmit.ip$coefficients
readmit.fe.ip <- as_tibble(getfe(reg.readmit.ip))
readmit.fe.ip <- readmit.fe.ip %>%
  dplyr::select(fe=effect, aha_hnpi=idx) %>%
  mutate(aha_hnpi=as.character(aha_hnpi))



for (y in 2010:2015) {
  choice.data <- choice.data.fn(y=y, t="Inpatient")
  
  choice.markets <- choice.data %>%
    filter(distance_mi<=100) %>%
    distinct(z_patid, fst_admtdt, last_dischdt, aha_hnpi, Year, distance_mi, pt_zip, hosp_zip) %>%
    left_join(zip.markets %>% rename(pt_market=hrr),
              by=c("pt_zip"="zip")) %>%
    left_join(zip.markets %>% rename(hosp_market=hrr),
              by=c("hosp_zip"="zip")) %>%
    distinct(z_patid, fst_admtdt, last_dischdt, aha_hnpi, Year, pt_market, hosp_market) %>%
    filter(pt_market==hosp_market)
  
  
  cdata.merge <- choice.data %>%
    inner_join(claims.only %>% filter(Type=="Inpatient") %>%
                dplyr::select(-aha_hnpi, -Year, -Type, hosp_zip_select=hosp_zip),
              by=c("z_patid","fst_admtdt","last_dischdt")) %>%
    inner_join(hosp.only %>%
                dplyr::select(-hosp_obs_count, -hosp_zip), 
              by=c("aha_hnpi","Year")) %>%
    inner_join(fips.only %>%
                dplyr::select(-fips_obs_count), 
              by=c("fips","Year")) %>%
    inner_join(choice.markets, 
               by=c("z_patid","fst_admtdt","last_dischdt","aha_hnpi","Year")) %>%
    mutate(unique_id=row_number())
  
  pred.price <- cdata.merge %>%
    dplyr::select(hcci_price, los, gdr, Surgery, aha_hnpi, Year, unique_id, ccc_flag, month, Type) %>%
    mutate(xb.price=reg.price.coeff.ip[1]*los + reg.price.coeff.ip[2]*gdr +
             reg.price.coeff.ip[3]*(Surgery=="Appendectomy") +
             reg.price.coeff.ip[4]*(Surgery=="Circumcision") +
             reg.price.coeff.ip[5]*(Surgery=="Double") +
             reg.price.coeff.ip[6]*(Surgery=="Gallbladder") +
             reg.price.coeff.ip[7]*(Surgery=="Humerus") +
             reg.price.coeff.ip[8]*(Surgery=="IngHernia") +
             reg.price.coeff.ip[9]*(Surgery=="Knee") +
             reg.price.coeff.ip[10]*(Surgery=="Orchiopexy") +
             reg.price.coeff.ip[11]*(Surgery=="Spine") +
             reg.price.coeff.ip[12]*(Surgery=="Strabismus") +
             reg.price.coeff.ip[13]*(Surgery=="Tonsils") +
             reg.price.coeff.ip[14]*(Surgery=="Tympanostomy") +
             reg.price.coeff.ip[15]*(Surgery=="UmbHernia") +
             reg.price.coeff.ip[16]*(Year==2011) +
             reg.price.coeff.ip[17]*(Year==2012) +
             reg.price.coeff.ip[18]*(Year==2013) +
             reg.price.coeff.ip[19]*(Year==2014) +
             reg.price.coeff.ip[20]*(Year==2015) +
             reg.price.coeff.ip[21]*(ccc_flag==TRUE) +
             reg.price.coeff.ip[22]*(month==2) +
             reg.price.coeff.ip[23]*(month==3) +
             reg.price.coeff.ip[24]*(month==4) +
             reg.price.coeff.ip[25]*(month==5) +
             reg.price.coeff.ip[26]*(month==6) +
             reg.price.coeff.ip[27]*(month==7) +
             reg.price.coeff.ip[28]*(month==8) +
             reg.price.coeff.ip[29]*(month==9) +
             reg.price.coeff.ip[30]*(month==10) +
             reg.price.coeff.ip[31]*(month==11) +
             reg.price.coeff.ip[32]*(month==12)) %>%
    left_join(price.fe.ip, by=c("aha_hnpi")) %>%
    mutate(pred.price=fe+xb.price)    
  
  pred.comp <- cdata.merge %>%
    dplyr::select(Comp_Any, los, gdr, Surgery, aha_hnpi, Year, unique_id, ccc_flag, month) %>%
    mutate(xb.comp=reg.comp.coeff.ip[1]*los + reg.comp.coeff.ip[2]*gdr +
             reg.comp.coeff.ip[3]*(Surgery=="Appendectomy") +
             reg.comp.coeff.ip[4]*(Surgery=="Circumcision") +
             reg.comp.coeff.ip[5]*(Surgery=="Double") +
             reg.comp.coeff.ip[6]*(Surgery=="Gallbladder") +
             reg.comp.coeff.ip[7]*(Surgery=="Humerus") +
             reg.comp.coeff.ip[8]*(Surgery=="IngHernia") +
             reg.comp.coeff.ip[9]*(Surgery=="Knee") +
             reg.comp.coeff.ip[10]*(Surgery=="Orchiopexy") +
             reg.comp.coeff.ip[11]*(Surgery=="Spine") +
             reg.comp.coeff.ip[12]*(Surgery=="Strabismus") +
             reg.comp.coeff.ip[13]*(Surgery=="Tonsils") +
             reg.comp.coeff.ip[14]*(Surgery=="Tympanostomy") +
             reg.comp.coeff.ip[15]*(Surgery=="UmbHernia") +
             reg.comp.coeff.ip[16]*(Year==2011) +
             reg.comp.coeff.ip[17]*(Year==2012) +
             reg.comp.coeff.ip[18]*(Year==2013) +
             reg.comp.coeff.ip[19]*(Year==2014) +
             reg.comp.coeff.ip[20]*(Year==2015) +
             reg.comp.coeff.ip[21]*(ccc_flag==TRUE) +
             reg.comp.coeff.ip[22]*(month==2) +
             reg.comp.coeff.ip[23]*(month==3) +
             reg.comp.coeff.ip[24]*(month==4) +
             reg.comp.coeff.ip[25]*(month==5) +
             reg.comp.coeff.ip[26]*(month==6) +
             reg.comp.coeff.ip[27]*(month==7) +
             reg.comp.coeff.ip[28]*(month==8) +
             reg.comp.coeff.ip[29]*(month==9) +
             reg.comp.coeff.ip[30]*(month==10) +
             reg.comp.coeff.ip[31]*(month==11) +
             reg.comp.coeff.ip[32]*(month==12)) %>%
    left_join(comp.fe.ip, by=c("aha_hnpi")) %>%
    mutate(pred.comp=fe+xb.comp)
  
  pred.readmit <- cdata.merge %>%
    dplyr::select(any_readmit_30, los, gdr, Surgery, aha_hnpi, Year, unique_id, ccc_flag, month) %>%
    mutate(xb.readmit=reg.readmit.coeff.ip[1]*los + reg.readmit.coeff.ip[2]*gdr +
             reg.readmit.coeff.ip[3]*(Surgery=="Appendectomy") +
             reg.readmit.coeff.ip[4]*(Surgery=="Circumcision") +
             reg.readmit.coeff.ip[5]*(Surgery=="Double") +
             reg.readmit.coeff.ip[6]*(Surgery=="Gallbladder") +
             reg.readmit.coeff.ip[7]*(Surgery=="Humerus") +
             reg.readmit.coeff.ip[8]*(Surgery=="IngHernia") +
             reg.readmit.coeff.ip[9]*(Surgery=="Knee") +
             reg.readmit.coeff.ip[10]*(Surgery=="Orchiopexy") +
             reg.readmit.coeff.ip[11]*(Surgery=="Spine") +
             reg.readmit.coeff.ip[12]*(Surgery=="Strabismus") +
             reg.readmit.coeff.ip[13]*(Surgery=="Tonsils") +
             reg.readmit.coeff.ip[14]*(Surgery=="Tympanostomy") +
             reg.readmit.coeff.ip[15]*(Surgery=="UmbHernia") +
             reg.readmit.coeff.ip[16]*(Year==2011) +
             reg.readmit.coeff.ip[17]*(Year==2012) +
             reg.readmit.coeff.ip[18]*(Year==2013) +
             reg.readmit.coeff.ip[19]*(Year==2014) +
             reg.readmit.coeff.ip[20]*(Year==2015) +
             reg.readmit.coeff.ip[21]*(ccc_flag==TRUE) +
             reg.readmit.coeff.ip[22]*(month==2) +
             reg.readmit.coeff.ip[23]*(month==3) +
             reg.readmit.coeff.ip[24]*(month==4) +
             reg.readmit.coeff.ip[25]*(month==5) +
             reg.readmit.coeff.ip[26]*(month==6) +
             reg.readmit.coeff.ip[27]*(month==7) +
             reg.readmit.coeff.ip[28]*(month==8) +
             reg.readmit.coeff.ip[29]*(month==9) +
             reg.readmit.coeff.ip[30]*(month==10) +
             reg.readmit.coeff.ip[31]*(month==11) +
             reg.readmit.coeff.ip[32]*(month==12)) %>%
    left_join(readmit.fe.ip, by=c("aha_hnpi")) %>%
    mutate(pred.readmit=fe+xb.readmit)
  
  final.merge <- cdata.merge %>%
    left_join(pred.price %>%
                dplyr::select(unique_id, pred.price), 
              by=c("unique_id")) %>%
    left_join(pred.comp %>%
                dplyr::select(unique_id, pred.comp), 
              by=c("unique_id")) %>%
    left_join(pred.readmit %>%
                dplyr::select(unique_id, pred.readmit), 
              by=c("unique_id")) %>%
    left_join(mean.price.ip,
              by=c("Year","aha_hnpi","Surgery")) %>%
    left_join(min.price.ip,
            by=c("Year","aha_hnpi"))

  final.merge <- as.data.table(final.merge)
  final.merge[pred.price<min_price,pred.price := min_price]
  final.merge <- as_tibble(final.merge)

  final.merge <- final.merge %>%
    group_by(z_patid, fst_admtdt, last_dischdt) %>%
    mutate(id = cur_group_id()) %>%
    ungroup()
  
  clean.choice <- final.merge %>%
    group_by(id) %>%
    dplyr::select(id, choice) %>%
    summarize(any_choice=max(choice))
  
  clean.hosp <- final.merge %>%
    group_by(id, aha_hnpi) %>%
    dplyr::select(id, aha_hnpi, distance_mi) %>%
    arrange(id, aha_hnpi, distance_mi) %>%
    summarize(hosp_count=n())
  
  choice.dist <- final.merge %>%
    group_by(id) %>%
    dplyr::select(id, distance_mi) %>%
    summarize(nearest=min(distance_mi)) %>%
    dplyr::select(id, nearest)
  
  choice.reg <- final.merge %>%
    left_join(clean.choice, by=c("id")) %>%
    left_join(clean.hosp, by=c("id","aha_hnpi")) %>%
    filter(hosp_count==1 & any_choice==1 & !is.na(Surgery)) %>%
    left_join(choice.dist, by=c("id")) %>%
    dplyr::select(-hosp_count, -any_choice)
  
  choice.reg <- choice.reg %>%
    group_by(id) %>%
    mutate_at(c("pred.price", "pred.comp", "pred.readmit"), 
              list(~ ifelse(is.na(.),mean(.,na.rm=TRUE),.))) %>%
    ungroup()
  
  choice.reg <- choice.reg %>%
    mutate(diff_dist=distance_mi-nearest,
           perc_cs=pmin(tot_mem_cs/hcci_price,1),
           pred_cs=pmax(pred.price*perc_cs,0),
           no_cs=(tot_mem_cs==0),
           predprice_dist=pred.price*diff_dist,
           tierA=(CH_TierAll=="Tier A"),
           tierB=(CH_TierAll=="Tier B"),
           tierC=(CH_TierAll=="Tier C"),
           beds_dist=diff_dist*bdtot,
           system_dist=diff_dist*system,
           teaching_dist=diff_dist*teaching,
           nonprofit_dist=diff_dist*nonprofit,
           predcs_dist=diff_dist*pred_cs,
           nocs_dist=diff_dist*no_cs,
           nocs_beds=no_cs*bdtot,
           nocs_system=no_cs*system,
           nocs_teaching=no_cs*teaching,
           nocs_nonprofit=no_cs*nonprofit,
           gdr_dist=diff_dist*gdr,
           gdr_beds=gdr*bdtot,
           gdr_system=gdr*system,
           gdr_teaching=gdr*teaching,
           gdr_nonprofit=gdr*nonprofit,
           los_dist=diff_dist*los,
           los_beds=los*bdtot,
           los_system=los*system,
           los_teaching=los*teaching,
           los_nonprofit=los*nonprofit,
           ccc_system=ccc_flag*system,
           ccc_dist=ccc_flag*diff_dist,
           ccc_beds=ccc_flag*bdtot,
           ccc_teaching=ccc_flag*teaching,
           ccc_nonprofit=ccc_flag*nonprofit,
           app_dist=(Surgery=="Appendectomy")*diff_dist,
           app_beds=(Surgery=="Appendectomy")*bdtot,
           app_system=(Surgery=="Appendectomy")*system,
           app_teaching=(Surgery=="Appendectomy")*teaching,
           app_nonprofit=(Surgery=="Appendectomy")*nonprofit,
           bone_dist=(Surgery=="Humerus")*diff_dist,
           bone_beds=(Surgery=="Humerus")*bdtot,
           bone_system=(Surgery=="Humerus")*system,
           bone_teaching=(Surgery=="Humerus")*teaching,
           bone_nonprofit=(Surgery=="Humerus")*nonprofit,
           spine_dist=(Surgery=="Spine")*diff_dist,
           spine_beds=(Surgery=="Spine")*bdtot,
           spine_system=(Surgery=="Spine")*system,
           spine_teaching=(Surgery=="Spine")*teaching,
           spine_nonprofit=(Surgery=="Spine")*nonprofit,
           tonsil_dist=(Surgery=="Tonsils")*diff_dist,
           tonsil_beds=(Surgery=="Tonsils")*bdtot,
           tonsil_system=(Surgery=="Tonsils")*system,
           tonsil_teaching=(Surgery=="Tonsils")*teaching,
           tonsil_nonprofit=(Surgery=="Tonsils")*nonprofit
    )
  
  assign(paste("ip.choice.reg.",y,sep=""),choice.reg)
}

ip.choice.reg <- bind_rows(ip.choice.reg.2010, ip.choice.reg.2011, ip.choice.reg.2012,
                           ip.choice.reg.2013, ip.choice.reg.2014, ip.choice.reg.2015) %>%
  group_by(pt_market) %>%
  mutate(tot_tiera=sum(tierA, na.rm=TRUE), 
         tot_tierb=sum(tierB, na.rm=TRUE),
         tot_obs=sum(choice, na.rm=TRUE)) %>%
  select(-id) %>%
  ungroup()

ip.choice.reg <- ip.choice.reg %>%
  group_by(z_patid, fst_admtdt, last_dischdt) %>%
  mutate(id = cur_group_id()) %>% ungroup() %>%
  mutate(hosp_id=as.factor(aha_hnpi)) %>%
  group_by(id) %>%
  mutate(set_count=row_number(),
         max_set=max(set_count, na.rm=TRUE)) %>%
  filter(max_set>2)