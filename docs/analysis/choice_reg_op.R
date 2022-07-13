
# Meta --------------------------------------------------------------------

## Author:        Ian McCarthy
## Date Created:  9/4/2019
## Date Edited:   10/19/2021
## Description:   Hospital choice model for outpatient visits
## Notes:         Check market variable. "market" denotes community detection


# Predicted prices and quality outcomes -----------------------------------

mean.price.op <- ch.data.final %>%
  filter(Type=="Outpatient") %>%
  group_by(Year, aha_hnpi, Surgery) %>%
  summarize(mean_price=mean(hcci_price, rm.na=TRUE)) %>% ungroup()

min.price.op <- claims.only %>%
  filter(Type=="Outpatient") %>%
  group_by(aha_hnpi, Year) %>%
  summarize(min_price=min(hcci_price, na.rm=TRUE))

reg.price.op <- felm(hcci_price ~ gdr + Surgery + factor(Year) + ccc_flag + factor(month) | aha_hnpi,
                     data=claims.only %>% filter(Type=="Outpatient"))
reg.price.coeff.op <- reg.price.op$coefficients
price.fe.op <- as_tibble(getfe(reg.price.op))
price.fe.op <- price.fe.op %>%
  dplyr::select(fe=effect, aha_hnpi=idx) %>%
  mutate(aha_hnpi=as.character(aha_hnpi))

reg.comp.op <- felm(Comp_Any ~ gdr + Surgery + factor(Year) + ccc_flag + factor(month) | aha_hnpi,
                    data=claims.only %>% filter(Type=="Outpatient"))
reg.comp.coeff.op <- reg.comp.op$coefficients
comp.fe.op <- as_tibble(getfe(reg.comp.op))
comp.fe.op <- comp.fe.op %>%
  dplyr::select(fe=effect, aha_hnpi=idx) %>%
  mutate(aha_hnpi=as.character(aha_hnpi))

reg.readmit.op <- felm(any_readmit_90 ~ gdr + Surgery + factor(Year) + ccc_flag + factor(month) | aha_hnpi,
                       data=claims.only %>% filter(Type=="Outpatient"))
reg.readmit.coeff.op <- reg.readmit.op$coefficients
readmit.fe.op <- as_tibble(getfe(reg.readmit.op))
readmit.fe.op <- readmit.fe.op %>%
  dplyr::select(fe=effect, aha_hnpi=idx) %>%
  mutate(aha_hnpi=as.character(aha_hnpi))


for (y in 2010:2015) {
  
  choice.data <- choice.data.fn(y=y, t="Outpatient")
  
  choice.markets <-  choice.data %>%
    filter(distance_mi<=100) %>%
    distinct(z_patid, fst_admtdt, last_dischdt, aha_hnpi, Year, distance_mi, pt_zip, hosp_zip) %>%
    left_join(zip.markets %>% rename(pt_market=hrr),
              by=c("pt_zip"="zip")) %>%
    left_join(zip.markets %>% rename(hosp_market=hrr),
              by=c("hosp_zip"="zip")) %>%
    distinct(z_patid, fst_admtdt, last_dischdt, aha_hnpi, Year, pt_market, hosp_market) %>%
    filter(pt_market==hosp_market)
   
  claims.merge <- claims.only %>% 
    filter(Type=="Outpatient", Year==y) %>%
    dplyr::select(hosp_zip_select=hosp_zip, z_patid, fst_admtdt, last_dischdt, gdr, Surgery, tot_mem_cs, 
                  hcci_price, Surgery, ccc_flag, month, any_readmit_90, Comp_Any)
  
  hosp.merge <- hosp.only %>%
    filter(Year==y) %>%
    dplyr::select(aha_hnpi, bdtot, CH_TierAll, nonprofit, teaching, system)
  
  cdata.merge <- choice.data %>%
    inner_join(claims.merge,
               by=c("z_patid","fst_admtdt","last_dischdt")) %>%
    inner_join(hosp.merge, 
               by=c("aha_hnpi")) %>%
    inner_join(choice.markets, 
               by=c("z_patid","fst_admtdt","last_dischdt","aha_hnpi","Year")) %>%
    mutate(unique_id=row_number())
  
  pred.price <- cdata.merge %>%
    dplyr::select(hcci_price, gdr, Surgery, aha_hnpi, Year, unique_id, ccc_flag, month, Type) %>%
    mutate(xb.price=reg.price.coeff.op[1]*gdr +
             reg.price.coeff.op[2]*(Surgery=="Appendectomy") +
             reg.price.coeff.op[3]*(Surgery=="Circumcision") +
             reg.price.coeff.op[4]*(Surgery=="Double") +
             reg.price.coeff.op[5]*(Surgery=="Gallbladder") +
             reg.price.coeff.op[6]*(Surgery=="Humerus") +
             reg.price.coeff.op[7]*(Surgery=="IngHernia") +
             reg.price.coeff.op[8]*(Surgery=="Knee") +
             reg.price.coeff.op[9]*(Surgery=="Orchiopexy") +
             reg.price.coeff.op[10]*(Surgery=="Spine") +
             reg.price.coeff.op[11]*(Surgery=="Strabismus") +
             reg.price.coeff.op[12]*(Surgery=="Tonsils") +
             reg.price.coeff.op[13]*(Surgery=="Tympanostomy") +
             reg.price.coeff.op[14]*(Surgery=="UmbHernia") +
             reg.price.coeff.op[15]*(Year==2011) +
             reg.price.coeff.op[16]*(Year==2012) +
             reg.price.coeff.op[17]*(Year==2013) +
             reg.price.coeff.op[18]*(Year==2014) +
             reg.price.coeff.op[19]*(Year==2015) +
             reg.price.coeff.op[20]*(ccc_flag==TRUE) +
             reg.price.coeff.op[21]*(month==2) +
             reg.price.coeff.op[22]*(month==3) +
             reg.price.coeff.op[23]*(month==4) +
             reg.price.coeff.op[24]*(month==5) +
             reg.price.coeff.op[25]*(month==6) +
             reg.price.coeff.op[26]*(month==7) +
             reg.price.coeff.op[27]*(month==8) +
             reg.price.coeff.op[28]*(month==9) +
             reg.price.coeff.op[29]*(month==10) +
             reg.price.coeff.op[30]*(month==11) +
             reg.price.coeff.op[31]*(month==12)) %>%
    left_join(price.fe.op, by=c("aha_hnpi")) %>%
    mutate(pred.price=fe+xb.price)    
  
  pred.comp <- cdata.merge %>%
    dplyr::select(Comp_Any, gdr, Surgery, aha_hnpi, Year, unique_id, ccc_flag, month) %>%
    mutate(xb.comp= reg.comp.coeff.op[1]*gdr +
             reg.comp.coeff.op[2]*(Surgery=="Appendectomy") +
             reg.comp.coeff.op[3]*(Surgery=="Circumcision") +
             reg.comp.coeff.op[4]*(Surgery=="Double") +
             reg.comp.coeff.op[5]*(Surgery=="Gallbladder") +
             reg.comp.coeff.op[6]*(Surgery=="Humerus") +
             reg.comp.coeff.op[7]*(Surgery=="IngHernia") +
             reg.comp.coeff.op[8]*(Surgery=="Knee") +
             reg.comp.coeff.op[9]*(Surgery=="Orchiopexy") +
             reg.comp.coeff.op[10]*(Surgery=="Strabismus") +
             reg.comp.coeff.op[11]*(Surgery=="Tonsils") +
             reg.comp.coeff.op[12]*(Surgery=="Tympanostomy") +
             reg.comp.coeff.op[13]*(Surgery=="UmbHernia") +
             reg.comp.coeff.op[14]*(Year==2011) +
             reg.comp.coeff.op[15]*(Year==2012) +
             reg.comp.coeff.op[16]*(Year==2013) +
             reg.comp.coeff.op[17]*(Year==2014) +
             reg.comp.coeff.op[18]*(Year==2015) +
             reg.comp.coeff.op[19]*(ccc_flag==TRUE) +
             reg.comp.coeff.op[20]*(month==2) +
             reg.comp.coeff.op[21]*(month==3) +
             reg.comp.coeff.op[22]*(month==4) +
             reg.comp.coeff.op[23]*(month==5) +
             reg.comp.coeff.op[24]*(month==6) +
             reg.comp.coeff.op[25]*(month==7) +
             reg.comp.coeff.op[26]*(month==8) +
             reg.comp.coeff.op[27]*(month==9) +
             reg.comp.coeff.op[28]*(month==10) +
             reg.comp.coeff.op[29]*(month==11) +
             reg.comp.coeff.op[30]*(month==12)) %>%
    left_join(comp.fe.op, by=c("aha_hnpi")) %>%
    mutate(pred.comp=fe+xb.comp)
  
  pred.readmit <- cdata.merge %>%
    dplyr::select(any_readmit_90, gdr, Surgery, aha_hnpi, Year, unique_id, ccc_flag, month) %>%
    mutate(xb.readmit= reg.readmit.coeff.op[1]*gdr +
             reg.readmit.coeff.op[2]*(Surgery=="Appendectomy") +
             reg.readmit.coeff.op[3]*(Surgery=="Circumcision") +
             reg.readmit.coeff.op[4]*(Surgery=="Double") +
             reg.readmit.coeff.op[5]*(Surgery=="Gallbladder") +
             reg.readmit.coeff.op[6]*(Surgery=="Humerus") +
             reg.readmit.coeff.op[7]*(Surgery=="IngHernia") +
             reg.readmit.coeff.op[8]*(Surgery=="Knee") +
             reg.readmit.coeff.op[9]*(Surgery=="Orchiopexy") +
             reg.readmit.coeff.op[10]*(Surgery=="Strabismus") +
             reg.readmit.coeff.op[11]*(Surgery=="Tonsils") +
             reg.readmit.coeff.op[12]*(Surgery=="Tympanostomy") +
             reg.readmit.coeff.op[13]*(Surgery=="UmbHernia") +
             reg.readmit.coeff.op[14]*(Year==2011) +
             reg.readmit.coeff.op[15]*(Year==2012) +
             reg.readmit.coeff.op[16]*(Year==2013) +
             reg.readmit.coeff.op[17]*(Year==2014) +
             reg.readmit.coeff.op[18]*(Year==2015) +
             reg.readmit.coeff.op[19]*(ccc_flag==TRUE) +
             reg.readmit.coeff.op[20]*(month==2) +
             reg.readmit.coeff.op[21]*(month==3) +
             reg.readmit.coeff.op[22]*(month==4) +
             reg.readmit.coeff.op[23]*(month==5) +
             reg.readmit.coeff.op[24]*(month==6) +
             reg.readmit.coeff.op[25]*(month==7) +
             reg.readmit.coeff.op[26]*(month==8) +
             reg.readmit.coeff.op[27]*(month==9) +
             reg.readmit.coeff.op[28]*(month==10) +
             reg.readmit.coeff.op[29]*(month==11) +
             reg.readmit.coeff.op[30]*(month==12)) %>%
    left_join(readmit.fe.op, by=c("aha_hnpi")) %>%
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
    left_join(mean.price.op,
              by=c("Year","aha_hnpi","Surgery")) %>%
    left_join(min.price.op,
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
  
  assign(paste("op.choice.reg.",y,sep=""),choice.reg)
}

op.choice.reg <- bind_rows(op.choice.reg.2010, op.choice.reg.2011, op.choice.reg.2012,
                           op.choice.reg.2013, op.choice.reg.2014, op.choice.reg.2015) %>%
  group_by(pt_market) %>%
  mutate(tot_tiera=sum(tierA, na.rm=TRUE), 
         tot_tierb=sum(tierB, na.rm=TRUE),
         tot_obs=sum(choice, na.rm=TRUE)) %>%
  select(-id) %>%
  ungroup()

op.choice.reg <- op.choice.reg %>%
  group_by(z_patid, fst_admtdt, last_dischdt) %>%
  mutate(id = cur_group_id()) %>% ungroup() %>%
  mutate(hosp_id=as.factor(aha_hnpi)) %>%
  group_by(id) %>%
  mutate(set_count=row_number(),
         max_set=max(set_count, na.rm=TRUE)) %>%
  filter(max_set>2)