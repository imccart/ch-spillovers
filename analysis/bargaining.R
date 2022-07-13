
# Meta --------------------------------------------------------------------

## Author:        Ian McCarthy
## Date Created:  2/16/2021
## Date Edited:   12/16/2021
## Description:   Analysis of CH spillovers from specialized procedures into generic procedures
##                The goal of this analysis is to examine the spillover effects from offering
##                highly specialized procedures (effectively as a monopolist) on the prices for
##                other, more homogeneous products. The identification strategy is to exploit
##                new "entrants" for these specialized procedures, and the effects of such expansion
##                onto prices for other procedures.


# Identify market-level "expansion" ---------------------------------------


## count hospitals offering specialized procedures
market.specialty <- hosp.only %>% 
  select(aha_hnpi, Year, specialty, zip=hosp_zip) %>%
  mutate(zip=as.numeric(zip)) %>%
  left_join(zip.markets, by="zip") %>%
  group_by(market, Year) %>%
  summarize(specialty_market=sum(specialty, na.rm=TRUE)) %>%
  group_by(market) %>%
  arrange(market, Year) %>%
  mutate(specialty_market_lag=lag(specialty_market)) %>%
  mutate(specialty_expand=ifelse(specialty_market>specialty_market_lag, 1, 0))

## identify year of specialty expansion in data
specialty.expand.dat <- market.specialty %>%
  filter(specialty_expand==1) %>%
  group_by(market) %>%
  summarize(min_specialty_expand=min(Year))

hosp.panel <- hosp.only %>%
  filter(specialty==1) %>%
  distinct(aha_hnpi, Year) %>%
  group_by(aha_hnpi) %>%
  summarize(min_year=min(Year, na.rm=TRUE),
            max_year=max(Year, na.rm=TRUE))


# Market-level Dataset ----------------------------------------------------

## dataset focusing on expansion of new specialty services
market.dat.specialty <- ch.data.final %>% 
  dplyr::select(hcci_price, gdr, los, Surgery, aha_hnpi, Year, Comp_Any,
                any_readmit_30, any_readmit_60, any_readmit_90, zip=hosp_zip,
                CH_TierAll, aha_hnpi, Type, specialty) %>%
  mutate(zip=as.numeric(zip),
         Tier_A=ifelse(CH_TierAll=="Tier A",1,0),
         Tier_B=ifelse(CH_TierAll=="Tier B", 1, 0),
         Tier_C=ifelse(CH_TierAll=="Tier C",1,0),
         Other=ifelse(CH_TierAll=="Other", 1, 0)) %>%
  left_join(hosp.panel, by="aha_hnpi") %>%  
  left_join(zip.markets, by="zip") %>%
  left_join(specialty.expand.dat, by="market") %>%
  mutate(
    keep_obs=case_when(
      !is.na(min_specialty_expand) & min_specialty_expand>min_year ~ 1,
      is.na(min_specialty_expand) ~ 1,
      TRUE ~ NA_real_)) %>%
  filter(keep_obs==1 & CH_TierAll %in% c("Tier A", "Tier B")) %>%
  mutate(
    specialty_price=case_when(
      specialty==1 & min_year<=2011 ~ hcci_price,
      TRUE ~ NA_real_),
    nonspecialty_price=case_when(
      specialty==0 ~ hcci_price,
      TRUE ~ NA_real_)) %>%
  add_count(aha_hnpi, Year, name="hosp_patients") %>%
  group_by(market, Year) %>%
  summarize(mean_price=mean(hcci_price, na.rm=TRUE),
            mean_patients=mean(hosp_patients, na.rm=TRUE),
            mean_specialty_price=mean(specialty_price, na.rm=TRUE),
            mean_nonspecialty_price=mean(nonspecialty_price, na.rm=TRUE),            
            mkt_patients=n(),
            tot_readmit_30=sum(any_readmit_30, na.rm=TRUE),
            tot_readmit_60=sum(any_readmit_60, na.rm=TRUE),
            tot_readmit_90=sum(any_readmit_90, na.rm=TRUE),
            tot_comp=sum(Comp_Any, na.rm=TRUE),
            tier_a_patient=sum(Tier_A, na.rm=TRUE),
            tier_b_patient=sum(Tier_B, na.rm=TRUE))



  

# Specialty over time -----------------------------------------------------

panel.mkt.dat <- market.dat.specialty %>%
  left_join(specialty.expand.dat, by="market") %>%
  mutate(specialty_treat=ifelse(Year>=min_specialty_expand & !is.na(min_specialty_expand), 1, 0)) %>%
  group_by(market) %>%
  mutate(max_patients=max(mkt_patients, na.rm=TRUE),
         mkt_count=n()) %>%
  ungroup() %>%
  filter(mkt_count==6, Year>=2011) %>%
  mutate(mkt_num=as.integer(market))

panel.spec <- panelview(mean_price~specialty_treat, data=panel.mkt.dat %>% select(mean_price, specialty_treat, mkt_num, Year),
          index=c("mkt_num","Year"), legendOff=TRUE, 
          theme.bw=TRUE, by.timing=TRUE, xlab="Year", ylab="Market",
          main="", color=c("white","gray"), axis.lab="time")
ggsave("results/f4-panel-view-expansion.png", panel.spec)




# Event study at claim level ----------------------------------------------

claim.reg.spec <- ch.data.final %>%
  mutate(zip=as.numeric(hosp_zip)) %>%
  left_join(zip.markets, by=c("zip")) %>%
  left_join(specialty.expand.dat, by="market") %>%
  left_join(market.specialty, by=c("market","Year")) %>%
  left_join(hosp.panel, by="aha_hnpi") %>%    
  mutate(specialty_treat=ifelse(Year>=min_specialty_expand & !is.na(min_specialty_expand), 1, 0),
         ever_treat=ifelse(min_specialty_expand>2011 & !is.na(min_specialty_expand), 1, 0)) %>%
  mutate(log_price=log(hcci_price),
         event_time=case_when(
           !is.na(min_specialty_expand) ~ Year - min_specialty_expand,
           is.na(min_specialty_expand) ~ -1),
         time_m3=1*(event_time<=-3),
         time_m2=1*(event_time==-2),
         time_0=1*(event_time==0),
         time_p1=1*(event_time==1),
         time_p2=1*(event_time==2),
         time_p3=1*(event_time>=3))
         

  
mod1 <- feols(log_price ~  time_m3 + time_m2 + time_0 + time_p1 + time_p2 + time_p3 + 
                Surgery + Type + ccc_flag + gdr + los + any_readmit_30 + Comp_Any +
                bdtot + labor_nurse + labor_phys + labor_residents + labor_other + 
                total_discharges + mcaid_discharges + mcare_discharges +
                totalpop + age_18to34 + age_35to64 + age_65plus + 
                race_black + race_asian + race_other +
                income_25to50 + income_50to75 + income_75to100 +
                income_100to150 + income_150plus + educ_gradeschool +
                educ_highschool + educ_hsgrad + educ_graduate + emp_fulltime 
              | Year + aha_hnpi, 
              cluster="aha_hnpi", 
              data=claim.reg.spec)
summary(mod1)

mod2 <- feols(log_price ~ time_m3 + time_m2 + time_0 + time_p1 + time_p2 + time_p3 +
                Surgery + Type + ccc_flag + gdr + los + any_readmit_30 + Comp_Any +
                bdtot + labor_nurse + labor_phys + labor_residents + labor_other + 
                total_discharges + mcaid_discharges + mcare_discharges +
                totalpop + age_18to34 + age_35to64 + age_65plus + 
                race_black + race_asian + race_other +
                income_25to50 + income_50to75 + income_75to100 +
                income_100to150 + income_150plus + educ_gradeschool +
                educ_highschool + educ_hsgrad + educ_graduate + emp_fulltime 
              | Year + aha_hnpi,
              cluster="aha_hnpi", 
              data=claim.reg.spec %>% filter(CH_TierAll %in% c("Tier A", "Tier B")))
summary(mod2)


mod3 <- feols(log_price ~ time_m3 + time_m2 + time_0 + time_p1 + time_p2 + time_p3 +
                Surgery + Type + ccc_flag + gdr + los + any_readmit_30 + Comp_Any +
                bdtot + labor_nurse + labor_phys + labor_residents + labor_other + 
                total_discharges + mcaid_discharges + mcare_discharges +
                totalpop + age_18to34 + age_35to64 + age_65plus + 
                race_black + race_asian + race_other +
                income_25to50 + income_50to75 + income_75to100 +
                income_100to150 + income_150plus + educ_gradeschool +
                educ_highschool + educ_hsgrad + educ_graduate + emp_fulltime 
              | Year + aha_hnpi,
              cluster="aha_hnpi", 
              data=claim.reg.spec %>% filter(!CH_TierAll %in% c("Tier A", "Tier B")))
summary(mod3)

point.spec1 <- as_tibble(mod1$coefficients, rownames="term") %>%
  filter(term %in% c("time_m3", "time_m2", "time_0", "time_p1", "time_p2", "time_p3")) %>%
  rename(estimate=value)
ci.spec1 <- as_tibble(confint(mod1), rownames="term") %>%
  filter(term %in% c("time_m3", "time_m2", "time_0", "time_p1", "time_p2", "time_p3")) %>%
  rename(conf.low = `2.5 %`, conf.high = `97.5 %`)

point.spec2 <- as_tibble(mod2$coefficients, rownames="term") %>%
  filter(term %in% c("time_m3", "time_m2", "time_0", "time_p1", "time_p2", "time_p3")) %>%
  rename(estimate=value)
ci.spec2 <- as_tibble(confint(mod2), rownames="term") %>%
  filter(term %in% c("time_m3", "time_m2", "time_0", "time_p1", "time_p2", "time_p3")) %>%
  rename(conf.low = `2.5 %`, conf.high = `97.5 %`)

point.spec3 <- as_tibble(mod3$coefficients, rownames="term") %>%
  filter(term %in% c("time_m3", "time_m2", "time_0", "time_p1", "time_p2", "time_p3")) %>%
  rename(estimate=value)
ci.spec3 <- as_tibble(confint(mod3), rownames="term") %>%
  filter(term %in% c("time_m3", "time_m2", "time_0", "time_p1", "time_p2", "time_p3")) %>%
  rename(conf.low = `2.5 %`, conf.high = `97.5 %`)

new.row <- tibble(
  term= "time_m1",
  estimate=0,
  conf.low=0,
  conf.high=0,
  event_time=-1
)

est.spec1 <- point.spec1 %>%
  left_join(ci.spec1, by="term") %>%
  mutate(event_time = c(-3, -2, 0, 1, 2, 3),
         model=1) %>%
  bind_rows(new.row) %>%
  arrange(event_time)

est.spec2 <- point.spec2 %>%
  left_join(ci.spec2, by="term") %>%
  mutate(event_time = c(-3, -2, 0, 1, 2, 3),
         model=2) %>%
  bind_rows(new.row) %>%
  arrange(event_time)

est.spec3 <- point.spec3 %>%
  left_join(ci.spec3, by="term") %>%
  mutate(event_time = c(-3, -2, 0, 1, 2, 3),
         model=3) %>%
  bind_rows(new.row) %>%
  arrange(event_time)

est.spec <- rbind(est.spec1, est.spec2, est.spec3)

plot.spec <- dwplot(est.spec,
                  vline=geom_vline(xintercept=0, linetype=2),
                  vars_order = c("time_p3","time_p2","time_p1",
                                 "time_0", "time_m1", "time_m2",
                                 "time_m3"),
                  whisker_args = list(aes(linetype=model), size=0.8),
                  dot_args = list(aes(shape=model), size=2)) +
  coord_flip() + theme_bw() +
  scale_color_grey(start=0.1, end=0.1,
                   name="",
                   breaks="",
                   labels="") +
  scale_shape_discrete(name="Group",
                       breaks=c(1,2,3),
                       labels=c("All Hospitals", "CH Only", "NCH Only")) +
  labs(y = "Time",
       x = "Estimate and 95% CI",
       title = "") +
  scale_y_discrete(labels=c("time_p3"="t+3",
                            "time_p2"="t+2",
                            "time_p1"="t+1",
                            "time_0"="0",
                            "time_m1" = "t-1",
                            "time_m2" = "t-2",
                            "time_m3" = "t-3"))
ggsave("results/f5-event-spec.png", plot.spec)

## sun and abraham approach
sunab.dat <- claim.reg.spec %>%
  mutate(cohort=ifelse(is.na(min_specialty_expand),-1,0),
         event_time=case_when(
           event_time < -3 & !is.na(event_time) ~ -3,
           event_time >  3 & !is.na(event_time) ~ 3,
           TRUE ~ event_time
         ))
mod.sa1 <- feols(log_price ~  sunab(cohort, event_time) + 
                   Surgery + Type + ccc_flag + gdr + los + any_readmit_30 + Comp_Any +
                   bdtot + labor_nurse + labor_phys + labor_residents + labor_other + 
                   total_discharges + mcaid_discharges + mcare_discharges +
                   totalpop + age_18to34 + age_35to64 + age_65plus + 
                   race_black + race_asian + race_other +
                   income_25to50 + income_50to75 + income_75to100 +
                   income_100to150 + income_150plus + educ_gradeschool +
                   educ_highschool + educ_hsgrad + educ_graduate + emp_fulltime 
                 | Year + aha_hnpi, 
                 cluster="aha_hnpi", 
                 data=sunab.dat)

mod.sa2 <- feols(log_price ~ sunab(cohort, event_time) + 
                   Surgery + Type + ccc_flag + gdr + los + any_readmit_30 + Comp_Any +
                   bdtot + labor_nurse + labor_phys + labor_residents + labor_other + 
                   total_discharges + mcaid_discharges + mcare_discharges +
                   totalpop + age_18to34 + age_35to64 + age_65plus + 
                   race_black + race_asian + race_other +
                   income_25to50 + income_50to75 + income_75to100 +
                   income_100to150 + income_150plus + educ_gradeschool +
                   educ_highschool + educ_hsgrad + educ_graduate + emp_fulltime 
                 | Year + aha_hnpi,
                 cluster="aha_hnpi", 
                 data=sunab.dat %>% filter(CH_TierAll %in% c("Tier A", "Tier B")))


mod.sa3 <- feols(log_price ~ sunab(cohort, event_time) + 
                   Surgery + Type + ccc_flag + gdr + los + any_readmit_30 + Comp_Any +
                   bdtot + labor_nurse + labor_phys + labor_residents + labor_other + 
                   total_discharges + mcaid_discharges + mcare_discharges +
                   totalpop + age_18to34 + age_35to64 + age_65plus + 
                   race_black + race_asian + race_other +
                   income_25to50 + income_50to75 + income_75to100 +
                   income_100to150 + income_150plus + educ_gradeschool +
                   educ_highschool + educ_hsgrad + educ_graduate + emp_fulltime 
                 | Year + aha_hnpi,
                 cluster="aha_hnpi", 
                 data=sunab.dat %>% filter(!CH_TierAll %in% c("Tier A", "Tier B")))

iplot(list(mod.sa1, mod.sa2, mod.sa3), ref.line=-1, xlab="Time", main="",
      ylab = "Estimate and 95% CI", pt.pch=c(20,17,15), pt.col="black", ci.col="black")
legend("bottomleft", pch=c(20,17,15), bty="n",
       legend=c("All Hospitals","CH Only","NCH Only"))
dev.copy(png,"results/f5-sa-spec.png")
dev.off()



## group-time average treatment effects
hosp.drop.spec <- claim.reg.spec %>%
  group_by(aha_hnpi) %>% 
  mutate(miss_expand=ifelse(is.na(min_specialty_expand),1,0)) %>%
  summarize(max_expand=max(min_specialty_expand), 
            min_expand=min(min_specialty_expand),
            max_miss=max(miss_expand),
            min_miss=min(miss_expand)) %>%
  mutate(bad_obs=case_when(
    !is.na(max_expand) & is.na(min_expand) ~ 1,
    is.na(max_expand) & !is.na(min_expand) ~ 1,
    max_miss!=min_miss ~ 1,
    !is.na(max_expand) & !is.na(min_expand) & min_expand!=max_expand ~ 1,
    TRUE ~ 0))

dd.pre.reg <- feols(log_price ~ Surgery + Type + ccc_flag + gdr + los + any_readmit_30 + Comp_Any +
                      bdtot + labor_nurse + labor_phys + labor_residents + labor_other + 
                      total_discharges + mcaid_discharges + mcare_discharges +
                      totalpop + age_18to34 + age_35to64 + age_65plus + 
                      race_black + race_asian + race_other +
                      income_25to50 + income_50to75 + income_75to100 +
                      income_100to150 + income_150plus + educ_gradeschool +
                      educ_highschool + educ_hsgrad + educ_graduate + emp_fulltime 
                    | Year + aha_hnpi,
                    data=claim.reg.spec)

dd.pre.reg2 <- feols(log_price ~ Surgery + Type | Year + aha_hnpi,
                    data=claim.reg.spec)

dd.claim.spec <- claim.reg.spec %>%
  add_residuals(dd.pre.reg) %>%
  add_residuals(dd.pre.reg2, var="resid2") %>%
  left_join(hosp.drop.spec, by="aha_hnpi") %>%
  filter(bad_obs==0) %>%
  group_by(aha_hnpi) %>%
  mutate(min_specialty_expand=case_when(
    !is.na(min_specialty_expand) ~ min_specialty_expand,
    is.na(min_specialty_expand) ~ 0
  )) %>%
  group_by(z_patid, fst_admtdt, last_dischdt) %>%
  mutate(person_id=cur_group_id(),
         person_count=n()) %>%
  filter(person_count==1)

## baseline without adjustment
out.claim.all <- att_gt(yname="log_price",
                        gname="min_specialty_expand",
                        idname="person_id",
                        tname="Year",
                        xformla = NULL,
                        panel=FALSE,
                        control_group="notyettreated",
                        data = dd.claim.spec %>% filter(Year>2010),
                        est_method="reg")


out.claim1 <- att_gt(yname="log_price",
                     gname="min_specialty_expand",
                     idname="person_id",
                     tname="Year",
                     xformla = NULL,
                     panel=FALSE,
                     control_group="notyettreated",
                     data = dd.claim.spec %>% filter(CH_TierAll %in% c("Tier A","Tier B"), Year>2010),
                     est_method="reg")

out.claim2 <- att_gt(yname="log_price",
                     gname="min_specialty_expand",
                     idname="person_id",
                     tname="Year",
                     xformla = NULL,
                     panel=FALSE,
                     control_group="notyettreated",
                     data = dd.claim.spec %>% filter(!CH_TierAll %in% c("Tier A","Tier B"), Year>2010),
                     est_method="reg")

event.study.all <- aggte(out.claim.all, type="dynamic", na.rm=TRUE)
low.ci.all <- round(event.study.all$overall.att - 1.96*event.study.all$overall.se,4)
high.ci.all <- round(event.study.all$overall.att + 1.96*event.study.all$overall.se,4)

event.study1 <- aggte(out.claim1, type="dynamic", na.rm=TRUE)
low.ci1 <- round(event.study1$overall.att - 1.96*event.study1$overall.se,4)
high.ci1 <- round(event.study1$overall.att + 1.96*event.study1$overall.se,4)

event.study2 <- aggte(out.claim2, type="dynamic", na.rm=TRUE)
low.ci2 <- round(event.study2$overall.att - 1.96*event.study2$overall.se,4)
high.ci2 <- round(event.study2$overall.att + 1.96*event.study2$overall.se,4)

c <- event.study.all$crit.val.egt
es.dat.all <- tibble(
  estimate=event.study.all$att.egt,
  se=event.study.all$se.egt,
  term=event.study.all$egt) %>%
  mutate(conf.low=estimate-se*c,
         conf.high=estimate+se*c,
         model=1) %>%
  select(-se)

c <- event.study1$crit.val.egt
es.dat1 <- tibble(
  estimate=event.study1$att.egt,
  se=event.study1$se.egt,
  term=event.study1$egt) %>%
  mutate(conf.low=estimate-se*c,
         conf.high=estimate+se*c,
         model=2) %>%
  select(-se)

c <- event.study2$crit.val.egt
es.dat2 <- tibble(
  estimate=event.study2$att.egt,
  se=event.study2$se.egt,
  term=event.study2$egt) %>%
  mutate(conf.low=estimate-se*c,
         conf.high=estimate+se*c,
         model=3) %>%
  select(-se)

event.study <- bind_rows(es.dat.all, es.dat1, es.dat2)
plot.es <- dwplot(event.study,
                  vline=geom_vline(xintercept=0, linetype=2),
                  vars_order = c("3","2","1",
                                 "0", "-1", "-2",
                                 "-3"),
                  whisker_args = list(aes(linetype=model), size=0.8),
                  dot_args = list(aes(shape=model), size=2),
) +
  coord_flip() + theme_bw() +
  scale_color_grey(start=0.1, end=0.1,
                   name="",
                   breaks="",
                   labels="") +
  scale_shape_discrete(name="Group",
                       breaks=c(1,2,3),
                       labels=c("All Hospitals", "CH Only", "NCH Only")) +
  labs(y = "Time",
       x = "Estimate and 95% CI",
       title = "") +
  scale_y_discrete(labels=c("-3"="t-3",
                            "-2"="t-2",
                            "-1"="t-1",
                            "0"="0",
                            "1" = "t+1",
                            "2" = "t+2",
                            "3" = "t+3")) +
  annotate(geom="text", y=5.5, x=0.12, label=paste0("Overall ATT: [",low.ci.all,", ",high.ci.all,"]"), hjust=0) +
  annotate(geom="text", y=5.5, x=0.11, label=paste0("CH ATT: [",low.ci1,", ",high.ci1,"]"), hjust=0) +
  annotate(geom="text", y=5.5, x=0.10, label=paste0("NCH ATT: [",low.ci2,", ",high.ci2,"]"), hjust=0)
ggsave("results/f6-did-base-spec.png", plot.es)




## residualized outcome
out.claim.all <- att_gt(yname="resid",
                     gname="min_specialty_expand",
                     idname="person_id",
                     tname="Year",
                     xformla = NULL,
                     panel=FALSE,
                     control_group="notyettreated",
                     data = dd.claim.spec %>% filter(Year>2010),
                     est_method="reg")


out.claim1 <- att_gt(yname="resid",
                     gname="min_specialty_expand",
                     idname="person_id",
                     tname="Year",
                     xformla = NULL,
                     panel=FALSE,
                     control_group="notyettreated",
                     data = dd.claim.spec %>% filter(CH_TierAll %in% c("Tier A","Tier B"), Year>2010),
                     est_method="reg")

out.claim2 <- att_gt(yname="resid",
                     gname="min_specialty_expand",
                     idname="person_id",
                     tname="Year",
                     xformla = NULL,
                     panel=FALSE,
                     control_group="notyettreated",
                     data = dd.claim.spec %>% filter(!CH_TierAll %in% c("Tier A","Tier B"), Year>2010),
                     est_method="reg")

event.study.all <- aggte(out.claim.all, type="dynamic", na.rm=TRUE)
low.ci.all <- round(event.study.all$overall.att - 1.96*event.study.all$overall.se,4)
high.ci.all <- round(event.study.all$overall.att + 1.96*event.study.all$overall.se,4)
dd.spec.plot <- ggdid(event.study.all) + geom_hline(yintercept=0, linetype=2) +
  theme_bw() + theme(legend.position="none") +
  labs(y = "Estimate and 95% CI",
       x = "Time",
       title = "") +
  scale_color_manual(values=c("Black","Black")) +
  annotate(geom="text", x=2.5, y=0.12, label=paste0("Overall ATT 95% CI \n[",low.ci.all,", ",high.ci.all,"]"))
ggsave("results/f6-did-event-spec-all.png", dd.spec.plot)  


event.study1 <- aggte(out.claim1, type="dynamic", na.rm=TRUE)
low.ci1 <- round(event.study1$overall.att - 1.96*event.study1$overall.se,4)
high.ci1 <- round(event.study1$overall.att + 1.96*event.study1$overall.se,4)
dd.spec.plot <- ggdid(event.study1) + geom_hline(yintercept=0, linetype=2) +
  theme_bw() + theme(legend.position="none") +
  labs(y = "Estimate and 95% CI",
       x = "Time",
       title = "") +
  scale_color_manual(values=c("Black","Black")) +
  annotate(geom="text", x=2.5, y=0.12, label=paste0("Overall ATT 95% CI \n[",low.ci1,", ",high.ci1,"]"))
ggsave("results/f6-did-event-spec1.png", dd.spec.plot)  


event.study2 <- aggte(out.claim2, type="dynamic", na.rm=TRUE)
low.ci2 <- round(event.study2$overall.att - 1.96*event.study2$overall.se,4)
high.ci2 <- round(event.study2$overall.att + 1.96*event.study2$overall.se,4)
dd.spec.plot <- ggdid(event.study2) + geom_hline(yintercept=0, linetype=2) +
  theme_bw() + theme(legend.position="none") +
  labs(y = "Estimate and 95% CI",
       x = "Time",
       title = "") +
  scale_color_manual(values=c("Black","Black")) +
  annotate(geom="text", x=2.5, y=0.10, label=paste0("Overall ATT 95% CI \n[",low.ci2,", ",high.ci2,"]"))
ggsave("results/f6-did-event-spec2.png", dd.spec.plot)  


c <- event.study.all$crit.val.egt
es.dat.all <- tibble(
  estimate=event.study.all$att.egt,
  se=event.study.all$se.egt,
  term=event.study.all$egt) %>%
  mutate(conf.low=estimate-se*c,
         conf.high=estimate+se*c,
         model=1) %>%
  select(-se)

c <- event.study1$crit.val.egt
es.dat1 <- tibble(
  estimate=event.study1$att.egt,
  se=event.study1$se.egt,
  term=event.study1$egt) %>%
  mutate(conf.low=estimate-se*c,
         conf.high=estimate+se*c,
         model=2) %>%
  select(-se)

c <- event.study2$crit.val.egt
es.dat2 <- tibble(
  estimate=event.study2$att.egt,
  se=event.study2$se.egt,
  term=event.study2$egt) %>%
  mutate(conf.low=estimate-se*c,
         conf.high=estimate+se*c,
         model=3) %>%
  select(-se)

event.study <- bind_rows(es.dat.all, es.dat1, es.dat2)
plot.es <- dwplot(event.study,
                  vline=geom_vline(xintercept=0, linetype=2),
                  vars_order = c("3","2","1",
                                 "0", "-1", "-2",
                                 "-3"),
                  whisker_args = list(aes(linetype=model), size=0.8),
                  dot_args = list(aes(shape=model), size=2),
) +
  coord_flip() + theme_bw() +
  scale_color_grey(start=0.1, end=0.1,
                   name="",
                   breaks="",
                   labels="") +
  scale_shape_discrete(name="Group",
                       breaks=c(1,2,3),
                       labels=c("All Hospitals", "CH Only", "NCH Only")) +
  labs(y = "Time",
       x = "Estimate and 95% CI",
       title = "") +
  scale_y_discrete(labels=c("-3"="t-3",
                            "-2"="t-2",
                            "-1"="t-1",
                            "0"="0",
                            "1" = "t+1",
                            "2" = "t+2",
                            "3" = "t+3")) +
  annotate(geom="text", y=5.5, x=0.12, label=paste0("Overall ATT: [",low.ci.all,", ",high.ci.all,"]"), hjust=0) +
  annotate(geom="text", y=5.5, x=0.11, label=paste0("CH ATT: [",low.ci1,", ",high.ci1,"]"), hjust=0) +
  annotate(geom="text", y=5.5, x=0.10, label=paste0("NCH ATT: [",low.ci2,", ",high.ci2,"]"), hjust=0)
ggsave("results/f6-did-event-spec.png", plot.es)



## residualized outcome (only conditioning on procedure and place FEs)
out.claim.all <- att_gt(yname="resid2",
                        gname="min_specialty_expand",
                        idname="person_id",
                        tname="Year",
                        xformla = NULL,
                        panel=FALSE,
                        control_group="notyettreated",
                        data = dd.claim.spec %>% filter(Year>2010),
                        est_method="reg")


out.claim1 <- att_gt(yname="resid2",
                     gname="min_specialty_expand",
                     idname="person_id",
                     tname="Year",
                     xformla = NULL,
                     panel=FALSE,
                     control_group="notyettreated",
                     data = dd.claim.spec %>% filter(CH_TierAll %in% c("Tier A","Tier B"), Year>2010),
                     est_method="reg")

out.claim2 <- att_gt(yname="resid2",
                     gname="min_specialty_expand",
                     idname="person_id",
                     tname="Year",
                     xformla = NULL,
                     panel=FALSE,
                     control_group="notyettreated",
                     data = dd.claim.spec %>% filter(!CH_TierAll %in% c("Tier A","Tier B"), Year>2010),
                     est_method="reg")

event.study.all <- aggte(out.claim.all, type="dynamic", na.rm=TRUE)
low.ci.all <- round(event.study.all$overall.att - 1.96*event.study.all$overall.se,4)
high.ci.all <- round(event.study.all$overall.att + 1.96*event.study.all$overall.se,4)
dd.spec.plot <- ggdid(event.study.all) + geom_hline(yintercept=0, linetype=2) +
  theme_bw() + theme(legend.position="none") +
  labs(y = "Estimate and 95% CI",
       x = "Time",
       title = "") +
  scale_color_manual(values=c("Black","Black")) +
  annotate(geom="text", x=2.5, y=0.12, label=paste0("Overall ATT 95% CI \n[",low.ci.all,", ",high.ci.all,"]"))
ggsave("results/f6-did2-event-spec-all.png", dd.spec.plot)  


event.study1 <- aggte(out.claim1, type="dynamic", na.rm=TRUE)
low.ci1 <- round(event.study1$overall.att - 1.96*event.study1$overall.se,4)
high.ci1 <- round(event.study1$overall.att + 1.96*event.study1$overall.se,4)
dd.spec.plot <- ggdid(event.study1) + geom_hline(yintercept=0, linetype=2) +
  theme_bw() + theme(legend.position="none") +
  labs(y = "Estimate and 95% CI",
       x = "Time",
       title = "") +
  scale_color_manual(values=c("Black","Black")) +
  annotate(geom="text", x=2.5, y=0.12, label=paste0("Overall ATT 95% CI \n[",low.ci1,", ",high.ci1,"]"))
ggsave("results/f6-did2-event-spec1.png", dd.spec.plot)  


event.study2 <- aggte(out.claim2, type="dynamic", na.rm=TRUE)
low.ci2 <- round(event.study2$overall.att - 1.96*event.study2$overall.se,4)
high.ci2 <- round(event.study2$overall.att + 1.96*event.study2$overall.se,4)
dd.spec.plot <- ggdid(event.study2) + geom_hline(yintercept=0, linetype=2) +
  theme_bw() + theme(legend.position="none") +
  labs(y = "Estimate and 95% CI",
       x = "Time",
       title = "") +
  scale_color_manual(values=c("Black","Black")) +
  annotate(geom="text", x=2.5, y=0.10, label=paste0("Overall ATT 95% CI \n[",low.ci2,", ",high.ci2,"]"))
ggsave("results/f6-did2-event-spec2.png", dd.spec.plot)  


c <- event.study.all$crit.val.egt
es.dat.all <- tibble(
  estimate=event.study.all$att.egt,
  se=event.study.all$se.egt,
  term=event.study.all$egt) %>%
  mutate(conf.low=estimate-se*c,
         conf.high=estimate+se*c,
         model=1) %>%
  select(-se)

c <- event.study1$crit.val.egt
es.dat1 <- tibble(
  estimate=event.study1$att.egt,
  se=event.study1$se.egt,
  term=event.study1$egt) %>%
  mutate(conf.low=estimate-se*c,
         conf.high=estimate+se*c,
         model=2) %>%
  select(-se)

c <- event.study2$crit.val.egt
es.dat2 <- tibble(
  estimate=event.study2$att.egt,
  se=event.study2$se.egt,
  term=event.study2$egt) %>%
  mutate(conf.low=estimate-se*c,
         conf.high=estimate+se*c,
         model=3) %>%
  select(-se)

event.study <- bind_rows(es.dat.all, es.dat1, es.dat2)
plot.es <- dwplot(event.study,
                  vline=geom_vline(xintercept=0, linetype=2),
                  vars_order = c("3","2","1",
                                 "0", "-1", "-2",
                                 "-3"),
                  whisker_args = list(aes(linetype=model), size=0.8),
                  dot_args = list(aes(shape=model), size=2),
) +
  coord_flip() + theme_bw() +
  scale_color_grey(start=0.1, end=0.1,
                   name="",
                   breaks="",
                   labels="") +
  scale_shape_discrete(name="Group",
                       breaks=c(1,2,3),
                       labels=c("All Hospitals", "CH Only", "NCH Only")) +
  labs(y = "Time",
       x = "Estimate and 95% CI",
       title = "") +
  scale_y_discrete(labels=c("-3"="t-3",
                            "-2"="t-2",
                            "-1"="t-1",
                            "0"="0",
                            "1" = "t+1",
                            "2" = "t+2",
                            "3" = "t+3")) +
  annotate(geom="text", y=5.5, x=0.12, label=paste0("Overall ATT: [",low.ci.all,", ",high.ci.all,"]"), hjust=0) +
  annotate(geom="text", y=5.5, x=0.11, label=paste0("CH ATT: [",low.ci1,", ",high.ci1,"]"), hjust=0) +
  annotate(geom="text", y=5.5, x=0.10, label=paste0("NCH ATT: [",low.ci2,", ",high.ci2,"]"), hjust=0)
ggsave("results/f6-did2-event-spec.png", plot.es)
