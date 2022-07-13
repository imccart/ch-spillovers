
# Meta --------------------------------------------------------------------

## Author:        Ian McCarthy
## Date Created:  10/7/2020
## Date Edited:   12/16/2021
## Description:   Analysis of CH entry/exit with minimum 5 pediatric claims per hospital/year
##                The goal of this analysis is to quantify the role of "differentiation"
##                at the hospital-level. In other words, does the entry of a CH hospital
##                non-CH hospital prices, or do they appear sufficiently differentiated
##                such that competitive effects are isolated within CH types?



# Identify market-level entry ---------------------------------------------
options(scipen=999)

## count CH by market/year to identify changes over time
claim.count <- ch.data.final %>%
  group_by(aha_hnpi, Year) %>%
  summarize(tot_ped_claims=n())

hosp.years <- hosp.only %>% ungroup() %>% expand(aha_hnpi, Year) %>%
  left_join(claim.count, by=c("aha_hnpi", "Year")) %>%
  mutate(tot_ped_claims=ifelse(is.na(tot_ped_claims),0,tot_ped_claims)) %>%
  left_join(hosp.only %>% select(aha_hnpi, Year, CH_TierAll, zip=hosp_zip), 
            by=c("aha_hnpi","Year"))

hosp.entry <- hosp.years %>%
  mutate(CH_TierAll=ifelse(is.na(CH_TierAll),"Not Present", CH_TierAll),
         hosp_a=ifelse(CH_TierAll=="Tier A",1,0),
         hosp_b=ifelse(CH_TierAll=="Tier B", 1, 0),
         hosp_c=ifelse(CH_TierAll=="Tier C",1,0),
         hosp_d=ifelse(CH_TierAll=="Tier D", 1, 0),
         hosp_other=ifelse(CH_TierAll=="Other", 1, 0),
         zip=as.numeric(zip),
         hosp_ch=hosp_a+hosp_b,
         hosp_nch=hosp_c+hosp_d+hosp_other) %>%
  group_by(aha_hnpi) %>%
  arrange(aha_hnpi, Year) %>%
  mutate(hosp_ch_lag=lag(hosp_ch),
         hosp_nch_lag=lag(hosp_nch),
         ped_claims_lag=lag(tot_ped_claims)) %>%
  mutate(hosp_ch_lag=ifelse(is.na(hosp_ch_lag),0,hosp_ch_lag),
         hosp_nch_lag=ifelse(is.na(hosp_nch_lag),0,hosp_nch_lag),
         ped_claims_lag=ifelse(is.na(ped_claims_lag),0,ped_claims_lag)) %>%
  mutate(ch_entry=ifelse(hosp_ch==1 & tot_ped_claims - ped_claims_lag >= 5, 1,0),
         nch_entry=ifelse(hosp_nch==1 & tot_ped_claims - ped_claims_lag >= 5, 1,0)) %>%
  left_join(zip.markets, by="zip") %>%
  group_by(market, Year) %>%
  summarize(ch_entry_big=sum(ch_entry, na.rm=TRUE),
            nch_entry_big=sum(nch_entry, na.rm=TRUE))

market.tiers <- market.tiers.base %>% 
  rename(ch_entry_any=ch_entry, nch_entry_any=nch_entry) %>%
  left_join(hosp.entry, by=c("market", "Year")) %>%
  mutate(ch_entry=ifelse(ch_entry_any==1 & ch_entry_big>0,1,0),
         nch_entry=ifelse(nch_entry_any==1 & nch_entry_big>0,1,0))

## identify balanced panel of hospitals
hosp.panel <- hosp.only %>%
  distinct(aha_hnpi, Year) %>%
  group_by(aha_hnpi) %>%
  summarize(min_year=min(Year, na.rm=TRUE),
            max_year=max(Year, na.rm=TRUE))

## identify year of ch entry in data
ch.entry.dat <- market.tiers %>%
  filter(ch_entry==1) %>%
  group_by(market) %>%
  summarize(min_ch_entry=min(Year))

## identify year of non-ch entry in data
nch.entry.dat <- market.tiers %>%
  filter(nch_entry==1) %>%
  group_by(market) %>%
  summarize(min_nch_entry=min(Year))

# Market-level Dataset ----------------------------------------------------

## dataset focusing on entry of new children's hospitals (tier a or tier b)
market.dat.ch <- ch.data.final %>% 
  dplyr::select(hcci_price, gdr, los, Surgery, aha_hnpi, Year, Comp_Any,
                any_readmit_30, any_readmit_60, any_readmit_90, zip=hosp_zip,
                CH_TierAll, aha_hnpi, Type) %>%
  mutate(zip=as.numeric(zip),
         Tier_A=ifelse(CH_TierAll=="Tier A",1,0),
         Tier_B=ifelse(CH_TierAll=="Tier B", 1, 0),
         Tier_C=ifelse(CH_TierAll=="Tier C",1,0),
         Other=ifelse(CH_TierAll=="Other", 1, 0)) %>%
  left_join(hosp.panel, by="aha_hnpi") %>%
  left_join(zip.markets, by="zip") %>%
  left_join(ch.entry.dat, by="market") %>%
  mutate(
    keep_obs=case_when(
      !is.na(min_ch_entry) & min_ch_entry>min_year ~ 1,
      !is.na(min_ch_entry) & min_ch_entry<=min_year & !CH_TierAll %in% c("Tier A", "Tier B") ~ 1,
      !is.na(min_ch_entry) & min_ch_entry<=min_year & CH_TierAll %in% c("Tier A", "Tier B") ~ 0,
      is.na(min_ch_entry) ~ 1,
      TRUE ~ NA_real_)) %>%
  filter(keep_obs==1) %>%
  mutate(
    ch_price=case_when(
      CH_TierAll %in% c("Tier A", "Tier B") ~ hcci_price,
      TRUE ~ NA_real_),
    nonch_price=case_when(
      !CH_TierAll %in% c("Tier A", "Tier B") ~ hcci_price,
      TRUE ~ NA_real_)) %>%
  add_count(aha_hnpi, Year, name="hosp_patients") %>%
  group_by(market, Year) %>%
  summarize(mean_price=mean(hcci_price, na.rm=TRUE),
            mean_patients=mean(hosp_patients, na.rm=TRUE),
            mean_ch_price=mean(ch_price, na.rm=TRUE),
            mean_nonch_price=mean(nonch_price, na.rm=TRUE),
            mkt_patients=n(),
            tot_readmit_30=sum(any_readmit_30, na.rm=TRUE),
            tot_readmit_60=sum(any_readmit_60, na.rm=TRUE),
            tot_readmit_90=sum(any_readmit_90, na.rm=TRUE),
            tot_comp=sum(Comp_Any, na.rm=TRUE),
            tier_a_patient=sum(Tier_A, na.rm=TRUE),
            tier_b_patient=sum(Tier_B, na.rm=TRUE),
            tier_c_patient=sum(Tier_C, na.rm=TRUE),
            tier_other_patient=sum(Other, na.rm=TRUE))


## dataset focusing on entry of new non-ch hospitals (tier c, d, or other)
market.dat.nch <- ch.data.final %>% 
  dplyr::select(hcci_price, gdr, los, Surgery, aha_hnpi, Year, Comp_Any,
                any_readmit_30, any_readmit_60, any_readmit_90, zip=hosp_zip,
                CH_TierAll, aha_hnpi, Type) %>%
  mutate(zip=as.numeric(zip),
         Tier_A=ifelse(CH_TierAll=="Tier A",1,0),
         Tier_B=ifelse(CH_TierAll=="Tier B", 1, 0),
         Tier_C=ifelse(CH_TierAll=="Tier C",1,0),
         Other=ifelse(CH_TierAll=="Other", 1, 0)) %>%
  left_join(hosp.panel, by="aha_hnpi") %>%
  left_join(zip.markets, by="zip") %>%
  left_join(nch.entry.dat, by="market") %>%
  mutate(
    keep_obs=case_when(
      !is.na(min_nch_entry) & min_nch_entry>min_year ~ 1,
      !is.na(min_nch_entry) & min_nch_entry<=min_year & !CH_TierAll %in% c("Tier A", "Tier B")  ~ 0,
      !is.na(min_nch_entry) & min_nch_entry<=min_year & CH_TierAll %in% c("Tier A", "Tier B") ~ 1,
      is.na(min_nch_entry) ~ 1,
      TRUE ~ NA_real_)) %>%
  filter(keep_obs==1) %>%
  mutate(
    ch_price=case_when(
      CH_TierAll %in% c("Tier A", "Tier B") ~ hcci_price,
      TRUE ~ NA_real_),
    nonch_price=case_when(
      !CH_TierAll %in% c("Tier A", "Tier B") ~ hcci_price,
      TRUE ~ NA_real_)) %>%
  add_count(aha_hnpi, Year, name="hosp_patients") %>%
  group_by(market, Year) %>%
  summarize(mean_price=mean(hcci_price, na.rm=TRUE),
            mean_patients=mean(hosp_patients, na.rm=TRUE),
            mean_ch_price=mean(ch_price, na.rm=TRUE),
            mean_nonch_price=mean(nonch_price, na.rm=TRUE),
            mkt_patients=n(),
            tot_readmit_30=sum(any_readmit_30, na.rm=TRUE),
            tot_readmit_60=sum(any_readmit_60, na.rm=TRUE),
            tot_readmit_90=sum(any_readmit_90, na.rm=TRUE),
            tot_comp=sum(Comp_Any, na.rm=TRUE),
            tier_a_patient=sum(Tier_A, na.rm=TRUE),
            tier_b_patient=sum(Tier_B, na.rm=TRUE),
            tier_c_patient=sum(Tier_C, na.rm=TRUE),
            tier_other_patient=sum(Other, na.rm=TRUE))


# Entry over time ---------------------------------------------------------

panel.mkt.ch <- market.dat.ch %>%
  left_join(ch.entry.dat, by="market") %>%
  mutate(ch_treat=ifelse(Year>=min_ch_entry & !is.na(min_ch_entry), 1, 0)) %>%
  group_by(market) %>%
  mutate(max_patients=max(mkt_patients, na.rm=TRUE)) %>%
  ungroup() %>%
  filter(max_patients>50, Year>=2011)

panel.ch <- panelView(mean_price~ch_treat, data=data.frame(panel.mkt.ch),
                      index=c("market","Year"), legendOff=TRUE, 
                      theme.bw=TRUE, by.timing=TRUE, xlab="Year", ylab="Market",
                      main="", color=c("white","gray"), axis.lab="time")
ggsave("results/min-5claims/f4-panelview-ch-entry.png", panel.ch)


panel.mkt.nch <- market.dat.nch %>%
  left_join(nch.entry.dat, by="market") %>%
  mutate(nch_treat=ifelse(Year>=min_nch_entry & !is.na(min_nch_entry), 1, 0)) %>%
  group_by(market) %>%
  mutate(max_patients=max(mkt_patients, na.rm=TRUE)) %>%
  ungroup() %>%
  filter(max_patients>50, Year>=2011)

panel.nch <- panelView(mean_price~nch_treat, data=data.frame(panel.mkt.nch),
                       index=c("market","Year"), legendOff=TRUE, 
                       theme.bw=TRUE, by.timing=TRUE, xlab="Year", ylab="Market",
                       main="", color=c("white","gray"), axis.lab="time")
ggsave("results/min-5claims/f4-panelview-nch-entry.png", panel.nch)



# Effects of CH entry at claim level --------------------------------------

claim.reg.ch <- ch.data.final %>%
  mutate(zip=as.numeric(hosp_zip)) %>%
  left_join(zip.markets, by=c("zip")) %>%
  left_join(ch.entry.dat, by="market") %>%
  left_join(market.tiers, by=c("market","Year")) %>%
  mutate(ch_treat=ifelse(Year>=min_ch_entry & !is.na(min_ch_entry), 1, 0),
         ever_treat=ifelse(min_ch_entry>2011 & !is.na(min_ch_entry), 1, 0)) %>%
  mutate(
    event_time=case_when(
      !is.na(min_ch_entry) ~ Year - min_ch_entry,
      is.na(min_ch_entry) ~ -1)) %>%
  mutate(time_m3=1*(event_time<=-3),
         time_m2=1*(event_time==-2),
         time_0=1*(event_time==0),
         time_p1=1*(event_time==1),
         time_p2=1*(event_time==2),
         time_p3=1*(event_time>=3)) %>%
  mutate(log_price=log(hcci_price)) 


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
                 data=claim.reg.ch)
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
                    data=claim.reg.ch %>% filter(CH_TierAll %in% c("Tier A", "Tier B")))
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
                      data=claim.reg.ch %>% filter(!CH_TierAll %in% c("Tier A", "Tier B")))
summary(mod3)

point.ch1 <- as_tibble(mod1$coefficients, rownames="term") %>%
  filter(term %in% c("time_m3", "time_m2", "time_0", "time_p1", "time_p2", "time_p3")) %>%
  rename(estimate=value)
ci.ch1 <- as_tibble(confint(mod1), rownames="term") %>%
  filter(term %in% c("time_m3", "time_m2", "time_0", "time_p1", "time_p2", "time_p3")) %>%
  rename(conf.low = `2.5 %`, conf.high = `97.5 %`)

point.ch2 <- as_tibble(mod2$coefficients, rownames="term") %>%
  filter(term %in% c("time_m3", "time_m2", "time_0", "time_p1", "time_p2", "time_p3")) %>%
  rename(estimate=value)
ci.ch2 <- as_tibble(confint(mod2), rownames="term") %>%
  filter(term %in% c("time_m3", "time_m2", "time_0", "time_p1", "time_p2", "time_p3")) %>%
  rename(conf.low = `2.5 %`, conf.high = `97.5 %`)

point.ch3 <- as_tibble(mod3$coefficients, rownames="term") %>%
  filter(term %in% c("time_m3", "time_m2", "time_0", "time_p1", "time_p2", "time_p3")) %>%
  rename(estimate=value)
ci.ch3 <- as_tibble(confint(mod3), rownames="term") %>%
  filter(term %in% c("time_m3", "time_m2", "time_0", "time_p1", "time_p2", "time_p3")) %>%
  rename(conf.low = `2.5 %`, conf.high = `97.5 %`)

new.row <- tibble(
  term= "time_m1",
  estimate=0,
  conf.low=0,
  conf.high=0,
  event_time=-1
)

est.ch1 <- point.ch1 %>%
  left_join(ci.ch1, by="term") %>%
  mutate(event_time = c(-3, -2, 0, 1, 2, 3),
         model=1) %>%
  bind_rows(new.row) %>%
  arrange(event_time)

est.ch2 <- point.ch2 %>%
  left_join(ci.ch2, by="term") %>%
  mutate(event_time = c(-3, -2, 0, 1, 2, 3),
         model=2) %>%
  bind_rows(new.row) %>%
  arrange(event_time)

est.ch3 <- point.ch3 %>%
  left_join(ci.ch3, by="term") %>%
  mutate(event_time = c(-3, -2, 0, 1, 2, 3),
         model=3) %>%
  bind_rows(new.row) %>%
  arrange(event_time)

est.ch <- rbind(est.ch1, est.ch2, est.ch3)

plot.ch <- dwplot(est.ch,
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
ggsave("results/min-5claims/f5-event-ch.png", plot.ch)


## sun and abraham approach
sunab.dat <- claim.reg.ch %>%
  mutate(cohort=ifelse(is.na(min_ch_entry),-1,0),
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
dev.copy(png,"results/min-5claims/f5-sa-ch.png")
dev.off()

## group-time average treatment effects
hosp.drop.ch <- claim.reg.ch %>%
  group_by(aha_hnpi) %>% 
  mutate(miss_entry=ifelse(is.na(min_ch_entry),1,0)) %>%
  summarize(max_entry=max(min_ch_entry), 
            min_entry=min(min_ch_entry),
            max_miss=max(miss_entry),
            min_miss=min(miss_entry)) %>%
  mutate(bad_obs=case_when(
    !is.na(max_entry) & is.na(min_entry) ~ 1,
    is.na(max_entry) & !is.na(min_entry) ~ 1,
    max_miss!=min_miss ~ 1,
    !is.na(max_entry) & !is.na(min_entry) & min_entry!=max_entry ~ 1,
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
                    data=claim.reg.ch)


dd.claim.ch <- claim.reg.ch %>%
  add_residuals(dd.pre.reg) %>%
  left_join(hosp.drop.ch, by="aha_hnpi") %>%
  filter(bad_obs==0) %>%
  group_by(aha_hnpi) %>%
  mutate(min_ch_entry=case_when(
    !is.na(min_ch_entry) ~ min_ch_entry,
    is.na(min_ch_entry) ~ 0
  )) %>%
  group_by(z_patid, fst_admtdt, last_dischdt) %>%
  mutate(person_id=cur_group_id(),
         person_count=n()) %>%
  filter(person_count==1)


out.claim.all <- att_gt(yname="resid",
                     gname="min_ch_entry",
                     idname="person_id",
                     tname="Year",
                     xformla = NULL,
                     panel=FALSE,
                     control_group="notyettreated",
                     data = dd.claim.ch %>% filter(Year>2010),
                     est_method="reg")


out.claim1 <- att_gt(yname="resid",
                     gname="min_ch_entry",
                     idname="person_id",
                     tname="Year",
                     xformla = NULL,
                     panel=FALSE,
                     control_group="notyettreated",
                     data = dd.claim.ch %>% filter(CH_TierAll %in% c("Tier A","Tier B"), Year>2010),
                     est_method="reg")

out.claim2 <- att_gt(yname="resid",
                     gname="min_ch_entry",
                     idname="person_id",
                     tname="Year",
                     xformla = NULL,
                     panel=FALSE,
                     control_group="notyettreated",
                     data = dd.claim.ch %>% filter(!CH_TierAll %in% c("Tier A","Tier B"), Year>2010),
                     est_method="reg")

event.study.all <- aggte(out.claim.all, type="dynamic", na.rm=TRUE)
low.ci.all <- round(event.study.all$overall.att - 1.96*event.study.all$overall.se,4)
high.ci.all <- round(event.study.all$overall.att + 1.96*event.study.all$overall.se,4)
dd.ch.plot <- ggdid(event.study.all) + geom_hline(yintercept=0, linetype=2) +
  theme_bw() + theme(legend.position="none") +
  labs(y = "Estimate and 95% CI",
       x = "Time",
       title = "") +
  scale_color_manual(values=c("Black","Black")) +
  annotate(geom="text", x=2.5, y=0.12, label=paste0("Overall ATT 95% CI \n[",low.ci.all,", ",high.ci.all,"]"))
ggsave("results/min-5claims/f6-did-event-ch-all.png", dd.ch.plot)  


event.study1 <- aggte(out.claim1, type="dynamic", na.rm=TRUE)
low.ci1 <- round(event.study1$overall.att - 1.96*event.study1$overall.se,4)
high.ci1 <- round(event.study1$overall.att + 1.96*event.study1$overall.se,4)
dd.ch.plot <- ggdid(event.study1) + geom_hline(yintercept=0, linetype=2) +
  theme_bw() + theme(legend.position="none") +
  labs(y = "Estimate and 95% CI",
       x = "Time",
       title = "") +
  scale_color_manual(values=c("Black","Black")) +
  annotate(geom="text", x=2.5, y=0.12, label=paste0("Overall ATT 95% CI \n[",low.ci1,", ",high.ci1,"]"))
ggsave("results/min-5claims/f6-did-event-ch1.png", dd.ch.plot)  


event.study2 <- aggte(out.claim2, type="dynamic", na.rm=TRUE)
low.ci2 <- round(event.study2$overall.att - 1.96*event.study2$overall.se,4)
high.ci2 <- round(event.study2$overall.att + 1.96*event.study2$overall.se,4)
dd.ch.plot <- ggdid(event.study2) + geom_hline(yintercept=0, linetype=2) +
  theme_bw() + theme(legend.position="none") +
  labs(y = "Estimate and 95% CI",
       x = "Time",
       title = "") +
  scale_color_manual(values=c("Black","Black")) +
  annotate(geom="text", x=2.5, y=0.12, label=paste0("Overall ATT 95% CI \n[",low.ci2,", ",high.ci2,"]"))
ggsave("results/min-5claims/f6-did-event-ch2.png", dd.ch.plot)  


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
  annotate(geom="text", y=5.5, x=0.105, label=paste0("CH ATT: [",low.ci1,", ",high.ci1,"]"), hjust=0) +
  annotate(geom="text", y=5.5, x=0.09, label=paste0("NCH ATT: [",low.ci2,", ",high.ci2,"]"), hjust=0)
ggsave("results/min-5claims/f6-did-event-ch.png", plot.es)








# Effects of NCH entry at claim level -------------------------------------

claim.reg.nch <- ch.data.final %>%
  mutate(zip=as.numeric(hosp_zip)) %>%
  left_join(zip.markets, by=c("zip")) %>%
  left_join(nch.entry.dat, by="market") %>%
  left_join(market.tiers, by=c("market","Year")) %>%
  mutate(nch_treat=ifelse(Year>=min_nch_entry & !is.na(min_nch_entry), 1, 0),
         ever_treat=ifelse(min_nch_entry>2011 & !is.na(min_nch_entry), 1, 0)) %>%
  mutate(
    event_time=case_when(
      !is.na(min_nch_entry) ~ Year - min_nch_entry,
      is.na(min_nch_entry) ~ -1)) %>%
  mutate(time_m3=1*(event_time<=-3),
         time_m2=1*(event_time==-2),
         time_0=1*(event_time==0),
         time_p1=1*(event_time==1),
         time_p2=1*(event_time==2),
         time_p3=1*(event_time>=3)) %>%
  mutate(log_price=log(hcci_price)) 


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
              data=claim.reg.nch)
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
              data=claim.reg.nch %>% filter(CH_TierAll %in% c("Tier A", "Tier B")))
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
              data=claim.reg.nch %>% filter(!CH_TierAll %in% c("Tier A", "Tier B")))
summary(mod3)

point.nch1 <- as_tibble(mod1$coefficients, rownames="term") %>%
  filter(term %in% c("time_m3", "time_m2", "time_0", "time_p1", "time_p2", "time_p3")) %>%
  rename(estimate=value)
ci.nch1 <- as_tibble(confint(mod1), rownames="term") %>%
  filter(term %in% c("time_m3", "time_m2", "time_0", "time_p1", "time_p2", "time_p3")) %>%
  rename(conf.low = `2.5 %`, conf.high = `97.5 %`)

point.nch2 <- as_tibble(mod2$coefficients, rownames="term") %>%
  filter(term %in% c("time_m3", "time_m2", "time_0", "time_p1", "time_p2", "time_p3")) %>%
  rename(estimate=value)
ci.nch2 <- as_tibble(confint(mod2), rownames="term") %>%
  filter(term %in% c("time_m3", "time_m2", "time_0", "time_p1", "time_p2", "time_p3")) %>%
  rename(conf.low = `2.5 %`, conf.high = `97.5 %`)

point.nch3 <- as_tibble(mod3$coefficients, rownames="term") %>%
  filter(term %in% c("time_m3", "time_m2", "time_0", "time_p1", "time_p2", "time_p3")) %>%
  rename(estimate=value)
ci.nch3 <- as_tibble(confint(mod3), rownames="term") %>%
  filter(term %in% c("time_m3", "time_m2", "time_0", "time_p1", "time_p2", "time_p3")) %>%
  rename(conf.low = `2.5 %`, conf.high = `97.5 %`)

new.row <- tibble(
  term= "time_m1",
  estimate=0,
  conf.low=0,
  conf.high=0,
  event_time=-1
)

est.nch1 <- point.nch1 %>%
  left_join(ci.nch1, by="term") %>%
  mutate(event_time = c(-3, -2, 0, 1, 2, 3),
         model=1) %>%
  bind_rows(new.row) %>%
  arrange(event_time)

est.nch2 <- point.nch2 %>%
  left_join(ci.nch2, by="term") %>%
  mutate(event_time = c(-3, -2, 0, 1, 2, 3),
         model=2) %>%
  bind_rows(new.row) %>%
  arrange(event_time)

est.nch3 <- point.nch3 %>%
  left_join(ci.nch3, by="term") %>%
  mutate(event_time = c(-3, -2, 0, 1, 2, 3),
         model=3) %>%
  bind_rows(new.row) %>%
  arrange(event_time)

est.nch <- rbind(est.nch1, est.nch2, est.nch3)

plot.nch <- dwplot(est.nch,
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
ggsave("results/min-5claims/f5-event-nch.png", plot.nch)


## sun and abraham approach
sunab.dat <- claim.reg.nch %>%
  mutate(cohort=ifelse(is.na(min_nch_entry),-1,0),
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
dev.copy(png,"results/min-5claims/f5-sa-nch.png")
dev.off()



## group-time average treatment effects
hosp.drop.nch <- claim.reg.nch %>%
  group_by(aha_hnpi) %>% 
  mutate(miss_entry=ifelse(is.na(min_nch_entry),1,0)) %>%
  summarize(max_entry=max(min_nch_entry), 
            min_entry=min(min_nch_entry),
            max_miss=max(miss_entry),
            min_miss=min(miss_entry)) %>%
  mutate(bad_obs=case_when(
    !is.na(max_entry) & is.na(min_entry) ~ 1,
    is.na(max_entry) & !is.na(min_entry) ~ 1,
    max_miss!=min_miss ~ 1,
    !is.na(max_entry) & !is.na(min_entry) & min_entry!=max_entry ~ 1,
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
                    data=claim.reg.nch)

dd.claim.nch <- claim.reg.nch %>%
  add_residuals(dd.pre.reg) %>%
  left_join(hosp.drop.nch, by="aha_hnpi") %>%
  filter(bad_obs==0) %>%
  group_by(aha_hnpi) %>%
  mutate(min_nch_entry=case_when(
    !is.na(min_nch_entry) ~ min_nch_entry,
    is.na(min_nch_entry) ~ 0
  )) %>%
  group_by(z_patid, fst_admtdt, last_dischdt) %>%
  mutate(person_id=cur_group_id(),
         person_count=n()) %>%
  filter(person_count==1)


out.claim.all <- att_gt(yname="resid",
                     gname="min_nch_entry",
                     idname="person_id",
                     tname="Year",
                     xformla = NULL,
                     panel=FALSE,
                     control_group="notyettreated",
                     data = dd.claim.nch %>% filter(Year>2010),
                     est_method="reg")


out.claim1 <- att_gt(yname="resid",
                    gname="min_nch_entry",
                    idname="person_id",
                    tname="Year",
                    xformla = NULL,
                    panel=FALSE,
                    control_group="notyettreated",
                    data = dd.claim.nch %>% filter(CH_TierAll %in% c("Tier A","Tier B"), Year>2010),
                    est_method="reg")

out.claim2 <- att_gt(yname="resid",
                     gname="min_nch_entry",
                     idname="person_id",
                     tname="Year",
                     xformla = NULL,
                     panel=FALSE,
                     control_group="notyettreated",
                     data = dd.claim.nch %>% filter(!CH_TierAll %in% c("Tier A","Tier B"), Year>2010),
                     est_method="reg")


event.study.all <- aggte(out.claim.all, type="dynamic", na.rm=TRUE)
low.ci.all <- round(event.study.all$overall.att - 1.96*event.study.all$overall.se,4)
high.ci.all <- round(event.study.all$overall.att + 1.96*event.study.all$overall.se,4)
dd.nch.plot <- ggdid(event.study.all) + geom_hline(yintercept=0, linetype=2) +
  theme_bw() + theme(legend.position="none") +
  labs(y = "Estimate and 95% CI",
       x = "Time",
       title = "") +
  scale_color_manual(values=c("Black","Black")) +
  annotate(geom="text", x=2.5, y=0.12, label=paste0("Overall ATT 95% CI \n[",low.ci.all,", ",high.ci.all,"]"))
ggsave("results/min-5claims/f6-did-event-nch-all.png", dd.nch.plot)  


event.study1 <- aggte(out.claim1, type="dynamic", na.rm=TRUE)
low.ci1 <- round(event.study1$overall.att - 1.96*event.study1$overall.se,4)
high.ci1 <- round(event.study1$overall.att + 1.96*event.study1$overall.se,4)
dd.nch.plot <- ggdid(event.study1) + geom_hline(yintercept=0, linetype=2) +
  theme_bw() + theme(legend.position="none") +
  labs(y = "Estimate and 95% CI",
       x = "Time",
       title = "") +
  scale_color_manual(values=c("Black","Black")) +
  annotate(geom="text", x=2.5, y=0.12, label=paste0("Overall ATT 95% CI \n[",low.ci1,", ",high.ci1,"]"))
ggsave("results/min-5claims/f6-did-event-nch1.png", dd.nch.plot)  


event.study2 <- aggte(out.claim2, type="dynamic", na.rm=TRUE)
low.ci2 <- round(event.study2$overall.att - 1.96*event.study2$overall.se,4)
high.ci2 <- round(event.study2$overall.att + 1.96*event.study2$overall.se,4)
dd.nch.plot <- ggdid(event.study2) + geom_hline(yintercept=0, linetype=2) +
  theme_bw() + theme(legend.position="none") +
  labs(y = "Estimate and 95% CI",
       x = "Time",
       title = "") +
  scale_color_manual(values=c("Black","Black")) +
  annotate(geom="text", x=2.5, y=0.10, label=paste0("Overall ATT 95% CI \n[",low.ci2,", ",high.ci2,"]"))
ggsave("results/min-5claims/f6-did-event-nch2.png", dd.nch.plot)  


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
  annotate(geom="text", y=5.5, x=0.105, label=paste0("CH ATT: [",low.ci1,", ",high.ci1,"]"), hjust=0) +
  annotate(geom="text", y=5.5, x=0.09, label=paste0("NCH ATT: [",low.ci2,", ",high.ci2,"]"), hjust=0)
ggsave("results/min-5claims/f6-did-event-nch.png", plot.es)

