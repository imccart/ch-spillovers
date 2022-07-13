
# Meta --------------------------------------------------------------------

## Author:        Ian McCarthy
## Date Created:  10/6/2020
## Date Edited:   5/10/2021
## Description:   Summary statistics for individual, hospital, and choice data



# Hospital summary stats --------------------------------------------------

## counts of observations (years) by hospital
hosp.count <- hosp.data %>%
  dplyr::select(aha_hnpi) %>%
  add_count(aha_hnpi,name="tot_years")

hosp.count <- hosp.count %>%
  group_by(aha_hnpi) %>%
  dplyr::mutate(obs_count=row_number()) %>%
  filter(obs_count==1)

## overall histogram
hosp.count.plot <- hosp.count %>% 
  ggplot() + geom_bar(mapping=aes(factor(tot_years))) +
  xlab("Number of Years Observed") + 
  ylab("Count of Hospitals") + scale_y_continuous(labels=comma) +
  geom_text(stat='count', aes(x=tot_years, label = comma(stat(count)), vjust=-0.3)) +
  theme_bw()
ggsave("results/fxx-hosp-years-total.png", hosp.count.plot)

## bar plot of CH/NCH counts by year
ch.count.plot <- hosp.data %>% mutate(Hospital_Type=case_when(
  CH_TierAll %in% c("Tier A", "Tier B") ~ "CH",
  !CH_TierAll %in% c("Tier A", "Tier B") ~ "NCH",
  TRUE ~ NA_character_)) %>%
  filter(tot_patients>5) %>%
  ggplot() + geom_bar(mapping=aes(x=Year, fill=factor(Hospital_Type))) +
  labs(
    x="Year",
    y="Number of Hospitals \n (5+ patients)",
    fill= ""
  ) + scale_y_continuous(labels=comma) + 
  scale_x_continuous(breaks=c(2010,2011,2012,2013,2014,2015)) + scale_fill_grey() +
  geom_text(stat='count', 
            aes(x=Year, group=Hospital_Type, 
                label = scales::comma(round(..count..),1)),
            position="stack",
            color="white",vjust=1.5) +
  theme_bw()
ggsave("results/f1-hospital-counts.png", ch.count.plot)


## hospital-level summary statistics
sum.data.ch <- hosp.data %>% ungroup() %>%
  filter(CH_TierAll %in% c("Tier A", "Tier B")) %>%
  summarize(across(c("hcci_price","adj_price",
                 "Comp_Any","any_readmit_90",
                 "bdtot","nonprofit","teaching","system",
                 "labor_nurse","labor_phys","total_discharges",
                 "mcare_discharges","mcaid_discharges","tot_patients"),
               list(Mean=mean, SD=sd, N=~n(),
                    q1=~quantile(.,probs=0.10, na.rm=TRUE),
                    q9=~quantile(.,probs=0.90, na.rm=TRUE)), 
               na.rm=TRUE,
               .names="{col}_{fn}"))

sum.data.nch <- hosp.data %>% ungroup() %>%
    filter(! CH_TierAll %in% c("Tier A", "Tier B")) %>%
  summarize(across(c("hcci_price","adj_price",
                     "Comp_Any","any_readmit_90",
                     "bdtot","nonprofit","teaching","system",
                     "labor_nurse","labor_phys","total_discharges",
                     "mcare_discharges","mcaid_discharges","tot_patients"),
                   list(Mean=mean, SD=sd, N=~n(), 
                        q1=~quantile(.,probs=0.10, na.rm=TRUE),
                        q9=~quantile(.,probs=0.90, na.rm=TRUE)), 
                   na.rm=TRUE,
                   .names="{col}_{fn}"))

sum.data.all <- hosp.data %>% ungroup() %>%
  summarize(across(c("hcci_price","adj_price",
                     "Comp_Any","any_readmit_90",
                     "bdtot","nonprofit","teaching","system",
                     "labor_nurse","labor_phys","total_discharges",
                     "mcare_discharges","mcaid_discharges","tot_patients"),
                   list(Mean=mean, SD=sd, N=~n(),
                        q1=~quantile(.,probs=0.10, na.rm=TRUE),
                        q9=~quantile(.,probs=0.90, na.rm=TRUE)), 
                   na.rm=TRUE,
                   .names="{col}_{fn}"))


count.data.ch <- sum.data.ch %>%
  select(ends_with("_N")) %>%
  pivot_longer(cols=ends_with("_N"),
               values_to="N_ch") %>%
  mutate(name=str_remove(name,"_N"))

count.data.nch <- sum.data.nch %>%
  select(ends_with("_N")) %>%
  pivot_longer(cols=ends_with("_N"),
               values_to="N_nch") %>%
  mutate(name=str_remove(name,"_N"))

    
mean.data.ch <- sum.data.ch %>%
  select(ends_with("_Mean")) %>%
  pivot_longer(cols=ends_with("_Mean"),
               values_to="mean_ch") %>%
  mutate(name=str_remove(name,"_Mean"))
  
mean.data.nch <- sum.data.nch %>%
  select(ends_with("_Mean")) %>%
  pivot_longer(cols=ends_with("_Mean"),
               values_to="mean_nch") %>%
  mutate(name=str_remove(name,"_Mean"))

sd.data.ch <- sum.data.ch %>%
  select(ends_with("_SD")) %>%
  pivot_longer(cols=ends_with("_SD"),
               values_to="std_ch") %>%
  mutate(name=str_remove(name,"_SD"))

sd.data.nch <- sum.data.nch %>%
  select(ends_with("_SD")) %>%
  pivot_longer(cols=ends_with("_SD"),
               values_to="std_nch") %>%
  mutate(name=str_remove(name,"_SD"))

q1.data.ch <- sum.data.ch %>%
  select(ends_with("_q1")) %>%
  pivot_longer(cols=ends_with("_q1"),
               values_to="q1_ch") %>%
  mutate(name=str_remove(name,"_q1"))

q1.data.nch <- sum.data.nch %>%
  select(ends_with("_q1")) %>%
  pivot_longer(cols=ends_with("_q1"),
               values_to="q1_nch") %>%
  mutate(name=str_remove(name,"_q1"))

q9.data.ch <- sum.data.ch %>%
  select(ends_with("_q9")) %>%
  pivot_longer(cols=ends_with("_q9"),
               values_to="q9_ch") %>%
  mutate(name=str_remove(name,"_q9"))

q9.data.nch <- sum.data.nch %>%
  select(ends_with("_q9")) %>%
  pivot_longer(cols=ends_with("_q9"),
               values_to="q9_nch") %>%
  mutate(name=str_remove(name,"_q9"))

hospital.summary <- mean.data.ch %>%
  left_join(sd.data.ch, by=c("name")) %>%
  left_join(q1.data.ch, by=c("name")) %>%  
  left_join(q9.data.ch, by=c("name")) %>%
  left_join(count.data.ch, by=c("name")) %>%  
  left_join(mean.data.nch, by=c("name")) %>%
  left_join(sd.data.nch, by=c("name")) %>%  
  left_join(q1.data.nch, by=c("name")) %>%
  left_join(q9.data.nch, by=c("name")) %>%
  left_join(count.data.nch, by=c("name")) %>%
  mutate(name=case_when(
    name=="hcci_price" ~ "Price",
    name=="adj_price" ~ "Adjusted Price",
    name=="Comp_Any" ~ "Complication Rate",
    name=="any_readmit_90" ~ "Readmission Rate",
    name=="bdtot" ~ "Bed Size",
    name=="nonprofit" ~ "Nonprofit",
    name=="teaching" ~ "Teaching Status",
    name=="system" ~ "System Status",
    name=="labor_nurse" ~ "Nurse FTEs",
    name=="labor_phys" ~ "Physician FTEs",
    name=="total_discharges" ~ "Total Discharges",
    name=="mcare_discharges" ~ "Medicare Discharges",
    name=="mcaid_discharges" ~ "Medicaid Discharges",
    name=="tot_patients" ~ "HCCI Patients"
  ))
  
kable(hospital.summary, format="latex",
      col.names=c("Variable","Mean","St. Dev.", "10th Pctl", "90th Pctl","Count","Mean","St. Dev.", "10th Pctl", "90th Pctl","Count"),
      digits=c(0,2,2,2,2,2,2,2,2,2,2),
      format.args=list(big.mark=","),
      booktabs=T) %>%
  add_header_above(c(" "=1, "Children's Hospitals"=5,"Non-Childen's Hospitals"=5)) %>%
  kable_styling(latex_options=c("HOLD_position")) %>%
  save_kable("results/t1_hospital_summaries.tex")

kable(hospital.summary, format="html",
      col.names=c("Variable","Mean","St. Dev.", "10th Pctl", "90th Pctl","Count","Mean","St. Dev.", "10th Pctl", "90th Pctl","Count"),
      digits=c(0,2,2,2,2,2,2,2,2,2,2),
      format.args=list(big.mark=","),
      booktabs=T) %>%
  add_header_above(c(" "=1, "Children's Hospitals"=5,"Non-Childen's Hospitals"=5)) %>%
  kable_styling(latex_options=c("HOLD_position")) %>%
  save_kable("results/t1_hospital_summaries.html")



price.plot <- hosp.data %>% 
  ggplot(aes(CH_TierAll,adj_price)) + geom_boxplot() +
  xlab("Children's Hospital Tier") +
  ylab("Hospital-level Adjusted Price ($)") + scale_y_continuous(labels=comma) +
  theme_bw()
ggsave("results/fxx-hospital-price-boxplot.png", price.plot)

comp.plot <- hosp.data %>% 
  ggplot(aes(CH_Tier,adj_comp)) + geom_boxplot() +
  xlab("Children's Hospital Tier") +
  ylab("Hospital-level Adjusted Complications") + 
  scale_y_continuous(labels=scales::number_format(accuracy=0.01)) +
  theme_bw()
ggsave("results/fxx-hospital-comp-boxplot.png", comp.plot)

readmit.plot <- hosp.data %>% 
  ggplot(aes(CH_Tier,adj_readmit)) + geom_boxplot() +
  xlab("Children's Hospital Tier") +
  ylab("Hospital-level Adjusted Readmissions") + 
  scale_y_continuous(labels=scales::number_format(accuracy=0.01)) +
  theme_bw()
ggsave("results/fxx-hospital-readmit-boxplot.png", readmit.plot)



# Claims-level summary stats ----------------------------------------------

ch.overall.summary <- ch.data.final %>%
  ungroup() %>%
  filter(CH_TierAll %in% c("Tier A", "Tier B")) %>%
  mutate(IP=if_else(Type=="Inpatient",1,0)) %>%
  summarize(mean_price=mean(hcci_price, na.rm=TRUE),
            sd_price=sd(hcci_price, na.rm=TRUE),
            mean_readmit=mean(any_readmit_90, na.rm=TRUE),
            mean_comp=mean(Comp_Any, na.rm=TRUE),
            share_IP=mean(IP, na.rm=TRUE),
            share_female=mean(female, na.rm=TRUE),
            share_ccc=mean(ccc_flag, na.rm=TRUE),
            count=n()) %>%
  mutate(across(c(mean_readmit,mean_comp,share_IP,share_female,share_ccc), ~ifelse(.*count<10 | (1-.)*count<10,NA,.))) %>%
  mutate(Surgery="All")

ch.summary <- ch.data.final %>%
  filter(CH_TierAll %in% c("Tier A", "Tier B")) %>%  
  group_by(Surgery) %>%
  mutate(IP=if_else(Type=="Inpatient",1,0)) %>%
  summarize(mean_price=mean(hcci_price, na.rm=TRUE),
            sd_price=sd(hcci_price, na.rm=TRUE),
            mean_readmit=mean(any_readmit_90, na.rm=TRUE),
            mean_comp=mean(Comp_Any, na.rm=TRUE),
            share_IP=mean(IP, na.rm=TRUE),
            share_female=mean(female, na.rm=TRUE),
            share_ccc=mean(ccc_flag, na.rm=TRUE),
            count=n()) %>%
  mutate(across(c(mean_readmit,mean_comp,share_IP,share_female,share_ccc), ~ifelse(.*count<10 | (1-.)*count<10,NA,.))) %>%
  mutate(Surgery=case_when(
    Surgery=="AntiReflux" ~ "Anti-Reflux",
    Surgery=="Appendectomy" ~ "Appendectomy",
    Surgery=="Circumcision" ~ "Circumcision",
    Surgery=="Double" ~ "2+ procedures",
    Surgery=="Gallbladder" ~ "Gallbladder Removal",
    Surgery=="Humerus" ~ "Broken Arm",
    Surgery=="IngHernia" ~ "Inguinal Hernia",
    Surgery=="Knee" ~ "Knee Arthroscopy",
    Surgery=="Orchiopexy" ~ "Orchiopexy",
    Surgery=="Spine" ~ "Spine",
    Surgery=="Strabismus" ~ "Strabismus",
    Surgery=="Tonsils" ~ "Tonsillectomy",
    Surgery=="Tympanostomy" ~ "Ear Tubes",
    Surgery=="UmbHernia" ~ "Umbilical Hernia"
  )) %>%
  arrange(-count) %>%
  bind_rows(ch.overall.summary)


nch.overall.summary <- ch.data.final %>%
  ungroup() %>%
  filter(CH_TierAll %in% c("Tier C", "Tier D")) %>%
  mutate(IP=if_else(Type=="Inpatient",1,0)) %>%
  summarize(mean_price=mean(hcci_price, na.rm=TRUE),
            sd_price=sd(hcci_price, na.rm=TRUE),
            mean_readmit=mean(any_readmit_90, na.rm=TRUE),
            mean_comp=mean(Comp_Any, na.rm=TRUE),
            share_IP=mean(IP, na.rm=TRUE),
            share_female=mean(female, na.rm=TRUE),
            share_ccc=mean(ccc_flag, na.rm=TRUE),
            count=n()) %>%
  mutate(across(c(mean_readmit,mean_comp,share_IP,share_female,share_ccc), ~ifelse(.*count<10 | (1-.)*count<10,NA,.))) %>%
  mutate(Surgery="All")

nch.summary <- ch.data.final %>%
  filter(CH_TierAll %in% c("Tier C", "Tier D")) %>%  
  group_by(Surgery) %>%
  mutate(IP=if_else(Type=="Inpatient",1,0)) %>%
  summarize(mean_price=mean(hcci_price, na.rm=TRUE),
            sd_price=sd(hcci_price, na.rm=TRUE),
            mean_readmit=mean(any_readmit_90, na.rm=TRUE),
            mean_comp=mean(Comp_Any, na.rm=TRUE),
            share_IP=mean(IP, na.rm=TRUE),
            share_female=mean(female, na.rm=TRUE),
            share_ccc=mean(ccc_flag, na.rm=TRUE),
            count=n()) %>%
  mutate(across(c(mean_readmit,mean_comp,share_IP,share_female,share_ccc), ~ifelse(.*count<10 | (1-.)*count<10,NA,.))) %>%
  mutate(Surgery=case_when(
    Surgery=="AntiReflux" ~ "Anti-Reflux",
    Surgery=="Appendectomy" ~ "Appendectomy",
    Surgery=="Circumcision" ~ "Circumcision",
    Surgery=="Double" ~ "2+ procedures",
    Surgery=="Gallbladder" ~ "Gallbladder Removal",
    Surgery=="Humerus" ~ "Broken Arm",
    Surgery=="IngHernia" ~ "Inguinal Hernia",
    Surgery=="Knee" ~ "Knee Arthroscopy",
    Surgery=="Orchiopexy" ~ "Orchiopexy",
    Surgery=="Spine" ~ "Spine",
    Surgery=="Strabismus" ~ "Strabismus",
    Surgery=="Tonsils" ~ "Tonsillectomy",
    Surgery=="Tympanostomy" ~ "Ear Tubes",
    Surgery=="UmbHernia" ~ "Umbilical Hernia"
  )) %>%
  arrange(-count) %>%
  bind_rows(nch.overall.summary)

overall.summary <- ch.data.final %>%
  ungroup() %>%
  mutate(IP=if_else(Type=="Inpatient",1,0)) %>%
  summarize(mean_price=mean(hcci_price, na.rm=TRUE),
            sd_price=sd(hcci_price, na.rm=TRUE),
            mean_readmit=mean(any_readmit_90, na.rm=TRUE),
            mean_comp=mean(Comp_Any, na.rm=TRUE),
            share_IP=mean(IP, na.rm=TRUE),
            share_female=mean(female, na.rm=TRUE),
            share_ccc=mean(ccc_flag, na.rm=TRUE),
            count=n()) %>%
  mutate(across(c(mean_readmit,mean_comp,share_IP,share_female,share_ccc), ~ifelse(.*count<10 | (1-.)*count<10,NA,.))) %>%
  mutate(Surgery="All")

summary <- ch.data.final %>%
  group_by(Surgery) %>%
  mutate(IP=if_else(Type=="Inpatient",1,0)) %>%
  summarize(mean_price=mean(hcci_price, na.rm=TRUE),
            sd_price=sd(hcci_price, na.rm=TRUE),
            mean_readmit=mean(any_readmit_90, na.rm=TRUE),
            mean_comp=mean(Comp_Any, na.rm=TRUE),
            share_IP=mean(IP, na.rm=TRUE),
            share_female=mean(female, na.rm=TRUE),
            share_ccc=mean(ccc_flag, na.rm=TRUE),
            count=n()) %>%
  mutate(across(c(mean_readmit,mean_comp,share_IP,share_female,share_ccc), ~ifelse(.*count<10 | (1-.)*count<10,NA,.))) %>%
  mutate(Surgery=case_when(
    Surgery=="AntiReflux" ~ "Anti-Reflux",
    Surgery=="Appendectomy" ~ "Appendectomy",
    Surgery=="Circumcision" ~ "Circumcision",
    Surgery=="Double" ~ "2+ procedures",
    Surgery=="Gallbladder" ~ "Gallbladder Removal",
    Surgery=="Humerus" ~ "Broken Arm",
    Surgery=="IngHernia" ~ "Inguinal Hernia",
    Surgery=="Knee" ~ "Knee Arthroscopy",
    Surgery=="Orchiopexy" ~ "Orchiopexy",
    Surgery=="Spine" ~ "Spine",
    Surgery=="Strabismus" ~ "Strabismus",
    Surgery=="Tonsils" ~ "Tonsillectomy",
    Surgery=="Tympanostomy" ~ "Ear Tubes",
    Surgery=="UmbHernia" ~ "Umbilical Hernia"
  )) %>%
  arrange(-count) %>%
  bind_rows(overall.summary)


opts <- options(knitr.kable.NA="n/a")
kable(summary, format="latex",
      col.names=c("Surgery","Mean Price","St. Dev. Price", "Readmission","Complication","Inpatient","Female","CCC","Count"),
      digits=c(0,0,0,3,3,3,3,3,0),
      format.args=list(big.mark=","),
      booktabs=T) %>%
  kable_styling(latex_options=c("HOLD_position")) %>%
  footnote(general = "Summary statistics for prices, 90-day readmissions, 90-day complications, procedures performed in the inpatient (versus the outpatient) setting, and patient characteristics including gender and presence of 'pediatric complex chronic conditions' (ccc).") %>%
  save_kable("results/t2_all_surgery_summaries.tex")

kable(summary, format="html",
      col.names=c("Surgery","Mean Price","St. Dev. Price", "Readmission","Complication","Inpatient","Female","CCC","Count"),
      digits=c(0,0,0,3,3,3,3,3,0),
      format.args=list(big.mark=","),
      booktabs=T) %>%
  kable_styling(latex_options=c("HOLD_position")) %>%
  footnote(general = "Summary statistics for prices, 90-day readmissions, 90-day complications, procedures performed in the inpatient (versus the outpatient) setting, and patient characteristics including gender and presence of 'pediatric complex chronic conditions' (ccc).") %>%
  save_kable("results/t2_all_surgery_summaries.html")


ch.nch.summary <- bind_rows(ch.summary, nch.summary)
kable(ch.nch.summary, format="latex",
      col.names=c("Surgery","Mean Price","St. Dev. Price", "Readmission","Complication","Inpatient","Female","CCC","Count"),
      digits=c(0,0,0,3,3,3,3,3,0),
      format.args=list(big.mark=","),
      booktabs=T) %>%
  kable_styling(latex_options=c("HOLD_position")) %>%
  pack_rows("Children's Hospitals", 1, 15) %>%
  pack_rows("Non-Children's Hospitals", 16, 30) %>%
  footnote(general = "Summary statistics for prices, 90-day readmissions, 90-day complications, procedures performed in the inpatient (versus the outpatient) setting, and patient characteristics including gender and presence of 'pediatric complex chronic conditions' (ccc).") %>% 
  save_kable("results/t2_ch_surgery_summaries.tex")

kable(ch.nch.summary, format="html",
      col.names=c("Surgery","Mean Price","St. Dev. Price", "Readmission","Complication","Inpatient","Female","CCC","Count"),
      digits=c(0,0,0,3,3,3,3,3,0),
      format.args=list(big.mark=","),
      booktabs=T) %>%
  kable_styling(latex_options=c("HOLD_position")) %>%
  pack_rows("Children's Hospitals", 1, 15) %>%
  pack_rows("Non-Children's Hospitals", 16, 30) %>%
  footnote(general = "Summary statistics for prices, 90-day readmissions, 90-day complications, procedures performed in the inpatient (versus the outpatient) setting, and patient characteristics including gender and presence of 'pediatric complex chronic conditions' (ccc).") %>% 
  save_kable("results/t2_ch_surgery_summaries.html")




# Market summary stats ----------------------------------------------------

market.hosp <- hosp.only %>% 
  mutate(zip=as.numeric(hosp_zip)) %>%
  left_join(zip.markets, by=c("zip")) %>%
  group_by(market,Year) %>%
  summarize(tot_cha=sum((CH_TierAll=="Tier A")),
            tot_chb=sum((CH_TierAll=="Tier B")),
            tot_chc=sum((CH_TierAll=="Tier C"))) %>%
  mutate(tot_ch=tot_cha+tot_chb) %>%
  group_by(market) %>%
  mutate(ever_ch=max(tot_ch))

mkt.total <- ch.data.final %>%
  mutate(zip=as.numeric(hosp_zip)) %>%
  left_join(zip.markets, by=c("zip")) %>%  
  group_by(market,Year) %>%
  summarize(tot_patients=n())

mkt.tiera <- ch.data.final %>%
  mutate(zip=as.numeric(hosp_zip)) %>%
  left_join(zip.markets, by=c("zip")) %>%  
  group_by(market,Year) %>%
  filter(CH_TierAll=="Tier A") %>%
  summarize(tot_tiera=n())

mkt.tierb <- ch.data.final %>%
  mutate(zip=as.numeric(hosp_zip)) %>%
  left_join(zip.markets, by=c("zip")) %>%  
  group_by(market,Year) %>%
  filter(CH_TierAll=="Tier B") %>%
  summarize(tot_tierb=n())

mkt.tierc <- ch.data.final %>%
  mutate(zip=as.numeric(hosp_zip)) %>%
  left_join(zip.markets, by=c("zip")) %>%  
  group_by(market,Year) %>%
  filter(CH_TierAll=="Tier C") %>%
  summarize(tot_tierc=n())

mkt.counts <- market.hosp %>%
  left_join(mkt.total,by=c("market","Year")) %>%
  left_join(mkt.tiera,by=c("market","Year")) %>%
  left_join(mkt.tierb,by=c("market","Year")) %>%
  left_join(mkt.tierc,by=c("market","Year")) %>%
  group_by(market) %>%
  mutate(max_patients=max(tot_patients)) %>%
  mutate(mkt_count=n()) %>%
  ungroup()

mkt.counts <- mkt.counts %>%
  mutate_at(c("tot_patients","tot_tiera","tot_tierb","tot_tierc"),
            list(~ ifelse(is.na(.),0,.)))

mkt.counts <- mkt.counts %>%
  filter(tot_patients>0) %>%
  mutate(share_tiera=tot_tiera/tot_patients,
         share_tierb=tot_tierb/tot_patients,
         share_tierab=(tot_tiera+tot_tierb)/tot_patients)


# overall shares by tier a and b
mkt1 <- mkt.counts %>%
  group_by(Year) %>%
  summarize(mean_share=mean(share_tiera)) %>%
  mutate(Type="Freestanding CH")

mkt2 <- mkt.counts %>%
  group_by(Year) %>%
  summarize(mean_share=mean(share_tierb)) %>%
  mutate(Type="Designated Children's Unit")

mkt.mean <- mkt1 %>% bind_rows(mkt2)

share.plot <- ggplot() + geom_line(data=mkt.mean, aes(Year, mean_share, linetype=Type))+
  geom_text(data=mkt.mean %>% filter(Year==last(Year)),aes(label=Type,
                                                           x=Year-.5,
                                                           y=mean_share),
            vjust=-0.5) +
  theme_bw() + ylab("Average Share of Patients") + xlab("Year") +
  scale_y_continuous(limits=c(0,0.20)) +
  guides(linetype=FALSE)
ggsave("results/fxx-tierab-shares-mkt.png", share.plot)

mkt.ch <- mkt.counts %>%
  filter(ever_ch>0) %>%  
  group_by(Year) %>%
  summarize(mean_share=mean(share_tierab)) %>%
  mutate(Type="Average for All Markets")

share.plot <- ggplot() + geom_line(data=mkt.ch, aes(Year, mean_share))+
  theme_bw() + ylab("Average Share of Patients") + xlab("Year") +
  scale_y_continuous(limits=c(.30,0.60)) +
  guides(linetype=FALSE)
ggsave("results/fxx-ch-shares-mkt.png", share.plot)


# shares of ch by top markets
share.change <- mkt.counts %>% 
  filter(ever_ch>0,
         max_patients>200,
         mkt_count==6) %>%
  filter(Year==2010) %>% select(market, share_2010=share_tierab) %>%
  left_join(mkt.counts %>% 
              filter(ever_ch>0) %>%
              filter(Year==2015) %>% select(market, share_2015=share_tierab),
            by=c("market")) %>%
  mutate(share_change=share_2015-share_2010)

high.change <- share.change %>% ungroup() %>% top_n(5, wt=share_change) %>% mutate(change_group="high")
low.change <- share.change %>% ungroup() %>% top_n(-5, wt=share_change) %>% mutate(change_group="low")
change.group <- rbind(high.change, low.change)

top.bottom.share <- mkt.counts %>%
  inner_join(change.group %>% select(market, change_group),
             by=c("market"))

top.share <- top.bottom.share %>%
  mutate(mean_share=share_tierab) %>%
  filter(change_group=="high") %>%
  mutate(Type="Top 5 Markets") %>%
  select(market,mean_share, Year, Type) %>%
  mutate(market=as.factor(market))

mkt.share.plot <- ggplot() + geom_line(data=top.share, aes(Year, mean_share, group=market), color="gray") +
  geom_text(data=top.share %>% filter(Year==last(Year), market==168),
            aes(label=Type,
                x=Year-.5,
                y=mean_share),
            vjust=-0.5) +
  geom_line(data=mkt.ch, aes(Year, mean_share, linetype=Type))+
  geom_text(data=mkt.ch %>% filter(Year==last(Year)),aes(label=Type,
                                                           x=Year-.5,
                                                           y=mean_share),
            vjust=-0.5) +
  geom_point(data=mkt.ch, aes(Year, mean_share)) +
  geom_text(data=mkt.ch, aes(x=Year, y=mean_share, label=round(mean_share,3)),
            vjust="top",hjust="center", show.legend=FALSE)+
  theme_bw() + ylab("Share of Patients") + xlab("Year") +
  scale_y_continuous(limits=c(0.10,0.90), breaks=c(.1,.2,.3,.4,.5,.6,.7,.8,.9)) +
  guides(linetype=FALSE)
ggsave("results/f2-shares-mkt.png", mkt.share.plot)



# Basic price regressions -------------------------------------------------

init.reg.data <- ch.data.final %>%
  mutate(CH=ifelse(CH_TierAll %in% c("Tier A", "Tier B"),1,0),
         CH_A=ifelse(CH_TierAll %in% c("Tier A"),1,0),
         CH_B=ifelse(CH_TierAll %in% c("Tier B"),1,0),
         log_price=log(hcci_price),
         bad=ifelse(any_readmit_90+Comp_Any>0,1,0))
  

price.reg.all <- feols(log_price ~ CH + Surgery + ccc_flag + female + Type +
                      bdtot + labor_nurse + labor_phys + labor_residents + labor_other + 
                      total_discharges + mcaid_discharges + mcare_discharges +
                      totalpop + age_18to34 + age_35to64 + age_65plus + 
                      race_black + race_asian + race_other +
                      income_25to50 + income_50to75 + income_75to100 +
                      income_100to150 + income_150plus + educ_gradeschool +
                      educ_highschool + educ_hsgrad + educ_graduate + emp_fulltime 
                    | Year + month + z_group_id, 
                    cluster="aha_hnpi",
                    data=init.reg.data)
ch.price.all <- tidy(price.reg.all,conf.int=TRUE) %>% 
  mutate(model="Overall", outcome="Price") %>% filter(term=="CH")


price.reg.ip <- feols(log_price ~ CH + Surgery + ccc_flag + female +
                         bdtot + labor_nurse + labor_phys + labor_residents + labor_other + 
                         total_discharges + mcaid_discharges + mcare_discharges +
                         totalpop + age_18to34 + age_35to64 + age_65plus + 
                         race_black + race_asian + race_other +
                         income_25to50 + income_50to75 + income_75to100 +
                         income_100to150 + income_150plus + educ_gradeschool +
                         educ_highschool + educ_hsgrad + educ_graduate + emp_fulltime 
                       | Year + month + z_group_id, 
                       cluster="aha_hnpi",
                       data=init.reg.data %>% filter(Type=="Inpatient"))
ch.price.ip <- tidy(price.reg.ip,conf.int=TRUE) %>% 
  mutate(model="Inpatient", outcome="Price") %>% filter(term=="CH")

price.reg.op <- feols(log_price ~ CH + Surgery + ccc_flag + female +
                        bdtot + labor_nurse + labor_phys + labor_residents + labor_other + 
                        total_discharges + mcaid_discharges + mcare_discharges +
                        totalpop + age_18to34 + age_35to64 + age_65plus + 
                        race_black + race_asian + race_other +
                        income_25to50 + income_50to75 + income_75to100 +
                        income_100to150 + income_150plus + educ_gradeschool +
                        educ_highschool + educ_hsgrad + educ_graduate + emp_fulltime 
                      | Year + month + z_group_id, 
                      cluster="aha_hnpi",
                      data=init.reg.data %>% filter(Type=="Outpatient"))
ch.price.op <- tidy(price.reg.op,conf.int=TRUE) %>% 
  mutate(model="Outpatient", outcome="Price") %>% filter(term=="CH")



readmit.reg.all <- feols(any_readmit_90 ~ CH + Surgery + ccc_flag + female + Type +
                         bdtot + labor_nurse + labor_phys + labor_residents + labor_other + 
                         total_discharges + mcaid_discharges + mcare_discharges +
                         totalpop + age_18to34 + age_35to64 + age_65plus + 
                         race_black + race_asian + race_other +
                         income_25to50 + income_50to75 + income_75to100 +
                         income_100to150 + income_150plus + educ_gradeschool +
                         educ_highschool + educ_hsgrad + educ_graduate + emp_fulltime 
                       | Year + month + z_group_id, 
                       cluster="aha_hnpi",
                       data=init.reg.data)
ch.readmit.all <- tidy(readmit.reg.all,conf.int=TRUE) %>% 
  mutate(model="Overall", outcome="Readmission") %>% filter(term=="CH")


readmit.reg.ip <- feols(any_readmit_90 ~ CH + Surgery + ccc_flag + female +
                        bdtot + labor_nurse + labor_phys + labor_residents + labor_other + 
                        total_discharges + mcaid_discharges + mcare_discharges +
                        totalpop + age_18to34 + age_35to64 + age_65plus + 
                        race_black + race_asian + race_other +
                        income_25to50 + income_50to75 + income_75to100 +
                        income_100to150 + income_150plus + educ_gradeschool +
                        educ_highschool + educ_hsgrad + educ_graduate + emp_fulltime 
                      | Year + month + z_group_id, 
                      cluster="aha_hnpi",
                      data=init.reg.data %>% filter(Type=="Inpatient"))
ch.readmit.ip <- tidy(readmit.reg.ip,conf.int=TRUE) %>% 
  mutate(model="Inpatient", outcome="Readmission") %>% filter(term=="CH")

readmit.reg.op <- feols(any_readmit_90 ~ CH + Surgery + ccc_flag + female +
                        bdtot + labor_nurse + labor_phys + labor_residents + labor_other + 
                        total_discharges + mcaid_discharges + mcare_discharges +
                        totalpop + age_18to34 + age_35to64 + age_65plus + 
                        race_black + race_asian + race_other +
                        income_25to50 + income_50to75 + income_75to100 +
                        income_100to150 + income_150plus + educ_gradeschool +
                        educ_highschool + educ_hsgrad + educ_graduate + emp_fulltime 
                      | Year + month + z_group_id, 
                      cluster="aha_hnpi",
                      data=init.reg.data %>% filter(Type=="Outpatient"))
ch.readmit.op <- tidy(readmit.reg.op,conf.int=TRUE) %>% 
  mutate(model="Outpatient", outcome="Readmission") %>% filter(term=="CH")



comp.reg.all <- feols(Comp_Any ~ CH + Surgery + ccc_flag + female + Type +
                           bdtot + labor_nurse + labor_phys + labor_residents + labor_other + 
                           total_discharges + mcaid_discharges + mcare_discharges +
                           totalpop + age_18to34 + age_35to64 + age_65plus + 
                           race_black + race_asian + race_other +
                           income_25to50 + income_50to75 + income_75to100 +
                           income_100to150 + income_150plus + educ_gradeschool +
                           educ_highschool + educ_hsgrad + educ_graduate + emp_fulltime 
                         | Year + month + z_group_id, 
                         cluster="aha_hnpi",
                         data=init.reg.data)
ch.comp.all <- tidy(comp.reg.all,conf.int=TRUE) %>% 
  mutate(model="Overall", outcome="Complication") %>% filter(term=="CH")


comp.reg.ip <- feols(Comp_Any ~ CH + Surgery + ccc_flag + female +
                          bdtot + labor_nurse + labor_phys + labor_residents + labor_other + 
                          total_discharges + mcaid_discharges + mcare_discharges +
                          totalpop + age_18to34 + age_35to64 + age_65plus + 
                          race_black + race_asian + race_other +
                          income_25to50 + income_50to75 + income_75to100 +
                          income_100to150 + income_150plus + educ_gradeschool +
                          educ_highschool + educ_hsgrad + educ_graduate + emp_fulltime 
                        | Year + month + z_group_id, 
                        cluster="aha_hnpi",
                        data=init.reg.data %>% filter(Type=="Inpatient"))
ch.comp.ip <- tidy(comp.reg.ip,conf.int=TRUE) %>% 
  mutate(model="Inpatient", outcome="Complication") %>% filter(term=="CH")

comp.reg.op <- feols(Comp_Any ~ CH + Surgery + ccc_flag + female +
                          bdtot + labor_nurse + labor_phys + labor_residents + labor_other + 
                          total_discharges + mcaid_discharges + mcare_discharges +
                          totalpop + age_18to34 + age_35to64 + age_65plus + 
                          race_black + race_asian + race_other +
                          income_25to50 + income_50to75 + income_75to100 +
                          income_100to150 + income_150plus + educ_gradeschool +
                          educ_highschool + educ_hsgrad + educ_graduate + emp_fulltime 
                        | Year + month + z_group_id, 
                        cluster="aha_hnpi",
                        data=init.reg.data %>% filter(Type=="Outpatient"))
ch.comp.op <- tidy(comp.reg.op,conf.int=TRUE) %>% 
  mutate(model="Outpatient", outcome="Complication") %>% filter(term=="CH")


bad.reg.all <- feols(bad ~ CH + Surgery + ccc_flag + female + Type +
                        bdtot + labor_nurse + labor_phys + labor_residents + labor_other + 
                        total_discharges + mcaid_discharges + mcare_discharges +
                        totalpop + age_18to34 + age_35to64 + age_65plus + 
                        race_black + race_asian + race_other +
                        income_25to50 + income_50to75 + income_75to100 +
                        income_100to150 + income_150plus + educ_gradeschool +
                        educ_highschool + educ_hsgrad + educ_graduate + emp_fulltime 
                      | Year + month + z_group_id, 
                      cluster="aha_hnpi",
                      data=init.reg.data)
ch.bad.all <- tidy(bad.reg.all,conf.int=TRUE) %>% 
  mutate(model="Overall", outcome="Comp or Readm") %>% filter(term=="CH")


bad.reg.ip <- feols(bad ~ CH + Surgery + ccc_flag + female +
                       bdtot + labor_nurse + labor_phys + labor_residents + labor_other + 
                       total_discharges + mcaid_discharges + mcare_discharges +
                       totalpop + age_18to34 + age_35to64 + age_65plus + 
                       race_black + race_asian + race_other +
                       income_25to50 + income_50to75 + income_75to100 +
                       income_100to150 + income_150plus + educ_gradeschool +
                       educ_highschool + educ_hsgrad + educ_graduate + emp_fulltime 
                     | Year + month + z_group_id, 
                     cluster="aha_hnpi",
                     data=init.reg.data %>% filter(Type=="Inpatient"))
ch.bad.ip <- tidy(bad.reg.ip,conf.int=TRUE) %>% 
  mutate(model="Inpatient", outcome="Comp or Readm") %>% filter(term=="CH")

bad.reg.op <- feols(bad ~ CH + Surgery + ccc_flag + female +
                       bdtot + labor_nurse + labor_phys + labor_residents + labor_other + 
                       total_discharges + mcaid_discharges + mcare_discharges +
                       totalpop + age_18to34 + age_35to64 + age_65plus + 
                       race_black + race_asian + race_other +
                       income_25to50 + income_50to75 + income_75to100 +
                       income_100to150 + income_150plus + educ_gradeschool +
                       educ_highschool + educ_hsgrad + educ_graduate + emp_fulltime 
                     | Year + month + z_group_id, 
                     cluster="aha_hnpi",
                     data=init.reg.data %>% filter(Type=="Outpatient"))
ch.bad.op <- tidy(bad.reg.op,conf.int=TRUE) %>% 
  mutate(model="Outpatient", outcome="Comp or Readm") %>% filter(term=="CH")


est.final <- rbind(ch.price.all, ch.price.ip, ch.price.op, 
                   ch.readmit.all, ch.readmit.op, ch.readmit.ip,
                   ch.comp.all, ch.comp.op, ch.comp.ip,
                   ch.bad.all, ch.bad.op, ch.bad.ip)
est.final$model <- factor(est.final$model, levels=c("Overall","Inpatient","Outpatient"))
est.final$outcome <- factor(est.final$outcome, levels=c("Price","Complication","Readmission","Comp or Readm"))

ch.price <- est.final %>% filter(outcome=="Price") %>%
  ggplot(aes(x=as.factor(outcome), y=estimate, shape=as.factor(model))) +
  geom_hline(aes(yintercept=0), linetype="dashed") +
  geom_errorbar(aes(ymin=conf.low, ymax=conf.high),
                lwd=1, width=0, position=position_dodge(width=0.5)) +
  labs(
    y="Estimate and \n95% Confidence Interval",
    x="Outcome",
    shape="Setting") +
  geom_point(size=3, position=position_dodge(width=0.5)) +
  scale_y_continuous(breaks=c(-.1,0,.1,.2,.3,.4,.5)) +
  theme_bw()
ggsave("results/f3-prelim-price.png", ch.price)


ch.quality <- est.final %>% filter(outcome!="Price") %>%
  ggplot(aes(x=as.factor(outcome), y=estimate, shape=as.factor(model))) +
  geom_hline(aes(yintercept=0), linetype="dashed") +
  geom_errorbar(aes(ymin=conf.low, ymax=conf.high),
                lwd=1, width=0, position=position_dodge(width=0.5)) +
  labs(
    y="Estimate and \n95% Confidence Interval",
    x="Outcome",
    shape="Setting") +
  geom_point(size=3, position=position_dodge(width=0.5)) +
  scale_y_continuous(limits=c(-0.01, 0.02), breaks=c(-.01,0,.01,.02)) +
  theme_bw()
ggsave("results/f3-prelim-quality.png", ch.quality)

ch.estimates <- est.final %>%
  ggplot(aes(x=as.factor(model),y=estimate)) +
  geom_hline(aes(yintercept=0), linetype="dashed") +
  geom_errorbar(aes(ymin=conf.low, ymax=conf.high),
                lwd=1, width=0, position=position_dodge(width=0.5)) +
  labs(
    y="Estimate and \n95% Confidence Interval",
    x=" ",
    shape="Setting") +
  geom_point(size=3, position=position_dodge(width=0.5)) +
  facet_wrap(~ outcome, scales="free_y") +
  theme_bw() 
ggsave("results/f3-prelim-compare.png", ch.estimates)


