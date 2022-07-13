
# Meta --------------------------------------------------------------------

## Author:        Ian McCarthy
## Date Created:  9/4/2019
## Date Edited:   8/30/2021
## Description:   Estimate hospital choice model and willingness to pay estimates by year


# Choice Model ------------------------------------------------------------

for (t in 2010:2015) {
  ip.choice.regt <- ip.choice.reg %>% filter(Year==t)
  op.choice.regt <- op.choice.reg %>% filter(Year==t)
  exp.util.ipt <- dchoice.est(data=ip.choice.regt)$pred
  exp.util.opt <- dchoice.est(data=op.choice.regt)$pred
  exp.utilt <- bind_rows(exp.util.ipt, exp.util.opt)
  if (t==2010) {
    exp.util <- exp.utilt
  } else {
    exp.util <- bind_rows(exp.util, exp.utilt)
  }
}
  
# form ex ante WTP by summing across all patients and all conditions.
# note: this is the same as a probabalistic approach if we use market-based
# probabilities and patient counts...
# i.e., if xbar1 = (1/N1) * sum(x1), xbar2 = (1/N2) * sum(x2)
# N*(p1*xbar1 + p2*xbar2) = N1*xbar1 + N2*xbar2 = sum(x1) + sum(x2) = sum(wtp)
final.wtp <- exp.util %>%
  dplyr::select(aha_hnpi, Year, wtp) %>%
  group_by(aha_hnpi, Year) %>%
  summarize(total_wtp=mean(wtp,na.rm=TRUE))
  
final.wtp <- final.wtp %>%
  left_join(hosp.only, by=c("aha_hnpi", "Year")) %>%
  filter(total_wtp>0)

final.type <- exp.util %>% 
  group_by(Surgery, Year, aha_hnpi, Type) %>%
  summarize(surg_count=n())
  
wtp.a <- final.wtp %>%
  group_by(Year) %>%
  filter(CH_TierAll=="Tier A") %>%
  summarize(mean_wtp=mean(total_wtp)) %>%
  mutate(Type="Tier A")
  
wtp.b <- final.wtp %>%
  group_by(Year) %>%
  filter(CH_TierAll=="Tier B") %>%
  summarize(mean_wtp=mean(total_wtp)) %>%
  mutate(Type="Tier B")
  
wtp.c <- final.wtp %>%
  group_by(Year) %>%
  filter(CH_TierAll=="Tier C") %>%
  summarize(mean_wtp=mean(total_wtp)) %>%
  mutate(Type="Tier C")

wtp.ch <- final.wtp %>%
  group_by(Year) %>%
  filter(CH_TierAll %in% c("Tier A", "Tier B")) %>%
  summarize(mean_wtp=mean(total_wtp)) %>%
  mutate(tier="Children's Hospitals")

wtp.nch <- final.wtp %>%
  group_by(Year) %>%
  filter(!CH_TierAll %in% c("Tier A", "Tier B")) %>%
  summarize(mean_wtp=mean(total_wtp)) %>%
  mutate(tier="Non-children's Hospitals")
  
wtp.mean <- wtp.a %>% bind_rows(wtp.b) %>% bind_rows(wtp.c)
wtp.plot <- ggplot() + geom_line(data=wtp.mean, aes(Year, mean_wtp, linetype=Type))+
  geom_text(data=wtp.mean %>% filter(Year==last(Year)),aes(label=Type,
                                                           x=Year+.2,
                                                           y=mean_wtp)) +
  theme_bw() + ylab("Average Willingness to Pay") + xlab("Year") +
  scale_y_continuous(labels=comma) +s
  guides(linetype=FALSE)
ggsave("results/tierwtp-year.png", wtp.plot)

wtp.mean.ch <- wtp.ch %>% bind_rows(wtp.nch)
ch.wtp.plot <- ggplot() + geom_line(data=wtp.mean.ch, aes(Year, mean_wtp, linetype=tier))+
  geom_text(data=wtp.mean.ch %>% filter(Year==last(Year)),aes(label=tier,
                                                           x=Year-.3,
                                                           y=mean_wtp+100),
            vjust=1) +
  theme_bw() + ylab("Average Willingness to Pay per Patient") + xlab("Year") +
  scale_y_continuous(labels=comma, limits=c(0,5000), breaks=c(0,1000,2000,3000,4000,5000)) +
  guides(linetype=FALSE)
ggsave("results/ch-nch-wtp-year.png", ch.wtp.plot)



# Bootstrap Standard Errors -----------------------------------------------

bootsrp <- function(j) {
  bs.choicet <- dchoice.bs(data.ip=ip.choice.regt, data.op=op.choice.regt)
  return(list("coef"=bs.choicet$coef, "pred"=bs.choicet$pred))
}
max.boot <- 250

for (t in 2010:2015) {
  ip.choice.regt <- ip.choice.reg %>% filter(Year==t)
  op.choice.regt <- op.choice.reg %>% filter(Year==t)
  sim.bs <- lapply(1:max.boot, bootsrp)
  bs.coeft <- sim.bs[[1]]$coef %>% mutate(boot=1)
  bs.predt <- sim.bs[[1]]$pred %>% mutate(boot=1)
  for (i in 2:max.boot) {
    bs.coeft <- bind_rows(bs.coeft, sim.bs[[i]]$coef %>% mutate(boot=i))
    bs.predt <- bind_rows(bs.predt, sim.bs[[i]]$pred %>% mutate(boot=i))
  }
  if (t==2010) {
    bs.coef.all <- bs.coeft %>% mutate(Year=t)
    bs.pred.all <- bs.predt %>% mutate(Year=t)
  } else {
    bs.coef.all <- bind_rows(bs.coeft %>% mutate(Year=t), bs.coef.all)    
    bs.pred.all <- bind_rows(bs.predt %>% mutate(Year=t), bs.pred.all)
  }
}


## implement bias correction for CI
bc.year <- bs.pred.all %>%
  left_join(wtp.mean.ch %>% rename(base_wtp=mean_wtp),
            by=c("tier", "Year")) %>%
  mutate(below=(mean_wtp<=base_wtp)) %>%
  group_by(tier, Year) %>%
  summarize(bc=sum(below, na.rm=TRUE)/(max.boot+1),
            bc=ifelse(bc>=1,.999,bc),
            bc_q=qnorm(bc, mean=0, sd=1),
            low_bc_obs=ceiling((max.boot+1)*pnorm(2*bc_q-1.64)),
            high_bc_obs=floor((max.boot+1)*pnorm(2*bc_q+1.64)),
            low_bc_obs=ifelse(low_bc_obs>max.boot,max.boot-1,low_bc_obs),
            high_bc_obs=ifelse(high_bc_obs<1,1,high_bc_obs))


#bc$bc_q <- sapply(bc$bc, qnorm)

wtp.ci <- bs.pred.all %>% 
  group_by(tier, Year) %>%
  arrange(mean_wtp) %>%
  mutate(row_num=row_number()) %>%
  left_join(bc.year %>% select(tier, low_bc_obs, high_bc_obs, Year),
            by=c("tier", "Year"))

  
wtp.ci <- as.data.table(wtp.ci)
wtp.ci[row_num==low_bc_obs, low_bc := mean_wtp, by=list(tier, Year)]
wtp.ci[row_num==high_bc_obs, high_bc := mean_wtp, by=list(tier, Year)]
wtp.ci <- as_tibble(wtp.ci)

wtp.ci <- wtp.ci %>%
  group_by(tier, Year) %>%
  summarize(low=quantile(mean_wtp, probs=0.05, na.rm=TRUE),
            high=quantile(mean_wtp, probs=0.95, na.rm=TRUE),
            low_bc=min(low_bc, na.rm=TRUE),
            high_bc=max(high_bc, na.rm=TRUE))

wtp.data <- wtp.mean.ch %>%
  left_join(wtp.ci, by=c("tier", "Year"))

ci.plot <- ggplot(data=wtp.data, aes(Year, mean_wtp, linetype=tier)) +
  geom_line(data=wtp.data, aes(Year, mean_wtp, linetype=tier))+
  geom_text(data=wtp.mean.ch %>% filter(Year==last(Year)),aes(label=tier,
                                                              x=Year-.5,
                                                              y=mean_wtp+100),
            vjust=1) +
  geom_point(size=2) +
  geom_errorbar(data=wtp.data, aes(ymin=low_bc, ymax=high_bc), width=0, size=1) +
  theme_bw() + ylab("Average Willingness to Pay") + xlab("Year") +
  scale_y_continuous(labels=comma, limits=c(0,6005), breaks=c(0,1000,2000,3000,4000,5000,6000)) +
  guides(linetype="none")
ggsave("results/tierwtp_year_ci.png", ci.plot)

