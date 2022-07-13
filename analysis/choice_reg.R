
# Meta --------------------------------------------------------------------

## Author:        Ian McCarthy
## Date Created:  9/4/2019
## Date Edited:   3/16/2021
## Description:   Estimate hospital choice model and willingness to pay estimates


# Choice Model ------------------------------------------------------------

exp.util.ip <- dchoice.est(data=ip.choice.reg)$pred
exp.util.op <- dchoice.est(data=op.choice.reg)$pred

exp.util <- bind_rows(exp.util.ip, exp.util.op)
  
# form ex ante WTP by summing across all patients and all conditions.
# note: this is the same as a probabalistic approach if we use market-based
# probabilities and patient counts...
# i.e., if xbar1 = (1/N1) * sum(x1), xbar2 = (1/N2) * sum(x2)
# N*(p1*xbar1 + p2*xbar2) = N1*xbar1 + N2*xbar2 = sum(x1) + sum(x2) = sum(wtp)
final.wtp <- exp.util %>%
  dplyr::select(aha_hnpi, Year, wtp) %>%
  group_by(aha_hnpi, Year) %>%
  summarize(total_wtp=sum(wtp,na.rm=TRUE))
  
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
  scale_y_continuous(labels=comma) +
  guides(linetype=FALSE)
ggsave("results/tierwtp.png", wtp.plot)

wtp.mean.ch <- wtp.ch %>% bind_rows(wtp.nch)
ch.wtp.plot <- ggplot() + geom_line(data=wtp.mean.ch, aes(Year, mean_wtp, linetype=tier))+
  geom_text(data=wtp.mean.ch %>% filter(Year==last(Year)),aes(label=tier,
                                                           x=Year-.3,
                                                           y=mean_wtp),
            vjust=1) +
  theme_bw() + ylab("Average Willingness to Pay") + xlab("Year") +
  scale_y_continuous(labels=comma, limits=c(0,130000), breaks=c(0,25000,50000,75000,100000,125000)) +
  guides(linetype=FALSE)
ggsave("results/ch-nch-wtp.png", ch.wtp.plot)



wtp.a.all <- final.wtp %>% ungroup() %>%
  filter(CH_TierAll=="Tier A") %>%
  summarize(mean_wtp=mean(total_wtp)) %>%
  mutate(Type="Tier A")

wtp.b.all <- final.wtp %>% ungroup() %>%
  filter(CH_TierAll=="Tier B") %>%
  summarize(mean_wtp=mean(total_wtp)) %>%
  mutate(Type="Tier B")

wtp.c.all <- final.wtp %>% ungroup() %>%
  filter(CH_TierAll=="Tier C") %>%
  summarize(mean_wtp=mean(total_wtp)) %>%
  mutate(Type="Tier C")



wtp.ch.all <- final.wtp %>% ungroup() %>%
  filter(CH_TierAll %in% c("Tier A", "Tier B")) %>%
  summarize(mean_wtp=mean(total_wtp)) %>%
  mutate(tier="Children's Hospitals")

wtp.nch.all <- final.wtp %>% ungroup() %>%
  filter(!CH_TierAll %in% c("Tier A", "Tier B")) %>%
  summarize(mean_wtp=mean(total_wtp)) %>%
  mutate(tier="Non-children's Hospitals")

wtp.mean.all <- wtp.ch.all %>% bind_rows(wtp.nch.all)



# Bootstrap Standard Errors -----------------------------------------------

bootsrp <- function(j) {
  bs.choice <- dchoice.bs(data.ip=ip.choice.reg, data.op=op.choice.reg)
  return(list("coef"=bs.choice$coef, "pred"=bs.choice$pred))
}
max.boot <- 250

sim.bs <- lapply(1:max.boot, bootsrp)
bs.coef <- sim.bs[[1]]$coef %>% mutate(boot=1)
bs.pred <- sim.bs[[1]]$pred %>% mutate(boot=1)
for (i in 2:max.boot) {
  bs.coef <- bind_rows(bs.coef, sim.bs[[i]]$coef %>% mutate(boot=i))
  bs.pred <- bind_rows(bs.pred, sim.bs[[i]]$pred %>% mutate(boot=i))
}

## implement bias correction for CI
bc <- bs.pred %>%
  left_join(wtp.mean.all %>% rename(base_wtp=mean_wtp),
            by=c("tier")) %>%
  mutate(below=(mean_wtp<=base_wtp)) %>%
  group_by(tier) %>%
  summarize(bc=sum(below, na.rm=TRUE)/(max.boot+1),
            bc=ifelse(bc>=1,.999,bc),
            bc_q=qnorm(bc, mean=0, sd=1),
            low_bc_obs=ceiling((max.boot+1)*pnorm(2*bc_q-1.64)),
            high_bc_obs=floor((max.boot+1)*pnorm(2*bc_q+1.64)),
            low_bc_obs=ifelse(low_bc_obs>max.boot,max.boot-1,low_bc_obs),
            high_bc_obs=ifelse(high_bc_obs<1,1,high_bc_obs))


#bc$bc_q <- sapply(bc$bc, qnorm)

wtp.ci <- bs.pred %>% 
  group_by(tier) %>%
  arrange(mean_wtp) %>%
  mutate(row_num=row_number()) %>%
  left_join(bc %>% select(tier, low_bc_obs, high_bc_obs),
            by=c("tier"))

wtp.ci <- as.data.table(wtp.ci)
wtp.ci[row_num==low_bc_obs, low_bc := mean_wtp]
wtp.ci[row_num==high_bc_obs, high_bc := mean_wtp]
wtp.ci <- as_tibble(wtp.ci)

wtp.ci <- wtp.ci %>%
  group_by(tier) %>%
  summarize(low=quantile(mean_wtp, probs=0.05, na.rm=TRUE),
            high=quantile(mean_wtp, probs=0.95, na.rm=TRUE),
            low_bc=min(low_bc, na.rm=TRUE),
            high_bc=max(high_bc, na.rm=TRUE))

wtp.data <- wtp.mean.all %>%
  left_join(wtp.ci, by=c("tier"))

ci.plot <- ggplot(data=wtp.data, aes(x=tier, y=mean_wtp)) +
  geom_point(size=2) +
  geom_errorbar(data=wtp.data, aes(ymin=low_bc, ymax=high_bc), width=0, size=1) +
  theme_bw() + ylab("Average Willingness to Pay") + xlab(" ") +
  scale_y_continuous(labels=comma, limits=c(0,150000)) +
  guides(linetype=FALSE)
ggsave("results/tierwtp_ci.png", ci.plot)

