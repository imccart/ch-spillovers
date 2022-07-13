
# Meta --------------------------------------------------------------------

## Author:        Ian McCarthy
## Date Created:  9/14/2020
## Date Edited:   2/16/2021
## Description:   Create risk adjusted price at hospital level for inpatient
##                procedures only



# Risk adjustment ---------------------------------------------------------
price.reg.ip <- ch.data.final %>% 
  filter(Type=="Inpatient") %>%
  dplyr::select(hcci_price, gdr, los, ccc_flag, Surgery, aha_hnpi, Year, Comp_Any,
                any_readmit_30, any_readmit_60, any_readmit_90) %>%
  add_count(aha_hnpi, Year, name="tot_patients")
price.reg.final <- price.reg.ip %>%
  filter(tot_patients>=10)

for (t in 2010:2015) {
  reg.dat <- price.reg.final %>%
    filter(Year==t)
  
  ## Price regressions
  reg.price <- felm(hcci_price ~ los + gdr + Surgery + ccc_flag | aha_hnpi,
                    data=reg.dat)
  pred.fe <- as_tibble(getfe(reg.price))
  pred.fe <- pred.fe %>%
    mutate(Year=t) %>%
    dplyr::select(adj_price_ip=effect, aha_hnpi=idx, Year) %>%
    mutate(aha_hnpi=as.character(aha_hnpi))
  assign(paste("pred.fe.",t,sep=""),pred.fe)
  
  
  ## Complication regressions
  reg.comp <- felm(Comp_Any ~ los + gdr + Surgery + ccc_flag | aha_hnpi,
                   data=reg.dat)
  pred.fe.comp <- as_tibble(getfe(reg.comp))
  pred.fe.comp <- pred.fe.comp %>%
    mutate(Year=t) %>%
    dplyr::select(adj_comp_ip=effect, aha_hnpi=idx, Year) %>%
    mutate(aha_hnpi=as.character(aha_hnpi))
  assign(paste("pred.fe.comp.",t,sep=""),pred.fe.comp)  
  
  
  ## Readmission regressions
  reg.readmit <- felm(any_readmit_30 ~ los + gdr + Surgery + ccc_flag | aha_hnpi,
                      data=reg.dat)
  pred.fe.readmit <- as_tibble(getfe(reg.readmit))
  pred.fe.readmit <- pred.fe.readmit %>%
    mutate(Year=t) %>%
    dplyr::select(adj_readmit_ip=effect, aha_hnpi=idx, Year) %>%
    mutate(aha_hnpi=as.character(aha_hnpi))
  assign(paste("pred.fe.readmit.",t,sep=""),pred.fe.readmit)  
  
  
}
adj.price=rbind(pred.fe.2010, pred.fe.2011, pred.fe.2012, pred.fe.2013,
                pred.fe.2014, pred.fe.2015)

adj.comp=rbind(pred.fe.comp.2010, pred.fe.comp.2011, pred.fe.comp.2012, pred.fe.comp.2013,
               pred.fe.comp.2014, pred.fe.comp.2015)

adj.readmit=rbind(pred.fe.readmit.2010, pred.fe.readmit.2011, pred.fe.readmit.2012, pred.fe.readmit.2013,
                  pred.fe.readmit.2014, pred.fe.readmit.2015)


## Form hospital-level average prices and quality measures
hosp.avgs <- claims.only %>%
  filter(Type=="Inpatient") %>%
  group_by(aha_hnpi, Year) %>%
  summarize(hcci_price_ip=mean(hcci_price),
            Comp_Any_IP=mean(Comp_Any),
            any_readmit_30_ip=mean(any_readmit_30),
            any_readmit_60_ip=mean(any_readmit_60),
            any_readmit_90_ip=mean(any_readmit_90),
            tot_patients_ip=n())

## Combine original hospital data with prices/quality from claims data
hosp.data <- hosp.data %>%
  left_join(hosp.avgs, by=c("aha_hnpi","Year")) %>%
  left_join(adj.price, by=c("aha_hnpi","Year")) %>%
  left_join(adj.comp, by=c("aha_hnpi","Year")) %>%
  left_join(adj.readmit, by=c("aha_hnpi", "Year"))