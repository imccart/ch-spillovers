
# Meta --------------------------------------------------------------------

## Author:        Ian McCarthy
## Date Created:  9/21/2019
## Date Edited:   10/30/2020
## Description:   Choice estimation for bootstrapping standard errors



# Estimation function -----------------------------------------------------

dchoice.bs <- function(data.op, data.ip) {

    ## select bootstrap sample
    unique.ip <- data.ip %>%
      group_by(id) %>%
      mutate(id_count=seq(n())) %>%
      filter(id_count==1) %>%
      select(id) %>% ungroup()
    
    unique.op <- data.op %>%
      group_by(id) %>%
      mutate(id_count=seq(n())) %>%
      filter(id_count==1) %>%
      select(id) %>% ungroup()
    
    sample.ip <- sample_n(unique.ip, size=nrow(unique.ip), replace=TRUE) %>%
      mutate(new_id=row_number())
      
    sample.op <- sample_n(unique.op, size=nrow(unique.op), replace=TRUE) %>%
      mutate(new_id=row_number())
        
    choice.bs.ip <- data.ip %>%
      inner_join(sample.ip, by=c("id")) %>%
      mutate(id=new_id) %>% select(-new_id)

    choice.bs.op <- data.op %>%
      inner_join(sample.op, by=c("id")) %>%
      mutate(id=new_id) %>% select(-new_id)
    
      
    # estimate multinomial logit model
    model.run.ip <- dchoice.est(data=choice.bs.ip)
    model.run.op <- dchoice.est(data=choice.bs.op)
    coef.vals.ip <- as_tibble(model.run.ip$coef) %>% mutate(type="Inpatient")
    coef.vals.op <- as_tibble(model.run.op$coef) %>% mutate(type="Outpatient")
    coef.vals <- bind_rows(coef.vals.ip, coef.vals.op)
    exp.util.bs.ip <- model.run.ip$pred
    exp.util.bs.op <- model.run.op$pred    
          
#    wtp.bs <- bind_rows(exp.util.bs.ip, exp.util.bs.op) %>%
#      dplyr::select(aha_hnpi, wtp, Surgery, Year, Type) %>%
#      group_by(aha_hnpi, Surgery, Year, Type) %>%
#      summarize(mean_wtp=sum(wtp,na.rm=TRUE)) %>%
#      left_join(final.type,
#                by=c("aha_hnpi","Surgery","Year", "Type")) %>%
#      mutate(wtp_surgery=mean_wtp*surg_count) %>%
#      group_by(aha_hnpi, Year) %>%
#      summarize(total_wtp=mean(wtp_surgery, na.rm=TRUE))
 
    wtp.bs <- bind_rows(exp.util.bs.ip, exp.util.bs.op) %>%
      dplyr::select(aha_hnpi, wtp, Surgery, Year, Type) %>%
      group_by(aha_hnpi, Year) %>%
      summarize(total_wtp=mean(wtp,na.rm=TRUE))
           
    wtp.bs <- wtp.bs %>%
      left_join(hosp.only, by=c("aha_hnpi", "Year")) %>%
      filter(total_wtp>0)    
          
    wtp.ch.bs <- wtp.bs %>% ungroup() %>%
      filter(CH_TierAll %in% c("Tier A","Tier B")) %>%
      summarize(mean_wtp=mean(total_wtp)) %>%
      mutate(tier="Children's Hospitals")
          
    wtp.nch.bs <- wtp.bs %>% ungroup() %>%
      filter(!CH_TierAll %in% c("Tier A","Tier B")) %>%
      summarize(mean_wtp=mean(total_wtp)) %>%
      mutate(tier="Non-children's Hospitals")

    
    wtp.yearch.bs <- wtp.bs %>% ungroup() %>% group_by(Year) %>%
      filter(CH_TierAll %in% c("Tier A","Tier B")) %>%
      summarize(mean_wtp=mean(total_wtp)) %>%
      mutate(tier="Children's Hospitals")

    wtp.yearnch.bs <- wtp.bs %>% ungroup() %>% group_by(Year) %>%
      filter(!CH_TierAll %in% c("Tier A","Tier B")) %>%
      summarize(mean_wtp=mean(total_wtp)) %>%
      mutate(tier="Non-children's Hospitals")
    
              
    wtp.mean.bs <- bind_rows(wtp.ch.bs,wtp.nch.bs)
    wtp.year.bs <- bind_rows(wtp.yearch.bs,wtp.yearnch.bs)
  
  return(list("pred"=wtp.mean.bs, "coef"=coef.vals, "pred_year"=wtp.year.bs))  

}