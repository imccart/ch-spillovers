
# Meta --------------------------------------------------------------------

## Author:        Ian McCarthy
## Date Created:  9/4/2019
## Date Edited:   10/30/2020
## Description:   Code for estimating logit model. Separated from other code
##                for consistency of specifications across files.



# Logit specification -----------------------------------------------------

dchoice.est <- function(data) {
  
logit.reg <- mclogit(cbind(choice,id) ~ pred_cs + diff_dist + 
                       pred.price + pred.comp + pred.readmit +
                       bdtot + system + teaching + nonprofit + tierA + tierB + tierC +
                       beds_dist + system_dist + teaching_dist + nonprofit_dist +
                       nocs_dist + nocs_beds + nocs_system + nocs_teaching + nocs_nonprofit +
                       gdr_dist + gdr_beds + gdr_system + gdr_teaching + gdr_nonprofit +
                       ccc_beds + ccc_system + ccc_teaching + ccc_nonprofit + ccc_dist,
                     data=data)
logit.coef <- logit.reg$coef

# ## form predicted utilities and WTP manually
# pred <- t(coef(logit.reg) %*% t(data[,c("pred_cs","diff_dist","pred.price","pred.comp","pred.readmit", "tierA",
#                                         "bdtot","system","teaching","nonprofit",
#                                         "beds_dist","system_dist","teaching_dist","nonprofit_dist",
#                                         "nocs_dist","nocs_beds","nocs_system","nocs_teaching","nocs_nonprofit",
#                                         "gdr_dist","gdr_beds","gdr_system","gdr_teaching","gdr_nonprofit",
#                                         "ccc_beds","ccc_system","ccc_teaching","ccc_nonprofit","ccc_dist")]))
# colnames(pred) <- "pred"
# pred <- as_tibble(pred)
# exp.util <- bind_cols(data, pred)
# 
# exp.util <- exp.util %>%
#   mutate(exp_pred=exp(pred))
# 
# sum.util <- exp.util %>%
#   dplyr::select(id, exp_pred) %>%
#   group_by(id) %>%
#   summarize(sum_iu=sum(exp_pred, na.rm=TRUE),
#             choice_count=n())
# 
# exp.util <- exp.util %>%
#   left_join(sum.util, by=c("id")) %>%
#   mutate(sum_iu_notj=pmax(sum_iu - exp_pred,0.00001),
#          sum_iu=pmax(sum_iu,0.00001),
#          change_pred=log(sum_iu)-log(sum_iu_notj),
#          wtp=change_pred/abs(logit.reg$coef[1]))


## form predictions and WTP with prediction functions
pred.base <- prediction(logit.reg, type="response")
#pred.base <- add_predictions(logit.reg$data, logit.reg, var="fitted", type="response")
exp.util <- as_tibble(pred.base) %>%
  mutate(fitted=pmin(fitted, 0.99),
         change_pred=log(1/(1-fitted)),
         wtp=change_pred/abs(logit.reg$coef[1]))

return(list("pred"=exp.util, "coef"=logit.coef))  
}
