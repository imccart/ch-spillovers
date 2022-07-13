
# Meta --------------------------------------------------------------------

## Author:        Ian McCarthy
## Date Created:  9/14/2020
## Date Edited:   8/30/2021
## Description:   Run all cleanup and analysis files


# Preliminaries -----------------------------------------------------------
if (!require("pacman")) install.packages("pacman")
pacman::p_load(data.table, ggplot2, lubridate, stringr, readxl, 
               stargazer, janitor, labelled, sjlabelled, lfe, plm, Matching,
               panelView, mclogit, prediction, kableExtra, fixest,
               scales, forcats, ggbeeswarm, tidyverse, dplyr, did,
               purrr, here, igraph, ggraph, modelsummary, broom, modelr,
               dotwhisker, ebal)

path.data="K:/Research/Raval_Emory/Childrens Hospitals/data/Final"
set.seed(7572)

# Read-in data ------------------------------------------------------------

## data on claims for standard conditions
source("data-code/claims-common-proc.R", local=TRUE, echo=FALSE)

## data on claims for *all* conditions (used to identify highly specialized procedures later)
source("data-code/claims-all-proc.R", local=TRUE, echo=FALSE)


# Combine and create new data ---------------------------------------------

## create hospital markets based on patient flows for standard conditions
source("data-code/markets.R", local=TRUE, echo=FALSE)

## build final claims, hospital, market, and choice datasets
source("data-code/_BuildData.R", local=TRUE, echo=FALSE)

## create hospital-level price dataset (seperately for overall, inaptient, and outpatient settings)
source("data-code/hospital_riskadj.R", local=TRUE, echo=FALSE)
source("data-code/hospital_riskadj_ip.R", local=TRUE, echo=FALSE)
source("data-code/hospital_riskadj_op.R", local=TRUE, echo=FALSE)

rm("ch.data.full")
save.image(file="data/analysis-workspace.Rdata")

# Analysis files ----------------------------------------------------------
load("data/analysis-workspace.Rdata")
set.seed(7572)

## summary statistics
source("analysis/summary_stats.R", local=TRUE, echo=FALSE)

## functions and data for choice model
source("analysis/fn_logit_model.R", local=TRUE, echo=FALSE)
source("analysis/fn_choice_reg_bs.R", local=TRUE, echo=FALSE)
source("data-code/choice.R", local=TRUE, echo=FALSE)

## form choice sets for IP and OP choices separately
source("analysis/choice_reg_ip.R", local=TRUE, echo=FALSE)
source("analysis/choice_reg_op.R", local=TRUE, echo=FALSE)

## estimate choice model
source("analysis/choice_reg.R", local=TRUE, echo=FALSE)
source("analysis/choice_reg_year.R", local=TRUE, echo=FALSE)

## analysis of entry and differentiation effects
source("analysis/differentiation.R", local=TRUE, echo=FALSE)
source("analysis/differentiation_5claims.R", local=TRUE, echo=FALSE)
source("analysis/differentiation_10claims.R", local=TRUE, echo=FALSE)

## analysis of expansion and bargaining effects
source("analysis/bargaining.R", local=TRUE, echo=FALSE)