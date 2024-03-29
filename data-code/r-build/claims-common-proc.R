ch.data=read_delim(paste0(path.data,"/FinalData.txt"),delim="\t",
                   col_types=cols(z_patid = col_double(),
                                  z_group_id = col_double(),
                                  mdc = col_integer(),
                                  drg = col_integer(),
                                  dstatus = col_integer(),
                                  age_band_cd = col_integer(),
                                  gdr = col_integer(),
                                  mbr_zip_5_cd = col_character(),
                                  prod = col_character(),
                                  mkt_sgmnt_cd = col_character(),
                                  funding = col_logical(),
                                  fst_admtdt = col_character(),
                                  last_dischdt = col_character(),
                                  hosp_zip = col_character(),
                                  charge = col_double(),
                                  calc_allwd = col_double(),
                                  tot_mem_cs = col_double(),
                                  coins = col_double(),
                                  copay = col_double(),
                                  deduct = col_double(),
                                  er_flag = col_logical(),
                                  ntwrk_ind = col_double(),
                                  primary_cov_ind = col_double(),
                                  los = col_double(),
                                  Year = col_double(),
                                  drg_type = col_character(),
                                  drg_wt = col_double(),
                                  pt_zip = col_character(),        
                                  pt_lat = col_double(),
                                  pt_long = col_double(),
                                  hosp_lat = col_double(),
                                  hosp_long = col_double(),
                                  distance_mi = col_double(),
                                  wtd_price = col_double(),
                                  wtd_charge = col_double(),
                                  aha_match = col_double(),
                                  aha_hnpi = col_character(),
                                  bdtot = col_double(),
                                  hrrcode = col_integer(),
                                  nonprofit = col_logical(),
                                  profit = col_logical(),
                                  teaching_hospital1 = col_logical(),
                                  teaching_hospital2 = col_logical(),
                                  system = col_logical(),
                                  labor_phys = col_double(),
                                  labor_residents = col_double(),
                                  labor_nurse = col_double(),
                                  labor_other = col_double(),
                                  capital_imaging = col_double(),
                                  capital_caresetting = col_double(),
                                  capital_services = col_double(),
                                  percent_profitable = col_double(),
                                  int_hos = col_character(),
                                  state = col_character(),
                                  county = col_character(),
                                  beds = col_double(),
                                  total_discharges = col_double(),
                                  mcare_discharges = col_double(),
                                  mcaid_discharges = col_double(),
                                  tot_charges = col_double(),
                                  tot_discounts = col_double(),
                                  tot_opexp = col_double(),
                                  ip_charges = col_double(),
                                  intcare_charges = col_double(),
                                  ancserv_charges = col_double(),
                                  tot_mcarepymt = col_double(),
                                  sec_mcarepymt = col_double(),
                                  hvbp_payment = col_double(),
                                  hrrp_payment = col_double(),
                                  zip = col_integer(),
                                  discount_factor = col_double(),
                                  price = col_double(),
                                  market_discharges = col_double(),
                                  discharge_share = col_double(),
                                  cmi = col_double(),
                                  adjusted_cases = col_double(),
                                  urban = col_logical(),
                                  totalpop = col_double(),
                                  age_below18 = col_double(),
                                  age_18to34 = col_double(),
                                  age_35to64 = col_double(),
                                  age_65plus = col_double(),
                                  race_white = col_double(),
                                  race_black = col_double(),
                                  race_asian = col_double(),
                                  race_other = col_double(),
                                  income_75to100 = col_double(),
                                  income_25 = col_double(),
                                  income_25to50 = col_double(),
                                  income_50to75 = col_double(),
                                  income_100to150 = col_double(),
                                  income_150plus = col_double(),
                                  educ_noschool = col_double(),
                                  educ_gradeschool = col_double(),
                                  educ_highschool = col_double(),
                                  educ_hsgrad = col_double(),
                                  educ_somecollege = col_double(),
                                  educ_assoc = col_double(),
                                  educ_bach = col_double(),
                                  educ_graduate = col_double(),
                                  emp_fulltime = col_double(),
                                  fips = col_integer(),
                                  msa_no = col_integer(),
                                  msaname = col_character(),
                                  totalhhi = col_double(),
                                  insurer1 = col_character(),
                                  share1 = col_double(),
                                  insurer2 = col_character(),
                                  share2 = col_double(),
                                  tiera = col_logical(),
                                  notier = col_logical(),
                                  tierb = col_logical(),
                                  tierc = col_logical(),
                                  tierd = col_logical(),
                                  Type = col_character(),
                                  Surg_AntiReflux = col_logical(),
                                  Surg_Appendectomy = col_logical(),
                                  Surg_Circumcision = col_logical(),
                                  Surg_IngHernia = col_logical(),
                                  Surg_Orchiopexy = col_logical(),
                                  Surg_Spine = col_logical(),
                                  Surg_Humerus = col_logical(),
                                  Surg_Strabismus = col_logical(),
                                  Surg_Tonsils = col_logical(),
                                  Surg_Tympanostomy = col_logical(),
                                  Surg_UmbHernia = col_logical(),
                                  Surg_Gallbladder = col_logical(),
                                  Surg_Knee = col_logical(),
                                  neuromusc_ccc = col_logical(),
                                  cvd_ccc = col_logical(),
                                  respiratory_ccc = col_logical(),
                                  renal_ccc = col_logical(),
                                  gi_ccc = col_logical(),
                                  hemato_immu_ccc = col_logical(),
                                  metabolic_ccc = col_logical(),
                                  congeni_genetic_ccc = col_logical(),
                                  malignancy_ccc = col_logical(),
                                  neonatal_ccc = col_logical(),
                                  tech_dep_ccc = col_logical(),
                                  transplant_ccc = col_logical(),
                                  num_ccc = col_double(),
                                  ccc_flag = col_logical(),
                                  Comp_Wound = col_logical(),
                                  Comp_MinorSSI = col_logical(),
                                  Comp_MajorSSI = col_logical(),
                                  Comp_UTI = col_logical(),
                                  Comp_PostRenal = col_logical(),
                                  Comp_Pneumonia = col_logical(),
                                  Comp_RespFail = col_logical(),
                                  Comp_Sepsis = col_logical(),
                                  Comp_DVT = col_logical(),
                                  Comp_PulEmb = col_logical(),
                                  Comp_AMI = col_logical(),
                                  Comp_CardiacArrest = col_logical(),
                                  Comp_CVA = col_logical(),
                                  Comp_Intraop = col_logical(),
                                  Comp_General = col_logical(),
                                  Comp_Any = col_double(),
                                  any_readmit_30 = col_logical(),
                                  any_readmit_60 = col_logical(),
                                  any_readmit_90 = col_logical(),
                                  Quality_Match = col_character(),
                                  new_admit = col_double(),
                                  newborn_flag = col_double(),
                                  new_ped_admit = col_double(),
                                  Admits_Match = col_character(),
                                  CH_Tier = col_character(),
                                  Surgery = col_character()
                                ) 
                   )


ch.data <- ch.data %>%
  mutate(fst_admtdt=dmy(fst_admtdt),
         last_dischdt=dmy(last_dischdt))
  