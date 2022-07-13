
# Input baseline zipcode, hrr, hsa data -----------------------------------

zip.markets=read_excel(paste0(path.data,"/ZipHsaHrr15.xls"),
                       col_types=c("text","numeric","text","text","numeric","text","text"))

zip.markets <- zip.markets %>%
  rename(zip = zipcode15,
         hsa = hsanum,
         hsa_city = hsacity,
         hsa_state = hsastate,
         hrr = hrrnum,
         hrr_city = hrrcity,
         hrr_state = hrrstate) %>%
  mutate(zip = str_pad(zip,5, pad = "0"))


zip.fips1 <- read_csv(paste0(path.data,"/zcta-to-county.csv")) %>%
  filter(row_number() != 1) %>%
  mutate(zip = as.numeric(zcta5),
         fips = str_pad(as.numeric(county), width=5, pad="0"),
         percent_pop = as.numeric(afact)) %>%
  as_tibble() %>%
  select(zip, fips, percent_pop)

zip.max1 <- zip.fips1 %>%
  group_by(zip) %>%
  summarize(max_percent = max(percent_pop, na.rm=TRUE)) 

zip.fips1 <- zip.fips1 %>%
  inner_join(zip.max1, by=c("zip")) %>%
  filter(percent_pop==max_percent) %>%
  select(zip, fips) %>%
  group_by(zip) %>%
  mutate(zip_count=row_number()) %>%
  ungroup()

zip.fips1 <- zip.fips1 %>%
  pivot_wider(names_from=zip_count, values_from=fips, names_prefix="fips_v1_")


zip.fips2 <- read_excel(paste0(path.data,"/ZIP_COUNTY_032018.xlsx"))

zip.fips2 <- zip.fips2 %>%
  mutate(zip=as.numeric(zip),
         fips = str_pad(as.numeric(county), width=5, pad="0"),
         percent_pop = tot_ratio) %>%
  select(zip, fips, percent_pop)
  
zip.max2 <- zip.fips2 %>%
  group_by(zip) %>%
  summarize(max_percent = max(percent_pop, na.rm=TRUE))

zip.fips2 <- zip.fips2 %>%
  inner_join(zip.max2, by=c("zip")) %>%
  filter(percent_pop==max_percent) %>%
  select(zip, fips) %>%
  group_by(zip) %>%
  mutate(zip_count=row_number()) %>%
  ungroup()

zip.fips2 <- zip.fips2 %>%
  pivot_wider(names_from=zip_count, values_from=fips, names_prefix="fips_v2_")
  

zip.fips <- bind_rows(zip.fips1, zip.fips2) %>%
  distinct(zip) %>%
  left_join(zip.fips2, by="zip") %>%
  left_join(zip.fips1, by="zip") %>%
  mutate(fips = case_when(
    !is.na(fips_v2_1) ~ fips_v2_1,
    is.na(fips_v2_1) ~ fips_v1_1
  )) %>%
  select(zip, fips)



# Patient flows -----------------------------------------------------------

patient.flows.zip <- ch.data %>%
  group_by(aha_hnpi, pt_zip) %>%
  summarize(tot_patients=n()) %>%
  filter(!is.na(pt_zip)) %>%
  mutate(zip=as.numeric(pt_zip)) %>%
  left_join(zip.fips, by=c("zip")) %>%
  select(aha_hnpi, tot_patients, fips)

patient.flows.fips <- patient.flows.zip %>%
  group_by(aha_hnpi, fips) %>%
  summarize(total_cases=sum(tot_patients, na.rm=TRUE))


# Identify contiguous counties --------------------------------------------

bp.contig <- readRDS(paste0(path.data,"/county-info.rds")) %>%
  as_tibble() %>%
  mutate(fips = str_pad(geoid, width=5, pad="0")) %>%
  select(fips, starts_with("contig_")) %>%
  gather(key, fips_contig, -fips) %>%
  filter(!is.na(fips_contig) & !is.na(fips)) %>%
  select(fips, fips_contig) %>%
  mutate(contig=1) %>%
  spread(fips_contig, contig)


# Assign market variable --------------------------------------------------

## function to convert dataframe to bipartite matrix
convert_bp <- function(df, id) {
  id <- enquo(id)
  nn <- df %>% pull(!!id)
  foo <- df %>% select(-!!id) %>%
    as.matrix()
  
  rownames(foo) <- nn
  foo
}
  
## collect patient flows by contiguous counties
minimum_share=0.10

bp.hosp.fips <- patient.flows.fips %>%
  group_by(fips) %>%
  mutate(patient_share = total_cases / sum(total_cases, na.rm=TRUE)) %>%
  ungroup() %>%
  mutate(connected = as.integer(patient_share >= minimum_share),
         share = ifelse(connected==1, patient_share, 0)) %>%
  select(fips, aha_hnpi, connected) %>%
  inner_join(bp.contig %>% select(fips), by="fips") %>%
  spread(aha_hnpi, connected) %>%
  convert_bp(id = fips)

bp.hosp.fips[is.na(bp.hosp.fips)] <- 0

## create unipartite matrix out of fips x hosp matrix
up.final <- bp.hosp.fips %*% t(bp.hosp.fips)

## graph data
graph.dat <- graph_from_adjacency_matrix(up.final, weighted=TRUE) %>%
  simplify(., remove.loops = TRUE)

## cluster walktrap
initial.communities <- walktrap.community(graph.dat,
                                          steps=1,
                                          merges=TRUE,
                                          modularity=TRUE,
                                          membership=TRUE)

market <- membership(initial.communities)
walktrap.dat <- bind_cols(fips=names(market), market=market) %>%
  mutate(state_fips = str_sub(fips, 1, 2))



# Final crosswalk ---------------------------------------------------------

zip.markets <- zip.markets %>% 
  mutate(zip=as.numeric(zip)) %>%
  left_join(zip.fips, by=c("zip")) %>%
  left_join(walktrap.dat, by=c("fips"))
