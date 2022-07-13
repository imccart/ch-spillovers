
choice.data.fn <- function(y, t) {
  choice.data=read_delim(paste0(path.data,"/FinalChoiceSets_",y,".txt"),delim="\t", 
                   col_types=cols(z_patid = col_double(),
                                  fst_admtdt = col_character(),
                                  last_dischdt = col_character(),
                                  pt_zip = col_double(),
                                  Type = col_character(),
                                  hosp_zip = col_double(),
                                  distance_mi = col_double(),
                                  aha_hnpi = col_character(),
                                  choice = col_logical()
      ) 
    )
  choice.data <- choice.data %>%
    mutate(fst_admtdt=dmy(fst_admtdt),
           last_dischdt=dmy(last_dischdt),
           Year=y) %>%
    filter(Type==t)
  return(choice.data)
}
  