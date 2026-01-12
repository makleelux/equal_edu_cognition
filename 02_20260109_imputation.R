# Define the list of required packages
required_packages <- c("tidyverse", "VIM", "visdat", "missForest")  

# Loop through each package and install it if not already installed
for (i in 1:length(required_packages)) {
  if (!required_packages[i] %in% installed.packages()) 
    install.packages(required_packages[i])
  
  # Load the package (character.only = TRUE allows using variable names as package names)
  library(required_packages[i], character.only = TRUE)
}

# setup 
setwd("./infile") # not to be done if already executed during the current session
today <- Sys.Date()
       
load(paste0(today, "_", "cognition_data_final.RData"))

set.seed(123)

# impute time-fixed vars based on data set with first non-missing value
time_fixed <- c("education", "education_p",
                "FMFINH", "FAUNEM", "RTHLTHCH",
                "FAMFIN", "MOVFIN", "FJOB",
                "HACOHORT", "RARACEM", "RAHISPAN", "RAGENDER")

first_non_miss <- cognition_data_final %>% 
  select(HHIDPN, time_fixed) %>% 
  group_by(HHIDPN) %>% 
  summarise(across(everything(), ~ .[which(!is.na(.))[1]]))

first_non_miss_HHIDPN <- first_non_miss$HHIDPN

# remove those with too many missings
first_non_miss <- first_non_miss %>% select(time_fixed) %>% as.data.frame()

cl <- parallel::makeCluster(10)
doParallel::registerDoParallel(cl) # set based on number of CPU cores
doRNG::registerDoRNG(seed = 123)
imp_first_non_miss <- missForest(first_non_miss, parallelize = 'forests', verbose = TRUE)$ximp
parallel::stopCluster(cl)
foreach::registerDoSEQ()
# finished after 4 iterations

# attach imputed time-fixed
cognition_data_final_imp <- cognition_data_final %>% 
  select(-names(imp_first_non_miss)) %>% 
  left_join(bind_cols(HHIDPN = first_non_miss_HHIDPN, imp_first_non_miss))

cognition_data_final_out <- cognition_data_final_imp %>% 
  select(HHIDPN, wave, RTHLTHCH, FAMFIN, FJOB,
         RARACEM, RAHISPAN, RAGENDER, RAGEY_B,
         RCOG27, RWTRESP, HACOHORT, education, education_p) %>% 
  mutate(RTHLTHCH = round(RTHLTHCH, 0))

save(cognition_data_final_out, file = paste0(today, "_", "cognition_data_final_out.RData"))
