# Install packages if required
required_packages <- c("haven", "dplyr", "tidyverse", "readr", "Hmisc", "mice", "stringr")  
installed_packages <- rownames(installed.packages())

for (pkg in required_packages) {
  if (!pkg %in% installed_packages) install.packages(pkg)
}

# Load Required Libraries
library(haven)     
library(dplyr)     
library(tidyverse)
library(readr)
library(Hmisc)
library(mice)
library(stringr)

setwd("./infile")
today <- Sys.Date()

set.seed(123)

# Variables --------------------------------------------------------------------

# cognition variables
cognition <- c(
  # https://hrs.isr.umich.edu/sites/default/files/biblio/COGIMP9220_dd.pdf
  'R3COG27',  # W3 27-POINT COGNITION SUMMARY SCORE
  'R4COG27',  # W4 27-POINT COGNITION SUMMARY SCORE 
  'R5COG27',  # W5 27-POINT COGNITION SUMMARY SCORE 
  'R6COG27',  # W6 27-POINT COGNITION SUMMARY SCORE 
  'R7COG27',  # W7 27-POINT COGNITION SUMMARY SCORE 
  'R8COG27',  # W8 27-POINT COGNITION SUMMARY SCORE 
  'R9COG27',  # W9 27-POINT COGNITION SUMMARY SCORE 
  'R10COG27', # W10 27-POINT COGNITION SUMMARY SCORE 
  'R11COG27', # W11 27-POINT COGNITION SUMMARY SCORE 
  'R12COG27', # W12 27-POINT COGNITION SUMMARY SCORE 
  'R13COG27', # W13 27-POINT COGNITION SUMMARY SCORE 
  'R14COG27', # W14 27-POINT COGNITION SUMMARY SCORE 
  'R15COG27'  # W15 27-POINT COGNITION SUMMARY SCORE 
)

# education variables 
education <- c(
  'RAEDUC',  # R education (categ)
  'RAMEDUC', # R Mother's Years Education
  'RAFEDUC'  # R Father's Years Education
)

# childhood variables 
childhood <- c(
  'FMFINH',   # Family got financial help in childhood
  'FAUNEM',   # Father unemployed during childhood
  'RTHLTHCH', # Rate health as child
  'FAMFIN',   # RATE FAM FINANCIAL SITUATION-SES
  'MOVFIN',   # MOVE DUE TO FINANCIAL DIFF
  'FJOB'      # FATHER USUAL OCC WHEN R AGE 16
  )

# demographics 
demographics <- c(
  'HACOHORT', #  Sample cohort
  'RADAGE_Y', # Age at Death in Years
  'RARACEM',  # R Race-masked
  'RAHISPAN', # R Hispanic
  'RAGENDER', # R Gender
  'R1AGEY_B', # W1 R Age (years) at Ivw BegMon            
  'R2AGEY_B', # W2 R Age (years) at Ivw BegMon
  'R3AGEY_B', # W3 R Age (years) at Ivw BegMon            
  'R4AGEY_B', # W4 R Age (years) at Ivw BegMon
  'R5AGEY_B', # W5 R Age (years) at Ivw BegMon            
  'R6AGEY_B', # W6 R Age (years) at Ivw BegMon
  'R7AGEY_B', # W7 R Age (years) at Ivw BegMon            
  'R8AGEY_B', # W8 R Age (years) at Ivw BegMon
  'R9AGEY_B', # W9 R Age (years) at Ivw BegMon            
  'R10AGEY_B',# W10 R Age (years) at Ivw BegMon
  'R11AGEY_B',# W11 R Age (years) at Ivw BegMon            
  'R12AGEY_B',# W12 R Age (years) at Ivw BegMon
  'R13AGEY_B',# W13 R Age (years) at Ivw BegMon            
  'R14AGEY_B',# W14 R Age (years) at Ivw BegMon
  'R15AGEY_B' # W15 R Age (years) at Ivw BegMon
)

# Weights
weights <- c(
  'R1WTRESP',
  'R2WTRESP',
  'R3WTRESP',
  'R4WTRESP',
  'R5WTRESP',
  'R6WTRESP',
  'R7WTRESP',
  'R8WTRESP',
  'R9WTRESP',
  'R10WTRESP',
  'R11WTRESP',
  'R12WTRESP',
  'R13WTRESP',
  'R14WTRESP',
  'R15WTRESP'
)

# Setup ------------------------------------------------------------------------

# adapted from https://stackoverflow.com/questions/45109400/how-can-i-read-a-da-file-directly-into-r
data.file <- "AGGCHLDFH2016A_R.da"                                                # Set path to the data file "*.DA"
dict.file <- "AGGCHLDFH2016A_R.dct"                                               # Set path to the dictionary file "*.DCT"
df.dict <- read.table(dict.file, skip = 1, fill = TRUE, stringsAsFactors = FALSE) # Read the dictionary file
colnames(df.dict) <- c("col.num","col.type","col.name","col.width","col.lbl")     # Set column names for dictionary dataframe
df.dict <- df.dict[-nrow(df.dict),]                                               # Remove last row which only contains a closing }
df.dict$col.width <- as.integer(                                                  # Extract numeric value from column width field
  sapply(
    df.dict$col.width, 
    gsub, 
    pattern = "[^0-9\\.]", 
    replacement = "")
  ) 
df.dict$col.type <- sapply(                                                       # Convert column types to format to be used with read_fwf function
  df.dict$col.type, 
  function(x){
    ifelse(x %in% c("int","byte","long"), "i", 
           ifelse(x == "float", "n", 
                  ifelse(x == "double", "d", "c")))
    }
  ) 
df.dict <- df.dict[-1,]                                                           # Remove first non-variable row (dictionary metadata)
df <- read_fwf(                                                                   # Read the data file into a dataframe
  file = data.file, 
  fwf_widths(widths = df.dict$col.width, col_names = df.dict$col.name), 
  col_types = paste(df.dict$col.type, collapse = "")) 
attributes(df)$label <- df.dict$col.lbl                                           # Add column labels to headers

# save the reformatted adversity file as .sav file
write_sav(df, paste0(today, "_", "AGGCHLDFH2016A_R.sav"))

# rand file size is 915.1mb (took roughly 2 min on my machine, 16gb ram)
randhrs <- read_sav('randhrs1992_2020v2.sav')
child_fam <- read_sav(paste0(today, "_", 'AGGCHLDFH2016A_R.sav'))

# Convert hhid and pn to numeric
child_fam$hhid <- as.numeric(child_fam$HHID)
child_fam$pn <- as.numeric(child_fam$PN)

# Create HHIDPN 
child_fam$HHIDPN <- child_fam$hhid * 1000 + child_fam$pn

# Drop duplicate variables
child_fam <- child_fam %>% select(-hhid, -pn)

# Merge all datasets using HHIDPN (left join)
merged_data <- left_join(randhrs, child_fam, by = "HHIDPN", relationship = "one-to-one") # left join with child_fam

# Save the complete dataset as a .sav file 
write_sav(merged_data, paste0(today, "_", "merged_data.sav"))
 
# Variable Selection -----------------------------------------------------------

# load merged data
merged_data <- read_sav(paste0(today, "_", "merged_data.sav"))

# select interesting variables 
merged_data_sub <- merged_data %>%
  select( 'HHIDPN', 
          cognition,
          education,
          childhood,
          demographics,
          weights,
         'HACOHORT' 
         )

# Save the selected dataset as a .sav file
write_sav(merged_data_sub, paste0(today, "_", "merged_data_sub.sav"))

# Data prep --------------------------------------------------------------------

# codebooks
# childhood family and health https://hrs.isr.umich.edu/sites/default/files/meta/xyear/childfamhealth/codebook/aggchldfh2016a_r.htm
# rand https://www.rand.org/health/surveys/hrs/hrs-data.html

merged_data_sub <- read_sav(paste0(today, "_", "merged_data_sub.sav"))

# prep and check
merged_data_prep <- merged_data_sub %>% 
  mutate(HACOHORT = as_factor(HACOHORT),
         RARACEM = as_factor(RARACEM),
         RAHISPAN = as_factor(RAHISPAN),
         RAGENDER = as_factor(RAGENDER),
         education = case_when(
           RAEDUC == 5 ~ "college",
           RAEDUC %in% c(1, 2, 3, 4) ~ "below_college") %>% 
           factor(levels = c("college", "below_college")),
         education_p = case_when(
           RAMEDUC > 8 ~ "one_parent_above_8_yedu",
           RAFEDUC > 8 ~ "one_parent_above_8_yedu",
           RAMEDUC <= 8 & RAFEDUC <= 8 ~ "no_parent_above_8_yedu",
           RAMEDUC <= 8 & is.na(RAFEDUC) ~ "no_parent_above_8_yedu",
           RAFEDUC <= 8 & is.na(RAMEDUC) ~ "no_parent_above_8_yedu",
           is.na(RAMEDUC) & is.na(RAFEDUC) ~ NA) %>% 
           factor(levels = c('one_parent_above_8_yedu', 'no_parent_above_8_yedu')),
         FMFINH = case_when(
           FMFINH == 5 ~ "no",
           FMFINH == 1 ~ "yes") %>% factor(levels = c("no", "yes")),
         FAUNEM = case_when(
           FAUNEM == 5 ~ "no",
           FAUNEM == 1 ~ "yes",
           FAUNEM == 6 ~ "f_never_worked",
           FAUNEM == 7 ~ "no_father") %>% factor(levels = c("no", "yes", "f_never_worked", "no_father")),
         FAMFIN = case_when(
           FAMFIN %in% c(5, 6) ~ "POOR",
           FAMFIN == 1 ~ "WELL",
           FAMFIN == 3 ~ "AVERAGE") %>% factor(levels = c("WELL", "AVERAGE", "POOR")),
         MOVFIN = case_when(
           MOVFIN == 5 ~ "no",
           MOVFIN == 1 ~ "yes") %>% factor(levels = c("no", "yes")),
         FJOB = case_when(
           FJOB == 1 ~ "MANAGERIAL",
           FJOB == 2 ~ "SALES",
           FJOB == 3 ~ "CLERICAL",
           FJOB == 4 ~ "SERVICE",
           FJOB == 5 ~ "MANUAL",
           FJOB == 6 ~ "ARMED") %>% factor(levels = c("MANAGERIAL", "SALES", "CLERICAL", "SERVICE", "MANUAL", "ARMED")),
         RTHLTHCH = ifelse(RTHLTHCH >= 1 & RTHLTHCH <= 5, RTHLTHCH, NA)
         ) %>% 
  select(-RAEDUC, -RAMEDUC, -RAFEDUC)

save(merged_data_prep, file = paste0(today, "_", "merged_data_prep.RData"))
load(paste0(today, "_", "merged_data_prep.RData"))

# transform to long (create wave indicator)
long_data <- function(data){
  # remove variables without wave indicator
  data_constant <- data %>% 
    select(HHIDPN,  
           education, education_p,
           FMFINH, FAUNEM, RTHLTHCH, 
           FAMFIN, MOVFIN, FJOB,
           HACOHORT, RADAGE_Y, RARACEM, RAHISPAN, RAGENDER)

  data_long <- data %>%
    # deselect constant vars except for HHIDPN
    select(-names(data_constant)[-1]) %>% 
    mutate_if(is.labelled, as.double) %>%             
    pivot_longer(
      cols = -HHIDPN, 
      names_to = "variable", 
      values_to = "value") %>%  
    mutate(
      wave = as.numeric(str_extract(variable, "\\d+")), # Wave information is extracted from the variable names
      var_name = str_remove(variable, "\\d+")) %>%      # Variable names are cleaned by removing wave numbers
    arrange(HHIDPN, wave, var_name)

    # rejoin constant vars
    data_long <- data_long %>% left_join(data_constant, by = "HHIDPN")
  
  return(data_long)
}

cognition_data_long <- long_data(merged_data_prep)

# re-transform wider (with wave indicator)
cognition_data_wide <- cognition_data_long %>% 
  select(-variable) %>% 
  pivot_wider(id_cols = c(HHIDPN, wave,
                          education, education_p,
                          FMFINH, FAUNEM, RTHLTHCH, 
                          FAMFIN, MOVFIN, FJOB,
                          HACOHORT, RADAGE_Y, RARACEM, RAHISPAN, RAGENDER), 
              names_from = var_name, values_from = value) %>% 
  # restore ordering
  select(HHIDPN, wave, names(.))

save(cognition_data_wide, file = paste0(today, "_", "cognition_data_wide.RData"))
load(paste0(today, "_", "cognition_data_wide.RData"))

cognition_data_final <- cognition_data_wide %>% 
  filter(wave > 2) %>% 
  drop_na(RCOG27)

# Write final data frame to file -----------------------------------------------

save(cognition_data_final, file = paste0(today, "_", "cognition_data_final.RData"))
