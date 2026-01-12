# Setup ------------------------------------------------------------------------

## Handle package installation

# Define the list of required packages
required_packages <- c("tidyverse", "haven", "ggplot2", "psych", "corrplot",
                       "caret", "nnet", "gbm", "ranger", "gdata", "xtable", "cdgd")  

# Loop through each package and install it if not already installed
for (i in 1:length(required_packages)) {
  if (!required_packages[i] %in% installed.packages()) 
    install.packages(required_packages[i])
  
  # Load the package (character.only = TRUE allows using variable names as package names)
  library(required_packages[i], character.only = TRUE)
}

## Setup environment and load data 

# Set a seed for reproducibility
set.seed(123)

# Set the working directory where the input file is stored
setwd("./infile") # not to be done if already executed during the current session

# Define the output path (relative to the working directory)
output_path <- "../output"

# Load the RData file containing the imputed cognition dataset
today <- Sys.Date()
load(paste0(today, "_", "cognition_data_final_out.RData"))

# Preparation ------------------------------------------------------------------

## Filter and preprocess cognitive data based on age 

cognition <- cognition_data_final_out %>% 
  
  # Keep only rows where age (RAGEY_B) is between 50 and 60 (inclusive)
  filter(RAGEY_B >= 50 & RAGEY_B <= 60) %>% 
  
  # Group the data by a unique respondent identifier (HHIDPN)
  group_by(HHIDPN) %>% 
  
  # Create a new variable 'ind' that numbers the rows within each group
  # For example, if a person has 3 time points, they'll get 1, 2, 3
  mutate(ind = 1:n()) %>% 
  
  # Remove the grouping so further operations treat the data as a whole
  ungroup()

# Functions --------------------------------------------------------------------
## Function: run_cdgd0
## Purpose: Run a non-parametric decomposition of educational effects
##          using a flexible machine learning model (default: GBM).

run_cdgd0 <- function(df, 
                      edu_p_levels = c("no_parent_above_8_yedu", "one_parent_above_8_yedu"), 
                      method = "gbm",
                      outcome = "RCOG27", # may also be "cogfunction"
                      covariates,
                      use_weights = FALSE) {
  
  # -------------------------------
  # Function parameters:
  # -------------------------------
  # df:             The input data frame.
  # edu_p_levels:   Two-level string for parental education. The first level
  #                 is used as the reference category during dummy coding.
  # method:         Estimation method; can be "gbm", "nnet", or "param".
  # outcome:        Character name of outcome variable; can be RCOG27 or cogfunction.
  # covariates:     Vector containing names of covariates to account for.
  # use_weights:    Boolean indicating if analysis should be weighted or not.
  
  set.seed(123)
  
  # -------------------------------
  # STEP 1: Data preparation
  # -------------------------------
  
  temp_dat <- df %>% 
    mutate(
      
      # Set levels for education
      education = factor(education, levels = c("below_college", "college")),
      
      # Set levels for parental education according to edu_p_levels
      education_p = factor(education_p, levels = edu_p_levels)
      
      ) %>% 
    
    # Keep only covariates and outcome variables for modeling
    select(all_of(covariates), education, education_p, RWTRESP)
  
  # -------------------------------
  # STEP 2: Dummy code categorical variables
  # -------------------------------
  temp_dat <- temp_dat %>% 
    select_if(is.numeric) %>%     
    cbind(
      predict(
        # Create dummy variables (excluding the reference level)
        dummyVars(~ ., data = temp_dat %>% select_if(is.factor), 
                  fullRank = TRUE, sep = "_"),
        newdata = temp_dat
      )
    )
    
  # -------------------------------
  # STEP 3: Clean variable names
  # -------------------------------
  names(temp_dat) <- str_replace(names(temp_dat), "[.]", "_")   # Replace dots
  names(temp_dat) <- str_replace(names(temp_dat), "[/]", "_")   # Replace slashes
  names(temp_dat) <- str_replace(names(temp_dat), " ", "_")     # Replace spaces
  
  # Identify covariates (exclude outcome and treatment indicators)
  cov_clean <- setdiff(
    names(temp_dat), 
    c("education_college", paste0("education_p_", edu_p_levels[[2]]), "RWTRESP")
  )
  
  # -------------------------------
  # STEP 4: Outcome definition
  # -------------------------------
  if (outcome == "RCOG27") {
    temp_dat$y = df$RCOG27
  } else if (outcome == "cogfunction") {
    temp_dat$y = ifelse(df$cogfunction == "Normal", 1, 0)
  } else {
      stop("no valid outcome chosen")  # Error if unsupported outcome is passed
  }
  
  # -------------------------------
  # STEP 5: Run the decomposition model
  # -------------------------------
  if (use_weights) {
    if (method %in% c("nnet", "gbm")) {
      result <- cdgd0_ml(
        Y = "y",                                     # Outcome
        D = "education_college",                     # Treatment (own education)
        G = paste0("education_p_", edu_p_levels[2]), # Group (parental education)
        X = cov_clean,                               # Covariates
        algorithm = method,                          # Algorithm: gbm or nnet
        data = temp_dat,
        weight = "RWTRESP"
      )
    } else if (method == "param") {
      result <- cdgd0_pa(
        Y = "y",                                     # Outcome
        D = "education_college",
        G = paste0("education_p_", edu_p_levels[2]),
        X = cov_clean,
        data = temp_dat,
        weight = "RWTRESP"
      )
    } else {
      stop("no valid method chosen")  # Error if unsupported method is passed
    }
  }
  else {
    if (method %in% c("nnet", "gbm")) {
      result <- cdgd0_ml(
        Y = "y",                                     # Outcome
        D = "education_college",                     # Treatment (own education)
        G = paste0("education_p_", edu_p_levels[2]), # Group (parental education)
        X = cov_clean,                               # Covariates
        algorithm = method,                          # Algorithm: gbm or nnet
        data = temp_dat
      )
    } else if (method == "param") {
      result <- cdgd0_pa(
        Y = "y",                                     # Outcome
        D = "education_college",
        G = paste0("education_p_", edu_p_levels[2]),
        X = cov_clean,
        data = temp_dat
      )
    } else {
      stop("no valid method chosen")  # Error if unsupported method is passed
    }
  }
  
  return(result)
}

# Function: plot_dat_result
# Purpose: Prepare decomposition results for plotting by cleaning and
#          formatting the results into a tidy dataframe.

plot_dat_result <- function(result) {
  # Extract results table and convert it to a data frame
  plot_df <- as.data.frame(result$results) %>% 
    # Move the row names into a new column called "rowname"
    rownames_to_column() %>% 
    
    # Create and mutate columns:
    mutate(
      # Capitalize and label the components for better readability
      Component = case_when(
        rowname == "total"    ~ "Total",
        rowname == "baseline" ~ "Baseline",
        rowname == "prevalence" ~ "Prevalence",
        rowname == "effect"   ~ "Effect",
        rowname == "selection" ~ "Selection"
      ) %>% 
        # Convert Component into an ordered factor for consistent plotting order
        factor(levels = c("Total", "Baseline", "Prevalence", "Effect", "Selection")),
      
      # Rename the 'point' column to 'Estimate' for clarity
      Estimate = point
    ) %>% 
    
    # Select and reorder columns for the final output
    select(Component, Estimate, se, p_value, CI_lower, CI_upper)
  
  # Return the cleaned and formatted dataframe
  return(plot_df)
}

# Function: plot_style_bar
# Purpose: Helper function for your standard bar plot style

plot_style_bar <- function(y_limits = c(-0.75, 2.5), y_breaks = 10, guide = "legend") {
  list(
    geom_bar(stat = "identity", position = position_dodge2(width = 0.9)),
    geom_errorbar(width = 0.4, linewidth = 0.6, position = position_dodge(width = 0.9)),
    geom_hline(yintercept = 0),
    scale_fill_viridis_d(guide = guide),
    theme_minimal(),
    theme(text = element_text(size = 20),
          legend.position = "bottom"),
    scale_y_continuous(breaks = scales::pretty_breaks(n = y_breaks)),
    coord_cartesian(ylim = y_limits)
  )
}

# Function: filter_age_group
# Purpose: Helper function to filter according to age bounds provided 
#          in a character vector.

filter_age_group <- function(df, age_group) {
  # Parse lower and upper age bounds from the input string
  bounds <- strsplit(age_group, "-")[[1]]
  lower  <- as.numeric(bounds[1])
  upper  <- as.numeric(bounds[2])
  
  # Filter dataset for this age group and keep first observation per person
  df_age_group <- df %>%
    filter(RAGEY_B >= lower & RAGEY_B <= upper) %>%
    group_by(HHIDPN) %>%
    slice_head(n = 1) %>%
    ungroup()
  
  return(df_age_group)
}

# Covariates, Age Groups, Cohorts ----------------------------------------------
# Define the covariates (variables) to be used in the model
covariates_all <- c("wave", "RTHLTHCH", "FAMFIN", "FJOB", "RARACEM", "RAHISPAN", "RAGENDER", "RAGEY_B")
covariates_gen <- c("wave", "RTHLTHCH", "FAMFIN", "FJOB", "RARACEM", "RAHISPAN", "RAGEY_B") # by RAGENDER, without RAGENDER
covariates_rac <- c("wave", "RTHLTHCH", "FAMFIN", "FJOB", "RAHISPAN", "RAGENDER", "RAGEY_B") # by RARACEM, without RARACEM
covariates_his <- c("wave", "RTHLTHCH", "FAMFIN", "FJOB", "RARACEM", "RAGENDER", "RAGEY_B") # by RAHISPAN, without RAHISPAN
covariates_pxy <- c("wave", "RTHLTHCH", "FAMFIN", "FJOB", "RARACEM", "RAHISPAN", "RAGENDER", "RAGEY_B", "proxy") # with proxy
covariates_mod <- c("wave", "RTHLTHCH", "FAMFIN", "FJOB", "RARACEM", "RAHISPAN", "RAGENDER", "RAGEY_B", "iwmode") # with mode

# Define age groups (50 to 60 already captured with "all")
age_groups <- c("55-65", "60-70", "65-75", "70-80")

# Define cohorts
cohorts <- cognition_data_final_out$HACOHORT %>% summary %>% names

# ALL 50-60 w/wo weights -------------------------------------------------------

# Select only the first observation for each person (HHIDPN) 
# in the age restricted (50-60 years data set)
all <- cognition %>% filter(ind == 1) 

# Run the GBM-based decomposition analysis on the filtered data
result_all <- run_cdgd0(df = all, covariates = covariates_all)
result_all_weighted <- run_cdgd0(df = all, covariates = covariates_all, use_weights = TRUE)

# Save the results object to an external file for later use
save(result_all, file = paste(output_path, "/", today, "_all.RData", sep=""))
save(result_all_weighted, file = paste(output_path, "/", today, "_all_weighted.RData", sep=""))

# Process the results for plotting (tidy the output)
plot_dat_all <- plot_dat_result(result_all)
plot_dat_all_weighted <- plot_dat_result(result_all_weighted)

# Create a bar plot of the decomposition components except the "Total" component
plot_all <- plot_dat_all %>% 
  filter(Component != "Total") %>%    # Exclude the total disparity bar
  ggplot(aes(x = Component, y = Estimate, ymin = CI_lower, ymax = CI_upper, fill = Component)) + 
  plot_style_bar(guide = "none") +
  # Add dashed line indicating total disparity value
  geom_hline(yintercept = plot_dat_all$Estimate[plot_dat_all$Component == "Total"], 
             linetype = "dashed", color = "tomato3", size = 0.7) + 
  # Label the dashed line
  annotate("text", x = 4, y = min(plot_dat_all$Estimate[plot_dat_all$Component == "Total"]) - 0.1, 
           label = "Total Disparity", 
           size = 5, 
           color = "tomato3")

plot_all_weighted <- plot_dat_all_weighted %>% 
  filter(Component != "Total") %>%    # Exclude the total disparity bar
  ggplot(aes(x = Component, y = Estimate, ymin = CI_lower, ymax = CI_upper, fill = Component)) + 
  plot_style_bar(guide = "none") +
  # Add dashed line indicating total disparity value
  geom_hline(yintercept = plot_dat_all_weighted$Estimate[plot_dat_all_weighted$Component == "Total"], 
             linetype = "dashed", color = "tomato3", size = 0.7) + 
  # Label the dashed line
  annotate("text", x = 4, y = min(plot_dat_all_weighted$Estimate[plot_dat_all_weighted$Component == "Total"]) - 0.1, 
           label = "Total Disparity", 
           size = 5, 
           color = "tomato3")

# Save the plot as a PNG file with specified dimensions and resolution
png(paste(output_path, "/", today, "_all.png", sep = ""), width = 7, height = 5, res = 300, units = "in")
plot_all
dev.off()

# Save the plot as a PNG file with specified dimensions and resolution
png(paste(output_path, "/", today, "_all_weighted.png", sep = ""), width = 7, height = 5, res = 300, units = "in")
plot_all_weighted
dev.off()

# ALL 50-60 for Gender-subsets -------------------------------------------------

# filter gender and first obs per HHIDPN
male <- cognition %>% filter(ind == 1 & RAGENDER == "1.Male") 
female <- cognition %>% filter(ind == 1 & RAGENDER == "2.Female") 

# compute GBMs
result_male <- run_cdgd0(df = male, covariates = covariates_gen)
result_female <- run_cdgd0(df = female, covariates = covariates_gen)

save(result_male, file = paste(output_path, "/", today, "_male.RData", sep=""))
save(result_female, file = paste(output_path, "/", today, "_female.RData", sep=""))

plot_dat_male <- plot_dat_result(result_male)
plot_dat_female <- plot_dat_result(result_female)

# create plot
plot_gender <- plot_dat_male %>% mutate(Gender = "Male") %>% 
  rbind(plot_dat_female %>% mutate(Gender = "Female")) %>% 
  ggplot(aes(x=Component, y=Estimate, ymin = CI_lower, ymax = CI_upper, fill = Gender)) + 
  plot_style_bar()

png(paste(output_path, "/", today, "_gender.png", sep=""), width=7, height=5, res = 300, units = "in")
plot_gender
dev.off()

# ALL 50-60 for Race-subsets ---------------------------------------------------

# filter RAHISPAN and first obs per HHIDPN
white <- cognition %>% filter(ind == 1 & RARACEM  == "1.White/Caucasian") 
black <- cognition %>% filter(ind == 1 & RARACEM  == "2.Black/African American") 
other <- cognition %>% filter(ind == 1 & RARACEM  == "3.Other") 

# compute GBMs
result_white <- run_cdgd0(df = white, covariates = covariates_rac)
result_black <- run_cdgd0(df = black, covariates = covariates_rac)
result_other <- run_cdgd0(df = other, covariates = covariates_rac)

save(result_white, file = paste(output_path, "/", today, "_white.RData", sep=""))
save(result_black, file = paste(output_path, "/", today, "_black.RData", sep=""))
save(result_other, file = paste(output_path, "/", today, "_other.RData", sep=""))

plot_dat_white <- plot_dat_result(result_white)
plot_dat_black <- plot_dat_result(result_black)
plot_dat_other <- plot_dat_result(result_other)

# create plot
plot_race <- plot_dat_white %>% mutate(Race = "White/Caucasian") %>% 
  rbind(plot_dat_black %>% mutate(Race = "Black/African American")) %>%
  rbind(plot_dat_other %>% mutate(Race = "Other")) %>%
  ggplot(aes(x=Component, y=Estimate, ymin = CI_lower, ymax = CI_upper, fill = Race)) + 
  plot_style_bar()

png(paste(output_path, "/", today, "_race.png", sep=""), width=7, height=5, res = 300, units = "in")
plot_race
dev.off()

# ALL 50-60 for Ethnicity-subsets ----------------------------------------------

# filter RAHISPAN and first obs per HHIDPN
hispanic <- cognition %>% filter(ind == 1 & RAHISPAN == "1.Hispanic") 
not_hispanic <- cognition %>% filter(ind == 1 & RAHISPAN == "0.Not Hispanic") 

# compute GBMs
result_hispanic <- run_cdgd0(df = hispanic, covariates = covariates_his)
result_not_hispanic <- run_cdgd0(df = not_hispanic, covariates = covariates_his)

save(result_hispanic, file = paste(output_path, "/", today, "_hispanic.RData", sep=""))
save(result_not_hispanic, file = paste(output_path, "/", today, "_not_hispanic.RData", sep=""))

plot_dat_hispanic <- plot_dat_result(result_hispanic)
plot_dat_not_hispanic <- plot_dat_result(result_not_hispanic)

# create plot
plot_hispanic <- plot_dat_hispanic %>% mutate(Ethnicity = "Hispanic") %>% 
  rbind(plot_dat_not_hispanic %>% mutate(Ethnicity = "Not Hispanic")) %>% 
  ggplot(aes(x=Component, y=Estimate, ymin = CI_lower, ymax = CI_upper, fill = Ethnicity)) + 
  plot_style_bar()

png(paste(output_path, "/", today, "_hispanic.png", sep=""), width=7, height=5, res = 300, units = "in")
plot_hispanic
dev.off()

# ALL 50-60 with nnet ----------------------------------------------------------

# compute nnet
result_all_nnet <- run_cdgd0(df = all, covariates = covariates_all, method = "nnet")

save(result_all_nnet, file = paste(output_path, "/", today, "_all_nnet.RData", sep=""))

plot_dat_all_nnet <- plot_dat_result(result_all_nnet)

# create plot
plot_all_nnet <- plot_dat_all_nnet %>% 
  filter(Component != "Total") %>% 
  ggplot(aes(x=Component, y=Estimate, ymin = CI_lower, ymax = CI_upper, fill = Component)) + 
  plot_style_bar(guide = "none") +
  geom_hline(yintercept = plot_dat_all_nnet$Estimate[plot_dat_all_nnet$Component == "Total"],
             linetype = "dashed", color = "tomato3", size = .7) +
  annotate("text", x=4, y=min(plot_dat_all_nnet$Estimate[plot_dat_all_nnet$Component == "Total"])-.1, 
           label= "Total Disparity", 
           size=5,
           color = "tomato3")

png(paste(output_path, "/", today, "_all_nnet.png", sep=""), width=7, height=5, res = 300, units = "in")
plot_all_nnet
dev.off()

# ALL 50-60 with param ---------------------------------------------------------

# compute param
result_all_param <- run_cdgd0(df = all, covariates = covariates_all, method = "param")

save(result_all_param, file = paste(output_path, "/", today, "_all_param.RData", sep=""))

plot_dat_all_param <- plot_dat_result(result_all_param)

# create plot
plot_all_param <- plot_dat_all_param %>% 
  filter(Component != "Total") %>% 
  ggplot(aes(x=Component, y=Estimate, ymin = CI_lower, ymax = CI_upper, fill = Component)) + 
  plot_style_bar(guide = "none") +
  geom_hline(yintercept = plot_dat_all_param$Estimate[plot_dat_all_param$Component == "Total"],
             linetype = "dashed", color = "tomato3", size = .7) +
  annotate("text", x=4, y=min(plot_dat_all_param$Estimate[plot_dat_all_param$Component == "Total"])-.1, 
           label= "Total Disparity", 
           size=5,
           color = "tomato3")

png(paste(output_path, "/", today, "_all_param.png", sep=""), width=7, height=5, res = 300, units = "in")
plot_all_param
dev.off()

# ALL 50-60, median split of parental education --------------------------------

# Note that individuals from earlier cohorts are removed 
# since continuous years of education, coded binary with 7.5 and 8.5, could not be recovered

# load df containing raw RAFEDUC/RAMEDUC
merged_data_sub <- read_sav(paste0(today, "_merged_data_sub.sav"))

all_edu_sens <- cognition %>% 
  filter(ind == 1) %>% 
  left_join(merged_data_sub %>% select(HHIDPN, RAFEDUC, RAMEDUC), relationship = "one-to-one") %>% 
  # filter participants with binary parental education
  # filter participants with missing info on both parental education variables (previously imputed)
  filter(
    ( !is.na(RAFEDUC) & !(RAFEDUC %in% c(7.5, 8.5)) ) |
      ( !is.na(RAMEDUC) & !(RAMEDUC %in% c(7.5, 8.5)) )
  ) 

median_RAMEDUC = median(all_edu_sens$RAMEDUC, na.rm = T)
median_RAFEDUC = median(all_edu_sens$RAFEDUC, na.rm = T)

all_edu_sens <- all_edu_sens %>% 
  mutate(
    education_p = factor(case_when(
      RAMEDUC >= median_RAMEDUC ~ "one_parent_above_median",
      RAFEDUC >= median_RAFEDUC ~ "one_parent_above_median",
      RAMEDUC < median_RAMEDUC & RAFEDUC < median_RAFEDUC ~ "no_parent_above_median",
      RAMEDUC < median_RAMEDUC & is.na(RAFEDUC) ~ "no_parent_above_median",
      RAFEDUC < median_RAFEDUC & is.na(RAMEDUC) ~ "no_parent_above_median",
      is.na(RAMEDUC) & is.na(RAFEDUC) ~ NA),
      levels = c('one_parent_above_median', 'no_parent_above_median'))) %>% 
  select(HHIDPN, covariates_all, RCOG27, education, education_p, RWTRESP)

# compute GBM
result_all_edu_sens  <- run_cdgd0(df = all_edu_sens, covariates = covariates_all, edu_p_levels = c("no_parent_above_median", "one_parent_above_median"))

save(result_all_edu_sens, file = paste(output_path, "/", today, "_all_edu_sens.RData", sep=""))

plot_dat_all_edu_sens <- plot_dat_result(result_all_edu_sens)

# create plot
plot_all_edu_sens <- plot_dat_all_edu_sens %>% 
  filter(Component != "Total") %>% 
  ggplot(aes(x=Component, y=Estimate, ymin = CI_lower, ymax = CI_upper, fill = Component)) + 
  plot_style_bar(guide = "none") + 
  geom_hline(yintercept = plot_dat_all_edu_sens$Estimate[plot_dat_all_edu_sens$Component == "Total"],
             linetype = "dashed", color = "tomato3", size = .7) +
  annotate("text", x=4, y=min(plot_dat_all_edu_sens$Estimate[plot_dat_all_edu_sens$Component == "Total"])-.1, 
           label= "Total Disparity", 
           size=5,
           color = "tomato3")

png(paste(output_path, "/", today, "_all_edu_sens.png", sep=""), width=7, height=5, res = 300, units = "in")
plot_all_edu_sens
dev.off()

# ALL no age restriction, per age group ----------------------------------------

# Loop over all age groups 
# For every age group, keep the first observation per ID
# Repeat analytic steps used in above analyses

for(age_group in age_groups){
  
  # Run the GBM-based decomposition analysis on the filtered data
  # Try running the function on filtered data for the current age_group
  res_age_group <- tryCatch(
    {
      run_cdgd0(
        df = filter_age_group(df = cognition_data_final_out, age_group = age_group), 
        covariates = covariates_all)
    },
    error = function(e) {
      message(paste("Error in run_cdgd0 for age_group", age_group, ":", e$message))
      # Return NULL or some placeholder so the loop continues
      return(NULL)
    }
  )
  
  # Save the results object to an external file for later use
  save(res_age_group, file = paste(output_path, "/", today, "_", str_replace(age_group,"-", "to"), ".RData", sep=""))
  
  # Process the results for plotting (tidy the output)
  plot_dat_age_group <- plot_dat_result(res_age_group)
  
  # Create a bar plot of the decomposition components except the "Total" component
  plot_age_group <- plot_dat_age_group %>% 
    filter(Component != "Total") %>%    # Exclude the total disparity bar
    ggplot(aes(x = Component, y = Estimate, ymin = CI_lower, ymax = CI_upper, fill = Component)) + 
    plot_style_bar(guide = "none") + 
    # Add dashed line indicating total disparity value
    geom_hline(yintercept = plot_dat_age_group$Estimate[plot_dat_age_group$Component == "Total"], 
               linetype = "dashed", color = "tomato3", size = 0.7) + 
    # Label the dashed line
    annotate("text", x = 4, y = min(plot_dat_age_group$Estimate[plot_dat_age_group$Component == "Total"]) - 0.1, 
             label = "Total Disparity", 
             size = 5, 
             color = "tomato3")                        
  
  # Define file path and name for saving plot
  filename <- paste(output_path, "/", today, "_", str_replace(age_group,"-", "to"), ".png", sep = "")
  
  # Save the plot as a PNG file with specified dimensions and resolution
  plot(plot_age_group)
  ggsave(filename = filename, plot = last_plot(), width = 7, height = 5, dpi = 300, units = "in")
}

# ALL no age restriction, per cohort -------------------------------------------

# Loop over all cohorts 
# For every cohort, keep the first observation per ID
# Repeat analytic steps used in above analyses

for(cohort in cohorts[2:8]){
  
  # Run the GBM-based decomposition analysis on the filtered data
  # Try running the function on filtered data for the current cohort
  all_cohort <- cognition_data_final_out %>%
    filter(HACOHORT == cohort) %>%                   # Use dynamic cohort
    group_by(HHIDPN) %>%
    slice_head(n = 1) %>%                            # First observation per person
    ungroup()
  
  res_cohort <- tryCatch(
    {
      run_cdgd0(df = all_cohort, covariates = covariates_all)
    },
    error = function(e) {
      message(paste("Error in run_cdgd0 for cohort", cohort, ":", e$message))
      # Return NULL or some placeholder so the loop continues
      return(NULL)
    }
  )
  
  # Save the results object to an external file for later use
  save(res_cohort, file = paste(output_path, "/", today, "_", cohort, ".RData", sep=""))
  
  # Process the results for plotting (tidy the output)
  plot_dat_cohort <- plot_dat_result(res_cohort)
  
  # Create a bar plot of the decomposition components except the "Total" component
  plot_cohort <- plot_dat_cohort %>% 
    filter(Component != "Total") %>%    # Exclude the total disparity bar
    ggplot(aes(x = Component, y = Estimate, ymin = CI_lower, ymax = CI_upper, fill = Component)) + 
    plot_style_bar(guide = "none") + 
    # Add dashed line indicating total disparity value
    geom_hline(yintercept = plot_dat_cohort$Estimate[plot_dat_cohort$Component == "Total"], 
               linetype = "dashed", color = "tomato3", size = 0.7) + 
    # Label the dashed line
    annotate("text", x = 4, y = min(plot_dat_cohort$Estimate[plot_dat_cohort$Component == "Total"]) - 0.1, 
             label = "Total Disparity", 
             size = 5, 
             color = "tomato3")
  
  # Define file path and name for saving plot
  filename <- paste(output_path, "/", today, "_", cohort, ".png", sep = "")
  
  # Save the plot as a PNG file with specified dimensions and resolution
  plot(plot_cohort)
  ggsave(filename = filename, plot = last_plot(), width = 7, height = 5, dpi = 300, units = "in")
}

# ALL no age restriction, per age group, with LW cognition as outcome ----------

# see https://hrsdata.isr.umich.edu/data-products/langa-weir-classification-cognitive-function-1995-2020
lw <- read_dta("cogfinalimp_9520wide.dta")

# Create HHIDPN
lw$HHIDPN <- as.numeric(lw$hhid) * 1000 + as.numeric(lw$pn)

# Select variables
lw_sub <- lw %>% select(HHIDPN, contains(c("cogfunction", "proxy")))

# Pivot longer and extract study year
lw_long <- lw_sub %>% 
  pivot_longer(
    cols = matches("\\d{4}$"),               # pick all columns ending with 4 digits
    names_to = c("variable", "year"),        # split into variable and year
    names_pattern = "(.+?)(\\d{4})$",        # group 1 = variable name, group 2 = year
    values_to = "value"
  )

# Pivot wider and match year to wave
lw_filtered <- lw_long %>% 
  pivot_wider(names_from = variable, values_from = value) %>% 
  drop_na(cogfunction) %>% 
  mutate(
    cogfunction = case_when(
      cogfunction %in% c(2, 3) ~ "Impaired", # includes Demented and CIND
      cogfunction == 1 ~ "Normal") %>% 
      factor(levels = c("Impaired", "Normal")),
    proxy = case_when(
      proxy == 1 ~ "Spouse",
      proxy == 2 ~ "Other",
      proxy == 5 ~ "Self"
      ) %>% 
      factor(levels = c("Self", "Spouse", "Other")), 
    # https://hrsdata.isr.umich.edu/sites/default/files/documentation/other/1746818083/randhrs1992_2022v1.pdf
    wave = case_when(
      year %in% c(1995, 1996) ~ 3,
      year == 1998 ~ 4,
      year == 2000 ~ 5,
      year == 2002 ~ 6,
      year == 2004 ~ 7,
      year == 2006 ~ 8,
      year == 2008 ~ 9,
      year == 2010 ~ 10,
      year == 2012 ~ 11,
      year == 2014 ~ 12,
      year == 2016 ~ 13,
      year == 2018 ~ 14,
      year == 2020 ~ 15
      )
  )

# read cognition data still including proxies (prior to imputation)
load(paste0(today, "_cognition_data_wide.RData"))

# Join cogfunction status by wave and HHIDPN
cognition_with_proxies <- cognition_data_wide %>%
  filter(wave > 2) %>% 
  left_join(lw_filtered %>% select(HHIDPN, wave, cogfunction, proxy), relationship = "one-to-one") %>% 
  select(HHIDPN, cogfunction, covariates_pxy, education, education_p, RCOG27, RWTRESP) %>% 
  drop_na(covariates_pxy, education_p, education) 

# Loop over all age groups 
# For every age group
# For every age group, keep the first observation per ID
# Repeat analytic steps used in above analyses

for(age_group in age_groups){

  # Run the GBM-based decomposition analysis on the filtered data
  # Try running the function on filtered data for the current age_group
  res_age_group <- tryCatch(
    {
      run_cdgd0(df = filter_age_group(
        df = cognition_with_proxies, age_group = age_group), 
        outcome = "cogfunction", 
        covariates = covariates_pxy)
    },
    error = function(e) {
      message(paste("Error in run_cdgd0 for age_group", age_group, ":", e$message))
      # Return NULL or some placeholder so the loop continues
      return(NULL)
    }
  )

  # Save the results object to an external file for later use
  save(res_age_group, file = paste(output_path, "/", today, "_", str_replace(age_group,"-", "to"), "_proxy.RData", sep=""))
  
  # Process the results for plotting (tidy the output)
  plot_dat_age_group <- plot_dat_result(res_age_group)
  
  # Create a bar plot of the decomposition components except the "Total" component
  plot_age_group <- plot_dat_age_group %>% 
    filter(Component != "Total") %>%    # Exclude the total disparity bar
    ggplot(aes(x = Component, y = Estimate, ymin = CI_lower, ymax = CI_upper, fill = Component)) + 
    plot_style_bar(guide = "none") +
    # Add dashed line indicating total disparity value
    geom_hline(yintercept = plot_dat_age_group$Estimate[plot_dat_age_group$Component == "Total"], 
               linetype = "dashed", color = "tomato3", size = 0.7) + 
    # Label the dashed line
    annotate("text", x = 4, y = min(plot_dat_age_group$Estimate[plot_dat_age_group$Component == "Total"]) - 0.03, 
             label = "Total Disparity", 
             size = 5, 
             color = "tomato3") +
    coord_cartesian(ylim = c(-0.1, .3))
  
  # Define file path and name for saving plot
  filename <- paste(output_path, "/", today, "_", str_replace(age_group,"-", "to"), "_proxy.png", sep = "")
  
  # Save the plot as a PNG file with specified dimensions and resolution
  plot(plot_age_group)
  ggsave(filename = filename, plot = last_plot(), width = 7, height = 5, dpi = 300, units = "in")
}

# ALL no age restriction, per age group, with iwmode as covariate --------------

# see https://hrsdata.isr.umich.edu/data-products/cross-wave-tracker-file
tracker = read_sav("trk2022tr_r.sav")

# Reshape iwmode columns long
tracker_long <- tracker %>%
  select(hhid, pn, matches("^[A-Z]iwmode$")) %>%                # select iwmode vars
  mutate(HHIDPN = as.numeric(hhid) * 1000 + as.numeric(pn)) %>% # compute HHIDPN
  pivot_longer(
    cols = matches("^[A-Z]iwmode$"),                            # select iwmode vars
    names_to = c("year_char", ".value"),                        # split into wave letter and variable name
    names_pattern = "([A-Za-z])(.+)"                            # capture leading letter + rest (iwmode)
  ) %>%
  # note year specific char
  mutate(
    year = case_when(
      year_char == "a" ~ 1992,
      year_char == "b" ~ 1993,
      year_char == "c" ~ 1994,
      year_char == "d" ~ 1995,
      year_char == "e" ~ 1996,
      year_char == "f" ~ 1998,
      year_char == "g" ~ 2000,
      year_char == "h" ~ 2002,
      year_char == "j" ~ 2004,
      year_char == "k" ~ 2006,
      year_char == "l" ~ 2008,
      year_char == "m" ~ 2010,
      year_char == "n" ~ 2012,
      year_char == "o" ~ 2014,
      year_char == "p" ~ 2016,
      year_char == "q" ~ 2018,
      year_char == "r" ~ 2020,
      year_char == "s" ~ 2022,
      TRUE ~ NA_real_
    )
  ) %>%
  select(HHIDPN, year_char, year, iwmode)

# Cohort-specific year char mapping
# see https://hrsdata.isr.umich.edu/sites/default/files/documentation/other/1746818083/randhrs1992_2022v1.pdf
# page 13
cohort_map <- tribble(
  ~HACOHORT, ~wave, ~year_char_map,
  # HRS cohort
  "3.Hrs", 1, "a",
  "3.Hrs", 2, "c",
  "3.Hrs", 3, "e",
  "3.Hrs", 4, "f",
  "3.Hrs", 5, "g",
  "3.Hrs", 6, "h",
  "3.Hrs", 7, "j",
  "3.Hrs", 8, "k",
  "3.Hrs", 9, "l",
  "3.Hrs",10, "m",
  "3.Hrs",11, "n",
  "3.Hrs",12, "o",
  "3.Hrs",13, "p",
  "3.Hrs",14, "q",
  "3.Hrs",15, "r",
  
  # AHEAD cohort (0/1)
  "1.Ahead", 1, "a",
  "1.Ahead", 2, "b",
  "1.Ahead", 3, "d",
  "1.Ahead", 4, "f",
  "1.Ahead", 5, "g",
  "1.Ahead", 6, "h",
  "1.Ahead", 7, "j",
  "1.Ahead", 8, "k",
  "1.Ahead", 9, "l",
  "1.Ahead",10, "m",
  "1.Ahead",11, "n",
  "1.Ahead",12, "o",
  "1.Ahead",13, "p",
  "1.Ahead",14, "q",
  "1.Ahead",15, "r",
  
  # CODA cohort (2) 
  "2.Coda", 4, "f",
  "2.Coda", 5, "g",
  "2.Coda", 6, "h",
  "2.Coda", 7, "j",
  "2.Coda", 8, "k",
  "2.Coda", 9, "l",
  "2.Coda",10, "m",
  "2.Coda",11, "n",
  "2.Coda",12, "o",
  "2.Coda",13, "p",
  "2.Coda",14, "q",
  "2.Coda",15, "r",
  
  # War Baby cohort (4)
  "4.WarBabies", 4, "f",
  "4.WarBabies", 5, "g",
  "4.WarBabies", 6, "h",
  "4.WarBabies", 7, "j",
  "4.WarBabies", 8, "k",
  "4.WarBabies", 9, "l",
  "4.WarBabies", 10, "m",
  "4.WarBabies", 11, "n",
  "4.WarBabies", 12, "o",
  "4.WarBabies", 13, "p",
  "4.WarBabies", 14, "q",
  "4.WarBabies", 15, "r",
  
  # Early Baby Boomer (5) 
  "5.Early BabyBoomers", 7, "j",
  "5.Early BabyBoomers", 8, "k",
  "5.Early BabyBoomers", 9, "l",
  "5.Early BabyBoomers", 10, "m",
  "5.Early BabyBoomers", 11, "n",
  "5.Early BabyBoomers", 12, "o",
  "5.Early BabyBoomers", 13, "p",
  "5.Early BabyBoomers", 14, "q",
  "5.Early BabyBoomers", 15, "r",
  
  # Mid Baby Boomer (6) 
  "6.Mid BabyBoomers", 10, "m",
  "6.Mid BabyBoomers", 11, "n",
  "6.Mid BabyBoomers", 12, "o",
  "6.Mid BabyBoomers", 13, "p",
  "6.Mid BabyBoomers", 14, "q",
  "6.Mid BabyBoomers", 15, "r",
  
  # Late Baby Boomer (7) 
  "7.Late BabyBoomers", 13, "p",
  "7.Late BabyBoomers", 14, "q",
  "7.Late BabyBoomers", 15, "r"
)

all_modes <- cognition_data_final_out %>% 
  left_join(cohort_map, by = c("HACOHORT", "wave")) %>%                    # join cohort and wave specific year char
  left_join(tracker_long, by = c("HHIDPN", "year_char_map" = "year_char"), # join year_char specific iwmode 
            relationship = "one-to-one") %>% 
  mutate(
    iwmode = case_when(
      iwmode == 1 ~ "Face_to_face",
      iwmode == 2 ~ "Telefone",
      iwmode == 3 ~ "Web"
    ) %>% factor(levels = c("Face_to_face", "Telefone", "Web"))) %>% 
  drop_na(iwmode)

# Loop over all age groups 
# For every age group, keep the first observation per ID
# Repeat analytic steps used in above analyses

for(age_group in age_groups){
  
  # Run the GBM-based decomposition analysis on the filtered data
  # Try running the function on filtered data for the current age_group
  res_age_group <- tryCatch(
    {
      run_cdgd0(df = filter_age_group(
        df = all_modes, age_group = age_group), 
        covariates = covariates_mod)
    },
    error = function(e) {
      message(paste("Error in run_cdgd0 for age_group", age_group, ":", e$message))
      # Return NULL or some placeholder so the loop continues
      return(NULL)
    }
  )
  
  # Save the results object to an external file for later use
  save(res_age_group, file = paste(output_path, "/", today, "__all_modes", str_replace(age_group,"-", "to"), ".RData", sep=""))
  
  # Process the results for plotting (tidy the output)
  plot_dat_age_group <- plot_dat_result(res_age_group)
  
  # Create a bar plot of the decomposition components except the "Total" component
  plot_age_group <- plot_dat_age_group %>% 
    filter(Component != "Total") %>%    # Exclude the total disparity bar
    ggplot(aes(x = Component, y = Estimate, ymin = CI_lower, ymax = CI_upper, fill = Component)) + 
    plot_style_bar(guide = "none") +
    # Add dashed line indicating total disparity value
    geom_hline(yintercept = plot_dat_age_group$Estimate[plot_dat_age_group$Component == "Total"], 
               linetype = "dashed", color = "tomato3", size = 0.7) + 
    # Label the dashed line
    annotate("text", x = 4, y = min(plot_dat_age_group$Estimate[plot_dat_age_group$Component == "Total"]) - 0.1, 
             label = "Total Disparity", 
             size = 5, 
             color = "tomato3") 
  
  # Define file path and name for saving plot
  filename <- paste(output_path, "/", today, "__all_modes", str_replace(age_group,"-", "to"), ".png", sep = "")
  
  # Save the plot as a PNG file with specified dimensions and resolution
  plot(plot_age_group)
  ggsave(filename = filename, plot = last_plot(), width = 7, height = 5, dpi = 300, units = "in")
}
