#### Load Libraries and Fetch datasets ####

install.packages("tidyverse")
install.packages("dplyr")
install.packages("readr")
install.packages("openxlsx")
install.packages("countrycode")
install.packages("ggplot2")
install.packages("ggrepel")
install.packages("sandwich")
install.packages("lmtest")
install.packages("MASS")
install.packages("scales")

library(tidyverse)
library(dplyr)
library(readr)
library(openxlsx)
library(countrycode)
library(ggplot2)
library(ggrepel)
library(sandwich)
library(lmtest)
library(MASS)
library(scales)

#Age dependency ratio
age_depend <- read.csv("age-dependency-ratio-of-working-age-population.csv")

#Median age
median_age <- read.csv("median-age.csv")

#outcomes
cvd_deaths <- read.xlsx("cvd_mortality.xlsx")
all_cause <- read.xlsx("all_cause.xlsx")
diabetes <- read.xlsx("diabetes_new.xlsx")
obesity <- read.csv("obesity.csv") 

#TB mortality
tb_mortality <- read.xlsx("tb_mortality_stat.xlsx")

# Rename "Sex" to "sex" in each data frame
all_cause <- all_cause %>%
  rename(sex = Sex)

cvd_deaths <- cvd_deaths %>%
  rename(sex = Sex)

tb_mortality <- tb_mortality %>%
  rename(sex = Sex)

diabetes <- diabetes %>%
  rename(sex = Sex)

obesity <- obesity %>%
  rename(sex = Sex)



#Agricultural Employment
agri <- read.xlsx("agri.xlsx")

#Mini dataset with countries in Tessa's paper but not in my merged dataset all_df
old_who <- read.xlsx("old_who.xlsx")


##Educational attainment (at least primary scool)
education <- read.xlsx("education.xlsx")

# Reshape the data from wide to long format
education_long <- education %>%
  pivot_longer(
    cols = starts_with("19") | starts_with("20"), # Adjust as needed for year columns
    names_to = "Year",
    values_to = "Educational_Attainment",
    values_drop_na = TRUE # Drops NA values immediately
  )

# Convert the Year column to numeric for easier sorting
education_long$Year <- as.numeric(education_long$Year)

# For each country, select the most recent available year
education_recent <- education_long %>%
  group_by(country) %>%
  filter(Year == max(Year, na.rm = TRUE)) %>%
  ungroup()

education <- education_recent[,c(1,2,3)]

##Smoking prevalence
smoking_strat <- read.xlsx("sex_strat_smoke.xlsx")

##Air pollution (PM25)
pm <- read.csv("pm25-air-pollution.csv")

#GDP per capita
gdp <- read.csv("gdp_per_capita.csv")

names(gdp)[1] <- "country"

##Dataset from WHO cleaning script

all_df <- read.csv("combined_df_cleaned_mod_new.csv")

# Transform 'sex' column from numeric to categorical, preserving NA as NA
all_df <- all_df %>%
  mutate(sex = case_when(
    sex == 1 ~ "Male",
    sex == 2 ~ "Female",
    TRUE ~ as.character(sex)  # Keeps NA or other values as-is
  ))

### Summarize WHO STEPS data for ecological analysis
summary_stats <- all_df %>%
  filter(!is.na(sex)) %>%  # Filter out rows where sex is NA
  group_by(iso3_code, sex, country_class, population_2022, region) %>%
  summarise(
    omvpa_mean = mean(omvpa, na.rm = TRUE),
    nonocpa_mean = mean(nonocpa, na.rm = TRUE),
    total_mvpa_mean = mean(total_mvpa, na.rm = TRUE)
  )


#### Set Up ####

# Add ISO 3 codes 

add_iso3_code <- function(df, country_col = "country") {
  df$iso3_code <- countrycode(df[[country_col]], origin = "country.name", destination = "iso3c")
  return(df)
}

dataframes <- list(age_depend, median_age, tb_mortality, cvd_deaths, all_cause, diabetes, agri, obesity, smoking_strat, pm, gdp,education)

# Apply the function to each dataframe
dataframes <- lapply(dataframes, add_iso3_code)

# Assign the updated dataframes back to their original names
list2env(setNames(dataframes, c("age_depend", "median_age", "tb_mortality", "cvd_deaths", "all_cause", "diabetes", "agri", "obesity", "smoking_strat", "pm", "gdp","education")), envir = .GlobalEnv)

old_who$iso3_code <- countrycode(old_who$country, origin = "country.name", destination = "iso3c")

old_who <- merge(old_who,education)

old_who <- old_who[,c(1:21,23)]

gdp[219,3] <- "STP"

# Convert sex column in summary_stats to character to match summary_stats_combined
final_summary <- summary_stats %>%
  mutate(sex = as.character(sex))


cvd_deaths$iso3_code[cvd_deaths$country == "Lebanese Republic"] <- "LBN"
all_cause$iso3_code[all_cause$country == "Lebanese Republic"] <- "LBN"
tb_mortality$iso3_code[tb_mortality$country == "Lebanese Republic"] <- "LBN"
diabetes$iso3_code[diabetes$country == "Lebanese Republic"] <- "LBN"

cvd_deaths$iso3_code[cvd_deaths$country == "Republic of Guyana"] <- "GUY"
all_cause$iso3_code[all_cause$country == "Republic of Guyana"] <- "GUY"
tb_mortality$iso3_code[tb_mortality$country == "Republic of Guyana"] <- "GUY"
diabetes$iso3_code[diabetes$country == "Republic of Guyana"] <- "GUY"


###Stepwise merging

#Merge to GDP

final_summary <- merge(final_summary,gdp,by="iso3_code")
final_summary <- final_summary[,c(1:8,10)]


final_merged <- final_summary

# Merge age_depend 
final_merged <- merge(final_summary, age_depend, by = "iso3_code", all.x = TRUE)

# Merge median_age 
final_merged <- merge(final_merged, median_age, by = "iso3_code", all.x = TRUE)

# Merge agri 
final_merged <- merge(final_merged, agri, by = "iso3_code", all.x = TRUE)

# Merge smoking_strat 
final_merged <- merge(final_merged, smoking_strat, by = "iso3_code", all.x = TRUE)

# Merge pm 
final_merged <- merge(final_merged, pm, by = "iso3_code", all.x = TRUE)

final_merged <- final_merged[,c(1:9,11,13,15,17:19,21)]

# Merge obesity 
final_merged <- merge(final_merged, obesity, by = c("iso3_code", "sex"), all.x = TRUE)

# Merge education
final_merged <- merge(final_merged, education, by = c("iso3_code"), all.x = TRUE)

final_merged <- final_merged[,c(1:16,18,21)]

names(final_merged)[13] <- "year"

#Merge diabetes and mortality by iso3_code, sex, and age_name
diabetes_mortality_merged <- merge(diabetes, all_cause, by = c("location_id","sex"), all = TRUE)
diabetes_mortality_merged <- merge(diabetes_mortality_merged, cvd_deaths, by = c("location_id", "sex"), all = TRUE)
diabetes_mortality_merged <- merge(diabetes_mortality_merged, tb_mortality, by = c("location_id", "sex"), all = TRUE)

diabetes_mortality_merged <- diabetes_mortality_merged[,c(1:5,7,10,13)]


names(diabetes_mortality_merged)[3] <- "country"
names(diabetes_mortality_merged)[5] <- "iso3_code"


#Merge the combined diabetes_mortality dataset with final_merged by iso3_code, sex then remove both country.x and country.y
final_merged <- merge(final_merged, diabetes_mortality_merged, by = c("iso3_code", "sex"), all.x = TRUE)

##Remove location_id

final_merged <- final_merged[,c(1:18,21:24)]

names(final_merged)[1] <- "country"


old_who <- old_who %>%
  rename(sex=Sex)


# Create country-year mapping and merge into old_who
old_who <- old_who %>%
  left_join(
    data.frame(
      country = c("BFA", "BRA", "BTN", "CHL", "CHN", "EGY", "IND", "MEX", "PNG", 
                    "RUS", "SYC", "TTO", "USA", "ZAF"),
      year_raw = c("2013", "2013-14", "2014", "2016-17", "2008-10", "2017", "2007-08", 
                   "2009-10", "2007-08", "2007-10", "2013-14", "2011", "2017-18", "2007-08")
    ) %>%
      mutate(year = case_when(
        grepl("-", year_raw) ~ as.integer(sub(".*-(\\d{2,4})$", "20\\1", year_raw)),
        TRUE ~ as.integer(year_raw)
      )) %>%
      dplyr::select(country, year),
    by = "country"
  )

old_who <- old_who[,c(2:23)]

names(old_who)[1] <- "country"

#Merge with remaining countries from old Tessa paper
final_merged <- rbind(final_merged,old_who)


##Impute missing data from small island nations based on sub regions (Caribbean and Pacific)

# Define your country groups
caribbean_countries <- c("AIA", "ATG", "BHS", "BRB", "VGB", "BMU", "CYM", "GRD", 
                         "HTI", "JAM","LCA","TTO", "VIR", "PRI")

pacific_countries <- c("COK", "SYC", "FSM", "FJI", 
                       "KIR", "NRU", "NIU", "PLW","PYF", "MHL", 
                       "WSM", "ASM", "SLB", "TKL", "TON", 
                       "TUV", "VUT", "MNP", "GUM","WLF")

# Caribbean and Pacific means for variables
caribbean_means <- list(
  pm_25 = mean(final_merged$pm_25[final_merged$country %in% caribbean_countries & !is.na(final_merged$pm_25)], na.rm = TRUE),
  agri = mean(final_merged$agri[final_merged$country %in% caribbean_countries & !is.na(final_merged$agri)], na.rm = TRUE),
  age_depend = mean(final_merged$age_depend[final_merged$country %in% caribbean_countries & !is.na(final_merged$age_depend)], na.rm = TRUE),
  median_age = mean(final_merged$median_age[final_merged$country %in% caribbean_countries & !is.na(final_merged$median_age)], na.rm = TRUE),
  Educational_Attainment = mean(final_merged$Educational_Attainment[final_merged$country %in% caribbean_countries & !is.na(final_merged$Educational_Attainment)], na.rm = TRUE),
  male_smoke = mean(final_merged$male_smoke[final_merged$country %in% caribbean_countries & !is.na(final_merged$male_smoke)], na.rm = TRUE),
  female_smoke = mean(final_merged$female_smoke[final_merged$country %in% caribbean_countries & !is.na(final_merged$female_smoke)], na.rm = TRUE)
)

pacific_means <- list(
  pm_25 = mean(final_merged$pm_25[final_merged$country %in% pacific_countries & !is.na(final_merged$pm_25)], na.rm = TRUE),
  agri = mean(final_merged$agri[final_merged$country %in% pacific_countries & !is.na(final_merged$agri)], na.rm = TRUE),
  age_depend = mean(final_merged$age_depend[final_merged$country %in% pacific_countries & !is.na(final_merged$age_depend)], na.rm = TRUE),
  median_age = mean(final_merged$median_age[final_merged$country %in% pacific_countries & !is.na(final_merged$median_age)], na.rm = TRUE),
  Educational_Attainment = mean(final_merged$Educational_Attainment[final_merged$country %in% pacific_countries & !is.na(final_merged$Educational_Attainment)], na.rm = TRUE),
  male_smoke = mean(final_merged$male_smoke[final_merged$country %in% pacific_countries & !is.na(final_merged$male_smoke)], na.rm = TRUE),
  female_smoke = mean(final_merged$female_smoke[final_merged$country %in% pacific_countries & !is.na(final_merged$female_smoke)], na.rm = TRUE)
)

# Function to impute missing values with sub-regional means
impute_with_mean <- function(df, variable, caribbean_mean, pacific_mean) {
  df[[variable]] <- ifelse(df$country %in% caribbean_countries & is.na(df[[variable]]), caribbean_mean, df[[variable]])
  df[[variable]] <- ifelse(df$country %in% pacific_countries & is.na(df[[variable]]), pacific_mean, df[[variable]])
  return(df)
}

# Impute missing values for pm_25, agri, Educational_Attainment, smoking
final_merged <- impute_with_mean(final_merged, "pm_25", caribbean_means$pm_25, pacific_means$pm_25)
final_merged <- impute_with_mean(final_merged, "agri", caribbean_means$agri, pacific_means$agri)
final_merged <- impute_with_mean(final_merged, "Educational_Attainment", caribbean_means$Educational_Attainment, pacific_means$Educational_Attainment)
final_merged <- impute_with_mean(final_merged, "age_depend", caribbean_means$age_depend, pacific_means$age_depend)
final_merged <- impute_with_mean(final_merged, "median_age", caribbean_means$median_age, pacific_means$median_age)
final_merged <- impute_with_mean(final_merged, "male_smoke", caribbean_means$male_smoke, pacific_means$male_smoke)
final_merged <- impute_with_mean(final_merged, "female_smoke", caribbean_means$female_smoke, pacific_means$female_smoke)


final_merged$gdp_per_capita <- as.numeric(final_merged$gdp_per_capita)

##Rename Oceania to Smal Island Nations, more appropriate
final_merged <- final_merged %>%
  mutate(region = ifelse(region == "Oceania", "Small Island Nations", region))

##Keep complete cases for analysis

complete_data <- final_merged %>%
  dplyr::filter(
    !is.na(cvd_deaths) &
      !is.na(all_cause) &
      !is.na(obesity_rate) &
      !is.na(t2d_prev) &
      !is.na(Educational_Attainment) &
      !is.na(gdp_per_capita) &
      !is.na(country_class) &
      !is.na(age_depend) &
      !is.na(male_smoke) &
      !is.na(female_smoke) &
      !is.na(median_age) &
      !is.na(agri) &
      !is.na(tb_deaths) &
      !is.na(pm_25)
  )


final_merged <- complete_data

write_csv(final_merged,"pa_eco_new.csv")


#### PA Ecological Models ####

final_merged <- read.csv("pa_eco_new.csv")

##Drop Republic of Congo, missing travel and occupational PA
final_merged <- final_merged %>%
  filter(country!="COG")

# Z scores for MVPA
final_merged$z_omvpa_mean <- as.numeric(scale(final_merged$omvpa_mean, center = TRUE, scale = TRUE))
final_merged$z_nonocpa_mean <- as.numeric(scale(final_merged$nonocpa_mean, center = TRUE, scale = TRUE))
final_merged$z_total_mvpa_mean <- as.numeric(scale(final_merged$total_mvpa_mean, center = TRUE, scale = TRUE))

sd(final_merged$omvpa_mean, na.rm = TRUE)
sd(final_merged$nonocpa_mean, na.rm = TRUE)
sd(final_merged$total_mvpa_mean, na.rm = TRUE)


final_merged$obesity <- final_merged$obesity_rate

# Convert obesity and smoking from percentage to proportion
final_merged <- final_merged %>%
  mutate(obesity_rate = obesity_rate / 100)

final_merged <- final_merged %>%
  mutate(male_smoke = male_smoke / 100)

final_merged <- final_merged %>%
  mutate(female_smoke = female_smoke / 100)

final_merged$country_class <- as.factor(final_merged$country_class)

final_merged$country_class <- relevel(final_merged$country_class, ref = "Low-income countries")

#Rates to counts

final_merged <- final_merged %>%
  mutate(cvd_deaths = cvd_deaths * (population_2022 / 100000),
         all_cause = all_cause * (population_2022 / 100000),
         t2d_prev = t2d_prev * (population_2022 / 100000))


final_merged <- final_merged %>%
  mutate(
    cvd_deaths = ceiling(cvd_deaths),  # Round up cvd_deaths
    all_cause = ceiling(all_cause),
    t2d_prev = ceiling(t2d_prev) # Round up all_cause
  )

#Covariates
covariates <- c("country_class","median_age", "age_depend", "gdp_per_capita", "Educational_Attainment","agri","male_smoke", "pm_25", "tb_deaths", "obesity")
covariates_no_obesity <- setdiff(covariates, "obesity")

#Outcomes
outcomes <- c("cvd_deaths", "all_cause", "t2d_prev")

# Extract coefficients and calculate robust confidence intervals
extract_model_results_robust_manual <- function(model, relevant_vars, model_name, outcome) {
  coefs <- exp(coef(model)[relevant_vars])
  robust_se <- sqrt(diag(vcovHC(model, type = "HC0")))[relevant_vars]  # Calculate robust standard errors
  
  # Calculate robust confidence intervals
  z_value <- 1.96  # For 95% confidence intervals
  confint_lower <- exp(coef(model)[relevant_vars] - z_value * robust_se)
  confint_upper <- exp(coef(model)[relevant_vars] + z_value * robust_se)
  
  # results dataframe
  result_df <- data.frame(
    Outcome = outcome,
    Model_Type = model_name,
    Coefficient = relevant_vars,
    Estimate = round(coefs, 3),
    CI_Lower = round(confint_lower, 3),
    CI_Upper = round(confint_upper, 3)
  )
  
  return(result_df)
}


run_pa_models <- function(data, outcome, covariates, covariates_no_obesity, offset_var, sex_filter) {
  
  # Define the correct smoking variable based on sex
  smoke_var <- ifelse(sex_filter == "Male", "male_smoke", "female_smoke")
  
  # Replace generic 'smoke' variable with the sex-specific one
  covariates <- gsub("male_smoke", smoke_var, covariates)
  covariates_no_obesity <- gsub("male_smoke", smoke_var, covariates_no_obesity)
  
  # Filter dataset by sex and remove missing values in outcome, offset, and covariates
  data_filtered <- data %>%
    filter(sex == sex_filter) %>%
    filter(!is.na(.data[[outcome]]), !is.na(.data[[offset_var]])) %>%
    drop_na(all_of(c(covariates, covariates_no_obesity)))
  
  if(nrow(data_filtered) == 0) {
    stop("Filtered data has no rows. Please check the input data or filters.")
  }
  
  # data frame to store results
  results <- data.frame()
  
  # PA exposures
  exposures <- c("z_omvpa_mean", "z_nonocpa_mean", "z_total_mvpa_mean")
  
  # Loop through each PA variable
  for (exposure in exposures) {
    
    # Define the exposure term for the model
    relevant_vars <- exposure
    
    # Model 1: PA variable only
    model_1 <- glm.nb(as.formula(paste(outcome, "~", exposure, "+ offset(log(", offset_var, "))")), data = data_filtered)
    
    # Model 2: PA variable + covariates without obesity
    model_2 <- glm.nb(as.formula(paste(outcome, "~", exposure, "+", paste(covariates_no_obesity, collapse = " + "), "+ offset(log(", offset_var, "))")), 
                      data = data_filtered)
    
    # Model 3: PA variable + all covariates
    model_3 <- glm.nb(as.formula(paste(outcome, "~", exposure, "+", paste(covariates, collapse = " + "), "+ offset(log(", offset_var, "))")),
                      data = data_filtered)
    
    # Collect results from all three models using a helper function
    results <- rbind(
      results,
      extract_model_results_robust_manual(model_1, relevant_vars, paste(exposure, "PA_Model1", sep = "_"), outcome),
      extract_model_results_robust_manual(model_2, relevant_vars, paste(exposure, "PA_Model2", sep = "_"), outcome),
      extract_model_results_robust_manual(model_3, relevant_vars, paste(exposure, "PA_Model3", sep = "_"), outcome)
    )
  }
  
  return(results)
}



# Run the PA models by sex
male_pa_results <- lapply(outcomes, function(outcome) run_pa_models(final_merged, outcome, covariates, covariates_no_obesity, "population_2022", "Male"))
female_pa_results <- lapply(outcomes, function(outcome) run_pa_models(final_merged, outcome, covariates, covariates_no_obesity, "population_2022", "Female"))

# Combine the results into a single dataframe for each sex
combined_male_pa <- do.call(rbind, male_pa_results)
combined_female_pa <- do.call(rbind, female_pa_results)

combined_male_pa$Sex <- "Male"
combined_female_pa$Sex <- "Female"

# Combine male and female results into one dataframe
combined_pa_results <- rbind(combined_male_pa, combined_female_pa)


### Obesity Models

covariates <- c("country_class","median_age", "age_depend", "gdp_per_capita", "Educational_Attainment","agri","male_smoke", "pm_25", "tb_deaths")

run_obesity_pa_models <- function(data, sex_filter) {
  # Choose the correct smoking variable based on sex
  smoke_var <- ifelse(sex_filter == "Male", "male_smoke", "female_smoke")
  
  # Update the covariates to include the correct smoking variable
  covariates <- gsub("male_smoke", smoke_var, covariates)
  
  # Filter out rows with missing values in the outcome and covariates
  data_filtered <- data %>%
    filter(sex == sex_filter) %>%
    filter(!is.na(obesity_rate)) %>%
    drop_na(all_of(c(covariates)))
  
  results <- data.frame()
  
  # Define the exposures (PA variables)
  exposures <- c("z_omvpa_mean", "z_nonocpa_mean", "z_total_mvpa_mean")
  
  for (exposure in exposures) {
    relevant_vars <- exposure
    
    # Model 1: PA variable only
    model_1 <- glm(as.formula(paste("obesity_rate ~", exposure)),
                   family = quasibinomial(link = "logit"), data = data_filtered)
    
    # Model 2: PA variable + covariates
    model_2 <- glm(as.formula(paste("obesity_rate ~", exposure, "+", paste(covariates, collapse = " + "))),
                   family = quasibinomial(link = "logit"), data = data_filtered)
    
    # Collect results from PA models
    results <- rbind(
      results,
      extract_model_results_robust_manual(model_1, relevant_vars, paste(exposure, "Obesity_Model1", sep = "_"), "obesity_rate"),
      extract_model_results_robust_manual(model_2, relevant_vars, paste(exposure, "Obesity_Model2", sep = "_"), "obesity_rate")
    )
  }
  
  return(results)
}


# Run the obesity PA models for both sexes
male_obesity_pa_results <- run_obesity_pa_models(final_merged, "Male")
female_obesity_pa_results <- run_obesity_pa_models(final_merged, "Female")

# Combine the results into a single dataframe for each sex
combined_male_obesity_pa <- do.call(rbind, list(male_obesity_pa_results))
combined_female_obesity_pa <- do.call(rbind, list(female_obesity_pa_results))

combined_male_obesity_pa$sex <- "Male"
combined_female_obesity_pa$sex <- "Female"

# Combine male and female results into one dataframe
combined_obesity_pa_results <- rbind(combined_male_obesity_pa, combined_female_obesity_pa)

names(combined_pa_results)[7] <- "sex"


# Combine all results
all_results <- rbind(combined_pa_results, combined_obesity_pa_results)

# Add a significance column based on confidence intervals
all_results <- all_results %>%
  mutate(Significance = ifelse(CI_Lower <= 1 & CI_Upper >= 1, "NS", "*"))


##Meta analyze male and female results

# Function to calculate standard error from confidence intervals
ci_to_se <- function(ci_lower, ci_upper) {
  (ci_upper - ci_lower) / (2 * 1.96)
}

# Function to perform meta-analysis
meta_analysis <- function(female_estimate, female_se, male_estimate, male_se) {
  # Calculate weights (inverse of variance)
  female_weight <- 1 / (female_se ^ 2)
  male_weight <- 1 / (male_se ^ 2)
  
  # Pooled estimate
  pooled_estimate <- (female_estimate * female_weight + male_estimate * male_weight) / (female_weight + male_weight)
  
  # Pooled standard error
  pooled_se <- sqrt(1 / (female_weight + male_weight))
  
  # Confidence intervals
  pooled_ci_lower <- pooled_estimate - 1.96 * pooled_se
  pooled_ci_upper <- pooled_estimate + 1.96 * pooled_se
  
  return(c(pooled_estimate, pooled_ci_lower, pooled_ci_upper))
}

# Pivot wider
all_results <- all_results %>%
  pivot_wider(
    names_from = sex,  # Columns to create (Male, Female)
    values_from = c(Estimate, CI_Lower, CI_Upper, Significance),  # Values to spread
    names_sep = "."  # Separator for new column names
  )

all_results <- all_results %>%
  mutate(
    Female_SE = ci_to_se(CI_Lower.Female, CI_Upper.Female),  # Calculate Female SE
    Male_SE = ci_to_se(CI_Lower.Male, CI_Upper.Male)         # Calculate Male SE
  ) %>%
  rowwise() %>%
  mutate(
    # Apply the meta-analysis function
    Both = list(meta_analysis(Estimate.Female, Female_SE, Estimate.Male, Male_SE))
  ) %>%
  unnest_wider(Both, names_sep = "_") %>%  # Unnest the results into separate columns
  rename(
    Both_Estimate = Both_1,                
    Both_CI_Lower = Both_2,                
    Both_CI_Upper = Both_3                 
  ) %>%
  ungroup()



# Rename outcomes for cleaner labels
all_results <- all_results %>%
  mutate(
    Outcome = case_when(
      Outcome == "cvd_deaths" ~ "CVD Mortality",
      Outcome == "all_cause" ~ "All-Cause Mortality",
      Outcome == "obesity_rate" ~ "Obesity Prevalence",
      Outcome == "t2d_prev" ~ "Type 2 Diabetes Prevalence",
      TRUE ~ Outcome  # Default if no match
    ),
    # Simplify Model_Type names
    Model_Type = case_when(
      grepl("z_omvpa_mean", Model_Type) ~ paste("OMVPA", sub(".*Model", "Model", Model_Type)),
      grepl("z_nonocpa_mean", Model_Type) ~ paste("Non-Occupational PA", sub(".*Model", "Model", Model_Type)),
      grepl("z_total_mvpa_mean", Model_Type) ~ paste("Total MVPA", sub(".*Model", "Model", Model_Type)),
      TRUE ~ Model_Type
    )
  )


# Define custom ordering logic
custom_order <- all_results %>%
  arrange(
    factor(Outcome, levels = c(
      "CVD Mortality", "All-Cause Mortality", 
      "Type 2 Diabetes Prevalence", "Obesity Prevalence"  # Desired order of outcomes
    )),
    factor(
      Model_Type,
      levels = c(
        "OMVPA Model 1", "OMVPA Model 2", "OMVPA Model 3",
        "Non-Occupational PA Model 1", "Non-Occupational PA Model 2", "Non-Occupational PA Model 3",
        "Total MVPA Model 1", "Total MVPA Model 2", "Total MVPA Model 3"
      )
    )
  )

# Create a new column for the custom label
custom_order <- custom_order %>%
  mutate(Outcome_Model = paste(Outcome, Model_Type, sep = " - ")) %>%
  mutate(Outcome_Model = factor(Outcome_Model, levels = rev(unique(Outcome_Model))))  # Reverse order

# Plot with the reordered labels
ggplot(custom_order, aes(x = Both_Estimate, y = Outcome_Model)) +
  geom_point(size = 3) +  # Increase point size
  geom_errorbarh(aes(xmin = Both_CI_Lower, xmax = Both_CI_Upper), height = 0.3, size = 1) +  # Error bars
  geom_vline(xintercept = 1, linetype = "dashed", color = "black", size = 1) +  # Null effect line
  theme_minimal() +
  labs(
    x = "RR per SD of Domain-Specific PA",
    y = NULL,
    title = "Domain-Specific PA and Health Endpoints"
  ) +
  theme(
    plot.title = element_text(hjust = 0.5, size = 14),
    axis.text.y = element_text(size = 10, face = "bold"),
    axis.text.x = element_text(size = 12)
  ) +
  scale_x_log10()  # Log scale for x-axis



write_csv(all_results,"eco_results_lower.csv")

#### Higher Level Ecological Set Up and Models ####

data <- read.xlsx('global_set.xlsx')

data$obesity <- data$obesity_rate

# Convert obesity_rate and smoking from percentage to proportion 
data <- data %>%
  mutate(obesity_rate = obesity_rate / 100)

data <- data %>%
  mutate(male_smoke = male_smoke / 100)

data <- data %>%
  mutate(female_smoke = female_smoke / 100)


data <- data %>%
  mutate(cvd_deaths = cvd_deaths * (population / 100000),
         all_cause = all_cause * (population / 100000),
         t2d_prev = t2d_prev * (population / 100000))


data <- data %>%
  mutate(
    cvd_deaths = ceiling(cvd_deaths),  # Round up cvd_deaths
    all_cause = ceiling(all_cause),
    t2d_prev = ceiling(t2d_prev) # Round up all_cause
  )


data$agri <- scale(data$agri)


# Negative binomial regression for all-cause mortality with population offset
model_all_cause <- glm.nb(all_cause ~ agri + Educational_Attainment + obesity_rate + gdp + median_age + age_depend + male_smoke + female_smoke + pm_25 + 
                            offset(log(population)), data = data)

# Negative binomial regression for CVD mortality with population offset
model_cvd_deaths <- glm.nb(cvd_deaths ~ agri + Educational_Attainment + obesity_rate + gdp + median_age + age_depend + male_smoke + female_smoke + pm_25 + 
                             offset(log(population)), data = data)

# Negative binomial regression for Type 2 diabetes prevalence with population offset
model_t2d_prev <- glm.nb(t2d_prev ~ agri + Educational_Attainment + obesity_rate + gdp + median_age + age_depend + male_smoke + female_smoke + pm_25 + tb_deaths + 
                           offset(log(population)), data = data)

# Model 4: Quasibinomial regression for obesity rate (remains as is)
model_obesity_rate <- glm(obesity_rate ~ agri + Educational_Attainment + median_age + age_depend + gdp + male_smoke + female_smoke + pm_25 + tb_deaths, 
                          family = quasibinomial(link = "logit"), data = data)

# extract exponentiated coefficients and CIs for Negative Binomial/Quasibinomial models
extract_exponentiated_results <- function(model) {
  coefs <- exp(coef(model))  # Exponentiate coefficients
  conf_int <- exp(confint(model))  # Exponentiate confidence intervals
  results <- data.frame(
    Coefficient = names(coefs),
    Estimate = coefs,
    CI_Lower = conf_int[, 1],
    CI_Upper = conf_int[, 2]
  )
  return(results)
}

# Extract results for each model
results_all_cause <- extract_exponentiated_results(model_all_cause)
results_cvd_deaths <- extract_exponentiated_results(model_cvd_deaths)
results_t2d_prev <- extract_exponentiated_results(model_t2d_prev)
results_obesity_rate <- extract_exponentiated_results(model_obesity_rate)

# Filter for the "agri" variable only 
results_all_cause_agri <- results_all_cause[results_all_cause$Coefficient == "agri", ]
results_cvd_deaths_agri <- results_cvd_deaths[results_cvd_deaths$Coefficient == "agri", ]
results_t2d_prev_agri <- results_t2d_prev[results_t2d_prev$Coefficient == "agri", ]
results_obesity_rate_agri <- results_obesity_rate[results_obesity_rate$Coefficient == "agri", ]

# Combine the results for the forest plot
combined_results <- rbind(
  cbind(results_all_cause_agri, Outcome = "All-Cause Mortality"),
  cbind(results_cvd_deaths_agri, Outcome = "CVD Mortality"),
  cbind(results_t2d_prev_agri, Outcome = "Type 2 Diabetes Prevalence"),
  cbind(results_obesity_rate_agri, Outcome = "Obesity Prevalence")
)

combined_results$Outcome <- fct_rev(combined_results$Outcome)

ggplot(combined_results, aes(x = Estimate, y = Outcome)) +
  geom_point(size = 3) +  # Increase point size
  geom_errorbarh(aes(xmin = CI_Lower, xmax = CI_Upper), height = 0.3, size = 1) +  # Increase error bar thickness
  geom_vline(xintercept = 1, linetype = "dashed", color = "black", size = 1) +  # Increase thickness of reference line
  theme_minimal() +
  labs(x = "RR per SD of % Agricultural Employment", y = NULL, title = "Agriculture (% Employment)") +
  theme(
    plot.title = element_text(hjust = 0.5, size = 14),
    axis.text.y = element_text(size = 12, face = "bold"),
    axis.text.x = element_text(size = 12)
  ) +
  scale_x_log10()  # Apply log scale to x-axis


#### Higher Level All Models (200) ####

# Diabetes Models
model_diabetes_1 <- glm.nb(
  t2d_prev ~ agri + offset(log(population)), 
  data = data
)

model_diabetes_2 <- glm.nb(
  t2d_prev ~ agri + Educational_Attainment + gdp + median_age + age_depend + 
    male_smoke + female_smoke + pm_25 + tb_deaths + offset(log(population)), 
  data = data
)

model_diabetes_3 <- glm.nb(
  t2d_prev ~ agri + Educational_Attainment + obesity_rate + gdp + 
    median_age + age_depend + male_smoke + female_smoke + pm_25 + 
    tb_deaths + offset(log(population)), 
  data = data
)

# CVD Models
model_cvd_1 <- glm.nb(
  cvd_deaths ~ agri + offset(log(population)), 
  data = data
)

model_cvd_2 <- glm.nb(
  cvd_deaths ~ agri + Educational_Attainment + gdp + median_age + age_depend + 
    male_smoke + female_smoke + pm_25 + offset(log(population)), 
  data = data
)

model_cvd_3 <- glm.nb(
  cvd_deaths ~ agri + Educational_Attainment + obesity_rate + gdp + 
    median_age + age_depend + male_smoke + female_smoke + pm_25 + 
    offset(log(population)), 
  data = data
)

# All-Cause Mortality Models
model_all_cause_1 <- glm.nb(
  all_cause ~ agri + offset(log(population)), 
  data = data
)

model_all_cause_2 <- glm.nb(
  all_cause ~ agri + Educational_Attainment + gdp + median_age + age_depend + 
    male_smoke + female_smoke + pm_25 + offset(log(population)), 
  data = data
)

model_all_cause_3 <- glm.nb(
  all_cause ~ agri + Educational_Attainment + obesity_rate + gdp + 
    median_age + age_depend + male_smoke + female_smoke + pm_25 + 
    offset(log(population)), 
  data = data
)

# Obesity Models
model_obesity_1 <- glm(
  obesity_rate ~ agri, 
  family = quasibinomial(link = "logit"), 
  data = data
)

model_obesity_2 <- glm(
  obesity_rate ~ agri + Educational_Attainment + median_age + age_depend + 
    gdp + male_smoke + female_smoke + pm_25 + tb_deaths, 
  family = quasibinomial(link = "logit"), 
  data = data
)


# Function to extract exponentiated coefficients and CIs
extract_exponentiated_results <- function(model) {
  coefs <- exp(coef(model))  # Exponentiate coefficients
  conf_int <- exp(confint(model))  # Exponentiate confidence intervals
  results <- data.frame(
    Coefficient = names(coefs),
    Estimate = coefs,
    CI_Lower = conf_int[, 1],
    CI_Upper = conf_int[, 2]
  )
  return(results)
}

# Extract results for each model
# Diabetes Models
results_diabetes_1 <- extract_exponentiated_results(model_diabetes_1)
results_diabetes_2 <- extract_exponentiated_results(model_diabetes_2)
results_diabetes_3 <- extract_exponentiated_results(model_diabetes_3)

# CVD Models
results_cvd_1 <- extract_exponentiated_results(model_cvd_1)
results_cvd_2 <- extract_exponentiated_results(model_cvd_2)
results_cvd_3 <- extract_exponentiated_results(model_cvd_3)

# All-Cause Mortality Models
results_all_cause_1 <- extract_exponentiated_results(model_all_cause_1)
results_all_cause_2 <- extract_exponentiated_results(model_all_cause_2)
results_all_cause_3 <- extract_exponentiated_results(model_all_cause_3)

# Obesity Models
results_obesity_1 <- extract_exponentiated_results(model_obesity_1)
results_obesity_2 <- extract_exponentiated_results(model_obesity_2)

# Filter for the "agri" variable only
results_diabetes_1_agri <- results_diabetes_1[results_diabetes_1$Coefficient == "agri", ]
results_diabetes_2_agri <- results_diabetes_2[results_diabetes_2$Coefficient == "agri", ]
results_diabetes_3_agri <- results_diabetes_3[results_diabetes_3$Coefficient == "agri", ]

results_cvd_1_agri <- results_cvd_1[results_cvd_1$Coefficient == "agri", ]
results_cvd_2_agri <- results_cvd_2[results_cvd_2$Coefficient == "agri", ]
results_cvd_3_agri <- results_cvd_3[results_cvd_3$Coefficient == "agri", ]

results_all_cause_1_agri <- results_all_cause_1[results_all_cause_1$Coefficient == "agri", ]
results_all_cause_2_agri <- results_all_cause_2[results_all_cause_2$Coefficient == "agri", ]
results_all_cause_3_agri <- results_all_cause_3[results_all_cause_3$Coefficient == "agri", ]

results_obesity_1_agri <- results_obesity_1[results_obesity_1$Coefficient == "agri", ]
results_obesity_2_agri <- results_obesity_2[results_obesity_2$Coefficient == "agri", ]

# Combine the results for visualization
combined_results <- rbind(
  cbind(results_diabetes_1_agri, Outcome = "Diabetes - Model 1"),
  cbind(results_diabetes_2_agri, Outcome = "Diabetes - Model 2"),
  cbind(results_diabetes_3_agri, Outcome = "Diabetes - Model 3"),
  
  cbind(results_cvd_1_agri, Outcome = "CVD Mortality - Model 1"),
  cbind(results_cvd_2_agri, Outcome = "CVD Mortality - Model 2"),
  cbind(results_cvd_3_agri, Outcome = "CVD Mortality - Model 3"),
  
  cbind(results_all_cause_1_agri, Outcome = "All-Cause Mortality - Model 1"),
  cbind(results_all_cause_2_agri, Outcome = "All-Cause Mortality - Model 2"),
  cbind(results_all_cause_3_agri, Outcome = "All-Cause Mortality - Model 3"),
  
  cbind(results_obesity_1_agri, Outcome = "Obesity - Model 1"),
  cbind(results_obesity_2_agri, Outcome = "Obesity - Model 2")
)

print(combined_results)

write_csv(combined_results,"eco_results_higher.csv")

combined_results$Outcome <- fct_rev(combined_results$Outcome)

ggplot(combined_results, aes(x = Estimate, y = Outcome)) +
  geom_point(size = 3) +  # Increase point size
  geom_errorbarh(aes(xmin = CI_Lower, xmax = CI_Upper), height = 0.3, size = 1) +  # Increase error bar thickness
  geom_vline(xintercept = 1, linetype = "dashed", color = "black", size = 1) +  # Increase thickness of reference line
  theme_minimal() +
  labs(x = "RR per SD of % Agricultural Employment", y = NULL, title = "Agriculture (% Employment)") +
  theme(
    plot.title = element_text(hjust = 0.5, size = 14),
    axis.text.y = element_text(size = 12, face = "bold"),
    axis.text.x = element_text(size = 12)
  ) +
  scale_x_log10()  # Apply log scale to x-axis


####Interaction Analysis ####

##Full 200

data <- read.xlsx('global_set.xlsx')

data$iso3_code <- countrycode(data$country, origin = "country.name", destination = "iso3c")

data$obesity <- data$obesity_rate

# Convert obesity_rate and smoking from percentage to proportion 
data <- data %>%
  mutate(obesity_rate = obesity_rate / 100)

data <- data %>%
  mutate(male_smoke = male_smoke / 100)

data <- data %>%
  mutate(female_smoke = female_smoke / 100)


data <- data %>%
  mutate(cvd_deaths = cvd_deaths * (population / 100000),
         all_cause = all_cause * (population / 100000),
         t2d_prev = t2d_prev * (population / 100000))


data <- data %>%
  mutate(
    cvd_deaths = ceiling(cvd_deaths),  # Round up cvd_deaths
    all_cause = ceiling(all_cause),
    t2d_prev = ceiling(t2d_prev) # Round up all_cause
  )


data$agri <- scale(data$agri)


# Negative binomial regression for all-cause mortality with population offset
model_all_cause <- glm.nb(all_cause ~ agri + Educational_Attainment  + gdp + median_age + age_depend + male_smoke + female_smoke + pm_25 + 
                            offset(log(population)), data = data)


# Negative binomial regression for Type 2 diabetes prevalence with population offset
model_t2d_prev <- glm.nb(t2d_prev ~ agri + Educational_Attainment  + gdp + median_age + age_depend + male_smoke + female_smoke + pm_25 + tb_deaths + 
                           offset(log(population)), data = data)


##Subset 

data_sub <- read.xlsx('global_set.xlsx')
final_merged <- read.csv("pa_eco_new.csv")
names(final_merged)[1] <- "iso3_code"

##Drop Republic of Congo, missing travel and occupational PA
final_merged <- final_merged %>%
  filter(iso3_code!="COG")

data_sub$iso3_code <- countrycode(data_sub$country, origin = "country.name", destination = "iso3c")

data_sub <- data_sub %>%
  filter(iso3_code %in% final_merged$iso3_code)

data_sub$obesity <- data_sub$obesity_rate

# Convert obesity_rate and smoking from percentage to proportion 
data_sub <- data_sub %>%
  mutate(obesity_rate = obesity_rate / 100)

data_sub <- data_sub %>%
  mutate(male_smoke = male_smoke / 100)

data_sub <- data_sub %>%
  mutate(female_smoke = female_smoke / 100)


data_sub <- data_sub %>%
  mutate(cvd_deaths = cvd_deaths * (population / 100000),
         all_cause = all_cause * (population / 100000),
         t2d_prev = t2d_prev * (population / 100000))


data_sub <- data_sub %>%
  mutate(
    cvd_deaths = ceiling(cvd_deaths),  # Round up cvd_deaths
    all_cause = ceiling(all_cause),
    t2d_prev = ceiling(t2d_prev) # Round up all_cause
  )


data_sub$agri <- scale(data_sub$agri)

model_diabetes_2 <- glm.nb(
  t2d_prev ~ agri + Educational_Attainment + gdp + median_age + age_depend + 
    male_smoke + female_smoke + pm_25 + tb_deaths + offset(log(population)), 
  data = data_sub
)


model_all_cause_2 <- glm.nb(
  all_cause ~ agri + Educational_Attainment + gdp + median_age + age_depend + 
    male_smoke + female_smoke + pm_25 + offset(log(population)), 
  data = data_sub
)




# Assuming 'data' and 'data_sub' are your data frames
data$stage <- 1  # Stage 1 for full dataset
data_sub$stage <- 2  # Stage 2 for subset

# Bind the datasets (assuming all variable names match)
combined_data <- rbind(data, data_sub)

# Convert stage to factor for interaction
combined_data$stage <- as.factor(combined_data$stage)


# All-Cause Mortality

# Model 1: Only agri as predictor
model1_all_cause <- glm.nb(
  all_cause ~ agri*stage + offset(log(population)), 
  data = combined_data
)
summary(model1_all_cause)

# Model 2: Current model with interaction
model2_all_cause <- glm.nb(
  all_cause ~ agri * stage + Educational_Attainment + gdp + median_age + age_depend + 
    male_smoke + female_smoke + pm_25 + offset(log(population)), 
  data = combined_data
)
summary(model2_all_cause)

# Model 3: Model 2 with additional adjustment for obesity_rate
model3_all_cause <- glm.nb(
  all_cause ~ agri * stage + Educational_Attainment + gdp + median_age + age_depend + 
    male_smoke + female_smoke + pm_25 + obesity_rate + offset(log(population)), 
  data = combined_data
)
summary(model3_all_cause)

# Diabetes Prevalence

# Model 1: Only agri as predictor
model1_t2d_prev <- glm.nb(
  t2d_prev ~ agri*stage + offset(log(population)), 
  data = combined_data
)
summary(model1_t2d_prev)

# Model 2: Current model with interaction
model2_t2d_prev <- glm.nb(
  t2d_prev ~ agri * stage + Educational_Attainment + gdp + median_age + age_depend + 
    male_smoke + female_smoke + pm_25 + tb_deaths + offset(log(population)), 
  data = combined_data
)
summary(model2_t2d_prev)

# Model 3: Model 2 with additional adjustment for obesity_rate
model3_t2d_prev <- glm.nb(
  t2d_prev ~ agri * stage + Educational_Attainment + gdp + median_age + age_depend + 
    male_smoke + female_smoke + pm_25 + tb_deaths + obesity_rate + offset(log(population)), 
  data = combined_data
)
summary(model3_t2d_prev)


#### All Models Higher Level (100) ####

data <- read.xlsx('global_set.xlsx')
final_merged <- read.csv("pa_eco_new.csv")

##Drop Republic of Congo, missing travel and occupational PA
final_merged <- final_merged %>%
  filter(country!="COG")

data$iso3_code <- countrycode(data$country, origin = "country.name", destination = "iso3c")

data <- data %>%
  filter(iso3_code %in% final_merged$country)

data$obesity <- data$obesity_rate

# Convert obesity_rate and smoking from percentage to proportion 
data <- data %>%
  mutate(obesity_rate = obesity_rate / 100)

data <- data %>%
  mutate(male_smoke = male_smoke / 100)

data <- data %>%
  mutate(female_smoke = female_smoke / 100)


data <- data %>%
  mutate(cvd_deaths = cvd_deaths * (population / 100000),
         all_cause = all_cause * (population / 100000),
         t2d_prev = t2d_prev * (population / 100000))


data <- data %>%
  mutate(
    cvd_deaths = ceiling(cvd_deaths),  # Round up cvd_deaths
    all_cause = ceiling(all_cause),
    t2d_prev = ceiling(t2d_prev) # Round up all_cause
  )


data$agri <- scale(data$agri)

# Diabetes Models
model_diabetes_1 <- glm.nb(
  t2d_prev ~ agri + offset(log(population)), 
  data = data
)

model_diabetes_2 <- glm.nb(
  t2d_prev ~ agri + Educational_Attainment + gdp + median_age + age_depend + 
    male_smoke + female_smoke + pm_25 + tb_deaths + offset(log(population)), 
  data = data
)

model_diabetes_3 <- glm.nb(
  t2d_prev ~ agri + Educational_Attainment + obesity_rate + gdp + 
    median_age + age_depend + male_smoke + female_smoke + pm_25 + 
    tb_deaths + offset(log(population)), 
  data = data
)

# CVD Models
model_cvd_1 <- glm.nb(
  cvd_deaths ~ agri + offset(log(population)), 
  data = data
)

model_cvd_2 <- glm.nb(
  cvd_deaths ~ agri + Educational_Attainment + gdp + median_age + age_depend + 
    male_smoke + female_smoke + pm_25 + offset(log(population)), 
  data = data
)

model_cvd_3 <- glm.nb(
  cvd_deaths ~ agri + Educational_Attainment + obesity_rate + gdp + 
    median_age + age_depend + male_smoke + female_smoke + pm_25 + 
    offset(log(population)), 
  data = data
)

# All-Cause Mortality Models
model_all_cause_1 <- glm.nb(
  all_cause ~ agri + offset(log(population)), 
  data = data
)

model_all_cause_2 <- glm.nb(
  all_cause ~ agri + Educational_Attainment + gdp + median_age + age_depend + 
    male_smoke + female_smoke + pm_25 + offset(log(population)), 
  data = data
)

model_all_cause_3 <- glm.nb(
  all_cause ~ agri + Educational_Attainment + obesity_rate + gdp + 
    median_age + age_depend + male_smoke + female_smoke + pm_25 + 
    offset(log(population)), 
  data = data
)

# Obesity Models
model_obesity_1 <- glm(
  obesity_rate ~ agri, 
  family = quasibinomial(link = "logit"), 
  data = data
)

model_obesity_2 <- glm(
  obesity_rate ~ agri + Educational_Attainment + median_age + age_depend + 
    gdp + male_smoke + female_smoke + pm_25 + tb_deaths, 
  family = quasibinomial(link = "logit"), 
  data = data
)


# Function to extract exponentiated coefficients and CIs
extract_exponentiated_results <- function(model) {
  coefs <- exp(coef(model))  # Exponentiate coefficients
  conf_int <- exp(confint(model))  # Exponentiate confidence intervals
  results <- data.frame(
    Coefficient = names(coefs),
    Estimate = coefs,
    CI_Lower = conf_int[, 1],
    CI_Upper = conf_int[, 2]
  )
  return(results)
}

# Extract results for each model
# Diabetes Models
results_diabetes_1 <- extract_exponentiated_results(model_diabetes_1)
results_diabetes_2 <- extract_exponentiated_results(model_diabetes_2)
results_diabetes_3 <- extract_exponentiated_results(model_diabetes_3)

# CVD Models
results_cvd_1 <- extract_exponentiated_results(model_cvd_1)
results_cvd_2 <- extract_exponentiated_results(model_cvd_2)
results_cvd_3 <- extract_exponentiated_results(model_cvd_3)

# All-Cause Mortality Models
results_all_cause_1 <- extract_exponentiated_results(model_all_cause_1)
results_all_cause_2 <- extract_exponentiated_results(model_all_cause_2)
results_all_cause_3 <- extract_exponentiated_results(model_all_cause_3)

# Obesity Models
results_obesity_1 <- extract_exponentiated_results(model_obesity_1)
results_obesity_2 <- extract_exponentiated_results(model_obesity_2)

# Filter for the "agri" variable only
results_diabetes_1_agri <- results_diabetes_1[results_diabetes_1$Coefficient == "agri", ]
results_diabetes_2_agri <- results_diabetes_2[results_diabetes_2$Coefficient == "agri", ]
results_diabetes_3_agri <- results_diabetes_3[results_diabetes_3$Coefficient == "agri", ]

results_cvd_1_agri <- results_cvd_1[results_cvd_1$Coefficient == "agri", ]
results_cvd_2_agri <- results_cvd_2[results_cvd_2$Coefficient == "agri", ]
results_cvd_3_agri <- results_cvd_3[results_cvd_3$Coefficient == "agri", ]

results_all_cause_1_agri <- results_all_cause_1[results_all_cause_1$Coefficient == "agri", ]
results_all_cause_2_agri <- results_all_cause_2[results_all_cause_2$Coefficient == "agri", ]
results_all_cause_3_agri <- results_all_cause_3[results_all_cause_3$Coefficient == "agri", ]

results_obesity_1_agri <- results_obesity_1[results_obesity_1$Coefficient == "agri", ]
results_obesity_2_agri <- results_obesity_2[results_obesity_2$Coefficient == "agri", ]

# Combine the results for visualization
combined_results <- rbind(
  cbind(results_diabetes_1_agri, Outcome = "Diabetes - Model 1"),
  cbind(results_diabetes_2_agri, Outcome = "Diabetes - Model 2"),
  cbind(results_diabetes_3_agri, Outcome = "Diabetes - Model 3"),
  
  cbind(results_cvd_1_agri, Outcome = "CVD Mortality - Model 1"),
  cbind(results_cvd_2_agri, Outcome = "CVD Mortality - Model 2"),
  cbind(results_cvd_3_agri, Outcome = "CVD Mortality - Model 3"),
  
  cbind(results_all_cause_1_agri, Outcome = "All-Cause Mortality - Model 1"),
  cbind(results_all_cause_2_agri, Outcome = "All-Cause Mortality - Model 2"),
  cbind(results_all_cause_3_agri, Outcome = "All-Cause Mortality - Model 3"),
  
  cbind(results_obesity_1_agri, Outcome = "Obesity - Model 1"),
  cbind(results_obesity_2_agri, Outcome = "Obesity - Model 2")
)

print(combined_results)

write_csv(combined_results,"eco_results_higher_subset.csv")

combined_results$Outcome <- fct_rev(combined_results$Outcome)

combined_results$Outcome <- fct_rev(combined_results$Outcome)

ggplot(combined_results, aes(x = Estimate, y = Outcome)) +
  geom_point(size = 3) +  # Increase point size
  geom_errorbarh(aes(xmin = CI_Lower, xmax = CI_Upper), height = 0.3, size = 1) +  # Increase error bar thickness
  geom_vline(xintercept = 1, linetype = "dashed", color = "black", size = 1) +  # Increase thickness of reference line
  theme_minimal() +
  labs(x = "RR per SD of % Agricultural Employment", y = NULL, title = "Agriculture (% Employment)") +
  theme(
    plot.title = element_text(hjust = 0.5, size = 14),
    axis.text.y = element_text(size = 12, face = "bold"),
    axis.text.x = element_text(size = 12)
  ) +
  scale_x_log10()  # Apply log scale to x-axis


#### More Visualizations ####

final_merged <- read.csv("pa_eco_new.csv")

##Drop Republic of Congo, missing travel and occupational PA
final_merged <- final_merged %>%
  filter(country!="COG")

data <- read.xlsx('global_set.xlsx')

##Scatterplots

# Define a manual winsorizing function 
winsorize <- function(x, lower_quantile = 0.05, upper_quantile = 0.95) {
  lower_bound <- quantile(x, probs = lower_quantile, na.rm = TRUE)
  upper_bound <- quantile(x, probs = upper_quantile, na.rm = TRUE)
  x[x < lower_bound] <- lower_bound
  x[x > upper_bound] <- upper_bound
  return(x)
}

# Define the outcomes, predictors, and corresponding axis labels
outcomes <- c("cvd_deaths", "all_cause", "t2d_prev", "obesity_rate")
outcome_labels <- c("CVD Mortality", 
                    "All-Cause Mortality", 
                    "Diabetes Prevalence", 
                    "Obesity Prevalence")

# Axis labels with units for the y-axis
outcome_axis_labels <- c("CVD Mortality (per 100,000)", 
                         "All-Cause Mortality (per 100,000)", 
                         "Type 2 Diabetes Prevalence (per 100,000)", 
                         "Obesity Prevalence (%)")

# For final_merged, z_omvpa will be the predictor
# For data, agri will be the predictor
predictors <- c("z_omvpa", "agri")
predictor_labels <- c("Z-Score Occupational Physical Activity", 
                      "% Employment in Agriculture")


# Step 1: Average the 'omvpa_mean' for both sexes and calculate a new z-score

# Average the 'omvpa_mean' for both sexes and calculate a new z-score
final_merged_avg <- final_merged %>%
  group_by(country) %>%
  summarise(across(c("omvpa_mean", all_of(outcomes), "population_2022"), mean, na.rm = TRUE), 
            region = first(region)) %>%
  ungroup()

# Calculate the z-score for the averaged 'omvpa_mean'
final_merged_avg <- final_merged_avg %>%
  mutate(z_omvpa_avg = scale(omvpa_mean))

# Step 2: Create a unified color palette for regions in both dataframes

# Combine the unique regions from both dataframes
unique_regions <- unique(c(final_merged_avg$region, data$region))

# Create a color palette for all unique regions
region_colors <- scales::hue_pal()(length(unique_regions))

# Assign colors to regions
region_color_mapping <- setNames(region_colors, unique_regions)

names(data)[6] <- "population_2022"


#Create scatterplots



# Apply Winsorization to outcome variables in both dataframes
final_merged_avg <- final_merged_avg %>%
  mutate(across(all_of(outcomes), ~ winsorize(., lower_quantile = 0.05, upper_quantile = 0.95)))

data <- data %>%
  mutate(across(all_of(outcomes), ~ winsorize(., lower_quantile = 0.05, upper_quantile = 0.95)))

# Apply Winsorization to predictors
final_merged_avg <- final_merged_avg %>%
  mutate(z_omvpa_avg = winsorize(z_omvpa_avg, lower_quantile = 0.05, upper_quantile = 0.95))

data <- data %>%
  mutate(agri = winsorize(agri, lower_quantile = 0.05, upper_quantile = 0.95))



##For subset of agriculture with WHO data

data$iso3_code <- countrycode(data$country, origin = "country.name", destination = "iso3c")

data <- data %>%
  filter(iso3_code %in% final_merged$country)

# For data, use 'agri' as x-axis
for (i in seq_along(outcomes)) {
  create_plot(data, "agri", outcomes[i], predictor_labels[2], outcome_labels[i], paste0("data_", outcomes[i], "_agri"))
}


###Country names


final_merged_avg$iso3_code <- countrycode(final_merged_avg$country, origin = "country.name", destination = "iso3c")

final_merged_avg$iso3_code[final_merged_avg$country == "Lebanese Republic"] <- "LBN"
final_merged_avg$iso3_code[final_merged_avg$country == "Republic of Guyana"] <- "GUY"


create_plot_with_labels <- function(df, x_var, outcome_var, x_label, outcome_label, plot_name, save_plot = TRUE) {
  p <- ggplot(df, aes_string(x = x_var, y = outcome_var, color = "region", label = "iso3_code")) +
    geom_text_repel(max.overlaps = 10, size = 3.5, show.legend = FALSE) +  # Display only iso3_code labels
    geom_point(aes(color = region), alpha = 1, size = 3) +  # Add visible points for legend and color reference
    scale_color_manual(values = region_color_mapping) +  # Apply the color mapping
    labs(title = paste(outcome_label, "vs", x_label),
         x = x_label,  
         y = outcome_label,
         color = "Region") + 
    theme_minimal() +
    theme(plot.title = element_text(hjust = 0.5))
  
  if (save_plot) {
    ggsave(filename = paste0(plot_name, ".jpeg"), plot = p, width = 8, height = 6, dpi = 300)
  }
  
  print(p)
}

names(final_merged_avg)[1] <- "iso3_code"

# Generate labeled scatterplots for final_merged_avg
for (i in seq_along(outcomes)) {
  create_plot_with_labels(
    final_merged_avg, 
    "z_omvpa_avg", 
    outcomes[i], 
    predictor_labels[1], 
    outcome_labels[i], 
    paste0("final_merged_", outcomes[i], "_z_omvpa_avg_labels")
  )
}

# Generate labeled scatterplots for data
for (i in seq_along(outcomes)) {
  create_plot_with_labels(
    data, 
    "agri", 
    outcomes[i], 
    predictor_labels[2], 
    outcome_labels[i], 
    paste0("data_", outcomes[i], "_agri_labels")
  )
}


##Versions with ellipses per region

create_plot_with_labels <- function(df, x_var, outcome_var, x_label, outcome_label, plot_name, save_plot = TRUE) {
  p <- ggplot(df, aes_string(x = x_var, y = outcome_var, color = "region", label = "iso3_code")) +
    geom_text_repel(max.overlaps = 10, size = 3.5, show.legend = FALSE) +  # Display only iso3_code labels
    geom_point(aes(color = region), alpha = 1, size = 3) +  # Add visible points for legend and color reference
    stat_ellipse(aes(group = region), level = 0.95, alpha = 0.2, linetype = "solid", size = 1) +  # Add 95% confidence ellipses
    scale_color_manual(values = region_color_mapping) +  # Apply the color mapping
    labs(title = paste(outcome_label, "vs", x_label),
         x = x_label,  
         y = outcome_label,
         color = "Region") + 
    theme_minimal() +
    theme(plot.title = element_text(hjust = 0.5))
  
  if (save_plot) {
    ggsave(filename = paste0(plot_name, ".jpeg"), plot = p, width = 8, height = 6, dpi = 300)
  }
  
  print(p)
}



# Generate labeled scatterplots for final_merged_avg
for (i in seq_along(outcomes)) {
  create_plot_with_labels(
    final_merged_avg, 
    "z_omvpa_avg", 
    outcomes[i], 
    predictor_labels[1], 
    outcome_labels[i], 
    paste0("final_merged_", outcomes[i], "_z_omvpa_avg_labels")
  )
}

# Generate labeled scatterplots for data
for (i in seq_along(outcomes)) {
  create_plot_with_labels(
    data, 
    "agri", 
    outcomes[i], 
    predictor_labels[2], 
    outcome_labels[i], 
    paste0("data_", outcomes[i], "_agri_labels")
  )
}

####Forest plots

all_results <- read.csv("eco_results_higher_subset.csv")

all_results$Outcome <- fct_rev(all_results$Outcome)

all_results$Outcome <- fct_rev(all_results$Outcome)

# Assuming 'all_results' is your data frame
all_results <- all_results[!all_results$Outcome %in% c(
  "Diabetes - Model 1",
  "CVD Mortality - Model 1",
  "Obesity - Model 1",
  "All-Cause Mortality - Model 1"
), ]


all_results$Outcome <- gsub("Diabetes", "Diabetes Prevalence", all_results$Outcome)

all_results$Outcome <- gsub("Obesity", "Obesity Prevalence", all_results$Outcome)

# Replace 'Model 2' and 'Model 3' in the 'Outcome' column
all_results$Outcome <- gsub("Model 2", "Adjusted for Standard Covariates", all_results$Outcome)
all_results$Outcome <- gsub("Model 3", "Adjusted for Standard Covariates + Obesity", all_results$Outcome)

# Set the levels of Outcome for the desired order
all_results$Outcome <- factor(all_results$Outcome, levels = c(
  "All-Cause Mortality - Adjusted for Standard Covariates + Obesity",
  "All-Cause Mortality - Adjusted for Standard Covariates",
  "CVD Mortality - Adjusted for Standard Covariates + Obesity",
  "CVD Mortality - Adjusted for Standard Covariates",
  "Diabetes Prevalence - Adjusted for Standard Covariates + Obesity",
  "Diabetes Prevalence - Adjusted for Standard Covariates",
  "Obesity Prevalence - Adjusted for Standard Covariates"
))



# Now plot with reversed order of the levels
ggplot(all_results, aes(x = Estimate, y = Outcome)) +
  geom_point(size = 3) +  # Increase point size
  geom_errorbarh(aes(xmin = CI_Lower, xmax = CI_Upper), height = 0.3, size = 1) +  # Increase error bar thickness
  geom_vline(xintercept = 1, linetype = "dashed", color = "black", size = 1) +  # Increase thickness of reference line
  theme_minimal() +
  labs(x = NULL, y = NULL, title = NULL) +
  theme(
    plot.title = element_text(hjust = 0.5, size = 14),
    axis.text.y = element_text(size = 12, face = "bold"),
    axis.text.x = element_text(size = 12)
  ) +
  scale_x_log10()  # Apply log scale to x-axis



all_results <- read.csv("eco_results_lower.csv")

all_results <- all_results[all_results$Model_Type %in% c("OMVPA Model2", "OMVPA Model3"), ]

all_results <- all_results %>%
  mutate(Model_Type = ifelse(Model_Type == "OMVPA Model2", 
                             "Adjusted for Standard Covariates", 
                             ifelse(Model_Type == "OMVPA Model3", 
                                    "Additionally Adjusted for Obesity", 
                                    Model_Type)))

# Define custom ordering logic
custom_order <- all_results %>%
  arrange(
    factor(Outcome, levels = c(
      "Obesity Prevalence",
      "Type 2 Diabetes Prevalence",
      "CVD Mortality",
      "All-Cause Mortality"    # Reversed order as requested
    )),
    factor(
      Model_Type,
      levels = c(
        "Adjusted for Standard Covariates", 
        "Additionally Adjusted for Obesity"
      )
    )
  )

# Create a new column for the custom label with the reversed order
custom_order <- custom_order %>%
  mutate(Outcome_Model = paste(Outcome, Model_Type, sep = " - ")) %>%
  mutate(Outcome_Model = factor(Outcome_Model, levels = rev(c(
    "Obesity Prevalence - Adjusted for Standard Covariates",
    "Obesity Prevalence - Additionally Adjusted for Obesity",
    "Type 2 Diabetes Prevalence - Adjusted for Standard Covariates",
    "Type 2 Diabetes Prevalence - Additionally Adjusted for Obesity",
    "CVD Mortality - Adjusted for Standard Covariates",
    "CVD Mortality - Additionally Adjusted for Obesity",
    "All-Cause Mortality - Adjusted for Standard Covariates",
    "All-Cause Mortality - Additionally Adjusted for Obesity"
  ))))  # Using rev() to reverse the order for plotting



# Plot with the reordered labels
ggplot(custom_order, aes(x = Both_Estimate, y = Outcome_Model)) +
  geom_point(size = 3) +  # Increase point size
  geom_errorbarh(aes(xmin = Both_CI_Lower, xmax = Both_CI_Upper), height = 0.3, size = 1) +  # Error bars
  geom_vline(xintercept = 1, linetype = "dashed", color = "black", size = 1) +  # Null effect line
  theme_minimal() +
  labs(
    x = NULL,
    y = NULL,
    title = NULL
  ) +
  theme(
    plot.title = element_text(hjust = 0.5, size = 14),
    axis.text.y = element_text(size = 10, face = "bold"),
    axis.text.x = element_text(size = 12)
  ) +
  scale_x_log10()  # Log scale for x-axis





#### Poisson v Negative Binomial LR Tests ####

### PA Ecological 


# Model 2 for All-Cause Mortality (using final_merged and population_2022 for the offset)
model_all_cause_poisson <- glm(all_cause ~ z_omvpa_mean + Educational_Attainment + obesity_rate + gdp_per_capita + median_age + age_depend + male_smoke + female_smoke + pm_25,
                               family = poisson(link = "log"), data = final_merged, offset = log(population_2022))

model_all_cause_nb <- glm.nb(all_cause ~ z_omvpa_mean + Educational_Attainment + obesity_rate + gdp_per_capita + median_age + age_depend + male_smoke + female_smoke + pm_25 + 
                               offset(log(population_2022)), data = final_merged)

# Model 2 for CVD Mortality
model_cvd_deaths_poisson <- glm(cvd_deaths ~ z_omvpa_mean + Educational_Attainment + obesity_rate + gdp_per_capita + median_age + age_depend + male_smoke + female_smoke + pm_25,
                                family = poisson(link = "log"), data = final_merged, offset = log(population_2022))

model_cvd_deaths_nb <- glm.nb(cvd_deaths ~ z_omvpa_mean + Educational_Attainment + obesity_rate + gdp_per_capita + median_age + age_depend + male_smoke + female_smoke + pm_25 + 
                                offset(log(population_2022)), data = final_merged)

# Model 2 for Type 2 Diabetes Prevalence
model_t2d_prev_poisson <- glm(t2d_prev ~ z_omvpa_mean + Educational_Attainment + obesity_rate + gdp_per_capita + median_age + age_depend + male_smoke + female_smoke + pm_25 + tb_deaths,
                              family = poisson(link = "log"), data = final_merged, offset = log(population_2022))

model_t2d_prev_nb <- glm.nb(t2d_prev ~ z_omvpa_mean + Educational_Attainment + obesity_rate + gdp_per_capita + median_age + age_depend + male_smoke + female_smoke + pm_25 + tb_deaths + 
                              offset(log(population_2022)), data = final_merged)

# Likelihood ratio tests for each model comparison
lrt_all_cause <- lrtest(model_all_cause_poisson, model_all_cause_nb)
lrt_cvd_deaths <- lrtest(model_cvd_deaths_poisson, model_cvd_deaths_nb)
lrt_t2d_prev <- lrtest(model_t2d_prev_poisson, model_t2d_prev_nb)

# Display results
print(lrt_all_cause)
print(lrt_cvd_deaths)
print(lrt_t2d_prev)


##Higher Level Ecological

# Define Poisson models
model_all_cause_poisson <- glm(all_cause ~ agri + Educational_Attainment + obesity_rate + gdp + median_age + age_depend + male_smoke + female_smoke + pm_25, 
                               family = poisson(link = "log"), data = data, offset = log(population))

model_cvd_deaths_poisson <- glm(cvd_deaths ~ agri + Educational_Attainment + obesity_rate + gdp + median_age + age_depend + male_smoke + female_smoke + pm_25, 
                                family = poisson(link = "log"), data = data, offset = log(population))

model_t2d_prev_poisson <- glm(t2d_prev ~ agri + Educational_Attainment + obesity_rate + gdp + median_age + age_depend + male_smoke + female_smoke + pm_25 + tb_deaths, 
                              family = poisson(link = "log"), data = data, offset = log(population))

# Define Negative Binomial models
model_all_cause_nb <- glm.nb(all_cause ~ agri + Educational_Attainment + obesity_rate + gdp + median_age + age_depend + male_smoke + female_smoke + pm_25 + 
                               offset(log(population)), data = data)

model_cvd_deaths_nb <- glm.nb(cvd_deaths ~ agri + Educational_Attainment + obesity_rate + gdp + median_age + age_depend + male_smoke + female_smoke + pm_25 + 
                                offset(log(population)), data = data)

model_t2d_prev_nb <- glm.nb(t2d_prev ~ agri + Educational_Attainment + obesity_rate + gdp + median_age + age_depend + male_smoke + female_smoke + pm_25 + tb_deaths + 
                              offset(log(population)), data = data)



# Likelihood ratio tests for each model comparison
lrt_all_cause <- lrtest(model_all_cause_poisson, model_all_cause_nb)
lrt_cvd_deaths <- lrtest(model_cvd_deaths_poisson, model_cvd_deaths_nb)
lrt_t2d_prev <- lrtest(model_t2d_prev_poisson, model_t2d_prev_nb)


print(lrt_all_cause)
print(lrt_cvd_deaths)
print(lrt_t2d_prev)




