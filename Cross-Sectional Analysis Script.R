# Load required libraries
install.packages("readr")
install.packages("dplyr")
install.packages("tidyverse")
install.packages("MASS")
install.packages("lme4")
install.packages("lmtest")
install.packages("marginaleffects")
install.packages("optimx")
install.packages("countrycode")
install.packages("car")
install.packages("ggplot2")
install.packages("cowplot")
install.packages("gridExtra")
install.packages("ggplotify")
install.packages("readxl")

library(readr)
library(dplyr)
library(tidyverse)
library(MASS)
library(lme4)
library(lmtest)
library(marginaleffects)
library(optimx)
library(countrycode)
library(car)
library(ggplot2)
library(cowplot)
library(gridExtra)
library(ggplotify)
library(readxl)
library(stringr)

#### Non-Compositional ####
#### FG, Total Chol, SBP, DBP #####

# Load data
all_df <- read.csv("combined_df_cleaned_mod_new.csv")

#Exclude Congo (incomplete PA data), and observations missing age, sex, height, and weight
all_df <- all_df %>% 
  filter(iso3_code != "COG") %>%
  filter(!is.na(total_mvpa) & !is.na(omvpa) & !is.na(nonocpa) & !is.na(sex) & !is.na(age) & !is.na(BMI))

#Exclude unfasted
all_df$fasting_glucose[all_df$b1 == 1] <- NA

#Exclude Liberia from total cholesterol analysis only 16 observations)
all_df$total_chol[all_df$iso3_code == "LBR"] <- NA

#Center the year variable on 2011, midpoint of data collection period
all_df$year_centered <- all_df$year - 2011

all_df$occupation <- as.factor(all_df$occupation)

# Scale PA per 150 min/week
all_df <- all_df %>%
  mutate(
    total_mvpa_150 = total_mvpa / 150,
    omvpa_150 = omvpa / 150,
    nonocpa_150 = nonocpa / 150
  )

# Create binary activity indicators
all_df$active_total_mvpa <- ifelse(all_df$total_mvpa > 0, 1, 0)
all_df$active_omvpa <- ifelse(all_df$omvpa > 0, 1, 0)
all_df$active_nonocpa <- ifelse(all_df$nonocpa > 0, 1, 0)

# Relevel categorical covariates
all_df$BMI_group <- factor(all_df$BMI_group, levels = c("Normal", "Underweight", "Overweight", "Obese", "missing_unknown"))
all_df$marital_status <- factor(all_df$marital_status, levels = c("2", "1", "3", "4", "5", "6", "missing_unknown"))
all_df$smoking_status <- factor(all_df$smoking_status, levels = c("2", "1", "missing_unknown"))
all_df$alcohol_status <- factor(all_df$alcohol_status, levels = c("2", "1", "missing_unknown"))
all_df$meet_eat <- factor(all_df$meet_eat, levels = c("2", "1", "missing_unknown"))

# Define variables for models
exposure_list <- c("total_mvpa_150", "omvpa_150", "nonocpa_150")
control_vars_2 <- c("sex", "marital_status", "alcohol_status", "smoking_status", "meet_eat", "country_class")
selection_vars <- c("occupation", "education", "country_class", "sex", "age")
control_vars_3 <- c("BMI_group", "hr")

# Define outcomes
outcomes <- c("fasting_glucose", "total_chol", "sbp", "dbp")
original_results <- data.frame()

# Select relevant variables

relevant_vars <- unique(c(
  "total_mvpa", "omvpa", "nonocpa", "log_ratio_omvpa",
  "total_mvpa_150", "omvpa_150", "nonocpa_150",
  "active_total_mvpa", "active_omvpa", "active_nonocpa",
  "sex", "marital_status", "alcohol_status", "smoking_status", "meet_eat", "country_class",
  "BMI_group", "hr", "year_centered",
  "occupation", "education", "country_class", "sex", "age",
  "fasting_glucose", "total_chol", "sbp", "dbp",
  "iso3_code", "country"
))
all_df <- all_df %>% dplyr::select(all_of(relevant_vars))



# Loop through outcomes and exposures
for (outcome in outcomes) {
  cat("Processing outcome:", outcome, "\n")
  
  # Selection equation
  all_df[[paste0(outcome, "_select")]] <- ifelse(!is.na(all_df[[outcome]]), 1, 0)
  selection_formula <- as.formula(paste0(outcome, "_select ~ ", paste(selection_vars, collapse = " + ")))
  probit_model <- tryCatch({
    glm(selection_formula, family = binomial(link = "probit"), data = all_df, control = glm.control(maxit = 100))
  }, error = function(e) {
    cat("Error in probit for", outcome, ":", e$message, "\n")
    return(NULL)
  })
  if (is.null(probit_model)) next
  all_df$z <- predict(probit_model, type = "link")
  all_df$Lambda <- dnorm(all_df$z) / pnorm(all_df$z)
  
  # Subset data for outcome model
  model_data <- all_df[!is.na(all_df[[outcome]]) & !is.na(all_df$iso3_code), ]
  cat("Number of observations for", outcome, ":", nrow(model_data), "\n")
  
  if (outcome == "fasting_glucose") {
    sapply(control_vars_2, function(x) cat("Missing", x, ":", sum(is.na(model_data[[x]])), "\n"))
  }
  
  for (exposure in exposure_list) {
    active_var <- switch(exposure,
                         "total_mvpa_150" = "active_total_mvpa",
                         "omvpa_150" = "active_omvpa",
                         "nonocpa_150" = "active_nonocpa")
    control_vars_1 <- c(active_var)
    
    for (i in 1:3) {
      if (i == 1) controls <- control_vars_1
      if (i == 2) controls <- c(control_vars_1, control_vars_2)
      if (i == 3) controls <- c(control_vars_1, control_vars_2, control_vars_3)
      
      # Outcome model with random slope for year
      formula <- as.formula(paste(outcome, "~", exposure, "+", paste(controls, collapse = " + "), 
                                  "+ year_centered + Lambda + (1 + year_centered | country)"))
      model_data_subset <- model_data[complete.cases(model_data[, c(outcome, exposure, controls, 
                                                                    "year_centered", "Lambda", "country")]), ]
      cat("Observations after NA removal for", outcome, exposure, "stage", i, ":", nrow(model_data_subset), "\n")
      
      # Check observations per country
      country_counts <- table(model_data_subset$country)
      if (any(country_counts < 10)) {
        cat("Warning: Some countries have fewer than 10 observations for", outcome, exposure, "stage", i, "\n")
        valid_countries <- names(country_counts[country_counts >= 10])
        model_data_subset <- model_data_subset %>% filter(country %in% valid_countries)
        cat("Observations after filtering low-count countries:", nrow(model_data_subset), "\n")
      }
      
      # Fit mixed-effects model
      model <- tryCatch({
        lmer(formula, data = model_data_subset, 
             control = lmerControl(optimizer = "bobyqa", optCtrl = list(maxfun = 2e5)))
      }, error = function(e) {
        cat("Error in lmer:", e$message, "\n")
        return(NULL)
      })
      if (is.null(model)) next
      
      # Extract coefficients and standard errors
      model_summary <- summary(model)$coefficients
      coefs <- model_summary[, "Estimate"]
      std_errors <- model_summary[, "Std. Error"]
      ci_lower <- coefs - 1.96 * std_errors
      ci_upper <- coefs + 1.96 * std_errors
      
      # Calculate semi-elasticities
      me <- tryCatch({
        slopes(model, variables = c(exposure, active_var), type = "response")
      }, error = function(e) {
        cat("Error in slopes for", outcome, exposure, "stage", i, ":", e$message, "\n")
        return(NULL)
      })
      if (is.null(me)) next
      
      mean_outcome <- mean(model_data_subset[[outcome]], na.rm = TRUE)
      
      me_exp <- me[me$term == exposure, ]
      slope_exp <- mean(me_exp$estimate)
      semi_elasticity_exp <- (slope_exp / mean_outcome) * 100
      se_semi_elasticity_exp <- (mean(me_exp$std.error) / mean_outcome) * 100
      ci_lower_se_exp <- semi_elasticity_exp - 1.96 * se_semi_elasticity_exp
      ci_upper_se_exp <- semi_elasticity_exp + 1.96 * se_semi_elasticity_exp
      
      me_active <- me[me$term == active_var, ]
      slope_active <- mean(me_active$estimate)
      semi_elasticity_active <- (slope_active / mean_outcome) * 100
      se_semi_elasticity_active <- (mean(me_active$std.error) / mean_outcome) * 100
      ci_lower_se_active <- semi_elasticity_active - 1.96 * se_semi_elasticity_active
      ci_upper_se_active <- semi_elasticity_active + 1.96 * se_semi_elasticity_active
      
      # Store results
      temp_coef_df <- data.frame(
        Outcome = outcome,
        Exposure = exposure,
        Model_Stage = i,
        Variable = rownames(model_summary),
        Estimate = coefs,
        Std_Error = std_errors,
        CI_Lower = ci_lower,
        CI_Upper = ci_upper,
        Semi_Elasticity = ifelse(rownames(model_summary) == exposure, semi_elasticity_exp, 
                                 ifelse(rownames(model_summary) == active_var, semi_elasticity_active, NA)),
        SE_Semi_Elasticity = ifelse(rownames(model_summary) == exposure, se_semi_elasticity_exp, 
                                    ifelse(rownames(model_summary) == active_var, se_semi_elasticity_active, NA)),
        CI_Lower_SE = ifelse(rownames(model_summary) == exposure, ci_lower_se_exp, 
                             ifelse(rownames(model_summary) == active_var, ci_lower_se_active, NA)),
        CI_Upper_SE = ifelse(rownames(model_summary) == exposure, ci_upper_se_exp, 
                             ifelse(rownames(model_summary) == active_var, ci_upper_se_active, NA))
      )
      
      temp_fit_df <- data.frame(
        Outcome = outcome,
        Exposure = exposure,
        Model_Stage = i,
        Variable = c("AIC", "BIC"),
        Estimate = c(AIC(model), BIC(model)),
        Std_Error = NA,
        CI_Lower = NA,
        CI_Upper = NA,
        Semi_Elasticity = NA,
        SE_Semi_Elasticity = NA,
        CI_Lower_SE = NA,
        CI_Upper_SE = NA
      )
      
      temp_coef_df <- rbind(temp_coef_df, temp_fit_df)
      original_results <- rbind(original_results, temp_coef_df)
    }
  }
  
  # Clean up temporary variables
  all_df[[paste0(outcome, "_select")]] <- NULL
  all_df$z <- NULL
  all_df$Lambda <- NULL
}

# Save results
write.csv(original_results, "original_noncomp_results.csv", row.names = FALSE)

original_noncomp_results <- original_results

#### HR ####

# Load data
all_df <- read.csv("combined_df_cleaned_mod_new.csv")


#Exclude Congo (incomplete PA data), and observations missing age, sex, height, and weight

all_df <- all_df %>% 
  filter(iso3_code != "COG") %>%
  filter(!is.na(total_mvpa) & !is.na(omvpa) & !is.na(nonocpa) & !is.na(sex) & !is.na(age) & !is.na(BMI))

#Exclude unfasted
#Exclude Liberia from total cholesterol analysis only 16 observations)
#Center the year variable on 2011, midpoint of data collection period
all_df$fasting_glucose[all_df$b1 == 1] <- NA
all_df$total_chol[all_df$iso3_code == "LBR"] <- NA
all_df$year_centered <- all_df$year - 2011

all_df$occupation <- as.factor(all_df$occupation)

# Scale PA per 150 min/week
all_df <- all_df %>%
  mutate(
    total_mvpa_150 = total_mvpa / 150,
    omvpa_150 = omvpa / 150,
    nonocpa_150 = nonocpa / 150
  )

# Create binary activity indicators
all_df$active_total_mvpa <- ifelse(all_df$total_mvpa > 0, 1, 0)
all_df$active_omvpa <- ifelse(all_df$omvpa > 0, 1, 0)
all_df$active_nonocpa <- ifelse(all_df$nonocpa > 0, 1, 0)

# Relevel categorical covariates
all_df$BMI_group <- factor(all_df$BMI_group, levels = c("Normal", "Underweight", "Overweight", "Obese", "missing_unknown"))
all_df$marital_status <- factor(all_df$marital_status, levels = c("2", "1", "3", "4", "5", "6", "missing_unknown"))
all_df$smoking_status <- factor(all_df$smoking_status, levels = c("2", "1", "missing_unknown"))
all_df$alcohol_status <- factor(all_df$alcohol_status, levels = c("2", "1", "missing_unknown"))
all_df$meet_eat <- factor(all_df$meet_eat, levels = c("2", "1", "missing_unknown"))

# Define variables for models (unscaled)
exposure_list <- c("total_mvpa_150", "omvpa_150", "nonocpa_150")
control_vars_2 <- c("sex", "marital_status", "alcohol_status", "smoking_status", "meet_eat", "country_class")
selection_vars <- c("occupation", "education", "country_class", "sex", "age")
control_vars_3_hr <- "BMI_group"
hr_outcome <- "hr"
hr_results <- data.frame()

# Select relevant variables

relevant_vars <- unique(c(
  "total_mvpa", "omvpa", "nonocpa", "log_ratio_omvpa",
  "total_mvpa_150", "omvpa_150", "nonocpa_150",
  "active_total_mvpa", "active_omvpa", "active_nonocpa",
  "sex", "marital_status", "alcohol_status", "smoking_status", "meet_eat", "country_class",
  "BMI_group", "hr", "year_centered",
  "occupation", "education", "country_class", "sex", "age",
  "fasting_glucose", "total_chol", "sbp", "dbp",
  "iso3_code", "country"
))
all_df <- all_df %>% dplyr::select(all_of(relevant_vars))



cat("Processing outcome:", hr_outcome, "\n")

# Selection equation
all_df[[paste0(hr_outcome, "_select")]] <- ifelse(!is.na(all_df[[hr_outcome]]), 1, 0)
selection_formula <- as.formula(paste0(hr_outcome, "_select ~ ", paste(selection_vars, collapse = " + ")))
probit_model <- tryCatch({
  glm(selection_formula, family = binomial(link = "probit"), data = all_df, control = glm.control(maxit = 100))
}, error = function(e) {
  cat("Error in probit for", hr_outcome, ":", e$message, "\n")
  return(NULL)
})

if (is.null(probit_model)) {
  cat("Skipping", hr_outcome, "due to probit model failure\n")
} else {
  all_df$z <- predict(probit_model, type = "link")
  all_df$Lambda <- dnorm(all_df$z) / pnorm(all_df$z)
  
  # Subset data for outcome model
  model_data <- all_df[!is.na(all_df[[hr_outcome]]) & !is.na(all_df$iso3_code), ]
  cat("Number of observations for", hr_outcome, ":", nrow(model_data), "\n")
  
  for (exposure in exposure_list) {
    active_var <- switch(exposure,
                         "total_mvpa_150" = "active_total_mvpa",
                         "omvpa_150" = "active_omvpa",
                         "nonocpa_150" = "active_nonocpa")
    control_vars_1 <- c(active_var)
    
    for (i in 1:3) {
      if (i == 1) controls <- control_vars_1
      if (i == 2) controls <- c(control_vars_1, control_vars_2)
      if (i == 3) controls <- c(control_vars_1, control_vars_2, control_vars_3_hr)
      
      # Outcome model with random slope for year
      formula <- as.formula(paste(hr_outcome, "~", exposure, "+", paste(controls, collapse = " + "), 
                                  "+ year_centered + Lambda + (1 + year_centered | country)"))
      model_data_subset <- model_data[complete.cases(model_data[, c(hr_outcome, exposure, controls, 
                                                                    "year_centered", "Lambda", "country")]), ]
      cat("Observations after NA removal for", hr_outcome, exposure, "stage", i, ":", nrow(model_data_subset), "\n")
      
      # Check observations per country
      country_counts <- table(model_data_subset$country)
      if (any(country_counts < 10)) {
        cat("Warning: Some countries have fewer than 10 observations for", hr_outcome, exposure, "stage", i, "\n")
        valid_countries <- names(country_counts[country_counts >= 10])
        model_data_subset <- model_data_subset %>% filter(country %in% valid_countries)
        cat("Observations after filtering low-count countries:", nrow(model_data_subset), "\n")
      }
      
      # Fit mixed-effects model
      model <- tryCatch({
        lmer(formula, data = model_data_subset, 
             control = lmerControl(optimizer = "bobyqa", optCtrl = list(maxfun = 2e5)))
      }, error = function(e) {
        cat("Error in lmer:", e$message, "\n")
        return(NULL)
      })
      if (is.null(model)) next
      
      # Extract coefficients and standard errors
      model_summary <- summary(model)$coefficients
      coefs <- model_summary[, "Estimate"]
      std_errors <- model_summary[, "Std. Error"]
      ci_lower <- coefs - 1.96 * std_errors
      ci_upper <- coefs + 1.96 * std_errors
      
      # Calculate semi-elasticities
      me <- tryCatch({
        slopes(model, variables = c(exposure, active_var), type = "response")
      }, error = function(e) {
        cat("Error in slopes for", hr_outcome, exposure, "stage", i, ":", e$message, "\n")
        return(NULL)
      })
      if (is.null(me)) next
      
      mean_outcome <- mean(model_data_subset[[hr_outcome]], na.rm = TRUE)
      
      me_exp <- me[me$term == exposure, ]
      slope_exp <- mean(me_exp$estimate)
      semi_elasticity_exp <- (slope_exp / mean_outcome) * 100
      se_semi_elasticity_exp <- (mean(me_exp$std.error) / mean_outcome) * 100
      ci_lower_se_exp <- semi_elasticity_exp - 1.96 * se_semi_elasticity_exp
      ci_upper_se_exp <- semi_elasticity_exp + 1.96 * se_semi_elasticity_exp
      
      me_active <- me[me$term == active_var, ]
      slope_active <- mean(me_active$estimate)
      semi_elasticity_active <- (slope_active / mean_outcome) * 100
      se_semi_elasticity_active <- (mean(me_active$std.error) / mean_outcome) * 100
      ci_lower_se_active <- semi_elasticity_active - 1.96 * se_semi_elasticity_active
      ci_upper_se_active <- semi_elasticity_active + 1.96 * se_semi_elasticity_active
      
      # Store results
      temp_coef_df <- data.frame(
        Outcome = hr_outcome,
        Exposure = exposure,
        Model_Stage = i,
        Variable = rownames(model_summary),
        Estimate = coefs,
        Std_Error = std_errors,
        CI_Lower = ci_lower,
        CI_Upper = ci_upper,
        Semi_Elasticity = ifelse(rownames(model_summary) == exposure, semi_elasticity_exp, 
                                 ifelse(rownames(model_summary) == active_var, semi_elasticity_active, NA)),
        SE_Semi_Elasticity = ifelse(rownames(model_summary) == exposure, se_semi_elasticity_exp, 
                                    ifelse(rownames(model_summary) == active_var, se_semi_elasticity_active, NA)),
        CI_Lower_SE = ifelse(rownames(model_summary) == exposure, ci_lower_se_exp, 
                             ifelse(rownames(model_summary) == active_var, ci_lower_se_active, NA)),
        CI_Upper_SE = ifelse(rownames(model_summary) == exposure, ci_upper_se_exp, 
                             ifelse(rownames(model_summary) == active_var, ci_upper_se_active, NA))
      )
      
      temp_fit_df <- data.frame(
        Outcome = hr_outcome,
        Exposure = exposure,
        Model_Stage = i,
        Variable = c("AIC", "BIC"),
        Estimate = c(AIC(model), BIC(model)),
        Std_Error = NA,
        CI_Lower = NA,
        CI_Upper = NA,
        Semi_Elasticity = NA,
        SE_Semi_Elasticity = NA,
        CI_Lower_SE = NA,
        CI_Upper_SE = NA
      )
      
      temp_coef_df <- rbind(temp_coef_df, temp_fit_df)
      hr_results <- rbind(hr_results, temp_coef_df)
    }
  }
  
  # Clean up temporary variables
  all_df[[paste0(hr_outcome, "_select")]] <- NULL
  all_df$z <- NULL
  all_df$Lambda <- NULL
}

write.csv(hr_results, "hr_noncomp_results.csv", row.names = FALSE)

hr_noncomp_results <- hr_results


#### BMI ####

# Load data
all_df <- read.csv("combined_df_cleaned_mod_new.csv")


#Exclude Congo (incomplete PA data), and observations missing age and sex

all_df <- all_df %>% 
  filter(iso3_code != "COG") %>%
  filter(!is.na(total_mvpa) & !is.na(omvpa) & !is.na(nonocpa) & !is.na(sex) & !is.na(age))

#Exclude unfasted
#Exclude Liberia from total cholesterol analysis only 16 observations)
#Center the year variable on 2011, midpoint ofdata collection period

all_df$fasting_glucose[all_df$b1 == 1] <- NA
all_df$total_chol[all_df$iso3_code == "LBR"] <- NA
all_df$year_centered <- all_df$year - 2011
all_df$occupation <- as.factor(all_df$occupation)

# Scale PA per 150 min/week
all_df <- all_df %>%
  mutate(
    total_mvpa_150 = total_mvpa / 150,
    omvpa_150 = omvpa / 150,
    nonocpa_150 = nonocpa / 150
  )

# Create binary activity indicators
all_df$active_total_mvpa <- ifelse(all_df$total_mvpa > 0, 1, 0)
all_df$active_omvpa <- ifelse(all_df$omvpa > 0, 1, 0)
all_df$active_nonocpa <- ifelse(all_df$nonocpa > 0, 1, 0)

# Relevel categorical covariates
all_df$BMI_group <- factor(all_df$BMI_group, levels = c("Normal", "Underweight", "Overweight", "Obese", "missing_unknown"))
all_df$marital_status <- factor(all_df$marital_status, levels = c("2", "1", "3", "4", "5", "6", "missing_unknown"))
all_df$smoking_status <- factor(all_df$smoking_status, levels = c("2", "1", "missing_unknown"))
all_df$alcohol_status <- factor(all_df$alcohol_status, levels = c("2", "1", "missing_unknown"))
all_df$meet_eat <- factor(all_df$meet_eat, levels = c("2", "1", "missing_unknown"))

# Define variables for models (unscaled)
exposure_list <- c("total_mvpa_150", "omvpa_150", "nonocpa_150")
control_vars_2 <- c("sex", "marital_status", "alcohol_status", "smoking_status", "meet_eat", "country_class")
selection_vars <- c("occupation", "education", "country_class", "sex", "age")

bmi_outcome <- "BMI"
bmi_results <- data.frame()

# Select relevant variables

relevant_vars <- unique(c(
  "total_mvpa", "omvpa", "nonocpa", "log_ratio_omvpa",
  "total_mvpa_150", "omvpa_150", "nonocpa_150",
  "active_total_mvpa", "active_omvpa", "active_nonocpa",
  "sex", "marital_status", "alcohol_status", "smoking_status", "meet_eat", "country_class",
  "BMI_group", "hr", "year_centered",
  "occupation", "education", "country_class", "sex", "age",
  "fasting_glucose", "total_chol", "sbp", "dbp","BMI",
  "iso3_code", "country"
))
all_df <- all_df %>% dplyr::select(all_of(relevant_vars))


cat("Processing outcome:", bmi_outcome, "\n")

# Selection equation
all_df[[paste0(bmi_outcome, "_select")]] <- ifelse(!is.na(all_df[[bmi_outcome]]), 1, 0)
selection_formula <- as.formula(paste0(bmi_outcome, "_select ~ ", paste(selection_vars, collapse = " + ")))
probit_model <- tryCatch({
  glm(selection_formula, family = binomial(link = "probit"), data = all_df, control = glm.control(maxit = 100))
}, error = function(e) {
  cat("Error in probit for", bmi_outcome, ":", e$message, "\n")
  return(NULL)
})

if (is.null(probit_model)) {
  cat("Skipping", bmi_outcome, "due to probit model failure\n")
} else {
  all_df$z <- predict(probit_model, type = "link")
  all_df$Lambda <- dnorm(all_df$z) / pnorm(all_df$z)
  
  # Subset data for outcome model
  model_data <- all_df[!is.na(all_df[[bmi_outcome]]) & !is.na(all_df$iso3_code), ]
  cat("Number of observations for", bmi_outcome, ":", nrow(model_data), "\n")
  
  for (exposure in exposure_list) {
    active_var <- switch(exposure,
                         "total_mvpa_150" = "active_total_mvpa",
                         "omvpa_150" = "active_omvpa",
                         "nonocpa_150" = "active_nonocpa")
    control_vars_1 <- c(active_var)
    
    for (i in 1:2) {
      if (i == 1) controls <- control_vars_1
      if (i == 2) controls <- c(control_vars_1, control_vars_2)
      
      # Outcome model with random slope for year
      formula <- as.formula(paste(bmi_outcome, "~", exposure, "+", paste(controls, collapse = " + "), 
                                  "+ year_centered + Lambda + (1 + year_centered | country)"))
      model_data_subset <- model_data[complete.cases(model_data[, c(bmi_outcome, exposure, controls, 
                                                                    "year_centered", "Lambda", "country")]), ]
      cat("Observations after NA removal for", bmi_outcome, exposure, "stage", i, ":", nrow(model_data_subset), "\n")
      
      # Check observations per country
      country_counts <- table(model_data_subset$country)
      if (any(country_counts < 10)) {
        cat("Warning: Some countries have fewer than 10 observations for", bmi_outcome, exposure, "stage", i, "\n")
        valid_countries <- names(country_counts[country_counts >= 10])
        model_data_subset <- model_data_subset %>% filter(country %in% valid_countries)
        cat("Observations after filtering low-count countries:", nrow(model_data_subset), "\n")
      }
      
      # Fit mixed-effects model
      model <- tryCatch({
        lmer(formula, data = model_data_subset, 
             control = lmerControl(optimizer = "bobyqa", optCtrl = list(maxfun = 2e5)))
      }, error = function(e) {
        cat("Error in lmer:", e$message, "\n")
        return(NULL)
      })
      if (is.null(model)) next
      
      # Extract coefficients and standard errors
      model_summary <- summary(model)$coefficients
      coefs <- model_summary[, "Estimate"]
      std_errors <- model_summary[, "Std. Error"]
      ci_lower <- coefs - 1.96 * std_errors
      ci_upper <- coefs + 1.96 * std_errors
      
      # Calculate semi-elasticities
      me <- tryCatch({
        slopes(model, variables = c(exposure, active_var), type = "response")
      }, error = function(e) {
        cat("Error in slopes for", bmi_outcome, exposure, "stage", i, ":", e$message, "\n")
        return(NULL)
      })
      if (is.null(me)) next
      
      mean_outcome <- mean(model_data_subset[[bmi_outcome]], na.rm = TRUE)
      
      me_exp <- me[me$term == exposure, ]
      slope_exp <- mean(me_exp$estimate)
      semi_elasticity_exp <- (slope_exp / mean_outcome) * 100
      se_semi_elasticity_exp <- (mean(me_exp$std.error) / mean_outcome) * 100
      ci_lower_se_exp <- semi_elasticity_exp - 1.96 * se_semi_elasticity_exp
      ci_upper_se_exp <- semi_elasticity_exp + 1.96 * se_semi_elasticity_exp
      
      me_active <- me[me$term == active_var, ]
      slope_active <- mean(me_active$estimate)
      semi_elasticity_active <- (slope_active / mean_outcome) * 100
      se_semi_elasticity_active <- (mean(me_active$std.error) / mean_outcome) * 100
      ci_lower_se_active <- semi_elasticity_active - 1.96 * se_semi_elasticity_active
      ci_upper_se_active <- semi_elasticity_active + 1.96 * se_semi_elasticity_active
      
      # Store results
      temp_coef_df <- data.frame(
        Outcome = bmi_outcome,
        Exposure = exposure,
        Model_Stage = i,
        Variable = rownames(model_summary),
        Estimate = coefs,
        Std_Error = std_errors,
        CI_Lower = ci_lower,
        CI_Upper = ci_upper,
        Semi_Elasticity = ifelse(rownames(model_summary) == exposure, semi_elasticity_exp, 
                                 ifelse(rownames(model_summary) == active_var, semi_elasticity_active, NA)),
        SE_Semi_Elasticity = ifelse(rownames(model_summary) == exposure, se_semi_elasticity_exp, 
                                    ifelse(rownames(model_summary) == active_var, se_semi_elasticity_active, NA)),
        CI_Lower_SE = ifelse(rownames(model_summary) == exposure, ci_lower_se_exp, 
                             ifelse(rownames(model_summary) == active_var, ci_lower_se_active, NA)),
        CI_Upper_SE = ifelse(rownames(model_summary) == exposure, ci_upper_se_exp, 
                             ifelse(rownames(model_summary) == active_var, ci_upper_se_active, NA))
      )
      
      temp_fit_df <- data.frame(
        Outcome = bmi_outcome,
        Exposure = exposure,
        Model_Stage = i,
        Variable = c("AIC", "BIC"),
        Estimate = c(AIC(model), BIC(model)),
        Std_Error = NA,
        CI_Lower = NA,
        CI_Upper = NA,
        Semi_Elasticity = NA,
        SE_Semi_Elasticity = NA,
        CI_Lower_SE = NA,
        CI_Upper_SE = NA
      )
      
      temp_coef_df <- rbind(temp_coef_df, temp_fit_df)
      bmi_results <- rbind(bmi_results, temp_coef_df)
    }
  }
  
  # Clean up temporary variables
  all_df[[paste0(bmi_outcome, "_select")]] <- NULL
  all_df$z <- NULL
  all_df$Lambda <- NULL
}

write.csv(bmi_results, "bmi_noncomp_results.csv", row.names = FALSE)

bmi_noncomp_results <- bmi_results

#Number of observations for fasting_glucose : 289194 

#stage 1 : 289194 
#stage 2 : 289194 
#stage 3 : 243978 

#Number of observations for total_chol : 265208 
#stage 1 : 265208 
#stage 2 : 265208 
#stage 3 : 223954 

#Number of observations for sbp : 420993 
#stage 1 : 420993 
#stage 2 : 420993 
#stage 3 : 351401 

#Number of observations for dbp : 420993 
#stage 1 : 420993 
#stage 2 : 420993 
#stage 3 : 351401 

#Number of observations for hr : 362471 
#stage 1 : 362471 
#stage 2 : 362471 
#stage 3 : 362471 

#Number of observations for BMI : 436961 
#stage 1 : 436961 
#stage 2 : 436961 

# Combine all results
all_noncomp_results <- rbind(original_noncomp_results, hr_noncomp_results, bmi_noncomp_results)
write.csv(all_noncomp_results, "all_noncomp_results.csv", row.names = FALSE)


#### Compositional ####

#### FG, Total Chol, SBP, DBP ####

# Load data
all_df <- read.csv("combined_df_cleaned_mod_new.csv")


#Exclude Congo (incomplete PA data), and observations missing age, sex, height, and weight

all_df <- all_df %>% 
  filter(iso3_code != "COG") %>%
  filter(!is.na(total_mvpa) & !is.na(omvpa) & !is.na(nonocpa) & !is.na(sex) & !is.na(age) & !is.na(BMI))

#Exclude unfasted
#Exclude Liberia from total cholesterol analysis only 16 observations)
#Center the year variable on 2011, midpoint of data collection period
all_df$fasting_glucose[all_df$b1 == 1] <- NA
all_df$total_chol[all_df$iso3_code == "LBR"] <- NA
all_df$year_centered <- all_df$year - 2011
all_df$occupation <- as.factor(all_df$occupation)

# Scale PA per 150 min/week
all_df <- all_df %>%
  mutate(
    total_mvpa_150 = total_mvpa / 150,
    active_total_mvpa = ifelse(total_mvpa > 0, 1, 0)
  )

# Relevel categorical covariates
all_df$BMI_group <- factor(all_df$BMI_group, levels = c("Normal", "Underweight", "Overweight", "Obese", "missing_unknown"))
all_df$marital_status <- factor(all_df$marital_status, levels = c("2", "1", "3", "4", "5", "6", "missing_unknown"))
all_df$smoking_status <- factor(all_df$smoking_status, levels = c("2", "1", "missing_unknown"))
all_df$alcohol_status <- factor(all_df$alcohol_status, levels = c("2", "1", "missing_unknown"))
all_df$meet_eat <- factor(all_df$meet_eat, levels = c("2", "1", "missing_unknown"))

# Define control variables
control_vars_2 <- c("sex", "marital_status", "alcohol_status", "smoking_status", "meet_eat", "country_class")
selection_vars <- c("occupation", "education", "country_class", "sex", "age")
control_vars_3 <- c("BMI_group", "hr")

# Define outcomes
outcomes <- c("fasting_glucose", "total_chol", "sbp", "dbp")
results <- data.frame()

# Compositional PA variables
pa_vars <- c("active_total_mvpa", "total_mvpa_150", "log_ratio_omvpa")

# Select relevant variables

relevant_vars <- unique(c(
  "total_mvpa", "omvpa", "nonocpa", "log_ratio_omvpa",
  "total_mvpa_150",
  "active_total_mvpa",
  "sex", "marital_status", "alcohol_status", "smoking_status", "meet_eat", "country_class",
  "BMI_group", "hr", "year_centered",
  "occupation", "education", "country_class", "sex", "age",
  "fasting_glucose", "total_chol", "sbp", "dbp",
  "iso3_code", "country"
))
all_df <- all_df %>% dplyr::select(all_of(relevant_vars))


# Loop through outcomes
for (outcome in outcomes) {
  cat("Processing outcome:", outcome, "\n")
  
  # Selection equation
  all_df[[paste0(outcome, "_select")]] <- ifelse(!is.na(all_df[[outcome]]), 1, 0)
  selection_formula <- as.formula(paste0(outcome, "_select ~ ", paste(selection_vars, collapse = " + ")))
  probit_model <- tryCatch({
    glm(selection_formula, family = binomial(link = "probit"), data = all_df, control = glm.control(maxit = 100))
  }, error = function(e) {
    cat("Error in probit for", outcome, ":", e$message, "\n")
    return(NULL)
  })
  if (is.null(probit_model)) next
  all_df$z <- predict(probit_model, type = "link")
  all_df$Lambda <- dnorm(all_df$z) / pnorm(all_df$z)
  
  # Subset data for outcome model
  model_data <- all_df[!is.na(all_df[[outcome]]) & !is.na(all_df$iso3_code), ]
  cat("Number of observations for", outcome, ":", nrow(model_data), "\n")
  
  if (outcome == "fasting_glucose") {
    sapply(control_vars_2, function(x) cat("Missing", x, ":", sum(is.na(model_data[[x]])), "\n"))
  }
  
  # Loop through stages
  for (i in 1:3) {
    if (i == 1) controls <- NULL  # Stage 1: PA variables only
    if (i == 2) controls <- control_vars_2
    if (i == 3) controls <- c(control_vars_2, control_vars_3)
    
    # Outcome model with random slope for year
    formula <- as.formula(paste(outcome, "~", paste(pa_vars, collapse = " + "),
                                if (!is.null(controls)) paste(" + ", paste(controls, collapse = " + ")) else "",
                                "+ year_centered + Lambda + (1 + year_centered | country)"))
    model_data_subset <- model_data[complete.cases(model_data[, c(outcome, pa_vars, controls,
                                                                  "year_centered", "Lambda", "country")]), ]
    cat("Observations after NA removal for", outcome, "stage", i, ":", nrow(model_data_subset), "\n")
    
    # Check observations per country
    country_counts <- table(model_data_subset$country)
    if (any(country_counts < 10)) {
      cat("Warning: Some countries have fewer than 10 observations for", outcome, "stage", i, "\n")
      valid_countries <- names(country_counts[country_counts >= 10])
      model_data_subset <- model_data_subset %>% filter(country %in% valid_countries)
      cat("Observations after filtering low-count countries:", nrow(model_data_subset), "\n")
    }
    
    # Fit mixed-effects model
    model <- tryCatch({
      lmer(formula, data = model_data_subset,
           control = lmerControl(optimizer = "bobyqa", optCtrl = list(maxfun = 2e5)))
    }, error = function(e) {
      cat("Error in lmer:", e$message, "\n")
      return(NULL)
    })
    if (is.null(model)) next
    
    # Extract coefficients and standard errors
    model_summary <- summary(model)$coefficients
    coefs <- model_summary[, "Estimate"]
    std_errors <- model_summary[, "Std. Error"]
    ci_lower <- coefs - 1.96 * std_errors
    ci_upper <- coefs + 1.96 * std_errors
    
    # Calculate semi-elasticities
    me <- tryCatch({
      slopes(model, variables = pa_vars, type = "response")
    }, error = function(e) {
      cat("Error in slopes for", outcome, "stage", i, ":", e$message, "\n")
      return(NULL)
    })
    if (is.null(me)) next
    
    mean_outcome <- mean(model_data_subset[[outcome]], na.rm = TRUE)
    
    # Store results
    temp_coef_df <- data.frame(
      Outcome = outcome,
      Model_Stage = i,
      Variable = rownames(model_summary),
      Estimate = coefs,
      Std_Error = std_errors,
      CI_Lower = ci_lower,
      CI_Upper = ci_upper,
      Semi_Elasticity = NA,
      SE_Semi_Elasticity = NA,
      CI_Lower_SE = NA,
      CI_Upper_SE = NA
    )
    
    # Add semi-elasticities for PA variables
    for (var in pa_vars) {
      me_var <- me[me$term == var, ]
      slope_var <- mean(me_var$estimate)
      semi_elasticity_var <- (slope_var / mean_outcome) * 100
      se_semi_elasticity_var <- (mean(me_var$std.error) / mean_outcome) * 100
      ci_lower_se_var <- semi_elasticity_var - 1.96 * se_semi_elasticity_var
      ci_upper_se_var <- semi_elasticity_var + 1.96 * se_semi_elasticity_var
      temp_coef_df$Semi_Elasticity[temp_coef_df$Variable == var] <- semi_elasticity_var
      temp_coef_df$SE_Semi_Elasticity[temp_coef_df$Variable == var] <- se_semi_elasticity_var
      temp_coef_df$CI_Lower_SE[temp_coef_df$Variable == var] <- ci_lower_se_var
      temp_coef_df$CI_Upper_SE[temp_coef_df$Variable == var] <- ci_upper_se_var
    }
    
    temp_fit_df <- data.frame(
      Outcome = outcome,
      Model_Stage = i,
      Variable = c("AIC", "BIC"),
      Estimate = c(AIC(model), BIC(model)),
      Std_Error = NA,
      CI_Lower = NA,
      CI_Upper = NA,
      Semi_Elasticity = NA,
      SE_Semi_Elasticity = NA,
      CI_Lower_SE = NA,
      CI_Upper_SE = NA
    )
    
    temp_coef_df <- rbind(temp_coef_df, temp_fit_df)
    results <- rbind(results, temp_coef_df)
  }
  
  # Clean up temporary variables
  all_df[[paste0(outcome, "_select")]] <- NULL
  all_df$z <- NULL
  all_df$Lambda <- NULL
}

# Save results
write.csv(results, "original_comp_results.csv", row.names = FALSE)

original_comp_results <- results


#### HR ####

# Load data
all_df <- read.csv("combined_df_cleaned_mod_new.csv")

#Exclude Congo (incomplete PA data), and observations missing age, sex, height, and weight

all_df <- all_df %>% 
  filter(iso3_code != "COG") %>%
  filter(!is.na(total_mvpa) & !is.na(omvpa) & !is.na(nonocpa) & !is.na(sex) & !is.na(age) & !is.na(BMI))

#Exclude unfasted
#Exclude Liberia from total cholesterol analysis only 16 observations)
#Center the year variable on 2011, midpoint ofdata collection period

all_df$fasting_glucose[all_df$b1 == 1] <- NA
all_df$total_chol[all_df$iso3_code == "LBR"] <- NA
all_df$year_centered <- all_df$year - 2011

all_df$occupation <- as.factor(all_df$occupation)

# Scale PA per 150 min/week

all_df <- all_df %>%
  mutate(
    total_mvpa_150 = total_mvpa / 150,
    active_total_mvpa = ifelse(total_mvpa > 0, 1, 0)
  )

# Relevel categorical covariates
all_df$BMI_group <- factor(all_df$BMI_group, levels = c("Normal", "Underweight", "Overweight", "Obese", "missing_unknown"))
all_df$marital_status <- factor(all_df$marital_status, levels = c("2", "1", "3", "4", "5", "6", "missing_unknown"))
all_df$smoking_status <- factor(all_df$smoking_status, levels = c("2", "1", "missing_unknown"))
all_df$alcohol_status <- factor(all_df$alcohol_status, levels = c("2", "1", "missing_unknown"))
all_df$meet_eat <- factor(all_df$meet_eat, levels = c("2", "1", "missing_unknown"))

# Define control variables
control_vars_2 <- c("sex", "marital_status", "alcohol_status", "smoking_status", "meet_eat", "country_class")
selection_vars <- c("occupation", "education", "country_class", "sex", "age")
control_vars_3_hr <- "BMI_group"

# Define outcome
hr_outcome <- "hr"
results <- data.frame()

# Compositional PA variables
pa_vars <- c("active_total_mvpa", "total_mvpa_150", "log_ratio_omvpa")

# Select relevant variables

relevant_vars <- unique(c(
  "total_mvpa", "omvpa", "nonocpa", "log_ratio_omvpa",
  "total_mvpa_150", 
  "active_total_mvpa", 
  "sex", "marital_status", "alcohol_status", "smoking_status", "meet_eat", "country_class",
  "BMI_group", "hr", "year_centered",
  "occupation", "education", "country_class", "sex", "age",
  "fasting_glucose", "total_chol", "sbp", "dbp",
  "iso3_code", "country"
))
all_df <- all_df %>% dplyr::select(all_of(relevant_vars))


cat("Processing outcome:", hr_outcome, "\n")

# Selection equation
all_df[[paste0(hr_outcome, "_select")]] <- ifelse(!is.na(all_df[[hr_outcome]]), 1, 0)
selection_formula <- as.formula(paste0(hr_outcome, "_select ~ ", paste(selection_vars, collapse = " + ")))
probit_model <- tryCatch({
  glm(selection_formula, family = binomial(link = "probit"), data = all_df, control = glm.control(maxit = 100))
}, error = function(e) {
  cat("Error in probit for", hr_outcome, ":", e$message, "\n")
  return(NULL)
})

if (is.null(probit_model)) {
  cat("Skipping", hr_outcome, "due to probit model failure\n")
} else {
  all_df$z <- predict(probit_model, type = "link")
  all_df$Lambda <- dnorm(all_df$z) / pnorm(all_df$z)
  
  # Subset data for outcome model
  model_data <- all_df[!is.na(all_df[[hr_outcome]]) & !is.na(all_df$iso3_code), ]
  cat("Number of observations for", hr_outcome, ":", nrow(model_data), "\n")
  
  # Loop through stages
  for (i in 1:3) {
    if (i == 1) controls <- NULL  # Stage 1: PA variables only
    if (i == 2) controls <- control_vars_2
    if (i == 3) controls <- c(control_vars_2, control_vars_3_hr)
    
    # Outcome model with random slope for year
    formula <- as.formula(paste(hr_outcome, "~", paste(pa_vars, collapse = " + "),
                                if (!is.null(controls)) paste(" + ", paste(controls, collapse = " + ")) else "",
                                "+ year_centered + Lambda + (1 + year_centered | country)"))
    model_data_subset <- model_data[complete.cases(model_data[, c(hr_outcome, pa_vars, controls,
                                                                  "year_centered", "Lambda", "country")]), ]
    cat("Observations after NA removal for", hr_outcome, "stage", i, ":", nrow(model_data_subset), "\n")
    
    # Check observations per country
    country_counts <- table(model_data_subset$country)
    if (any(country_counts < 10)) {
      cat("Warning: Some countries have fewer than 10 observations for", hr_outcome, "stage", i, "\n")
      valid_countries <- names(country_counts[country_counts >= 10])
      model_data_subset <- model_data_subset %>% filter(country %in% valid_countries)
      cat("Observations after filtering low-count countries:", nrow(model_data_subset), "\n")
    }
    
    # Fit mixed-effects model
    model <- tryCatch({
      lmer(formula, data = model_data_subset,
           control = lmerControl(optimizer = "bobyqa", optCtrl = list(maxfun = 2e5)))
    }, error = function(e) {
      cat("Error in lmer:", e$message, "\n")
      return(NULL)
    })
    if (is.null(model)) next
    
    # Extract coefficients and standard errors
    model_summary <- summary(model)$coefficients
    coefs <- model_summary[, "Estimate"]
    std_errors <- model_summary[, "Std. Error"]
    ci_lower <- coefs - 1.96 * std_errors
    ci_upper <- coefs + 1.96 * std_errors
    
    # Calculate semi-elasticities
    me <- tryCatch({
      slopes(model, variables = pa_vars, type = "response")
    }, error = function(e) {
      cat("Error in slopes for", hr_outcome, "stage", i, ":", e$message, "\n")
      return(NULL)
    })
    if (is.null(me)) next
    
    mean_outcome <- mean(model_data_subset[[hr_outcome]], na.rm = TRUE)
    
    # Store results
    temp_coef_df <- data.frame(
      Outcome = hr_outcome,
      Model_Stage = i,
      Variable = rownames(model_summary),
      Estimate = coefs,
      Std_Error = std_errors,
      CI_Lower = ci_lower,
      CI_Upper = ci_upper,
      Semi_Elasticity = NA,
      SE_Semi_Elasticity = NA,
      CI_Lower_SE = NA,
      CI_Upper_SE = NA
    )
    
    # Add semi-elasticities for PA variables
    for (var in pa_vars) {
      me_var <- me[me$term == var, ]
      slope_var <- mean(me_var$estimate)
      semi_elasticity_var <- (slope_var / mean_outcome) * 100
      se_semi_elasticity_var <- (mean(me_var$std.error) / mean_outcome) * 100
      ci_lower_se_var <- semi_elasticity_var - 1.96 * se_semi_elasticity_var
      ci_upper_se_var <- semi_elasticity_var + 1.96 * se_semi_elasticity_var
      temp_coef_df$Semi_Elasticity[temp_coef_df$Variable == var] <- semi_elasticity_var
      temp_coef_df$SE_Semi_Elasticity[temp_coef_df$Variable == var] <- se_semi_elasticity_var
      temp_coef_df$CI_Lower_SE[temp_coef_df$Variable == var] <- ci_lower_se_var
      temp_coef_df$CI_Upper_SE[temp_coef_df$Variable == var] <- ci_upper_se_var
    }
    
    temp_fit_df <- data.frame(
      Outcome = hr_outcome,
      Model_Stage = i,
      Variable = c("AIC", "BIC"),
      Estimate = c(AIC(model), BIC(model)),
      Std_Error = NA,
      CI_Lower = NA,
      CI_Upper = NA,
      Semi_Elasticity = NA,
      SE_Semi_Elasticity = NA,
      CI_Lower_SE = NA,
      CI_Upper_SE = NA
    )
    
    temp_coef_df <- rbind(temp_coef_df, temp_fit_df)
    results <- rbind(results, temp_coef_df)
  }
  
  # Clean up temporary variables
  all_df[[paste0(hr_outcome, "_select")]] <- NULL
  all_df$z <- NULL
  all_df$Lambda <- NULL
}

# Save results
write.csv(results, "hr_comp_results.csv", row.names = FALSE)

hr_comp_results <- results

#### BMI ####

# Load data
all_df <- read.csv("combined_df_cleaned_mod_new.csv")

#Exclude Congo (incomplete PA data), and observations missing age and sex

all_df <- all_df %>% 
  filter(iso3_code != "COG") %>%
  filter(!is.na(total_mvpa) & !is.na(omvpa) & !is.na(nonocpa) & !is.na(sex) & !is.na(age))


#Exclude unfasted
#Exclude Liberia from total cholesterol analysis only 16 observations)
#Center the year variable on 2011, midpoint ofdata collection period
all_df$fasting_glucose[all_df$b1 == 1] <- NA
all_df$total_chol[all_df$iso3_code == "LBR"] <- NA
all_df$year_centered <- all_df$year - 2011
all_df$occupation <- as.factor(all_df$occupation)

# Scale PA per 150 min/week
all_df <- all_df %>%
  mutate(
    total_mvpa_150 = total_mvpa / 150,
    active_total_mvpa = ifelse(total_mvpa > 0, 1, 0)
  )

# Relevel categorical covariates
all_df$BMI_group <- factor(all_df$BMI_group, levels = c("Normal", "Underweight", "Overweight", "Obese", "missing_unknown"))
all_df$marital_status <- factor(all_df$marital_status, levels = c("2", "1", "3", "4", "5", "6", "missing_unknown"))
all_df$smoking_status <- factor(all_df$smoking_status, levels = c("2", "1", "missing_unknown"))
all_df$alcohol_status <- factor(all_df$alcohol_status, levels = c("2", "1", "missing_unknown"))
all_df$meet_eat <- factor(all_df$meet_eat, levels = c("2", "1", "missing_unknown"))

# Define control variables
control_vars_2 <- c("sex", "marital_status", "alcohol_status", "smoking_status", "meet_eat", "country_class")
selection_vars <- c("occupation", "education", "country_class", "sex", "age")

# Define outcome
bmi_outcome <- "BMI"
results <- data.frame()

# Compositional PA variables
pa_vars <- c("active_total_mvpa", "total_mvpa_150", "log_ratio_omvpa")

# Select relevant variables

relevant_vars <- unique(c(
  "total_mvpa", "omvpa", "nonocpa", "log_ratio_omvpa",
  "total_mvpa_150", 
  "active_total_mvpa",
  "sex", "marital_status", "alcohol_status", "smoking_status", "meet_eat", "country_class",
  "BMI_group", "hr", "year_centered",
  "occupation", "education", "country_class", "sex", "age",
  "fasting_glucose", "total_chol", "sbp", "dbp","BMI",
  "iso3_code", "country"
))
all_df <- all_df %>% dplyr::select(all_of(relevant_vars))


cat("Processing outcome:", bmi_outcome, "\n")

# Selection equation
all_df[[paste0(bmi_outcome, "_select")]] <- ifelse(!is.na(all_df[[bmi_outcome]]), 1, 0)
selection_formula <- as.formula(paste0(bmi_outcome, "_select ~ ", paste(selection_vars, collapse = " + ")))
probit_model <- tryCatch({
  glm(selection_formula, family = binomial(link = "probit"), data = all_df, control = glm.control(maxit = 100))
}, error = function(e) {
  cat("Error in probit for", bmi_outcome, ":", e$message, "\n")
  return(NULL)
})

if (is.null(probit_model)) {
  cat("Skipping", bmi_outcome, "due to probit model failure\n")
} else {
  all_df$z <- predict(probit_model, type = "link")
  all_df$Lambda <- dnorm(all_df$z) / pnorm(all_df$z)
  
  # Subset data for outcome model
  model_data <- all_df[!is.na(all_df[[bmi_outcome]]) & !is.na(all_df$iso3_code), ]
  cat("Number of observations for", bmi_outcome, ":", nrow(model_data), "\n")
  
  # Loop through stages (1 and 2 only)
  for (i in 1:2) {
    if (i == 1) controls <- NULL  # Stage 1: PA variables only
    if (i == 2) controls <- control_vars_2
    
    # Outcome model with random slope for year
    formula <- as.formula(paste(bmi_outcome, "~", paste(pa_vars, collapse = " + "),
                                if (!is.null(controls)) paste(" + ", paste(controls, collapse = " + ")) else "",
                                "+ year_centered + Lambda + (1 + year_centered | country)"))
    model_data_subset <- model_data[complete.cases(model_data[, c(bmi_outcome, pa_vars, controls,
                                                                  "year_centered", "Lambda", "country")]), ]
    cat("Observations after NA removal for", bmi_outcome, "stage", i, ":", nrow(model_data_subset), "\n")
    
    # Check observations per country
    country_counts <- table(model_data_subset$country)
    if (any(country_counts < 10)) {
      cat("Warning: Some countries have fewer than 10 observations for", bmi_outcome, "stage", i, "\n")
      valid_countries <- names(country_counts[country_counts >= 10])
      model_data_subset <- model_data_subset %>% filter(country %in% valid_countries)
      cat("Observations after filtering low-count countries:", nrow(model_data_subset), "\n")
    }
    
    # Fit mixed-effects model
    model <- tryCatch({
      lmer(formula, data = model_data_subset,
           control = lmerControl(optimizer = "bobyqa", optCtrl = list(maxfun = 2e5)))
    }, error = function(e) {
      cat("Error in lmer:", e$message, "\n")
      return(NULL)
    })
    if (is.null(model)) next
    
    # Extract coefficients and standard errors 
    model_summary <- summary(model)$coefficients
    coefs <- model_summary[, "Estimate"]
    std_errors <- model_summary[, "Std. Error"]
    ci_lower <- coefs - 1.96 * std_errors
    ci_upper <- coefs + 1.96 * std_errors
    
    # Calculate semi-elasticities
    me <- tryCatch({
      slopes(model, variables = pa_vars, type = "response")
    }, error = function(e) {
      cat("Error in slopes for", bmi_outcome, "stage", i, ":", e$message, "\n")
      return(NULL)
    })
    if (is.null(me)) next
    
    mean_outcome <- mean(model_data_subset[[bmi_outcome]], na.rm = TRUE)
    
    # Store results
    temp_coef_df <- data.frame(
      Outcome = bmi_outcome,
      Model_Stage = i,
      Variable = rownames(model_summary),
      Estimate = coefs,
      Std_Error = std_errors,
      CI_Lower = ci_lower,
      CI_Upper = ci_upper,
      Semi_Elasticity = NA,
      SE_Semi_Elasticity = NA,
      CI_Lower_SE = NA,
      CI_Upper_SE = NA
    )
    
    # Add semi-elasticities for PA variables
    for (var in pa_vars) {
      me_var <- me[me$term == var, ]
      slope_var <- mean(me_var$estimate)
      semi_elasticity_var <- (slope_var / mean_outcome) * 100
      se_semi_elasticity_var <- (mean(me_var$std.error) / mean_outcome) * 100
      ci_lower_se_var <- semi_elasticity_var - 1.96 * se_semi_elasticity_var
      ci_upper_se_var <- semi_elasticity_var + 1.96 * se_semi_elasticity_var
      temp_coef_df$Semi_Elasticity[temp_coef_df$Variable == var] <- semi_elasticity_var
      temp_coef_df$SE_Semi_Elasticity[temp_coef_df$Variable == var] <- se_semi_elasticity_var
      temp_coef_df$CI_Lower_SE[temp_coef_df$Variable == var] <- ci_lower_se_var
      temp_coef_df$CI_Upper_SE[temp_coef_df$Variable == var] <- ci_upper_se_var
    }
    
    temp_fit_df <- data.frame(
      Outcome = bmi_outcome,
      Model_Stage = i,
      Variable = c("AIC", "BIC"),
      Estimate = c(AIC(model), BIC(model)),
      Std_Error = NA,
      CI_Lower = NA,
      CI_Upper = NA,
      Semi_Elasticity = NA,
      SE_Semi_Elasticity = NA,
      CI_Lower_SE = NA,
      CI_Upper_SE = NA
    )
    
    temp_coef_df <- rbind(temp_coef_df, temp_fit_df)
    results <- rbind(results, temp_coef_df)
  }
  
  # Clean up temporary variables
  all_df[[paste0(bmi_outcome, "_select")]] <- NULL
  all_df$z <- NULL
  all_df$Lambda <- NULL
}

# Save results
write.csv(results, "bmi_comp_results.csv", row.names = FALSE)

bmi_comp_results <- results

# Combine all comp results
all_comp_results <- rbind(original_comp_results, hr_comp_results, bmi_comp_results)
write.csv(all_comp_results, "all_compositional_results.csv", row.names = FALSE)

# Combine comp and noncomp results

all_comp_results$Exposure <- "Compositional"
all_results <- rbind(all_noncomp_results,all_comp_results)

write.csv(all_results, "all_cross_results.csv", row.names = FALSE)


#### Multicollinearity Checks ####

# Load data
all_df <- read.csv("combined_df_cleaned_mod_new.csv") %>%
  filter(iso3_code != "COG",
         !is.na(age) & !is.na(sex) & 
           !is.na(total_mvpa) & !is.na(omvpa) & !is.na(nonocpa)) %>%
  mutate(year_centered = year - 2011,
         occupation = as.factor(occupation),
         fasting_glucose = ifelse(b1 == 1, NA, fasting_glucose),
         total_mvpa_150 = total_mvpa / 150,
         omvpa_150 = omvpa / 150,
         nonocpa_150 = nonocpa / 150,
         nonocpa_150_scaled = scale(nonocpa_150),
         active_total_mvpa = ifelse(total_mvpa > 0, 1, 0),
         active_omvpa = ifelse(omvpa > 0, 1, 0),
         active_nonocpa = ifelse(nonocpa > 0, 1, 0))

# Relevel categorical covariates
all_df$BMI_group <- factor(all_df$BMI_group, levels = c("Normal", "Underweight", "Overweight", "Obese", "missing_unknown"))
all_df$marital_status <- factor(all_df$marital_status, levels = c("2", "1", "3", "4", "5", "6", "missing_unknown"))
all_df$smoking_status <- factor(all_df$smoking_status, levels = c("2", "1", "missing_unknown"))
all_df$alcohol_status <- factor(all_df$alcohol_status, levels = c("2", "1", "missing_unknown"))
all_df$meet_eat <- factor(all_df$meet_eat, levels = c("2", "1", "missing_unknown"))
all_df$country_class <- factor(all_df$country_class, levels = c("High-income countries", "Low-income countries", 
                                                                "Lower-middle-income countries", "Upper-middle-income countries"))

# Define variables
selection_vars <- c("occupation", "education", "country_class", "sex", "age")
controls_stage3 <- c("active_nonocpa", "sex", "marital_status", "alcohol_status", "smoking_status", "meet_eat", "country_class", "BMI_group", "hr")
outcomes <- c("fasting_glucose", "total_chol", "sbp", "dbp", "BMI")
exposure <- "nonocpa_150_scaled"
stage <- 3

# Function to calculate VIF for an outcome
calculate_vif <- function(outcome, data, exposure, controls, selection_vars) {
  cat("\n--- Calculating VIF for", outcome, "---\n")
  
  # Selection model
  data[[paste0(outcome, "_select")]] <- ifelse(!is.na(data[[outcome]]), 1, 0)
  selection_formula <- as.formula(paste0(outcome, "_select ~ ", paste(selection_vars, collapse = " + ")))
  probit_model <- tryCatch({
    glm(selection_formula, family = binomial(link = "probit"), data = data, control = glm.control(maxit = 100))
  }, error = function(e) {
    cat("Error in probit for", outcome, ":", e$message, "\n")
    return(NULL)
  })
  if (is.null(probit_model)) return(NULL)
  
  data$z <- predict(probit_model, type = "link")
  data$Lambda <- dnorm(data$z) / pnorm(data$z)
  
  # Subset data for outcome model
  model_data <- data[!is.na(data[[outcome]]) & !is.na(data$iso3_code), ] %>%
    .[complete.cases(.[, c(outcome, exposure, controls, "year_centered", "Lambda")]), ]
  
  # Linear model for VIF
  lm_formula <- as.formula(paste(outcome, "~", exposure, "+", paste(controls, collapse = " + "), "+ year_centered + Lambda"))
  lm_model <- tryCatch({
    lm(lm_formula, data = model_data)
  }, error = function(e) {
    cat("Error fitting linear model for", outcome, ":", e$message, "\n")
    return(NULL)
  })
  if (is.null(lm_model)) return(NULL)
  
  # Calculate and display VIF
  vif_values <- vif(lm_model)
  cat("VIF values for", outcome, ":\n")
  print(vif_values)
  
  # Clean up
  data[[paste0(outcome, "_select")]] <- NULL
  data$z <- NULL
  data$Lambda <- NULL
  
  return(vif_values)
}

# Run VIF for all outcomes
vif_list <- lapply(outcomes, calculate_vif, data = all_df, exposure = exposure, 
                   controls = controls_stage3, selection_vars = selection_vars)


#### Figures ####

# Load results
all_results <- read.csv("all_cross_results.csv") 

# Filter for PA variables
all_results <- all_results %>%
  filter(grepl("active|mvpa|nonocpa", Variable, ignore.case = TRUE))

# Rename outcomes for consistency
all_results <- all_results %>%
  mutate(Outcome = case_when(
    Outcome == "fasting_glucose" ~ "Fasting Glucose",
    Outcome == "total_chol" ~ "Total Cholesterol",
    Outcome == "sbp" ~ "Systolic BP",
    Outcome == "dbp" ~ "Diastolic BP",
    Outcome == "hr" ~ "RHR",
    Outcome == "BMI" ~ "BMI",
    TRUE ~ as.character(Outcome)
  ))

# Define predictor labels for clarity in plots and tables
predictor_labels <- c(
  "active_total_mvpa" = "Non-Zero Total MVPA",
  "total_mvpa_150" = "Total MVPA",
  "active_omvpa" = "Non-Zero OMVPA",
  "omvpa_150" = "OMVPA",
  "active_nonocpa" = "Non-Zero Non-OCPA",
  "nonocpa_150" = "Non-OCPA",
  "log_ratio_omvpa" = "OMVPA Proportion"
)

# Function to create a forest plot for a single predictor
create_forest_plot <- function(data, variable, model_stage, title) {
  # Set the order of outcomes
  outcome_order <- c("BMI", "RHR", "Diastolic BP", "Systolic BP", "Total Cholesterol", "Fasting Glucose")
  
  # Filter data for the specific variable and model stage
  plot_data <- data %>% 
    filter(Variable == variable, Model_Stage == model_stage)
  
  # Reorder Outcome levels
  plot_data$Outcome <- factor(plot_data$Outcome, levels = outcome_order)
  
  # Determine symmetric range for x-axis
  max_abs_value <- max(abs(plot_data$CI_Lower_SE), abs(plot_data$CI_Upper_SE), na.rm = TRUE)
  buffer <- max_abs_value * 0.1
  plot_limits <- c(-max_abs_value - buffer, max_abs_value + buffer)
  
  # Create the plot
  plot <- plot_data %>%
    ggplot(aes(x = Semi_Elasticity, y = Outcome)) +
    geom_point(color = "black") +
    geom_errorbarh(aes(xmin = CI_Lower_SE, xmax = CI_Upper_SE), height = 0) +
    geom_vline(xintercept = 0, color = "black", linetype = "solid") +
    scale_x_continuous(limits = plot_limits, expand = c(0, 0), oob = scales::oob_squish) +
    geom_segment(data = . %>% filter(CI_Upper_SE > plot_limits[2]),
                 aes(x = plot_limits[2], xend = plot_limits[2] - buffer * 0.05, yend = Outcome),
                 arrow = arrow(type = "closed", length = unit(0.1, "inches")),
                 color = "red") +
    geom_segment(data = . %>% filter(CI_Lower_SE < plot_limits[1]),
                 aes(x = plot_limits[1], xend = plot_limits[1] + buffer * 0.05, yend = Outcome),
                 arrow = arrow(type = "closed", length = unit(0.1, "inches")),
                 color = "red") +
    labs(x = "Semi-Elasticity (%)", y = NULL, title = title) +
    theme_minimal() +
    theme(
      axis.title.y = element_blank(),
      plot.title = element_text(size = 12, face = "bold", hjust = 0.5),
      strip.text = element_text(size = 10),
      axis.text.y = element_text(margin = margin(t = 0, r = 10, b = 0, l = 0))
    )
  
  return(plot)
}

# Filter out Model 1
filtered_results <- all_results %>% filter(Model_Stage != 1)

# --- Compositional Forest Plots ---
comp_results <- filtered_results %>% filter(Exposure == "Compositional")

# Create plots for Model 2
plot_comp_active_model2 <- create_forest_plot(comp_results, "active_total_mvpa", 2, "Non-Zero MVPA - Model 2")
plot_comp_total_mvpa_model2 <- create_forest_plot(comp_results, "total_mvpa_150", 2, "Total MVPA - Model 2")
plot_comp_log_ratio_model2 <- create_forest_plot(comp_results, "log_ratio_omvpa", 2, "OMVPA Proportion - Model 2")

# Create plots for Model 3
plot_comp_active_model3 <- create_forest_plot(comp_results, "active_total_mvpa", 3, "Non-Zero MVPA - Model 3")
plot_comp_total_mvpa_model3 <- create_forest_plot(comp_results, "total_mvpa_150", 3, "Total MVPA - Model 3")
plot_comp_log_ratio_model3 <- create_forest_plot(comp_results, "log_ratio_omvpa", 3, "OMVPA Proportion - Model 3")

# Combine plots into rows
combined_row_comp_model2 <- plot_grid(
  plot_comp_active_model2, plot_comp_total_mvpa_model2, plot_comp_log_ratio_model2,
  ncol = 3, labels = c("A", "B", "C"), label_size = 14, align = "h"
)
combined_row_comp_model3 <- plot_grid(
  plot_comp_active_model3, plot_comp_total_mvpa_model3, plot_comp_log_ratio_model3,
  ncol = 3, labels = c("D", "E", "F"), label_size = 14, align = "h"
)

# Final compositional plot
final_combined_plot_comp <- plot_grid(
  ggdraw() + draw_label("Model 2 - Compositional", fontface = 'bold', size = 14, hjust = -0.1),
  combined_row_comp_model2,
  ggdraw() + draw_label("Model 3 - Compositional", fontface = 'bold', size = 14, hjust = -0.1),
  combined_row_comp_model3,
  ncol = 1, rel_heights = c(0.1, 1, 0.1, 1)
)

# Save the compositional plot
ggsave("compositional_forest_plot.jpeg", plot = final_combined_plot_comp, width = 15, height = 10, dpi = 300)

# --- Non-Compositional Forest Plots ---
# Filter for non-compositional models
noncomp_results <- all_results %>% filter(Exposure != "Compositional")

# Define domains and their corresponding binary and continuous variables
domains <- list(
  "total_mvpa_150" = c(binary = "active_total_mvpa", continuous = "total_mvpa_150"),
  "omvpa_150" = c(binary = "active_omvpa", continuous = "omvpa_150"),
  "nonocpa_150" = c(binary = "active_nonocpa", continuous = "nonocpa_150")
)

# Generate forest plots for each domain
for (domain in names(domains)) {
  # Filter data for the current domain
  domain_data <- noncomp_results %>% filter(Exposure == domain)
  plot_list <- list()
  
  # Create plots for Model 2 and Model 3, for both binary and continuous variables
  for (model_stage in c(2, 3)) {
    for (var_type in c("binary", "continuous")) {
      variable <- domains[[domain]][var_type]
      title <- paste(predictor_labels[variable], "- Model", model_stage)
      plot <- create_forest_plot(
        data = domain_data,
        variable = variable,
        model_stage = model_stage,
        title = title
      )
      plot_list[[length(plot_list) + 1]] <- plot
    }
  }
  
  # Arrange the four plots in a 2x2 grid
  combined_plot <- plot_grid(
    plotlist = plot_list,
    ncol = 2,
    labels = "AUTO"
  )
  
  # Add a title for the domain
  domain_title <- switch(domain,
                         "total_mvpa_150" = "Total MVPA",
                         "omvpa_150" = "Occupational MVPA (OMVPA)",
                         "nonocpa_150" = "Non-Occupational MVPA (Non-OCPA)"
  )
  final_plot <- ggdraw() +
    draw_plot(combined_plot) 
  
  # Save the figure
  ggsave(
    filename = paste0("noncomp_forest_plot_", domain, ".jpeg"),
    plot = final_plot,
    width = 12,
    height = 10,
    dpi = 300
  )
}



#### Descriptive Table ####

# Load data
all_df <- read.csv("combined_df_cleaned_mod_new.csv")
all_df$country <- all_df$iso3_code

data <- read.xlsx('global_set.xlsx')
data$country <- countrycode(data$country, origin = "country.name", destination = "iso3c")
data$country[175] <- "TUR"

final_merged <- read.csv("pa_eco_new.csv")


# Exclude Congo (COG) from all three dataframes and filter missing valued
all_df <- all_df %>% 
  filter(country != "COG") %>%
  filter(!is.na(total_mvpa) & !is.na(omvpa) & !is.na(nonocpa) & 
           !is.na(sex) & !is.na(age) & !is.na(BMI))

#Exclude Liberia from total cholesterol analysis only 16 observations)
all_df$total_chol[all_df$iso3_code == "LBR"] <- NA


final_merged <- final_merged %>%
  filter(country != "COG")  


# Descriptives by country
all_stats_combined <- all_df %>%
  group_by(country, region) %>%
  summarise(
    n_obs = n(),
    age = sprintf("%0.1f (%0.1f)", median(age, na.rm = TRUE), IQR(age, na.rm = TRUE)),
    omvpa = sprintf("%0.2f (%0.2f)", mean(omvpa, na.rm = TRUE), sd(omvpa, na.rm = TRUE)),
    nonocpa = sprintf("%0.2f (%0.2f)", mean(nonocpa, na.rm = TRUE), sd(nonocpa, na.rm = TRUE)),
    obese = sprintf("%d (%0.2f%%)", sum(obese == 1, na.rm = TRUE), 100 * mean(obese == 1, na.rm = TRUE)),
    alcohol_status = sprintf("%d (%0.2f%%)", sum(alcohol_status == 1, na.rm = TRUE), 100 * mean(alcohol_status == 1, na.rm = TRUE)),
    smoking_status = sprintf("%d (%0.2f%%)", sum(smoking_status == 1, na.rm = TRUE), 100 * mean(smoking_status == 1, na.rm = TRUE)),
    male = sprintf("%d (%0.2f%%)", sum(sex == 1, na.rm = TRUE), 100 * mean(sex == 1, na.rm = TRUE)),
    meet_eat = sprintf("%d (%0.2f%%)", sum(meet_eat == 1, na.rm = TRUE), 100 * mean(meet_eat == 1, na.rm = TRUE)),
    n_total_chol = sum(!is.na(total_chol)),
    n_fasting_glucose = sum(!is.na(fasting_glucose)),
    n_sbp = sum(!is.na(sbp)),
    n_hr = sum(!is.na(hr)),
    n_bmi = sum(!is.na(BMI))
  ) %>%
  ungroup() %>%
  mutate(
    n_total_chol = as.character(ifelse(n_total_chol == 0, "-", n_total_chol)),
    n_fasting_glucose = as.character(ifelse(n_fasting_glucose == 0, "-", n_fasting_glucose)),
    n_sbp = as.character(ifelse(n_sbp == 0, "-", n_sbp)),
    n_hr = as.character(ifelse(n_hr == 0, "-", n_hr)),
    n_bmi = as.character(ifelse(n_bmi == 0, "-", n_bmi))
  )

# Summarize for the entire dataset
total_row <- all_df %>%
  summarise(
    country = "Total",
    region = "Total",
    n_obs = n(),
    age = sprintf("%0.1f (%0.1f)", median(age, na.rm = TRUE), IQR(age, na.rm = TRUE)),
    omvpa = sprintf("%0.2f (%0.2f)", mean(omvpa, na.rm = TRUE), sd(omvpa, na.rm = TRUE)),
    nonocpa = sprintf("%0.2f (%0.2f)", mean(nonocpa, na.rm = TRUE), sd(nonocpa, na.rm = TRUE)),
    obese = sprintf("%d (%0.2f%%)", sum(obese == 1, na.rm = TRUE), 100 * mean(obese == 1, na.rm = TRUE)),
    alcohol_status = sprintf("%d (%0.2f%%)", sum(alcohol_status == 1, na.rm = TRUE), 100 * mean(alcohol_status == 1, na.rm = TRUE)),
    smoking_status = sprintf("%d (%0.2f%%)", sum(smoking_status == 1, na.rm = TRUE), 100 * mean(smoking_status == 1, na.rm = TRUE)),
    male = sprintf("%d (%0.2f%%)", sum(sex == 1, na.rm = TRUE), 100 * mean(sex == 1, na.rm = TRUE)),
    meet_eat = sprintf("%d (%0.2f%%)", sum(meet_eat == 1, na.rm = TRUE), 100 * mean(meet_eat == 1, na.rm = TRUE)),
    n_total_chol = sum(!is.na(total_chol)),
    n_fasting_glucose = sum(!is.na(fasting_glucose)),
    n_sbp = sum(!is.na(sbp)),
    n_hr = sum(!is.na(hr)),
    n_bmi = sum(!is.na(BMI))
  ) %>%
  mutate(
    n_total_chol = as.character(ifelse(n_total_chol == 0, "-", n_total_chol)),
    n_fasting_glucose = as.character(ifelse(n_fasting_glucose == 0, "-", n_fasting_glucose)),
    n_sbp = as.character(ifelse(n_sbp == 0, "-", n_sbp)),
    n_hr = as.character(ifelse(n_hr == 0, "-", n_hr)),
    n_bmi = as.character(ifelse(n_bmi == 0, "-", n_bmi))
  )

# Combine the summarised data with the regional and total rows
all_stats_combined <- bind_rows(all_stats_combined, total_row)


all_stats_combined_numeric <- all_stats_combined %>%
  filter(country != "Total") %>%  # Remove the summary row
  mutate(
    omvpa_numeric = as.numeric(str_extract(omvpa, "^[0-9]+\\.[0-9]+|^[0-9]+")),
    nonocpa_numeric = as.numeric(str_extract(nonocpa, "^[0-9]+\\.[0-9]+|^[0-9]+"))
  )


final_merged_country <- final_merged %>%
  group_by(country) %>%
  summarise(
    region = first(region),
    population_2022 = first(population_2022),
    agri = first(agri),
    age_depend = first(age_depend),
    median_age = first(median_age),
    male_smoke = first(male_smoke),
    female_smoke = first(female_smoke),
    pm_25 = first(pm_25),
    Educational_Attainment = first(Educational_Attainment),
    gdp_per_capita = first(gdp_per_capita),
    omvpa_mean = mean(omvpa_mean, na.rm = TRUE),
    nonocpa_mean = mean(nonocpa_mean, na.rm = TRUE),
    obesity_rate_eco = mean(obesity_rate, na.rm = TRUE),
    t2d_prev_eco = mean(t2d_prev, na.rm = TRUE),
    all_cause_eco = mean(all_cause, na.rm = TRUE),
    cvd_deaths_eco = mean(cvd_deaths, na.rm = TRUE)
  )

final_table <- data %>%
  rename(region_global = region) %>%  # Rename to avoid overlap
  full_join(all_stats_combined_numeric %>% rename(region_all = region), 
            by = "country", suffix = c("_global", "_all")) %>%
  full_join(final_merged_country %>% rename(region_eco = region), 
            by = "country", suffix = c("", "_final"))


final_table <- final_table %>%
  mutate(
    region = coalesce(region_all, region_eco, region_global)
  ) %>%
  dplyr::select(-region_global, -region_all, -region_eco)


final_table <- final_table %>%
  mutate(
    omvpa = coalesce(omvpa_numeric, omvpa_mean),
    nonocpa = coalesce(nonocpa_numeric, nonocpa_mean)
  ) %>%
  dplyr::select(-omvpa_numeric, -nonocpa_numeric, -omvpa_mean, -nonocpa_mean)

# select columns
required_columns <- c(
  "country",                        # Country
  "region",                         # Region
  "agri",                           # Employment in Agriculture (%)
  "omvpa",                          # OMVPA (numeric)
  "nonocpa",                        # Non-Occ PA (numeric)
  "t2d_prev",                       # Diabetes Prevalence
  "obesity_rate",                   # Obesity Prevalence (using obesity_rate from data as 'obesity')
  "cvd_deaths",                     # CVD Mortality
  "all_cause",                      # All-Cause Mortality
  "population",                     # Population
  "median_age",                     # Median Age
  "age_depend",                     # Age Dependency Ratio
  "gdp",                            # GDP (total GDP from data)
  "Educational_Attainment",         # Educational Attainment
  "female_smoke",                   # Smoking Prevalence (Female)
  "male_smoke",                     # Smoking Prevalence (Male)
  "pm_25",                          # Air Pollution from PM2.5
  # Remaining columns from all_stats_combined
  "n_obs",
  "age",
  "obese",
  "alcohol_status",
  "smoking_status",
  "male",
  "meet_eat",
  "n_total_chol",
  "n_fasting_glucose",
  "n_sbp",
  "n_hr",
  "n_bmi"
)

final_table <- final_table %>%
  dplyr::select(all_of(required_columns))

# Convert country codes to names
final_table$country <- countrycode(final_table$country, origin = "iso3c", destination = "country.name")

final_table <- final_table %>%
  filter(country != "Andorra")


write.csv(final_table, "who_pa_domain_desc.csv", row.names = FALSE)

