# STEPS_PA_Domain

The first step in the analysis was to clean harmonize and merge variables and WHO STEP survey datasets. This script is entitled "Dataset Cleaning and Harmonization Script". While the syntax is already annotated, a summary of how the variables were processed is below:

Here we describe the data processing steps applied to raw survey datasets to generate the cleaned dataset used in the analysis. The process involved loading, renaming, recoding, standardizing, cleaning, deriving variables, and subsetting data from multiple versions of the WHO STEP survey (1.3, 1.4, 2.0, 2.1, 2.2, 3.0, 3.1, 3.2), culminating in a dataset of approximately 493,000 observations.
 
1. Data Loading and Initial Setup
 
Source: Raw CSV files from country-specific surveys were loaded into a list (list_of_dfs) using read.csv() from the tidyverse package. Dataframes were named based on filenames (e.g., "afg2018" for Afghanistan 2018) via tools::file_path_sans_ext(basename(file_list)).
 
Metadata Extraction: Country (ISO3 code) and year were extracted from filenames using extract_metadata(). Non-standard codes (e.g., "alg" to "dza", "bvi" to "vgb") were mapped, and special cases (e.g., "MDG205sub" as Madagascar 2005, "lbn2017" as Lebanon 2017) were handled explicitly.
 
2. Variable Renaming and Initial Standardization
 
Version Mapping: Each dataset was assigned a version (1.3–3.2) using a predefined version_map (e.g., "afg2018" = 3.2, "alg2003sub" = 2.0), reflecting survey format differences.
 
Renaming: Variables were renamed to standardized names using var_map within standardize_df() (e.g., "c1" to "sex", "m3" or "m11" to "height"). Version-specific mappings ensured consistency across surveys. For versions 1.3 and 1.4, additional corrections were applied (e.g., "m16a" to "heart_rate_1").
 
Inconsistent Naming: Duplicates (e.g., "c1" vs. "sex") were resolved by prioritizing standardized names and dropping originals (e.g., select(-c1) if "sex" existed).
 
Unique Identifier: A unique_id was created as paste(country, year, pid, sep = "_"), with pid derived from "pid", "id", or "i4", defaulting to NA_character_ if absent.
 
 
3. Variable Recoding and Type Conversion
 
Initial Recoding in standardize_df():
 
Sex: Recoded to "1" (male) or "2" (female) from variants (e.g., "male", "men"), others as NA; factored with levels "1", "2".
 
Education: Recoded to "1"–"7" (e.g., "1" = no schooling, "4" = secondary completed, skipping "5"), others as NA; factored.
 
Marital Status: Recoded to "1"–"6" (e.g., "1" = never married, "6" = cohabiting), others as NA; factored.
 
Occupation: Recoded to "1"–"9", others as NA; factored.
 
Smoking Status: Recoded to "1" (smoker) or "2" (non-smoker) from variants (e.g., "0" as "2"), others as NA; factored.
 
Alcohol Status: Recoded to "1" (drinker) or "2" (non-drinker) from variants (e.g., "0" as "2"), others as NA; factored.
 
Numeric Variables: Converted to numeric (e.g., "age", "height", "sbp_1"), with non-numeric values coerced to NA via as.numeric().
 
Final Recoding in all_df:
 
Sex: Further cleaned to "1" (male) or "2" (female), with "0" mapped to "2" (female), others as NA; factored.
 
Age: Values < 15 set to NA.
 
Education: Recoded to "1"–"7", others as "missing_unknown"; factored with "missing_unknown".
 
Marital Status: Recoded to "1"–"6", values 7, 8, 88, 99, or Inf to "missing_unknown"; factored with "missing_unknown".
 
Occupation: Recoded to "1"–"9", others (including 0, 10–20, 77, 88, 95, 99, Inf) to "missing_unknown"; factored with "missing_unknown".
 
Smoking Status: Recoded to "1" or "2", values 0, 3–7, 77, 8, 9, 99, or Inf to "missing_unknown"; factored with "missing_unknown".
 
Alcohol Status: Recoded to "1" or "2", values 0, 3, 7, 77, 8, 88, 9, 99, or Inf to "missing_unknown"; factored with "missing_unknown".
 
Physical Activity (PA) Variables:
p1, p4, p7, p10, p13: Recoded to "1" or "2", others to "missing_unknown"; factored.
p2, p5, p8, p11, p14: Recoded to "0"–"7", others to "missing_unknown"; factored.
p3a, p6a, p9a, p12a, p15a: Recoded to "0"–"18", others to "missing_unknown"; factored, later converted to numeric.
p3b, p6b, p9b, p12b, p15b: Recoded to "0"–"60", others to "missing_unknown"; factored, later converted to numeric.
 
 
 
Diet Variables:
d1 (fruit_freq), d3 (veg_freq): Recoded to "0"–"7", others to "missing_unknown"; factored, later converted to numeric.
d2 (fruit_serv_day), d4 (veg_serv_day): Recoded to 0–15 (including decimals), others to "missing_unknown"; factored, later converted to numeric.
 
Fasting Status (b1): Recoded to "1" (fasted) or "2" (non-fasted), others (e.g., 0, 5, 7, 22, 77, 8, 9, 99, Inf) to "missing_unknown"; factored.
 
 
4. Merging and Column Selection
 
Column Selection: Retained a subset of relevant columns (e.g., "sex", "age", "height", "total_chol", "sbp_1"–"sbp_3", "b1") using select_relevant_columns(), adding missing columns as NA_character_. 
 
Merging: Combined into all_df via bind_rows(), initially as character, then converting numeric (e.g., "age", "sbp_1") and factor (e.g., "sex", "b1") columns post-merge.
 
5. Derived Variables
 
Height: Converted from cm to meters (height_m = height / 100).
 
BMI: Calculated as weight / (height_m^2); winsorized to 12–60 by adjusting weight.
•            Obese: Binary factor: "1" (BMI > 30), "2" (BMI ≤ 30).
 
Physical Activity (PA): Converted PA factors to numeric, treating "missing_unknown" as NA.
 
•            Vigorous Work (vig_work): (p3a * 60 + p3b) * p2 if p1 = "1", else 0 or NA.
•            Moderate Work (mod_work): (p6a * 60 + p6b) * p5 if p4 = "1", else 0 or NA.
•            Travel Activity (travel): (p9a * 60 + p9b) * p8 if p7 = "1", else 0 or NA.
•            Vigorous Leisure (vig_leisure): (p12a * 60 + p12b) * p11 if p10 = "1", else 0 or NA.
•            Moderate Leisure (mod_leisure): (p15a * 60 + p15b) * p14 if p13 = "1", else 0 or NA.
•            Occupational MVPA (omvpa): vig_work + mod_work, NA if both inputs are NA.
•            Leisure-Time MVPA (ltmvpa): vig_leisure + mod_leisure, NA if both inputs are NA.
•            Non-Occupational PA (nonocpa): travel + ltmvpa, NA if both inputs are NA.
•            Total MVPA (total_mvpa): omvpa + nonocpa, NA if both inputs are NA.
•            Meet PA (meet_pa): "1" (≥150 min/week), "0" (<150 min/week).
•            Active: "1" (total_mvpa > 0), "0" (total_mvpa = 0); factored.
•            Active Occupational (active_omvpa): "1" (omvpa > 0), "0" otherwise; factored.
•            Active Non-Occupational (active_nonocpa): "1" (nonocpa > 0), "0" otherwise; factored.
•            Log Ratio: log((omvpa + 1e-6) / (total_mvpa + 1e-6)).
 

Diet:
 
•            Converted diet factors to numeric, treating "missing_unknown" as NA.
•            Total Servings/Week: (fruit_freq * fruit_serv_day) + (veg_freq * veg_serv_day).
•            Average Servings/Day: total_servings_week / 7.
•            Meet Eat: "1" (≥5 servings/day), "2" (<5 servings/day), "missing_unknown" if NA; factored.
 
Weight Class: Categorized BMI into "1" (<18.5), "2" (18.5–24.9), "3" (25–29.9), "4" (≥30); factored.
•            BMI_group: WHO classification ("Underweight" <18.5, "Normal" 18.5–24.9, "Overweight" 25–29.9, "Obese" ≥30), "missing_unknown" if NA; factored.
 
 
6. Cleaning and Winsorization
 
Infinite Values: Numeric Inf replaced with NA; string "inf" in character columns as NA_character_.
 
Implausible Values: Filtered values (666, 666.6, 666.66, 77, 777, 777.7, 777.77, 888, 888.8, 888.88, 99.99, 999, 999.9, 999.99, Inf, >500) across "height", "weight", "sbp_1"–"sbp_3", "dbp_1"–"dbp_3", "heart_rate_1"–"heart_rate_3", "fasting_glucose", "total_chol" to NA using replace_implausible() with tolerance for floating-point comparison.
 
Assumption on Heart Rate Variables for Specific Countries: For versions 1.3 and 1.4 in Mozambique, Cameroon, DRC, Côte d'Ivoire, and Madagascar, heart rate data were coded as "m16a", "m16b", "m16c" (not "m8a", "m8b", "m8c" as expected). These were assumed to be harmonized during data collection and treated as equivalent to "heart_rate_1", "heart_rate_2", and "heart_rate_3", with the same cleaning applied.
 
Winsorization:
 
Height: <1st percentile to 1st percentile, >99th percentile capped at 230 cm; initial bounds 50–250 cm.
 
Weight: <1st percentile to 1st percentile, >99th percentile capped at 250 kg; initial bounds 20–300 kg.
 
BMI: Adjusted weight to enforce 12–60, recalculated as weight / (height_m^2).
 
Fasting Glucose: Converted mixed units (mg/dL if >20 to mmol/L via * 0.0555, mmol/L if 1.1–20) to fasting_glucose_mmol (<1.1 or >55.5 to NA); final value set to fasting_glucose_mmol.
 
Total Cholesterol: Converted mixed units (mg/dL if >40 to mmol/L via * 0.02586, mmol/L if 1–12.93) to total_chol_mmol (<1 or >12.93 to NA); final value set to total_chol_mmol.
SBP/DBP (3 readings):
 
•            Filters: SBP >190, <70, >180 with DBP >90, SBP < DBP, pulse pressure <20 or >120 to NA; DBP >110, <40 to NA.
•            Winsorized: sbp_ll_* to 70, sbp_ul_* to 190, dbp_ll_* to 40, dbp_ul_* to 110 if out of bounds or NA.
 
Heart Rate: <30 or >130 bpm to NA; hr_ll_* to 30, hr_ul_* to 130 if out of bounds or NA.
 
 
Physical Activity:
 
•            Components (vig_work, mod_work, travel, vig_leisure, mod_leisure) filtered: >8,000 min/week to NA.
•            Derived variables recalculated: omvpa, ltmvpa, nonocpa, total_mvpa.
•            All PA variables (omvpa, travel, ltmvpa, nonocpa, total_mvpa) filtered: >8,000 min/week to NA, winsorized <0 to 0, >99th percentile capped at 99th percentile.
 
Final Values:
 
SBP/DBP: Selected lowest plausible SBP from sbp_1–sbp_3 (sbp) with paired DBP (dbp) using pmin(), ensuring pulse pressure 20–120 mmHg; all-NA cases set to NA.
 
Heart Rate: Lowest plausible value from heart_rate_1–heart_rate_3 (hr) within 30–130 bpm using pmin(), all-NA cases set to NA.
 
7. Subsetting
 
Fasting Status: b1 recoded ("1" and "2" kept, others to "missing_unknown"); (b1 = "1", not fasted).
 
8. External Data Integration
 
Population: Merged 2022 population data via new_country (ISO3 to country name via countrycode); updated for Niue (2,000), Tokelau (1,647), Cook Islands (7,761), Wallis & Futuna (15,964).
 
Income Classification: Merged 2022 World Bank income groups; added Cook Islands, Niue, Tokelau, Wallis & Futuna as "Low-income countries".
 
Region: Assigned based on iso3_code (e.g., "Sub-Saharan Africa", "South and Southeast Asia") using predefined lists (e.g., "AFG" in "Central and East Asia").
 
 
9. Diagnostics
 
Initial Diagnostics: Applied to list_of_dfs_selected by country-year, generating histograms (plot_numeric), bar plots (plot_categorical), and summaries (summarize_numeric, summarize_categorical) for variables (e.g., "total_chol", "fasting_glucose"); output to "diagnostics/[country_year]/" folders. Non-numeric values in numeric columns were flagged.
 
10. Final Output
 
Dataset: Saved using write_csv().
 
Notes:
 
Units: fasting_glucose and total_chol started as mixed units (mg/dL if >20 or >40, mmol/L otherwise), standardized to mmol/L during winsorization.
 
Plausibility: Final ranges reflect clinical norms (e.g., BMI 12–60, HR 30–130 bpm), with initial implausible values (e.g., 666, >500) removed.
 
Missing Data: Non-numeric values coerced to NA; "missing_unknown" category used for categorical variables; outcome variables (sbp, dbp, fasting_glucose, total_chol, bmi, hr) vary in availability by analysis.
 
 
