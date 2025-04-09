library(tidyverse)
library(dplyr)
library(countrycode)
library(stringr)
library(openxlsx)
library(ggplot2)

#### Clean, Diagnostics, and Merge ####

# 1. Load all CSVs
file_list <- list.files(pattern = "\\.csv$", full.names = TRUE)
list_of_dfs <- lapply(file_list, read.csv, stringsAsFactors = FALSE)
names(list_of_dfs) <- tools::file_path_sans_ext(basename(file_list))

# Save the original names of the dataframes
original_names <- names(list_of_dfs)

# 2. Dataset-Version Mapping
version_map <- c(
  "afg2018" = "3.2", "alg2003sub" = "2.0", "alg2016" = "3.1", "arm2016" = "3.1",
  "asm2004" = "1.4", "aze2017" = "3.2", "ben2007" = "2.0", "ben2008" = "2.0",
  "ben2015" = "3.1", "bgd2009" = "2.1", "bgd2018" = "3.1", "bhs2011" = "2.1",
  "blr2016" = "3.1", "bol2019" = "3.2", "brb2007" = "2.0", "brn2015" = "3.1",
  "BVI2009" = "2.1", "bwa2007" = "2.0", "bwa2014" = "3.0", "caf2010" = "2.1",
  "caf2017" = "2.1", "chad2008sub" = "2.0", "civ2005sub" = "1.4", "cmr2003sub" = "1.3",
  "COK2003" = "1.4", "cok2013" = "2.2", "com2011" = "2.1", "congo2004sub" = "1.3",
  "cpv2007" = "2.0", "cpv2020" = "3.2", "CYM2012" = "2.1", "drc2005sub" = "1.4",
  "ecu2018" = "3.2", "eri2004" = "1.4", "eri2010" = "2.1", "eth2006sub" = "2.0",
  "eth2015" = "3.1", "FJI2011" = "2.1", "FSM(Chuuk)2006sub" = "1.4",
  "FSM(Chuuk)2016sub" = "3.1", "FSM(Kosrae)2009sub" = "2.0", "FSM(Pohnpei)2002sub" = "1.3",
  "FSM(Pohnpei)2008sub" = "2.0", "FSM(Yap)2009sub" = "2.0", "GAB2009sub" = "2.0",
  "geo2010" = "2.1", "geo2016" = "3.1", "gha2006sub" = "2.0", "gin2009sub" = "2.1",
  "GMB2010" = "2.1", "GRD2010" = "2.1", "guy2016" = "3.1", "irq2015" = "3.1",
  "jor2019" = "3.2", "ken2015" = "3.1", "kgz2013" = "3.0", "khm2010" = "2.1",
  "KIR2004" = "1.4", "kir2015" = "3.1", "kwt2006" = "2.0", "kwt2014" = "3.0",
  "LAO2008" = "2.0", "lao2013" = "2.2", "lbn2017-lebanese" = "3.1", "lbn2017-syrian" = "3.1",
  "lbr2011" = "2.1", "lby2009" = "2.1", "lca2019" = "3.2", "lka2006" = "2.0",
  "LKA2014" = "3.1", "lso2012" = "2.1", "mar2017" = "3.1", "mda2013" = "3.0",
  "MDG205sub" = "1.3", "mdv2011" = "2.1", "mhl2002" = "2.0", "mhl2017" = "3.1",
  "MLI2007sub" = "1.4", "mli2013sub" = "2.0", "MMR2014" = "3.0", "mng2005" = "2.0",
  "MNG2009" = "2.1", "MNG2013" = "3.0", "mng2019" = "3.2", "moz2005" = "1.4",
  "mrt2006sub" = "2.0", "mwi2009" = "2.1", "mwi2017" = "3.2", "ner2007" = "2.0",
  "niu2011" = "2.1", "npl2012" = "2.2", "npl2019" = "3.2", "NRU2004" = "1.4",
  "NRU2015" = "3.1", "pak2013sub" = "3.0", "plw2011" = "2.1", "plw2016" = "3.1",
  "pse2010" = "2.1", "PYF2010" = "2.1", "Qatar2012" = "2.1", "rwanda2012" = "2.2",
  "samoa2013" = "2.2", "sdn2016" = "3.1", "slb2006" = "1.4", "slb2015" = "3.1",
  "sle2009" = "2.1", "stp2008" = "2.0", "stp2019" = "3.2", "swz2007" = "2.0",
  "swz2014" = "3.1", "tgo2010" = "2.1", "tjk2016" = "3.1", "tkl2005" = "2.0",
  "TKL2014" = "3.1", "tkm2018" = "3.2", "tls2014" = "3.0", "ton2004" = "1.4",
  "tonga2011" = "2.1", "tonga2017" = "3.1", "TUV2015" = "3.1", "TZA(Zanzibar)2011sub" = "2.1",
  "tza2012" = "2.0", "uga2014" = "3.0", "ukr2019" = "3.2", "ury2006" = "2.0",
  "ury2013" = "2.1", "vnm2009" = "2.0", "vnm2015" = "3.1", "vut2005" = "1.4",
  "vut2011" = "2.1", "wlf2019" = "3.2", "wsm2002" = "2.0", "ZMB2008sub" = "2.0",
  "zmb2017" = "3.1"
)

var_map <- list(
  "1.3" = c("c1" = "sex", "c3" = "age", "c6" = "education", "c7" = "occupation", "s1a" = "smoking_status",
            "a1b" = "alcohol_status", "d1a" = "fruit_freq", "d1b" = "fruit_serv_day", "d2a" = "veg_freq",
            "d2b" = "veg_serv_day", "p1" = "p1", "p2" = "p2", "p3a" = "p3a", "p3b" = "p3b", "p4" = "p4",
            "p5" = "p5", "p6a" = "p6a", "p6b" = "p6b", "p7" = "p7", "p8" = "p8", "p9a" = "p9a", "p9b" = "p9b",
            "p10" = "p10", "p11" = "p11", "p12a" = "p12a", "p12b" = "p12b", "p13" = "p13", "p14" = "p14",
            "p15a" = "p15a", "p15b" = "p15b",
            "h2"="hbp_diag","h3a"="hbp_med","h7"="t2d_diag","h8a"="insulin",
            "m3" = "height","m4" = "weight", "m8a" = "heart_rate_1", "m8b" = "heart_rate_2", "m8c" = "heart_rate_3",
            "m12a" = "sbp_1", "m12b" = "dbp_1", "m13a" = "sbp_2", "m13b" = "dbp_2", "m14a" = "sbp_3",
            "m14b" = "dbp_3", "b5" = "fasting_glucose", "b8" = "total_chol","b11" = "trig", "b14" = "hdl"),
  "1.4" = c("c1" = "sex", "c3" = "age", "c6" = "education", "c7" = "occupation", "s1a" = "smoking_status",
            "a1b" = "alcohol_status", "d1a" = "fruit_freq", "d1b" = "fruit_serv_day", "d2a" = "veg_freq",
            "d2b" = "veg_serv_day", "p1" = "p1", "p2" = "p2", "p3a" = "p3a", "p3b" = "p3b", "p4" = "p4",
            "p5" = "p5", "p6a" = "p6a", "p6b" = "p6b", "p7" = "p7", "p8" = "p8", "p9a" = "p9a", "p9b" = "p9b",
            "p10" = "p10", "p11" = "p11", "p12a" = "p12a", "p12b" = "p12b", "p13" = "p13", "p14" = "p14",
            "p15a" = "p15a", "p15b" = "p15b", "m3" = "height",
            "h2"="hbp_diag","h3a"="hbp_med","h7"="t2d_diag","h8a"="insulin",
            "m4" = "weight", "m17a" = "heart_rate_1", "m17b" = "heart_rate_2", "m17c" = "heart_rate_3",
            "m12a" = "sbp_1", "m12b" = "dbp_1", "m13a" = "sbp_2", "m13b" = "dbp_2", "m14a" = "sbp_3",
            "m14b" = "dbp_3", "b5" = "fasting_glucose", "b8" = "total_chol","b11" = "trig", "b14" = "hdl"),
  "2.0" = c("c1" = "sex", "c3" = "age", "c6" = "education", "c7" = "occupation", "t1" = "smoking_status",
            "a1" = "alcohol_status", "d1" = "fruit_freq", "d2" = "fruit_serv_day", "d3" = "veg_freq",
            "d4" = "veg_serv_day", "p1" = "p1", "p2" = "p2", "p3a" = "p3a", "p3b" = "p3b", "p4" = "p4",
            "p5" = "p5", "p6a" = "p6a", "p6b" = "p6b", "p7" = "p7", "p8" = "p8", "p9a" = "p9a", "p9b" = "p9b",
            "p10" = "p10", "p11" = "p11", "p12a" = "p12a", "p12b" = "p12b", "p13" = "p13", "p14" = "p14",
            "p15a" = "p15a", "p15b" = "p15b", "m3" = "height", "m4" = "weight", "m16a" = "heart_rate_1",
            "h2"="hbp_diag","h3a"="hbp_med","h7a"="t2d_diag","h8a"="insulin",
            "m16b" = "heart_rate_2", "m16c" = "heart_rate_3", "m11a" = "sbp_1", "m11b" = "dbp_1",
            "m12a" = "sbp_2", "m12b" = "dbp_2", "m13a" = "sbp_3", "m13b" = "dbp_3", "b5" = "fasting_glucose",
            "b7" = "total_chol","b8" = "trig", "b9" = "hdl"),
  "2.1" = c("c1" = "sex", "c3" = "age", "c5" = "education", "c7" = "marital_status", "c8" = "occupation",
            "t1" = "smoking_status", "a1b" = "alcohol_status", "d1" = "fruit_freq", "d2" = "fruit_serv_day",
            "d3" = "veg_freq", "d4" = "veg_serv_day", "p1" = "p1", "p2" = "p2", "p3a" = "p3a", "p3b" = "p3b", "p4" = "p4",
            "p5" = "p5", "p6a" = "p6a", "p6b" = "p6b", "p7" = "p7", "p8" = "p8", "p9a" = "p9a", "p9b" = "p9b",
            "p10" = "p10", "p11" = "p11", "p12a" = "p12a", "p12b" = "p12b", "p13" = "p13", "p14" = "p14",
            "p15a" = "p15a", "p15b" = "p15b", "m3" = "height", "m4" = "weight",
            "h2b"="hbp_diag","h3a"="hbp_med","h7a"="t2d_diag","h8a"="insulin",
            "m16a" = "heart_rate_1", "m16b" = "heart_rate_2", "m16c" = "heart_rate_3", "m11a" = "sbp_1",
            "m11b" = "dbp_1", "m12a" = "sbp_2", "m12b" = "dbp_2", "m13a" = "sbp_3", "m13b" = "dbp_3",
            "b5" = "fasting_glucose", "b8" = "total_chol","b10" = "trig", "b11" = "hdl"),
  "2.2" = c("c1" = "sex", "c3" = "age", "c5" = "education", "c7" = "marital_status", "c8" = "occupation",
            "t1" = "smoking_status", "a1b" = "alcohol_status", "d1" = "fruit_freq", "d2" = "fruit_serv_day",
            "d3" = "veg_freq", "d4" = "veg_serv_day", "p1" = "p1", "p2" = "p2", "p3a" = "p3a", "p3b" = "p3b", "p4" = "p4",
            "p5" = "p5", "p6a" = "p6a", "p6b" = "p6b", "p7" = "p7", "p8" = "p8", "p9a" = "p9a", "p9b" = "p9b",
            "p10" = "p10", "p11" = "p11", "p12a" = "p12a", "p12b" = "p12b", "p13" = "p13", "p14" = "p14",
            "p15a" = "p15a", "p15b" = "p15b", "m3" = "height", "m4" = "weight",
            "h2b"="hbp_diag","h3a"="hbp_med","h7a"="t2d_diag","h8a"="insulin",
            "m16a" = "heart_rate_1", "m16b" = "heart_rate_2", "m16c" = "heart_rate_3", "m11a" = "sbp_1",
            "m11b" = "dbp_1", "m12a" = "sbp_2", "m12b" = "dbp_2", "m13a" = "sbp_3", "m13b" = "dbp_3",
            "b5" = "fasting_glucose", "b8" = "total_chol","b10" = "trig", "b11" = "hdl"),
  "3.0" = c("c1" = "sex", "c3" = "age", "c5" = "education", "c7" = "marital_status", "c8" = "occupation",
            "t1" = "smoking_status", "a2" = "alcohol_status", "d1" = "fruit_freq", "d2" = "fruit_serv_day",
            "d3" = "veg_freq", "d4" = "veg_serv_day","p1" = "p1", "p2" = "p2", "p3a" = "p3a", "p3b" = "p3b", "p4" = "p4",
            "p5" = "p5", "p6a" = "p6a", "p6b" = "p6b", "p7" = "p7", "p8" = "p8", "p9a" = "p9a", "p9b" = "p9b",
            "p10" = "p10", "p11" = "p11", "p12a" = "p12a", "p12b" = "p12b", "p13" = "p13", "p14" = "p14",
            "p15a" = "p15a", "p15b" = "p15b", "m11" = "height", "m12" = "weight",
            "h2b"="hbp_diag","h3"="hbp_med","h7a"="t2d_diag","h9"="insulin",
            "m16a" = "heart_rate_1", "m16b" = "heart_rate_2", "m16c" = "heart_rate_3", "m4a" = "sbp_1",
            "m4b" = "dbp_1", "m5a" = "sbp_2", "m5b" = "dbp_2", "m6a" = "sbp_3", "m6b" = "dbp_3",
            "b5" = "fasting_glucose", "b8" = "total_chol","b10" = "trig", "b11" = "hdl"),
  "3.1" = c("c1" = "sex", "c3" = "age", "c5" = "education", "c7" = "marital_status", "c8" = "occupation",
            "t1" = "smoking_status", "a2" = "alcohol_status", "d1" = "fruit_freq", "d2" = "fruit_serv_day",
            "d3" = "veg_freq", "d4" = "veg_serv_day", "p1" = "p1", "p2" = "p2", "p3a" = "p3a", "p3b" = "p3b", "p4" = "p4",
            "p5" = "p5", "p6a" = "p6a", "p6b" = "p6b", "p7" = "p7", "p8" = "p8", "p9a" = "p9a", "p9b" = "p9b",
            "p10" = "p10", "p11" = "p11", "p12a" = "p12a", "p12b" = "p12b", "p13" = "p13", "p14" = "p14",
            "p15a" = "p15a", "p15b" = "p15b", "m11" = "height", "m12" = "weight",
            "h2"="hbp_diag","h3a"="hbp_med","h7a"="t2d_diag","h9"="insulin",
            "m16a" = "heart_rate_1", "m16b" = "heart_rate_2", "m16c" = "heart_rate_3", "m4a" = "sbp_1",
            "m4b" = "dbp_1", "m5a" = "sbp_2", "m5b" = "dbp_2", "m6a" = "sbp_3", "m6b" = "dbp_3",
            "b5" = "fasting_glucose", "b8" = "total_chol","b14" = "u_sod","b15"="u_cre","b16" = "trig", "b17" = "hdl"),
  "3.2" = c("c1" = "sex", "c3" = "age", "c5" = "education", "c7" = "marital_status", "c8" = "occupation",
            "t1" = "smoking_status", "a2" = "alcohol_status", "d1" = "fruit_freq", "d2" = "fruit_serv_day",
            "d3" = "veg_freq", "d4" = "veg_serv_day", "p1" = "p1", "p2" = "p2", "p3a" = "p3a", "p3b" = "p3b", "p4" = "p4",
            "p5" = "p5", "p6a" = "p6a", "p6b" = "p6b", "p7" = "p7", "p8" = "p8", "p9a" = "p9a", "p9b" = "p9b",
            "p10" = "p10", "p11" = "p11", "p12a" = "p12a", "p12b" = "p12b", "p13" = "p13", "p14" = "p14",
            "p15a" = "p15a", "p15b" = "p15b", "m11" = "height", "m12" = "weight",
            "h2"="hbp_diag","h3a"="hbp_med","h7a"="t2d_diag","h9"="insulin",
            "m16a" = "heart_rate_1", "m16b" = "heart_rate_2", "m16c" = "heart_rate_3", "m4a" = "sbp_1",
            "m4b" = "dbp_1", "m5a" = "sbp_2", "m5b" = "dbp_2", "m6a" = "sbp_3", "m6b" = "dbp_3",
            "b5" = "fasting_glucose", "b8" = "total_chol","b14" = "u_sod","b15"="u_cre","b16" = "trig", "b17" = "hdl")
)

# 3. Function to extract country and year from filename

extract_metadata <- function(df_name) {
  # Define a mapping for non-standard country codes to standard ISO3 codes
  country_code_map <- c(
    "alg" = "dza",  # Algeria
    "bvi" = "vgb",  # British Virgin Islands
    "cha" = "tcd",  # Chad
    "con" = "cog",  # Republic of the Congo
    "drc" = "cod",  # Democratic Republic of the Congo
    "sam" = "wsm",  # Samoa
    "mdg" = "mdg",  # Madagascar 
    "lbn" = "lbn",  # Lebanon
    "qat" = "qat"   # Qatar 
  )
  
  # Special cases
  if (df_name == "MDG205sub") {
    return(list(country = "mdg", year = "2005"))
  } else if (grepl("lbn2017", df_name)) {
    return(list(country = "lbn", year = "2017"))
  } else if (df_name == "Qatar2012") {
    return(list(country = "qat", year = "2012"))
  } else {
    # Extract the first three characters as the country code
    iso3 <- tolower(substr(df_name, 1, 3))
    
    # Map non-standard codes to standard ISO3 codes if present in the map
    if (iso3 %in% names(country_code_map)) {
      iso3 <- country_code_map[iso3]
    }
    
    # Extract the year (four digits)
    year_match <- str_extract(df_name, "\\d{4}")
    year <- ifelse(is.na(year_match), NA, year_match)
    
    return(list(country = iso3, year = year))
  }
}


# 4. Standardization function with initial recoding

standardize_df <- function(df, df_name) {
  
  library(dplyr)
  
  # Add country and year columns (assuming extract_metadata is defined elsewhere)
  version <- version_map[df_name]
  meta <- extract_metadata(df_name)
  df$country <- meta$country
  df$year <- meta$year
  
  # Handle sloppy naming: prioritize sex over c1, age over c3, pid over id/i4
  if ("sex" %in% names(df) && "c1" %in% names(df)) {
    df <- df %>% dplyr::select(-c1)
  } else if ("c1" %in% names(df) && !("sex" %in% names(df))) {
    df <- df %>% rename(sex = c1)
  }
  if ("age" %in% names(df) && "c3" %in% names(df)) {
    df <- df %>% dplyr::select(-c3)
  } else if ("c3" %in% names(df) && !("age" %in% names(df))) {
    df <- df %>% rename(age = c3)
  }
  if ("pid" %in% names(df)) {
    if ("id" %in% names(df)) df <- df %>% dplyr::select(-id)
    if ("i4" %in% names(df)) df <- df %>% dplyr::select(-i4)
  } else if ("id" %in% names(df) && !("pid" %in% names(df))) {
    df <- df %>% rename(pid = id)
    if ("i4" %in% names(df)) df <- df %>% dplyr::select(-i4)
  } else if ("i4" %in% names(df) && !("pid" %in% names(df)) && !("id" %in% names(df))) {
    df <- df %>% rename(pid = i4)
  } else if (!any(c("pid", "id", "i4") %in% names(df))) {
    df <- df %>% mutate(pid = NA_character_)
  }
  
  # Ensure pid is character and create unique_id
  df <- df %>%
    mutate(pid = as.character(pid),
           unique_id = paste(country, year, pid, sep = "_"))
  
  # Subset var_map to only columns present in df with valid mappings
  existing_cols <- intersect(names(df), names(var_map[[version]]))
  valid_cols <- existing_cols[!sapply(var_map[[version]][existing_cols], is.null)]
  if (length(valid_cols) < length(existing_cols)) {
    cat(sprintf("Warning: Dataset '%s' has unmapped columns in var_map: %s\n", 
                df_name, paste(setdiff(existing_cols, valid_cols), collapse = ", ")))
  }
  
  # Rename only columns with valid mappings
  if (length(valid_cols) > 0) {
    df <- df %>% rename_with(~ var_map[[version]][.], all_of(valid_cols))
  }
  
  # Define columns to convert to numeric
  numeric_cols <- c("age", "height", "weight", "heart_rate_1", "heart_rate_2", "heart_rate_3",
                    "sbp_1", "dbp_1", "sbp_2", "dbp_2", "sbp_3", "dbp_3", "fasting_glucose", "total_chol",
                    "u_cre","u_sod","trig","hdl","p1", "p2", "p3a", "p3b", "p4", "p5", "p6a", "p6b", "p7", "p8", "p9a", "p9b", "p10",
                    "p11", "p12a", "p12b", "p13", "p14", "p15a", "p15b", "fruit_freq", "fruit_serv_day",
                    "veg_freq", "veg_serv_day")
  numeric_cols_present <- intersect(numeric_cols, names(df))
  
  # Diagnostic check for non-numeric values
  for (col in numeric_cols_present) {
    original_values <- df[[col]]
    # Check if any values become NA when coerced to numeric, but weren't NA originally
    coerced <- suppressWarnings(as.numeric(original_values))
    if (any(is.na(coerced) & !is.na(original_values))) {
      problematic_values <- unique(original_values[is.na(coerced) & !is.na(original_values)])
      message("In dataframe '", df_name, "', column '", col, "' has non-numeric values: ",
              paste(problematic_values, collapse = ", "))
    }
  }
  
  # Define columns to convert to factors
  factor_cols <- c("sex", "education", "marital_status", "occupation", "smoking_status", "alcohol_status","hbp_diag","hbp_med","t2d_diag","insulin")
  factor_cols_present <- intersect(factor_cols, names(df))
  
  # Perform conversions and recoding
  df <- df %>%
    mutate(
      if ("sex" %in% names(df)) {
        sex = case_when(
          tolower(sex) %in% c("1", "male", "men") ~ "1",
          tolower(sex) %in% c("2", "female", "women") ~ "2",
          TRUE ~ NA_character_
        )
      },
      if ("education" %in% names(df)) {
        education = case_when(
          tolower(education) %in% c("1", "No formal schooling") ~ "1",
          tolower(education) %in% c("2", "Primary school incomplete") ~ "2",
          tolower(education) %in% c("3", "Primary school completed", "Secondary school incomplete") ~ "3",
          tolower(education) %in% c("4", "Secondary school completed", "A-Level completed") ~ "4",
          tolower(education) %in% c("6", "College/University completed") ~ "6",
          tolower(education) %in% c("7", "Post-graduate degree") ~ "7",
          TRUE ~ NA_character_
        )
      },
      if ("marital_status" %in% names(df)) {
        marital_status = case_when(
          tolower(marital_status) %in% c("1", "Never married") ~ "1",
          tolower(marital_status) %in% c("2", "Currently married") ~ "2",
          tolower(marital_status) %in% c("3", "Separated") ~ "3",
          tolower(marital_status) %in% c("4", "Divorced") ~ "4",
          tolower(marital_status) %in% c("5", "Widowed") ~ "5",
          tolower(marital_status) %in% c("6", "Cohabiting") ~ "6",
          TRUE ~ NA_character_
        )
      },
      if ("occupation" %in% names(df)) {
        occupation = case_when(
          as.numeric(occupation) %in% 1:9 ~ as.character(as.numeric(occupation)),
          TRUE ~ NA_character_
        )
      },
      if ("smoking_status" %in% names(df)) {
        smoking_status = case_when(
          tolower(smoking_status) %in% c("1", "smoker") ~ "1",
          tolower(smoking_status) %in% c("2", "non-smoker", "non smoker", "nonsmoker") ~ "2",
          tolower(smoking_status) == "0" ~ "2",
          TRUE ~ NA_character_
        )
      },
      if ("alcohol_status" %in% names(df)) {
        alcohol_status = case_when(
          tolower(alcohol_status) %in% c("1", "drinker") ~ "1",
          tolower(alcohol_status) %in% c("2", "non-drinker", "non drinker", "nondrinker") ~ "2",
          tolower(alcohol_status) == "0" ~ "2",
          TRUE ~ NA_character_
        )
      },
      across(all_of(factor_cols_present), as.factor),
      across(all_of(numeric_cols_present), as.numeric)
    )
  
  return(df)
}


# 5. Apply standardization
list_of_dfs <- lapply(names(list_of_dfs), function(nm) standardize_df(list_of_dfs[[nm]], nm))

# Restore the names after standardization
names(list_of_dfs) <- original_names


# Subset countries with versions 1.3 and 1.4
versions_1.3_1.4 <- c("1.3", "1.4")

# Get names of datasets with versions 1.3 or 1.4
datasets_1.3_1.4_names <- names(version_map)[version_map %in% versions_1.3_1.4]

# Create a new list with only those datasets
list_of_dfs_1.3_1.4 <- list_of_dfs[names(list_of_dfs) %in% datasets_1.3_1.4_names]

# Keep the remaining datasets in a separate list
list_of_dfs_other <- list_of_dfs[!names(list_of_dfs) %in% datasets_1.3_1.4_names]


# Define the mappings for versions 1.3 and 1.4
correct_mappings <- c(
  "t1" = "smoking_status",
  "a1" = "alcohol_status",
  "d1" = "fruit_freq",
  "d2" = "fruit_serv_day",
  "d3" = "veg_freq",
  "d4" = "veg_serv_day",
  "m16a" = "heart_rate_1",
  "m16b" = "heart_rate_2",
  "m16c" = "heart_rate_3"
)


# Function to rename columns if they exist in the dataframe
rename_columns <- function(df, mappings) {
  # Identify which columns from mappings exist in the dataframe
  existing_cols <- intersect(names(mappings), names(df))
  
  # Rename only the existing columns
  if (length(existing_cols) > 0) {
    df <- df %>% rename_with(~ mappings[.x], all_of(existing_cols))
  }
  
  return(df)
}

# Apply the renaming to each dataframe in list_of_dfs_1.3_1.4
list_of_dfs_1.3_1.4 <- lapply(list_of_dfs_1.3_1.4, function(df) {
  rename_columns(df, correct_mappings)
})


# Update list_of_dfs with the corrected versions from list_of_dfs_1.3_1.4
list_of_dfs[names(list_of_dfs_1.3_1.4)] <- list_of_dfs_1.3_1.4



# Define the list of variables you want to keep
relevant_columns <- c(
  "sex", "age", "education", "marital_status", "occupation",
  "smoking_status", "alcohol_status", "fruit_freq", "fruit_serv_day",
  "veg_freq", "veg_serv_day", "p1", "p2", "p3a", "p3b",
  "p4", "p5", "p6a", "p6b", "p7", "p8", "p9a", "p9b", "p10",
  "p11", "p12a", "p12b", "p13", "p14", "p15a", "p15b", "height",
  "weight", "heart_rate_1", "heart_rate_2", "heart_rate_3", "sbp_1",
  "dbp_1", "sbp_2", "dbp_2", "sbp_3", "dbp_3","b1", "fasting_glucose",
  "total_chol", "country", "unique_id", "year", "pid","trig","hdl","u_sod","u_cre","hbp_diag","hbp_med","t2d_diag","insulin"
)


select_relevant_columns <- function(df, columns) {
  # Load dplyr if not already loaded
  library(dplyr)
  
  # Keep only the columns present in both the dataframe and the relevant list
  present_columns <- intersect(columns, names(df))
  df_selected <- df[, present_columns, drop = FALSE]
  
  # Identify missing columns from the relevant list
  missing_columns <- setdiff(columns, present_columns)
  
  # Add missing columns with NA values (as character type)
  for (col in missing_columns) {
    df_selected[[col]] <- NA_character_  # Use NA_character_ for consistency
  }
  
  # Convert all columns to character type
  df_selected <- df_selected %>% mutate(across(everything(), as.character))
  
  return(df_selected)
}


list_of_dfs_selected <- lapply(list_of_dfs, select_relevant_columns, columns = relevant_columns)


#### Diagnostics ####

# Define variables and intended types
vars_to_check <- c("sex", "age", "education", "marital_status", "occupation",
                   "smoking_status", "alcohol_status", "fruit_freq", "fruit_serv_day",
                   "veg_freq", "veg_serv_day", "height", "weight","p1", "p2", "p3a", "p3b",
                   "p4", "p5", "p6a", "p6b", "p7", "p8", "p9a", "p9b", "p10",
                   "p11", "p12a", "p12b", "p13", "p14", "p15a", "p15b",
                   "heart_rate_1", "heart_rate_2", "heart_rate_3", "sbp_1", "dbp_1",
                   "sbp_2", "dbp_2", "sbp_3", "dbp_3", "fasting_glucose", "total_chol",
                   "trig","hdl","u_sod","u_cre","hbp_diag","hbp_med","t2d_diag","insulin")

intended_types <- list(
  numeric = c("age", "height", "weight", "heart_rate_1", "heart_rate_2", "heart_rate_3",
              "sbp_1", "dbp_1", "sbp_2", "dbp_2", "sbp_3", "dbp_3", "fasting_glucose",
              "total_chol","trig","hdl","u_sod","u_cre","p3a", "p3b",
              "p6a", "p6b", "p9a", "p9b", "p12a",
              "p12b", "p15a", "p15b", "fruit_serv_day", "veg_serv_day"),
  factor = c("sex", "education", "marital_status", "occupation", "smoking_status",
             "alcohol_status","fruit_freq","veg_freq","p1", "p2", "p4", "p5",
             "p7", "p8", "p10", "p11",
             "p13", "p14","hbp_diag","hbp_med","t2d_diag","insulin")
)


# Define plotting functions
plot_numeric <- function(df, var, country_year) {
  p1 <- ggplot(df, aes(x = .data[[var]])) +
    geom_histogram(bins = 30, fill = "blue", color = "black") +
    ggtitle(paste("Histogram of", var, "in", country_year)) +
    theme_minimal()
  list(hist = p1)  # Return only histogram in a list
}

plot_categorical <- function(df, var, country_year) {
  p <- ggplot(df, aes(x = .data[[var]])) +
    geom_bar(fill = "green", color = "black") +
    ggtitle(paste("Bar Plot of", var, "in", country_year)) +
    theme_minimal() +
    theme(axis.text.x = element_text(angle = 45, hjust = 1))
  p
}

# Define summary functions with redundant value check
summarize_numeric <- function(x) {
  tab <- sort(table(x, useNA = "ifany"), decreasing = TRUE)
  n <- length(x)
  most_frequent <- names(tab)[1]
  freq <- as.numeric(tab[1])
  prop <- freq / n  # Proportion is numeric by definition
  
  redundant_info <- list(
    MostFrequentValue = most_frequent,
    Frequency = freq,
    Proportion = prop
  )
  
  stats <- c(
    NAs = sum(is.na(x)),
    Min = min(x, na.rm = TRUE),
    Max = max(x, na.rm = TRUE),
    Mean = mean(x, na.rm = TRUE),
    Median = median(x, na.rm = TRUE),
    SD = sd(x, na.rm = TRUE),
    Unique = length(unique(x[!is.na(x)]))
  )
  
  list(stats = stats, redundant = redundant_info)
}

summarize_categorical <- function(x) {
  tab <- sort(table(x, useNA = "ifany"), decreasing = TRUE)
  n <- length(x)
  most_frequent <- names(tab)[1]
  freq <- as.numeric(tab[1])
  prop <- freq / n  # Proportion is numeric by definition
  
  redundant_info <- list(
    MostFrequentValue = most_frequent,
    Frequency = freq,
    Proportion = prop
  )
  
  list(
    NAs = sum(is.na(x)),
    Categories = tab,
    Redundant = redundant_info
  )
}

# Function to check for non-numeric values in character columns
check_non_numeric <- function(x) {
  if (is.character(x)) {
    non_numeric <- x[!grepl("^-?\\d*\\.?\\d+$", x) & !is.na(x)]
    unique_non_numeric <- unique(non_numeric)
    if (length(unique_non_numeric) > 0) {
      return(paste(unique_non_numeric, collapse = ", "))
    }
  }
  return(NULL)
}

# Main diagnostics loop
dir.create("diagnostics", showWarnings = FALSE)

for (df_name in names(list_of_dfs_selected)) {
  df <- list_of_dfs_selected[[df_name]]
  country_year <- df_name  # e.g., "afg2018"
  
  # Create subdirectory for this country/year
  dir_path <- file.path("diagnostics", country_year)
  dir.create(dir_path, showWarnings = FALSE)
  
  # Open a summary file
  summary_file <- file.path(dir_path, "summary.txt")
  sink(summary_file)
  
  cat("Diagnostics for", country_year, "\n\n")
  
  # Process each variable
  for (var in vars_to_check) {
    if (var %in% names(df)) {
      # Record the original class before conversion
      original_class <- class(df[[var]])
      cat("Variable:", var, "\n")
      cat("Original Class:", original_class, "\n")
      
      if (var %in% intended_types$numeric) {
        # Check for non-numeric values if the column is character
        if (is.character(df[[var]])) {
          non_numeric_values <- check_non_numeric(df[[var]])
          if (!is.null(non_numeric_values)) {
            cat("Non-numeric values found:", non_numeric_values, "\n")
          }
        }
        # Convert to numeric and count NAs
        df[[var]] <- as.numeric(df[[var]])
        na_count <- sum(is.na(df[[var]]))
        if (na_count > 0) {
          cat("NAs introduced when converting to numeric:", na_count, "\n")
          message(paste("In", country_year, "for", var, ":", na_count, "NAs introduced when converting to numeric."))
        }
        
        # Numeric variable diagnostics
        plots <- plot_numeric(df, var, country_year)
        ggsave(file.path(dir_path, paste0(var, "_hist.png")), plots$hist, width = 6, height = 4)
        
        summary_data <- summarize_numeric(df[[var]])
        summary_stats <- summary_data$stats
        redundant_stats <- summary_data$redundant
        
        cat("Summary Statistics:\n")
        cat(paste(names(summary_stats), round(summary_stats, 2), sep = ": ", collapse = "\n"), "\n")
        cat("Most Redundant Value Info:\n")
        cat("MostFrequentValue:", redundant_stats$MostFrequentValue, "\n")
        cat("Frequency:", redundant_stats$Frequency, "\n")
        cat("Proportion:", sprintf("%.3f", redundant_stats$Proportion), "\n")
        if (redundant_stats$Proportion > 0.5) {
          cat("Warning: Over 50% of values are", redundant_stats$MostFrequentValue, "\n")
        }
        cat("\n")
      } else if (var %in% intended_types$factor) {
        # Convert to factor
        df[[var]] <- as.factor(df[[var]])
        
        # Categorical variable diagnostics
        plot <- plot_categorical(df, var, country_year)
        ggsave(file.path(dir_path, paste0(var, "_bar.png")), plot, width = 6, height = 4)
        
        summary_data <- summarize_categorical(df[[var]])
        summary_stats <- summary_data$Categories
        redundant_stats <- summary_data$Redundant
        
        cat("NAs:", summary_data$NAs, "\n")
        cat("Categories:\n")
        print(summary_stats)
        cat("Most Redundant Value Info:\n")
        cat("MostFrequentValue:", redundant_stats$MostFrequentValue, "\n")
        cat("Frequency:", redundant_stats$Frequency, "\n")
        cat("Proportion:", sprintf("%.3f", redundant_stats$Proportion), "\n")
        if (redundant_stats$Proportion > 0.5) {
          cat("Warning: Over 50% of values are", redundant_stats$MostFrequentValue, "\n")
        }
        cat("\n")
      }
    }
  }
  
  sink()  # Close the summary file
}

#### More Cleaning and Winsorizing ####

combined_df <- bind_rows(list_of_dfs_selected)

# Define numeric columns
numeric_cols <- c("age", "height", "weight", "heart_rate_1", "heart_rate_2", "heart_rate_3",
                  "sbp_1", "dbp_1", "sbp_2", "dbp_2", "sbp_3", "dbp_3", "fasting_glucose", "total_chol",
                  "u_cre","u_sod","trig","hdl","p1", "p2", "p3a", "p3b", "p4", "p5", "p6a", "p6b", "p7", "p8", "p9a", "p9b", "p10",
                  "p11", "p12a", "p12b", "p13", "p14", "p15a", "p15b", "fruit_freq", "fruit_serv_day",
                  "veg_freq", "veg_serv_day")

# Convert to numeric
combined_df <- combined_df %>% mutate(across(all_of(numeric_cols), as.numeric))

# Define categorical columns
factor_cols <- c("sex", "education", "marital_status", "occupation", "smoking_status", "alcohol_status","hbp_diag","hbp_med","t2d_diag","insulin","b1")

# Convert to factor
combined_df <- combined_df %>% mutate(across(all_of(factor_cols), as.factor))



# Convert country codes to country names
combined_df$new_country <- countrycode(combined_df$country, "iso3c", "country.name")


# Add iso3_code by converting new_country (country names) to ISO3 codes
combined_df <- combined_df %>%
  mutate(iso3_code = countrycode(new_country, "country.name", "iso3c"))


# Rearrange columns to have 'country', 'year', 'unique_id' first
combined_df <- combined_df %>% dplyr::select(new_country,iso3_code,country, year, unique_id, everything())


all_df <- combined_df


# Clean sex and age
all_df <- all_df %>%
  mutate(
    sex = case_when(
      # Male (1)
      sex %in% c("1", "men", "Men") ~ "1",
      # Female (2)
      sex %in% c("2", "0", "women", "Women") ~ "2",
      # Anything else becomes NA
      TRUE ~ NA_character_
    ),
    sex = factor(sex, levels = c("1", "2")),
    # Clean age: set values < 15 to NA
    age = ifelse(age < 15, NA, age)
  )

##Cleaning categoricals and adding missing category

all_df <- all_df %>%
  mutate(
    # Education: 1, 2, 3, 4, 6, 7 kept, others as "missing_unknown"
    education = case_when(
      tolower(education) %in% c("1", "no formal schooling") ~ "1",
      tolower(education) %in% c("2", "primary school incomplete") ~ "2",
      tolower(education) %in% c("3", "primary school completed", "secondary school incomplete") ~ "3",
      tolower(education) %in% c("4", "secondary school completed", "a-level completed") ~ "4",
      tolower(education) %in% c("6", "college/university completed") ~ "6",
      tolower(education) %in% c("7", "post-graduate degree") ~ "7",
      TRUE ~ "missing_unknown"
    ),
    education = factor(education, levels = c("1", "2", "3", "4", "6", "7", "missing_unknown")),
    
    # Marital Status: 1-6 kept, 7, 8, 88, 99, Inf, and NA as "missing_unknown"
    marital_status = case_when(
      tolower(marital_status) %in% c("1", "never married") ~ "1",
      tolower(marital_status) %in% c("2", "currently married") ~ "2",
      tolower(marital_status) %in% c("3", "separated") ~ "3",
      tolower(marital_status) %in% c("4", "divorced") ~ "4",
      tolower(marital_status) %in% c("5", "widowed") ~ "5",
      tolower(marital_status) %in% c("6", "cohabiting") ~ "6",
      as.character(marital_status) %in% as.character(c(7, 8, 88, 99)) | is.infinite(marital_status) ~ "missing_unknown",
      TRUE ~ "missing_unknown"  # Updated to include NA
    ),
    marital_status = factor(marital_status, levels = c("1", "2", "3", "4", "5", "6", "missing_unknown")),
    
    # Occupation: 1-9 kept, all others as "missing_unknown"
    occupation = case_when(
      as.character(occupation) %in% as.character(1:9) ~ as.character(occupation),
      TRUE ~ "missing_unknown"
    ),
    occupation = factor(occupation, levels = c(as.character(1:9), "missing_unknown")),
    
    # Smoking Status: 1-2 kept, 0, 3-7, 77, 8, 9, 99, Inf, and NA as "missing_unknown"
    smoking_status = case_when(
      tolower(smoking_status) %in% c("1", "smoker") ~ "1",
      tolower(smoking_status) %in% c("2", "non-smoker", "non smoker", "nonsmoker", "0") ~ "2",
      as.character(smoking_status) %in% as.character(c(0, 3:7, 77, 8, 9, 99)) | is.infinite(smoking_status) ~ "missing_unknown",
      TRUE ~ "missing_unknown"  # Updated to include NA
    ),
    smoking_status = factor(smoking_status, levels = c("1", "2", "missing_unknown")),
    
    # Alcohol Status: 1-2 kept, 0, 3, 7, 77, 8, 88, 9, 99, Inf, and NA as "missing_unknown"
    alcohol_status = case_when(
      tolower(alcohol_status) %in% c("1", "drinker") ~ "1",
      tolower(alcohol_status) %in% c("2", "non-drinker", "non drinker", "nondrinker", "0") ~ "2",
      as.character(alcohol_status) %in% as.character(c(0, 3, 7, 77, 8, 88, 9, 99)) | is.infinite(alcohol_status) ~ "missing_unknown",
      TRUE ~ "missing_unknown"  # Updated to include NA
    ),
    alcohol_status = factor(alcohol_status, levels = c("1", "2", "missing_unknown")),
    
    # p1, p4, p7, p10, p13: Keep 1 and 2, others to "missing_unknown"
    p1 = case_when(
      as.character(p1) %in% c("1", "2") ~ as.character(p1),
      TRUE ~ "missing_unknown"
    ),
    p1 = factor(p1, levels = c("1", "2", "missing_unknown")),
    
    p4 = case_when(
      as.character(p4) %in% c("1", "2") ~ as.character(p4),
      TRUE ~ "missing_unknown"
    ),
    p4 = factor(p4, levels = c("1", "2", "missing_unknown")),
    
    p7 = case_when(
      as.character(p7) %in% c("1", "2") ~ as.character(p7),
      TRUE ~ "missing_unknown"
    ),
    p7 = factor(p7, levels = c("1", "2", "missing_unknown")),
    
    p10 = case_when(
      as.character(p10) %in% c("1", "2") ~ as.character(p10),
      TRUE ~ "missing_unknown"
    ),
    p10 = factor(p10, levels = c("1", "2", "missing_unknown")),
    
    p13 = case_when(
      as.character(p13) %in% c("1", "2") ~ as.character(p13),
      TRUE ~ "missing_unknown"
    ),
    p13 = factor(p13, levels = c("1", "2", "missing_unknown")),
    
    # p2, p5, p8, p11, p14: Keep 0 to 7, others to "missing_unknown"
    p2 = case_when(
      as.character(p2) %in% as.character(0:7) ~ as.character(p2),
      TRUE ~ "missing_unknown"
    ),
    p2 = factor(p2, levels = c(as.character(0:7), "missing_unknown")),
    
    p5 = case_when(
      as.character(p5) %in% c("0", "1", "2", "3", "4", "5", "6", "7") ~ as.character(p5),
      TRUE ~ "missing_unknown"
    ),
    p5 = factor(p5, levels = c(as.character(0:7), "missing_unknown")),
    
    p8 = case_when(
      as.character(p8) %in% as.character(0:7) ~ as.character(p8),
      TRUE ~ "missing_unknown"
    ),
    p8 = factor(p8, levels = c(as.character(0:7), "missing_unknown")),
    
    p11 = case_when(
      as.character(p11) %in% as.character(0:7) ~ as.character(p11),
      TRUE ~ "missing_unknown"
    ),
    p11 = factor(p11, levels = c(as.character(0:7), "missing_unknown")),
    
    p14 = case_when(
      as.character(p14) %in% as.character(0:7) ~ as.character(p14),
      TRUE ~ "missing_unknown"
    ),
    p14 = factor(p14, levels = c(as.character(0:7), "missing_unknown")),
    
    # p3a: Keep 0 to 18, others to "missing_unknown" (assuming p6a, p9a, p12a, p15a are similar)
    p3a = case_when(
      as.character(p3a) %in% as.character(0:18) ~ as.character(p3a),
      TRUE ~ "missing_unknown"
    ),
    p3a = factor(p3a, levels = c(as.character(0:18), "missing_unknown")),
    
    # p3b: Keep 0 to 60, others to "missing_unknown" (assuming p6b, p9b, p12b, p15b are similar)
    p3b = case_when(
      as.character(p3b) %in% as.character(0:60) ~ as.character(p3b),
      TRUE ~ "missing_unknown"
    ),
    p3b = factor(p3b, levels = c(as.character(0:60), "missing_unknown")),
    
    # fruit_freq, veg_freq: Keep 0 to 7, others to "missing_unknown"
    fruit_freq = case_when(
      as.character(fruit_freq) %in% as.character(0:7) ~ as.character(fruit_freq),
      TRUE ~ "missing_unknown"
    ),
    fruit_freq = factor(fruit_freq, levels = c(as.character(0:7), "missing_unknown")),
    
    veg_freq = case_when(
      as.character(veg_freq) %in% as.character(0:7) ~ as.character(veg_freq),
      TRUE ~ "missing_unknown"
    ),
    veg_freq = factor(veg_freq, levels = c(as.character(0:7), "missing_unknown")),
    
    # fruit_serv_day, veg_serv_day: Keep 0 to 15 (including decimals), others to "missing_unknown"
    fruit_serv_day = case_when(
      as.numeric(fruit_serv_day) >= 0 & as.numeric(fruit_serv_day) <= 15 ~ as.character(fruit_serv_day),
      TRUE ~ "missing_unknown"
    ),
    fruit_serv_day = factor(fruit_serv_day, levels = c(as.character(unique(fruit_serv_day[fruit_serv_day != "missing_unknown"])), "missing_unknown")),
    
    veg_serv_day = case_when(
      as.numeric(veg_serv_day) >= 0 & as.numeric(veg_serv_day) <= 15 ~ as.character(veg_serv_day),
      TRUE ~ "missing_unknown"
    ),
    veg_serv_day = factor(veg_serv_day, levels = c(as.character(unique(veg_serv_day[veg_serv_day != "missing_unknown"])), "missing_unknown"))
  )



# Define the specific implausible values to filter out
implausible_values <- c(666, 666.6, 666.66, 77,777, 777.7, 777.77, 888, 888.8, 888.88,99.99, 999, 999.9, 999.99)

# Define plausible upper bounds
max_plausible_height <- 230  
max_plausible_weight <- 250 

# Filter and winsorize with percentile bounds, but respect plausible maximums
all_df <- all_df %>%
  mutate(
    height = case_when(
      height %in% implausible_values | is.infinite(height) | height > 500 ~ NA_real_,
      TRUE ~ height
    ),
    height = case_when(
      is.na(height) ~ NA_real_,  # Keep NA as NA
      height < quantile(height, 0.01, na.rm = TRUE) ~ quantile(height, 0.01, na.rm = TRUE),
      height > quantile(height, 0.99, na.rm = TRUE) & height <= max_plausible_height ~ quantile(height, 0.99, na.rm = TRUE),
      height > max_plausible_height ~ max_plausible_height,  # Cap at 230 cm
      TRUE ~ height
    ),
    weight = case_when(
      weight %in% implausible_values | is.infinite(weight) | weight > 500 ~ NA_real_,
      TRUE ~ weight
    ),
    weight = case_when(
      is.na(weight) ~ NA_real_,
      weight < quantile(weight, 0.01, na.rm = TRUE) ~ quantile(weight, 0.01, na.rm = TRUE),
      weight > quantile(weight, 0.99, na.rm = TRUE) & weight <= max_plausible_weight ~ quantile(weight, 0.99, na.rm = TRUE),
      weight > max_plausible_weight ~ max_plausible_weight,  # Cap at 250 kg
      TRUE ~ weight
    )
  )


# Define BMI bounds
min_plausible_bmi <- 12  # Minimum plausible BMI
max_plausible_bmi <- 60  # Maximum plausible BMI

# Calculate BMI and apply bounds
all_df <- all_df %>%
  mutate(
    height_m = height / 100,  # Convert height to meters
    BMI = weight / (height_m^2),  # Initial BMI calculation
    weight = case_when(
      is.na(BMI) ~ weight,  # If BMI is NA, keep weight as is
      BMI < min_plausible_bmi ~ min_plausible_bmi * (height_m^2),  # Adjust weight up to BMI = 12
      BMI > max_plausible_bmi ~ max_plausible_bmi * (height_m^2),  # Adjust weight down to BMI = 60
      TRUE ~ weight
    ),
    BMI = weight / (height_m^2)  # Recalculate BMI with adjusted weight
  )

# Create "obese" variable
all_df$obese <- ifelse(all_df$BMI > 30, 1, 2)
all_df$obese <- as.factor(all_df$obese)



# Convert factor variables to numeric, treating "missing_unknown" as NA
all_df <- all_df %>%
  mutate(
    across(c("p3a", "p3b", "p2", "p6a", "p6b", "p5", "p9a", "p9b", "p8", 
             "p12a", "p12b", "p11", "p15a", "p15b", "p14"), 
           ~ as.numeric(ifelse(. == "missing_unknown", NA, as.character(.))))
  )

# Vigorous work activity
all_df$vig_work <- ifelse(all_df$p1 == "1",
                          (all_df$p3a * 60 + all_df$p3b) * all_df$p2,
                          ifelse(all_df$p1 == "2", 0, NA))

# Moderate work activity
all_df$mod_work <- ifelse(all_df$p4 == "1",
                          (all_df$p6a * 60 + all_df$p6b) * all_df$p5,
                          ifelse(all_df$p4 == "2", 0, NA))

# Travel activity
all_df$travel <- ifelse(all_df$p7 == "1",
                        (all_df$p9a * 60 + all_df$p9b) * all_df$p8,
                        ifelse(all_df$p7 == "2", 0, NA))

# Vigorous leisure activity
all_df$vig_leisure <- ifelse(all_df$p10 == "1",
                             (all_df$p12a * 60 + all_df$p12b) * all_df$p11,
                             ifelse(all_df$p10 == "2", 0, NA))

# Moderate leisure activity
all_df$mod_leisure <- ifelse(all_df$p13 == "1",
                             (all_df$p15a * 60 + all_df$p15b) * all_df$p14,
                             ifelse(all_df$p13 == "2", 0, NA))

# Intermediate aggregates with NA preservation
all_df$omvpa <- rowSums(cbind(all_df$vig_work, all_df$mod_work), na.rm = TRUE)
all_df$omvpa <- ifelse(is.na(all_df$vig_work) & is.na(all_df$mod_work), NA_real_, all_df$omvpa)
all_df$ltmvpa <- rowSums(cbind(all_df$vig_leisure, all_df$mod_leisure), na.rm = TRUE)
all_df$ltmvpa <- ifelse(is.na(all_df$vig_leisure) & is.na(all_df$mod_leisure), NA_real_, all_df$ltmvpa)
all_df$nonocpa <- rowSums(cbind(all_df$travel, all_df$ltmvpa), na.rm = TRUE)
all_df$nonocpa <- ifelse(is.na(all_df$travel) & is.na(all_df$ltmvpa), NA_real_, all_df$nonocpa)

# Total MVPA
all_df$total_mvpa <- rowSums(cbind(all_df$omvpa, all_df$nonocpa), na.rm = TRUE)
all_df$total_mvpa <- ifelse(is.na(all_df$omvpa) & is.na(all_df$nonocpa), NA_real_, all_df$total_mvpa)


# Create new variable based on PA criteria (150 minutes/week threshold)
all_df$meet_pa <- ifelse(all_df$total_mvpa < 150, 0, 1)

all_df$active <- ifelse(all_df$total_mvpa == 0, 0, 1)
all_df$active <- as.factor(all_df$active)
all_df$active_nonocpa <- ifelse(all_df$nonocpa > 0, 1, 0)
all_df$active_omvpa <- ifelse(all_df$omvpa > 0, 1, 0)
all_df$active_omvpa <- as.factor(all_df$active_omvpa)
all_df$active_nonocpa <- as.factor(all_df$active_nonocpa)

small_constant <- 1e-6
all_df$log_ratio_omvpa <- log((all_df$omvpa + small_constant) / (all_df$total_mvpa + small_constant))



# Convert variables variables to numeric, treating "missing_unknown" as NA
all_df <- all_df %>%
  mutate(
    across(c("fruit_freq", "fruit_serv_day", "veg_freq", "veg_serv_day"), 
           ~ as.numeric(ifelse(. == "missing_unknown", NA, as.character(.))))
  )

# Calculate total servings of fruits and veggies per week
all_df$total_servings_week <- (all_df$fruit_freq * all_df$fruit_serv_day) + 
  (all_df$veg_freq * all_df$veg_serv_day)

# Calculate average servings per day
all_df$avg_servings_day <- all_df$total_servings_week / 7

# Create meet_eat variable (1 if >= 5 servings/day, 2 if < 5, "missing_unknown" if NA)
all_df <- all_df %>%
  mutate(
    meet_eat = case_when(
      is.na(avg_servings_day) ~ "missing_unknown",
      avg_servings_day >= 5 ~ "1",
      avg_servings_day < 5 ~ "2"
    ),
    meet_eat = factor(meet_eat, levels = c("1", "2", "missing_unknown"))
  )

### 11. Population

population <- read.xlsx("2022_population.xlsx")

population$new_country <- countrycode(population$Country.Code, "iso3c", "country.name")

names(population)[2] <- "population_2022"

population <-population[,c(2,3)]

all_df <- merge(all_df,population,by="new_country",all.x=TRUE)

# Update population_2022 values for specific countries World Factbook
all_df$population_2022 <- ifelse(all_df$new_country == "Niue", 2000,
                                 ifelse(all_df$new_country == "Tokelau", 1647,
                                        ifelse(all_df$new_country == "Cook Islands", 7761,
                                               ifelse(all_df$new_country == "Wallis & Futuna", 15964,
                                                      all_df$population_2022))))

## 12. Add World Bank Income Classification ##

world_bank_income <- read.xlsx("world-bank-income-groups.xlsx")

world_bank_income <- world_bank_income %>%
  filter(Year=="2022")

world_bank_income$new_country <- countrycode(world_bank_income$Code, "iso3c", "country.name")

names(world_bank_income)[4] <- "country_class"

world_bank_income <- world_bank_income[,c(4,5)]

missing_countries_df <- data.frame(
  new_country = c("Cook Islands", "Niue", "Tokelau", "Wallis & Futuna"),
  country_class = rep("Low-income countries", 4)
)

# Add the missing countries to the 'world_bank_income' dataframe
world_bank_income <- rbind(world_bank_income, missing_countries_df)

all_df <- merge(all_df,world_bank_income)

### 13. Regions ###


# Define regions using ISO3 codes
east_europe_west_asia <- c("MDA", "GEO", "ARM", "AZE", "BLR", "UKR")
central_east_asia <- c("MNG", "AFG", "TKM", "KGZ", "TJK")
south_southeast_asia <- c("LKA", "BGD", "BRN", "MMR", "NPL", "KHM", "VNM", "LAO", "MDV", "PAK", "TLS")
sub_saharan_africa <- c("CAF", "CPV", "LSO", "KEN", "TZA", "ZMB", "STP", "GIN", "UGA", "ERI", "MOZ", 
                        "ETH", "BEN", "SWZ", "CMR", "GHA", "LBR", "MDG", "MWI", "MLI", "MRT", "NER", 
                        "BWA", "RWA", "SLE", "TGO", "TCD", "COM", "COD", "COG", "CIV", "GMB", "GAB")
latin_south_america <- c("LCA", "ECU", "GRD", "CYM", "BRB", "GUY", "BOL", "VGB", "URY", "BHS")
oceania <- c("SLB", "COK", "KIR", "MHL", "FSM", "NRU", "TON", "TUV", "ASM", "PYF", "NIU", "PLW", 
             "WSM", "SYC", "TKL", "VUT", "WLF", "FJI")
mena <- c("DZA", "PSE", "IRQ", "KWT", "JOR", "LBN", "LBY", "MAR", "QAT", "SDN")

# Mutate region based on iso3_code
all_df <- all_df %>% 
  mutate(
    region = case_when(
      iso3_code %in% east_europe_west_asia ~ "East Europe and West Asia",
      iso3_code %in% central_east_asia ~ "Central and East Asia",
      iso3_code %in% south_southeast_asia ~ "South and Southeast Asia",
      iso3_code %in% sub_saharan_africa ~ "Sub-Saharan Africa",
      iso3_code %in% latin_south_america ~ "Latin and South America",
      iso3_code %in% oceania ~ "Small Island Nations",
      iso3_code %in% mena ~ "MENA",
      TRUE ~ "Unknown"
    )
  )



all_df$weight_class <- cut(all_df$BMI, breaks = c(-Inf, 18.5, 24.9, 29.9, Inf),
                           labels = c("1", "2", "3", "4"), include.lowest = TRUE)

all_df$weight_class <- as.factor(all_df$weight_class)


# Replace all Inf with NA
all_df <- all_df %>%
  mutate(
    # Handle numeric columns (Inf/-Inf to NA)
    across(where(is.numeric), ~ replace(., is.infinite(.), NA)),
    # Handle character columns ("inf" to NA_character_)
    across(where(is.character), ~ replace(., tolower(.) == "inf", NA_character_))
  )



# Define implausible values
implausible_values <- c(666, 666.6, 666.66, 77, 777, 777.7, 777.77, 888, 888.8, 888.88, 99.99, 999, 999.9, 999.99)

# Ensure numeric conversion for all relevant columns
numeric_cols <- c("height", "weight", "fasting_glucose", "total_chol",
                  "sbp_1", "sbp_2", "sbp_3", "dbp_1", "dbp_2", "dbp_3")
all_df <- all_df %>%
  mutate(across(all_of(numeric_cols), as.numeric))

# Function to replace implausible values with NA (with tolerance for floating-point)
replace_implausible <- function(x, implausible_vals) {
  case_when(
    # Use near() for floating-point comparison with tolerance
    sapply(implausible_vals, function(val) near(x, val, tol = 0.01)) %>% rowSums() > 0 |
      is.infinite(x) ~ NA_real_,
    TRUE ~ x
  )
}



# Apply to fasting_glucose and total_chol with additional checks
all_df <- all_df %>%
  mutate(
    fasting_glucose = replace_implausible(fasting_glucose, implausible_values),
    fasting_glucose = case_when(
      is.na(fasting_glucose) | fasting_glucose > 500 | fasting_glucose < 0 ~ NA_real_,
      TRUE ~ fasting_glucose
    ),
    total_chol = replace_implausible(total_chol, implausible_values),
    total_chol = case_when(
      is.na(total_chol) | total_chol > 500 | total_chol < 0 ~ NA_real_,
      TRUE ~ total_chol
    ),
    # Unit conversions
    fasting_glucose_mmol = case_when(
      is.na(fasting_glucose) ~ NA_real_,
      fasting_glucose > 20 ~ fasting_glucose * 0.0555,
      fasting_glucose >= 1.1 & fasting_glucose <= 20 ~ fasting_glucose,
      TRUE ~ NA_real_
    ),
    total_chol_mmol = case_when(
      is.na(total_chol) ~ NA_real_,
      total_chol > 40 ~ total_chol * 0.02586,
      total_chol >= 1.0 & total_chol <= 12.93 ~ total_chol,
      TRUE ~ NA_real_
    ),
    fasting_glucose_ll = case_when(
      fasting_glucose_mmol > 3 ~ fasting_glucose_mmol,
      fasting_glucose_mmol == 3 ~ 0.1,
      TRUE ~ NA_real_
    ),
    fasting_glucose_ul = case_when(
      fasting_glucose_mmol < 11 ~ fasting_glucose_mmol,
      TRUE ~ NA_real_
    ),
    total_chol_ll = case_when(
      total_chol_mmol > 1 ~ total_chol_mmol,
      total_chol_mmol == 1 ~ 0.1,
      TRUE ~ NA_real_
    ),
    total_chol_ul = total_chol_mmol
  )

# Apply to height and weight
all_df <- all_df %>%
  mutate(
    height = replace_implausible(height, implausible_values),
    height = case_when(
      is.na(height) | height < 50 | height > 250 ~ NA_real_,  # Add range check
      TRUE ~ height
    ),
    weight = replace_implausible(weight, implausible_values),
    weight = case_when(
      is.na(weight) | weight < 20 | weight > 300 ~ NA_real_,  # Add range check
      TRUE ~ weight
    )
  )

# Apply to SBP and DBP
for (i in 1:3) {
  sbp_col <- paste0("sbp_", i)
  dbp_col <- paste0("dbp_", i)
  all_df <- all_df %>%
    mutate(
      !!sbp_col := replace_implausible(!!sym(sbp_col), implausible_values),
      !!sbp_col := case_when(
        is.na(!!sym(sbp_col)) | !!sym(sbp_col) > 500 |
          !!sym(sbp_col) > 180 & !!sym(dbp_col) > 90 |
          !!sym(sbp_col) > 190 | !!sym(sbp_col) < 70 |
          !!sym(dbp_col) > 110 | !!sym(dbp_col) < 40 |
          !!sym(sbp_col) < !!sym(dbp_col) |
          (!!sym(sbp_col) - !!sym(dbp_col)) > 120 | (!!sym(sbp_col) - !!sym(dbp_col)) < 20 ~ NA_real_,
        TRUE ~ !!sym(sbp_col)
      ),
      !!dbp_col := replace_implausible(!!sym(dbp_col), implausible_values),
      !!dbp_col := case_when(
        is.na(!!sym(dbp_col)) | !!sym(dbp_col) > 500 |
          !!sym(sbp_col) > 180 & !!sym(dbp_col) > 90 |
          !!sym(sbp_col) > 190 | !!sym(sbp_col) < 70 |
          !!sym(dbp_col) > 110 | !!sym(dbp_col) < 40 |
          !!sym(sbp_col) < !!sym(dbp_col) |
          (!!sym(sbp_col) - !!sym(dbp_col)) > 120 | (!!sym(sbp_col) - !!sym(dbp_col)) < 20 ~ NA_real_,
        TRUE ~ !!sym(dbp_col)
      ),
      !!paste0("sbp_", i, "_ll") := case_when(
        is.na(!!sym(sbp_col)) | !!sym(sbp_col) == 70 | !!sym(sbp_col) == 190 ~ 70,
        TRUE ~ !!sym(sbp_col)
      ),
      !!paste0("sbp_", i, "_ul") := case_when(
        is.na(!!sym(sbp_col)) | !!sym(sbp_col) == 70 | !!sym(sbp_col) == 190 ~ 190,
        TRUE ~ !!sym(sbp_col)
      ),
      !!paste0("dbp_", i, "_ll") := case_when(
        is.na(!!sym(dbp_col)) | !!sym(dbp_col) == 40 | !!sym(dbp_col) == 110 ~ 40,
        TRUE ~ !!sym(dbp_col)
      ),
      !!paste0("dbp_", i, "_ul") := case_when(
        is.na(!!sym(dbp_col)) | !!sym(dbp_col) == 40 | !!sym(dbp_col) == 110 ~ 110,
        TRUE ~ !!sym(dbp_col)
      )
    )
}


# Heart Rate (beats/min) - Apply to all 3 measurements, lower bound changed to 30
for (i in 1:3) {
  hr_col <- paste0("heart_rate_", i)
  all_df <- all_df %>%
    mutate(
      !!hr_col := case_when(
        !!sym(hr_col) %in% implausible_values | is.infinite(!!sym(hr_col)) | !!sym(hr_col) > 500 ~ NA_real_,
        !!sym(hr_col) > 130 ~ NA_real_,
        !!sym(hr_col) < 30 ~ NA_real_,  # Updated from 35 to 30
        TRUE ~ !!sym(hr_col)
      ),
      !!paste0("heart_rate_", i, "_ll") := case_when(
        is.na(!!sym(hr_col)) | !!sym(hr_col) == 30 | !!sym(hr_col) == 130 ~ 30,  # Updated from 35 to 30
        TRUE ~ !!sym(hr_col)
      ),
      !!paste0("heart_rate_", i, "_ul") := case_when(
        is.na(!!sym(hr_col)) | !!sym(hr_col) == 30 | !!sym(hr_col) == 130 ~ 130,  # Updated from 35 to 30
        TRUE ~ !!sym(hr_col)
      )
    )
}

all_df$total_chol <- all_df$total_chol_mmol

all_df$fasting_glucose <- all_df$fasting_glucose_mmol


##Choose lowest plausible value for sbp, dbp, and hr

all_df <- all_df %>%
  mutate(
    # Step 1: Find minimum SBP index, but only among valid pairs
    valid_sbp_dbp_1 = if_else(!is.na(sbp_1) & !is.na(dbp_1), sbp_1, NA_real_),
    valid_sbp_dbp_2 = if_else(!is.na(sbp_2) & !is.na(dbp_2), sbp_2, NA_real_),
    valid_sbp_dbp_3 = if_else(!is.na(sbp_3) & !is.na(dbp_3), sbp_3, NA_real_),
    min_sbp_index = pmin(valid_sbp_dbp_1, valid_sbp_dbp_2, valid_sbp_dbp_3, na.rm = TRUE),
    # Step 2: Assign SBP and DBP based on valid minimum
    sbp = case_when(
      is.na(valid_sbp_dbp_1) & is.na(valid_sbp_dbp_2) & is.na(valid_sbp_dbp_3) ~ NA_real_,
      min_sbp_index == valid_sbp_dbp_1 ~ sbp_1,
      min_sbp_index == valid_sbp_dbp_2 ~ sbp_2,
      min_sbp_index == valid_sbp_dbp_3 ~ sbp_3,
      TRUE ~ NA_real_
    ),
    dbp = case_when(
      is.na(valid_sbp_dbp_1) & is.na(valid_sbp_dbp_2) & is.na(valid_sbp_dbp_3) ~ NA_real_,
      min_sbp_index == valid_sbp_dbp_1 ~ dbp_1,
      min_sbp_index == valid_sbp_dbp_2 ~ dbp_2,
      min_sbp_index == valid_sbp_dbp_3 ~ dbp_3,
      TRUE ~ NA_real_
    ),
    # HR 
    hr = case_when(
      is.na(heart_rate_1) & is.na(heart_rate_2) & is.na(heart_rate_3) ~ NA_real_,
      TRUE ~ pmin(heart_rate_1, heart_rate_2, heart_rate_3, na.rm = TRUE)
    )
  ) %>%
  dplyr::select(-min_sbp_index, -valid_sbp_dbp_1, -valid_sbp_dbp_2, -valid_sbp_dbp_3)  # Drop temporary columns

##Fasting or not fasted? 

# Step 1: Recode b1
all_df <- all_df %>%
  mutate(
    b1 = case_when(
      b1 %in% c(1, 2) ~ as.character(b1),  # Keep 1 and 2
      TRUE ~ "missing_unknown"             # All others to missing_unknown
    ),
    b1 = factor(b1, levels = c("1", "2", "missing_unknown"))
  )


pa_components <- c("omvpa", "travel", "ltmvpa")
pa_derived <- c("nonocpa", "total_mvpa")
pa_vars <- c(pa_components, pa_derived)

all_df <- all_df %>%
  mutate(
    # Filter extreme values in sub-components first
    across(c("vig_work", "mod_work", "travel", "vig_leisure", "mod_leisure"),
           ~ case_when(
             is.na(.) ~ NA_real_,
             . > 8000 ~ NA_real_,  # Cap at 8000 minutes/week
             TRUE ~ .
           )),
    # Recalculate aggregates with NA preservation
    omvpa = rowSums(cbind(vig_work, mod_work), na.rm = TRUE),
    omvpa = ifelse(is.na(vig_work) & is.na(mod_work), NA_real_, omvpa),
    ltmvpa = rowSums(cbind(vig_leisure, mod_leisure), na.rm = TRUE),
    ltmvpa = ifelse(is.na(vig_leisure) & is.na(mod_leisure), NA_real_, ltmvpa),
    nonocpa = rowSums(cbind(travel, ltmvpa), na.rm = TRUE),
    nonocpa = ifelse(is.na(travel) & is.na(ltmvpa), NA_real_, nonocpa),
    total_mvpa = rowSums(cbind(omvpa, nonocpa), na.rm = TRUE),
    total_mvpa = ifelse(is.na(omvpa) & is.na(nonocpa), NA_real_, total_mvpa),
    # Winsorize all PA variables
    across(all_of(pa_vars),
           ~ case_when(
             is.na(.) ~ NA_real_,
             . < 0 ~ 0,
             . > quantile(., 0.99, na.rm = TRUE) ~ quantile(., 0.99, na.rm = TRUE),
             TRUE ~ .
           ))
  )


all_df <- all_df[,c(1,2,4:110)]

names(all_df)[1] <- "country"


# Define Asian countries with Asia-Pacific classification
asian_countries <- c("AFG", "BGD", "BRN", "KHM", "LAO", "MDV", "MNG", "MMR", "NPL", "PAK", "LKA", "VNM", "KGZ", "TJK", "TKM")

# Create both BMI_mixed and BMI_group variables with missing_unknown
all_df <- all_df %>%
  mutate(
    # BMI_group: WHO classification for all countries
    BMI_group = case_when(
      is.na(BMI) ~ "missing_unknown",
      BMI < 18.5 ~ "Underweight",
      BMI >= 18.5 & BMI < 25 ~ "Normal",
      BMI >= 25 & BMI < 30 ~ "Overweight",
      BMI >= 30 ~ "Obese",
      TRUE ~ "missing_unknown"  # Catch any other unexpected cases
    ),
    # BMI_mixed: Asia-Pacific for Asian countries, WHO for others
    BMI_mixed = case_when(
      is.na(BMI) ~ "missing_unknown",
      iso3_code %in% asian_countries ~ case_when(
        BMI < 18.5 ~ "Underweight",
        BMI >= 18.5 & BMI < 23 ~ "Normal",
        BMI >= 23 & BMI < 25 ~ "Overweight",
        BMI >= 25 ~ "Obese",
        TRUE ~ "missing_unknown"  # Shouldn't occur, but included for consistency
      ),
      TRUE ~ case_when(
        BMI < 18.5 ~ "Underweight",
        BMI >= 18.5 & BMI < 25 ~ "Normal",
        BMI >= 25 & BMI < 30 ~ "Overweight",
        BMI >= 30 ~ "Obese",
        TRUE ~ "missing_unknown"  # Shouldn't occur, but included for consistency
      )
    ),
    # Convert both to factors with specified levels including missing_unknown
    BMI_group = factor(BMI_group, levels = c("Underweight", "Normal", "Overweight", "Obese", "missing_unknown")),
    BMI_mixed = factor(BMI_mixed, levels = c("Underweight", "Normal", "Overweight", "Obese", "missing_unknown"))
  )


# Output cleaned dataframe
write_csv(all_df, "combined_df_cleaned_mod_new.csv")


#### Pre and Post Transformation for FG and Total Cholesterol ####


library(dplyr)

# Add assumed unit columns based on original values
all_df <- all_df %>%
  mutate(
    fg_unit_assumed = case_when(
      fasting_glucose > 20 ~ "mg/dL",
      fasting_glucose >= 1.1 & fasting_glucose <= 20 ~ "mmol/L",
      TRUE ~ "invalid"
    ),
    tc_unit_assumed = case_when(
      total_chol > 40 ~ "mg/dL",
      total_chol >= 1.0 & total_chol <= 12.93 ~ "mmol/L",
      TRUE ~ "invalid"
    )
  )


# Countries with mixed units for fasting glucose
countries_with_mix_fg <- all_df %>%
  group_by(country) %>%
  summarize(
    has_mg_dl = any(fg_unit_assumed == "mg/dL", na.rm = TRUE),
    has_mmol_l = any(fg_unit_assumed == "mmol/L", na.rm = TRUE)
  ) %>%
  filter(has_mg_dl & has_mmol_l) %>%
  pull(country)

# Countries with mixed units for total cholesterol
countries_with_mix_tc <- all_df %>%
  group_by(country) %>%
  summarize(
    has_mg_dl = any(tc_unit_assumed == "mg/dL", na.rm = TRUE),
    has_mmol_l = any(tc_unit_assumed == "mmol/L", na.rm = TRUE)
  ) %>%
  filter(has_mg_dl & has_mmol_l) %>%
  pull(country)



# Your provided function
replace_implausible <- function(x, implausible_vals) {
  case_when(
    sapply(implausible_vals, function(val) near(x, val, tol = 0.01)) %>% rowSums() > 0 |
      is.infinite(x) ~ NA_real_,
    TRUE ~ x
  )
}

# Apply transformations
all_df <- all_df %>%
  mutate(
    fasting_glucose = replace_implausible(fasting_glucose, implausible_values),
    fasting_glucose = case_when(
      is.na(fasting_glucose) | fasting_glucose > 500 | fasting_glucose < 0 ~ NA_real_,
      TRUE ~ fasting_glucose
    ),
    total_chol = replace_implausible(total_chol, implausible_values),
    total_chol = case_when(
      is.na(total_chol) | total_chol > 500 | total_chol < 0 ~ NA_real_,
      TRUE ~ total_chol
    ),
    # Unit conversions
    fasting_glucose_mmol = case_when(
      is.na(fasting_glucose) ~ NA_real_,
      fasting_glucose > 20 ~ fasting_glucose * 0.0555,
      fasting_glucose >= 1.1 & fasting_glucose <= 20 ~ fasting_glucose,
      TRUE ~ NA_real_
    ),
    total_chol_mmol = case_when(
      is.na(total_chol) ~ NA_real_,
      total_chol > 40 ~ total_chol * 0.02586,
      total_chol >= 1.0 & total_chol <= 12.93 ~ total_chol,
      TRUE ~ NA_real_
    )
  )



library(ggplot2)

# Subset for countries with mixed units for fasting glucose
df_mix_fg <- all_df %>% filter(country %in% countries_with_mix_fg)

# Before transformation: original values colored by assumed unit
ggplot(df_mix_fg, aes(x = fasting_glucose, fill = fg_unit_assumed)) +
  geom_histogram(binwidth = 1, alpha = 0.7) +
  facet_wrap(~ country, scales = "free") +
  labs(
    title = "Original Fasting Glucose by Country with Mixed Units",
    x = "Fasting Glucose",
    y = "Count",
    fill = "Assumed Unit"
  ) +
  theme_minimal()

# After transformation: converted to mmol/L
ggplot(df_mix_fg, aes(x = fasting_glucose_mmol)) +
  geom_histogram(binwidth = 0.1, fill = "blue", alpha = 0.7) +
  facet_wrap(~ country, scales = "free") +
  labs(
    title = "Transformed Fasting Glucose (mmol/L) by Country with Mixed Units",
    x = "Fasting Glucose (mmol/L)",
    y = "Count"
  ) +
  theme_minimal()




# Subset for countries with mixed units for total cholesterol
df_mix_tc <- all_df %>% filter(country %in% countries_with_mix_tc)

# Before transformation: original values colored by assumed unit
ggplot(df_mix_tc, aes(x = total_chol, fill = tc_unit_assumed)) +
  geom_histogram(binwidth = 1, alpha = 0.7) +
  facet_wrap(~ country, scales = "free") +
  labs(
    title = "Original Total Cholesterol by Country with Mixed Units",
    x = "Total Cholesterol",
    y = "Count",
    fill = "Assumed Unit"
  ) +
  theme_minimal()

# After transformation: converted to mmol/L
ggplot(df_mix_tc, aes(x = total_chol_mmol)) +
  geom_histogram(binwidth = 0.1, fill = "blue", alpha = 0.7) +
  facet_wrap(~ country, scales = "free") +
  labs(
    title = "Transformed Total Cholesterol (mmol/L) by Country with Mixed Units",
    x = "Total Cholesterol (mmol/L)",
    y = "Count"
  ) +
  theme_minimal()

