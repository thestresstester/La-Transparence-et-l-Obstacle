## Get current path for wd
# path <- dirname(rstudioapi::getSourceEditorContext()$path)
# setwd(paste0(path, "/../.."))
# getwd()

categorical_cols <- c("TP", "ISO2", "rel_period", "Common_Exposure", "Framework")

numeric_cols = c("Period", "Scenario", "Country", "Maturity",
                 "Category", "Status", "Portfolio", "Perf_Status", "IFRS9_Stages",
                 "Perf_Forborne", "Exposure", "Country_Rank",
                 "MKT_Risk", "MKT_ModProd", "Assets_FV", "Assets_Stages", "Accounting_Portfolio",
                 "NACE_Codes", "Financial_Instruments", "Fin_End_Year", "retail_sample", "retail_sample_ex",
                 "CR_exp_moratoria", "CR_guarantees", "IFRS9_Stages")

# Create DuckDB connection (add this at the top of global.R, around line 13)
con <- dbConnect(duckdb::duckdb(), dbdir = ":memory:")

# Instead of reading and registering, create views directly from parquet files
# DuckDB will read only what it needs
dbExecute(con, "
  CREATE VIEW st_data_table AS 
  SELECT * FROM read_parquet('Meta/Original Data/Total_Stress_Tests.parquet')
")

dbExecute(con, "
  CREATE VIEW ssm_data_table AS 
  SELECT * FROM read_parquet('Meta/Original Data/Total_SSM.parquet')
")

dbExecute(con, "
  CREATE VIEW tr_data_table AS 
  SELECT * FROM read_parquet('Meta/Original Data/Total_Transparency.parquet')
")

dbExecute(con, "
  CREATE VIEW exp_data_table AS 
  SELECT * FROM read_parquet('Meta/Original Data/Merged Datasets/EBA_Exposure.parquet')
")

dbExecute(con, "
  CREATE VIEW mkt_data_table AS 
  SELECT * FROM read_parquet('Meta/Original Data/Merged Datasets/EBA_Market.parquet')
")

dbExecute(con, "
  CREATE VIEW plc_data_table AS 
  SELECT * FROM read_parquet('Meta/Original Data/Merged Datasets/EBA_PLC.parquet')
")

dbExecute(con, "
  CREATE VIEW sov_data_table AS 
  SELECT * FROM read_parquet('Meta/Original Data/Merged Datasets/EBA_Sovereign.parquet')
")

dbExecute(con, "
  CREATE VIEW chart_db_table AS 
  SELECT * FROM read_parquet('Meta/Original Data/chart_db.parquet')
")


dbExecute(con, "
  CREATE VIEW rps_data_table AS 
  SELECT * FROM read_parquet('Meta/Original Data/EBA_Risk_Parameters.parquet')
")

# Modified load functions that use DuckDB
load_st_data <- function() {
  query <- "SELECT * FROM st_data_table"
  db <- dbGetQuery(con, query) %>%
    mutate(across(intersect(categorical_cols, colnames(.)), ~as.character(.))) %>%
    mutate(across(intersect(numeric_cols, colnames(.)), ~as.numeric(as.character(.))))
  return(db)
}

load_ssm_data <- function() {
  query <- "SELECT * FROM ssm_data_table"
  db <- dbGetQuery(con, query) %>%
    mutate(across(intersect(categorical_cols, colnames(.)), ~as.character(.))) %>%
    mutate(across(intersect(numeric_cols, colnames(.)), ~as.numeric(as.character(.))))
  return(db)
}

load_tr_data <- function() {
  query <- "SELECT * FROM tr_data_table"
  db <- dbGetQuery(con, query) %>%
    mutate(across(intersect(categorical_cols, colnames(.)), ~as.character(.))) %>%
    mutate(across(intersect(numeric_cols, colnames(.)), ~as.numeric(as.character(.))))
  return(db)
}

load_eba_exposure_data <- function() {
  query <- "SELECT * FROM exp_data_table"
  db <- dbGetQuery(con, query) %>%
    mutate(across(intersect(categorical_cols, colnames(.)), ~as.character(.))) %>%
    mutate(across(intersect(numeric_cols, colnames(.)), ~as.numeric(as.character(.))))
  return(db)
}

load_eba_market_data <- function() {
  query <- "SELECT * FROM mkt_data_table"
  db <- dbGetQuery(con, query) %>%
    mutate(across(intersect(categorical_cols, colnames(.)), ~as.character(.))) %>%
    mutate(across(intersect(numeric_cols, colnames(.)), ~as.numeric(as.character(.))))
  return(db)
}

load_eba_plc_data <- function() {
  query <- "SELECT * FROM plc_data_table"
  db <- dbGetQuery(con, query) %>%
    mutate(across(intersect(categorical_cols, colnames(.)), ~as.character(.))) %>%
    mutate(across(intersect(numeric_cols, colnames(.)), ~as.numeric(as.character(.))))
  return(db)
}

load_eba_sovereign_data <- function() {
  query <- "SELECT * FROM sov_data_table"
  db <- dbGetQuery(con, query) %>%
    mutate(across(intersect(categorical_cols, colnames(.)), ~as.character(.))) %>%
    mutate(across(intersect(numeric_cols, colnames(.)), ~as.numeric(as.character(.))))
  return(db)
}

loadRPSdata <- function() {
  query <- "SELECT * FROM rps_data_table"
  db <- dbGetQuery(con, query) %>%
    mutate(across(intersect(categorical_cols, colnames(.)), ~as.character(.))) %>%
    mutate(across(intersect(numeric_cols, colnames(.)), ~as.numeric(as.character(.))))
  return(db)
}

# load_st_data <- function() {
#   # This function loads the EU-Wide Stress-Test dataset.
#   db = fst::read_fst("Meta/Original Data/Total_Stress_Tests.fst") %>% 
#     dplyr::select(-retail_sample, -retail_sample_ex) %>%
#     mutate(across(intersect(categorical_cols, colnames(.)), ~as.character(.))) %>%
#     mutate(across(intersect(numeric_cols, colnames(.)), ~as.numeric(as.character(.))))
#   return(db)
# }
# 
# load_ssm_data <- function() {
#   # This function loads the SSM Stress-Test dataset.
#   db = fst::read_fst("Meta/Original Data/Total_SSM.fst") %>% 
#     dplyr::select(-retail_sample, -retail_sample_ex) %>%
#     mutate(across(intersect(categorical_cols, colnames(.)), ~as.character(.))) %>%
#     mutate(across(intersect(numeric_cols, colnames(.)), ~as.numeric(as.character(.))))
#   return(db)
# }
# 
# load_tr_data <- function() {
#   # This function loads the Transparency Exercise dataset.
#   db = fst::read_fst("Meta/Original Data/Total_Transparency.fst") %>% 
#     dplyr::select(-retail_sample, -retail_sample_ex) %>%
#     mutate(across(intersect(categorical_cols, colnames(.)), ~as.character(.))) %>%
#     mutate(across(intersect(numeric_cols, colnames(.)), ~as.numeric(as.character(.))))
#   return(db)
# }
# 
# load_eba_exposure_data <- function() {
#   db = fst::read_fst("Meta/Original Data/Merged Datasets/EBA_Exposure.fst") %>% 
#     dplyr::select(-retail_sample, -retail_sample_ex) %>%
#     mutate(across(intersect(categorical_cols, colnames(.)), ~as.character(.))) %>%
#     mutate(across(intersect(numeric_cols, colnames(.)), ~as.numeric(as.character(.))))
#   return(db)
# }
# 
# load_eba_market_data <- function() {
#   db = fst::read_fst("Meta/Original Data/Merged Datasets/EBA_Market.fst") %>% 
#     dplyr::select(-retail_sample, -retail_sample_ex) %>%
#     mutate(across(intersect(categorical_cols, colnames(.)), ~as.character(.))) %>%
#     mutate(across(intersect(numeric_cols, colnames(.)), ~as.numeric(as.character(.))))
#   return(db)
# }
# 
# load_eba_plc_data <- function() {
#   db = fst::read_fst("Meta/Original Data/Merged Datasets/EBA_PLC.fst") %>% 
#     dplyr::select(-retail_sample, -retail_sample_ex) %>%
#     mutate(across(intersect(categorical_cols, colnames(.)), ~as.character(.))) %>%
#     mutate(across(intersect(numeric_cols, colnames(.)), ~as.numeric(as.character(.))))
#   return(db)
# }
# 
# load_eba_sovereign_data <- function() {
#   db = fst::read_fst("Meta/Original Data/Merged Datasets/EBA_Sovereign.fst") %>% 
#     dplyr::select(-retail_sample, -retail_sample_ex) %>%
#     mutate(across(intersect(categorical_cols, colnames(.)), ~as.character(.))) %>%
#     mutate(across(intersect(numeric_cols, colnames(.)), ~as.numeric(as.character(.))))
#   return(db)
# }
# 
# load_eba_ssm_data <- function() {
#   db = fst::read_fst("Meta/Original Data/Merged Datasets/EBA_SSM.fst") %>% 
#     dplyr::select(-retail_sample, -retail_sample_ex) %>%
#     mutate(across(intersect(categorical_cols, colnames(.)), ~as.character(.))) %>%
#     mutate(across(intersect(numeric_cols, colnames(.)), ~as.numeric(as.character(.))))
#   return(db)
# }

### Metadata Items

## Bank Structure
bk_struct = read.fst("Meta/Original Data/Structure/bank_structure.fst") %>% 
  mutate(Exercise = ifelse(Exercise == 20201, "2020 Summer", 
                           ifelse(Exercise == 20202, "2020 Autumn", Exercise)))
itm_struct = read.fst("Meta/Original Data/Structure/item_structure.fst") 

### Item tables
itm_tot = read_xlsx("Meta/Total_Metadata_App.xlsx", sheet = "Final_Matches")

itm_table_st = itm_tot %>%
  dplyr::select(Common_Item, "Label Stress Test", matches("ST_\\d{1,}")) %>%
  filter(!if_all(matches("ST_\\d{1,}"), is.na)) %>%
  rename_with(~str_remove(., "ST_"), matches("ST_\\d{1,}")) %>% 
  rename_all(~str_replace_all(., "_", " ")) %>% 
  arrange(as.numeric(str_extract(`Common Item`, "\\d{1,3}")))

itm_table_ssm = itm_tot %>%
  dplyr::select(Common_Item, "Label Stress Test", SSM) %>%
  na.omit() %>% 
  rename_all(~str_replace_all(., "_", " ")) %>% 
  arrange(as.numeric(str_extract(`Common Item`, "\\d{1,3}")))

itm_table_tr = itm_tot %>%
  dplyr::select(Common_Item, "Label Transparency", matches("TR_\\d{1,}")) %>%
  filter(!if_all(matches("TR_\\d{1,}"), is.na)) %>%
  rename_with(~str_replace(., "_A", " Autumn"), matches("TR_2020_A")) %>%
  rename_with(~str_replace(., "_S", " Summer"), matches("TR_2020_S")) %>%
  rename_with(~str_remove(., "TR_"), matches("TR_\\d{1,}")) %>% 
  rename_all(~str_replace_all(., "_", " ")) %>% 
  arrange(as.numeric(str_extract(`Common Item`, "\\d{1,3}"))) %>% 
  dplyr::select(-`2014`)

itm_table_total = bk_struct %>% 
  dplyr::select(DB, Common_Item) %>% 
  distinct()
itm_table_total = left_join(itm_table_total, itm_tot, by = "Common_Item") %>% 
  dplyr::select(-Stress_Test_Item, -Transparency_Item) %>% 
  rename_with(~str_replace(., "_A", " Autumn"), matches("TR_2020_A")) %>%
  rename_with(~str_replace(., "_S", " Summer"), matches("TR_2020_S")) %>%
  rename_all(~str_replace_all(., "_", " ")) 

itm_table_sov = itm_table_total %>% 
  filter(DB == "SOV") %>% 
  dplyr::select(where(~!all(is.na(.x)))) %>%
  distinct() %>% 
  dplyr::select(-DB) %>% 
  filter(!if_all(matches("TR \\d{1,}|ST \\d{1,}|SSM \\d{1,}"), is.na)) %>%
  arrange(as.numeric(str_extract(`Common Item`, "\\d{1,3}"))) %>% 
  dplyr::select(-`TR 2014`)

itm_table_exp = itm_table_total %>% 
  filter(DB == "EXP") %>% 
  dplyr::select(where(~!all(is.na(.x)))) %>%
  distinct() %>% 
  dplyr::select(-DB) %>% 
  filter(!if_all(matches("TR \\d{1,}|ST \\d{1,}|SSM \\d{1,}"), is.na)) %>%
  arrange(as.numeric(str_extract(`Common Item`, "\\d{1,3}"))) %>% 
  dplyr::select(-`TR 2014`)

itm_table_mkt = itm_table_total %>% 
  filter(DB == "MKT") %>% 
  dplyr::select(where(~!all(is.na(.x)))) %>%
  distinct() %>% 
  dplyr::select(-DB) %>% 
  filter(!if_all(matches("TR \\d{1,}|ST \\d{1,}|SSM \\d{1,}"), is.na)) %>%
  arrange(as.numeric(str_extract(`Common Item`, "\\d{1,3}"))) %>% 
  dplyr::select(-`TR 2014`, -`ST 2014`)

itm_table_plc = itm_table_total %>% 
  filter(DB == "PLC") %>% 
  dplyr::select(where(~!all(is.na(.x)))) %>%
  distinct() %>% 
  dplyr::select(-DB) %>% 
  filter(!if_all(matches("TR \\d{1,}|ST \\d{1,}|SSM \\d{1,}"), is.na)) %>%
  arrange(as.numeric(str_extract(`Common Item`, "\\d{1,3}"))) %>% 
  dplyr::select(-`TR 2014`)

## Metadata

st_meta = read_xlsx("Meta/Metadata_DB.xlsx", sheet = "ST")
tr_meta = read_xlsx("Meta/Metadata_DB.xlsx", sheet = "TE")
ssm_meta = read_xlsx("Meta/Metadata_DB.xlsx", sheet = "SSM")



### Country Structure
countries_tot_st = bk_struct %>% 
  filter(DB == "ST") %>% 
  dplyr::select(Country_Label, ISO2) %>% 
  na.omit() %>% 
  distinct()
countries_st = countries_tot_st$ISO2
names(countries_st) = countries_tot_st$Country

countries_tot_ssm = bk_struct %>% 
  filter(DB == "SSM") %>% 
  dplyr::select(Country_Label, ISO2) %>% 
  na.omit() %>% 
  distinct()
countries_ssm = countries_tot_ssm$ISO2
names(countries_ssm) = countries_tot_ssm$Country

countries_tot_tr = bk_struct %>% 
  filter(DB == "TR") %>% 
  dplyr::select(Country_Label, ISO2) %>% 
  na.omit() %>% 
  distinct()
countries_tr = countries_tot_tr$ISO2
names(countries_tr) = countries_tot_tr$Country

countries_tot_plc = bk_struct %>% 
  filter(DB == "PLC") %>% 
  dplyr::select(Country_Label, ISO2) %>% 
  na.omit() %>% 
  distinct()
countries_plc = countries_tot_plc$ISO2
names(countries_plc) = countries_tot_plc$Country

countries_tot_mkt = bk_struct %>% 
  filter(DB == "MKT") %>% 
  dplyr::select(Country_Label, ISO2) %>% 
  na.omit() %>% 
  distinct()
countries_mkt = countries_tot_mkt$ISO2
names(countries_mkt) = countries_tot_mkt$Country

countries_tot_sov = bk_struct %>% 
  filter(DB == "SOV") %>% 
  dplyr::select(Country_Label, ISO2) %>% 
  na.omit() %>% 
  distinct()
countries_sov = countries_tot_sov$ISO2
names(countries_sov) = countries_tot_sov$Country

countries_tot_exp = bk_struct %>% 
  filter(DB == "EXP") %>% 
  dplyr::select(Country_Label, ISO2) %>% 
  na.omit() %>% 
  distinct()
countries_exp = countries_tot_exp$ISO2
names(countries_exp) = countries_tot_exp$Country

### Bank Structure
banks_orig_st = bk_struct %>% 
  filter(DB == "ST") %>% 
  dplyr::select(Bank_ID, Name, ISO2) %>% 
  na.omit() %>%
  distinct() %>% 
  mutate(CountryName = names(countries_st)[match(ISO2, countries_st)]) %>%
  mutate(DisplayName = paste0(Name, " (", CountryName, ")"))
banks_st = banks_orig_st$Bank_ID
names(banks_st) = banks_orig_st$Name
initial_bank_st <- setNames(banks_orig_st$Bank_ID, banks_orig_st$DisplayName)

banks_orig_ssm = bk_struct %>% 
  filter(DB == "SSM") %>% 
  dplyr::select(Bank_ID, Name, ISO2) %>% 
  na.omit() %>%
  distinct() %>% 
  mutate(CountryName = names(countries_ssm)[match(ISO2, countries_ssm)]) %>%
  mutate(DisplayName = paste0(Name, " (", CountryName, ")"))
banks_ssm = banks_orig_ssm$Bank_ID
names(banks_ssm) = banks_orig_ssm$Name
initial_bank_ssm <- setNames(banks_orig_ssm$Bank_ID, banks_orig_ssm$DisplayName)

banks_orig_tr = bk_struct %>% 
  filter(DB == "TR") %>% 
  dplyr::select(Bank_ID, Name, ISO2) %>% 
  na.omit() %>%
  distinct() %>% 
  mutate(CountryName = names(countries_tr)[match(ISO2, countries_tr)]) %>%
  mutate(DisplayName = paste0(Name, " (", CountryName, ")"))
banks_tr = banks_orig_tr$Bank_ID
names(banks_tr) = banks_orig_tr$Name
initial_bank_tr <- setNames(banks_orig_tr$Bank_ID, banks_orig_tr$DisplayName)

banks_orig_exp = bk_struct %>% 
  filter(DB == "EXP") %>% 
  dplyr::select(Bank_ID, Name, ISO2) %>% 
  na.omit() %>%
  distinct() %>% 
  mutate(CountryName = names(countries_exp)[match(ISO2, countries_exp)]) %>%
  mutate(DisplayName = paste0(Name, " (", CountryName, ")"))
banks_exp = banks_orig_exp$Bank_ID
names(banks_exp) = banks_orig_exp$Name
initial_bank_exp <- setNames(banks_orig_exp$Bank_ID, banks_orig_exp$DisplayName)

banks_orig_sov = bk_struct %>% 
  filter(DB == "SOV") %>% 
  dplyr::select(Bank_ID, Name, ISO2) %>% 
  na.omit() %>%
  distinct() %>% 
  mutate(CountryName = names(countries_sov)[match(ISO2, countries_sov)]) %>%
  mutate(DisplayName = paste0(Name, " (", CountryName, ")"))
banks_sov = banks_orig_sov$Bank_ID
names(banks_sov) = banks_orig_sov$Name
initial_bank_sov <- setNames(banks_orig_sov$Bank_ID, banks_orig_sov$DisplayName)

banks_orig_plc = bk_struct %>% 
  filter(DB == "PLC") %>% 
  dplyr::select(Bank_ID, Name, ISO2) %>% 
  na.omit() %>%
  distinct() %>% 
  mutate(CountryName = names(countries_plc)[match(ISO2, countries_plc)]) %>%
  mutate(DisplayName = paste0(Name, " (", CountryName, ")"))
banks_plc = banks_orig_plc$Bank_ID
names(banks_plc) = banks_orig_plc$Name
initial_bank_plc <- setNames(banks_orig_plc$Bank_ID, banks_orig_plc$DisplayName)

banks_orig_mkt = bk_struct %>% 
  filter(DB == "MKT") %>% 
  dplyr::select(Bank_ID, Name, ISO2) %>% 
  na.omit() %>%
  distinct() %>% 
  mutate(CountryName = names(countries_mkt)[match(ISO2, countries_mkt)]) %>%
  mutate(DisplayName = paste0(Name, " (", CountryName, ")"))
banks_mkt = banks_orig_mkt$Bank_ID
names(banks_mkt) = banks_orig_mkt$Name
initial_bank_mkt <- setNames(banks_orig_mkt$Bank_ID, banks_orig_mkt$DisplayName)

## Creating dictionary

st_dict = read_xlsx("Meta/Total_Metadata_App.xlsx", sheet = "ST_Dictionary")
tr_dict = read_xlsx("Meta/Total_Metadata_App.xlsx", sheet = "TR_Dictionary")
ssm_dict = read_xlsx("Meta/Total_Metadata_App.xlsx", sheet = "SSM_Dictionary")

## Scenario (same for all)
scenario_st = st_dict$Value_Scenario_Final[which(!is.na(st_dict$Value_Scenario_Final))]
names(scenario_st) = st_dict$Label_Scenario_Final[which(!is.na(st_dict$Label_Scenario_Final))]

## Country Exposure
country_st = st_dict$Label_Country_Final[which(!is.na(st_dict$Label_Country_Final))]
names(country_st) = st_dict$Value_Country_Final[which(!is.na(st_dict$Value_Country_Final))]

country_tr = tr_dict$Value_Country_Final[which(!is.na(tr_dict$Value_Country_Final))]
names(country_tr) = tr_dict$Label_Country_Final[which(!is.na(tr_dict$Label_Country_Final))]

final_country = c(country_st, country_tr[which(!(country_tr%in%country_st))])

## Portfolio
portfolio_st = st_dict$Value_Portfolio_Final[which(!is.na(st_dict$Value_Portfolio_Final))]
names(portfolio_st) = st_dict$Label_Portfolio_Final[which(!is.na(st_dict$Label_Portfolio_Final))]

portfolio_tr = tr_dict$Value_Portfolio[which(!is.na(tr_dict$Value_Portfolio))]
names(portfolio_tr) = tr_dict$Label_Portfolio[which(!is.na(tr_dict$Label_Portfolio))]

final_portfolio = c(portfolio_st, portfolio_tr[which(!(portfolio_tr%in%portfolio_st))])

## Maturity
maturity_st = st_dict$Value_Maturity_Final[which(!is.na(st_dict$Value_Maturity_Final))]
names(maturity_st) = st_dict$Label_Maturity_Final[which(!is.na(st_dict$Label_Maturity_Final))]

maturity_tr = tr_dict$Value_Maturity_Final[which(!is.na(tr_dict$Value_Maturity_Final))]
names(maturity_tr) = tr_dict$Label_Maturity_Final[which(!is.na(tr_dict$Label_Maturity_Final))]

final_maturity = c(maturity_st, maturity_tr[which(!(maturity_tr%in%maturity_st))])

## Exposure
exposure_st = st_dict$Value_Exposure_Final[which(!is.na(st_dict$Value_Exposure_Final))]
names(exposure_st) = st_dict$Label_Exposure_Final[which(!is.na(st_dict$Label_Exposure_Final))]

exposure_tr = tr_dict$Value_Exposure_Final[which(!is.na(tr_dict$Value_Exposure_Final))]
names(exposure_tr) = tr_dict$Label_Exposure_Final[which(!is.na(tr_dict$Label_Exposure_Final))]

final_exposure = c(exposure_st, exposure_tr[which(!(exposure_tr%in%exposure_st))])

## Status
status_st = st_dict$Value_Status_Final[which(!is.na(st_dict$Value_Status_Final))]
names(status_st) = st_dict$Label_Status_Final[which(!is.na(st_dict$Label_Status_Final))]

status_tr = tr_dict$Value_Status_Final[which(!is.na(tr_dict$Value_Status_Final))]
names(status_tr) = tr_dict$Label_Status_Final[which(!is.na(tr_dict$Label_Status_Final))]

final_status = c(status_st, status_tr[which(!(status_tr%in%status_st))])

## Perf_Status
perf_status_st = st_dict$Value_Perf_Status_Final[which(!is.na(st_dict$Value_Perf_Status_Final))]
names(perf_status_st) = st_dict$Label_Perf_Status_Final[which(!is.na(st_dict$Label_Perf_Status_Final))]

perf_status_tr = tr_dict$Value_Perf_Status_Final[which(!is.na(tr_dict$Value_Perf_Status_Final))]
names(perf_status_tr) = tr_dict$Label_Perf_Status_Final[which(!is.na(tr_dict$Label_Perf_Status_Final))]

final_perf_status = c(perf_status_st, perf_status_tr[which(!(perf_status_tr%in%perf_status_st))])

## Forborne
forborne_st = st_dict$Value_Perf_Forborne_Final[which(!is.na(st_dict$Value_Perf_Forborne_Final))]
names(forborne_st) = st_dict$Label_Perf_Forborne_Final[which(!is.na(st_dict$Label_Perf_Forborne_Final))]

forborne_tr = tr_dict$Value_Perf_Forborne_Final[which(!is.na(tr_dict$Value_Perf_Forborne_Final))]
names(forborne_tr) = tr_dict$Label_Perf_Forborne_Final[which(!is.na(tr_dict$Label_Perf_Forborne_Final))]

final_forborne = c(forborne_st, forborne_tr[which(!(forborne_tr%in%forborne_st))])

## ST Specific
ifrs_st = st_dict$Value_IFRS9_Stages_Final[which(!is.na(st_dict$Value_IFRS9_Stages_Final))]
names(ifrs_st) = st_dict$Label_IFRS9_Stages_Final[which(!is.na(st_dict$Label_IFRS9_Stages_Final))]

# cr_guarantees_st = st_dict$Value_CR_Guarantees_Final[which(!is.na(st_dict$Value_CR_Guarantees_Final))]
# names(cr_guarantees_st) = st_dict$Label_CR_Guarantees_Final[which(!is.na(st_dict$Label_CR_Guarantees_Final))]
# 
# cr_moratoria_st = st_dict$Value_CR_exp_moratoria_Final[which(!is.na(st_dict$Value_CR_exp_moratoria_Final))]
# names(cr_moratoria_st) = st_dict$Label_CR_exp_moratoria_Final[which(!is.na(st_dict$Label_CR_exp_moratoria_Final))]
# 
# category_st = st_dict$Value_Category_Final[which(!is.na(st_dict$Value_Category_Final))]
# names(category_st) = st_dict$Label_Category_Final[which(!is.na(st_dict$Label_Category_Final))]
# 
# fact_char_st = st_dict$Value_Fact_Chart_Final[which(!is.na(st_dict$Value_Fact_Chart_Final))]
# names(fact_char_st) = st_dict$Label_Fact_Chart_Final[which(!is.na(st_dict$Label_Fact_Chart_Final))]

rel_period = c("Realised", "Year1", "Year2", "Year3")
names(rel_period) = c("Realised", "Year 1", "Year 2", "Year 3")

##------------------------------------
## TR specific
fin_instr_tr = tr_dict$Value_Financial_Instrument_Final[which(!is.na(tr_dict$Value_Financial_Instrument_Final))]
names(fin_instr_tr) = tr_dict$Label_Financial_Instrument_Final[which(!is.na(tr_dict$Label_Financial_Instrument_Final))]

mkt_modprod_tr = tr_dict$Value_MKT_modprod_Final[which(!is.na(tr_dict$Value_MKT_modprod_Final))]
names(mkt_modprod_tr) = tr_dict$Label_MKT_modprod_Final[which(!is.na(tr_dict$Label_MKT_modprod_Final))]

mkt_risk_tr = tr_dict$Value_MKT_risk_Final[which(!is.na(tr_dict$Value_MKT_risk_Final))]
names(mkt_risk_tr) = tr_dict$Label_MKT_risk_Final[which(!is.na(tr_dict$Label_MKT_risk_Final))]

acc_portfolio_tr = tr_dict$Value_Accounting_Portfolio_Final[which(!is.na(tr_dict$Value_Accounting_Portfolio_Final))]
names(acc_portfolio_tr) = tr_dict$Label_Accounting_Portfolio_Final[which(!is.na(tr_dict$Label_Accounting_Portfolio_Final))]

asset_stages_tr = tr_dict$Value_Asset_Stages_Final[which(!is.na(tr_dict$Value_Asset_Stages_Final))]
names(asset_stages_tr) = tr_dict$Label_Asset_Stages_Final[which(!is.na(tr_dict$Label_Asset_Stages_Final))]

asset_fv_tr = tr_dict$Value_Asset_FV_Final[which(!is.na(tr_dict$Value_Asset_FV_Final))]
names(asset_fv_tr) = tr_dict$Label_Asset_FV_Final[which(!is.na(tr_dict$Label_Asset_FV_Final))]

nace_tr = tr_dict$Value_NACE_Final[which(!is.na(tr_dict$Value_NACE_Final))]
names(nace_tr) = tr_dict$Label_NACE_Final[which(!is.na(tr_dict$Label_NACE_Final))]

## Fin year end not used

### Common Exposure
com_dict = read_xlsx("Meta/Metadata_DB.xlsx", sheet = "Common_Exposure") 

common_exposure_final = com_dict$Common_Exposure[which(!is.na(com_dict$Common_Exposure))]
names(common_exposure_final) = com_dict$Exposure_Label[which(!is.na(com_dict$Common_Exposure))]

## Labelling item structure 
itm_table_total = bk_struct %>% 
  dplyr::select(DB, Common_Item) %>% 
  distinct()

itm_struct_labelled = left_join(itm_table_total, itm_struct, by = "Common_Item", relationship = "many-to-many") %>% 
  mutate(Scenario = ifelse(DB %in% c("SOV", "MKT", "TR"), NA, as.character(Scenario))) %>% 
  mutate(NACE_Codes = ifelse(DB %in% c("ST", "SSM"), NA, as.character(NACE_Codes))) 

itm_struct_labelled = itm_struct_labelled %>% 
  mutate(Scenario = factor(Scenario, scenario_st, names(scenario_st))) %>% 
  mutate(Country = factor(Country, final_country, names(final_country))) %>% 
  mutate(Portfolio = factor(Portfolio, final_portfolio, names(final_portfolio))) %>% 
  mutate(Maturity = factor(Maturity, final_maturity, names(final_maturity))) %>% 
  mutate(Exposure = factor(Exposure, final_exposure, names(final_exposure))) %>%
  mutate(Status = factor(Status, final_status, names(final_status))) %>% 
  mutate(Perf_Status = factor(Perf_Status, final_perf_status, names(final_perf_status))) %>% 
  mutate(Perf_Forborne = factor(Perf_Forborne, final_forborne, names(final_forborne))) %>% 
  mutate(IFRS9_Stages = factor(IFRS9_Stages, ifrs_st, names(ifrs_st))) %>% 
  mutate(Financial_Instruments = factor(Financial_Instruments, fin_instr_tr, names(fin_instr_tr))) %>% 
  mutate(MKT_Risk = factor(MKT_Risk, mkt_risk_tr, names(mkt_risk_tr))) %>% 
  mutate(MKT_ModProd = factor(MKT_ModProd, mkt_modprod_tr, names(mkt_modprod_tr))) %>% 
  mutate(Assets_FV = factor(Assets_FV, asset_fv_tr, names(asset_fv_tr))) %>% 
  mutate(Assets_Stages = factor(Assets_Stages, asset_stages_tr, names(asset_stages_tr))) %>% 
  mutate(Accounting_Portfolio = factor(Accounting_Portfolio, acc_portfolio_tr, names(acc_portfolio_tr))) %>% 
  mutate(NACE_Codes = factor(NACE_Codes, nace_tr, names(nace_tr))) %>% 
  mutate(Common_Exposure = factor(Common_Exposure, common_exposure_final, names(common_exposure_final))) %>% 
  distinct()

includeMarkdownSection = function(section_name) {
  # Read the markdown file
  markdown_content = readLines("Meta/markdown.rmd", warn = FALSE)
  
  # Create the start and end delimiters
  start_delimiter = paste0("# DELIMITER - ", section_name)
  
  # Find the line numbers of the start and end delimiters
  start_line = which(markdown_content == start_delimiter)
  
  # Find all delimiter lines
  delimiter_lines = which(grepl("^# DELIMITER - ", markdown_content))
  
  # Find the next delimiter line after the start line
  next_delimiter_line = delimiter_lines[delimiter_lines > start_line]
  
  if (length(next_delimiter_line) > 0) {
    end_line = min(next_delimiter_line)
  } else {
    end_line = length(markdown_content) + 1
  }
  
  # Extract the content between the delimiters
  if (length(start_line) == 1) {
    section_content = markdown_content[(start_line + 1):(end_line - 1)]
    return(paste(section_content, collapse = "\n"))
  } else {
    return(paste("Section", section_name, "not found."))
  }
}

## Distress event dataset 
## Import dataset 
bds_file_path <- "Meta/Original Data/Banking Distress Database.xlsx"

distress_event = function(){
  distress_db = read_xlsx("Meta/Original Data/Banking Distress Database.xlsx", sheet = "Main Table", skip = 3) %>% 
    rename_all(~str_to_title(str_replace_all(., "_", " "))) %>% 
    rename(ISO2=Iso2, RIC = Ric, LEI = Lei)
  
  return(distress_db)
}

## Risk parameters dataset 
risk_parameters = function(){
  db = fst::read_fst("Meta/Original Data/EBA_Risk_Parameters.fst") 
  return(db)
}

risk_parameters_structure = fst::read_fst("Meta/Original Data/EBA_Risk_Parameters.fst") %>% 
  dplyr::select(Country_Label, Common_Item, Period, Exposure_Label) %>% 
  distinct()

countries_rps = sort(unique(risk_parameters_structure$Country_Label))

distress_structure = read_xlsx("Meta/Original Data/Banking Distress Database.xlsx", sheet = "Main Table", skip = 3) %>% 
  rename_all(~str_to_title(str_replace_all(., "_", " "))) %>% 
  dplyr::select(Country, Name, Iso2) %>% 
  rename(ISO2=Iso2) %>% 
  distinct() %>% 
  na.omit()

countries_bds = sort(unique(distress_structure$Country))
initial_bank_bds = sort(unique(distress_structure$Name))

banks_orig_bds = distress_structure %>% 
  dplyr::select(Name, ISO2, Country) %>% 
  na.omit() %>%
  distinct() %>% 
  mutate(DisplayName = paste0(Name, " (", Country, ")"))

###------------------------------------
### Metadata

## Bank_ID
metabanks <- read_xlsx("Meta/Total_Metadata_App.xlsx", sheet = "Bank_ID") %>%
  pivot_longer(matches("\\d{4}"), names_to = "Exercise", values_to = "Sample") %>%
  filter(!is.na(Sample)) %>%
  mutate(Exercise = str_replace_all(Exercise, "_", " ")) %>% 
  mutate(Exercise = str_replace(Exercise, " S$", " AASummer")) %>% 
  mutate(Exercise = str_replace(Exercise, " A$", " Autumn")) %>% 
  arrange(Bank_ID, ISO2, Exercise) %>%
  rename(`EBA Name` = Name, Name = Name_Clean) %>%
  mutate(Country = ifelse(ISO2 == "XX", "Other", countrycode(ISO2, origin = "iso2c", destination = "country.name", custom_match = c("XX" = "Other")))) %>% 
  dplyr::select(Name, `EBA Name`, ISO2, Country, Bank_ID, LEI, Exercise, Sample) %>%
  distinct() %>%
  mutate(Sample = str_replace(Sample, "1", "X")) %>%
  arrange(Exercise) %>% 
  pivot_wider(names_from = Exercise, values_from = Sample) %>%
  mutate(across(matches("\\d{4}"), ~ ifelse(is.na(.) | . == "NA", NA, .))) %>%
  arrange(ISO2, Name) %>% 
  rename(`TR 2020 Summer`=`TR 2020 AASummer`)

## Final Matches
final_matches = read_xlsx("Meta/Total_Metadata_App.xlsx", sheet = "Final_Matches") %>% 
  dplyr::select(-TR_2014) %>% 
  rename_with(~str_replace(., "_A", " Autumn"), matches("TR_2020_A")) %>%
  rename_with(~str_replace(., "_S", " Summer"), matches("TR_2020_S")) %>%
  rename_all(~str_replace_all(., "_", " ")) %>% 
  arrange(as.numeric(str_extract(`Common Item`, "\\d{1,3}")))

# item_map <- read_xlsx("Meta/Parameters.xlsx", sheet = "Item_Correspondence") %>%
#   rename_with(~ str_replace(., "ST_", "Stress Test "), matches("ST_")) %>%
#   rename_with(~ str_replace(., "TR_", "Transparency "), matches("TR_")) %>%
#   mutate(across(-Common_Item, ~ ifelse(is.na(.), "", .))) %>%
#   rename(`Common Item` = "Common_Item")

## All_Items
all_items = read_xlsx("Meta/Total_Metadata_App.xlsx", sheet = "All_Items", col_types = rep("text", 32))  

## All worksheets in Metadata App
st_dict = read_xlsx("Meta/Metadata_DB.xlsx", sheet = "ST") 
tr_dict = read_xlsx("Meta/Metadata_DB.xlsx", sheet = "TE")
ssm_dict = read_xlsx("Meta/Metadata_DB.xlsx", sheet = "SSM")

common_exposures = read_xlsx("Meta/Total_Metadata_App.xlsx", sheet = "Correspondence - ST_TR", range = "M34:P111")  %>% 
  arrange(`Common Exposure`, `Exposure ST`, `Exposure TR`) %>% 
  dplyr::select(`Common Exposure`, everything())

## Footnotes and All other banks in metadata (overview)
footnotes = read_xlsx("Meta/Total_Metadata_App.xlsx", sheet = "Metadata", range = "U111:Y319")

## Other banks
other_banks <- read_xlsx("Meta/Total_Metadata_App.xlsx", sheet = "Metadata", range = "U4:BF95") %>% ## Colnames...
  mutate(Country = ifelse(ISO2 == "XX", "Other", countrycode(ISO2, origin = "iso2c", destination = "country.name", custom_match = c("XX" = "Other")))) %>%
  mutate(across(matches("\\d{4}"), ~ str_replace(., "1", "X"))) %>%
  mutate(across(matches("\\d{4}"), ~ ifelse(is.na(.) | . == "NA", NA, .))) %>%
  dplyr::select(Name, ISO2, Country, Bank_ID, LEI, everything()) 


## Markdown for BDS div(class = "markdown-content", br(), includeMarkdown(includeMarkdownSection("The Bank Distress Event Dataset")))

# ##------------------------------------
# ## SSM dictionary
# 
# ssm_exercise = c(2021, 2023, 2025)
# 
# ssm1_range = ssm_dict$Range_SSM001_SSM002_Final[which(!is.na(ssm_dict$Range_SSM001_SSM002_Final))]
# names(ssm1_range) = str_to_title(ssm_dict$Range_SSM001_SSM002[which(!is.na(ssm_dict$Range_SSM001_SSM002))])
# 
# ssm3_range = ssm_dict$Range_SSM003_SSM004_Final[which(!is.na(ssm_dict$Range_SSM003_SSM004_Final))]
# names(ssm3_range) = str_to_title(ssm_dict$Range_SSM003_SSM004[which(!is.na(ssm_dict$Range_SSM003_SSM004))])
# 
# ssm5_range = ssm_dict$Range_SSM005_SSM006_Final[which(!is.na(ssm_dict$Range_SSM005_SSM006_Final))]
# names(ssm5_range) = str_to_title(ssm_dict$Range_SSM005_SSM006[which(!is.na(ssm_dict$Range_SSM005_SSM006))])

# final_struct = bk_struct %>% 
#   dplyr::select(Common_Item, Exercise) %>% 
#   distinct()

# final_struct = left_join(final_struct, itm_struct, by = "Common_Item", relationship = "many-to-many")
# 
# ## Bank structure
# st_structure = fst::read_fst("Meta/Original Data/Total_Stress_Tests.fst") %>%
#   dplyr::select(Bank_ID, Common_Item, ISO2, Exercise, Scenario, Exposure, Country, Maturity, Status, Portfolio,
#                 Perf_Status, IFRS9_Stages, Perf_Forborne) %>%
#   dplyr::select(where(~!all(is.na(.x)))) %>%
#   distinct()
# ssm_structure = fst::read_fst("Meta/Original Data/Total_SSM.fst") %>%
#   dplyr::select(Bank_ID, Common_Item, ISO2, Exercise, Scenario) %>%
#   dplyr::select(where(~!all(is.na(.x)))) %>%
#   distinct()
# tr_structure = fst::read_fst("Meta/Original Data/Total_Transparency.fst") %>%
#   dplyr::select(Bank_ID, Common_Item, ISO2, Exercise, Exposure, Country, Maturity, Status, Portfolio,
#                 MKT_Risk, MKT_ModProd, Perf_Status, Assets_FV, Assets_Stages, Accounting_Portfolio,
#                 NACE_Codes, Financial_Instruments, Perf_Forborne) %>%
#   dplyr::select(where(~!all(is.na(.x)))) %>%
#   distinct()
# plc_structure = fst::read_fst("Meta/Original Data/Merged Datasets/EBA_PLC.fst") %>%
#   dplyr::select(Bank_ID, Common_Item, ISO2, Exercise, Scenario, Exposure, Country, Maturity, Status, Portfolio,
#                 Assets_FV, Assets_Stages, Financial_Instruments, Common_Exposure) %>%
#   dplyr::select(where(~!all(is.na(.x)))) %>%
#   distinct()
# market_structure = fst::read_fst("Meta/Original Data/Merged Datasets/EBA_Market.fst") %>%
#   dplyr::select(Bank_ID, Common_Item, ISO2, Exercise, Exposure, Country, Maturity, Status, Portfolio,
#                 MKT_Risk, MKT_ModProd, Common_Exposure) %>%
#   dplyr::select(where(~!all(is.na(.x)))) %>%
#   distinct()
# sovereign_structure = fst::read_fst("Meta/Original Data/Merged Datasets/EBA_Sovereign.fst") %>%
#   dplyr::select(Bank_ID, Common_Item, ISO2, Exercise, Scenario, Exposure, Country, Maturity, Status, Portfolio,
#                 Accounting_Portfolio, Common_Exposure) %>%
#   dplyr::select(where(~!all(is.na(.x)))) %>%
#   distinct()
# 
# exposure_structure = fst::read_fst("Meta/Original Data/Merged Datasets/EBA_Exposure.fst") %>%
#   dplyr::select(Bank_ID, Common_Item, ISO2, Exercise, Exposure, Country, Maturity, Status, Portfolio,
#                 Perf_Status, NACE_Codes, Perf_Forborne, IFRS9_Stages) %>%
#   dplyr::select(where(~!all(is.na(.x)))) %>%
#   distinct()
# 
# ## Bank, Countries, Exercises
# st_structure_bk = st_structure %>%
#   mutate(DB = "ST") %>%
#   dplyr::select(DB, ISO2, Bank_ID, Exercise, Common_Item) %>%
#   distinct()
# ssm_structure_bk = ssm_structure %>%
#   mutate(DB = "SSM") %>%
#   dplyr::select(DB, ISO2, Bank_ID, Exercise, Common_Item) %>%
#   distinct()
# tr_structure_bk = tr_structure %>%
#   mutate(DB = "TR") %>%
#   dplyr::select(DB, ISO2, Bank_ID, Exercise, Common_Item) %>%
#   distinct()
# plc_structure_bk = plc_structure %>%
#   mutate(DB = "PLC") %>%
#   dplyr::select(DB, ISO2, Bank_ID, Exercise, Common_Item) %>%
#   distinct()
# market_structure_bk = market_structure %>%
#   mutate(DB = "MKT") %>%
#   dplyr::select(DB, ISO2, Bank_ID, Exercise, Common_Item) %>%
#   distinct()
# sovereign_structure_bk = sovereign_structure %>%
#   mutate(DB = "SOV") %>%
#   dplyr::select(DB, ISO2, Bank_ID, Exercise, Common_Item) %>%
#   distinct()
# exposure_structure_bk = exposure_structure %>%
#   mutate(DB = "EXP") %>%
#   dplyr::select(DB, ISO2, Bank_ID, Exercise, Common_Item) %>%
#   distinct()
# bk_struct = rbind(st_structure_bk, ssm_structure_bk, tr_structure_bk,
#                  plc_structure_bk, market_structure_bk, sovereign_structure_bk, exposure_structure_bk)
# 
# names = read_xlsx("Meta/Metadata_DB.xlsx", sheet = "Dictionary")
# 
# bk_struct = left_join(bk_struct, names, by = c("Bank_ID", "ISO2"))
# write.fst(bk_struct, "bank_structure.fst", compress = 100)
# 
# ## Items and the reste
# st_structure_bk = st_structure %>%
#   # mutate(DB = "ST") %>%
#   dplyr::select(-c(ISO2, Bank_ID, Exercise)) %>%
#   distinct()
# ssm_structure_bk = ssm_structure %>%
#   # mutate(DB = "SSM") %>%
#   dplyr::select(-c(ISO2, Bank_ID, Exercise)) %>%
#   distinct()
# tr_structure_bk = tr_structure %>%
#   # mutate(DB = "TR") %>%
#   dplyr::select(-c(ISO2, Bank_ID, Exercise)) %>%
#   distinct()
# plc_structure_bk = plc_structure %>%
#   # mutate(DB = "PLC") %>%
#   dplyr::select(-c(ISO2, Bank_ID, Exercise)) %>%
#   distinct()
# market_structure_bk = market_structure %>%
#   # mutate(DB = "MKT") %>%
#   dplyr::select(-c(ISO2, Bank_ID, Exercise)) %>%
#   distinct()
# sovereign_structure_bk = sovereign_structure %>%
#   # mutate(DB = "SOV") %>%
#   dplyr::select(-c(ISO2, Bank_ID, Exercise)) %>%
#   distinct()
# exposure_structure_bk = exposure_structure %>%
#   # mutate(DB = "EXP") %>%
#   dplyr::select(-c(ISO2, Bank_ID, Exercise)) %>%
#   distinct()
# bk_struct = data.table::rbindlist(list(st_structure_bk, ssm_structure_bk, tr_structure_bk,
#                   plc_structure_bk, market_structure_bk, sovereign_structure_bk, exposure_structure_bk), fill = T)
# 
# write.fst(bk_struct, "item_structure.fst", compress = 100)

### Utils for visualisation 
scenarnames <- c(0, 1, 11, 2, 3, 4)
cnames <- c("No breakdown", "Actual", "Actual restated", "Baseline", "Adverse", "Adverse Sovereign Shock")
names(scenarnames) <- cnames

metadata_countries <- read_xlsx("Meta/Total_Metadata_App.xlsx", sheet = "TR_Dictionary") %>% 
  dplyr::select(Label_Country_Final, Value_Country_Final, ISO_Country_Final)

bank_names <- read_xlsx("Meta/Total_Metadata_App.xlsx", sheet = "Bank_ID") %>%
  dplyr::select(Name_Clean, Bank_ID) %>%
  rename(Name = Name_Clean) %>% 
  distinct()

exposures_names <- read_xlsx("Meta/Metadata_DB.xlsx", sheet = "Common_Exposure")
labels <- read_xlsx("Meta/Metadata_DB.xlsx", sheet = "Labels")

# Load data
# chart_db <- read.fst("Meta/Original Data/chart_db.fst")

# Helper function to convert YYYYMM to YYYY QX
convert_to_quarter <- function(period) {
  year <- as.numeric(substr(period, 1, 4))
  month <- as.numeric(substr(period, 5, 6))
  quarter <- ceiling(month / 3)
  paste0(year, " Q", quarter)
}

