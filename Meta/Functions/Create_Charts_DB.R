## (c) Quentin Bro de Comères 
## Visualisation package for the Galaxy app

rm(list = ls())
## Get current path for wd
path <- dirname(rstudioapi::getSourceEditorContext()$path)

setwd(path)
getwd()

# Function to check for and install missing packages
install_if_missing <- function(packages) {
  new_packages <- packages[!(packages %in% installed.packages()[, "Package"])]
  if (length(new_packages)) {
    install.packages(new_packages, dependencies = TRUE)
  }
}

# List of required packages
required_packages <- c(
  "tidyverse", "readr", "readxl", "writexl", "data.table", "zoo", "httr",
  "fst", "arrow", "beepr", "wbstats", "openxlsx", "tsutils", "lubridate", "pracma",
  "sandwich", "ggpubr", "grid", "countrycode", "ggraph", "igraph", "ggridges",
  "visNetwork", "plotly", "treemapify"
)

# Install missing packages
install_if_missing(required_packages)

# Load all required packages
lapply(required_packages, library, character.only = TRUE)

## To get the significance stars
significance <- function(pval) {
  res <- NULL
  if (pval < .1 & pval >= .05) {
    res <- "*"
  } else if (pval < .05 & pval >= .01) {
    res <- "**"
  } else if (pval < .01) {
    res <- "***"
  } else {
    res <- NA
  }
  return(res)
}

cbindlist <- function(list_obj) {
  lengths <- unlist(lapply(list_obj, length))
  NA_lengths <- sapply(lengths, function(x) max(lengths) - x)
  cols <- names(list_obj)
  out <- data.frame(sapply(1:length(list_obj), function(x) c(list_obj[[x]], rep(NA, NA_lengths[x]))))
  colnames(out) <- cols
  return(out)
}

## for colours (hex codes are recognised)
## https://www.color-hex.com/popular-colors.php

## --------------------------
### Metadata
scenarnames <- c(0,1,11,2,3,4)

cnames = c("No breakdown", 
           "Actual",
           "Actual restated",
           "Baseline",
           "Adverse",
           "Adverse Sovereign Shock")

names(scenarnames) = cnames

metadata_countries <- read_xlsx("../Total_Metadata_App.xlsx", sheet = "TR_Dictionary") %>% 
  dplyr::select(Label_Country_Final, Value_Country_Final, ISO_Country_Final)

## ---------------------------------------------------------------------------
## Memos

EU <- c(
  "AT", "BE", "BG", "HR", "CY", "CZ", "DK", "EE", "FI",
  "FR", "DE", "GR", "HU", "IE", "IT", "LV", "LT", "LU", "MT",
  "NL", "PL", "PT", "RO", "SK", "SI", "ES", "SE"
) # "GB" to include the UK

EA <- c(
  "AT", "BE", "HR", "CY", "EE", "FI",
  "FR", "DE", "GR", "IE", "IT", "LV", "LT", "LU", "MT",
  "NL", "PT", "SK", "SI", "ES"
)

bank_names <- read_xlsx("../Total_Metadata_App.xlsx", sheet = "Bank_ID") %>%
  dplyr::select(Name_Clean, Bank_ID) %>%
  rename(Name = Name_Clean) %>% 
  distinct() 

## To improve memory efficiency, most columns are converted to factors
## We first need to convert them back to numeric or character
categorical_cols <- c("TP", "ISO2", "rel_period", "Common_Exposure", "Framework")

numeric_cols <- c(
  "Period", "Scenario", "Country", "Maturity",
  "Category", "Status", "Portfolio", "Perf_Status", "IFRS9_Stages",
  "Perf_Forborne", "Exposure", "Country_Rank",
  "MKT_Risk", "MKT_ModProd", "Assets_FV", "Assets_Stages", "Accounting_Portfolio",
  "NACE_Codes", "Financial_Instruments", "Fin_End_Year", "retail_sample", "retail_sample_ex",
  "CR_exp_moratoria", "CR_guarantees", "IFRS9_Stages"
)

## 1. Import relevant datasets (once and for all)

## PLC
dt_pnl <- read.fst("../Original Data/Merged Datasets/EBA_PLC.fst") %>%
  dplyr::select(-LEI) %>%
  distinct() %>%
  mutate(across(intersect(categorical_cols, colnames(.)), ~ as.character(.))) %>%
  mutate(across(intersect(numeric_cols, colnames(.)), ~ as.numeric(as.character(.)))) %>% 
  dplyr::select(-Name) %>% 
  distinct()

### Exposures
dt_exp <- read.fst("../Original Data/Merged Datasets/EBA_Exposure.fst") %>%
  dplyr::select(-LEI) %>%
  distinct() %>%
  mutate(across(intersect(categorical_cols, colnames(.)), ~ as.character(.))) %>%
  mutate(across(intersect(numeric_cols, colnames(.)), ~ as.numeric(as.character(.)))) %>% 
  dplyr::select(-Name) %>% 
  distinct()

### Sovereign
dt_sov <- read.fst("../Original Data/Merged Datasets/EBA_Sovereign.fst") %>%
  dplyr::select(-LEI) %>%
  distinct() %>%
  mutate(across(intersect(categorical_cols, colnames(.)), ~ as.character(.))) %>%
  mutate(across(intersect(numeric_cols, colnames(.)), ~ as.numeric(as.character(.))))%>% 
  dplyr::select(-Name) %>% 
  distinct()

##--------------------------------
## Building datasets for charts


#### @@
#### I- Time series items and density ####
#### @@

## PLC 
# Annual Factors
# Q1	4
# Q2	2
# Q3	1.33
# Q4	1

items <- c(
  "ITM_113", "ITM_349", "ITM_67", "ITM_66", "ITM_170", "ITM_7", "ITM_128", "ITM_122", "ITM_36", "ITM_123", "ITM_124", ## Capital Adequacy
  "ITM_46", "ITM_47", "ITM_48", "ITM_4", "ITM_538", "ITM_65", "ITM_54", "ITM_433", "ITM_434", "ITM_452", "ITM_50", "ITM_2", "ITM_3" ## Profitability
)

## Need to compute fully loaded for 2014, 2016, 2018 exercises
# ITM_67	1690802
# ITM_87	1690818
# ITM_100	1690827
# ITM_107	1690835
# ITM_87	1690818
# ITM_109	1690837
# ITM_111	1690839
# ITM_114	1690841
# ITM_109	1690837
# ITM_117	1690844
# ITM_35	1690845
# ITM_174	1690846
# ITM_67	993402
# ITM_87	993419
# ITM_100	993426
# ITM_107	993430
# ITM_87	993419
# ITM_114	993433
# ITM_35	993434

add_1618 = dt_pnl %>% 
  dplyr::select(-Item, -Stress_Test_Item, -Transparency_Item) %>% 
  filter(Common_Item %in% c("ITM_35", "ITM_36", "ITM_175", "ITM_100", "ITM_107", "ITM_109", "ITM_114", "ITM_117", "ITM_174", "ITM_67", "ITM_87", "ITM_122", "ITM_111", "ITM_123", "ITM_124") & Category %in% c(1,NA)) %>%
  mutate(ST2025 = ifelse(Framework == "ST" & Exercise == 2025 & Scenario != 1, 1, 0)) %>% 
  pivot_wider(names_from = "Common_Item", values_from = "Amount") %>% 
  mutate(across(matches("ITM_"), ~ifelse(is.na(.), 0, .))) %>%
  rowwise() %>% 
  mutate(ITM_122 = ifelse(ST2025 == 0, ITM_67 - ITM_87 - ITM_100 + min(0, ITM_107 + ITM_87 - ITM_109 - ITM_111 + min(0, ITM_114 + ITM_109 - ITM_117)), ITM_122)) %>%
  mutate(ITM_123 = ifelse(ST2025 == 0, ITM_67 - ITM_100 + ITM_107 - ITM_109 - ITM_111 + min(0, ITM_114 + ITM_109 - ITM_117), ITM_123)) %>%
  mutate(ITM_124 = ifelse(ST2025 == 0, ITM_67 - ITM_100 + ITM_107 - ITM_111 + ITM_114 - ITM_117, ITM_124)) %>% 
  mutate(ITM_36 = ifelse(ST2025 == 0, ITM_35 - ITM_174, ITM_36)) %>%
  # mutate(ITM_CET1 = ITM_122/ITM_36) %>% 
  # mutate(ITM_T1 = ITM_123/ITM_36) %>% 
  # mutate(ITM_Total = ITM_124/ITM_36) %>% 
  pivot_longer(matches("ITM_"), names_to = "Common_Item", values_to = "Amount") %>% 
  filter(Common_Item %in% c("ITM_122", "ITM_123", "ITM_124", "ITM_36")) %>% 
  distinct() %>% 
  ungroup() %>% 
  dplyr::select(-ST2025)

dt_pnl = dt_pnl %>% 
  filter(!(Common_Item %in% c("ITM_122", "ITM_123", "ITM_124", "ITM_36")))

dt_pnl = plyr::rbind.fill(dt_pnl, add_1618) ## Adding FL values

## Country version
tr_ratios_orig_countries <- dt_pnl %>%
  dplyr::select(-retail_sample, -retail_sample_ex) %>%
  distinct() %>%
  arrange(Framework, Bank_ID, Exercise, Period, Common_Item, Item) %>%
  group_by(Framework, ISO2, Common_Item, Period, rel_period, Exercise, Scenario) %>%
  mutate(Amount = sum(Amount, na.rm = T)) %>%
  ungroup() %>%
  mutate(Bank_ID = NA) %>% 
  distinct() 

## Bank Version
tr_ratios_orig_banks <- dt_pnl %>%
  dplyr::select(-retail_sample, -retail_sample_ex) %>%
  distinct() %>%
  arrange(Framework, Bank_ID, Exercise, Period, Common_Item, Item) %>%
  group_by(Framework, ISO2, Common_Item, Period, rel_period, Exercise, Scenario, Bank_ID) %>%
  mutate(Amount = sum(Amount, na.rm = T)) %>%
  ungroup() %>%
  distinct() 

tr_ratios_orig = rbind(tr_ratios_orig_countries, tr_ratios_orig_banks) %>% 
  filter(Common_Item %in% c(items)) %>%
  pivot_wider(id_cols = -c(Stress_Test_Item, Transparency_Item, Item), names_from = "Common_Item", values_from = "Amount") %>%
  mutate(ITM_3 = ifelse(Framework == "ST", ITM_3, -1 * ITM_3)) %>% ## Impairments reported as negative for ST, not TR
  mutate(ITM_170 = coalesce(ITM_170, ITM_7)) %>%
  mutate(ITM_130 = ITM_113 / ITM_128) %>% ## Transitional Leverage Ratio
  mutate(ITM_67_1 = ITM_67) %>%
  mutate(across(c(ITM_113, ITM_349, ITM_67, ITM_66, ITM_128), ~ . / ITM_170)) %>%
  rename(ITM_120 = ITM_113) %>% ## Transitional Tier 1 Capital Ratio
  rename(ITM_314 = ITM_349) %>% ## Core Tier 1 capital ratio (%) - Correspondence with 2011 Exercise only
  rename(ITM_119 = ITM_67) %>% ## Transitional Common Equity Tier 1 Capital Ratio
  rename(ITM_67 = ITM_67_1) %>% ## Transitional Common Equity Tier 1 Capital Ratio
  rename(ITM_121 = ITM_66) %>% ## Transitional Total Capital Ratio
  mutate(ITM_46 = ifelse(Framework == "TR", ITM_47 - ITM_48, ITM_46)) %>% ## NII
  mutate(ITM_54 = ifelse(Period < 201412, NA, ITM_54)) %>% ## Total Operating Income is computed weirdly before 2014
  mutate(ITM_54 = ifelse(Exercise == 2014 & Scenario %in% c(2,3), NA, ITM_54)) %>% ## Total Operating Income is computed weirdly before 2014
  mutate(ITM_120 = coalesce(ITM_120, ITM_314)) %>%
  mutate(ITM_NII = ITM_46) %>%
  mutate(across(c(ITM_46, ITM_2, ITM_3, ITM_50), ~ . / ITM_54)) %>% ## Trading Income, NFCI/Net Operating Income
  mutate(across(c(ITM_433, ITM_434, ITM_452), ~ ifelse(is.na(.), 0, .))) %>%
  mutate(ITM_CIR = (ITM_433 + ITM_434 + ITM_452) / ITM_54) %>% ## Cost to Income
  mutate(ITM_CETFL = ITM_122 / ITM_36) %>%
  mutate(ITM_TFL = ITM_123 / ITM_36) %>%
  mutate(ITM_TOTFL = ITM_124 / ITM_36) %>%
  mutate(Month = month(as.Date(paste0(Period, "01"), format = "%Y%m%d"))) %>%
  mutate(ITM_4_ANN = case_when( ## Profit/losses must be annualised to compute ROA and ROE
    Month == 3 ~ ITM_4 * 4,
    Month == 6 ~ ITM_4 * 2,
    Month == 9 ~ ITM_4 * 4 / 3,
    Month == 12 ~ ITM_4 * 1,
    TRUE ~ ITM_4
  )) %>%
  mutate(Year = as.numeric(str_extract(Period, "^\\d{4}"))) %>% ## This is to compute YTD average of denominator (note: the EBA considers previous December too, we do not)
  mutate(across(c(ITM_538, ITM_65), ~ ifelse(Month == 3, ., NA), .names = "{.col}_X3")) %>%
  mutate(across(c(ITM_538, ITM_65), ~ ifelse(Month %in% c(3, 6), ., NA), .names = "{.col}_X6")) %>%
  mutate(across(c(ITM_538, ITM_65), ~ ifelse(Month %in% c(3, 6, 9), ., NA), .names = "{.col}_X9")) %>%
  mutate(across(c(ITM_538, ITM_65), ~ ifelse(Month %in% c(3, 6, 9, 12), ., NA), .names = "{.col}_X12")) %>%
  group_by(Framework, ISO2, rel_period, Scenario, Year) %>%
  mutate(across(matches("_X3|_X6|_X9|_X12"), ~ mean(., na.rm = T))) %>% ## Computing YTD average
  ungroup() %>%
  mutate(ITM_538_YTD = ifelse(Month == 3, ITM_538_X3,
                              ifelse(Month == 6, ITM_538_X6,
                                     ifelse(Month == 9, ITM_538_X9,
                                            ifelse(Month == 12, ITM_538_X12, NA)
                                     )
                              )
  )) %>%
  mutate(ITM_65_YTD = ifelse(Month == 3, ITM_65_X3,
                             ifelse(Month == 6, ITM_65_X6,
                                    ifelse(Month == 9, ITM_65_X9,
                                           ifelse(Month == 12, ITM_65_X12, NA)
                                    )
                             )
  )) %>%
  mutate(ITM_ROE = ITM_4_ANN / ITM_538_YTD) %>% ## ROE
  mutate(ITM_ROA = ITM_4_ANN / ITM_65_YTD) %>% ## ROA
  dplyr::select(
    Framework, Exercise, Period, Scenario, rel_period, Bank_ID, ISO2, TP, ITM_120, ITM_121, ITM_314, ITM_119, ITM_46, ITM_4, ITM_130, ITM_67, ITM_170,
    ITM_538, ITM_65, ITM_ROE, ITM_ROA, ITM_CIR, ITM_54, ITM_2, ITM_50, ITM_NII, ITM_3, ITM_CETFL, ITM_TFL, ITM_TOTFL
  ) %>%
  pivot_longer(c(
    ITM_120, ITM_314, ITM_119, ITM_46, ITM_4, ITM_121, ITM_130, ITM_67, ITM_170,
    ITM_538, ITM_65, ITM_ROE, ITM_ROA, ITM_CIR, ITM_54, ITM_2, ITM_50, ITM_NII, ITM_3, ITM_CETFL, ITM_TFL, ITM_TOTFL
  ), names_to = "Common_Item", values_to = "Amount") %>%
  # mutate(TP = "PLC", Scenario = 1, rel_period = "Realised") %>%
  distinct()

overlap <- tr_ratios_orig %>% ## ST and TR overlaps
  filter(Period %in% c(201512, 201712, 202012, 202212, 202412) & Scenario %in% c(1, NA)) %>%
  mutate(TP = "PLC", Scenario = 1, rel_period = "Realised") %>%
  filter(!is.na(Amount)) %>%
  # mutate(Amount = ifelse(is.na(Amount), 0, Amount)) %>%
  pivot_wider(names_from = "Framework", values_from = "Amount") %>%
  arrange(ISO2, Period, Common_Item) %>%
  mutate(Amount = coalesce(TR, ST, SSM)) %>% ## Priority given to TR, then ST
  mutate(Framework = "TR") %>%
  dplyr::select(-TR, -ST, -SSM) %>%
  distinct()

tr_ratios <- tr_ratios_orig %>%
  mutate(bx = ifelse(Period %in% c(201512, 201712, 202012, 202212, 202412) & Scenario %in% c(1, NA), 1, 0)) %>%
  filter(bx == 0) %>%
  dplyr::select(-bx)

tr_ratios <- rbind(tr_ratios, overlap) %>%
  arrange(ISO2, Period, Common_Item) %>%
  dplyr::select(where(~ !all(is.na(.x)))) %>%
  dplyr::select(
    Framework, ISO2, Exercise, Period, rel_period, Common_Item, Bank_ID, 
    Scenario, Amount
  ) %>%
  distinct() %>%
  mutate(Scenario = ifelse(is.na(Scenario), 1, Scenario)) %>%
  mutate(Amount = ifelse(Common_Item == "ITM_130" & Period == 201512 & Amount == 0, NA, Amount)) %>%
  mutate(Amount = ifelse(Common_Item == "ITM_CIR" & Period < 201412 & Amount == 0, NA, Amount)) %>%
  mutate(Amount = ifelse(Common_Item %in% c("ITM_65", "ITM_ROA") & Period < 201809 & Amount == 0, NA, Amount)) %>%
  mutate(Amount = ifelse(Common_Item %in% c("ITM_538", "ITM_ROE") & Period < 202003 & Amount == 0, NA, Amount)) %>% 
  filter(!is.na(Amount))

## Adverse sovereign shock (Bank specific)
adv_sov <- dt_pnl %>% 
  filter(Common_Item == "ITM_119" & Scenario == 4) %>% 
  dplyr::select(Framework, ISO2, Exercise, Period, rel_period, Common_Item, Bank_ID, Scenario, Amount)

tr_ratios = rbind(tr_ratios, adv_sov)

tr_ratios = left_join(tr_ratios, bank_names, by = c("Bank_ID"), relationship = "many-to-many") %>% 
  mutate(Country = ifelse(ISO2 != "OT", countrycode(ISO2, origin = "iso2c", destination = "country.name"),
                          ifelse(ISO2 == "OT", "Other", ISO2)
  ))

##------------------------
## Export final datasets
tr_ratios_export = tr_ratios %>% 
  mutate(DB = "tr_ratios")

## %% Chart

tr_ratios = tr_ratios_export %>% 
  dplyr::select(-DB)

### %% Time series
lbl = "ITM_CETFL"
iso = "Ireland"
bank = NULL

labels <- read_xlsx("../Metadata_DB.xlsx", sheet = "Labels")

lbls = labels %>%
  filter(Common_Item == lbl)

annual = lbls$Annual
measure = lbls$Measure
ratio = lbls$Ratio

## Filtering
data = tr_ratios %>%
  filter(Common_Item %in% lbl) %>%
  filter(if(!is.null(iso)){Country%in%iso}else{Country%in%Country}) %>%
  filter(if(!is.null(bank)){Name%in%bank}else{is.na(Name)}) %>%
  mutate(Scenario = factor(Scenario, scenarnames, names(scenarnames)))

## Accounting for restatement in 2018 and 2025 exercises
if("Actual restated" %in% unique(data$Scenario)){
  data = data %>%
    pivot_wider(names_from = Scenario, values_from = Amount) %>%
    mutate(Actual = ifelse(Period %in% c(201712, 202412) & !is.na(`Actual restated`), `Actual restated`, Actual)) %>%
    mutate(Actual = ifelse(Period %in% c(201712, 202412) & Framework == "TR", NA, Actual)) %>%
    pivot_longer(-c(Framework, ISO2, Exercise, Period, rel_period, Common_Item, Bank_ID, Name, Country), names_to = "Scenario", values_to = "Amount")
}

scale <- 10^(floor(log10(median(data$Amount, na.rm = T))) - 1)

if(ratio == 1){
  data$Amount = data$Amount*100
  scale = scale*100
}
if(annual ==1){
  data = data %>%
    mutate(Amount = case_when(
      str_extract(Period, "\\d{2}$") == "03" ~ Amount * 4,
      str_extract(Period, "\\d{2}$") == "06" ~ Amount * 2,
      str_extract(Period, "\\d{2}$") == "09" ~ Amount * 4 / 3,
      str_extract(Period, "\\d{2}$") == "12" ~ Amount * 1,
      TRUE ~ Amount
    ))
}

data = data %>%
  filter(Scenario != "Actual restated") %>%
  # mutate(Amount = ifelse(Period %in% c(201712, 202412) & Framework == "TR", NA, Amount)) %>% ## Avoid double counting between TR and ST
  # mutate(Amount = ifelse(("Actual restated" %in% scens) & Period %in% c(201712, 202412) & Scenario == "Actual", NA, Amount)) %>% ## Avoid double counting between restated and nonrestated when applicable
  mutate(Scenario = ifelse(is.na(Scenario), "Actual", as.character(Scenario))) %>%
  pivot_wider(names_from = "Scenario", values_from = "Amount") %>%
  add_column(!!!cnames[setdiff(names(cnames), names(.))]) %>%
  # mutate(Actual = ifelse(!is.na(`Actual restated`), NA, Actual)) %>%
  # mutate(Actual = coalesce(Actual, `Actual restated`)) %>%
  mutate(Actual_ST = ifelse(rel_period == "Realised" & !is.na(Actual), Actual, NA)) %>%
  mutate(across(c(Baseline, Adverse), ~ ifelse(rel_period == "Realised" & is.na(.), Actual, .))) %>%
  mutate(Framrel = ifelse((!is.na(Actual)), 1, NA)) %>%
  mutate(Framework = ifelse(!is.na(Actual_ST), "ST", Framework)) %>%
  mutate(Exercise = ifelse(Framework == "TR", paste0("TR", Exercise), Exercise)) %>%
  mutate(Group = ifelse(Framework == "ST", paste(Framework, Exercise), NA))

bank = if(is.null(bank)){iso}else{bank}

chart <-
  data %>%
  mutate(Date = as.Date(paste0(Period, "01"), format = "%Y%m%d")) %>%
  ggplot(aes(x = Date)) +
  geom_line(aes(y = Baseline, colour = "Baseline", group = Group)) +
  geom_line(aes(y = Adverse, colour = "Adverse", group = Group)) +
  geom_point(aes(y = Baseline, colour = "Baseline", text = paste("Period:", year(Date), "<br>Amount:", paste0(format(round(Baseline, 2), big.mark = ","), measure)))) +
  geom_point(aes(y = Adverse, colour = "Adverse", text = paste("Period:", year(Date), "<br>Amount:", paste0(format(round(Adverse, 2), big.mark = ","), measure)))) +
  geom_point(aes(y = Actual, colour = "Realised", text = paste("Period:", as.yearqtr(Date), "<br>Amount:", paste0(format(round(Actual, 2), big.mark = ","), measure)))) +
  geom_line(aes(y = Actual, colour = "Realised", group = Framrel), linetype = "dotdash") +
  # geom_point(aes(y = `Actual restated`, colour = "Realised (Restated)")) +
  geom_point(aes(y = Actual_ST, text = paste("Period:", as.yearqtr(Date), "<br>Amount:", paste0(format(round(Actual_ST, 2), big.mark = ","), measure))), colour = "#642FAC") +
  labs(
    x = "Date",
    y = lbls$Label,
    title = lbls$Label,
    subtitle = bank,
    # caption = "Reported values from EBA Transparency Exercise."
  ) +
  theme_classic() +
  theme(
    axis.text.x = element_text(angle = 45, vjust = 1, hjust = 1),
    axis.title.x = element_text(vjust = 1),
    text = element_text(size = 9),
    legend.position = "bottom"
  ) +
  scale_colour_manual("Scenario",
                      breaks = c("Realised", "Realised (Restated)", "Sovereign Shock", "Baseline", "Adverse"),
                      values = c("#7e878e", "#FCAF17", "#D2E288", "#007DC5", "#D12E7C", "#7e878e")
  ) +
  geom_text(aes(y = Actual_ST, label = Exercise),
            nudge_y = 0.2 * scale, hjust = .7, check_overlap = T,
            size = 3
  )

if (sum(data$`Adverse Sovereign Shock`, na.rm = T) == 0|is.null(data$`Adverse Sovereign Shock`)) {
  chart <- chart +
    guides(colour = guide_legend(
      title.position = "top", title.hjust = .5,
      override.aes = list(
        shape = c(16, 16, 16),
        linetype = c(1, 1, 1)
      )
    ))
} else {
  chart <- chart +
    geom_point(aes(y = `Adverse Sovereign Shock`, colour = "Sovereign Shock", text = paste("Period:", year(Date), "<br>Amount:", paste0(format(round(`Adverse Sovereign Shock`, 2), big.mark = ","), mes)))) +
    guides(colour = guide_legend(
      title.position = "top", title.hjust = .5,
      override.aes = list(
        shape = c(16, 16, 16, 16),
        linetype = c(1, NA, 1, 1)
      )
    ))
}

if (lbl %in% c("ITM_119", "ITM_CETFL")) {
  chart <- chart +
    geom_hline(yintercept = c(5, 6, 5.5, 8), linewidth = .5, linetype = "dotted", colour = "#7e878e") +
    annotate("text",
             x = as.Date("2025-12-01"), y = c(5.85, 4.85, 5.35, 7.85),
             label = c(
               "2010 Pass/Fail Threshold", "2011 Pass/Fail Threshold",
               "2014 Pass/Fail Threshold (Adverse)", "2014 Pass/Fail Threshold (Baseline)"
             ), color = "#7e878e",
             size = 3, hjust = 1
    )
}

plotly::ggplotly(chart, tooltip = "text")

##-----------------
## %% Chart

### %% Density
lbl = "ITM_119"
iso = NULL
relp = "Year3"

lbls = labels %>%
  filter(Common_Item %in% lbl)

annual = lbls$Annual
measure = lbls$Measure
ratio = lbls$Ratio

## Filtering
data = tr_ratios %>%
  filter(Common_Item %in% lbl) %>%
  filter(!is.na(Bank_ID)) %>% ## Bank-specific only
  filter(if(!is.null(iso)){Country%in%iso}else{Country%in%Country}) %>%
  mutate(Scenario = factor(Scenario, scenarnames, names(scenarnames))) %>%
  filter(Framework == "ST")

## Accounting for restatement in 2018 and 2025 exercises
if("Actual restated" %in% unique(data$Scenario)){
  data = data %>%
    pivot_wider(names_from = Scenario, values_from = Amount) %>%
    mutate(Actual = ifelse(Period %in% c(201712, 202412) & !is.na(`Actual restated`), `Actual restated`, Actual)) %>%
    mutate(Actual = ifelse(Period %in% c(201712, 202412) & Framework == "TR", NA, Actual)) %>%
    pivot_longer(-c(Framework, ISO2, Exercise, Period, rel_period, Common_Item, Bank_ID, Name, Country), names_to = "Scenario", values_to = "Amount")
}

data_1011 = data %>%
  filter(Exercise %in% c(2010,2011) & rel_period == "Year2") %>%
  mutate(rel_period = "Year3")

data = rbind(data, data_1011) %>%
  filter(Scenario %in% c("Baseline", "Adverse") & rel_period %in% relp)

if(ratio == 1){
  data$Amount = data$Amount*100
}

if(annual ==1){
  data = data %>%
    mutate(Amount = case_when(
      str_extract(Period, "\\d{2}$") == "03" ~ Amount * 4,
      str_extract(Period, "\\d{2}$") == "06" ~ Amount * 2,
      str_extract(Period, "\\d{2}$") == "09" ~ Amount * 4 / 3,
      str_extract(Period, "\\d{2}$") == "12" ~ Amount * 1,
      TRUE ~ Amount
    ))
}

if(relp == "Year3"){
  note <- paste("Total", lbls$Label, "variation after two years for 2010 and 2011 exercises, then three years. Restated values for 2018 and 2025 exercises.", sep = " ")
}else{
  note <- "Restated values for 2018 and 2025 exercises."
}

relp = paste("Year", str_extract(relp, "\\d{1}$"))

## Plot
dens_plot <-
  data %>%
  # group_by(Exercise, Scenario) %>%
  # mutate(mean = mean(Amount, na.rm = T)) %>%
  # ungroup() %>%
  ggplot() +
  geom_density_ridges(aes(x = Amount, y = as.character(Exercise), fill = Scenario), alpha = .6) +
  geom_vline(xintercept = 0, linetype = "dashed", colour = "#7e878e") +
  theme_classic() +
  labs(
    x = paste(lbls$Label, sep = " "),
    y = "Density",
    title = paste("Density Total", lbls$Label, paste0("(", relp, ")"), sep = " "),
    caption = note
  ) +
  theme(
    # axis.text.x = element_text(angle=0, vjust=1, hjust=1),
    axis.title.x = element_text(vjust = 1),
    text = element_text(size = 9),
    legend.position = "bottom",
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    strip.background = element_blank(),
    plot.title = element_text(hjust = .5),
    legend.title = element_text(hjust = .5)
  ) +
  scale_fill_manual(
    breaks = c("Adverse", "Baseline"),
    values = c("#0083A0", "#D2E288")
  ) +
  guides(fill = guide_legend(title.position = "top", title = "Scenario"))

plotly::ggplotly(dens_plot, tooltip = "text")


#### @@
#### II- Waterfalls ####
#### @@

### Extract stress test data for waterfall

## Stress Test Results (2010-2025)
ids <- read_xlsx("../Metadata_DB.xlsx", sheet = "Selection") %>% 
  dplyr::select(Common, Order, Sign, RM) %>% 
  filter(!is.na(Common)) %>% 
  rename(Common_Item = Common)

cnames = c("No breakdown", 
           "Actual",
           "Actual - Restated",
           "Baseline",
           "Adverse",
           "Adverse Sovereign Shock")

## Change to apply to ITM_79
## 2011 - ITM_343
## 2014, 2016, 2018 - ITM_78
## 2021-2025 - ITM_79

## Extracting data from dataset
st_dta = dt_pnl %>% 
  dplyr::select(-Item, -Stress_Test_Item, -Transparency_Item) %>% 
  filter(Common_Item %in% c(unique(ids$Common_Item), "ITM_35", "ITM_175", "ITM_100", "ITM_107", "ITM_109", "ITM_114", "ITM_117", "ITM_174", "ITM_67", "ITM_87", "ITM_272", "ITM_111", "ITM_343", "ITM_78", "ITM_252", "ITM_270", "ITM_387", "ITM_388") & Category %in% c(1,NA) & Framework == "ST") %>%
  mutate(ST2025 = ifelse(Framework == "ST" & Exercise == 2025 & Scenario != 1, 1, 0)) %>% 
  pivot_wider(names_from = "Common_Item", values_from = "Amount") %>% 
  mutate(across(matches("ITM_"), ~ifelse(is.na(.), 0, .))) %>%
  rowwise() %>% 
  mutate(ITM_79 = ifelse(Exercise == 2011, ITM_343, ITM_79)) %>% 
  mutate(ITM_79 = ifelse(Exercise %in% c(2014, 2016, 2018), ITM_78, ITM_79)) %>% 
  mutate(ITM_52 = ifelse(Exercise == 2014, ITM_270+ITM_271, ITM_52)) %>% 
  mutate(ITM_72 = ifelse(Exercise %in% c(2014, 2016), ITM_252+ITM_253, ITM_72)) %>% 
  mutate(ITM_122 = ifelse(ST2025 == 0, ITM_67 - ITM_87 - ITM_100 + min(0, ITM_107 + ITM_87 - ITM_109 - ITM_111 + min(0, ITM_114 + ITM_109 - ITM_117)), ITM_122)) %>%
  mutate(ITM_36 = ifelse(ST2025 == 0, ITM_35 - ITM_174, ITM_36)) %>%
  mutate(ITM_57 = ifelse(Exercise == 2010, ITM_386+ITM_387+ITM_388, ITM_57)) %>% 
  mutate(ITM_57 = ifelse(Exercise == 2011, ITM_56+ITM_272, ITM_57)) %>% 
  mutate(ITM_55 = ifelse(Exercise == 2010, ITM_387, ITM_55)) %>%
  mutate(ITM_51 = ifelse(Exercise == 2010, ITM_388, ITM_51)) %>%
  # mutate(ITM_CET1 = ITM_122/ITM_36) %>% 
  # mutate(ITM_T1 = ITM_123/ITM_36) %>% 
  # mutate(ITM_Total = ITM_124/ITM_36) %>% 
  pivot_longer(matches("ITM_"), names_to = "Common_Item", values_to = "Amount") %>% 
  filter(Common_Item %in% unique(ids$Common_Item)) %>% 
  distinct() %>% 
  ungroup() %>% 
  dplyr::select(-ST2025) %>% 
  mutate(Scenario = factor(Scenario, scenarnames, str_to_title(cnames))) %>% 
  dplyr::select(Bank_ID, Exercise, ISO2, Scenario, rel_period, Common_Item, Amount) %>%
  pivot_wider(names_from = rel_period, values_from = Amount) 

st_dta = left_join(st_dta, ids, by = "Common_Item", relationship = "many-to-many") %>% 
  group_by(Bank_ID, Exercise, Scenario, Order) %>%
  # mutate(across(c(Realised, matches("Year")), ~ sum(. * Sign))) %>%
  ungroup() %>%
  filter(RM == 0) %>%
  dplyr::select(-Sign, -RM) %>% 
  arrange(ISO2, Bank_ID, Exercise, Order, Scenario) %>%
  dplyr::select(ISO2, Bank_ID, Exercise, Common_Item, Scenario, Order, Realised, Year1, Year2, Year3) %>% 
  filter(!(Common_Item %in% c("ITM_56", "ITM_271")))

st_dta = left_join(st_dta, bank_names, by = c("Bank_ID"), relationship = "many-to-many") %>% 
  mutate(Country = ifelse(ISO2 != "OT", countrycode(ISO2, origin = "iso2c", destination = "country.name"),
                          ifelse(ISO2 == "OT", "Other", ISO2)))

st_dta_tr_bank <- st_dta %>%
  mutate(Order = paste("o", Order, sep = "_")) %>%
  pivot_longer(-c(Country, ISO2, Name, Bank_ID, Exercise, Common_Item, Scenario, Order), names_to = "rel_period", values_to = "Amount") %>%
  na.omit() %>%
  distinct() %>%
  pivot_wider(id_cols = -c(Common_Item), names_from = "Order", values_from = "Amount") %>%
  arrange(ISO2, Bank_ID, Exercise, Scenario, rel_period) %>%
  mutate(across(matches("o_"), ~ ifelse(is.na(.), 0, .))) %>%
  group_by(Bank_ID, Exercise, Scenario) %>%
  mutate(across(c(o_13, o_17, o_18, o_20, o_15, o_16, o_23), ~ ifelse(rel_period == "Year2" & Exercise > 2010, . + dplyr::lag(., 1), ifelse(rel_period == "Year3" & Exercise > 2010, . + dplyr::lag(., 1) + dplyr::lag(., 2), .)), .names = "{.col}x")) %>%
  ungroup() %>%
  mutate(o_14 = o_13x - (o_17x + o_18x + o_20x)) %>%
  mutate(o_16.5 = o_14 - (o_15x + o_16x)) %>%
  mutate(I1 = ifelse(rel_period == "Realised", o_1 / o_2 * 100, NA)) %>%
  mutate(I3 = ifelse(rel_period == "Realised", o_10 / o_11 * 100, NA)) %>%
  mutate(I2 = I1 - dplyr::lag(I1, 1)) %>%
  mutate(denom = ifelse(Scenario == "Actual - Restated", o_11, NA)) %>%
  mutate(denom = ifelse(is.na(denom), ifelse(Scenario == "Actual", o_11, NA), denom)) %>%
  mutate(denom = na.locf(denom, na.rm = F)) %>%
  mutate(I4 = (o_13x - (o_17x + o_18x + o_20x)) / denom * 100) %>%
  mutate(I4.1 = o_15x / denom * 100) %>%
  mutate(I4.2 = o_16x / denom * 100) %>%
  mutate(I4.3 = o_16.5 / denom * 100) %>%
  mutate(I5 = o_17x / denom * 100) %>%
  mutate(across(c(o_19, o_21, o_22), ~ ifelse(Scenario == "Actual", ., NA), .names = "{.col}_a")) %>%
  mutate(across(c(o_19_a, o_21_a, o_22_a), ~ na.locf(., na.rm = F))) %>%
  group_by(Bank_ID, Exercise, Scenario) %>%
  mutate(across(c(o_18, o_20), ~ ifelse(rel_period == "Year1", ., NA), .names = "{.col}b")) %>%
  mutate(across(c(o_18b, o_20b), ~ na.locf(., na.rm = F))) %>%
  ungroup() %>%
  mutate(I6 = (o_18x + o_20x + (o_19 - o_19_a) + (o_21 - o_21_a)) / denom * 100) %>%
  mutate(I7 = ((o_22 - o_22_a) - (o_19 - o_19_a)) / denom * 100) %>%
  mutate(I8 = -1 * (o_23x / denom) * 100) %>%
  mutate(I9 = o_24 * (1 / o_25 - 1 / denom) * 100) %>%
  mutate(I3 = na.locf(I3, na.rm = F)) %>%
  mutate(I11 = o_24 / o_25 * 100) %>%
  mutate(I10 = I11 - (I3 + I4 + I5 + I6 + I7 + I8 + I9)) %>%
  dplyr::select(ISO2, Name, Country, Bank_ID, Exercise, Scenario, rel_period, I1, I2, I3, I4, I4.1, I4.2, I4.3, I5, I6, I7, I8, I9, I10, I11) %>%
  mutate(across(-c(ISO2, Name, Country, Bank_ID, Exercise, Scenario, rel_period, I1, I2, I3), ~ ifelse(rel_period == "Realised", NA, .))) %>%
  mutate(I1 = ifelse(Scenario == "Actual - Restated", NA, I1)) %>%
  mutate(I3 = ifelse(Scenario != "Actual - Restated", NA, I3)) %>%
  group_by(Bank_ID, Exercise) %>%
  mutate(across(c(I2, I3), ~ dplyr::lead(., 1))) %>%
  mutate(I3 = ifelse(is.na(I3) & Scenario == "Actual", I1, I3)) %>%
  ungroup() %>%
  mutate(I2 = I3 - I1) %>%
  pivot_longer(-c(ISO2, Name, Country, Bank_ID, Exercise, Scenario, rel_period), names_to = "Items", values_to = "Amount") %>%
  na.omit() %>%
  mutate(Period = ifelse(rel_period == "Realised", as.numeric(paste0(as.numeric(Exercise) - 1, 12)),
                         ifelse(rel_period == "Year1", as.numeric(paste0(as.numeric(Exercise), 12)),
                                ifelse(rel_period == "Year2", as.numeric(paste0(as.numeric(Exercise) + 1, 12)),
                                       ifelse(rel_period == "Year3", as.numeric(paste0(as.numeric(Exercise) + 2, 12)), NA)
                                )
                         )
  )) %>%
  dplyr::select(ISO2, Name, Country, Bank_ID, Exercise, Period, Scenario, rel_period, Items, Amount) %>%
  mutate(Exercise = as.numeric(Exercise))

st_dta_tr_country <- st_dta %>%
  mutate(Order = paste("o", Order, sep = "_")) %>%
  pivot_longer(-c(Country, ISO2, Name, Bank_ID, Exercise, Common_Item, Scenario, Order), names_to = "rel_period", values_to = "Amount") %>%
  na.omit() %>% 
  distinct() %>%
  group_by(Exercise, Country, Scenario, Common_Item, rel_period, Order) %>% 
  mutate(Amount = sum(Amount,na.rm=T)) %>% 
  ungroup() %>% 
  mutate(Bank_ID = "Total") %>% 
  mutate(Name = "Total") %>% 
  distinct() %>% 
  pivot_wider(id_cols = -c(Common_Item), names_from = "Order", values_from = "Amount") %>%
  arrange(ISO2, Bank_ID, Exercise, Scenario, rel_period) %>%
  mutate(across(matches("o_"), ~ ifelse(is.na(.), 0, .))) %>%
  group_by(Country, Exercise, Scenario) %>%
  mutate(across(c(o_13, o_17, o_18, o_20, o_15, o_16, o_23), ~ ifelse(rel_period == "Year2" & Exercise > 2010, . + dplyr::lag(., 1), ifelse(rel_period == "Year3" & Exercise > 2010, . + dplyr::lag(., 1) + dplyr::lag(., 2), .)), .names = "{.col}x")) %>%
  ungroup() %>%
  mutate(o_14 = o_13x - (o_17x + o_18x + o_20x)) %>%
  mutate(o_16.5 = o_14 - (o_15x + o_16x)) %>%
  mutate(I1 = ifelse(rel_period == "Realised", o_1 / o_2 * 100, NA)) %>%
  mutate(I3 = ifelse(rel_period == "Realised", o_10 / o_11 * 100, NA)) %>%
  mutate(I2 = I1 - dplyr::lag(I1, 1)) %>%
  mutate(denom = ifelse(Scenario == "Actual - Restated", o_11, NA)) %>%
  mutate(denom = ifelse(is.na(denom), ifelse(Scenario == "Actual", o_11, NA), denom)) %>%
  mutate(denom = na.locf(denom, na.rm = F)) %>%
  mutate(I4 = (o_13x - (o_17x + o_18x + o_20x)) / denom * 100) %>%
  mutate(I4.1 = o_15x / denom * 100) %>%
  mutate(I4.2 = o_16x / denom * 100) %>%
  mutate(I4.3 = o_16.5 / denom * 100) %>%
  mutate(I5 = o_17x / denom * 100) %>%
  mutate(across(c(o_19, o_21, o_22), ~ ifelse(Scenario == "Actual", ., NA), .names = "{.col}_a")) %>%
  mutate(across(c(o_19_a, o_21_a, o_22_a), ~ na.locf(., na.rm = F))) %>%
  group_by(Country, Exercise, Scenario) %>%
  mutate(across(c(o_18, o_20), ~ ifelse(rel_period == "Year1", ., NA), .names = "{.col}b")) %>%
  mutate(across(c(o_18b, o_20b), ~ na.locf(., na.rm = F))) %>%
  ungroup() %>%
  mutate(I6 = (o_18x + o_20x + (o_19 - o_19_a) + (o_21 - o_21_a)) / denom * 100) %>%
  mutate(I7 = ((o_22 - o_22_a) - (o_19 - o_19_a)) / denom * 100) %>%
  mutate(I8 = -1 * (o_23x / denom) * 100) %>%
  mutate(I9 = o_24 * (1 / o_25 - 1 / denom) * 100) %>%
  mutate(I3 = na.locf(I3, na.rm = F)) %>%
  mutate(I11 = o_24 / o_25 * 100) %>%
  mutate(I10 = I11 - (I3 + I4 + I5 + I6 + I7 + I8 + I9)) %>%
  dplyr::select(ISO2, Name, Country, Bank_ID, Exercise, Scenario, rel_period, I1, I2, I3, I4, I4.1, I4.2, I4.3, I5, I6, I7, I8, I9, I10, I11) %>%
  mutate(across(-c(ISO2, Name, Country, Bank_ID, Exercise, Scenario, rel_period, I1, I2, I3), ~ ifelse(rel_period == "Realised", NA, .))) %>%
  mutate(I1 = ifelse(Scenario == "Actual - Restated", NA, I1)) %>%
  mutate(I3 = ifelse(Scenario != "Actual - Restated", NA, I3)) %>%
  group_by(Country, Exercise) %>%
  mutate(across(c(I2, I3), ~ dplyr::lead(., 1))) %>%
  mutate(I3 = ifelse(is.na(I3) & Scenario == "Actual", I1, I3)) %>%
  ungroup() %>%
  mutate(I2 = I3 - I1) %>%
  pivot_longer(-c(ISO2, Name, Country, Bank_ID, Exercise, Scenario, rel_period), names_to = "Items", values_to = "Amount") %>%
  na.omit() %>% 
  mutate(Period = ifelse(rel_period == "Realised", as.numeric(paste0(as.numeric(Exercise) - 1, 12)),
                         ifelse(rel_period == "Year1", as.numeric(paste0(as.numeric(Exercise), 12)),
                                ifelse(rel_period == "Year2", as.numeric(paste0(as.numeric(Exercise) + 1, 12)),
                                       ifelse(rel_period == "Year3", as.numeric(paste0(as.numeric(Exercise) + 2, 12)), NA)
                                )
                         )
  )) %>%
  dplyr::select(ISO2, Name, Country, Bank_ID, Exercise, Period, Scenario, rel_period, Items, Amount) %>%
  mutate(Exercise = as.numeric(Exercise))

st_dta_tr_total <- st_dta %>%
  mutate(Order = paste("o", Order, sep = "_")) %>%
  pivot_longer(-c(Country, ISO2, Name, Bank_ID, Exercise, Common_Item, Scenario, Order), names_to = "rel_period", values_to = "Amount") %>%
  na.omit() %>% 
  distinct() %>%
  group_by(Exercise, Scenario, Common_Item, rel_period, Order) %>% 
  mutate(Amount = sum(Amount,na.rm=T)) %>% 
  ungroup() %>% 
  mutate(Bank_ID = "Total") %>% 
  mutate(Name = "Total") %>% 
  mutate(Country = "Total") %>% 
  mutate(ISO2 = "TOT") %>% 
  distinct() %>% 
  pivot_wider(id_cols = -c(Common_Item), names_from = "Order", values_from = "Amount") %>%
  arrange(ISO2, Bank_ID, Exercise, Scenario, rel_period) %>%
  mutate(across(matches("o_"), ~ ifelse(is.na(.), 0, .))) %>%
  group_by(Exercise, Scenario) %>%
  mutate(across(c(o_13, o_17, o_18, o_20, o_15, o_16, o_23), ~ ifelse(rel_period == "Year2" & Exercise > 2010, . + dplyr::lag(., 1), ifelse(rel_period == "Year3" & Exercise > 2010, . + dplyr::lag(., 1) + dplyr::lag(., 2), .)), .names = "{.col}x")) %>%
  ungroup() %>%
  mutate(o_14 = o_13x - (o_17x + o_18x + o_20x)) %>%
  mutate(o_16.5 = o_14 - (o_15x + o_16x)) %>%
  mutate(I1 = ifelse(rel_period == "Realised", o_1 / o_2 * 100, NA)) %>%
  mutate(I3 = ifelse(rel_period == "Realised", o_10 / o_11 * 100, NA)) %>%
  mutate(I2 = I1 - dplyr::lag(I1, 1)) %>%
  mutate(denom = ifelse(Scenario == "Actual - Restated", o_11, NA)) %>%
  mutate(denom = ifelse(is.na(denom), ifelse(Scenario == "Actual", o_11, NA), denom)) %>%
  mutate(denom = na.locf(denom, na.rm = F)) %>%
  mutate(I4 = (o_13x - (o_17x + o_18x + o_20x)) / denom * 100) %>%
  mutate(I4.1 = o_15x / denom * 100) %>%
  mutate(I4.2 = o_16x / denom * 100) %>%
  mutate(I4.3 = o_16.5 / denom * 100) %>%
  mutate(I5 = o_17x / denom * 100) %>%
  mutate(across(c(o_19, o_21, o_22), ~ ifelse(Scenario == "Actual", ., NA), .names = "{.col}_a")) %>%
  mutate(across(c(o_19_a, o_21_a, o_22_a), ~ na.locf(., na.rm = F))) %>%
  group_by(Exercise, Scenario) %>%
  mutate(across(c(o_18, o_20), ~ ifelse(rel_period == "Year1", ., NA), .names = "{.col}b")) %>%
  mutate(across(c(o_18b, o_20b), ~ na.locf(., na.rm = F))) %>%
  ungroup() %>%
  mutate(I6 = (o_18x + o_20x + (o_19 - o_19_a) + (o_21 - o_21_a)) / denom * 100) %>%
  mutate(I7 = ((o_22 - o_22_a) - (o_19 - o_19_a)) / denom * 100) %>%
  mutate(I8 = -1 * (o_23x / denom) * 100) %>%
  mutate(I9 = o_24 * (1 / o_25 - 1 / denom) * 100) %>%
  mutate(I3 = na.locf(I3, na.rm = F)) %>%
  mutate(I11 = o_24 / o_25 * 100) %>%
  mutate(I10 = I11 - (I3 + I4 + I5 + I6 + I7 + I8 + I9)) %>%
  dplyr::select(ISO2, Name, Country, Bank_ID, Exercise, Scenario, rel_period, I1, I2, I3, I4, I4.1, I4.2, I4.3, I5, I6, I7, I8, I9, I10, I11) %>%
  mutate(across(-c(ISO2, Name, Country, Bank_ID, Exercise, Scenario, rel_period, I1, I2, I3), ~ ifelse(rel_period == "Realised", NA, .))) %>%
  mutate(I1 = ifelse(Scenario == "Actual - Restated", NA, I1)) %>%
  mutate(I3 = ifelse(Scenario != "Actual - Restated", NA, I3)) %>%
  group_by(Exercise) %>%
  mutate(across(c(I2, I3), ~ dplyr::lead(., 1))) %>%
  mutate(I3 = ifelse(is.na(I3) & Scenario == "Actual", I1, I3)) %>%
  ungroup() %>%
  mutate(I2 = I3 - I1) %>%
  pivot_longer(-c(ISO2, Name, Country, Bank_ID, Exercise, Scenario, rel_period), names_to = "Items", values_to = "Amount") %>%
  na.omit() %>% 
  mutate(Period = ifelse(rel_period == "Realised", as.numeric(paste0(as.numeric(Exercise) - 1, 12)),
                         ifelse(rel_period == "Year1", as.numeric(paste0(as.numeric(Exercise), 12)),
                                ifelse(rel_period == "Year2", as.numeric(paste0(as.numeric(Exercise) + 1, 12)),
                                       ifelse(rel_period == "Year3", as.numeric(paste0(as.numeric(Exercise) + 2, 12)), NA)
                                )
                         )
  )) %>%
  dplyr::select(ISO2, Name, Country, Bank_ID, Exercise, Period, Scenario, rel_period, Items, Amount) %>%
  mutate(Exercise = as.numeric(Exercise))

## Fully-loaded
st_dta_fl_bank <- st_dta %>%
  mutate(Order = paste("o", Order, sep = "_")) %>%
  pivot_longer(-c(Country, ISO2, Name, Bank_ID, Exercise, Common_Item, Scenario, Order), names_to = "rel_period", values_to = "Amount") %>%
  na.omit() %>%
  distinct() %>%
  pivot_wider(id_cols = -c(Common_Item), names_from = "Order", values_from = "Amount") %>%
  arrange(ISO2, Bank_ID, Exercise, Scenario, rel_period) %>%
  mutate(across(matches("o_"), ~ ifelse(is.na(.), 0, .))) %>%
  group_by(Bank_ID, Exercise, Scenario) %>%
  mutate(across(c(o_13, o_17, o_18, o_20, o_15, o_16, o_23), ~ ifelse(rel_period == "Year2" & Exercise > 2010, . + dplyr::lag(., 1), ifelse(rel_period == "Year3" & Exercise > 2010, . + dplyr::lag(., 1) + dplyr::lag(., 2), .)), .names = "{.col}x")) %>%
  ungroup() %>%
  mutate(o_14 = o_13x - (o_17x + o_18x + o_20x)) %>%
  mutate(o_16.5 = o_14 - (o_15x + o_16x)) %>%
  mutate(J1.1 = ifelse(rel_period == "Realised", o_1 / o_2 * 100, NA)) %>% ## CET1 Transitional - CRR 2
  mutate(J1.2 = ifelse(rel_period == "Realised", o_4 / o_5 * 100, NA)) %>% ## CET1 Fully Loaded - CRR 2
  mutate(J1.1.1 = J1.2 - J1.1) %>%
  mutate(J1.3 = ifelse(rel_period == "Realised", o_7 / o_8 * 100, NA)) %>% ## CET1 Fully Loaded - CRR 3
  mutate(J1.2.1 = ifelse(!is.na(dplyr::lag(J1.3)), ifelse(rel_period == "Realised", J1.3 - dplyr::lag(J1.3), 0), NA)) %>%
  mutate(J1.4 = ifelse(rel_period == "Realised", o_10 / o_11 * 100, NA)) %>% ## CET1 Transitional - CRR 3
  mutate(J1.3.1 = J1.4 - J1.3) %>%
  group_by(Bank_ID, Exercise) %>%
  mutate(across(c(J1.2.1, J1.3, J1.3.1, J1.4), ~ ifelse(Exercise %in% c(2018, 2025), dplyr::lead(., 1), .))) %>%
  mutate(across(c(J1.2.1), ~ ifelse(!(Exercise %in% c(2018, 2025)), dplyr::lead(., 1), .))) %>%
  ungroup() %>% 
  mutate(denom = ifelse(Scenario == "Actual - Restated", o_11, NA)) %>%
  mutate(denom = ifelse(is.na(denom), ifelse(Scenario == "Actual", o_11, NA), denom)) %>%
  mutate(denom = na.locf(denom, na.rm = F)) %>%
  mutate(J4 = (o_13x - (o_17x + o_18x + o_20x)) / denom * 100) %>%
  mutate(J4.1 = o_15x / denom * 100) %>%
  mutate(J4.2 = o_16x / denom * 100) %>%
  mutate(J4.3 = o_16.5 / denom * 100) %>%
  mutate(J5 = o_17x / denom * 100) %>%
  mutate(across(c(o_19, o_21, o_22), ~ ifelse(Scenario == "Actual", ., NA), .names = "{.col}_a")) %>%
  mutate(across(c(o_19_a, o_21_a, o_22_a), ~ na.locf(., na.rm = F))) %>%
  group_by(Bank_ID, Exercise, Scenario) %>%
  mutate(across(c(o_18, o_20), ~ ifelse(rel_period == "Year1", ., NA), .names = "{.col}b")) %>%
  mutate(across(c(o_18b, o_20b), ~ na.locf(., na.rm = F))) %>%
  ungroup() %>%
  mutate(J6 = (o_18x + o_20x + (o_19 - o_19_a) + (o_21 - o_21_a)) / denom * 100) %>%
  mutate(J7 = ((o_22 - o_22_a) - (o_19 - o_19_a)) / denom * 100) %>%
  mutate(J8 = -1 * (o_23x / denom) * 100) %>%
  mutate(J9 = o_24 * (1 / o_25 - 1 / denom) * 100) %>%
  mutate(J1.4 = na.locf(J1.4, na.rm = F)) %>%
  mutate(J11 = o_24 / o_25 * 100) %>% ## Final transitional
  mutate(J10 = J11 - (J1.4 + J4 + J5 + J6 + J7 + J8 + J9)) %>%
  mutate(J12 = o_27 / o_28 * 100) %>%
  mutate(J11.1 = J12 - J11) %>%
  dplyr::select(
    ISO2, Name, Bank_ID, Exercise, Scenario, rel_period, Country,
    J1.1, J1.1.1, J1.2, J1.2.1, J1.3, J1.3.1, J1.4, J4, J4.1, J4.2, J4.3, J5, J6, J7, J8, J9, J10, J11, J11.1, J12
  ) %>%
  mutate(across(-c(ISO2, Name, Bank_ID, Exercise, Scenario, rel_period, Country, J1.1, J1.1.1, J1.2, J1.2.1, J1.3, J1.3.1, J1.4), ~ ifelse(rel_period == "Realised", NA, .))) %>%
  mutate(across(c(J1.1, J1.1.1, J1.2, J1.2.1, J1.3, J1.3.1, J1.4), ~ ifelse(Scenario == "Actual - Restated", NA, .))) %>%
  group_by(Bank_ID, Exercise) %>%
  mutate(J1.3 = ifelse(is.na(J1.3) & Scenario == "Actual", J1.2, J1.3)) %>%
  mutate(J1.4 = ifelse(is.na(J1.4) & Scenario == "Actual", J1.1, J1.4)) %>%
  mutate(J1.4 = ifelse(Scenario != "Actual", NA, J1.4)) %>%
  ungroup() %>%
  pivot_longer(-c(ISO2, Country, Name, Bank_ID, Exercise, Scenario, rel_period), names_to = "Items", values_to = "Amount") %>%
  na.omit() %>%
  mutate(Period = ifelse(rel_period == "Realised", as.numeric(paste0(as.numeric(Exercise) - 1, 12)),
                         ifelse(rel_period == "Year1", as.numeric(paste0(as.numeric(Exercise), 12)),
                                ifelse(rel_period == "Year2", as.numeric(paste0(as.numeric(Exercise) + 1, 12)),
                                       ifelse(rel_period == "Year3", as.numeric(paste0(as.numeric(Exercise) + 2, 12)), NA)
                                )
                         )
  )) %>%
  dplyr::select(ISO2, Name, Country, Bank_ID, Exercise, Period, Scenario, rel_period, Items, Amount) %>%
  mutate(Exercise = as.numeric(Exercise))

st_dta_fl_country <- st_dta %>%
  mutate(Order = paste("o", Order, sep = "_")) %>%
  pivot_longer(-c(Country, ISO2, Name, Bank_ID, Exercise, Common_Item, Scenario, Order), names_to = "rel_period", values_to = "Amount") %>%
  na.omit() %>%
  distinct() %>%
  group_by(Exercise, Country, Scenario, Common_Item, rel_period, Order) %>% 
  mutate(Amount = sum(Amount,na.rm=T)) %>% 
  ungroup() %>% 
  mutate(Bank_ID = "Total") %>% 
  mutate(Name = "Total") %>% 
  distinct() %>% 
  pivot_wider(id_cols = -c(Common_Item), names_from = "Order", values_from = "Amount") %>%
  arrange(ISO2, Bank_ID, Exercise, Scenario, rel_period) %>%
  mutate(across(matches("o_"), ~ ifelse(is.na(.), 0, .))) %>%
  group_by(Country, Exercise, Scenario) %>%
  mutate(across(c(o_13, o_17, o_18, o_20, o_15, o_16, o_23), ~ ifelse(rel_period == "Year2" & Exercise > 2010, . + dplyr::lag(., 1), ifelse(rel_period == "Year3" & Exercise > 2010, . + dplyr::lag(., 1) + dplyr::lag(., 2), .)), .names = "{.col}x")) %>%
  ungroup() %>%
  mutate(o_14 = o_13x - (o_17x + o_18x + o_20x)) %>%
  mutate(o_16.5 = o_14 - (o_15x + o_16x)) %>%
  mutate(J1.1 = ifelse(rel_period == "Realised", o_1 / o_2 * 100, NA)) %>% ## CET1 Transitional - CRR 2
  mutate(J1.2 = ifelse(rel_period == "Realised", o_4 / o_5 * 100, NA)) %>% ## CET1 Fully Loaded - CRR 2
  mutate(J1.1.1 = J1.2 - J1.1) %>%
  mutate(J1.3 = ifelse(rel_period == "Realised", o_7 / o_8 * 100, NA)) %>% ## CET1 Fully Loaded - CRR 3
  mutate(J1.2.1 = ifelse(!is.na(dplyr::lag(J1.3)), ifelse(rel_period == "Realised", J1.3 - dplyr::lag(J1.3), 0), NA)) %>%
  mutate(J1.4 = ifelse(rel_period == "Realised", o_10 / o_11 * 100, NA)) %>% ## CET1 Transitional - CRR 3
  mutate(J1.3.1 = J1.4 - J1.3) %>%
  group_by(Country, Exercise) %>%
  mutate(across(c(J1.2.1, J1.3, J1.3.1, J1.4), ~ ifelse(Exercise %in% c(2018, 2025), dplyr::lead(., 1), .))) %>%
  mutate(across(c(J1.2.1), ~ ifelse(!(Exercise %in% c(2018, 2025)), dplyr::lead(., 1), .))) %>%
  ungroup() %>% 
  mutate(denom = ifelse(Scenario == "Actual - Restated", o_11, NA)) %>%
  mutate(denom = ifelse(is.na(denom), ifelse(Scenario == "Actual", o_11, NA), denom)) %>%
  mutate(denom = na.locf(denom, na.rm = F)) %>%
  mutate(J4 = (o_13x - (o_17x + o_18x + o_20x)) / denom * 100) %>%
  mutate(J4.1 = o_15x / denom * 100) %>%
  mutate(J4.2 = o_16x / denom * 100) %>%
  mutate(J4.3 = o_16.5 / denom * 100) %>%
  mutate(J5 = o_17x / denom * 100) %>%
  mutate(across(c(o_19, o_21, o_22), ~ ifelse(Scenario == "Actual", ., NA), .names = "{.col}_a")) %>%
  mutate(across(c(o_19_a, o_21_a, o_22_a), ~ na.locf(., na.rm = F))) %>%
  group_by(Country, Exercise, Scenario) %>%
  mutate(across(c(o_18, o_20), ~ ifelse(rel_period == "Year1", ., NA), .names = "{.col}b")) %>%
  mutate(across(c(o_18b, o_20b), ~ na.locf(., na.rm = F))) %>%
  ungroup() %>%
  mutate(J6 = (o_18x + o_20x + (o_19 - o_19_a) + (o_21 - o_21_a)) / denom * 100) %>%
  mutate(J7 = ((o_22 - o_22_a) - (o_19 - o_19_a)) / denom * 100) %>%
  mutate(J8 = -1 * (o_23x / denom) * 100) %>%
  mutate(J9 = o_24 * (1 / o_25 - 1 / denom) * 100) %>%
  mutate(J1.4 = na.locf(J1.4, na.rm = F)) %>%
  mutate(J11 = o_24 / o_25 * 100) %>% ## Final transitional
  mutate(J10 = J11 - (J1.4 + J4 + J5 + J6 + J7 + J8 + J9)) %>%
  mutate(J12 = o_27 / o_28 * 100) %>%
  mutate(J11.1 = J12 - J11) %>%
  dplyr::select(
    ISO2, Name, Bank_ID, Exercise, Scenario, rel_period, Country,
    J1.1, J1.1.1, J1.2, J1.2.1, J1.3, J1.3.1, J1.4, J4, J4.1, J4.2, J4.3, J5, J6, J7, J8, J9, J10, J11, J11.1, J12
  ) %>%
  mutate(across(-c(ISO2, Name, Bank_ID, Exercise, Scenario, rel_period, Country, J1.1, J1.1.1, J1.2, J1.2.1, J1.3, J1.3.1, J1.4), ~ ifelse(rel_period == "Realised", NA, .))) %>%
  mutate(across(c(J1.1, J1.1.1, J1.2, J1.2.1, J1.3, J1.3.1, J1.4), ~ ifelse(Scenario == "Actual - Restated", NA, .))) %>%
  group_by(Country, Exercise) %>%
  mutate(J1.3 = ifelse(is.na(J1.3) & Scenario == "Actual", J1.2, J1.3)) %>%
  mutate(J1.4 = ifelse(is.na(J1.4) & Scenario == "Actual", J1.1, J1.4)) %>%
  mutate(J1.4 = ifelse(Scenario != "Actual", NA, J1.4)) %>%
  ungroup() %>%
  pivot_longer(-c(ISO2, Country, Name, Bank_ID, Exercise, Scenario, rel_period), names_to = "Items", values_to = "Amount") %>%
  na.omit() %>%
  mutate(Period = ifelse(rel_period == "Realised", as.numeric(paste0(as.numeric(Exercise) - 1, 12)),
                         ifelse(rel_period == "Year1", as.numeric(paste0(as.numeric(Exercise), 12)),
                                ifelse(rel_period == "Year2", as.numeric(paste0(as.numeric(Exercise) + 1, 12)),
                                       ifelse(rel_period == "Year3", as.numeric(paste0(as.numeric(Exercise) + 2, 12)), NA)
                                )
                         )
  )) %>%
  dplyr::select(ISO2, Name, Country, Bank_ID, Exercise, Period, Scenario, rel_period, Items, Amount) %>%
  mutate(Exercise = as.numeric(Exercise))

st_dta_fl_total <- st_dta %>%
  mutate(Order = paste("o", Order, sep = "_")) %>%
  pivot_longer(-c(Country, ISO2, Name, Bank_ID, Exercise, Common_Item, Scenario, Order), names_to = "rel_period", values_to = "Amount") %>%
  na.omit() %>%
  distinct() %>%
  group_by(Exercise, Scenario, Common_Item, rel_period, Order) %>% 
  mutate(Amount = sum(Amount,na.rm=T)) %>% 
  ungroup() %>% 
  mutate(Bank_ID = "Total") %>% 
  mutate(Name = "Total") %>%
  mutate(Country = "Total") %>% 
  mutate(ISO2 = "Total") %>% 
  distinct() %>% 
  pivot_wider(id_cols = -c(Common_Item), names_from = "Order", values_from = "Amount") %>%
  arrange(ISO2, Bank_ID, Exercise, Scenario, rel_period) %>%
  mutate(across(matches("o_"), ~ ifelse(is.na(.), 0, .))) %>%
  group_by(Exercise, Scenario) %>%
  mutate(across(c(o_13, o_17, o_18, o_20, o_15, o_16, o_23), ~ ifelse(rel_period == "Year2" & Exercise > 2010, . + dplyr::lag(., 1), ifelse(rel_period == "Year3" & Exercise > 2010, . + dplyr::lag(., 1) + dplyr::lag(., 2), .)), .names = "{.col}x")) %>%
  ungroup() %>%
  mutate(o_14 = o_13x - (o_17x + o_18x + o_20x)) %>%
  mutate(o_16.5 = o_14 - (o_15x + o_16x)) %>%
  mutate(J1.1 = ifelse(rel_period == "Realised", o_1 / o_2 * 100, NA)) %>% ## CET1 Transitional - CRR 2
  mutate(J1.2 = ifelse(rel_period == "Realised", o_4 / o_5 * 100, NA)) %>% ## CET1 Fully Loaded - CRR 2
  mutate(J1.1.1 = J1.2 - J1.1) %>%
  mutate(J1.3 = ifelse(rel_period == "Realised", o_7 / o_8 * 100, NA)) %>% ## CET1 Fully Loaded - CRR 3
  mutate(J1.2.1 = ifelse(!is.na(dplyr::lag(J1.3)), ifelse(rel_period == "Realised", J1.3 - dplyr::lag(J1.3), 0), NA)) %>%
  mutate(J1.4 = ifelse(rel_period == "Realised", o_10 / o_11 * 100, NA)) %>% ## CET1 Transitional - CRR 3
  mutate(J1.3.1 = J1.4 - J1.3) %>%
  group_by(Exercise) %>%
  mutate(across(c(J1.2.1, J1.3, J1.3.1, J1.4), ~ ifelse(Exercise %in% c(2018, 2025), dplyr::lead(., 1), .))) %>%
  mutate(across(c(J1.2.1), ~ ifelse(!(Exercise %in% c(2018, 2025)), dplyr::lead(., 1), .))) %>%
  ungroup() %>% 
  mutate(denom = ifelse(Scenario == "Actual - Restated", o_11, NA)) %>%
  mutate(denom = ifelse(is.na(denom), ifelse(Scenario == "Actual", o_11, NA), denom)) %>%
  mutate(denom = na.locf(denom, na.rm = F)) %>%
  mutate(J4 = (o_13x - (o_17x + o_18x + o_20x)) / denom * 100) %>%
  mutate(J4.1 = o_15x / denom * 100) %>%
  mutate(J4.2 = o_16x / denom * 100) %>%
  mutate(J4.3 = o_16.5 / denom * 100) %>%
  mutate(J5 = o_17x / denom * 100) %>%
  mutate(across(c(o_19, o_21, o_22), ~ ifelse(Scenario == "Actual", ., NA), .names = "{.col}_a")) %>%
  mutate(across(c(o_19_a, o_21_a, o_22_a), ~ na.locf(., na.rm = F))) %>%
  group_by(Exercise, Scenario) %>%
  mutate(across(c(o_18, o_20), ~ ifelse(rel_period == "Year1", ., NA), .names = "{.col}b")) %>%
  mutate(across(c(o_18b, o_20b), ~ na.locf(., na.rm = F))) %>%
  ungroup() %>%
  mutate(J6 = (o_18x + o_20x + (o_19 - o_19_a) + (o_21 - o_21_a)) / denom * 100) %>%
  mutate(J7 = ((o_22 - o_22_a) - (o_19 - o_19_a)) / denom * 100) %>%
  mutate(J8 = -1 * (o_23x / denom) * 100) %>%
  mutate(J9 = o_24 * (1 / o_25 - 1 / denom) * 100) %>%
  mutate(J1.4 = na.locf(J1.4, na.rm = F)) %>%
  mutate(J11 = o_24 / o_25 * 100) %>% ## Final transitional
  mutate(J10 = J11 - (J1.4 + J4 + J5 + J6 + J7 + J8 + J9)) %>%
  mutate(J12 = o_27 / o_28 * 100) %>%
  mutate(J11.1 = J12 - J11) %>%
  dplyr::select(
    ISO2, Name, Bank_ID, Exercise, Scenario, rel_period, Country,
    J1.1, J1.1.1, J1.2, J1.2.1, J1.3, J1.3.1, J1.4, J4, J4.1, J4.2, J4.3, J5, J6, J7, J8, J9, J10, J11, J11.1, J12
  ) %>%
  mutate(across(-c(ISO2, Name, Bank_ID, Exercise, Scenario, rel_period, Country, J1.1, J1.1.1, J1.2, J1.2.1, J1.3, J1.3.1, J1.4), ~ ifelse(rel_period == "Realised", NA, .))) %>%
  mutate(across(c(J1.1, J1.1.1, J1.2, J1.2.1, J1.3, J1.3.1, J1.4), ~ ifelse(Scenario == "Actual - Restated", NA, .))) %>%
  group_by(Exercise) %>%
  mutate(J1.3 = ifelse(is.na(J1.3) & Scenario == "Actual", J1.2, J1.3)) %>%
  mutate(J1.4 = ifelse(is.na(J1.4) & Scenario == "Actual", J1.1, J1.4)) %>%
  mutate(J1.4 = ifelse(Scenario != "Actual", NA, J1.4)) %>%
  ungroup() %>%
  pivot_longer(-c(ISO2, Country, Name, Bank_ID, Exercise, Scenario, rel_period), names_to = "Items", values_to = "Amount") %>%
  na.omit() %>%
  mutate(Period = ifelse(rel_period == "Realised", as.numeric(paste0(as.numeric(Exercise) - 1, 12)),
                         ifelse(rel_period == "Year1", as.numeric(paste0(as.numeric(Exercise), 12)),
                                ifelse(rel_period == "Year2", as.numeric(paste0(as.numeric(Exercise) + 1, 12)),
                                       ifelse(rel_period == "Year3", as.numeric(paste0(as.numeric(Exercise) + 2, 12)), NA)
                                )
                         )
  )) %>%
  dplyr::select(ISO2, Name, Country, Bank_ID, Exercise, Period, Scenario, rel_period, Items, Amount) %>%
  mutate(Exercise = as.numeric(Exercise))

final_tr = rbind(st_dta_tr_bank, st_dta_tr_country, st_dta_tr_total) %>% 
  mutate(TR = 1)
final_fl = rbind(st_dta_fl_bank, st_dta_fl_country, st_dta_fl_total) %>% 
  mutate(TR = 0)
final_waterfall = rbind(final_tr, final_fl) %>% 
  mutate(Scenario = as.character(Scenario))

##------------------------
## Export final datasets
final_waterfall_export = final_waterfall %>% 
  mutate(DB = "final_waterfall")

## %% Chart
final_waterfall = final_waterfall_export %>% 
  dplyr::select(-DB)

transitional = 0 ## 1 or 0
iso = "France"
bank = "BNP Paribas"
exercise = 2025
period = "Year 3"
scenario = "Adverse"

data = final_waterfall
fact = 1

palette <- c("Total" = "#204980",
             "Increase" = "#D2E288",
             "Decrease" = "#9e202a",
             "Additional Second Round Effect" = "#FCAF17")

if(transitional == 1){
  dimensions = list(
    "I1", "I2", "I3", "I4", "I4.1", "I4.2", "I4.3",
    "I5", "I6", "I7", "I8", "I9", "I10", "I11"
  )
  
  names(dimensions) = c(
    "CET1 Capital Ratio - Transitional", "Restatement", "CET1 Capital Ratio - Transitional - Restated",
    "Net Profit and Losses", "Of Which Net Interest Income", "Of Which NFCI",
    "Of Which Other Income/Expenses", "Credit Risk Losses", "Market Risk Losses",
    "Other Comprehensive Income", "Dividends",
    "Increase in REAs", "Other Items", "CET1 Capital Ratio - Transitional - End"
  )
  
}else{
  dimensions = list(
    "J1.1", "J1.1.1", "J1.2", "J1.2.1", "J1.3", "J1.3.1", "J1.4", "J4",
    "J4.1", "J4.2", "J4.3", "J5", "J6", "J7", "J8", "J9", "J10", "J11",
    "J11.1", "J12"
  )
  
  names(dimensions) = c(
    "CET1 Capital Ratio - Transitional", "Transitional Adjustments", "CET1 Capital Ratio - Fully Loaded",
    "Restatement", "CET1 Capital Ratio - Fully Loaded - Restated", "Transitional Adjustments - Restated",
    "CET1 Capital Ratio - Transitional - Restated", "Net Profit and Losses",
    "Of Which Net Interest Income", "Of Which NFCI",
    "Of Which Other Income/Expenses", "Credit Risk Losses", "Market Risk Losses",
    "Other Comprehensive Income", "Dividends",
    "Increase in REAs", "Other Items", "CET1 Capital Ratio - Transitional - End",
    "Transitional Adjustments - End", "CET1 Capital Ratio - Fully Loaded - End"
  )
}


if (!is.null(bank)) {
  bank_id = unique(data[data$Name == bank, "Bank_ID"])
  data = data %>% filter(Bank_ID == as.character(bank_id))
} else if (!is.null(iso)) {
  data = data %>% filter(Country == iso)
}

if(transitional == 1){
  if (exercise %in% c(2018, 2025)) {
    dt = data %>%
      filter(
        TR == transitional,
        Exercise == exercise,
        rel_period %in% c("Realised", str_remove_all(period, "\\s{0,}")),
        Scenario %in% c("Actual", scenario)
      ) %>%
      mutate(Amount_out = ifelse(!(Items %in% c("I3", "I4.1", "I4.2", "I4.3")), Amount, 0)) %>%
      mutate(Amount_in = ifelse(Items %in% c("I3", "I4.1", "I4.2", "I4.3"), Amount, 0)) %>%
      mutate(end = ifelse(!(Items %in% c("I11")), cumsum(Amount_out), 0)) %>%
      mutate(end = ifelse(Items %in% c("I4.1", "I4.2", "I4.3"), cumsum(Amount_in), end)) %>%
      mutate(start = ifelse(!Items %in% c("I1", "I3"), dplyr::lag(end, 1), 0)) %>%
      mutate(start = ifelse(Items %in% c("I4.1"), dplyr::lag(start, 1), start)) %>%
      mutate(id = as.numeric(rownames(.))) %>%
      mutate(sign = ifelse(Items %in% c("I1", "I3", "I11"), "Total",
                           ifelse(sign(Amount) >= 0, "Increase", "Decrease")
      )) %>%
      mutate(ofwhich = ifelse(str_detect(Items, "4\\."), 1, 0)) %>%
      arrange(desc(id)) %>%
      mutate(stand = ifelse(sign(Amount) >= 0, end + fact, start + fact)) %>%
      mutate(stand = ifelse(Items == "I11", Amount + fact, stand)) %>%
      mutate(Items = factor(Items, rev(dimensions), labels = rev(names(dimensions))))
  } else {
    dt = data %>%
      filter(
        TR == transitional,
        Exercise == exercise,
        rel_period %in% c("Realised", str_remove_all(period, "\\s{0,}")),
        Scenario %in% c("Actual", scenario)
      ) %>%
      mutate(Amount_out = ifelse(!(Items %in% c("I3", "I4.1", "I4.2", "I4.3")), Amount, 0)) %>%
      mutate(Amount_in = ifelse(Items %in% c("I3", "I4.1", "I4.2", "I4.3"), Amount, 0)) %>%
      mutate(end = ifelse(!(Items %in% c("I11")), cumsum(Amount_out), 0)) %>%
      mutate(end = ifelse(Items %in% c("I4.1", "I4.2", "I4.3"), cumsum(Amount_in), end)) %>%
      mutate(start = ifelse(!Items %in% c("I1", "I3"), dplyr::lag(end, 1), 0)) %>%
      mutate(start = ifelse(Items %in% c("I4.1"), dplyr::lag(start, 1), start)) %>%
      mutate(id = as.numeric(rownames(.))) %>%
      mutate(sign = ifelse(Items %in% c("I1", "I3", "I11"), "Total",
                           ifelse(sign(Amount) >= 0, "Increase", "Decrease")
      )) %>%
      mutate(ofwhich = ifelse(str_detect(Items, "4\\."), 1, 0)) %>%
      arrange(desc(id)) %>%
      mutate(stand = ifelse(sign(Amount) >= 0, end + fact, start + fact)) %>%
      mutate(stand = ifelse(Items == "I11", Amount + fact, stand)) %>%
      mutate(Amount = ifelse(!(Exercise %in% c(2018, 2025)) & Items %in% c("I2", "I3"), NA, Amount)) %>%
      filter(!is.na(Amount)) %>%
      mutate(Items = factor(Items, rev(dimensions[-c(2, 3)]), labels = rev(names(dimensions)[-c(2, 3)]))) %>%
      mutate(id = rev(as.numeric(rownames(.))))
  }
}else{
  if (exercise %in% c(2018, 2025)) {
    dt = data %>%
      filter(
        TR == transitional,
        Exercise == exercise,
        rel_period %in% c("Realised", str_remove_all(period, "\\s{0,}")),
        Scenario %in% c("Actual", scenario)
      ) %>%
      mutate(Amount_out = ifelse(!(Items %in% c("J1.2", "J1.3", "J1.4", "J4.1", "J4.2", "J4.3", "J11", "J12")), Amount, 0)) %>%
      mutate(Amount_in = ifelse(Items %in% c("J1.4", "J4.1", "J4.2", "J4.3"), Amount, 0)) %>%
      mutate(end = ifelse(!(Items %in% c("J11", "J12")), cumsum(Amount_out), 0)) %>%
      mutate(end = ifelse(Items %in% c("J4.1", "J4.2", "J4.3"), cumsum(Amount_in), end)) %>%
      mutate(start = ifelse(!Items %in% c("J1.1", "J1.2", "J1.3", "J1.4"), dplyr::lag(end, 1), 0)) %>%
      mutate(start = ifelse(Items %in% c("J4.1"), dplyr::lag(start, 1), start)) %>%
      mutate(start = ifelse(Items %in% c("J11.1"), dplyr::lag(start, 1), start)) %>%
      mutate(id = as.numeric(rownames(.))) %>%
      mutate(sign = ifelse(Items %in% c("J1.1", "J1.2", "J1.3", "J1.4", "J11", "J12"), "Total",
                           ifelse(sign(Amount) >= 0, "Increase", "Decrease")
      )) %>%
      mutate(ofwhich = ifelse(str_detect(Items, "4\\."), 1, 0)) %>%
      arrange(desc(id)) %>%
      mutate(stand = ifelse(sign(Amount) >= 0, end + fact, start + fact)) %>%
      mutate(stand = ifelse(Items %in% c("J11", "J12"), Amount + fact, stand)) %>%
      mutate(Items = factor(Items, rev(dimensions), labels = rev(names(dimensions)))) %>%
      mutate(alpha = ifelse(str_detect(Items, "Of Which"), 1, 0))
  } else {
    dt = data %>%
      filter(
        TR == transitional,
        Exercise == exercise,
        rel_period %in% c("Realised", str_remove_all(period, "\\s{0,}")),
        Scenario %in% c("Actual", scenario)
      ) %>%
      mutate(Amount_out = ifelse(!(Items %in% c("J1.2", "J1.3", "J1.4", "J4.1", "J4.2", "J4.3", "J11", "J12")), Amount, 0)) %>%
      mutate(Amount_in = ifelse(Items %in% c("J1.4", "J4.1", "J4.2", "J4.3"), Amount, 0)) %>%
      mutate(end = ifelse(!(Items %in% c("J11", "J12")), cumsum(Amount_out), 0)) %>%
      mutate(end = ifelse(Items %in% c("J4.1", "J4.2", "J4.3"), cumsum(Amount_in), end)) %>%
      mutate(start = ifelse(!Items %in% c("J1.1", "J1.2", "J1.3", "J1.4"), dplyr::lag(end, 1), 0)) %>%
      mutate(start = ifelse(Items %in% c("J4.1"), dplyr::lag(start, 1), start)) %>%
      mutate(start = ifelse(Items %in% c("J11.1"), dplyr::lag(start, 1), start)) %>%
      mutate(id = as.numeric(rownames(.))) %>%
      mutate(sign = ifelse(Items %in% c("J1.1", "J1.2", "J1.3", "J1.4", "J11", "J12"), "Total",
                           ifelse(sign(Amount) >= 0, "Increase", "Decrease")
      )) %>%
      mutate(ofwhich = ifelse(str_detect(Items, "4\\."), 1, 0)) %>%
      arrange(desc(id)) %>%
      mutate(stand = ifelse(sign(Amount) >= 0, end + fact, start + fact)) %>%
      mutate(stand = ifelse(Items %in% c("J11", "J12"), Amount + fact, stand)) %>%
      mutate(Amount = ifelse(!(Exercise %in% c(2018, 2025)) & Items %in% c("J1.1", "J1.1.1", "J1.2.1", "J1.3"), NA, Amount)) %>%
      filter(!is.na(Amount)) %>%
      mutate(Items = factor(Items, rev(dimensions[-c(1, 2, 4, 5)]), labels = rev(names(dimensions)[-c(1, 2, 4, 5)]))) %>%
      mutate(Items = str_remove_all(Items, "\\s{0,}-\\s{0,}Restated")) %>%
      mutate(Items = factor(Items, Items)) %>%
      mutate(id = rev(as.numeric(rownames(.)))) %>%
      mutate(alpha = ifelse(str_detect(Items, "Of Which"), 1, 0))
  }
}

minx = dt %>%
  mutate(id = abs(id - (max(id) + 1)))
minx = min(minx[minx$ofwhich == 1, "id"], na.rm = T)

chart =
  dt %>%
  mutate(id = abs(id - (max(id) + 1))) %>%
  mutate(across(c(start, end, id), ~ ifelse(ofwhich == 1, 0, .), .names = "{.col}1")) %>%
  mutate(across(c(start, end, id), ~ ifelse(ofwhich != 1, 0, .), .names = "{.col}2")) %>%
  ggplot(aes(x = Items)) +
  annotate("rect",
           xmin = minx - .45, xmax = minx + 2.45, ymin = 0, ymax = max(dt$end, na.rm = T),
           alpha = .5, fill = "#f6f6f6"
  ) +
  geom_rect(aes(
    xmin = id1 - .45, xmax = id1 + .45, ymin = end1, ymax = start1,
    fill = factor(sign, c("Total", "Increase", "Decrease"))
  )) +
  geom_rect(aes(
    xmin = id2 - .45, xmax = id2 + .45, ymin = end2, ymax = start2,
    fill = factor(sign, c("Total", "Increase", "Decrease"))
  ), alpha = 0.6, show.legend = FALSE) +
  geom_hline(yintercept = 0, linetype = "dotted", colour = "#7e878e") +
  geom_text(aes(x = id, y = stand, label = round(Amount, digits = 2)), size = 3) +
  coord_flip() +
  theme_classic() +
  theme(
    axis.title.x = element_text(vjust = 1),
    text = element_text(size = 12),
    legend.position = "bottom",
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    strip.background = element_blank(),
    legend.title = element_blank(),
  ) +
  labs(
    x = "",
    y = "% Starting REAs",
    caption = "REAs: Risk exposure amounts."
  ) +
  scale_fill_manual("", values = palette) +
  guides(
    fill = guide_legend(title.position = "top", order = 1, override.aes = list(alpha = 1, colour = NA))
  )

ggplotly(chart, tooltip = F)

#### @@
#### III- Decomposition of total assets, liabilities, RWAs ####
#### @@

### Decomposition of total assets

assets <- c("ITM_493", "ITM_494", "ITM_495", "ITM_496", "ITM_497", "ITM_498", "ITM_499", "ITM_500", "ITM_501", "ITM_65")

names(assets) <- c(
  "Cash, cash balances at central banks and other demand deposits",
  "Financial assets held for trading",
  "Non-trading financial assets mandatorily at fair value through profit or loss",
  "Financial assets designated at fair value through profit or loss",
  "Financial assets at fair value through other comprehensive income",
  "Financial assets at amortised cost",
  "Derivatives - Hedge accounting",
  "Fair value changes of the hedged items in portfolio hedge of interest rate risk",
  "Other assets",
  "Total assets"
)

items_rwa <- c(
  "ITM_29", "ITM_405", "ITM_410", "ITM_411", "ITM_412", "ITM_30", "ITM_414", "ITM_418", "ITM_33", "ITM_34",
  "ITM_35", "ITM_32", "ITM_275", "ITM_274", "ITM_155", "ITM_587", "ITM_591"
)

rwas <- c("ITM_405", "ITM_410", "ITM_411", "ITM_412", "ITM_30", "ITM_414", "ITM_418", "ITM_33", "ITM_34", "ITM_700", "ITM_35")

names(rwas) <- c(
  "Credit risk (excluding CCR and Securitisations)",
  "Counterparty credit risk (CCR, excluding CVA)",
  "Credit valuation adjustment - CVA",
  "Settlement risk",
  "Securitisation exposures in the banking book (after the cap)",
  "Position, foreign exchange and commodities risks (market risk)",
  "Large exposures in the trading book",
  "Operational risk",
  "Other risk exposure amounts",
  "Transitional Floors",
  "Total risk exposure amounts"
)

liabilities <- c(
  "ITM_523", "ITM_524", "ITM_525", "ITM_526", "ITM_527", "ITM_528", "ITM_529", "ITM_530", "ITM_531", "ITM_532", "ITM_533", "ITM_534",
  "ITM_535", "ITM_538", "ITM_539"
)
names(liabilities) <- c(
  "Financial liabilities held for trading",
  "Trading financial liabilities",
  "Financial liabilities designated at fair value through profit or loss",
  "Financial liabilities measured at amortised cost",
  "Non-trading non-derivative financial liabilities measured at a cost-based method",
  "Derivatives - Hedge accounting",
  "Fair value changes of the hedged items in portfolio hedge of interest rate risk",
  "Provisions",
  "Tax liabilities",
  "Share capital repayable on demand",
  "Other liabilities",
  "Liabilities included in disposal groups classified as held for sale",
  "Haircuts for trading liabilities at fair value",
  "Total Equity",
  "Total Equity and Total Liabilities"
)

##----------------------------
## Bank, country, or system
bank = "BNP Paribas"
iso = "France"
##----------------------------

tr_rwas = left_join(dt_pnl, bank_names, by = c("Bank_ID"), relationship = "many-to-many") %>% 
  mutate(Country = ifelse(ISO2 != "OT", countrycode(ISO2, origin = "iso2c", destination = "country.name"),
                          ifelse(ISO2 == "OT", "Other", ISO2))) %>% 
  dplyr::select(-Stress_Test_Item, -Transparency_Item, -Item, -retail_sample, -retail_sample_ex) %>%
  mutate(across(c(Exposure, Country_Rank, Maturity, Status, Portfolio, Assets_FV, Assets_Stages, Financial_Instruments, Fin_End_Year, Common_Exposure), ~NA)) %>%
  distinct() %>%
  mutate(Framework = ifelse(Framework == "ST" & Exercise != 2014, NA, Framework)) %>%
  filter(Common_Item %in% items_rwa & rel_period %in% c("Realised", NA) & !is.na(Framework)) %>%
  distinct() %>%
  pivot_wider(names_from = Common_Item, values_from = Amount) %>%
  mutate(ITM_29 = ITM_29 - ITM_30) %>%
  mutate(ITM_405 = coalesce(ITM_405, ITM_29)) %>%
  mutate(ITM_414 = coalesce(ITM_414, ITM_32)) %>%
  mutate(across(matches("ITM_"), ~ ifelse(is.na(.), 0, .))) %>%
  mutate(ITM_700 = ITM_274 + ITM_275 + ITM_591 + ITM_587) %>% ## Including transitional floors in 2013 and 2025
  mutate(ITM_35 = ITM_405 + ITM_410 + ITM_411 + ITM_412 + ITM_30 + ITM_414 + ITM_418 + ITM_33 + ITM_34 + ITM_700) 

tr_assets <- left_join(dt_pnl, bank_names, by = c("Bank_ID"), relationship = "many-to-many") %>% 
  mutate(Country = ifelse(ISO2 != "OT", countrycode(ISO2, origin = "iso2c", destination = "country.name"),
                          ifelse(ISO2 == "OT", "Other", ISO2))) %>% 
  filter(Common_Item %in% c(assets, liabilities)) %>%
  filter(Assets_FV == 0 & Financial_Instruments %in% c(0, NA) & Exposure %in% c(0, NA) & Fin_End_Year == 0) %>% ## total carrying amount only
  dplyr::select(-retail_sample, -retail_sample_ex) %>%
  distinct() %>%
  arrange(Framework, Bank_ID, Exercise, Period, Common_Item, Item) %>%
  mutate(Common_Item = ifelse(Common_Item %in% c("ITM_500", "ITM_499", "ITM_496", "ITM_495"), "ITM_501", Common_Item)) %>% ## Reducing the number of categories by removing the least important ones
  mutate(Common_Item = ifelse(Common_Item %in% c("ITM_529", "ITM_535", "ITM_524", "ITM_532", "ITM_527", "ITM_528", "ITM_530", "ITM_534"), "ITM_533", Common_Item)) %>% ## Reducing the number of categories by removing the least important ones
  group_by(Framework, ISO2, Common_Item, Period, rel_period, Exercise, Scenario) %>%
  mutate(Amount = sum(Amount, na.rm = T)) %>% 
  ungroup() %>%
  dplyr::select(-Stress_Test_Item, -Transparency_Item, -Item) %>%
  distinct() %>%
  pivot_wider(names_from = "Common_Item", values_from = "Amount") 

##------------------------
## Export final datasets
tr_rwas_export = tr_rwas %>% 
  mutate(DB = "tr_rwas")
tr_assets_export = tr_assets %>% 
  mutate(DB = "tr_assets")

## %% Chart

tr_rwas = tr_rwas_export %>% 
  dplyr::select(-DB)
tr_assets = tr_assets_export %>% 
  dplyr::select(-DB)

# assets <- c("ITM_493", "ITM_494", "ITM_495", "ITM_496", "ITM_497", "ITM_498", "ITM_499", "ITM_500", "ITM_501", "ITM_65")
# 
# names(assets) <- c(
#   "Cash, cash balances at central banks and other demand deposits",
#   "Financial assets held for trading",
#   "Non-trading financial assets mandatorily at fair value through profit or loss",
#   "Financial assets designated at fair value through profit or loss",
#   "Financial assets at fair value through other comprehensive income",
#   "Financial assets at amortised cost",
#   "Derivatives - Hedge accounting",
#   "Fair value changes of the hedged items in portfolio hedge of interest rate risk",
#   "Other assets",
#   "Total assets"
# )
# 
# items_rwa <- c(
#   "ITM_29", "ITM_405", "ITM_410", "ITM_411", "ITM_412", "ITM_30", "ITM_414", "ITM_418", "ITM_33", "ITM_34",
#   "ITM_35", "ITM_32", "ITM_275", "ITM_274", "ITM_155", "ITM_587", "ITM_591"
# )
# 
# rwas <- c("ITM_405", "ITM_410", "ITM_411", "ITM_412", "ITM_30", "ITM_414", "ITM_418", "ITM_33", "ITM_34", "ITM_700", "ITM_35")
# 
# names(rwas) <- c(
#   "Credit risk (excluding CCR and Securitisations)",
#   "Counterparty credit risk (CCR, excluding CVA)",
#   "Credit valuation adjustment - CVA",
#   "Settlement risk",
#   "Securitisation exposures in the banking book (after the cap)",
#   "Position, foreign exchange and commodities risks (market risk)",
#   "Large exposures in the trading book",
#   "Operational risk",
#   "Other risk exposure amounts",
#   "Transitional Floors",
#   "Total risk exposure amounts"
# )
# 
# liabilities <- c(
#   "ITM_523", "ITM_524", "ITM_525", "ITM_526", "ITM_527", "ITM_528", "ITM_529", "ITM_530", "ITM_531", "ITM_532", "ITM_533", "ITM_534",
#   "ITM_535", "ITM_538", "ITM_539"
# )
# names(liabilities) <- c(
#   "Financial liabilities held for trading",
#   "Trading financial liabilities",
#   "Financial liabilities designated at fair value through profit or loss",
#   "Financial liabilities measured at amortised cost",
#   "Non-trading non-derivative financial liabilities measured at a cost-based method",
#   "Derivatives - Hedge accounting",
#   "Fair value changes of the hedged items in portfolio hedge of interest rate risk",
#   "Provisions",
#   "Tax liabilities",
#   "Share capital repayable on demand",
#   "Other liabilities",
#   "Liabilities included in disposal groups classified as held for sale",
#   "Haircuts for trading liabilities at fair value",
#   "Total Equity",
#   "Total Equity and Total Liabilities"
# )

##----------------------------
## Bank, country, or system
bank = "BNP Paribas"
iso = "France"
##----------------------------

if(!is.null(bank)){
  tr_rwas = tr_rwas %>%
    filter(Name %in% bank)
  tr_assets = tr_assets %>%
    filter(Name %in% bank)
}else if(!is.null(iso)){
  tr_rwas = tr_rwas %>%
    filter(Country %in% iso) %>%
    group_by(Exercise, Period) %>%
    mutate(across(matches("ITM_"), ~sum(., na.rm = T))) %>%
    ungroup() %>%
    mutate(across(c(Bank_ID, Name), ~"Total")) %>%
    distinct()
  tr_assets = tr_assets %>%
    filter(Country %in% iso) %>%
    group_by(Exercise, Period) %>%
    mutate(across(matches("ITM_"), ~sum(., na.rm = T))) %>%
    ungroup() %>%
    mutate(across(c(Bank_ID, Name), ~"Total")) %>%
    distinct()
}else if(is.null(iso) & is.null(bank)){
  tr_rwas = tr_rwas %>%
    group_by(Exercise, Period) %>%
    mutate(across(matches("ITM_"), ~sum(., na.rm = T))) %>%
    ungroup() %>%
    mutate(across(c(Bank_ID, Name, Country, ISO2), ~"Total")) %>%
    distinct()
  tr_assets = tr_assets %>%
    group_by(Exercise, Period) %>%
    mutate(across(matches("ITM_"), ~sum(., na.rm = T))) %>%
    ungroup() %>%
    mutate(across(c(Bank_ID, Name, Country, ISO2), ~"Total")) %>%
    distinct()
}

tr_rwas = tr_rwas %>%
  mutate(across(c(ITM_405, ITM_410, ITM_411, ITM_412, ITM_30, ITM_414, ITM_418, ITM_33, ITM_34, ITM_700), ~ . / ITM_35)) %>%
  pivot_longer(matches("ITM_"), names_to = "Common_Item", values_to = "Amount") %>%
  mutate(RWA = ifelse(Common_Item %in% rwas[which(names(rwas) != "Total risk exposure amounts")], 1, NA)) %>%
  filter(!is.na(Amount)) %>%
  # mutate(Exposure = factor(str_remove(Common_Item, "ITM_"), str_remove(c(assets, liabilities, rwas), "ITM_"), c(names(assets), names(liabilities), names(rwas)))) %>%
  mutate(Exposure = str_remove(Common_Item, "ITM_")) %>%
  mutate(RWA = ifelse(RWA == 1, "ITM_RWA", NA)) %>%
  mutate(Common_Item = RWA) %>%
  mutate(Scenario = 1) %>%
  filter(!is.na(Common_Item)) %>%
  dplyr::select(-RWA)

tr_assets = tr_assets %>%
  mutate(across(c(ITM_493, ITM_494, ITM_497, ITM_498, ITM_501), ~ . / ITM_65)) %>%
  mutate(across(c(ITM_523, ITM_525, ITM_526, ITM_531, ITM_533, ITM_538), ~ . / ITM_539)) %>%
  pivot_longer(matches("ITM_"), names_to = "Common_Item", values_to = "Amount") %>%
  mutate(Asset = ifelse(Common_Item %in% assets[which(names(assets) != "Total assets")], 1, NA)) %>%
  mutate(Liability = ifelse(Common_Item %in% liabilities[which(names(liabilities) != "Total Equity and Total Liabilities")], 1, NA)) %>%
  filter(!is.na(Amount)) %>%
  # mutate(Exposure = factor(str_remove(Common_Item, "ITM_"), str_remove(c(assets, liabilities, rwas), "ITM_"), c(names(assets), names(liabilities), names(rwas)))) %>%
  mutate(Exposure = str_remove(Common_Item, "ITM_")) %>%
  mutate(Asset = ifelse(Asset == 1, "ITM_ASSETS", NA)) %>%
  mutate(Liability = ifelse(Liability == 1, "ITM_LIABILITIES", NA)) %>%
  mutate(Common_Item = coalesce(Asset, Liability)) %>%
  mutate(Scenario = 1) %>%
  filter(!is.na(Common_Item)) %>%
  dplyr::select(-Asset, -Liability)

tr_assets_fin <- plyr::rbind.fill(tr_assets, tr_rwas) %>%
  mutate(Exposure = factor(paste("ITM", Exposure, sep = "_"), c(rev(assets), rev(rwas), rev(liabilities)), c(rev(names(assets)), rev(names(rwas)), rev(names(liabilities))))) %>%
  filter(!is.na(Amount))

## Assets or RWAs?
itm = "ITM_RWA" ## ITM_ASSETS or ITM_RWA or ITM_LIABILITIES

if(itm == "ITM_LIABILITIES" ){
  title = "Breakdown of Liabilities"
}else if(itm == "ITM_ASSETS"){
  title = "Breakdown of Assets"
}else if(itm == "ITM_RWA"){
  title = "Breakdown of Risk Exposure Amounts"
}

palette = rep(c("#5EC5C2", "#4471B9", "#0FA1F5", "#1858E1", "#51438F", "#18AFC7", "#454DD1"), length(unique(tr_assets$Exposure)))

chart =
  tr_assets_fin %>%
  filter(Common_Item %in% itm) %>%
  ggplot(aes(
    x = as.yearqtr(as.Date(paste0(Period, "01"), format = "%Y%m%d")),
    text = paste0("Exposure: ", Exposure, "\nAmount: ", round(Amount*100, 2), "%")
  )) +
  geom_bar(aes(y = Amount*100, fill = Exposure), stat = "identity", show.legend = T) +
  scale_x_yearqtr(format = "%Y Q%q") +
  scale_y_continuous(labels = scales::comma) +
  labs(
    x = "",
    y = title,
  ) +
  theme_classic() +
  theme(
    axis.text.x = element_text(angle = 45, vjust = 1, hjust = 1),
    axis.title.x = element_text(vjust = 1),
    text = element_text(size = 11),
    legend.position = "bottom"
  ) +
  scale_fill_manual("", values = palette)

ggplotly(chart, tooltip = "text")

#### @@
#### IV- Exposure Networks ####
#### @@

bank_exp_orig <- left_join(dt_exp, bank_names, by = c("Bank_ID"), relationship = "many-to-many") %>% 
  mutate(ISO2 = ifelse(ISO2 != "OT", countrycode(ISO2, origin = "iso2c", destination = "country.name"),
                       ifelse(ISO2 == "OT", "Other", ISO2))) %>%
  mutate(Scenario = factor(Scenario, levels = scenarnames, labels = names(scenarnames))) %>%
  mutate(Scenario = ifelse(is.na(Scenario) | Scenario == "No breakdown", "Actual", as.character(Scenario))) %>%
  filter(Scenario == "Actual") 

item <- c("ITM_16", "ITM_21") ## We go for risk exposure amounts - 16 is IRB, 21 is STA

## Parameters
bank_exp <- bank_exp_orig %>%
  # filter(if (!(str_to_lower("total") %in% str_to_lower(iso))) str_to_lower(ISO2) %in% str_to_lower(iso) else ISO2 %in% ISO2) %>%
  # filter(if (!(str_to_lower("total") %in% bank)) Bank_ID %in% bank else Bank_ID %in% Bank_ID) %>%
  filter(if (!("total" %in% item)) Common_Item %in% item else Common_Item %in% Common_Item) %>%
  dplyr::select(TP, Framework, Bank_ID, Name, ISO2, Period, Exercise, Amount, Country, Status, Portfolio, Perf_Status, Common_Exposure) %>%
  distinct()

# if(exposure == "C000" ){
bank_sta_tr <- bank_exp %>%
  filter(Portfolio == 1 & Framework == "TR" & !(Common_Exposure %in% c("C000", "C304", "C302", "C406", "C407", "C408", "C409", "C410", "C411", "C412", "C405", "C502", "CTR001", "CTR002"))) %>% ## Need to compute total - CTR: securitisation
  group_by(Bank_ID, Period, Country) %>% ## Total defaulted and nondefaulted
  mutate(Amount = sum(Amount, na.rm = T)) %>%
  ungroup() %>%
  mutate(Common_Exposure = "C000") %>%
  mutate(Status = 0) %>%
  mutate(Perf_Status = 0) %>%
  distinct()

bank_irb_tr <- bank_exp %>%
  filter(Portfolio == 2 & Framework == "TR" & Status == 0 &!(Common_Exposure %in% c("C000", "C304", "C302", "C406", "C407", "C408", "C409", "C410", "C411", "C412", "CTR001", "CTR002", "C405", "C502"))) %>% ## Need to compute total
  group_by(Bank_ID, Period, Country) %>% ## Total defaulted and nondefaulted
  mutate(Amount = sum(Amount, na.rm = T)) %>%
  ungroup() %>%
  mutate(Common_Exposure = "C000") %>%
  mutate(Status = 0) %>%
  mutate(Perf_Status = 0) %>%
  distinct()

bank_sta_st <- bank_exp %>%
  filter(Portfolio == 1 & Framework == "ST"  & !(Common_Exposure %in% c("C000", "C304", "C302", "C406", "C407", "C408", "C409", "C410", "C411", "C412", "CST012", "CST013", "C405", "C502", "CST008", "CST009", "CST010", "CST003"))) %>%
  group_by(Bank_ID, Period, Country) %>% ## Total defaulted and nondefaulted
  mutate(Amount = sum(Amount, na.rm = T)) %>%
  ungroup() %>%
  mutate(Common_Exposure = "C000") %>%
  mutate(Status = 0) %>%
  mutate(Perf_Status = 0) %>%
  distinct()

bank_irb_st <- bank_exp %>%
  filter(Portfolio == 2 & Framework == "ST" & Status == 0 
         & !(Common_Exposure %in% c("C000", "C304", "C302", "C406", "C407", "C408", "C409", "C410", "C411", "C412", "CST012", "CST013", "C405", "C502",
                                    "CST008", "CST009", "CST010", "CST003", "CST004", "CST005", "CST007", "CST014"))) %>%
  group_by(Bank_ID, Period, Country) %>% ## Total defaulted and nondefaulted
  mutate(Amount = sum(Amount, na.rm = T)) %>%
  ungroup() %>%
  mutate(Common_Exposure = "C000") %>%
  mutate(Status = 0) %>%
  mutate(Perf_Status = 0) %>%
  distinct()

bank_exp_tr_C000 <- rbind(bank_sta_tr, bank_irb_tr)
bank_exp_st_C000 <- rbind(bank_sta_st, bank_irb_st)

# }else{
bank_sta_tr <- bank_exp %>%
  filter(Portfolio == 1 & Framework == "TR" & Status != 2) ## Defaulted not available for STA

bank_irb_tr <- bank_exp %>%
  filter(Portfolio == 2 & Framework == "TR") %>%
  mutate(Perf_Status = 0) %>%
  pivot_wider(names_from = Status, values_from = Amount) %>%
  mutate(`2` = ifelse(is.na(`2`), 0, `2`)) %>% 
  mutate(`1` = `0`-`2`) %>%
  pivot_longer(c(`1`,`2`,`0`), names_to = "Status", values_to = "Amount") %>%
  filter(Status == 1) ## Removing defaulted from IRB

bank_exp_st <- bank_exp %>%
  filter(Framework == "ST") %>%  ## Defaulted already included for IRB, not STA - removing defaulted for IRB
  mutate(Perf_Status = 0) %>%
  pivot_wider(names_from = Status, values_from = Amount) %>%
  mutate(`2` = ifelse(is.na(`2`), 0, `2`)) %>% 
  mutate(`1` = ifelse(Portfolio == 2, `0`-`2`, `1`)) %>%  ## IRB only
  pivot_longer(c(`1`,`2`,`0`), names_to = "Status", values_to = "Amount") %>%
  filter(Status == 1) %>% 
  filter(Common_Exposure != "C000")

bank_exp_tr <- rbind(bank_sta_tr, bank_irb_tr) %>% 
  filter(Common_Exposure != "C000")
# }

## Important note:
## - IRB = sum of defaulted and nondefaulted // remove defaulted
## - STA = non-defaulted only

bank_exp_total <- rbind(bank_exp_tr, bank_exp_st, bank_exp_tr_C000, bank_exp_st_C000) 

##------------------------
## Export final datasets
bank_exp_total_export = bank_exp_total %>% 
  mutate(DB = "bank_exp_total")

## %% Chart
bank_exp_total = bank_exp_total_export %>% 
  dplyr::select(-DB)

exposures_names <- read_xlsx("../Metadata_DB.xlsx", sheet = "Common_Exposure")

bank_exp_total = bank_exp_total %>%
  # filter(ISO2 == "France") %>% ## Filter here by bank of country (Bank_ID or ISO2) ## Comment to export all data
  distinct() %>%
  pivot_wider(names_from = Framework, values_from = Amount) %>%
  arrange(Bank_ID, ISO2, Period, Country, Common_Exposure) %>%
  mutate(Amount = coalesce(TR, ST)) %>% ## Priority given to TR, then ST
  # filter(!is.na(ST)) %>%
  mutate(Framework = "TR") %>%
  dplyr::select(-TR, -ST) %>%
  distinct() %>%
  filter(Country != 0) %>% ## Removing total to compute it oneself
  group_by(TP, ISO2, Bank_ID, Period, Exercise, Portfolio, Common_Exposure, Country) %>%
  mutate(Amount = sum(Amount, na.rm = T)) %>%
  ungroup() %>%
  mutate(Common_Item = "ITM_SECEXP") %>%
  distinct() %>%
  mutate(Country = factor(Country, as.vector(na.omit(metadata_countries$Label_Country_Final)), as.vector(na.omit(metadata_countries$Value_Country_Final)))) %>% ## Country names
  dplyr::select(ISO2, Bank_ID, Name, Period, Country, Common_Exposure, Portfolio, Amount) %>%
  distinct()

###-------------------
## Parameters
bank = NULL ##"BNP Paribas"
iso = NULL ## "France"
portfolio = "Standardised" ## "IRB", "Standardised", "Total"
exposure = "C000"
i = 39 ## June 2025 ## Select period here

if(!is.null(bank)){
  if(portfolio == "Total"){
    
    data = bank_exp_total %>%
      filter(Name %in% bank & Common_Exposure %in% exposure) %>%
      mutate(Portfolio = 0) %>%
      distinct() %>%
      group_by(ISO2, Bank_ID, Period, Portfolio, Common_Exposure, Country) %>%
      mutate(Amount = sum(Amount, na.rm = T)) %>%
      ungroup() %>%
      distinct()
    
  }else if(portfolio == "IRB"){
    
    data = bank_exp_total %>%
      filter(Name %in% bank & Common_Exposure %in% exposure) %>%
      filter(Portfolio == 2) %>%
      distinct()
    
  }else if(portfolio == "Standardised"){
    
    data = bank_exp_total %>%
      filter(Name %in% bank & Common_Exposure %in% exposure) %>%
      filter(Portfolio == 1) %>%
      distinct()
    
  }
  
}else if(!is.null(iso)){
  
  if(portfolio == "Total"){
    
    data = bank_exp_total %>%
      filter(ISO2 %in% iso & Common_Exposure %in% exposure) %>%
      mutate(Portfolio = 0) %>%
      distinct() %>%
      group_by(ISO2, Bank_ID, Period, Portfolio, Common_Exposure, Country) %>%
      mutate(Amount = sum(Amount, na.rm = T)) %>%
      ungroup() %>%
      distinct()
    
  }else if(portfolio == "IRB"){
    
    data = bank_exp_total %>%
      filter(ISO2 %in% iso & Common_Exposure %in% exposure) %>%
      filter(Portfolio == 2) %>%
      group_by(ISO2, Bank_ID, Period, Portfolio, Common_Exposure, Country) %>%
      mutate(Amount = sum(Amount, na.rm = T)) %>%
      ungroup() %>%
      distinct()
    
  }else if(portfolio == "Standardised"){
    
    data = bank_exp_total %>%
      filter(ISO2 %in% iso & Common_Exposure %in% exposure) %>%
      filter(Portfolio == 1) %>%
      group_by(ISO2, Bank_ID, Period, Portfolio, Common_Exposure, Country) %>%
      mutate(Amount = sum(Amount, na.rm = T)) %>%
      ungroup() %>%
      distinct()
  }
  
  
}else if(is.null(bank) & is.null(iso)){
  
  if(portfolio == "Total"){
    
    data = bank_exp_total %>%
      filter(Common_Exposure %in% exposure) %>%
      mutate(Portfolio = 0) %>%
      distinct() %>%
      group_by(ISO2, Bank_ID, Period, Portfolio, Common_Exposure, Country) %>%
      mutate(Amount = sum(Amount, na.rm = T)) %>%
      ungroup() %>%
      distinct()
    
  }else if(portfolio == "IRB"){
    
    data = bank_exp_total %>%
      filter(Common_Exposure %in% exposure) %>%
      filter(Portfolio == 2) %>%
      distinct() %>%
      group_by(ISO2, Bank_ID, Period, Portfolio, Common_Exposure, Country) %>%
      mutate(Amount = sum(Amount, na.rm = T)) %>%
      ungroup() %>%
      distinct()
    
  }else if(portfolio == "Standardised"){
    
    data = bank_exp_total %>%
      filter(Common_Exposure %in% exposure) %>%
      filter(Portfolio == 1) %>%
      distinct() %>%
      group_by(ISO2, Bank_ID, Period, Portfolio, Common_Exposure, Country) %>%
      mutate(Amount = sum(Amount, na.rm = T)) %>%
      ungroup() %>%
      distinct()
    
  }
  
}

network_dta = data %>%
  group_by(Period) %>% ## % All exposures of the bank
  mutate(REA_all = sum(Amount, na.rm = T)) %>% ## Total Exposures in the sample
  ungroup() %>%
  arrange(ISO2, Period, Country) %>% ## Ensure total is on top
  mutate(Share_all = Amount / REA_all) %>%
  # mutate(Country_Status = ifelse(Country %in% bank_stat$ISO2, "EBA Sample", "Non-EBA")) %>%
  filter(ISO2 != "Other") %>% ## Cannot draw network for this ## Note that this means the sum will not be 1
  filter(Share_all > 0)

periods <- sort(unique(network_dta$Period))

size_orig <- network_dta %>%
  select(Country, Share_all, Period) %>%
  group_by(Country, Period) %>%
  mutate(Share_all = sum(Share_all)) %>%
  ungroup() %>%
  distinct()

### Interactive chart
# for (i in 1:length(periods)) {

period <- periods[i]
bank_exp <- network_dta %>%
  filter(Period == period) %>%
  dplyr::select(-Period)

structure <- unique(c(bank_exp$Name, as.character(bank_exp$Country)))

bank_stat = c("Austria", "Belgium", "Bulgaria", "Cyprus", "Germany", "Denmark", "Estonia", "Spain", "Finland", "France", "United Kingdom", "Greece",
              "Hungary", "Ireland", "Iceland", "Italy", "Liechtenstein", "Lithuania","Luxembourg", "Slovenia", "Latvia", "Malta", "Netherlands", "Norway",
              "Poland", "Portugal", "Romania", "Sweden", "Other")

size <- size_orig %>%
  filter(Period == period) %>%
  dplyr::select(-Period) %>%
  right_join(data.frame(Country = structure), by = "Country")

nodes <- size %>%
  mutate(id = Country) %>%
  rename(label = Country) %>%
  mutate(
    group = ifelse(id %in% bank_stat, "EBA Sample", "Non-EBA"),
    group = ifelse(is.na(Share_all), "Banks", group),
    Share_all = ifelse(is.na(Share_all), 0, Share_all),
    value = Share_all * 100, # Scale for visibility
    id = as.character(id),
    label = as.character(label),
    title = paste0("Share of Exposure: ", round(value, 2), "%"),
    title = ifelse(group == "Banks", "Issuer", title)
  ) %>%
  select(id, label, group, value, title)

edges <- bank_exp %>%
  mutate(
    from = Name,
    to = as.character(Country),
    value = Share_all * 10 # Scale for visibility
  ) %>%
  filter(from != to) %>% # Remove self-loops
  select(from, to, value)

sorted_nodes_ids <- nodes %>%
  filter(group != "Banks") %>%
  arrange(desc(value)) %>%
  pull(id)

# if(scenario == "Actual"){scenar = "Reported Values"}else{scenar = paste(scenario, "Scenario")}

title <- paste(
  '<span style="font-family:Lato;">Bank Risk Exposure Amounts - Non-Defaulted -', portfolio, "Exposures -",
  lubridate::month(as.numeric(str_extract(period, "\\d{2}$")), label = T, abbr = F),
  as.numeric(str_extract(period, "^\\d{4}")), "- Reported values"
)
caption <- "<span style='font-family:Lato;'>Size of dots relates to the share of exposures in the sample."

bk_exp <-
  visNetwork(nodes, edges, main = title, submain = paste0("\n", caption)) %>%
  visNodes(size = "value",  title = "title") %>%
  visEdges(smooth = list(enabled = TRUE, type = "continuous"), arrows = NULL, color = list(color = "#cccccc", highlight = "#7e878e"), font = list(family = "Lato")) %>%
  visGroups(groupname = "EBA Sample", color = "#5EC5C2") %>%
  visGroups(groupname = "Non-EBA", color = "#0083A0") %>%
  visGroups(groupname = "Banks", color = "#BF9F66") %>%
  visPhysics(solver = "hierarchicalRepulsion", hierarchicalRepulsion = list(gravitationalConstant = -20000, springLength = 500, springConstant = .1, damping = 2)) %>%
  visOptions(highlightNearest = list(enabled = T, hover = T), nodesIdSelection = list(enabled = TRUE, values = sorted_nodes_ids, main = "All Countries"),
             selectedBy = list(variable = "group", main = "All Groups")) %>%
  visLayout(randomSeed = 123) %>%
  visLegend(useGroups = F)

bk_exp

##--------------------------
## Sovereign exposures
## Before 2016 NET DIRECT EXPOSURES (accounting value gross of provisions) ITM_237
## 2016-2017 Financial assets: Carrying Amount - broken down by country ITM_570
## 2018+ Direct exposures - On balance sheet - Total carrying amount of non-derivative financial assets (net of short positions) ITM_483

sov_exp <- left_join(dt_sov, bank_names, by = c("Bank_ID"), relationship = "many-to-many") %>% 
  mutate(ISO2 = ifelse(ISO2 != "OT", countrycode(ISO2, origin = "iso2c", destination = "country.name"),
                       ifelse(ISO2 == "OT", "Other", ISO2))) %>% 
  filter(Common_Item %in% c("ITM_237", "ITM_570", "ITM_483") & Country != 279) %>%  
  dplyr::select(-Stress_Test_Item, -Transparency_Item, -Item, -Scenario, -rel_period) %>%
  mutate(Amount = ifelse(Common_Item == "ITM_570" & Period == 201512 & Framework == "TR", NA, Amount)) %>%
  filter(!is.na(Amount)) %>%
  filter(Country != 0) %>%
  mutate(Maturity = ifelse(Maturity %in% c(NA, 0), 8, Maturity)) %>% 
  # group_by(ISO2, Bank_ID, Period, Exercise, Country, TP) %>%
  # mutate(Amount = sum(Amount, na.rm = T)) %>%
  # ungroup() %>%
  mutate(Common_Item = "ITM_SOVEXP") %>%
  distinct() %>%
  mutate(Country = factor(Country, as.vector(na.omit(metadata_countries$Label_Country_Final)), as.vector(na.omit(metadata_countries$Value_Country_Final)))) %>% ## Country names
  dplyr::select(ISO2, Bank_ID, Name, Period, Country, Maturity, Amount) %>%
  distinct()

##------------------------
## Export final datasets
sov_exp_export = sov_exp %>% 
  mutate(DB = "sov_exp")

## %% Chart

sov_exp = sov_exp_export %>% 
  dplyr::select(-DB)

##-------
maturity = 8 ## 1 to 8, 8 is Total
bank = NULL ## "BNP Paribas"
iso =  NULL ##"France"

if(!is.null(bank)){
  
  data_sov = sov_exp %>%
    filter(Name %in% bank & Maturity %in% maturity)
  
}else if(!is.null(iso)){
  
  data_sov = sov_exp %>%
    filter(ISO2 %in% iso & Maturity %in% maturity)
  
}else if(is.null(bank) & is.null(iso)){
  
  data_sov = sov_exp %>%
    filter(Maturity %in% maturity)
}

data_sov = data_sov %>%
  group_by(Period) %>%
  mutate(Exp_all = sum(Amount, na.rm = T)) %>% ## Total Exposures in the sample
  ungroup() %>%
  arrange(ISO2, Bank_ID, Period, Country) %>% ## Ensure total is on top
  mutate(Share_all = Amount / Exp_all) %>%
  filter(ISO2 != "Other") %>%  ## Cannot draw network for this
  filter(Share_all > 0)

periods <- sort(unique(data_sov$Period))

bank_stat <- data_sov %>%
  dplyr::select(ISO2) %>%
  distinct()

size_orig <- data_sov %>%
  select(Country, Share_all, Period) %>%
  group_by(Country, Period) %>%
  mutate(Share_all = sum(Share_all, na.rm = T)) %>%
  ungroup() %>%
  distinct()

### Interactive chart
# for (i in 1:length(periods)) {
i = length(periods)
period <- periods[i]

data_sov <- data_sov %>%
  filter(Period == period) %>%
  dplyr::select(-Period)

structure <- unique(c(data_sov$Name, as.character(data_sov$Country)))

bank_stat = c("Austria", "Belgium", "Bulgaria", "Cyprus", "Germany", "Denmark", "Estonia", "Spain", "Finland", "France", "United Kingdom", "Greece",
              "Hungary", "Ireland", "Iceland", "Italy", "Liechtenstein", "Lithuania","Luxembourg", "Slovenia", "Latvia", "Malta", "Netherlands", "Norway",
              "Poland", "Portugal", "Romania", "Sweden", "Other")

size <- size_orig %>%
  filter(Period == period) %>%
  dplyr::select(-Period) %>%
  right_join(data.frame(Country = structure), by = "Country")

nodes <- size %>%
  mutate(id = Country) %>%
  rename(label = Country) %>%
  mutate(
    group = ifelse(id %in% bank_stat, "EBA Sample", "Non-EBA"),
    group = ifelse(is.na(Share_all), "Banks", group),
    Share_all = ifelse(is.na(Share_all), 0, Share_all),
    value = Share_all * 100, # Scale for visibility
    id = as.character(id),
    label = as.character(label),
    title = paste0("Share of Exposure: ", round(value, 2), "%"),
    title = ifelse(group == "Banks", "Holder", title)
  ) %>%
  select(id, label, group, value, title)

edges <- data_sov %>%
  mutate(
    from = Name,
    to = as.character(Country),
    value = Share_all * 10 # Scale for visibility
  ) %>%
  filter(from != to) %>% # Remove self-loops
  select(from, to, value)

sorted_nodes_ids <- nodes %>%
  filter(group != "Banks") %>%
  arrange(desc(value)) %>%
  pull(id)

matur = seq(1,8)
names(matur) = c("[0-3M]",
                 "[3M-1Y]",
                 "[1Y-2Y]",
                 "[2Y-3Y]",
                 "[3Y-5Y]",
                 "[5Y-10Y]",
                 "[10Y+]",
                 "Total")

title <- paste(
  '<span style="font-family:Lato;">Bank General Government Exposures -', names(matur[matur == maturity]), 'Maturity - ',
  lubridate::month(as.numeric(str_extract(period, "\\d{2}$")), label = T, abbr = F),
  as.numeric(str_extract(period, "^\\d{4}")), "- Reported Values"
)
caption <- "<span style='font-family:Lato;'>Size of dots relates to the share of exposures in the sample. Pre-2016: Net Direct Long Exposures. 2016-2018: Financial Assets. 2019-: Direct Exposures on Balance Sheet."

bk_exp <-
  visNetwork(nodes, edges, main = title, submain = paste0("\n", caption)) %>%
  visNodes(size = "value",  title = "title") %>%
  visEdges(smooth = list(enabled = TRUE, type = "continuous"), arrows = NULL, color = list(color = "#cccccc", highlight = "#7e878e"), font = list(family = "Lato")) %>%
  visGroups(groupname = "EBA Sample", color = "#5EC5C2") %>%
  visGroups(groupname = "Non-EBA", color = "#0083A0") %>%
  visGroups(groupname = "Banks", color = "#BF9F66") %>%
  visPhysics(solver = "hierarchicalRepulsion", hierarchicalRepulsion = list(gravitationalConstant = -20000, springLength = 500, springConstant = .1, damping = 2)) %>%
  visOptions(highlightNearest = list(enabled = T, hover = T), nodesIdSelection = list(enabled = TRUE, values = sorted_nodes_ids, main = "All Countries"),
             selectedBy = list(variable = "group", main = "All Groups")) %>%
  visLayout(randomSeed = 123) %>%
  visLegend(useGroups = F)

bk_exp

# }

#### @@
####  V- NACE Exposures ####
#### @@

###---------------
## NACE exposures
naces <- seq(0, 19, 1)
names(naces) <- c(
  "No breakdown", "A Agriculture", "B Mining", "C Manufacturing",
  "D Electricity", "E Water", "F Construction",
  "G Trade", "H Transport", "I Accommodation",
  "J Communication", "K Financials", "L Real Estate",
  "M Professional", "N Support Service",
  "O Administration", "P Education",
  "Q Health", "R Recreation", "S Other"
)

bank_nace <- bank_exp_orig %>%
  filter(Common_Item %in% c("ITM_540")) %>%
  dplyr::select(TP, Framework, Bank_ID, Name, ISO2, Period, Exercise, Amount, Status, Perf_Status, Common_Item, NACE_Codes) %>%
  distinct() %>% 
  mutate(NACE_Codes = paste("NACE", NACE_Codes, sep = "_")) %>%
  pivot_wider(names_from = NACE_Codes, values_from = Amount)

##------------------------
## Export final datasets
bank_nace_export = bank_nace %>% 
  mutate(DB = "bank_nace")

## %% Chart
bank_nace = bank_nace_export %>% 
  dplyr::select(-DB)

bank = NULL ## "BNP Paribas"
iso = "France"
period = 202412
item = "Total" ## NPL or Total

if(item == "Total"){itm = "ITM_EXP"}else{itm = "ITM_NPL"}

if(!is.null(bank)){
  chart_nace = bank_nace %>%
    filter(Name %in% bank)
  
}else if(!is.null(iso)){
  chart_nace = bank_nace %>%
    filter(ISO2 %in% iso) %>%
    group_by(ISO2, Period, Perf_Status) %>%
    mutate(across(matches("NACE_"), ~sum(., na.rm = T))) %>%
    ungroup() %>%
    mutate(Bank_ID = "Total") %>%
    mutate(Name = "Total") %>%
    ungroup() %>%
    distinct()
  
}else if(is.null(bank) & is.null(iso)){
  chart_nace = bank_nace %>%
    group_by(Period, Perf_Status) %>%
    mutate(across(matches("NACE_"), ~sum(., na.rm = T))) %>%
    ungroup() %>%
    mutate(Bank_ID = "Total") %>%
    mutate(ISO2 = "Total") %>%
    mutate(Name = "Total") %>%
    ungroup() %>%
    distinct()
  
}

chart_nace = chart_nace %>%
  mutate(across(matches("NACE_[1-9]"), ~ . / NACE_0, .names = "EXP_{.col}")) %>%
  arrange(ISO2, Bank_ID, Period, Perf_Status) %>%
  filter(Perf_Status %in% c(0,2)) %>%  ## total, non-performing
  group_by(Bank_ID, Period) %>%
  mutate(across(matches("^NACE_[0-9]"), ~ . / dplyr::lag(., 1), .names = "NPL_{.col}")) %>%
  ungroup() %>%
  pivot_longer(matches("NACE"), names_to = "NACE_Codes", values_to = "Amount") %>%
  filter(str_detect(NACE_Codes, "EXP|NPL")) %>%
  mutate(Common_Item = ifelse(str_detect(NACE_Codes,"EXP"), "ITM_EXP",
                              ifelse(str_detect(NACE_Codes, "NPL"), "ITM_NPL", Common_Item))) %>%
  mutate(Amount = ifelse(Common_Item == "ITM_EXP" & Perf_Status != 0, NA,
                         ifelse(Common_Item == "ITM_NPL" & Perf_Status != 2, NA, Amount)
  )) %>%
  filter(!is.na(Amount)) %>%
  mutate(Exposure = as.numeric(str_extract(NACE_Codes, "\\d{1,}"))) %>%
  arrange(ISO2, Period, Common_Item) %>%
  dplyr::select(where(~ !all(is.na(.x)))) %>%
  filter(Exposure > 0)

## Test chart
# chart_data <- chart_nace %>%
#   filter(Period == period & Common_Item %in% itm) %>%
#   mutate(Exposure = factor(Exposure, naces, names(naces))) %>%
#   mutate(Perf_Status = ifelse(Perf_Status == 0, "Total", "(of which) Nonperforming")) %>%
#   mutate(
#     labels = paste(Exposure, paste0(round(Amount*100, 2), "%"), sep = " - "),
#     parents = Perf_Status,
#     values = Amount * 100
#   )
# 
# chart =
#   chart_data %>%
#   ggplot(aes(area = Amount*100, fill = Amount*100)) +
#   geom_treemap(start = "topleft", show.legend = F) +
#   geom_treemap_text(aes(label = paste(Exposure, paste0("\n", round(Amount*100, 2), "%"), sep = " - ")), colour = "white", place = "top", start = "topleft",
#                     grow = TRUE) +
#   scale_fill_gradientn(colors = c("#5EC5C2", "#4471B9", "#0FA1F5", "#1858E1", "#51438F", "#18AFC7"))

exp_data <- chart_nace %>%
  mutate(Exposure = factor(Exposure, naces, names(naces))) %>%
  mutate(Exposure = as.character(Exposure)) %>%
  filter(Period == period & Common_Item == "ITM_EXP") %>%
  mutate(
    labels = Exposure,
    parents = "",  # Root level
    values = Amount * 100,
    display_text = paste(Exposure, paste0(round(Amount*100, 2), "%"), sep = " - "),
    color_val = Amount * 100,
    item_type = "EXP"
  )

# Get NPL data (subgroups)
npl_data <- chart_nace %>%
  mutate(Exposure = factor(Exposure, naces, names(naces))) %>%
  mutate(Exposure = as.character(Exposure)) %>%
  filter(Period == period & Common_Item == "ITM_NPL") %>%
  mutate(
    labels = paste0(Exposure, "_NPL"),  # Unique label for NPL
    parents = Exposure,  # Parent is the exposure with same name
    values = Amount * 100,
    display_text = paste("(of which) NPL", paste0(round(Amount*100, 2), "%"), sep = " - "),
    color_val = -1,  # Flag for NPL coloring
    item_type = "NPL"
  )

# Combine data
plot_data <- bind_rows(exp_data, npl_data) %>% 
  mutate(ids = labels,  # Keep original labels as IDs
         labels = display_text)  # Use display_text for display

plot_ly(
  data = plot_data,
  type = "treemap",
  ids = ~ids,  # Use original labels as unique identifiers
  labels = ~labels,  # Use display_text for what's shown
  parents = ~parents,  # Parents should still reference the original IDs
  values = ~values,
  textposition = "top left",
  textfont = list(color = "white"),
  hovertemplate = paste0(
    "%{label}<br>",
    "<extra></extra>"
  ),
  marker = list(
    colors = ~ifelse(color_val == -1, "rgba(128, 128, 128, 0.6)", color_val),
    colorscale = list(
      c(0, "#5EC5C2"),
      c(0.05, "#4471B9"),
      c(0.1, "#0FA1F5"),
      c(0.15, "#1858E1"),
      c(0.2, "#51438F"),
      c(1, "#18AFC7")
    ),
    line = list(color = "#FFFFFF", width = 2)
  ),
  branchvalues = "remainder"
)

####--------------------------- ##
#### Export whole final dataset

chart_db = plyr::rbind.fill(tr_ratios_export, final_waterfall_export, tr_rwas_export, tr_assets_export, bank_exp_total_export, bank_nace_export, sov_exp_export)

chart_output_dir <- "../Original Data/Chart Data"
dir.create(chart_output_dir, recursive = TRUE, showWarnings = FALSE)

arrow::write_parquet(tr_ratios_export %>% dplyr::select(-DB), file.path(chart_output_dir, "tr_ratios.parquet"), compression = "snappy")
arrow::write_parquet(final_waterfall_export %>% dplyr::select(-DB), file.path(chart_output_dir, "final_waterfall.parquet"), compression = "snappy")
arrow::write_parquet(tr_rwas_export %>% dplyr::select(-DB), file.path(chart_output_dir, "tr_rwas.parquet"), compression = "snappy")
arrow::write_parquet(tr_assets_export %>% dplyr::select(-DB), file.path(chart_output_dir, "tr_assets.parquet"), compression = "snappy")
arrow::write_parquet(bank_exp_total_export %>% dplyr::select(-DB), file.path(chart_output_dir, "bank_exp_total.parquet"), compression = "snappy")
arrow::write_parquet(bank_nace_export %>% dplyr::select(-DB), file.path(chart_output_dir, "bank_nace.parquet"), compression = "snappy")
arrow::write_parquet(sov_exp_export %>% dplyr::select(-DB), file.path(chart_output_dir, "sov_exp.parquet"), compression = "snappy")

arrow::write_parquet(chart_db, "../Original Data/chart_db.parquet", compression = "snappy")

write.fst(chart_db, "../Original Data/chart_db.fst", compress = 100)

# ## Code to Import datasets (once and for all)
# chart_db = read.fst("../Original Data/chart_db.fst")
# 
# tr_ratios = chart_db %>%
#   filter(DB == "tr_ratios") %>% 
#   dplyr::select(-DB) %>% 
#   select_if(function(x) !(all(is.na(x)) | all(x=="")))
# final_waterfall = chart_db %>% 
#   filter(DB == "final_waterfall") %>% 
#   dplyr::select(-DB) %>% 
#   select_if(function(x) !(all(is.na(x)) | all(x=="")))
# tr_rwas = chart_db %>% 
#   filter(DB == "tr_rwas") %>% 
#   dplyr::select(-DB) %>% 
#   select_if(function(x) !(all(is.na(x)) | all(x=="")))
# tr_assets = chart_db %>% 
#   filter(DB == "tr_assets") %>% 
#   dplyr::select(-DB) %>% 
#   select_if(function(x) !(all(is.na(x)) | all(x=="")))
# bank_exp_total = chart_db %>% 
#   filter(DB == "bank_exp_total") %>% 
#   dplyr::select(-DB) %>% 
#   select_if(function(x) !(all(is.na(x)) | all(x=="")))
# sov_exp = chart_db %>% 
#   filter(DB == "sov_exp") %>% 
#   dplyr::select(-DB) %>% 
#   select_if(function(x) !(all(is.na(x)) | all(x=="")))
# bank_nace = chart_db %>% 
#   filter(DB == "bank_nace") %>% 
#   dplyr::select(-DB) %>% 
#   select_if(function(x) !(all(is.na(x)) | all(x=="")))

