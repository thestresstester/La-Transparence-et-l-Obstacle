# rm(list = ls())
# cat("\014")
# if (requireNamespace("rstudioapi", quietly = TRUE) && rstudioapi::isAvailable()) {
#   editor_path <- tryCatch(rstudioapi::getSourceEditorContext()$path, error = function(...) "")
#   if (nzchar(editor_path)) {
#     setwd(dirname(editor_path))
#   }
# }

library(shiny)
library(shinyjs)
library(shinybusy)
library(DT)
library(htmltools)
library(tidyverse)
library(readxl)
library(Hmisc) 
library(writexl)
library(zip)
library(countrycode)
library(plotly)
library(ggridges)
library(visNetwork)
library(zoo)
library(lubridate)
library(markdown)
library(duckdb)
library(arrow)

source("Meta/Functions/Global.R")

addResourcePath(
  "downloads",
  normalizePath("Meta/Original Data", winslash = "/", mustWork = TRUE)
)

encode_download_path <- function(path) {
  path_segments <- strsplit(path, "/", fixed = TRUE)[[1]]
  encoded_segments <- vapply(path_segments, utils::URLencode, character(1), reserved = TRUE)
  paste(encoded_segments, collapse = "/")
}

build_static_download_href <- function(path) {
  paste0("downloads/", encode_download_path(path))
}

static_download_button <- function(path, download_name) {
  tags$a(
    href = build_static_download_href(path),
    download = download_name,
    class = "btn btn-default circle-button",
    icon("download")
  )
}

ui <- fluidPage(
  
  useShinyjs(),
  
  tags$head(
    tags$style(HTML("
      .pill-button .fa {
        display: none;
      }
    ")),
    tags$link(rel = "stylesheet", type = "text/css", href = "styles.css"),
    tags$script(HTML("
      $(document).ready(function() {
        $('body').on('click', '.main-title', function(e) {
          e.preventDefault();
          $('html, body').animate({ scrollTop: 0 }, 'slow');
        });
      });
    "))
  ),
  div(class = "main-title",
      br(),
      tags$h1(HTML("La <span class='transparence-color'>Transparence</span> & l'Obstacle"))                                          
  ),
  tabsetPanel(id = "main-tabs",
              tabPanel("Overview",
                       div(class = "markdown-content", br(), includeMarkdown(includeMarkdownSection("USER GUIDE"))),
                       br(),
                       fluidRow(
                         column(3),
                         column(3,
                                div(class = "dataset-box",
                                    style = "margin: 20px; text-align: center;",
                                    fluidRow(
                                      div(class = "button-container",
                                          p("Download Total Metadata"),
                                          downloadButton("downloadTotalMetadata_overview", "", class = "circle-button")
                                      )
                                    ))),
                         column(3,
                                div(class = "dataset-box",
                                    style = "margin: 20px; text-align: center;",
                                    div(class = "button-container",
                                        p("Download Loading Script"),
                                        downloadButton("downloadRScript_overview", "", class = "circle-button")
                                    )
                                )
                         ),
                         column(3),
                       )
              ),
              
              
              tabPanel("Stress-Testing Exercises",
                       div(class = "full-width-box",
                           fluidRow(class = "v-align-row",
                                    column(1),
                                    column(4,
                                           div(class = "dataset-box",
                                               h4("EU-Wide Stress-Tests Dataset", style = "text-align: center;"),
                                               fluidRow(
                                                 column(3),
                                                 column(2,
                                                        div(class = "button-container",
                                                            p("Load"),
                                                            actionButton("loadSTdata", icon("file-alt", style = "color: #BF9F66; font-size: 2.0em;"), class = "circle-button")
                                                        )
                                                 ),
                                                 column(2,
                                                        div(class = "button-container",
                                                            p("Download"),
                                                      static_download_button("Total_Stress_Tests.parquet", "EU-Wide-Stress-Tests-Dataset.parquet")
                                                        )
                                                 ),
                                                 column(2,
                                                        div(class = "button-container",
                                                            p("Reset"),
                                                            actionButton("resetSTdata", icon("times", style = "color: #BF9F66; font-size: 2.5em;"), class = "circle-button")
                                                        )
                                                 ),
                                                 column(3)                                               )
                                           )
                                    ),
                                    
                                    column(2,
                                           tags$div(style = "display: flex; align-items: center; justify-content: center; height: 10%;",
                                                    div(class = "circle-box",
                                                        shinyjs::hidden(tags$img(src = "KC.gif", id = "loading_gif_st", class = "circle-gif"))
                                                    )
                                           )
                                    ),
                                    
                                    column(4,
                                           div(class = "dataset-box",
                                               h4("SSM Stress-Tests Dataset", style = "text-align: center;"),
                                               fluidRow(
                                                 column(3),
                                                 column(2,
                                                        div(class = "button-container",
                                                            p("Load"),
                                                            actionButton("loadSSMdata", icon("file-alt", style = "color: #BF9F66; font-size: 2.0em;"), class = "circle-button")
                                                        )
                                                 ),
                                                 column(2,
                                                        div(class = "button-container",
                                                            p("Download"),
                                                      static_download_button("Total_SSM.parquet", "SSM-Stress-Tests-Dataset.parquet")
                                                        )
                                                 ),
                                                 column(2,
                                                        div(class = "button-container",
                                                            p("Reset"),
                                                            actionButton("resetSSMdata", icon("times", style = "color: #BF9F66; font-size: 2.5em;"), class = "circle-button")
                                                        )
                                                 ),
                                                 column(3)
                                               )
                                           )
                                    ),
                                    column(1)
                           ),
                           conditionalPanel(
                             condition = "output.st_data_loaded",
                             fluidRow(class = "equal-height-row",
                                      column(6,
                                             div(class = "dataset-box scroll-box",
                                                 conditionalPanel(
                                                   condition = "output.st_data_visible",
                                                   uiOutput("dynamic_st_itemtable")
                                                 ),
                                             )
                                      ),
                                      column(6,
                                             div(id = "st_filters_wrapper",
                                                 div(class = "dataset-box",
                                                     conditionalPanel(
                                                       condition = "output.st_data_visible",
                                                       fluidRow(
                                                         column(3,
                                                                selectizeInput("country_filter_st", "Select Country",
                                                                               choices = c("All Countries", countries_st),
                                                                               multiple = TRUE)
                                                         ),
                                                         column(3,
                                                                selectizeInput("bank_filter_st", "Select Bank",
                                                                               choices = c("All Banks", initial_bank_st),
                                                                               multiple = TRUE)
                                                         ),
                                                         column(3,
                                                                selectizeInput("exercise_filter_st", "Select Exercise",
                                                                               choices = NULL,
                                                                               multiple = TRUE)
                                                         ),
                                                         column(3,
                                                                selectizeInput("item_filter_st", "Select Item",
                                                                               choices = NULL,
                                                                               multiple = TRUE)
                                                         )
                                                       ),
                                                       uiOutput("dynamic_st_filters"),
                                                       fluidRow(
                                                         column(2,
                                                                div(class = "button-container",
                                                                    p("Go"),
                                                                    actionButton("go_button_st", icon("play-circle", style = "color: #BF9F66; font-size: 2.5em;"), class = "circle-button")
                                                                )
                                                         ),
                                                         column(2,
                                                                div(class = "button-container",
                                                                    p("Reset"),
                                                                    actionButton("reset_dynamic_filters_st", icon("times", style = "color: #BF9F66; font-size: 2.5em;"), class = "circle-button")
                                                                )
                                                         ),
                                                         column(2,
                                                                div(class = "button-container",
                                                                    p("Download"),
                                                                    downloadButton("downloadFilteredData_st", "", class = "circle-button")
                                                                )
                                                         )
                                                       )
                                                     ),
                                                 )
                                             )
                                      )
                             ),
                             fluidRow(
                               column(12,
                                      div(class = "dataset-box scroll-box sticky-table-container",
                                          DTOutput("ST_datatable")
                                      )
                               )
                             )
                           )
                       )
                       
              ),
              
              
              
              
              tabPanel("Transparency Exercise",
                       div(class = "full-width-box",
                           fluidRow(class = "v-align-row",
                                    column(3),
                                    column(4,
                                           div(class = "dataset-box",
                                               h4("Transparency Exercise Dataset", style = "text-align: center;"),
                                               fluidRow(
                                                 column(3),
                                                 column(2,
                                                        div(class = "button-container",
                                                            p("Load"),
                                                            actionButton("loadTRdata", icon("file-alt", style = "color: #BF9F66; font-size: 2.0em;"), class = "circle-button")
                                                        )
                                                 ),
                                                 column(2,
                                                        div(class = "button-container",
                                                            p("Download"),
                                                      static_download_button("Total_Transparency.parquet", "Transparency-Exercise-Dataset.parquet")
                                                        )
                                                 ),
                                                 column(2,
                                                        div(class = "button-container",
                                                            p("Reset"),
                                                            actionButton("resetTRdata", icon("times", style = "color: #BF9F66; font-size: 2.5em;"), class = "circle-button")
                                                        )
                                                 ),
                                                 column(3),
                                               )
                                           )
                                    ),
                                    
                                    column(2,
                                           tags$div(style = "display: flex; align-items: center; justify-content: center; height: 10%;",
                                                    div(class = "circle-box",
                                                        shinyjs::hidden(tags$img(src = "KC.gif", id = "loading_gif_tr", class = "circle-gif"))
                                                    )
                                           )
                                    ),
                           ),
                           conditionalPanel(
                             condition = "output.tr_data_loaded",
                             fluidRow(class = "equal-height-row",
                                      column(6,
                                             div(class = "dataset-box scroll-box",
                                                 conditionalPanel(
                                                   condition = "output.tr_data_visible",
                                                   DTOutput("tr_itemtable")
                                                 ),
                                             )
                                      ),
                                      column(6,
                                             div(id = "tr_filters_wrapper",
                                                 div(class = "dataset-box",
                                                     conditionalPanel(
                                                       condition = "output.tr_data_visible",
                                                       fluidRow(
                                                         column(3,
                                                                selectizeInput("country_filter_tr", "Select Country",
                                                                               choices = c("All Countries", countries_st),
                                                                               multiple = TRUE)
                                                         ),
                                                         column(3,
                                                                selectizeInput("bank_filter_tr", "Select Bank",
                                                                               choices = c("All Banks", initial_bank_st),
                                                                               multiple = TRUE)
                                                         ),
                                                         column(3,
                                                                selectizeInput("exercise_filter_tr", "Select Exercise",
                                                                               choices = NULL,
                                                                               multiple = TRUE)
                                                         ),
                                                         column(3,
                                                                selectizeInput("item_filter_tr", "Select Item",
                                                                               choices = NULL,
                                                                               multiple = TRUE)
                                                         )
                                                       ),
                                                       uiOutput("dynamic_tr_filters"),
                                                       fluidRow(
                                                         column(2,
                                                                div(class = "button-container",
                                                                    p("Go"),
                                                                    actionButton("go_button_tr", icon("play-circle", style = "color: #BF9F66; font-size: 2.5em;"), class = "circle-button")
                                                                )
                                                         ),
                                                         column(2,
                                                                div(class = "button-container",
                                                                    p("Reset"),
                                                                    actionButton("reset_dynamic_filters_tr", icon("times", style = "color: #BF9F66; font-size: 2.5em;"), class = "circle-button")
                                                                )
                                                         ),
                                                         column(2,
                                                                div(class = "button-container",
                                                                    p("Download"),
                                                                    downloadButton("downloadFilteredData_tr", "", class = "circle-button")
                                                                )
                                                         )
                                                       )
                                                     ),
                                                 )
                                             )
                                      )
                             ),
                             fluidRow(
                               column(12,
                                      div(class = "dataset-box scroll-box sticky-table-container",
                                          DTOutput("TR_datatable")
                                      )
                               )
                             )
                           )
                       )
                       
              ),
              
              
              tabPanel("Thematic Datasets",
                       div(class = "full-width-box",
                           fluidRow(class = "v-align-row",
                                    column(2),
                                    column(4,
                                           div(class = "dataset-box",
                                               h4("Profit, Losses, Capital Dataset", style = "text-align: center;"),
                                               fluidRow(
                                                 column(3),
                                                 column(2,
                                                        div(class = "button-container",
                                                            p("Load"),
                                                            actionButton("loadPLCdata", icon("file-alt", style = "color: #BF9F66; font-size: 2.0em;"), class = "circle-button")
                                                        )
                                                 ),
                                                 column(2,
                                                        div(class = "button-container",
                                                            p("Download"),
                                                      static_download_button("Merged Datasets/EBA_PLC.parquet", "Profit-Losses-Capital-Dataset.parquet")
                                                        )
                                                 ),
                                                 column(2,
                                                        div(class = "button-container",
                                                            p("Reset"),
                                                            actionButton("resetPLCdata", icon("times", style = "color: #BF9F66; font-size: 2.5em;"), class = "circle-button")
                                                        )
                                                 ),
                                                 column(3),
                                               )
                                           )
                                    ),
                                    column(4,
                                           div(class = "dataset-box",
                                               h4("Sector Exposure Dataset", style = "text-align: center;"),
                                               fluidRow(
                                                 column(3),
                                                 column(2,
                                                        div(class = "button-container",
                                                            p("Load"),
                                                            actionButton("loadEXPdata", icon("file-alt", style = "color: #BF9F66; font-size: 2.0em;"), class = "circle-button")
                                                        )
                                                 ),
                                                 column(2,
                                                        div(class = "button-container",
                                                            p("Download"),
                                                      static_download_button("Merged Datasets/EBA_Exposure.parquet", "Sector-Exposure-Dataset.parquet")
                                                        )
                                                 ),
                                                 column(2,
                                                        div(class = "button-container",
                                                            p("Reset"),
                                                            actionButton("resetEXPdata", icon("times", style = "color: #BF9F66; font-size: 2.5em;"), class = "circle-button")
                                                        )
                                                 ),
                                                 column(3),
                                               )
                                           )
                                    ),
                                    column(2)
                           ),
                           fluidRow(class = "v-align-row",
                                    column(1),
                                    column(4,
                                           div(class = "dataset-box",
                                               h4("Sovereign Exposure Dataset", style = "text-align: center;"),
                                               fluidRow(
                                                 column(3),
                                                 column(2,
                                                        div(class = "button-container",
                                                            p("Load"),
                                                            actionButton("loadSOVdata", icon("file-alt", style = "color: #BF9F66; font-size: 2.0em;"), class = "circle-button")
                                                        )
                                                 ),
                                                 column(2,
                                                        div(class = "button-container",
                                                            p("Download"),
                                                      static_download_button("Merged Datasets/EBA_Sovereign.parquet", "Sovereign-Exposure-Dataset.parquet")
                                                        )
                                                 ),
                                                 column(2,
                                                        div(class = "button-container",
                                                            p("Reset"),
                                                            actionButton("resetSOVdata", icon("times", style = "color: #BF9F66; font-size: 2.5em;"), class = "circle-button")
                                                        )
                                                 ),
                                                 column(3),
                                               )
                                           )
                                    ),
                                    column(2,
                                           tags$div(style = "display: flex; align-items: center; justify-content: center; height: 10%;",
                                                    div(class = "circle-box",
                                                        shinyjs::hidden(tags$img(src = "KC.gif", id = "loading_gif_th", class = "circle-gif"))
                                                    )
                                           )
                                    ),
                                    column(4,
                                           div(class = "dataset-box",
                                               h4("Market Risk Dataset", style = "text-align: center;"),
                                               fluidRow(
                                                 column(3),
                                                 column(2,
                                                        div(class = "button-container",
                                                            p("Load"),
                                                            actionButton("loadMKTdata", icon("file-alt", style = "color: #BF9F66; font-size: 2.0em;"), class = "circle-button")
                                                        )
                                                 ),
                                                 column(2,
                                                        div(class = "button-container",
                                                            p("Download"),
                                                      static_download_button("Merged Datasets/EBA_Market.parquet", "Market-Risk-Dataset.parquet")
                                                        )
                                                 ),
                                                 column(2,
                                                        div(class = "button-container",
                                                            p("Reset"),
                                                            actionButton("resetMKTdata", icon("times", style = "color: #BF9F66; font-size: 2.5em;"), class = "circle-button")
                                                        )
                                                 ),
                                                 column(3),
                                               )
                                           )
                                    )
                           ),
                           fluidRow(class = "v-align-row",
                                    column(2),
                                    column(4,
                                           div(class = "dataset-box",
                                               h4("Risk Parameters Dataset", style = "text-align: center;"),
                                               fluidRow(
                                                 column(3),
                                                 column(2,
                                                        div(class = "button-container",
                                                            p("Load"),
                                                            actionButton("loadRPSdata", icon("file-alt", style = "color: #BF9F66; font-size: 2.0em;"), class = "circle-button")
                                                        )
                                                 ),
                                                 column(2,
                                                        div(class = "button-container",
                                                            p("Download"),
                                                      static_download_button("EBA_Risk_Parameters.parquet", "Risk-Parameters-Dataset.parquet")
                                                        )
                                                 ),
                                                 column(2,
                                                        div(class = "button-container",
                                                            p("Reset"),
                                                            actionButton("resetRPSdata", icon("times", style = "color: #BF9F66; font-size: 2.5em;"), class = "circle-button")
                                                        )
                                                 ),
                                                 column(3),
                                               )
                                           )
                                    ),
                                    column(4,
                                           div(class = "dataset-box",
                                               h4("Bank Distress Dataset", style = "text-align: center;"),
                                               fluidRow(
                                                 column(3),
                                                 column(2,
                                                        div(class = "button-container",
                                                            p("Load"),
                                                            actionButton("loadBDSdata", icon("file-alt", style = "color: #BF9F66; font-size: 2.0em;"), class = "circle-button")
                                                        )
                                                 ),
                                                 column(2,
                                                        div(class = "button-container",
                                                            p("Download"),
                                                      static_download_button("Banking Distress Database.xlsx", "Bank-Distress-Dataset.xlsx")
                                                        )
                                                 ),
                                                 column(2,
                                                        div(class = "button-container",
                                                            p("Reset"),
                                                            actionButton("resetBDSdata", icon("times", style = "color: #BF9F66; font-size: 2.5em;"), class = "circle-button")
                                                        )
                                                 ),
                                                 column(3),
                                               )
                                           ),
                                           column(2)
                                    ),
                           ),
                       ),
                       conditionalPanel(
                         condition = "output.thematic_data_loaded",
                         fluidRow(class = "equal-height-row",
                                  column(6,
                                         div(class = "dataset-box scroll-box",
                                             conditionalPanel(
                                               condition = "output.th_data_visible",
                                               uiOutput("th_itemtable")
                                             ),
                                         )
                                  ),
                                  column(6,
                                         div(id = "th_filters_wrapper",
                                             div(class = "dataset-box",
                                                 # Standard datasets (PLC, EXP, SOV, MKT)
                                                 conditionalPanel(
                                                   condition = "output.active_th_type != 'RPS' && output.active_th_type != 'BDS'",
                                                   fluidRow(
                                                     column(3,
                                                            selectizeInput("country_filter_th_standard", "Select Country",
                                                                           choices = NULL,
                                                                           multiple = TRUE)
                                                     ),
                                                     column(3,
                                                            selectizeInput("bank_filter_th_standard", "Select Bank",
                                                                           choices = NULL,
                                                                           multiple = TRUE)
                                                     ),
                                                     column(3,
                                                            selectizeInput("exercise_filter_th_standard", "Select Exercise",
                                                                           choices = NULL,
                                                                           multiple = TRUE)
                                                     ),
                                                     column(3,
                                                            selectizeInput("item_filter_th_standard", "Select Item",
                                                                           choices = NULL,
                                                                           multiple = TRUE)
                                                     )
                                                   )
                                                 ),
                                                 
                                                 # BDS-specific filters
                                                 conditionalPanel(
                                                   condition = "output.active_th_type == 'BDS'",
                                                   fluidRow(
                                                     column(6,
                                                            selectizeInput("country_filter_th_bds", "Select Country",
                                                                           choices = NULL,
                                                                           multiple = TRUE)
                                                     ),
                                                     column(6,
                                                            selectizeInput("bank_filter_th_bds", "Select Bank",
                                                                           choices = NULL,
                                                                           multiple = TRUE)
                                                     )
                                                   )
                                                 ),
                                                 
                                                 # RPS-specific filters
                                                 conditionalPanel(
                                                   condition = "output.active_th_type == 'RPS'",
                                                   fluidRow(
                                                     column(3,
                                                            selectizeInput("country_filter_th_rps", "Select Country",
                                                                           choices = NULL,
                                                                           multiple = TRUE)
                                                     ),
                                                     column(3,
                                                            selectizeInput("period_filter_th", "Select Period",
                                                                           choices = NULL,
                                                                           multiple = TRUE)
                                                     ),
                                                     column(3,
                                                            selectizeInput("exposure_filter_th", "Select Exposure",
                                                                           choices = NULL,
                                                                           multiple = TRUE)
                                                     ),
                                                     column(3,
                                                            selectizeInput("item_filter_th_rps", "Select Item",
                                                                           choices = NULL,
                                                                           multiple = TRUE)
                                                     )
                                                   )
                                                 ),
                                                 uiOutput("dynamic_th_filters"),
                                                 fluidRow(
                                                   column(2,
                                                          div(class = "button-container",
                                                              p("Go"),
                                                              actionButton("go_button_th", icon("play-circle", style = "color: #BF9F66; font-size: 2.5em;"), class = "circle-button")
                                                          )
                                                   ),
                                                   column(2,
                                                          div(class = "button-container",
                                                              p("Reset"),
                                                              actionButton("reset_dynamic_filters_th", icon("times", style = "color: #BF9F66; font-size: 2.5em;"), class = "circle-button")
                                                          )
                                                   ),
                                                   column(2,
                                                          div(class = "button-container",
                                                              p("Download"),
                                                              downloadButton("downloadFilteredData_th", "", class = "circle-button")
                                                          )
                                                   )
                                                 )
                                             ),
                                         )
                                  )
                         ),
                         fluidRow(
                           column(12,
                                  div(class = "dataset-box scroll-box sticky-table-container",
                                      DTOutput("TH_datatable")
                                  )
                           )
                         )
                       )
              ),
              
              
              tabPanel("Visualisation",
                       div(class = "full-width-box",
                           
                           tabsetPanel(id = "visualisation-tabs",
                                       
                                       # Time Series Tab
                                       tabPanel("Time Series Charts",
                                                fluidRow(
                                                  column(3,
                                                         div(class = "dataset-box",
                                                             style = "height: 600px;",
                                                             h4("Filters"),
                                                             selectInput("vis_ts_item", "Item", choices = NULL),
                                                             selectInput("vis_ts_country", "Country", choices = NULL),
                                                             selectInput("vis_ts_bank", "Bank", choices = c("All Banks" = "ALL"))
                                                         )
                                                  ),
                                                  column(9,
                                                         div(class = "dataset-box",
                                                             style = "height: 600px; position: relative;",
                                                             add_busy_spinner(spin = "fading-circle", color = "#BF9F66"),
                                                             plotlyOutput("vis_ts_chart", height = "580px")
                                                         )
                                                  )
                                                )
                                       ),
                                       
                                       # Density Tab
                                       tabPanel("Density Charts",
                                                fluidRow(
                                                  column(3,
                                                         div(class = "dataset-box",
                                                             style = "height: 600px;",
                                                             h4("Filters"),
                                                             selectInput("vis_dens_item", "Item", choices = NULL),
                                                             selectInput("vis_dens_period", "Relative Period",
                                                                         choices = c("Year 1" = "Year1", "Year 2" = "Year2", "Year 3" = "Year3"),
                                                                         selected = "Year3")
                                                         )
                                                  ),
                                                  column(9,
                                                         div(class = "dataset-box",
                                                             style = "height: 600px; position: relative;",
                                                             add_busy_spinner(spin = "fading-circle", color = "#BF9F66"),
                                                             plotlyOutput("vis_density_plot", height = "580px")
                                                         )
                                                  )
                                                )
                                       ),
                                       
                                       # Waterfall Tab
                                       tabPanel("Waterfall Charts",
                                                fluidRow(
                                                  column(3,
                                                         div(class = "dataset-box",
                                                             style = "height: 600px;",
                                                             h4("Filters"),
                                                             selectInput("vis_wf_transitional", "Capital Ratio",
                                                                         choices = c("Fully Loaded" = 0, "Transitional" = 1), selected = 0),
                                                             selectInput("vis_wf_exercise", "Exercise", choices = NULL),
                                                             selectInput("vis_wf_period", "Period", 
                                                                         choices = c("Year 1" = "Year1", "Year 2" = "Year2", "Year 3" = "Year3"),
                                                                         selected = "Year3"),
                                                             selectInput("vis_wf_scenario", "Scenario", 
                                                                         choices = c("Baseline", "Adverse"), selected = "Adverse"),
                                                             selectInput("vis_wf_country", "Country", choices = NULL),
                                                             selectInput("vis_wf_bank", "Bank", choices = c("All Banks" = "ALL"))
                                                         )
                                                  ),
                                                  column(9,
                                                         div(class = "dataset-box",
                                                             style = "height: 600px; position: relative;",
                                                             add_busy_spinner(spin = "fading-circle", color = "#BF9F66"),
                                                             plotlyOutput("vis_waterfall_chart", height = "580px")
                                                         )
                                                  )
                                                )
                                       ),
                                       
                                       # Network Tab
                                       tabPanel("Exposure Networks",
                                                fluidRow(
                                                  column(3,
                                                         div(class = "dataset-box",
                                                             style = "height: 600px;",
                                                             h4("Filters"),
                                                             selectInput("vis_net_exp_type", "Exposure Type",
                                                                         choices = c("Risk Exposures" = "risk", "Sovereign Exposures" = "sovereign"),
                                                                         selected = "risk"),
                                                             selectInput("vis_net_country", "Country", choices = c("All Countries" = "ALL")),
                                                             selectInput("vis_net_bank", "Bank", choices = c("All Banks (Aggregate)" = "ALL")),
                                                             selectInput("vis_net_period", "Period", choices = NULL),
                                                             conditionalPanel(
                                                               condition = "input.vis_net_exp_type == 'risk'",
                                                               selectInput("vis_net_portfolio", "Portfolio", 
                                                                           choices = c("Total", "IRB", "Standardised"), selected = "Total"),
                                                               selectInput("vis_net_exposure", "Exposure", choices = NULL)
                                                             ),
                                                             conditionalPanel(
                                                               condition = "input.vis_net_exp_type == 'sovereign'",
                                                               selectInput("vis_net_maturity", "Maturity",
                                                                           choices = c("[0-3M]" = 1, "[3M-1Y]" = 2, "[1Y-2Y]" = 3, 
                                                                                       "[2Y-3Y]" = 4, "[3Y-5Y]" = 5, "[5Y-10Y]" = 6, 
                                                                                       "[10Y+]" = 7, "Total" = 8), selected = 8)
                                                             ),
                                                             uiOutput("network_action_controls")
                                                         )
                                                  ),
                                                  column(9,
                                                         div(class = "dataset-box",
                                                             style = "height: 600px; position: relative; overflow: hidden;",
                                                             add_busy_spinner(spin = "fading-circle", color = "#BF9F66"),
                                                             visNetworkOutput("vis_network", height = "580px")
                                                         )
                                                  )
                                                )
                                       ),
                                       
                                       # NACE Tab
                                       tabPanel("NACE Exposures",
                                                fluidRow(
                                                  column(3,
                                                         div(class = "dataset-box",
                                                             style = "height: 600px;",
                                                             h4("Filters"),
                                                             selectInput("vis_nace_country", "Country", choices = c("All Countries" = "ALL")),
                                                             selectInput("vis_nace_bank", "Bank", choices = c("All Banks (Aggregate)" = "ALL")),
                                                             selectInput("vis_nace_period", "Period", choices = NULL)
                                                         )
                                                  ),
                                                  column(9,
                                                         div(class = "dataset-box",
                                                             style = "height: 600px; position: relative;",
                                                             add_busy_spinner(spin = "fading-circle", color = "#BF9F66"),
                                                             plotlyOutput("vis_treemap", height = "580px")
                                                         )
                                                  )
                                                )
                                       ),
                                       
                                       # Balance Sheet Tab
                                       tabPanel("Assets, Liabilities, REAs",
                                                fluidRow(
                                                  column(3,
                                                         div(class = "dataset-box",
                                                             style = "height: 600px;",
                                                             h4("Filters"),
                                                             selectInput("vis_bs_type", "Breakdown Type",
                                                                         choices = c("Assets" = "ITM_ASSETS", "Liabilities" = "ITM_LIABILITIES", 
                                                                                     "Risk Exposure Amounts" = "ITM_RWA"), selected = "ITM_RWA"),
                                                             selectInput("vis_bs_country", "Country", choices = c("All Countries" = "ALL")),
                                                             selectInput("vis_bs_bank", "Bank", choices = c("All Banks (Aggregate)" = "ALL"))
                                                         )
                                                  ),
                                                  column(9,
                                                         div(class = "dataset-box",
                                                             style = "height: 600px; position: relative;",
                                                             add_busy_spinner(spin = "fading-circle", color = "#BF9F66"),
                                                             plotlyOutput("vis_breakdown_chart", height = "580px")
                                                         )
                                                  )
                                                )
                                       ),
                                       
                                       # Get Code Tab
                                       tabPanel("Get Code",
                                                fluidRow(
                                                  column(12,
                                                         div(class = "dataset-box",
                                                             style = "height: 600px; display: flex; flex-direction: column; align-items: center; justify-content: center;",
                                                             h3("Download this script to replicate the charts in this tab.", style = "text-align: center; margin-bottom: 20px;"),
                                                             div(class = "button-container",
                                                                 downloadButton("downloadChartsScript", "", class = "circle-button")
                                                             )
                                                         )
                                                  )
                                                )
                                       )
                           )
                       )
              ),
              
              tabPanel("Metadata",
                       tabsetPanel(id = "metadata-tabs",
                                   tabPanel("Bank Sample",
                                            fluidRow(
                                              column(12,
                                                     div(class = "metadata-table-header",
                                                         p("Available banks across years and exercises."),
                                                         downloadButton("downloadMetaBanks", "", class = "circle-button")
                                                     ),
                                                     div(class = "dataset-box metadata-scroll-box sticky-table-container",
                                                         DTOutput("metabanks_table")
                                                     )
                                              )
                                            )
                                   ),
                                   tabPanel("Other Banks",
                                            fluidRow(
                                              column(12,
                                                     div(class = "metadata-table-header",
                                                         p("Banks incorporated in the aggregate 'Other Banks' category in the Transparency Exercise"),
                                                         downloadButton("downloadOtherBanks", "", class = "circle-button")
                                                     ),
                                                     div(class = "dataset-box metadata-scroll-box sticky-table-container",
                                                         DTOutput("other_banks_table")
                                                     )
                                              )
                                            )
                                   ),
                                   tabPanel("Item Matches",
                                            fluidRow(
                                              column(12,
                                                     div(class = "metadata-table-header",
                                                         p("Accounting items matches across the stress-testing and transparency frameworks."),
                                                         downloadButton("downloadFinalMatches", "", class = "circle-button")
                                                     ),
                                                     div(class = "dataset-box metadata-scroll-box sticky-table-container",
                                                         DTOutput("final_matches_table")
                                                     )
                                              )
                                            )
                                   ),
                                   tabPanel("Available Items",
                                            fluidRow(
                                              column(12,
                                                     div(class = "metadata-table-header",
                                                         p("Available items in the framework with their original EBA identifier, dataset, and their dimensions."),
                                                         downloadButton("downloadAllItems", "", class = "circle-button")
                                                     ),
                                                     div(class = "dataset-box metadata-scroll-box sticky-table-container",
                                                         DTOutput("all_items_table")
                                                     )
                                              )
                                            )
                                   ),
                                   tabPanel("Exposures",
                                            fluidRow(
                                              column(12,
                                                     div(class = "metadata-table-header",
                                                         p("Correspondance between stress-testing and transparency exposures categories."),
                                                         downloadButton("downloadCommonExposures", "", class = "circle-button")
                                                     ),
                                                     div(class = "dataset-box metadata-scroll-box sticky-table-container",
                                                         DTOutput("common_exposures_table")
                                                     )
                                              )
                                            )
                                   ),
                                   tabPanel("Footnotes",
                                            fluidRow(
                                              column(12,
                                                     div(class = "metadata-table-header",
                                                         p("Footnotes from the EBA 'Footnotes' clarification category in the stress-testing and transparency exercises."),
                                                         downloadButton("downloadFootnotes", "", class = "circle-button")
                                                     ),
                                                     div(class = "dataset-box metadata-scroll-box sticky-table-container",
                                                         DTOutput("footnotes_table")
                                                     )
                                              )
                                            )
                                   ),
                                   tabPanel("Dictionaries",
                                            br(),
                                            tabsetPanel(id = "dict-subtabs",
                                                        tabPanel("Stress-Test Dictionary",
                                                                 fluidRow(
                                                                   column(12,
                                                                          div(class = "metadata-table-header",
                                                                              p("Dictionary of variables and values present in the stress-testing framework."),                                                                              
                                                                              downloadButton("downloadSTDict", "", class = "circle-button")
                                                                          ),
                                                                          div(class = "dataset-box dict-scroll-box",
                                                                              DTOutput("st_dict_table")
                                                                          )
                                                                   )
                                                                 )
                                                        ),
                                                        tabPanel("Transparency Dictionary",
                                                                 fluidRow(
                                                                   column(12,
                                                                          div(class = "metadata-table-header",
                                                                              p("Dictionary of variables and values present in the Transparency framework."), 
                                                                              downloadButton("downloadTRDict", "", class = "circle-button")
                                                                          ),
                                                                          div(class = "dataset-box dict-scroll-box sticky-table-container",
                                                                              DTOutput("tr_dict_table")
                                                                          )
                                                                   )
                                                                 )
                                                        ),
                                                        tabPanel("SSM Dictionary",
                                                                 fluidRow(
                                                                   column(12,
                                                                          div(class = "metadata-table-header",
                                                                              p("Dictionary of variables and values present in the SSM framework."),                                                                              
                                                                              downloadButton("downloadSSMDict", "", class = "circle-button")
                                                                          ),
                                                                          div(class = "dataset-box dict-scroll-box sticky-table-container",
                                                                              DTOutput("ssm_dict_table")
                                                                          )
                                                                   )
                                                                 )
                                                        )
                                            )
                                   ),
                                   
                       ),
                       fluidRow(
                         column(3,
                                div(class = "metadata-download-section",
                                    downloadButton("downloadTotalMetadata", "", class = "circle-button"),
                                    p("Download Total Metadata", style = "font-size: 16px; font-weight: normal;")
                                )
                         )
                       ),
              )
  )     
)




server <- function(input, output, session) {
  
  mappings <- list(
    "Scenario" = scenario_st,
    "Country" = final_country,
    "Portfolio" = final_portfolio,
    "Maturity" = final_maturity,
    "Exposure" = final_exposure,
    "Status" = final_status,
    "Perf_Status" = final_perf_status,
    "Perf_Forborne" = final_forborne,
    "IFRS9_Stages" = ifrs_st,
    "Financial_Instruments" = fin_instr_tr,
    "MKT_Risk" = mkt_risk_tr,
    "MKT_ModProd" = mkt_modprod_tr,
    "Assets_FV" = asset_fv_tr,
    "Assets_Stages" = asset_stages_tr,
    "Accounting_Portfolio" = acc_portfolio_tr,
    "NACE_Codes" = nace_tr,
    "Common_Exposure" = common_exposure_final
  )
  
  
  # Helper function to sort Common_Item
  sort_common_items <- function(items) {
    if (length(items) == 0) return(character(0))
    # Extract numeric part for sorting
    numeric_parts <- as.numeric(stringr::str_extract(items, "\\d+"))
    items[order(numeric_parts)]
  }

  has_all_selection <- function(selected_values, all_label) {
    is.null(selected_values) || length(selected_values) == 0 || all_label %in% selected_values
  }

  has_filter_selection <- function(selected_values, all_label) {
    !has_all_selection(selected_values, all_label)
  }

  has_dynamic_filter_selection <- function(filter_names, input_values) {
    any(vapply(filter_names, function(col_name) {
      selected_values <- input_values[[paste0("dynamic_", col_name)]]
      !is.null(selected_values) && length(selected_values) > 0 && !("All" %in% selected_values)
    }, logical(1)))
  }

  make_cast_in_clause <- function(column_name, selected_values) {
    if (is.null(selected_values) || length(selected_values) == 0) {
      return(character(0))
    }

    paste0("CAST(", column_name, " AS VARCHAR) IN (", quote_sql_values(selected_values), ")")
  }

  make_total_clause <- function(column_name) {
    paste0(
      "(",
      column_name,
      " IS NULL OR lower(trim(CAST(",
      column_name,
      " AS VARCHAR))) IN ('0', 'total', 'no breakdown', 'total / no breakdown')",
      ")"
    )
  }

  make_dynamic_filter_clauses <- function(filter_names, input_values) {
    clauses <- character()

    for (col_name in filter_names) {
      input_val <- input_values[[paste0("dynamic_", col_name)]]
      if (is.null(input_val) || "All" %in% input_val) {
        next
      }

      if (col_name == "Scenario") {
        scenario_mapping <- mappings[["Scenario"]]
        selected_scenarios <- as.character(setdiff(input_val, "0"))
        always_include <- as.character(
          scenario_mapping[names(scenario_mapping) %in% c("Actual", "Actual Restated", "Actual restated")]
        )
        all_codes <- unique(c(selected_scenarios, always_include))

        adverse_code <- as.character(scenario_mapping[names(scenario_mapping) == "Adverse"])
        if (length(adverse_code) == 1 && adverse_code %in% selected_scenarios) {
          adverse_shock_code <- as.character(scenario_mapping[names(scenario_mapping) == "Adverse Sovereign Shock"])
          all_codes <- unique(c(all_codes, adverse_shock_code))
        }

        scenario_clause <- make_cast_in_clause(col_name, all_codes)
        if ("0" %in% input_val) {
          clauses <- c(clauses, paste0("(", scenario_clause, " OR ", make_total_clause(col_name), ")"))
        } else {
          clauses <- c(clauses, scenario_clause)
        }

        next
      }

      selected_values <- as.character(setdiff(input_val, "0"))
      value_clause <- if (length(selected_values) > 0) make_cast_in_clause(col_name, selected_values) else character(0)

      if ("0" %in% input_val) {
        total_clause <- make_total_clause(col_name)
        if (length(value_clause) > 0) {
          clauses <- c(clauses, paste0("(", value_clause, " OR ", total_clause, ")"))
        } else {
          clauses <- c(clauses, total_clause)
        }
      } else if (length(value_clause) > 0) {
        clauses <- c(clauses, value_clause)
      }
    }

    clauses
  }

  build_standard_where_clauses <- function(country_input, bank_input, exercise_input, item_input, filter_names, input_values) {
    where_clauses <- character()

    if (!has_all_selection(country_input, "All Countries")) {
      where_clauses <- c(where_clauses, make_cast_in_clause("ISO2", country_input))
    }
    if (!has_all_selection(bank_input, "All Banks")) {
      where_clauses <- c(where_clauses, make_cast_in_clause("Bank_ID", bank_input))
    }
    if (!has_all_selection(exercise_input, "All Exercises")) {
      where_clauses <- c(where_clauses, make_cast_in_clause("Exercise", exercise_input))
    }
    if (!has_all_selection(item_input, "All Items")) {
      where_clauses <- c(where_clauses, make_cast_in_clause("Common_Item", item_input))
    }

    c(where_clauses, make_dynamic_filter_clauses(filter_names, input_values))
  }

  build_rps_where_clauses <- function(country_input, period_input, exposure_input, item_input) {
    where_clauses <- character()

    if (!has_all_selection(country_input, "All Countries")) {
      where_clauses <- c(where_clauses, make_cast_in_clause("Country_Label", country_input))
    }

    if (!has_all_selection(period_input, "All Periods")) {
      selected_periods <- vapply(period_input, function(formatted_date) {
        format(zoo::as.yearmon(formatted_date, format = "%B %Y"), "%Y%m")
      }, character(1))
      where_clauses <- c(where_clauses, make_cast_in_clause("Period", selected_periods))
    }

    if (!has_all_selection(exposure_input, "All Exposures")) {
      where_clauses <- c(where_clauses, make_cast_in_clause("Exposure_Label", exposure_input))
    }

    if (!has_all_selection(item_input, "All Items")) {
      where_clauses <- c(where_clauses, make_cast_in_clause("Common_Item", item_input))
    }

    where_clauses
  }

  fetch_preview_result <- function(view_name, where_clauses = character()) {
    total_rows <- count_dataset_rows(view_name, where_clauses)
    rows_to_fetch <- if (total_rows > 0) min(preview_row_limit, total_rows) else 0L

    list(
      data = fetch_dataset_rows(view_name, where_clauses, rows_to_fetch),
      total_rows = total_rows,
      limited = total_rows > preview_row_limit
    )
  }

  show_preview_notice <- function(notification_id, total_rows) {
    showNotification(
      paste0(
        "Showing the first ",
        format(preview_row_limit, big.mark = ","),
        " rows out of ",
        format(total_rows, big.mark = ","),
        ". Use the filtered download to export the full result."
      ),
      type = "message",
      duration = 8,
      id = notification_id
    )
  }

  export_query_result <- function(view_name, where_clauses, file_path, row_count) {
    export_format <- if (row_count >= 500000) "parquet" else "csv"
    copy_dataset_export(view_name, file_path, where_clauses, export_format)
    export_format
  }

  get_export_extension <- function(view_name, where_clauses) {
    if (count_dataset_rows(view_name, where_clauses) >= 500000) ".parquet" else ".csv"
  }

  sql_values_to_r_vector <- function(value_string) {
    if (!nzchar(value_string)) {
      return(character(0))
    }

    matches <- gregexpr("'(?:''|[^'])*'", value_string, perl = TRUE)
    values <- regmatches(value_string, matches)[[1]]

    if (length(values) == 1 && identical(values[[1]], "")) {
      return(character(0))
    }

    values <- substring(values, 2, nchar(values) - 1)
    gsub("''", "'", values, fixed = TRUE)
  }

  make_r_vector_literal <- function(values) {
    if (length(values) == 0) {
      return("character(0)")
    }

    sprintf("c(%s)", paste(vapply(values, function(value) deparse(value), character(1)), collapse = ", "))
  }

  sql_clause_to_dplyr_filter <- function(clause) {
    trimmed_clause <- trimws(clause)

    if (startsWith(trimmed_clause, "(") && endsWith(trimmed_clause, ")") && grepl(" OR ", trimmed_clause, fixed = TRUE)) {
      inner_clause <- substring(trimmed_clause, 2, nchar(trimmed_clause) - 1)
      or_parts <- strsplit(inner_clause, " OR ", fixed = TRUE)[[1]]

      if (length(or_parts) == 2) {
        left_expr <- sql_clause_to_dplyr_filter(or_parts[[1]])
        right_expr <- sql_clause_to_dplyr_filter(or_parts[[2]])
        return(sprintf("(%s) | (%s)", left_expr, right_expr))
      }
    }

    in_match <- regexec("^CAST\\((.+?) AS VARCHAR\\) IN \\((.*)\\)$", trimmed_clause, perl = TRUE)
    in_parts <- regmatches(trimmed_clause, in_match)[[1]]
    if (length(in_parts) == 3) {
      column_name <- in_parts[[2]]
      selected_values <- sql_values_to_r_vector(in_parts[[3]])
      return(sprintf("as.character(%s) %%in%% %s", column_name, make_r_vector_literal(selected_values)))
    }

    total_match <- regexec(
      "^\\((.+?) IS NULL OR lower\\(trim\\(CAST\\(.+? AS VARCHAR\\)\\)\\) IN \\((.*)\\)\\)$",
      trimmed_clause,
      perl = TRUE
    )
    total_parts <- regmatches(trimmed_clause, total_match)[[1]]
    if (length(total_parts) == 3) {
      column_name <- total_parts[[2]]
      total_values <- tolower(sql_values_to_r_vector(total_parts[[3]]))
      return(sprintf(
        "is.na(%s) | tolower(trimws(as.character(%s))) %%in%% %s",
        column_name,
        column_name,
        make_r_vector_literal(total_values)
      ))
    }

    stop(sprintf("Unsupported filter clause for reproduction script: %s", clause))
  }

  write_filtered_reproduction_script <- function(script_path, dataset_label, dataset_source_path, where_clauses, output_filename) {
    dplyr_filters <- vapply(where_clauses, sql_clause_to_dplyr_filter, character(1))

    code_content <- c(
      paste0("# Reproduce filtered ", dataset_label, " extract locally"),
      "# ---------------------------------------------",
      "# Place this script in the same folder as the full original parquet dataset before running it.",
      "",
      "if (!requireNamespace('arrow', quietly = TRUE)) install.packages('arrow')",
      "if (!requireNamespace('dplyr', quietly = TRUE)) install.packages('dplyr')",
      "if (!requireNamespace('readr', quietly = TRUE)) install.packages('readr')",
      "",
      "library(arrow)",
      "library(dplyr)",
      "library(readr)",
      "",
      paste0("path_to_full_dataset <- ", deparse(basename(dataset_source_path))),
      paste0("output_filename <- ", deparse(output_filename)),
      "",
      "full_data <- arrow::read_parquet(path_to_full_dataset)",
      "filtered_data <- full_data",
      "",
      if (length(dplyr_filters) > 0) {
        vapply(dplyr_filters, function(filter_expr) {
          paste0("filtered_data <- filtered_data %>% filter(", filter_expr, ")")
        }, character(1))
      } else {
        character(0)
      },
      "",
      "print(sprintf('Rows returned: %s', nrow(filtered_data)))",
      "",
      "if (grepl('\\.parquet$', output_filename, ignore.case = TRUE)) {",
      "  arrow::write_parquet(filtered_data, output_filename, compression = 'snappy')",
      "} else {",
      "  readr::write_csv(filtered_data, output_filename)",
      "}",
      "",
      "print(sprintf('Saved filtered extract to %s', output_filename))"
    )

    writeLines(code_content, script_path)
  }

  create_filtered_download_bundle <- function(zip_file, dataset_label, dataset_source_path, dataset_export_basename, view_name, where_clauses) {
    temp_dir <- tempfile(pattern = "filtered-download-")
    dir.create(temp_dir, recursive = TRUE, showWarnings = FALSE)
    on.exit(unlink(temp_dir, recursive = TRUE, force = TRUE), add = TRUE)

    export_extension <- get_export_extension(view_name, where_clauses)
    export_filename <- paste0(dataset_export_basename, export_extension)
    export_file_path <- file.path(temp_dir, export_filename)
    row_count <- count_dataset_rows(view_name, where_clauses)

    export_query_result(view_name, where_clauses, export_file_path, row_count)

    script_filename <- paste0("reproduce_", dataset_export_basename, ".R")
    script_file_path <- file.path(temp_dir, script_filename)
    write_filtered_reproduction_script(
      script_path = script_file_path,
      dataset_label = dataset_label,
      dataset_source_path = dataset_source_path,
      where_clauses = where_clauses,
      output_filename = export_filename
    )

    zip::zip(zipfile = zip_file, files = c(export_filename, script_filename), root = temp_dir)
  }

  write_filtered_bds_reproduction_script <- function(script_path, selected_countries, selected_banks, output_filename) {
    code_content <- c(
      "# Reproduce filtered bank distress extract locally",
      "# ----------------------------------------------",
      "",
      "if (!requireNamespace('readxl', quietly = TRUE)) install.packages('readxl')",
      "if (!requireNamespace('dplyr', quietly = TRUE)) install.packages('dplyr')",
      "if (!requireNamespace('stringr', quietly = TRUE)) install.packages('stringr')",
      "",
      "library(readxl)",
      "library(dplyr)",
      "library(stringr)",
      "",
      paste0("path_to_full_dataset <- ", deparse("Meta/Original Data/Banking Distress Database.xlsx")),
      paste0("output_filename <- ", deparse(output_filename)),
      paste0("selected_countries <- ", deparse(as.character(selected_countries))),
      paste0("selected_banks <- ", deparse(as.character(selected_banks))),
      "",
      "filtered_data <- readxl::read_xlsx(path_to_full_dataset, sheet = 'Main Table', skip = 3) %>%",
      "  rename_with(~ stringr::str_to_title(stringr::str_replace_all(., '_', ' '))) %>%",
      "  rename(ISO2 = Iso2, RIC = Ric, LEI = Lei)",
      "",
      "if (length(selected_countries) > 0) {",
      "  filtered_data <- filtered_data %>% filter(ISO2 %in% selected_countries)",
      "}",
      "if (length(selected_banks) > 0) {",
      "  filtered_data <- filtered_data %>% filter(Name %in% selected_banks)",
      "}",
      "",
      "write.csv(filtered_data, output_filename, row.names = FALSE)",
      "print(head(filtered_data))",
      "print(sprintf('Rows returned: %s', nrow(filtered_data)))"
    )

    writeLines(code_content, script_path)
  }

  create_filtered_bds_download_bundle <- function(zip_file, selected_countries, selected_banks) {
    temp_dir <- tempfile(pattern = "filtered-bds-download-")
    dir.create(temp_dir, recursive = TRUE, showWarnings = FALSE)
    on.exit(unlink(temp_dir, recursive = TRUE, force = TRUE), add = TRUE)

    export_filename <- "filtered_bank_distress_data.csv"
    export_file_path <- file.path(temp_dir, export_filename)
    script_filename <- "reproduce_filtered_bank_distress_data.R"
    script_file_path <- file.path(temp_dir, script_filename)

    filtered_data <- distress_event()
    if (length(selected_countries) > 0) {
      filtered_data <- filtered_data %>% filter(ISO2 %in% selected_countries)
    }
    if (length(selected_banks) > 0) {
      filtered_data <- filtered_data %>% filter(Name %in% selected_banks)
    }

    write.csv(filtered_data, export_file_path, row.names = FALSE)
    write_filtered_bds_reproduction_script(script_file_path, selected_countries, selected_banks, export_filename)

    zip::zip(zipfile = zip_file, files = c(export_filename, script_filename), root = temp_dir)
  }

  filtered_st_download_ready <- reactive({
    has_filter_selection(input$country_filter_st, "All Countries") ||
      has_filter_selection(input$bank_filter_st, "All Banks") ||
      has_filter_selection(input$exercise_filter_st, "All Exercises") ||
      has_filter_selection(input$item_filter_st, "All Items") ||
      has_dynamic_filter_selection(dynamic_filters_st(), input)
  })

  filtered_tr_download_ready <- reactive({
    has_filter_selection(input$country_filter_tr, "All Countries") ||
      has_filter_selection(input$bank_filter_tr, "All Banks") ||
      has_filter_selection(input$exercise_filter_tr, "All Exercises") ||
      has_filter_selection(input$item_filter_tr, "All Items") ||
      has_dynamic_filter_selection(dynamic_filters_tr(), input)
  })

  filtered_th_download_ready <- reactive({
    dataset_type <- active_th_dataset_type()

    if (is.null(dataset_type) || identical(dataset_type, "BDS") && !thematic_data_loaded()) {
      return(FALSE)
    }

    if (identical(dataset_type, "RPS")) {
      return(
        has_filter_selection(input$country_filter_th_rps, "All Countries") ||
          has_filter_selection(input$period_filter_th, "All Periods") ||
          has_filter_selection(input$exposure_filter_th, "All Exposures") ||
          has_filter_selection(input$item_filter_th_rps, "All Items")
      )
    }

    if (identical(dataset_type, "BDS")) {
      return(
        has_filter_selection(input$country_filter_th_bds, "All Countries") ||
          has_filter_selection(input$bank_filter_th_bds, "All Banks")
      )
    }

    has_filter_selection(input$country_filter_th_standard, "All Countries") ||
      has_filter_selection(input$bank_filter_th_standard, "All Banks") ||
      has_filter_selection(input$exercise_filter_th_standard, "All Exercises") ||
      has_filter_selection(input$item_filter_th_standard, "All Items") ||
      has_dynamic_filter_selection(dynamic_filters_th(), input)
  })

  load_chart_dataset <- function(chart_key, columns = "*") {
    chart_spec <- get_chart_view_spec(chart_key)
    query <- if (length(columns) == 1 && identical(columns, "*")) {
      paste("SELECT * FROM", chart_spec$view)
    } else {
      paste("SELECT", paste(columns, collapse = ", "), "FROM", chart_spec$view)
    }

    dbGetQuery(con, query)
  }

  load_chart_dataset_where <- function(chart_key, columns = "*", where_clauses = character()) {
    chart_spec <- get_chart_view_spec(chart_key)

    query <- if (length(columns) == 1 && identical(columns, "*")) {
      build_select_query(chart_spec$view, where_clauses)
    } else {
      paste(
        "SELECT", paste(columns, collapse = ", "), "FROM", chart_spec$view,
        if (length(where_clauses) > 0) paste("WHERE", paste(where_clauses, collapse = " AND ")) else ""
      )
    }

    dbGetQuery(con, query)
  }

  load_enabled_chart_dataset <- function(chart_key, columns = "*") {
    req(chart_data_loaded())
    req(chart_dataset_enabled(chart_key))

    load_chart_dataset(chart_key, columns) %>%
      dplyr::select(-any_of("DB")) %>%
      select_if(function(x) !(all(is.na(x)) | all(x == "")))
  }

  load_enabled_chart_dataset_where <- function(chart_key, columns = "*", where_clauses = character()) {
    req(chart_data_loaded())
    req(chart_dataset_enabled(chart_key))

    load_chart_dataset_where(chart_key, columns, where_clauses) %>%
      dplyr::select(-any_of("DB")) %>%
      select_if(function(x) !(all(is.na(x)) | all(x == "")))
  }

  load_chart_distinct_values <- function(chart_key, column, where_clauses = character(), descending = FALSE) {
    req(chart_data_loaded())
    req(chart_dataset_enabled(chart_key))

    chart_spec <- get_chart_view_spec(chart_key)
    query <- paste(
      "SELECT DISTINCT", column, "FROM", chart_spec$view,
      if (length(where_clauses) > 0) paste("WHERE", paste(where_clauses, collapse = " AND ")) else "",
      "ORDER BY", column, if (descending) "DESC" else "ASC"
    )

    values <- dbGetQuery(con, query)[[column]]
    values <- values[!is.na(values)]

    if (is.character(values)) {
      values <- values[values != ""]
    }

    values
  }

  build_network_where_clauses <- function(country = NULL, bank = NULL, exposure = NULL,
                                          maturity = NULL, period = NULL, portfolio = NULL) {
    where_clauses <- character()

    if (!is.null(country) && !identical(country, "ALL")) {
      where_clauses <- c(where_clauses, sprintf("ISO2 = '%s'", escape_sql_string(country)))
    }

    if (!is.null(bank) && !identical(bank, "ALL")) {
      where_clauses <- c(where_clauses, sprintf("Name = '%s'", escape_sql_string(bank)))
    }

    if (!is.null(exposure) && nzchar(exposure)) {
      where_clauses <- c(where_clauses, sprintf("Common_Exposure = '%s'", escape_sql_string(exposure)))
    }

    if (!is.null(maturity) && nzchar(as.character(maturity))) {
      where_clauses <- c(where_clauses, sprintf("Maturity = %s", as.integer(maturity)))
    }

    if (!is.null(period) && nzchar(as.character(period))) {
      where_clauses <- c(where_clauses, sprintf("Period = %s", as.numeric(period)))
    }

    if (!is.null(portfolio) && identical(portfolio, "IRB")) {
      where_clauses <- c(where_clauses, "Portfolio = 2")
    } else if (!is.null(portfolio) && identical(portfolio, "Standardised")) {
      where_clauses <- c(where_clauses, "Portfolio = 1")
    }

    where_clauses
  }

  load_standard_thematic_dataset <- function(type, country_choices, bank_choices) {
    reset_all_panels()
    shinyjs::reset("th_filters_wrapper")
    output$dynamic_th_filters <- renderUI({ NULL })
    shinyjs::show("loading_gif_th")
    on.exit(shinyjs::hide("loading_gif_th"), add = TRUE)

    preview_result <- fetch_preview_result(get_dataset_source(type)$view)
    th_data(preview_result$data)
    display_th_data(preview_result$data)

    thematic_data_visible(TRUE)
    thematic_data_loaded(TRUE)
    active_th_dataset_type(type)

    if (preview_result$limited) {
      show_preview_notice(paste0(tolower(type), "-preview-notice"), preview_result$total_rows)
    }

    updateSelectizeInput(session, "country_filter_th_standard",
                         choices = c("All Countries" = "All Countries", country_choices),
                         selected = NULL,
                         server = TRUE)

    updateSelectizeInput(session, "bank_filter_th_standard",
                         choices = c("All Banks" = "All Banks", bank_choices),
                         selected = NULL,
                         server = TRUE)

    available_exercises <- bk_struct %>%
      filter(DB == type) %>%
      dplyr::select(Exercise) %>%
      distinct()

    updateSelectizeInput(session, "exercise_filter_th_standard",
                         choices = c("All Exercises", sort(as.character(available_exercises$Exercise))),
                         selected = NULL,
                         server = TRUE)

    available_items <- bk_struct %>%
      filter(DB == type, Exercise %in% available_exercises$Exercise) %>%
      dplyr::select(Common_Item) %>%
      distinct()

    available_items_th(available_items$Common_Item)
    updateSelectizeInput(session, "item_filter_th_standard",
                         choices = c("All Items", sort_common_items(available_items$Common_Item)),
                         selected = NULL,
                         server = TRUE)
  }

  register_reset_button <- function(input_id, loading_gif_id) {
    observeEvent(input[[input_id]], {
      shinyjs::show(loading_gif_id)
      reset_all_panels()
      shinyjs::hide(loading_gif_id)
    })
  }
  
  st_data_visible <- reactiveVal(FALSE)
  tr_data_visible = reactiveVal(FALSE)
  
  st_data_loaded <- reactiveVal(FALSE)
  tr_data_loaded <- reactiveVal(FALSE)
  
  st_data <- reactiveVal()
  tr_data <- reactiveVal()
  th_data <- reactiveVal()
  display_st_data <- reactiveVal()
  display_tr_data <- reactiveVal()
  display_th_data <- reactiveVal()
  
  active_st_dataset_type <- reactiveVal(NULL) 
  active_th_dataset_type <- reactiveVal(NULL)
  dynamic_filters_st <- reactiveVal(list())
  dynamic_filters_tr <- reactiveVal(list())
  dynamic_filters_th <- reactiveVal(list())
  available_items_st <- reactiveVal()
  available_items_tr <- reactiveVal()
  available_items_th <- reactiveVal()
  
  thematic_data_loaded <- reactiveVal(FALSE)
  thematic_data_visible <- reactiveVal(FALSE)
  
  chart_data_loaded <- reactiveVal(FALSE)
  visualisation_dataset_ready <- reactiveValues(
    tr_ratios = FALSE,
    final_waterfall = FALSE,
    bank_exp_total = FALSE,
    sov_exp = FALSE,
    bank_nace = FALSE,
    tr_rwas = FALSE,
    tr_assets = FALSE
  )

  chart_dataset_enabled <- function(chart_key) {
    isTRUE(visualisation_dataset_ready[[chart_key]])
  }

  observeEvent(input$`visualisation-tabs`, {
    current_tab <- input$`visualisation-tabs`

    if (identical(current_tab, "Time Series Charts") || identical(current_tab, "Density Charts")) {
      visualisation_dataset_ready$tr_ratios <- TRUE
    }
    if (identical(current_tab, "Waterfall Charts")) {
      visualisation_dataset_ready$final_waterfall <- TRUE
    }
    if (identical(current_tab, "Exposure Networks")) {
      visualisation_dataset_ready$bank_exp_total <- TRUE
      visualisation_dataset_ready$sov_exp <- TRUE
    }
    if (identical(current_tab, "NACE Exposures")) {
      visualisation_dataset_ready$bank_nace <- TRUE
    }
    if (identical(current_tab, "Assets, Liabilities, REAs")) {
      visualisation_dataset_ready$tr_rwas <- TRUE
      visualisation_dataset_ready$tr_assets <- TRUE
    }
  }, ignoreNULL = FALSE)
  
  
  output$st_data_loaded <- reactive(st_data_loaded())
  outputOptions(output, "st_data_loaded", suspendWhenHidden = FALSE)
  
  output$tr_data_loaded <- reactive(tr_data_loaded())
  outputOptions(output, "tr_data_loaded", suspendWhenHidden = FALSE)
  
  output$st_data_visible <- reactive(st_data_visible())
  outputOptions(output, "st_data_visible", suspendWhenHidden = FALSE)
  
  output$tr_data_visible <- reactive(tr_data_visible())
  outputOptions(output, "tr_data_visible", suspendWhenHidden = FALSE)
  
  output$thematic_data_loaded <- reactive(thematic_data_loaded())
  outputOptions(output, "thematic_data_loaded", suspendWhenHidden = FALSE)
  
  output$th_data_visible <- reactive(thematic_data_visible())
  outputOptions(output, "th_data_visible", suspendWhenHidden = FALSE)
  
  output$active_th_type <- reactive({
    active_th_dataset_type()
  })
  outputOptions(output, "active_th_type", suspendWhenHidden = FALSE)

  observe({
    shinyjs::toggleState("downloadFilteredData_st", condition = filtered_st_download_ready())
  })

  observe({
    shinyjs::toggleState("downloadFilteredData_tr", condition = filtered_tr_download_ready())
  })

  observe({
    shinyjs::toggleState("downloadFilteredData_th", condition = filtered_th_download_ready())
  })
  
  output$dynamic_st_itemtable <- renderUI({
    req(st_data_visible()) # Ensure data is visible
    if (active_st_dataset_type() == "ST") {
      DTOutput("itm_table_st")
    } else if (active_st_dataset_type() == "SSM") {
      DTOutput("itm_table_ssm")
    } else {
      NULL 
    }
  })
  
  output$itm_table_st <- renderDT({
    datatable(itm_table_st, 
              class = 'custom-datatable',
              options = list(
                pageLength = 200, 
                scrollX = TRUE
              ),
              rownames = FALSE)
  })
  
  output$itm_table_ssm <- renderDT({
    datatable(itm_table_ssm, 
              class = 'custom-datatable',
              options = list(
                pageLength = 200, 
                scrollX = TRUE
              ),
              rownames = FALSE)
  })
  
  # Update the TR item table
  output$tr_itemtable <- renderDT({
    datatable(itm_table_tr, 
              class = 'custom-datatable',
              options = list(
                pageLength = 200, 
                scrollX = TRUE
              ),
              rownames = FALSE)
  })
  
  output$ST_datatable <- renderDT({
    req(display_st_data())
    datatable(display_st_data(),
              class = 'custom-datatable',
              options = list(
                pageLength = 200, 
                scrollX = TRUE
              ),
              rownames = FALSE)
  })
  
  output$TR_datatable <- renderDT({
    req(display_tr_data())
    datatable(display_tr_data(),
              class = 'custom-datatable',
              options = list(
                pageLength = 200, 
                scrollX = TRUE
              ),
              rownames = FALSE)
  })
  
  output$TH_datatable <- renderDT({
    req(display_th_data())
    datatable(display_th_data(),
              class = 'custom-datatable',
              options = list(
                pageLength = 200, 
                scrollX = TRUE
              ),
              rownames = FALSE)
  })
  
  output$th_itemtable <- renderUI({
    req(active_th_dataset_type())
    
    # Special case for BDS - show markdown instead of table
    if (active_th_dataset_type() == "BDS") {
      return(div(class = "markdown-content", br(), includeMarkdown(includeMarkdownSection("The Bank Distress Event Dataset"))))
    }
    
    # For other datasets, show the appropriate table
    DTOutput("th_itemtable_dt")
  })
  
  
  output$th_itemtable_dt <- renderDT({
    req(active_th_dataset_type())
    req(active_th_dataset_type() != "BDS")
    
    tbl <- switch(active_th_dataset_type(), 
                  "SOV" = itm_table_sov,
                  "EXP" = itm_table_exp,
                  "PLC" = itm_table_plc, 
                  "MKT" = itm_table_mkt,
                  "RPS" = itm_table_total %>% filter(DB == "RPS"),
                  NULL
    )
    
    if (!is.null(tbl)) {
      datatable(tbl,
                class = 'custom-datatable',
                options = list(
                  pageLength = 200, 
                  scrollX = TRUE
                ),
                rownames = FALSE)
    }
  })
  
  ## Load data
  observeEvent(input$loadSTdata, {
    reset_all_panels() # Call to reset all panels
    shinyjs::reset("st_filters_wrapper") # Reset filters for Stress Test tab
    output$dynamic_st_filters <- renderUI({ NULL })
    shinyjs::show("loading_gif_st")
    
    preview_result <- fetch_preview_result(get_dataset_source("ST")$view)
    st_data(preview_result$data)
    display_st_data(preview_result$data)
    
    shinyjs::hide("loading_gif_st")
    st_data_visible(TRUE)
    st_data_loaded(TRUE)
    active_st_dataset_type("ST") # Set dataset type

    if (preview_result$limited) {
      show_preview_notice("st-preview-notice", preview_result$total_rows)
    }
    
    # Update filters with pre-defined objects
    updateSelectizeInput(session, "country_filter_st",
                         choices = c("All Countries" = "All Countries", countries_st),
                         selected = NULL,
                         server = TRUE)
    
    updateSelectizeInput(session, "bank_filter_st",
                         choices = c("All Banks" = "All Banks", initial_bank_st),
                         selected = NULL,
                         server = TRUE)
    
    # Update exercise and item filters from bk_struct
    available_exercises <- bk_struct %>% filter(DB == "ST") %>% 
      dplyr::select(Exercise) %>% 
      distinct()
    updateSelectizeInput(session, "exercise_filter_st",
                         choices = c("All Exercises", sort(as.character(available_exercises$Exercise))),
                         selected = NULL,
                         server = TRUE)
    
    available_items <- bk_struct %>% 
      filter(DB == "ST" & Exercise %in% available_exercises) %>% 
      dplyr::select(Common_Item) %>% 
      distinct()
    available_items_st(available_items$Common_Item)
    updateSelectizeInput(session, "item_filter_st",
                         choices = c("All Items", sort_common_items(available_items$Common_Item)),
                         selected = NULL,
                         server = TRUE)
  })
  
  observeEvent(input$loadSSMdata, {
    reset_all_panels() # Call to reset all panels
    shinyjs::reset("st_filters_wrapper") # Reset filters for Stress Test tab
    output$dynamic_st_filters <- renderUI({ NULL })
    shinyjs::show("loading_gif_st")
    
    preview_result <- fetch_preview_result(get_dataset_source("SSM")$view)
    st_data(preview_result$data)
    display_st_data(preview_result$data)
    
    shinyjs::hide("loading_gif_st")
    st_data_visible(TRUE)
    st_data_loaded(TRUE)
    active_st_dataset_type("SSM") # Set dataset type

    if (preview_result$limited) {
      show_preview_notice("ssm-preview-notice", preview_result$total_rows)
    }
    
    # Update filters with pre-defined objects
    updateSelectizeInput(session, "country_filter_st",
                         choices = c("All Countries" = "All Countries", countries_ssm),
                         selected = NULL,
                         server = TRUE)
    
    updateSelectizeInput(session, "bank_filter_st",
                         choices = c("All Banks" = "All Banks", initial_bank_ssm),
                         selected = NULL,
                         server = TRUE)
    
    # Update exercise and item filters from bk_struct
    available_exercises <- bk_struct %>% filter(DB == "SSM") %>% 
      dplyr::select(Exercise) %>% 
      distinct()
    updateSelectizeInput(session, "exercise_filter_st",
                         choices = c("All Exercises", sort(as.character(available_exercises$Exercise))),
                         selected = NULL,
                         server = TRUE)
    
    available_items <- bk_struct %>% filter(DB == "SSM" & Exercise %in% available_exercises) %>% dplyr::select(Common_Item) %>% distinct()
    available_items_st(available_items$Common_Item)
    updateSelectizeInput(session, "item_filter_st",
                         choices = c("All Items", sort_common_items(available_items$Common_Item)),
                         selected = NULL,
                         server = TRUE)
  })
  
  observeEvent(input$loadTRdata, {
    reset_all_panels() # Call to reset all panels
    shinyjs::reset("tr_filters_wrapper") # Reset filters for Transparency Exercise tab
    output$dynamic_tr_filters <- renderUI({ NULL })
    shinyjs::show("loading_gif_tr")
    
    preview_result <- fetch_preview_result(get_dataset_source("TR")$view)
    tr_data(preview_result$data)
    display_tr_data(preview_result$data)
    
    shinyjs::hide("loading_gif_tr")
    tr_data_visible(TRUE)
    tr_data_loaded(TRUE)

    if (preview_result$limited) {
      show_preview_notice("tr-preview-notice", preview_result$total_rows)
    }
    
    # Update filters with pre-defined objects (using TR specific ones)
    updateSelectizeInput(session, "country_filter_tr",
                         choices = c("All Countries" = "All Countries", countries_tr),
                         selected = NULL,
                         server = TRUE)
    
    updateSelectizeInput(session, "bank_filter_tr",
                         choices = c("All Banks" = "All Banks", initial_bank_tr),
                         selected = NULL,
                         server = TRUE)
    
    # Update exercise and item filters from bk_struct for TR
    available_exercises <- bk_struct %>% filter(DB == "TR") %>% 
      dplyr::select(Exercise) %>% 
      distinct() %>% 
      arrange(Exercise)
    updateSelectizeInput(session, "exercise_filter_tr",
                         choices = c("All Exercises", sort(as.character(available_exercises$Exercise))),
                         selected = NULL,
                         server = TRUE)
    
    available_items <- bk_struct %>% 
      filter(DB == "TR" & Exercise %in% available_exercises$Exercise) %>% 
      dplyr::select(Common_Item) %>% 
      distinct()
    available_items_tr(available_items$Common_Item) # Populate available_items_tr
    updateSelectizeInput(session, "item_filter_tr",
                         choices = c("All Items", sort_common_items(available_items$Common_Item)),
                         selected = NULL,
                         server = TRUE)
  })
  
  observeEvent(input$loadPLCdata, {
    load_standard_thematic_dataset("PLC", countries_plc, initial_bank_plc)
  })
  
  observeEvent(input$loadEXPdata, {
    load_standard_thematic_dataset("EXP", countries_exp, initial_bank_exp)
  })
  
  observeEvent(input$loadSOVdata, {
    load_standard_thematic_dataset("SOV", countries_sov, initial_bank_sov)
  })
  
  observeEvent(input$loadMKTdata, {
    load_standard_thematic_dataset("MKT", countries_mkt, initial_bank_mkt)
  })
  
  observeEvent(input$loadRPSdata, {
    reset_all_panels()
    shinyjs::reset("th_filters_wrapper")
    output$dynamic_th_filters <- renderUI({ NULL })
    shinyjs::show("loading_gif_th")
    
    preview_result <- fetch_preview_result(get_dataset_source("RPS")$view)
    th_data(preview_result$data)
    display_th_data(preview_result$data)
    
    shinyjs::hide("loading_gif_th")
    thematic_data_visible(TRUE)
    thematic_data_loaded(TRUE)
    active_th_dataset_type("RPS")

    if (preview_result$limited) {
      show_preview_notice("rps-preview-notice", preview_result$total_rows)
    }
    
    # Populate RPS-specific filters immediately
    updateSelectizeInput(session, "country_filter_th_rps",
                         choices = c("All Countries" = "All Countries", sort(countries_rps)),
                         selected = NULL,
                         server = TRUE)
    
    # Get available periods from the data structure
    available_periods <- unique(risk_parameters_structure$Period)
    period_labels <- format(zoo::as.yearmon(as.Date(paste0(available_periods, "01"), format = "%Y%m%d")), "%B %Y")
    
    updateSelectizeInput(session, "period_filter_th",
                         choices = c("All Periods" = "All Periods", sort(period_labels)),
                         selected = NULL,
                         server = TRUE)
    
    # Get available exposures from the data structure
    available_exposures <- unique(risk_parameters_structure$Exposure_Label)
    
    updateSelectizeInput(session, "exposure_filter_th",
                         choices = c("All Exposures" = "All Exposures", sort(available_exposures)),
                         selected = NULL,
                         server = TRUE)
    
    # Get available items
    available_items <- unique(risk_parameters_structure$Common_Item)
    
    available_items_th(available_items)
    updateSelectizeInput(session, "item_filter_th_rps",
                         choices = c("All Items", sort_common_items(available_items)),
                         selected = NULL,
                         server = TRUE)
  })
  
  
  
  observeEvent(input$loadBDSdata, {
    reset_all_panels()
    shinyjs::reset("th_filters_wrapper")
    output$dynamic_th_filters <- renderUI({ NULL })
    shinyjs::show("loading_gif_th")
    
    loaded_data <- distress_event() 
    th_data(loaded_data)
    display_th_data(loaded_data)
    
    shinyjs::hide("loading_gif_th")
    thematic_data_visible(TRUE)
    thematic_data_loaded(TRUE)
    active_th_dataset_type("BDS")
    
    # Create country choices - using distress_structure directly
    country_choices_bds <- distress_structure %>% 
      select(Country, ISO2) %>% 
      distinct() %>% 
      arrange(Country)
    country_choices_named <- setNames(country_choices_bds$ISO2, country_choices_bds$Country)
    
    updateSelectizeInput(session, "country_filter_th_bds",  # Changed from country_filter_th
                         choices = c("All Countries" = "All Countries", country_choices_named),
                         selected = NULL,
                         server = TRUE)
    
    # For BDS, create bank choices using Name instead of Bank_ID
    bank_choices_bds <- banks_orig_bds %>% 
      arrange(DisplayName)
    bank_choices_named <- setNames(bank_choices_bds$Name, bank_choices_bds$DisplayName)
    
    updateSelectizeInput(session, "bank_filter_th_bds",  # Changed from bank_filter_th
                         choices = c("All Banks" = "All Banks", bank_choices_named),
                         selected = NULL,
                         server = TRUE)
  })
  
  # --- Cascading filters logic ---
  observeEvent(input$country_filter_st, {
    req(st_data())
    shinyjs::show("loading_gif_st")
    context <- active_st_dataset_type()
    
    if (is.null(input$country_filter_st) || "All Countries" %in% input$country_filter_st) {
      # Reset bank, exercise, and item filters to all for the current context
      if (context == "ST") {
        updateSelectizeInput(session, "bank_filter_st", choices = c("All Banks" = "All Banks", initial_bank_st), selected = NULL)
        available_exercises <- bk_struct %>% filter(DB == "ST") %>% dplyr::select(Exercise) %>% distinct()
        available_items <- bk_struct %>% filter(DB == "ST") %>% dplyr::select(Common_Item) %>% distinct()
      } else {
        updateSelectizeInput(session, "bank_filter_st", choices = c("All Banks" = "All Banks", initial_bank_ssm), selected = NULL)
        available_exercises <- bk_struct %>% filter(DB == "SSM") %>% dplyr::select(Exercise) %>% distinct()
        available_items <- bk_struct %>% filter(DB == "SSM") %>% dplyr::select(Common_Item) %>% distinct()
      }
      updateSelectizeInput(session, "exercise_filter_st", choices = c("All Exercises", sort(as.character(available_exercises$Exercise))), selected = NULL)
      updateSelectizeInput(session, "item_filter_st", choices = c("All Items", sort_common_items(available_items$Common_Item)), selected = NULL)
      shinyjs::hide("loading_gif_st")  
      return()
    }
    
    # Filter choices based on selected country
    if (context == "ST") {
      banks_for_country <- banks_orig_st %>% filter(ISO2 %in% input$country_filter_st)
      bank_choices <- setNames(banks_for_country$Bank_ID, banks_for_country$DisplayName)
      updateSelectizeInput(session, "bank_filter_st", choices = c("All Banks" = "All Banks", bank_choices), selected = NULL)
    } else if (context == "SSM") {
      banks_for_country <- banks_orig_ssm %>% filter(ISO2 %in% input$country_filter_st)
      bank_choices <- setNames(banks_for_country$Bank_ID, banks_for_country$DisplayName)
      updateSelectizeInput(session, "bank_filter_st", choices = c("All Banks" = "All Banks", bank_choices), selected = NULL)
    }
    
    filtered_bk_country <- bk_struct %>% filter(DB == context, ISO2 %in% input$country_filter_st)
    available_exercises <- filtered_bk_country %>% dplyr::select(Exercise) %>% distinct()
    updateSelectizeInput(session, "exercise_filter_st", choices = c("All Exercises", sort(as.character(available_exercises$Exercise))), selected = NULL)
    
    available_items <- filtered_bk_country %>% dplyr::select(Common_Item) %>% distinct()
    available_items_st(available_items$Common_Item)
    updateSelectizeInput(session, "item_filter_st", choices = c("All Items", sort_common_items(available_items$Common_Item)), selected = NULL)
    
    shinyjs::hide("loading_gif_st")  
  }, ignoreNULL = FALSE)
  
  
  observeEvent(input$country_filter_tr, {
    req(tr_data())
    shinyjs::show("loading_gif_tr")
    
    if (is.null(input$country_filter_tr) || "All Countries" %in% input$country_filter_tr) {
      # Reset bank, exercise, and item filters to all for the current context
      updateSelectizeInput(session, "bank_filter_tr", choices = c("All Banks" = "All Banks", initial_bank_tr), selected = NULL)
      available_exercises <- bk_struct %>% filter(DB == "TR") %>% dplyr::select(Exercise) %>% distinct()
      available_items <- bk_struct %>% filter(DB == "TR") %>% dplyr::select(Common_Item) %>% distinct()
      
      updateSelectizeInput(session, "exercise_filter_tr", choices = c("All Exercises", sort(as.character(available_exercises$Exercise))), selected = NULL)
      updateSelectizeInput(session, "item_filter_tr", choices = c("All Items", sort_common_items(available_items$Common_Item)), selected = NULL)
      shinyjs::hide("loading_gif_tr")
      return()
    }
    
    # Filter choices based on selected country
    banks_for_country <- banks_orig_tr %>% filter(ISO2 %in% input$country_filter_tr)
    bank_choices <- setNames(banks_for_country$Bank_ID, banks_for_country$DisplayName)
    updateSelectizeInput(session, "bank_filter_tr", choices = c("All Banks" = "All Banks", bank_choices), selected = NULL)
    
    filtered_bk_country <- bk_struct %>% filter(DB == "TR", ISO2 %in% input$country_filter_tr)
    available_exercises <- filtered_bk_country %>% dplyr::select(Exercise) %>% distinct()
    updateSelectizeInput(session, "exercise_filter_tr", choices = c("All Exercises", sort(as.character(available_exercises$Exercise))), selected = NULL)
    
    available_items <- filtered_bk_country %>% dplyr::select(Common_Item) %>% distinct()
    available_items_tr(available_items$Common_Item) # Populate available_items_tr
    updateSelectizeInput(session, "item_filter_tr", choices = c("All Items", sort_common_items(available_items$Common_Item)), selected = NULL)
    shinyjs::hide("loading_gif_tr")
  }, ignoreNULL = FALSE)
  
  observeEvent(input$bank_filter_tr, { # New TR bank filter
    req(tr_data())
    
    if (is.null(input$bank_filter_tr) || "All Banks" %in% input$bank_filter_tr) {
      return()
    }
    
    filtered_bk_bank <- bk_struct %>% filter(DB == "TR", Bank_ID %in% input$bank_filter_tr)
    
    available_exercises <- filtered_bk_bank %>% dplyr::select(Exercise) %>% distinct()
    updateSelectizeInput(session, "exercise_filter_tr", choices = c("All Exercises", sort(as.character(available_exercises$Exercise))), selected = NULL)
    
    available_items <- filtered_bk_bank %>% dplyr::select(Common_Item) %>% distinct()
    available_items_tr(available_items$Common_Item) # Populate available_items_tr
    updateSelectizeInput(session, "item_filter_tr", choices = c("All Items", sort_common_items(available_items$Common_Item)), selected = NULL)
    
  }, ignoreNULL = FALSE) # End of observeEvent(input$bank_filter_tr, ...)
  
  observeEvent(input$country_filter_th_standard, {
    req(th_data())
    shinyjs::show("loading_gif_th")
    context <- active_th_dataset_type()
    
    # Only proceed if context is a standard dataset (PLC, EXP, SOV, MKT)
    if (!(context %in% c("PLC", "EXP", "SOV", "MKT"))) {
      shinyjs::hide("loading_gif_th")
      return()
    }
    
    if (is.null(input$country_filter_th_standard) || "All Countries" %in% input$country_filter_th_standard) {
      # Determine base filter for bk_struct based on context
      base_filter <- bk_struct %>% filter(DB == context)
      
      initial_bank_th_choices <- switch(context,
                                        "PLC" = initial_bank_plc,
                                        "EXP" = initial_bank_exp,
                                        "SOV" = initial_bank_sov,
                                        "MKT" = initial_bank_mkt,
                                        c()
      )
      
      updateSelectizeInput(session, "bank_filter_th_standard", 
                           choices = c("All Banks" = "All Banks", initial_bank_th_choices), 
                           selected = NULL)
      
      available_exercises <- base_filter %>% dplyr::select(Exercise) %>% distinct()
      updateSelectizeInput(session, "exercise_filter_th_standard", 
                           choices = c("All Exercises", sort(as.character(available_exercises$Exercise))), 
                           selected = NULL)
      
      available_items <- base_filter %>% dplyr::select(Common_Item) %>% distinct()
      available_items_th(available_items$Common_Item)
      updateSelectizeInput(session, "item_filter_th_standard", 
                           choices = c("All Items", sort_common_items(available_items$Common_Item)), 
                           selected = NULL)
      shinyjs::hide("loading_gif_th")
      return()
    }
    
    # Filter choices based on selected country
    banks_orig_th_choices <- switch(context,
                                    "PLC" = banks_orig_plc,
                                    "EXP" = banks_orig_exp,
                                    "SOV" = banks_orig_sov,
                                    "MKT" = banks_orig_mkt,
                                    data.frame()
    )
    
    banks_for_country <- banks_orig_th_choices %>% filter(ISO2 %in% input$country_filter_th_standard)
    bank_choices <- setNames(banks_for_country$Bank_ID, banks_for_country$DisplayName)
    updateSelectizeInput(session, "bank_filter_th_standard", 
                         choices = c("All Banks" = "All Banks", bank_choices), 
                         selected = NULL)
    
    filtered_bk_country <- bk_struct %>% filter(DB == context, ISO2 %in% input$country_filter_th_standard)
    available_exercises <- filtered_bk_country %>% dplyr::select(Exercise) %>% distinct()
    updateSelectizeInput(session, "exercise_filter_th_standard", 
                         choices = c("All Exercises", sort(as.character(available_exercises$Exercise))), 
                         selected = NULL)
    
    available_items <- filtered_bk_country %>% dplyr::select(Common_Item) %>% distinct()
    available_items_th(available_items$Common_Item)
    updateSelectizeInput(session, "item_filter_th_standard", 
                         choices = c("All Items", sort_common_items(available_items$Common_Item)), 
                         selected = NULL)
    shinyjs::hide("loading_gif_th")
  }, ignoreNULL = FALSE)
  
  observeEvent(input$bank_filter_th_standard, {
    req(th_data())
    context <- active_th_dataset_type()
    
    if (!(context %in% c("PLC", "EXP", "SOV", "MKT"))) {
      return()
    }
    
    if (is.null(input$bank_filter_th_standard) || "All Banks" %in% input$bank_filter_th_standard) {
      return()
    }
    
    filtered_bk_bank <- bk_struct %>% filter(DB == context, Bank_ID %in% input$bank_filter_th_standard)
    
    available_exercises <- filtered_bk_bank %>% dplyr::select(Exercise) %>% distinct()
    updateSelectizeInput(session, "exercise_filter_th_standard", 
                         choices = c("All Exercises", sort(as.character(available_exercises$Exercise))), 
                         selected = NULL)
    
    available_items <- filtered_bk_bank %>% dplyr::select(Common_Item) %>% distinct()
    available_items_th(available_items$Common_Item)
    updateSelectizeInput(session, "item_filter_th_standard", 
                         choices = c("All Items", sort_common_items(available_items$Common_Item)), 
                         selected = NULL)
    
  }, ignoreNULL = FALSE)
  
  observeEvent(input$exercise_filter_th_standard, {
    req(th_data())
    context <- active_th_dataset_type()
    
    if (!(context %in% c("PLC", "EXP", "SOV", "MKT"))) {
      return()
    }
    
    if (is.null(input$exercise_filter_th_standard) || "All Exercises" %in% input$exercise_filter_th_standard) {
      base_filter <- bk_struct %>% filter(DB == context)
      if (!is.null(input$country_filter_th_standard) && !("All Countries" %in% input$country_filter_th_standard)) {
        base_filter <- base_filter %>% filter(ISO2 %in% input$country_filter_th_standard)
      }
      if (!is.null(input$bank_filter_th_standard) && !("All Banks" %in% input$bank_filter_th_standard)) {
        base_filter <- base_filter %>% filter(Bank_ID %in% input$bank_filter_th_standard)
      }
      
      available_items <- base_filter %>% dplyr::select(Common_Item) %>% distinct()
      available_items_th(available_items$Common_Item)
      updateSelectizeInput(session, "item_filter_th_standard",
                           choices = c("All Items", sort_common_items(available_items$Common_Item)),
                           selected = NULL,
                           server = TRUE)
      return()
    }
    
    filtered_bk <- bk_struct %>% filter(DB == context)
    
    if (!is.null(input$country_filter_th_standard) && !("All Countries" %in% input$country_filter_th_standard)) {
      filtered_bk <- filtered_bk %>% filter(ISO2 %in% input$country_filter_th_standard)
    }
    if (!is.null(input$bank_filter_th_standard) && !("All Banks" %in% input$bank_filter_th_standard)) {
      filtered_bk <- filtered_bk %>% filter(Bank_ID %in% input$bank_filter_th_standard)
    }
    
    filtered_bk <- filtered_bk %>% filter(Exercise %in% input$exercise_filter_th_standard)
    
    available_items <- filtered_bk %>% dplyr::select(Common_Item) %>% distinct()
    available_items_th(available_items$Common_Item)
    updateSelectizeInput(session, "item_filter_th_standard",
                         choices = c("All Items", sort_common_items(available_items$Common_Item)),
                         selected = NULL,
                         server = TRUE)
    
  }, ignoreNULL = FALSE)
  
  observeEvent(input$country_filter_th_bds, {
    req(th_data())
    shinyjs::show("loading_gif_th")
    context <- active_th_dataset_type()
    
    # Only proceed if context is BDS
    if (context != "BDS") {
      shinyjs::hide("loading_gif_th")
      return()
    }
    
    if (is.null(input$country_filter_th_bds) || "All Countries" %in% input$country_filter_th_bds) {
      # Reset to all banks for BDS
      bank_choices_bds <- banks_orig_bds %>% 
        arrange(DisplayName)
      bank_choices_named <- setNames(bank_choices_bds$Name, bank_choices_bds$DisplayName)
      
      updateSelectizeInput(session, "bank_filter_th_bds", 
                           choices = c("All Banks" = "All Banks", bank_choices_named), 
                           selected = NULL)
      shinyjs::hide("loading_gif_th")
      return()
    }
    
    # Filter banks based on selected country
    banks_for_country <- banks_orig_bds %>% 
      filter(ISO2 %in% input$country_filter_th_bds)
    
    bank_choices <- setNames(banks_for_country$Name, banks_for_country$DisplayName)
    updateSelectizeInput(session, "bank_filter_th_bds", 
                         choices = c("All Banks" = "All Banks", bank_choices), 
                         selected = NULL)
    
    shinyjs::hide("loading_gif_th")
  }, ignoreNULL = FALSE)
  
  
  observeEvent(input$bank_filter_st, {
    req(st_data())
    context <- active_st_dataset_type()
    
    if (is.null(input$bank_filter_st) || "All Banks" %in% input$bank_filter_st) {
      return()
    }
    
    filtered_bk_bank <- bk_struct %>% filter(DB == context, Bank_ID %in% input$bank_filter_st)
    
    available_exercises <- filtered_bk_bank %>% dplyr::select(Exercise) %>% distinct()
    updateSelectizeInput(session, "exercise_filter_st", choices = c("All Exercises", sort(as.character(available_exercises$Exercise))), selected = NULL)
    
    available_items <- filtered_bk_bank %>% dplyr::select(Common_Item) %>% distinct()
    available_items_st(available_items$Common_Item)
    updateSelectizeInput(session, "item_filter_st", choices = c("All Items", sort_common_items(available_items$Common_Item)), selected = NULL)
    
  }, ignoreNULL = FALSE) # End of observeEvent(input$bank_filter_st, ...)
  
  observeEvent(input$exercise_filter_st, {
    req(st_data())
    context <- active_st_dataset_type()
    
    if (is.null(input$exercise_filter_st) || "All Exercises" %in% input$exercise_filter_st) {
      # If no specific exercise is selected, or "All Exercises" is chosen,
      # reset item filter based on current country/bank selections.
      # This logic is similar to what happens when country/bank filters are reset.
      
      # Determine base filter for bk_struct
      base_filter <- bk_struct %>% filter(DB == context)
      if (!is.null(input$country_filter_st) && !("All Countries" %in% input$country_filter_st)) {
        base_filter <- base_filter %>% filter(ISO2 %in% input$country_filter_st)
      }
      if (!is.null(input$bank_filter_st) && !("All Banks" %in% input$bank_filter_st)) {
        base_filter <- base_filter %>% filter(Bank_ID %in% input$bank_filter_st)
      }
      
      available_items <- base_filter %>% dplyr::select(Common_Item) %>% distinct()
      available_items_st(available_items$Common_Item)
      updateSelectizeInput(session, "item_filter_st",
                           choices = c("All Items", sort_common_items(available_items$Common_Item)),
                           selected = NULL,
                           server = TRUE)
      return()
    }
    
    # Filter bk_struct based on selected country, bank, and exercise
    filtered_bk <- bk_struct %>% filter(DB == context)
    
    if (!is.null(input$country_filter_st) && !("All Countries" %in% input$country_filter_st)) {
      filtered_bk <- filtered_bk %>% filter(ISO2 %in% input$country_filter_st)
    }
    if (!is.null(input$bank_filter_st) && !("All Banks" %in% input$bank_filter_st)) {
      filtered_bk <- filtered_bk %>% filter(Bank_ID %in% input$bank_filter_st)
    }
    
    filtered_bk <- filtered_bk %>% filter(Exercise %in% input$exercise_filter_st)
    
    available_items <- filtered_bk %>% dplyr::select(Common_Item) %>% distinct()
    available_items_st(available_items$Common_Item)
    updateSelectizeInput(session, "item_filter_st",
                         choices = c("All Items", sort_common_items(available_items$Common_Item)),
                         selected = NULL,
                         server = TRUE)
    
  }, ignoreNULL = FALSE) # End of observeEvent(input$exercise_filter_st, ...)
  
  observeEvent(input$exercise_filter_tr, { # New TR exercise filter
    req(tr_data())
    
    if (is.null(input$exercise_filter_tr) || "All Exercises" %in% input$exercise_filter_tr) {
      # If no specific exercise is selected, or "All Exercises" is chosen,
      # reset item filter based on current country/bank selections.
      
      # Determine base filter for bk_struct
      base_filter <- bk_struct %>% filter(DB == "TR")
      if (!is.null(input$country_filter_tr) && !("All Countries" %in% input$country_filter_tr)) {
        base_filter <- base_filter %>% filter(ISO2 %in% input$country_filter_tr)
      }
      if (!is.null(input$bank_filter_tr) && !("All Banks" %in% input$bank_filter_tr)) {
        base_filter <- base_filter %>% filter(Bank_ID %in% input$bank_filter_tr)
      }
      
      available_items <- base_filter %>% dplyr::select(Common_Item) %>% distinct()
      available_items_tr(available_items$Common_Item) # Populate available_items_tr
      updateSelectizeInput(session, "item_filter_tr",
                           choices = c("All Items", sort_common_items(available_items$Common_Item)),
                           selected = NULL,
                           server = TRUE)
      return()
    }
    
    # Filter bk_struct based on selected country, bank, and exercise
    filtered_bk <- bk_struct %>% filter(DB == "TR")
    
    if (!is.null(input$country_filter_tr) && !("All Countries" %in% input$country_filter_tr)) {
      filtered_bk <- filtered_bk %>% filter(ISO2 %in% input$country_filter_tr)
    }
    if (!is.null(input$bank_filter_tr) && !("All Banks" %in% input$bank_filter_tr)) {
      filtered_bk <- filtered_bk %>% filter(Bank_ID %in% input$bank_filter_tr)
    }
    
    filtered_bk <- filtered_bk %>% filter(Exercise %in% input$exercise_filter_tr)
    
    available_items <- filtered_bk %>% dplyr::select(Common_Item) %>% distinct()
    available_items_tr(available_items$Common_Item) # Populate available_items_tr
    updateSelectizeInput(session, "item_filter_tr",
                         choices = c("All Items", sort_common_items(available_items$Common_Item)),
                         selected = NULL,
                         server = TRUE)
    
  }, ignoreNULL = FALSE) # End of observeEvent(input$exercise_filter_tr, ...)
  
  observeEvent(c(input$country_filter_st, input$bank_filter_st, input$exercise_filter_st, input$item_filter_st), {
    
    if (!is.null(input$item_filter_st) && length(input$item_filter_st) > 0) {
      
      selected_item <- input$item_filter_st
      active_dataset <- active_st_dataset_type()
      
      items_to_filter <- if ("All Items" %in% selected_item) {
        available_items_st()
      } else {
        selected_item
      }
      
      item_data <- itm_struct_labelled %>% filter(DB == active_dataset, Common_Item %in% items_to_filter)
      
      # Identify columns with more than one unique value and are not all NA
      cols_to_check <- setdiff(names(item_data), "Common_Item")
      filterable_cols <- cols_to_check[sapply(cols_to_check, function(col) {
        length(unique(item_data[[col]])) > 1 && !all(is.na(item_data[[col]]))
      })]
      
      dynamic_filters_st(filterable_cols)
      
      # Create the dynamic UI
      output$dynamic_st_filters <- renderUI({
        fluidRow(
          lapply(filterable_cols, function(col_name) {
            
            # Logic for factor type columns
            if (is.factor(item_data[[col_name]])) {
              choices_factor <- sort(unique(item_data[[col_name]]))
              
              if (length(choices_factor) > 0) {
                labels <- as.character(choices_factor)
                mapping <- mappings[[col_name]]
                codes <- if (is.null(mapping)) as.character(unclass(choices_factor)) else mapping[match(labels, names(mapping))]
                
                is_total <- grepl("total", labels, ignore.case = TRUE) | grepl("no breakdown", labels, ignore.case = TRUE) | is.na(labels)
                other_labels <- labels[!is_total]
                other_codes <- codes[!is_total]
                
                if (col_name == "Scenario") {
                  labels_to_remove_lower <- tolower(c("Actual", "Actual Restated", "Adverse Sovereign Shock"))
                  
                  # Convert other_labels to lower case for comparison
                  indices_to_remove <- which(tolower(other_labels) %in% labels_to_remove_lower)
                  
                  if (length(indices_to_remove) > 0) {
                    other_labels <- other_labels[-indices_to_remove]
                    other_codes <- other_codes[-indices_to_remove]
                  }
                }
                
                other_formatted_choices <- if (length(other_codes) > 0) {
                  if (col_name == "Exposure") setNames(as.character(other_codes), paste(as.character(other_codes), "-", other_labels))
                  else setNames(as.character(other_codes), other_labels)
                } else { character(0) }
                
                # Only render the dropdown if there are meaningful choices
                if (length(other_formatted_choices) > 0) {
                  all_choice <- c("All" = "All")
                  total_choice <- c("Total / No Breakdown" = "0")
                  new_choices <- if (any(is_total)) c(all_choice, total_choice, other_formatted_choices) else c(all_choice, other_formatted_choices)
                  
                  column(3, selectizeInput(paste0("dynamic_", col_name), label = col_name, choices = new_choices, multiple = TRUE))
                } else { NULL }
                
              } else { NULL }
              
            } else { # Logic for non-factor type columns
              choices <- unique(item_data[[col_name]])
              has_na <- any(is.na(choices))
              choices_no_na <- choices[!is.na(choices)]
              
              is_total_str <- grepl("total", choices_no_na, ignore.case = TRUE) | grepl("no breakdown", choices_no_na, ignore.case = TRUE)
              other_choices <- choices_no_na[!is_total_str]
              
              # Only render the dropdown if there are meaningful choices
              if (length(other_choices) > 0) {
                all_choice <- c("All" = "All")
                total_choice <- c("Total / No Breakdown" = "0")
                new_choices <- if (has_na || any(is_total_str)) c(all_choice, total_choice, other_choices) else c(all_choice, choices)
                
                column(3, selectizeInput(paste0("dynamic_", col_name), label = col_name, choices = new_choices, multiple = TRUE))
              } else { NULL }
            }
          })
        )
      })
    } else {
      output$dynamic_st_filters <- renderUI({ NULL })
    }}, ignoreNULL = FALSE) # End of observeEvent(c(input$country_filter_st, ...), ...)
  
  observeEvent(c(input$country_filter_tr, input$bank_filter_tr, input$exercise_filter_tr, input$item_filter_tr), {
    
    if (!is.null(input$item_filter_tr) && length(input$item_filter_tr) > 0) {
      
      selected_item <- input$item_filter_tr
      items_to_filter <- if ("All Items" %in% selected_item) {
        available_items_tr()
      } else {
        selected_item
      }
      
      item_data <- itm_struct_labelled %>% filter(DB == "TR", Common_Item %in% items_to_filter)
      
      # Identify columns with more than one unique value and are not all NA
      cols_to_check <- setdiff(names(item_data), "Common_Item")
      filterable_cols <- cols_to_check[sapply(cols_to_check, function(col) {
        length(unique(item_data[[col]])) > 1 && !all(is.na(item_data[[col]]))
      })]
      
      dynamic_filters_tr(filterable_cols) # Use dynamic_filters_tr
      
      # Create the dynamic UI
      output$dynamic_tr_filters <- renderUI({ # Use dynamic_tr_filters
        fluidRow(
          lapply(filterable_cols, function(col_name) {
            
            # Logic for factor type columns
            if (is.factor(item_data[[col_name]])) {
              choices_factor <- sort(unique(item_data[[col_name]]))
              
              if (length(choices_factor) > 0) {
                labels <- as.character(choices_factor)
                mapping <- mappings[[col_name]]
                codes <- if (is.null(mapping)) as.character(unclass(choices_factor)) else mapping[match(labels, names(mapping))]
                
                is_total <- grepl("total", labels, ignore.case = TRUE) | grepl("no breakdown", labels, ignore.case = TRUE) | is.na(labels)
                other_labels <- labels[!is_total]
                other_codes <- codes[!is_total]
                
                # Special handling for Scenario in TR if needed, but here it's default
                # if (col_name == "Scenario") { ... }
                
                other_formatted_choices <- if (length(other_codes) > 0) {
                  if (col_name == "Exposure") setNames(as.character(other_codes), paste(as.character(other_codes), "-", other_labels))
                  else setNames(as.character(other_codes), other_labels)
                } else { character(0) }
                
                # Only render the dropdown if there are meaningful choices
                if (length(other_formatted_choices) > 0) {
                  all_choice <- c("All" = "All")
                  total_choice <- c("Total / No Breakdown" = "0")
                  new_choices <- if (any(is_total)) c(all_choice, total_choice, other_formatted_choices) else c(all_choice, other_formatted_choices)
                  
                  column(3, selectizeInput(paste0("dynamic_", col_name), label = col_name, choices = new_choices, multiple = TRUE))
                } else { NULL }
                
              } else { NULL }
              
            } else { # Logic for non-factor type columns
              choices <- unique(item_data[[col_name]])
              has_na <- any(is.na(choices))
              choices_no_na <- choices[!is.na(choices)]
              
              is_total_str <- grepl("total", choices_no_na, ignore.case = TRUE) | grepl("no breakdown", choices_no_na, ignore.case = TRUE)
              other_choices <- choices_no_na[!is_total_str]
              
              # Only render the dropdown if there are meaningful choices
              if (length(other_choices) > 0) {
                all_choice <- c("All" = "All")
                total_choice <- c("Total / No Breakdown" = "0")
                new_choices <- if (has_na || any(is_total_str)) c(all_choice, total_choice, other_choices) else c(all_choice, choices)
                
                column(3, selectizeInput(paste0("dynamic_", col_name), label = col_name, choices = new_choices, multiple = TRUE))
              } else { NULL }
            }
          })
        )
      })
    } else {
      output$dynamic_tr_filters <- renderUI({ NULL }) # Use dynamic_tr_filters
    }
  }, ignoreNULL = FALSE) # End of observeEvent(c(input$country_filter_tr, ...), ...)
  
  observeEvent(c(input$country_filter_th_standard, input$bank_filter_th_standard, input$exercise_filter_th_standard, input$item_filter_th_standard), {
    
    # Get the active dataset type
    active_dataset <- active_th_dataset_type()
    
    # Only proceed if we have a valid dataset type AND it's a standard thematic dataset
    if (is.null(active_dataset)) {
      output$dynamic_th_filters <- renderUI({ NULL })
      return()
    }
    
    if (!(active_dataset %in% c("PLC", "EXP", "SOV", "MKT"))) {
      output$dynamic_th_filters <- renderUI({ NULL })
      return()
    }
    
    # Check if we have a valid item selection
    if (is.null(input$item_filter_th_standard) || length(input$item_filter_th_standard) == 0) {
      output$dynamic_th_filters <- renderUI({ NULL })
      return()
    }
    
    selected_item <- input$item_filter_th_standard
    
    items_to_filter <- if ("All Items" %in% selected_item) {
      available_items_th()
    } else {
      selected_item
    }
    
    # Check if we have valid items to filter
    if (is.null(items_to_filter) || length(items_to_filter) == 0) {
      output$dynamic_th_filters <- renderUI({ NULL })
      return()
    }
    
    item_data <- itm_struct_labelled %>% filter(DB == active_dataset, Common_Item %in% items_to_filter)
    
    # Check if item_data has any rows
    if (nrow(item_data) == 0) {
      output$dynamic_th_filters <- renderUI({ NULL })
      return()
    }
    
    # Identify columns with more than one unique value and are not all NA
    cols_to_check <- setdiff(names(item_data), 
                             c("Common_Item", "ISO2", "Bank_ID", "Exercise"))
    
    filterable_cols <- cols_to_check[sapply(cols_to_check, function(col) {
      col_data <- item_data[[col]]
      !is.null(col_data) && length(col_data) > 0 && length(unique(col_data)) > 1 && !all(is.na(col_data))
    })]
    
    dynamic_filters_th(filterable_cols)
    
    # Create the dynamic UI
    output$dynamic_th_filters <- renderUI({
      if (length(filterable_cols) == 0) {
        return(NULL)
      }
      
      fluidRow(
        lapply(filterable_cols, function(col_name) {
          
          # Get column data
          col_data <- item_data[[col_name]]
          
          # Skip if column data is NULL or empty
          if (is.null(col_data) || length(col_data) == 0) {
            return(NULL)
          }
          
          # Logic for factor type columns
          if (is.factor(col_data)) {
            choices_factor <- sort(unique(col_data))
            
            # Check if we have valid choices
            if (is.null(choices_factor) || length(choices_factor) == 0) {
              return(NULL)
            }
            
            labels <- as.character(choices_factor)
            mapping <- mappings[[col_name]]
            codes <- if (is.null(mapping)) as.character(unclass(choices_factor)) else mapping[match(labels, names(mapping))]
            
            is_total <- grepl("total", labels, ignore.case = TRUE) | grepl("no breakdown", labels, ignore.case = TRUE) | is.na(labels)
            other_labels <- labels[!is_total]
            other_codes <- codes[!is_total]
            
            if (col_name == "Scenario") {
              labels_to_remove_lower <- tolower(c("Actual", "Actual Restated", "Adverse Sovereign Shock"))
              
              # Convert other_labels to lower case for comparison
              indices_to_remove <- which(tolower(other_labels) %in% labels_to_remove_lower)
              
              if (length(indices_to_remove) > 0) {
                other_labels <- other_labels[-indices_to_remove]
                other_codes <- other_codes[-indices_to_remove]
              }
            }
            
            other_formatted_choices <- if (length(other_codes) > 0) {
              if (col_name == "Exposure") setNames(as.character(other_codes), paste(as.character(other_codes), "-", other_labels))
              else setNames(as.character(other_codes), other_labels)
            } else { character(0) }
            
            # Only render the dropdown if there are meaningful choices
            if (length(other_formatted_choices) > 0) {
              all_choice <- c("All" = "All")
              total_choice <- c("Total / No Breakdown" = "0")
              new_choices <- if (any(is_total)) c(all_choice, total_choice, other_formatted_choices) else c(all_choice, other_formatted_choices)
              
              column(3, selectizeInput(paste0("dynamic_", col_name), label = col_name, choices = new_choices, multiple = TRUE))
            } else { NULL }
            
          } else { # Logic for non-factor type columns
            choices <- unique(col_data)
            
            # Check if we have valid choices
            if (is.null(choices) || length(choices) == 0) {
              return(NULL)
            }
            
            has_na <- any(is.na(choices))
            choices_no_na <- choices[!is.na(choices)]
            
            is_total_str <- grepl("total", choices_no_na, ignore.case = TRUE) | grepl("no breakdown", choices_no_na, ignore.case = TRUE)
            other_choices <- choices_no_na[!is_total_str]
            
            # Only render the dropdown if there are meaningful choices
            if (length(other_choices) > 0) {
              all_choice <- c("All" = "All")
              total_choice <- c("Total / No Breakdown" = "0")
              new_choices <- if (has_na || any(is_total_str)) c(all_choice, total_choice, other_choices) else c(all_choice, choices)
              
              column(3, selectizeInput(paste0("dynamic_", col_name), label = col_name, choices = new_choices, multiple = TRUE))
            } else { NULL }
          }
        })
      )
    })
  }, ignoreNULL = FALSE)
  
  
  
  
  # --- "Go" button logic ---
  
  observeEvent(input$go_button_th, {
    active_dataset <- active_th_dataset_type()
    req(active_dataset)
    
    # Determine which input IDs to use based on dataset type
    if (active_dataset == "BDS") {
      country_input <- input$country_filter_th_bds
      bank_input <- input$bank_filter_th_bds
      exercise_input <- NULL
      item_input <- NULL
    } else if (active_dataset == "RPS") {
      country_input <- input$country_filter_th_rps
      period_input <- input$period_filter_th
      exposure_input <- input$exposure_filter_th
      item_input <- input$item_filter_th_rps
    } else {
      country_input <- input$country_filter_th_standard
      bank_input <- input$bank_filter_th_standard
      exercise_input <- input$exercise_filter_th_standard
      item_input <- input$item_filter_th_standard
    }
    
    # Special handling for RPS dataset
    if (!is.null(active_dataset) && active_dataset == "RPS") {
      where_clauses <- build_rps_where_clauses(country_input, period_input, exposure_input, item_input)
      preview_result <- fetch_preview_result(get_dataset_source("RPS")$view, where_clauses)
      display_th_data(preview_result$data)

      if (preview_result$limited) {
        show_preview_notice("rps-preview-notice", preview_result$total_rows)
      }
      
    } else if (!is.null(active_dataset) && active_dataset == "BDS") {
      data <- th_data()
      # BDS-specific filtering
      if (!is.null(country_input) && !("All Countries" %in% country_input)) {
        data <- data %>% filter(ISO2 %in% country_input)
      }
      
      if (!is.null(bank_input) && !("All Banks" %in% bank_input)) {
        data <- data %>% filter(Name %in% bank_input)
      }
      # No exercise/item filtering for BDS
      display_th_data(data)
      
    } else {
      where_clauses <- build_standard_where_clauses(
        country_input,
        bank_input,
        exercise_input,
        item_input,
        dynamic_filters_th(),
        input
      )
      preview_result <- fetch_preview_result(get_dataset_source(active_dataset)$view, where_clauses)
      display_th_data(preview_result$data)

      if (preview_result$limited) {
        show_preview_notice(paste0(tolower(active_dataset), "-preview-notice"), preview_result$total_rows)
      }
    }
  })
  
  observeEvent(input$go_button_tr, { # New TR Go button logic
    where_clauses <- build_standard_where_clauses(
      input$country_filter_tr,
      input$bank_filter_tr,
      input$exercise_filter_tr,
      input$item_filter_tr,
      dynamic_filters_tr(),
      input
    )
    preview_result <- fetch_preview_result(get_dataset_source("TR")$view, where_clauses)
    display_tr_data(preview_result$data)

    if (preview_result$limited) {
      show_preview_notice("tr-preview-notice", preview_result$total_rows)
    }
  }) # End of observeEvent(input$go_button_tr, ...)
  
  observeEvent(input$go_button_st, {
    active_dataset <- active_st_dataset_type()
    req(active_dataset)
    where_clauses <- build_standard_where_clauses(
      input$country_filter_st,
      input$bank_filter_st,
      input$exercise_filter_st,
      input$item_filter_st,
      dynamic_filters_st(),
      input
    )
    preview_result <- fetch_preview_result(get_dataset_source(active_dataset)$view, where_clauses)
    display_st_data(preview_result$data)

    if (preview_result$limited) {
      show_preview_notice(paste0(tolower(active_dataset), "-preview-notice"), preview_result$total_rows)
    }
  })
  
  
  # --- Reset filters logic ---
  
  observeEvent(input$reset_dynamic_filters_st, {    # Reset selections for all filters in the wrapper
    shinyjs::reset("st_filters_wrapper")
    
    # Manually clear choices for exercise and item filters to ensure they are empty
    updateSelectizeInput(session, "exercise_filter_st", choices = NULL, selected = character(0))
    updateSelectizeInput(session, "item_filter_st", choices = NULL, selected = character(0))
    
    # Hide dynamic filters UI
    output$dynamic_st_filters <- renderUI({ NULL })
    
    # Reset the displayed data to the full dataset
    display_st_data(st_data())
    available_items_st(NULL)
  }) # End of observeEvent(input$reset_dynamic_filters_st, ...)
  
  observeEvent(input$reset_dynamic_filters_tr, { # New TR reset filters logic
    # Reset selections for all filters in the wrapper
    shinyjs::reset("tr_filters_wrapper")
    
    # Manually clear choices for exercise and item filters to ensure they are empty
    updateSelectizeInput(session, "exercise_filter_tr", choices = NULL, selected = character(0))
    updateSelectizeInput(session, "item_filter_tr", choices = NULL, selected = character(0))
    
    # Hide dynamic filters UI
    output$dynamic_tr_filters <- renderUI({ NULL })
    
    # Reset the displayed data to the full dataset
    display_tr_data(tr_data())
    available_items_tr(NULL)
  }) # End of observeEvent(input$reset_dynamic_filters_tr, ...)
  
  observeEvent(input$reset_dynamic_filters_th, { # New TH reset filters logic
    # Reset selections for all filters in the wrapper
    shinyjs::reset("th_filters_wrapper")
    
    # Manually clear choices for exercise and item filters to ensure they are empty
    updateSelectizeInput(session, "exercise_filter_th_standard", choices = NULL, selected = character(0))
    updateSelectizeInput(session, "item_filter_th_standard", choices = NULL, selected = character(0))
    
    # Hide dynamic filters UI
    output$dynamic_th_filters <- renderUI({ NULL })
    
    # Reset the displayed data to the full dataset
    display_th_data(th_data())
    available_items_th(NULL)
  })
  
  register_reset_button("resetSTdata", "loading_gif_st")
  register_reset_button("resetSSMdata", "loading_gif_st")
  register_reset_button("resetTRdata", "loading_gif_tr")
  register_reset_button("resetPLCdata", "loading_gif_th")
  register_reset_button("resetEXPdata", "loading_gif_th")
  register_reset_button("resetSOVdata", "loading_gif_th")
  register_reset_button("resetMKTdata", "loading_gif_th")
  register_reset_button("resetRPSdata", "loading_gif_th")
  register_reset_button("resetBDSdata", "loading_gif_th")
  
  reset_all_panels <- function() {
    st_data_visible(FALSE)
    tr_data_visible(FALSE)
    st_data_loaded(FALSE)
    st_data(NULL)
    display_st_data(NULL)
    tr_data_loaded(FALSE)
    tr_data(NULL)
    display_tr_data(NULL)
    th_data(NULL)
    display_th_data(NULL)
    thematic_data_loaded(FALSE)
    thematic_data_visible(FALSE)
    active_st_dataset_type(NULL)
    active_th_dataset_type(NULL)
    dynamic_filters_st(list())
    dynamic_filters_tr(list())
    dynamic_filters_th(list())
    available_items_st(NULL)
    available_items_tr(NULL)
    available_items_th(NULL)
  }
  
  
  # Helper function to generate simple R loading script
  generate_simple_r_script <- function(filename_with_extension) {
    file_extension <- tools::file_ext(filename_with_extension)
    
    code_content <- c(
      "# R Script to Load EBA Dataset",
      "# ============================",
      "",
      "# Set the path to your downloaded dataset file",
      'path_to_dataset <- ""  # Enter the full path to your file here',
      "",
      "# Install and load required packages",
      "required_packages <- c('arrow', 'readr')",
      "install_if_missing <- function(packages) {",
      "  new_packages <- packages[!(packages %in% installed.packages()[, 'Package'])]",
      "  if (length(new_packages)) {",
      "    install.packages(new_packages, dependencies = TRUE)",
      "  }",
      "}",
      "install_if_missing(required_packages)",
      "",
      "library(arrow)",
      "library(readr)",
      "",
      "# Load the dataset based on file extension",
      "file_ext <- tools::file_ext(path_to_dataset)",
      "",
      "if (file_ext == 'parquet') {",
      "  data <- arrow::read_parquet(path_to_dataset)",
      "  cat('Loaded data from Parquet file\\n')",
      "} else if (file_ext == 'csv') {",
      "  data <- read_csv(path_to_dataset)",
      "  cat('Loaded data from CSV file\\n')",
      "} else {",
      "  stop('Unsupported file format. Please use .parquet or .csv files.')",
      "}",
      "",
      "# Display first few rows",
      "print(head(data))"
    )
    
    return(code_content)
  } 
  
  
  output$downloadFilteredData_st <- downloadHandler(
    filename = function() { 
      type <- isolate(active_st_dataset_type())
      if (is.null(type)) {
        return(paste0("filtered_stress_test_data_", Sys.Date(), ".zip"))
      }

      if (type == "SSM") {
        paste0("filtered_ssm_data_", Sys.Date(), ".zip")
      } else {
        paste0("filtered_stress_test_data_", Sys.Date(), ".zip")
      }
    },
    content = function(file) {
      type <- isolate(active_st_dataset_type())
      req(type)
      req(filtered_st_download_ready())
      
      shinyjs::show("loading_gif_st")
      on.exit(shinyjs::hide("loading_gif_st"))

      where_clauses <- build_standard_where_clauses(
        input$country_filter_st,
        input$bank_filter_st,
        input$exercise_filter_st,
        input$item_filter_st,
        dynamic_filters_st(),
        input
      )
      create_filtered_download_bundle(
        zip_file = file,
        dataset_label = if (type == "SSM") "SSM stress tests" else "EU-wide stress tests",
        dataset_source_path = get_dataset_source(type)$file,
        dataset_export_basename = if (type == "SSM") "filtered_ssm_data" else "filtered_stress_test_data",
        view_name = get_dataset_source(type)$view,
        where_clauses = where_clauses
      )
    }
  )
  
  
  
  # Filtered TR data download
  output$downloadFilteredData_tr <- downloadHandler(
    filename = function() {
      paste0("filtered_transparency_data_", Sys.Date(), ".zip")
    },
    content = function(file) {
      req(filtered_tr_download_ready())
      shinyjs::show("loading_gif_tr")
      on.exit(shinyjs::hide("loading_gif_tr"))

      where_clauses <- build_standard_where_clauses(
        input$country_filter_tr,
        input$bank_filter_tr,
        input$exercise_filter_tr,
        input$item_filter_tr,
        dynamic_filters_tr(),
        input
      )
      create_filtered_download_bundle(
        zip_file = file,
        dataset_label = "transparency exercise",
        dataset_source_path = get_dataset_source("TR")$file,
        dataset_export_basename = "filtered_transparency_data",
        view_name = get_dataset_source("TR")$view,
        where_clauses = where_clauses
      )
    }
  )
  
  # Filtered TH data download
  output$downloadFilteredData_th <- downloadHandler(
    filename = function() {
      type <- isolate(active_th_dataset_type())
      dataset_name_lower <- if (is.null(type)) "thematic" else tolower(type)

      paste0("filtered_", dataset_name_lower, "_data_", Sys.Date(), ".zip")
    },
    content = function(file) {
      req(filtered_th_download_ready())
      shinyjs::show("loading_gif_th")
      on.exit(shinyjs::hide("loading_gif_th"))

      # Special handling for BDS - it doesn't get filtered
      dataset_type <- isolate(active_th_dataset_type())
      req(dataset_type)
      if (!is.null(dataset_type) && dataset_type == "BDS") {
        selected_countries <- if (has_filter_selection(input$country_filter_th_bds, "All Countries")) input$country_filter_th_bds else character(0)
        selected_banks <- if (has_filter_selection(input$bank_filter_th_bds, "All Banks")) input$bank_filter_th_bds else character(0)
        create_filtered_bds_download_bundle(file, selected_countries, selected_banks)
        return()
      }

      if (!is.null(dataset_type) && dataset_type == "RPS") {
        where_clauses <- build_rps_where_clauses(
          input$country_filter_th_rps,
          input$period_filter_th,
          input$exposure_filter_th,
          input$item_filter_th_rps
        )
        create_filtered_download_bundle(
          zip_file = file,
          dataset_label = "risk parameters",
          dataset_source_path = get_dataset_source("RPS")$file,
          dataset_export_basename = "filtered_rps_data",
          view_name = get_dataset_source("RPS")$view,
          where_clauses = where_clauses
        )
        return()
      }

      where_clauses <- build_standard_where_clauses(
        input$country_filter_th_standard,
        input$bank_filter_th_standard,
        input$exercise_filter_th_standard,
        input$item_filter_th_standard,
        dynamic_filters_th(),
        input
      )
      create_filtered_download_bundle(
        zip_file = file,
        dataset_label = dataset_type,
        dataset_source_path = get_dataset_source(dataset_type)$file,
        dataset_export_basename = paste0("filtered_", tolower(dataset_type), "_data"),
        view_name = get_dataset_source(dataset_type)$view,
        where_clauses = where_clauses
      )
    }
  )
  
  
  
  
  # Overview panel downloads
  output$downloadTotalMetadata_overview <- downloadHandler(
    filename = "Total_Metadata_App.xlsx",
    content = function(file) {
      file.copy("Meta/Total_Metadata_App.xlsx", file, overwrite = TRUE)
    }
  )
  
  output$downloadRScript_overview <- downloadHandler(
    filename = "load_EBA_dataset.R",
    content = function(file) {
      code_content <- generate_simple_r_script("dataset.parquet")  # Generic name
      writeLines(code_content, file)
    }
  )
  
  
  
  # Metadata table outputs
  output$metabanks_table <- renderDT({
    datatable(metabanks,
              class = 'custom-datatable',
              options = list(pageLength = 500, scrollX = TRUE),
              rownames = FALSE)
  })
  
  output$other_banks_table <- renderDT({
    datatable(other_banks,
              class = 'custom-datatable',
              options = list(pageLength = 500, scrollX = TRUE),
              rownames = FALSE)
  })
  
  output$final_matches_table <- renderDT({
    datatable(final_matches,
              class = 'custom-datatable',
              options = list(pageLength = 500, scrollX = TRUE),
              rownames = FALSE)
  })
  
  output$all_items_table <- renderDT({
    datatable(all_items,
              class = 'custom-datatable',
              options = list(pageLength = 500, scrollX = TRUE),
              rownames = FALSE)
  })
  
  output$common_exposures_table <- renderDT({
    datatable(common_exposures,
              class = 'custom-datatable',
              options = list(pageLength = 500, scrollX = TRUE),
              rownames = FALSE)
  })
  
  output$footnotes_table <- renderDT({
    datatable(footnotes,
              class = 'custom-datatable',
              options = list(pageLength = 500, scrollX = TRUE),
              rownames = FALSE)
  })
  
  output$st_dict_table <- renderDT({
    datatable(st_dict,
              class = 'custom-datatable',
              options = list(pageLength = 500, scrollX = TRUE),
              rownames = FALSE)
  })
  
  output$tr_dict_table <- renderDT({
    datatable(tr_dict,
              class = 'custom-datatable',
              options = list(pageLength = 500, scrollX = TRUE),
              rownames = FALSE)
  })
  
  output$ssm_dict_table <- renderDT({
    datatable(ssm_dict,
              class = 'custom-datatable',
              options = list(pageLength = 500, scrollX = TRUE),
              rownames = FALSE)
  })
  
  # Individual metadata table download handlers
  output$downloadMetaBanks <- downloadHandler(
    filename = "meta_banks.csv",
    content = function(file) {
      write.csv(metabanks, file, row.names = FALSE)
    }
  )
  
  output$downloadOtherBanks <- downloadHandler(
    filename = "other_banks.csv",
    content = function(file) {
      write.csv(other_banks, file, row.names = FALSE)
    }
  )
  
  output$downloadFinalMatches <- downloadHandler(
    filename = "final_matches.csv",
    content = function(file) {
      write.csv(final_matches, file, row.names = FALSE)
    }
  )
  
  output$downloadAllItems <- downloadHandler(
    filename = "all_items.csv",
    content = function(file) {
      write.csv(all_items, file, row.names = FALSE)
    }
  )
  
  output$downloadCommonExposures <- downloadHandler(
    filename = "common_exposures.csv",
    content = function(file) {
      write.csv(common_exposures, file, row.names = FALSE)
    }
  )
  
  output$downloadFootnotes <- downloadHandler(
    filename = "footnotes.csv",
    content = function(file) {
      write.csv(footnotes, file, row.names = FALSE)
    }
  )
  
  output$downloadSTDict <- downloadHandler(
    filename = "st_dictionary.csv",
    content = function(file) {
      write.csv(st_dict, file, row.names = FALSE)
    }
  )
  
  output$downloadTRDict <- downloadHandler(
    filename = "tr_dictionary.csv",
    content = function(file) {
      write.csv(tr_dict, file, row.names = FALSE)
    }
  )
  
  output$downloadSSMDict <- downloadHandler(
    filename = "ssm_dictionary.csv",
    content = function(file) {
      write.csv(ssm_dict, file, row.names = FALSE)
    }
  )
  
  # Download handler for total metadata
  output$downloadTotalMetadata <- downloadHandler(
    filename = "Total_Metadata_App.xlsx",
    content = function(file) {
      file.copy("Meta/Total_Metadata_App.xlsx", file, overwrite = TRUE)
    }
  )
  
  # Load all required datasets - wrap these in reactive expressions
  available_items_ratios <- c("ITM_119", "ITM_120", "ITM_121", "ITM_130", "ITM_CETFL", 
                              "ITM_NII", "ITM_TFL", "ITM_TOTFL", "ITM_54", "ITM_67", 
                              "ITM_50", "ITM_46", "ITM_4")

  # Lazily enable chart datasets when the visualisation area is first opened.
  observeEvent(input$`main-tabs`, {
    if(input$`main-tabs` == "Visualisation" && !chart_data_loaded()) {
      showModal(modalDialog(
        title = "Loading Visualisation Data",
        "Preparing chart data sources...",
        footer = NULL,
        easyClose = FALSE
      ))
      
      tryCatch({
        # DuckDB connection is already established in Global.R
        chart_data_loaded(TRUE)
        removeModal()
      }, error = function(e) {
        removeModal()
        showModal(modalDialog(
          title = "Error",
          paste("Failed to load chart database:", e$message),
          footer = modalButton("Close")
        ))
      })
    }
  }, priority = 100)
  
  tr_ratios <- reactive({
    data <- load_enabled_chart_dataset(
      "tr_ratios",
      c("Framework", "ISO2", "Exercise", "Period", "rel_period", "Common_Item",
        "Bank_ID", "Name", "Country", "Scenario", "Amount")
    ) %>%
      filter(Common_Item %in% available_items_ratios) %>%
      left_join(labels, by = "Common_Item")
    
    return(data)
  })
  
  
  final_waterfall <- reactive({
    data <- load_enabled_chart_dataset(
      "final_waterfall",
      c("Exercise", "Country", "Name", "TR", "rel_period", "Scenario", "Items", "Amount")
    )
    
    return(data)
  })
  
  bank_exp_total <- reactive({
    data <- load_enabled_chart_dataset(
      "bank_exp_total",
      c("TP", "ISO2", "Bank_ID", "Name", "Period", "Exercise", "Portfolio",
        "Common_Exposure", "Country", "Framework", "Amount")
    ) %>%
      distinct() %>%
      pivot_wider(names_from = Framework, values_from = Amount) %>%
      arrange(Bank_ID, ISO2, Period, Country, Common_Exposure) %>%
      mutate(Amount = coalesce(TR, ST), Framework = "TR") %>%
      dplyr::select(-TR, -ST) %>%
      distinct() %>%
      filter(Country != 0) %>%
      group_by(TP, ISO2, Bank_ID, Period, Exercise, Portfolio, Common_Exposure, Country) %>%
      mutate(Amount = sum(Amount, na.rm = TRUE)) %>%
      ungroup() %>%
      mutate(Common_Item = "ITM_SECEXP") %>%
      distinct() %>%
      mutate(Country = factor(Country, as.vector(na.omit(metadata_countries$Label_Country_Final)), 
                              as.vector(na.omit(metadata_countries$Value_Country_Final)))) %>%
      dplyr::select(ISO2, Bank_ID, Name, Period, Country, Common_Exposure, Portfolio, Amount) %>%
      distinct()
    
    return(data)
  })
  
  sov_exp <- reactive({
    data <- load_enabled_chart_dataset(
      "sov_exp",
      c("ISO2", "Bank_ID", "Name", "Period", "Country", "Maturity", "Amount")
    )
    
    return(data)
  })
  
  bank_nace <- reactive({
    data <- load_enabled_chart_dataset("bank_nace")
    
    return(data)
  })
  
  tr_rwas <- reactive({
    data <- load_enabled_chart_dataset("tr_rwas")
    
    return(data)
  })
  
  tr_assets <- reactive({
    data <- load_enabled_chart_dataset("tr_assets")
    
    return(data)
  })
  
  
  # ============ TAB 1: TIME SERIES ============
  observe({
    
    req(tr_ratios())
    
    item_labels <- tr_ratios() %>%
      distinct(Label) %>%
      arrange(Label) %>%
      pull(Label)
    
    updateSelectInput(session, "vis_ts_item",
                      choices = item_labels,
                      selected = "Transitional Common Equity Tier 1 Capital Ratio (%)")
  })
  
  observe({
    req(tr_ratios())
    # Get countries with actual data (that have at least some scenario data)
    countries_with_data <- tr_ratios() %>%
      filter(!is.na(Amount)) %>%
      group_by(Country) %>%
      # Check if country has data for at least some scenarios
      filter(any(!is.na(Amount))) %>%
      ungroup() %>%
      distinct(Country) %>%
      arrange(Country) %>%
      pull(Country)
    
    updateSelectInput(session, "vis_ts_country",
                      choices = countries_with_data,
                      selected = "France")
  })
  
  observe({
    req(tr_ratios())
    req(input$vis_ts_country, input$vis_ts_item)
    
    lbls <- labels %>% filter(Label == input$vis_ts_item)
    
    # Get banks that have data for at least one scenario for this item and country
    # This prevents the error when trying to access non-existent Baseline/Adverse columns
    banks_with_data <- tr_ratios() %>%
      filter(
        Country == input$vis_ts_country, 
        Common_Item == lbls$Common_Item,
        !is.na(Name),
        !is.na(Amount)
      ) %>%
      # Group by bank and check if they have any non-NA amounts
      group_by(Name) %>%
      filter(any(!is.na(Amount))) %>%
      ungroup() %>%
      distinct(Name) %>%
      arrange(Name) %>%
      pull(Name)
    
    # Only include banks in the dropdown if they have data
    if(length(banks_with_data) > 0) {
      updateSelectInput(session, "vis_ts_bank",
                        choices = c("All Banks" = "ALL", banks_with_data),
                        selected = "ALL")
    } else {
      updateSelectInput(session, "vis_ts_bank",
                        choices = c("All Banks" = "ALL"),
                        selected = "ALL")
    }
  })
  
  
  ts_filtered_data <- reactive({
    req(input$vis_ts_item, input$vis_ts_country)
    req(tr_ratios())
    lbls <- labels %>% filter(Label == input$vis_ts_item)
    data <- tr_ratios() %>%
      filter(Label == input$vis_ts_item) %>%
      filter(Country == input$vis_ts_country)
    
    if(input$vis_ts_bank == "ALL") {
      data <- data %>% filter(is.na(Name))
    } else {
      data <- data %>% filter(Name == input$vis_ts_bank)
    }
    
    data <- data %>%
      mutate(Scenario = factor(Scenario, scenarnames, names(scenarnames)))
    
    if("Actual restated" %in% unique(data$Scenario)) {
      # Get all available scenarios in the data
      available_scenarios <- unique(data$Scenario)
      
      data <- data %>%
        pivot_wider(names_from = Scenario, values_from = Amount)
      
      # Only perform operations on columns that exist
      if("Actual restated" %in% names(data) && "Actual" %in% names(data)) {
        data <- data %>%
          mutate(Actual = ifelse(Period %in% c(201712, 202412) & !is.na(`Actual restated`), 
                                 `Actual restated`, Actual)) %>%
          mutate(Actual = ifelse(Period %in% c(201712, 202412) & Framework == "TR", NA, Actual))
      }
      
      data <- data %>%
        pivot_longer(-c(Framework, ISO2, Exercise, Period, rel_period, Common_Item, 
                        Bank_ID, Name, Country, Label, Annual, Measure, Ratio, Category, Order), 
                     names_to = "Scenario", values_to = "Amount")
    }
    
    scale <- 10^(floor(log10(median(data$Amount, na.rm = TRUE))) - 1)
    
    if(lbls$Ratio == 1) {
      data$Amount <- data$Amount * 100
      scale <- scale * 100
    }
    
    if(lbls$Annual == 1) {
      data <- data %>%
        mutate(Amount = case_when(
          str_extract(Period, "\\d{2}$") == "03" ~ Amount * 4,
          str_extract(Period, "\\d{2}$") == "06" ~ Amount * 2,
          str_extract(Period, "\\d{2}$") == "09" ~ Amount * 4 / 3,
          str_extract(Period, "\\d{2}$") == "12" ~ Amount * 1,
          TRUE ~ Amount
        ))
    }
    
    data <- data %>%
      filter(Scenario != "Actual restated") %>%
      mutate(Scenario = ifelse(is.na(Scenario), "Actual", as.character(Scenario))) %>%
      pivot_wider(names_from = "Scenario", values_from = "Amount")
    
    # Add missing scenario columns if they don't exist
    for(scenario_name in cnames) {
      if(!(scenario_name %in% names(data))) {
        data[[scenario_name]] <- NA
      }
    }
    
    data <- data %>%
      mutate(Actual_ST = ifelse(rel_period == "Realised" & !is.na(Actual), Actual, NA))
    
    # Only update Baseline and Adverse if they exist
    if("Baseline" %in% names(data)) {
      data <- data %>%
        mutate(Baseline = ifelse(rel_period == "Realised" & is.na(Baseline), Actual, Baseline))
    }
    if("Adverse" %in% names(data)) {
      data <- data %>%
        mutate(Adverse = ifelse(rel_period == "Realised" & is.na(Adverse), Actual, Adverse))
    }
    
    data <- data %>%
      mutate(Framrel = ifelse(!is.na(Actual), 1, NA)) %>%
      mutate(Framework = ifelse(!is.na(Actual_ST), "ST", Framework)) %>%
      mutate(Exercise = ifelse(Framework == "TR", paste0("TR", Exercise), Exercise)) %>%
      mutate(Group = ifelse(Framework == "ST", paste(Framework, Exercise), NA))
    
    
    list(data = data, lbls = lbls, scale = scale)
  })
  
  output$vis_ts_chart <- renderPlotly({
    result <- ts_filtered_data()
    data <- result$data
    lbls <- result$lbls
    scale <- result$scale
    
    subtitle_parts <- c(input$vis_ts_country)
    if(input$vis_ts_bank != "ALL") {
      subtitle_parts <- c(subtitle_parts, input$vis_ts_bank)
    }
    bank_name <- paste(subtitle_parts, collapse = " - ")
    
    chart <- data %>%
      mutate(Date = as.Date(paste0(Period, "01"), format = "%Y%m%d")) %>%
      ggplot(aes(x = Date)) +
      geom_line(aes(y = Baseline, colour = "Baseline", group = Group)) +
      geom_line(aes(y = Adverse, colour = "Adverse", group = Group)) +
      geom_point(aes(y = Baseline, colour = "Baseline", 
                     text = paste("Period:", year(Date), "<br>Amount:", 
                                  paste0(format(round(Baseline, 2), big.mark = ","), lbls$Measure)))) +
      geom_point(aes(y = Adverse, colour = "Adverse", 
                     text = paste("Period:", year(Date), "<br>Amount:", 
                                  paste0(format(round(Adverse, 2), big.mark = ","), lbls$Measure)))) +
      geom_point(aes(y = Actual, colour = "Realised", 
                     text = paste("Period:", as.yearqtr(Date), "<br>Amount:", 
                                  paste0(format(round(Actual, 2), big.mark = ","), lbls$Measure)))) +
      geom_line(aes(y = Actual, colour = "Realised", group = Framrel), linetype = "dotdash") +
      geom_point(aes(y = Actual_ST, 
                     text = paste("Period:", as.yearqtr(Date), "<br>Amount:", 
                                  paste0(format(round(Actual_ST, 2), big.mark = ","), lbls$Measure))), 
                 colour = "#642FAC") +
      labs(x = "Date", y = lbls$Label, title = lbls$Label, subtitle = bank_name) +
      theme_classic() +
      theme(axis.text.x = element_text(angle = 45, vjust = 1, hjust = 1),
            axis.title.x = element_text(vjust = 1),
            text = element_text(size = 9),
            legend.position = "bottom") +
      scale_colour_manual("Scenario",
                          breaks = c("Realised", "Sovereign Shock", "Baseline", "Adverse"),
                          values = c("#7e878e", "#D2E288", "#007DC5", "#D12E7C")) +
      geom_text(aes(y = Actual_ST, label = Exercise),
                nudge_y = 0.2 * scale, hjust = .7, check_overlap = TRUE, size = 3)
    
    if(sum(data$`Adverse Sovereign Shock`, na.rm = TRUE) > 0) {
      chart <- chart +
        geom_point(aes(y = `Adverse Sovereign Shock`, colour = "Sovereign Shock", 
                       text = paste("Period:", year(Date), "<br>Amount:", 
                                    paste0(format(round(`Adverse Sovereign Shock`, 2), big.mark = ","), lbls$Measure))))
    }
    
    if(lbls$Common_Item %in% c("ITM_119", "ITM_CETFL")) {
      chart <- chart +
        geom_hline(yintercept = c(5, 6, 5.5, 8), linewidth = .5, linetype = "dotted", colour = "#7e878e") +
        annotate("text", x = as.Date("2025-12-01"), y = c(5.85, 4.85, 5.35, 7.85),
                 label = c("2010 Pass/Fail Threshold", "2011 Pass/Fail Threshold",
                           "2014 Pass/Fail Threshold (Adverse)", "2014 Pass/Fail Threshold (Baseline)"),
                 color = "#7e878e", size = 3, hjust = 1)
    }
    
    ggplotly(chart, tooltip = "text")
  })
  
  # ============ TAB 2: DENSITY ============
  observe({
    req(tr_ratios())
    item_labels <- tr_ratios() %>%
      distinct(Label) %>%
      arrange(Label) %>%
      pull(Label)
    
    updateSelectInput(session, "vis_dens_item",
                      choices = item_labels,
                      selected = "Transitional Common Equity Tier 1 Capital Ratio (%)")
  })
  
  
  
  
  dens_filtered_data <- reactive({
    req(input$vis_dens_item, input$vis_dens_period)
    req(tr_ratios())
    lbls <- labels %>% filter(Label == input$vis_dens_item)
    
    data <- tr_ratios() %>%
      filter(Common_Item == lbls$Common_Item) %>%
      filter(!is.na(Bank_ID)) %>%
      filter(Framework == "ST")
    
    data <- data %>%
      mutate(Scenario = factor(Scenario, scenarnames, names(scenarnames)))
    
    if("Actual restated" %in% unique(data$Scenario)) {
      # Specify all columns to keep explicitly
      keep_cols <- c("Framework", "ISO2", "Exercise", "Period", "rel_period", 
                     "Common_Item", "Bank_ID", "Name", "Country", "Label", 
                     "Annual", "Measure", "Ratio", "Category", "Order")
      
      data <- data %>%
        pivot_wider(names_from = Scenario, values_from = Amount) %>%
        mutate(Actual = ifelse(Period %in% c(201712, 202412) & !is.na(`Actual restated`), 
                               `Actual restated`, Actual)) %>%
        mutate(Actual = ifelse(Period %in% c(201712, 202412) & Framework == "TR", NA, Actual)) %>%
        pivot_longer(cols = -all_of(keep_cols), names_to = "Scenario", values_to = "Amount")
    }
    
    data_1011 <- data %>%
      filter(Exercise %in% c(2010, 2011) & rel_period == "Year2") %>%
      mutate(rel_period = "Year3")
    
    data <- rbind(data, data_1011) %>%
      filter(Scenario %in% c("Baseline", "Adverse") & rel_period == input$vis_dens_period)
    
    scale <- 10^(floor(log10(median(data$Amount, na.rm = TRUE))) - 1)
    
    if(lbls$Ratio == 1) {
      data$Amount <- data$Amount * 100
      scale <- scale * 100
    }
    
    if(lbls$Annual == 1) {
      data <- data %>%
        mutate(Amount = case_when(
          str_extract(Period, "\\d{2}$") == "03" ~ Amount * 4,
          str_extract(Period, "\\d{2}$") == "06" ~ Amount * 2,
          str_extract(Period, "\\d{2}$") == "09" ~ Amount * 4 / 3,
          str_extract(Period, "\\d{2}$") == "12" ~ Amount * 1,
          TRUE ~ Amount
        ))
    }
    
    # Create note
    note <- if(input$vis_dens_period == "Year3") {
      paste("Total", lbls$Label, "variation after two years for 2010 and 2011 exercises, then three years. Restated values for 2018 and 2025 exercises.")
    } else {
      "Restated values for 2018 and 2025 exercises."
    }
    
    relp_label <- paste("Year", str_extract(input$vis_dens_period, "\\d{1}$"))
    
    list(data = data, lbls = lbls, note = note, relp_label = relp_label)
  })
  
  output$vis_density_plot <- renderPlotly({
    result <- dens_filtered_data()
    data <- result$data
    lbls <- result$lbls
    note <- result$note
    relp_label <- result$relp_label
    
    dens_plot <- data %>%
      ggplot() +
      geom_density_ridges(aes(x = Amount, y = as.character(Exercise), fill = Scenario), 
                          alpha = .6) +
      geom_vline(xintercept = 0, linetype = "dashed", colour = "#7e878e") +
      theme_classic() +
      labs(
        x = lbls$Label,
        y = "Exercise Year",
        title = paste("Density Total", lbls$Label, paste0("(", relp_label, ")")),
        caption = note
      ) +
      theme(
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
    
    ggplotly(dens_plot, tooltip = "text")
  })
  
  # ============ TAB 3: WATERFALL ============
  observe({
    req(final_waterfall())
    exercises <- final_waterfall() %>%
      distinct(Exercise) %>%
      arrange(desc(Exercise)) %>%
      pull(Exercise)
    
    updateSelectInput(session, "vis_wf_exercise",
                      choices = exercises,
                      selected = exercises[1])
  })
  
  observe({
    req(input$vis_wf_exercise)
    
    if(input$vis_wf_exercise == 2010) {
      updateSelectInput(session, "vis_wf_period",
                        choices = c("Year 2" = "Year2"),
                        selected = "Year2")
    } else if(input$vis_wf_exercise == 2011) {
      updateSelectInput(session, "vis_wf_period",
                        choices = c("Year 1" = "Year1", "Year 2" = "Year2"),
                        selected = "Year2")
    } else {
      updateSelectInput(session, "vis_wf_period",
                        choices = c("Year 1" = "Year1", "Year 2" = "Year2", "Year 3" = "Year3"),
                        selected = "Year3")
    }
  })
  
  observe({
    req(input$vis_wf_exercise)
    req(final_waterfall())
    
    countries <- final_waterfall() %>%
      filter(Exercise == input$vis_wf_exercise) %>%
      filter(Country != "Total") %>%
      distinct(Country) %>%
      arrange(Country) %>%
      pull(Country)
    
    updateSelectInput(session, "vis_wf_country",
                      choices = c("All Countries" = "ALL", countries),
                      selected = "ALL")
  })
  
  observe({
    req(final_waterfall())
    req(input$vis_wf_exercise, input$vis_wf_country)
    
    if(input$vis_wf_country == "ALL") {
      updateSelectInput(session, "vis_wf_bank",
                        choices = c("All Banks" = "ALL"),
                        selected = "ALL")
    } else {
      banks_in_country <- final_waterfall() %>%
        filter(Exercise == input$vis_wf_exercise, Country == input$vis_wf_country) %>%
        filter(!is.na(Name), Name != "Total") %>%
        distinct(Name) %>%
        arrange(Name) %>%
        pull(Name)
      
      updateSelectInput(session, "vis_wf_bank",
                        choices = c("All Banks" = "ALL", banks_in_country),
                        selected = "ALL")
    }
  })
  
  wf_filtered_data <- reactive({
    req(input$vis_wf_exercise, input$vis_wf_period, input$vis_wf_scenario, input$vis_wf_country, input$vis_wf_bank)
    req(final_waterfall())
    
    transitional <- as.numeric(input$vis_wf_transitional)
    data <- final_waterfall()
    fact <- 1
    
    # Define dimensions based on transitional setting
    if(transitional == 1) {
      dimensions <- list(
        "I1", "I2", "I3", "I4", "I4.1", "I4.2", "I4.3",
        "I5", "I6", "I7", "I8", "I9", "I10", "I11"
      )
      
      names(dimensions) <- c(
        "CET1 Capital Ratio - Transitional", "Restatement", "CET1 Capital Ratio - Transitional - Restated",
        "Net Profit and Losses", "Of Which Net Interest Income", "Of Which NFCI",
        "Of Which Other Income/Expenses", "Credit Risk Losses", "Market Risk Losses",
        "Other Comprehensive Income", "Dividends",
        "Increase in REAs", "Other Items", "CET1 Capital Ratio - Transitional - End"
      )
    } else {
      dimensions <- list(
        "J1.1", "J1.1.1", "J1.2", "J1.2.1", "J1.3", "J1.3.1", "J1.4", "J4",
        "J4.1", "J4.2", "J4.3", "J5", "J6", "J7", "J8", "J9", "J10", "J11",
        "J11.1", "J12"
      )
      
      names(dimensions) <- c(
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
    
    # Filter by bank or country
    if(input$vis_wf_country == "ALL" && input$vis_wf_bank == "ALL") {
      data <- data %>% filter(Country == "Total", Name == "Total")
    } else if(input$vis_wf_country != "ALL" && input$vis_wf_bank == "ALL") {
      data <- data %>% filter(Country == input$vis_wf_country, Name == "Total")
    } else {
      data <- data %>% filter(Country == input$vis_wf_country, Name == input$vis_wf_bank)
    }
    
    # Process data based on transitional setting
    if(transitional == 1) {
      if (input$vis_wf_exercise %in% c(2018, 2025)) {
        dt <- data %>%
          filter(
            TR == transitional,
            Exercise == input$vis_wf_exercise,
            rel_period %in% c("Realised", str_remove_all(input$vis_wf_period, "\\s{0,}")),
            Scenario %in% c("Actual", input$vis_wf_scenario)
          ) %>%
          mutate(Amount_out = ifelse(!(Items %in% c("I3", "I4.1", "I4.2", "I4.3")), Amount, 0)) %>%
          mutate(Amount_in = ifelse(Items %in% c("I3", "I4.1", "I4.2", "I4.3"), Amount, 0)) %>%
          mutate(end = ifelse(!(Items %in% c("I11")), cumsum(Amount_out), 0)) %>%
          mutate(end = ifelse(Items %in% c("I4.1", "I4.2", "I4.3"), cumsum(Amount_in), end)) %>%
          mutate(start = ifelse(!Items %in% c("I1", "I3"), dplyr::lag(end, 1), 0)) %>%
          mutate(start = ifelse(Items %in% c("I4.1"), dplyr::lag(start, 1), start)) %>%
          mutate(id = as.numeric(rownames(.))) %>%
          mutate(sign = ifelse(Items %in% c("I1", "I3", "I11"), "Total",
                               ifelse(sign(Amount) >= 0, "Increase", "Decrease"))) %>%
          mutate(ofwhich = ifelse(str_detect(Items, "4\\."), 1, 0)) %>%
          arrange(desc(id)) %>%
          mutate(stand = ifelse(sign(Amount) >= 0, end + fact, start + fact)) %>%
          mutate(stand = ifelse(Items == "I11", Amount + fact, stand)) %>%
          mutate(Items = factor(Items, rev(dimensions), labels = rev(names(dimensions))))
      } else {
        dt <- data %>%
          filter(
            TR == transitional,
            Exercise == input$vis_wf_exercise,
            rel_period %in% c("Realised", str_remove_all(input$vis_wf_period, "\\s{0,}")),
            Scenario %in% c("Actual", input$vis_wf_scenario)
          ) %>%
          mutate(Amount_out = ifelse(!(Items %in% c("I3", "I4.1", "I4.2", "I4.3")), Amount, 0)) %>%
          mutate(Amount_in = ifelse(Items %in% c("I3", "I4.1", "I4.2", "I4.3"), Amount, 0)) %>%
          mutate(end = ifelse(!(Items %in% c("I11")), cumsum(Amount_out), 0)) %>%
          mutate(end = ifelse(Items %in% c("I4.1", "I4.2", "I4.3"), cumsum(Amount_in), end)) %>%
          mutate(start = ifelse(!Items %in% c("I1", "I3"), dplyr::lag(end, 1), 0)) %>%
          mutate(start = ifelse(Items %in% c("I4.1"), dplyr::lag(start, 1), start)) %>%
          mutate(id = as.numeric(rownames(.))) %>%
          mutate(sign = ifelse(Items %in% c("I1", "I3", "I11"), "Total",
                               ifelse(sign(Amount) >= 0, "Increase", "Decrease"))) %>%
          mutate(ofwhich = ifelse(str_detect(Items, "4\\."), 1, 0)) %>%
          arrange(desc(id)) %>%
          mutate(stand = ifelse(sign(Amount) >= 0, end + fact, start + fact)) %>%
          mutate(stand = ifelse(Items == "I11", Amount + fact, stand)) %>%
          mutate(Amount = ifelse(!(input$vis_wf_exercise %in% c(2018, 2025)) & Items %in% c("I2", "I3"), 
                                 NA, Amount)) %>%
          filter(!is.na(Amount)) %>%
          mutate(Items = factor(Items, rev(dimensions[-c(2, 3)]), 
                                labels = rev(names(dimensions)[-c(2, 3)]))) %>%
          mutate(id = rev(as.numeric(rownames(.))))
      }
    } else {
      # Fully loaded calculations
      if (input$vis_wf_exercise %in% c(2018, 2025)) {
        dt <- data %>%
          filter(
            TR == transitional,
            Exercise == input$vis_wf_exercise,
            rel_period %in% c("Realised", str_remove_all(input$vis_wf_period, "\\s{0,}")),
            Scenario %in% c("Actual", input$vis_wf_scenario)
          ) %>%
          mutate(Amount_out = ifelse(!(Items %in% c("J1.2", "J1.3", "J1.4", "J4.1", 
                                                    "J4.2", "J4.3", "J11", "J12")), Amount, 0)) %>%
          mutate(Amount_in = ifelse(Items %in% c("J1.4", "J4.1", "J4.2", "J4.3"), Amount, 0)) %>%
          mutate(end = ifelse(!(Items %in% c("J11", "J12")), cumsum(Amount_out), 0)) %>%
          mutate(end = ifelse(Items %in% c("J4.1", "J4.2", "J4.3"), cumsum(Amount_in), end)) %>%
          mutate(start = ifelse(!Items %in% c("J1.1", "J1.2", "J1.3", "J1.4"), 
                                dplyr::lag(end, 1), 0)) %>%
          mutate(start = ifelse(Items %in% c("J4.1"), dplyr::lag(start, 1), start)) %>%
          mutate(start = ifelse(Items %in% c("J11.1"), dplyr::lag(start, 1), start)) %>%
          mutate(id = as.numeric(rownames(.))) %>%
          mutate(sign = ifelse(Items %in% c("J1.1", "J1.2", "J1.3", "J1.4", "J11", "J12"), "Total",
                               ifelse(sign(Amount) >= 0, "Increase", "Decrease"))) %>%
          mutate(ofwhich = ifelse(str_detect(Items, "4\\."), 1, 0)) %>%
          arrange(desc(id)) %>%
          mutate(stand = ifelse(sign(Amount) >= 0, end + fact, start + fact)) %>%
          mutate(stand = ifelse(Items %in% c("J11", "J12"), Amount + fact, stand)) %>%
          mutate(Items = factor(Items, rev(dimensions), labels = rev(names(dimensions)))) %>%
          mutate(alpha = ifelse(str_detect(Items, "Of Which"), 1, 0))
      } else {
        dt <- data %>%
          filter(
            TR == transitional,
            Exercise == input$vis_wf_exercise,
            rel_period %in% c("Realised", str_remove_all(input$vis_wf_period, "\\s{0,}")),
            Scenario %in% c("Actual", input$vis_wf_scenario)
          ) %>%
          mutate(Amount_out = ifelse(!(Items %in% c("J1.2", "J1.3", "J1.4", "J4.1", 
                                                    "J4.2", "J4.3", "J11", "J12")), Amount, 0)) %>%
          mutate(Amount_in = ifelse(Items %in% c("J1.4", "J4.1", "J4.2", "J4.3"), Amount, 0)) %>%
          mutate(end = ifelse(!(Items %in% c("J11", "J12")), cumsum(Amount_out), 0)) %>%
          mutate(end = ifelse(Items %in% c("J4.1", "J4.2", "J4.3"), cumsum(Amount_in), end)) %>%
          mutate(start = ifelse(!Items %in% c("J1.1", "J1.2", "J1.3", "J1.4"), 
                                dplyr::lag(end, 1), 0)) %>%
          mutate(start = ifelse(Items %in% c("J4.1"), dplyr::lag(start, 1), start)) %>%
          mutate(start = ifelse(Items %in% c("J11.1"), dplyr::lag(start, 1), start)) %>%
          mutate(id = as.numeric(rownames(.))) %>%
          mutate(sign = ifelse(Items %in% c("J1.1", "J1.2", "J1.3", "J1.4", "J11", "J12"), "Total",
                               ifelse(sign(Amount) >= 0, "Increase", "Decrease"))) %>%
          mutate(ofwhich = ifelse(str_detect(Items, "4\\."), 1, 0)) %>%
          arrange(desc(id)) %>%
          mutate(stand = ifelse(sign(Amount) >= 0, end + fact, start + fact)) %>%
          mutate(stand = ifelse(Items %in% c("J11", "J12"), Amount + fact, stand)) %>%
          mutate(Amount = ifelse(!(input$vis_wf_exercise %in% c(2018, 2025)) & 
                                   Items %in% c("J1.1", "J1.1.1", "J1.2.1", "J1.3"), NA, Amount)) %>%
          filter(!is.na(Amount)) %>%
          mutate(Items = factor(Items, rev(dimensions[-c(1, 2, 4, 5)]), 
                                labels = rev(names(dimensions)[-c(1, 2, 4, 5)]))) %>%
          mutate(Items = str_remove_all(Items, "\\s{0,}-\\s{0,}Restated")) %>%
          mutate(Items = factor(Items, Items)) %>%
          mutate(id = rev(as.numeric(rownames(.)))) %>%
          mutate(alpha = ifelse(str_detect(Items, "Of Which"), 1, 0))
      }
    }
    
    validate(
      need(nrow(dt) > 0, "No data available for selected combination.")
    )
    
    list(data = dt)
  })
  
  output$vis_waterfall_chart <- renderPlotly({
    result <- wf_filtered_data()
    dt <- result$data
    
    bank_name <- if(input$vis_wf_bank == "ALL") {
      if(input$vis_wf_country == "ALL") "All Countries - All Banks" 
      else paste(input$vis_wf_country, "- All Banks")
    } else {
      paste(input$vis_wf_country, "-", input$vis_wf_bank)
    }
    
    capital_type <- if(input$vis_wf_transitional == "1") "Transitional" else "Fully Loaded"
    
    palette <- c("Total" = "#204980", "Increase" = "#D2E288", "Decrease" = "#9e202a")
    fact <- 1
    
    minx <- dt %>% mutate(id = abs(id - (max(id) + 1)))
    minx <- min(minx[minx$ofwhich == 1, "id"], na.rm = TRUE)
    if(is.infinite(minx)) minx <- 0
    
    chart <- dt %>%
      mutate(id = abs(id - (max(id) + 1))) %>%
      mutate(across(c(start, end, id), ~ ifelse(ofwhich == 1, 0, .), .names = "{.col}1")) %>%
      mutate(across(c(start, end, id), ~ ifelse(ofwhich != 1, 0, .), .names = "{.col}2")) %>%
      ggplot(aes(x = Items)) +
      annotate("rect", xmin = minx - .45, xmax = minx + 2.45, ymin = 0, 
               ymax = max(dt$end, na.rm = TRUE), alpha = .5, fill = "#f6f6f6") +
      geom_rect(aes(xmin = id1 - .45, xmax = id1 + .45, ymin = end1, ymax = start1,
                    fill = factor(sign, c("Total", "Increase", "Decrease")))) +
      geom_rect(aes(xmin = id2 - .45, xmax = id2 + .45, ymin = end2, ymax = start2,
                    fill = factor(sign, c("Total", "Increase", "Decrease"))), 
                alpha = 0.6, show.legend = FALSE) +
      geom_hline(yintercept = 0, linetype = "dotted", colour = "#7e878e") +
      geom_text(aes(x = id, y = stand, label = round(Amount, 2)), size = 3) +
      coord_flip() +
      theme_classic() +
      theme(
        axis.title.x = element_text(vjust = 1),
        text = element_text(size = 12),
        legend.position = "bottom",
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        strip.background = element_blank(),
        legend.title = element_blank()
      ) +
      labs(
        x = "", y = "% Starting REAs",
        title = paste("Waterfall Chart -", capital_type, "CET1 Ratio"),
        subtitle = paste(bank_name, "-", input$vis_wf_period, "-", input$vis_wf_scenario, "-", input$vis_wf_exercise),
        caption = "REAs: Risk exposure amounts.") +
      scale_fill_manual("", values = palette) +
      guides(
        fill = guide_legend(title.position = "top", order = 1, override.aes = list(alpha = 1, colour = NA))
      )      
    
    p <- ggplotly(chart, tooltip = FALSE)   
    p <- style(p, hoverinfo = "skip", traces = 1)
    
    p
  })
  
  # ============ TAB 4: NETWORK ============

  output$network_action_controls <- renderUI({
    req(input$vis_net_exp_type)

    if (identical(input$vis_net_exp_type, "risk")) {
      div(
        style = "display: flex; justify-content: center; align-items: flex-start; gap: 20px;",
        div(
          class = "button-container",
          tags$p(style = "font-weight: bold;", "Load Network"),
          actionButton("load_network", "Load Network", class = "btn btn-default")
        ),
        div(
          class = "button-container",
          tags$p(style = "font-weight: bold;", "Center Network"),
          actionButton("center_network", icon("crosshairs", style = "color: #BF9F66; font-size: 2.0em;"), class = "circle-button")
        )
      )
    } else {
      tagList(
        div(style = "height: 20px;"),
        div(
          class = "button-container",
          tags$p(style = "font-weight: bold;", "Load Network"),
          actionButton("load_network", "Load Network", class = "btn btn-default")
        ),
        div(
          class = "button-container",
          tags$p(style = "font-weight: bold;", "Center Network"),
          actionButton("center_network", icon("crosshairs", style = "color: #BF9F66; font-size: 2.0em;"), class = "circle-button")
        )
      )
    }
  })
  
  network_chart_key <- reactive({
    if (identical(input$vis_net_exp_type, "risk")) {
      "bank_exp_total"
    } else {
      "sov_exp"
    }
  })

  observe({
    req(input$vis_net_exp_type)
    req(chart_data_loaded())
    req(chart_dataset_enabled(network_chart_key()))

    countries <- load_chart_distinct_values(network_chart_key(), "ISO2")
    countries <- sort(setdiff(as.character(countries), "Other"))

    updateSelectInput(session, "vis_net_country",
                      choices = c("All Countries" = "ALL", countries),
                      selected = "ALL")
  })

  observe({
    req(input$vis_net_exp_type, input$vis_net_country)
    req(chart_data_loaded())
    req(chart_dataset_enabled(network_chart_key()))

    if (identical(input$vis_net_country, "ALL")) {
      updateSelectInput(session, "vis_net_bank",
                        choices = c("All Banks (Aggregate)" = "ALL"),
                        selected = "ALL")
      return()
    }

    banks <- load_chart_distinct_values(
      network_chart_key(),
      "Name",
      build_network_where_clauses(country = input$vis_net_country)
    )

    updateSelectInput(session, "vis_net_bank",
                      choices = c("All Banks (Aggregate)" = "ALL", as.character(banks)),
                      selected = "ALL")
  })

  observe({
    req(input$vis_net_exp_type)

    if (input$vis_net_exp_type == "risk") {
      req(chart_data_loaded())
      req(chart_dataset_enabled("bank_exp_total"))

      exposure_values <- load_chart_distinct_values(
        "bank_exp_total",
        "Common_Exposure",
        build_network_where_clauses(country = input$vis_net_country, bank = input$vis_net_bank)
      )

      exposure_choices <- tibble(Common_Exposure = as.character(exposure_values)) %>%
        left_join(exposures_names %>% select(Common_Exposure, Exposure_Label),
                  by = "Common_Exposure") %>%
        mutate(Exposure_Label = coalesce(Exposure_Label, Common_Exposure)) %>%
        arrange(Common_Exposure)

      choices <- setNames(exposure_choices$Common_Exposure,
                          exposure_choices$Exposure_Label)

      updateSelectInput(session, "vis_net_exposure",
                        choices = choices,
                        selected = if (nrow(exposure_choices) > 0) exposure_choices$Common_Exposure[1] else NULL)
    }
  })

  observe({
    req(input$vis_net_exp_type)

    if (input$vis_net_exp_type == "risk") {
      req(input$vis_net_exposure, input$vis_net_portfolio)
      req(chart_data_loaded())
      req(chart_dataset_enabled("bank_exp_total"))

      periods <- load_chart_distinct_values(
        "bank_exp_total",
        "Period",
        build_network_where_clauses(
          country = input$vis_net_country,
          bank = input$vis_net_bank,
          exposure = input$vis_net_exposure,
          portfolio = input$vis_net_portfolio
        ),
        descending = TRUE
      )
    } else {
      req(input$vis_net_maturity)
      req(chart_data_loaded())
      req(chart_dataset_enabled("sov_exp"))

      periods <- load_chart_distinct_values(
        "sov_exp",
        "Period",
        build_network_where_clauses(
          country = input$vis_net_country,
          bank = input$vis_net_bank,
          maturity = input$vis_net_maturity
        ),
        descending = TRUE
      )
    }

    if (length(periods) == 0) {
      updateSelectInput(session, "vis_net_period", choices = character(0), selected = NULL)
      return()
    }

    period_choices <- setNames(periods, sapply(periods, convert_to_quarter))

    updateSelectInput(session, "vis_net_period",
                      choices = period_choices,
                      selected = periods[1])
  })

  network_selection <- eventReactive(input$load_network, {
    req(input$vis_net_period, input$vis_net_exp_type)

    if (identical(input$vis_net_exp_type, "risk")) {
      req(input$vis_net_portfolio, input$vis_net_exposure)
    } else {
      req(input$vis_net_maturity)
    }

    list(
      exp_type = input$vis_net_exp_type,
      country = input$vis_net_country,
      bank = input$vis_net_bank,
      period = input$vis_net_period,
      portfolio = input$vis_net_portfolio,
      exposure = input$vis_net_exposure,
      maturity = input$vis_net_maturity
    )
  })

  net_filtered_data <- reactive({
    selection <- network_selection()
    req(selection)

    if (selection$exp_type == "risk") {
      risk_columns <- c(
        "TP", "ISO2", "Bank_ID", "Name", "Period", "Exercise", "Portfolio",
        "Common_Exposure", "Country", "Framework", "Amount"
      )

      data <- load_enabled_chart_dataset_where(
        "bank_exp_total",
        risk_columns,
        build_network_where_clauses(
          country = selection$country,
          bank = selection$bank,
          exposure = selection$exposure,
          period = selection$period
        )
      ) %>%
        distinct() %>%
        pivot_wider(names_from = Framework, values_from = Amount) %>%
        {
          if (!"TR" %in% names(.)) {
            .$TR <- NA_real_
          }
          if (!"ST" %in% names(.)) {
            .$ST <- NA_real_
          }
          .
        } %>%
        arrange(Bank_ID, ISO2, Period, Country, Common_Exposure) %>%
        mutate(Amount = coalesce(TR, ST), Framework = "TR") %>%
        dplyr::select(-any_of(c("TR", "ST"))) %>%
        distinct() %>%
        filter(Country != 0) %>%
        mutate(Country = as.character(Country)) %>%
        left_join(
          metadata_countries %>%
            transmute(
              Country = as.character(Label_Country_Final),
              Country_Label = as.character(Value_Country_Final)
            ) %>%
            distinct(),
          by = "Country"
        ) %>%
        mutate(Country = coalesce(Country_Label, Country)) %>%
        dplyr::select(-Country_Label)

      if (selection$portfolio == "Total") {
        name_lookup <- data %>% distinct(ISO2, Bank_ID, Name)

        data <- data %>%
          mutate(Portfolio = 0) %>%
          group_by(ISO2, Bank_ID, Period, Portfolio, Common_Exposure, Country) %>%
          summarise(Amount = sum(Amount, na.rm = TRUE), .groups = "drop") %>%
          left_join(name_lookup, by = c("ISO2", "Bank_ID"))
      } else if (selection$portfolio == "IRB") {
        data <- data %>% filter(Portfolio == 2)
      } else {
        data <- data %>% filter(Portfolio == 1)
      }

      network_dta <- data %>%
        group_by(Period) %>%
        mutate(REA_all = sum(Amount, na.rm = TRUE)) %>%
        ungroup() %>%
        arrange(ISO2, Period, Country) %>%
        mutate(Share_all = Amount / REA_all) %>%
        filter(ISO2 != "Other") %>%
        filter(Share_all > 0)
    } else {
      sovereign_columns <- c("ISO2", "Bank_ID", "Name", "Period", "Country", "Maturity", "Amount")

      data <- load_enabled_chart_dataset_where(
        "sov_exp",
        sovereign_columns,
        build_network_where_clauses(
          country = selection$country,
          bank = selection$bank,
          maturity = selection$maturity,
          period = selection$period
        )
      )

      network_dta <- data %>%
        group_by(Period) %>%
        mutate(Exp_all = sum(Amount, na.rm = TRUE)) %>%
        ungroup() %>%
        arrange(ISO2, Bank_ID, Period, Country) %>%
        mutate(Share_all = Amount / Exp_all) %>%
        filter(ISO2 != "Other") %>%
        filter(Share_all > 0)
    }

    network_dta
  })

  output$vis_network <- renderVisNetwork({
    selection <- network_selection()
    validate(need(!is.null(selection), "Select filters and click Load Network."))

    data <- net_filtered_data() %>%
      filter(Period == selection$period)

    validate(need(nrow(data) > 0, "No network data available for the selected filters."))

    bank_stat <- c("Austria", "Belgium", "Bulgaria", "Cyprus", "Germany", "Denmark", "Estonia",
                   "Spain", "Finland", "France", "United Kingdom", "Greece", "Hungary", "Ireland",
                   "Iceland", "Italy", "Liechtenstein", "Lithuania", "Luxembourg", "Slovenia",
                   "Latvia", "Malta", "Netherlands", "Norway", "Poland", "Portugal", "Romania",
                   "Sweden", "Other")

    size_data <- data %>%
      mutate(Country = as.character(Country)) %>%
      select(Country, Share_all) %>%
      group_by(Country) %>%
      summarise(Share_all = sum(Share_all, na.rm = TRUE), .groups = "drop")

    structure <- unique(c(as.character(data$Name), as.character(data$Country)))
    size <- size_data %>%
      right_join(data.frame(Country = as.character(structure)), by = "Country")

    nodes <- size %>%
      mutate(id = Country) %>%
      rename(label = Country) %>%
      mutate(
        group = ifelse(id %in% bank_stat, "EBA Sample", "Non-EBA"),
        group = ifelse(is.na(Share_all), "Banks", group),
        Share_all = ifelse(is.na(Share_all), 0, Share_all),
        value = Share_all * 100,
        id = as.character(id),
        label = as.character(label),
        title = paste0("Share of Exposure: ", round(value, 2), "%"),
        title = ifelse(group == "Banks",
                       ifelse(selection$exp_type == "risk", "Issuer", "Holder"),
                       title)
      ) %>%
      select(id, label, group, value, title)

    edges <- data %>%
      mutate(
        from = Name,
        to = as.character(Country),
        value = Share_all * 10
      ) %>%
      filter(from != to) %>%
      select(from, to, value)

    sorted_nodes_ids <- nodes %>%
      filter(group != "Banks") %>%
      arrange(desc(value)) %>%
      pull(id)

    if (selection$exp_type == "risk") {
      exposure_label <- exposures_names %>%
        filter(Common_Exposure == selection$exposure) %>%
        pull(Exposure_Label) %>%
        first()

      if (length(exposure_label) == 0 || is.na(exposure_label)) {
        exposure_label <- selection$exposure
      }

      title <- paste(
        'Bank Risk Exposure Amounts -', exposure_label, '-', selection$portfolio, "Exposures -",
        convert_to_quarter(selection$period), "- Reported values"
      )
      caption <- "Size of dots relates to the share of exposures in the sample."
    } else {
      matur <- c(1, 2, 3, 4, 5, 6, 7, 8)
      names(matur) <- c("[0-3M]", "[3M-1Y]", "[1Y-2Y]", "[2Y-3Y]", "[3Y-5Y]", "[5Y-10Y]", "[10Y+]", "Total")

      title <- paste(
        'Bank General Government Exposures -', names(matur[matur == as.numeric(selection$maturity)]), 'Maturity -',
        convert_to_quarter(selection$period), "- Reported Values"
      )
      caption <- "Size of dots relates to the share of exposures in the sample. Pre-2016: Net Direct Long Exposures. 2016-2018: Financial Assets. 2019-: Direct Exposures on Balance Sheet."
    }

    visNetwork(nodes, edges, main = title, submain = list(text = paste0("\n", caption), style = "margin-top: 0px; margin-bottom: 5px; padding: 0px;")) %>%
      visNodes(size = "value", title = "title") %>%
      visEdges(
        smooth = list(enabled = TRUE, type = "continuous"),
        arrows = NULL,
        color = list(color = "#cccccc", highlight = "#7e878e"),
        font = list(family = "Lato")
      ) %>%
      visGroups(groupname = "EBA Sample", color = "#5EC5C2") %>%
      visGroups(groupname = "Non-EBA", color = "#0083A0") %>%
      visGroups(groupname = "Banks", color = "#BF9F66") %>%
      visPhysics(
        solver = "hierarchicalRepulsion",
        hierarchicalRepulsion = list(
          gravitationalConstant = -20000,
          springLength = 500,
          springConstant = 0.1,
          damping = 2
        )
      ) %>%
      visOptions(
        highlightNearest = list(enabled = TRUE, hover = TRUE),
        nodesIdSelection = list(enabled = TRUE, values = sorted_nodes_ids, main = "All Countries"),
        selectedBy = list(variable = "group", main = "All Groups")
      ) %>%
      visLayout(randomSeed = 123) %>%
      visLegend(useGroups = FALSE)
  })
  
  
  # ============ TAB 5: NACE ============
  observe({
    req(bank_nace())
    
    countries <- bank_nace() %>%
      distinct(ISO2) %>%
      arrange(ISO2) %>%
      pull(ISO2)
    
    updateSelectInput(session, "vis_nace_country",
                      choices = c("All Countries" = "ALL", countries),
                      selected = "ALL")
  })
  
  observe({
    req(bank_nace())
    req(input$vis_nace_country)
    
    if(input$vis_nace_country != "ALL") {
      banks <- bank_nace() %>%
        filter(ISO2 == input$vis_nace_country) %>%
        distinct(Name) %>%
        arrange(Name) %>%
        pull(Name)
      
      updateSelectInput(session, "vis_nace_bank",
                        choices = c("All Banks (Aggregate)" = "ALL", banks),
                        selected = "ALL")
    } else {
      updateSelectInput(session, "vis_nace_bank",
                        choices = c("All Banks (Aggregate)" = "ALL"),
                        selected = "ALL")
    }
  })
  
  observe({
    req(bank_nace())
    req(input$vis_nace_country, input$vis_nace_bank)
    
    periods <- NULL
    
    if(input$vis_nace_bank != "ALL") {
      periods <- bank_nace() %>%
        filter(Name == input$vis_nace_bank) %>%
        filter(if_any(starts_with("NACE_"), ~ !is.na(.) & . != 0)) %>%
        distinct(Period) %>%
        arrange(desc(Period)) %>%
        pull(Period)
    } else if(input$vis_nace_country != "ALL") {
      periods <- bank_nace() %>%
        filter(ISO2 == input$vis_nace_country) %>%
        filter(if_any(starts_with("NACE_"), ~ !is.na(.) & . != 0)) %>%
        distinct(Period) %>%
        arrange(desc(Period)) %>%
        pull(Period)
    } else {
      periods <- bank_nace() %>%
        filter(if_any(starts_with("NACE_"), ~ !is.na(.) & . != 0)) %>%
        distinct(Period) %>%
        arrange(desc(Period)) %>%
        pull(Period)
    }
    
    # Add validation for empty periods
    if(is.null(periods) || length(periods) == 0) {
      updateSelectInput(session, "vis_nace_period",
                        choices = character(0),
                        selected = NULL)
      return()
    }
    
    period_choices <- setNames(periods, sapply(periods, convert_to_quarter))
    
    updateSelectInput(session, "vis_nace_period",
                      choices = period_choices,
                      selected = periods[1])
  })
  
  
  nace_filtered_data <- reactive({
    req(input$vis_nace_period)
    req(bank_nace())
    
    data <- bank_nace()
    
    if(input$vis_nace_bank != "ALL") {
      chart_nace <- data %>% filter(Name == input$vis_nace_bank)
    } else if(input$vis_nace_country != "ALL") {
      chart_nace <- data %>%
        filter(ISO2 == input$vis_nace_country) %>%
        group_by(ISO2, Period, Perf_Status) %>%
        mutate(across(matches("NACE_"), ~sum(., na.rm = TRUE))) %>%
        ungroup() %>%
        mutate(Bank_ID = "Total", Name = "Total") %>%
        distinct()
    } else {
      chart_nace <- data %>%
        group_by(Period, Perf_Status) %>%
        mutate(across(matches("NACE_"), ~sum(., na.rm = TRUE))) %>%
        ungroup() %>%
        mutate(Bank_ID = "Total", ISO2 = "Total", Name = "Total") %>%
        distinct()
    }
    
    chart_nace <- chart_nace %>%
      mutate(across(matches("NACE_[1-9]"), ~ . / NACE_0, .names = "EXP_{.col}")) %>%
      arrange(ISO2, Bank_ID, Period, Perf_Status) %>%
      filter(Perf_Status %in% c(0, 2)) %>%
      group_by(Bank_ID, Period) %>%
      mutate(across(matches("^NACE_[0-9]"), ~ . / dplyr::lag(., 1), .names = "NPL_{.col}")) %>%
      ungroup() %>%
      pivot_longer(matches("NACE"), names_to = "NACE_Codes", values_to = "Amount") %>%
      filter(str_detect(NACE_Codes, "EXP|NPL")) %>%
      mutate(Common_Item = ifelse(str_detect(NACE_Codes, "EXP"), "ITM_EXP",
                                  ifelse(str_detect(NACE_Codes, "NPL"), "ITM_NPL", Common_Item))) %>%
      mutate(Amount = ifelse(Common_Item == "ITM_EXP" & Perf_Status != 0, NA,
                             ifelse(Common_Item == "ITM_NPL" & Perf_Status != 2, NA, Amount))) %>%
      filter(!is.na(Amount)) %>%
      mutate(Exposure = as.numeric(str_extract(NACE_Codes, "\\d{1,}"))) %>%
      arrange(ISO2, Period, Common_Item) %>%
      dplyr::select(where(~ !all(is.na(.x)))) %>%
      filter(Exposure > 0)
    
    chart_nace
  })
  
  output$vis_treemap <- renderPlotly({
    data <- nace_filtered_data()
    data <- data %>% filter(Period == input$vis_nace_period)
    
    validate(
      need(nrow(data) > 0, "No data available for selected combination.")
    )
    
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
    
    exp_data <- data %>%
      mutate(Exposure = factor(Exposure, naces, names(naces))) %>%
      mutate(Exposure = as.character(Exposure)) %>%
      filter(Common_Item == "ITM_EXP") %>%
      mutate(
        labels = Exposure,
        parents = "",
        values = Amount * 100,
        display_text = paste(Exposure, paste0(round(Amount * 100, 2), "%"), sep = " - "),
        color_val = Amount * 100,
        item_type = "EXP"
      )
    
    npl_data <- data %>%
      mutate(Exposure = factor(Exposure, naces, names(naces))) %>%
      mutate(Exposure = as.character(Exposure)) %>%
      filter(Common_Item == "ITM_NPL") %>%
      mutate(
        labels = paste0(Exposure, "_NPL"),
        parents = Exposure,
        values = Amount * 100,
        display_text = paste("(of which) NPL", paste0(round(Amount * 100, 2), "%"), sep = " - "),
        color_val = -1,
        item_type = "NPL"
      )
    
    plot_data <- bind_rows(exp_data, npl_data) %>%
      mutate(ids = labels, labels = display_text)
    
    title_text <- if(input$vis_nace_bank != "ALL") {
      paste(input$vis_nace_country, "-", input$vis_nace_bank, "-", convert_to_quarter(input$vis_nace_period))
    } else if(input$vis_nace_country != "ALL") {
      paste(input$vis_nace_country, "- All Banks -", convert_to_quarter(input$vis_nace_period))
    } else {
      paste("All Countries - All Banks -", convert_to_quarter(input$vis_nace_period))
    }
    
    plot_ly(
      data = plot_data,
      type = "treemap",
      ids = ~ids,
      labels = ~labels,
      parents = ~parents,
      values = ~values,
      textposition = "top left",
      textfont = list(color = "white"),
      hovertemplate = paste0("%{label}<br><extra></extra>"),
      marker = list(
        colors = ~ifelse(color_val == -1, "rgba(128, 128, 128, 0.6)", color_val),
        colorscale = list(
          c(0, "#5EC5C2"), c(0.05, "#4471B9"), c(0.1, "#0FA1F5"),
          c(0.15, "#1858E1"), c(0.2, "#51438F"), c(1, "#18AFC7")
        ),
        line = list(color = "#FFFFFF", width = 2)
      ),
      branchvalues = "remainder"
    ) %>%
      layout(title = list(text = title_text, font = list(size = 16)))
  })
  
  
  # ============ TAB 6: BALANCE SHEET ============
  observe({
    req(tr_assets())
    
    countries <- tr_assets() %>%
      distinct(Country) %>%
      arrange(Country) %>%
      pull(Country)
    
    updateSelectInput(session, "vis_bs_country",
                      choices = c("All Countries" = "ALL", countries),
                      selected = "ALL")
  })
  
  observe({
    req(tr_assets())
    
    req(input$vis_bs_country)
    
    if(input$vis_bs_country != "ALL") {
      banks <- tr_assets() %>%
        filter(Country == input$vis_bs_country) %>%
        distinct(Name) %>%
        arrange(Name) %>%
        pull(Name)
      
      updateSelectInput(session, "vis_bs_bank",
                        choices = c("All Banks (Aggregate)" = "ALL", banks),
                        selected = "ALL")
    } else {
      updateSelectInput(session, "vis_bs_bank",
                        choices = c("All Banks (Aggregate)" = "ALL"),
                        selected = "ALL")
    }
  })
  
  bs_filtered_data <- reactive({
    
    req(tr_rwas())
    req(tr_assets())
    
    
    rwas_data <- tr_rwas()
    assets_data <- tr_assets()
    
    if(input$vis_bs_bank != "ALL") {
      rwas_data <- rwas_data %>% filter(Name == input$vis_bs_bank)
      assets_data <- assets_data %>% filter(Name == input$vis_bs_bank)
    } else if(input$vis_bs_country != "ALL") {
      rwas_data <- rwas_data %>%
        filter(Country == input$vis_bs_country) %>%
        group_by(Exercise, Period) %>%
        mutate(across(matches("ITM_"), ~sum(., na.rm = TRUE))) %>%
        ungroup() %>%
        mutate(across(c(Bank_ID, Name), ~"Total")) %>%
        distinct()
      
      assets_data <- assets_data %>%
        filter(Country == input$vis_bs_country) %>%
        group_by(Exercise, Period) %>%
        mutate(across(matches("ITM_"), ~sum(., na.rm = TRUE))) %>%
        ungroup() %>%
        mutate(across(c(Bank_ID, Name), ~"Total")) %>%
        distinct()
    } else {
      rwas_data <- rwas_data %>%
        group_by(Exercise, Period) %>%
        mutate(across(matches("ITM_"), ~sum(., na.rm = TRUE))) %>%
        ungroup() %>%
        mutate(across(c(Bank_ID, Name, Country, ISO2), ~"Total")) %>%
        distinct()
      
      assets_data <- assets_data %>%
        group_by(Exercise, Period) %>%
        mutate(across(matches("ITM_"), ~sum(., na.rm = TRUE))) %>%
        ungroup() %>%
        mutate(across(c(Bank_ID, Name, Country, ISO2), ~"Total")) %>%
        distinct()
    }
    
    # Define categories
    assets_items <- c("ITM_493", "ITM_494", "ITM_495", "ITM_496", "ITM_497", "ITM_498", "ITM_499", "ITM_500", "ITM_501", "ITM_65")
    names(assets_items) <- c(
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
    
    rwas_items <- c("ITM_405", "ITM_410", "ITM_411", "ITM_412", "ITM_30", "ITM_414", "ITM_418", "ITM_33", "ITM_34", "ITM_700", "ITM_35")
    names(rwas_items) <- c(
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
    
    liabilities_items <- c(
      "ITM_523", "ITM_524", "ITM_525", "ITM_526", "ITM_527", "ITM_528", "ITM_529", "ITM_530", "ITM_531", "ITM_532", "ITM_533", "ITM_534",
      "ITM_535", "ITM_538", "ITM_539"
    )
    names(liabilities_items) <- c(
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

    ensure_item_columns <- function(data, required_columns, fill_value = 0) {
      missing_columns <- setdiff(required_columns, names(data))

      for (column_name in missing_columns) {
        data[[column_name]] <- fill_value
      }

      data
    }

    rwas_required_columns <- unique(c(rwas_items, "ITM_35"))
    asset_required_columns <- unique(c(assets_items, liabilities_items))

    rwas_data <- ensure_item_columns(rwas_data, rwas_required_columns)
    assets_data <- ensure_item_columns(assets_data, asset_required_columns)
    
    # Process RWAs
    rwas_processed <- rwas_data %>%
      mutate(ITM_35 = dplyr::na_if(ITM_35, 0)) %>%
      mutate(across(c(ITM_405, ITM_410, ITM_411, ITM_412, ITM_30, ITM_414, ITM_418, ITM_33, ITM_34, ITM_700), ~ . / ITM_35)) %>%
      pivot_longer(matches("ITM_"), names_to = "Common_Item", values_to = "Amount") %>%
      mutate(RWA = ifelse(Common_Item %in% rwas_items[which(names(rwas_items) != "Total risk exposure amounts")], 1, NA)) %>%
      filter(!is.na(Amount)) %>%
      mutate(Exposure = str_remove(Common_Item, "ITM_")) %>%
      mutate(RWA = ifelse(RWA == 1, "ITM_RWA", NA)) %>%
      mutate(Common_Item = RWA) %>%
      mutate(Scenario = 1) %>%
      filter(!is.na(Common_Item)) %>%
      dplyr::select(-RWA)
    
    # Process Assets
    assets_processed <- assets_data %>%
      mutate(
        ITM_65 = dplyr::na_if(ITM_65, 0),
        ITM_539 = dplyr::na_if(ITM_539, 0)
      ) %>%
      mutate(across(c(ITM_493, ITM_494, ITM_497, ITM_498, ITM_501), ~ . / ITM_65)) %>%
      mutate(across(c(ITM_523, ITM_525, ITM_526, ITM_531, ITM_533, ITM_538), ~ . / ITM_539)) %>%
      pivot_longer(matches("ITM_"), names_to = "Common_Item", values_to = "Amount") %>%
      mutate(Asset = ifelse(Common_Item %in% assets_items[which(names(assets_items) != "Total assets")], 1, NA)) %>%
      mutate(Liability = ifelse(Common_Item %in% liabilities_items[which(names(liabilities_items) != "Total Equity and Total Liabilities")], 1, NA)) %>%
      filter(!is.na(Amount)) %>%
      mutate(Exposure = str_remove(Common_Item, "ITM_")) %>%
      mutate(Asset = ifelse(Asset == 1, "ITM_ASSETS", NA)) %>%
      mutate(Liability = ifelse(Liability == 1, "ITM_LIABILITIES", NA)) %>%
      mutate(Common_Item = coalesce(Asset, Liability)) %>%
      mutate(Scenario = 1) %>%
      filter(!is.na(Common_Item)) %>%
      dplyr::select(-Asset, -Liability)
    
    # Combine data
    combined_data <- bind_rows(assets_processed, rwas_processed) %>%
      mutate(Exposure = factor(paste("ITM", Exposure, sep = "_"), 
                               c(rev(assets_items), rev(rwas_items), rev(liabilities_items)), 
                               c(rev(names(assets_items)), rev(names(rwas_items)), rev(names(liabilities_items))))) %>%
      filter(!is.na(Amount))
    
    combined_data
  })
  
  output$vis_breakdown_chart <- renderPlotly({
    data <- bs_filtered_data()
    data <- data %>% filter(Common_Item == input$vis_bs_type)
    
    validate(
      need(nrow(data) > 0, "No data available for selected combination.")
    )
    
    if(input$vis_bs_type == "ITM_LIABILITIES") {
      title_text <- "Breakdown of Liabilities"
    } else if(input$vis_bs_type == "ITM_ASSETS") {
      title_text <- "Breakdown of Assets"
    } else {
      title_text <- "Breakdown of Risk Exposure Amounts"
    }
    
    if(input$vis_bs_bank != "ALL") {
      title_text <- paste(title_text, "-", input$vis_bs_country, "-", input$vis_bs_bank)
    } else if(input$vis_bs_country != "ALL") {
      title_text <- paste(title_text, "-", input$vis_bs_country, "- All Banks")
    } else {
      title_text <- paste(title_text, "- All Countries - All Banks")
    }
    
    palette <- rep(c("#5EC5C2", "#4471B9", "#0FA1F5", "#1858E1", "#51438F", "#18AFC7", "#454DD1"), 
                   length(unique(data$Exposure)))
    
    chart <- data %>%
      ggplot(aes(
        x = as.yearqtr(as.Date(paste0(Period, "01"), format = "%Y%m%d")),
        text = paste0("Exposure: ", Exposure, "\nAmount: ", round(Amount * 100, 2), "%")
      )) +
      geom_bar(aes(y = Amount * 100, fill = Exposure), stat = "identity", show.legend = TRUE) +
      scale_x_yearqtr(format = "%Y Q%q") +
      scale_y_continuous(labels = scales::comma) +
      labs(
        x = "",
        y = title_text,
        title = title_text
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
  })
  
  # Center network button
  observeEvent(input$center_network, {
    visNetworkProxy("vis_network") %>%
      visFit(animation = list(duration = 500, easingFunction = "easeInOutQuad"))
  })
  
  # Download handler for Create_Charts_DB.R script
  output$downloadChartsScript <- downloadHandler(
    filename = "Create_Charts_DB.R",
    content = function(file) {
      # Path to your Create_Charts_DB.R script
      script_path <- "Meta/Functions/Create_Charts_DB.R"  # Adjust this path if the script is in a different location
      
      if (file.exists(script_path)) {
        file.copy(script_path, file, overwrite = TRUE)
      } else {
        # If the file doesn't exist, create a placeholder message
        writeLines("# Create_Charts_DB.R script not found. Please ensure the file exists in the app directory.", file)
      }
    }
  )
  
  
}

shinyApp(ui = ui, server = server)
## NA = total/no breakdown

