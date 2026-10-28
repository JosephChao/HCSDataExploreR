#### Load library ####
library(shiny)
library(dplyr)
library(tidyr) # for unite, replace_na
library(ggplot2)
library(stringr) # for str_extract, str_replace
# library(shinyWidgets) # for chooseSliderSkin, pickerInput
# library(plotrix) # for std.error()
library(DoseFinding) # for dose response non-linear regression curves
library(shinyjs) # for reset, enable, disable, show, hide
library(data.table) # for rbindlist
# library(scales) # for oob = squish
library(readr)  # for read_csv and locale
library(Hmisc)  # for mean_sdl and mean_cl_boot error bars. Masks dplyr summarize
library(openxlsx)


# Version
version_number <- 1.027
# 0.93: add appended data to save project
# 0.94: add plate size selector and clear uploaded data
# 0.95: add x_axis_order and group_order. fix sp2
# 0.951: speed up bc. need to fix losing appended data when changing tabs
# 0.96: download high res plots
# 0.961: fix missing Layout column in plate layout. Update layout size when loading project. Update Analysis tab to hide when clearing data.
# 0.962: fix plate_layout_def to not depend on DR_tab and ZF_tab
# 0.963: fix observe for curve fit tab. Fix bar chart manual axis. Remove latin char for layout upload
# 0.964: Add uploading tsv. Fix load sample data updates to 96 plate size, layout upload does not overwrite data upload plate size. Fix title for sp and dr graphs
# 0.965: need to fix max width of bc plot. add , ignoreInit = TRUE to observeEvent, add x axis label angle
# 0.966: fix zf calculations for non CX5 data. Fix plot helper Dose Response option
# 0.967: fix 2nd plot errors due to TL removed. Fix plot 2 titles
# 0.968: fix sp2 duplicate legend. Fix bc2 legend and remove legend title. Remove dr2 legend title. Sort Plate Layout colour by alpha numeric. Default show Duration
# 0.969: Sort Plate Layout colour by alpha numeric. Default show Duration
#        Fix angled X label for SP graph.
# 0.97:  Change uploading layout to read_csv to handle unusual characters. Fix write plate layout csv to handle mu character.
# 0.971: Revert to read.csv and use file encoding ISO-8859-1 for upload and download of plate layout. Use remove_multibyte function for HCS data
# 0.972: Fix error with IXM data when removing multibyte chars
# 0.973: Revert HCS upload to UTF-8, no remove multibytes
# 0.974: Load only numeric data columns. Detect if no MEAN_ columns and display Z Factor for all columns
# 0.975: Add bar chart custom height width. Fix png resolution. Fix bar chart spacing. Add exclude/include/invert exclusion.
#        Add Exclude/Unexclude buttons to Outliers In progess: Add Control_Region
# 0.976: Fix missing isolate for plot 2 Raw Filter input. Add Time Point intervals. Increase font size for Bar Chart
# 0.977: Add data selection module. Update conditional panels. Separate plate_n. Add Exclude edge wells. Fix Unexclude well(s) button to unexclude selected plate only.
# 0.99: Switch to using modules

# 1.00: beginning in HSCDE_clean: fix fill legend showing in cf(2) plots (guides(fill = "none"))
#       fix sp log x, fix BC2 mismatched Fields not working. Fix plate_layout_def remove NA
#       added ctrl/shift selection highlighting
#       add version number to save project
# 1.001: Update saved names 
# 1.002: Upload HCS data file separator and decimal type
# 1.003: Add K^Y Y^K. Fix Excluded FALSE when changing plate sizes
# 1.004: Fix ordering grouped data when multiple control region and fix Scatter Plot ordering of grouped data.
#        Fix Z Factors analysis function when dealing with integer Values. Show Z Factors function for all raw data.
#        Remove concatenating group with multiple control_regions
# 1.005: removing filter_data input prevents excluding data
# 1.006: Fix Value_mean for SP2 threshold Data Points unchecked
# 1.007: Fix continuous scale of Control_Region (make factors). ADDING VIOLIN PLOT
#        Change !duplicated to unique and remove unlist for cf_compound_fn - NEED TO VERIFY.
#        Add importing data files with header metadata auto excluded
#        Add percent difference to Transform analysis function
#        Fix Hide legend for ScatterPlot 2, Fix Download Layout vertical centering
#        Add Help button
# 1.008: Fix Z Factor tab error (TODO: Need to have a proper fix for multiple control regions)
# 1.009: Add multigroup facet_grid.
#        Fix custom order resulting in empty graph when changing input choice
#        Fix SP legend title size. Add Smooth Line to SP
#        Add advanced layout column add/combine
# 1.010: Add ordering of multigroup grid and data point size to custum inputs. Adjust custom inputs.
#        Add functions to repeated graph code (eg CI)
#        Add colourpicker for plate view
# 1.011: Fix PV Legend and colourpicker values. Fix Download csv and png colour.
# 1.012: Add uploading well-plate layout format
# 1.013: Remove xls upload option. Fix uploading well-plate format empty columns. Remove loading 'scales' library for oob=squish
#        Fix save multigroup. Add req(input$data_upload). Change exclude wells to include all when mixed.
#        Use Timepoint instead of Time Point (updated load project too)
#        Automatically detect data file format: separator, row start, table format
# 1.014: Catch error if uploaded data file is not a data.frame. Upload .xlsx files and improve auto detect data for xlsx.
#        Increase max upload size. Fix convert/combine layout columns with Field data.
# 1.015: Add back library readr
# 1.016: Fix fread to fill = TRUE
# 1.017: Fix input plate_n (layout_plate_n_choice, multiplate_n_choice, multiplate_n2_choice, pv_plate_n_choice) and field_n_choice to be integer
# 1.018: Fix back compatibility of project with integer layout plate_n
# 1.019: Fix Field sum error for integers (convert to numeric)
# 1.020: 25-09-18 Add click to select wells. Add tips for Well selection. Add Concentration dilutions. Rename compound_units to concentration_units
# 1.021: 25-09-26 Add sys date to saved project. Change default name to include version number.
#        Add back compatibility to all projects
# 1.022: 25-12-03 Allow uploading multitabs from excel files. Fix PV all NA values. Add upload tooltips. Autodetect case insensitive Well, Col(umn), Row.
#        Add grouping to transform function and recode
#        Fix append to change Value name first to new name in case 'Value' is already a column header
# 1.023: 26-03-05 Add violin plot. Add req input$plot_type_bc to prevent error. Add option for line on Bar chart with explanation of mean.
# 1.024: 26-04-28 Fix load project saved settings
# 1.025: 26-06-19 Move autofill inputs. Swap heatmap min/max default colours. Fix autoloading excel files with empty header rows (need readxl).
#        Add trimws for autodetection. Add first and last columns to upload for microplate formats.
#        Update rbind to plyr::rbind.fill for dual analysis bar chart to prevent errors with mixed table data (eg. Z factor analysis)
#        Update file names (drop the number and lower case): analysis_server1, plot_helper2, Utilities2, Utilities_analysis.R
#        Update analysisFuncs.csv for Transform function. (Need to add Z score info)
# 1.026: 2026-06-29 Save PV settings. Clean all commented out data. Update colorPickerInputIDs. Update sample data. 
#        Add tooltip for Manual Y-axis settings for dual analysis
# 1.027: 2026-10-24 Remove requirement for specific layout definitions. Update clear and reset wells to allow for custom layout definitions.
#        Find numeric columns. Update sample data (remove name). 
#        Fix manual upload columns input. Find case insensitive Well, Row, Column, Field headers. Add manual input of these header names.
#        Change download of plate layout to UTF-8 encoding and add sub of mu character to uploading
#        Fixed plate view customization settings showing up on Dual Analysis tab
#        Update MIC/IC50 values to any interpolated value using Threshold and add customization options
#        Update name to HCS DataExploreR

# options(shiny.reactlog = TRUE)  # log visualizer (press Ctrl F3 after loading app)


#TODO 
# error if mix and match with/without field data (sometimes?)
# Need to update uninf_def_ctrl_data_selected in modules.R when switching to DT_Utilities_analysis for both PVdataWrang_moduleServer and GraphDataWrang_moduleServer
# Remove % growth/inhibition analysis if no neg ctrl
# Occasional long pause when loading data (appears to be due to 'accept = c(".csv", ".xlsx", ".txt")')
# Z Factor tab bug when loading multiple mixed plates with and without controls needed to calculate Z'
# Add check if plate layout is loaded (should not be able to press Go on PH tab and no calculations on PV tab)
# analysis functions of SP2 not saving to project. Saved settings, eg hide legend not applying after loading project
# show data table long for SP2 error



#### Initial Setup ####
rowNumber <- setNames(1:32, c(LETTERS[1:26], paste0("A", LETTERS[1:6])))  # to convert row letters to numbers
rowLetter <- c(LETTERS[1:26], paste0("A", LETTERS[1:6]))  # to convert row numbers to letters

# Create 384 well plate with layout columns
plate384 <- expand.grid(Row = LETTERS[1:16], Column = 1:24)  # Create 384 well plate of Rows and Columns
plate384$Well <- paste0(plate384$Row, plate384$Column)  # Create Well column of Row and Column
plate384 <- plate384[order(plate384$Row), ]  # Order by Row
plate384$Row <- as.character(plate384$Row)

# Layout column headers. Must match UI inputIDs
layout_cols <- c("Row",
                 "Column",
                 "Well",
                 "Layout",
                 "plate",
                 "definition",
                 "Sample",
                 "Description",
                 "Description_2",
                 "Duration",
                 "duration_units",
                 "Compound",
                 "Concentration",
                 "concentration_units",
                 "Compound_2",
                 "Concentration_2",
                 "concentration_units_2",
                 "Control_Region",
                 "unique_duration",
                 "unique_compound",
                 "unique_compound_2",
                 "Excluded")


# Sample_col <- match("Sample", layout_cols)  # quick access to indexing Sample column in layout
# Control_Region_col <- match("Control_Region", layout_cols)
# Excluded_col <- match("Excluded", layout_cols)  # quick access to indexing Excluded column in layout

Base_defs <- c("Row", "Column", "Well", "Layout", "plate")
Numeric_defs <- c("Duration", "Concentration", "Concentration_2")
Unit_defs <- c("duration_units", "concentration_units", "concentration_units_2")

Basic_defs <- layout_cols[match("Sample", layout_cols):match("Control_Region", layout_cols)]
Basic_defs_no_units <- Basic_defs[!Basic_defs %in% Unit_defs]

Sample_controls <- c("Positive Control",
                     "Negative Control",
                     "Uninfected Control",
                     "Uninfected Control by definition")

exclude_filters_cols <- layout_cols[!layout_cols %in% c("Layout", "plate", "Sample")]

# Setup empty plate layout
plate384[layout_cols[-(1:3)]] <- NA
plate384$plate <- 1
plate384$Layout <- "Layout 1"
plate384$Excluded <- FALSE

# Empty 96-well plate for filling errors
empty_plate96 <- plate384[which(plate384$Column < 13 & plate384$Row %in% LETTERS[1:8]), c(1:3, 5)]

# Plate sizes
plate_sizes <- data.frame(Well = c(6,12,24,48,96,384,1536),
                          Row = c(2,3,4,6,8,16,32),
                          Column = c(3,4,6,8,12,24,48))

well_px_size <- data.frame(Well = c(6,12,24,48,96,384,1536),
                         Px = c(50,36,26,18,12,8,6))

#### set color palette ####
# For plate layout selector - set up 96 unique colours
rc1 <- colorRampPalette(colors = c("#006d2c", "#e5f5e0"))(12)    #green
rc2 <- colorRampPalette(colors = c("#deebf7", "#08519c"))(12)    #blue
rc3 <- colorRampPalette(colors = c("#214398", "#3B3593"))(2)    #blue purple
rc4 <- colorRampPalette(colors = c("#54278f", "#efedf5"))(12)    #purple
rc5 <- colorRampPalette(colors = c("#f0f0f0", "#525252"))(12)    #grey
rc6 <- colorRampPalette(colors = c("#6E3C3E", "#892529"))(2)    #grey red
rc7 <- colorRampPalette(colors = c("#a50f15", "#fee0d2"))(12)    #red
rc8 <- colorRampPalette(colors = c("#fee6ce", "#CD6600"))(12)    #orange
rc9 <- colorRampPalette(colors = c("#C87610", "#C28621"))(2)    #orange yellow
rc10 <- colorRampPalette(colors =c("#BD9631", "#fff7bc"))(12)    #yellow
rc11 <- colorRampPalette(colors =c("#DBE3A7", "#248141"))(6)    #yellow green
rampcolor96 <- c(rc1, rc2, rc3, rc4, rc5, rc6, rc7, rc8, rc9, rc10, rc11)

greyscale_colors <- grey.colors(10, start = 0, end = 1, rev = TRUE)

# Use every other colour for greater distinction between colours followed by the skipped colours - also used in Bar Chart
alt_colors96 <- c(rampcolor96[c(TRUE, FALSE)], rampcolor96[c(FALSE, TRUE)])

d_alt_colors96 <- c(alt_colors96[c(TRUE, FALSE)], alt_colors96[c(FALSE, TRUE)])
# Repeat the colour palette 4 times to handle a max of 384 colours
alt_colors384 <- rep(alt_colors96, 4)
d_alt_colors384 <- rep(d_alt_colors96, 4)

sp_col <- c("purple", alt_colors384)

#### Load Sample Data and analysis descriptions ####
example_data <- read.csv("data/sample_data.csv", header = TRUE, check.names = FALSE)
example_layout <- read.csv("data/sample_layout.csv", check.names = FALSE, fileEncoding = "UTF-8")
# example_layout <- readr::read_csv("data/sample_layout.csv", locale = locale(encoding = "ISO-8859-1"), show_col_types = FALSE)

# Import descriptions for all calculations used in app
graphOptDescs_csv <- read.csv("data/graphOptDescs.csv", check.names = FALSE)
ctrlsTabsDescs_csv <- read.csv("data/ctrlsTabsDescs.csv", check.names = FALSE)

# NEED TO CLEAR ANY EMPTY ROWS
analysisFuncs_csv <- read.csv("data/analysisFuncs.csv", check.names = FALSE)[-1, ]  # Remove explanation row
# Replace "Ref" with the regular expression (perl compatible) to find %LOW or %HIGH HCS data parameters
analysisFuncs_csv$filter_reference[which(analysisFuncs_csv$filter_reference == "Ref")] <- "%(LOW_|HIGH_)(?=.*Ch2)(?!.*(Ratio|Diff))"
# add "analysis_" to reference, compare and factor to match the input name. Non-empty cells needed to satisfy req(analysis_multi_input()) in dataSelection module
analysisFuncs_csv$multi_input <- ifelse(analysisFuncs_csv$multi_input != "", tolower(analysisFuncs_csv$multi_input), "NULL")


rawInputFilters <- analysisFuncs_csv$filter_raw_input[-1]
rawInputFilters <- rawInputFilters[which(rawInputFilters != "ALL" & rawInputFilters != "")] # For filtering Analysis functions based on Raw Data column names

# Change max resolution for download
options(ragg.max_dim = 1000000)

# Increase file upload size
options(shiny.maxRequestSize = 2000 * 1024^2)  # 200 MB

#### Define tab abbreviations ####
# Names must match tab names
tab_abbr <- list(
  "Plate View" = "pv",
  "Bar Chart" = "bc",
  "Scatter Plot" = "sp",
  "Curve Fit" = "cf",
  "Z Factor" = "zf",
  "Bar Chart 2" = "bc2",
  "Scatter Plot 2" = "sp2",
  "Curve Fit 2" = "cf2"
)

#### Set up standard input options ####
# Options where comparison is needed for analysis
compare_options <- c("Negative Control",
                     "Positive Control",
                     "Uninfected Control",
                     "Uninfected Control by definition")

# default customization saved values
customizationList <- list(
  graph_title_choice = "",
  graph_title_font_sz_choice = 16,
  
  y_title_font_sz_choice = 14,
  y_label_font_sz_choice = 12,
  x_title_choice = "",
  x_title_font_sz_choice = 14,
  x_label_font_sz_choice = 12,
  x_label_angle_choice = 0,
  y_label_angle_choice = 0,
  
  show_legend_choice = TRUE,
  legend_title_choice = "",
  legend_title_font_sz_choice = 14,
  legend_label_font_sz_choice = 12,
  legend_key_sz_choice = 18
)

# Plate View customization color picker saved input names
colorPickerInputIDs <- c("cp_scale_n", "cp_auto_val", "cp_text_colr", "cp_min_val", "cp_mid_val", "cp_max_val", "cp_min_colr", "cp_mid_colr", "cp_max_colr")

# Lists used for manual upload data format settings
separatorList <- list(
  "auto" = "auto",
  "comma" = ",",
  "tab" = "\t",
  "semicolon" = ";"
)

decimalList <- list(
  "auto" = "auto",
  "period" = ".",
  "comma" = ","
)

#### Utilities ####
# Action button function
actionButton_fn <- function(ID,LAB,COL="Black",PAD=4,FT=18,W=150,H=50,TTL=NULL) {
  actionButton(inputId = ID, label = LAB,
               style = paste0("padding:",PAD,"px; font-size:",FT,";width:",W,"px;height:",H,"px;color:",COL),
               title = TTL)
}

#### Source files ####
source('autodetector.R', local = TRUE)
source('utilities.R', local = TRUE)
source('utilities_analysis.R', local = TRUE)
source('modules.R', local = TRUE)
source('html_header.R', local = TRUE)

#### ---UI--- ####
ui <- navbarPage(
  paste0("HCS DataExploreR v", sprintf("%.3f", version_number)), id = "navbar",
  
  header = html_header,

  # Plate Layout Tab
  tabPanel("Plate Layout Entry",
           verticalLayout(
             fluidRow(
               column(width = 3,
                      checkboxGroupInput("layout_options", "Show Input Options",
                                         choices = c("Description 2 / Duration" = "dur_input",
                                                     "Compound / Concentration" = "cf_input",
                                                     "Synergy (2nd Compound)" = "syn_input",
                                                     "Multiple Controls" = "ctrl_input"
                                         ),
                                         selected = c("dur_input","cf_input","syn_input","ctrl_input"))),
                      
               column(width = 3,
                      div(style="position:relative; display:inline-block; float:left; width: 5%; top: 25px; right: 8%",
                          title="Change 'Upload HCS data' file type",
                          uiOutput("hcs_data_upload_setting")),
                      
                      div(style="position:relative; display:inline-block; float:left; width: 95%",
                          fileInput("data_upload", label = "Upload HCS data", multiple = TRUE, accept = c(".csv", ".txt", ".xlsx")),
                      ),
                      conditionalPanel(
                        condition = "output.data_uploaded",
                        div(style="position:relative; display:inline-block; float:left; left:5%; top:-20px",
                            title="Clear Uploaded Data",
                            actionButton_fn("clear_data_plate","Clear Data","grey", FT = 12, W = NULL, H = NULL))
                      )),
               
               column(width = 3,
                      fileInput(inputId = "layout_upload", label = "Upload Plate Layout", multiple = TRUE, accept = ".csv")),
               column(width = 3,
                      fileInput("load_proj", "Load Project", accept = ".rds"),
                      div(style="top:-10px; position:relative; float:left; right:0%; display:inline-block; width:75px",
                          title="Select Plate",
                          uiOutput("layout_plate_n")),
                      div(style="top:-10px; position:relative; float:left; right:0%; display:inline-block",
                          title="Copy layout to new plate",
                          actionButton_fn("add_plate","+","blue",1,28,35,35)),
                      div(style="top:-10px; position:relative; float:left; right:0%; display:inline-block",
                          title="Delete last plate layout",
                          actionButton_fn("delete_plate","-","red",1,28,35,35)),
                      div(style="top:-10px; position:relative; float:left; left:10%; display:inline-block; width:75px",
                          title="Select Plate Layout size. Disabled when Data file uploaded.",
                          selectInput(inputId = "select_plate_size",
                                      label = NULL,
                                      choices = plate_sizes$Well[-7],  # Excluding 1536-well plate until updated
                                      selected = 96)
                          )
                      ),
               
             ),  #close fluidRow 1
             sidebarLayout(
               
               sidebarPanel(
                 column(width = 12,
                        uiOutput("well_selection")),
                 fluidRow(
                   column(width = 6,
                          uiOutput("Sample")),
                   column(width = 6,
                          textInput(inputId = "Description", label = "Description"))),
                 
                 conditionalPanel(
                   condition = "input.layout_options.includes('dur_input')",
                   fluidRow(
                     column(width = 6,
                            textInput(inputId = "Description_2", label = "Description 2")),
                     column(width = 3,
                            numericInput(inputId = "Duration", label = "Duration", value = NULL)),
                     column(width = 3,
                            selectInput(inputId = "duration_units", label = "Units",
                                        choices = c("d","hr","min"), selected = "hr")))
                   
                 ),
                 conditionalPanel(
                   condition = "input.layout_options.includes('cf_input')",
                   fluidRow(
                     column(width = 6,
                            textInput(inputId = "Compound", label = "Compound")),
                     column(width = 3,
                            numericInput(inputId = "Concentration", label = "Concentration", value = NULL)),
                     column(width = 3,
                            selectInput(inputId = "concentration_units", label = "Units", choices = c("µM", "µg/mL", "%"))))
                   
                 ),
                 conditionalPanel(
                   condition = "input.layout_options.includes('syn_input')",
                   fluidRow(
                     column(width = 6,
                            textInput(inputId = "Compound_2", label = "Compound 2")),
                     column(width = 3,
                            numericInput(inputId = "Concentration_2", label = "Concentration 2", value = NULL)),
                     column(width = 3,
                            selectInput(inputId = "concentration_units_2", label = "Units 2", choices = c("µM", "µg/mL", "%"))))
                   
                 ),
                 conditionalPanel(
                   condition = "input.layout_options.includes('ctrl_input')",
                   fluidRow(
                     column(width = 4,
                            div(title="Used to identify multiple experiments with different controls on the same plate. Mark wells that correspond to their given Positive and/or Negative Control(s) with the same number",
                                uiOutput("Control_Region"))))
                 ),
                 
                 fluidRow(
                   column(width = 1, br(),
                          conditionalPanel(
                            condition = "output.multiplate_output",
                            div(title="Apply Add/Clear/Exclude Wells to selected well on ALL plates",
                                checkboxInput("apply_to_all", "All"))
                          )
                   ),
                   
                   column(width = 4, br(),
                          actionButton_fn("add_wells","Add well(s)","blue")
                   ),
                   column(width = 4, br(),
                          actionButton_fn("clear_wells","Clear well(s)","red", TTL = "Clear selected well(s)")
                   ),
                   column(width = 2, br(),
                          uiOutput("exclude_wells")
                   )
                 ),
                 width = 5),  #close layout sidebarPanel
               
               mainPanel(
                 
                 plotOutput("plot_selector", width = 700, height = 350, brush = "plot_brush", click = "plot_click"),
                 
                 fluidRow(
                   column(width = 3, offset = 1, br(),
                          downloadButton(
                            outputId = "downloadLayout", 
                            label = "Download Layout",
                            style = 'display: flex; 
                                     align-items: center; 
                                     justify-content: center; 
                                     gap: 8px; 
                                     width: 150px; 
                                     height: 50px; 
                                     color: blue; 
                                     padding: 0;
                                    '
                          )
                   ),
                   column(width = 3, br(),
                          actionButton_fn("reset","Reset all wells","red")),
                   column(width = 3, br(),
                          actionButton_fn("Example_data","Load Sample Data","darkgreen"))
                 ),
                 width = 7)  #close layout mainPanel
             ),  #close layout sidebarLayout
             
             sidebarLayout(
               
               sidebarPanel(
                 fluidRow(
                   column(width = 8, uiOutput("create_layout_from_data")),
                   column(width = 4, uiOutput("layout_autofill"))
                 ),
                 br(),
                 uiOutput("layout_table_format"),
                 uiOutput("select_definition"),
                 
                 width = 3
               ),  #close layout sidebarPanel 2
               
               mainPanel(
                 # List uploaded plate filenames
                 tableOutput("plate_filenames"),
                 
                 tableOutput(outputId = "plate_layout")
               )  #close layout mainPanel 2
             ),
             
             # uiOutput("button"), # testing only
             # tableOutput("testinglayout"), # testing only
             # tableOutput("testinglayout2"), # testing only
           )  #close verticalLayout
           
  ),  #close tabPanel "Plate Layout Entry"
  
  #### Analysis Tab ####
  tabPanel("Analysis",
           verticalLayout(
             sidebarLayout(
               sidebarPanel(
                 style = "height: 90vh; overflow-y: auto;",
                 
                 # Plot Helper show all analysis functions
                 conditionalPanel(
                   condition = "input.tabset_panel == 'Plot Helper'",
                   uiOutput("show_all_analysis_ph")
                 ),
                 
                 # Multiplate input options
                 conditionalPanel(
                   condition = "output.multiplate_output && ['Bar Chart', 'Scatter Plot', 'Curve Fit'].includes(input.tabset_panel)",
                   div(style="display:inline-block; width:79%",
                            uiOutput("multiplate_n")),
                   div(style="display:inline-block; margin-top: 25px; float: right; width:19%",
                            title="Select all plates",
                            uiOutput("multiplate_add_all")),
                   
                   conditionalPanel(
                     condition = "input.multiplate_n.length > 1",
                     uiOutput("plate_slider_avg_tl"), # Show plate slider and take plate average for biological replicates
                     uiOutput("plate_slider")  # Slider input for filtering by plate - mainly for changing duration time point
                   )
                 ),
                 
                 dataSelection_moduleUI("dataSelection"),
                 
                 conditionalPanel(
                   condition = "input.tabset_panel == 'Plot Helper'",
                   uiOutput("graph_type_ph")
                 ),
                 
                 # Plate View input options
                 conditionalPanel(
                   condition = "input.tabset_panel == 'Plate View' || (input.tabset_panel == 'Plot Helper' & input.graph_type_ph == 'Plate View')",
                   div(title = "Calculate outliers for defined wells only.",
                   uiOutput("pv_outlier")),  # Show Outliers
                   conditionalPanel(
                     condition = "input.pv_outlier",
                     fluidRow(
                       column(width = 8,
                              uiOutput("pv_outlier_function")
                       ),
                       conditionalPanel(
                         condition = "input.pv_outlier_function == 'Z-Score'",
                         column(width = 4,
                                uiOutput("pv_outlier_z_score_SD")
                         )
                       )
                     ),
                     fluidRow(
                       column(width = 12,
                              uiOutput("pv_outlier_group")
                       )
                     ),
                     fluidRow(
                       column(width = 6,
                              title = "Click to exclude outlier well(s) on the selected plate",
                              actionButton("pv_outlier_exclude_wells", "Exclude Outlier(s)")),
                       column(width = 6,
                              title = "Click to include ALL excluded well(s) on the selected plate",
                              actionButton("pv_outlier_unexclude_all_wells", "Include All"))
                     ),
                     div(title = "Exclude the first and last rows and columns from the selected plate",
                         uiOutput("pv_outlier_exclude_edge_wells"))
                   ), # close pv_outlier condition
                 ), # close Plate View condition
                 
                 conditionalPanel(
                   condition = "input.tabset_panel == 'Plate View'",
                   uiOutput("pv_checkbox_inputs_tl"),  # plate view input check boxes (display, average, ignore empty, ignore uninfected)
                 ),
                 
                 conditionalPanel(
                   condition = "['Bar Chart', 'Scatter Plot'].includes(input.tabset_panel) || (input.tabset_panel == 'Plot Helper' && ['Bar Chart', 'Scatter Plot'].includes(input.graph_type_ph))",
                   
                   fluidRow(
                     column(width = 6,
                            # title = "",
                            style = "padding: 0px; padding-left: 15px; padding-right: 2px",
                            uiOutput("plot_x_axis")),
                     
                     column(width = 6,
                            title = "Select same input as 'Plot X-axis' for no grouping",
                            style = "padding: 0px; padding-right: 15px; padding-left: 2px",
                            uiOutput("group_fill"))
                   ),
                   
                   conditionalPanel(
                     condition = "input.tabset_panel != 'Plot Helper'",
                       fluidRow(
                         column(width = 6,
                                uiOutput("multigroup"))
                       )
                   )
                   
                 ),
                 
                 conditionalPanel(
                   condition = "input.tabset_panel == 'Curve Fit' || (input.tabset_panel == 'Plot Helper' & input.graph_type_ph == 'Curve Fit')",
                   
                   fluidRow(
                     column(width = 6,
                            style = "padding: 0px; padding-left: 15px; padding-right: 2px",
                            uiOutput("plot_x_axis_cf")),
                     
                     
                     column(width = 6,
                            title = "Select input that describes the 'Plot X-axis' selection",
                            style = "padding: 0px; padding-right: 15px; padding-left: 2px",
                            uiOutput("group_fill_cf"))
                   ),
                 ),
                 
                 conditionalPanel(
                   condition = "input.tabset_panel == 'Curve Fit'",
                   div(
                     fluidRow(
                       column(width = 6,
                              style = "padding: 0px; padding-left: 15px",
                              uiOutput("cf_selection")),
                       column(width = 6,
                              style = "padding: 0px; padding-right: 15px",
                              uiOutput("best_fit_function"))
                     ))
                 ),

                 
                 conditionalPanel(
                   condition = "input.tabset_panel == 'Plot Helper'",
                   uiOutput("go_ph"),
                 ),
                 
                 # BC SP CF input options
                 conditionalPanel(
                   condition = "['Bar Chart', 'Scatter Plot', 'Curve Fit'].includes(input.tabset_panel)",
                   
                   div(title = "SD: Standard Deviation; SEM: Standard Error of the Mean; CL: bootstrapped Confidence Limits",
                       uiOutput("errorbars")
                   ),
                   
                   conditionalPanel(
                     condition = "input.tabset_panel == 'Bar Chart'",
                     uiOutput("plot_type_bc")
                   ),
                   conditionalPanel(
                     condition = "input.tabset_panel == 'Scatter Plot'",
                     uiOutput("connecting_line_sp_tl") # connecting line and log X
                   ),
                   conditionalPanel(
                     condition = "input.tabset_panel == 'Curve Fit'",
                     uiOutput("prediction_band_cf_tl")  # prediction band and separate graphs (removed)
                   ),
                   uiOutput("data_points_tl"),
                   
                   fluidRow(
                     div(
                       column(width = 4,
                              title = "Add Threshold line(s) on the Y-axis. Comma separate to add multiple lines",
                              style = "padding: 0px; padding-left: 15px",
                              uiOutput("threshold_line")  # Add threshold line
                       )
                     ),
                     div(title = "Manually adjust the Y-axis. Requires both Min and Max inputs. Leave empty to auto-adjust.",
                         column(width = 4,
                                style = "padding: 0px; padding-left: 15px",
                                uiOutput("y_axis_min")),
                         column(width = 4,
                                style = "padding: 0px; padding-right: 15px",
                                uiOutput("y_axis_max"))
                     )
                   ),
                   
                   uiOutput("include_sample"),  # Include Sample types
                   div(title = "Excluded data will be removed prior to analysis calculations. This does not apply to control wells.",
                       uiOutput("data_filters")
                   ),
                   uiOutput("exclude_filters_tl"),
                 ),
                 
                 # Show data table options
                 conditionalPanel(
                   condition = "input.tabset_panel != 'Plot Helper'",
                   
                   fluidRow(
                     column(width = 6,
                            div(style = "position:relative; margin-top: -6%",
                                uiOutput("show_data_table"))),
                     conditionalPanel(
                       condition = "input.show_data_table",
                       column(width = 6,
                              uiOutput("data_table_format"))
                     )
                   ),
                   column(width = 6,
                          div(title = "Download Data Table. Format can be adjusted under 'Show data table' checkbox.",
                              downloadButton(outputId = "downloadDataButton", label = "csv", style = paste0("color: ", "blue")))),
                   br()
                 ),
                 
                 # Multi plate view plots
                 conditionalPanel(
                   condition = "input.tabset_panel == 'Plate View'",
                   br(),
                   br(),
                   uiOutput("show_pv2"),
                   conditionalPanel(
                     condition = "input.show_pv2",
                     dataSelection_moduleUI("dataSelection_pv2"),
                   ),
                 ),
                 
                 width = 3
               ), #sidebarPanel

               #### Analysis Main Panel ####
               mainPanel(
                 tabsetPanel(id = "tabset_panel",
                             tabPanel("Plot Helper",
                                      br(),
                                      uiOutput("ph_descriptions"),
                                      uiOutput("ph_equations"),
                                      
                                      uiOutput("ph_descriptions_2"),
                                      uiOutput("ph_equations_2"),
                                      uiOutput("ph_analysis_ref"),
                                      
                                      uiOutput("ph_tabs"),
                                      br(),
                                      tableOutput("ph_additional_options"),
                             ),
                             
                             tabPanel("Plate View",
                                      customization_moduleUI("pv_customization"),
                                      plotOutput("render_pv_plot", height = "400px"),
                                      conditionalPanel(
                                        condition = "input.show_data_table",
                                        br(),
                                        tableOutput("render_pv_table")),
                                      
                                      conditionalPanel(
                                        condition = "input.show_pv2",
                                        br(),
                                        uiOutput("pv2_spacer"),
                                        plotOutput("render_pv_plot2", height = "400px"),
                                      )
                             ),
                             tabPanel("Bar Chart", 
                                      customization_moduleUI("bc_customization"),
                                      plotOutput("render_bc_plot", height = "500px"),
                                      conditionalPanel(
                                        condition = "input.show_data_table",
                                        tableOutput("render_bc_table"))
                             ),
                             tabPanel("Scatter Plot",
                                      customization_moduleUI("sp_customization"),
                                      plotOutput("render_sp_plot", height = "500px"),
                                      conditionalPanel(
                                        condition = "input.show_data_table",
                                        tableOutput("render_sp_table"))
                             ),
                             tabPanel("Curve Fit",
                                      customization_moduleUI("cf_customization"),
                                      plotOutput("render_cf_plot", height = "500px"),
                                      conditionalPanel(
                                        condition = "input.show_data_table",
                                        tableOutput("render_cf_table"))
                             ),
                             tabPanel("Z Factor",
                                      customization_moduleUI("zf_customization"),
                                      plotOutput("render_zf_plot", height = "500px"),
                                      conditionalPanel(
                                        condition = "input.show_data_table",
                                        tableOutput("render_zf_table"))
                             )
                 ), #tabsetPanel
                 
                 # uiOutput("uitest"),
                 # tableOutput("test_table"),
                 # tableOutput("test_table2")
               ) #mainPanel
             ) #sidebarLayout
           ) #verticalLayout
  ), #tabPanel Analysis
  
  #### Dual Analysis tab ####
  tabPanel("Dual Analysis",
           verticalLayout(
             sidebarLayout(
               sidebarPanel(
                 style = "height: 90vh; overflow-y: auto;",
                 
                 conditionalPanel(
                   condition = "output.multiplate_output",
                   div(style="display:inline-block; width:79%",
                       uiOutput("multiplate_n2")),
                   div(style="display:inline-block; margin-top: 25px; float: right; width:19%",
                       title="Select all plates",
                       uiOutput("multiplate2_add_all")),
                   
                   conditionalPanel(
                     condition = "input.multiplate_n2.length > 1",
                     uiOutput("plate_avg2"),
                   )
                 ),
                 
                 conditionalPanel(
                   condition = "input.navbar == 'Dual Analysis'",
                   dataSelection_moduleUI("dataSelection_dual1"),
                   dataSelection_moduleUI("dataSelection_dual2")
                 ),
                 
                 # X-inputs - Bar Chart 2
                 conditionalPanel(
                   condition = "input.tabset_panel_dual == 'Bar Chart 2'",
                   uiOutput("plot_x_axis_bc2")
                 ),
                 
                 # X-inputs - Scatter Plot 2
                 conditionalPanel(
                   condition = "input.tabset_panel_dual == 'Scatter Plot 2'",
                   
                   fluidRow(
                     column(width = 6,
                            title = "Values with the selected layout definition will be averaged and plotted along the X-axis. Values will be averaged by unique definitions if multiple definitions are selected.",
                            style = "padding: 0px; padding-left: 15px; padding-right: 2px",
                            uiOutput("x_axis_avg_sp2")),
                     column(width = 6,
                            style = "padding: 0px; padding-right: 15px; padding-left: 2px",
                            uiOutput("group_fill_sp2")),
                   )
                 ),
                 
                 # X-inputs - Curve Fit 2
                 conditionalPanel(
                   condition = "input.tabset_panel_dual == 'Curve Fit 2'",
                   fluidRow(
                     column(width = 6,
                            style = "padding: 0px; padding-left: 15px; padding-right: 2px",
                            uiOutput("plot_x_axis_cf2")),
                     column(width = 6,
                            style = "padding: 0px; padding-right: 15px; padding-left: 2px",
                            uiOutput("group_fill_cf2")),
                   ),
                   
                   fluidRow(
                     column(width = 6,
                            style = "padding: 0px; padding-left: 15px",
                            uiOutput("cf2_selection")),
                     column(width = 6,
                            style = "padding: 0px; padding-right: 15px",
                            uiOutput("best_fit_function2"))
                   )
                 ),
                 
                 conditionalPanel(
                   condition = "input.tabset_panel_dual == 'Scatter Plot 2'",
                   fluidRow(
                     column(width = 6,
                            title = "Add Threshold line(s) on the Y-axis. Comma separate to add multiple lines.",
                            style = "padding: 0px; padding-left: 15px; padding-right: 2px",
                            uiOutput("threshold_line2_sp")),
                     
                     column(width = 6,
                            title = "Add Threshold line(s) on the X-axis. Comma separate to add multiple lines.",
                            style = "padding: 0px; padding-right: 15px; padding-left: 2px",
                            uiOutput("threshold_line2x_sp"))
                   )
                 ),
                 
                 div(title = "SD: Standard Deviation; SEM: Standard Error of the Mean; CL: bootstrapped Confidence Limits",
                     style = "margin-bottom: -10px",
                     uiOutput("errorbars2")),
                 
                 fluidRow(
                   column(width = 6,
                          style = "padding: 0px; padding-left: 15px; padding-right: 2px",
                          uiOutput("data_points2")),
                   conditionalPanel(
                     condition = "input.tabset_panel_dual != 'Bar Chart 2'",
                     column(width = 6,
                            style = "padding: 0px; padding-right: 15px; padding-left: 2px",
                            uiOutput("log_y_axis2"))
                   )
                 ),
                 
                 conditionalPanel(
                   condition = "input.tabset_panel_dual == 'Scatter Plot 2'",
                   fluidRow(
                     column(width = 6),
                     column(width = 6,
                            style = "padding: 0px; padding-right: 15px; padding-left: 2px; margin-top: -10px",
                            uiOutput("log_x_axis2"))
                   )
                 ),
                 
                 conditionalPanel(
                   condition = "input.tabset_panel_dual == 'Curve Fit 2'",
                   fluidRow(
                     column(width = 6,
                            style = "padding: 0px; padding-left: 15px; padding-right: 2px; margin-top: -10px",
                            uiOutput("prediction_band_cf2")),
                     column(width = 6,
                            style = "padding: 0px; padding-right: 15px; padding-left: 2px; margin-top: -10px",
                            uiOutput("multigraph_cf2")),
                   )
                 ),
                 
                 conditionalPanel(
                   condition = "input.tabset_panel_dual != 'Scatter Plot 2'",
                   fluidRow(
                     column(width = 6,
                            title = "Add Threshold line(s). Comma separate to add multiple lines.",
                            style = "padding: 0px; padding-left: 15px; padding-right: 2px",
                            uiOutput("threshold_line2"))
                   )
                 ),
                 
                 fluidRow(
                   div(title = "Manually adjust the Y-axis. Requires both Min and Max inputs. Leave empty to auto-adjust.",
                       column(width = 6,
                              style = "padding: 0px; padding-left: 15px; padding-right: 2px",
                              uiOutput("y_axis_min2")),
                       column(width = 6,
                              style = "padding: 0px; padding-right: 15px; padding-left: 2px",
                              uiOutput("y_axis_max2"))
                   )
                 ),
                 
                 uiOutput("include_sample2"),  # Include Sample types
                 div(title = "Excluded data will be removed prior to analysis calculations. This does not apply to control wells.",
                     uiOutput("data_filters2")
                 ),
                 uiOutput("exclude_filters2_tl"),
                 
                 fluidRow(
                   column(width = 6,
                          div(style = "position:relative; margin-top: -6%",
                              uiOutput("show_data_table2"))),
                   conditionalPanel(
                     condition = "input.show_data_table2",
                     column(width = 6,
                            uiOutput("data_table2_format"))
                   )
                 ),
                 column(width = 6,
                        div(title = "Download Data Table. Format can be adjusted under 'Show data table' checkbox.",
                            downloadButton(outputId = "downloadDataButton2", label = "csv", style = paste0("color: ", "blue")))),
                 
                 width = 3
               ), #sidebarPanel plot2
               
               #### Dual Analysis Main Panel ####
               mainPanel(
                 tabsetPanel(id = "tabset_panel_dual",
                             tabPanel("Bar Chart 2",
                                      customization_moduleUI("bc2_customization"),
                                      plotOutput("render_bc2_plot", height = "700px"),
                                      conditionalPanel(
                                        condition = "input.show_data_table2",
                                        tableOutput("render_bc2_table"))
                             ),
                             tabPanel("Scatter Plot 2",
                                      customization_moduleUI("sp2_customization"),
                                      plotOutput("render_sp2_plot", height = "500px"),
                                      conditionalPanel(
                                        condition = "input.show_data_table2",
                                        tableOutput("render_sp2_table"))
                             ),
                             tabPanel("Curve Fit 2",
                                      customization_moduleUI("cf2_customization"),
                                      plotOutput("render_cf2_plot", height = "500px"),
                                      conditionalPanel(
                                        condition = "input.show_data_table2",
                                        tableOutput("render_cf2_table"))
                             ),
                 ), #close tabsetPanel
               ) #close mainPanel dual plot
             ) #close sidebarLayout dual plot
           )  #close verticalLayout dual plot
  ), #close tabPanel Dual Analysis
  
  footer = div(
    class = "custom-navbar-buttons",
    downloadButton("save_proj", "Save Project", class = "save_proj_btn"),
    actionButton("info_help", label = NULL, icon = icon("question-circle"), class = "info_help_btn")
  )
)

#### ---SERVER--- ####
server <- function(input, output, session) {
  # Hide the Analysis tab until HCS data loaded
  shinyjs::hide(selector = "#navbar li a[data-value=Analysis]")
  
  shinyjs::hide(selector = "#navbar li a[data-value='Dual Analysis']")
  
  # Load server functions for Plate Layout tab
  source('plate_layout_server.R', local = TRUE)
  
  # Hide Duration and Synergy input options after loading (loading required to obtain default values)
  updateCheckboxGroupInput(session, inputId = "layout_options", selected = "cf_input") # c("dur_input", "cf_input"))
  
  # Load server functions for Analysis tab
  source('analysis_server.R', local = TRUE)
  
  source('dual_analysis_server.R', local = TRUE)
  
  observeEvent(input$info_help, {
    showModal(modalDialog(
      title = "Help",
      p("Hover over input buttons for additional help."),
      p(),
      p("Tutorials coming..."),
      p(),
      p("For bug reports and help using this app, contact"),
      p("joseph.chao@ubc.ca"),
      easyClose = TRUE,
      footer = NULL
    ))
  })
}

# Run the application 
shinyApp(ui = ui, server = server)
