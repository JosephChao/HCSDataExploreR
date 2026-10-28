#### Load source Utilities ####
source('plot_helper.R', local = TRUE)

#### Saved input options ####
# create defaults for all input options
default_sv <- sv <- reactiveValues(
  layout_numeric_cols = c("Duration", "Concentration", "Concentration_2"),
  # Plate Layout tab
  layout_plate_n_choice = 1,
  select_definition_choice = c("Sample", "definition"),
  
  # Plot Helper
  show_all_analysis_ph_choice = FALSE,
  
  # Analysis/Dual Analysis tabs
  multiplate_n_choice = 1,
  multiplate_n2_choice = 1,
  
  plate_slider_choice = 1,
  
  # dataSelection_moduleServer
  dataSelection = list(
    pv_plate_n_choice = 1,
    factor_choice = 1,
    field_n_choice = 1:100,
    definition_col_choice = "definition"
  ),
  dataSelection_pv2 = list(
    factor_choice = 1,
    field_n_choice = 1:100
  ),
  
  dataSelection_dual1 = list(
    factor_choice = 1,
    field_n_choice = 1:100
  ),
  dataSelection_dual2 = list(
    factor_choice = 1,
    field_n_choice = 1:100
  ),
  
  # PV inputs
  outlier_function_choice = "Z-Score",
  pv_outlier_z_score_SD_choice = 3,
  pv_value_display_choice = TRUE,
  
  # Graph (BC, SP, CF) inputs
  plot_x_axis_choice = "definition",
  group_fill_choice = "Compound",
  
  x_axis_avg_sp2_choice = "definition",
  
  cf_selection_choice = list(
    "Concentration" = list(),
    "Duration" = list(),
    "Concentration_2" = list()
  ),
  
  cf_selected = "",
  
  best_fit_function_choice = "sigEmax",
  
  multigraph_cf_choice = FALSE,
  
  errorbars_sp_choice = TRUE,
  
  data_points_choice = TRUE,
  data_points2_choice = TRUE,
  
  include_sample_choice = c("Unknown","Negative Control","Positive Control"),
  include_sample2_choice = c("Unknown","Negative Control","Positive Control"),
  
  y_axis = list(
    min = NA,
    max = NA
  ),
  
  y_axis2 = list(
    min = NA,
    max = NA
  ),
  
  log_y_axis_choice = FALSE,
  
  exclude_filters = list(
    Column = NA
  ),
  exclude_index = list(),
  
  exclude_filters2 = list(
    Column = NA
  ),
  exclude_index2 = list(),
  
  pv_customization = modifyList(customizationList, list(
    data_txt_sz_choice = 12,
    cp_scale_n_choice = "2-colour",
    cp_auto_val = "auto",
    cp_text_colr_choice = "white",
    cp_min_colr_choice = "#132B43",
    cp_max_colr_choice = "#56B1F7"
    
  )),
  
  bc_customization = modifyList(customizationList, list(
    x_label_angle_choice = 45,
    data_pt_sz_choice = 1.5
  )),
  
  sp_customization = modifyList(customizationList, list(
    legend_key_sz_choice = 1.5,
    data_pt_sz_choice = 1.5
  )),
  
  cf_customization = modifyList(customizationList, list(
    legend_key_sz_choice = 1.5,
    legend_position_choice = "bottom",
    data_pt_sz_choice = 1.5,
    ic_show_val_choice = TRUE,
    ic_show_threshold_choice = TRUE,
    ic_name_choice = ""
  )),
  
  zf_customization = modifyList(customizationList, list(
    data_txt_sz_choice = 12
  )),
  
  bc2_customization = modifyList(customizationList, list(
    data_pt_sz_choice = 1.5
  )),
  
  sp2_customization = modifyList(customizationList, list(
    legend_key_sz_choice = 1.5,
    data_pt_sz_choice = 1.5
  )),
  
  cf2_customization = modifyList(customizationList, list(
    legend_key_sz_choice = 1.5,
    data_pt_sz_choice = 1.5
  ))
)

#### Excluded Plate Layout Definitions ####
excluded_plate_layout <- reactive({
  pl <- plate_layout_def_cf()
  pl <- pl[which(!is.na(pl$Sample)), ]  # remove rows with no definitions
  # ex_pos <- match("Excluded", names(pl))
  # cols <- (Sample_col - 1):(ex_pos - 1) # select columns from 'definition' up to but not including 'Excluded'
  
  # Set excluded well definitions to NA
  # pl[which(pl$Exclude), cols] <- NA
  pl[which(pl$Exclude), -match(c(Base_defs, "Excluded"), names(pl))] <- NA
  
  # Remove Excluded column
  pl$Excluded <- NULL
  
  return(pl)
})

#### Layout and Data Info controls ####
layout_info <- reactive({
  pl <- excluded_plate_layout()
  lcn <- names(pl)
  layout_sample <- pl$Sample
  p_ctrl <- "Positive Control" %in% layout_sample
  n_ctrl <- "Negative Control" %in% layout_sample
  u_ctrl <- "Uninfected Control" %in% layout_sample
  def_opts <- lapply(pl, unique)  # find all unique plate layout definitions for each layout input field
  names(def_opts) <- lcn
  
  return(
    list(
      col_names = lcn,
      def_names = lcn[!lcn %in% c("Row", "Column", "Well", "Layout", "plate")],
      # numeric_names = lcn[lcn %in% Numeric_defs],
      numeric_names = numeric_col_names_fn(pl),
      pos_ctrl = p_ctrl,
      neg_ctrl = n_ctrl,
      uninfected_ctrl = u_ctrl,
      compare_options = Sample_controls[c(p_ctrl, n_ctrl, u_ctrl, u_ctrl)],
      definition_options = def_opts,
      mult_ctrl_regions = length(def_opts$Control_Region) > 1
    )
  )
})

data_info <- reactive({
  dat <- hcs_data()
  layout_info <- layout_info()
  # numeric_col_ind <- as.logical(lapply(dat, is.numeric))
  # numeric_col_names <- names(dat[, -ign])[which(numeric_col_ind)]
  # numeric_col_names <- numeric_col_names[!numeric_col_names %in% c("Plate", "Row", "Column", "Well", "plate", "Field")]
  
  plt_n <- unique(dat$plate)
  multiplate <- length(plt_n) > 1
  
  fld_inc <- "Field" %in% names(dat)
  ref_ctrl = sv$show_all_analysis_ph_choice || any(grepl("%(HIGH_|LOW_)(?=.*Ch2)(?!.*(Ratio|Diff))", data_col_names(), perl = TRUE))
  
  if ("Plate" %in% names(dat)) {
    ttl <- unique(dat$Plate)  # Extract HCS data Plate title
    hcs <- "TFCI"
  } else {
    ttl <- unique(dat$plate)  # return plate number if no title
    hcs <- "other"
  }
  
  if (ref_ctrl) {
    analysis_opt <- analysisFuncs_csv[-1, 1:2]
  } else {
    analysis_opt <- analysisFuncs_csv[which(analysisFuncs_csv$filter_analysis_input == "F"), 1:2]
  }
  
  if (fld_inc) {
    fld_n <- unique(dat$Field[!is.na(dat$Field)])
  } else {
    fld_n <- NULL
  }
  
  return(
    list(
      platform = hcs,
      title = ttl,
      # ori_col_names = numeric_col_names,
      ori_col_names = numeric_col_names_fn(dat),
      multiplate_incl = multiplate,
      plate_n_options = plt_n,
      fields_incl = fld_inc,
      field_n_options = fld_n,
      ref_incl = ref_ctrl,
      analysis_options = analysis_opt
    )
  )
})

#### Field Included Check and Set valid input options ####
output$fields_included_output <- reactive({
  return(data_info()$fields_incl)
})
outputOptions(output, "fields_included_output", suspendWhenHidden = FALSE)


#### Current tab ####
current_tab <- reactive({
  list(navbar = input$navbar,
       active = ifelse(input$navbar == "Analysis", input$tabset_panel, input$tabset_panel_dual),
       single = input$tabset_panel,
       dual = input$tabset_panel_dual
  )
})

#### UI Data Selection Module ####
input_dataSelection <- dataSelection_moduleServer(
  id = "dataSelection",
  sv = sv,
  data_col_names = data_col_names,
  layout_info = layout_info,
  data_info = data_info
)


#### UI Multiplate Input options ####
output$multiplate_n <- renderUI({
  selectInput(inputId = "multiplate_n",
              label = "Plate",
              choices = plate_n_options(),
              multiple = TRUE,
              selected = isolate(sv$multiplate_n_choice))
})
outputOptions(output, "multiplate_n", suspendWhenHidden = FALSE) # without this line, bug where brushedPoints and conditionalPanel to hide fields in module will not work 

observeEvent(debounce(reactive({
  input$multiplate_n
}), 300)(), {
  if (data_info()$multiplate_incl) {  # required to reset multiplate value when loading a single plate after loading multiplate data
    pn <- input$multiplate_n
  } else {
    pn <- 1
  }
  
  sv$multiplate_n_choice <<- as.integer(pn)
})

output$multiplate_add_all <- renderUI({
  actionButton("multiplate_add_all",
               label = "All")
})

observeEvent(input$multiplate_add_all, {
  sv$multiplate_n_choice <<- data_info()$plate_n_options
  updateSelectInput(session, "multiplate_n", selected = data_info()$plate_n_options)
})

# Show Plate Slider option and Average by Plate checkbox option for BC SP DR tabs
output$plate_slider_avg_tl <- renderUI({
    tagList(
      tags$div(style="display:inline-block; width:49%; margin-top: -5%",
               checkboxInput("show_plate_slider", "Plate Slider", value = isolate(sv$show_plate_slider_choice))),
      
      tags$div(title = "Replicates on each plate are averaged first and plotted as a single data point - useful for plotting biological replicates",
               style="display:inline-block; width:49%; margin-top: -5%",
               checkboxInput("plate_avg", label = "Avg by plate",
                             value = isolate(sv$plate_avg_choice)))
    )
})
# Save Show Plate Slider input
observeEvent(input$show_plate_slider, {sv$show_plate_slider_choice <<- input$show_plate_slider})

# Save multiplate average option
observeEvent(input$plate_avg, {sv$plate_avg_choice <<- input$plate_avg})

# Plate input slider bar for easy graphing of different plates
output$plate_slider <- renderUI({
    req(input$show_plate_slider)
    if (length(sv$multiplate_n_choice) > 1 & input$show_plate_slider) {
      Min <- min(as.numeric(sv$multiplate_n_choice))
      Max <- max(as.numeric(sv$multiplate_n_choice))
      
      sliderInput("plate_slider",
                  label = NULL,
                  min = Min,
                  max = Max,
                  step = 1,
                  value = isolate(sv$plate_slider_choice)
      )
    }
})

# Save Plate Slider input option
observeEvent(input$plate_slider, {sv$plate_slider_choice <<- input$plate_slider})

#### Show Data table for download ####
output$show_data_table <- renderUI({
    checkboxInput("show_data_table", label = "Show data table", value = isolate(sv$show_data_table_choice))
})
# Save Show Data Table selection
observeEvent(input$show_data_table, {sv$show_data_table_choice <<- input$show_data_table})

# Pivot Data table wide for copy-paste into GraphPad Prism, long for full layout definitions
output$data_table_format <- renderUI({
    radioButtons("data_table_format", label = NULL,
                 choices = c("Wide", "Long"),
                 selected = isolate(sv$data_table_format_choice),
                 inline = TRUE)
})
# Save Data Table Format selection
observeEvent(input$data_table_format, {sv$data_table_format_choice <<- input$data_table_format})

# Set T/F for selected Data table wide/long format
data_table_format_wide <- reactive({
  if (is.null(input$data_table_format)) {return(TRUE)}
  input$data_table_format == "Wide"
})

#### UI for Plate View ####
# Show Outliers
output$pv_outlier <- renderUI({
  if (input$tabset_panel == "Plot Helper" & mutate_null_fn(input$graph_type_ph) != "Plate View") {return(NULL)}
    checkboxInput("pv_outlier", label = "Highlight Outliers", value = isolate(sv$pv_outlier_choice))
})
outputOptions(output, "pv_outlier", suspendWhenHidden = FALSE)
# Save Highlight Outliers selection
observeEvent(input$pv_outlier, {sv$pv_outlier_choice <<- input$pv_outlier})

output$pv_outlier_function <- renderUI({
  selectInput("pv_outlier_function", label = "Method",
              choices = c("Z-Score", "Interquartile Range"),  #, "Hampel Filter"),
              selected = isolate(sv$pv_outlier_function_choice)
  )
})
outputOptions(output, "pv_outlier_function", suspendWhenHidden = FALSE)

output$pv_outlier_z_score_SD <- renderUI({
  tags$div(title = "Standard deviations away from the mean",
           numericInput("pv_outlier_z_score_SD", label = "SD",
                        value = isolate(sv$pv_outlier_z_score_SD_choice)
           )
  )
})

# Save Outlier function selection
observeEvent(input$pv_outlier_function, {sv$pv_outlier_function_choice <<- input$pv_outlier_function})
# Save Outlier z-score SD selection
observeEvent(input$pv_outlier_z_score_SD, {sv$pv_outlier_z_score_SD_choice <<- input$pv_outlier_z_score_SD})

output$pv_outlier_group <- renderUI({
  radioButtons("pv_outlier_group", label = NULL,
               choices = c("by plate", "by definition"),
               selected = isolate(sv$pv_outlier_group_choice),
               inline = TRUE)
})

output$pv_outlier_exclude_edge_wells <- renderUI({
  checkboxInput("pv_outlier_exclude_edge_wells", label = "Exclude edge wells",
                value = isolate(sv$pv_outlier_exclude_edge_wells_choice))
})
# Save Outlier Group selection
observeEvent(input$pv_outlier_group, {sv$pv_outlier_group_choice <<- input$pv_outlier_group})
observeEvent(input$pv_outlier_exclude_edge_wells, {sv$pv_outlier_exclude_edge_wells_choice <<- input$pv_outlier_exclude_edge_wells})

output$pv_checkbox_inputs_tl <- renderUI({
  tagList(
    checkboxInput("pv_value_display", label = "Display Values", value = isolate(sv$pv_value_display_choice)),
    checkboxInput("pv_average", label = "Display Averages", value = isolate(sv$pv_average_choice)),
    checkboxInput("pv_ignore_empty", label = "Ignore Empty Definitions", value = isolate(sv$pv_ignore_empty_choice)),
    checkboxInput("pv_ignore_uninfected", label = "Ignore Uninfected Wells", value = isolate(sv$pv_ignore_uninfected_choice))
  )
})

# Save Display selection
observeEvent(input$pv_value_display, {sv$pv_value_display_choice <<- input$pv_value_display})
# Save Average selection
observeEvent(input$pv_average, {sv$pv_average_choice <<- input$pv_average})
# Save Ignore Empty selection
observeEvent(input$pv_ignore_empty, {sv$pv_ignore_empty_choice <<- input$pv_ignore_empty})
# Save Ignore Uninfected selection
observeEvent(input$pv_ignore_uninfected, {sv$pv_ignore_uninfected_choice <<- input$pv_ignore_uninfected})

output$show_pv2 <- renderUI({
  checkboxInput("show_pv2", "Show 2nd Plot", value = isolate(sv$show_pv2_choice))
})
outputOptions(output, "show_pv2", suspendWhenHidden = FALSE)
observeEvent(input$show_pv2, {sv$show_pv2_choice <<- input$show_pv2})

pv_all_inputs <- reactive({
  req(is.logical(input$pv_value_display))
  list(
    pv_outlier = input$pv_outlier,
    pv_outlier_function = input$pv_outlier_function,
    pv_outlier_z_score_SD = input$pv_outlier_z_score_SD,
    pv_outlier_group = input$pv_outlier_group,
    pv_value_display = input$pv_value_display,
    pv_average = input$pv_average,
    pv_ignore_empty = input$pv_ignore_empty,
    pv_ignore_uninfected = input$pv_ignore_uninfected
  )
})

pv_default_inputs = reactive({
  list(
    pv_outlier = FALSE,
    pv_average = FALSE,
    pv_ignore_empty = FALSE,
    pv_ignore_uninfected = FALSE
  )
})

#### UI for X-axis and Group by inputs ####
output$plot_x_axis <- renderUI({
  selectInput("plot_x_axis", "X-axis:",
              choices = c(if (data_info()$multiplate_incl) {"plate"}, layout_info()$def_names),  #layout_col_names()[-(1:5-as.integer(multiplate()))],
              multiple = FALSE,
              selected = isolate(sv$plot_x_axis_choice)
  )
})

output$group_fill <- renderUI({
  selectInput("group_fill", "Group by:",
              choices = c("plate", layout_info()$def_names),
              multiple = FALSE,
              selected = isolate(sv$group_fill_choice)
  )
})

# Save X-axis selection
observeEvent(input$plot_x_axis, {
  sv$plot_x_axis_choice <<- input$plot_x_axis
  if (input$tabset_panel == "Scatter Plot" & input$plot_x_axis %in% layout_info()$numeric_names) {
  # if (input$tabset_panel == "Scatter Plot" & input$plot_x_axis %in% Numeric_defs) {
    shinyjs::enable("log_x_axis_sp")
  } else {
    shinyjs::disable("log_x_axis_sp")
  }
})
# Save group fill selection
observeEvent(input$group_fill, {sv$group_fill_choice <<- input$group_fill})



#### Adding multi group ####
output$multigroup <- renderUI({
  tags$div(
    title = "Select up to two additional definitions to group data by and display in a grid of plots",
    selectizeInput("multigroup", "Multigroup:",
                   choices = c("plate", layout_info()$def_names),
                   multiple = TRUE,
                   options = list(maxItems = 2),
                   selected = isolate(sv$multigroup_choice)
    )
  )
})

observeEvent(input$multigroup, {sv$multigroup_choice <<- input$multigroup})

output$plot_x_axis_cf <- renderUI({
  selectInput("plot_x_axis_cf", "X-axis:",
              choices = layout_info()$numeric_names,
              selected = isolate(sv$plot_x_axis_cf_choice)
  )
})
outputOptions(output, "plot_x_axis_cf", suspendWhenHidden = FALSE)

output$group_fill_cf <- renderUI({
  opt <- names(cf_selection_options()[[req(input$plot_x_axis_cf)]])
  
  selectInput("group_fill_cf", "Group by:",
              choices = opt,
              selected = isolate(sv$group_fill_cf_choice)
  )
})
outputOptions(output, "group_fill_cf", suspendWhenHidden = FALSE)
# Save X-axis selection
observeEvent(input$plot_x_axis_cf, {sv$plot_x_axis_cf_choice <<- input$plot_x_axis_cf})
# Save group by selection
observeEvent(input$group_fill_cf, {sv$group_fill_cf_choice <<- input$group_fill_cf})

# Select layout definitions to plot
output$cf_selection <- renderUI({
  gf <- req(input$group_fill_cf)
  px <- req(input$plot_x_axis_cf)

  selectInput("cf_selection", "Selection",
              choices = cf_selection_options()[[px]][[gf]],
              multiple = TRUE,
              selected = isolate(sv$cf_selection_choice[[px]][[gf]])
  )
})

observeEvent(input$cf_selection, {
  gf <- req(input$group_fill_cf)
  px <- req(input$plot_x_axis_cf)
  
  sv$cf_selection_choice[[px]][[gf]] <<- input$cf_selection
}, ignoreNULL = FALSE)

cf_selected <- reactive({
  opts <- cf_selection_options()[[req(input$plot_x_axis_cf)]][[req(input$group_fill_cf)]]
  
  if (length(opts) > 1) {
    mutate_null_fn(input$cf_selection, opts)
  } else {
    opts
  }
})

#### NEW UI for Errorbars, Checkbox inputs, and Threshhold lines ####
output$errorbars <- renderUI({
  radioButtons("errorbars",
               label = "Error Bars",
               choices = c("SD", "SEM", "CL", "None"),
               selected = isolate(sv$errorbars_choice),
               inline = TRUE
  )
})
# Save errorbar display option
observeEvent(input$errorbars, {sv$errorbars_choice <<- input$errorbars})

output$plot_type_bc <- renderUI({
  tags$div(title = "'Bar' and 'Line' display the mean.",
           style = "display: flex; align-items: center; gap: 10px;",
           tags$div(style = "font-size: 10pt; font-weight: bold; margin-top: -5%;", "Plot:"),
           radioButtons("plot_type_bc", label = NULL,
                        choices = c("Bar", "Box", "Violin", "Line"),
                        selected = isolate(sv$plot_type_bc_choice),
                        inline = TRUE)
  )
})

observeEvent(input$plot_type_bc, {sv$plot_type_bc_choice <<- input$plot_type_bc})

output$connecting_line_sp_tl <- renderUI({
  tagList(
    div(style="display:inline-block; margin-top: -4%; width:4%",
             checkboxInput("connecting_line_sp", label = NULL, value = isolate(sv$connecting_line_sp_choice))),
    div(class = "tightSelect",
        # style = "margin-left: -2px; margin-right: 10px; ",
             style="display:inline-block; margin-top: -1%; width:44%",
             selectizeInput("connecting_line_menu", label = NULL, choices = c("Connecting Line", "Smooth Line"))),
    div(
      style="display:inline-block; margin-top: -5%; width:49%",
             checkboxInput("log_x_axis_sp", label = "Log X-axis", value = isolate(sv$log_x_axis_sp_choice))
    )
  )
})

# Save connecting line display option
observeEvent(input$connecting_line_sp, {sv$connecting_line_sp_choice <<- input$connecting_line_sp})
# Save log X scale
observeEvent(input$log_x_axis_sp, {sv$log_x_axis_sp_choice <<- input$log_x_axis_sp})

output$prediction_band_cf_tl <- renderUI({
  tagList(
    tags$div(style="display:inline-block; width: 49%; margin-top: -5%",
             checkboxInput("prediction_band_cf", label = "Prediction Band", value = isolate(sv$prediction_band_cf_choice))),
    
    if (length(cf_selected()) > 1) {
      tags$div(style="display:inline-block; width: 49%; margin-top: -5%",
               checkboxInput("multigraph_cf", label = "Separate graphs", value = isolate(sv$multigraph_cf_choice)))
    }
  )
})
# Save prediction band option
observeEvent(input$prediction_band_cf, {sv$prediction_band_cf_choice <<- input$prediction_band_cf})
# Save multigraph options
observeEvent(input$multigraph_cf, {sv$multigraph_cf_choice <<- input$multigraph_cf})

output$data_points_tl <- renderUI({
  tagList(
    tags$div(title = "Show all data points. Unchecked shows the mean.",
             style="display:inline-block; width:49%; margin-top: -5%",
             checkboxInput("data_points", label = "Data Points", value = TRUE)),#isolate(sv$data_points_choice))),
    tags$div(style="display:inline-block; width:49%; margin-top: -5%",
             checkboxInput("log_y_axis", label = "Log Y-axis", value = isolate(sv$log_y_axis_choice)))
    
  )
})
# Save data point selection option
observeEvent(input$data_points, {sv$data_points_choice <<- input$data_points})
# Save y log scale
observeEvent(input$log_y_axis, {sv$log_y_axis_choice <<- input$log_y_axis})

# Dropdown menu of non-linear curve fitting functions from DoseFinding package
output$best_fit_function <- renderUI({
    selectInput("best_fit_function", "Best-Fit Function",
                choices = c("sigEmax", "logistic", "emax", "linlog", "linear", "quadratic", "linInt", "exponential", "betaMod"),
                selected = isolate(sv$best_fit_function_choice)
    )
})
# Save best fit curve function choice
observeEvent(input$best_fit_function, {sv$best_fit_function_choice <<- input$best_fit_function})

output$threshold_line <- renderUI({
  textInput("threshold_line", label = "Threshold", value = isolate(sv$threshold_line_choice))
})
# Save threshold
observeEvent(input$threshold_line, {sv$threshold_line_choice <<- input$threshold_line})

#### Manual Y Range ####
output$y_axis_min <- renderUI({
  numericInput("y_axis_min", "Min Y-axis", value = isolate(sv$y_axis$min))
})

observeEvent(debounce(reactive({
  input$y_axis_min
}), 400)(), {
  sv$y_axis$min <<- input$y_axis_min
})

output$y_axis_max <- renderUI({
  numericInput("y_axis_max", "Max Y-axis", value = isolate(sv$y_axis$max))
})

observeEvent(debounce(reactive({
  input$y_axis_max
}), 400)(), {
  
  sv$y_axis$max <<- input$y_axis_max
})
#### Check for valid Y-axis manual range values ####
manual_y_range <- reactiveVal(FALSE)

observeEvent(list(sv$y_axis, input$log_y_axis), {
  if (isTruthy(input$y_axis_min) & isTruthy(input$y_axis_max)) {
    current_val <- isolate(manual_y_range())
    if ((input$y_axis_min < input$y_axis_max & ((!input$log_y_axis) || (input$y_axis_min > 0 & input$y_axis_max > 0))) != current_val) {
      manual_y_range(!current_val)
    }
  } else {
    manual_y_range(FALSE)
  }
}, ignoreNULL = FALSE)

#### UI for Include Sample and Exclude Filters ####
output$include_sample <- renderUI({
    selectInput("include_sample", label = "Select Sample types to display",
                choices = layout_info()$definition_options$Sample,
                multiple = TRUE,
                selected = isolate(sv$include_sample_choice))
})
# Save Sample type inclusions
observeEvent(input$include_sample, {sv$include_sample_choice <<- input$include_sample})

#### NEW EXCLUDE HANDLING ####
output$data_filters <- renderUI({
  lcn <- layout_info()$col_names
  selectInput("data_filters", label = "Filter data by:",
              choices = lcn[!lcn %in% c("Layout", "plate", "Sample")],
              multiple = TRUE,
              selected = isolate(sv$data_filters_choice)
  )
})
outputOptions(output, "data_filters", suspendWhenHidden = FALSE)

observeEvent(input$data_filters, {
  # identify removed input
  dif <- base::setdiff(sv$data_filters_choice, input$data_filters)
  
  sv$data_filters_choice <<- input$data_filters
  
  # Clear exclude filters for removed input data filters
  if (length(dif) == 1) {
    sv$exclude_index[[dif]] <- NULL
    sv$exclude_filters[[dif]] <- NULL
    updateSelectInput(session, paste0("exclude_filters_", dif), selected = "")
  }
}, ignoreNULL = FALSE)

# Dynamically create renderUI components for Exclude Filters
lapply(exclude_filters_cols, function(col) {
  output[[paste0("exclude_filters_", col)]] <- renderUI({
    selectInput(
      inputId = paste0("exclude_filters_", col), 
      label = paste("Exclude", col),
      choices = layout_info()$definition_options[[col]], 
      multiple = TRUE,
      selected = isolate(sv$exclude_filters[[col]])
    )
  })
})

# Dynamically create observers with debounce
lapply(exclude_filters_cols, function(col) {
  observeEvent(debounce(reactive({    # OBSERVE NOT WORKING WHEN CLEARING THE FIELD. use ignoreNULL = FALSE
    input[[paste0("exclude_filters_", col)]]
  }), 400)(), {
    input_excl <- input[[paste0("exclude_filters_", col)]]
    
    sv$exclude_filters[[col]] <- input_excl
    
    # Index rows of excluded values
    if (length(input_excl) == 0) {
      sv$exclude_index[[col]] <- NULL
    } else {
      input_excl[input_excl == "NA"] <- NA
      sv$exclude_index[[col]] <- which(!data_merged()[[col]] %in% input_excl)
    }
    
  }, ignoreNULL = FALSE, ignoreInit = TRUE)
})

# Dynamically generate conditional panels to display Excluded Filter columns
output$exclude_filters_tl <- renderUI({
  tagList(
    lapply(exclude_filters_cols, function(col) {
      conditionalPanel(
        condition = sprintf("input.data_filters.includes('%s')", col),
        uiOutput(paste0("exclude_filters_", col))
      )
    })
  )
})

sv_exclude_index <- reactive(sv$exclude_index)

#### BC SP CF Module Plot Inputs ####
graph_inputs <- reactive({
  list(
    plate_avg = input$plate_avg,
    include_sample = input$include_sample
  )
})

bc_sp_x_inputs <- reactive({
  list(
    plot_x_axis = input$plot_x_axis,
    group_fill = input$group_fill,
    multigroup_x = input$multigroup[1],
    multigroup_y = input$multigroup[2]
  )
})

cf_x_inputs <- reactive({
  list(
    plot_x_axis = input$plot_x_axis_cf,
    group_fill = input$group_fill_cf
  )
})

#### Merge HCS data and Plate Layout dataframes ####
# Merge Plate Layout with HCS data by plate and well
layout_data_merged <- reactive({
  dat <- hcs_data()
  p <- match("Plate", names(dat))
  if (!is.na(p)) {
    dat <- dat[, -p]
  }
  
  merged <- dat |>
    left_join(excluded_plate_layout(), by = c("plate", "Well", "Row", "Column"), relationship = "many-to-many") |>
    arrange(plate, Row, Column) |>
    relocate(layout_info()$col_names)
  
  return(merged)
})

# Set up reactive values for merged data and layout and data measurement headers
current_merged <- reactiveValues(
  data = data.frame(),
  headers = vector()
)

observeEvent(layout_data_merged(), {
  current_merged$data <- isolate(layout_data_merged())
  current_merged$headers <- data_info()$ori_col_names
})

#### Append Data - Update HCS data ####
# Run analysis on entire data using the PV data Wrangling module without PV options
analysed_full_data <- PVdataWrang_moduleServer(
  "append", # module requires this id to be 'append' in order to include all plates and all fields
  data_merged,
  input_dataSelection,
  pv_inputs = pv_default_inputs,
  layout_info,
  ctrl_data,
  ctrl_sd_data,
  uninf_def_ctrl_data)

observeEvent(input[["dataSelection-append_data_button"]], {
  merged_dat <- isolate(current_merged$data)
  
  #Update merged data with new analysed values
  col_names <- isolate(current_merged$headers)
  new_col_name <- isolate(input[["dataSelection-append_data_name"]])
  
  if (!new_col_name %in% names(merged_dat)) {
    if (data_info()$fields_incl) {
      fldn <- "Field"
    } else {
      fldn <- NULL
    }
    append_dat <- analysed_full_data()[, c("Well", "plate", fldn, "Value")]
    names(append_dat)[names(append_dat) == "Value"] <- new_col_name
    new_merged_dat <- left_join(merged_dat, append_dat, by = c("Well", "plate", fldn))
    
    current_merged$data <- new_merged_dat
    current_merged$headers <- c(col_names, new_col_name)
  }
  
  # Update Raw Data with new appended data. Clear Analysis field and Transform field
  updateSelectInput(session, "dataSelection-raw", selected = new_col_name)
  shinyWidgets::updatePickerInput(session, "dataSelection-analysis", selected = "")
  updateSelectInput(session, "dataSelection-transform", choices = c("factor", "reference", "definition", "NULL"), selected = "NULL")
})

data_merged <- reactive({
  return(current_merged$data)
})

data_col_names <- reactive({
  return(current_merged$headers)
})

#### Control Data Summarized ####
ctrl_data <- reactive({
  dat <- data_merged()
  ctrl <- dat[which(dat$Sample %in% c("Positive Control", "Negative Control", "Uninfected Control")), match(c("plate", "Sample", "Control_Region", data_col_names()), names(dat))] 
  ctrl$control_avgs <- FALSE
  
  ctrl_mean <- ctrl |>
    group_by(plate, Sample, Control_Region) |>
    dplyr::summarize(across(all_of(data_col_names()) & where(is.numeric),
                     ~ mean(., na.rm = TRUE)), .groups = 'drop')
  ctrl_mean$control_avgs <- TRUE

  return(bind_rows(ctrl_mean, ctrl))
})

ctrl_sd_data <- reactive({
  dat <- data_merged()
  ctrl <- dat[which(dat$Sample %in% c("Positive Control", "Negative Control")), ] |>
    group_by(plate, Sample, Control_Region) |>
    dplyr::summarize(across(all_of(data_col_names()) & where(is.numeric),
                     ~ sd(., na.rm = TRUE)), .groups = 'drop')
  ctrl$control_avgs <- TRUE
  return(ctrl)
})

uninf_def_ctrl_data <- reactive({
  dat <- data_merged()
  ctrl <- dat[which(dat$Sample == "Uninfected Control"), ] |>
    group_by(plate, definition, Control_Region) |>
    dplyr::summarize(across(all_of(data_col_names()) & where(is.numeric),
                     ~ mean(., na.rm = TRUE)), .groups = 'drop')
  return(ctrl)
})

#### Customization Module Server ####
# Set up reactive values for graph customization inputs (module)
custom_inputs <- reactiveValues(
  pv = isolate(sv$pv_customization),
  bc = isolate(sv$bc_customization),
  sp = isolate(sv$sp_customization),
  cf = isolate(sv$cf_customization),
  zf = isolate(sv$zf_customization),
  bc2 = isolate(sv$bc2_customization),
  sp2 = isolate(sv$sp2_customization),
  cf2 = isolate(sv$cf2_customization)
)

customization_moduleServer("pv_customization",
                           current_tab = current_tab,
                           sv = sv,
                           plate_layout_def = plate_layout_def,
                           plot_image = pv_plot,
                           plot_dim = reactive(plot_dims_fn(custom_inputs, input$tabset_panel)))

customization_moduleServer("bc_customization",
                           current_tab = current_tab,
                           sv = sv,
                           plate_layout_def = plate_layout_def,
                           x_inputs = bc_sp_x_inputs,
                           plot_image = bc_plot,
                           plot_dim = reactive(plot_dims_fn(custom_inputs, input$tabset_panel)))

customization_moduleServer("sp_customization",
                           current_tab = current_tab,
                           sv = sv,
                           plate_layout_def = plate_layout_def,
                           x_inputs = bc_sp_x_inputs,
                           plot_image = sp_plot,
                           plot_dim = reactive(plot_dims_fn(custom_inputs, input$tabset_panel)))

customization_moduleServer("cf_customization",
                           current_tab = current_tab,
                           sv = sv,
                           plate_layout_def = plate_layout_def,
                           x_inputs = cf_x_inputs,
                           plot_image = cf_plot,
                           plot_dim = reactive(plot_dims_fn(custom_inputs, input$tabset_panel)))

customization_moduleServer("zf_customization",
                           current_tab = current_tab,
                           sv = sv,
                           plate_layout_def = plate_layout_def,
                           plot_image = zf_plot,
                           plot_dim = zf_dims)

observeEvent(input[["pv_customization-customization_apply"]], {
  custom_inputs$pv <- sv$pv_customization
})

observeEvent(input[["bc_customization-customization_apply"]], {
  custom_inputs$bc <- sv$bc_customization
})

observeEvent(input[["sp_customization-customization_apply"]], {
  custom_inputs$sp <- sv$sp_customization
})

observeEvent(input[["cf_customization-customization_apply"]], {
  custom_inputs$cf <- sv$cf_customization
})

observeEvent(input[["zf_customization-customization_apply"]], {
  custom_inputs$zf <- sv$zf_customization
})

# Reset custom order when changing input (or get empty graph)
observeEvent(input$plot_x_axis, {
  custom_inputs$bc$x_axis_order_choice <- NULL
  custom_inputs$sp$x_axis_order_choice <- NULL
})

observeEvent(input$group_fill, {
  custom_inputs$bc$group_order_choice <- NULL
  custom_inputs$sp$group_order_choice <- NULL
})

observeEvent(input$multigroup[1], {
  custom_inputs$bc$multigroup_x_order_choice <- NULL
  custom_inputs$sp$multigroup_x_order_choice <- NULL
})

observeEvent(input$multigroup[2], {
  custom_inputs$bc$multigroup_y_order_choice <- NULL
  custom_inputs$sp$multigroup_y_order_choice <- NULL
})

#### PV Module data selection ####
input_dataSelection_pv2 <- dataSelection_moduleServer(
  id = "dataSelection_pv2",
  sv = sv,
  data_col_names = data_col_names,
  layout_info = layout_info,
  data_info = data_info
)

pv_data <- PVdataWrang_moduleServer(
  "pv1",
  data_merged,
  input_dataSelection,
  pv_all_inputs,
  layout_info,
  ctrl_data,
  ctrl_sd_data,
  uninf_def_ctrl_data)

pv_data2 <- PVdataWrang_moduleServer(
  "pv2",
  data_merged,
  input_dataSelection_pv2,
  pv_all_inputs,
  layout_info,
  ctrl_data,
  ctrl_sd_data,
  uninf_def_ctrl_data)

#### Exclude Outlier/Edge Wells ####
observeEvent(input$pv_outlier_exclude_wells, {
  pl <- isolate(current_data$layout)
  dat <- pv_data()
  
  exclude_wells <- dat[which(dat$outliers == TRUE & !is.na(dat$Sample)), "Well"]
  
  selected_rows <- which(pl$plate == input_dataSelection()$pv_plate_n & pl$Well %in% exclude_wells)
  
  current_data$layout$Excluded[selected_rows] <- !pl$Excluded[selected_rows]
})

observeEvent(input$pv_outlier_unexclude_all_wells, {
  pl <- current_data$layout
  
  # Do not unexclude edge wells if Exclude edge wells checkbox checked
  if (input$pv_outlier_exclude_edge_wells) {
    ind <- which(pl$plate == input_dataSelection()$pv_plate_n & pl$Column != 1 & pl$Row != "A" & pl$Column != max(col_size()) & pl$Row != LETTERS[row_size()])
  } else {
    ind <- which(pl$plate == input_dataSelection()$pv_plate_n)
  }
  
  current_data$layout$Excluded[ind] <- FALSE
})

observeEvent(input$pv_outlier_exclude_edge_wells, {
  pl <- isolate(current_data$layout)
  
  edge_wells <- which(pl$Column == 1 | pl$Row == "A" | pl$Column == max(col_size()) | pl$Row == LETTERS[row_size()])
  
  current_data$layout$Excluded[edge_wells] <- input$pv_outlier_exclude_edge_wells
})

#### PV Plate View Heatmap plot ####
pv_plot <- reactive({
  pv_plot_fn(
    DAT = pv_data(),
    DIS = mutate_null_fn(input$pv_value_display),
    SUBT = data_info()$title[as.integer(input_dataSelection()$pv_plate_n)],  # Requires "Plate" to be in the HCS data file
    CI = custom_inputs$pv,
    RW = row_size(),
    CL = col_size(),
    OUT = input$pv_outlier
  )
})

pv_plot2 <- reactive({
  pv_plot_fn(
    DAT = pv_data2(),
    DIS = mutate_null_fn(input$pv_value_display),
    SUBT = data_info()$title[as.integer(input_dataSelection()$pv_plate_n)],  # Requires "Plate" to be in the HCS data file
    CI = custom_inputs$pv,
    RW = row_size(),
    CL = col_size()
  )
})
 
pv_dims <- reactive({
  list(mutate_null_fn(custom_inputs$pv$graph_width_choice, "auto"), mutate_null_fn(custom_inputs$pv$graph_height_choice, 400))
})

output$render_pv_plot <- renderPlot({
  pv_plot()
}, width = exprToFunction(pv_dims()[[1]]), height = exprToFunction(pv_dims()[[2]]))

output$pv2_spacer <- renderUI({
  brs <- max(ceiling((pv_dims()[[2]] - 400) / 20), 0)
  tagList(replicate(brs, br(), simplify = FALSE))
})

output$render_pv_plot2 <- renderPlot({
  pv_plot2()
}, width = exprToFunction(pv_dims()[[1]]), height = exprToFunction(pv_dims()[[2]]))

#### BC SP CF Data Wrangle Module ####
bc_sp_data_wrang <- GraphDataWrang_moduleServer(
    id = "bc_sp_data_wrang",
    sv_exclude_index = sv_exclude_index,
    data_merged = data_merged,
    multiplate_n = reactive(sv$multiplate_n_choice),
    input_dataSelection = input_dataSelection,
    graph_inputs = graph_inputs,
    x_inputs = bc_sp_x_inputs,
    layout_info = layout_info,
    data_info = data_info,
    ctrl_data = ctrl_data,
    ctrl_sd_data = ctrl_sd_data,
    uninf_def_ctrl_data = uninf_def_ctrl_data)

cf_data_wrang <- GraphDataWrang_moduleServer(
  id = "cf_data_wrang",
  sv_exclude_index = sv_exclude_index,
  data_merged = data_merged,
  multiplate_n = reactive(sv$multiplate_n_choice),
  input_dataSelection = input_dataSelection,
  graph_inputs = graph_inputs,
  x_inputs = cf_x_inputs,
  layout_info = layout_info,
  data_info = data_info,
  ctrl_data = ctrl_data,
  ctrl_sd_data = ctrl_sd_data,
  uninf_def_ctrl_data = uninf_def_ctrl_data)

#### BC Bar Chart plot ####
# Filter data using plate slider
single_plate_slider_data <- reactive({
  dat <- bc_sp_data_wrang()
  
  if (mutate_null_fn(input$show_plate_slider) & data_info()$multiplate_incl) {
    dat <- dat[which(dat$plate == req(input$plate_slider)), ]
  }
  return(dat)
})

# Define error bar width
errorbar_width <- reactive({
  dat <- single_plate_slider_data()
  xInput <- input$plot_x_axis
  fillInput <- input$group_fill
  # adjust error bar width when no grouping of data
  ew <- ifelse(
    sum(!duplicated(dat[xInput])) == sum(!duplicated(dat[c(xInput,fillInput)])),
    0.25,
    0.65
  )
  return(ew)
})

# Set colour palette for group fill
scalefill <- reactive({
  dat <- single_plate_slider_data()
  fill_num <- length(unique(dat[[input$group_fill]]))
  
  # selects scale fill palette based on number of colours needed
  if (fill_num < 9) {
    scalefill <- scale_fill_brewer(na.value = "dark grey")
  } else if (fill_num < 385) {
    scalefill <- scale_fill_manual(values = alt_colors384, na.value = "dark grey")
  } else {
    # repeat colours if more than 384 needed
    scalefill <- scale_fill_manual(values = rep(alt_colors384, ceiling(fill_num / 384)), na.value = "dark grey")
  }
  return(scalefill)
})

#### Bar Chart Plot ####
bc_plot <- reactive({
  req(!is.null(input$data_points), is.logical(input$log_y_axis), input$errorbars, input$plot_type_bc)

  dat <- single_plate_slider_data()
  if (nrow(dat) == 0) {return(NULL)}  # prevent error if data not loaded yet
  
  xInput <- input$plot_x_axis
  xAxisOrder <- custom_inputs$bc$x_axis_order_choice
  
  fillInput <- input$group_fill
  fillOrder <- custom_inputs$bc$group_order_choice
  
  dat[[xInput]] <- factor_fn(dat, xInput, xAxisOrder)
  dat[[fillInput]] <- req(factor_fn(dat, fillInput, fillOrder))
  
  dat <- dat[which(!is.na(dat[[xInput]]) & !is.na(dat[[fillInput]])), ]
  
  
  multigroup <- input$multigroup
  multigroup_x_order <- custom_inputs$bc$multigroup_x_order_choice
  multigroup_y_order <- custom_inputs$bc$multigroup_y_order_choice
  
  if (length(multigroup) > 0) {
    dat[[multigroup[1]]] <- factor_fn(dat, multigroup[1], multigroup_x_order)
    
    if (length(multigroup) > 1) {
      dat[[multigroup[2]]] <- factor_fn(dat, multigroup[2], multigroup_y_order)
      dat <- dat[which(!is.na(dat[[multigroup[1]]]) & !is.na(dat[[multigroup[2]]])), ]
    } else {
      dat <- dat[which(!is.na(dat[[multigroup[1]]])), ]
    }
  }
  
  # remove 0 and negative values when plotting log Y-axis
  if (isTruthy(input$log_y_axis)) {
    dat <- dat[which(dat$Value > 0), ]
  }
  
  y_title <- y_title_fn(custom_inputs$bc$y_title_choice, input_dataSelection()$raw, input_dataSelection()$analysis)
  
  p <- dat |>
    ggplot(aes(fill = interaction(.data[[fillInput]]), y = Value, x = .data[[xInput]])) +
    
    theme_labs_fn(
      custom_inputs$bc,
      YLAB = y_title,
      LEG = "fill"
    ) +
    
    scalefill()
  
  if (input$plot_type_bc == "Box") {
    p <- p +
      geom_boxplot(
        position=position_dodge2(preserve = "single"),
        width = 0.65,
        show.legend = custom_inputs$bc$show_legend_choice)
  } else if (input$plot_type_bc == "Violin") {
    p <- p +
      geom_violin(
        position = "dodge",
        trim = FALSE,
        # position=position_dodge2(preserve = "single"),
        width = 0.65,
        show.legend = custom_inputs$bc$show_legend_choice)
  
  } else if (input$plot_type_bc == "Bar") {
    p <- p +
      stat_summary(
        position=position_dodge2(preserve = "single"),
        fun = mean, geom = "bar", colour = "black", width = 0.65, show.legend = custom_inputs$bc$show_legend_choice)
  } else {  # Line
    p <- p +
      stat_summary(
        position=position_dodge2(preserve = "single"),
        fun = mean, geom = "crossbar", colour = "black", width = 0.65, linewidth = 0.1, show.legend = custom_inputs$bc$show_legend_choice)
  }
  
  if (input$data_points) {
    p <- p +
      geom_point(
        size = custom_inputs$bc$data_pt_sz_choice,
        position = position_jitterdodge(jitter.width = 0.1, dodge.width = errorbar_width()),
        show.legend = FALSE)
  }
  
  if (input$errorbars != "None") {
    # Get the corresponding function based on user input
    error_func <- stat_summary_errorbar_functions[[input$errorbars]]

    # Add stat_summary to the plot with the selected error function
    p <- p +
      stat_summary(
        position = position_dodge2(preserve = "single", padding = 0.5),
        fun.data = error_func,
        geom = "errorbar",
        width = errorbar_width(),
        linewidth = .25
      )
  }

  if (isTruthy(input$threshold_line)) {
    p <- p +
      geom_hline(yintercept = threshold_line_fn(input$threshold_line),
                 linetype = "dashed", color = "darkgrey", linewidth = .75)
  }

  if (manual_y_range()) {
    req(sv$y_axis)
    p <- p +
      coord_cartesian(ylim = unlist(sv$y_axis))
  }

  if (isTruthy(input$log_y_axis)) {
    p <- p +
      scale_y_log10()
  }
  
  if (isTruthy(multigroup)) {
   if (length(multigroup) > 1) {
      p <- p +
        facet_grid(get(multigroup[2]) ~ get(multigroup[1]), scales = "free_x", space = "free_x")
    } else {
      p <- p +
        facet_grid(~ get(multigroup), scales = "free_x", space = "free_x")
    }
  }
  
  return(p)
})

bc_dims <- reactive({
  list(mutate_null_fn(custom_inputs$bc$graph_width_choice, "auto"), mutate_null_fn(custom_inputs$bc$graph_height_choice, "auto"))
})

output$render_bc_plot <- renderPlot({
  return(bc_plot())
  
}, width = exprToFunction(bc_dims()[[1]]), height = exprToFunction(bc_dims()[[2]]))

#### SP Data ####
sp_errorbar_width <- reactive({
  dat <- single_plate_slider_data()
  
  xInput <- input$plot_x_axis
  numeric_x <- xInput %in% layout_info()$numeric_names
  # numeric_x <- xInput %in% Numeric_defs
  log_x <- mutate_null_fn(input$log_x_axis_sp) & numeric_x
  
  if (log_x) {
    dat <- dat[which(dat[, xInput] > 0), ]
  }
  
  if (numeric_x) {
    Max <- max(finite_fn(dat[xInput]))
    Min <- min(finite_fn(dat[xInput]))
    
    if (log_x) {
      ew <- (log(Max, 10) - log(Min, 10)) / 150
    } else {
      ew <- (Max - Min) / 150
    }
    
  } else {
    x <- length(unique(dat[[xInput]]))
    ew <- x^(1/1.1) / 100
  }
  
  return(ew)
})

#### SP Plot ####
sp_plot <- reactive({
  req(is.logical(input$connecting_line_sp))
  req(!is.null(input$data_points), input$errorbars)
  req(input$include_sample)
  
  pt_sz <- req(custom_inputs$sp$data_pt_sz_choice)
  
  dat <- single_plate_slider_data()
  if (nrow(dat) == 0) {return(NULL)}  # prevent error if data not loaded yet
  
  xInput <- input$plot_x_axis
  
  numeric_x <- xInput %in% layout_info()$numeric_names
  # numeric_x <- xInput %in% Numeric_defs
  xAxisOrder <- custom_inputs$sp$x_axis_order_choice
  
  fillInput <- input$group_fill
  fillOrder <- custom_inputs$sp$group_order_choice
  
  if (!numeric_x) {
    dat[[xInput]] <- factor_fn(dat, xInput, xAxisOrder)
  }
  
  dat[[fillInput]] <- factor_fn(dat, fillInput, fillOrder)
  
  dat <- dat[which(!is.na(dat[[xInput]]) & !is.na(dat[[fillInput]])), ]
  
  multigroup <- input$multigroup
  multigroup_x_order <- custom_inputs$sp$multigroup_x_order_choice
  multigroup_y_order <- custom_inputs$sp$multigroup_y_order_choice
  
  if (length(multigroup) > 0) {
    dat[[multigroup[1]]] <- factor_fn(dat, multigroup[1], multigroup_x_order)
    
    if (length(multigroup) > 1) {
      dat[[multigroup[2]]] <- factor_fn(dat, multigroup[2], multigroup_y_order)
      dat <- dat[which(!is.na(dat[[multigroup[1]]]) & !is.na(dat[[multigroup[2]]])), ]
    } else {
      dat <- dat[which(!is.na(dat[[multigroup[1]]])), ]
    }
  }
  
  log_x <- mutate_null_fn(input$log_x_axis_sp) & numeric_x
  
  y_title <- y_title_fn(custom_inputs$sp$y_title_choice, input_dataSelection()$raw, input_dataSelection()$analysis)
  
  ew <- sp_errorbar_width()
  
  if (isTruthy(input$log_y_axis)) {
    dat <- dat[which(dat$Value > 0), ]
  }
  
  if (numeric_x) {
    dat <- dat[which(dat[xInput] != "" ), ]
  }
  
  if (log_x) {
    dat <- dat[which(dat[xInput] > 0), ]
  }
  
  p <- dat |>
    ggplot(aes(y = Value, x = .data[[xInput]], color = .data[[fillInput]], group = .data[[fillInput]])) +
    
    theme_labs_fn(
      custom_inputs$sp,
      YLAB = y_title
    )
    
  # Add individual points or summary points based on condition
  if (input$data_points) {
    p <- p + geom_point(size = pt_sz, show.legend = custom_inputs$sp$show_legend_choice)
  } else {
    p <- p + stat_summary(fun = mean, geom = "point", size = pt_sz, show.legend = custom_inputs$sp$show_legend_choice)
  }
  
  if (input$errorbars != "None") {
    # Get the corresponding function based on user input
    error_func <- stat_summary_errorbar_functions[[input$errorbars]]

    # Add stat_summary to the plot with the selected error function
    p <- p +
      stat_summary(
        fun.data = error_func,
        geom = "errorbar",
        width = ew,
        show.legend = FALSE
      )
  }
  
  # Add connecting line if selected
  if (input$connecting_line_sp) {
    if (input$connecting_line_menu == "Connecting Line") {
      p <- p + stat_summary(fun = mean, geom = "line", linewidth = 0.6, show.legend = FALSE)
    } else if (input$connecting_line_menu == "Smooth Line") {
      p <- p + geom_smooth(
        aes(fill = .data[[fillInput]]), se = TRUE, alpha = 0.1,
        # show.legend = c(fill = FALSE)
        show.legend = FALSE
      )
    }
  }
  
  # Add horizontal threshold line if selected
  if (isTruthy(input$threshold_line)) {
    p <- p + geom_hline(yintercept = threshold_line_fn(input$threshold_line), linetype = "dashed", color = "grey")
  }
  
  if (manual_y_range()) {
    req(sv$y_axis)
    p <- p +
      coord_cartesian(ylim = unlist(sv$y_axis))
  }
  
  # Log10 transformation of x-axis if applicable
  if (log_x) {
    p <- p + scale_x_log10()
  }
  
  # Log10 transformation of y-axis if applicable
  if (input$log_y_axis) {
    p <- p + scale_y_log10()
  }
  
  if (isTruthy(input$multigroup)) {
    if (length(input$multigroup) > 1) {
      p <- p +
        facet_grid(get(input$multigroup[2]) ~ get(input$multigroup[1]), scales = "free_x", space = "free_x")
    } else {
      p <- p +
        facet_grid(~ get(input$multigroup), scales = "free_x", space = "free_x")
    }
  }
  
  return(p)
})

sp_dims <- reactive({
  list(mutate_null_fn(custom_inputs$sp$graph_width_choice, "auto"), mutate_null_fn(custom_inputs$sp$graph_height_choice, "auto"))
})

output$render_sp_plot <- renderPlot({
  sp_plot()
}, width = exprToFunction(sp_dims()[[1]]), height = exprToFunction(sp_dims()[[2]]))

#### CF Data ####
cf_data_final <- reactive({
  gf <- req(input$group_fill_cf)
  px <- req(input$plot_x_axis_cf)
  
  dat <- cf_data_wrang()
  
  filter_selection <- cf_selected()
  
  if (gf == "plate") {
    dat$Selection <- dat$plate
  } else {
    names(dat)[names(dat) == gf] <- "Selection"
  }
  
  names(dat)[names(dat) == px] <- "Dose"
  dat <- dat[which(!is.na(dat$Dose) & dat$Dose >= 0 & dat$Selection %in% filter_selection), match(c("plate", "Selection", "Dose", "Value"), names(dat))]
  
  # Convert 0 X values to 100x less than the lowest positive value
  if (min(dat$Dose) == 0) {
    dat$Dose[dat$Dose == 0] <- unique(sort(dat$Dose))[2] / 100
  }
  return(dat)
})

#### CF Curve Fit #####
cf_plot <- reactive({
  dat <- cf_data_final()
  
  req(input$best_fit_function)
  pt_sz <- custom_inputs$cf$data_pt_sz_choice
  
  if (mutate_null_fn(input$show_plate_slider) & data_info()$multiplate_incl) {  # multiplate() condition needed to prevent errors when switching from multiplate datasets to single plate datasets
    req(input$show_plate_slider)
    dat <- dat[which(dat$plate == input$plate_slider), ]
  }
  
  seln <- cf_selected()
  seln <- seln[seln %in% dat$Selection]
  if (length(seln) == 0) {return(NULL)}
  
  curve_fit_df <- predict_cf_fn(DAT = dat,
                                SELN = seln,
                                FCT = input$best_fit_function)
  
  Max <- max(finite_fn(dat$Dose))
  Min <- min(finite_fn(dat$Dose))
  
  if (manual_y_range()) {
    req(sv$y_axis)
    ic_pos <- sv$y_axis$max
  } else {
    ic_pos <- NULL
  }
  
  ew <- (log(Max, 10) - log(Min, 10)) / 60
  y_title <- y_title_fn(custom_inputs$cf$y_title_choice, input_dataSelection()$raw, input_dataSelection()$analysis)
  
  x_title <- y_title_fn(custom_inputs$cf$x_title_choice, input$plot_x_axis_cf)
  
  if (isTruthy(input$log_y_axis)) {
    dat <- dat[which(dat$Value > 0), ]
  }
  
  dat$Selection <- factor(dat$Selection)
  curve_fit_df$Selection <- factor(curve_fit_df$Selection)
  
  p <- ggplot(dat, aes(x = Dose, y = Value)) +
    
    theme_labs_fn(
      custom_inputs$cf,
      XLAB = x_title,
      YLAB = y_title
    )
    
  # Add individual points or summary points based on input
  if (input$data_points) {
    p <- p + 
      geom_point(aes(colour = Selection),
                 size = pt_sz,
                 show.legend = custom_inputs$cf$show_legend_choice)
  } else {
    p <- p + 
      stat_summary(fun.data = mean_se, geom = "point",
                   aes(color = Selection),
                   size = pt_sz,
                   show.legend = custom_inputs$cf$show_legend_choice)
  }
  
  # Add error bars if selected
  if (input$errorbars != "None") {
    error_func <- stat_summary_errorbar_functions[[input$errorbars]]
    p <- p + 
      stat_summary(
        aes(color = Selection),
        fun.data = error_func,
        geom = "errorbar",
        width = ew,
        linewidth = .25,
        show.legend = FALSE)
  }
  
  # Add prediction band if selected
  if (isTruthy(input$prediction_band_cf)) {
    p <- p + geom_ribbon(data = curve_fit_df, aes(x = dose_seq, y = Prediction, ymin = Lower, ymax = Upper, fill = Selection), alpha = 0.2)
  }
  
  # Add line for the predicted curve
  p <- p + geom_line(data = curve_fit_df, aes(x = dose_seq, y = Prediction, colour = Selection), show.legend = custom_inputs$cf$show_legend_choice)
  
  # Log10 transformation of x-axis
  p <- p + scale_x_continuous(trans = 'log10')
  
  # Faceting if multiple graphs are selected
  if (sv$multigraph_cf_choice & length(seln) > 1) {
    p <- p + facet_wrap(~Selection)
  }
  
  # Add threshold line / interpolation value(s)
  if (isTruthy(input$threshold_line)) {
    thr <- threshold_line_fn(input$threshold_line)
    
    if (custom_inputs$cf$ic_show_threshold_choice) {
      p <- p + geom_hline(yintercept = thr, linetype = "dashed", color = "grey")
    }
    
    if (length(seln) == 1 & custom_inputs$cf$ic_show_val_choice) {
      p <- p + ic_fn(curve_fit_df, dat, input_dataSelection()$analysis, ic_pos, thr, custom_inputs$cf$ic_name_choice)
    }
  }
  
  if (manual_y_range()) {
    req(sv$y_axis)
    p <- p +
      coord_cartesian(ylim = unlist(sv$y_axis))
  }
  
  # Log10 transformation of y-axis if applicable
  if (input$log_y_axis) {
    p <- p + scale_y_log10()
  }
  
  return(p)
})

cf_dims <- reactive({
  list(mutate_null_fn(custom_inputs$cf$graph_width_choice, "auto"), mutate_null_fn(custom_inputs$cf$graph_height_choice, "auto"))
})

output$render_cf_plot <- renderPlot({
  cf_plot()
}, width = exprToFunction(cf_dims()[[1]]), height = exprToFunction(cf_dims()[[2]]))

#### ZF Z Factor heatmap summary ####
zf_col_ind <- reactive({
  dat <- ctrl_data()[which(ctrl_data()$control_avgs), names(ctrl_data()) != "control_avgs"]
  # Search only column headers with 'MEAN_' (Thermo data)
  col_ind <- grepl("MEAN_", names(dat[, -(1:2)]))
  
  # If no column headers with 'MEAN_', or show_all_analysis checked, show everything
  if (sv$show_all_analysis_ph_choice | !any(col_ind)) {
    col_ind <- as.logical(lapply(dat[, -(1:2)], function(col) !all(is.na(col) | col == 0)))
  }
  return(col_ind)
})

# Calculate Z
z_factors <- reactive({
  dat <- ctrl_data()[which(ctrl_data()$control_avgs), names(ctrl_data()) != "control_avgs"]
  dat_sd <- ctrl_sd_data()
  col_ind <- zf_col_ind()
  
  p_mean <- dat[which(dat$Sample == "Positive Control"), which(c(TRUE, TRUE, col_ind))]
  n_mean <- dat[which(dat$Sample == "Negative Control"), which(c(FALSE, FALSE, col_ind))]
  p_sd <- dat_sd[which(dat_sd$Sample == "Positive Control"), which(c(FALSE, FALSE, col_ind))]
  n_sd <- dat_sd[which(dat_sd$Sample == "Negative Control"), which(c(FALSE, FALSE, col_ind))]
  
  p_mean[, -(1:2)] <- 1 - (3 * (p_sd + n_sd)) / abs(p_mean[, -(1:2)] - n_mean)
  
  zf <- as.data.frame(t(p_mean[-(1:2)])) |>  # transpose dataframe
    round(2) |>  # round to 2 decimals
    tibble::rownames_to_column("Parameter") |>
    pivot_longer(!Parameter, names_to = "Plate", values_to = "Z_Factor")  # Convert table to long format for ggplot
  
  zf$Plate <- gsub("V","",zf$Plate) |>  # Remove V from plate number (added during transpose)
    as.integer()  # Convert plate number to integer for proper sorting
  
  return(zf)
})

# Z Factor Heatmap
zf_plot <- reactive({
  
  if (layout_info()$pos_ctrl & layout_info()$neg_ctrl) {
    df <- z_factors()
    
    pl_n = df[which(df$Plate == input_dataSelection()$pv_plate_n), ] # dataframe of selected plate
    
    df$Parameter <- factor(df$Parameter, levels = pl_n$Parameter[order(pl_n$Z_Factor)])
    
    b <- c(-1,0,1) # breaks for heatmap colors
    
    zf <- df |>  
      ggplot(aes(x = factor(Plate), y = Parameter, fill = Z_Factor)) +
      geom_tile(width = 1) +
      theme_void() +
      geom_text(aes(label = Z_Factor), size = custom_inputs$zf$data_txt_sz_choice / 2.9) +
      scale_fill_gradientn(colors = c("magenta", "white", "green"), limits = c(-1,1), breaks = b, labels = format(b), oob = scales::squish) +
      scale_x_discrete(position = "top") +
      
      theme_labs_fn(
        custom_inputs$zf,
        LEG = "fill",
        BW = FALSE
      )
      
    return(zf)
  } else {
    return(NULL)
  }
})

zf_dims <- reactive({
  c(mutate_null_fn(custom_inputs$zf$graph_width_choice, min(max(plate_n_options()) * 50 + 400, 8000)),
       mutate_null_fn(custom_inputs$zf$graph_height_choice, min(length(zf_col_ind()[zf_col_ind()]) * 40 + 40, 8000)))
})

output$render_zf_plot <- renderPlot({
  zf_plot()
}, width = exprToFunction(zf_dims()[[1]]), height = exprToFunction(zf_dims()[[2]]))

#### Format data for download ####
pv_dl_data <- reactive({
  dat <- pv_data()
  
  if (data_table_format_wide()) {
    dat <- dat[, match(c("Row","Column","Value"), names(dat))] |>
      right_join(all_plate_wells()[, -match("Well", names(all_plate_wells()))], by = c("Row", "Column")) |>
      arrange(Row, Column) |>
      pivot_wider(names_from = "Column", values_from = "Value") |>
      as.data.frame()
  }
  
  dat[is.na(dat)] <- ""
  
  return(dat)
})

bcsp_dl_data <- reactive({
  dat <- bc_sp_data_wrang()
  if (data_table_format_wide()) {
    names(dat)[names(dat) == input$group_fill] <- 'Cohort'
    names(dat)[names(dat) == input$plot_x_axis] <- 'X_axis'
    dat <- dl_pivot_data_fn2(dat, input$plot_x_axis != input$group_fill)
  }
  
  mutate(dat, across(everything(), ~ replace(.x, is.na(.x), ""))) # no error
  
  return(dat)
})

cf_dl_data <- reactive({
  dat <- cf_data_final()
  
  if (data_table_format_wide()) {
    names(dat)[names(dat) == "Dose"] <- "X_axis"
    names(dat)[names(dat) == "Selection"] <- "Cohort"
    dat <- dl_pivot_data_fn2(dat)
  }
  
  dat[is.na(dat)] <- ""
  return(dat)
})

zf_dl_data <- reactive({
  pivot_wider(z_factors(), names_from = "Plate", values_from = "Z_Factor", names_prefix = "Plate ")
})


output$render_pv_table <- renderUI({
  tags$div(style = shift_table_fn(pv_dims()[[2]], 400),
           renderTable(pv_dl_data())
  )
})

output$render_bc_table <- renderUI({
  tags$div(style = shift_table_fn(bc_dims()[[2]]),
           renderTable(bcsp_dl_data())
  )
})

output$render_sp_table <- renderUI({
  tags$div(style = shift_table_fn(sp_dims()[[2]]),
           renderTable(bcsp_dl_data())
  )
})

output$render_cf_table <- renderUI({
  tags$div(style = shift_table_fn(cf_dims()[[2]]),
           renderTable(cf_dl_data())
  )
})

output$render_zf_table <- renderUI({
  tags$div(style = shift_table_fn(zf_dims()[[2]]),
           renderTable(zf_dl_data())
  )
})


PV2_dl_data <- reactive({
  if (data_table_format_wide2()) {
    dat <- pv_dl_data_fn(pv2_data_final(), all_plate_wells())
  } else {
    dat <- pv2_data_final()
  }
  return(dat)
})


#### Download data ####
dl_tab_to_data <- list(
  "Plate View" = pv_dl_data,
  "Bar Chart" = bcsp_dl_data,
  "Scatter Plot" = bcsp_dl_data,
  "Curve Fit" = cf_dl_data,
  "Z Factor" = zf_dl_data
)

output$downloadDataButton <- downloadHandler(
  filename = function() { paste0(input$tabset_panel, "_", input_dataSelection()$raw, "_", input_dataSelection()$analysis, "_data.csv") },
  content = function(file) {
    write.csv(dl_tab_to_data[[input$tabset_panel]](), file, row.names = FALSE)
  }
)

#### Save Project ####
proj_data <- reactive({
  # remove layout columns from merged data
  merged_data <- current_merged$data
  layout_data_names <- names(current_merged$data)
  data_names <- layout_data_names[!layout_data_names %in% c("Layout", layout_info()$def_names)]
  new_hcs_data <- merged_data[, data_names]
  
  # add Plate column from TF HCS data
  if ("Plate" %in% names(hcs_data())) {
    ori_data <- hcs_data() |>
      arrange(plate, Row, Column)
    new_hcs_data$Plate <- ori_data$Plate
  }
  
  list(
    data = new_hcs_data,
    data_name = current_data$data_file,
    layout = current_data$layout,
    layout_name = current_data$layout_file,
    settings = reactiveValuesToList(sv),
    current_tab = current_tab(),
    version = version_number,
    date = Sys.Date()
  )
})

output$save_proj <- downloadHandler(
  filename = function() {
    paste(Sys.Date(), "_HCSDEv", version_number,".rds", sep = "")
  },
  content = function(file) {
    saveRDS(proj_data(), file)
  }
)
