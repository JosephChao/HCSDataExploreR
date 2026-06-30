#### UI Plot 2 Plate Input options ####
output$multiplate_n2 <- renderUI({
  selectInput(inputId = "multiplate_n2",
              label = "Plate",
              choices = plate_n_options(),
              multiple = TRUE,
              selected = isolate(sv$multiplate_n2_choice))
})
outputOptions(output, "multiplate_n2", suspendWhenHidden = FALSE)

observeEvent(debounce(reactive({
  input$multiplate_n2
}), 300)(), {
  if (data_info()$multiplate_incl) {  # required to reset multiplate value when loading a single plate after loading multiplate data
    pn <- input$multiplate_n2
  } else {
    pn <- 1
  }
  
  sv$multiplate_n2_choice <<- as.integer(pn)
})

output$multiplate2_add_all <- renderUI({
  actionButton("multiplate2_add_all",
               label = "All")
})

observeEvent(input$multiplate2_add_all, {
  sv$multiplate_n2_choice <<- data_info()$plate_n_options
  updateSelectInput(session, "multiplate_n2", selected = data_info()$plate_n_options)
})

# Plate Average option - calculate average of each plate prior to plotting for biological reps
output$plate_avg2 <- renderUI({
  tags$div(title = "Replicates on each plate are averaged first and plotted as a single data point - useful for plotting biological replicates",
           checkboxInput("plate_avg2", label = "Average by plate",
                         value = isolate(sv$plate_avg2_choice)))
})
# Save multiplate average option
observeEvent(input$plate_avg2, {sv$plate_avg2_choice <<- input$plate_avg2}) #, ignoreInit = TRUE)


#### UI Data Selection Module ####
input_dataSelection_dual1 <- dataSelection_moduleServer(
  id = "dataSelection_dual1",
  sv = sv,
  data_col_names = data_col_names,
  layout_info = layout_info,
  data_info = data_info
)

input_dataSelection_dual2 <- dataSelection_moduleServer(
  id = "dataSelection_dual2",
  sv = sv,
  data_col_names = data_col_names,
  layout_info = layout_info,
  data_info = data_info
)

#### UI X-axis Inputs ####
# BC2 X-axis Plot
output$plot_x_axis_bc2 <- renderUI({
  selectInput("plot_x_axis_bc2",
              label = "Plot:",
              choices = c(if (data_info()$multiplate_incl) {"plate"}, layout_info()$def_names),
              selected = isolate(sv$plot_x_axis_bc2_choice)
  )
})
# Save X-axis selection
observeEvent(input$plot_x_axis_bc2, {sv$plot_x_axis_bc2_choice <<- input$plot_x_axis_bc2}) #, ignoreInit = TRUE)

bc2_x_inputs <- reactive({
  list(
    plot_x_axis = input$plot_x_axis_bc2,
    group_fill = NULL
  )
})

# SP2 X-axis average and group order
output$x_axis_avg_sp2 <- renderUI({
  shinyWidgets::pickerInput("x_axis_avg_sp2",
              label = "X-axis average by:",
              choices = layout_info()$def_names,
              multiple = TRUE,
              selected = isolate(sv$x_axis_avg_sp2_choice)
  )
})
observeEvent(input$x_axis_avg_sp2, {sv$x_axis_avg_sp2_choice <<- input$x_axis_avg_sp2}) #, ignoreInit = TRUE)

output$group_fill_sp2 <- renderUI({
  selectInput("group_fill_sp2",
              label = "Group by",
              choices = c("Threshold", if (data_info()$multiplate_incl) {"plate"}, layout_info()$def_names),
              selected = isolate(sv$group_fill_sp2_choice)
  )
})
# Save X-axis selection
observeEvent(input$group_fill_sp2, {sv$group_fill_sp2_choice <<- input$group_fill_sp2}) #, ignoreInit = TRUE)

sp2_x_inputs <- reactive({
  list(
    plot_x_axis = input$x_axis_avg_sp2,
    group_fill = input$group_fill_sp2
  )
})

# CF2 X-axis and Group Fill
output$plot_x_axis_cf2 <- renderUI({
  selectInput("plot_x_axis_cf2", "X-axis:",
              choices = layout_info()$numeric_names,
              selected = isolate(sv$plot_x_axis_cf2_choice))
})
outputOptions(output, "plot_x_axis_cf2", suspendWhenHidden = FALSE)

output$group_fill_cf2 <- renderUI({
  opt <- names(cf_selection_options()[[req(input$plot_x_axis_cf)]])
  
  selectInput("group_fill_cf2", "Group by:",
              choices = opt,
              selected = isolate(sv$group_fill_cf2_choice))
})
outputOptions(output, "group_fill_cf2", suspendWhenHidden = FALSE)

# Save X-axis selection
observeEvent(input$plot_x_axis_cf2, {sv$plot_x_axis_cf2_choice <<- input$plot_x_axis_cf2}) #, ignoreInit = TRUE)
# Save group by selection
observeEvent(input$group_fill_cf2, {sv$group_fill_cf2_choice <<- input$group_fill_cf2}) #, ignoreInit = TRUE)

cf2_x_inputs <- reactive({
  list(
    plot_x_axis = input$plot_x_axis_cf2,
    group_fill = input$group_fill_cf2
  )
})

output$cf2_selection <- renderUI({
  gf <- req(input$group_fill_cf2)
  px <- req(input$plot_x_axis_cf2)
  
  selectInput("cf2_selection", "Selection",
              choices = cf_selection_options()[[px]][[gf]],
              multiple = TRUE,
              # selected = isolate(sv$cf2_selection_choice[[input$plot_x_axis_cf2]][[input$group_fill_cf2]])
              selected = isolate(sv$cf2_selection_choice[[px]][[gf]])
  )
})
# Save DR compound selection selection
observeEvent(input$cf2_selection, {
  sv$cf2_selection_choice[[req(input$plot_x_axis_cf2)]][[req(input$group_fill_cf2)]] <<- input$cf2_selection
}, ignoreNULL = FALSE) #, ignoreInit = TRUE)

cf2_selected <- reactive({
  opts <- cf_selection_options()[[req(input$plot_x_axis_cf2)]][[req(input$group_fill_cf2)]]
  
  if (length(opts) > 1) {
    mutate_null_fn(input$cf2_selection, opts)
  } else {
    opts
  }
})

#### UI Plot Inputs - errorbars, data points, threshold ####
output$errorbars2 <- renderUI({
  radioButtons("errorbars2",
               label = "Error Bars",
               choices = c("SD", "SEM", "CL", "None"),
               selected = isolate(sv$errorbars2_choice),
               inline = TRUE
  )
})
# Save errorbar display option
observeEvent(input$errorbars2, {sv$errorbars2_choice <<- input$errorbars2}) #, ignoreInit = TRUE)

output$data_points2 <- renderUI({
  div(title = "Show all data points. Unchecked shows the mean.",
      checkboxInput("data_points2", label = "Data Points", value = isolate(sv$data_points2_choice))
  )
})

output$log_y_axis2 <- renderUI({
  checkboxInput("log_y_axis2", label = "Log Y-axis", value = isolate(sv$log_y_axis2_choice))
})

outputOptions(output, "log_y_axis2", suspendWhenHidden = FALSE) # needed to calculate y_manual_range2 for Bar Chart 2

# Save data point selection option
observeEvent(input$data_points2, {sv$data_points2_choice <<- input$data_points2}) #, ignoreInit = TRUE)
# Save y log scale
observeEvent(input$log_y_axis2, {sv$log_y_axis2_choice <<- input$log_y_axis2}) #, ignoreInit = TRUE)


output$log_x_axis2 <- renderUI({
  checkboxInput("log_x_axis2", label = "Log X-axis", value = isolate(sv$log_x_axis2_choice))
})
observeEvent(input$log_x_axis2, {sv$log_x_axis2_choice <<- input$log_x_axis2}) #, ignoreInit = TRUE)

#### Manual Y Range ####
output$y_axis_min2 <- renderUI({
  numericInput("y_axis_min2", "Min Y-axis", value = isolate(sv$y_axis2$min))
})

observeEvent(debounce(reactive({
  input$y_axis_min2
}), 400)(), {
  sv$y_axis2$min <<- input$y_axis_min2
})

output$y_axis_max2 <- renderUI({
  numericInput("y_axis_max2", "Max Y-axis", value = isolate(sv$y_axis2$max))
})

observeEvent(debounce(reactive({
  input$y_axis_max2
}), 400)(), {
  sv$y_axis2$max <<- input$y_axis_max2
})

manual_y_range2 <- reactiveVal(FALSE)

observeEvent(list(sv$y_axis2, input$log_y_axis2, input$tabset_panel_dual), {
  if (isTruthy(input$y_axis_min2) & isTruthy(input$y_axis_max2)) {
    current_val <- isolate(manual_y_range2())
    if ((input$y_axis_min2 < input$y_axis_max2 & ((!input$log_y_axis2 || input$tabset_panel_dual == "Bar Chart 2") || (input$y_axis_min2 > 0 & input$y_axis_max2 > 0))) != current_val) {
      manual_y_range2(!current_val)
    }
  } else {
    manual_y_range2(FALSE)
  }
}, ignoreNULL = FALSE)

# Threshold Lines for SP2
output$threshold_line2_sp <- renderUI({
  textInput("threshold_line2_sp", label = "Y-axis Threshold", value = isolate(sv$threshold_line2_sp_choice))
})
# Save threshold
observeEvent(input$threshold_line2_sp, {sv$threshold_line2_sp_choice <<- input$threshold_line2_sp}) #, ignoreInit = TRUE)

output$threshold_line2x_sp <- renderUI({
  textInput("threshold_line2x_sp", label = "X-axis Threshold", value = isolate(sv$threshold_line2x_choice))
})
# Save X-axis threshold
observeEvent(input$threshold_line2x_sp, {sv$threshold_line2x_choice <<- input$threshold_line2x_sp}) #, ignoreInit = TRUE)

# Threshold Line for BC2 and CF2 - appears above Y-axis Min/Max
output$threshold_line2 <- renderUI({
  textInput("threshold_line2", label = "Threshold Line", value = isolate(sv$threshold_line2_choice))
})
# Save threshold
observeEvent(input$threshold_line2, {sv$threshold_line2_choice <<- input$threshold_line2}) #, ignoreInit = TRUE)

#### NEW EXCLUDE HANDLING ####
output$include_sample2 <- renderUI({
  selectInput("include_sample2", label = "Select sample types to display",
              choices = unique(plate_layout_def()["Sample"]),
              multiple = TRUE,
              selected = isolate(sv$include_sample2_choice))
})
# Save Sample type inclusions
observeEvent(input$include_sample2, {sv$include_sample2_choice <<- input$include_sample2}) #, ignoreInit = TRUE)

graph2_inputs <- reactive({
  list(
    plate_avg = mutate_null_fn(input$plate_avg2),
    include_sample = input$include_sample2
  )
})

output$data_filters2 <- renderUI({
  lcn <- layout_info()$col_names
  
  selectInput("data_filters2", label = "Filter data by:",
              choices = lcn[!lcn %in% c("Layout", "plate", "Sample")],
              multiple = TRUE,
              selected = isolate(sv$data_filters_choice)
  )
})
outputOptions(output, "data_filters2", suspendWhenHidden = FALSE)

observeEvent(input$data_filters2, {sv$data_filters2_choice <<- input$data_filters2}) #, ignoreInit = TRUE)

# Dynamically create renderUI components for Exclude Filters
lapply(exclude_filters_cols, function(col) {
  output[[paste0("exclude_filters2_", col)]] <- renderUI({
    selectInput(
      inputId = paste0("exclude_filters2_", col), 
      label = paste("Exclude", col),
      choices = unique(plate_layout_def()[[col]]), 
      multiple = TRUE,
      selected = isolate(sv$exclude_filters2[[col]])
    )
  })
})

# Dynamically create observers with debounce
lapply(exclude_filters_cols, function(col) {
  observeEvent(debounce(reactive({    # OBSERVE NOT WORKING WHEN CLEARING THE FIELD
    input[[paste0("exclude_filters2_", col)]]
  }), 400)(), {
    sv$exclude_filters2_previous[[col]] <- sv$exclude_filters2[[col]]
    sv$exclude_filters2[[col]] <- input[[paste0("exclude_filters2_", col)]]
    
    if (length(input[[paste0("exclude_filters2_", col)]]) == 0) {
      sv$exclude_index2[[col]] <- NULL
    } else {
      sv$exclude_index2[[col]] <- which(!data_merged()[[col]] %in% input[[paste0("exclude_filters2_", col)]])
    }
  }, ignoreNULL = FALSE, ignoreInit = TRUE)
})

# Dynamically generate conditional panels
output$exclude_filters2_tl <- renderUI({
  tagList(
    lapply(exclude_filters_cols, function(col) {
      conditionalPanel(
        condition = sprintf("input.data_filters2.includes('%s')", col),
        uiOutput(paste0("exclude_filters2_", col))
      )
    })
  )
})

sv_exclude_index2 <- reactive(sv$exclude_index2)

#### UI Plot 2 Curve Fit 2 Input options ####
output$prediction_band_cf2 <- renderUI ({
  checkboxInput("prediction_band_cf2", label = "Predition Band", value = isolate(sv$prediction_band_cf2_choice))
})
observeEvent(input$prediction_band_cf2, {sv$prediction_band_cf2_choice <<- input$prediction_band_cf2}) #, ignoreInit = TRUE)

output$best_fit_function2 <- renderUI({
  selectInput("best_fit_function2", "Best-Fit Function",
              choices = c("sigEmax", "logistic", "emax", "linlog", "linear", "quadratic", "linInt", "exponential", "betaMod")
  )
})

#### UI Plot 2 Show Data Table ####
output$show_data_table2 <- renderUI({
  checkboxInput("show_data_table2", label = "Show data table", value = isolate(sv$show_data_table2_choice))
})
# Save Show Data Table selection
observeEvent(input$show_data_table2, {sv$show_data_table2_choice <<- input$show_data_table2}) #, ignoreInit = TRUE)

output$data_table2_format <- renderUI({
  req(input$show_data_table2)
  radioButtons("data_table2_format", label = NULL,
               choices = c("Wide", "Long"),
               selected = isolate(sv$data_table2_format_choice),
               inline = TRUE)
})
# Save Data Table Format selection
observeEvent(input$data_table2_format, {sv$data_table2_format_choice <<- input$data_table2_format}) #, ignoreInit = TRUE)

data_table_format_wide2 <- reactive({
  if (is.null(input$data_table2_format)) {return(TRUE)}
  input$data_table2_format == "Wide"
})

#### Customization Module Server ####
customization_moduleServer("bc2_customization",
                           current_tab = current_tab,
                           sv = sv,
                           plate_layout_def = plate_layout_def,
                           x_inputs = bc2_x_inputs,
                           plot_image = bc2_plot,
                           plot_dim = reactive(plot_dims_fn(custom_inputs, input$tabset_panel_dual)))

customization_moduleServer("sp2_customization",
                           current_tab = current_tab,
                           sv = sv,
                           plate_layout_def = plate_layout_def,
                           x_inputs = sp2_x_inputs,
                           sp_data2_threshold = sp_data2_threshold,
                           plot_image = sp2_plot,
                           plot_dim = reactive(plot_dims_fn(custom_inputs, input$tabset_panel_dual)))

customization_moduleServer("cf2_customization",
                           current_tab = current_tab,
                           sv = sv,
                           plate_layout_def = plate_layout_def,
                           x_inputs = cf2_x_inputs,
                           plot_image = cf2_plot,
                           plot_dim = reactive(plot_dims_fn(custom_inputs, input$tabset_panel_dual)))

observeEvent(input[["bc2_customization-customization_apply"]], {
  custom_inputs$bc2 <- sv$bc2_customization
})

observeEvent(input[["sp2_customization-customization_apply"]], {
  custom_inputs$sp2 <- sv$sp2_customization
})

observeEvent(input[["cf2_customization-customization_apply"]], {
  custom_inputs$cf2 <- sv$cf2_customization
})

#### Data Wrangling Module ####
bc2_data_1 <- GraphDataWrang_moduleServer(
  id = "bc2_data_1",
  sv_exclude_index = sv_exclude_index2,
  data_merged = data_merged,
  multiplate_n = reactive(sv$multiplate_n2_choice),
  input_dataSelection = input_dataSelection_dual1,
  graph_inputs = graph2_inputs,
  x_inputs = bc2_x_inputs,
  layout_info = layout_info,
  data_info = data_info,
  ctrl_data = ctrl_data,
  ctrl_sd_data = ctrl_sd_data,
  uninf_def_ctrl_data = uninf_def_ctrl_data)

bc2_data_2 <- GraphDataWrang_moduleServer(
  id = "bc2_data_2",
  sv_exclude_index = sv_exclude_index2,
  data_merged = data_merged,
  multiplate_n = reactive(sv$multiplate_n2_choice),
  input_dataSelection = input_dataSelection_dual2,
  graph_inputs = graph2_inputs,
  x_inputs = bc2_x_inputs,
  layout_info = layout_info,
  data_info = data_info,
  ctrl_data = ctrl_data,
  ctrl_sd_data = ctrl_sd_data,
  uninf_def_ctrl_data = uninf_def_ctrl_data)

bc2_data_final <- reactive({
  dat1 <- bc2_data_1()
  dat2 <- bc2_data_2()
  dat1$Source <- "Value 1"
  dat2$Source <- "Value 2"
  dat2$Value <- -(dat2$Value)
  if ("Field" %in% names(dat1)) {
    dat1$Field <- NULL
    dat2$Field <- NULL
  }
  dat <- plyr::rbind.fill(dat1, dat2)
})

sp2_data_1 <- GraphDataWrang_moduleServer(
  id = "sp2_data_1",
  sv_exclude_index = sv_exclude_index2,
  data_merged = data_merged,
  multiplate_n = reactive(sv$multiplate_n2_choice),
  input_dataSelection = input_dataSelection_dual1,
  graph_inputs = graph2_inputs,
  x_inputs = sp2_x_inputs,
  layout_info = layout_info,
  data_info = data_info,
  ctrl_data = ctrl_data,
  ctrl_sd_data = ctrl_sd_data,
  uninf_def_ctrl_data = uninf_def_ctrl_data)

sp2_data_2 <- GraphDataWrang_moduleServer(
  id = "sp2_data_2",
  sv_exclude_index = sv_exclude_index2,
  data_merged = data_merged,
  multiplate_n = reactive(sv$multiplate_n2_choice),
  input_dataSelection = input_dataSelection_dual2,
  graph_inputs = graph2_inputs,
  x_inputs = sp2_x_inputs,
  layout_info = layout_info,
  data_info = data_info,
  ctrl_data = ctrl_data,
  ctrl_sd_data = ctrl_sd_data,
  uninf_def_ctrl_data = uninf_def_ctrl_data)

sp2_data_combined <- reactive({
  dat1 <- sp2_data_1()
  dat2 <- sp2_data_2()

  dat1$Value_2 <- dat2$Value

  xavg <- req(input$x_axis_avg_sp2)

  dat1 <- dat1 |>
    group_by(across(all_of(xavg))) |>
    mutate(Value_2 = mean(Value_2),
           Value_mean = mean(Value)) |>
    ungroup()

  return(dat1)
})

cf2_data_1 <- GraphDataWrang_moduleServer(
  id = "cf2_data_1",
  sv_exclude_index = sv_exclude_index2,
  data_merged = data_merged,
  multiplate_n = reactive(sv$multiplate_n2_choice),
  input_dataSelection = input_dataSelection_dual1,
  graph_inputs = graph2_inputs,
  x_inputs = cf2_x_inputs,
  layout_info = layout_info,
  data_info = data_info,
  ctrl_data = ctrl_data,
  ctrl_sd_data = ctrl_sd_data,
  uninf_def_ctrl_data = uninf_def_ctrl_data)

cf2_data_2 <- GraphDataWrang_moduleServer(
  id = "cf2_data_2",
  sv_exclude_index = sv_exclude_index2,
  data_merged = data_merged,
  multiplate_n = reactive(sv$multiplate_n2_choice),
  input_dataSelection = input_dataSelection_dual2,
  graph_inputs = graph2_inputs,
  x_inputs = cf2_x_inputs,
  layout_info = layout_info,
  data_info = data_info,
  ctrl_data = ctrl_data,
  ctrl_sd_data = ctrl_sd_data,
  uninf_def_ctrl_data = uninf_def_ctrl_data)

cf2_data_combined <- reactive({
  dat1 <- cf2_data_1()
  dat2 <- cf2_data_2()

  dat1$Value_2 <- dat2$Value

  return(dat1)
})

#### Bar Chart 2 Plot ####
bc2_plot <- reactive({
  req(bc2_data_final(), input$plot_x_axis_bc2)
  
  dat <- bc2_data_final()
  
  xInput <- input$plot_x_axis_bc2
  xAxisOrder <- custom_inputs$bc2$x_axis_order_choice
  
  if (length(xAxisOrder) > 1) {
    # convert X axis to factor and sorted alphanumerically
    dat[[xInput]] <- factor_fn(dat, xInput, xAxisOrder, REV = TRUE)
    
    dat <- dat[!is.na(dat[[xInput]]), ]
  
  } else {
    dat[[xInput]] <- factor_fn(dat, xInput, REV = TRUE)
  }
  
  x_title <- y_title_fn(custom_inputs$bc2$x_title_choice,
                        input_dataSelection_dual2()$raw, input_dataSelection_dual2()$analysis,
                        input_dataSelection_dual1()$raw, input_dataSelection_dual1()$analysis)
  
  p <- ggplot(dat, aes(x = .data[[xInput]], y = Value, fill = Source, color = Source)) +
    
    theme_labs_fn(
      custom_inputs$bc2,
      XLAB = custom_inputs$bc2$y_title_choice,  # Note X and Y axis are flipped due to coord_flip() for labs() but not for theme()
      YLAB = x_title,
      LEG = "fill"
    ) +
    
    stat_summary(position="identity", fun = mean, geom = "bar", width = 0.5, alpha = 0.2) +
    
    scale_color_manual(values=c("royalblue3","darkorange"),  # bar outline colour
                       labels = c(paste(input_dataSelection_dual1()$raw,input_dataSelection_dual1()$analysis),
                                  paste(input_dataSelection_dual2()$raw,input_dataSelection_dual2()$analysis))) +
    scale_fill_manual(values=c("royalblue3","darkorange"),  # bar fill colour
                      labels = c(paste(input_dataSelection_dual1()$raw,input_dataSelection_dual1()$analysis),
                                 paste(input_dataSelection_dual2()$raw,input_dataSelection_dual2()$analysis))) +
    
    guides(color = "none")  # Hide 2nd legend for color
  
  if (isTruthy(input$threshold_line2)) {
    p <- p +
      geom_hline(yintercept = threshold_line_fn(input$threshold_line2), linetype = "dashed", color = "grey")
  }
  
  if (input$errorbars2 != "None") {
    # Get the corresponding function based on user input
    error_func <- stat_summary_errorbar_functions[[input$errorbars2]]
    
    # Add stat_summary to the plot with the selected error function
    p <- p +
      stat_summary(
        position = "identity",
        fun.data = error_func,
        geom = "errorbar",
        width = .15,
        linewidth = .25
      )
  }
  
  if (input$data_points2) {
    p <- p +
      geom_point(
        size = custom_inputs$bc2$data_pt_sz_choice,
        position = position_jitterdodge(jitter.width = 0.1, dodge.width = .15),
        show.legend = FALSE)
  }
  
  if (manual_y_range2()) {
    req(sv$y_axis2)
    p <- p +
      coord_flip(ylim = unlist(sv$y_axis2))
  } else {
    p <- p + coord_flip()
  }
  
  return(p)
})

bc2_dims <- reactive({
  list(mutate_null_fn(custom_inputs$bc2$graph_width_choice, "auto"), mutate_null_fn(custom_inputs$bc2$graph_height_choice, "auto"))
})

output$render_bc2_plot <- renderPlot({
  bc2_plot()
}, width = exprToFunction(bc2_dims()[[1]]), height = exprToFunction(bc2_dims()[[2]]))

#### Scatter Plot 2 Data ####
sp_data2_threshold <- reactive({
  dat <- sp2_data_combined()
  dat$Threshold <- "1,1"
  if (mutate_null_fn(input$group_fill_sp2) == "Threshold"  & (isTruthy(input$threshold_line2_sp) | isTruthy(input$threshold_line2x_sp))) {
    
    if (isTruthy(input$threshold_line2_sp)) {
      line_y <- c(-Inf, threshold_line_fn(input$threshold_line2_sp))
      line_y <- line_y[order(line_y)] # Use -Inf
    } else {
      line_y <- min(dat$Value)
    }
    
    if (isTruthy(input$threshold_line2x_sp)) {
      line_x <- c(-Inf, threshold_line_fn(input$threshold_line2x_sp))
      line_x <- line_x[order(line_x)]
    } else {
      line_x <- min(dat$Value_2)
    }
    
    for (x in 1:length(line_x)) {
      for (y in 1:length(line_y)) {
        
        if (input$data_points2) {
          dat$Threshold[which(dat$Value >= line_y[y] & dat$Value_2 >= line_x[x])] <- paste(x, y, sep = ",")
        } else {
          dat$Threshold[which(dat$Value_mean >= line_y[y] & dat$Value_2 >= line_x[x])] <- paste(x, y, sep = ",")
        }
      }
    }
  }
  return(dat)
})

#### SCATTER PLOT 2 ####
sp2_plot <- reactive({
  dat <- sp_data2_threshold()
  
  pt_sz <- custom_inputs$sp2$data_pt_sz_choice
  
  group_fill <- req(input$group_fill_sp2)
  group_order <- custom_inputs$sp2$group_order_choice
  
  if (length(group_order) > 1) {
    # convert X axis to factor and sorted alphanumerically
    dat[[group_fill]] <- factor_fn(dat, group_fill, group_order)
    
    dat <- dat[!is.na(dat[[group_fill]]), ]
    
  } else {
    dat[[group_fill]] <- factor_fn(dat, group_fill, REV = group_fill == "Threshold")
  }
  
  log_x <- input$log_x_axis2
  
  if (log_x) {
    dat <- dat[which(dat$Value_2 > 0), ]
  }
  
  Max <- max(finite_fn(dat$Value_2))
  Min <- min(finite_fn(dat$Value_2))
  
  if (log_x) {
    ew <- (log(Max, 10) - log(Min, 10)) / 150
  } else {
    ew <- (Max - Min) / 150
  }
  
  y_title <- y_title_fn(custom_inputs$sp2$y_title_choice,
                        input_dataSelection_dual1()$raw, input_dataSelection_dual1()$analysis)
  
  x_title <- y_title_fn(custom_inputs$sp2$x_title_choice,
                        input_dataSelection_dual2()$raw, input_dataSelection_dual2()$analysis)
  
  p <- dat |>
    ggplot(aes(y = Value, x = Value_2, color = .data[[group_fill]])) +
    
    theme_labs_fn(
      custom_inputs$sp2,
      XLAB = x_title,  # Note X and Y axis are flipped due to coord_flip() for labs() but not for theme()
      YLAB = y_title,
      LEG = "color"
    ) +
    
    scale_color_manual(values = sp_col, na.value = "dark grey")
  
  if (input$data_points2) {
    p <- p + geom_point(size = pt_sz, show.legend = custom_inputs$sp2$show_legend_choice)
  } else {
    p <- p + `stat_summary`(fun = mean, geom = "point", size = pt_sz, show.legend = custom_inputs$sp2$show_legend_choice)
  }  # Use Value_mean instead?
  
  if (isTruthy(input$threshold_line2_sp)) {
    p <- p +
      geom_hline(yintercept = threshold_line_fn(input$threshold_line2_sp),
                 linetype = "dashed", color = "darkgrey", linewidth = .75)
  }
  
  if (isTruthy(input$threshold_line2x_sp)) {
    p <- p +
      geom_vline(xintercept = threshold_line_fn(input$threshold_line2x_sp),
                 linetype = "dashed", color = "darkgrey", linewidth = .75)
  }
  
  if (input$errorbars2 != "None") {
    # Get the corresponding function based on user input
    error_func <- stat_summary_errorbar_functions[[input$errorbars2]]
    
    # Add stat_summary to the plot with the selected error function
    p <- p +
      stat_summary(
        fun.data = error_func,
        geom = "errorbar",
        width = ew,
        show.legend = custom_inputs$sp2$show_legend_choice
      )
  }
  
  # Log10 transformation of x-axis
  if (log_x) {
    p <- p + scale_x_log10()
  }
  
  # Log10 transformation of y-axis
  if (input$log_y_axis2) {
    p <- p + scale_y_log10()
  }
  
  if (manual_y_range2()) {
    req(sv$y_axis2)
    p <- p +
      coord_cartesian(ylim = unlist(sv$y_axis2))
  }
  
  return(p)
})

sp2_dims <- reactive({
  list(mutate_null_fn(custom_inputs$sp2$graph_width_choice, "auto"), mutate_null_fn(custom_inputs$sp2$graph_height_choice, "auto"))
})

output$render_sp2_plot <- renderPlot({
  sp2_plot() +
    theme(
      axis.title.y = element_text(size = custom_inputs$sp2$y_title_font_sz_choice, face = "bold", margin = margin(r = 15), colour = "royalblue"),
      axis.title.x = element_text(size = custom_inputs$sp2$x_title_font_sz_choice, face = "bold", margin = margin(t = 15), colour = "darkorange"),
    )
}, width = exprToFunction(sp2_dims()[[1]]), height = exprToFunction(sp2_dims()[[2]]))

#### Curve Fit 2 data ####
cf2_data_final <- reactive({
  gf <- req(input$group_fill_cf2)
  px <- req(input$plot_x_axis_cf2)
  filter_selection <- cf2_selected()
  
  dat <- cf2_data_combined()
  
  if (gf == "plate") {
    dat$Selection <- dat$plate
  } else {
    names(dat)[names(dat) == gf] <- "Selection"
  }
  
  names(dat)[names(dat) == px] <- "Dose"
  dat <- dat[which(!is.na(dat$Dose) & dat$Dose >= 0 & dat$Selection %in% filter_selection), match(c("plate", "Selection", "Dose", "Value", "Value_2"), names(dat))]
  
  # Convert 0 X values to 100x less than the lowest positive value
  if (min(dat$Dose) == 0) {
    dat$Dose[dat$Dose == 0] <- unique(sort(dat$Dose))[2] / 100
  }
  return(dat)
})

#### CF2 plot ####
cf2_plot <- reactive({
  
  dat <- cf2_data_final()
  
  pt_sz <- custom_inputs$cf2$data_pt_sz_choice
  
  seln <- cf2_selected()
  seln <- seln[seln %in% dat$Selection]
  if (length(seln) == 0) {return(NULL)}
  
  fct <- req(input$best_fit_function2)
  
  curve_fit_dat1 <- predict_cf_fn(DAT = dat[, 1:4],
                                  SELN = seln,
                                  FCT = fct)
  
  curve_fit_dat2 <- predict_cf_fn(DAT = dat[, c(1:3, 5)],
                                  SELN = seln,
                                  FCT = fct)
  
  Max <- max(finite_fn(dat$Dose))
  Min <- min(finite_fn(dat$Dose))
  ew <- (log(Max, 10) - log(Min, 10)) / 60
  
  y_title <- y_title_fn(custom_inputs$cf2$y_title_choice,
                        input_dataSelection_dual2()$raw, input_dataSelection_dual2()$analysis,
                        input_dataSelection_dual1()$raw, input_dataSelection_dual1()$analysis)
  
  x_title <- y_title_fn(custom_inputs$cf2$x_title_choice,
                        input$plot_x_axis_cf2)
  
  dat$Selection <- factor(dat$Selection)
  curve_fit_dat1$Selection <- factor(curve_fit_dat1$Selection)
  curve_fit_dat2$Selection <- factor(curve_fit_dat2$Selection)
  
  p <- ggplot() +
    geom_line(data=curve_fit_dat1, aes(x=dose_seq, y=Prediction, colour = "royalblue3"), show.legend = custom_inputs$cf2$show_legend_choice) +
    geom_line(data=curve_fit_dat2, aes(x=dose_seq, y=Prediction, colour = "darkorange"), show.legend = custom_inputs$cf2$show_legend_choice) +
    
    theme_labs_fn(
      custom_inputs$cf2,
      XLAB = x_title,  # Note X and Y axis are flipped due to coord_flip() for labs() but not for theme()
      YLAB = y_title,
      LEG = "color"
    ) +
    
    scale_x_continuous(trans='log10') +
    scale_color_identity(guide = "legend", labels = c(paste(input_dataSelection_dual2()$raw,input_dataSelection_dual2()$analysis),
                                                      paste(input_dataSelection_dual1()$raw,input_dataSelection_dual1()$analysis))) +
    scale_fill_identity()
  
  if (input$data_points2) {
    p <- p +
      geom_point(data = dat, aes(x = Dose, y = Value, colour = "royalblue3"),
                 size = pt_sz, show.legend = custom_inputs$cf2$show_legend_choice) +
      geom_point(data = dat, aes(x = Dose, y = Value_2, colour = "darkorange"),
                 size = pt_sz, show.legend = custom_inputs$cf2$show_legend_choice)
  } 
  else {
    p <- p +
      stat_summary(data = dat, aes(x = Dose, y = Value, colour = "royalblue3"),
                   fun.data = mean_se, geom = "point",
                   # width = ew, linewidth = .25,
                   show.legend = custom_inputs$cf2$show_legend_choice) +
      stat_summary(data = dat, aes(x = Dose, y = Value_2, colour = "darkorange"),
                   fun.data = mean_se, geom = "point",
                   # width = ew, linewidth = .25,
                   show.legend = custom_inputs$cf2$show_legend_choice)
  }
  
  if (isTruthy(input$threshold_line2)) {
    p <- p +
      geom_hline(yintercept = threshold_line_fn(input$threshold_line2),
                 linetype = "dashed", color = "darkgrey", linewidth = .75)
  }
  
  if (input$errorbars2 != "None") {
    # Get the corresponding function based on user input
    error_func <- stat_summary_errorbar_functions[[input$errorbars2]]
    
    # Add stat_summary to the plot with the selected error function
    p <- p +
      stat_summary(
        data = dat, aes(x = Dose, y = Value, colour = "royalblue3"),
        fun.data = error_func,
        geom = "errorbar",
        width = ew,
        linewidth = .25,
        show.legend = FALSE
      ) +
      stat_summary(
        data = dat, aes(x = Dose, y = Value_2, colour = "darkorange"),
        fun.data = error_func,
        geom = "errorbar",
        width = ew,
        linewidth = .25,
        show.legend = FALSE
      )
  }
  
  if (input$prediction_band_cf2) {
    p <- p +
      geom_ribbon(data=curve_fit_dat1, aes(x=dose_seq, y=Prediction, ymin=Lower, ymax=Upper, fill = "royalblue3"), alpha=0.2, show.legend = FALSE) +
      geom_ribbon(data=curve_fit_dat2, aes(x=dose_seq, y=Prediction, ymin=Lower, ymax=Upper, fill = "darkorange"), alpha=0.2, show.legend = FALSE)
  }
  
  if (length(seln) > 1) {
    p <- p + facet_wrap(~Selection)
  }
  
  if (input$log_y_axis2) {
    p <- p + scale_y_log10()
  }
  
  if (manual_y_range2()) {
    req(sv$y_axis2)
    p <- p +
      coord_cartesian(ylim = unlist(sv$y_axis2))
  }
  
  return(p)
  
})

cf2_dims <- reactive({
  list(mutate_null_fn(custom_inputs$cf2$graph_width_choice, "auto"), mutate_null_fn(custom_inputs$cf2$graph_height_choice, "auto"))
})

output$render_cf2_plot <- renderPlot({
  cf2_plot()
}, width = exprToFunction(cf2_dims()[[1]]), height = exprToFunction(cf2_dims()[[2]]))

#### Download data 2 ####
bc2_dl_data <- reactive({
  if (data_table_format_wide2()) {
    bc2_data_final() |>
      rename(X_axis = input$plot_x_axis_bc2,
             Cohort = Source) |>
      dl_pivot_data_fn2()
  } else {
    bc2_data_final() |>
      pivot_wider(names_from = Source, values_from = Value)
  }
})

sp2_dl_data <- reactive({
  dat <- sp2_data_combined()
  if (data_table_format_wide2()) {
    if (isTruthy(input$x_axis_avg_sp2)) {
      names(dat)[names(dat) == "Value_2"] <- "X_axis"
      formatted_dat <- dl_pivot_data_fn2(dat, AVGB = input$x_axis_avg_sp2)
      
      # TODO why is are empty rows added
      val <- formatted_dat[, -1]
      formatted_dat <- formatted_dat[rowSums(is.na(val)) < ncol(val), ]
      
    } else {
      formatted_dat <- dat[, match(c("definition", "Value_2", "Value"), names(dat))] |>
        arrange(Value_2)
      names(formatted_dat)[names(formatted_dat) == "Value_2"] <- "X_axis"
    }
  } else {
    formatted_dat <- dat |>
      arrange(X_axis)
  }
  return(formatted_dat)
})

cf2_dl_data <- reactive({
  if (data_table_format_wide2()) {
    cf2_data_final() |>
      rename(X_axis = Dose,
             Cohort = Selection) |>
      dl_pivot_data_fn2(VAL = c("Value", "Value_2"))
  } else {
    cf2_data_final()
  }
})

dl2_tab_to_data <- list(
  "Bar Chart 2" = bc2_dl_data,
  "Scatter Plot 2" = sp2_dl_data,
  "Curve Fit 2" = cf2_dl_data
)

#### Show data table ####
output$render_bc2_table <- renderUI({
  tags$div(style = shift_table_fn(bc2_dims()[[2]], 700),
           renderTable(bc2_dl_data())
  )
})

output$render_sp2_table <- renderUI({
  tags$div(style = shift_table_fn(sp2_dims()[[2]]),
           renderTable(sp2_dl_data())
  )
})

output$render_cf2_table <- renderUI({
  tags$div(style = shift_table_fn(cf2_dims()[[2]]),
           renderTable(cf2_dl_data())
  )
})

output$downloadDataButton2 <- downloadHandler(
  filename = function() { paste0(input$tabset_panel_dual, "_data.csv") },
  content = function(file) {
    write.csv(dl2_tab_to_data[[input$tabset_panel_dual]](), file, row.names = FALSE)
  }
)