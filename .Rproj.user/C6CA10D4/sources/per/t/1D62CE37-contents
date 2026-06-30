#### UI Plot Helper ####
output$show_all_analysis_ph <- renderUI({
  checkboxInput("show_all_analysis_ph",
                "Show all analysis functions",
                value = isolate(sv$show_all_analysis_ph_choice))
})

observeEvent(input$show_all_analysis_ph, {sv$show_all_analysis_ph_choice <<- input$show_all_analysis_ph})

outputOptions(output, "show_all_analysis_ph", suspendWhenHidden = FALSE)

output$graph_type_ph <- renderUI({
  selectInput("graph_type_ph",
              "What type of graph would you like to plot?",
              choices = c("", ctrlsTabsDescs_csv$Tab_name),
              selected = ""
  )
})

outputOptions(output, "graph_type_ph", suspendWhenHidden = FALSE)

go_ready <- reactive({
  if (isTruthy(input$graph_type_ph)) {
    input$graph_type_ph == "Plate View" ||
      input$graph_type_ph == "Curve Fit" ||
      (input$graph_type_ph == "Bar Chart" || input$graph_type_ph == "Scatter Plot") & 
      isTruthy(input$plot_x_axis) & isTruthy(input$group_fill)
  } else {
    FALSE
  }
})

# Go button
output$go_ph <- renderUI({
  if(go_ready()) {
    actionButton_fn("go_ph","Go!","darkgreen")
  }
})

observeEvent(input$go_ph, {
  if (data_info()$multiplate_incl & input$graph_type_ph != "Plate View") {
    updateSelectInput(session, "multiplate_n", selected = input[["dataSelection-pv_plate_n"]])
  }
  updateTabsetPanel(session, inputId = "tabset_panel", selected = input$graph_type_ph)
})

output$ph_descriptions <- renderUI({
  
  tagList(
    if (data_info()$multiplate_incl) {
      tagList(
        h4(tags$b("Plate")),
        h5("When multiple data plates are loaded, you can chose which plate data to view here. Bar Chart, Scatter Plot, and Curve Fit graphs allow loading multiple plates.")
      )
    },
    
    if (data_info()$fields_incl) {
      tagList(
        h4(tags$b("Field")),
        h5("When multiple image fields per well are included in the data, you can chose which field(s) to include and whether to display the average (avg) or sum of all fields loaded.
           Analysis calculations are performed on the avg/sum.")
      )
    },
    
    h4(tags$b("Raw Data")),
    h5("All numerical uploaded HCS data parameters can be selected here. Select the data you want to plot or analyze."),

    h4(tags$b("Filter Raw Data")),
    h5("Raw Data options can be filtered for convenience if dealing with many HCS data parameters. Filters are case sensitive and support regular expressions."),

    h4(tags$b("Analysis")),
    h5("Analysis options are filtered based on ThermoFisher CellInsight HCS Raw Data names.
      If using data from another HCS platform or to see all built-in analysis options, check the 'Show all Analysis options' box
      to display all built-in calculations; however, the calculations may not support the uploaded data and/or may not make sense."),
    
    if (mutate_null_fn(input_dataSelection()$analysis) == "") {
      h5("Select the Analysis function you would like to perform on the Raw Data. Leave this field empty to plot the Raw Data")
    } else {
      tagList(
        h4(tags$b("Append Data (+)")),
        h5("The '+' button will save the analyzed data to the availale Raw Data parameters. This analysis will ignore any filters (if applied) 
           and will save the entire analyzed data set. Saved analysis data will be lost if changes to Excluded well(s) are made.
           This is useful for quick loading of analyzed data or for performing sequential and non-sequential analyses of analyzed data."),
        br(),
        h4(input_dataSelection()$analysis),
        HTML(analysisFuncs_csv$Description[analysisFuncs_csv$Analysis == input_dataSelection()$analysis]),
        br(),
      )
    }
  )
})

# Display in-line equation using MathJax
output$ph_equations <- renderUI({
  # withMathJax(" \\(100\times \frac{Sample}{\mu_{Control}}\\)")
  
  req(input_dataSelection()$analysis)
  if (!input_dataSelection()$analysis %in% analysisFuncs_csv$Analysis[analysisFuncs_csv$Equation != ""]) {  # Check for equation entry
    return(NULL)
  } else {
    eq <- analysisFuncs_csv$Equation[analysisFuncs_csv$Analysis == input_dataSelection()$analysis]
    withMathJax(paste0("  \\(", eq, "\\)"))
  }
})

# Additional description
output$ph_descriptions_2 <- renderUI({
  req(input_dataSelection()$analysis)
  tagList(
    br(),
    HTML(analysisFuncs_csv$Description_2[analysisFuncs_csv$Analysis == input_dataSelection()$analysis])
  )
})

# Second equation - currently used for plate Z Factor
output$ph_equations_2 <- renderUI({
  req(input_dataSelection()$analysis)
  if (!input_dataSelection()$analysis %in% analysisFuncs_csv$Analysis[analysisFuncs_csv$Equation_2 != ""]) {  # Check for equation entry
    return(NULL)
  } else {
    eq <- analysisFuncs_csv$Equation_2[analysisFuncs_csv$Analysis == input_dataSelection()$analysis]
    withMathJax(paste0("  \\(", eq, "\\)"))
  }
})

output$ph_analysis_ref <- renderUI({
  if (input_dataSelection()$analysis_multi_input[1] == "analysis_compare") {
    tagList(
      br(),
      h4(tags$b("Compare to:")),
      h5("When an analysis function compares Unknown Sample wells to Positive Control, Negative Control, or Uninfected Control Sample wells, the user can select the comparison (if defined) under 'Compare to'."),
      HTML(ctrlsTabsDescs_csv$Control_description[ctrlsTabsDescs_csv$Control == input_dataSelection()$analysis_compare]),
      br(),
    )
  } else if (input_dataSelection()$analysis_multi_input[1] == "analysis_reference") {
    tagList(
      br(),
      h4(tags$b("Reference")),
      h5("When an analysis function compares one HCS data parameter ('Raw Data') with another HCS data parameter, the second data parameter must be selected under 'Reference'.
         The analysis function may compare well to well or to the average of replicate wells based on layout definitions."),
      br(),
    )
  } else if (input_dataSelection()$analysis_multi_input[1] == "analysis_factor") {
    tagList(
      br(),
      h4(tags$b("Factor")),
      h5("When an analysis function transforms the 'Raw Data' HCS data parameter by a constant, the constant can be entered under 'Factor'.
         All wells (and all image fields if applicable) will be transformed by this constant"),
      br(),
    )
  }
})

output$ph_tabs <- renderUI({
  req(input$graph_type_ph)
  tagList(
    h3(input$graph_type_ph),
    HTML(ctrlsTabsDescs_csv$Tab_description[ctrlsTabsDescs_csv$Tab_name == input$graph_type_ph]),
    br(),
    
    if (input$graph_type_ph == "Plate View") {
      tagList(
        h4(tags$b("Highlight Outliers")),
        h5("Check this box to find and highlight outlier data."),
        
        if (input$pv_outlier) {
          tagList(
            h4(input$pv_outlier_function),
            
            if (input$pv_outlier_function == "Z-Score") {
              h5(paste0("The standard deviation (SD) from the mean of ",
                        ifelse(input$pv_outlier_group == "by plate",
                               "all values of the plate are calculated for each well. ",
                               "technical replicates (by layout definition) are calculated for each well within the technical replicates. "),
                        "By default, outliers are defined as having a z-score 3 standard deviations away from the mean, but can be adjusted using the 'SD' input."))
            } else if (input$pv_outlier_function == "Interquartile Range") {
              h5(paste0("Outliers are determined as below the lower limit (Q1 - 1.5*IQR) or above the upper limit (Q3 + 1.5*IQR) based on ",
                        ifelse(input$pv_outlier_group == "by plate",
                               "all plate values.",
                               "technical replicate values.")))
            },
            h5("Outliers can be calculated based on the whole plate ('by plate') or technical replicates based on identical layout definitions ('by definition')."),
            br(),
            h4(tags$b("Exclude Outliers(s) / Include All / Exclude Edge Wells")),
            h5("Data from excluded wells will be excluded from all analysis and/or plots, except the Plate View Heatmap (if 'Ignore Empty Definitions' is not checked). 
               Data and layout definitions will not be lost and can be re-included using the Include All button. 
               Excluding and re-including data can also be performed on the 'Plate Layout Entry tab'. Mouse over these buttons/checkbox for additional details.")
          ) #tagList
        },
        br(),
      )
    }
  )
})

output$ph_additional_options <- renderTable({
  req(input$graph_type_ph)
  ind_r <- which(grepl(input$graph_type_ph, graphOptDescs_csv$Tab_source))
  ind_c <- match(c("Additional Options", "Option Descriptions"), names(graphOptDescs_csv))
 
  return(graphOptDescs_csv[ind_r, ind_c])
})#, caption = "Additional Options", caption.placement = "top")
