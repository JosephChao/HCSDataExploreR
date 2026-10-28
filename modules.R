####     UI data selection: Field, Raw, Raw Filters, Analysis, Compare, Reference, Factor ####
dataSelection_moduleUI <- function(id) {
  ns <- NS(id)
  tagList(
    div(
      id = ns("module-wrapper"),
      class = paste0("module-", id),
      
      conditionalPanel(
        condition = "input.navbar == 'Analysis' && ['Plate View', 'Z Factor', 'Plot Helper'].includes(input.tabset_panel)",
        uiOutput(ns("pv_plate_n"))
      ),
      
      conditionalPanel(
        condition = "output.fields_included_output",
        tags$div(style="display:inline-block; margin-top: 0%; width:79%",
                 title="Select which image fields to load",
                 uiOutput(ns("field_n"))),
        tags$div(style="display:inline-block; margin-top: 3.5%; float: right; width:19%",
                 title="Take the average or sum of the image fields",
                 uiOutput(ns("field_avg_sum")))
      ),
      
      uiOutput(ns("raw")),
      uiOutput(ns("raw_filters")),
      uiOutput(ns("analysis_tl")),
      
      uiOutput(ns("rename_append_tl")),
      
      conditionalPanel(
        condition = "input.analysis != ''",
        fluidRow(
          column(width = 6,
                 title = "Values with the selected layout definition will be averaged and plotted along the X-axis. Values will be averaged by unique definitions if multiple definitions are selected.",
                 style = "padding: 0px; padding-left: 15px; padding-right: 2px",
                 uiOutput(ns("operation"))),
          column(width = 6,
                 style = "padding: 0px; padding-right: 15px; padding-left: 2px",
                 uiOutput(ns(("transform")))),
        ),
        
        uiOutput(ns("compare")),
        uiOutput(ns("factor")),
        uiOutput(ns("reference")),
        uiOutput(ns("definition_col")),
        uiOutput(ns("definition")),
        uiOutput(ns("definition_group")),
        uiOutput(ns("definition_adv_group"))
      )
    ) #div
  ) #tagList
}


#### Server data selection ####
dataSelection_moduleServer <- function(id, sv, data_col_names, layout_info, data_info) {

  moduleServer(id, function(input, output, session) {

    ns <- session$ns
    
    output$pv_plate_n <- renderUI({
      if (data_info()$multiplate_incl) {
        selectInput(inputId = ns("pv_plate_n"),
                    label = "Plate",
                    choices = data_info()$plate_n_options,
                    selected = isolate(sv[[id]]$pv_plate_n_choice)
        )
      }
    })
    
    observeEvent(input$pv_plate_n, {sv[[id]]$pv_plate_n_choice <<- as.integer(input$pv_plate_n)}, ignoreInit = TRUE)
    
    output$field_n <- renderUI({
      selectInput(ns("field_n"),
                  label = "Field",
                  choices = data_info()$field_n_options,
                  multiple = TRUE,
                  selected = isolate(sv[[id]]$field_n_choice)
      )
    })
    
    observeEvent(input$field_n, {sv[[id]]$field_n_choice <<- as.integer(input$field_n)}, ignoreInit = TRUE)

    output$field_avg_sum <- renderUI({
      radioButtons(ns("field_avg_sum"),
                   label = NULL,
                   choices = c("avg", "sum"),
                   selected = isolate(sv[[id]]$field_avg_sum_choice)
      )
    })
    
    observeEvent(input$field_avg_sum, {sv[[id]]$field_avg_sum_choice <<- input$field_avg_sum}, ignoreInit = TRUE)
    
    # Select raw HCS data
    output$raw <- renderUI({
      # Filter Raw data based on input$raw_filters
      filt <- input$raw_filters
      data_cols <- data_col_names()

      if (length(filt) == 0) {
        opt <- data_cols
      } else {
        opt <- data_cols[grepl(filt, data_cols, perl=TRUE)]
      }

      selectInput(
        ns("raw"),
        label = "Raw Data",
        choices = opt,
        selected = isolate(sv[[id]]$raw_choice)
      )
    })
    
    outputOptions(output, "raw", suspendWhenHidden = FALSE)
    
    # Save Raw data selection
    observeEvent(input$raw, {sv[[id]]$raw_choice <<- input$raw}, ignoreInit = TRUE)
    
    output$raw_filters <- renderUI({
      textInput(ns("raw_filters"), label = "Filter Raw Data")
    })

    # Select Analysis function
    output$analysis_tl <- renderUI({
      opt <- analysis_choice_fn(
        RAW = input$raw,
        OPT = data_info()$analysis_options,
        ALL = sv$show_all_analysis_ph_choice || data_info()$platform != "TFCI"
      )
      
      show_append_button <- id == "dataSelection"

      tagList(
        tags$div(style = paste0("display:inline-block; margin-top: 0%; width:", ifelse(show_append_button,"79%","100%")),
                 shinyWidgets::pickerInput(
                   ns("analysis"),
                   label = "Analysis",
                   choices = opt,
                   selected = isolate(input$analysis)
                 )
        ),
        if (isTruthy(input$analysis) && show_append_button) {
          tags$div(style="display:inline-block; margin-top: 0%; width:19%",
                   title="Append Analysed Data to Raw Data Input",
                   actionButton(inputId = ns("append_data_button2"), label = "+",
                                style = paste0("padding: 2px; font-size: 140%; width: 90%; color:", "blue"))
          )
        }
      ) #tagList
    })
    outputOptions(output, "analysis_tl", suspendWhenHidden = FALSE)
    
    analysed_data_name <- reactive({
      req(input$analysis)
      if (analysis_multi_input() == "") {
        suff <- input$analysis
      } else if (analysis_multi_input() == "operation"){
        suff <- paste(input$operation, paste(input[[input$transform]], collapse = "."), sep = "_")
      } else {
        suff <- paste(input$analysis, input[[analysis_multi_input]], sep = "_")
      }
      
      return(paste0("+", input$raw, "_", suff))
    })
    
    show_append <- reactiveVal(FALSE)
    
    observeEvent(input$append_data_button2, {
      show_append(!show_append())
    })
    
    output$rename_append_tl <- renderUI({
      if (show_append()) {
        if (analysis_multi_input()[1] == "") {
          suff <- input$analysis
        } else if (analysis_multi_input()[1] == "operation"){
          suff <- paste(input$operation, paste(input[[input$transform]], collapse = "."), sep = "_")
        } else {
          suff <- paste(input$analysis, input[[analysis_multi_input()[1]]], sep = "_")
        }
        
        data_name <- paste0("+", input$raw, "_", suff)
        
        tagList(
          tags$div(style = "display:inline-block; margin-top: 0%; width: 79%",
                   title = "How the analyzed data will appear in the Raw Data input. Cannot have the same name as an existing Raw Data parameter.",
                   textInput(ns("append_data_name"), label = "Data Name", value = data_name)
          ),
          tags$div(style="display:inline-block; margin-top: 0%; width:19%",
                     title="Append Analysed Data to Raw Data Input",
                     actionButton(inputId = ns("append_data_button"), label = "Add",
                                  style = paste0("padding: 2px; width: 90%; height: 35px; color:", "blue"))
            )
        )
      }
    })
    
    observeEvent(input$append_data_button, {
      show_append(FALSE)
    })
    # Save Analysis selection
    observeEvent(input$analysis, {sv[[id]]$analysis_choice <<- input$analysis}, ignoreInit = TRUE)

    analysis_multi_input <- reactive({
      if (mutate_null_fn(input$analysis, "") != "") {
        c(
          multi_input = analysisFuncs_csv$multi_input[analysisFuncs_csv$Analysis == input$analysis],
          filter_reference = analysisFuncs_csv$filter_reference[analysisFuncs_csv$Analysis == input$analysis]
        )
      } else {
        c(multi_input = "F",
          filter_reference = "F")
      }
    })
    
    output$operation <- renderUI({
      if (analysis_multi_input()[1] == "operation") {
        selectInput(
          ns("operation"),
          label = "Operation",
          choices = c("Y = Y * K",
                      "Y = Y + K",
                      "Y = Y / K",
                      "Y = Y - K",
                      "Y = K / Y",
                      "Y = K - Y",
                      "Y = K ^ Y",
                      "Y = Y ^ K",
                      "Y = (Y - K) / K * 100",
                      "Y = (K - Y) / Y * 100"),
          selected = isolate(sv[[id]]$operation_choice)
        )
      }
    })
    
    output$transform <- renderUI({
      if (analysis_multi_input()[1] == "operation") {
        selectInput(
          ns("transform"),
          label = "K =",
          choices = c("factor", "reference", "definition"),
          selected = isolate(sv[[id]]$transform_choice)
        )
      }
    })
    
    observeEvent(input$operation, {sv[[id]]$operation_choice <<- input$operation}, ignoreInit = TRUE)
    observeEvent(input$transform, {sv[[id]]$transform_choice <<- input$transform}, ignoreInit = TRUE)
    
    output$compare <- renderUI({
        if (analysis_multi_input()[1] == "compare") {
        selectInput(
          ns("compare"),
          label = "Compare to:",
          choices = layout_info()$compare_options,
          selected = isolate(sv[[id]]$compare_choice)
        )
      }
    })
    observeEvent(input$compare, {sv[[id]]$compare_choice <<- input$compare}, ignoreInit = TRUE)

    output$reference <- renderUI ({
      if (analysis_multi_input()[1] == "reference" | (analysis_multi_input()[1] == "operation" & mutate_null_fn(input$transform) == "reference")) {

        opt <- data_col_names()
        
        if (data_info()$platform == "TFCI" & analysis_multi_input()[2] != "ALL") {
          opt <- opt[grepl(analysis_multi_input()[2], opt, perl=TRUE)]
        }

        selectInput(
          ns("reference"),
          label = "Reference:",
          choices = opt,
          selected = isolate(sv[[id]]$reference_choice)
        )
      }
    })
    # Save Reference selection
    observeEvent(input$reference, {sv[[id]]$reference_choice <<- input$reference}, ignoreInit = TRUE)

    output$factor <- renderUI ({
      if (analysis_multi_input()[1] == "factor" | (analysis_multi_input()[1] == "operation" & mutate_null_fn(input$transform) == "factor")) {
        
        numericInput(
          ns("factor"),
          label = "Factor",
          value = isolate(sv[[id]]$factor_choice)
        )
      }
    })
    observeEvent(input$factor, {sv[[id]]$factor_choice <<- input$factor}, ignoreInit = TRUE)
    
    output$definition_col <- renderUI({
      if (analysis_multi_input()[1] == "definition" | (analysis_multi_input()[1] == "operation" & mutate_null_fn(input$transform) == "definition")) {
        selectInput(
          ns("definition_col"),
          label = "Definition",
          choices = layout_info()$col_names,
          selected = isolate(sv[[id]]$definition_col_choice)
        )
      }
    })
    observeEvent(input$definition_col, {sv[[id]]$definition_choice <<- input$definition_col}, ignoreInit = TRUE)
    
    output$definition <- renderUI({
      if (analysis_multi_input()[1] == "definition" | (analysis_multi_input()[1] == "operation" & mutate_null_fn(input$transform) == "definition")) {
        selectInput(
          ns("definition"),
          label = "Selection",
          choices = layout_info()$definition_options[[req(input$definition_col)]],
          multiple = TRUE,
          selected = isolate(sv[[id]]$definition_choice)
        )
      }
    })
    observeEvent(input$definition, {sv[[id]]$definition_choice <<- input$definition}, ignoreInit = TRUE)

    output$definition_group <- renderUI({
      if (analysis_multi_input()[1] == "definition" | (analysis_multi_input()[1] == "operation" & mutate_null_fn(input$transform) == "definition")) {
        selectInput(
          ns("definition_group"),
          label = "Grouping",
          choices = layout_info()$col_names,
          multiple = TRUE,
          selected = isolate(sv[[id]]$definition_group_choice)
        )
      }
    })
    observeEvent(input$definition_group, {sv[[id]]$definition_group_choice <<- input$definition_group}, ignoreInit = TRUE)
    
    return(
        reactive({
          req(analysis_multi_input())
          if (data_info()$multiplate_incl) {
            pln <- input$pv_plate_n
          } else {
            pln <- 1
          }
          
          if (analysis_multi_input()[1] == "operation") {
            tform <- c(req(input$operation), req(input$transform))
          } else {
            tform <- c(NULL, NULL)
          }
          
          if ("reference" %in% c(analysis_multi_input()[1], mutate_null_fn(input$transform))) {
            ref <- req(input$reference)
          } else {
            ref <- NULL
          }
          
          if ("definition" %in% c(analysis_multi_input()[1], mutate_null_fn(input$transform))) {
            inp_def <- req(input$definition_col)
            
            if (length(input$definition) == 0) {
              seln <- layout_info()$definition_options[[inp_def]]
            } else {
              seln <- input$definition
            }
            
            def <- list(inp_def, seln, input$definition_group)
          } else {
            def <- list(NULL, NULL, NULL)
          }
          
          list(
            pv_plate_n = as.integer(pln),
            field_n = as.integer(input$field_n),
            field_avg_sum = input$field_avg_sum,
            raw_filters = input$raw_filters,
            raw = input$raw,
            analysis = input$analysis,
            analysis_multi_input = analysis_multi_input()[1],
            operation = tform[1],
            transform = tform[2],
            compare = input$compare,
            reference = ref,
            factor = input$factor,
            definition_col = def[[1]],
            definition = def[[2]],
            definition_group = def[[3]]
          )
        })
    )

  })
}

#### Customization Options ####
customization_table_fn <- function(ID, TTL, DSID, VAL=NULL, PH, FSID, FSVAL=NULL, LSID=NULL, LSVAL=NULL, LAID=NULL, LAVAL=NULL, LPID=NULL, LPSEL=NULL) {
  tags$tr(
    tags$td(style = "font-weight: bold; padding: 8px; padding-left: 10px; white-space: nowrap; width = 12%", TTL),
    tags$td(div(style = "padding-right: 10px;", textInput(NS(ID, DSID), label = NULL, value = VAL, width = "150%", placeholder = PH))),  # 174% for 4 col
    tags$td(div(style = "padding-right: 10px;", numericInput(NS(ID, FSID), label = NULL, value = FSVAL, min = 0))),
    if (!is.null(LSID)) {
      tags$td(div(style = "padding-right: 10px;", numericInput(NS(ID, LSID), label = NULL, value = LSVAL, min = 0)))
    },
    if (!is.null(LAID)) {
      tags$td(div(style = "padding-right: 10px;", numericInput(NS(ID, LAID), label = NULL, value = LAVAL, min = 0, max = 90, step = 45)))
    },
    if (!is.null(LPID)) {
      tags$td(div(style = "padding-right: 10px;", selectInput(NS(ID, LPID), label = NULL, choices = c("right", "bottom", "top", "left"), selected = LPSEL)))
    }
  )
}

# Module UI
customization_moduleUI <- function(id) {
  ns <- NS(id)
  
  fluidRow(
    column(width = 3,
           # UI elements for the dropdown
           shinyWidgets::dropdownButton(
             inputId = ns("customization_dropdown"),
             
             fluidRow(
               column(width = 9,
                      actionButton(ns("customization_apply"), "Apply",
                                   style = paste0("padding: 10px; width: 100px; color: white; background-color:", "#31b0d5"))),
               column(width = 3,
                      uiOutput(ns("show_legend")))
             ),
             
             fluidRow(
               div(style = "overflow-y: auto; max-height: 400px;",
                   uiOutput(ns("title_font_table_tl"))
               )
             ),
             fluidRow(
               column(width = 3,
                      conditionalPanel(
                        condition = "input.navbar == 'Dual Analysis' || input.tabset_panel != 'Plate View' && input.tabset_panel != 'Z Factor'",
                        uiOutput(ns("data_pt_sz"))
                      ),
                      conditionalPanel(
                        condition = "input.navbar == 'Analysis' && (input.tabset_panel == 'Plate View' || input.tabset_panel == 'Z Factor')",
                        uiOutput(ns("data_txt_sz"))
                      )
               ),
               column(width = 3,
                      tags$div(title = "Set height in pixels, eg. '500'. Leave empty for standard size.",
                               uiOutput(ns("graph_height")))
               ),
               column(width = 3,
                      tags$div(title = "Set width in pixels, eg. '1000'. Leave empty for standard size.",
                               uiOutput(ns("graph_width")))
               ),
               column(width = 3,
                      uiOutput(ns("legend_key_sz"))
               )
             ),
             fluidRow(
               column(width = 6,
                      tags$div(title = "Reorder values on the X-axis. Requires a minimum of two inputs. Unused inputs will be hidden.",
                               uiOutput(ns("x_axis_order")))),
               column(width = 6,
                      tags$div(title = "Reorder grouped values. Requires a minimum of two inputs. Unused inputs will be hidden.",
                               uiOutput(ns("group_order"))
                      )
               )
             ),
             fluidRow(
               column(width = 6,
                      tags$div(title = "Reorder horizontal grids. Requires a minimum of two inputs. Unused inputs will be hidden.",
                               uiOutput(ns("multigroup_x_order")))),
               column(width = 6,
                      tags$div(title = "Reorder vertical grids. Requires a minimum of two inputs. Unused inputs will be hidden.",
                               uiOutput(ns("multigroup_y_order"))
                      )
               )
             ),
             conditionalPanel(
               condition = "input.tabset_panel == 'Plate View' && input.navbar == 'Analysis'",
               
               fluidRow(
                 column(width = 4,
                        uiOutput(ns("cp_scale_n"))),
                 column(width = 4,
                        uiOutput(ns("cp_auto_val"))),
                 column(width = 4,
                        colourpicker::colourInput(
                          ns("cp_text_colr"),
                          "Text Colour",
                          returnName = TRUE,
                          allowTransparent = TRUE,
                          value = "white"
                        )
                 )
               ),
               uiOutput(ns("cp_colr")),
               uiOutput(ns("cp_val")),
             ),
             
             conditionalPanel(
               condition = "input.tabset_panel == 'Curve Fit' && input.navbar == 'Analysis'",
               
               fluidRow(
                 column(width = 3,
                        uiOutput(ns("ic_show_val"))),
                 column(width = 3,
                        uiOutput(ns("ic_show_threshold"))),
                 column(width = 3,
                        uiOutput(ns("ic_position"))),
                 column(width = 3,
                        uiOutput(ns("ic_name"))),
               )
             ),
             circle = FALSE, status = "info", icon = icon("cog"), width = "600px",
             tooltip = shinyWidgets::tooltipOptions(title = "Customize plot appearance")
           )
    ),
    
    column(width = 6),
    
    column(width = 3, align = "right",  # Align right for both buttons
             tags$div(
               title = "Download Plot",
               style = "position:relative; display:inline-block",
               downloadButton(
                 outputId = ns("downloadPlotButton"), label = "png",
                 style = paste0(
                   "color:", "blue", "; position:relative"
                 )
               )
             ),
    )  #close column
  )  #close fluidRow
}

# Module Server
customization_moduleServer <- function(id, current_tab, sv, plate_layout_def, plot_image, plot_dim, x_inputs = reactive({list(plot_x_axis = NULL, group_fill = NULL, multigroup_x = NULL, multigroup_y = NULL)}), sp_data2_threshold = reactive(NULL)) {
  
  moduleServer(id, function(input, output, session) {
    ns <- session$ns
    
    output$title_font_table_tl <- renderUI({
      
      ylaid <- "y_label_angle"
      xlaid <- "x_label_angle"
      la_title <- "Angle/Position"
      
      tagList(
        tags$table(
          style = "width: 100%; table-layout: fixed;",  # Ensure table layout is fixed for column width control
          # Column headers
          tags$tr(
            tags$th(style = "width: 12%; padding: 8px;", ""),
            tags$th(style = "width: 40%; padding: 8px;", "Title"),  # 58%
            tags$th(style = "width: 15%; padding: 8px;", "Title Size"),
            tags$th(style = "width: 15%; padding: 8px;", "Label Size"),
            tags$th(style = "width: 18%; padding: 1px;", la_title)
          ),
          customization_table_fn(ID = id,
                                TTL = "Title",
                                DSID = "graph_title",
                                VAL = isolate(sv[[id]]$graph_title_choice),
                                PH = "Enter Plot title",
                                FSID = "graph_title_font_sz",
                                FSVAL = isolate(sv[[id]]$graph_title_font_sz_choice)),
          customization_table_fn(ID = id,
                                TTL = "Y-axis",
                                DSID = "y_title",
                                VAL = isolate(sv[[id]]$y_title_choice),
                                PH = "Enter Y-axis title",
                                FSID = "y_title_font_sz",
                                FSVAL = isolate(sv[[id]]$y_title_font_sz_choice),
                                LSID = "y_label_font_sz",
                                LSVAL = isolate(sv[[id]]$y_label_font_sz_choice),
                                # LAID = "y_label_angle",
                                LAID = ylaid,
                                LAVAL = isolate(sv[[id]]$y_label_angle_choice)),
          customization_table_fn(ID = id,
                                TTL = "X-axis",
                                DSID = "x_title",
                                VAL = isolate(sv[[id]]$x_title_choice),
                                PH = "Enter X-axis title",
                                FSID = "x_title_font_sz",
                                FSVAL = isolate(sv[[id]]$x_title_font_sz_choice),
                                LSID = "x_label_font_sz",
                                LSVAL = isolate(sv[[id]]$x_label_font_sz_choice),
                                # LAID = "x_label_angle",
                                LAID = xlaid,
                                LAVAL = isolate(sv[[id]]$x_label_angle_choice)),
          customization_table_fn(ID = id,
                                 TTL = "Legend",
                                 DSID = "legend_title",
                                 VAL = isolate(sv[[id]]$legend_title_choice),
                                 PH = "Enter Legend title",
                                 FSID = "legend_title_font_sz",
                                 FSVAL = isolate(sv[[id]]$legend_title_font_sz_choice),
                                 LSID = "legend_label_font_sz",
                                 LSVAL = isolate(sv[[id]]$legend_label_font_sz_choice),
                                 LPID = "legend_position",
                                 LPSEL = isolate(sv[[id]]$legend_position_choice))
        )
      )
    })
    observeEvent(input$graph_title, {sv[[id]]$graph_title_choice <<- input$graph_title}, ignoreInit = TRUE)
    observeEvent(input$graph_title_font_sz, {sv[[id]]$graph_title_font_sz_choice <<- input$graph_title_font_sz}, ignoreInit = TRUE)
    observeEvent(input$y_title, {sv[[id]]$y_title_choice <<- input$y_title}, ignoreInit = TRUE)
    observeEvent(input$y_title_font_sz, {sv[[id]]$y_title_font_sz_choice <<- input$y_title_font_sz}, ignoreInit = TRUE)
    observeEvent(input$y_label_font_sz, {sv[[id]]$y_label_font_sz_choice <<- input$y_label_font_sz}, ignoreInit = TRUE)
    observeEvent(input$x_title, {sv[[id]]$x_title_choice <<- input$x_title}, ignoreInit = TRUE)
    observeEvent(input$x_title_font_sz, {sv[[id]]$x_title_font_sz_choice <<- input$x_title_font_sz}, ignoreInit = TRUE)
    observeEvent(input$x_label_font_sz, {sv[[id]]$x_label_font_sz_choice <<- input$x_label_font_sz}, ignoreInit = TRUE)
    observeEvent(input$legend_title, {sv[[id]]$legend_title_choice <<- input$legend_title}, ignoreInit = TRUE)
    observeEvent(input$legend_title_font_sz, {sv[[id]]$legend_title_font_sz_choice <<- input$legend_title_font_sz}, ignoreInit = TRUE)
    observeEvent(input$legend_label_font_sz, {sv[[id]]$legend_label_font_sz_choice <<- input$legend_label_font_sz}, ignoreInit = TRUE)
    observeEvent(input$x_label_angle, {sv[[id]]$x_label_angle_choice <<- input$x_label_angle}, ignoreInit = TRUE)
    observeEvent(input$y_label_angle, {sv[[id]]$y_label_angle_choice <<- input$y_label_angle}, ignoreInit = TRUE)
    observeEvent(input$legend_position, {sv[[id]]$legend_position_choice <<- input$legend_position}, ignoreInit = TRUE)
    
    output$data_pt_sz <- renderUI({
        numericInput(ns("data_pt_sz"), label = "Data point size", min = 0,
                     value = isolate(sv[[id]]$data_pt_sz_choice))
    })
    
    output$data_txt_sz <- renderUI({
        numericInput(ns("data_txt_sz"), label = "Data text size", min = 0,
                     value = isolate(sv[[id]]$data_txt_sz_choice))
    })
    observeEvent(input$data_pt_sz, {sv[[id]]$data_pt_sz_choice <<- input$data_pt_sz}, ignoreInit = TRUE)
    observeEvent(input$data_txt_sz, {sv[[id]]$data_txt_sz_choice <<- input$data_txt_sz}, ignoreInit = TRUE)
    
    output$graph_height <- renderUI({
      numericInput(ns("graph_height"), label = "Graph Height",
                   value = isolate(sv[[id]]$graph_height_choice))
    })
    observeEvent(input$graph_height, {sv[[id]]$graph_height_choice <<- input$graph_height}, ignoreInit = TRUE)
    
    output$graph_width <- renderUI({
      numericInput(ns("graph_width"), label = "Graph Width",
      value = isolate(sv[[id]]$graph_width_choice))
    })
    observeEvent(input$graph_width, {sv[[id]]$graph_width_choice <<- input$graph_width}, ignoreInit = TRUE)
    
    output$show_legend <- renderUI({
      checkboxInput(ns("show_legend"),label = "Show Legend",
                    value = isolate(sv[[id]]$show_legend_choice))
    })
    observeEvent(input$show_legend, {sv[[id]]$show_legend_choice <<- input$show_legend}, ignoreInit = TRUE)
    
    output$legend_key_sz <- renderUI({
      numericInput(ns("legend_key_sz"), label = "Legend Size",
                   value = isolate(sv[[id]]$legend_key_sz_choice))
    })
    observeEvent(input$legend_key_sz, {sv[[id]]$legend_key_sz_choice <<- input$legend_key_sz}, ignoreInit = TRUE)
    
    output$x_axis_order <- renderUI({
      if (current_tab()$active %in% c("Bar Chart", "Scatter Plot", "Bar Chart 2")) {
        
        if (length(x_inputs()$plot_x_axis) == 0) {return(NULL)}  # needed when switching from PV customization to BC/SP chart
        if (x_inputs()$plot_x_axis %in% Numeric_defs & current_tab()$active == "Scatter Plot") {
          return(NULL)
        } else {
          selectInput(ns("x_axis_order"),
                      label = "Order X-axis",
                      choices = unique(plate_layout_def()[[req(x_inputs()$plot_x_axis)]]),
                      multiple = TRUE,
                      selected = isolate(sv[[id]]$x_axis_order_choice)
          )
        }
      }
    })
    observeEvent(input$x_axis_order, {
      if (length(input$x_axis_order) > 1) {
        sv[[id]]$x_axis_order_choice <<- input$x_axis_order
      } else {
        sv[[id]]$x_axis_order_choice <<- NULL
      }
    }, ignoreNULL = FALSE, ignoreInit = TRUE)
    
    output$group_order <- renderUI({
      
      if (!current_tab()$active %in% c("Plate View", "Curve Fit", "Bar Chart 2", "Curve Fit 2")) {
        if (current_tab()$active == "Scatter Plot 2") {
          opt <- unique(sp_data2_threshold()[[req(x_inputs()$group_fill)]])
          ttl <- ifelse(x_inputs()$group_fill == "Threshold",
                        "Reorder colours of Sector coordinates (x,y). Eg, 1,1 is bottom left sector. Requires a minimum of two inputs. Unused inputs will be hidden.",
                        "Reorder grouped values. Requires a minimum of two inputs. Unused inputs will be hidden.")
        } else {
          opt <- unique(plate_layout_def()[[req(x_inputs()$group_fill)]])
          ttl <- "Reorder grouped values. Requires a minimum of two inputs. Unused inputs will be hidden."
        }
        tags$div(
          title = ttl,
          selectInput(ns("group_order"),
                      label = "Order Groups",
                      choices = opt,
                      multiple = TRUE,
                      selected = isolate(sv[[id]]$group_order_choice)
          )
        )
      }
    })
    observeEvent(input$group_order, {
      if (length(input$group_order) > 1) {
        sv[[id]]$group_order_choice <<- input$group_order
      } else {
        sv[[id]]$group_order_choice <<- NULL
      }
      
    }, ignoreNULL = FALSE, ignoreInit = TRUE)
    
    output$multigroup_x_order <- renderUI({
      if (current_tab()$active %in% c("Bar Chart", "Scatter Plot")) {
        selectInput(ns("multigroup_x_order"),
                    label = "Order Multigroup X Grid",
                    choices = unique(plate_layout_def()[[req(x_inputs()$multigroup_x)]]),
                    multiple = TRUE,
                    selected = isolate(sv[[id]]$multigroup_x_order_choice)
        )
      }
    })
    observeEvent(input$multigroup_x_order, {
      if (length(input$multigroup_x_order) > 1) {
        sv[[id]]$multigroup_x_order_choice <<- input$multigroup_x_order
      } else {
        sv[[id]]$multigroup_x_order_choice <<- NULL
      }
    }, ignoreNULL = FALSE, ignoreInit = TRUE)
    
    output$multigroup_y_order <- renderUI({
      
      if (current_tab()$active %in% c("Bar Chart", "Scatter Plot")) {
        selectInput(ns("multigroup_y_order"),
                    label = "Order Multigroup Y Grid",
                    choices = unique(plate_layout_def()[[req(x_inputs()$multigroup_y)]]),
                    multiple = TRUE,
                    selected = isolate(sv[[id]]$multigroup_y_order_choice)
        )
      }
    })
    observeEvent(input$multigroup_y_order, {
      if (length(input$multigroup_y_order) > 1) {
        sv[[id]]$multigroup_y_order_choice <<- input$multigroup_y_order
      } else {
        sv[[id]]$multigroup_y_order_choice <<- NULL
      }
      
    }, ignoreNULL = FALSE, ignoreInit = TRUE)
    
    
    output$cp_scale_n <- renderUI({
      selectInput(ns("cp_scale_n"),
                  label = "Colour Scale",
                  choices = c("2-colour", "3-colour"),
                  selected = isolate(sv[[id]]$cp_scale_n_choice))
    })
    
    output$cp_auto_val <- renderUI({
      selectInput(ns("cp_auto_val"),
                  label = "Scale Limits",
                  choices = c("auto", "value"),
                  selected = isolate(sv[[id]]$cp_auto_val_choice))
    })
    
    output$cp_colr <- renderUI({{
      tagList(
        div(style = "display: flex; align-items: center; gap: 30px;",
            colourpicker::colourInput(ns("cp_min_colr"), "Min", 
                                      returnName = TRUE,  allowTransparent = TRUE,
                                      value = isolate(sv[[id]]$cp_min_colr_choice)),
            {if (req(input$cp_scale_n) == "3-colour")
              colourpicker::colourInput(ns("cp_mid_colr"), "Mid",
                                        returnName = TRUE,  allowTransparent = TRUE,
                                        value = isolate(sv[[id]]$cp_mid_colr_choice))
            },
            colourpicker::colourInput(ns("cp_max_colr"), "Max",
                                      returnName = TRUE,  allowTransparent = TRUE,
                                      value = isolate(sv[[id]]$cp_max_colr_choice))
        )
      )
    }})
    
    output$cp_val <- renderUI({{
      if (mutate_null_fn(input$cp_auto_val == "value")) {
        tagList(
          div(style = "display: flex; align-items: center; gap: 30px;",
              numericInput(ns("cp_min_val"), label = NULL, value = isolate(sv[[id]]$cp_min_val_choice)),
              {if (req(input$cp_scale_n) == "3-colour")
                numericInput(ns("cp_mid_val"), label = NULL, value = isolate(sv[[id]]$cp_mid_val_choice))
              },
              numericInput(ns("cp_max_val"), label = NULL, value = isolate(sv[[id]]$cp_max_val_choice))
          )
        )
      }
    }})
    
    
    lapply(colorPickerInputIDs, function(x) {
      observeEvent(input[[x]], {
        sv$pv_customization[[paste0(x, "_choice")]] <<- input[[x]]
      }, ignoreInit = TRUE)
    })
    
    output$ic_show_val <- renderUI({
      checkboxInput(ns("ic_show_val"),label = "Show Interpolation",
                    value = isolate(sv[[id]]$ic_show_val_choice))
    })
    observeEvent(input$ic_show_val, {sv[[id]]$ic_show_val_choice <<- input$ic_show_val}, ignoreInit = TRUE)
    
    output$ic_show_threshold <- renderUI({
      checkboxInput(ns("ic_show_threshold"),label = "Show Threshold Line",
                    value = isolate(sv[[id]]$ic_show_threshold_choice))
    })
    observeEvent(input$ic_show_threshold, {sv[[id]]$ic_show_threshold_choice <<- input$ic_show_threshold}, ignoreInit = TRUE)
    
    output$ic_position <- renderUI({
      selectInput(ns("ic_position"),
                  label = "Position",
                  choices = c("Top Left", "Top Right", "Bottom Left", "Bottom Right"),
                  selected = isolate(sv[[id]]$ic_position_choice)
      )
    })
    observeEvent(input$ic_position, {sv[[id]]$ic_position_choice <<- input$ic_position}, ignoreInit = TRUE)
    
    output$ic_name <- renderUI({
      textInput(ns("ic_name"),
                label = "Prefix",
                value = sv[[id]]$ic_name_choice,
                placeholder = "MIC")
    })
    observeEvent(input$ic_name, {sv[[id]]$ic_name_choice <<- input$ic_name}, ignoreInit = TRUE)
    
    
    
    observeEvent(input$customization_apply, {
      # Close the dropdown manually with JavaScript
      session$sendCustomMessage(type = "closeDropdown", message = list(inputId = ns("customization_dropdown")))
    })
    
    output$downloadPlotButton <- downloadHandler(
      filename = function() { paste0(current_tab()$active, "_plot.png") },
      content = function(file) {
        w <- plot_dim()[[1]]
        h <- plot_dim()[[2]]
        
        ggsave(file, plot = plot_image(), device = "png",
               width = w,
               height = h,
               scale = min(50, 12 / sqrt(w * h)),
               dpi = 600, limitsize = FALSE)
      }
    )
  })  #close moduleServer
}

#### Plate View Data Wrangling ####
PVdataWrang_moduleServer <- function(id, data_merged, input_dataSelection, pv_inputs, layout_info, ctrl_data, ctrl_sd_data, uninf_def_ctrl_data) {
  moduleServer(id, function(input, output, session) {
    
    ctrl_data_selected <- reactive({
      dat <- select_data_index_fn(
        DAT = ctrl_data(),
        INFO = c("plate", "Sample", "control_avgs", "Control_Region"),
        RAW = input_dataSelection()$raw,
        REF = input_dataSelection()$reference)

      return(dat)
    })

    ctrl_sd_data_selected <- reactive({
      dat <- select_data_index_fn(
        DAT = ctrl_sd_data(),
        INFO = c("plate", "Sample", "control_avgs", "Control_Region"),
        RAW = input_dataSelection()$raw,
        REF = input_dataSelection()$reference)

      return(dat)
    })

    uninf_def_ctrl_data_selected <- reactive({
      dat <- select_data_index_fn(
        DAT = uninf_def_ctrl_data(),
        INFO = c("plate", "definition", "Control_Region"),
        RAW = input_dataSelection()$raw,
        REF = input_dataSelection()$reference,
        # VAL = "Value"  # NEED TO UPDATE WHEN USING DT_Utilities_analysis
        VAL = "Uninfected_Value")  # 'Uninfected_Value' must match utilities.R Cell_Survival_fn
      return(dat)
    })

    # Filter layout by plate and select Raw data and Reference columns
    pv_data_plate <- reactive({
      dat <- data_merged()

      if ("Field" %in% names(dat)) {
        fldn <- "Field"
      } else {
        fldn <- NULL
      }

      if (id != "append") {
        ind <- which(dat$plate %in% input_dataSelection()$pv_plate_n)
      } else {
        ind = NULL
      }

      select_data_index_fn(
        DAT = dat,
        IND = ind,
        INFO = layout_info()$col_names,
        RAW = req(input_dataSelection()$raw),  # req required to prevent error on first load
        REF = input_dataSelection()$reference,
        FLD = fldn
      )
    })

    # Filter and average by Field
    pv_data_field <- reactive({
      dat <- req(pv_data_plate())

      if ("Field" %in% names(dat) & id != "append") {
        fldn <- req(input_dataSelection()$field_n)
        dat <- dat[which(dat$Field %in% fldn), ]
        if (length(fldn) > 1) {
          dat <- DTsummarize_field_fn(
            DAT = dat,
            FUNC = input_dataSelection()$field_avg_sum,
            INFO = req(layout_info()$col_names)
          )
        }
      }
      return(dat)
    })

    # Perform analysis calculations
    pv_data_analysis <- reactive({
      dat_analysis_fn(DAT = pv_data_field(),
                      DS = input_dataSelection(),
                      CTRL = ctrl_data_selected(),
                      CSD = ctrl_sd_data_selected(),
                      UDC = uninf_def_ctrl_data_selected()
      )
    })

    # Average wells by definition
    pv_data_avg <- reactive({
      dat <- req(pv_data_analysis())
      if (isTruthy(pv_inputs()$pv_average)) {
        dat <- avg_fn(dat)
      }
      return(dat)
    })

    pv_data_outlier <- reactive({
      dat <- pv_data_avg()

      if (pv_inputs()$pv_outlier) {
        if (req(pv_inputs()$pv_outlier_group) == "by plate") {
          outlier_group <- "plate"
        } else {
          outlier_group <- c("definition", "Sample")
        }

        dat <- dat[which(!is.na(dat$Sample)), ]

        dat$outliers <- ave(dat$Value,
                            dat[outlier_group],
                            FUN = function(x) outlier_fns[[pv_inputs()$pv_outlier_function]](x, pv_inputs()$pv_outlier_z_score_SD))
      }

      return(dat)
    })
    
    # filter layout by empty definitions or uninfected wells based on user input
    pv_data_final <- reactive({
      dat <- pv_data_outlier()
      emp <- isTruthy(pv_inputs()$pv_ignore_empty)
      uninf <- isTruthy(pv_inputs()$pv_ignore_uninfected)
      
      if (emp && uninf) {
        dat <- dat[which(!is.na(dat$Sample) & dat$Sample != "Uninfected Control"), ]
      } else if (emp) {
        dat <- dat[which(!is.na(dat$Sample)), ]
      } else if (uninf) {
        dat <- dat[which(dat$Sample != "Uninfected Control"), ]
      }
      
      return(dat)
    })
    
    return(
      reactive({
        pv_data_final()
      })
    )
  })
}

#### BC SP CF Data Wraggling ####
GraphDataWrang_moduleServer <- function(id, sv_exclude_index, data_merged, multiplate_n, input_dataSelection, graph_inputs, x_inputs, layout_info, data_info, ctrl_data, ctrl_sd_data, uninf_def_ctrl_data) {
  moduleServer(id, function(input, output, session) {
    ctrl_data_selected <- reactive({
      dat <- select_data_index_fn(
        DAT = ctrl_data(),
        INFO = c("plate", "Sample", "control_avgs", "Control_Region"),
        RAW = input_dataSelection()$raw,
        REF = input_dataSelection()$reference)
      
      return(dat)
    })
    
    ctrl_sd_data_selected <- reactive({
      dat <- select_data_index_fn(
        DAT = ctrl_sd_data(),
        INFO = c("plate", "Sample", "control_avgs", "Control_Region"),
        RAW = input_dataSelection()$raw,
        REF = input_dataSelection()$reference)
      
      return(dat)
    })
    
    uninf_def_ctrl_data_selected <- reactive({
      dat <- select_data_index_fn(
        DAT = uninf_def_ctrl_data(),
        INFO = c("plate", "definition", "Control_Region"),
        RAW = input_dataSelection()$raw,
        REF = input_dataSelection()$reference,
        # VAL = "Value"  # NEED TO UPDATE WHEN USING DT_Utilities_analysis
        VAL = "Uninfected_Value")  # 'Uninfected_Value' must match utilities.R Cell_Survival_fn
      return(dat)
    })
    
    
    data_plate_index <- reactive({
      ind <- which(data_merged()$plate %in% multiplate_n())
      return(ind)
    })
    
    data_field_index <- reactive({
      ind <- which(data_merged()$Field %in% input_dataSelection()$field_n)
      return(ind)
    })
    
    # filter layout by sample type
    data_sample_index <- reactive({
      req(graph_inputs()$include_sample)
      ind <- which(data_merged()$Sample %in% graph_inputs()$include_sample)
      return(ind)
    })
    
    
    data_filtered <- reactive({
      p <- data_plate_index()
      s <- data_sample_index()
      
      ind <- p[p %in% s]
      
      if (length(sv_exclude_index()) > 0) {
        ind <- ind[ind %in% Reduce(intersect, sv_exclude_index())]
      }
      
      fldn <- NULL
      if (data_info()$fields_incl) {
        ind <- ind[ind %in% data_field_index()]
        fldn <- "Field"
      }
      
      dat <- select_data_index_fn(
        DAT = data_merged(),
        IND = ind,
        INFO = layout_info()$col_names,
        RAW = input_dataSelection()$raw,
        REF = input_dataSelection()$reference,
        FLD = fldn)
      
      return(dat)
    })
    
    data_field <- reactive({
      dat <- data_filtered()
      
      fldn <- req(input_dataSelection()$field_n)
      if (length(fldn) > 1) {
        dat <- DTsummarize_field_fn(
          DAT = dat,
          FUNC = input_dataSelection()$field_avg_sum,
          INFO = req(layout_info()$col_names)
        )
      }
      return(dat)
    })
    
    data_analysis <- reactive({
      if (data_info()$fields_incl) {
        dat <- data_field()
      } else {
        dat <- data_filtered()
      }
      
      dat <- dat_analysis_fn(DAT = dat,
                             DS = input_dataSelection(),
                             CTRL = ctrl_data_selected(),
                             CSD = ctrl_sd_data_selected(),
                             # CINF = ctrl_infected_data_selected()
                             UDC = uninf_def_ctrl_data_selected()
      )
      dat$Ref_Val <- NULL
      return(dat)
    })
    
    # Calculate plate average - Also feeds into Dose Response graph
    data_plate_average <- reactive({
      dat <- data_analysis()
      if (isTruthy(graph_inputs()$plate_avg)) {
        info <- c("plate", layout_info()$def_names)
        dat <- plate_average_fn(dat, info)
      }
      return(dat)
    })
    
    data_final <- reactive({
      dat <- data_plate_average()
      
      samp <- graph_inputs()$include_sample
      
      if (!is.null(x_inputs()$group_fill)) {
        grp <- req(x_inputs()$group_fill)
        
        if ("Uninfected Control" %in% samp & grp != "Sample") {
          dat[[grp]] <- ifelse(dat$Sample == "Uninfected Control", paste0("ui_", dat[[grp]]), dat[[grp]])
        }
      }
      
      return(dat)
    })
    
    return(
      reactive({
        data_final()
      })
    )
  })
}