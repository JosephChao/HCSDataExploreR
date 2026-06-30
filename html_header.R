html_header <- tagList(
  useShinyjs(),  # Include shinyjs for reset, show/hide, enable/disable
  
  tags$head(
    # CSS Styles
    tags$style(HTML("
    .green-select div > select,
     .green-select .selectize-input,
     .green-select .form-control {
       border: 1px solid green !important;
       border-color: green !important;
     }
     .green-select .selectize-dropdown-content {
       border-color: green !important;
     }
     .green-select .selectize-input::after,
     .green-select .selectize-input .selectize-input::after {
       border-color: green transparent transparent transparent !important;
     }
     .green-select .control-label {
       color: green !important;
     }
     
     .green-numeric .form-control {
    border: 1px solid green !important;
  }
  .green-numeric .control-label {
    color: green !important;
  }
  
    
      /* Padding utilities */
      .tight-left {
        padding-left: 2px !important;
      }
      
      .tight-right {
        padding-right: 2px !important;
      }
      
      .tight-left-right {
        padding-left: 1px !important;
        padding-right: 1px !important;
      }
      
      /* Compact selectize input styling */
      .tightSelect .selectize-control.single .selectize-input {
        width: 119px;
        min-height: 20px !important;
        padding: 0 4px !important;
      }

      /* Hide selectize dropdown arrow */
      .tightSelect .selectize-control.single .selectize-input::after {
        display: none !important;
      }
      
      /* Custom navbar buttons */
      .custom-navbar-buttons {
        position: absolute;
        top: 0;
        right: 20px;
        display: flex;
        gap: 10px;
        align-items: center;
        height: 49px;
        z-index: 1000;
      }
      
      .save_proj_btn {
        height: 49px;
        display: flex;
        align-items: center;
        padding: 0 12px;
        font-size: 14px;
      }
      
      .info_help_btn {
        border-radius: 50%;
        width: 36px;
        height: 36px;
        font-size: 24px;
        text-align: center;
        background-color: #31b0d5;
        color: white;
        border: none;
        margin-top: 4px;
        display: flex;
        align-items: center;
        justify-content: center;
      }

      .circle-button:hover {
        background-color: #0056b3;
      }

      .circle-button i {
        margin: 0;
      }
      
      /* MathJax equation sizing */
      .MathJax {
        font-size: 2em !important;
      }
      
      /* Module color schemes */
      .module-dataSelection_dual1 label {
        color: #3A5FCD;
      }
      
      .module-dataSelection_dual2 label {
        color: orange;
      }
      
      /* HCS Upload data settings button */
      .btn-HCS_upload_settings {
        background-color: white;
        color: gray;
      }
    ")),
    
    # JavaScript for keyboard detection
    tags$script(HTML("
      // Detect Ctrl and Shift key states
      document.addEventListener('keydown', function(e) {
        if (e.key === 'Control') {
          Shiny.setInputValue('ctrlKey', true, {priority: 'event'});
        }
        if (e.key === 'Shift') {
          Shiny.setInputValue('shiftKey', true, {priority: 'event'});
        }
      });

      document.addEventListener('keyup', function(e) {
        if (e.key === 'Control') {
          Shiny.setInputValue('ctrlKey', false, {priority: 'event'});
        }
        if (e.key === 'Shift') {
          Shiny.setInputValue('shiftKey', false, {priority: 'event'});
        }
      });
    ")),
    
    # Window dimension detection
    tags$script(HTML("
      var dimension = [0, 0];
      $(document).on('shiny:connected', function(e) {
        dimension[0] = window.innerWidth;
        dimension[1] = window.screen.height;
        Shiny.onInputChange('dimension', dimension);
      });
      
      $(window).resize(function(e) {
        dimension[0] = window.innerWidth;
        dimension[1] = window.screen.height;
        Shiny.onInputChange('dimension', dimension);
      });
    ")),
    
    # Custom dropdown close handler
    tags$script(HTML("
      Shiny.addCustomMessageHandler('closeDropdown', function(message) {
        $('#' + message.inputId).dropdown('toggle');
      });
    ")),
    
    # Numeric input validation for threshold_line
    tags$script(HTML("
      $(document).on('input', '#threshold_line, #data_upload_xl_sheet', function() {
        var val = $(this).val();
        var regex = /^[0-9,. ]*$/;
        if (!regex.test(val)) {
          $(this).val(val.slice(0, -1));
        }
      });
    "))
  ),
  
  # Slider skin configuration
  shinyWidgets::chooseSliderSkin("Modern", color = "LightSlateGray"),
)