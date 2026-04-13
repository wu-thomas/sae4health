#' The application User-Interface
#'
#' @param request Internal parameter for `{shiny}`.
#'     DO NOT REMOVE.
#' @import shiny
#' @importFrom dplyr %>%
#' @noRd
#'
#'
#'

# library(shinydashboard)
# library(sf)
# library(magrittr)
# library(sn)
# library(prettymapr)
# library(mapview)
# library(bookdown)
# library(markdown)
# library(grid)
# library(gridExtra)
# library(INLA)

app_ui <- function(request) {
  options(shiny.maxRequestSize=550*1024^2)

  tagList(
    tags$link(rel = "stylesheet", href = "https://fonts.googleapis.com/css?family=Roboto:400,700&display=swap"),
    tags$style(HTML("
          /* Add space between icons and text in sidebar menu items */
          .fas.fa-globe {
            margin-right: 6px;
          }
          .fas.fa-database {
            margin-right: 7px;
          }
          .fas.fa-sliders {
            margin-right: 6px;
          }
          .fas.fa-chart-line {
            margin-right: 6px;
          }
          .fas.fa-earth-americas {
            margin-right: 6px;
          }
          h1, h2, h3, h4, h5, h6 {
        font-family: 'Roboto', sans-serif;
          }
          .module-title {
          background-color: #f7f7f7;
          border-bottom: 1px solid #e1e1e1;
          padding: 10px;
          text-align: left;
          margin-bottom: 20px;
          }

        ")),

    # Global loading screen, defined outside of any module
    # Define a global spinner with custom text

    # Leave this function for adding external resources
    golem_add_external_resources(),

    custom_spinner("loadingSpinnerCountry", "Loading country meta data, please wait..."),

    # Your application UI logic
    shinydashboard::dashboardPage(skin = "black",
                                  #shinydashboard::dashboardHeader(title = "Small Area Estimation for Improving Maternal Health in the region of Africa",titleWidth='750px'),
                                  shinydashboard::dashboardHeader(title = "Small Area Estimation for Improving Health in LMICs",titleWidth='550px'),
                                  shinydashboard::dashboardSidebar(
                                     tags$style(HTML("
                                        /* Hide data upload tab by default */
                                        a[data-value='data_upload'] {
                                          display: none !important;
                                        }
                                        /* Show it when this class is added */
                                        a[data-value='data_upload'].show-menu-item {
                                          display: block !important;
                                        }
                                      ")),
                                      tags$script(HTML("
                                        Shiny.addCustomMessageHandler('show_data_upload_tab', function(message) {
                                          $('a[data-value=\"data_upload\"]').addClass('show-menu-item');
                                        });
                                      ")),
                                    shinydashboard::sidebarMenu(
                                      id = "Overall_tabs", ## very important for switching between panels
                                      shinydashboard::menuItem("Country Specification", tabName = "country_spec", icon = icon("globe")),
                                         shinydashboard::menuItem("Data Upload", tabName = "data_upload", icon = icon("database")),
                                      shinydashboard::menuItem("Model Fitting", tabName = "model_fit", icon = icon("sliders-h")),
                                      shinydashboard::menuItem("Result Visualization", tabName = "res_visual", icon = icon("earth"),
                                       shinydashboard::menuSubItem(HTML("&nbsp &nbsp &nbsp &nbsp Prevalence Map"), tabName = "res_prev_adm1_map",icon = NULL),
                                       shinydashboard::menuSubItem(HTML("&nbsp &nbsp &nbsp &nbsp Map Comparison"), tabName = "res_compare_map",icon = NULL),
                                       shinydashboard::menuSubItem(HTML("&nbsp &nbsp &nbsp &nbsp Scatter Plot"), tabName = "res_scatter",icon = NULL),
                                       shinydashboard::menuSubItem(HTML("&nbsp &nbsp &nbsp &nbsp Ridge Plot"), tabName = "res_ridge",icon = NULL),
                                       shinydashboard::menuSubItem(HTML("&nbsp &nbsp &nbsp &nbsp Ranking Plot"), tabName = "res_rankings",icon = NULL),
                                       shinydashboard::menuSubItem(HTML("&nbsp &nbsp &nbsp &nbsp Interval Comparison"), tabName = "res_compare_interval",icon = NULL)),
                                      shinydashboard::menuItem("Result Tabulation", tabName = "res_tab", icon = icon("line-chart")),
                                      shinydashboard::menuItem(HTML("&nbsp Report Generation"), tabName = "report_tab", icon = icon("print")),
                                      shinydashboard::menuItem(HTML("&nbsp More Instructions"), tabName = "get_start", icon = icon("home")),
                                      shinydashboard::menuItem(HTML("&nbsp Tool Kit"), tabName = "tool_kit", icon = icon("screwdriver-wrench"),
                                     shinydashboard::menuSubItem(HTML("&nbsp &nbsp &nbsp &nbsp Indicators in the App"), tabName = "indicator_in_app",icon = NULL),
                                     shinydashboard::menuSubItem(HTML("&nbsp &nbsp &nbsp &nbsp DHS Indicator Dictionary"), tabName = "indicator_dictionary",icon = NULL),
                                     shinydashboard::menuSubItem(HTML("&nbsp &nbsp &nbsp &nbsp DHS Report Estimate"), tabName = "DHS_API_est",icon = NULL)) #,
                                      # div(style = "position: absolute; bottom: 50px; width: 100%; padding: 10px;",
                                      #     img(src = "www/WHO_logo.jpg",
                                      #           height = "200px", style = "display: block; margin: auto;")
                                      # )
          )
      ),
      shinydashboard::dashboardBody(
        shinyjs::useShinyjs(),
        shinyjs::extendShinyjs(text =htmlwidgets::JS(
          "shinyjs.init = function() {",
          "  $(document).on('shown.bs.tab', 'a[data-toggle=\"tab\"]', function (e) {",
          "    var activeTab = $(e.target);",
          "    var menuItem = activeTab.closest('li.treeview');",
          "    menuItem.addClass('menu-open');",
          "    menuItem.children('.treeview-menu').css('display', 'block');",
          "    $('.treeview').each(function () {",
          "      if ($(this).find('.treeview-menu .active').length) {",
          "        $(this).addClass('active');",
          "      } else {",
          "        $(this).removeClass('active');",
          "      }",
          "    });",
          "  });",
          "}"
        ), functions = c("activateTab")),
        shinydashboard::tabItems(
          shinydashboard::tabItem(tabName = "get_start",
                                  mod_landing_page_ui("landing_page_1")), # Use the module UI here
          shinydashboard::tabItem(tabName = "country_spec",
                  mod_country_specify_ui("country_specify_1")), # Use the module UI here
          shinydashboard::tabItem(tabName = "data_upload",
                  mod_survey_dat_input_ui("survey_dat_input_1")),
          shinydashboard::tabItem(tabName = "model_fit",
                  mod_model_selection_ui("model_selection_1")),
          shinydashboard::tabItem(tabName = "res_tab",
                  mod_result_tabulate_ui("result_tabulate_1")),
          # Adding individual content for each subtab
          shinydashboard::tabItem(tabName = "res_prev_adm1_map",
                  mod_res_visual_prev_map_by_state_ui("res_visual_prev_map_by_state_1")), # Content for Admin1 Prev Map subtab
          shinydashboard::tabItem(tabName = "res_compare_map",
                  mod_res_visual_multiple_maps_ui("res_visual_multiple_maps_1")), # Content for Map Comparison subtab
          shinydashboard::tabItem(tabName = "res_scatter",
                  mod_res_visual_scatter_ui("res_visual_scatter_1")), # Content for Comparison Scatter subtab
          shinydashboard::tabItem(tabName = "res_ridge",
                  mod_res_visual_ridge_ui("res_visual_ridge_1")), # Content for ridge plot subtab
          shinydashboard::tabItem(tabName = "res_rankings",
                  mod_rankings_caterpillar_ui("rankings_caterpillar_1")), # Content for rankings plot subtab
          shinydashboard::tabItem(tabName = "res_compare_interval",
                  mod_res_visual_interval_compare_ui("res_visual_interval_compare_1")), # Content for rankings plot subtab
          shinydashboard::tabItem(tabName = "report_tab",
                  mod_report_preparation_ui("report_preparation_1")), # Report generation
          shinydashboard::tabItem(tabName = "indicator_in_app",
                  mod_indicator_in_app_ui("indicator_in_app_1")), # Indicator supported in the app
          shinydashboard::tabItem(tabName = "indicator_dictionary",
                  mod_indicator_dictionary_ui("indicator_dictionary_1")), # Indicator Dictionary
          shinydashboard::tabItem(tabName = "DHS_API_est",
                                  mod_DHS_API_est_ui("DHS_API_est_1")) # Indicator Dictionary
        )
      ),
      tags$head(
        # include customized font
        tags$style(HTML("
        @import url('https://fonts.googleapis.com/css?family=Lato:400,700&display=swap');
        * {
          font-family: 'Lato', sans-serif;
        }")),

        # include google analytics only when deploying onto the server
        if ((!is.null(golem::get_golem_options()$server_link))&
            (file.exists(system.file("app", "www", "google_analytics.html", package = "sae4health")))) {
          message('including google analytics for server version.')
          includeHTML(
            system.file("app", "www", "google_analytics.html", package = "sae4health")
          )
        }
      )

    )
    # Your application UI logic

  )
}

#' Add external Resources to the Application
#'
#' This function is internally used to add external
#' resources inside the Shiny application.
#'
#' @import shiny
#' @importFrom golem add_resource_path activate_js favicon bundle_resources
#' @noRd
golem_add_external_resources <- function() {
  add_resource_path(
    "www",
    app_sys("/app/www")
  )

  tags$head(
    #favicon(ext = 'png'),
    bundle_resources(
      path = app_sys("/app/www"),
      app_title = "sae4health"
    ),

    ### add message handler
    #tags$script(src = "handlers.js"),

    ### add style sheets for html objects
    #tags$link(href = "div_style.css"),

    # Add here other external resources
    # for example, you can add shinyalert::useShinyalert()
    shinyFeedback::useShinyFeedback(),
    shinyjs::useShinyjs(),
  )

}






