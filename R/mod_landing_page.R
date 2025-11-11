#' landing_page UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd
#'
#' @importFrom shiny NS tagList
mod_landing_page_ui <- function(id) {
  ns <- NS(id)

  website_link <- "https://sae4health.stat.uw.edu"

  fluidPage(
    tags$head(
      tags$style(HTML("
      .welcome-note {
        border-left: 5px solid #2196F3; /* Blue left border */
        background-color: #e9f4e7; /*  #e8f5e9 Light green background */
        padding: 20px; /* Slightly larger padding for better spacing */
        margin: 20px 0;
        font-family: Arial, sans-serif;
        font-size: 18px; /* Larger font size for improved readability */
      }
      .welcome-note h4 {
        margin-top: 0;
        color: #333;
        font-size: 24px; /* Increased size for heading */
      }
      .welcome-note p {
        margin-bottom: 0;
      }
      .official-link {
        color: blue; /* Blue for official site links */
        text-decoration: none; /* Optional: Removes underline */
      }
      .instruction-link {
        color: #11be06; /* Green for instruction links */
        text-decoration: none; /* Optional: Removes underline */
      }
      .instructions {
          font-size: 16px; /* Larger font size */
      }
      .instructions li {
          margin-bottom: 5px; /* Spacing between items */
      }
      .instructions b { /* Bold formatting for keywords */
          font-weight: bold;
      }
        .box0 .box-header > .box-title {
background:#666666
        }
.box0 .box.box-solid.box-primary>.box-header {
background:#666666
}
.box0 .box.box-solid.box-primary {
border: 1px solid #666666}
        .box1 .box-header > .box-title {background: #9FDA3AFF;}
        .box1 .box.box-solid.box-primary > .box-header {background: #9FDA3AFF;}
        .box1 .box.box-solid.box-primary {border: 1px solid #9FDA3AFF;}
        .box2 .box-header > .box-title {background: #4AC16DFF;}
        .box2 .box.box-solid.box-primary > .box-header {background: #4AC16DFF;}
        .box2 .box.box-solid.box-primary {border: 1px solid #4AC16DFF;}
        .box3 .box-header > .box-title {background: #1FA187FF;}
        .box3 .box.box-solid.box-primary > .box-header {background: #1FA187FF;}
        .box3 .box.box-solid.box-primary {border: 1px solid #1FA187FF;}
        .box4 .box-header > .box-title {background: #277F8EFF;}
        .box4 .box.box-solid.box-primary > .box-header {background: #277F8EFF;}
        .box4 .box.box-solid.box-primary {border: 1px solid #277F8EFF;}
    "))

    ),


    div(class = "welcome-note",
        h4("Welcome to the sae4health R Shiny App!"),

        HTML(paste0(
          "<p style='font-size: large; margin-bottom: 20px;'>",
          "Our tool empowers hands-on analysis of health and demographic indicators in low- and middle-income countries (LMICs) at the subnational level.  <br> ",
          "Visit our ",
          tags$a("official website", href = paste0(website_link),
                 target = "_blank", class = "official-link"),
          " for detailed ",
          tags$a("instruction videos", href = paste0(website_link,"/overview/youtube_app_demo/"),
                 target = "_blank", class = "instruction-link"),
          ", ",
          tags$a("methodology descriptions", href = paste0(website_link,"/method/approach_overview/"),
                 target = "_blank", class = "instruction-link"),
          " and guidance on ",
          tags$a("results visualization and interpretation", href = paste0(website_link,"/gallery/visual_overview/"),
                 target = "_blank", class = "instruction-link"),
          ".",
          "<br>",
          "Follow the step-by-step instructions below to navigate through the application.",
          "<br>",
          "<span style='font-weight: bold; color: #FF0000;'>*</span> ",
          "<strong>Expandable Instructions Boxes</strong>: ",
          "Click the button in the top-right corner of each box to expand or collapse its contents.",
          "</p>",
          "</p>",
          "<p style='font-size: small; text-align: right;margin-top:-10px'>", "Version 1.2.4 (2025-08-22)",
          "</p>"
        ))
    ),


    # colors
    # color_codes <- viridis::viridis(n=8,direction=-1)  # Generate 5 colors from the default Viridis palette
    # message((color_codes)[2:5])

    ### Step 0 prepare data

    #uiOutput(ns("box0_ui")),

    if (is.null(golem::get_golem_options()$server_link)) {
      div(
        class = "box0",
        shinydashboard::box(
          title = "Step 0 - Request Data",
          status = "primary", solidHeader = TRUE, collapsible = TRUE,
          width = NULL,
          tags$div(class = "instructions",
                   HTML(paste0(
                     "<p style='margin-bottom:10px;'>",
                     "Before using the App, the user needs access to the required DHS survey data. ",
                     "DHS maintains strict data access rules, particularly for sensitive GPS-tagged data needed for SAE analysis.</p>",
                     "<p>",
                     "Follow these steps to <strong>obtain access to DHS datasets</strong> (instruction videos also available ",
                     tags$a("here", href = paste0(website_link, "/overview/youtube_data_request/"),
                            target = "_blank", class = "official-link"), "):</p>",
                     tags$ol(type = "a",
                             tags$li(tags$b("Register an account"), ": Start by creating an account on the ",
                                     tags$a("DHS website", href = "https://dhsprogram.com/Data/",
                                            target = "_blank", class = "official-link"), "."),
                             tags$li(tags$b("Provide project information"), ": Complete the forms with specific details about the project."),
                             tags$li(tags$b("Request survey access"), ": Post-registration, formally request access to the country/year specific survey datasets. Be sure to request ",
                                     tags$b("GPS data"), "; approval typically occurs within 48 hours.")
                     )
                   ))
          )
        )
      )
    } else {
      div(
        class = "box0",
        shinydashboard::box(
          title = "Step 0 - Data Access Overview",
          status = "primary", solidHeader = TRUE, collapsible = TRUE,
          width = NULL,
          tags$div(class = "instructions",
                   HTML(paste0(
                     "<p style='margin-bottom:10px;'>",
                     "The online version of the app comes with most surveys already <strong>preloaded</strong> from the server, allowing immediate use. ",
                     "All features, including data analysis and visualization, are fully supported <strong>without the need to upload any data</strong>.</p>",

                     "<p>",
                     "Due to confidentiality restrictions, the underlying raw datasets cannot be downloaded or fully displayed. ",
                     "For full access to raw data, users may request permission and use the local version of the app.</p>"
                   ))
          )
        )
      )
    },





    ### Step 1 specifying survey meta data
    div(
      class = "box1",
    shinydashboard::box(title = "Step 1 - Country/Survey Meta Data Specification",
                        status = "primary", solidHeader = TRUE, collapsible = TRUE,
                        collapsed = T,
                        width = NULL,
                        tags$div(class="instructions",
                                 HTML(paste0(
                                   "<p style=' margin-bottom:10px;'>",
                                   "After obtaining access to the DHS data (if using the local version), the user may proceed to  ",
                                   actionButton(
                                     ns("switch_country_tab"),  # Button ID to trigger the modal
                                     "country specification panel ",
                                     style = "border: none; background: none; color: blue; padding: 0; margin-bottom: 3px; font-size: 16px;"  # Larger font
                                   ),
                                   " to specify the meta data for the analysis, including: </p> ",
                                   tags$ul(
                                     tags$li(tags$b("Specify Country/Survey"), ": Select a country first; then, surveys available for that country will be displayed."),
                                     tags$li(tags$b("Choose an Indicator"),
                                             ": Indicators are organized by the chapters they appear within the DHS final report. For a detailed list of supported indicators, visit the",
                                             actionButton(
                                               ns("switch_app_ind"),
                                               "Tool Kit",
                                               style = "border: none; background: none; color: blue; padding: 0; margin-bottom: 3px; font-size: 16px;"
                                             ),
                                             "tab."
                                     ),
                                     tags$li(tags$b("Select Administration Levels"),
                                             ": Choose the administrative levels (e.g., Admin-1, Admin-2) for your analysis."),
                                     tags$li(tags$b("Display Boundaries and Check Region Count"),
                                             ": Use our mapping tool to ensure the administrative regions match your analytical needs. Default boundary sources are GADM or WHO, depending on the app version. For custom boundaries, please contact us.")
                                   )
                                 ))
                        )
    )
    ),


    ### Step 2 data upload
    if (is.null(golem::get_golem_options()$server_link)) {
    div(
      class = "box2",
    shinydashboard::box(title = "Step 2 - Data Upload",
                        status = "primary", solidHeader = TRUE, collapsible = TRUE,
                        width = NULL,
                        collapsed = T,
                        tags$div(class="instructions",
                                 HTML(paste0(
                                   "<p>Access the ",
                                   actionButton(
                                     ns("switch_data_upload"),
                                     "Data Upload Panel",
                                     style = "border: none; background: none; color: blue; padding: 0; margin-bottom: 3px; font-size: 16px;"
                                   ),
                                   " after obtaining your DHS data (see instructional videos ",
                                   tags$a("here", href = paste0(website_link,"/overview/youtube_data_request/"),
                                          target = "_blank", class = "official-link"),
                                   ").</p>",
                                   tags$ul(
                                     tags$li(tags$b("Data Download"),': ',
                                             tags$ol(type="a",
                                                     tags$li("Log in at the",
                                                     tags$a("DHS website", href = "https://dhsprogram.com/Data/",
                                                                    target = "_blank", class = "official-link"),
                                                     "navigate to your pre-approved project, and select the country."),
                                                     tags$li("Follow the instructions on the data upload page to select the required Stata format recode data. Be sure to include ", tags$b("Geographic Data"), "."),
                                                     tags$li("Download all datasets in a zip file.")
                                             )
                                     ),
                                     tags$li(tags$b("Data Upload"),':',
                                             " Click ", tags$b("Data Upload"), ", select the downloaded .zip file, and submit.",
                                             "Once the data upload is complete (indicated by the green check marks on the upper right of the tab),",
                                             "the app will automatically prepare the dataset for analysis."
                                             ),
                                     tags$li(tags$b("Cluster Map"),
                                             ": Use this visualization tool to examine cluster density across Admin areas. Hover to see number of clusters and evaluate data sparsity issue."),
                                     tags$li(tags$b("Data Preview"),':',
                                             "The user may preview and download the prepared analysis dataset in .csv format.")
                                   )
                                 ))
                        )
    )
    )
      }else{


        div(
          class = "box2",
          shinydashboard::box(title = "Step 2 - Data Upload",
                              status = "primary", solidHeader = TRUE, collapsible = TRUE,
                              width = NULL,
                              collapsed = T,
                              tags$div(class="instructions",
                                       HTML(paste0(
                                         "<p>Access the ",
                                         actionButton(
                                           ns("switch_data_upload"),
                                           "Data Upload Panel",
                                           style = "border: none; background: none; color: blue; padding: 0; margin-bottom: 3px; font-size: 16px;"
                                         ),
                                         ". </p>",
                                         tags$ul(
                                           tags$li(tags$b("Data Step"),':',
                                                   " Click ", tags$b("Load Data from Server."),
                                                   "Once the data retrieval from the server is complete (indicated by the green check marks on the upper right of the tab),",
                                                   "the app will automatically prepare the dataset for analysis."
                                           ),
                                           tags$li(tags$b("Cluster Map"),
                                                   ": Use this visualization tool to examine cluster density across Admin areas. Hover to see number of clusters and evaluate data sparsity issue."),
                                           tags$li(tags$b("Data Preview"),':',
                                                   "The user may preview and download the prepared analysis dataset in .csv format (only in the local version).")
                                         )
                                       ))
                              )
          )
        )


    },


    ### Step 3 Model Fitting
    div(
      class = "box3",
    shinydashboard::box(title = "Step 3 - Model Fitting",
                        status = "primary", solidHeader = TRUE, collapsible = TRUE,
                        width = NULL,collapsed = T,
                        tags$div(class="instructions",
                                 HTML(paste0(
                                   "<p>Before proceeding to ",
                                   actionButton(
                                     ns("switch_model_fitting"),
                                     "model fitting",
                                     style = "border: none; background: none; color: blue; padding: 0; margin-bottom: 3px; font-size: 16px;"
                                   ),
                                   ", we strongly recommend familiarizing yourself with the ",
                                   tags$a("methods", href = paste0(website_link,"/method/approach_overview/"),
                                          target = "_blank", class = "official-link"),
                                   " used in our analysis.</p>",
                                   "<p>Follow the steps below to carry out analysis:</p>",
                                   tags$ol(type="a",
                                     tags$li("Select from various model types across different administrative levels. Recommended models are listed for guidance."),
                                     tags$li(tags$b("Data Sparsity Check"),
                                             ": to assess data sufficiency for model fitting. Results are categorized as:",
                                             tags$ul(
                                               tags$li(tags$b("Green check message", style="color: green;"), ": Data sufficient. Model is ready to be implemented."),
                                               tags$li(tags$b("Orange warning message", style="color: orange;"), ": Potential data sparsity issue. Fitting the model is possible but advised against."),
                                               tags$li(tags$b("Red warning message", style="color: red;"), ": Data sparsity check failed. Model fitting is not recommended and may result in errors.")
                                             ),
                                             "Note: Admin-2 level analysis typically lack sufficient data, requiring Small Area Estimation (SAE).",
                                             "Contact us if you encounter non-meaningful error messages."
                                     ),
                                     tags$li("Following the data sparsity check, choose to ",
                                             tags$ul(
                                               tags$li(tags$b("Run Models that Passed Check"), " (conservative and recommended option) or "),
                                               tags$li(tags$b("Run All Selected Models"), " (extra caution needed for results interpretation).")
                                             )),
                                      tags$li("Model fitting statuses are:",
                                             tags$ul(
                                               tags$li(tags$b("Successful", style="color: green;"), ": The model is fitted successfully."),
                                               tags$li(tags$b("Model not fitted", style="color: orange;"), ": The model was not chosen for fitting."),
                                               tags$li(tags$b("Model will not be fitted", style="color: gray;"), ": The model fails the data sparsity check and will not be fitted.")
                                             ))
                                   )
                                 ))
                        )
    )
    ),

    ### Step 4 - Result visualization and tabulation
    div(
      class = "box4",
    shinydashboard::box(title = "Step 4 - Results Visualization and Tabulation",
                        status = "primary", solidHeader = TRUE, collapsible = TRUE,
                        collapsed = T,
                        width = NULL,
                        tags$div(class="instructions",
                                 HTML(paste0(
                                   "<p>Before using the visualization tools, we strongly recommend reviewing our comprehensive ",
                                   tags$a("visualization interpretation guide", href = paste0(website_link,"/gallery/visual_overview/"),
                                          target = "_blank", class = "official-link"),
                                   ". Access the tools under the ",
                                   actionButton(
                                     ns("switch_res_visual"),
                                     "results visualization tab",
                                     style = "border: none; background: none; color: blue; padding: 0; margin-bottom: 3px; font-size: 16px;"
                                   ),
                                   " to explore subnational mapping and other analytical plots.</p>",
                                   tags$ul(
                                     tags$li(tags$b("Single Map"),
                                             ": Map subnational estimates with four key statistics:.",
                                                       tags$ul(
                                                         tags$li(tags$b("Mean"), ": Point estimates across regions."),
                                                         tags$li(tags$b("Coefficient of Variation"), ": Measure of relative variability among estimates."),
                                                         tags$li(tags$b("Width of 95% Credible Interval"), ": Reflects the uncertainty around the estimates."),
                                                         tags$li(tags$b("Exceedance Probability"), ": Probability that prevalence exceeds a certain threshold.")
                                                       )
                                             ),
                                     tags$li(tags$b("Map Comparison"),
                                             ": Visualize different statistics from the same model or compare the same statistics across different models."),
                                     tags$li(tags$b("Scatter Plot"),
                                             ": Compare estimates from different models for the same admin level, and evaluate model performance."),
                                     tags$li(tags$b("Ridge Plot"),
                                             ": Compare full distribution of estimates across regions.")
                                   ),
                                   '<p> Tabulated results can be exported as .csv through ',
                                   actionButton(
                                     ns("switch_res_tab"),
                                     "result tabulation",
                                     style = "border: none; background: none; color: blue; padding: 0; margin-bottom: 3px; font-size: 16px;"
                                   ),
                                   '.</p>'
                                   ))
                        )
    ))

    )
}
#' landing_page Server Functions
#'
#' @noRd
#'
#'

mod_landing_page_server <- function(id,CountryInfo,AnalysisInfo,MetaInfo,parent_session){
  moduleServer( id, function(input, output, session){

    ns <- session$ns


    #############################################################
    #### switch tabs setup
    #############################################################

    observeEvent(input$switch_country_tab, {
      message('switching')
      shinydashboard::updateTabItems(parent_session, "Overall_tabs", selected = "country_spec")
    })

    ### switch to tab with indicator supported by our app
    observeEvent(input$switch_app_ind, {
      shinydashboard::updateTabItems(parent_session, "Overall_tabs", selected = "indicator_in_app")
      shinyjs::js$activateTab("tool_kit")
    })



    observeEvent(input$switch_data_upload, {
      message('switching')
      shinydashboard::updateTabItems(parent_session, "Overall_tabs", selected = "data_upload")
    })

    observeEvent(input$switch_model_fitting, {
      #message('switching')
      shinydashboard::updateTabItems(parent_session, "Overall_tabs", selected = "model_fit")
    })

    observeEvent(input$switch_res_visual, {
      #message('switching')
      shinydashboard::updateTabItems(parent_session, "Overall_tabs", selected = "res_prev_map")
      shinyjs::js$activateTab("res_visual")

    })

    observeEvent(input$switch_res_tab, {
      shinydashboard::updateTabItems(parent_session, "Overall_tabs", selected = "res_tab")
    })



    #############################################################
    #### box 0 message setup
    #############################################################

    # output$box0_ui <- renderUI({
    #   if (!CountryInfo$server_version()) {
    #     div(
    #       class = "box0",
    #       shinydashboard::box(
    #         title = "Step 0 - Request Data",
    #         status = "primary", solidHeader = TRUE, collapsible = TRUE,
    #         width = NULL,
    #         tags$div(class = "instructions",
    #                  HTML(paste0(
    #                    "<p style='margin-bottom:10px;'>",
    #                    "Before using the App, the user needs access to the required DHS survey data. ",
    #                    "DHS maintains strict data access rules, particularly for sensitive GPS-tagged data needed for SAE analysis.</p>",
    #                    "<p>",
    #                    "Follow these steps to <strong>obtain access to DHS datasets</strong> (instruction videos also available ",
    #                    tags$a("here", href = paste0(website_link, "/overview/youtube_data_request/"),
    #                           target = "_blank", class = "official-link"), "):</p>",
    #                    tags$ol(type = "a",
    #                            tags$li(tags$b("Register an account"), ": Start by creating an account on the ",
    #                                    tags$a("DHS website", href = "https://dhsprogram.com/Data/",
    #                                           target = "_blank", class = "official-link"), "."),
    #                            tags$li(tags$b("Provide project information"), ": Complete the forms with specific details about the project."),
    #                            tags$li(tags$b("Request survey access"), ": Post-registration, formally request access to the country/year specific survey datasets. Be sure to request ",
    #                                    tags$b("GPS data"), "; approval typically occurs within 48 hours.")
    #                    )
    #                  ))
    #         )
    #       )
    #     )
    #   } else {
    #     div(
    #       class = "box0",
    #       shinydashboard::box(
    #         title = "Step 0 - Data Access Overview",
    #         status = "primary", solidHeader = TRUE, collapsible = TRUE,
    #         width = NULL,
    #         tags$div(class = "instructions",
    #                  HTML(paste0(
    #                    "<p style='margin-bottom:10px;'>",
    #                    "The online version of the app comes with most surveys already <strong>preloaded</strong>, allowing immediate use. ",
    #                    "All features, including data analysis and visualization, are fully supported <strong>without the need to upload any data</strong>.</p>",
    #
    #                    "<p>",
    #                    "Due to confidentiality restrictions, the underlying raw datasets cannot be downloaded or fully displayed. ",
    #                    "For full access to raw data, users may request permission and use the local version of the app.</p>"
    #                  ))
    #         )
    #       )
    #     )
    #   }
    # })




  })
}

## To be copied in the UI
# mod_landing_page_ui("landing_page_1")

## To be copied in the server
# mod_landing_page_server("landing_page_1")
