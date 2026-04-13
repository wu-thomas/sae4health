#' res_visual_interval_compare UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd
#'
#' @importFrom shiny NS tagList
mod_res_visual_interval_compare_ui <- function(id){
  ns <- NS(id)
  fluidPage(
    tags$head(
      tags$style(HTML(" 
      .button-container {
        display: flex;
        justify-content: center;
        width: max(50%, 600px);
        margin: 20px auto;
      }
      "))
    ),

    div(class = "module-title",
        h4("Compare Interval Plots Across Fitted Models")
    ),

    fluidRow(
      column(12,
             div(style = " margin: auto;float: left;margin-top: 5px",
                 uiOutput(ns("info_display"))
             )
      )
    ),

    fluidRow(
      column(4,
             selectInput(ns("selected_adm"),
                         "Select Admin Level",
                         choices = character(0))
      )
    ),

    fluidRow(
      column(12,
             div(style = " margin: auto;float: left;",
                 uiOutput(ns("model_fitted_text"))
             )
      )
    ),

    tags$hr(style="border-top-color: #E0E0E0;"),

    fluidRow(
      column(12,
             div(
               style = "width: min(98%, 1100px); margin-top: 10px;margin-left: 20px;margin-right: 10px;",
               plotOutput(ns("interval_plot"), height = "auto")
             ),
             div(
               style = "width: min(98%, 1100px); margin-top: 20px; display: flex; justify-content: center;",
               uiOutput(ns("download_button_interval"))
             )
      )
    )
  )
}

#' res_visual_interval_compare Server Functions
#'
#' @noRd
mod_res_visual_interval_compare_server <- function(id, CountryInfo, AnalysisInfo, MetaInfo){
  moduleServer(id, function(input, output, session){
    ns <- session$ns
    DHS.country.meta <- isolate(MetaInfo$DHS.country.meta())

    output$info_display <- renderUI({
      req(CountryInfo$country())
      req(CountryInfo$svy_indicator_var())
      req(CountryInfo$svy_analysis_dat())

      country <- CountryInfo$country()
      svy_year <- CountryInfo$svyYear_selected()

      HTML(paste0(
        "<p style='font-size: large;'>",
        "Selected Country: <span style='font-weight:bold;background-color: #D0E4F7;'>", country, "</span>.",
        " Survey Year: <span style='font-weight:bold;background-color: #D0E4F7;'>", svy_year, "</span>.",
        "<br>",
        "Indicator: <span style='font-weight:bold;background-color: #D0E4F7;'>", CountryInfo$svy_indicator_des(),
        "</span>.</p>",
        "<hr style='border-top-color: #E0E0E0;'>"
      ))
    })

    ### update admin choices
    col_names <- reactive({ CountryInfo$GADM_analysis_levels() })

    observeEvent(col_names(), {
      adm.choice <- col_names()
      if (is.null(adm.choice)) adm.choice <- character(0)
      adm.choice <- adm.choice[adm.choice != "National"]
      updateSelectInput(
        session = session,
        inputId = "selected_adm",
        choices = adm.choice,
        selected = if (length(adm.choice) > 0) adm.choice[1] else character(0)
      )
    }, ignoreInit = FALSE)

    ### helper: collect fitted models at selected admin level
    fitted_model_info <- reactive({
      req(input$selected_adm)

      model_res_all <- AnalysisInfo$model_res_list()
      req(model_res_all)

      method_ids <- c("Direct", "FH", "Unit")
      method_labels <- c(
        "Direct" = "Direct Estimates",
        "FH" = "Area-level Model",
        "Unit" = "Cluster-level Model"
      )

      selected_adm <- input$selected_adm
      out <- list()
      out_labels <- character(0)

      for (m in method_ids) {
        tmp <- NULL
        tmp <- tryCatch(model_res_all[[m]][[selected_adm]], error = function(e) NULL)
        if (!is.null(tmp)) {
          out[[length(out) + 1]] <- tmp
          out_labels[length(out_labels) + 1] <- unname(method_labels[m])
        }
      }

      names(out) <- out_labels
      list(models = out, labels = out_labels)
    })

    output$model_fitted_text <- renderUI({
      if (length(input$selected_adm) == 0 || input$selected_adm == "") {
        return(NULL)
      }

      fit_info <- fitted_model_info()
      fitted_labels <- fit_info$labels

      if (length(fitted_labels) == 0) {
        return(HTML(paste0(
          "<p style='font-size: large;'>",
          "No fitted model results are available at ",
          "<span style='background-color: #D0E4F7;'><b>", input$selected_adm, "</b></span>",
          " level.",
          "</p>"
        )))
      }

      HTML(paste0(
        "<p style='font-size: large;'>",
        "Presenting interval plot comparison at ",
        "<span style='background-color: #D0E4F7;'><b>", input$selected_adm, "</b></span>",
        " level for: ",
        "<span style='background-color: #D0E4F7;'><b>", paste(fitted_labels, collapse = ", "), "</b></span>",
        ".</p>"
      ))
    })


    infer_interval_cols <- function(plot_obj) {
      dat <- tryCatch(plot_obj$data, error = function(e) NULL)
      if (is.null(dat) || !is.data.frame(dat) || ncol(dat) == 0) {
        return(list(region = NULL, estimate = NULL, model = NULL))
      }

      nms <- names(dat)
      lower_nms <- tolower(nms)

      pick_first <- function(cands) {
        idx <- match(cands, lower_nms)
        idx <- idx[!is.na(idx)]
        if (length(idx) == 0) return(NULL)
        nms[idx[1]]
      }

      region_col <- pick_first(c(
        "domain", "region", "area", "name", "region.name",
        "admin.name", "admin1.name", "admin2.name", "admin3.name",
        "admin1.name.full", "admin2.name.full", "admin3.name.full",
        "label"
      ))

      if (is.null(region_col)) {
        chr_cols <- nms[vapply(dat, function(x) is.character(x) || is.factor(x), logical(1))]
        chr_cols <- setdiff(chr_cols, c("model", "Model"))
        if (length(chr_cols) > 0) region_col <- chr_cols[1]
      }

      estimate_col <- pick_first(c(
        "mean", "estimate", "est", "fit", "value", "median", "prev", "prevalence"
      ))

      if (is.null(estimate_col)) {
        num_cols <- nms[vapply(dat, is.numeric, logical(1))]
        num_cols <- setdiff(num_cols, c("lower", "upper", "lb", "ub", "lcl", "ucl", "q025", "q975"))
        if (length(num_cols) > 0) estimate_col <- num_cols[1]
      }

      model_col <- pick_first(c("model"))
      if (is.null(model_col)) {
        fac_cols <- nms[vapply(dat, function(x) is.character(x) || is.factor(x), logical(1))]
        fac_cols <- setdiff(fac_cols, region_col)
        if (length(fac_cols) > 0) model_col <- fac_cols[1]
      }

      list(region = region_col, estimate = estimate_col, model = model_col)
    }

    reorder_interval_plot <- function(plot_obj) {
      dat <- tryCatch(plot_obj$data, error = function(e) NULL)
      cols <- infer_interval_cols(plot_obj)

      if (is.null(dat) || is.null(cols$region) || is.null(cols$estimate)) {
        return(plot_obj + ggplot2::labs(
          y = "Estimate", colour = "Model"
        ))
      }

      ord_df <- stats::aggregate(
        dat[[cols$estimate]],
        by = list(region = as.character(dat[[cols$region]])),
        FUN = mean,
        na.rm = TRUE
      )
      names(ord_df)[2] <- "ord_est"
      ord_df <- ord_df[order(ord_df$ord_est, decreasing = FALSE), , drop = FALSE]

      dat[[cols$region]] <- factor(as.character(dat[[cols$region]]), levels = ord_df$region)
      plot_obj$data <- dat

      plot_obj + ggplot2::labs(
        y = "Estimate", colour = "Model"
      )
    }

    interval_plot_output <- reactiveVal(NULL)
    interval_plot_height <- reactiveVal(500)

    observe({
      req(input$selected_adm)
      fit_info <- fitted_model_info()
      model_list <- fit_info$models

      if (length(model_list) == 0) {
        interval_plot_output(NULL)
        interval_plot_height(500)
        return(NULL)
      }

      if (CountryInfo$use_preloaded_Madagascar()) {
        AnalysisInfo$model_res_list(mdg.ex.model.res)
      }

      admin_num <- tryCatch(admin_to_num(input$selected_adm), error = function(e) NULL)
      if (is.null(admin_num)) {
        if (identical(input$selected_adm, "National")) {
          admin_num <- 0
        } else {
          interval_plot_output(NULL)
          interval_plot_height(500)
          return(NULL)
        }
      }

      tmp.plot <- tryCatch({
        surveyPrev::intervalPlot(
          admin = admin_num,
          compare = length(model_list) > 1,
          model = model_list,
          group = FALSE
        )
      }, error = function(e) {
        message("Interval plot error:")
        message(e$message)
        return(NULL)
      })

      if (is.null(tmp.plot)) {
        interval_plot_output(NULL)
        interval_plot_height(500)
        return(NULL)
      }

      tmp.plot <- tryCatch(reorder_interval_plot(tmp.plot), error = function(e) tmp.plot)

      interval_plot_output(tmp.plot)

      tmp.height <- tryCatch({
        if (!is.null(tmp.plot$data)) {
          n.plot.rows <- max(1, length(unique(tmp.plot$data[[1]])))
          350 + min(n.plot.rows, 10) * 20 + n.plot.rows * 18
        } else {
          700
        }
      }, error = function(e) 700)

      interval_plot_height(tmp.height)
    })

    output$interval_plot <- renderPlot({
      req(interval_plot_output())
      req(interval_plot_height())
      interval_plot_output()
    }, height = function(){ interval_plot_height() })

    output$download_button_interval <- renderUI({
      if (is.null(interval_plot_output())) {
        return(NULL)
      }

      downloadButton(
        ns("download_interval_plot"),
        "Download as PDF",
        icon = icon("download"),
        class = "btn-primary"
      )
    })

    output$download_interval_plot <- downloadHandler(
      filename = function() {
        DHS_country_code <- DHS.country.meta[DHS.country.meta$CountryName == CountryInfo$country(), ]$DHS_CountryCode
        file.prefix <- paste0(
          DHS_country_code,
          CountryInfo$svyYear_selected(), '_',
          CountryInfo$svy_indicator_var(), '_',
          input$selected_adm, '_interval_compare'
        )
        file.prefix <- gsub("[-.]", "_", file.prefix)
        paste0(file.prefix, ".pdf")
      },
      content = function(file) {
        plot.download <- interval_plot_output()
        plot.download <- plot.download +
          ggplot2::theme(plot.margin = ggplot2::unit(c(0.25, 0.25, 0.25, 0.25), "inches"))

        grDevices::pdf(file, width = 11, height = round(interval_plot_height() / 100))
        print(plot.download)
        grDevices::dev.off()
      }
    )
  })
}

## To be copied in the UI
# mod_res_visual_interval_compare_ui("res_visual_interval_compare_1")

## To be copied in the server
# mod_res_visual_interval_compare_server(
#   "res_visual_interval_compare_1",
#   CountryInfo = CountryInfo,
#   AnalysisInfo = AnalysisInfo,
#   MetaInfo = MetaInfo
# )
