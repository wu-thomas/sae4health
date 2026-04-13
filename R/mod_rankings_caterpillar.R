#' rankings_caterpillar UI Function
#'
#' @description A shiny Module for posterior rank caterpillar plots and an
#' interactive map of posterior mean ranks.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd
#'
#' @importFrom shiny NS tagList
mod_rankings_caterpillar_ui <- function(id){
  ns <- NS(id)

  fluidPage(
    div(class = "module-title",
        h4("Subnational Posterior Rank Caterpillar Plot")
    ),

    fluidRow(
      column(
        12,
        div(style = "margin: auto; float: left; margin-top: 5px;",
            uiOutput(ns("info_display"))
        )
      )
    ),

    fluidRow(
      column(
        4,
        selectInput(
          ns("selected_method"),
          "Select Method",
          choices = c("Area-level Model" = "FH",
                      "Unit-level Model" = "Unit")
        )
      ),
      column(
        4,
        selectInput(
          ns("selected_adm"),
          "Select Admin Level",
          choices = character(0)
        )
      ),
      column(
        4,
        uiOutput(ns("select_upper_adm"))
      )
    ),

    fluidRow(
      column(
        12,
        div(style = "margin: auto; float: left;",
            uiOutput(ns("model_fitted_text"))
        )
      )
    ),

    tags$hr(style = "border-top-color: #E0E0E0;"),

    fluidRow(
      column(
        12,
        div(
          style = "width: min(98%, 1100px); margin-top: 10px; margin-left: 20px; margin-right: 10px;",
          plotOutput(ns("rank_plot"), height = "auto")
        ),
        div(
          style = "width: min(98%, 1100px); margin-top: 20px; display: flex; justify-content: center;",
          uiOutput(ns("download_button_rank"))
        )
      )
    ),

    tags$hr(style = "border-top-color: #E0E0E0;"),

    fluidRow(
      column(
        12,
        div(
          style = "width: min(98%, 1100px); margin-top: 15px; margin-left: 20px; margin-right: 10px;",
          h4("Interactive map of posterior mean ranks"),
          leaflet::leafletOutput(ns("rank_map"), height = "650px")
        )
      )
    )
  )
}


#' rankings_caterpillar Server Functions
#'
#' @noRd
mod_rankings_caterpillar_server <- function(id, CountryInfo, AnalysisInfo, MetaInfo){
  moduleServer(id, function(input, output, session){
    ns <- session$ns

    DHS.country.meta <- isolate(MetaInfo$DHS.country.meta())

    admin_to_num_local <- function(x){
      if(is.null(x) || length(x) == 0 || is.na(x) || x == "") return(NA_integer_)
      as.integer(gsub("[^0-9]", "", x))
    }

    num_to_admin_local <- function(x){
      if(is.null(x) || is.na(x)) return(NA_character_)
      paste0("Admin-", x)
    }

    first_existing_col <- function(dat, candidates){
      if(is.null(dat)) return(NULL)
      hits <- candidates[candidates %in% names(dat)]
      if(length(hits) == 0) return(NULL)
      hits[1]
    }

    build_rank_summary <- function(res.obj, selected_adm, upper_focus = NULL){
      adm.num <- admin_to_num_local(selected_adm)
      if(is.na(adm.num) || adm.num < 1){
        return(NULL)
      }

      post.name <- paste0("admin", adm.num, "_post")
      res.name  <- paste0("res.admin", adm.num)

      samples <- res.obj[[post.name]]
      res.tab <- res.obj[[res.name]]

      if(is.null(samples) || is.null(res.tab)){
        return(NULL)
      }

      samples <- as.matrix(samples)
      if(nrow(samples) < 1 || ncol(samples) < 1){
        return(NULL)
      }

      join_candidates <- c(
        paste0("admin", adm.num, ".name.full"),
        "region.name",
        paste0("admin", adm.num, ".name"),
        paste0("NAME_", adm.num)
      )
      display_candidates <- c(
        paste0("admin", adm.num, ".name"),
        paste0("NAME_", adm.num),
        paste0("admin", adm.num, ".name.full"),
        "region.name"
      )

      join.col <- first_existing_col(res.tab, join_candidates)
      display.col_res <- first_existing_col(res.tab, display_candidates)

      if(is.null(join.col)){
        return(NULL)
      }

      res.tab2 <- res.tab
      res.tab2$region.name <- as.character(res.tab2[[join.col]])
      if(!is.null(display.col_res) && display.col_res %in% names(res.tab2)){
        res.tab2$display_name_raw <- as.character(res.tab2[[display.col_res]])
      } else {
        res.tab2$display_name_raw <- as.character(res.tab2[[join.col]])
      }

      parent.col <- NULL
      if(adm.num > 1){
        parent.col <- first_existing_col(
          res.tab2,
          c(
            paste0("admin", adm.num - 1, ".name"),
            paste0("admin", adm.num - 1, ".name.full"),
            paste0("NAME_", adm.num - 1)
          )
        )
      }

      prev.col <- first_existing_col(
        res.tab2,
        c("median", "Mean", "mean", "estimate", "Est", "prev", "prevalence")
      )

      subset_idx <- rep(TRUE, nrow(res.tab2))
      if(!is.null(upper_focus) && adm.num > 1 && !is.null(parent.col)){
        subset_idx <- !is.na(res.tab2[[parent.col]]) &
          as.character(res.tab2[[parent.col]]) == as.character(upper_focus)
      }

      if(length(subset_idx) != ncol(samples)){
        return(NULL)
      }

      samples_sub <- samples[, subset_idx, drop = FALSE]
      res.tab_sub <- res.tab2[subset_idx, , drop = FALSE]

      if(ncol(samples_sub) < 1 || nrow(res.tab_sub) < 1){
        return(NULL)
      }

      ranked.samples <- t(apply(samples_sub, 1, rank, ties.method = "random"))

      sum.df <- data.frame(
        region.name = as.character(res.tab_sub[[join.col]]),
        mean_rank = apply(ranked.samples, 2, mean, na.rm = TRUE),
        med_rank  = apply(ranked.samples, 2, median, na.rm = TRUE),
        sd_rank   = apply(ranked.samples, 2, sd, na.rm = TRUE),
        q05 = apply(ranked.samples, 2, quantile, probs = 0.05, na.rm = TRUE),
        q25 = apply(ranked.samples, 2, quantile, probs = 0.25, na.rm = TRUE),
        q75 = apply(ranked.samples, 2, quantile, probs = 0.75, na.rm = TRUE),
        q95 = apply(ranked.samples, 2, quantile, probs = 0.95, na.rm = TRUE),
        stringsAsFactors = FALSE
      )

      keep.cols <- unique(c(parent.col, prev.col, join.col, display.col_res,
                            paste0("admin", adm.num, ".name"),
                            paste0("admin", adm.num, ".name.full"),
                            paste0("NAME_", adm.num)))
      keep.cols <- keep.cols[!is.na(keep.cols) & !sapply(keep.cols, is.null)]
      keep.cols <- intersect(keep.cols, names(res.tab_sub))
      res.join <- res.tab_sub[, keep.cols, drop = FALSE]

      out <- cbind(sum.df, res.join)

      if(!is.null(parent.col)){
        names(out)[names(out) == parent.col] <- "upper_region"
      } else {
        out$upper_region <- NA_character_
      }

      if(!is.null(prev.col)){
        names(out)[names(out) == prev.col] <- "estimate_value"
      } else {
        out$estimate_value <- NA_real_
      }

      if("display_name_raw" %in% names(out)){
        out$display_name <- as.character(out$display_name_raw)
      } else {
        out$display_name <- out$region.name
      }

      # Keep only the finest-level label for display.
      out$display_name <- sub(".*_", "", out$display_name)

      out <- out[order(out$mean_rank, decreasing = FALSE), , drop = FALSE]
      rownames(out) <- NULL
      out
    }

    rank_plot_obj <- function(rank.df, selected_method, selected_adm, upper_focus = NULL){
      if(is.null(rank.df) || nrow(rank.df) == 0) return(NULL)

      method_match <- c("Unit" = "Unit-level", "FH" = "Area-level")
      method_des <- unname(method_match[selected_method])
      if(is.na(method_des) || length(method_des) == 0) method_des <- selected_method

      subtitle_txt <- "Point = mean posterior rank; thick bar = 50% interval; thin bar = 90% interval"
      if(!is.null(upper_focus) && !is.na(upper_focus) && nzchar(upper_focus)){
        subtitle_txt <- paste0(subtitle_txt, " | Within ", upper_focus)
      }

      rank.df$display_name_f <- factor(rank.df$display_name, levels = rev(rank.df$display_name))
      #rank.df$display_name_f <- factor(rank.df$display_name, levels = rev(rank.df$mean_rank))
      rank.df$display_name_f <- sub(".*_", "", rank.df$display_name_f)
      #rank.df <- rank.df[order(rank.df$mean_rank), ]

      ggplot2::ggplot(rank.df, ggplot2::aes(x = mean_rank, 
                                            y = reorder(display_name_f,-mean_rank))) +
        ggplot2::geom_errorbarh(ggplot2::aes(xmin = q05, xmax = q95), height = 0, linewidth = 0.4) +
        ggplot2::geom_errorbarh(ggplot2::aes(xmin = q25, xmax = q75), height = 0, linewidth = 1.1) +
        ggplot2::geom_point(size = 2) +
        ggplot2::labs(
          x = "Posterior rank",
          y = NULL,
          title = paste0(method_des, " model posterior rankings at ", selected_adm),
          subtitle = subtitle_txt
        ) +
        ggplot2::theme_minimal(base_size = 12) +
        ggplot2::theme(
          plot.title = ggplot2::element_text(face = "bold"),
          axis.text.y = ggplot2::element_text(size = 10)
        )
    }

    build_map_data <- function(rank.df, selected_adm, upper_focus = NULL){
      req(CountryInfo$GADM_list_smoothed())

      adm.num <- admin_to_num_local(selected_adm)
      if(is.na(adm.num) || adm.num < 1){
        return(NULL)
      }

      gadm.list <- CountryInfo$GADM_list_smoothed()
      shp <- gadm.list[[selected_adm]]
      if(is.null(shp)){
        shp <- CountryInfo$GADM_list()[[selected_adm]]
      }
      if(is.null(shp)){
        return(NULL)
      }

      fine_col <- paste0("NAME_", adm.num)
      parent_col <- if(adm.num > 1) paste0("NAME_", adm.num - 1) else NULL

      if(!fine_col %in% names(shp)){
        return(NULL)
      }

      shp2 <- shp
      shp2$fine_name <- as.character(shp2[[fine_col]])

      if(!is.null(parent_col) && parent_col %in% names(shp2)){
        shp2$upper_region <- as.character(shp2[[parent_col]])
      } else {
        shp2$upper_region <- NA_character_
      }

      if(!is.null(upper_focus) && adm.num > 1){
        shp2 <- shp2[!is.na(shp2$upper_region) & shp2$upper_region == as.character(upper_focus), , drop = FALSE]
      }

      if(nrow(shp2) < 1){
        return(NULL)
      }

      rank.join <- rank.df
      rank.join$fine_name <- as.character(rank.join$display_name)

      shp2 <- dplyr::left_join(shp2, rank.join, by = c("fine_name" = "fine_name"))

      shp2$label_html <- sprintf(
        paste0(
          "<strong>%s</strong><br/>",
          "Mean rank: %.2f<br/>",
          "Median rank: %.2f<br/>",
          "SD rank: %.2f<br/>",
          "50%% interval: [%.2f, %.2f]<br/>",
          "90%% interval: [%.2f, %.2f]%s"
        ),
        shp2$fine_name,
        shp2$mean_rank,
        shp2$med_rank,
        shp2$sd_rank,
        shp2$q25,
        shp2$q75,
        shp2$q05,
        shp2$q95,
        ifelse(
          is.na(shp2$estimate_value),
          "",
          paste0("<br/>Estimate: ", format(round(shp2$estimate_value, 4), nsmall = 4))
        )
      )

      shp2
    }

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

    col_names <- reactive({
      CountryInfo$GADM_analysis_levels()
    })

    observeEvent(col_names(), {
      adm.choice <- col_names()
      adm.choice <- adm.choice[adm.choice != "National"]
      updateSelectInput(
        session = session,
        inputId = "selected_adm",
        choices = adm.choice,
        selected = if(length(adm.choice) > 0) adm.choice[1] else character(0)
      )
    }, ignoreInit = FALSE)

    upper_choices <- reactive({
      req(CountryInfo$GADM_list())
      req(input$selected_adm)

      selected.adm.num <- admin_to_num_local(input$selected_adm)
      if(is.na(selected.adm.num) || selected.adm.num <= 1){
        return(character(0))
      }

      upper.gadm.num <- selected.adm.num - 1
      upper.gadm <- num_to_admin_local(upper.gadm.num)

      gadm.list <- CountryInfo$GADM_list()
      upper.adm.gadm <- gadm.list[[upper.gadm]]
      if(is.null(upper.adm.gadm)){
        return(character(0))
      }

      upper.col <- paste0("NAME_", upper.gadm.num)
      if(!upper.col %in% names(upper.adm.gadm)){
        return(character(0))
      }

      sort(unique(stats::na.omit(as.character(upper.adm.gadm[[upper.col]]))))
    })

    output$select_upper_adm <- renderUI({
      req(input$selected_adm)
      selected.adm.num <- admin_to_num_local(input$selected_adm)

      if(is.na(selected.adm.num) || selected.adm.num <= 1){
        return(NULL)
      }

      upper.adm.label <- paste0("Choose a ", tolower(num_to_admin_local(selected.adm.num - 1)), " region")

      selectInput(
        ns("selected_upper_adm"),
        upper.adm.label,
        choices = upper_choices()
      )
    })

    output$model_fitted_text <- renderUI({
      if(length(input$selected_adm) == 0 || input$selected_adm == ""){
        return(NULL)
      }

      selected_adm <- input$selected_adm
      selected_method <- input$selected_method
      model_res_all <- AnalysisInfo$model_res_list()
      model_res_selected <- model_res_all[[selected_method]][[selected_adm]]

      method_match <- c("Unit" = "Unit-level", "FH" = "Area-level")
      method_des <- unname(method_match[selected_method])
      if(is.na(method_des) || length(method_des) == 0) method_des <- selected_method

      if(is.null(model_res_selected)){
        HTML(paste0(
          "<p style='font-size: large;'>",
          "Results for ",
          "<span style='background-color: #D0E4F7;'><b>", method_des, "</b></span> ",
          "model at ",
          "<span style='background-color: #D0E4F7;'><b>", selected_adm, "</b></span>",
          " level are ",
          "<strong style='color: red;'>NOT</strong>",
          " available. Please make sure the model has been successfully fitted.",
          "</p>"
        ))
      } else {
        extra.txt <- ""
        if(admin_to_num_local(selected_adm) > 1){
          extra.txt <- " Select a coarser region to view rankings within it."
        }

        HTML(paste0(
          "<p style='font-size: large;'>",
          "Presenting posterior rank caterpillar plot and interactive map for ",
          "<span style='background-color: #D0E4F7;'><b>", method_des, "</b></span> ",
          "model at ",
          "<span style='background-color: #D0E4F7;'><b>", selected_adm, "</b></span> level.",
          extra.txt,
          "</p>"
        ))
      }
    })

    rank_plot_data <- reactive({
      req(input$selected_method)
      req(input$selected_adm)

      model_res_all <- AnalysisInfo$model_res_list()
      model_res_selected <- model_res_all[[input$selected_method]][[input$selected_adm]]
      req(!is.null(model_res_selected))

      selected.adm.num <- admin_to_num_local(input$selected_adm)
      upper_focus <- NULL
      if(!is.na(selected.adm.num) && selected.adm.num > 1){
        req(input$selected_upper_adm)
        if(is.null(input$selected_upper_adm) || input$selected_upper_adm == ""){
          return(NULL)
        }
        upper_focus <- input$selected_upper_adm
      }

      build_rank_summary(
        res.obj = model_res_selected,
        selected_adm = input$selected_adm,
        upper_focus = upper_focus
      )
    })

    map_data <- reactive({
      dat <- rank_plot_data()
      req(dat)

      selected.adm.num <- admin_to_num_local(input$selected_adm)
      upper_focus <- NULL
      if(!is.na(selected.adm.num) && selected.adm.num > 1){
        req(input$selected_upper_adm)
        upper_focus <- input$selected_upper_adm
      }

      build_map_data(
        rank.df = dat,
        selected_adm = input$selected_adm,
        upper_focus = upper_focus
      )
    })

    rank_plot_height <- reactive({
      dat <- rank_plot_data()
      if(is.null(dat) || nrow(dat) == 0) return(400)
      220 + 26 * nrow(dat)
    })

    output$rank_plot <- renderPlot({
      req(input$selected_method)
      req(input$selected_adm)

      dat <- rank_plot_data()
      validate(
        need(!is.null(dat), "Select an available level and, when required, a coarser region."),
        need(nrow(dat) > 0, "No regions available for the current selection.")
      )

      selected.adm.num <- admin_to_num_local(input$selected_adm)
      upper_focus <- NULL
      if(!is.na(selected.adm.num) && selected.adm.num > 1){
        upper_focus <- input$selected_upper_adm
      }

      rank_plot_obj(
        rank.df = dat,
        selected_method = input$selected_method,
        selected_adm = input$selected_adm,
        upper_focus = upper_focus
      )
    }, height = function(){
      rank_plot_height()
    })

    output$rank_map <- leaflet::renderLeaflet({
      shp <- map_data()
      validate(
        need(!is.null(shp), "Map data are not available for the current selection."),
        need(nrow(shp) > 0, "No map regions available for the current selection."),
        need(any(!is.na(shp$mean_rank)), "Map could not be joined to ranking summaries for the current selection.")
      )

      pal <- leaflet::colorNumeric(
        palette = "viridis",
        domain = shp$mean_rank,
        na.color = "#D9D9D9"
      )

      leaflet::leaflet(shp) |>
        leaflet::addProviderTiles(leaflet::providers$CartoDB.Positron) |>
        leaflet::addPolygons(
          fillColor = ~pal(mean_rank),
          fillOpacity = 0.8,
          color = "#555555",
          weight = 1,
          opacity = 1,
          smoothFactor = 0.2,
          label = ~lapply(label_html, htmltools::HTML),
          highlightOptions = leaflet::highlightOptions(
            weight = 2,
            color = "#111111",
            fillOpacity = 0.95,
            bringToFront = TRUE
          )
        ) |>
        leaflet::addLegend(
          position = "bottomright",
          pal = pal,
          values = ~mean_rank,
          title = "Mean posterior rank",
          opacity = 0.9
        )
    })

    output$download_button_rank <- renderUI({
      dat <- rank_plot_data()
      if(is.null(dat) || nrow(dat) == 0){
        return(NULL)
      }

      downloadButton(
        ns("download_rank_plot"),
        "Download as PDF",
        icon = icon("download"),
        class = "btn-primary"
      )
    })

    output$download_rank_plot <- downloadHandler(
      filename = function() {
        DHS_country_code <- DHS.country.meta[DHS.country.meta$CountryName == CountryInfo$country(), ]$DHS_CountryCode

        file.prefix <- paste0(
          DHS_country_code,
          CountryInfo$svyYear_selected(), "_",
          CountryInfo$svy_indicator_var(), "_",
          input$selected_adm, "_",
          input$selected_method, "_rank_caterpillar"
        )

        if(!is.null(input$selected_upper_adm) && nzchar(input$selected_upper_adm)){
          file.prefix <- paste0(file.prefix, "_", gsub("[^A-Za-z0-9]+", "_", input$selected_upper_adm))
        }

        file.prefix <- gsub("[-.]", "_", file.prefix)
        paste0(file.prefix, ".pdf")
      },
      content = function(file) {
        dat <- rank_plot_data()
        req(dat)

        selected.adm.num <- admin_to_num_local(input$selected_adm)
        upper_focus <- NULL
        if(!is.na(selected.adm.num) && selected.adm.num > 1){
          upper_focus <- input$selected_upper_adm
        }

        p <- rank_plot_obj(
          rank.df = dat,
          selected_method = input$selected_method,
          selected_adm = input$selected_adm,
          upper_focus = upper_focus
        ) + ggplot2::theme(
          plot.margin = ggplot2::unit(c(0.25, 0.25, 0.25, 0.25), "inches")
        )

        grDevices::pdf(file, width = 10, height = max(4, round(rank_plot_height() / 100)))
        print(p)
        grDevices::dev.off()
      }
    )

  })
}

## To be copied in the UI
# mod_rankings_caterpillar_ui("rankings_caterpillar_1")

## To be copied in the server
# mod_rankings_caterpillar_server(
#   "rankings_caterpillar_1",
#   CountryInfo = CountryInfo,
#   AnalysisInfo = AnalysisInfo,
#   MetaInfo = MetaInfo
# )
