#' rankings_caterpillar UI Function
#'
#' @description A shiny Module for posterior rank caterpillar plots and interactive ranking maps.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#' @noRd
#' @importFrom shiny NS tagList
mod_rankings_caterpillar_ui <- function(id){
  ns <- NS(id)

  fluidPage(
    div(class = "module-title", h4("Subnational Posterior Rank Caterpillar Plot")),

    fluidRow(
      column(12, div(style = "margin: auto; float: left; margin-top: 5px;", uiOutput(ns("info_display"))))
    ),

    fluidRow(
      column(3, selectInput(ns("selected_method"), "Select Method", choices = c("Area-level Model" = "FH", "Unit-level Model" = "Unit"))),
      column(3, selectInput(ns("selected_adm"), "Select Admin Level", choices = character(0))),
      column(3, uiOutput(ns("select_upper_adm")))
    ),

    fluidRow(column(12, div(style = "margin: auto; float: left;", uiOutput(ns("model_fitted_text"))))),
    tags$hr(style = "border-top-color: #E0E0E0;"),

    fluidRow(
      column(12,
        div(style = "width: min(98%, 1100px); margin-top: 10px; margin-left: 20px; margin-right: 10px;", plotOutput(ns("rank_plot"), height = "auto")),
        div(style = "width: min(98%, 1100px); margin-top: 20px; display: flex; justify-content: center;", uiOutput(ns("download_button_rank")))
      )
    ),

    tags$hr(style = "border-top-color: #E0E0E0;"),
    
    fluidRow(
      column(3, uiOutput(ns("top_rank_slider")))
    ),

    fluidRow(
      column(12,
        div(style = "width: min(98%, 1100px); margin-top: 15px; margin-left: 20px; margin-right: 10px;",
            uiOutput(ns("top_rank_map_title")),
            leaflet::leafletOutput(ns("top_rank_prob_map"), height = "650px"))
      )
    ),
    fluidRow(
      column(12,
        div(style = "width: min(98%, 1100px); margin-top: 25px; margin-left: 20px; margin-right: 10px;",
            h4("Mean posterior rank map"),
            leaflet::leafletOutput(ns("rank_map"), height = "650px"))
      )
    )
  )
}

#' rankings_caterpillar Server Functions
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
      if(is.null(x) || is.na(x) || x < 1) return(NA_character_)
      paste0("Admin-", x)
    }

    first_existing_col <- function(dat, candidates){
      if(is.null(dat)) return(NULL)
      hits <- candidates[candidates %in% names(dat)]
      if(length(hits) == 0) return(NULL)
      hits[1]
    }

    clean_display_name <- function(x){
      x <- as.character(x)
      sub(".*_", "", x)
    }

    get_gadm_list <- function(){
      gadm <- tryCatch(CountryInfo$GADM_list_smoothed(), error = function(e) NULL)
      if(is.null(gadm)) gadm <- tryCatch(CountryInfo$GADM_list(), error = function(e) NULL)
      gadm
    }

    build_rank_summary <- function(res.obj, selected_adm, upper_focus = NULL){
      adm.num <- admin_to_num_local(selected_adm)
      if(is.na(adm.num) || adm.num < 1) return(NULL)

      post.name <- paste0("admin", adm.num, "_post")
      res.name <- paste0("res.admin", adm.num)
      samples <- res.obj[[post.name]]
      res.tab <- res.obj[[res.name]]
      if(is.null(samples) || is.null(res.tab)) return(NULL)

      samples <- as.matrix(samples)
      if(nrow(samples) < 1 || ncol(samples) < 1) return(NULL)

      join.col <- first_existing_col(res.tab, c(paste0("admin", adm.num, ".name.full"), "region.name", paste0("admin", adm.num, ".name"), paste0("NAME_", adm.num)))
      display.col <- first_existing_col(res.tab, c(paste0("admin", adm.num, ".name"), paste0("NAME_", adm.num), paste0("admin", adm.num, ".name.full"), "region.name"))
      if(is.null(join.col)) return(NULL)
      if(is.null(display.col)) display.col <- join.col

      parent.col <- NULL
      if(adm.num > 1){
        parent.col <- first_existing_col(res.tab, c(paste0("admin", adm.num - 1, ".name"), paste0("admin", adm.num - 1, ".name.full"), paste0("NAME_", adm.num - 1)))
      }
      prev.col <- first_existing_col(res.tab, c("median", "Mean", "mean", "estimate", "Est", "prev", "prevalence"))

      res.tab2 <- res.tab
      res.tab2$region_name_internal <- as.character(res.tab2[[join.col]])
      res.tab2$display_name <- clean_display_name(as.character(res.tab2[[display.col]]))

      subset_idx <- rep(TRUE, nrow(res.tab2))
      if(adm.num > 1 && !is.null(parent.col) && !is.null(upper_focus) &&
         nzchar(as.character(upper_focus)) && !identical(as.character(upper_focus), "National")){
        subset_idx <- !is.na(res.tab2[[parent.col]]) & as.character(res.tab2[[parent.col]]) == as.character(upper_focus)
      }

      if(length(subset_idx) == ncol(samples)){
        samples.sub <- samples[, subset_idx, drop = FALSE]
        res.tab.sub <- res.tab2[subset_idx, , drop = FALSE]
      } else {
        domain.names <- attr(res.obj, "domain.names")
        if(is.null(domain.names) || length(domain.names) != ncol(samples)) domain.names <- colnames(samples)
        if(is.null(domain.names) || length(domain.names) != ncol(samples)) return(NULL)
        keep.names <- as.character(res.tab2$region_name_internal[subset_idx])
        keep.idx <- match(keep.names, as.character(domain.names))
        keep.valid <- !is.na(keep.idx)
        if(!any(keep.valid)) return(NULL)
        samples.sub <- samples[, keep.idx[keep.valid], drop = FALSE]
        res.tab.sub <- res.tab2[subset_idx, , drop = FALSE][keep.valid, , drop = FALSE]
      }

      if(ncol(samples.sub) < 1 || nrow(res.tab.sub) < 1) return(NULL)

      # Rank 1 is the highest posterior prevalence in each posterior draw (the worst/highest-burden area).
      ranked.samples <- t(apply(samples.sub, 1, function(z) rank(-z, ties.method = "random")))

      out <- data.frame(
        region.name = as.character(res.tab.sub$region_name_internal),
        display_name = as.character(res.tab.sub$display_name),
        mean_rank = apply(ranked.samples, 2, mean, na.rm = TRUE),
        med_rank = apply(ranked.samples, 2, median, na.rm = TRUE),
        sd_rank = apply(ranked.samples, 2, sd, na.rm = TRUE),
        q05 = apply(ranked.samples, 2, quantile, probs = 0.05, na.rm = TRUE),
        q25 = apply(ranked.samples, 2, quantile, probs = 0.25, na.rm = TRUE),
        q75 = apply(ranked.samples, 2, quantile, probs = 0.75, na.rm = TRUE),
        q95 = apply(ranked.samples, 2, quantile, probs = 0.95, na.rm = TRUE),
        stringsAsFactors = FALSE
      )

      if(!is.null(parent.col) && parent.col %in% names(res.tab.sub)) out$upper_region <- as.character(res.tab.sub[[parent.col]]) else out$upper_region <- NA_character_
      if(!is.null(prev.col) && prev.col %in% names(res.tab.sub)) out$estimate_value <- suppressWarnings(as.numeric(res.tab.sub[[prev.col]])) else out$estimate_value <- NA_real_

      old.order <- out$region.name
      out <- out[order(out$med_rank, decreasing = FALSE), , drop = FALSE]
      ranked.samples <- ranked.samples[, match(out$region.name, old.order), drop = FALSE]
      attr(out, "ranked_samples") <- ranked.samples
      rownames(out) <- NULL
      out
    }

    add_top_rank_probability <- function(rank.df, top_n){
      if(is.null(rank.df) || nrow(rank.df) == 0) return(rank.df)
      ranked.samples <- attr(rank.df, "ranked_samples")
      if(is.null(ranked.samples)) return(rank.df)
      top_n <- max(1, min(as.integer(top_n), ncol(ranked.samples)))
      rank.df$prob_rank_top_n <- apply(ranked.samples, 2, function(z) mean(z <= top_n, na.rm = TRUE))
      attr(rank.df, "ranked_samples") <- ranked.samples
      rank.df
    }

    rank_plot_obj <- function(rank.df, selected_method, selected_adm, upper_focus = NULL){
      if(is.null(rank.df) || nrow(rank.df) == 0) return(NULL)

      method_match <- c("Unit" = "Unit-level", "FH" = "Area-level")
      method_des <- unname(method_match[selected_method])
      if(is.na(method_des) || length(method_des) == 0) method_des <- selected_method

      subtitle_txt <- "Top Ranking = Highest Prevalence (Worst)\nPoint = median posterior rank; thick bar = 50% interval; thin bar = 90% interval"
      if(!is.null(upper_focus) && !is.na(upper_focus) && nzchar(upper_focus)){
        if(identical(upper_focus, "National")) {
          subtitle_txt <- paste0(subtitle_txt, " | Across all regions nationally")
        } else {
          subtitle_txt <- paste0(subtitle_txt, " | Within ", upper_focus)
        }
      }

      rank.df$display_name_clean <- sub(".*_", "", as.character(rank.df$display_name))
      rank.df$display_name_f <- factor(rank.df$display_name_clean, levels = rev(rank.df$display_name_clean))

      ggplot2::ggplot(rank.df, ggplot2::aes(x = med_rank, y = display_name_f)) +
        ggplot2::geom_errorbarh(ggplot2::aes(xmin = q05, xmax = q95), height = 0, linewidth = 0.45, color = "#2C7FB8") +
        ggplot2::geom_errorbarh(ggplot2::aes(xmin = q25, xmax = q75), height = 0, linewidth = 1.25, color = "#2C7FB8") +
        ggplot2::geom_point(size = 2.2, color = "#08306B") +
        ggplot2::labs(
          x = "Posterior rank",
          y = NULL,
          title = paste0(method_des, " model posterior rankings at ", selected_adm),
          subtitle = subtitle_txt
        ) +
        ggplot2::theme_minimal(base_size = 12) +
        ggplot2::theme(
          plot.title = ggplot2::element_text(face = "bold"),
          axis.text.y = ggplot2::element_text(size = 10),
          panel.grid.major.y = ggplot2::element_blank(),
          panel.grid.minor.y = ggplot2::element_blank()
        ) +
        ggplot2::scale_x_continuous(breaks = seq_len(nrow(rank.df)))
    }

    build_map_data <- function(rank.df, selected_adm, upper_focus = NULL){
      if(is.null(rank.df) || nrow(rank.df) == 0) return(NULL)
      adm.num <- admin_to_num_local(selected_adm)
      if(is.na(adm.num) || adm.num < 1) return(NULL)

      gadm.list <- get_gadm_list()
      if(is.null(gadm.list)) return(NULL)
      shp <- gadm.list[[selected_adm]]
      if(is.null(shp)) return(NULL)

      fine_col <- paste0("NAME_", adm.num)
      parent_col <- if(adm.num > 1) paste0("NAME_", adm.num - 1) else NULL
      if(!fine_col %in% names(shp)) return(NULL)

      shp2 <- shp
      shp2$fine_name <- as.character(shp2[[fine_col]])
      if(!is.null(parent_col) && parent_col %in% names(shp2)) shp2$upper_region <- as.character(shp2[[parent_col]]) else shp2$upper_region <- NA_character_

      if(adm.num > 1 && !is.null(upper_focus) && nzchar(as.character(upper_focus)) && !identical(as.character(upper_focus), "National")){
        shp2 <- shp2[!is.na(shp2$upper_region) & shp2$upper_region == as.character(upper_focus), , drop = FALSE]
      }
      if(nrow(shp2) < 1) return(NULL)

      name_cols <- paste0("NAME_", seq_len(adm.num))
      name_cols <- name_cols[name_cols %in% names(shp2)]
      if(length(name_cols) > 0){
        shp2$join_name <- apply(as.data.frame(sf::st_drop_geometry(shp2[, name_cols, drop = FALSE])), 1, function(z) paste(as.character(z), collapse = "_"))
      } else {
        shp2$join_name <- shp2$fine_name
      }

      rank.join <- rank.df
      rank.join$join_name <- as.character(rank.join$region.name)
      rank.join$fine_name <- as.character(rank.join$display_name)

      shp2 <- dplyr::left_join(shp2, rank.join, by = "join_name")

      if(!"mean_rank" %in% names(shp2) || all(is.na(shp2$mean_rank))){
        rank.keep <- rank.join[, intersect(c("fine_name", "region.name", "display_name", "mean_rank", "med_rank", "sd_rank", "q05", "q25", "q75", "q95", "prob_rank_top_n", "estimate_value"), names(rank.join)), drop = FALSE]
        keep_cols <- setdiff(names(shp2), intersect(names(shp2), setdiff(names(rank.keep), "fine_name")))
        shp2 <- shp2[, keep_cols, drop = FALSE]
        shp2 <- dplyr::left_join(shp2, rank.keep, by = "fine_name")
      }

      shp2
    }

    leaflet_empty <- function(message = "No map data available for the current selection."){
      leaflet::leaflet() |>
        leaflet::addTiles() |>
        leaflet::addControl(html = htmltools::HTML(message), position = "topright")
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
      if(is.null(upper.adm.gadm)) return("National")

      upper.col <- paste0("NAME_", upper.gadm.num)
      if(!upper.col %in% names(upper.adm.gadm)) return("National")

      c("National", sort(unique(stats::na.omit(as.character(upper.adm.gadm[[upper.col]])))))
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
        choices = upper_choices(),
        selected = "National"
      )
    })

    output$model_fitted_text <- renderUI({
      if(length(input$selected_adm) == 0 || input$selected_adm == "") return(NULL)

      selected_adm <- input$selected_adm
      selected_method <- input$selected_method
      model_res_all <- AnalysisInfo$model_res_list()
      model_res_selected <- model_res_all[[selected_method]][[selected_adm]]

      method_match <- c("Unit" = "Unit-level", "FH" = "Area-level")
      method_des <- unname(method_match[selected_method])
      if(is.na(method_des) || length(method_des) == 0) method_des <- selected_method

      if(is.null(model_res_selected)){
        HTML(paste0(
          "<p style='font-size: large;'>Results for <span style='background-color: #D0E4F7;'><b>", method_des,
          "</b></span> model at <span style='background-color: #D0E4F7;'><b>", selected_adm,
          "</b></span> level are <strong style='color: red;'>NOT</strong> available. Please make sure the model has been successfully fitted.</p>"
        ))
      } else {
        extra.txt <- ""
        if(admin_to_num_local(selected_adm) > 1){
          extra.txt <- " Select National for the full country, or a coarser region to view rankings within it."
        }
        HTML(paste0(
          "<p style='font-size: large;'>Presenting posterior rank caterpillar plot and rank maps for <span style='background-color: #D0E4F7;'><b>",
          method_des, "</b></span> model at <span style='background-color: #D0E4F7;'><b>", selected_adm,
          "</b></span> level.", extra.txt, "</p>"
        ))
      }
    })

    rank_base_data <- reactive({
      req(input$selected_method)
      req(input$selected_adm)

      model_res_all <- AnalysisInfo$model_res_list()
      model_res_selected <- model_res_all[[input$selected_method]][[input$selected_adm]]
      req(!is.null(model_res_selected))

      selected.adm.num <- admin_to_num_local(input$selected_adm)
      upper_focus <- NULL
      if(!is.na(selected.adm.num) && selected.adm.num > 1){
        req(input$selected_upper_adm)
        if(is.null(input$selected_upper_adm) || input$selected_upper_adm == "") return(NULL)
        upper_focus <- input$selected_upper_adm
      }

      build_rank_summary(
        res.obj = model_res_selected,
        selected_adm = input$selected_adm,
        upper_focus = upper_focus
      )
    })

    rank_plot_data <- reactive({ rank_base_data() })

    rank_prob_data <- reactive({
      dat <- rank_base_data()
      if(is.null(dat) || nrow(dat) == 0) return(dat)
      n_regions <- nrow(dat)
      top_n <- input$top_rank_n
      if(is.null(top_n) || is.na(top_n)) top_n <- ceiling(0.25 * n_regions)
      top_n <- max(1, min(as.integer(top_n), n_regions))
      add_top_rank_probability(dat, top_n)
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
      if(!is.na(selected.adm.num) && selected.adm.num > 1) upper_focus <- input$selected_upper_adm
      rank_plot_obj(dat, input$selected_method, input$selected_adm, upper_focus)
    }, height = function(){ rank_plot_height() })
    
    output$top_rank_slider <- renderUI({
      dat <- rank_base_data()
      if(is.null(dat) || nrow(dat) == 0) return(NULL)
      n_regions <- nrow(dat)
      old_val <- input$top_rank_n
      if(is.null(old_val) || is.na(old_val)) old_val <- ceiling(0.25 * n_regions)
      old_val <- max(1, min(as.integer(old_val), n_regions))
      
      sliderInput(
        ns("top_rank_n"),
        "Top-rank threshold N",
        min = 1,
        max = n_regions,
        value = old_val,
        step = 1,
        ticks = FALSE,
        width = "100%"
      )
    })

    output$top_rank_map_title <- renderUI({
      dat <- rank_base_data()
      if(is.null(dat) || nrow(dat) == 0) return(h4("Posterior probability of being in the top N ranks"))
      n_regions <- nrow(dat)
      top_n <- input$top_rank_n
      if(is.null(top_n) || is.na(top_n)) top_n <- ceiling(0.25 * n_regions)
      top_n <- max(1, min(as.integer(top_n), n_regions))
      h4(paste0("Posterior probability of rank ≤ ", top_n))
    })

    output$top_rank_prob_map <- leaflet::renderLeaflet({
      dat <- rank_prob_data()
      validate(need(!is.null(dat) && nrow(dat) > 0, "No ranking data available for the current selection."))
      selected.adm.num <- admin_to_num_local(input$selected_adm)
      upper_focus <- NULL
      if(!is.na(selected.adm.num) && selected.adm.num > 1) upper_focus <- input$selected_upper_adm
      map.dat <- build_map_data(dat, input$selected_adm, upper_focus)
      if(is.null(map.dat) || !"prob_rank_top_n" %in% names(map.dat) || all(is.na(map.dat$prob_rank_top_n))) return(leaflet_empty())

      top_n <- input$top_rank_n
      if(is.null(top_n) || is.na(top_n)) top_n <- ceiling(0.25 * nrow(dat))
      top_n <- max(1, min(as.integer(top_n), nrow(dat)))
      pal <- leaflet::colorNumeric("YlOrRd", domain = map.dat$prob_rank_top_n, na.color = "#D9D9D9")
      labels <- paste0(
        "<strong>", htmltools::htmlEscape(map.dat$display_name), "</strong><br/>",
        "Pr(rank ≤ ", top_n, ") = ", ifelse(is.na(map.dat$prob_rank_top_n), "NA", sprintf("%.3f", map.dat$prob_rank_top_n))
      )
      leaflet::leaflet(map.dat) |>
        leaflet::addProviderTiles(leaflet::providers$CartoDB.Positron) |>
        leaflet::addPolygons(
          fillColor = ~pal(prob_rank_top_n), fillOpacity = 0.75,
          color = "#444444", weight = 0.6, opacity = 1,
          label = lapply(labels, htmltools::HTML),
          highlightOptions = leaflet::highlightOptions(weight = 2, color = "#000000", bringToFront = TRUE)
        ) |>
        leaflet::addLegend(pal = pal, values = ~prob_rank_top_n, opacity = 0.75, title = paste0("Pr(rank ≤ ", top_n, ")"), position = "bottomright")
    })

    output$rank_map <- leaflet::renderLeaflet({
      dat <- rank_prob_data()
      validate(need(!is.null(dat) && nrow(dat) > 0, "No ranking data available for the current selection."))
      selected.adm.num <- admin_to_num_local(input$selected_adm)
      upper_focus <- NULL
      if(!is.na(selected.adm.num) && selected.adm.num > 1) upper_focus <- input$selected_upper_adm
      map.dat <- build_map_data(dat, input$selected_adm, upper_focus)
      if(is.null(map.dat) || !"mean_rank" %in% names(map.dat) || all(is.na(map.dat$mean_rank))) return(leaflet_empty())

      pal <- leaflet::colorNumeric("viridis", domain = map.dat$mean_rank, reverse = TRUE, na.color = "#D9D9D9")
      labels <- paste0(
        "<strong>", htmltools::htmlEscape(map.dat$display_name), "</strong><br/>",
        "Mean rank: ", ifelse(is.na(map.dat$mean_rank), "NA", sprintf("%.2f", map.dat$mean_rank)), "<br/>",
        "Median rank: ", ifelse(is.na(map.dat$med_rank), "NA", sprintf("%.2f", map.dat$med_rank)), "<br/>",
        "SD rank: ", ifelse(is.na(map.dat$sd_rank), "NA", sprintf("%.2f", map.dat$sd_rank)), "<br/>",
        "50% interval: [", ifelse(is.na(map.dat$q25), "NA", sprintf("%.1f", map.dat$q25)), ", ", ifelse(is.na(map.dat$q75), "NA", sprintf("%.1f", map.dat$q75)), "]<br/>",
        "90% interval: [", ifelse(is.na(map.dat$q05), "NA", sprintf("%.1f", map.dat$q05)), ", ", ifelse(is.na(map.dat$q95), "NA", sprintf("%.1f", map.dat$q95)), "]"
      )
      leaflet::leaflet(map.dat) |>
        leaflet::addProviderTiles(leaflet::providers$CartoDB.Positron) |>
        leaflet::addPolygons(
          fillColor = ~pal(mean_rank), fillOpacity = 0.75,
          color = "#444444", weight = 0.6, opacity = 1,
          label = lapply(labels, htmltools::HTML),
          highlightOptions = leaflet::highlightOptions(weight = 2, color = "#000000", bringToFront = TRUE)
        ) |>
        leaflet::addLegend(pal = pal, values = ~mean_rank, opacity = 0.75, title = "Mean rank", position = "bottomright")
    })

    output$download_button_rank <- renderUI({
      dat <- rank_plot_data()
      if(is.null(dat) || nrow(dat) == 0) return(NULL)
      downloadButton(ns("download_rank_plot"), "Download as PDF", icon = icon("download"), class = "btn-primary")
    })

    output$download_rank_plot <- downloadHandler(
      filename = function() {
        DHS_country_code <- DHS.country.meta[DHS.country.meta$CountryName == CountryInfo$country(), ]$DHS_CountryCode
        file.prefix <- paste0(DHS_country_code, CountryInfo$svyYear_selected(), "_", CountryInfo$svy_indicator_var(), "_", input$selected_adm, "_", input$selected_method, "_rank_caterpillar")
        if(!is.null(input$selected_upper_adm) && nzchar(input$selected_upper_adm)) file.prefix <- paste0(file.prefix, "_", gsub("[^A-Za-z0-9]+", "_", input$selected_upper_adm))
        file.prefix <- gsub("[-.]", "_", file.prefix)
        paste0(file.prefix, ".pdf")
      },
      content = function(file) {
        dat <- rank_plot_data()
        req(dat)
        selected.adm.num <- admin_to_num_local(input$selected_adm)
        upper_focus <- NULL
        if(!is.na(selected.adm.num) && selected.adm.num > 1) upper_focus <- input$selected_upper_adm
        p <- rank_plot_obj(dat, input$selected_method, input$selected_adm, upper_focus) +
          ggplot2::theme(plot.margin = ggplot2::unit(c(0.25, 0.25, 0.25, 0.25), "inches"))
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
