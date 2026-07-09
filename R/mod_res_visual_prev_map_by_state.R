#' result_visual UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd
#'
#' @importFrom shiny NS tagList
mod_res_visual_prev_map_by_state_ui <- function(id) {
  ns <- NS(id)
  
  fluidPage(
    shinyWidgets::chooseSliderSkin("Flat", color = "#b0c4de"),
    tags$head(
      tags$style(
        type = "text/css",
        "#big_slider .irs-grid-text, #big_slider .irs-min,
         #big_slider .irs-max,#big_slider .irs-single {font-size: 14px;}"
      ),
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
        h4("Subnational Results Mapping")
    ),
    
    fluidRow(
      column(
        12,
        div(
          style = "margin: auto; float: left; margin-top: 5px",
          uiOutput(ns("info_display"))
        )
      )
    ),
    
    fluidRow(
      column(
        4,
        selectInput(
          ns("selected_adm"),
          "Admin Level to Map",
          choices = character(0)
        )
      ),
      column(
        4,
        selectInput(
          ns("selected_focus"),
          "Map Region",
          choices = "National",
          selected = "National"
        )
      ),
      column(
        4,
        selectInput(
          ns("selected_method"),
          "Select Method",
          choices = c(
            "Direct Estimates" = "Direct",
            "Area-level Model" = "FH",
            "Unit-level Model" = "Unit"
          )
        )
      )
    ),
    
    fluidRow(
      column(
        4,
        selectInput(
          ns("selected_measure"),
          "Select Statistics",
          choices = c(
            "Mean" = "mean",
            "Coefficient of Variation" = "cv",
            "Width of 95% Credible Interval" = "CI.width",
            "Exceedance Probability" = "exceed_prob"
          )
        )
      ),
      div(
        id = "big_slider",
        column(
          4,
          uiOutput(ns("choose_prob"))
        )
      )
    ),
    
    fluidRow(
      column(
        12,
        div(
          style = "margin: auto; float: left;",
          uiOutput(ns("text_display"))
        )
      )
    ),
    
    fluidRow(
      column(
        12,
        hr(style = "border-top-color: #E0E0E0;"),
        shinyWidgets::materialSwitch(
          inputId = ns("PrevmapType"),
          label = "Interactive Map Enabled",
          status = "success",
          value = TRUE
        ),
        div(
          id = "map-container",
          style = "width: max(50%, 600px); margin-top: 20px;",
          uiOutput(ns("prev_map"))
        ),
        div(
          style = paste0(
            "width: max(50%, 600px); margin-top: 6px; ",
            "font-size: 12px; color: #888888;"
          ),
          "* gray areas are indicative of locations with insufficient sample"
        ),
        div(
          style = "width: max(50%, 600px); margin-top: 20px; display: flex; justify-content: center;",
          uiOutput(ns("download_button_ui"))
        )
      )
    )
  )
}


#' result_visual Server Functions
#'
#' @noRd
mod_res_visual_prev_map_by_state_server <- function(id, CountryInfo, AnalysisInfo) {
  moduleServer(id, function(input, output, session) {
    
    ns <- session$ns
    
    `%||%` <- function(x, y) if (is.null(x) || length(x) == 0) y else x
    
    ###############################################################
    ### helpers
    ###############################################################
    
    analysis_levels <- reactive({
      levs <- CountryInfo$GADM_analysis_levels()
      req(levs)
      levs
    })
    
    available_map_levels <- reactive({
      levs <- analysis_levels()
      levs[levs != "National"]
    })
    
    available_methods <- reactive({
      res <- AnalysisInfo$model_res_list()
      if (is.null(res)) {
        return(c("Direct", "FH", "Unit"))
      }
      names(res)
    })
    
    level_num <- function(level_name) {
      if (identical(level_name, "National")) return(0L)
      out <- suppressWarnings(as.integer(gsub(".*Admin-", "", level_name)))
      if (is.na(out)) return(NA_integer_)
      out
    }
    
    get_parent_level <- reactive({
      req(input$selected_adm)
      this_num <- level_num(input$selected_adm)
      req(!is.na(this_num))
      
      if (this_num <= 1) {
        "National"
      } else {
        paste0("Admin-", this_num - 1)
      }
    })
    
    get_sf_for_level <- function(level_name) {
      shp_list <- CountryInfo$GADM_list_smoothed()
      req(shp_list)
      shp <- shp_list[[level_name]]
      req(!is.null(shp))
      shp
    }
    
    guess_name_col <- function(sf_obj, level_name = NULL) {
      nm <- names(sf_obj)
      
      candidates <- character(0)
      
      if (!is.null(level_name) && !identical(level_name, "National")) {
        lvl_num <- level_num(level_name)
        if (!is.na(lvl_num)) {
          candidates <- c(
            candidates,
            paste0("NAME_", lvl_num),
            paste0("name_", lvl_num),
            paste0("ADM", lvl_num, "_EN"),
            paste0("ADM", lvl_num, "_NAME"),
            paste0("admin", lvl_num),
            paste0("Admin", lvl_num)
          )
        }
      }
      
      candidates <- c(
        candidates,
        "shapeName",
        "NAME_1", "NAME_2", "NAME_3", "NAME_4",
        "name_1", "name_2", "name_3", "name_4",
        "ADM1_EN", "ADM2_EN", "ADM3_EN", "ADM4_EN",
        "ADM1_NAME", "ADM2_NAME", "ADM3_NAME", "ADM4_NAME",
        "admin1", "admin2", "admin3", "admin4",
        "Admin1", "Admin2", "Admin3", "Admin4",
        "state", "State", "district", "District",
        "county", "County", "province", "Province"
      )
      
      hit <- unique(candidates[candidates %in% nm])
      
      if (length(hit) == 0) {
        stop("Could not infer region-name column for shapefile.")
      }
      
      hit[1]
    }
    
    guess_parent_col_in_child <- function(child_sf, parent_level) {
      nm <- names(child_sf)
      
      if (identical(parent_level, "National")) {
        return(NULL)
      }
      
      parent_num <- level_num(parent_level)
      req(!is.na(parent_num))
      
      candidates <- c(
        paste0("NAME_", parent_num),
        paste0("name_", parent_num),
        paste0("ADM", parent_num, "_EN"),
        paste0("ADM", parent_num, "_NAME"),
        paste0("admin", parent_num),
        paste0("Admin", parent_num)
      )
      
      hit <- unique(candidates[candidates %in% nm])
      
      if (length(hit) == 0) {
        stop(
          paste0(
            "Could not infer parent-region column for ",
            parent_level,
            " inside child shapefile."
          )
        )
      }
      
      hit[1]
    }
    
    get_focus_choices <- reactive({
      req(input$selected_adm)
      
      parent_level <- get_parent_level()
      
      if (identical(parent_level, "National")) {
        return("National")
      }
      
      parent_sf <- get_sf_for_level(parent_level)
      parent_name_col <- guess_name_col(parent_sf, parent_level)
      
      vals <- parent_sf[[parent_name_col]]
      vals <- vals[!is.na(vals)]
      vals <- sort(unique(as.character(vals)))
      
      c("National", vals)
    })
    
    get_selected_poly <- reactive({
      req(input$selected_adm, input$selected_focus)
      
      child_level <- input$selected_adm
      focus_region <- input$selected_focus %||% "National"
      
      child_sf <- get_sf_for_level(child_level)
      
      if (identical(focus_region, "National")) {
        return(child_sf)
      }
      
      parent_level <- get_parent_level()
      req(!identical(parent_level, "National"))
      
      parent_col_in_child <- guess_parent_col_in_child(child_sf, parent_level)
      
      keep <- as.character(child_sf[[parent_col_in_child]]) == focus_region
      keep[is.na(keep)] <- FALSE
      
      child_sf_subset <- child_sf[keep, , drop = FALSE]
      req(nrow(child_sf_subset) > 0)
      
      child_sf_subset
    })
    
    get_selected_results <- reactive({
      req(input$selected_method, input$selected_adm)
      res_all <- AnalysisInfo$model_res_list()
      req(res_all)
      
      method_res <- res_all[[input$selected_method]]
      req(!is.null(method_res))
      
      method_res[[input$selected_adm]]
    })
    
    method_label <- reactive({
      req(input$selected_method)
      method_match <- c(
        "Direct" = "Direct estimates",
        "Unit" = "Unit-level",
        "FH" = "Area-level"
      )
      method_match[[input$selected_method]] %||% input$selected_method
    })
    
    ###############################################################
    ### dynamic inputs
    ###############################################################
    
    observe({
      method_choices <- available_methods()
      
      label_map <- c(
        "Direct" = "Direct Estimates",
        "FH" = "Area-level Model",
        "Unit" = "Unit-level Model"
      )
      
      named_choices <- stats::setNames(
        method_choices,
        label_map[method_choices] %||% method_choices
      )
      
      selected_method <- if ("Direct" %in% method_choices) "Direct" else method_choices[1]
      
      updateSelectInput(
        session = session,
        inputId = "selected_method",
        choices = named_choices,
        selected = selected_method
      )
    })
    
    observe({
      levs <- available_map_levels()
      req(length(levs) > 0)
      
      selected_adm <- if ("Admin-2" %in% levs) "Admin-2" else levs[1]
      
      updateSelectInput(
        session = session,
        inputId = "selected_adm",
        choices = levs,
        selected = selected_adm
      )
    })
    
    observe({
      req(input$selected_adm)
      
      focus_choices <- get_focus_choices()
      parent_level <- get_parent_level()
      
      updateSelectInput(
        session = session,
        inputId = "selected_focus",
        label = if (identical(parent_level, "National")) {
          "Map Region"
        } else {
          # paste("Map Region", parent_level)
          "Map Region"
        },
        choices = focus_choices,
        selected = "National"
      )
    })
    
    ###############################################################
    ### display info
    ###############################################################
    
    output$info_display <- renderUI({
      HTML(paste0(
        "<p style='font-size: medium;'>",
        "Choose the admin level to map. ",
        "For Admin-2 or higher, you can optionally focus the map within one sub-region, ",
        "or keep <b>National</b> selected to view the full country.",
        "</p>"
      ))
    })
    
    # output$choose_prob <- renderUI({
    #   req(input$selected_measure)
    #   
    #   if (input$selected_measure == "exceed_prob") {
    #     shinyWidgets::sliderTextInput(
    #       inputId = ns("selected_threshold"),
    #       label = "Choose Exceedance Threshold",
    #       choices = as.character(seq(0, 1, by = 0.05)),
    #       selected = "0.20",
    #       grid = TRUE,
    #       width = "100%"
    #     )
    #   } else {
    #     NULL
    #   }
    # })
    
    ## Reference mean used as the default exceedance threshold and marked on the
    ## slider with a dotted line. It tracks the focus selection: the National mean
    ## when viewing the whole country, or the parent-region ("Admin-n") mean when
    ## the user looks within a specific sub-region.
    get_threshold_reference <- reactive({
      focus <- input$selected_focus %||% "National"
      parent_level <- get_parent_level()
      
      natl_mean <- function() {
        natl <- tryCatch(AnalysisInfo$Natl_res(), error = function(e) NULL)
        if (!is.null(natl) && !is.null(natl$direct.est)) {
          as.numeric(natl$direct.est)[1]
        } else {
          NA_real_
        }
      }
      
      ## whole country (or mapping Admin-1, whose parent is National)
      if (identical(focus, "National") || identical(parent_level, "National")) {
        return(list(value = natl_mean(), label = "National Mean"))
      }
      
      ## focus is a specific region at parent_level (Admin-1 or deeper):
      ## pull that region's mean from the parent-level fitted result.
      res_all <- tryCatch(AnalysisInfo$model_res_list(), error = function(e) NULL)
      method <- input$selected_method
      val <- NA_real_
      if (!is.null(res_all) && !is.null(method)) {
        parent_res <- res_all[[method]][[parent_level]]
        if (!is.null(parent_res)) {
          tab <- tryCatch(.harmonize_res(parent_res), error = function(e) NULL)
          if (!is.null(tab)) {
            keyn <- tolower(trimws(as.character(tab$region.name)))
            hit <- which(keyn == tolower(trimws(focus)))
            if (length(hit) >= 1) val <- as.numeric(tab$mean[hit[1]])
          }
        }
      }
      
      ## fall back to the National mean if the regional mean is unavailable
      if (is.na(val)) {
        return(list(value = natl_mean(), label = "National Mean"))
      }
      
      list(value = val, label = paste0(focus, " Mean"))
    })
    
    output$choose_prob <- renderUI({
      req(input$selected_measure)
      
      if (input$selected_measure != "exceed_prob") {
        return(NULL)
      }
      
      ref <- get_threshold_reference()
      ref_val <- ref$value
      if (is.null(ref_val) || is.na(ref_val)) ref_val <- 0.50
      ref_val <- min(max(ref_val, 0), 1)
      
      wrap_id <- ns("threshold_slider_wrap")
      
      ## Position the dotted reference line against the slider track (.irs-line);
      ## min = 0 / max = 1, so the value maps directly to a fraction of the track.
      ## The line spans from the track down past the 0-1 tick labels, and the
      ## text label is placed just below them so it never overlaps the moving
      ## value bubble above the handle or the min/max tick labels.
      marker_js_tmpl <- "
(function(){
  var WRAP_ID='__WRAP__';
  var frac=__FRAC__;
  var wrap=document.getElementById(WRAP_ID);
  if(!wrap){return;}
  function place(){
    var irs=wrap.querySelector('.irs');
    var line=wrap.querySelector('.irs-line');
    var marker=wrap.querySelector('.threshold-ref-marker');
    var label=wrap.querySelector('.threshold-ref-label');
    if(!irs||!line||!marker||!label){return false;}
    var lr=line.getBoundingClientRect();
    var ir=irs.getBoundingClientRect();
    var wr=wrap.getBoundingClientRect();
    if(lr.width===0){return false;}
    var x=(lr.left-wr.left)+frac*lr.width;
    var top=(lr.top-wr.top)-2;
    var bottom=(ir.bottom-wr.top);
    if(bottom<=top){bottom=top+lr.height+18;}
    marker.style.left=x+'px';
    marker.style.top=top+'px';
    marker.style.height=(bottom-top)+'px';
    marker.style.display='block';
    label.style.left=x+'px';
    label.style.top=(bottom+2)+'px';
    label.style.display='block';
    return true;
  }
  var n=0;
  var iv=setInterval(function(){n++; if(place()||n>60){clearInterval(iv);}},50);
  window.__threshRef=window.__threshRef||{};
  if(window.__threshRef[WRAP_ID]){window.removeEventListener('resize',window.__threshRef[WRAP_ID]);}
  window.__threshRef[WRAP_ID]=place;
  window.addEventListener('resize',window.__threshRef[WRAP_ID]);
})();
"
      marker_js <- gsub("__WRAP__", wrap_id, marker_js_tmpl, fixed = TRUE)
      marker_js <- gsub("__FRAC__",
                        format(ref_val, digits = 6, scientific = FALSE),
                        marker_js, fixed = TRUE)
      
      tagList(
        div(
          id = wrap_id,
          ## extra bottom room so the reference label below the ticks has space
          style = "position: relative; padding-bottom: 22px;",
          sliderInput(
            ns("selected_threshold"),
            "Select Threshold",
            min = 0,
            max = 1,
            value = ref_val,
            step = 0.01
          ),
          div(
            class = "threshold-ref-marker",
            style = paste0(
              "position: absolute; display: none; width: 0; ",
              "border-left: 2px dotted #555555; pointer-events: none; z-index: 5;"
            )
          ),
          div(
            class = "threshold-ref-label",
            style = paste0(
              "position: absolute; display: none; transform: translateX(-50%); ",
              "white-space: nowrap; font-size: 11px; color: #555555; ",
              "pointer-events: none; z-index: 5;"
            ),
            ref$label
          ),
          tags$script(HTML(marker_js))
        )
      )
    })
    
    output$text_display <- renderUI({
      req(input$selected_adm, input$selected_focus, input$selected_method, input$selected_measure)
      req(AnalysisInfo$model_res_list())
      
      selected_adm <- input$selected_adm
      selected_focus <- input$selected_focus
      selected_measure_code <- input$selected_measure
      
      measure_labels <- c(
        "mean" = "Mean",
        "cv" = "Coefficient of Variation",
        "CI.width" = "Width of 95% Credible Interval",
        "exceed_prob" = "Exceedance Probability"
      )
      
      selected_measure <- measure_labels[[selected_measure_code]] %||% selected_measure_code
      method_des <- method_label()
      
      model_res_selected <- tryCatch(
        get_selected_results(),
        error = function(e) NULL
      )
      
      focus_text <- if (identical(selected_focus, "National")) {
        "the whole country"
      } else {
        paste("within", selected_focus)
      }
      
      if (is.null(model_res_selected)) {
        HTML(paste0(
          "<p style='font-size: large;'>",
          "Results for ",
          "<span style='background-color: #D0E4F7;'><b>", method_des, "</b></span> ",
          "at ",
          "<span style='background-color: #D0E4F7;'><b>", selected_adm, "</b></span> ",
          "are <strong style='color: red;'>NOT</strong> available. ",
          "Please make sure the model has been successfully fitted.",
          "</p>"
        ))
      } else {
        HTML(paste0(
          "<p style='font-size: large;'>",
          "Presenting ",
          "<span style='background-color: #D0E4F7;'><b>", selected_measure, "</b></span> ",
          "for the ",
          "<span style='background-color: #D0E4F7;'><b>", method_des, "</b></span> ",
          "model at ",
          "<span style='background-color: #D0E4F7;'><b>", selected_adm, "</b></span> ",
          "for ",
          "<span style='background-color: #D0E4F7;'><b>", focus_text, "</b></span>.",
          "</p>"
        ))
      }
    })
    
    ###############################################################
    ### map output selector
    ###############################################################
    
    output$prev_map <- renderUI({
      if (isTRUE(input$PrevmapType)) {
        leaflet::leafletOutput(ns("prev_map_interactive"), height = "700px")
      } else {
        plotOutput(ns("prev_map_static"), height = "700px")
      }
    })
    
    output$download_button_ui <- renderUI({
      downloadButton(
        ns("download_static"),
        "Download Static Map",
        icon = icon("download"),
        class = "btn-primary"
      )
    })
    
    ###############################################################
    ### robust value extraction + fallback rendering helpers
    ###############################################################
    ##
    ## Background:
    ## surveyPrev::prevMap.web() determines its legend range from
    ##   range(value, na.rm = TRUE). When every region maps to NA (which happens
    ##   for some Direct/Admin-2 results with data-sparsity warnings, or whenever
    ##   the display shapefile region names do not line up with the names the
    ##   model was fit on), that range collapses to c(Inf, -Inf) and the internal
    ##   seq() call errors with "'from' must be a finite number". The module's
    ##   tryCatch then swallows the error and returns the bare base map, i.e. the
    ##   "Presenting ..." text shows but the map is blank.
    ##
    ## The helpers below replicate just enough of prevMap.web's data preparation
    ## to (a) decide whether the native renderer can safely be used, (b) hand it
    ## an explicit, finite value.range / num_bins so it can never hit that crash,
    ## and (c) draw a self-contained choropleth fallback when the native join
    ## would be empty -- including a normalized-name retry that recovers data
    ## when the only problem is cosmetic mismatches in region names.
    
    .res_adm_level <- function(res.obj) {
      lvl <- suppressWarnings(as.integer(res.obj$admin))
      if (length(lvl) == 0 || is.na(lvl)) 2L else lvl
    }
    
    # Harmonize a surveyPrev result object into a tidy keyed data.frame.
    .harmonize_res <- function(res.obj) {
      adm_level <- .res_adm_level(res.obj)
      survey.res <- res.obj[[paste0("res.admin", adm_level)]]
      if (is.null(survey.res) || nrow(survey.res) == 0) {
        return(NULL)
      }
      
      pick <- function(df, cols) {
        hit <- cols[cols %in% names(df)]
        if (length(hit) == 0) rep(NA_real_, nrow(df)) else as.numeric(df[[hit[1]]])
      }
      
      mean_v  <- pick(survey.res, c("direct.est", "mean"))
      sd_v    <- pick(survey.res, c("direct.se", "sd"))
      var_v   <- pick(survey.res, c("direct.var", "var"))
      lower_v <- pick(survey.res, c("direct.lower", "lower"))
      upper_v <- pick(survey.res, c("direct.upper", "upper"))
      
      ## invalidate degenerate uncertainties (mirrors surveyPrev harmonization)
      bad_sd <- is.na(sd_v) | sd_v < 1e-08 | sd_v > 1e10
      var_v[bad_sd] <- NA
      lower_v[bad_sd] <- NA
      upper_v[bad_sd] <- NA
      sd_v[bad_sd] <- NA
      
      ci_width <- upper_v - lower_v
      cv_v <- sd_v / mean_v
      
      if (adm_level == 2 && "admin2.name.full" %in% names(survey.res)) {
        join_key <- as.character(survey.res$admin2.name.full)
        region_name <- if ("admin2.name" %in% names(survey.res)) {
          as.character(survey.res$admin2.name)
        } else {
          join_key
        }
        upper_name <- if ("admin1.name" %in% names(survey.res)) {
          as.character(survey.res$admin1.name)
        } else {
          NA_character_
        }
      } else if (adm_level == 1 && "admin1.name" %in% names(survey.res)) {
        join_key <- as.character(survey.res$admin1.name)
        region_name <- join_key
        upper_name <- NA_character_
      } else {
        join_key <- rep("National", nrow(survey.res))
        region_name <- join_key
        upper_name <- NA_character_
      }
      
      data.frame(
        join_key = join_key,
        region.name = region_name,
        upper.adm.name = upper_name,
        mean = mean_v, sd = sd_v, var = var_v,
        lower = lower_v, upper = upper_v,
        CI.width = ci_width, cv = cv_v,
        stringsAsFactors = FALSE
      )
    }
    
    # Build the same join key prevMap.web builds on the polygon shapefile.
    .poly_key <- function(poly, res.obj) {
      adm_level <- .res_adm_level(res.obj)
      by.adm2 <- res.obj$admin.info$by.adm
      by.adm1 <- res.obj$admin.info$by.adm.upper %||% by.adm2
      
      if (adm_level == 2 && !is.null(by.adm1) && !is.null(by.adm2) &&
          by.adm1 %in% names(poly) && by.adm2 %in% names(poly)) {
        paste0(as.character(poly[[by.adm1]]), "_", as.character(poly[[by.adm2]]))
      } else if (adm_level == 1 && !is.null(by.adm2) && by.adm2 %in% names(poly)) {
        as.character(poly[[by.adm2]])
      } else {
        rep("National", nrow(poly))
      }
    }
    
    # Merge results onto the polygons, with an exact then normalized name match.
    .merge_results_to_poly <- function(poly, res.obj, value.to.plot, threshold.p = NULL) {
      poly <- sf::st_as_sf(poly)
      res_tab <- .harmonize_res(res.obj)
      if (is.null(res_tab)) {
        return(NULL)
      }
      
      poly$join_key <- .poly_key(poly, res.obj)
      
      norm <- function(x) {
        x <- tolower(trimws(gsub("\\s+", " ", as.character(x))))
        gsub("\\s*_\\s*", "_", x)
      }
      
      used_norm <- FALSE
      idx <- match(poly$join_key, res_tab$join_key)
      need <- is.na(idx)
      if (any(need)) {
        idx_norm <- match(norm(poly$join_key[need]), norm(res_tab$join_key))
        filled <- !is.na(idx_norm)
        if (any(filled)) {
          idx[which(need)[filled]] <- idx_norm[filled]
          used_norm <- TRUE
        }
      }
      
      for (cc in c("mean", "sd", "var", "lower", "upper", "CI.width", "cv")) {
        poly[[cc]] <- res_tab[[cc]][idx]
      }
      
      ## region labels come from the polygon itself so that no-data regions
      ## (absent from the result) are still named on the map.
      by.adm2 <- res.obj$admin.info$by.adm
      by.adm1 <- res.obj$admin.info$by.adm.upper %||% by.adm2
      poly$region.name <- if (!is.null(by.adm2) && by.adm2 %in% names(poly)) {
        as.character(poly[[by.adm2]])
      } else {
        poly$join_key
      }
      poly$upper.adm.name <- if (!is.null(by.adm1) && by.adm1 %in% names(poly) &&
                                 !identical(by.adm1, by.adm2)) {
        as.character(poly[[by.adm1]])
      } else {
        NA_character_
      }
      
      ## exceedance probability needs posterior samples
      poly$exceed_prob <- NA_real_
      if (identical(value.to.plot, "exceed_prob") && !is.null(threshold.p)) {
        adm_level <- .res_adm_level(res.obj)
        post <- res.obj[[paste0("admin", adm_level, "_post")]]
        if (!is.null(post)) {
          post <- as.matrix(post)
          if (ncol(post) != nrow(res_tab) && nrow(post) == nrow(res_tab)) {
            post <- t(post)
          }
          if (ncol(post) == nrow(res_tab)) {
            ep <- apply(post, 2, function(z) mean(z > threshold.p, na.rm = TRUE))
            ep[is.na(res_tab$var)] <- NA
            poly$exceed_prob <- ep[idx]
          }
        }
      }
      
      poly$value <- switch(
        value.to.plot,
        "mean" = poly$mean,
        "cv" = poly$cv,
        "CI.width" = poly$CI.width,
        "exceed_prob" = poly$exceed_prob,
        poly$mean
      )
      
      poly$warnings <- NA_character_
      poly$warnings[!is.na(poly$mean) & is.na(poly$sd)] <-
        "Data in this region are insufficient for reliable estimates with the current method."
      poly$warnings[is.na(poly$mean)] <- "No data in this region"
      
      list(
        poly = poly,
        n_finite = sum(is.finite(poly$value)),
        used_norm = used_norm
      )
    }
    
    .fmt_value <- function(x, value.to.plot) {
      if (is.na(x)) {
        return("N/A")
      }
      if (value.to.plot %in% c("cv", "exceed_prob")) {
        paste0(formatC(100 * x, format = "f", digits = 1), "%")
      } else {
        formatC(x, format = "f", digits = 3)
      }
    }
    
    # Self-contained interactive choropleth, used whenever prevMap.web cannot.
    .fallback_leaflet <- function(merged, value.to.plot, base_plot,
                                  legend.label = "Estimates", reverse = FALSE) {
      poly <- merged$poly
      finite_vals <- poly$value[is.finite(poly$value)]
      
      if (length(finite_vals) == 0) {
        no_data_labels <- lapply(poly$region.name, function(r) {
          htmltools::HTML(paste0("Region: ", r, "<br/>No reliable estimate"))
        })
        m <- base_plot %>%
          leaflet::addPolygons(
            data = poly, weight = 1, color = "gray",
            fillColor = "#AEAEAE", fillOpacity = 0.9, opacity = 1,
            label = no_data_labels
          ) %>%
          leaflet::addControl(
            html = paste0(
              "<div style='background:#fff;padding:6px 10px;border-radius:4px;",
              "font-size:13px;'>No mappable estimates for this selection.<br/>",
              "Regions lack valid values for the chosen measure.</div>"
            ),
            position = "topright"
          )
        return(m)
      }
      
      rng <- range(finite_vals)
      if (identical(value.to.plot, "exceed_prob")) {
        rng <- c(0, 1)
      } else if (diff(rng) < 0.005) {
        rng <- c(max(0, rng[1] - 0.005), min(1, rng[2] + 0.005))
      }
      pal <- leaflet::colorNumeric("viridis", domain = rng,
                                   na.color = "#AEAEAE", reverse = reverse)
      
      labels <- lapply(seq_len(nrow(poly)), function(i) {
        lab <- paste0("Region: ", poly$region.name[i], "<br/>")
        if (!is.na(poly$upper.adm.name[i])) {
          lab <- paste0(lab, "Upper Admin: ", poly$upper.adm.name[i], "<br/>")
        }
        lab <- paste0(lab, legend.label, ": ",
                      .fmt_value(poly$value[i], value.to.plot))
        if (!is.na(poly$warnings[i])) {
          lab <- paste0(lab, "<br/><span style='color:red;'>",
                        poly$warnings[i], "</span>")
        }
        htmltools::HTML(lab)
      })
      
      m <- base_plot %>%
        leaflet::addPolygons(
          data = poly, weight = 1, color = "gray",
          fillColor = ~ pal(value), fillOpacity = 1, opacity = 1,
          label = labels,
          highlightOptions = leaflet::highlightOptions(
            weight = 2, color = "#666", fillOpacity = 0.75,
            bringToFront = TRUE, sendToBack = TRUE
          )
        ) %>%
        leaflet::addLegend(
          pal = pal, values = finite_vals, title = legend.label,
          position = "bottomright", na.label = "No Data"
        )
      
      ## outline regions whose estimates are statistically unreliable
      sparse <- poly[!is.na(poly$mean) & is.na(poly$sd), ]
      if (nrow(sparse) > 0) {
        m <- m %>%
          leaflet::addPolygons(
            data = sparse, weight = 1.5, color = "gray",
            fill = FALSE, opacity = 0.9, dashArray = "4,4"
          )
      }
      m
    }
    
    # Self-contained static choropleth, used whenever prevMap() cannot.
    .fallback_static <- function(merged, value.to.plot,
                                 legend.label = "Estimates", reverse = FALSE) {
      poly <- merged$poly
      finite_vals <- poly$value[is.finite(poly$value)]
      g <- ggplot2::ggplot(poly)
      
      if (length(finite_vals) == 0) {
        g <- g +
          ggplot2::geom_sf(fill = "#AEAEAE", color = "white", linewidth = 0.2) +
          ggplot2::labs(caption = "No mappable estimates for this selection.")
      } else {
        cols <- viridisLite::viridis(10)
        if (reverse) cols <- rev(cols)
        g <- g +
          ggplot2::geom_sf(ggplot2::aes(fill = value),
                           color = "white", linewidth = 0.2) +
          ggplot2::scale_fill_gradientn(colours = cols, na.value = "#AEAEAE",
                                        name = legend.label)
      }
      
      g +
        ggplot2::theme_minimal() +
        ggplot2::theme(
          axis.text = ggplot2::element_blank(),
          axis.ticks = ggplot2::element_blank(),
          panel.grid = ggplot2::element_blank()
        )
    }
    
    ###############################################################
    ### interactive map
    ###############################################################
    
    prev.map.interactive.output <- reactiveVal(NULL)
    
    output$prev_map_interactive <- leaflet::renderLeaflet({
      
      prev.interactive.plot <- leaflet::leaflet()
      
      if (CountryInfo$use_basemap() == "OSM") {
        prev.interactive.plot <- prev.interactive.plot %>% leaflet::addTiles()
      }
      
      req(input$selected_adm, input$selected_focus, input$selected_method, input$selected_measure)
      req(AnalysisInfo$model_res_list())
      
      if (isTRUE(CountryInfo$use_preloaded_Madagascar())) {
        AnalysisInfo$model_res_list(mdg.ex.model.res)
      }
      
      model_res_selected <- tryCatch(
        get_selected_results(),
        error = function(e) NULL
      )
      
      if (is.null(model_res_selected)) {
        return(prev.interactive.plot)
      }
      
      poly_selected <- tryCatch(
        get_selected_poly(),
        error = function(e) {
          message("Polygon selection failed: ", e$message)
          NULL
        }
      )
      
      if (is.null(poly_selected) || nrow(poly_selected) == 0) {
        return(prev.interactive.plot)
      }
      
      selected_threshold <- if (input$selected_measure == "exceed_prob") {
        req(input$selected_threshold)
        as.numeric(input$selected_threshold)
      } else {
        NULL
      }
      
      reverse_col <- isTRUE(CountryInfo$legend_color_reverse())
      
      ## Pre-compute the values that will be mapped so a data-sparsity / region
      ## naming edge case cannot silently produce a blank map (see helper notes).
      merged <- tryCatch(
        .merge_results_to_poly(
          poly_selected, model_res_selected,
          input$selected_measure, selected_threshold
        ),
        error = function(e) {
          message("Value merge failed: ", e$message)
          NULL
        }
      )
      
      ## Native renderer is safe only when its own (exact-name) join yields at
      ## least one finite value; in that case we hand it an explicit, finite
      ## value.range + num_bins so the all-NA legend crash cannot occur.
      can_use_native <- !is.null(merged) && !merged$used_norm && merged$n_finite >= 1
      
      if (can_use_native) {
        fv <- merged$poly$value[is.finite(merged$poly$value)]
        vr <- range(fv)
        if (input$selected_measure == "exceed_prob") {
          vr <- c(0, 1)
        } else if (diff(vr) < 0.005) {
          vr <- c(max(0, vr[1] - 0.005), min(1, vr[2] + 0.005))
        }
        nb <- suppressWarnings(min(round(diff(vr) / 0.1), 6))
        nb <- max(4, nb)
        if (!is.finite(nb)) nb <- 4
        
        prev.interactive.plot <- tryCatch({
          suppressWarnings(
            surveyPrev::prevMap.web(
              res.obj = model_res_selected,
              poly.shp = poly_selected,
              admin1.focus = NULL,
              value.to.plot = input$selected_measure,
              value.range = vr,
              num_bins = nb,
              legend.label = "Estimates",
              map.title = NULL,
              threshold.p = selected_threshold,
              use.basemap = CountryInfo$use_basemap(),
              legend.color.reverse = reverse_col
            )
          )
        }, error = function(e) {
          message("prevMap.web failed, using fallback: ", e$message)
          tryCatch(
            .fallback_leaflet(merged, input$selected_measure,
                              prev.interactive.plot, "Estimates", reverse_col),
            error = function(e2) {
              message("Fallback map failed: ", e2$message)
              prev.interactive.plot
            }
          )
        })
      } else if (!is.null(merged)) {
        ## Native join would be empty (blank). Draw our own choropleth, which
        ## also recovers data when only a normalized name match succeeded.
        prev.interactive.plot <- tryCatch(
          .fallback_leaflet(merged, input$selected_measure,
                            prev.interactive.plot, "Estimates", reverse_col),
          error = function(e) {
            message("Fallback map failed: ", e$message)
            prev.interactive.plot
          }
        )
      } else {
        ## Could not even prepare values; preserve the legacy native attempt.
        prev.interactive.plot <- tryCatch({
          suppressWarnings(
            surveyPrev::prevMap.web(
              res.obj = model_res_selected,
              poly.shp = poly_selected,
              admin1.focus = NULL,
              value.to.plot = input$selected_measure,
              legend.label = "Estimates",
              map.title = NULL,
              threshold.p = selected_threshold,
              use.basemap = CountryInfo$use_basemap(),
              legend.color.reverse = reverse_col
            )
          )
        }, error = function(e) {
          message("prevMap.web failed: ", e$message)
          prev.interactive.plot
        })
      }
      
      prev.map.interactive.output(prev.interactive.plot)
      prev.interactive.plot
    })
    
    ###############################################################
    ### static map
    ###############################################################
    
    prev.map.static.output <- reactiveVal(NULL)
    
    output$prev_map_static <- renderPlot({
      req(input$selected_adm, input$selected_focus, input$selected_method, input$selected_measure)
      req(AnalysisInfo$model_res_list())
      
      if (isTRUE(CountryInfo$use_preloaded_Madagascar())) {
        AnalysisInfo$model_res_list(mdg.ex.model.res)
      }
      
      model_res_selected <- tryCatch(
        get_selected_results(),
        error = function(e) NULL
      )
      
      if (is.null(model_res_selected)) {
        return(NULL)
      }
      
      poly_selected <- tryCatch(
        get_selected_poly(),
        error = function(e) {
          message("Polygon selection failed: ", e$message)
          NULL
        }
      )
      
      if (is.null(poly_selected) || nrow(poly_selected) == 0) {
        return(NULL)
      }
      
      selected_threshold <- if (input$selected_measure == "exceed_prob") {
        req(input$selected_threshold)
        as.numeric(input$selected_threshold)
      } else {
        NULL
      }
      
      reverse_col <- isTRUE(CountryInfo$legend_color_reverse())
      
      merged <- tryCatch(
        .merge_results_to_poly(
          poly_selected, model_res_selected,
          input$selected_measure, selected_threshold
        ),
        error = function(e) {
          message("Value merge failed: ", e$message)
          NULL
        }
      )
      
      can_use_native <- !is.null(merged) && !merged$used_norm && merged$n_finite >= 1
      
      if (!is.null(merged) && !can_use_native) {
        ## Native join would be empty (blank). Draw our own choropleth, which
        ## also recovers data when only a normalized name match succeeded.
        prev.static.plot <- tryCatch(
          .fallback_static(merged, input$selected_measure, "Estimates", reverse_col),
          error = function(e) {
            message("Fallback static map failed: ", e$message)
            NULL
          }
        )
      } else {
        prev.static.plot <- tryCatch({
          suppressWarnings(
            surveyPrev::prevMap(
              res.obj = model_res_selected,
              poly.shp = poly_selected,
              admin1.focus = NULL,
              value.to.plot = input$selected_measure,
              threshold.p = selected_threshold,
              legend.label = "Estimates",
              color.reverse = reverse_col,
              map.title = NULL
            )
          )
        }, error = function(e) {
          message("prevMap failed, using fallback: ", e$message)
          if (!is.null(merged)) {
            tryCatch(
              .fallback_static(merged, input$selected_measure, "Estimates", reverse_col),
              error = function(e2) {
                message("Fallback static map failed: ", e2$message)
                NULL
              }
            )
          } else {
            NULL
          }
        })
      }
      
      prev.map.static.output(prev.static.plot)
      prev.static.plot
    })
    
    ###############################################################
    ### download static map
    ###############################################################
    
    output$download_static <- downloadHandler(
      filename = function() {
        DHS_country_code <- DHS.country.meta[
          DHS.country.meta$CountryName == CountryInfo$country(),
        ]$DHS_CountryCode
        
        focus_part <- gsub("[^A-Za-z0-9]+", "_", input$selected_focus %||% "National")
        
        file.prefix <- paste0(
          DHS_country_code,
          CountryInfo$svyYear_selected(), "_",
          CountryInfo$svy_indicator_var(), "_",
          input$selected_adm, "_",
          focus_part, "_",
          input$selected_method, "_",
          input$selected_measure
        )
        
        file.prefix <- gsub("[-.]", "_", file.prefix)
        
        paste0(file.prefix, "_prevMap.pdf")
      },
      
      content = function(file) {
        pdf(file, width = 10, height = 10)
        print(prev.map.static.output())
        dev.off()
      }
    )
    
  })
}

## To be copied in the UI
# mod_res_visual_prev_map_by_state_ui("result_visual_1")

## To be copied in the server
# mod_res_visual_prev_map_by_state_server("result_visual_1", CountryInfo, AnalysisInfo)