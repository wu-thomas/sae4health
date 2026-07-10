#' report_preparation UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd
#'
#' @importFrom shiny NS tagList
mod_report_preparation_ui <- function(id){
  ns <- NS(id)
  fluidPage(
    div(class = "module-title",
        h4("Report Generation")), # Add a title

    ## country, survey and indicator info
    fluidRow(
      column(12,
             div(style = " margin: auto;float: left;margin-top: 5px",
                 uiOutput(ns("info_display"))
             )
      )
    ),
    fluidRow(
      column(12,
             div(style = " margin: auto;float: left;margin-top: 10px",
                 uiOutput(ns("text_display"))
             )
      )
    ),

    ###############################################################
    ### NEW: language selector (placed BEFORE the download button).
    ### The label sits ABOVE the dropdown (stacked layout).
    ###############################################################
    fluidRow(
      column(12,
             div(style = "width:100%; max-width:1000px; margin-top: 20px;
                          display: flex; flex-direction: column; align-items: center;
                          gap: 6px;",
                  tags$label(`for` = ns("report_language"),
                             style = "font-weight: bold; margin: 0;",
                             "Report Language:"),
                  div(style = "min-width: 220px;",
                      selectInput(
                        inputId  = ns("report_language"),
                        label    = NULL,
                        choices  = c("English"    = "en",
                                     "Français"   = "fr",
                                     "Español"    = "es",
                                     "Português"  = "pt"),
                        selected = "en",
                        width    = "100%"
                      )
                  )
             )
      )
    ),

    fluidRow(
      column(12,
             div( style = "width:100%;max-width:1000px; margin-top: 20px; display: flex; justify-content: center;",
                  uiOutput(ns("download_button_ui"))
             )
      )
    )
    # Place additional UI elements below
  )
}

#' report_preparation Server Functions
#'
#' @importFrom grid gpar pushViewport grid.draw grid.newpage grid.text upViewport viewport
#' @importFrom gridExtra arrangeGrob tableGrob ttheme_default
#'
#' @noRd
mod_report_preparation_server <- function(id,CountryInfo,AnalysisInfo,MetaInfo){
  moduleServer( id, function(input, output, session){
    ns <- session$ns
    ref_tab_all <- surveyPrev::indicatorList
    DHS_api_est      <- isolate(MetaInfo$DHS_api_est())
    DHS.country.meta <- isolate(MetaInfo$DHS.country.meta())
    DHS.survey.meta  <- isolate(MetaInfo$DHS.survey.meta())
    DHS.dataset.meta <- isolate(MetaInfo$DHS.dataset.meta())
    if (!requireNamespace("grid", quietly = TRUE)) {
      stop("Package 'grid' is required for this function. Please install it with install.packages('grid').")
    }

    if (!requireNamespace("gridExtra", quietly = TRUE)) {
      stop("Package 'gridExtra' is required for this function. Please install it with install.packages('gridExtra').")
    }

    if (!requireNamespace("patchwork", quietly = TRUE)) {
      stop("Package 'patchwork' is required for this function. Please install it with install.packages('patchwork').")
    }

    ###############################################################
    ### NEW: Translation dictionaries
    ###############################################################
    # All user-facing strings used in the PDF and UI are keyed here.
    # Languages: en (English), fr (French), es (Spanish), pt (Portuguese).
    # If a key is missing for a language, code falls back to English.
    i18n_dict <- list(

      ## --- UI / info display strings ---
      ui_selected_country     = list(en = "Selected Country",
                                     fr = "Pays sélectionné",
                                     es = "País seleccionado",
                                     pt = "País selecionado"),
      ui_survey_year          = list(en = "Survey Year",
                                     fr = "Année de l'enquête",
                                     es = "Año de la encuesta",
                                     pt = "Ano da pesquisa"),
      ui_indicator            = list(en = "Indicator",
                                     fr = "Indicateur",
                                     es = "Indicador",
                                     pt = "Indicador"),
      ui_download_button      = list(en = "Download the Report",
                                     fr = "Télécharger le rapport",
                                     es = "Descargar el informe",
                                     pt = "Baixar o relatório"),
      ui_report_being_prepared = list(en = "Report is being prepared. Please wait...",
                                      fr = "Le rapport est en cours de préparation. Veuillez patienter...",
                                      es = "El informe se está preparando. Por favor, espere...",
                                      pt = "O relatório está sendo preparado. Por favor, aguarde..."),

      ## --- Generic error / fallback ---
      page_error              = list(en = "Something went wrong generating this page of the report.",
                                     fr = "Une erreur s'est produite lors de la génération de cette page du rapport.",
                                     es = "Algo salió mal al generar esta página del informe.",
                                     pt = "Algo deu errado ao gerar esta página do relatório."),

      ## --- Summary info page ---
      summary_info            = list(en = "Summary Info",
                                     fr = "Informations sommaires",
                                     es = "Información resumida",
                                     pt = "Informações resumidas"),
      country_label           = list(en = "Country: ",
                                     fr = "Pays : ",
                                     es = "País: ",
                                     pt = "País: "),
      survey_label            = list(en = "Survey: ",
                                     fr = "Enquête : ",
                                     es = "Encuesta: ",
                                     pt = "Pesquisa: "),
      indicator_label         = list(en = "Indicator: ",
                                     fr = "Indicateur : ",
                                     es = "Indicador: ",
                                     pt = "Indicador: "),
      levels_label            = list(en = "Levels: ",
                                     fr = "Niveaux : ",
                                     es = "Niveles: ",
                                     pt = "Níveis: "),
      n_regions_title         = list(en = "Number of regions at selected admin levels:",
                                     fr = "Nombre de régions aux niveaux administratifs sélectionnés :",
                                     es = "Número de regiones en los niveles administrativos seleccionados:",
                                     pt = "Número de regiões nos níveis administrativos selecionados:"),
      detailed_ind_title      = list(en = "Detailed information on the indicator:",
                                     fr = "Informations détaillées sur l'indicateur :",
                                     es = "Información detallada sobre el indicador:",
                                     pt = "Informações detalhadas sobre o indicador:"),
      footnote_default_a      = list(en = paste0("For indicators with multiple versions, the app defaults to the ",
                                                 "5-year period before the survey (unless specified otherwise),"),
                                     fr = paste0("Pour les indicateurs ayant plusieurs versions, l'application utilise par défaut la ",
                                                 "période de 5 ans précédant l'enquête (sauf indication contraire),"),
                                     es = paste0("Para los indicadores con múltiples versiones, la aplicación utiliza por defecto el ",
                                                 "período de 5 años antes de la encuesta (a menos que se especifique lo contrario),"),
                                     pt = paste0("Para indicadores com múltiplas versões, o aplicativo utiliza por padrão o ",
                                                 "período de 5 anos antes da pesquisa (salvo indicação contrária),")),
      footnote_default_b      = list(en = "and unstratified age groups (total).",
                                     fr = "et les groupes d'âge non stratifiés (total).",
                                     es = "y grupos de edad no estratificados (total).",
                                     pt = "e grupos etários não estratificados (total)."),

      ## --- Column headers for indicator details table ---
      col_dhs_id              = list(en = "DHS Standard ID",
                                     fr = "Identifiant standard DHS",
                                     es = "Identificador estándar DHS",
                                     pt = "Identificador padrão DHS"),
      col_dhs_chapter         = list(en = "DHS Report Chapter",
                                     fr = "Chapitre du rapport DHS",
                                     es = "Capítulo del informe DHS",
                                     pt = "Capítulo do relatório DHS"),
      col_definition          = list(en = "Definition",
                                     fr = "Définition",
                                     es = "Definición",
                                     pt = "Definição"),

      ## --- Estimate Consistency Check page ---
      consistency_title       = list(en = "Estimate Consistency Check (National Level)",
                                     fr = "Vérification de cohérence des estimations (niveau national)",
                                     es = "Verificación de coherencia de las estimaciones (nivel nacional)",
                                     pt = "Verificação de consistência das estimativas (nível nacional)"),
      consistency_intro       = list(en = "To ensure the accuracy of our indicator coding schemes, we highly recommend users to compare ",
                                     fr = "Pour garantir l'exactitude de nos schémas de codage des indicateurs, nous recommandons fortement aux utilisateurs de comparer ",
                                     es = "Para asegurar la exactitud de nuestros esquemas de codificación de indicadores, recomendamos encarecidamente a los usuarios comparar ",
                                     pt = "Para garantir a precisão dos nossos esquemas de codificação dos indicadores, recomendamos fortemente que os usuários comparem "),
      consistency_app_bold    = list(en = "app-calculated national estimates",
                                     fr = "les estimations nationales calculées par l'application",
                                     es = "las estimaciones nacionales calculadas por la aplicación",
                                     pt = "as estimativas nacionais calculadas pelo aplicativo"),
      consistency_with        = list(en = " with the ",
                                     fr = " avec les ",
                                     es = " con los ",
                                     pt = " com os "),
      consistency_dhs_bold    = list(en = "DHS final reports",
                                     fr = "rapports finaux DHS",
                                     es = "informes finales DHS",
                                     pt = "relatórios finais DHS"),
      natl_est_app            = list(en = "National estimate (from the app): ",
                                     fr = "Estimation nationale (à partir de l'application) : ",
                                     es = "Estimación nacional (de la aplicación): ",
                                     pt = "Estimativa nacional (do aplicativo): "),
      natl_est_dhs_unavail    = list(en = "Estimate from the DHS report is not available through DHS API. Please manually check for consistency. ",
                                     fr = "L'estimation du rapport DHS n'est pas disponible via l'API DHS. Veuillez vérifier manuellement la cohérence. ",
                                     es = "La estimación del informe DHS no está disponible a través de la API de DHS. Por favor, verifique manualmente la coherencia. ",
                                     pt = "A estimativa do relatório DHS não está disponível pela API do DHS. Por favor, verifique manualmente a consistência. "),
      natl_est_dhs            = list(en = "National estimate (from DHS Final Report): ",
                                     fr = "Estimation nationale (à partir du rapport final DHS) : ",
                                     es = "Estimación nacional (del informe final DHS): ",
                                     pt = "Estimativa nacional (do relatório final DHS): "),
      per_1000                = list(en = " per 1000 individuals",
                                     fr = " pour 1000 individus",
                                     es = " por cada 1000 personas",
                                     pt = " por 1000 indivíduos"),

      ## --- Overall Sample Info page ---
      overall_sample_info     = list(en = "Overall Sample Info",
                                     fr = "Informations globales sur l'échantillon",
                                     es = "Información general de la muestra",
                                     pt = "Informações gerais da amostra"),
      total_clusters          = list(en = "Total number of clusters: ",
                                     fr = "Nombre total de grappes : ",
                                     es = "Número total de conglomerados: ",
                                     pt = "Número total de conglomerados: "),
      total_sample_size       = list(en = "Total sample size: ",
                                     fr = "Taille totale de l'échantillon : ",
                                     es = "Tamaño total de la muestra: ",
                                     pt = "Tamanho total da amostra: "),
      total_events            = list(en = "Total number of events: ",
                                     fr = "Nombre total d'événements : ",
                                     es = "Número total de eventos: ",
                                     pt = "Número total de eventos: "),
      regions_no_data         = list(en = "Number of regions without any data:",
                                     fr = "Nombre de régions sans aucune donnée :",
                                     es = "Número de regiones sin datos:",
                                     pt = "Número de regiões sem dados:"),
      col_admin_level         = list(en = "Admin Level",
                                     fr = "Niveau administratif",
                                     es = "Nivel administrativo",
                                     pt = "Nível administrativo"),
      col_regions_no_clusters = list(en = "Regions without Any Clusters",
                                     fr = "Régions sans aucune grappe",
                                     es = "Regiones sin ningún conglomerado",
                                     pt = "Regiões sem nenhum conglomerado"),

      ## --- Admin-specific sample info ---
      sample_info_for         = list(en = "Sample Info for ",
                                     fr = "Informations sur l'échantillon pour ",
                                     es = "Información de la muestra para ",
                                     pt = "Informações da amostra para "),
      n_events                = list(en = "Number of Events",
                                     fr = "Nombre d'événements",
                                     es = "Número de eventos",
                                     pt = "Número de eventos"),
      n_clusters              = list(en = "Number of Clusters",
                                     fr = "Nombre de grappes",
                                     es = "Número de conglomerados",
                                     pt = "Número de conglomerados"),
      n_samples               = list(en = "Number of Samples",
                                     fr = "Nombre d'échantillons",
                                     es = "Número de muestras",
                                     pt = "Número de amostras"),

      ## --- Method labels ---
      method_direct           = list(en = "Direct estimates",
                                     fr = "Estimations directes",
                                     es = "Estimaciones directas",
                                     pt = "Estimativas diretas"),
      method_unit             = list(en = "Unit-level",
                                     fr = "Au niveau de l'unité",
                                     es = "A nivel de unidad",
                                     pt = "A nível de unidade"),
      method_fh               = list(en = "Area-level",
                                     fr = "Au niveau de la zone",
                                     es = "A nivel de área",
                                     pt = "A nível de área"),

      ## --- Measure labels (with line break) ---
      measure_cv_break        = list(en = "Coefficient \n of variation",
                                     fr = "Coefficient \n de variation",
                                     es = "Coeficiente \n de variación",
                                     pt = "Coeficiente \n de variação"),
      measure_mean_break      = list(en = "Mean",
                                     fr = "Moyenne",
                                     es = "Media",
                                     pt = "Média"),
      measure_ciwidth_break   = list(en = "Width of \n 95% CI",
                                     fr = "Largeur de \n l'IC à 95%",
                                     es = "Ancho del \n IC del 95%",
                                     pt = "Largura do \n IC de 95%"),
      measure_exceed_break    = list(en = "Exceedance \n probability",
                                     fr = "Probabilité \n de dépassement",
                                     es = "Probabilidad \n de excedencia",
                                     pt = "Probabilidade \n de excedência"),

      ## --- Measure labels (no line break) ---
      measure_cv              = list(en = "Coefficient of variation",
                                     fr = "Coefficient de variation",
                                     es = "Coeficiente de variación",
                                     pt = "Coeficiente de variação"),
      measure_mean            = list(en = "Mean",
                                     fr = "Moyenne",
                                     es = "Media",
                                     pt = "Média"),
      measure_ciwidth         = list(en = "Width of 95% CI",
                                     fr = "Largeur de l'IC à 95%",
                                     es = "Ancho del IC del 95%",
                                     pt = "Largura do IC de 95%"),
      measure_exceed          = list(en = "Exceedance probability",
                                     fr = "Probabilité de dépassement",
                                     es = "Probabilidad de excedencia",
                                     pt = "Probabilidade de excedência"),

      ## --- Map / scatter page titles ---
      key_stats_maps          = list(en = "Key Statistics Maps: ",
                                     fr = "Cartes des statistiques clés : ",
                                     es = "Mapas de estadísticas clave: ",
                                     pt = "Mapas das estatísticas principais: "),
      model_at                = list(en = " model at ",
                                     fr = " modèle au niveau ",
                                     es = " modelo en el nivel ",
                                     pt = " modelo no nível "),
      level_word              = list(en = " level",
                                     fr = "",
                                     es = "",
                                     pt = ""),
      model_comparison        = list(en = "Model Comparison: Direct Estimate vs ",
                                     fr = "Comparaison des modèles : estimation directe vs ",
                                     es = "Comparación de modelos: estimación directa vs ",
                                     pt = "Comparação de modelos: estimativa direta vs ")
    )

    # Topic / DHS Report Chapter translations (15 unique values in ref_tab_all).
    topic_dict <- list(
      "Chapter 02 - Housing Characteristics And Household Population" = list(
        fr = "Chapitre 02 - Caractéristiques du logement et population des ménages",
        es = "Capítulo 02 - Características de la vivienda y población del hogar",
        pt = "Capítulo 02 - Características da habitação e população do domicílio"),
      "Chapter 03 - Characteristics Of Respondents" = list(
        fr = "Chapitre 03 - Caractéristiques des répondants",
        es = "Capítulo 03 - Características de los encuestados",
        pt = "Capítulo 03 - Características dos respondentes"),
      "Chapter 04 - Marriage And Sexual Activity" = list(
        fr = "Chapitre 04 - Mariage et activité sexuelle",
        es = "Capítulo 04 - Matrimonio y actividad sexual",
        pt = "Capítulo 04 - Casamento e atividade sexual"),
      "Chapter 06 - Fertility Preferences" = list(
        fr = "Chapitre 06 - Préférences en matière de fécondité",
        es = "Capítulo 06 - Preferencias de fecundidad",
        pt = "Capítulo 06 - Preferências de fecundidade"),
      "Chapter 07 - Family Planning" = list(
        fr = "Chapitre 07 - Planification familiale",
        es = "Capítulo 07 - Planificación familiar",
        pt = "Capítulo 07 - Planejamento familiar"),
      "Chapter 08 - Infant And Child Mortality" = list(
        fr = "Chapitre 08 - Mortalité infantile et juvénile",
        es = "Capítulo 08 - Mortalidad infantil y de la niñez",
        pt = "Capítulo 08 - Mortalidade infantil e na infância"),
      "Chapter 09 - Maternal Health" = list(
        fr = "Chapitre 09 - Santé maternelle",
        es = "Capítulo 09 - Salud materna",
        pt = "Capítulo 09 - Saúde materna"),
      "Chapter 10 - Child Health" = list(
        fr = "Chapitre 10 - Santé de l'enfant",
        es = "Capítulo 10 - Salud infantil",
        pt = "Capítulo 10 - Saúde infantil"),
      "Chapter 11 - Nutrition Of Children And Adults" = list(
        fr = "Chapitre 11 - Nutrition des enfants et des adultes",
        es = "Capítulo 11 - Nutrición de niños y adultos",
        pt = "Capítulo 11 - Nutrição de crianças e adultos"),
      "Chapter 12 - Malaria" = list(
        fr = "Chapitre 12 - Paludisme",
        es = "Capítulo 12 - Malaria",
        pt = "Capítulo 12 - Malária"),
      "Chapter 13 - HIV/AIDS-Related Knowledge, Attitudes, And Behaviour" = list(
        fr = "Chapitre 13 - Connaissances, attitudes et comportements liés au VIH/SIDA",
        es = "Capítulo 13 - Conocimientos, actitudes y comportamientos relacionados con el VIH/SIDA",
        pt = "Capítulo 13 - Conhecimentos, atitudes e comportamentos relacionados ao HIV/AIDS"),
      "Chapter 14 - HIV Prevalence" = list(
        fr = "Chapitre 14 - Prévalence du VIH",
        es = "Capítulo 14 - Prevalencia del VIH",
        pt = "Capítulo 14 - Prevalência do HIV"),
      "Chapter 15 - Women's Empowerment" = list(
        fr = "Chapitre 15 - Autonomisation des femmes",
        es = "Capítulo 15 - Empoderamiento de la mujer",
        pt = "Capítulo 15 - Empoderamento da mulher"),
      "Chapter 17 - Domestic Violence" = list(
        fr = "Chapitre 17 - Violence domestique",
        es = "Capítulo 17 - Violencia doméstica",
        pt = "Capítulo 17 - Violência doméstica"),
      "Chapter 18 - Female Genital Cutting" = list(
        fr = "Chapitre 18 - Mutilations génitales féminines",
        es = "Capítulo 18 - Mutilación genital femenina",
        pt = "Capítulo 18 - Mutilação genital feminina")
    )

    # --- Building-block translations for indicator definitions ---
    # Rather than hand-translate all 180 full definitions (which would be
    # error-prone and balloon the script), we translate the recurring
    # phrase fragments that make up nearly every Full_definition in
    # ref_tab_all. `translate_definition()` applies these substitutions
    # in length-descending order so longer phrases are matched first.
    definition_phrase_dict <- list(

      ## Common opening phrases
      "Percentage of women age 15-49"          = list(fr = "Pourcentage des femmes âgées de 15 à 49 ans",
                                                     es = "Porcentaje de mujeres de 15 a 49 años",
                                                     pt = "Percentual de mulheres de 15 a 49 anos"),
      "Percentage of men age 15-49"            = list(fr = "Pourcentage des hommes âgés de 15 à 49 ans",
                                                     es = "Porcentaje de hombres de 15 a 49 años",
                                                     pt = "Percentual de homens de 15 a 49 anos"),
      "Percentage of women"                    = list(fr = "Pourcentage des femmes",
                                                     es = "Porcentaje de mujeres",
                                                     pt = "Percentual de mulheres"),
      "Percentage of men"                      = list(fr = "Pourcentage des hommes",
                                                     es = "Porcentaje de hombres",
                                                     pt = "Percentual de homens"),
      "Percentage of children"                 = list(fr = "Pourcentage des enfants",
                                                     es = "Porcentaje de niños",
                                                     pt = "Percentual de crianças"),
      "Percentage of live births"              = list(fr = "Pourcentage des naissances vivantes",
                                                     es = "Porcentaje de nacidos vivos",
                                                     pt = "Percentual de nascidos vivos"),
      "Percentage of households"               = list(fr = "Pourcentage des ménages",
                                                     es = "Porcentaje de hogares",
                                                     pt = "Percentual de domicílios"),
      "Percentage of currently married women"  = list(fr = "Pourcentage des femmes actuellement mariées",
                                                     es = "Porcentaje de mujeres actualmente casadas",
                                                     pt = "Percentual de mulheres atualmente casadas"),
      "Percentage of"                          = list(fr = "Pourcentage de",
                                                     es = "Porcentaje de",
                                                     pt = "Percentual de"),
      "Number of"                              = list(fr = "Nombre de",
                                                     es = "Número de",
                                                     pt = "Número de"),

      ## Common time-period phrases
      "in the five (or three) years preceding the survey" = list(
        fr = "au cours des cinq (ou trois) années précédant l'enquête",
        es = "en los cinco (o tres) años anteriores a la encuesta",
        pt = "nos cinco (ou três) anos anteriores à pesquisa"),
      "in the five years preceding the survey" = list(
        fr = "au cours des cinq années précédant l'enquête",
        es = "en los cinco años anteriores a la encuesta",
        pt = "nos cinco anos anteriores à pesquisa"),
      "in the two weeks preceding the survey"  = list(
        fr = "au cours des deux semaines précédant l'enquête",
        es = "en las dos semanas anteriores a la encuesta",
        pt = "nas duas semanas anteriores à pesquisa"),
      "preceding the survey"                   = list(fr = "précédant l'enquête",
                                                     es = "anteriores a la encuesta",
                                                     pt = "anteriores à pesquisa"),

      ## Common content phrases
      "born in the"                            = list(fr = "nés au cours des",
                                                     es = "nacidos en los",
                                                     pt = "nascidos nos"),
      "under five"                             = list(fr = "de moins de cinq ans",
                                                     es = "menores de cinco años",
                                                     pt = "menores de cinco anos"),
      "under age 5"                            = list(fr = "de moins de 5 ans",
                                                     es = "menores de 5 años",
                                                     pt = "menores de 5 anos"),
      "with diarrhea"                          = list(fr = "atteints de diarrhée",
                                                     es = "con diarrea",
                                                     pt = "com diarreia"),
      "with fever"                             = list(fr = "ayant eu de la fièvre",
                                                     es = "con fiebre",
                                                     pt = "com febre"),
      "who received"                           = list(fr = "qui ont reçu",
                                                     es = "que recibieron",
                                                     pt = "que receberam"),
      "who were"                               = list(fr = "qui étaient",
                                                     es = "que estaban",
                                                     pt = "que estavam"),
      "who"                                    = list(fr = "qui",
                                                     es = "que",
                                                     pt = "que"),
      "whose"                                  = list(fr = "dont",
                                                     es = "cuyo",
                                                     pt = "cujo"),
      "birth weight was less than 2.5 kg"      = list(fr = "le poids de naissance était inférieur à 2,5 kg",
                                                     es = "el peso al nacer fue inferior a 2,5 kg",
                                                     pt = "o peso ao nascer foi inferior a 2,5 kg"),
      "oral rehydration solution"              = list(fr = "solution de réhydratation orale",
                                                     es = "solución de rehidratación oral",
                                                     pt = "solução de reidratação oral"),
      "recommended home fluids"                = list(fr = "liquides maison recommandés",
                                                     es = "líquidos caseros recomendados",
                                                     pt = "líquidos caseiros recomendados"),
      "antibiotics"                            = list(fr = "antibiotiques",
                                                     es = "antibióticos",
                                                     pt = "antibióticos"),
      "vaccinated against"                     = list(fr = "vaccinés contre",
                                                     es = "vacunados contra",
                                                     pt = "vacinados contra"),
      "vaccine"                                = list(fr = "vaccin",
                                                     es = "vacuna",
                                                     pt = "vacina"),
      "currently married"                      = list(fr = "actuellement mariées",
                                                     es = "actualmente casadas",
                                                     pt = "atualmente casadas"),
      "currently using"                        = list(fr = "utilisant actuellement",
                                                     es = "actualmente usando",
                                                     pt = "atualmente usando"),
      "any modern method"                      = list(fr = "n'importe quelle méthode moderne",
                                                     es = "cualquier método moderno",
                                                     pt = "qualquer método moderno"),
      "any method"                             = list(fr = "n'importe quelle méthode",
                                                     es = "cualquier método",
                                                     pt = "qualquer método"),
      "contraception"                          = list(fr = "contraception",
                                                     es = "anticoncepción",
                                                     pt = "contracepção"),
      "family planning"                        = list(fr = "planification familiale",
                                                     es = "planificación familiar",
                                                     pt = "planejamento familiar"),
      "antenatal care"                         = list(fr = "soins prénatals",
                                                     es = "atención prenatal",
                                                     pt = "cuidado pré-natal"),
      "skilled provider"                       = list(fr = "professionnel qualifié",
                                                     es = "proveedor calificado",
                                                     pt = "profissional qualificado"),
      "skilled birth attendant"                = list(fr = "personnel qualifié pour l'accouchement",
                                                     es = "asistente calificado al parto",
                                                     pt = "profissional qualificado para o parto"),
      "delivered"                              = list(fr = "ayant accouché",
                                                     es = "que dieron a luz",
                                                     pt = "que deram à luz"),
      "delivery"                               = list(fr = "accouchement",
                                                     es = "parto",
                                                     pt = "parto"),
      "pregnancy"                              = list(fr = "grossesse",
                                                     es = "embarazo",
                                                     pt = "gravidez"),
      "pregnant"                               = list(fr = "enceintes",
                                                     es = "embarazadas",
                                                     pt = "grávidas"),
      "anemia"                                 = list(fr = "anémie",
                                                     es = "anemia",
                                                     pt = "anemia"),
      "anemic"                                 = list(fr = "anémiques",
                                                     es = "anémicas",
                                                     pt = "anêmicas"),
      "stunting"                               = list(fr = "retard de croissance",
                                                     es = "retraso del crecimiento",
                                                     pt = "déficit de crescimento"),
      "stunted"                                = list(fr = "ayant un retard de croissance",
                                                     es = "con retraso del crecimiento",
                                                     pt = "com déficit de crescimento"),
      "wasting"                                = list(fr = "émaciation",
                                                     es = "emaciación",
                                                     pt = "emaciação"),
      "wasted"                                 = list(fr = "émaciés",
                                                     es = "emaciados",
                                                     pt = "emaciados"),
      "underweight"                            = list(fr = "insuffisance pondérale",
                                                     es = "bajo peso",
                                                     pt = "baixo peso"),
      "overweight"                             = list(fr = "en surpoids",
                                                     es = "con sobrepeso",
                                                     pt = "com sobrepeso"),
      "breastfed"                              = list(fr = "allaités",
                                                     es = "amamantados",
                                                     pt = "amamentados"),
      "breastfeeding"                          = list(fr = "allaitement maternel",
                                                     es = "lactancia materna",
                                                     pt = "amamentação"),
      "exclusively breastfed"                  = list(fr = "exclusivement allaités",
                                                     es = "amamantados exclusivamente",
                                                     pt = "amamentados exclusivamente"),
      "malaria"                                = list(fr = "paludisme",
                                                     es = "malaria",
                                                     pt = "malária"),
      "mosquito net"                           = list(fr = "moustiquaire",
                                                     es = "mosquitero",
                                                     pt = "mosquiteiro"),
      "insecticide-treated"                    = list(fr = "imprégnée d'insecticide",
                                                     es = "tratada con insecticida",
                                                     pt = "tratado com inseticida"),
      "HIV"                                    = list(fr = "VIH",
                                                     es = "VIH",
                                                     pt = "HIV"),
      "AIDS"                                   = list(fr = "SIDA",
                                                     es = "SIDA",
                                                     pt = "AIDS"),
      "tested for"                             = list(fr = "ayant été testés pour le",
                                                     es = "que se hicieron la prueba de",
                                                     pt = "testados para"),
      "tested"                                 = list(fr = "testés",
                                                     es = "examinados",
                                                     pt = "testados"),
      "education"                              = list(fr = "éducation",
                                                     es = "educación",
                                                     pt = "educação"),
      "literate"                               = list(fr = "alphabétisés",
                                                     es = "alfabetizados",
                                                     pt = "alfabetizados"),
      "literacy"                               = list(fr = "alphabétisation",
                                                     es = "alfabetización",
                                                     pt = "alfabetização"),
      "drinking water"                         = list(fr = "eau de boisson",
                                                     es = "agua potable",
                                                     pt = "água potável"),
      "improved source"                        = list(fr = "source améliorée",
                                                     es = "fuente mejorada",
                                                     pt = "fonte melhorada"),
      "sanitation"                             = list(fr = "assainissement",
                                                     es = "saneamiento",
                                                     pt = "saneamento"),
      "toilet facility"                        = list(fr = "installation sanitaire",
                                                     es = "instalación sanitaria",
                                                     pt = "instalação sanitária"),
      "electricity"                            = list(fr = "électricité",
                                                     es = "electricidad",
                                                     pt = "eletricidade"),
      "household"                              = list(fr = "ménage",
                                                     es = "hogar",
                                                     pt = "domicílio"),
      "households"                             = list(fr = "ménages",
                                                     es = "hogares",
                                                     pt = "domicílios"),
      "children"                               = list(fr = "enfants",
                                                     es = "niños",
                                                     pt = "crianças"),
      "child"                                  = list(fr = "enfant",
                                                     es = "niño",
                                                     pt = "criança"),
      "women"                                  = list(fr = "femmes",
                                                     es = "mujeres",
                                                     pt = "mulheres"),
      "men"                                    = list(fr = "hommes",
                                                     es = "hombres",
                                                     pt = "homens"),
      "circumcised"                            = list(fr = "circoncis",
                                                     es = "circuncidados",
                                                     pt = "circuncidados"),
      "violence"                               = list(fr = "violence",
                                                     es = "violencia",
                                                     pt = "violência"),
      "with"                                   = list(fr = "avec",
                                                     es = "con",
                                                     pt = "com"),
      "or"                                     = list(fr = "ou",
                                                     es = "o",
                                                     pt = "ou"),
      "either"                                 = list(fr = "soit",
                                                     es = "ya sea",
                                                     pt = "tanto"),
      "and"                                    = list(fr = "et",
                                                     es = "y",
                                                     pt = "e"),
      "the"                                    = list(fr = "le",
                                                     es = "el",
                                                     pt = "o"),
      "of age"                                 = list(fr = "d'âge",
                                                     es = "de edad",
                                                     pt = "de idade"),
      "months"                                 = list(fr = "mois",
                                                     es = "meses",
                                                     pt = "meses"),
      "years"                                  = list(fr = "ans",
                                                     es = "años",
                                                     pt = "anos"),
      "age"                                    = list(fr = "âge",
                                                     es = "edad",
                                                     pt = "idade"),
      "born"                                   = list(fr = "nés",
                                                     es = "nacidos",
                                                     pt = "nascidos"),
      "who had"                                = list(fr = "qui ont eu",
                                                     es = "que tuvieron",
                                                     pt = "que tiveram")
    )

    ###############################################################
    ### NEW: Helper - look up a translation by key
    ###############################################################
    tr <- function(key, lang = "en") {
      entry <- i18n_dict[[key]]
      if (is.null(entry)) return(key) # fallback: the key itself
      val <- entry[[lang]]
      if (is.null(val) || is.na(val) || val == "") {
        return(entry[["en"]])           # fallback to English
      }
      val
    }

    ###############################################################
    ### NEW: Translate a DHS Topic / Chapter string
    ###############################################################
    translate_topic <- function(topic_str, lang = "en") {
      if (is.null(topic_str) || is.na(topic_str)) return(topic_str)
      if (lang == "en") return(topic_str)
      entry <- topic_dict[[topic_str]]
      if (is.null(entry)) return(topic_str)
      val <- entry[[lang]]
      if (is.null(val) || is.na(val) || val == "") return(topic_str)
      val
    }

    ###############################################################
    ### NEW: Per-indicator-ID translation table for Full_definition
    ###############################################################
    # FIX (definition mangling): the old approach was a fixed-string phrase
    # dictionary. It mangled output because short common words ('or', 'the')
    # matched inside longer unrelated words ("before" -> "befoe"/"befoue")
    # and many phrases simply weren't in the dictionary, leaving English
    # fragments. This table provides a complete, hand-translated definition
    # for every indicator ID currently in surveyPrev::indicatorList (n=180)
    # in fr/es/pt. translate_definition() now looks up the ID first and
    # only falls back to the phrase dictionary as a safety net for IDs that
    # may be added upstream later.
    definition_id_translations <- list(
      "CH_SZWT_C_L25" = list(
        fr = "Pourcentage de naissances vivantes au cours des cinq (ou trois) années précédant l'enquête dont le poids à la naissance était inférieur à 2,5 kg",
        es = "Porcentaje de nacidos vivos en los cinco (o tres) años anteriores a la encuesta cuyo peso al nacer fue inferior a 2,5 kg",
        pt = "Percentual de nascidos vivos nos cinco (ou três) anos anteriores à pesquisa cujo peso ao nascer foi inferior a 2,5 kg"
      ),
      "CH_DIAT_C_ORT" = list(
        fr = "Pourcentage d'enfants nés au cours des cinq (ou trois) années précédant l'enquête, atteints de diarrhée au cours des deux semaines précédant l'enquête, qui ont reçu soit une solution de réhydratation orale (SRO), soit des liquides maison recommandés (LMR)",
        es = "Porcentaje de niños nacidos en los cinco (o tres) años anteriores a la encuesta, con diarrea en las dos semanas anteriores a la encuesta, que recibieron solución de rehidratación oral (SRO) o líquidos caseros recomendados (LCR)",
        pt = "Percentual de crianças nascidas nos cinco (ou três) anos anteriores à pesquisa, com diarreia nas duas semanas anteriores à pesquisa, que receberam solução de reidratação oral (SRO) ou líquidos caseiros recomendados (LCR)"
      ),
      "CH_DIAT_C_ABI" = list(
        fr = "Pourcentage d'enfants nés au cours des cinq (ou trois) années précédant l'enquête, atteints de diarrhée au cours des deux semaines précédant l'enquête, qui ont reçu des antibiotiques",
        es = "Porcentaje de niños nacidos en los cinco (o tres) años anteriores a la encuesta, con diarrea en las dos semanas anteriores a la encuesta, que recibieron antibióticos",
        pt = "Percentual de crianças nascidas nos cinco (ou três) anos anteriores à pesquisa, com diarreia nas duas semanas anteriores à pesquisa, que receberam antibióticos"
      ),
      "CH_DIAT_C_ADV" = list(
        fr = "Pourcentage d'enfants nés au cours des cinq (ou trois) années précédant l'enquête, atteints de diarrhée au cours des deux semaines précédant l'enquête, pour lesquels des conseils ou un traitement ont été recherchés",
        es = "Porcentaje de niños nacidos en los cinco (o tres) años anteriores a la encuesta, con diarrea en las dos semanas anteriores a la encuesta, para quienes se buscó consejo o tratamiento",
        pt = "Percentual de crianças nascidas nos cinco (ou três) anos anteriores à pesquisa, com diarreia nas duas semanas anteriores à pesquisa, para as quais se buscou aconselhamento ou tratamento"
      ),
      "CH_DIAT_C_AMO" = list(
        fr = "Pourcentage d'enfants nés au cours des cinq (ou trois) années précédant l'enquête, atteints de diarrhée au cours des deux semaines précédant l'enquête, qui ont reçu des antidiarrhéiques (antimotilité)",
        es = "Porcentaje de niños nacidos en los cinco (o tres) años anteriores a la encuesta, con diarrea en las dos semanas anteriores a la encuesta, que recibieron medicamentos antimotilidad",
        pt = "Percentual de crianças nascidas nos cinco (ou três) anos anteriores à pesquisa, com diarreia nas duas semanas anteriores à pesquisa, que receberam medicamentos antimotilidade"
      ),
      "CH_ARIS_C_ADV" = list(
        fr = "Pourcentage d'enfants nés au cours des trois/cinq années précédant l'enquête, présentant des symptômes d'infection respiratoire aiguë au cours des deux semaines précédant l'enquête, pour lesquels des conseils ou un traitement ont été recherchés",
        es = "Porcentaje de niños nacidos en los tres/cinco años anteriores a la encuesta, con síntomas de infección respiratoria aguda en las dos semanas anteriores a la encuesta, para quienes se buscó consejo o tratamiento",
        pt = "Percentual de crianças nascidas nos três/cinco anos anteriores à pesquisa, com sintomas de infecção respiratória aguda nas duas semanas anteriores à pesquisa, para as quais se buscou aconselhamento ou tratamento"
      ),
      "CH_DIAT_C_ORS" = list(
        fr = "Pourcentage d'enfants nés au cours des cinq (ou trois) années précédant l'enquête, atteints de diarrhée au cours des deux semaines précédant l'enquête, qui ont reçu une solution de réhydratation orale (SRO), c'est-à-dire un liquide provenant d'un sachet de SRO ou un liquide SRO pré-conditionné",
        es = "Porcentaje de niños nacidos en los cinco (o tres) años anteriores a la encuesta, con diarrea en las dos semanas anteriores a la encuesta, que recibieron solución de rehidratación oral (SRO), es decir, líquido de un sobre de SRO o un líquido SRO pre-envasado",
        pt = "Percentual de crianças nascidas nos cinco (ou três) anos anteriores à pesquisa, com diarreia nas duas semanas anteriores à pesquisa, que receberam solução de reidratação oral (SRO), ou seja, líquido de um envelope de SRO ou um líquido SRO pré-embalado"
      ),
      "CH_DIAT_C_OSI" = list(
        fr = "Pourcentage d'enfants nés au cours des cinq (ou trois) années précédant l'enquête, atteints de diarrhée au cours des deux semaines précédant l'enquête, qui ont reçu une SRO ou une augmentation des liquides. La SRO comprend le liquide préparé à partir de sachets de sels de réhydratation orale (SRO) ou de SRO pré-conditionnée.",
        es = "Porcentaje de niños nacidos en los cinco (o tres) años anteriores a la encuesta, con diarrea en las dos semanas anteriores a la encuesta, que recibieron SRO o un aumento de líquidos. La SRO incluye líquido preparado a partir de sobres de sales de rehidratación oral (SRO) o SRO pre-envasada.",
        pt = "Percentual de crianças nascidas nos cinco (ou três) anos anteriores à pesquisa, com diarreia nas duas semanas anteriores à pesquisa, que receberam SRO ou aumento de líquidos. A SRO inclui líquido preparado a partir de envelopes de sais de reidratação oral (SRO) ou SRO pré-embalada."
      ),
      "CH_DIAT_C_RHF" = list(
        fr = "Pourcentage d'enfants nés au cours des cinq (ou trois) années précédant l'enquête, atteints de diarrhée au cours des deux semaines précédant l'enquête, qui ont reçu des liquides maison recommandés (LMR) à la maison",
        es = "Porcentaje de niños nacidos en los cinco (o tres) años anteriores a la encuesta, con diarrea en las dos semanas anteriores a la encuesta, que recibieron líquidos caseros recomendados (LCR) en el hogar",
        pt = "Percentual de crianças nascidas nos cinco (ou três) anos anteriores à pesquisa, com diarreia nas duas semanas anteriores à pesquisa, que receberam líquidos caseiros recomendados (LCR) em casa"
      ),
      "CH_DIAT_C_NOT" = list(
        fr = "Pourcentage d'enfants nés au cours des cinq (ou trois) années précédant l'enquête, atteints de diarrhée au cours des deux semaines précédant l'enquête, qui n'ont reçu aucun traitement",
        es = "Porcentaje de niños nacidos en los cinco (o tres) años anteriores a la encuesta, con diarrea en las dos semanas anteriores a la encuesta, que no recibieron tratamiento",
        pt = "Percentual de crianças nascidas nos cinco (ou três) anos anteriores à pesquisa, com diarreia nas duas semanas anteriores à pesquisa, que não receberam tratamento"
      ),
      "CH_FEVR_C_FEV" = list(
        fr = "Pourcentage d'enfants nés au cours des cinq (ou trois) années précédant l'enquête qui ont eu de la fièvre au cours des deux semaines précédant l'enquête",
        es = "Porcentaje de niños nacidos en los cinco (o tres) años anteriores a la encuesta que tuvieron fiebre durante las dos semanas anteriores a la encuesta",
        pt = "Percentual de crianças nascidas nos cinco (ou três) anos anteriores à pesquisa que tiveram febre durante as duas semanas anteriores à pesquisa"
      ),
      "CH_FEVT_C_ADV" = list(
        fr = "Pourcentage d'enfants nés au cours des trois/cinq années précédant l'enquête ayant eu de la fièvre au cours des deux semaines précédant l'enquête, pour lesquels des conseils ou un traitement ont été recherchés",
        es = "Porcentaje de niños nacidos en los tres/cinco años anteriores a la encuesta con fiebre en las dos semanas anteriores a la encuesta, para quienes se buscó consejo o tratamiento",
        pt = "Percentual de crianças nascidas nos três/cinco anos anteriores à pesquisa com febre nas duas semanas anteriores à pesquisa, para as quais se buscou aconselhamento ou tratamento"
      ),
      "CH_VACC_C_BAS" = list(
        fr = "Pourcentage d'enfants âgés de 12-23 (ou 24-35) mois entièrement vaccinés avec les 8 antigènes de base (BCG, Polio 1-3, DTC 1-3, rougeole)",
        es = "Porcentaje de niños de 12-23 (o 24-35) meses completamente vacunados con los 8 antígenos básicos (BCG, Polio 1-3, DPT 1-3, sarampión)",
        pt = "Percentual de crianças de 12-23 (ou 24-35) meses totalmente vacinadas com os 8 antígenos básicos (BCG, Pólio 1-3, DPT 1-3, sarampo)"
      ),
      "CH_VACC_C_BCG" = list(
        fr = "Pourcentage d'enfants âgés de 12-23 mois ayant reçu le vaccin BCG",
        es = "Porcentaje de niños de 12-23 meses que recibieron la vacuna BCG",
        pt = "Percentual de crianças de 12-23 meses que receberam a vacina BCG"
      ),
      "CH_VACC_C_PN3" = list(
        fr = "Pourcentage d'enfants âgés de 12-23 mois ayant reçu la 3e dose du vaccin antipneumococcique",
        es = "Porcentaje de niños de 12-23 meses que recibieron la 3ª dosis de la vacuna antineumocócica",
        pt = "Percentual de crianças de 12-23 meses que receberam a 3ª dose da vacina pneumocócica"
      ),
      "CH_VACC_C_MSL" = list(
        fr = "Pourcentage d'enfants âgés de 12-23 mois ayant reçu le vaccin contre la rougeole",
        es = "Porcentaje de niños de 12-23 meses que recibieron la vacuna contra el sarampión",
        pt = "Percentual de crianças de 12-23 meses que receberam a vacina contra o sarampo"
      ),
      "CH_VACC_C_NON" = list(
        fr = "Pourcentage d'enfants âgés de 12-23 mois n'ayant reçu aucun vaccin",
        es = "Porcentaje de niños de 12-23 meses que no recibieron ninguna vacuna",
        pt = "Percentual de crianças de 12-23 meses que não receberam nenhuma vacina"
      ),
      "CH_VACC_C_DP1" = list(
        fr = "Pourcentage d'enfants âgés de 12-23 mois ayant reçu la 1re dose du vaccin DTC",
        es = "Porcentaje de niños de 12-23 meses que recibieron la 1ª dosis de la vacuna DPT",
        pt = "Percentual de crianças de 12-23 meses que receberam a 1ª dose da vacina DPT"
      ),
      "CH_VACC_C_DP3" = list(
        fr = "Pourcentage d'enfants âgés de 12-23 mois ayant reçu la 3e dose du vaccin DTC",
        es = "Porcentaje de niños de 12-23 meses que recibieron la 3ª dosis de la vacuna DPT",
        pt = "Percentual de crianças de 12-23 meses que receberam a 3ª dose da vacina DPT"
      ),
      "CH_DIFP_C_FAL" = list(
        fr = "Pourcentage d'enfants nés au cours des cinq (ou trois) années précédant l'enquête ayant eu de la diarrhée au cours des deux semaines précédant l'enquête, qui ont reçu la TRO et continué à être alimentés. L'alimentation continue inclut les enfants ayant reçu plus de nourriture, la même quantité que d'habitude, ou un peu moins, pendant l'épisode de diarrhée.",
        es = "Porcentaje de niños nacidos en los cinco (o tres) años anteriores a la encuesta con diarrea en las dos semanas anteriores a la encuesta que recibieron TRO y continuaron alimentándose. La alimentación continuada incluye a los niños que recibieron más alimentos, la misma cantidad que de costumbre o un poco menos durante el episodio de diarrea.",
        pt = "Percentual de crianças nascidas nos cinco (ou três) anos anteriores à pesquisa com diarreia nas duas semanas anteriores à pesquisa que receberam TRO e continuaram a alimentar-se. A alimentação contínua inclui crianças que receberam mais alimentos, a mesma quantidade habitual ou um pouco menos durante o episódio de diarreia."
      ),
      "CM_ECMR_C_NNF" = list(
        fr = "Probabilité de décéder au cours du premier mois de vie, au cours des cinq ou dix années précédant l'enquête, pour 1 000 naissances vivantes. Les estimations sont présentées pour des périodes de dix ans pour toutes les caractéristiques, mais pour des périodes de cinq ans uniquement pour le total national, par lieu de résidence et par sexe.",
        es = "Probabilidad de morir durante el primer mes de vida, en los cinco o diez años anteriores a la encuesta, por 1.000 nacidos vivos. Las estimaciones se presentan para períodos de diez años para todas las características, pero para períodos de cinco años solo para el total nacional, por lugar de residencia y por sexo.",
        pt = "Probabilidade de morrer durante o primeiro mês de vida, nos cinco ou dez anos anteriores à pesquisa, por 1.000 nascidos vivos. As estimativas são apresentadas para períodos de dez anos para todas as características, mas para períodos de cinco anos apenas para o total nacional, por local de residência e por sexo."
      ),
      "CM_ECMR_C_NNR" = list(
        fr = "Probabilité de décéder au cours du premier mois de vie, au cours des cinq ou dix années précédant l'enquête, pour 1 000 naissances vivantes. Les estimations sont présentées pour des périodes de dix ans pour toutes les caractéristiques, mais pour des périodes de cinq ans uniquement pour le total national, par lieu de résidence et par sexe.",
        es = "Probabilidad de morir durante el primer mes de vida, en los cinco o diez años anteriores a la encuesta, por 1.000 nacidos vivos. Las estimaciones se presentan para períodos de diez años para todas las características, pero para períodos de cinco años solo para el total nacional, por lugar de residencia y por sexo.",
        pt = "Probabilidade de morrer durante o primeiro mês de vida, nos cinco ou dez anos anteriores à pesquisa, por 1.000 nascidos vivos. As estimativas são apresentadas para períodos de dez anos para todas as características, mas para períodos de cinco anos apenas para o total nacional, por local de residência e por sexo."
      ),
      "DV_PCPV_W_CHD" = list(
        fr = "Pourcentage de femmes ayant subi des violences physiques de la part de leur fille/fils",
        es = "Porcentaje de mujeres que sufrieron violencia física por parte de su hija/hijo",
        pt = "Percentual de mulheres que sofreram violência física por parte de sua filha/filho"
      ),
      "DV_EXPV_W_12M" = list(
        fr = "Pourcentage de femmes ayant subi des violences physiques au cours des 12 derniers mois, souvent ou parfois",
        es = "Porcentaje de mujeres que sufrieron violencia física en los últimos 12 meses, a menudo o algunas veces",
        pt = "Percentual de mulheres que sofreram violência física nos últimos 12 meses, frequentemente ou às vezes"
      ),
      "DV_AFSV_W_A12" = list(
        fr = "Pourcentage de femmes ayant subi des violences sexuelles avant l'âge exact de 12 ans",
        es = "Porcentaje de mujeres que sufrieron violencia sexual antes de la edad exacta de 12 años",
        pt = "Percentual de mulheres que sofreram violência sexual antes da idade exata de 12 anos"
      ),
      "DV_PCPV_W_CBF" = list(
        fr = "Pourcentage de femmes ayant subi des violences physiques de la part de leur petit ami actuel",
        es = "Porcentaje de mujeres que sufrieron violencia física por parte de su pareja (novio) actual",
        pt = "Percentual de mulheres que sofreram violência física por parte de seu namorado atual"
      ),
      "DV_EXSV_W_12M" = list(
        fr = "Pourcentage de femmes ayant subi des violences sexuelles au cours des 12 derniers mois",
        es = "Porcentaje de mujeres que sufrieron violencia sexual en los últimos 12 meses",
        pt = "Percentual de mulheres que sofreram violência sexual nos últimos 12 meses"
      ),
      "DV_EXSV_W_EVR" = list(
        fr = "Pourcentage de femmes ayant déjà subi des violences sexuelles",
        es = "Porcentaje de mujeres que alguna vez sufrieron violencia sexual",
        pt = "Percentual de mulheres que alguma vez sofreram violência sexual"
      ),
      "DV_PCPV_W_FLW" = list(
        fr = "Pourcentage de femmes ayant subi des violences physiques de la part de leur beau-père",
        es = "Porcentaje de mujeres que sufrieron violencia física por parte de su suegro",
        pt = "Percentual de mulheres que sofreram violência física por parte de seu sogro"
      ),
      "DV_PCPV_W_OLW" = list(
        fr = "Pourcentage de femmes ayant subi des violences physiques de la part d'un autre membre de la belle-famille",
        es = "Porcentaje de mujeres que sufrieron violencia física por parte de otro pariente político",
        pt = "Percentual de mulheres que sofreram violência física por parte de outro parente por afinidade"
      ),
      "DV_STPS_W_BFR" = list(
        fr = "Pourcentage de femmes ayant subi des violences physiques ou sexuelles et ayant cherché de l'aide pour mettre fin aux violences exercées par un petit ami actuel/ancien",
        es = "Porcentaje de mujeres que sufrieron violencia física o sexual y buscaron ayuda para detener la violencia ejercida por una pareja (novio) actual/anterior",
        pt = "Percentual de mulheres que sofreram violência física ou sexual e procuraram ajuda para fazer cessar a violência cometida por namorado atual/anterior"
      ),
      "DV_PCPV_W_EMP" = list(
        fr = "Pourcentage de femmes ayant subi des violences physiques de la part de leur employeur/d'une personne sur le lieu de travail",
        es = "Porcentaje de mujeres que sufrieron violencia física por parte de su empleador o alguien en el lugar de trabajo",
        pt = "Percentual de mulheres que sofreram violência física por parte de seu empregador ou alguém no local de trabalho"
      ),
      "DV_AFSV_W_A10" = list(
        fr = "Pourcentage de femmes ayant subi des violences sexuelles avant l'âge exact de 10 ans",
        es = "Porcentaje de mujeres que sufrieron violencia sexual antes de la edad exacta de 10 años",
        pt = "Percentual de mulheres que sofreram violência sexual antes da idade exata de 10 anos"
      ),
      "DV_VPRG_W_VPG" = list(
        fr = "Pourcentage de femmes ayant subi des violences pendant la grossesse",
        es = "Porcentaje de mujeres que sufrieron violencia durante el embarazo",
        pt = "Percentual de mulheres que sofreram violência durante a gravidez"
      ),
      "PR_DESL_W_WNM" = list(
        fr = "Pourcentage de femmes actuellement mariées ou en union qui ne souhaitent plus d'enfants ou qui sont stérilisées",
        es = "Porcentaje de mujeres actualmente casadas o en unión que no desean más hijos o están esterilizadas",
        pt = "Percentual de mulheres atualmente casadas ou em união que não desejam mais filhos ou que estão esterilizadas"
      ),
      "FG_PFCC_W_WCC" = list(
        fr = "Pourcentage de femmes excisées (femmes ayant subi des mutilations génitales féminines, MGF)",
        es = "Porcentaje de mujeres circuncidadas (mujeres que han sufrido mutilación genital femenina, MGF)",
        pt = "Percentual de mulheres circuncidadas (mulheres que sofreram mutilação genital feminina, MGF)"
      ),
      "FG_KFCC_W_HFC" = list(
        fr = "Pourcentage de femmes ayant déjà entendu parler de l'excision féminine",
        es = "Porcentaje de mujeres que han oído hablar alguna vez de la circuncisión femenina",
        pt = "Percentual de mulheres que alguma vez ouviram falar da circuncisão feminina"
      ),
      "FP_CUSA_W_MOD" = list(
        fr = "Pourcentage de femmes utilisant actuellement n'importe quelle méthode moderne de contraception",
        es = "Porcentaje de mujeres que actualmente usan cualquier método moderno de anticoncepción",
        pt = "Percentual de mulheres que atualmente usam qualquer método moderno de contracepção"
      ),
      "FP_KMTA_W_IMP" = list(
        fr = "Pourcentage de femmes qui connaissent les implants",
        es = "Porcentaje de mujeres que conocen los implantes",
        pt = "Percentual de mulheres que conhecem os implantes"
      ),
      "FP_KMTA_W_INJ" = list(
        fr = "Pourcentage de femmes qui connaissent les injections",
        es = "Porcentaje de mujeres que conocen las inyecciones",
        pt = "Percentual de mulheres que conhecem as injeções"
      ),
      "FP_KMTA_W_IUD" = list(
        fr = "Pourcentage de femmes qui connaissent le DIU (stérilet)",
        es = "Porcentaje de mujeres que conocen el DIU",
        pt = "Percentual de mulheres que conhecem o DIU"
      ),
      "FP_NADA_W_UNT" = list(
        fr = "Pourcentage de femmes ayant un besoin non satisfait en matière de planification familiale",
        es = "Porcentaje de mujeres con una necesidad insatisfecha de planificación familiar",
        pt = "Percentual de mulheres com necessidade não atendida de planejamento familiar"
      ),
      "FP_EFPM_M_TLV" = list(
        fr = "Pourcentage d'hommes selon qu'ils ont reçu un message sur la planification familiale à la télévision au cours des derniers mois précédant l'entretien",
        es = "Porcentaje de hombres según si recibieron un mensaje sobre planificación familiar por televisión en los últimos meses antes de la entrevista",
        pt = "Percentual de homens segundo se receberam uma mensagem sobre planejamento familiar pela televisão nos últimos meses antes da entrevista"
      ),
      "FP_EFPM_W_NWS" = list(
        fr = "Pourcentage de femmes selon qu'elles ont reçu un message sur la planification familiale dans un journal ou un magazine au cours des derniers mois précédant l'entretien",
        es = "Porcentaje de mujeres según si recibieron un mensaje sobre planificación familiar en un periódico o revista en los últimos meses antes de la entrevista",
        pt = "Percentual de mulheres segundo se receberam uma mensagem sobre planejamento familiar em um jornal ou revista nos últimos meses antes da entrevista"
      ),
      "FP_EFPM_W_RDO" = list(
        fr = "Pourcentage de femmes selon qu'elles ont reçu un message sur la planification familiale à la radio au cours des derniers mois précédant l'entretien",
        es = "Porcentaje de mujeres según si recibieron un mensaje sobre planificación familiar por radio en los últimos meses antes de la entrevista",
        pt = "Percentual de mulheres segundo se receberam uma mensagem sobre planejamento familiar pelo rádio nos últimos meses antes da entrevista"
      ),
      "FP_EFPM_W_TLV" = list(
        fr = "Pourcentage de femmes selon qu'elles ont reçu un message sur la planification familiale à la télévision au cours des derniers mois précédant l'entretien",
        es = "Porcentaje de mujeres según si recibieron un mensaje sobre planificación familiar por televisión en los últimos meses antes de la entrevista",
        pt = "Percentual de mulheres segundo se receberam uma mensagem sobre planejamento familiar pela televisão nos últimos meses antes da entrevista"
      ),
      "FP_EVUM_W_MOD" = list(
        fr = "Pourcentage de femmes actuellement mariées ou en union ayant déjà utilisé n'importe quelle méthode moderne de contraception",
        es = "Porcentaje de mujeres actualmente casadas o en unión que alguna vez usaron cualquier método moderno de anticoncepción",
        pt = "Percentual de mulheres atualmente casadas ou em união que alguma vez usaram qualquer método moderno de contracepção"
      ),
      "FP_KMTA_W_ANY" = list(
        fr = "Pourcentage de femmes qui connaissent n'importe quelle méthode de contraception",
        es = "Porcentaje de mujeres que conocen cualquier método de anticoncepción",
        pt = "Percentual de mulheres que conhecem qualquer método de contracepção"
      ),
      "FP_KMTA_W_EMC" = list(
        fr = "Pourcentage de femmes qui connaissent la contraception d'urgence",
        es = "Porcentaje de mujeres que conocen la anticoncepción de emergencia",
        pt = "Percentual de mulheres que conhecem a contracepção de emergência"
      ),
      "FP_KMTA_W_FCN" = list(
        fr = "Pourcentage de femmes qui connaissent le préservatif féminin",
        es = "Porcentaje de mujeres que conocen el condón femenino",
        pt = "Percentual de mulheres que conhecem o preservativo feminino"
      ),
      "FP_KMTA_W_FST" = list(
        fr = "Pourcentage de femmes qui connaissent la stérilisation féminine",
        es = "Porcentaje de mujeres que conocen la esterilización femenina",
        pt = "Percentual de mulheres que conhecem a esterilização feminina"
      ),
      "FP_CUSA_W_STD" = list(
        fr = "Pourcentage de femmes utilisant actuellement la méthode des jours fixes",
        es = "Porcentaje de mujeres que actualmente usan el método de días estándar",
        pt = "Percentual de mulheres que atualmente usam o método dos dias fixos"
      ),
      "FP_CUSA_W_TRA" = list(
        fr = "Pourcentage de femmes utilisant actuellement n'importe quelle méthode traditionnelle",
        es = "Porcentaje de mujeres que actualmente usan cualquier método tradicional",
        pt = "Percentual de mulheres que atualmente usam qualquer método tradicional"
      ),
      "FP_CUSA_W_WTH" = list(
        fr = "Pourcentage de femmes utilisant actuellement le retrait",
        es = "Porcentaje de mujeres que actualmente usan el retiro (coitus interruptus)",
        pt = "Percentual de mulheres que atualmente usam o coito interrompido"
      ),
      "FP_CUSM_W_MOD" = list(
        fr = "Pourcentage de femmes actuellement mariées ou en union utilisant actuellement n'importe quelle méthode moderne de contraception",
        es = "Porcentaje de mujeres actualmente casadas o en unión que actualmente usan cualquier método moderno de anticoncepción",
        pt = "Percentual de mulheres atualmente casadas ou em união que atualmente usam qualquer método moderno de contracepção"
      ),
      "FP_EFPM_M_NWS" = list(
        fr = "Pourcentage d'hommes selon qu'ils ont reçu un message sur la planification familiale dans un journal ou un magazine au cours des derniers mois précédant l'entretien",
        es = "Porcentaje de hombres según si recibieron un mensaje sobre planificación familiar en un periódico o revista en los últimos meses antes de la entrevista",
        pt = "Percentual de homens segundo se receberam uma mensagem sobre planejamento familiar em um jornal ou revista nos últimos meses antes da entrevista"
      ),
      "FP_KMTA_W_OMD" = list(
        fr = "Pourcentage de femmes qui connaissent d'autres méthodes modernes",
        es = "Porcentaje de mujeres que conocen otros métodos modernos",
        pt = "Percentual de mulheres que conhecem outros métodos modernos"
      ),
      "FP_KMTA_W_PIL" = list(
        fr = "Pourcentage de femmes qui connaissent la pilule",
        es = "Porcentaje de mujeres que conocen la píldora",
        pt = "Percentual de mulheres que conhecem a pílula"
      ),
      "FP_KMTA_W_STD" = list(
        fr = "Pourcentage de femmes qui connaissent la méthode des jours fixes",
        es = "Porcentaje de mujeres que conocen el método de días estándar",
        pt = "Percentual de mulheres que conhecem o método dos dias fixos"
      ),
      "FP_KMTA_W_TRA" = list(
        fr = "Pourcentage de femmes qui connaissent n'importe quelle méthode traditionnelle",
        es = "Porcentaje de mujeres que conocen cualquier método tradicional",
        pt = "Percentual de mulheres que conhecem qualquer método tradicional"
      ),
      "FP_KMTA_W_WTH" = list(
        fr = "Pourcentage de femmes qui connaissent le retrait",
        es = "Porcentaje de mujeres que conocen el retiro (coitus interruptus)",
        pt = "Percentual de mulheres que conhecem o coito interrompido"
      ),
      "FP_NADM_W_UNT" = list(
        fr = "Pourcentage de femmes actuellement mariées ou en union ayant un besoin non satisfait en matière de planification familiale",
        es = "Porcentaje de mujeres actualmente casadas o en unión con una necesidad insatisfecha de planificación familiar",
        pt = "Percentual de mulheres atualmente casadas ou em união com necessidade não atendida de planejamento familiar"
      ),
      "FP_CUSA_W_MCN" = list(
        fr = "Pourcentage de femmes utilisant actuellement le préservatif",
        es = "Porcentaje de mujeres que actualmente usan el condón",
        pt = "Percentual de mulheres que atualmente usam o preservativo"
      ),
      "FP_CUSA_W_MST" = list(
        fr = "Pourcentage de femmes utilisant actuellement la stérilisation masculine",
        es = "Porcentaje de mujeres que actualmente usan la esterilización masculina",
        pt = "Percentual de mulheres que atualmente usam a esterilização masculina"
      ),
      "FP_CUSA_W_PIL" = list(
        fr = "Pourcentage de femmes utilisant actuellement la pilule",
        es = "Porcentaje de mujeres que actualmente usan la píldora",
        pt = "Percentual de mulheres que atualmente usam a pílula"
      ),
      "FP_CUSA_W_EMC" = list(
        fr = "Pourcentage de femmes utilisant actuellement la contraception d'urgence",
        es = "Porcentaje de mujeres que actualmente usan la anticoncepción de emergencia",
        pt = "Percentual de mulheres que atualmente usam a contracepção de emergência"
      ),
      "FP_CUSA_W_FCN" = list(
        fr = "Pourcentage de femmes utilisant actuellement le préservatif féminin",
        es = "Porcentaje de mujeres que actualmente usan el condón femenino",
        pt = "Percentual de mulheres que atualmente usam o preservativo feminino"
      ),
      "FP_CUSA_W_FST" = list(
        fr = "Pourcentage de femmes utilisant actuellement la stérilisation féminine",
        es = "Porcentaje de mujeres que actualmente usan la esterilización femenina",
        pt = "Percentual de mulheres que atualmente usam a esterilização feminina"
      ),
      "FP_KMTA_W_MST" = list(
        fr = "Pourcentage de femmes qui connaissent la stérilisation masculine",
        es = "Porcentaje de mujeres que conocen la esterilización masculina",
        pt = "Percentual de mulheres que conhecem a esterilização masculina"
      ),
      "FP_CUSA_W_INJ" = list(
        fr = "Pourcentage de femmes utilisant actuellement les injections",
        es = "Porcentaje de mujeres que actualmente usan las inyecciones",
        pt = "Percentual de mulheres que atualmente usam as injeções"
      ),
      "FP_CUSA_W_IUD" = list(
        fr = "Pourcentage de femmes utilisant actuellement le DIU (stérilet)",
        es = "Porcentaje de mujeres que actualmente usan el DIU",
        pt = "Percentual de mulheres que atualmente usam o DIU"
      ),
      "FP_CUSA_W_IMP" = list(
        fr = "Pourcentage de femmes utilisant actuellement les implants",
        es = "Porcentaje de mujeres que actualmente usan los implantes",
        pt = "Percentual de mulheres que atualmente usam os implantes"
      ),
      "FP_CUSA_W_ANY" = list(
        fr = "Pourcentage de femmes utilisant actuellement n'importe quelle méthode de contraception",
        es = "Porcentaje de mujeres que actualmente usan cualquier método de anticoncepción",
        pt = "Percentual de mulheres que atualmente usam qualquer método de contracepção"
      ),
      "HA_STIS_M_DIS" = list(
        fr = "Pourcentage d'hommes ayant déclaré un écoulement génital nauséabond ou anormal au cours des 12 mois précédant l'enquête, parmi les hommes ayant déjà eu des rapports sexuels",
        es = "Porcentaje de hombres que reportaron una secreción genital maloliente o anormal en los 12 meses anteriores a la encuesta, entre los hombres que alguna vez tuvieron relaciones sexuales",
        pt = "Percentual de homens que relataram um corrimento genital com mau cheiro ou anormal nos 12 meses anteriores à pesquisa, entre os homens que alguma vez tiveram relações sexuais"
      ),
      "HA_HVTY_W_TRR" = list(
        fr = "Pourcentage de jeunes femmes sexuellement actives âgées de 15-24 ans, ayant eu des rapports sexuels au cours des 12 mois précédant l'enquête, qui ont fait un test de dépistage du VIH au cours des 12 mois précédant l'enquête et connaissent leur résultat",
        es = "Porcentaje de jóvenes mujeres sexualmente activas de 15-24 años, que tuvieron relaciones sexuales en los 12 meses anteriores a la encuesta, que se hicieron una prueba de VIH en los 12 meses anteriores a la encuesta y conocen el resultado",
        pt = "Percentual de jovens mulheres sexualmente ativas de 15-24 anos, que tiveram relações sexuais nos 12 meses anteriores à pesquisa, que fizeram um teste de HIV nos 12 meses anteriores à pesquisa e conhecem o resultado"
      ),
      "HA_KAID_W_HRD" = list(
        fr = "Pourcentage de femmes ayant entendu parler du VIH ou du SIDA",
        es = "Porcentaje de mujeres que han oído hablar del VIH o el SIDA",
        pt = "Percentual de mulheres que ouviram falar do HIV ou da AIDS"
      ),
      "HA_AFSY_M_A15" = list(
        fr = "Pourcentage de jeunes hommes âgés de 15-24 ans qui ont eu leur premier rapport sexuel avant l'âge de 15 ans.",
        es = "Porcentaje de jóvenes hombres de 15-24 años que tuvieron su primera relación sexual antes de los 15 años.",
        pt = "Percentual de jovens homens de 15-24 anos que tiveram sua primeira relação sexual antes dos 15 anos."
      ),
      "HA_HVTY_M_TRR" = list(
        fr = "Pourcentage de jeunes hommes sexuellement actifs âgés de 15-24 ans, ayant eu des rapports sexuels au cours des 12 mois précédant l'enquête, qui ont fait un test de dépistage du VIH au cours des 12 mois précédant l'enquête et connaissent leur résultat",
        es = "Porcentaje de jóvenes hombres sexualmente activos de 15-24 años, que tuvieron relaciones sexuales en los 12 meses anteriores a la encuesta, que se hicieron una prueba de VIH en los 12 meses anteriores a la encuesta y conocen el resultado",
        pt = "Percentual de jovens homens sexualmente ativos de 15-24 anos, que tiveram relações sexuais nos 12 meses anteriores à pesquisa, que fizeram um teste de HIV nos 12 meses anteriores à pesquisa e conhecem o resultado"
      ),
      "HA_STIS_W_STI" = list(
        fr = "Pourcentage de femmes ayant déclaré une infection sexuellement transmissible au cours des 12 mois précédant l'enquête, parmi les femmes ayant déjà eu des rapports sexuels",
        es = "Porcentaje de mujeres que reportaron una infección de transmisión sexual en los 12 meses anteriores a la encuesta, entre las mujeres que alguna vez tuvieron relaciones sexuales",
        pt = "Percentual de mulheres que relataram uma infecção sexualmente transmissível nos 12 meses anteriores à pesquisa, entre as mulheres que alguma vez tiveram relações sexuais"
      ),
      "HA_AFSY_W_A18" = list(
        fr = "Pourcentage de jeunes femmes âgées de 18-24 ans qui ont eu leur premier rapport sexuel avant l'âge de 18 ans.",
        es = "Porcentaje de jóvenes mujeres de 18-24 años que tuvieron su primera relación sexual antes de los 18 años.",
        pt = "Percentual de jovens mulheres de 18-24 anos que tiveram sua primeira relação sexual antes dos 18 anos."
      ),
      "HA_CATH_W_NRS" = list(
        fr = "Pourcentage de femmes ayant été testées pour le VIH, mais qui n'ont pas reçu le résultat lors de la visite prénatale pour la naissance la plus récente, parmi toutes les femmes ayant accouché au cours des deux années précédant l'enquête",
        es = "Porcentaje de mujeres que se hicieron la prueba de VIH, pero no recibieron el resultado durante la visita prenatal del último parto, entre todas las mujeres que dieron a luz en los dos años anteriores a la encuesta",
        pt = "Percentual de mulheres testadas para HIV, mas que não receberam o resultado durante a consulta pré-natal do último parto, entre todas as mulheres que deram à luz nos dois anos anteriores à pesquisa"
      ),
      "HA_STIS_M_SOR" = list(
        fr = "Pourcentage d'hommes ayant déclaré une plaie ou un ulcère génital au cours des 12 mois précédant l'enquête, parmi les hommes ayant déjà eu des rapports sexuels",
        es = "Porcentaje de hombres que reportaron una llaga o úlcera genital en los 12 meses anteriores a la encuesta, entre los hombres que alguna vez tuvieron relaciones sexuales",
        pt = "Percentual de homens que relataram uma ferida ou úlcera genital nos 12 meses anteriores à pesquisa, entre os homens que alguma vez tiveram relações sexuais"
      ),
      "HA_HVST_M_USD" = list(
        fr = "Pourcentage d'hommes âgés de 15-49 ans ayant déjà utilisé un autotest VIH",
        es = "Porcentaje de hombres de 15-49 años que alguna vez usaron un kit de autoprueba de VIH",
        pt = "Percentual de homens de 15-49 anos que alguma vez usaram um kit de autoteste de HIV"
      ),
      "HA_CATH_W_ATN" = list(
        fr = "Pourcentage de femmes testées pour le VIH lors des soins prénatals ou de l'accouchement pour la naissance la plus récente, mais n'ayant pas reçu les résultats, parmi toutes les femmes ayant accouché au cours des deux années précédant l'enquête",
        es = "Porcentaje de mujeres que se hicieron la prueba de VIH durante la atención prenatal o el parto del último nacimiento, pero no recibieron los resultados, entre todas las mujeres que dieron a luz en los dos años anteriores a la encuesta",
        pt = "Percentual de mulheres testadas para HIV durante o pré-natal ou o parto do último nascimento, mas que não receberam os resultados, entre todas as mulheres que deram à luz nos dois anos anteriores à pesquisa"
      ),
      "HA_HRSX_W_CND" = list(
        fr = "Pourcentage de femmes ayant déclaré avoir utilisé un préservatif lors du dernier rapport sexuel avec un partenaire non marital et non cohabitant, parmi celles ayant eu des rapports sexuels avec un tel partenaire au cours des 12 derniers mois.",
        es = "Porcentaje de mujeres que dijeron haber usado un condón la última vez que tuvieron relaciones sexuales con una pareja no marital y no cohabitante, entre quienes tuvieron relaciones sexuales con tal pareja en los últimos 12 meses.",
        pt = "Percentual de mulheres que disseram ter usado um preservativo na última vez que tiveram relações sexuais com um parceiro não marital e não coabitante, entre as que tiveram relações sexuais com tal parceiro nos últimos 12 meses."
      ),
      "HA_AFSY_M_A18" = list(
        fr = "Pourcentage de jeunes hommes âgés de 18-24 ans qui ont eu leur premier rapport sexuel avant l'âge de 18 ans.",
        es = "Porcentaje de jóvenes hombres de 18-24 años que tuvieron su primera relación sexual antes de los 18 años.",
        pt = "Percentual de jovens homens de 18-24 anos que tiveram sua primeira relação sexual antes dos 18 anos."
      ),
      "HA_HVST_M_HRD" = list(
        fr = "Pourcentage d'hommes âgés de 15-49 ans ayant déjà entendu parler des autotests VIH",
        es = "Porcentaje de hombres de 15-49 años que alguna vez oyeron hablar de los kits de autoprueba de VIH",
        pt = "Percentual de homens de 15-49 anos que alguma vez ouviram falar dos kits de autoteste de HIV"
      ),
      "HA_HIVP_B_HIV" = list(
        fr = "Pourcentage de personnes séropositives au VIH parmi les répondants adultes ayant été testés. Les données sont présentées avec les bornes inférieure et supérieure des intervalles de confiance, indiquant la plage de l'estimation avec une probabilité de 95 %. À partir d'environ 2015, le DHS Program a modifié l'algorithme de dépistage du VIH pour ajouter un test de confirmation à tous les échantillons EIA positifs. Ce changement peut affecter les tendances des estimations de la prévalence du VIH dans certains pays.",
        es = "Porcentaje de personas VIH positivas entre los encuestados adultos que se hicieron la prueba. Los datos se presentan con los límites inferior y superior de los intervalos de confianza, que muestran el rango de la estimación con una probabilidad del 95 %. A partir de aproximadamente 2015, el Programa DHS modificó el algoritmo de prueba de VIH para añadir una prueba confirmatoria a todas las muestras EIA positivas. Este cambio puede afectar las tendencias de las estimaciones de prevalencia de VIH en algunos países.",
        pt = "Percentual de HIV positivos entre os respondentes adultos testados. Os dados são apresentados com os limites inferior e superior dos intervalos de confiança, mostrando a faixa da estimativa com probabilidade de 95%. A partir de cerca de 2015, o DHS Program alterou o algoritmo de teste de HIV para acrescentar um teste confirmatório a todas as amostras EIA positivas. Essa mudança pode afetar as tendências das estimativas de prevalência de HIV em alguns países."
      ),
      "ML_NETP_H_IT2" = list(
        fr = "Pourcentage de ménages possédant au moins une moustiquaire imprégnée d'insecticide (MII) pour deux personnes ayant passé la nuit précédente dans le ménage",
        es = "Porcentaje de hogares con al menos un mosquitero tratado con insecticida (MTI) por cada dos personas que pasaron la noche anterior en el hogar",
        pt = "Percentual de domicílios com pelo menos um mosquiteiro tratado com inseticida (MTI) para cada duas pessoas que passaram a noite anterior no domicílio"
      ),
      "ML_NETC_C_ITN" = list(
        fr = "Pourcentage d'enfants de moins de cinq ans ayant dormi sous une moustiquaire imprégnée d'insecticide (MII) la nuit précédant l'enquête",
        es = "Porcentaje de niños menores de cinco años que durmieron bajo un mosquitero tratado con insecticida (MTI) la noche anterior a la encuesta",
        pt = "Percentual de crianças menores de cinco anos que dormiram sob um mosquiteiro tratado com inseticida (MTI) na noite anterior à pesquisa"
      ),
      "ML_NETU_P_ITN" = list(
        fr = "Pourcentage de la population de fait (de facto) des ménages ayant dormi sous une moustiquaire imprégnée d'insecticide la nuit précédant l'enquête",
        es = "Porcentaje de la población de hecho (de facto) de los hogares que durmió bajo un mosquitero tratado con insecticida la noche anterior a la encuesta",
        pt = "Percentual da população de fato (de facto) dos domicílios que dormiu sob um mosquiteiro tratado com inseticida na noite anterior à pesquisa"
      ),
      "MA_MBAY_W_B18" = list(
        fr = "Pourcentage de jeunes femmes âgées de 20-24 ans qui se sont mariées pour la première fois avant l'âge exact de 18 ans",
        es = "Porcentaje de jóvenes mujeres de 20-24 años que se casaron por primera vez antes de la edad exacta de 18 años",
        pt = "Percentual de jovens mulheres de 20-24 anos que se casaram pela primeira vez antes da idade exata de 18 anos"
      ),
      "MA_MBAY_W_B15" = list(
        fr = "Pourcentage de jeunes femmes âgées de 20-24 ans qui se sont mariées pour la première fois avant l'âge exact de 15 ans",
        es = "Porcentaje de jóvenes mujeres de 20-24 años que se casaron por primera vez antes de la edad exacta de 15 años",
        pt = "Percentual de jovens mulheres de 20-24 anos que se casaram pela primeira vez antes da idade exata de 15 anos"
      ),
      "MA_MSTA_W_NMA" = list(
        fr = "Pourcentage de femmes jamais mariées",
        es = "Porcentaje de mujeres nunca casadas",
        pt = "Percentual de mulheres nunca casadas"
      ),
      "MA_CWIV_W_CWV" = list(
        fr = "Pourcentage de femmes actuellement mariées ou en union dont le mari a une ou plusieurs autres épouses",
        es = "Porcentaje de mujeres actualmente casadas o en unión cuyo esposo tiene una o más esposas",
        pt = "Percentual de mulheres atualmente casadas ou em união cujo marido tem uma ou mais esposas"
      ),
      "AN_ANEM_W_ANY" = list(
        fr = "Pourcentage de femmes classées comme ayant une anémie quelconque (<12,0 g/dl pour les femmes non enceintes et <11,0 g/dl pour les femmes enceintes)",
        es = "Porcentaje de mujeres clasificadas con cualquier tipo de anemia (<12,0 g/dl para mujeres no embarazadas y <11,0 g/dl para mujeres embarazadas)",
        pt = "Percentual de mulheres classificadas com qualquer tipo de anemia (<12,0 g/dl para mulheres não grávidas e <11,0 g/dl para mulheres grávidas)"
      ),
      "AN_NUTS_W_THN" = list(
        fr = "Pourcentage de femmes maigres selon l'IMC (<18,5)",
        es = "Porcentaje de mujeres delgadas según el IMC (<18,5)",
        pt = "Percentual de mulheres com baixo peso segundo o IMC (<18,5)"
      ),
      "CN_NUTS_C_WA3" = list(
        fr = "Pourcentage d'enfants présentant une insuffisance pondérale sévère (inférieure à -3 ET du poids pour l'âge selon la norme OMS)",
        es = "Porcentaje de niños con bajo peso severo (por debajo de -3 DE del peso para la edad según el estándar de la OMS)",
        pt = "Percentual de crianças com baixo peso grave (abaixo de -3 DP de peso para idade segundo o padrão da OMS)"
      ),
      "CN_BRFI_C_EVR" = list(
        fr = "Parmi les derniers-nés au cours des deux années précédant l'enquête, pourcentage ayant déjà été allaités",
        es = "Entre los últimos hijos nacidos en los dos años anteriores a la encuesta, porcentaje que alguna vez fueron amamantados",
        pt = "Entre os últimos filhos nascidos nos dois anos anteriores à pesquisa, percentual que alguma vez foram amamentados"
      ),
      "CN_NUTS_C_HA3" = list(
        fr = "Pourcentage d'enfants présentant un retard de croissance sévère (inférieur à -3 ET de la taille pour l'âge selon la norme OMS)",
        es = "Porcentaje de niños con retraso del crecimiento severo (por debajo de -3 DE de la talla para la edad según el estándar de la OMS)",
        pt = "Percentual de crianças com déficit de crescimento grave (abaixo de -3 DP de altura para idade segundo o padrão da OMS)"
      ),
      "CN_NUTS_C_WA2" = list(
        fr = "Pourcentage d'enfants présentant une insuffisance pondérale (inférieure à -2 ET du poids pour l'âge selon la norme OMS)",
        es = "Porcentaje de niños con bajo peso (por debajo de -2 DE del peso para la edad según el estándar de la OMS)",
        pt = "Percentual de crianças com baixo peso (abaixo de -2 DP de peso para idade segundo o padrão da OMS)"
      ),
      "CN_BRFS_C_EXB" = list(
        fr = "Pourcentage des plus jeunes enfants de moins de deux ans vivant avec leur mère, exclusivement allaités",
        es = "Porcentaje de los niños menores de dos años más jóvenes que viven con la madre y son amamantados exclusivamente",
        pt = "Percentual das crianças mais novas com menos de dois anos que vivem com a mãe e são amamentadas exclusivamente"
      ),
      "CN_NUTS_C_WAP" = list(
        fr = "Pourcentage d'enfants en surpoids pour leur âge (au-dessus de +2 ET du poids pour l'âge selon la norme OMS)",
        es = "Porcentaje de niños con sobrepeso para su edad (por encima de +2 DE del peso para la edad según el estándar de la OMS)",
        pt = "Percentual de crianças com sobrepeso para a idade (acima de +2 DP de peso para idade segundo o padrão da OMS)"
      ),
      "CN_NUTS_C_WH3" = list(
        fr = "Pourcentage d'enfants gravement émaciés (inférieur à -3 ET du poids pour la taille selon la norme OMS)",
        es = "Porcentaje de niños severamente emaciados (por debajo de -3 DE del peso para la talla según el estándar de la OMS)",
        pt = "Percentual de crianças gravemente emaciadas (abaixo de -3 DP de peso para altura segundo o padrão da OMS)"
      ),
      "CN_ANMC_C_ANY" = list(
        fr = "Pourcentage d'enfants de moins de 5 ans classés comme ayant une anémie quelconque",
        es = "Porcentaje de niños menores de 5 años clasificados con cualquier tipo de anemia",
        pt = "Percentual de crianças menores de 5 anos classificadas com qualquer tipo de anemia"
      ),
      "AN_NUTS_W_OVW" = list(
        fr = "Pourcentage de femmes en surpoids selon l'IMC (25,0-29,9)",
        es = "Porcentaje de mujeres con sobrepeso según el IMC (25,0-29,9)",
        pt = "Percentual de mulheres com sobrepeso segundo o IMC (25,0-29,9)"
      ),
      "CN_NUTS_C_HA2" = list(
        fr = "Pourcentage d'enfants présentant un retard de croissance (inférieur à -2 ET de la taille pour l'âge selon la norme OMS)",
        es = "Porcentaje de niños con retraso del crecimiento (por debajo de -2 DE de la talla para la edad según el estándar de la OMS)",
        pt = "Percentual de crianças com déficit de crescimento (abaixo de -2 DP de altura para idade segundo o padrão da OMS)"
      ),
      "CN_NUTS_C_WH2" = list(
        fr = "Pourcentage d'enfants émaciés (inférieur à -2 ET du poids pour la taille selon la norme OMS)",
        es = "Porcentaje de niños emaciados (por debajo de -2 DE del peso para la talla según el estándar de la OMS)",
        pt = "Percentual de crianças emaciadas (abaixo de -2 DP de peso para altura segundo o padrão da OMS)"
      ),
      "CN_NUTS_C_WHP" = list(
        fr = "Pourcentage d'enfants en surpoids (au-dessus de +2 ET du poids pour la taille selon la norme OMS)",
        es = "Porcentaje de niños con sobrepeso (por encima de +2 DE del peso para la talla según el estándar de la OMS)",
        pt = "Percentual de crianças com sobrepeso (acima de +2 DP de peso para altura segundo o padrão da OMS)"
      ),
      "WS_TLET_P_BAS" = list(
        fr = "Pourcentage de la population de droit (de jure) vivant dans des ménages disposant d'un service d'assainissement de base, défini comme des installations sanitaires améliorées qui ne sont pas partagées avec d'autres ménages",
        es = "Porcentaje de la población de derecho (de jure) que vive en hogares con un servicio de saneamiento básico, definido como instalaciones de saneamiento mejoradas que no se comparten con otros hogares",
        pt = "Percentual da população de direito (de jure) que vive em domicílios com serviço de saneamento básico, definido como instalações sanitárias melhoradas que não são compartilhadas com outros domicílios"
      ),
      "WS_SRCE_P_BAS" = list(
        fr = "Pourcentage de la population de droit (de jure) vivant dans des ménages disposant d'un service d'eau de base, défini comme une source d'eau améliorée avec soit de l'eau sur place, soit un temps de collecte aller-retour de 30 minutes ou moins.",
        es = "Porcentaje de la población de derecho (de jure) que vive en hogares con un servicio de agua básico, definido como una fuente de agua mejorada con agua en el predio o un tiempo de recolección ida y vuelta de 30 minutos o menos.",
        pt = "Percentual da população de direito (de jure) que vive em domicílios com serviço básico de água, definido como uma fonte de água melhorada com água na propriedade ou tempo de coleta de ida e volta de 30 minutos ou menos."
      ),
      "HC_AGEG_P_ADL" = list(
        fr = "Pourcentage de la population de fait (de facto) qui sont des adolescents âgés de 10-19 ans",
        es = "Porcentaje de la población de hecho (de facto) que son adolescentes de 10-19 años",
        pt = "Percentual da população de fato (de facto) que são adolescentes de 10-19 anos"
      ),
      "HC_ELEC_H_ELC" = list(
        fr = "Pourcentage de ménages disposant de l'électricité",
        es = "Porcentaje de hogares con electricidad",
        pt = "Percentual de domicílios com eletricidade"
      ),
      "CP_BREG_C_CRT" = list(
        fr = "Pourcentage d'enfants âgés de 0-4 ans possédant un acte de naissance",
        es = "Porcentaje de niños de 0-4 años que tienen un certificado de nacimiento",
        pt = "Percentual de crianças de 0-4 anos que possuem certidão de nascimento"
      ),
      "HC_FLRM_H_CER" = list(
        fr = "Pourcentage de ménages avec un sol en carreaux de céramique",
        es = "Porcentaje de hogares con pisos de cerámica",
        pt = "Percentual de domicílios com pisos de cerâmica"
      ),
      "WS_TLET_H_IMP" = list(
        fr = "Pourcentage de ménages disposant d'une installation sanitaire améliorée",
        es = "Porcentaje de hogares con una instalación sanitaria mejorada",
        pt = "Percentual de domicílios com instalação sanitária melhorada"
      ),
      "HC_HEFF_H_RDO" = list(
        fr = "Pourcentage de ménages possédant une radio",
        es = "Porcentaje de hogares que poseen una radio",
        pt = "Percentual de domicílios que possuem rádio"
      ),
      "WS_WTRT_H_SOL" = list(
        fr = "Pourcentage de ménages traitant l'eau par désinfection solaire",
        es = "Porcentaje de hogares que tratan el agua mediante desinfección solar",
        pt = "Percentual de domicílios que tratam a água por desinfecção solar"
      ),
      "CP_BREG_C_NCT" = list(
        fr = "Pourcentage d'enfants âgés de 0-4 ans ne possédant pas d'acte de naissance mais ayant été enregistrés auprès d'une autorité civile",
        es = "Porcentaje de niños de 0-4 años que no tienen certificado de nacimiento pero han sido registrados ante una autoridad civil",
        pt = "Percentual de crianças de 0-4 anos que não possuem certidão de nascimento, mas foram registradas em uma autoridade civil"
      ),
      "WS_WTRT_P_STN" = list(
        fr = "Pourcentage de la population de droit (de jure) vivant dans des ménages qui traitent l'eau en la filtrant à travers un tissu",
        es = "Porcentaje de la población de derecho (de jure) que vive en hogares que tratan el agua filtrándola a través de una tela",
        pt = "Percentual da população de direito (de jure) que vive em domicílios que tratam a água filtrando-a através de um pano"
      ),
      "HC_WIXQ_P_LOW" = list(
        fr = "Pourcentage de la population de droit (de jure) dans le quintile de richesse le plus bas",
        es = "Porcentaje de la población de derecho (de jure) en el quintil de riqueza más bajo",
        pt = "Percentual da população de direito (de jure) no quintil de riqueza mais baixo"
      ),
      "HC_WIXQ_P_2ND" = list(
        fr = "Pourcentage de la population de droit (de jure) dans le deuxième quintile de richesse",
        es = "Porcentaje de la población de derecho (de jure) en el segundo quintil de riqueza",
        pt = "Percentual da população de direito (de jure) no segundo quintil de riqueza"
      ),
      "WS_WTRT_P_BLC" = list(
        fr = "Pourcentage de la population de droit (de jure) vivant dans des ménages qui traitent l'eau en ajoutant de l'eau de Javel ou du chlore",
        es = "Porcentaje de la población de derecho (de jure) que vive en hogares que tratan el agua añadiendo lejía o cloro",
        pt = "Percentual da população de direito (de jure) que vive em domicílios que tratam a água adicionando alvejante ou cloro"
      ),
      "CP_BREG_C_REG" = list(
        fr = "Pourcentage d'enfants âgés de 0-4 ans dont les naissances sont déclarées comme enregistrées.",
        es = "Porcentaje de niños de 0-4 años cuyos nacimientos se reportan como registrados.",
        pt = "Percentual de crianças de 0-4 anos cujos nascimentos são declarados como registrados."
      ),
      "WS_WTRT_P_SOL" = list(
        fr = "Pourcentage de la population de droit (de jure) vivant dans des ménages qui traitent l'eau par désinfection solaire",
        es = "Porcentaje de la población de derecho (de jure) que vive en hogares que tratan el agua mediante desinfección solar",
        pt = "Percentual da população de direito (de jure) que vive em domicílios que tratam a água por desinfecção solar"
      ),
      "WS_WTRT_P_BOL" = list(
        fr = "Pourcentage de la population de droit (de jure) vivant dans des ménages qui traitent l'eau en la faisant bouillir",
        es = "Porcentaje de la población de derecho (de jure) que vive en hogares que tratan el agua hirviéndola",
        pt = "Percentual da população de direito (de jure) que vive em domicílios que tratam a água fervendo-a"
      ),
      "HC_WIXQ_P_12Q" = list(
        fr = "Population dans le quintile de richesse le plus bas ou le deuxième",
        es = "Población en el quintil de riqueza más bajo o el segundo",
        pt = "População no quintil de riqueza mais baixo ou no segundo"
      ),
      "AH_TOBU_M_SNN" = list(
        fr = "Pourcentage d'hommes utilisant du tabac à priser (par le nez)",
        es = "Porcentaje de hombres que usan rapé (tabaco por la nariz)",
        pt = "Percentual de homens que usam rapé (tabaco pelo nariz)"
      ),
      "CO_INUS_W_EVU" = list(
        fr = "Pourcentage de femmes ayant déjà utilisé Internet",
        es = "Porcentaje de mujeres que alguna vez han usado Internet",
        pt = "Percentual de mulheres que alguma vez usaram a Internet"
      ),
      "ED_MDIA_W_RDO" = list(
        fr = "Pourcentage de femmes écoutant la radio au moins une fois par semaine",
        es = "Porcentaje de mujeres que escuchan la radio al menos una vez por semana",
        pt = "Percentual de mulheres que ouvem o rádio pelo menos uma vez por semana"
      ),
      "ED_MDIA_W_TLV" = list(
        fr = "Pourcentage de femmes regardant la télévision au moins une fois par semaine",
        es = "Porcentaje de mujeres que ven la televisión al menos una vez por semana",
        pt = "Percentual de mulheres que assistem à televisão pelo menos uma vez por semana"
      ),
      "AH_TOBU_M_ASM" = list(
        fr = "Pourcentage d'hommes utilisant n'importe quel type de tabac sans fumée",
        es = "Porcentaje de hombres que usan cualquier tipo de tabaco sin humo",
        pt = "Percentual de homens que usam qualquer tipo de tabaco sem fumaça"
      ),
      "ED_MDIA_W_NWS" = list(
        fr = "Pourcentage de femmes lisant un journal au moins une fois par semaine",
        es = "Porcentaje de mujeres que leen un periódico al menos una vez por semana",
        pt = "Percentual de mulheres que leem jornal pelo menos uma vez por semana"
      ),
      "AH_TOBU_W_SNM" = list(
        fr = "Pourcentage de femmes utilisant du tabac à priser (par la bouche)",
        es = "Porcentaje de mujeres que usan rapé (tabaco por la boca)",
        pt = "Percentual de mulheres que usam rapé (tabaco pela boca)"
      ),
      "CO_INUS_W_U12" = list(
        fr = "Pourcentage de femmes ayant utilisé Internet au cours des 12 derniers mois",
        es = "Porcentaje de mujeres que usaron Internet en los últimos 12 meses",
        pt = "Percentual de mulheres que usaram a Internet nos últimos 12 meses"
      ),
      "AH_TOBC_W_OTH" = list(
        fr = "Pourcentage de femmes fumant d'autres formes de tabac",
        es = "Porcentaje de mujeres que fuman otro tipo de tabaco",
        pt = "Percentual de mulheres que fumam outros tipos de tabaco"
      ),
      "ED_MDIA_W_3MD" = list(
        fr = "Pourcentage de femmes ayant accès au journal, à la télévision et à la radio au moins une fois par semaine",
        es = "Porcentaje de mujeres con acceso a periódico, televisión y radio al menos una vez por semana",
        pt = "Percentual de mulheres com acesso a jornal, televisão e rádio pelo menos uma vez por semana"
      ),
      "ED_MDIA_W_N3M" = list(
        fr = "Pourcentage de femmes sans accès aux médias de masse",
        es = "Porcentaje de mujeres sin acceso a medios de comunicación masivos",
        pt = "Percentual de mulheres sem acesso aos meios de comunicação de massa"
      ),
      "ED_EDUC_W_SEH" = list(
        fr = "Pourcentage de femmes ayant un niveau d'éducation secondaire ou supérieur",
        es = "Porcentaje de mujeres con educación secundaria o superior",
        pt = "Percentual de mulheres com educação secundária ou superior"
      ),
      "AH_TOBU_W_SNN" = list(
        fr = "Pourcentage de femmes utilisant du tabac à priser (par le nez)",
        es = "Porcentaje de mujeres que usan rapé (tabaco por la nariz)",
        pt = "Percentual de mulheres que usam rapé (tabaco pelo nariz)"
      ),
      "ED_LITR_W_LIT" = list(
        fr = "Pourcentage de femmes alphabétisées",
        es = "Porcentaje de mujeres alfabetizadas",
        pt = "Percentual de mulheres alfabetizadas"
      ),
      "RH_ANCN_W_N4F" = list(
        fr = "Pourcentage de femmes ayant eu une naissance vivante (ou une mortinaissance) au cours des deux à cinq années précédant l'enquête, ayant eu 4 visites prénatales ou plus",
        es = "Porcentaje de mujeres que tuvieron un nacido vivo (o un nacido muerto) en los dos a cinco años anteriores a la encuesta y que tuvieron 4 o más visitas de atención prenatal",
        pt = "Percentual de mulheres que tiveram um nascido vivo (ou natimorto) nos dois a cinco anos anteriores à pesquisa e que tiveram 4 ou mais consultas de pré-natal"
      ),
      "RH_ANCN_W_N01" = list(
        fr = "Pourcentage de femmes ayant eu une naissance vivante (ou une mortinaissance) au cours des deux (ou trois/cinq) années précédant l'enquête et ayant effectué 1 visite prénatale",
        es = "Porcentaje de mujeres que tuvieron un nacido vivo (o un nacido muerto) en los dos (o tres/cinco) años anteriores a la encuesta y que tuvieron 1 visita de atención prenatal",
        pt = "Percentual de mulheres que tiveram um nascido vivo (ou natimorto) nos dois (ou três/cinco) anos anteriores à pesquisa e que tiveram 1 consulta de pré-natal"
      ),
      "RH_DELA_C_SKF" = list(
        fr = "Pourcentage de naissances vivantes (ou de mortinaissances) au cours des cinq années précédant l'enquête assistées par un professionnel qualifié. Un professionnel qualifié comprend les médecins, les infirmiers, les sages-femmes et les infirmiers ou sages-femmes auxiliaires.",
        es = "Porcentaje de nacidos vivos (o nacidos muertos) en los cinco años anteriores a la encuesta atendidos por un proveedor calificado. Un proveedor calificado incluye médicos, enfermeros, parteras y enfermeros o parteras auxiliares.",
        pt = "Percentual de nascidos vivos (ou natimortos) nos cinco anos anteriores à pesquisa assistidos por um profissional qualificado. Um profissional qualificado inclui médicos, enfermeiros, parteiras e enfermeiros ou parteiras auxiliares."
      ),
      "RH_ANCN_W_N4P" = list(
        fr = "Pourcentage de femmes ayant eu une naissance vivante (ou une mortinaissance) au cours des deux années précédant l'enquête, ayant eu 4 visites prénatales ou plus",
        es = "Porcentaje de mujeres que tuvieron un nacido vivo (o un nacido muerto) en los dos años anteriores a la encuesta y que tuvieron 4 o más visitas de atención prenatal",
        pt = "Percentual de mulheres que tiveram um nascido vivo (ou natimorto) nos dois anos anteriores à pesquisa e que tiveram 4 ou mais consultas de pré-natal"
      ),
      "RH_DELA_C_SKP" = list(
        fr = "Pourcentage de naissances vivantes (ou de mortinaissances) au cours des deux années précédant l'enquête assistées par un professionnel qualifié. Un professionnel qualifié comprend les médecins, les infirmiers, les sages-femmes et les infirmiers ou sages-femmes auxiliaires.",
        es = "Porcentaje de nacidos vivos (o nacidos muertos) en los dos años anteriores a la encuesta atendidos por un proveedor calificado. Un proveedor calificado incluye médicos, enfermeros, parteras y enfermeros o parteras auxiliares.",
        pt = "Percentual de nascidos vivos (ou natimortos) nos dois anos anteriores à pesquisa assistidos por um profissional qualificado. Um profissional qualificado inclui médicos, enfermeiros, parteiras e enfermeiros ou parteiras auxiliares."
      ),
      "RH_PCCT_C_DY2" = list(
        fr = "Pourcentage de derniers-nés au cours des deux années précédant l'enquête ayant eu leur premier contrôle postnatal dans les deux premiers jours suivant la naissance",
        es = "Porcentaje de últimos nacimientos en los dos años anteriores a la encuesta que tuvieron su primer control postnatal dentro de los primeros dos días después del nacimiento",
        pt = "Percentual dos últimos nascimentos nos dois anos anteriores à pesquisa que tiveram a primeira consulta pós-natal dentro dos primeiros dois dias após o nascimento"
      ),
      "RH_DELP_C_DHF" = list(
        fr = "Pourcentage de naissances vivantes (ou de mortinaissances) au cours des deux à cinq années précédant l'enquête, ayant eu lieu dans un établissement de santé",
        es = "Porcentaje de nacidos vivos (o nacidos muertos) en los dos a cinco años anteriores a la encuesta cuyos partos ocurrieron en un establecimiento de salud",
        pt = "Percentual de nascidos vivos (ou natimortos) nos dois a cinco anos anteriores à pesquisa cujos partos ocorreram em um estabelecimento de saúde"
      ),
      "RH_DELP_C_DHT" = list(
        fr = "Pourcentage de naissances vivantes (ou de mortinaissances) au cours des deux années précédant l'enquête, ayant eu lieu dans un établissement de santé",
        es = "Porcentaje de nacidos vivos (o nacidos muertos) en los dos años anteriores a la encuesta cuyos partos ocurrieron en un establecimiento de salud",
        pt = "Percentual de nascidos vivos (ou natimortos) nos dois anos anteriores à pesquisa cujos partos ocorreram em um estabelecimento de saúde"
      ),
      "HA_ANSS_W_CND" = list(
        fr = "Pourcentage de femmes qui pensent qu'une femme est en droit de demander d'utiliser un préservatif si elle sait que son mari a une IST",
        es = "Porcentaje de mujeres que creen que una mujer tiene derecho a pedir que se use un condón si sabe que su esposo tiene una ITS",
        pt = "Percentual de mulheres que acreditam que uma mulher tem o direito de pedir que se use preservativo se ela souber que o marido tem uma IST"
      ),
      "WE_AWBT_M_REF" = list(
        fr = "Pourcentage d'hommes qui sont d'accord pour dire qu'un mari a le droit de frapper ou de battre sa femme si elle refuse d'avoir des rapports sexuels avec lui",
        es = "Porcentaje de hombres que están de acuerdo en que un esposo tiene derecho a golpear o pegar a su esposa si ella se niega a tener relaciones sexuales con él",
        pt = "Percentual de homens que concordam que um marido tem o direito de bater ou agredir a esposa se ela se recusa a ter relações sexuais com ele"
      ),
      "WE_AWBT_M_OUT" = list(
        fr = "Pourcentage d'hommes qui sont d'accord pour dire qu'un mari a le droit de frapper ou de battre sa femme si elle sort sans le lui dire",
        es = "Porcentaje de hombres que están de acuerdo en que un esposo tiene derecho a golpear o pegar a su esposa si ella sale sin decírselo",
        pt = "Percentual de homens que concordam que um marido tem o direito de bater ou agredir a esposa se ela sai sem lhe avisar"
      ),
      "EM_EMPM_W_EMP" = list(
        fr = "Pourcentage de femmes actuellement mariées ou en union ayant été employées au cours des 12 mois précédant l'enquête",
        es = "Porcentaje de mujeres actualmente casadas o en unión que estuvieron empleadas en los 12 meses anteriores a la encuesta",
        pt = "Percentual de mulheres atualmente casadas ou em união que estiveram empregadas nos 12 meses anteriores à pesquisa"
      ),
      "HA_ANSS_M_CND" = list(
        fr = "Pourcentage d'hommes qui pensent qu'une femme est en droit de demander d'utiliser un préservatif si elle sait que son mari a une IST [hommes]",
        es = "Porcentaje de hombres que creen que una mujer tiene derecho a pedir que se use un condón si sabe que su esposo tiene una ITS [hombres]",
        pt = "Percentual de homens que acreditam que uma mulher tem o direito de pedir que se use preservativo se ela souber que o marido tem uma IST [homens]"
      ),
      "WE_AWBT_M_BFD" = list(
        fr = "Pourcentage d'hommes qui sont d'accord pour dire qu'un mari a le droit de frapper ou de battre sa femme si elle brûle la nourriture",
        es = "Porcentaje de hombres que están de acuerdo en que un esposo tiene derecho a golpear o pegar a su esposa si ella quema la comida",
        pt = "Percentual de homens que concordam que um marido tem o direito de bater ou agredir a esposa se ela queimar a comida"
      ),
      "HA_ANSS_W_RSX" = list(
        fr = "Pourcentage de femmes qui pensent qu'une femme est en droit de refuser d'avoir des rapports sexuels avec son mari si elle sait qu'il a des relations sexuelles avec d'autres femmes",
        es = "Porcentaje de mujeres que creen que una mujer tiene derecho a negarse a tener relaciones sexuales con su esposo si sabe que él tiene relaciones sexuales con otras mujeres",
        pt = "Percentual de mulheres que acreditam que uma mulher tem o direito de recusar relações sexuais com o marido se ela souber que ele tem relações sexuais com outras mulheres"
      ),
      "WE_AWBT_W_ARG" = list(
        fr = "Pourcentage de femmes qui sont d'accord pour dire qu'un mari a le droit de frapper ou de battre sa femme si elle se dispute avec lui",
        es = "Porcentaje de mujeres que están de acuerdo en que un esposo tiene derecho a golpear o pegar a su esposa si ella discute con él",
        pt = "Percentual de mulheres que concordam que um marido tem o direito de bater ou agredir a esposa se ela discutir com ele"
      ),
      "WE_AWBT_M_ARG" = list(
        fr = "Pourcentage d'hommes qui sont d'accord pour dire qu'un mari a le droit de frapper ou de battre sa femme si elle se dispute avec lui",
        es = "Porcentaje de hombres que están de acuerdo en que un esposo tiene derecho a golpear o pegar a su esposa si ella discute con él",
        pt = "Percentual de homens que concordam que um marido tem o direito de bater ou agredir a esposa se ela discutir com ele"
      ),
      "CO_MOBB_W_MBF" = list(
        fr = "Pourcentage de femmes utilisant un téléphone portable pour des transactions financières",
        es = "Porcentaje de mujeres que usan un teléfono móvil para transacciones financieras",
        pt = "Percentual de mulheres que usam um telefone móvel para transações financeiras"
      ),
      "CO_MOBB_W_MOB" = list(
        fr = "Pourcentage de femmes possédant un téléphone portable",
        es = "Porcentaje de mujeres que poseen un teléfono móvil",
        pt = "Percentual de mulheres que possuem um telefone móvel"
      ),
      "WE_AWBT_W_REF" = list(
        fr = "Pourcentage de femmes qui sont d'accord pour dire qu'un mari a le droit de frapper ou de battre sa femme si elle refuse d'avoir des rapports sexuels avec lui",
        es = "Porcentaje de mujeres que están de acuerdo en que un esposo tiene derecho a golpear o pegar a su esposa si ella se niega a tener relaciones sexuales con él",
        pt = "Percentual de mulheres que concordam que um marido tem o direito de bater ou agredir a esposa se ela se recusar a ter relações sexuais com ele"
      ),
      "WE_AWBT_W_BFD" = list(
        fr = "Pourcentage de femmes qui sont d'accord pour dire qu'un mari a le droit de frapper ou de battre sa femme si elle brûle la nourriture",
        es = "Porcentaje de mujeres que están de acuerdo en que un esposo tiene derecho a golpear o pegar a su esposa si ella quema la comida",
        pt = "Percentual de mulheres que concordam que um marido tem o direito de bater ou agredir a esposa se ela queimar a comida"
      ),
      "WE_AWBT_W_OUT" = list(
        fr = "Pourcentage de femmes qui sont d'accord pour dire qu'un mari a le droit de frapper ou de battre sa femme si elle sort sans le lui dire",
        es = "Porcentaje de mujeres que están de acuerdo en que un esposo tiene derecho a golpear o pegar a su esposa si ella sale sin decírselo",
        pt = "Percentual de mulheres que concordam que um marido tem o direito de bater ou agredir a esposa se ela sai sem lhe avisar"
      ),
      "CO_MOBB_W_BNK" = list(
        fr = "Pourcentage de femmes qui possèdent et utilisent un compte bancaire",
        es = "Porcentaje de mujeres que tienen y usan una cuenta bancaria",
        pt = "Percentual de mulheres que têm e usam uma conta bancária"
      ),
      "WE_AWBT_W_NEG" = list(
        fr = "Pourcentage de femmes qui sont d'accord pour dire qu'un mari a le droit de frapper ou de battre sa femme si elle néglige les enfants",
        es = "Porcentaje de mujeres que están de acuerdo en que un esposo tiene derecho a golpear o pegar a su esposa si ella descuida a los hijos",
        pt = "Percentual de mulheres que concordam que um marido tem o direito de bater ou agredir a esposa se ela negligencia os filhos"
      ),
      "RH_DELP_C_HOT" = list(
        fr = "Pourcentage de naissances vivantes (ou de mortinaissances) au cours des deux années précédant l'enquête ayant eu lieu à la maison",
        es = "Porcentaje de nacidos vivos (o nacidos muertos) en los dos años anteriores a la encuesta cuyo parto ocurrió en casa",
        pt = "Percentual de nascidos vivos (ou natimortos) nos dois anos anteriores à pesquisa cujo parto ocorreu em casa"
      ),
      "RH_DELP_C_HOM" = list(
        fr = "Pourcentage de naissances vivantes (ou de mortinaissances) au cours des cinq années précédant l'enquête ayant eu lieu à la maison",
        es = "Porcentaje de nacidos vivos (o nacidos muertos) en los cinco años anteriores a la encuesta cuyo parto ocurrió en casa",
        pt = "Percentual de nascidos vivos (ou natimortos) nos cinco anos anteriores à pesquisa cujo parto ocorreu em casa"
      ),
      "RH_DELP_C_PRV" = list(
        fr = "Pourcentage de naissances vivantes au cours des cinq années précédant l'enquête, ayant eu lieu dans un établissement du secteur privé",
        es = "Porcentaje de nacidos vivos en los cinco años anteriores a la encuesta cuyos partos ocurrieron en un establecimiento del sector privado",
        pt = "Percentual de nascidos vivos nos cinco anos anteriores à pesquisa cujos partos ocorreram em um estabelecimento do setor privado"
      ),
      "RH_DELP_C_PRT" = list(
        fr = "Pourcentage de naissances vivantes au cours des deux années précédant l'enquête, ayant eu lieu dans un établissement du secteur privé",
        es = "Porcentaje de nacidos vivos en los dos años anteriores a la encuesta cuyos partos ocurrieron en un establecimiento del sector privado",
        pt = "Percentual de nascidos vivos nos dois anos anteriores à pesquisa cujos partos ocorreram em um estabelecimento do setor privado"
      ),
      "RH_DELP_C_PUB" = list(
        fr = "Pourcentage de naissances vivantes au cours des cinq années précédant l'enquête, ayant eu lieu dans un établissement du secteur public",
        es = "Porcentaje de nacidos vivos en los cinco años anteriores a la encuesta cuyos partos ocurrieron en un establecimiento del sector público",
        pt = "Percentual de nascidos vivos nos cinco anos anteriores à pesquisa cujos partos ocorreram em um estabelecimento do setor público"
      ),
      "RH_DELP_C_PUT" = list(
        fr = "Pourcentage de naissances vivantes au cours des deux années précédant l'enquête, ayant eu lieu dans un établissement du secteur public",
        es = "Porcentaje de nacidos vivos en los dos años anteriores a la encuesta cuyos partos ocurrieron en un establecimiento del sector público",
        pt = "Percentual de nascidos vivos nos dois anos anteriores à pesquisa cujos partos ocorreram em um estabelecimento do setor público"
      ),
      "CM_ECMR_C_U5M" = list(
        fr = "Probabilité de décéder avant le cinquième anniversaire, au cours des dix années précédant l'enquête, pour 1 000 naissances vivantes.",
        es = "Probabilidad de morir antes del quinto cumpleaños, en los diez años anteriores a la encuesta, por 1.000 nacidos vivos.",
        pt = "Probabilidade de morrer antes do quinto aniversário, nos dez anos anteriores à pesquisa, por 1.000 nascidos vivos."
      ),
      "CM_ECMR_C_U5F" = list(
        fr = "Probabilité de décéder avant le cinquième anniversaire, au cours des cinq années précédant l'enquête, pour 1 000 naissances vivantes.",
        es = "Probabilidad de morir antes del quinto cumpleaños, en los cinco años anteriores a la encuesta, por 1.000 nacidos vivos.",
        pt = "Probabilidade de morrer antes do quinto aniversário, nos cinco anos anteriores à pesquisa, por 1.000 nascidos vivos."
      ),
      "RH_PCMT_W_DY2" = list(
        fr = "Pourcentage de femmes ayant accouché au cours des deux années précédant l'enquête, ayant eu leur premier contrôle postnatal dans les deux premiers jours suivant la naissance",
        es = "Porcentaje de mujeres que dieron a luz en los dos años anteriores a la encuesta y que tuvieron su primer control postnatal dentro de los primeros dos días después del nacimiento",
        pt = "Percentual de mulheres que deram à luz nos dois anos anteriores à pesquisa e que tiveram a primeira consulta pós-natal dentro dos primeiros dois dias após o nascimento"
      ),
      "FP_NDYA_W_PDM" = list(
        fr = "Pourcentage de la demande en matière de planification familiale satisfaite par des méthodes modernes ; calculé comme le nombre de jeunes femmes âgées de 15-24 ans utilisant des méthodes modernes de planification familiale, divisé par le nombre de toutes les jeunes femmes ayant une demande de planification familiale (soit un besoin non satisfait, soit utilisant actuellement n'importe quelle méthode de planification familiale)",
        es = "Porcentaje de la demanda de planificación familiar satisfecha con métodos modernos; se calcula como el número de jóvenes mujeres de 15-24 años que usan métodos modernos de planificación familiar dividido por el número de todas las jóvenes mujeres con demanda de planificación familiar (ya sea con necesidad insatisfecha o usando actualmente cualquier método de planificación familiar)",
        pt = "Percentual da demanda por planejamento familiar satisfeita por métodos modernos; calculado como o número de jovens mulheres de 15-24 anos que usam métodos modernos de planejamento familiar, dividido pelo número de todas as jovens mulheres com demanda de planejamento familiar (seja com necessidade não atendida ou usando atualmente qualquer método de planejamento familiar)"
      ),
      "FP_NADM_W_PDM" = list(
        fr = "Pourcentage de la demande en matière de planification familiale satisfaite par des méthodes modernes ; calculé comme le nombre de femmes actuellement mariées utilisant des méthodes modernes de planification familiale, divisé par le nombre de femmes actuellement mariées ayant une demande de planification familiale (soit un besoin non satisfait, soit utilisant actuellement n'importe quelle méthode de planification familiale)",
        es = "Porcentaje de la demanda de planificación familiar satisfecha con métodos modernos; se calcula como el número de mujeres actualmente casadas que usan métodos modernos de planificación familiar dividido por el número de mujeres actualmente casadas con demanda de planificación familiar (ya sea con necesidad insatisfecha o usando actualmente cualquier método de planificación familiar)",
        pt = "Percentual da demanda por planejamento familiar satisfeita por métodos modernos; calculado como o número de mulheres atualmente casadas que usam métodos modernos de planejamento familiar, dividido pelo número de mulheres atualmente casadas com demanda de planejamento familiar (seja com necessidade não atendida ou usando atualmente qualquer método de planejamento familiar)"
      ),
      "RH_PCMT_W_NON" = list(
        fr = "Pourcentage de femmes ayant accouché au cours des deux années précédant l'enquête, n'ayant eu aucun contrôle postnatal au cours des 42 premiers jours après la naissance",
        es = "Porcentaje de mujeres que dieron a luz en los dos años anteriores a la encuesta sin ningún control postnatal en los primeros 42 días después del nacimiento",
        pt = "Percentual de mulheres que deram à luz nos dois anos anteriores à pesquisa sem nenhuma consulta pós-natal nos primeiros 42 dias após o nascimento"
      ),
      "RH_PAHC_W_DIS" = list(
        fr = "Pourcentage de femmes ayant déclaré avoir de grands problèmes liés à la distance à un établissement de santé pour leur propre traitement lorsqu'elles sont malades",
        es = "Porcentaje de mujeres que reportaron tener grandes problemas con la distancia al establecimiento de salud para recibir tratamiento para ellas mismas cuando están enfermas",
        pt = "Percentual de mulheres que relataram ter grandes problemas com a distância até o estabelecimento de saúde para receber tratamento para si mesmas quando estão doentes"
      ),
      "RH_PAHC_W_MON" = list(
        fr = "Pourcentage de femmes ayant déclaré avoir de grands problèmes pour obtenir de l'argent pour leur propre traitement lorsqu'elles sont malades",
        es = "Porcentaje de mujeres que reportaron tener grandes problemas para obtener dinero para su propio tratamiento cuando están enfermas",
        pt = "Percentual de mulheres que relataram ter grandes problemas para conseguir dinheiro para o próprio tratamento quando estão doentes"
      ),
      "RH_PAHC_W_PRM" = list(
        fr = "Pourcentage de femmes ayant déclaré avoir de grands problèmes pour obtenir la permission d'aller se faire soigner lorsqu'elles sont malades",
        es = "Porcentaje de mujeres que reportaron tener grandes problemas para obtener permiso para ir a recibir tratamiento cuando están enfermas",
        pt = "Percentual de mulheres que relataram ter grandes problemas para obter permissão para ir buscar tratamento quando estão doentes"
      )
    )

    ###############################################################
    ### NEW: Translate an indicator Full_definition string
    ###############################################################
    # Primary path: look up the indicator ID in definition_id_translations.
    # This is the only path that gives high-quality output. It covers every
    # indicator ID currently shipped by surveyPrev.
    #
    # Fallback path: if a future surveyPrev release adds an indicator before
    # this table is updated, we apply the phrase dictionary as a *partial*
    # translation -- BUT only for long, multi-word phrases (>= 10 chars).
    # Short single words like "or", "the", "and", "who", "with", "age" are
    # skipped: in the previous implementation they were matched as
    # substrings inside unrelated words ("or" inside "before" -> "befoe"),
    # and even with word boundaries their translation depends on
    # grammatical context (gender/number agreement) that we can't infer
    # from a flat dictionary. Leaving them in English produces clearly
    # mixed text rather than confidently broken text.
    translate_definition <- function(def_str, lang = "en", def_id = NULL) {
      if (is.null(def_str) || length(def_str) == 0 ||
          is.na(def_str) || def_str == "") return(def_str)
      if (lang == "en") return(def_str)

      # ---- Path 1: per-ID hand-translated lookup ----
      if (!is.null(def_id) && length(def_id) > 0 && !is.na(def_id) &&
          def_id != "") {
        entry <- definition_id_translations[[as.character(def_id)]]
        if (!is.null(entry)) {
          val <- entry[[lang]]
          if (!is.null(val) && !is.na(val) && val != "") return(val)
        }
      }

      # ---- Path 2: phrase-dictionary fallback ----
      # Only multi-word phrases (>= 10 characters) are applied. This avoids
      # the short-word-as-substring bug entirely. Phrases are applied
      # longest-first so that more specific phrases match before more
      # general ones that might be contained within them.
      MIN_PHRASE_LEN <- 10
      phrases <- names(definition_phrase_dict)
      phrases <- phrases[nchar(phrases) >= MIN_PHRASE_LEN]
      phrases <- phrases[order(-nchar(phrases))]

      out <- def_str
      for (ph in phrases) {
        repl <- definition_phrase_dict[[ph]][[lang]]
        if (is.null(repl) || is.na(repl) || repl == "") next
        out <- gsub(ph, repl, out, fixed = TRUE)
        # Capitalised-first-letter variant
        first <- substr(ph, 1, 1)
        if (first != toupper(first)) {
          ph_cap <- paste0(toupper(first), substr(ph, 2, nchar(ph)))
          repl_first <- substr(repl, 1, 1)
          repl_cap <- paste0(toupper(repl_first), substr(repl, 2, nchar(repl)))
          out <- gsub(ph_cap, repl_cap, out, fixed = TRUE)
        }
      }
      out
    }

    ###############################################################
    ### NEW: Safe vectorised translators (per-element try/catch)
    ###############################################################
    # FIX (issue 4 hardening): if translate_topic / translate_definition
    # ever raises an error on a particular row (e.g. unexpected encoding,
    # NA, factor, etc.), fall back to the original English string for that
    # row instead of letting the whole column become NA / breaking the
    # surrounding code.
    safe_translate_topics <- function(x, lang) {
      vapply(x, function(v) {
        tryCatch(translate_topic(v, lang),
                 error = function(e) { message(e$message); as.character(v) })
      }, character(1), USE.NAMES = FALSE)
    }

    safe_translate_definitions <- function(x, lang, ids = NULL) {
      # If ids are supplied (parallel to x), pass each id alongside its text
      # so translate_definition can use the per-ID lookup. Otherwise fall
      # back to phrase-dictionary translation only.
      if (is.null(ids)) {
        vapply(x, function(v) {
          tryCatch(translate_definition(v, lang),
                   error = function(e) { message(e$message); as.character(v) })
        }, character(1), USE.NAMES = FALSE)
      } else {
        # Recycle if lengths differ rather than erroring out
        n <- length(x)
        ids <- rep_len(as.character(ids), n)
        vapply(seq_len(n), function(i) {
          tryCatch(translate_definition(x[[i]], lang, def_id = ids[[i]]),
                   error = function(e) { message(e$message); as.character(x[[i]]) })
        }, character(1), USE.NAMES = FALSE)
      }
    }

    ###############################################################
    ### NEW: Reactive shortcut to current language
    ###############################################################
    current_lang <- reactive({
      lang <- input$report_language
      if (is.null(lang) || !(lang %in% c("en","fr","es","pt"))) "en" else lang
    })

    ###############################################################
    ### NEW: Country -> default language map
    ###############################################################
    # When the user selects a country, the language dropdown is auto-set to the
    # language the DHS Final Report for that country is typically issued in
    # (restricted to the four languages this report supports). The user can
    # still manually override the dropdown afterwards.
    #
    # Only non-English defaults are listed here; everything else falls back to
    # English ('en'). Country names follow the format used in
    # DHS.country.meta$CountryName (the same value CountryInfo$country()
    # returns). Common alternate names / older names are included as separate
    # keys (e.g. Swaziland -> Eswatini, Ivory Coast -> Cote d'Ivoire) so a
    # rename upstream does not break the lookup.
    country_default_lang <- list(
      ## --- French (DHS reports typically published in French) ---
      "Benin"                       = "fr",
      "Burkina Faso"                = "fr",
      "Burundi"                     = "fr",
      "Cameroon"                    = "fr",
      "Central African Republic"    = "fr",
      "Chad"                        = "fr",
      "Comoros"                     = "fr",
      "Congo"                       = "fr",
      "Congo Democratic Republic"   = "fr",
      "Congo (Democratic Republic)" = "fr",
      "Democratic Republic of the Congo" = "fr",
      "Cote d'Ivoire"               = "fr",
      "Cote d\u2019Ivoire"          = "fr",  # curly apostrophe variant
      "Ivory Coast"                 = "fr",
      "Djibouti"                    = "fr",
      "Gabon"                       = "fr",
      "Guinea"                      = "fr",
      "Haiti"                       = "fr",
      "Madagascar"                  = "fr",
      "Mali"                        = "fr",
      "Mauritania"                  = "fr",
      "Morocco"                     = "fr",
      "Niger"                       = "fr",
      "Senegal"                     = "fr",
      "Togo"                        = "fr",
      "Tunisia"                     = "fr",

      ## --- Portuguese ---
      "Angola"                      = "pt",
      "Cape Verde"                  = "pt",
      "Cabo Verde"                  = "pt",
      "Guinea-Bissau"               = "pt",
      "Mozambique"                  = "pt",
      "Sao Tome and Principe"       = "pt",
      "S\u00e3o Tom\u00e9 and Pr\u00edncipe" = "pt",
      "Timor-Leste"                 = "pt",
      "East Timor"                  = "pt",

      ## --- Spanish ---
      "Bolivia"                     = "es",
      "Colombia"                    = "es",
      "Dominican Republic"          = "es",
      "Ecuador"                     = "es",
      "El Salvador"                 = "es",
      "Guatemala"                   = "es",
      "Honduras"                    = "es",
      "Mexico"                      = "es",
      "Nicaragua"                   = "es",
      "Paraguay"                    = "es",
      "Peru"                        = "es"
      ## All other DHS countries (Tanzania, Kenya, Uganda, Ghana, Nigeria,
      ## Ethiopia, Zambia, Zimbabwe, Malawi, Rwanda, Lesotho, Eswatini,
      ## Namibia, South Africa, Sudan, South Sudan, Liberia, Sierra Leone,
      ## Gambia, Afghanistan, Bangladesh, Cambodia, Egypt, India, Indonesia,
      ## Jordan, Kazakhstan, Kyrgyz Republic, Maldives, Myanmar, Nepal,
      ## Pakistan, Philippines, Sri Lanka, Tajikistan, Thailand, Turkmenistan,
      ## Uzbekistan, Vietnam, Yemen, Albania, Armenia, Azerbaijan, Moldova,
      ## Turkey, Ukraine, Papua New Guinea, etc.) default to English.
    )

    # Helper: look up a country's default language, falling back to 'en'.
    lookup_country_lang <- function(country_name) {
      if (is.null(country_name) || length(country_name) == 0 ||
          is.na(country_name) || country_name == "") return("en")
      hit <- country_default_lang[[as.character(country_name)]]
      if (is.null(hit)) "en" else hit
    }

    # Observer: whenever the selected country changes, snap the language
    # dropdown to that country's default. Uses ignoreInit = TRUE so that on
    # initial app load we don't fire an update before the user has even
    # selected a country (and so we don't override a user preference that
    # was already set during this session). The dropdown remains a normal
    # selectInput, so the user can override after the auto-set.
    observeEvent(CountryInfo$country(), {
      tgt_lang <- lookup_country_lang(CountryInfo$country())
      # Only update if different from what's currently selected, to avoid
      # an unnecessary reactive cycle.
      cur <- isolate(input$report_language)
      if (is.null(cur) || cur != tgt_lang) {
        updateSelectInput(session, "report_language", selected = tgt_lang)
      }
    }, ignoreInit = FALSE, ignoreNULL = TRUE)

    ### Display general info
    output$info_display <- renderUI({

      req(CountryInfo$country())
      req(CountryInfo$svy_indicator_var())
      req(CountryInfo$svy_analysis_dat())

      country <- CountryInfo$country()
      svy_year <- CountryInfo$svyYear_selected()
      lang <- current_lang()

      HTML(paste0(
        "<p style='font-size: large;'>",
        tr("ui_selected_country", lang), ": <span style='font-weight:bold;background-color: #D0E4F7;'>", country, "</span>.",
        " ", tr("ui_survey_year", lang), ": <span style='font-weight:bold;background-color: #D0E4F7;'>", svy_year, "</span>.",
        "<br>",
        tr("ui_indicator", lang), ": <span style='font-weight:bold;background-color: #D0E4F7;'>", CountryInfo$svy_indicator_des(),
        "</span>.</p>",
        "<hr style='border-top-color: #E0E0E0;'>"
      ))

    })




    ### download button

    output$download_button_ui <- renderUI({

      req(CountryInfo$country())
      req(CountryInfo$svy_indicator_var())
      req(CountryInfo$svy_analysis_dat())

      downloadButton(ns("download_report"),
                     tr("ui_download_button", current_lang()),
                     icon = icon("download"),
                     class = "btn-primary")

    })


    output$download_report <- downloadHandler(
      filename = function() {

        # file.prefix <- paste0(CountryInfo$country(),'_',
        #                       input$selected_adm,'_',
        #                       input$selected_method,'_',
        #                       input$selected_measure)

        ### informative file name

        DHS_country_code <- DHS.country.meta[DHS.country.meta$CountryName == CountryInfo$country(),]$DHS_CountryCode

        file.prefix <- paste0(DHS_country_code,CountryInfo$svyYear_selected(),'_',
                              CountryInfo$svy_indicator_var())


        file.prefix <- gsub("[-.]", "_", file.prefix)

        # NEW: include language code in filename so EN/FR/ES/PT downloads
        # don't overwrite each other
        return(paste0(file.prefix, '_report_', current_lang(), '.pdf'))

      },

      content = function(file) {

        # NEW: snapshot the chosen language at download time so it is
        # used consistently throughout this content function
        lang <- current_lang()

        # Create the PDF
        grDevices::pdf(file, width = 10, height = 10)  # Set width and height of the PDF

        # Function to add header and footer
        # add_header_footer <- function(header_text, footer_text) {
        #   grid.text(header_text, x = 0.1, y = 0.98, gp = gpar(fontsize = 10, fontface = "bold"), just = "center")
        #   grid.text(footer_text, x = 0.5, y = 0.02, gp = gpar(fontsize = 8), just = "center")
        # }
        #
        # # Page 1: Table of Contents
        # grid.newpage()
        # grid.text("Table of Contents", y = 0.9, gp = gpar(fontsize = 14, fontface = "bold"))
        # toc_text <- "1. Plot on the First Page...........1\n2. Text Paragraph....................2\n3. Image.............................3"
        # grid.text(toc_text, x = 0.1, y = 0.7, gp = gpar(fontsize = 12), just = "left")
        # #add_header_footer("Document Title", "Page 1")
        #
        # # Page 2: Plot
        # grid.newpage()
        # plot(stats::rnorm(100), main = "Plot on the First Page")
        # add_header_footer("Document Title", "Page 2")
        #
        # # Page 3: Text Paragraph
        # grid.newpage()
        # grid.text("This is a paragraph of text. It appears on the second page of the document.\n
        #   You can add multiple lines here for longer paragraphs and provide more information.",
        #           x = 0.5, y = 0.5, gp = gpar(fontsize = 12), just = "center")
        # add_header_footer("Document Title", "Page 3")

        {

          session$sendCustomMessage('controlSpinner', list(action = "show",
                                                           message = tr("ui_report_being_prepared", lang)))

          ###############################################################
          ### preparation
          ###############################################################

          ### first make sure that the cluster info are prepared for all admin levels
          geo_info_list <- AnalysisInfo$cluster_admin_info_list()

          for (j in seq_len(length(CountryInfo$GADM_analysis_levels()))){

            tmp.adm <- CountryInfo$GADM_analysis_levels()[j]
            tmp.adm.num <- admin_to_num(tmp.adm)
            ### prepare admin level GPS info if not stored
            tmp.geo.info <- geo_info_list[[tmp.adm]]

            if(is.null(tmp.geo.info)){

              tryCatch({

                message(tmp.adm)

                tmp.cluster.adm.info <- cluster_admin_info(cluster.geo= CountryInfo$svy_GPS_dat(),  #mdg.ex.GPS
                                                           gadm.list = CountryInfo$GADM_list(),  #mdg.ex.GADM.list
                                                           model.gadm.level = admin_to_num(tmp.adm),
                                                           strat.gadm.level = CountryInfo$GADM_strata_level())


                AnalysisInfo$set_info_list(tmp.adm,tmp.cluster.adm.info)

                geo_info_list <- AnalysisInfo$cluster_admin_info_list()
                tmp.geo.info <- geo_info_list[[tmp.adm]]

              },error = function(e) {
                message(e$message)
              })
            }

          }

          geo_info_list <- AnalysisInfo$cluster_admin_info_list()


          ###########################
          ### initialize parameters
          ###########################

          page_counter = 1

          ###############################################################
          ### country/survey/indicator meta info
          ###############################################################

          grid.newpage()
          grid.text(tr("summary_info", lang), x = 0.05, y = 0.95, just = "left", gp = gpar(fontsize = 16))

          tryCatch({

            #pushViewport(viewport(layout = grid.layout(10, 1, heights = grid::unit(c(0.5, 0.4, 0.4, 0.4, 0.6, 0.4, 1.5, 0.4, 1.5, 2), "inches"))))

            # Title section for summary info
            #grid.text("Summary Info", x = 0.05, y = 0.95, just = "left", gp = gpar(fontsize = 16))

            # Country info section
            # FIX (issue 1): single grid.text call so the bold value follows the
            # translated label naturally and never overlaps with it, regardless
            # of label length.
            grid.text(bquote(paste(.(tr("country_label", lang)),
                                   bold(.(as.character(CountryInfo$country()))))),
                      x = 0.05, y = 0.85, just = "left", gp = gpar(fontsize = 12))

            grid.text(bquote(paste(.(tr("survey_label", lang)),
                                   bold(.(as.character(CountryInfo$svyYear_selected()))))),
                      x = 0.05, y = 0.8, just = "left", gp = gpar(fontsize = 12))

            grid.text(bquote(paste(.(tr("indicator_label", lang)),
                                   bold(.(as.character(CountryInfo$svy_indicator_des()))))),
                      x = 0.05, y = 0.75, just = "left", gp = gpar(fontsize = 12))

            grid.text(bquote(paste(.(tr("levels_label", lang)),
                                   bold(.(as.character(concatenate_vector_with_and(CountryInfo$GADM_analysis_levels())))))),
                      x = 0.05, y = 0.7, just = "left", gp = gpar(fontsize = 12))

            # Title for number of regions section
            grid.text(tr("n_regions_title", lang), x = 0.05, y = 0.6, just = "left", gp = gpar(fontsize = 12))

            # Table for number of admin regions (placed lower)
            pushViewport(viewport(y = 0.6, height = grid::unit(1.5, "inches"), just = "top"))
            n_region_tab <- check_gadm_levels(CountryInfo$GADM_list())
            n_region_tab_grob <- tableGrob(n_region_tab)  # Remove row names
            grid.draw(n_region_tab_grob)
            upViewport()

            # Title for detailed indicator info
            pushViewport(viewport(y = 0.5, height = grid::unit(2, "inches"), just = "top"))
            grid.text(tr("detailed_ind_title", lang), x = 0.05, y = 0.5, just = "left", gp = gpar(fontsize = 12))
            upViewport()

            # Table for detailed indicator info
            pushViewport(viewport(y = 0.4, height = grid::unit(2, "inches"), just = "top"))

            # Filter and wrap text for the indicator table

            tmp_indicator_ID  <- CountryInfo$svy_indicator_var()

            {
              ### customized window
              if(tmp_indicator_ID=='CM_ECMR_C_NNF'){tmp_indicator_ID = 'CM_ECMR_C_NNR'}

            }

            ind_detailed_tab <- ref_tab_all[ref_tab_all$ID == tmp_indicator_ID, c('ID', 'Topic', 'Full_definition')]

            # NEW: translate the Topic (DHS Report Chapter) and the Definition
            # columns before rendering the table. Uses safe wrappers so a single
            # bad row falls back to English rather than breaking the column.
            ind_detailed_tab$Topic           <- safe_translate_topics(ind_detailed_tab$Topic, lang)
            ind_detailed_tab$Full_definition <- safe_translate_definitions(
              ind_detailed_tab$Full_definition, lang, ids = ind_detailed_tab$ID)

            # NEW: translated column headers
            colnames(ind_detailed_tab) <- c(tr("col_dhs_id", lang),
                                            tr("col_dhs_chapter", lang),
                                            tr("col_definition", lang))

            # Text wrapping for the Definition column
            wrap_text <- function(text, width = 75) {
              paste(strwrap(text, width = width), collapse = "\n")
            }
            # NOTE: column names are now language-dependent, so reference them
            # by position to remain robust.
            ind_detailed_tab[[3]] <- sapply(ind_detailed_tab[[3]], wrap_text, width = 50)
            ind_detailed_tab[[2]] <- sapply(ind_detailed_tab[[2]], wrap_text, width = 50)

            # Create and draw the indicator details table
            ind_tab_grob <- tableGrob(ind_detailed_tab, rows = NULL)  # Remove row names
            grid.draw(ind_tab_grob)

            upViewport()

            grid.text(paste0("*"), x = 0.05, y = 0.15, gp = gpar(fontsize = 12, col = "red"),
                      just = "left")

            grid.text(tr("footnote_default_a", lang),
                      x = 0.06, y = 0.15, just = "left", gp = gpar(fontsize = 12))

            grid.text(tr("footnote_default_b", lang),
                      x = 0.05, y = 0.12, just = "left", gp = gpar(fontsize = 12))




          },error = function(e) {

            grid.text(tr("page_error", lang), x = 0.05, y = 0.1, just = "left", gp = gpar(fontsize = 12))

            message(e$message)

          })

          grid.text(paste0(page_counter), x = 0.5, y = 0.05, gp = gpar(fontsize = 12), just = "center")
          page_counter = page_counter +1


          ###############################################################
          ### Estimate Consistency Check (National Level)
          ###############################################################

          grid.newpage()
          grid.text(tr("consistency_title", lang),
                    x = 0.05, y = 0.95, just = "left", gp = gpar(fontsize = 16))

          tryCatch({

            ###########################
            ### Prepare
            ###########################

            ### prepare national estimates
            tmp.natl.res <- AnalysisInfo$Natl_res()
            natl_est = tmp.natl.res$direct.est*100

            if(grepl("dying", CountryInfo$svy_indicator_des())|grepl("ortality", CountryInfo$svy_indicator_des())){
              natl_est = natl_est*10
              description_suffix <- tr("per_1000", lang)
            }else{
              description_suffix <- ' %'
            }

            natl_est <- format(round(natl_est, digits=2), nsmall = 2)


            ### prepare DHS estimate

            tmp_indicator_ID  <- CountryInfo$svy_indicator_var()

            {
              ### customized window
              if(tmp_indicator_ID=='CM_ECMR_C_NNF'){tmp_indicator_ID = 'CM_ECMR_C_NNR'}

            }

            DHS_country_code <- DHS.country.meta[DHS.country.meta$CountryName == CountryInfo$country(),]$DHS_CountryCode
            ind_api_est <- DHS_api_est[DHS_api_est$`Country Code`==DHS_country_code&
                                         DHS_api_est$`DHS Standard ID`==tmp_indicator_ID&
                                         DHS_api_est$`Survey Year`==CountryInfo$svyYear_selected(),]

            if(dim(ind_api_est)[1]==0){
            }else{

              ind_api_est <- merge(ind_api_est,
                                   DHS_ind_dictionary[,c("DHS Standard Indicator ID","Full Definition")],
                                   by.x="DHS Standard ID",
                                   by.y="DHS Standard Indicator ID",
                                   all.x=T)


              ind_api_est <- ind_api_est[,c('Country','Survey Year',
                                            'DHS Standard ID','Full Definition','Estimate','By Variable Label')]

              wrap_text <- function(text, width = 75) {
                paste(strwrap(text, width = width), collapse = "\n")
              }

              # NEW: translate the Full Definition for this table too. Uses the
              # safe wrapper so a row-level translation failure does not blank
              # out the column and break the downstream table.
              ind_api_est$`Full Definition` <- safe_translate_definitions(
                ind_api_est$`Full Definition`, lang, ids = ind_api_est$`DHS Standard ID`)

              ind_api_est$`Full Definition` <- sapply(ind_api_est$`Full Definition`, wrap_text, width = 35)
              ind_api_est$`By Variable Label` <- sapply(ind_api_est$`By Variable Label`, wrap_text, width = 30)

              if(dim(ind_api_est)[1]>1){
                # NEW: the "Same as above" marker also needs translation
                same_as_above <- switch(lang,
                                        en = "Same as above",
                                        fr = "Idem ci-dessus",
                                        es = "Igual que arriba",
                                        pt = "Igual ao acima",
                                        "Same as above")
                ind_api_est$`Full Definition` <- c(ind_api_est$`Full Definition`[1],
                                                   rep(same_as_above, times = dim(ind_api_est)[1] - 1))
              }

              if('Total' %in% ind_api_est$`By Variable Label`){
                ind_api_est <- ind_api_est[ind_api_est$`By Variable Label` =='Total',]
              }




            }



            ###########################
            ### Draw
            ###########################
            grid.text(tr("consistency_intro", lang),
                      x = 0.05, y = 0.88, just = "left", gp = gpar(fontsize = 12))

            # NEW: dynamically build the bold mixed-format line per language.
            # We use bquote() instead of expression() so the translated
            # strings can be interpolated.
            grid.text(bquote(paste(
              bold(.(tr("consistency_app_bold", lang))),
              .(tr("consistency_with", lang)),
              bold(.(tr("consistency_dhs_bold", lang))),
              "."
            )),
            x = 0.05, y = 0.83, just = "left", gp = gpar(fontsize = 12, fontface = "bold"))


            # FIX (issue 1): render label + value as a single bquote() expression
            # so the bold value naturally follows the (potentially long) translated
            # label, instead of being placed at a fixed x that overlaps.
            grid.text(bquote(paste(.(tr("natl_est_app", lang)),
                                   bold(.(paste0(natl_est, description_suffix))))),
                      x = 0.05, y = 0.75, just = "left", gp = gpar(fontsize = 12))


            if(dim(ind_api_est)[1]==0){
              grid.text(tr("natl_est_dhs_unavail", lang),
                        x = 0.05, y = 0.68, just = "left", gp = gpar(fontsize = 12))
            }else{

              # FIX (issue 4): also render the DHS estimate VALUE as inline text
              # right next to the label, as a robust fallback. This guarantees the
              # number is visible even if the detailed table below fails to render
              # for any reason (e.g. unexpected data shape from the DHS API).
              dhs_est_value_text <- tryCatch({
                ## Prefer the 'Total' row when present, else the first row.
                est_row <- if ('Total' %in% ind_api_est$`By Variable Label`) {
                  ind_api_est[ind_api_est$`By Variable Label` == 'Total', ][1, ]
                } else {
                  ind_api_est[1, ]
                }
                est_val <- est_row$Estimate
                if (is.null(est_val) || length(est_val) == 0 || is.na(est_val)) "" else {
                  paste0(format(round(as.numeric(est_val), digits = 2), nsmall = 2),
                         description_suffix)
                }
              }, error = function(e) { message(e$message); "" })

              if (nzchar(dhs_est_value_text)) {
                grid.text(bquote(paste(.(tr("natl_est_dhs", lang)),
                                       bold(.(dhs_est_value_text)))),
                          x = 0.05, y = 0.68, just = "left", gp = gpar(fontsize = 12))
              } else {
                grid.text(tr("natl_est_dhs", lang),
                          x = 0.05, y = 0.68, just = "left", gp = gpar(fontsize = 12))
              }

              pushViewport(viewport(y = 0.63, height = grid::unit(3, "inches"), just = "top"))

              # Get the number of rows and columns in your table
              n_rows <- nrow(ind_api_est)
              n_cols <- ncol(ind_api_est)

              # Modify the theme to color only the estimate column
              custom_theme <- gridExtra::ttheme_default(
                core = list(
                  bg_params = list(
                    fill = matrix(c(rep("white", n_rows * (n_cols - 2)), rep("darkolivegreen1", n_rows),
                                    rep("white",n_rows)),
                                  nrow = n_rows, ncol = n_cols, byrow = FALSE)
                  )
                )
              )

              # Apply the theme to the table
              DHS_est_tab_grob <- tableGrob(ind_api_est, rows = NULL, theme = custom_theme)


              # color national estimates
              #DHS_est_tab_grob$grobs[23][[1]][["gp"]] <- gpar(fill="darkolivegreen1", col = "darkolivegreen4", lwd=5)

              grid.draw(DHS_est_tab_grob)

              upViewport()

              grid.text(paste0("*"), x = 0.05, y = 0.15, gp = gpar(fontsize = 12, col = "red"),
                        just = "left")

              grid.text(tr("footnote_default_a", lang),
                        x = 0.06, y = 0.15, just = "left", gp = gpar(fontsize = 12))

              grid.text(tr("footnote_default_b", lang),
                        x = 0.05, y = 0.12, just = "left", gp = gpar(fontsize = 12))

            }



          },error = function(e) {

            grid.text(tr("page_error", lang), x = 0.05, y = 0.1, just = "left", gp = gpar(fontsize = 12))

            message(e$message)

          })

          grid.text(paste0(page_counter), x = 0.5, y = 0.05, gp = gpar(fontsize = 12), just = "center")
          page_counter = page_counter +1

          ###############################################################
          ### Overall Sampling Info
          ###############################################################


          grid.newpage()
          grid.text(tr("overall_sample_info", lang), x = 0.05, y = 0.95, just = "left", gp = gpar(fontsize = 16))

          tryCatch({

            ###########################
            ### Prepare
            ###########################

            ### preparation
            analysis_dat <- CountryInfo$svy_analysis_dat()
            complete_dat <- analysis_dat[!is.na(analysis_dat$value),]

            ### calculate missing in regions
            samp_info_adm_map_list <- list()
            p_missing_cluster <- vector()


            for(tmp_adm in CountryInfo$GADM_analysis_levels()){
              if(tmp_adm!='National'){
                ex_adm_maps <- sample_info_map_static(model.gadm.level = admin_to_num(tmp_adm),
                                                      # FIX: use the survey's actual stratification level
                                                      # instead of a hardcoded 1, so the pseudo-level logic
                                                      # (and therefore the admin2.name.full join key) matches
                                                      # how cluster.info was constructed for this admin level.
                                                      strat.gadm.level = CountryInfo$GADM_strata_level(),
                                                      analysis.dat = CountryInfo$svy_analysis_dat(),
                                                      gadm.list.visual = CountryInfo$GADM_list_smoothed(),
                                                      cluster.info = geo_info_list[[tmp_adm]]$cluster.info)
                samp_info_adm_map_list[[tmp_adm]] <- ex_adm_maps

                p_missing_cluster <- c(p_missing_cluster,
                                       sum(is.na(ex_adm_maps$adm.sample.info$n.clusters))/dim(ex_adm_maps$adm.sample.info)[1])
              }
            }

            cluster_missing_info <- data.frame(
              Admin_Level = (CountryInfo$GADM_analysis_levels()),
              Percent_Missing = c(0,p_missing_cluster)
            )

            cluster_missing_info$Percent_Missing <- paste0(round(cluster_missing_info$Percent_Missing*100,digits=2),'%')
            row.names(cluster_missing_info) <- NULL
            # NEW: translated column headers
            colnames(cluster_missing_info) <- c(tr("col_admin_level", lang),
                                                tr("col_regions_no_clusters", lang))

            missing_tab <- as.data.frame(t(cluster_missing_info))
            colnames(missing_tab) <- missing_tab[1, ]
            missing_tab <-  missing_tab[-1, , drop = FALSE]

            ###########################
            ### Draw
            ###########################

            # Country info section
            # FIX (issue 1): single grid.text call so the bold value follows the
            # translated label naturally and never overlaps with it.
            grid.text(bquote(paste(.(tr("total_clusters", lang)),
                                   bold(.(as.character(length(unique(complete_dat$cluster))))))),
                      x = 0.05, y = 0.85, just = "left", gp = gpar(fontsize = 12))

            grid.text(bquote(paste(.(tr("total_sample_size", lang)),
                                   bold(.(as.character(dim(complete_dat)[1]))))),
                      x = 0.05, y = 0.8, just = "left", gp = gpar(fontsize = 12))

            grid.text(bquote(paste(.(tr("total_events", lang)),
                                   bold(.(as.character(sum(complete_dat$value)))))),
                      x = 0.05, y = 0.75, just = "left", gp = gpar(fontsize = 12))

            # Title for % of regions without any clusters
            grid.text(tr("regions_no_data", lang), x = 0.05, y = 0.65, just = "left", gp = gpar(fontsize = 12))

            # Table for number of admin regions (placed lower)
            pushViewport(viewport(y = 0.65, height = grid::unit(1.5, "inches"), just = "top"))
            p_missing_tab_grob <- tableGrob(missing_tab)

            grid.draw(p_missing_tab_grob)
            upViewport()


          },error = function(e) {

            grid.text(tr("page_error", lang), x = 0.05, y = 0.1, just = "left", gp = gpar(fontsize = 12))

            message(e$message)

          })


          grid.text(paste0(page_counter), x = 0.5, y = 0.05, gp = gpar(fontsize = 12), just = "center")
          page_counter = page_counter +1

          ###############################################################
          ### Admin Specific Sampling Info
          ###############################################################

          for(tmp_adm in CountryInfo$GADM_analysis_levels()){

            if(tmp_adm=='National'){next}

            grid.newpage()

            grid.text(paste0(tr("sample_info_for", lang), tmp_adm), x = 0.05, y = 0.95, just = "left", gp = gpar(fontsize = 16))

            tryCatch({

              ex_adm_maps <- sample_info_map_static(model.gadm.level = admin_to_num(tmp_adm),
                                                    # FIX: use the survey's actual stratification level
                                                    # instead of a hardcoded 1, so the pseudo-level logic
                                                    # (and therefore the admin2.name.full join key) matches
                                                    # how cluster.info was constructed for this admin level.
                                                    strat.gadm.level = CountryInfo$GADM_strata_level(),
                                                    analysis.dat = CountryInfo$svy_analysis_dat(),
                                                    gadm.list.visual = CountryInfo$GADM_list_smoothed(),
                                                    cluster.info = geo_info_list[[tmp_adm]]$cluster.info)

              # Define the map objects and their descriptions (translated)
              map_list <- list(
                tmp_map_event   = list(map = ex_adm_maps$n_event_map,   description = tr("n_events",   lang)),
                tmp_map_cluster = list(map = ex_adm_maps$n_cluster_map, description = tr("n_clusters", lang)),
                tmp_map_sample  = list(map = ex_adm_maps$n_sample_map,  description = tr("n_samples",  lang))
              )

              # Function to apply common modifications to each map
              apply_map_modifications <- function(map_obj, description) {
                map_obj$data$model_des <- description
                map_obj <- map_obj +
                  ggplot2::facet_wrap(ggplot2::vars(model_des)) +
                  ggplot2::theme(
                    legend.text = ggplot2::element_text(size = 11),
                    legend.title = ggplot2::element_text(size = 12),
                    # FIX (issue 2): shrink strip text so translated banner labels
                    # (e.g. "Nombre d'événements", "Número de conglomerados")
                    # fit within the facet strip width.
                    strip.text.x = ggplot2::element_text(size = 11),
                    legend.key.height = ggplot2::unit(0.5, 'cm'),
                    strip.text = ggplot2::element_text(size = 12),
                    legend.position = "bottom",
                    legend.justification = c(0.5, 0),      # Center the legend at the bottom
                    legend.box = "horizontal"              # Arrange legend items in a row
                  )
                return(map_obj)
              }

              # Apply the function to each map
              for (name in names(map_list)) {
                map_list[[name]]$map <- apply_map_modifications(map_list[[name]]$map, map_list[[name]]$description)
              }

              # Retrieve modified maps if needed
              tmp_map_event <- map_list$tmp_map_event$map
              tmp_map_cluster <- map_list$tmp_map_cluster$map
              tmp_map_sample <- map_list$tmp_map_sample$map


              #tmp_map_grob <- ggplot2::ggplotGrob(tmp_map)
              combined_grob <- arrangeGrob(
                tmp_map_cluster,  # Centered first row
                arrangeGrob(tmp_map_event, tmp_map_sample, ncol = 2),  # Second row with two plots
                nrow = 2)

              pushViewport(viewport(width = 0.9, height = 0.8, just = c("center", "center")))  # Set size within page
              # grid.arrange(
              #   tmp_map_grob,  # Centered first row
              #   arrangeGrob(tmp_map_grob, tmp_map_grob, ncol = 2),  # Second row with two plots
              #   nrow = 2,
              #   heights = c(0.45, 0.45))
              grid.draw(combined_grob)

              upViewport()
            },error = function(e) {

              grid.text(tr("page_error", lang), x = 0.05, y = 0.1, just = "left", gp = gpar(fontsize = 12))

              message(e$message)

            })

            grid.text(paste0(page_counter), x = 0.5, y = 0.05, gp = gpar(fontsize = 12), just = "center")
            page_counter = page_counter +1




          }





          ###############################################################
          ### Collect fitted model info and set common info
          ###############################################################

          selected_adms <- CountryInfo$GADM_analysis_levels()
          method_names <- c("Direct", "FH", "Unit")
          model_res_all <- AnalysisInfo$model_res_list() ## react if model results change

          fitted_model_matrix <- as.data.frame(matrix(vector('integer', length(method_names) * length(selected_adms)),
                                                      nrow = length(method_names),
                                                      dimnames = list(method_names, selected_adms)))


          #num_fitted_model <- 0
          # Populate the dataframe with checkbox inputs
          for (i in seq_len(length(method_names))) {
            for (j in seq_len(length(selected_adms))) {

              tmp.method <- method_names[i]
              tmp.adm <- selected_adms[j]
              #message(paste0('present model from method ',tmp.method, ' at ', tmp.adm,' level.'))
              model_res_selected <- model_res_all[[tmp.method]][[tmp.adm]]

              if(!is.null(model_res_selected)){
                fitted_model_matrix[i,j]=1
              }else{
                fitted_model_matrix[i,j]=0
              }

            }
          }

          ### set up labels used across plots
          # NEW: translated method labels
          method_match <- c(
            "Direct" = tr("method_direct", lang),
            "Unit"   = tr("method_unit",   lang),
            "FH"     = tr("method_fh",     lang)
          )

          # present measures as full name, maps
          # FIX (issue 2, continued): use the SINGLE-LINE measure labels (not
          # the line-broken ones) for the strip banner above each map. A line
          # break wastes horizontal room — translated phrases like
          # "Probabilidade \n de excedência" end up clipped because each half
          # is rendered narrow. A single line at a smaller font (set in the
          # prevMap theme below) uses the full panel width and fits cleanly.
          measure_match <- c(
            "cv"          = tr("measure_cv",      lang),
            "mean"        = tr("measure_mean",    lang),
            "CI.width"    = tr("measure_ciwidth", lang),
            "exceed_prob" = tr("measure_exceed",  lang)
          )

          # present measures as full name, scatter plot
          # NEW: translated measure labels
          measure_match_nobreak <- c(
            "cv"          = tr("measure_cv",      lang),
            "mean"        = tr("measure_mean",    lang),
            "CI.width"    = tr("measure_ciwidth", lang),
            "exceed_prob" = tr("measure_exceed",  lang)
          )



          ###############################################################
          ### Model specific maps
          ###############################################################

          #############################
          ### prepare plot
          #############################

          ### measures used in the plots
          all.measures <- c('mean','CI.width','cv','exceed_prob')
          #all.measures <- c('mean','CI.width','cv')


          single_model_maps_collection <- list()

          for (i in seq_len(length(method_names))) {
            for (j in seq_len(length(selected_adms))) {

              tmp.method <- method_names[i]
              tmp.adm <- selected_adms[j]

              if(!fitted_model_matrix[i,j]==1|tmp.adm=='National'){next}

              message(paste0('preparing key statistics map for ',tmp.method, ' model at ',tmp.adm, ' level.'))

              model_res_tmp <- model_res_all[[tmp.method]][[tmp.adm]]


              tmp.map.list <- list()

              for(tmp_measure in all.measures){

                if(tmp_measure=='exceed_prob'){
                  tmp.natl.res <- AnalysisInfo$Natl_res()
                  selected_threshold <- tmp.natl.res$direct.est
                  message(selected_threshold)
                }else{selected_threshold=NULL}


                one.static.plot <-  tryCatch({
                  tmp.plot <- suppressWarnings(surveyPrev::prevMap(res.obj = model_res_tmp,
                                                              poly.shp = CountryInfo$GADM_list_smoothed()[[tmp.adm]],
                                                              value.to.plot =tmp_measure,
                                                              legend.label = measure_match[tmp_measure],
                                                              map.title=NULL,
                                                              threshold.p = selected_threshold))

                  tmp.plot <- tmp.plot+
                    ggplot2::theme (legend.text=ggplot2::element_text(size=11),
                                    legend.title = ggplot2::element_text(size=12),
                                    # FIX (issue 2): shrink the strip banner text
                                    # so single-line translated measure labels
                                    # (e.g. "Probabilidade de excedência") fit
                                    # within the half-page panel width.
                                    strip.text.x = ggplot2::element_text(size = 10),
                                    legend.key.height = ggplot2::unit(1,'cm'))


                },error = function(e) {
                  message(e$message)
                  return(NULL)
                })


                if(!is.null(one.static.plot)){
                  tmp.map.list[[tmp_measure]]=one.static.plot
                }
              }

              # key_measure_maps_tmp <- tryCatch({patchwork::wrap_plots(tmp.map.list, ncol = 2)
              # },error = function(e) {
              #   message(e$message)
              #   return(NULL)
              # })


              #single_model_maps_collection[[tmp.method]][[tmp.adm]] <- key_measure_maps_tmp
              single_model_maps_collection[[tmp.method]][[tmp.adm]] <- tmp.map.list


            }
          }

          #############################
          ### draw maps, ind model
          #############################

          for (i in seq_len(length(method_names))) {
            for (j in seq_len(length(selected_adms))) {

              tmp.method <- method_names[i]
              tmp.adm <- selected_adms[j]

              if(!fitted_model_matrix[i,j]==1|tmp.adm=='National'){next}

              grid.newpage()

              # FIX (issue 2): split the page title across two lines so long
              # translated phrases (e.g. "Cartes des statistiques clés : ...")
              # are not clipped at the right edge of the page.
              grid.text(tr("key_stats_maps", lang),
                        x = 0.05, y = 0.95, just = "left", gp = gpar(fontsize = 14))
              grid.text(paste0(method_match[tmp.method],
                               tr("model_at", lang),
                               tmp.adm,
                               tr("level_word", lang)),
                        x = 0.05, y = 0.91, just = "left", gp = gpar(fontsize = 12, fontface = "italic"))

              tryCatch({

                ### extract prepared maps to plot
                #key_measure_maps_grob_tmp <- patchwork::patchworkGrob(single_model_maps_collection[[tmp.method]][[tmp.adm]] )

                tmp.map.collect <- single_model_maps_collection[[tmp.method]][[tmp.adm]]

                ### specify configurations depends on number of statistics selected
                if(length(all.measures)==1){
                  key_stat_map_grob <- arrangeGrob(tmp.map.collect[[1]], nrow = 1)
                }

                if(length(all.measures)==2){
                  key_stat_map_grob <- arrangeGrob(tmp.map.collect[[1]],tmp.map.collect[[2]], nrow = 1)
                }

                if(length(all.measures)==3){
                  key_stat_map_grob <- arrangeGrob(
                    tmp.map.collect[[1]],  # Centered first row
                    arrangeGrob(tmp.map.collect[[2]], tmp.map.collect[[3]], ncol = 2),  # Second row with two plots
                    nrow = 2)
                }

                if(length(all.measures)==4){
                  key_measure_patch <- patchwork::wrap_plots(tmp.map.collect, ncol = 2)
                  key_stat_map_grob <- patchwork::patchworkGrob(key_measure_patch)

                }


                pushViewport(viewport(width = 0.9, height = 0.8, just = c("center", "center")))  # Set size within page

                #grid.draw(key_measure_maps_grob_tmp)
                grid.draw(key_stat_map_grob)

                upViewport()

              },error = function(e) {

                grid.text(tr("page_error", lang), x = 0.05, y = 0.1, just = "left", gp = gpar(fontsize = 12))

                message(e$message)

              })


              grid.text(paste0(page_counter), x = 0.5, y = 0.05, gp = gpar(fontsize = 12), just = "center")
              page_counter = page_counter +1


            }
          }


          ###############################################################
          ### Scatter plot comparison with direct estimates
          ###############################################################

          scatter_measure <- c('mean','CI.width')

          #############################
          ### prepare plot
          #############################

          scatter_plot_collection <- list()

          for (j in seq_len(length(selected_adms))) {
            for (i in seq_len(length(method_names))) {

              tmp.method <- method_names[i]
              tmp.adm <- selected_adms[j]

              ## do not make scatter plot if model not fitted
              if(!fitted_model_matrix[i,j]==1|tmp.adm=='National'|tmp.method=='Direct'){next}

              ## do not make scatter plot if direct model at this level is not fitted
              if(fitted_model_matrix[1,j]==0){next}

              tmp.method.x <- 'Direct'
              tmp.method.y <- tmp.method

              tmp_scatter_plot_list <- list()

              for(t in c(1:length(scatter_measure))){

                tmp_measure <- scatter_measure[t]
                label_x <- method_match[tmp.method.x]
                label_y <- method_match[tmp.method.y]

                scatter.static.tmp <- tryCatch({
                  scatter.plot( res.obj.x = model_res_all[[tmp.method.x]][[tmp.adm]],
                                res.obj.y = model_res_all[[tmp.method.y]][[tmp.adm]],
                                value.to.plot = tmp_measure,
                                model.gadm.level = admin_to_num(tmp.adm),
                                strata.gadm.level = CountryInfo$GADM_strata_level(),
                                label.x = label_x,
                                label.y = label_y,
                                plot.title=measure_match_nobreak[tmp_measure],
                                interactive=F)
                },error = function(e) {
                  message(e$message)
                  return(NULL)
                })


                if(!is.null(scatter.static.tmp)){
                  tmp_scatter_plot_list[[tmp_measure]]=scatter.static.tmp
                }

              }


              scatter_plot_collection[[tmp.method]][[tmp.adm]] <- tmp_scatter_plot_list


            }
          }

          #############################
          ### draw scatter plots
          #############################

          for (i in seq_len(length(method_names))) {
            for (j in seq_len(length(selected_adms))) {

              tmp.method <- method_names[i]
              tmp.adm <- selected_adms[j]

              tmp_scatter_plot_obj <- scatter_plot_collection[[tmp.method]][[tmp.adm]]

              if(is.null(tmp_scatter_plot_obj)){next}

              grid.newpage()

              # FIX (issue 2): split the page title across two lines so long
              # translated phrases (e.g. "Comparaison des modèles : ...")
              # are not clipped at the right edge of the page.
              grid.text(tr("model_comparison", lang),
                        x = 0.05, y = 0.95, just = "left", gp = gpar(fontsize = 14))
              grid.text(paste0(method_match[tmp.method],
                               tr("model_at", lang),
                               tmp.adm,
                               tr("level_word", lang)),
                        x = 0.05, y = 0.91, just = "left", gp = gpar(fontsize = 12, fontface = "italic"))

              tryCatch({

                ### extract prepared maps to plot
                #key_measure_maps_grob_tmp <- patchwork::patchworkGrob(single_model_maps_collection[[tmp.method]][[tmp.adm]] )

                tmp.scatter.collect <- scatter_plot_collection[[tmp.method]][[tmp.adm]]

                ### specify configurations depends on number of statistics selected
                if(length(scatter_measure)==1){
                  scatter_plot_grob <- arrangeGrob(tmp.scatter.collect[[1]], nrow = 1)
                }

                if(length(scatter_measure)==2){
                  scatter_plot_grob <- arrangeGrob(tmp.scatter.collect[[1]],tmp.scatter.collect[[2]], nrow = 1)
                }

                if(length(scatter_measure)==3){
                  scatter_plot_grob <- arrangeGrob(
                    tmp.scatter.collect[[1]],  # Centered first row
                    arrangeGrob(tmp.scatter.collect[[2]], tmp.scatter.collect[[3]], ncol = 2),  # Second row with two plots
                    nrow = 2)
                }

                pushViewport(viewport(width = 0.9, height = 0.8, just = c("center", "center")))  # Set size within page

                #grid.draw(key_measure_maps_grob_tmp)
                grid.draw(scatter_plot_grob)

                upViewport()

              },error = function(e) {

                grid.text(tr("page_error", lang), x = 0.05, y = 0.1, just = "left", gp = gpar(fontsize = 12))

                message(e$message)

              })


              grid.text(paste0(page_counter), x = 0.5, y = 0.05, gp = gpar(fontsize = 12), just = "center")
              page_counter = page_counter +1


            }
          }



        }




        session$sendCustomMessage('controlSpinner', list(action = "hide"))






        # Close the PDF device
        grDevices::dev.off()



      }
    )


  })
}

## To be copied in the UI
# mod_report_preparation_ui("report_preparation_1")

## To be copied in the server
# mod_report_preparation_server("report_preparation_1")
