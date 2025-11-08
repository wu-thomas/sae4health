###############################################################
### get cluster.info and admin.info for any admin level
###############################################################

#' @description Prepare cluster.info and admin.info for further analysis
#'
#' @param cluster.geo cluster GPS from DHS
#'
#' @param gadm.list list of GADM shapefiles, with names ('National','Admin-1',...)
#'
#' @param strata.gadm.level which GADM admin level is used as the stratification in DHS
#'
#' @param model.gadm.level which GADM admin level to produce results for
#'
#' @return list(cluster.info, admin.info)
#'
#' @noRd

cluster_admin_info <- function(cluster.geo,
                               gadm.list,
                               model.gadm.level,
                               strat.gadm.level=1){


  if (!requireNamespace("survey", quietly = TRUE)) {
    stop("Package 'survey' is required for this function. Please install it with install.packages('survey').")
  }

  ### determine whether the gadm level is finer than stratification level
  if(model.gadm.level > strat.gadm.level){pseudo_level=2}else{pseudo_level=1}

  ###############################################################
  ### National level analysis
  ###############################################################

  if(model.gadm.level==0){

    ### cluster.info object
    cluster.info <- surveyPrev::clusterInfo(geo=cluster.geo,
                                            poly.adm1=gadm.list[[paste0('Admin-',1)]],
                                            poly.adm2=gadm.list[[paste0('Admin-',1)]],
                                            by.adm1 = paste0("NAME_",1),
                                            by.adm2 = paste0("NAME_",1))

    # admin.info object
    admin.info <- NULL
  }else{


    ######################################################################
    ### <= Admin-1 level analysis (not finer than stratification level)
    ######################################################################

    ### pseudo_level = Admin-1
    if(pseudo_level==1){

      # cluster.info object
      cluster.info <- surveyPrev::clusterInfo(geo=cluster.geo, ## same admin for two levels, since no need for information on upper admin region
                                              poly.adm1=gadm.list[[paste0('Admin-',model.gadm.level)]],
                                              poly.adm2=gadm.list[[paste0('Admin-',model.gadm.level)]],
                                              by.adm1 = paste0("NAME_",model.gadm.level),
                                              by.adm2 = paste0("NAME_",model.gadm.level)
      )

      # admin.info object
      admin.info <- surveyPrev::adminInfo(poly.adm = gadm.list[[paste0('Admin-',model.gadm.level)]],
                                          admin = pseudo_level,
                                          by.adm= paste0("NAME_",model.gadm.level))

    }


    ######################################################################
    ### >= Admin-2 level analysis (finer than stratification level)
    ######################################################################

    if(pseudo_level==2){

      # cluster.info object
      cluster.info <- surveyPrev::clusterInfo(geo=cluster.geo,## incorporate both this level and level above for information on upper admin region
                                              poly.adm1=gadm.list[[paste0('Admin-',strat.gadm.level)]],
                                              poly.adm2=gadm.list[[paste0('Admin-',model.gadm.level)]],
                                              by.adm1 = paste0("NAME_",strat.gadm.level),
                                              by.adm2 = paste0("NAME_",model.gadm.level)
      )

      # admin.info object
      admin.info <- surveyPrev::adminInfo(poly.adm = gadm.list[[paste0('Admin-',model.gadm.level)]],
                                          admin = pseudo_level,
                                          by.adm= paste0("NAME_",model.gadm.level),
                                          by.adm.upper = paste0("NAME_",strat.gadm.level))

    }

  }


  return.obj <- list('cluster.info'=cluster.info,
                     'admin.info'=admin.info)


}



### example
if(FALSE){
  tmp.cluster1 <- cluster_admin_info(cluster.geo= mdg.ex.GPS,  #mdg.ex.GPS
                                     gadm.list = mdg.ex.GADM.list,  #mdg.ex.GADM.list
                                     model.gadm.level = 0,
                                     strat.gadm.level = 2)
}



###############################################################
### conduct initial screening a model with any method
###############################################################

#' @description Screening on
#'
#' @param cluster.geo cluster GPS from DHS
#'
#' @param gadm.list list of GADM shapefiles, with names ('National','Admin-1',...)
#'
#' @param strata.gadm.level which GADM admin level is used as the stratification in DHS
#'
#' @param model.gadm.level which GADM admin level to produce results for
#'
#' @return list(cluster.info, admin.info)
#'
#' @noRd
#'
screen_svy_model <- function(cluster.admin.info,
                             analysis.dat,
                             model.gadm.level,
                             strat.gadm.level = 1,
                             method=c('Direct','FH','Unit')[1],
                             svy.strata=NULL){



  if (!requireNamespace("survey", quietly = TRUE)) {
    stop("Package 'survey' is required for this function. Please install it with install.packages('survey').")
  }

  ### return object
  N.region <- NA
  N.region.no.data <- NA
  N.region.invalid.direct.se <- NA

  screen.flag <- 'Clear'
  screen.message <- 'Model is ready to be implemented.'

  ### determine whether the gadm level is finer than stratification level
  if(model.gadm.level > strat.gadm.level){pseudo_level=2}else{pseudo_level=1}

  ###############################################################
  ### National level analysis
  ###############################################################

  if(model.gadm.level==0){

    N.region <- 1
    N.region.no.data <- 0
    N.region.invalid.direct.se <- 0

    #screen.flag <- 'Clear'
    #screen.message <- NA


  }


  ###############################################################
  ### subnational level analysis
  ###############################################################

  if(model.gadm.level!=0) {

    ### extract cluster.info and admin.info
    cluster.info <- cluster.admin.info$cluster.info
    admin.info <- cluster.admin.info$admin.info

    ### extract number of regions and number of regions without data
    N.region <- dim(admin.info$data)[1]
    N.region.no.data <- N.region-length(unique(cluster.info$data$admin2.name.full))


    ### calculate direct estimate
    res.direct <- surveyPrev::directEST(data = analysis.dat,
                                        cluster.info = cluster.info,
                                        admin = pseudo_level,
                                        admin.info = admin.info,
                                        aggregation = F,
                                        alt.strata=svy.strata)


    ### examine direct estimates
    if(pseudo_level==1){

      bad_admins <- subset(res.direct$res.admin1, direct.var < 1e-30|is.na(direct.var)|direct.var==Inf)$admin1.name
      N.region.invalid.direct.se <- length(bad_admins)+N.region.no.data
    }


    if(pseudo_level==2){

      bad_admins <- subset(res.direct$res.admin2, direct.var < 1e-30|is.na(direct.var)|direct.var==Inf)$admin2.name.full
      N.region.invalid.direct.se <- length(bad_admins)+N.region.no.data
    }



  }


  ###############################################################
  ### screening check, various warning messages
  ###############################################################
  if(N.region.invalid.direct.se/N.region > 0.25 & method=='Direct'){

    missing.percent <- round((N.region.invalid.direct.se/N.region)*100,digits=1)
    message(paste0(missing.percent,'% of regions do not have valid direct estimates.'))

    screen.flag <- 'Warning'
    #screen.message <- paste0('Sparse data: ',missing.percent,'% of regions do not have direct estimates with valid uncertainties.')
    screen.message <- paste0('Data is not present/sparse in ',missing.percent,'% of areas.')

  }

  if(N.region.invalid.direct.se/N.region > 0.25 & method=='FH'){

    missing.percent <- round((N.region.invalid.direct.se/N.region)*100,digits=1)
    message(paste0(missing.percent,'% of regions do not have valid direct estimates.'))

    screen.flag <- 'Warning'
    #screen.message <- paste0('Sparse data: ',missing.percent,'% of regions do not have direct estimates with valid uncertainties.')
    screen.message <- paste0('Data is not present/sparse in ',missing.percent,'% of areas, so model will not be fitted.')

  }

  #missing.percent <- round((N.region.no.data/N.region)*100,digits=1)
  #message(paste0(missing.percent,'% of regions do not have data.'))

  if(N.region.no.data/N.region > 0.25 & method=='Unit'){

    missing.percent <- round((N.region.no.data/N.region)*100,digits=1)
    message(paste0(missing.percent,'% of regions do not have data.'))

    screen.flag <- 'Warning'
    #screen.message <- paste0('Sparse data: ',missing.percent,'% of regions do not have direct estimates with valid uncertainties.')
    screen.message <- paste0('No data in ',missing.percent,'% of areas, so advice against fitting model.')

  }

  ### prepare return object

  return.obj <- list('N.region'= N.region,
                     'N.region.no.data' = N.region.no.data,
                     'N.region.invalid.direct.se' =N.region.invalid.direct.se,
                     'screen.flag' = screen.flag,
                     'screen.message' = screen.message)



  return(return.obj)


}






### example
if(FALSE){
  model.gadm.level = 1
  strat.gadm.level = 2

  tmp.analysis.dat <- surveyPrev::getDHSindicator(Rdata=mdg.ex.KR.dat,
                                                  indicator = 'CH_VACC_C_NON')

  tmp.svy.info <- cluster_admin_info(cluster.geo= mdg.ex.GPS,  #mdg.ex.GPS
                                     gadm.list = mdg.ex.GADM.list,  #mdg.ex.GADM.list
                                     model.gadm.level = model.gadm.level,
                                     strat.gadm.level = strat.gadm.level)

  tmp.screen <- screen_svy_model(cluster.admin.info=tmp.svy.info,
                                 analysis.dat=tmp.analysis.dat,
                                 model.gadm.level=model.gadm.level,
                                 strat.gadm.level = strat.gadm.level)


}



###############################################################
### function to fit a model with any method
###############################################################

#' fit model using functions in surveyPrev
#'
#' @description A fct function
#'
#' @param cluster.geo cluster GPS from DHS
#'
#' @param gadm.list list of GADM shapefiles, with names ('National','Admin-1',...)
#'
#' @param analysis.dat analysis dataset for the indicator
#'
#' @param strata.gadm.level which GADM admin level is used as the stratification in DHS
#'
#' @param model.gadm.level which GADM admin level to produce results for
#'
#' @param aggregation whether to conduct aggregation to upper national levels
#'
#' @param method %in% c('Direct','FH','Unit')
#'
#' @param nested whether to fit Admin-2 (or finer) model nested within Admin-1, default is FALSE.
#'
#' @return fitted surveyPrev
#'
#' @noRd


#cluster.geo <- mdg.ex.GPS
#gadm.list <- mdg.ex.GADM.list
#analysis.dat <- surveyPrev::getDHSindicator(Rdata=mdg.ex.KR.dat,indicator = 'CH_VACC_C_NON')

#model.gadm.level <- 1
#strat.gadm.level <- 2

fit_svy_model <- function(cluster.geo,
                          cluster.admin.info=NULL,
                          gadm.list,
                          analysis.dat,
                          model.gadm.level,
                          strat.gadm.level=1,
                          aggregation=T,
                          method=c('Direct','FH','Unit')[1],
                          svy.strata = NULL,
                          nested=F,
                          area_cov_frame=NULL){



  if (!requireNamespace("survey", quietly = TRUE)) {
    stop("Package 'survey' is required for this function. Please install it with install.packages('survey').")
  }

  aggregation = F
  process.info=T
  nsamp=1000



  ### whether to use processed cluster.info and admin.info object
  if(!is.null(cluster.admin.info)){
    cluster.info <- cluster.admin.info$cluster.info
    admin.info <- cluster.admin.info$admin.info
    process.info=F
    message('use processed cluster and admin info')
  }

  if(model.gadm.level==0){

    if(method=='Direct'){
      if(process.info){
        cluster.info <- surveyPrev::clusterInfo(geo=cluster.geo,
                                                poly.adm1=gadm.list[[paste0('Admin-',1)]],
                                                poly.adm2=gadm.list[[paste0('Admin-',1)]],
                                                by.adm1 = paste0("NAME_",1),
                                                by.adm2 = paste0("NAME_",1))
      }



      res_adm <- tryCatch({
        # First attempt with alt.strata='v022'
        surveyPrev::directEST(data = analysis.dat,
                              cluster.info = cluster.info,
                              admin = 0,
                              strata = "all",
                              alt.strata = 'v022')
      }, error = function(e) {
        # If the first attempt fails, try with alt.strata=svy.strata
        tryCatch({
          surveyPrev::directEST(data = analysis.dat,
                                cluster.info = cluster.info,
                                admin = 0,
                                strata = "all",
                                alt.strata = NULL)
        }, error = function(e) {
          # If both attempts fail, set res_adm to NULL
          NULL
        })
      })
#
#
#       res_adm <- surveyPrev::directEST(data=analysis.dat,
#                                        cluster.info= cluster.info,
#                                        admin=0,
#                                        strata="all",
#                                        alt.strata=svy.strata)

      ### draw samples using logit.est and logit.var
      sampled.post.vec <- SUMMER::expit(stats::rnorm(nsamp, mean = res_adm$res.natl$direct.logit.est,
                                              sd = sqrt(res_adm$res.admin0$direct.logit.var)))
      sampled.post.mat <- matrix(sampled.post.vec, nrow = nsamp)
      res_adm$admin0_post <- sampled.post.mat

      return(res_adm)

    }else{stop("Only direct estimates can be computed at National level.")}

  }


  ### determine whether the gadm level is finer than stratification level
  if(model.gadm.level > strat.gadm.level){pseudo_level=2}else{pseudo_level=1}


  if(pseudo_level==2 && aggregation==T){
    message('Currectly not supporting aggregation for fine spatial resolution.')
    aggregation = F
  }

  ### for levels not finer than stratification level

  if(pseudo_level==1){

    ### define cluster level
    if(process.info){
      cluster.info <- surveyPrev::clusterInfo(geo=cluster.geo,
                                              poly.adm1=gadm.list[[paste0('Admin-',model.gadm.level)]],
                                              poly.adm2=gadm.list[[paste0('Admin-',model.gadm.level)]],
                                              by.adm1 = paste0("NAME_",model.gadm.level),
                                              by.adm2 = paste0("NAME_",model.gadm.level))
    }


    if(aggregation){
      ### find aggregation weights using survey
      agg.survey <- surveyPrev::aggSurveyWeight(data = analysis.dat,
                                                cluster.info = cluster.info,
                                                admin = pseudo_level)

      ### define admin information
      admin.info <- surveyPrev::adminInfo(poly.adm = gadm.list[[paste0('Admin-',model.gadm.level)]],
                                          admin = pseudo_level,
                                          by.adm= paste0("NAME_",model.gadm.level),
                                          agg.pop =agg.survey)

    }else{
      if(process.info){
        admin.info <- surveyPrev::adminInfo(poly.adm = gadm.list[[paste0('Admin-',model.gadm.level)]],
                                            admin = pseudo_level,
                                            by.adm= paste0("NAME_",model.gadm.level))
      }
    }

  }

  ### for levels finer than stratification level

  if(pseudo_level==2){

    aggregation = F

    if(process.info){

      cluster.info <- surveyPrev::clusterInfo(geo=cluster.geo,
                                              poly.adm1=gadm.list[[paste0('Admin-',model.gadm.level-1)]],
                                              poly.adm2=gadm.list[[paste0('Admin-',model.gadm.level)]],
                                              by.adm1 = paste0("NAME_",model.gadm.level-1),
                                              by.adm2 = paste0("NAME_",model.gadm.level)
      )

      ### when need aggregation to stratification, use the following
      #cluster.info.tmp <- surveyPrev::clusterInfo(geo=tmp.geo,
      #                                            poly.adm1=gadm.list[[paste0('Admin-',strata.level)]],
      #                                            poly.adm2=gadm.list[[paste0('Admin-',model.gadm.level)]],
      #                                            by.adm1 = paste0("NAME_",strata.level),
      #                                            by.adm2 = paste0("NAME_",model.gadm.level)
      #)

      admin.info <- surveyPrev::adminInfo(poly.adm = gadm.list[[paste0('Admin-',model.gadm.level)]],
                                          admin = pseudo_level,
                                          by.adm= paste0("NAME_",model.gadm.level),
                                          by.adm.upper = paste0("NAME_",model.gadm.level-1))

    }


  }



  ### direct estimates, possibly with aggregation to natl and strata level (D:admin-1)
  if(method=='Direct'){
    message('Disabling aggregation for direct estimates.')

    #Some countries the stratification level is admin 0.5, causing admin-1 to have invalid uncertainty measures
    #Aggregation of direct estimates involves sampling
    # Error in draw.all= expit(apply(dd, 1, FUN = function(x) stats::rnorm(5000, mean = x[1], sd = x[2]))) # sqrt(colVars(draw.all))
    # Example, dominican republic 2013

    aggregation = F

    res_adm <- surveyPrev::directEST(data = analysis.dat,
                                     cluster.info = cluster.info,
                                     admin = pseudo_level,
                                     weight = "population",
                                     admin.info = admin.info,
                                     aggregation = aggregation,
                                     alt.strata=svy.strata)

    ### draw samples
    if(pseudo_level==1){
      res_summary <- res_adm$res.admin1
    }else{
      res_summary <- res_adm$res.admin2
    }

    sample_matrix <- matrix(nrow = nsamp, ncol = nrow(res_summary))

    ### draw samples for all regions
    for (i in 1:nrow(res_summary)) {

      # extract mean and sd (logit)
      mu <- res_summary$direct.logit.est[i]
      sigma <- sqrt(res_summary$direct.logit.var[i])

      # sample if uncertainty not NA
      if(is.na(sigma)){
        sample_matrix[, i] <- NA
      }else{
        samples <- SUMMER::expit(stats::rnorm(nsamp, mean = mu, sd = sigma))
        sample_matrix[, i] <- samples
      }

    }

    if(pseudo_level==1){
      res_adm[['admin1_post']] <- sample_matrix
    }else{
      res_adm[['admin2_post']] <- sample_matrix
    }


    if(pseudo_level==2){

      ### identify bad cluster
      bad_admins <- subset(res_adm$res.admin2, direct.var < 1e-30|is.na(direct.var)|direct.var==Inf)$admin2.name.full

      if(length(bad_admins)>0){

        missing.percent <- round((length(bad_admins)/dim(res_adm$res.admin2)[1])*100,digits=1)
        message(paste0(missing.percent,'% of regions do not have valid direct estimates.'))
        res_adm$warning <- paste0(missing.percent,'% of regions do not have valid estimates.')

      }
    }

  }


  ### F-H estimates, possibly with aggregation to natl and strata level (D:admin-1)

  if(method=='FH'){

    res_direct <- surveyPrev::directEST(data = analysis.dat,
                                        cluster.info = cluster.info,
                                        admin = pseudo_level,
                                        weight = "population",
                                        admin.info = admin.info,
                                        aggregation = F,
                                        alt.strata=svy.strata)

    if(pseudo_level==1){
      bad_admins <- subset(res_direct$res.admin1, direct.var < 1e-30|is.na(direct.var)|direct.var==Inf)$admin1.name

      ### remove bad clusters
      updated.analysis.dat <- analysis.dat

      if(length(bad_admins)>0){
        if((length(bad_admins)/
            dim(res_direct$res.admin1)[1])>0.25){
          message((length(bad_admins)/dim((res_direct$res.admin1)[1])))
          stop("More than 25% of regions do not have valid direct estimates.") #25%
        }

        bad_clusters <- subset(cluster.info$data, admin1.name %in% bad_admins)$cluster

        updated.analysis.dat <-subset(analysis.dat, !cluster %in% bad_clusters)

      }


      res_adm <- surveyPrev::fhModel(data= updated.analysis.dat,
                                     cluster.info = cluster.info,
                                     admin.info = admin.info,
                                     admin = pseudo_level,
                                     model = "bym2",
                                     aggregation =aggregation,
                                     alt.strata=svy.strata,
                                     X=area_cov_frame)

    }

    if(pseudo_level==2){

      ### identify bad cluster
      bad_admins <- subset(res_direct$res.admin2, direct.var < 1e-30|is.na(direct.var)|direct.var==Inf)$admin2.name.full

      ### remove bad clusters
      updated.analysis.dat <- analysis.dat

      if(length(bad_admins)>0){
        if((length(bad_admins)/
            dim(res_direct$res.admin2)[1])>0.25){
          message((length(bad_admins)/dim((res_direct$res.admin2)[1])))
          stop("More than 25% of regions do not have valid direct estimates.") #25%
        }

        bad_clusters <- subset(cluster.info$data, admin2.name.full %in% bad_admins)$cluster



        #message(bad_clusters)
        updated.analysis.dat <-subset(analysis.dat, !cluster %in% bad_clusters)

        #manually adjust only PSU, remove
        #updated.analysis.dat <- updated.analysis.dat %>%
        #  dplyr::group_by(v024) %>%
        #  dplyr::filter(dplyr::n_distinct(cluster) > 1) %>%
        #  dplyr::ungroup()

      }

      ### fit FH model
      #options(survey.lonely.psu="remove")
      #options(survey.adjust.domain.lonely=TRUE)

      res_adm <- surveyPrev::fhModel(data = updated.analysis.dat,
                                     cluster.info = cluster.info,
                                     admin.info = admin.info,
                                     admin = pseudo_level,
                                     model = "bym2",
                                     aggregation =aggregation,
                                     alt.strata=svy.strata,
                                     X=area_cov_frame)
    }


  }

  ### unit-level model estimates, possibly with aggregation to natl

  if(method=='Unit'){

    ### proportion of regions no data, (25% as cutoff)
    region_no_dat <- 1-length(unique(cluster.info$data$admin2.name.full))/dim(admin.info$mat)[1]
    #if(analysis.dat$)

    if(region_no_dat> 0.25){
      #stop("More than 25% of regions"," have no data. Model not fitted due to data sparsity.")
      message(paste0(round(region_no_dat*100,digits=0), "% of regions"," have no data. Model not fitted due to data sparsity."))

      #stop(paste0("Too many admin regions (",dim(admin.info$mat)[1],") to yield meaningful results."))
    }


    res_adm <- surveyPrev::clusterModel(data=analysis.dat,
                                        cluster.info= cluster.info,
                                        admin.info = admin.info,
                                        model = "bym2",
                                        stratification =FALSE,
                                        admin = pseudo_level,
                                        aggregation = aggregation,
                                        CI = 0.95,
                                        nested=nested,
                                        X=area_cov_frame)
  }


  ### return estimates
  return(res_adm)

}


if(FALSE){
  test.res <- fit_svy_model(cluster.admin.info=tmp.svy.info,
                            gadm.list=mdg.ex.GADM.list,
                            analysis.dat=tmp.analysis.dat,
                            model.gadm.level=1,
                            strat.gadm.level=2,
                            aggregation=T,
                            method=c('Direct','FH','Unit')[1])
}

###############################################################
### test assigning results
###############################################################

if(FALSE){
  reactiveConsole(TRUE)

  CountryInfo <- CountryInfo$new()
  AnalysisInfo <- AnalysisInfo$new()

  ### initialize Zambia example

  # Kenya meta info

  if(FALSE){

    ### country meta
    ex.country <- 'Kenya'
    ex.svy.year <- '2022'

    ### survey indicator meta
    ex.indicator.abbrev <-'FP_CUSA_W_MOD'
    recode_names_list <-'Individual Recode'
    recode_for_ind_abbrev()
    #file_path <-'C:/Users/wu-th/Downloads/KE_2022_DHS_04132024_852_143411.zip'

    CountryInfo$country(ex.country)
    CountryInfo$svyYear_list(ex.svy.year)

    country_GADM <- get_country_GADM(ex.country)
    CountryInfo$GADM_list(country_GADM)

    for (i in 1:length(recode_names_list)){
      file_prefix <- find_DHS_dat_name(ex.country,ex.svy.year,recode =recode_names_list[i])

      recode_path_found <- find_recode_path(file_path = file_path,
                                            recode_file =file_prefix,
                                            extensions = 'DTA')

      recode.data <- suppressWarnings(haven::read_dta(recode_path_found))

      recode.data <- as.data.frame(recode.data)

      CountryInfo$update_svy_dat(recode_abbrev=recode_for_ind_abbrev()[i], new_dat=recode.data)


    }




  }


  # Zambia meta info
  if(FALSE){
    CountryInfo$country('Zambia')
    CountryInfo$svyYear_list('2018')
    CountryInfo$GADM_list(zmb.ex.GADM.list)
    CountryInfo$svy_indicator_var('RH_ANCN_W_N4P')
    strat.gadm.level <- 1

    # analysis data info
    recode.data <- as.data.frame(zmb.ex.IR.dat)
    CountryInfo$update_svy_dat(recode_abbrev='IR', new_dat=recode.data)
    GPS.dat <- zmb.ex.GPS
    CountryInfo$svy_GPS_dat(GPS.dat)
    GPS.dat <- CountryInfo$svy_GPS_dat()


    svy_dat_list <- CountryInfo$svy_dat_list()
    svy_dat_recode <- svy_dat_list[['IR']]
  }

  ### initialize Madagascar example

  # Madagascar meta info
  if(FALSE){
    CountryInfo$country('Madagascar')
    CountryInfo$svyYear_list('2021')
    CountryInfo$GADM_list(mdg.ex.GADM.list)
    CountryInfo$svy_indicator_var('CH_VACC_C_NON')
    strat.gadm.level <- 2

    # analysis data info
    recode.data <- as.data.frame(mdg.ex.KR.dat)
    CountryInfo$update_svy_dat(recode_abbrev='KR', new_dat=recode.data)
    GPS.dat <- mdg.ex.GPS
    CountryInfo$svy_GPS_dat(GPS.dat)
    GPS.dat <- CountryInfo$svy_GPS_dat()


    svy_dat_list <- CountryInfo$svy_dat_list()
    svy_dat_recode <- svy_dat_list[['KR']]
  }



  analysis_dat <- surveyPrev::getDHSindicator(Rdata=svy_dat_recode,
                                              indicator = CountryInfo$svy_indicator_var())

  CountryInfo$svy_analysis_dat(analysis_dat)
  analysis_dat <- CountryInfo$svy_analysis_dat()


  ### initialize dimensions for storage of analysis results

  #row_names <- c("Direct", "FH", "Unit")
  #nrows <- length(row_names)

  #col_names <- reactive({ CountryInfo$GADM_analysis_levels()    })
  #ncols <- reactive({ length(col_names()) })




  ### initialize analysis related, once new country/survey/indicator is set

  #tmp.meta <- meta_snapshot()

  #n_admin <- length(CountryInfo$GADM_list())
  #adm_level_names <- names(CountryInfo$GADM_list())

  res_list <- list()
  res_tracker_list <- list()

  AnalysisInfo$model_res_list(res_list)
  AnalysisInfo$model_res_tracker_list(res_tracker_list)

  ### tryout model

  col_names <- names(CountryInfo$GADM_list())

  res_tracker_list <- AnalysisInfo$model_res_tracker_list()

  for (tmp.adm in col_names){

    tmp.adm.num <- admin_to_num(tmp.adm)

    for(tmp.method in c('Direct','FH','Unit')){

      message('Modelling at ',tmp.adm,' using ',tmp.method,' model.')


      tmp.tracker.list <- res_tracker_list[[tmp.method]][[tmp.adm]]
      if(!is.null(tmp.tracker.list$status)){

        message('Skip. Already tried modelling at Admin-',tmp.adm,' using ',tmp.method,' model.')

        next
      }


      ### set model fitting status to Successful, assuming no error occurs
      tmp.tracker.list$status <- 'Successful'
      tmp.tracker.list$message <- 'Successful'

      ### Run model
      tmp.res <- tryCatch(
        {
          #R.utils::withTimeout({
          tmp.res <- fit_svy_model(cluster.geo= CountryInfo$svy_GPS_dat(),  #mdg.ex.GPS
                                   gadm.list = CountryInfo$GADM_list(),  #mdg.ex.GADM.list
                                   analysis.dat =   CountryInfo$svy_analysis_dat(),
                                   model.gadm.level = tmp.adm.num,
                                   strat.gadm.level = strat.gadm.level,
                                   method = tmp.method,
                                   aggregation =T

          )
          #}, timeout = 300) ### 5 minutes for timeout
        },error = function(e) {
          tmp.tracker.list$status <<- 'Unsuccessful'

          if(inherits(e, "TimeoutException")) {
            message("The operation timed out!")
            tmp.tracker.list$message <<- 'Timed out. Took too long to fit the model.'

          } else {
            tmp.tracker.list$message <<- e$message
            message(e$message)
          }
          return(NULL)
        }
      )



      ### store model results
      AnalysisInfo$set_track_res(tmp.method,tmp.adm,tmp.tracker.list)

      AnalysisInfo$set_fitted_res(tmp.method,tmp.adm,tmp.res)



    }

  }

  examine.tracker <- AnalysisInfo$model_res_tracker_list()
  examine.res <- AnalysisInfo$model_res_list()

  #### save Madagascar example
  #mdg.ex.model.res <- examine.res
  #mdg.ex.res.tracker <- examine.tracker

  #save(mdg.ex.model.res,file='mdg_example_model_results.rda')
  #save(mdg.ex.res.tracker,file='mdg_example_model_tracker.rda')

  #AnalysisInfo$set_fitted_res('Direct','Admin-1',';yes')
  #AnalysisInfo$model_res_list()

  #examine.res[['Unit']][['Admin-3']]$res.admin2[1:10,]


}







###############################################################
### national U/R ratio
###############################################################



get_natl_UR_OR <- function(tmp.analysis.dat){

  if(sum(is.na(tmp.analysis.dat$value))>0){
    tmp.analysis.dat <- tmp.analysis.dat[rowSums(is.na(tmp.analysis.dat)) == 0, ]
    #message("Removing NAs in indicator response")
  }


  tmp.analysis.dat <- dplyr::mutate(tmp.analysis.dat, strata = dplyr::case_when(
    strata == "urban" ~ 1,
    strata == "rural" ~ 0,
    TRUE ~ NA_real_  # Sets to NA for any other case
  ))

  tmp.analysis.dat$value <- as.integer(tmp.analysis.dat$value)

  tmp.dhs.design <- survey::svydesign(id = ~cluster, weights = ~weight, strata = ~v024,
                                      nest = TRUE, survey.lonely.psu = "adjust", data = tmp.analysis.dat)

  tmp.glm.model <- survey::svyglm(value ~ strata, family = stats::quasibinomial , design = tmp.dhs.design)
  summary(tmp.glm.model)

  coef_model <- stats::coef(tmp.glm.model)

  odds_ratio_strata <- exp(coef_model["strata"])

  # Calculate the confidence intervals for the model coefficients
  confint_model <- stats::confint(tmp.glm.model, level = 0.95)

  # Confidence intervals for the odds ratio of 'strata'
  confint_strata <- exp(confint_model["strata", ])

  return(c(odds_ratio_strata, confint_strata[1], confint_strata[2]))
}

#get_natl_UR_OR(tmp.analysis.dat)
#formatted_output <- sprintf("%.2f(%.2f, %.2f)", odds_ratio_strata, confint_strata[1], confint_strata[2])


#res_ad0 <- surveyPrev::directEST(data=tmp.analysis.dat,
#                     cluster.info= tmp.cluster.info,
#                     admin=1,aggregation = F,
#                     strata="all")
