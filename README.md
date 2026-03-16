
<!-- README.md is generated from README.Rmd. Please edit that file -->

# sae4health

<!-- badges: start -->

[![CRAN_Status_Badge](https://www.r-pkg.org/badges/version/sae4health)](https://cran.r-project.org/package=sae4health)
[![](https://cranlogs.r-pkg.org/badges/sae4health)](https://cran.r-project.org/package=sae4health)
[![](https://cranlogs.r-pkg.org/badges/grand-total/sae4health?color=orange)](https://cran.r-project.org/package=sae4health)
<!-- badges: end -->

Small Area Estimation for Key Health and Demographic Indicators from
Household Surveys

## Overview

The ‘sae4health’ package enables small area estimation (SAE) of health
and demographic indicators in low- and middle-income countries (LMICs).
It powers an R Shiny app that helps public health analysts,
policymakers, and researchers generate subnational estimates and
prevalence maps for 150+ binary indicators from Demographic and Health
Surveys (DHS). Basing its core SAE analysis workflow on the ‘surveyPrev’
package, the app ensures methodological rigor through guided model
selection, automated fitting, and interactive visualization. For more
details, visit our
<a href="https://wu-thomas.github.io/surveyPrev_website/"
target="_blank">official website</a> for detailed documentation.

This software is for research purpose only and is licensed under the
terms specified in the [LICENSE
file](https://github.com/wu-thomas/sae4health/blob/main/LICENSE.md).

The Github repo maintains the source code for this R Shiny application
and it is structured as an R package. There are two primary venues to
utilize this application:

1.  **Web-Based Version**:

- Location: Deployed on an online server.
- Requirements: Internet access and a web browser.
- Usage: Ideal for users who prefer quick access without local setup.

2.  **Local R Package Installation**:

- Location: on user’s machine, launched by RStudio.
- Requirements: R environment and dependent packages.
- Usage: Suitable for users who wish to run the application directly
  within their local R environment. This method allows for enhanced
  customization.

## Web-based R Shiny app

The R Shiny app is readily accessible via
<https://rsc.stat.washington.edu/surveyPrevRShiny/>. The only
requirement is a stable internet connection. This web-based deployment
supports full functionality and serves as the primary distribution
channel.

One advantage of the online version is that it includes preloaded
shapefiles, reducing the need for manual uploads. While all versions
require users to upload their own DHS survey datasets for analysis,
currently the online version internally hosts the 2018 Nigeria DHS, 2022
Kenya DHS, and 2023 Senegal DHS (**without the need for data upload!**),
with more datasets planned for future inclusion.

## Installation of the R Shiny app as a R package

Some non-CRAN dependencies can be installed using the following command.
We strongly recommend installing the most recent version of SUMMER and
surveyPrev package from Github.

``` r
# install.packages("devtools")
devtools::install_github("richardli/SUMMER")
devtools::install_github("richardli/surveyPrev")

install.packages("INLA",repos=c(getOption("repos"),
                        INLA="https://inla.r-inla-download.org/R/testing"),dep=TRUE)
```

<!-- To install additional CRAN dependencies, run the following command:   -->
<!-- (Some dependencies are not listed in the CRAN `Imports` to meet submission requirements, so they need to be installed manually.) -->
<!-- devtools::install_github("rspatial/geodata") # for downloading GAM shapefile -->

You can then install the development version of sae4health with:

``` r
devtools::install_github("wu-thomas/sae4health")
```

<!-- Our tool depends specifically on 2.12.0 version of the labelled package, so we make sure the correct version is used.  -->
<!-- ``` {r, eval=F, echo=T} -->
<!-- remotes::install_version("labelled", "2.12.0") -->
<!-- ``` -->

Processing the analysis dataset for indicators, using specific coding
schemes and handling labels, requires the following libraries to be
**preloaded** before running the app:

``` r
library(labelled) ## requires version 2.12.0 or earlier
library(naniar)
library(sjlabelled)
library(dplyr)
library(data.table)
library(haven)
```

You can launch the R Shiny app with the following command, and we
recommend opening a new browser tab from within the launched R Shiny app
for better accessing web links.

``` r

library(sae4health)
sae4health::run_app()
```

If the colors in the interactive Leaflet maps appear inverted relative
to the legend (e.g., regions with high values in the legend are shown in
low-value colors), you can run the app with the following option. This
issue arises due to differences in R and Leaflet package versions and is
beyond our control.

``` r

sae4health::run_app(legend_color_reverse=T)
```

For WHO workshop attendees only:  
If you have received the WHO shapefile distributed by WHO officials for
this technical workshop and are using the WHO version of the app, please
run the following version:

``` r

sae4health::run_app(version='DHS-WHO')
```

<!-- ## Deploy the RShiny app as a Docker image -->
<!-- The user need to first install the Docker desktop app from [https://docs.docker.com/get-docker/](https://docs.docker.com/get-docker/). -->
<!-- Windows users may need to enable the Windows Subsystem for Linux (WSL), which can be done by executing the ‘wsl  --install' in the command line. -->
<!-- Several methods are available for deploying this RShiny app locally using Docker. Special thanks to \@Charlton Callender for providing these deployment techniques. -->
<!-- ### Docker desktop app -->
<!-- 1. Open the Docker desktop app -->
<!-- 2. Download the image: -->
<!--     - This is only required the first time you use the app or for downloading a new version/tag. -->
<!--     - In the search bar, search for 'yunhanwu/saeforhealth:v1.0.1'. -->
<!--     - Hover cursor over relevant result and click 'Pull'. -->
<!-- 3. Run a container with the downloaded image: -->
<!--     - From sidebar, open 'Images' -> 'Local'. Should now have a row for 'Name'='yunhanwu/saeforhealth' and 'Tag'='v1.0.1'.  -->
<!--     - Under actions click 'Run' (the play button). -->
<!--     - Expand 'optional settings'. -->
<!--       - Under 'Ports': Fill 'Host port' with '3838'. -->
<!--     - Click 'Run' -->
<!-- 4. Open the RShiny app in your web browser by navigating to 'http://localhost:3838/'. -->
<!-- 5. When done, close the webpage and: -->
<!--     - From sidebar, open 'Containers'. -->
<!--     - Look for rows with 'Image'='yunhanwu/saeforhealth' & Status = 'Running'. -->
<!--     - Under 'Actions' click 'Stop' -->
<!-- ### Command line (with internet access) -->
<!-- 1. Open the command line. -->
<!-- 2. Download the image: `docker pull yunhanwu/saeforhealth:v1.0.1` -->
<!-- 3. Run a container with the downloaded image:　`docker run --rm -p 3838:3838 yunhanwu/saeforhealth:v1.0.1` -->
<!-- 4. Open the RShiny app in your web browser by navigating to 'http://localhost:3838/'. -->
<!-- 5. When done, close the webpage and: -->
<!--     - From sidebar, open 'Containers'. -->
<!--     - Look for rows with 'Image'='yunhanwu/saeforhealth' & Status = 'Running'. -->
<!--     - Under 'Actions' click 'Stop' -->
<!-- ### Command line (with file copy of image) -->
<!-- 1. Open the command line. -->
<!-- 2. Obtain the docker image in tar format from another source. Load into docker with: `docker load --input file_path.tar` -->
<!-- 3. Run a container with the downloaded image:　`docker run --rm -p 3838:3838 yunhanwu/saeforhealth:v1.0.1` -->
<!-- 4. Open the RShiny app in your web browser by navigating to 'http://localhost:3838/'. -->
<!-- 5. When done, close the webpage and: -->
<!--     - From sidebar, open 'Containers'. -->
<!--     - Look for rows with 'Image'='yunhanwu/saeforhealth' & Status = 'Running'. -->
<!--     - Under 'Actions' click 'Stop' -->
