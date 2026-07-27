FROM rocker/shiny:4.4

ENV DEBIAN_FRONTEND=noninteractive

RUN apt-get update && apt-get install -y --no-install-recommends \
    build-essential \
    cmake \
    curl \
    g++ \
    gdal-bin \
    git \
    gfortran \
    libcurl4-openssl-dev \
    libfontconfig1-dev \
    libfribidi-dev \
    libgdal-dev \
    libgeos-dev \
    libglpk40 \
    libharfbuzz-dev \
    libjpeg-dev \
    libpng-dev \
    libproj-dev \
    libssl-dev \
    libtiff5-dev \
    libudunits2-dev \
    libuv1-dev \
    libxml2-dev \
    make \
    pandoc \
    zlib1g-dev \
    && rm -rf /var/lib/apt/lists/*

WORKDIR /srv/shiny-server

COPY renv.lock renv.lock
COPY renv renv

RUN R -e "utils::install.packages('renv', repos = 'https://cloud.r-project.org')"

RUN R -e "options(repos = c( \
      CRAN = 'https://cloud.r-project.org', \
      INLA = 'https://inla.r-inla-download.org/R/testing' \
    )); \
    renv::restore( \
      project = '/srv/shiny-server', \
      lockfile = '/srv/shiny-server/renv.lock', \
      repos = getOption('repos'), \
      prompt = FALSE \
    )"

COPY . .

EXPOSE 3838

CMD ["R", "-e", "setwd('/srv/shiny-server'); app <- source('app.R', chdir = TRUE)$value; shiny::runApp(app, host = '0.0.0.0', port = 3838)"]
