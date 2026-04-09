FROM rocker/shiny:4.4

ENV DEBIAN_FRONTEND=noninteractive

RUN apt-get update && apt-get install -y --no-install-recommends \
    build-essential \
    cmake \
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
    libxml2-dev \
    make \
    pandoc \
    zlib1g-dev \
    && rm -rf /var/lib/apt/lists/*

WORKDIR /srv/shiny-server

COPY renv.lock renv.lock
COPY renv renv

RUN R -e "install.packages('renv', repos = 'https://cloud.r-project.org')" \
    && R -e "renv::restore(lockfile = '/srv/shiny-server/renv.lock', prompt = FALSE)"

COPY . .

EXPOSE 3838

CMD ["R", "-e", "setwd('/srv/shiny-server'); app <- source('app.R', chdir = TRUE)$value; shiny::runApp(app, host = '0.0.0.0', port = 3838)"]
