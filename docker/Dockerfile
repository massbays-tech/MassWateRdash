FROM rocker/shiny-verse:latest

# System libraries for flextable (textshaping), rmarkdown, and sf/units (map rendering)
RUN apt-get update && apt-get install -y \
    libharfbuzz-dev \
    libfribidi-dev \
    pandoc \
    perl \
    libudunits2-dev \
    libgdal-dev \
    libgeos-dev \
    libproj-dev \
    && rm -rf /var/lib/apt/lists/*

# TinyTeX for PDF report generation; tlmgr path add makes binaries available to the shiny user
RUN R -e "install.packages('tinytex')" && \
    Rscript -e "tinytex::install_tinytex()" && \
    /root/.TinyTeX/bin/*/tlmgr path add

# CRAN packages not already in rocker/shiny-verse
RUN R -e "install.packages(c( \
    'bslib', 'bsicons', 'here', 'shinyWidgets', \
    'writexl', 'reactable', 'flextable', \
    'markdown', 'rhandsontable', 'remotes', \
    'config', 'gargoyle', 'golem', 'pkgload' \
  ), repos='https://packagemanager.posit.co/cran/latest')"

# GitHub packages
RUN R -e "remotes::install_github('massbays-tech/MassWateR')"
RUN R -e "remotes::install_github('massbays-tech/wqformat')"

# Shiny Server configuration (path is relative to build context = repo root)
COPY docker/shiny-server.conf /etc/shiny-server/shiny-server.conf

# Copy app
COPY . /srv/shiny-server/MassWateRdash/

EXPOSE 3838
