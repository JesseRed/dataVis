# Use the rocker/rstudio base image
FROM rocker/rstudio:4.0.5

# # Install GnuPG
# RUN apt-get update && apt-get install -y gnupg

# # Add the CRAN repository key
# RUN apt-key adv --keyserver keyserver.ubuntu.com --recv-keys 51716619E084DAB9

# # Install software-properties-common to add repositories
# RUN apt-get update && apt-get install -y software-properties-common

# # Add the CRAN repository for R 4.0
# RUN add-apt-repository 'deb https://cloud.r-project.org/bin/linux/ubuntu focal-cran40/'

# # Install libicu-dev for R dependencies
# RUN apt-get update && apt-get install -y libicu-dev

# Update package list and upgrade installed packages
RUN apt-get update && apt-get upgrade -y

RUN apt-get install libgeos-dev -y
RUN apt-get install libxml2-dev -y


# # Install R base
# RUN apt-get install -y r-base-core=4.0.5-1.2004.0

# # Install R recommended packages separately
# RUN apt-get install -y r-recommended=4.0.5-1.2004.0

# Copy the CSV file containing the list of installed packages
#COPY ./dataVis/installed_packages.csv .

# Install R packages from the CSV file
#RUN R -e "install.packages(read.csv('installed_packages.csv')$Package, repos='https://cran.r-project.org', dependencies=TRUE)"
# Copy the CSV file containing the list of installed packages

RUN Rscript -e "install.packages('rlang')"
RUN Rscript -e "install.packages('shiny')"
RUN Rscript -e "install.packages('shinydashboard')"
RUN Rscript -e "install.packages('shinyalert')"
RUN Rscript -e "install.packages('hrbrthemes')"
RUN Rscript -e "install.packages('GGally')"
RUN Rscript -e "install.packages('shinyFiles')"
RUN Rscript -e "install.packages('shinyWidgets')"
RUN Rscript -e "install.packages('GISTools')"
RUN Rscript -e "install.packages('abind')"
RUN Rscript -e "install.packages('readr')"
RUN Rscript -e "install.packages('rjson')"
RUN Rscript -e "install.packages('feather')"
RUN Rscript -e "install.packages('plotly')"
RUN Rscript -e "install.packages('corrr')"
RUN Rscript -e "install.packages('ggcorrplot')"
RUN Rscript -e "install.packages('vroom')"
RUN Rscript -e "install.packages('circlize')"
RUN Rscript -e "install.packages('rgeos')"
RUN Rscript -e "install.packages('corrplot')"
RUN Rscript -e "install.packages('pheatmap')"
RUN Rscript -e "install.packages('BiocManager')"

RUN apt-get install libglpk-dev -y

RUN Rscript -e "BiocManager::install('graph')"
RUN Rscript -e "install.packages('igraph')"


RUN Rscript -e "install.packages('ggm')"

RUN Rscript -e "install.packages('compute.es')"
RUN Rscript -e "install.packages('multcomp')"
RUN Rscript -e "install.packages('car')"
RUN Rscript -e "install.packages('pastecs')"
RUN Rscript -e "install.packages('effects')"
RUN Rscript -e "install.packages('sortable')"
RUN Rscript -e "install.packages('tidyverse')"
RUN Rscript -e "install.packages('shinyjqui')"
RUN Rscript -e "install.packages('optimbase')"
RUN Rscript -e "install.packages('gdata')"
RUN Rscript -e "install.packages('shinyBS')"
RUN Rscript -e "install.packages('DT')"
RUN Rscript -e "install.packages('cowplot')"
RUN Rscript -e "install.packages('patchwork')"
RUN Rscript -e "install.packages('R.matlab')"
RUN Rscript -e "install.packages('')"

# # Clean up
# RUN rm -rf /var/lib/apt/lists/*

# Expose RStudio Server port
EXPOSE 8787

# Set environment variables
ENV USER rstudio
ENV PASSWORD rstudio
ENV ROOT true


