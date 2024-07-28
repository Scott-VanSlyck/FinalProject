# Start from the rocker/r-ver image with R 4.4.1
FROM rocker/r-ver:4.3.1

# Install the linux libraries needed for plumber and other dependencies
RUN apt-get update -qq && apt-get install -y \
    libssl-dev \
    libcurl4-gnutls-dev \
    libpng-dev

# Install the R packages needed
RUN R -e "install.packages('tidyverse')"
RUN R -e "install.packages('caret')"
RUN R -e "install.packages('plumber')"

# Copy everything from the current directory into the container
COPY API.R /API.R
COPY diabetes_binary_health_indicators_BRFSS2015.csv /diabetes_binary_health_indicators_BRFSS2015.csv

# Expose port 8000 to traffic
EXPOSE 8000

# When the container starts, start the api.R script
ENTRYPOINT ["R", "-e", \
    "pr <- plumber::plumb('/API.R'); pr$run(host='0.0.0.0', port=8000)"]