FROM rocker/r-ver:4.3.2

# Install Linux system dependencies required for PostgreSQL and API networking
RUN apt-get update && apt-get install -y libpq-dev libcurl4-openssl-dev libssl-dev libxml2-dev

# Install your specific R packages
RUN R -e "install.packages(c('plumber', 'DBI', 'RPostgres', 'dplyr', 'lubridate', 'stringr'))"

# Copy your files into the container and start the API
COPY . /app
WORKDIR /app
EXPOSE 8080
CMD ["Rscript", "run_api.R"]