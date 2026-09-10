# Kalium360 - Docker-Image
FROM rocker/r-ver:4.4.2

RUN apt-get update && apt-get install -y --no-install-recommends \
    libcurl4-openssl-dev \
    libssl-dev \
    libxml2-dev \
    zlib1g-dev \
    && rm -rf /var/lib/apt/lists/*

# Use CRAN-Snapshot from 01.08.2026 
ENV CRAN_SNAPSHOT_DATE=2026-08-01
ENV CRAN_REPO=https://packagemanager.posit.co/cran/__linux__/jammy/${CRAN_SNAPSHOT_DATE}

RUN R -e "install.packages( \
      c('DBI', 'duckdb', 'glue', 'dplyr', 'purrr', 'lubridate', 'tibble', 'biglm'), \
      repos = Sys.getenv('CRAN_REPO') )"

# write versions to build log
RUN R -e "ip <- installed.packages()[c('DBI','duckdb','glue','dplyr','purrr','lubridate','tibble','biglm'), 'Version']; \
      cat('Installed package versions (CRAN snapshot ', Sys.getenv('CRAN_SNAPSHOT_DATE'), '):\n', sep=''); \
      print(ip)"

WORKDIR /app

# Copy Scripts
COPY *.R ./
COPY R/*.R ./R/
COPY unit_conversion.csv ./

# mount points for input, output and working dir
RUN mkdir -p /data /output /work

# Defaults for directorys
ENV KALIUM_DATA_DIR=/data
ENV KALIUM_OUTPUT_DIR=/output
ENV KALIUM_WORKING_DIR=/work

CMD ["Rscript", "Main.R"]
