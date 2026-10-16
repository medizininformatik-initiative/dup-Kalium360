# Kalium360 - Docker-Image
FROM rocker/r-ver:4.6.1

# DL3008 (pin apt versions) ignored: apt package versions come from the
# Ubuntu pockets and cannot be pinned reliably; reproducibility of the R
# stack is ensured by the pinned CRAN snapshot below.
# apt-get upgrade pulls in the Ubuntu security fixes released after the
# rocker base image was built (e.g. CVEs in linux-libc-dev).
# hadolint ignore=DL3008
RUN apt-get update \
    && apt-get install -y --no-install-recommends \
    libcurl4-openssl-dev \
    libssl-dev \
    libxml2-dev \
    zlib1g-dev \
    && rm -rf /var/lib/apt/lists/*

# Use CRAN-Snapshot from 01.08.2026
ENV CRAN_SNAPSHOT_DATE=2026-08-01
ENV CRAN_REPO=https://packagemanager.posit.co/cran/__linux__/jammy/${CRAN_SNAPSHOT_DATE}

# install packages and write their versions to the build log
# (one RUN to avoid consecutive layers - hadolint DL3059)
RUN R -e "install.packages( \
      c('DBI', 'duckdb', 'glue', 'dplyr', 'purrr', 'lubridate', 'tibble', 'biglm'), \
      repos = Sys.getenv('CRAN_REPO') )" \
    && R -e "ip <- installed.packages()[c('DBI','duckdb','glue','dplyr','purrr','lubridate','tibble','biglm'), 'Version']; \
      cat('Installed package versions (CRAN snapshot ', Sys.getenv('CRAN_SNAPSHOT_DATE'), '):\n', sep=''); \
      print(ip)"

WORKDIR /app

# Copy Scripts
COPY *.R ./
COPY R/*.R ./R/
COPY unit_conversion.csv ./

# mount points for input, output and working dir, and a non-root user
# that runs the scripts. UID/GID 1000 matches the first user on most Linux
# hosts; the README runs it with --user "$(id -u):$(id -g)" instead.
RUN groupadd --gid 1000 kalium \
    && useradd --uid 1000 --gid kalium --no-create-home \
      --home-dir /work --shell /usr/sbin/nologin kalium \
    && mkdir -p /data /output /work \
    && chown kalium:kalium /output /work

USER 1000:1000

# Defaults for directorys
ENV KALIUM_DATA_DIR=/data
ENV KALIUM_OUTPUT_DIR=/output
ENV KALIUM_WORKING_DIR=/work

CMD ["Rscript", "Main.R"]
