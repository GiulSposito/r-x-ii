FROM rocker/r-ver:3.6.3

RUN apt-get update -qq && apt-get install -y \
    libssl-dev \
    libcurl4-gnutls-dev \
    libxml2-dev
    
RUN R -e "install.packages('plumber')"

COPY plumber.R ./

EXPOSE 8080

ENTRYPOINT ["R","-e", "source('runAPI.R')"]