FROM ubuntu:14.04

RUN mkdir app
COPY app.R init.R $HOME/app/
COPY initialise.R $HOME/
COPY www/* $HOME/app/www/
COPY google-analytics.js $HOME/app/

RUN sh -c 'echo "deb http://cran.rstudio.com/bin/linux/ubuntu trusty/" >> /etc/apt/sources.list' &&\
    gpg --keyserver keyserver.ubuntu.com --recv-key E084DAB9 &&\
    apt-get update &&\
    apt-get -y --allow-unauthenticated install r-base
RUN apt-get -y install gdebi-core
RUN apt-get -y install wget curl
RUN apt-get -y install links2 links lynx w3m
RUN apt-get -y install libmariadbclient-dev
RUN apt-get -y install libpq-dev libcurl4-openssl-dev
RUN apt-get -y install postgresql-client
RUN wget https://download3.rstudio.org/ubuntu-12.04/x86_64/shiny-server-1.4.2.786-amd64.deb
RUN gdebi --option=APT::Get::force-yes=1,APT::Get::Assume-Yes=1 -n shiny-server-1.4.2.786-amd64.deb

RUN Rscript /app/init.R

RUN ln -s /app /srv/shiny-server/email-targeting
RUN ln -s /config/conf.yml /srv/shiny-server/email-targeting/conf.yml

EXPOSE 3838
