FROM ubuntu:14.04

RUN mkdir app
COPY * $HOME/app/
RUN sh -c 'echo "deb http://cran.rstudio.com/bin/linux/ubuntu trusty/" >> /etc/apt/sources.list' &&\
    gpg --keyserver keyserver.ubuntu.com --recv-key E084DAB9 &&\
    apt-get update &&\
    apt-get -y --allow-unauthenticated install r-base
RUN ln -s /config/conf.yml conf.yml
RUN apt-get -y install gdebi-core
RUN apt-get -y install wget
RUN apt-get -y install links2 links lynx w3m
RUN apt-get -y install libpq-dev
RUN apt-get -y install postgresql-client
RUN wget https://download3.rstudio.org/ubuntu-12.04/x86_64/shiny-server-1.4.2.786-amd64.deb
RUN gdebi --option=APT::Get::force-yes=1,APT::Get::Assume-Yes=1 -n shiny-server-1.4.2.786-amd64.deb
RUN Rscript /app/init.R
EXPOSE 7890
