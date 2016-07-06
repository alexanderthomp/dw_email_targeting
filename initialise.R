#!/usr/bin/Rscript
library(shiny)
ip="172.17.0.2"
runApp(appDir=getwd(),host=ip,port=7890)
