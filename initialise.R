#!/usr/bin/Rscript
library(shiny)
ip="0.0.0.0"
runApp(appDir=getwd(),host=ip,port=7890)
