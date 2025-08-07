rm(list = ls())
setwd(dirname(rstudioapi::getSourceEditorContext()$path))
options(scipen = 999)

knitr::purl("global.Rmd", output = "global_from_rmd.R")
source("global_from_rmd.R")
source("UI.R")
source("server.R")

shinyApp(ui = ui, server = server)

