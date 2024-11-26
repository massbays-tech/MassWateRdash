library(shiny)
library(bslib)
library(MassWateR)

source(here::here('R/funcs.R'))

tabfontsize <- 10 
padding <- 0
dqofontsize <- 10
wd <- 6.5

flextable::set_flextable_defaults(font.size = tabfontsize, padding = padding)