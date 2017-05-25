#startup file for RIL-Simulation
#run this before trying code. this is not useful for the app

rm(list=ls()) #will remove ALL objects 

library(grid)
library(gridExtra)
library(mgcv)
library(shiny)
library(tidyverse)
library(truncnorm)

source('R/directional_mortality.R')
source('R/do_enrichment.R')
source('R/forest_randomizer.R')
source('R/get_agb.R')
source('R/get_diameter_growth.R')
source('R/get_harvest.R')
source('R/get_height.R')
source('R/get_price.R')
source('R/get_volume.R')
source('R/ggplot_params.R')
source('R/get_harvest.R')
source('R/get_regeneration.R')
source('R/hurricane_mortality.R')
source('R/mean_sd.R')
source('R/mortality_calc.R')
source('R/multiplot.R')
source('R/skidding_mortality.R')
source('R/stand_parameters_calc.R')
source('R/stand_randomizer.R')

regen.params <- read.csv('data/regenation_params.csv', sep = ',')

#source('R/simulator.R')
source('R/simulator_full.R')

