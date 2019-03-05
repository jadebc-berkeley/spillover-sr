####################################
# 0-config.R

# spillover systematic review
# file configuration

# by Jade Benjamin-Chung
# jadebc@berkeley.edu
####################################

# load libraries for cleaning, analysis, table / figure generation
library(here)
library(dplyr)
library(ggplot2)
library(grid) 

# define directories
# data_dir = paste0(here::here(), "/public-data/")

data_dir = "~/Dropbox/Proposals/3ie/SR4/3ieData/Data/Public data/"

fig_dir = paste0(here::here(), "/figures/")
tab_dir = paste0(here::here(), "/tables/")

# source base functions
source(paste0(here::here(), "/0_base_functions.R"))
