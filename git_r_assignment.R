# Author: Joan Wang
# Purpose: Exploratory analysis of the DoT FARS data

# install.packages(c('readr', 'haven', 'dplyr', 'tidyr', 'stringr', 'ggplot2'))
library('readr')
library('haven')
library('dplyr')
library('tidyr')
library('stringr')
library('ggplot2')


acc2014 <- read_sas()
acc2015 <- read_csv()