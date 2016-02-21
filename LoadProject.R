#' This is to fullfill the requirements for
#' Project 2 of Coursera's Exploratory Data Analysis
#'
#' This file is loaded into all plot files.
#'

# ---------- Project Settings
# Set the seed: I prototype plots on a subsetted data frame before
# running it on the actual data set.
set.seed(0123456789)
Use.Prototype <- FALSE
options(scipen = 9999)

# ---------- Load Libraries

# Checks if a library is installed
Is.Installed <- function(lib) {
  lib %in% installed.packages()
}

# Libraries used in this analysis
# I'm using dplyr because it's nice for manipulating data frames
# during the exploratory phase.
libs <- c(
  'dplyr',
  'lattice',
  'ggplot2',
  'gridExtra',
  'scales'
)

# Loop through libs, loading/installing as necessary
sapply(libs, function(x) {
  if(Is.Installed(x)) {
    require(x, character.only = T)
  } else
  {
    install.packages(x, character.only = T)
    library(x, character.only = T)
  }
})

# ---------- Import Data
## Ensure you're in the proper working directory
## Conditionals are wrapped around everything because
## these are time intensive operations.

if(!file.exists('RawDat.zip'))
  download.file('https://d396qusza40orc.cloudfront.net/exdata%2Fdata%2FNEI_data.zip',
                'RawDat.zip')


if(!file.exists('summarySCC_PM25.rds') ||
   !file.exists('Source_Classification_Code.rds'))
  unzip('RawDat.zip')


if(!exists('dfNEI')) dfNEI <- readRDS('summarySCC_PM25.rds')
if(!exists('dfSCC')) dfSCC <- readRDS('Source_Classification_Code.rds')

# ---------- Merge data
## Faciliate left outer join
if(!exists('dfMerged'))
  dfMerged <- tbl_df(merge(dfNEI, dfSCC, by = 'SCC', all.x = TRUE))

## Subset dfMerged for rapid plot prototyping
## This is a hell of a lot quicker for fine tuning plots of larger data sets.

## I used this mostly in the initial exploratory phase -- I just aggregated
## the data in the end.
dfSub <- sample_n(dfMerged, 1000, replace = F)
