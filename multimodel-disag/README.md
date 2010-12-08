# Multimodel Disaggregation Framework

## Settings 

The file settings.R contains all of the the parameter values for a single run of the multimodel framework

## Data

All RData files listed in `config/data` will be loaded with `load()`

## Libraries

All libraries in `config/libraries` will be loaded with `require`

## Source Code

All Files in `lib/` will be `source()`'d 

##  Preprocessing 

The file `save_data.R` should read data files from `data/` and write RData files to the same directory. 

## Running

NOTE: All scripts should be run from the `multimodel_disag` directory. 

`download_predictors.R` - This script need only be run once, it downloads predictor data from 