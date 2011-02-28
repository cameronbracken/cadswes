# Multimodel Disaggregation Framework

## Settings 

The file settings.R contains all of the the parameter values for a single run of the multimodel framework

## Data

All RData files listed in `config/data` will be loaded with `load()`

### Obtaining the most recent predictor data

To download the most snow, climate and pdsi data a number of scripts must be run from the directory that they reside in:

`data/timeseries/climate_predictors/download_climate_predictors.R`

will generate the file: `data/climate_predictors.txt`

`data/timeseries/pdsi/download_process_pdsi.R`

will generate the file: `data/pdsi_crb.txt`

`data/timeseries/snow/snotel_nrcs/get_snotel_nrcs.R`

will generate the file: `data/timeseries/snow/snotel_nrcs/snotel.txt`

`data/timeseries/snow/snotel_recent_nrcs/get_snotel_recent_nrcs.R`

will generate the file: `data/timeseries/snow/snotel_recent_nrcs/snotel_recent.txt`

`data/timeseries/snow/snow_course_nrcs/get_snow_course_recent_nrcs.R`

will generate the file: `data/timeseries/snow/snow_course/snow_course.txt`

`data/timeseries/snow/snow_course_nrcs/get_snow_course_recent_nrcs.R`

Each of these files has a `dl` argument that if set to `TRUE` will re-download the most recent data and generate corresponding data files. 

After all the snow scripts have been run, run the script 

`data/timeseries/snow/make_swe_series.R`

to consolidate all the snow data into a single file: `data/swe_crb.txt`


## Libraries

All libraries in `config/libraries` will be loaded with `require`

## Source Code

All Files in `lib/` will be `source()`'d 

##  Preprocessing 

The file `save_data.R` should read data files from `data/` and write RData files to the same directory. 

## Running

NOTE: All scripts should be run from the `multimodel_disag` directory. 

`download_predictors.R` - This script need only be run once, it downloads predictor data from 