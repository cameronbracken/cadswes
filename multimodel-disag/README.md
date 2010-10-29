# Multimodel Disaggregation 

## Configuration

### Data

All RData files listed in `config/data` will be loaded with `load()`

### Libraries

All libraries in `config/libraries` will be loaded with `require`

### Source Code

All Files in `lib/` will be `source()`'d


###  Preprocessing 

The file `save_data.R` should read data files from `data/` and write RData files to the same directory. 