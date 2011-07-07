##################################################
#
# Newer rdf parser, waaaay faster
#
##################################################



#' Read a riverware data file
#'
#' This function is similar to read.rdf but it does not actually read in any data
#' and so is much faster. To access the data, use the \code{\link{get_rdf_slot}}
#' function. 
#'
#' @param file_name An RDF file
#'
#'
#' @return a list containing the structure of an rdf file
#'
#' @note This will be very fast (< 1 sec) if you have grep available on the 
#' command line, otherwise it can take a few seconds for large files. 
#'
#' @author Cameron Bracken <cameron.bracken@@gmail.com> 
#'
#' @examples
#'
#'  # None for now
#'
#' @export
read_rdf_2 <- function(file_name,method=NULL){
  
  
  meta <- read_rdf_meta_2(file_name,method=method)
  times <- read_rdf_times(file_name, meta)
  runs <- rdf_get_data_positions(file_name, meta)

  list(meta=meta,times=times,runs=runs)
  
}

#' Get the date stamps from an rdf file.
#' 
#'
#' @param file_name Name of an rdf file
#' @param meta metadata returned from \code{\link{read_rdf_meta_2}} 
#' (optional but slower if not supplied)
#'
#' @return A vector of dates coerced using \code{\link{as.Date}}
#'
#' @author Cameron Bracken <cameron.bracken@@gmail.com> 
#'
#' @examples
#'
#'  # None for now
#'
#' @export
read_rdf_times <- function(file_name, meta=NULL){
  
  if(is.null(meta))
    meta <- read_rdf_meta_2(file_name)
  
  attach(meta)
  times <- scan(file_name, skip = num_meta + num_run_header + 2, 
      n = num_time_steps, sep='\n', what=character(), quiet=TRUE)
  detach(meta)
  
  as.Date(times)
}

#' Get meta data from an rdf file. 
#'
#' Get listed and derived metadata from an rdf file including the number of 
#' slots.
#'
#' @param file_name Name of an rdf file
#'
#' @return A named list of meta data about an rdf file
#'
#' @author Cameron Bracken <cameron.bracken@@gmail.com> 
#'
#' @examples
#'
#'  # None for now
#'
#' @export
read_rdf_meta_2 <- function(file_name,method=NULL){
  
  con <- file(file_name,'r')
  meta <- read_rdf_header(con,'END_PACKAGE_PREAMBLE')
  first_run_header <- read_rdf_header(con,'END_RUN_PREAMBLE')
  close(con)

  meta <- within(meta, {
    
    num_meta <- length(meta)
      # Assume all runs have the same number of timesteps
    num_time_steps <- as.integer(first_run_header$time_steps)
    num_run_header <- length(first_run_header)
    rdf2 <- TRUE
    rdf_file_name <- file_name
    num_slots <- rdf_get_num_slots(file_name,method)
    num_runs <- as.integer(number_of_runs)
    
  })

  meta
  
}

#' Get the number of slots in an rdf file.
#'
#' This function can use the system grep and cut if available, if they are 
#' things will be waaay faster. This is actually the most time consuming 
#' process in parsing an rdf file since it requires searching through the 
#' entire file which can be very large. 
#'
#' @param file_name Name of an rdf file
#'
#' @return The number of runs (integer)
#'
#' @author Cameron Bracken <cameron.bracken@@gmail.com> 
#'
#' @examples
#'
#'  # None for now
#'
#' @export
rdf_get_num_slots <- function(file_name, method=NULL){
  
  meta <- read_rdf_header(file_name,'END_PACKAGE_PREAMBLE')
  num_runs <- as.integer(meta$number_of_runs)
  
  if(Sys.which('grep') == '' || Sys.which('cut') == '' || method == 'R'){

    message(paste('You dont seem to have grep or cut available in your system,',
      'finding the number of slots will take a lot longer for large files.'))
    flush.console()
    
      # No system grep or cut available, must read in data
    rdf_lines <- readLines(file_name)
    slot_end_lines <- grep(pattern='END_SLOT$',x=rdf_lines)

  }else{

    end_cmd <- function(end_mark)
      paste("grep -n",end_mark,file_name,"| cut -f 1 -d ':'")

    slot_end_lines <- as.integer(system(end_cmd('END_SLOT.$'),intern=T))

  } 
  
  length(slot_end_lines)/num_runs
  
}


#' Get a slot from a rdf file object
#'
#' Get a slot by name from an rdf file object that has already been read with 
#' read.rdf or read_rdf_2.
#'
#' @param x An rdf file object returned from read.rdf or read_rdf_2
#' @param obj A character string giving the name of the RiverWare object
#' @param slot A character string giving the name of the RiverWare slot
#' @param run The run number, defaults to 1
#'
#' @return A vector of data 
#'
#' @author Cameron Bracken <cameron.bracken@@gmail.com> 
#'
#' @examples
#'
#'  # None for now
#'
#' @export 
get_rdf_slot <- function(x, obj, slot, run = 1){
  
  if(is.null(x$meta$rdf2)){
    x$runs[[run]]$objects[[ paste(obj,slot,sep='.') ]]$values
  }else{
    this.object <- x$run[[run]]$objects[[ paste(obj,slot,sep='.') ]]
    scan(x$meta$rdf_file_name, skip=this.object$start_line-1, n=x$meta$num_time_steps,quiet=T)
  }
}

get_rdf_object_data <- function(file_name, meta){
  objects <- list()
  rdf_con <- file(file_name,'r')
  start_skip <- meta$num_meta + meta$num_run_header + 2 + meta$num_time_steps
  
  objects <- read_slot_meta_add_to_object(objects, rdf_con, start_skip)
  for(i in 2:meta$num_slots)
    objects <- read_slot_meta_add_to_object(objects, rdf_con, meta$num_time_steps+2)
  close(rdf_con)
  objects
}

rdf_get_data_positions <- function(file_name,meta=NULL){
  
  if(is.null(meta))
    meta <- read_rdf_meta_2(file_name)
    
  ## so I can use the meta data easier
  attach(meta)
  
  runs <- vector('list',num_runs)

  object_template <- get_rdf_object_data(file_name, meta)

    # start by skipping the meta data for the model, the first run, the tags, 
    # the timesteps and the meta data for the first slot.
  line_count <- num_meta + num_run_header + 2 + num_time_steps + 6

  for(i in 1:num_runs){
    runs[[i]]$objects <- object_template

    for(j in 1:num_slots){

      runs[[i]]$objects[[j]]$start_line <- line_count + 1
      runs[[i]]$objects[[j]]$end_line <- line_count + num_time_steps

        # skip the the next slot, if we are not done with a run skip the meta 
        # data for the next slot, otherwise just skip the end tags
      line_count <- line_count + num_time_steps + ifelse(j == num_slots, 2, 8)

    }

      # skip the meta data for the next run, the end tags, the date stamps and
      # the meta data for the first slot
    line_count <- line_count + num_run_header + 2 + num_time_steps + 6
  }
    
  detach(meta)
  runs
}

add_object <- function(obj,slot_meta){
  
  slot_name <- paste(slot_meta[,'object_name'],slot_meta[,'slot_name'],sep='.')
  obj[[slot_name]] <- list()
  slot_meta_names <- colnames(slot_meta)
  for(i in 1:length(slot_meta_names))
    obj[[slot_name]][[slot_meta_names[[i]]]] <- as.vector(slot_meta[, slot_meta_names[i]])
  obj
  
}

read_slot_meta_add_to_object <- function(obj,con,skip,n=6,out=-4){
  
   # first time is slightly different
  slot_meta <- scan(con,skip=skip,n=n,what=character(0),sep='\n',quiet=TRUE)
  tc <- textConnection(slot_meta[out])
  slot_meta <- read.dcf(tc)
  close(tc)
  add_object(obj,slot_meta)
  
}