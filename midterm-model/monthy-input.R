library('XLConnect')
library('lubridate')
source('../multimodel-disag/lib/proportion_disag.R')
source('../multimodel-disag/lib/libmm.R')
source('lib.R')

  #############################################
  #
  # data dimensions
  #
  #############################################
yrange <- 2000:2010
nyears <- length(yrange)
nmonths <- 32
nsites <- 9
ntraces <- 1000
start_month <- 4
season_len <- 4
season_start_month <- 4
season_end_month <- start_month + season_len - 1

#############################################
#
# data files
#
#############################################

multimodel_ouput <- 'data/jan_drop-one.RData'
multimodel_data <- '../multimodel-disag/data/multimodel.RData'
hmm_data <- '../second-year-fc/src/data/hmm-fc.RData'
lp_data <- 'data/LosPinosAtLaBoca_Colorado_Natural1928-2006.txt'
xlsx_dir <- 'xlsx'
if(!file.exists(xlsx_dir))dir.create(xlsx_dir)
#nat_datafile <- "../multimodel-disag/data/natural-flows-intervening-ac-ft.csv"

  #############################################
  #
  # Multimodel data, natural flow data and
  # unregualted data
  #
  #############################################
load(multimodel_ouput,envir=(mm <- new.env()))
load(multimodel_data,envir = (mmd <- new.env()))
load(hmm_data,envir = (hmm <- new.env()))
los_pinos_nat <- read.table(lp_data,header=TRUE)
#nat.mon.intv <- read.csv(nat_datafile,skip=3)
#i2t <- as.matrix(read.table('../multimodel-disag/data/i2t.txt'))
#nat.mon.tot <- intervening2total(nat.mon.intv,i2t)

mmdisag <- mm$d$disag * 10^6
obs_nat_intv <- mm$historical.intv * 10^6
obs_nat_intv_full <- mmd$historical.intv.full * 10^6
obs_nat_navajo <- mm$historical.tot[,,18] * 10^6
lf_ann <- hmm$x
lf_ann <- window(lf_ann,1949,2007)
lf_seas <- ts(rowSums(obs_nat_intv),1949)
lf_seas <- window(lf_seas,1949,2007)/10^6
ann_to_ses_fit <- lm(lf_seas~lf_ann)

  # need vallecito and morrow point
  # Vallecito will be split from the gains above navajo
  # Morrow point will be split from the gains above crystal (after blue mesa)
source('site_names.R')

  #############################################
  #
  # get the unreg data in a manageable format
  #
  #############################################
unreg <- read.csv('data/HistoricUnregInflow_24-MoStudyRes_cfs.csv')
names(unreg) <- c('Month','Day','Year',site_names_midterm)
mdy <- unreg[,!(names(unreg) %in% site_names_midterm)]
  # convert cfs to acre-ft/month
unreg <-as.data.frame(lapply(unreg[,site_names_midterm],
    cfs_to_acft_per_month, unreg$Year, unreg$Month))
mdy_full <- mdy[mdy$Year >=1963,]
unreg_full <- unreg[mdy$Year >=1963,]
  # get only the apr-july values
unreg <- unreg[mdy$Month %in% 4:7, ]
mdy <- mdy[mdy$Month %in% 4:7,]

  #############################################
  #
  # How to aggregate intervening flows in the MOM
  #
  #############################################
agg <- vector('list',length=nsites)
names(agg) <- site_names_midterm
agg$TaylorPark <- c(3)
agg$BlueMesa <- c(3,4)
agg$Crystal <- c(3,4,5)
agg$Fontenelle <- c(9)
agg$FlamingGorge <- c(9,10,11)
agg$Navajo <- c(18)
agg$LakePowell <- c(1:20)
agg$Vallecito <- agg$MorrowPoint <- numeric(0)


  #############################################
  #
  # aggregate the natural interveing flow to
  # to total flow for the midterm model
  #
  #############################################
disag_nat_mom <- nat_intv_to_mom_tot_sims(mmdisag, agg)
obs_nat_mom <- nat_intv_to_mom_tot_obs(obs_nat_intv, agg)
obs_nat_mom_full <- nat_intv_to_mom_tot_obs(obs_nat_intv_full, agg)

  #############################################
  #
  # Vallecito and Morrow point observed natural
  # data, use actual vallecito data and use 
  # a fraction of crystal data for morrow point 
  # (based on unregualted data)
  #
  #############################################
y <- unreg[mdy$Month %in% 4:7,'MorrowPoint']
x <- unreg[mdy$Month %in% 4:7,'Crystal']
mp_frac <- mean(y/x,na=T)

lp_nat_full <- ts(los_pinos_nat$acre.ft[los_pinos_nat$year >= 1909],1909,freq=12)
lp_nat_full <- stack.ts(window(lp_nat_full,1949))
obs_nat_mom_full[1:nrow(lp_nat_full),,site_names_midterm == 'Vallecito'] <- lp_nat_full
#obs_nat_mom_full[1:nrow(lp_nat_full),,site_names_midterm == 'Navajo']#lp_nat_full
obs_nat_mom_full[,,site_names_midterm == 'MorrowPoint'] <-
  mp_frac*obs_nat_mom_full[,,site_names_midterm == 'Crystal']

lp_nat <- ts(los_pinos_nat$acre.ft[los_pinos_nat$month %in% season_start_month:season_end_month],1909,freq=4)
lp_nat <- stack.ts(window(lp_nat,1949))
obs_nat_mom[1:nrow(lp_nat),,site_names_midterm == 'Vallecito'] <- lp_nat
#obs_nat_mom[1:nrow(lp_nat),,site_names_midterm == 'Navajo']#lp_nat
obs_nat_mom[,,site_names_midterm == 'MorrowPoint'] <-
  mp_frac*obs_nat_mom[,,site_names_midterm == 'Crystal']
#obs_nat_mom[,,site_names_midterm == 'Vallecito'] <-
#  obs_nat_mom[,,site_names_midterm == 'Navajo']

  #############################################
  #
  # generate Vallecito data 
  # by regressing observed los pinos data with
  #
  #############################################
vallecito_regs <- morrow_regs <- vector('list', 12)
navajo_ind <- which(site_names_midterm == 'Navajo')
vallecito_ind <- which(site_names_midterm == 'Vallecito')
morrow_ind <- which(site_names_midterm == 'MorrowPoint')
crystal_ind <- which(site_names_midterm == 'Crystal')

pb <- txtProgressBar(1,12,style=3)
cat('Generating Missing Data...\n')
flush.console()

for(i in 1:12){

  setTxtProgressBar(pb, i)
  
  x <- obs_nat_mom_full[1:nrow(lp_nat_full),i,navajo_ind]
  y <- obs_nat_mom_full[1:nrow(lp_nat_full),i,vallecito_ind]
  vallecito_regs[[i]] <- lm(y~x)

  x <- obs_nat_mom_full[1:nrow(lp_nat_full),i,crystal_ind]
  y <- obs_nat_mom_full[1:nrow(lp_nat_full),i,morrow_ind]
  morrow_regs[[i]] <- lm(y~x)

  if(i %in% season_start_month:season_end_month){
    for(j in 1:ntraces){
      disag_nat_mom[,i-season_start_month+1,vallecito_ind,j] <- 
        predict(vallecito_regs[[i]], 
          data.frame(x=disag_nat_mom[,i-season_start_month+1,navajo_ind,j]))
      disag_nat_mom[,i-season_start_month+1,morrow_ind,j] <- 
        predict(morrow_regs[[i]], 
            data.frame(x=disag_nat_mom[,i-season_start_month+1,crystal_ind,j]))
    }
  }
}
close(pb)
# check the regressions
# sapply(lapply(vallecito_regs,summary),'[[','r.squared')

  #############################################
  #
  # generate Morrow Point multimodel data as 
  # a fraction of crystal flow
  #
  #############################################
disag_nat_mom[,,morrow_ind,] <- mp_frac * disag_nat_mom[,,crystal_ind,]


  #############################################
  #
  # Create regressions of natural flow to
  # to unregulated
  #
  #############################################
regs_full <- vector('list',length(site_names_midterm))
names(regs_full) <- site_names_midterm
for(i in 1:length(site_names_midterm)){
  regs_full[[i]] <- vector('list',12)
  for(j in 1:12){
    if(!all(is.na(obs_nat_mom_full[,j,i]))){
      m <- 1:12
      y <- ts(unreg_full[[i]][mdy_full$Month == m[j]],mdy_full$Year[1])
      x <- ts(obs_nat_mom_full[,j,i],mm$years[1])
      x_time <- time(x)[!is.na(x)]
      y_time <- time(y)[!is.na(y)]
      #overlap <- y_time[y_time %in% x_time]
      x <- x[!is.na(x)][x_time %in% y_time]
      y <- y[!is.na(y)][y_time %in% x_time]
      regs_full[[i]][[j]] <- lm(y~x)
      if(i==5)browser()
    } else{
      regs_full[[i]][[j]] <- lm(0~0)
    }
  }
}
# get the r squared for all the regressions
# sapply(lapply(regs_full,lapply,summary),lapply,'[[','r.squared')


  #############################################
  #
  # Convert the annual values to seasonal
  #
  #############################################
hmm_ens <- hmm$hmm.fc.g.traces
#hmm_ens_ann <- hmm_ens
#for(i in 1:ncol(hmm_ens))
#    hmm_ens[,i,1] <- predict(ann_to_ses_fit,data.frame(lf_ann = hmm_ens[,i,1]))


  #############################################
  #
  # Disag the hmm data
  #
  #############################################
hmm_disag <- list()
  # disag the annual predictions for the out years
for(i in 1:dim(hmm_ens)[3]){
  hmm_disag[[i]] <- pdisag(list(ens = hmm_ens[,,i]*10^6),obs_nat_mom_full,
    agg=rowSums(obs_nat_mom_full),simname='ens')
  hmm_disag[[i]] <- disag(hmm_disag[[i]])
}



traces <- array(NA,c(nyears, nmonths, nsites, ntraces))
hmm.years <- hmm$verif.start:end(hmm$x)[1]
hist_seas_vol <- vector('list',nsites)
for(j in 1:nsites) 
  hist_seas_vol[[j]] <- wapply(ts(unreg[,j],freq=4),sum,win.len=4)
yrs <- unique(mdy$Year)
  
pb <- txtProgressBar(1,length(yrange),style=3)
cat('Building Traces...\n')
flush.console()
for(i in 1:length(yrange)){
  setTxtProgressBar(pb,i)
  for(j in 1:nsites){

    this.year.mm <- which(mm$years == yrange[i])
    this.year.hmm <- which(hmm.years == yrange[i])
      #apr-july year 1
    for(mn in 1:start_month){
      traces[i,mn,j,] <- predict(regs_full[[j]][[mn]],
        data.frame(x = disag_nat_mom[this.year.hmm, mn, j, ]))
      if(any(is.na(traces[i,mn,j,]))) browser()
    }

      # august - dec year 1
      # pick the data from the closest historical year
    for(k in 1:ntraces){
      this_seas_vol <- sum(traces[i,1:start_month,j,k])
      m <- which.min(abs(this_seas_vol - hist_seas_vol[[j]]))
      this_full_yr <- (mdy_full$Year == yrs[m])
      traces[i,(season_end_month+1):12-season_start_month+1,j,k] <- 
        unreg_full[this_full_yr,j][(season_end_month+1):12]
    }
      # out years
    lag <- 12 - start_month + 1
    for(mn in (lag+1):nmonths){
      mon <- (mn-lag) %% 12 + 1
      ind <- ceiling((mn-lag)/12)
      traces[i,mn,j,] <- predict(regs_full[[j]][[mon]],
        data.frame(x = hmm_disag[[ind]]$disag[this.year.hmm, mon, j, ]))
    }
  }
}
close(pb)

#############################################
#
# Write the data to an excel spreadsheet
#
#############################################

pb <- txtProgressBar(1,nyears*nsites,style=3)
cat('Writing Input Data...\n')
flush.console()
b <- 0 

for(i in 1:nyears){
  
  start_date <- paste(yrange[i], sprintf('%02d',start_month),'01', sep='-')
  dates <- as.Date(start_date) + months(0:(nmonths-1))
  fn <- file.path(xlsx_dir,paste(yrange[i],'.xlsx',sep=''))
  if(file.exists(fn))file.remove(fn)
  
  wb <- loadWorkbook(fn, create = TRUE)
  
  for(j in 1:nsites){
    
    b <- b + 1
    setTxtProgressBar(pb,b)
    this_data <- traces[i,,j,]
    rownames(this_data) <- format(dates,'%B 1, %Y')
    colnames(this_data) <- paste('Run',0:(ntraces-1),sep='')
    these_dates <- as.data.frame(rownames(this_data))
    names(these_dates) <- "."
    
    createSheet(wb, name = site_names_midterm_rw[j])
    writeWorksheet(wb, as.data.frame(this_data), sheet = site_names_midterm_rw[j],
      startRow = 3, startCol = 2)
    createName(wb, name = site_names_midterm_rw[j], 
      formula = paste(site_names_midterm_rw[j],'!$A$3',sep=''))
    writeNamedRegion(wb, these_dates, name = site_names_midterm_rw[j],)
    
    #if(j ==1)
    #  write.xlsx(this_data/1000, fn, sheetName = site_names_midterm_rw[j])
    #else
    #  write.xlsx(this_data/1000, fn, sheetName = site_names_midterm_rw[j], append=TRUE)

  }
  saveWorkbook(wb)
}
close(pb)

#for(i in )
