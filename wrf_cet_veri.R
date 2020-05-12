library(openair)


rmsp <- read.table('./cetesb_rmsp_d03.dat', header = T,
                   sep = ',', stringsAsFactors = F)


# Reading WRFout and Cetesb data

ReadCetWRF <- function(code, cet_wrf){
  file.name <- paste0(code, '_', cet_wrf, '.dat')
  df <- read.table(file.name, header = T, sep = ',', 
                   stringsAsFactors = F, dec = '.')
  if (cet_wrf == 'cetesb'){
    df$date <- as.POSIXct(strptime(df$date, '%Y-%m-%d %H:%M'), tz = 'America/Sao_Paulo')
    attributes(df$date)$tzone <- 'UTC'
    df$wd[df$wd  == 888 | df$wd  == 777] <- NA
  } else if (cet_wrf == 'wrfout') {
    df$date <- as.POSIXct(strptime(df$date, '%Y-%m-%d %H:%M'), tz = 'UTC')
  }
  return(df)
}

cet <- lapply(rmsp$code, ReadCetWRF, cet_wrf = 'cetesb')
wrf <- lapply(rmsp$code, ReadCetWRF, cet_wrf = 'wrfout')

# Clipping by simulation time

sim.date <- seq(as.POSIXct('2014-10-06 00:00 00:00', tz = 'UTC'),
                by = 'hour', 
                length.out = 168)

cet <- lapply(cet, function(df) df[df$date %in% sim.date, ])
wrf <- lapply(wrf, function(df) df[df$date %in% sim.date, ])

test.cet <- cet[[28]]
test.wrf <- wrf[[28]]


modStats(mod=test.wrf$tc, obs=test.cet$tc)


