grab_meps <- function(year, inscope_only=TRUE){
  
  if (year < 2018) {
    code <- paste0('h', seq(12, 201, by=9))
    names(code) <- seq(1996, 2017)
  } else if (year >= 2018){
    auto_codes <- paste0('h', seq(12, 201, by=9))
    manual_codes <- c("h209", "h216", "h224", "h233", "h243", "h251")
    code <- c(auto_codes, manual_codes)
    names(code) <- seq(1996, 2023)
  }
  
  stopifnot(year %in% names(code))
  year_code <- code[[as.character(year)]]
  local_rdata <- paste(year_code, 'Rdata', sep='.')
  
  if(file.exists(local_rdata)){
    load(file=local_rdata)
  } else {
    if(year < 2017){
      archive_filename <- paste0(year_code, 'ssp.zip')
      remote_archive <- paste0('https://meps.ahrq.gov/mepsweb/data_files/pufs/',
                               archive_filename)
      local_archive <- paste0(year_code, 'ssp.zip')
      old_timeout <- getOption('timeout')
      options(timeout=max(300, old_timeout))
      download.file(remote_archive, local_archive, mode='wb')
      options(timeout=old_timeout)
      dset <- foreign::read.xport(unzip(local_archive))
      local_file <- sub('ssp.zip$', '.ssp', local_archive)
    } else {
      archive_filename <- paste0(year_code, 'dta.zip')
      remote_archive <- paste0('https://meps.ahrq.gov/data_files/pufs/',
                               year_code,
                               '/',
                               archive_filename)
      local_archive <- paste0(year_code, 'dta.zip')
      old_timeout <- getOption('timeout')
      options(timeout=max(300, old_timeout))
      download.file(remote_archive, local_archive, mode='wb')
      options(timeout=old_timeout)
      dset <- haven::read_dta(unzip(local_archive))
      local_file <- sub('dta.zip$', '.dta', local_archive)
    }
    file.copy(local_file, local_rdata, overwrite=TRUE)
    unlink(local_file)
    save(dset, file=local_rdata)
  }
  if(inscope_only){
    suffix <- sprintf("%02d", as.numeric(substr(year, 3, 4)))
    # suffix <- substr(year, 3, 4)
    inscope <- paste0('INSCOP', suffix)
    dset <- dset[dset[[inscope]] == 1, ]
    
    # spare code for debugging
    # inscop_vars <- names(dset)[grepl("^INSCOP", names(dset))]
    # print(inscop_vars)
  }
  return(dset)
}

dat <- grab_meps(2017)
