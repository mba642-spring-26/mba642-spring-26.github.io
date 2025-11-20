##################################################
# Load or download, extract, read, and save NHANES
# nhanes Pakcage: https://cran.r-project.org/web/packages/nhanesA/vignettes/Introducing_nhanesA.html
##################################################
library(nhanesA)
library(tidyverse)



# Getting Data
grab_nhanes <- function(type, year) {
  # Convert type to table name format
  table_mapping <- list(
    'demo' = 'DEMO',
    'exam' = 'EXAM',
    'q' = "Q"
  )
  
  
  table_name <- table_mapping[[type]]
  if(is.null(table_name)) stop("Unknown table type: ", type)
  
  # Use nhanesA to download
  data_list <- nhanesTables(table_name, year)
  
  if (table_name == "DEMO"){
    dset <- nhanes(data_list$Data.File.Name, year)
  } else if (table_name == "EXAM"){
    dset <- nhanes(data_list[grepl("BMX", data_list[,1]),1], year)
  } else if (table_name == "Q"){
    dset <- nhanes(data_list[grepl("PAQ", data_list[,1]),1], year)
  }
  return(dset)
}


##################################################
# Load and filter NHANES demographic and BMI data
##################################################
grab_and_curate_nhanes <- function(year, minage=0, maxage=84, minbmi=0, maxbmi=Inf){
  dset <- grab_nhanes('demo', year)
  bset <- grab_nhanes('exam', year)
  dset <- dset %>% select(SEQN, RIAGENDR, RIDAGEYR, RIDRETH1)
  bset <- bset %>% select(SEQN, BMXBMI)
  mset <- right_join(dset, bset, by='SEQN')
  mset <- mset %>% rename(ID='SEQN',
                          SEX='RIAGENDR',
                          AGE='RIDAGEYR',
                          RACE='RIDRETH1',
                          BMI='BMXBMI') 
  mset <- mset %>% filter(!is.na(SEX), !is.na(RACE))
  mset <- mset %>% mutate(AGEGROUP=factor(AGE <= 50,
                                          levels=c('TRUE', 'FALSE'),
                                          labels=c('<=50', '>50')),
                          AGEDECADE=cut(AGE,
                                        breaks=seq(0, 90, by=10),
                                        labels=paste(seq(0, 80, by=10),
                                                     seq(10, 90, by=10),
                                                     sep='-'),
                                        right=FALSE),
                          RACE=recode_factor(RACE, 
                                             "Non-Hispanic Black" = "Non-Hispanic Black",
                                             "Non-Hispanic White" = "Non-Hispanic White", 
                                             "Other Race - Including Multi-Racial" = "Other/Mixed",
                                             "Other Hispanic" = "Other Hispanic",
                                             "Mexican American" = "Mexican American"),
                          YEAR=paste(year, year+1, sep='-'))
  mset <- mset %>% filter(between(AGE, minage, maxage),
                          between(BMI, minbmi, maxbmi))
  return(mset)
}

# for testing
# a <- grab_and_curate_nhanes(2015)
# b <- grab_and_curate_nhanes(2000)
# 
