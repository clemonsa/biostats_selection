## Create R function that reads the 'Biostatistics Programs - Information.csv'
## file downloaded from www.amstat.org listing schools with Biostatistics Programs
## within the United States and returns the 'Name', 'National Rank', 'Program Rank', 'Institution Type',
## 'Area', and 'Cost of Living' of schools within the 'State' and degree type ('MS' or 
## 'PHD') provided by user. 
## File downloaded on 2/18/2018.

## File was furthered edited to add  "NATIONAL.RANK" and "PROGRAM.RANK" columns based
## on U.S. News rankings for schools in the National Universities Category 
## ("https://www.usnews.com/best-colleges?int=994d08") or Regional Universities Category and
## Graduate Schools ("https://www.usnews.com/best-graduate-schools") Statistics Program 
## category. If schools had both Statistics and Biostatistics Department rankings then 
## Biostatistics ranking was used. "NA" rank values entered as either '999' for under "National.Rank"
## column. Ranks entered from website on 7/26/2017.

## File url ("https://drive.google.com/file/d/1QXiIeD3qO_NptdRFgDeVSWLcaridrZJw/view?usp=sharing")

## First you must set working directory to location w/ file

schoolstate <- function(state= "XX", degree= "XX", program = TRUE, website = FALSE, chair = FALSE, contact = FALSE) 
 {
  schools <- read.csv('./Biostatistics Programs - Information.csv')
  ## create data frame named 'schools'
  
  if (program == FALSE)
  schools <-schools[complete.cases(schools[,'PROGRAM.RANK']),]
  else
  ## Remove schools with 'NA' Program Rank value
  
  schools <- schools[order(schools[,'NATIONAL.RANK'], schools[,'INSTITUTION']), ]
  ## Order 'schools' data frame rows by National Rank in increasing order and then by name
  
  if (!any (degree == c('PHD', 'MS')))
    stop('missing or invalid degree')
  ## Creates error message if 'degree' argument is missing or invalid
  
  if (degree == 'PHD')
  schools <- schools[schools$BIOSTATISTICS.PhD == 1, ]
  
  else
    if (degree == 'MS')
      schools <- schools[schools$BIOSTATISTICS.Masters == 1, ]
  ## Remove schools not offering PhDs or Masters based on 'degree' argument selection
  
  States <- levels(factor(schools$STATE))
  ## Creates character vector listing unique names of States from 'schools' data frame
   # if (lengths(States) == 0)
   # stop('missing Program.Rank value recommend change setting to "TRUE"')
  ## Generates error message when 'Program.Rank' value is NA
  
  if (!any(States == state))
    stop('degree not offered or invalid state, recommend trying again with program argument set to "TRUE"')
  ## Creates error message if degree requested is not offered or 'state' argument is missing/misspelled 
  else
  info <- schools[schools$STATE == state,]
  ## Create 'info' data frame subsetting based on 'state' argument given by user
  
  options(warn=-1)
  
  if (website == TRUE)
    info[,c('INSTITUTION','NATIONAL.RANK', 'PROGRAM.RANK', 'INSTITUTION.TYPE', 'AREA', 'COST.OF.LIVING', 'WEBSITE')]
  else
  if (chair == TRUE)
      info[,c('INSTITUTION','NATIONAL.RANK', 'PROGRAM.RANK', 'INSTITUTION.TYPE', 'AREA', 'COST.OF.LIVING', 'CHAIR', 'CHAIR.EMAIL')]
  else
  if (contact == TRUE)
    info[,c('INSTITUTION','NATIONAL.RANK', 'PROGRAM.RANK', 'INSTITUTION.TYPE', 'AREA', 'COST.OF.LIVING', 'ADDITIONAL.CONTACT', 'ADDITIONAL.CONTACT.EMAIL')]
  else
    info[,c('INSTITUTION','NATIONAL.RANK', 'PROGRAM.RANK', 'INSTITUTION.TYPE', 'AREA', 'COST.OF.LIVING')]
  ## Returns value of school(s) names, national ranking, program ranking, type, area, and cost of living.
  ## Also displays additional information including Department Chair name and contact info
  ## based on argument settings chosen by user.
  
  }
