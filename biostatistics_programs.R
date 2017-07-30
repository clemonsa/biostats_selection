## Create R function that reads the 'Statistics and Biostatistics Programs - Biostatistics.csv'
## file downloaded from www.amstat.org listing schools with Statistics or Biostatics Programs
## within the United States and returns the 'Name', 'State', and 'Program.Rank' of schools
## within a range of 'National.Rank' values and degree type ('MS' or 'PHD') provided by user. 
## File downloaded on 7/26/2017.

## File was furthered edited to add  "National.Rank" and "Program Rank" columns based
## on U.S. News rankings for schools in the National Universities Category 
## ("https://www.usnews.com/best-colleges?int=994d08") or Regional Universities Category and
## Graduate Schools ("https://www.usnews.com/best-graduate-schools") Statistics Program 
## category. If schools had both Statistics and Biostatistics Department rankings then 
## Biostatistics ranking was used. Ranks entered from website on 7/26/2017.

## First you must set working directory to location w/ file

schoolrank <- function(national= "XX", degree= "XX", program = FALSE) {
  
  schools <- read.csv('./Statistics and Biostatistics Programs - Biostatistics.csv')
  ## create data frame named 'schools'
 
  if (program == FALSE)
    schools <-schools[complete.cases(schools[,'Program.Rank']),]
  else
    ## Remove schools with 'NA' Program Rank value unless 'program' argument is set to 'TRUE'
  
  schools <- schools[order(schools[,'National.Rank'], schools[,'Name']), ]
  ## Order 'schools' data frame rows by National Rank in increasing order and then by name
  
  if (!any (degree == c('PHD', 'MS')))
    stop('missing or invalid degree')
  
  if (degree == 'PHD')
  schools <- schools[schools$BIOSTATISTICS.PhD == 1, ]
  
  else
    if (degree == 'MS')
      schools <- schools[schools$BIOSTATISTICS.Masters == 1, ]
  ## Remove schools not offering PhDs or Masters based on 'degree' argument selection

  info <- schools[schools$National.Rank == national,]
  ## Create info data frame subsetting based on 'national' argument given by user
  
  options(warn=-1)
  ## Removes warning message
  if (lengths(info) == 0)
    stop('missing or invalid ranking')
  ## Generates error message when invalid 'national' argument is used
  
  else
  info[,c('Name','State', 'Program.Rank')]
  
  }
