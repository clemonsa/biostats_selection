## Create R function that reads the 'Statistics and Biostatistics Programs - Biostatistics.csv'
## file downloaded from www.amstat.org listing schools with Statistics or Biostatics Programs
## within the United States and returns the 'Name', 'National.Rank', 'Program.Rank' and 'Type'
## of schools within the 'state' value and degree type ('MS' or 'PHD') provided by user. 
## File downloaded on 7/26/2017.

## File was furthered edited to add  "National.Rank" and "Program Rank" columns based
## on U.S. News rankings for schools in the National Universities Category 
## ("https://www.usnews.com/best-colleges?int=994d08") or Regional Universities Category and
## Graduate Schools ("https://www.usnews.com/best-graduate-schools") Statistics Program 
## category. If schools had both Statistics and Biostatistics Department rankings then 
## Biostatistics ranking was used. "NA" rank values entered as either '999' for under "National.Rank"
## column. Ranks entered from website on 7/26/2017.

## File url ("https://drive.google.com/file/d/0B--Ray31aygCV0lyZ1pGdURwWm8/view?usp=sharing")

## First you must set working directory to location w/ file

schoolstate <- function(state= "XX", degree= "XX", program = FALSE) {
  
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
  ## Creates error message if 'degree' argument is missing or invalid
  
  if (degree == 'PHD')
  schools <- schools[schools$BIOSTATISTICS.PhD == 1, ]
  
  else
    if (degree == 'MS')
      schools <- schools[schools$BIOSTATISTICS.Masters == 1, ]
  ## Remove schools not offering PhDs or Masters based on 'degree' argument selection
  
  States <- levels(factor(schools$State))
  ## Creates character vector listing unique names of States from 'schools' data frame
  
  if (!any(States == state))
    stop('missing or invalid state')
  ## Creates error message if 'state' argument is missing or not misspelled
  else
  info <- schools[schools$State == state,]
  ## Create info data frame subsetting based on 'state' argument given by user
  
  if (lengths(info) == 0)
    stop('missing Program.Rank value recommend change setting to "TRUE"')
  ## Generates error message when 'Program.Rank' value is NA
  
  info[,c('Name','National.Rank', 'Program.Rank', 'Type')]
  ## Returns value of school(s) names, national ranking, program ranking, and type.
  
  }