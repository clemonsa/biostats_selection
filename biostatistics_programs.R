## Create R function that reads the 'Statistics and Biostatistics Programs - Biostatistics.csv'
## file downloaded from www.amstat.org listing schools with Statistics or Biostatics Programs
## within the United States and returns the 'Name', 'State', and 'Program.Rank' of schools
## within a range of "National.Rank' values provided by user. File downloaded on 7/26/2017.

## File was furthered edited to add  "National.Rank" and "Program Rank" columns based
## on U.S. News rankings for schools in the National Universities Category 
## ("https://www.usnews.com/best-colleges?int=994d08") or Regional Universities Category and
## Graduate Schools ("https://www.usnews.com/best-graduate-schools") Statistics Program 
## category. If schools had both Statistics and Biostatistics Department rankings then 
## Biostatistics ranking was used. Ranks entered from website on 7/26/2017.

## First you must set working directory to location w/ file

schools <- read.csv('./Statistics and Biostatistics Programs - Biostatistics.csv')
## create data frame named 'schools'
schools <-schools[complete.cases(schools[,'Program.Rank']),]
## Remove schools with 'NA' Program Rank value
schools <- schools[schools$BIOSTATISTICS.PhD == 1, ]
## Remove schools not offering PhDs