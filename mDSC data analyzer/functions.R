#---------------------------------
#Custom functions
#---------------------------------

#Function for adding suffixes in the user interface (for example the "st" in "1st")
ordinalSuffix <- function(x) {
  if (x %% 100 %in% c(11, 12, 13)) {
    suffix <- "th"
  } else {
    suffix <- switch(x %% 10,
                     "st",
                     "nd",
                     "rd",
                     "th"
    )
  }
  paste0(x, suffix)
}

# Function for cleaning values
cleanAndConvert <- function(x) {
  x <- gsub("[°C]", "", x)
  x <- gsub("[J/g]", "", x)
  x <- gsub("[%]", "", x)
  x <- gsub("[mg]", "", x)
  x <- gsub("[J/(g.°C)]", "", x)
  x <- gsub(",", ".", x)
}