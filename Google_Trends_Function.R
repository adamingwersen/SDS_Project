############################################
##    Query GoogleTrends from R
##
## by Christoph Riedl, Northeastern University
## Additional help and bug-fixing re cookies by
## Philippe Massicotte Université du Québec à Trois-Rivières (UQTR)
############################################


# Load required libraries
library(RCurl)		# For getURL() and curl handler / cookie / google login
library(stringr)	# For str_trim() to trip whitespace from strings

# Google account settings
username <- "sdsprojectgr15@gmail.com"
password <- "socialdatascience"

# URLs
loginURL 		<- "https://accounts.google.com/accounts/ServiceLogin"
authenticateURL <- "https://accounts.google.com/accounts/ServiceLoginAuth"
trendsURL 		<- "http://www.google.com/trends/TrendsRepport?"



############################################
## This gets the GALX cookie which we need to pass back with the login form
############################################
getGALX <- function(curl) {
  txt = basicTextGatherer()
  curlPerform( url=loginURL, curl=curl, writefunction=txt$update, header=TRUE, ssl.verifypeer=FALSE )
  
  tmp <- txt$value()
  
  val <- grep("Cookie: GALX", strsplit(tmp, "\n")[[1]], val = TRUE)
  strsplit(val, "[:=;]")[[1]][3]
  
  return( strsplit( val, "[:=;]")[[1]][3]) 
}


############################################
## Function to perform Google login and get cookies ready
############################################
gLogin <- function(username, password) {
  ch <- getCurlHandle()
  
  ans <- (curlSetOpt(curl = ch,
                     ssl.verifypeer = FALSE,
                     useragent = getOption('HTTPUserAgent', "R"),
                     timeout = 60,         
                     followlocation = TRUE,
                     cookiejar = "./cookies",
                     cookiefile = ""))
  
  galx <- getGALX(ch)
  authenticatePage <- postForm(authenticateURL, .params=list(Email=username, Passwd=password, GALX=galx, PersistentCookie="yes", continue="http://www.google.com/trends"), curl=ch)
  
  authenticatePage2 <- getURL("http://www.google.com", curl=ch)
  
  if(getCurlInfo(ch)$response.code == 200) {
    print("Google login successful!")
  } else {
    print("Google login failed!")
  }
  return(ch)
}


############################################
## Read data for a query
############################################
ch <- gLogin( username, password )
authenticatePage2 <- getURL("http://www.google.com", curl=ch)
res <- getForm(trendsURL, q="Paris", date = "2015-10", content=1, export=1, graph="all_csv", curl=ch)
res
# Check if quota limit reached
if( grepl( "You have reached your quota limit", res ) ) {
  stop( "Quota limit reached; You should wait a while and try again lateer" )
}

# Create function with double input: search-term & date (YYYY-MM)
Google_Trends_Fetch = function(x,y){
  ch <- gLogin( username, password )
  authenticatePage2 <- getURL("http://www.google.com", curl=ch)
  res <- getForm(trendsURL, q=x, date = y, content=1, export=1, graph="all_csv", curl=ch, cat="Nyheder")
}

# Example of how the function works
gtgetgoole = Google_Trends_Fetch("Attack+Peshawar+Taliban", "2015-02") # Feed in info

# For-loop that runs through list and outputs csv-file for each element, i, in list
  # Write/Read locale
out <- "C:/Users/Adam/Dropbox/7. Semester Polit/Social Data Science/Scripts_R/SDS_R_Code/SDS_Project/csv-symphony"
  
Google.Trends.write = list()
for(i in 1:length()){
  print(paste("processing", i, sep = " :: "))
  write.table(x=out, file= i, sep =";")
  Google.Trends.write[[i]] = Google_Trends_Fetch(i)
  cat("done!\n")
}
# Yet