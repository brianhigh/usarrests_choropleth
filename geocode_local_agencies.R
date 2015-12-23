# Geocoding the FBI list of local law enforcement agency locations.
# Two methods are compared: Yellow Pages API and Google API.
#
# The input file is: all_us_states_local_agencies_fbi_crime_data.csv
# If the output file (locations.csv) already exists, then just load it.

# Close connections and clear objects.
closeAllConnections()
rm(list=ls())

library(XML)
library(httr)
library(ggmap)

# -------------------------------------------------------------------
# Configuration
# -------------------------------------------------------------------

use.google <- TRUE         # Slower, small daily limit (2500), no key
use.yellowpages <- FALSE   # Faster, requires API key (free account)

# -------------------------------------------------------------------
# Functions
# -------------------------------------------------------------------

# Geocode using http://publisher.yp.com - API KEY REQUIRED.
# Register for a free API key here: https://publisher.yp.com/register
# Replace "XXXXXXXXX" with your own API key before using this function.
#
geocode_yp <- function(agency.name, city.name, state.name) {
    base.url <- 'http://pubapi.yp.com/search-api/search/devapi/search'
    searchloc <- paste(city.name, state.name, sep=", ")
    term <- agency.name
    businessName <- agency.name
    categories <- paste("Police Departments", "Government Offices", 
                        "Law Enforcement Agencies-Government", sep="|")
    additional.terms.1 <- 'format=xml&sort=distance&radius=5'
    additional.terms.2 <- '&searchResultType=BusinessName'
    additional.terms.3 <- paste('&categories=', categories, "|", sep="")
    additional.terms.4 <- '&listingcount=1&key=XXXXXXXXX'
    search.url <- paste(base.url, "?searchloc=", searchloc, "&term=", term, 
                        "&businessName=", businessName, "&", additional.terms.1, 
                        additional.terms.2, additional.terms.3, 
                        additional.terms.4, sep="")
    response <- GET(URLencode(search.url))
    if (response$status_code!=200) {
        # HTTP request failed!!
        print(paste("Failure:", agency.name, state.name, 
                    "Status:", response$status_code))
    } else {
        xmlfile <- xmlParse(response, encoding = "UTF-8", asText = TRUE)
        xmltop <- xmlRoot(xmlfile)
        if (! is.null(xmltop[["searchListings"]][["searchListing"]])) {
            search.result <- xmlSApply(
                xmltop[["searchListings"]][["searchListing"]], xmlValue)
            search.result.df <- data.frame(t(search.result), row.names=NULL, 
                                           stringsAsFactors = FALSE)
            variables <- c("businessName", "email", "street", "city", "state", 
                           "zip", "phone", "websiteURL", 
                           "latitude", "longitude")
            orig.df <- data.frame(state.name, agency.name, 
                                  stringsAsFactors = FALSE)
            return(cbind(search.result.df[1,variables], orig.df))
        }
    }
}

## Geocode using ggmap (Google) to geocode the agency locations.
#
#     D. Kahle and H. Wickham. ggmap: Spatial Visualization with ggplot2. 
#     The R Journal, 5(1), 144-161. URL 
#     http://journal.r-project.org/archive/2013-1/kahle-wickham.pdf
#
geocode_gg <- function(agency.name, city.name, state.name) {
    searchloc <- paste(agency.name, city.name, state.name, sep=", ")
    
    orig.df <- data.frame(state.name, agency.name, 
                          stringsAsFactors = FALSE)
    
    # Get geocodes from Google's Geocode Service using `geocode` function
    lonlat <- geocode(searchloc, 
                           messaging=FALSE, source = "google")
    
    return(cbind(lonlat, orig.df))
}


# -------------------------------------------------------------------
# Main Routine
# -------------------------------------------------------------------

# Create the data folder if needed.
datadir <- "data/doj_ucr"
dir.create(file.path(datadir), showWarnings = FALSE, recursive = TRUE)

fbi.data.filename <- "all_us_states_local_agencies_fbi_crime_data.csv"
fbi.data.filepath <- paste(datadir, fbi.data.filename, sep="/")

locations.filepath <- paste(datadir, 'locations.csv', sep="/")

# If the output file (locations.csv) already exists, then just load it.
if (file.exists(locations.filepath)) {
    locations <- read.csv(locations.filepath, stringsAsFactors = FALSE)
} else {
    if (file.exists(fbi.data.filepath)) {
        all.states.data <- read.csv(fbi.data.filepath, stringsAsFactors = FALSE)
        
        locations <- unique(all.states.data[,c("Agency", "State")])
        locations$location <- paste(locations$Agency, locations$State, sep=", ")
        
        locations <- locations[complete.cases(locations),]
        locations$City.guess <- gsub(
            "(?:City [Oo]f | (?:City )?(?:Police|Sheriff).*)", "", 
            locations$Agency)
        
        if (use.google == TRUE) {
            lonlat_gg <- do.call("rbind", 
                     lapply(1:nrow(locations), 
                            function(x) geocode_gg(locations[x, "Agency"],
                                                   locations[x, "City.guess"],
                                                   locations[x, "State"])))
            names(lonlat_gg)[names(lonlat_gg) == 'longitude'] <- 'lon'
            names(lonlat_gg)[names(lonlat_gg) == 'latitude'] <- 'lat'
            
            write.csv(lonlat_gg, locations.filepath, row.names=FALSE)
            locations <- lonlat_gg
        }
        if (use.yellowpages == TRUE) {
            lonlat_yp <- do.call("rbind", 
                     lapply(1:nrow(locations), 
                            function(x) geocode_yp(locations[x, "Agency"],
                                                   locations[x, "City.guess"],
                                                   locations[x, "State"])))
            names(lonlat_yp)[names(lonlat_yp) == 'longitude'] <- 'lon'
            names(lonlat_yp)[names(lonlat_yp) == 'latitude'] <- 'lat'
            
            write.csv(lonlat_yp, locations.filepath, row.names=FALSE)
            locations <- lonlat_yp
        }
    }
}

# Example Results
#
# Using this dataset: 
# > locations <- locations[1000:1005, ]
# > locations[, c("Agency", "State")]
#                                   Agency   State
# 27142                 Newnan Police Dept Georgia
# 27170   Newton County Sheriff Department Georgia
# 27198   Oconee County Sheriff Department Georgia
# 27225 Paulding County Sheriff Department Georgia
# 27253         Peachtree City Police Dept Georgia
# 27281                  Perry Police Dept Georgia
#
# We get these restuls using the two different methods of geocoding:
#
# > lonlat_gg[,c("lon", "lat", "state.name", "agency.name")]
#         lon      lat state.name                        agency.name
# 1 -84.79966 33.38067    Georgia                 Newnan Police Dept
# 2 -83.84730 33.55443    Georgia   Newton County Sheriff Department
# 3 -83.44301 33.82320    Georgia   Oconee County Sheriff Department
# 4 -84.89848 33.91420    Georgia Paulding County Sheriff Department
# 5 -84.39494 33.81029    Georgia         Peachtree City Police Dept
# 6 -83.73157 32.45821    Georgia                  Perry Police Dept
#
# > lonlat_yp[,c("lon", "lat", "state.name", "agency.name")]
#         lon       lat state.name                        agency.name
# 1 -84.79928 33.376736    Georgia                 Newnan Police Dept
# 2 -83.83561  33.59989    Georgia   Newton County Sheriff Department
# 3 -83.40872  33.86262    Georgia   Oconee County Sheriff Department
# 4 -84.81701 33.946518    Georgia Paulding County Sheriff Department
# 5 -84.34116 33.913635    Georgia         Peachtree City Police Dept
# 6 -83.73355   32.4618    Georgia                  Perry Police Dept

