# ---------------------------------------------------------------------------
# Get US crime data from the the US DOJ (originally from the FBI).
#
# Sources:
#
#   United States Department of Justice, Federal Bureau of Investigation. 
#   (September 2015). Crime in the United States, 2014. Retrieved 
#   Dec. 22, 2015, from UCRDATATOOL.gov.
# 
# Notes:
#
#   "National or state offense totals are based on data from all reporting 
#   agencies and estimates for unreported areas." -- From: UCRDATATOOL.gov
#
#   These data are just estimates with some degree of uncomparability.
#   See numerous notes in results page (or CSV) from UCRDATATOOL.gov.
# ---------------------------------------------------------------------------

# get_all_us_states_local_agencies_fbi_crime_data.R
#
# To get these data from the US DOJ, FBI UCR (UCRDATATOOL.gov), we
# have to search for one agency at a time if we want all years and all
# crime catagories. This would be very tedious to do manually for each
# agancy in each state. So we will automate this. But the web data query
# tool requires a multi-step process using forms, requiring cookies to
# track progress through the process. So, we will need to use cookies. 
# RCurl supports cookies, so this is not a problem. We will also need
# to use a generic user-agent string ("Mozilla/5.0") as the default
# user-agent for curl does not seem to be accepted by the web server.

# The process we want to automate can be shown in this 4-step example:
#
# curl -o states.html -c cookies.txt -A "Mozilla/5.0" \
#   "http://www.ucrdatatool.gov/Search/Crime/Local/JurisbyJuris.cfm"
#
# curl -o agencies.html -b cookies.txt -A "Mozilla/5.0" \
#   -e "http://www.ucrdatatool.gov/Search/Crime/Local/JurisbyJuris.cfm" \
#   --data "DataType=0&StateId=48&BJSPopulationGroupId=&NextPage=Next" \
#   "http://www.ucrdatatool.gov/Search/Crime/Local/JurisbyJurisStepTwo.cfm"
#
# curl -o agency_data.html -b cookies.txt -A "Mozilla/5.0" \
#   -e "http://www.ucrdatatool.gov/Search/Crime/Local/JurisbyJurisStepTwo.cfm" \
#   --data "StateId=48&BJSPopulationGroupId=&CrimeCrossId=20706&DataType=1&DataType=2&DataType=3&DataType=4&YearStart=1985&YearEnd=1985&NextPage=Get+Table" \
#   'http://www.ucrdatatool.gov/Search/Crime/Local/DownCrimeJurisbyJuris.cfm/LocalCrimeJurisbyJuris.csv"
#
# perl -wnle "/(^\d+\s*,|^Year)/ and print" agency_data.csv > agency_clean.csv
#
# (Note: These will run as-is in "bash", but to run in "DOS", you will need to
# replace the "\" character at the end of continuing lines with a "^" symbol.)

# These commands first get a page containing the state names and IDs as options
# in a selection list. Then, for a particular state ID, it fetches another page
# containing agency names and IDs from another pick list. Third, it gets crime
# counts for a particular state agency and range of years and saves as CSV.
# Finally, a perl command can be used to remove unwanted lines from the CSV.
#
# You could automate this by putting these commands in a script and looping
# through each state and agency until all CSV data files were downloaded. For
# this to work, you would need to devise a way to extract the options from the
# web pages, presumably using some library (package) designed to parse HTML.
#
# We will want to implement this in R and parse, tidy, and combine the data for
# all states, agencies, years, and crime types. That seems like a lot of work!
# It would be easier if the DOJ simply offered this dataset as a single file
# to be downloaded. For some reason, they do not, but have created this form
# query "wizard" instead. While the wizard offers various ways to slice the
# data, none will allow you to get all of the data in a single operation. So,
# we need to automate the task of getting the data for over 4,000 agencies, one
# agency at a time. Fortunately we can do this in R with RCurl.
#

library(XML)
library(RCurl)

# Close connections and clear objects.
closeAllConnections()
rm(list=ls())

# ---------------------------------------------------------------------------
# Functions
# ---------------------------------------------------------------------------

get_agencies_and_years <- function(state.id, state.name, datadir, curl) {
    url.str <- 'http://www.ucrdatatool.gov/Search/Crime/Local/JurisbyJurisStepTwo.cfm'
    referer <- 'http://www.ucrdatatool.gov/Search/Crime/Local/JurisbyJuris.cfm'
    formdata <- paste('DataType=0&StateId=', state.id, 
                      '&BJSPopulationGroupId=&NextPage=Next', sep='')
    r = dynCurlReader()
    res <- curlPerform(postfields = formdata, url = url.str,
                       referer = referer, curl=curl,
                       post = 1L, writefunction = r$update)
    result.string <- r$value() 
    
    doc = htmlParse(result.string)
    
    year.options <- getNodeSet(xmlRoot(doc), 
                               "//select[@id='years']/option")
    years <-  sapply(year.options, xmlGetAttr, "value")
    
    #write.csv(years, paste(datadir, paste(state.name, "_years.csv", 
    #                                      sep=""), sep="/"), row.names=FALSE)
    
    agency.options <- getNodeSet(xmlRoot(doc), 
                                 "//select[@id='agencies']/option")
    agency.ids <- sapply(agency.options, xmlGetAttr, "value")
    names <- sapply(agency.options, xmlValue)
    names <- gsub("[\t\r\n]+", "", names)
    names <- gsub("^[ ]+|[ ]+$", "", names)
    agencies <- data.frame(ID=agency.ids, Name=names, 
                           stringsAsFactors = FALSE)
    agencies$ID <- as.numeric(agencies$ID)
    
    #state.name.sub <- gsub("[ .]+", "_", tolower(state.name))
    #write.csv(agencies, paste(datadir, paste(state.name.sub, "_agencies.csv", 
    #                                    sep=""), sep="/"), row.names=FALSE)
    return(list(years, agencies))
}


get_agency_data <- function(state.id, state.name, agency.id, agency.name,
                            year.start, year.end, datadir, curl) {
    
    #x<-2
    #agency.id <- agencies[x, "ID"]
    #agency.name <- agencies[x, "Name"]
    
    agency.name
    
    url.str <- 'http://www.ucrdatatool.gov/Search/Crime/Local/DownCrimeJurisbyJuris.cfm/LocalCrimeJurisbyJuris.csv'
    referer <- 'http://www.ucrdatatool.gov/Search/Crime/Local/JurisbyJurisStepTwo.cfm'
    formdata <- paste('StateId=', state.id,
                      '&CrimeCrossId=', agency.id, 
                      '&DataType=1&DataType=2&DataType=3&DataType=4&',
                      'YearStart=', year.start, '&YearEnd=', year.end, 
                      '&NextPage=Get+Table', sep='')
    
    r = dynCurlReader()
    res <- curlPerform(postfields = formdata, url = url.str,
                       referer = referer, curl=curl,
                       post = 1L, writefunction = r$update)
    result.string <- r$value()
    
    datalines <- readLines(textConnection(result.string), warn = FALSE)
    line.start <- grep(agency.name, datalines, fixed=TRUE)
    skip <- line.start[1] + 1
    line.end <- grep("^$", datalines[skip:length(datalines)])
    n <- line.end[1] - 2
    
    agdata <- read.table(text=result.string, skip=skip, nrows=n, sep=",", 
                         stringsAsFactors = FALSE)
    
    names(agdata) <- c("Year", "Months", "Population", "Violent_crime_total",
                       "Murder_and_nonnegligent_Manslaughter", "Forcible_rape",
                       "Robbery", "Aggravated_assault", "Property_crime_total",
                       "Burglary", "Larceny_theft", "Motor_vehicle_theft",
                       "Violent_Crime_rate",
                       "Murder_and_nonnegligent_manslaughter_rate",
                       "Forcible_rape_rate", "Robbery_rate",
                       "Aggravated_assault_rate", "Property_crime_rate",
                       "Burglary_rate", "Larceny_theft_rate",
                       "Motor_vehicle_theft_rate")
    
    agdata$State <- rep(state.name, times = nrow(agdata))
    agdata$Agency <- rep(agency.name, times = nrow(agdata))
    
    #state.name.sub <- gsub("[ .]+", "_", tolower(state.name))
    #agency.name.sub <- gsub("[ .]+", "_", tolower(agency.name))
    #output.file <- paste(datadir, paste(state.name.sub, agency.name.sub,
    #                                    "data.csv",  sep="_"), sep="/")
    #
    #write.csv(agdata, output.file, row.names=FALSE)
    
    return(agdata)
}


get_state_agency_data <- function(state.id, state.name, 
                                  agencies, year.start, year.end, 
                                  datadir, curl) {
    state.agency.data <- do.call("rbind", lapply(1:nrow(agencies), function(x)
        get_agency_data(state.id, 
                        state.name, 
                        agencies[x, "ID"], 
                        agencies[x, "Name"],
                        year.start,
                        year.end,
                        datadir, 
                        curl)))
    return(state.agency.data)
}

get_state_data <- function(state.id, state.name, datadir, curl) {
    
    state.name.sub <- gsub("[ .]+", "_", tolower(state.name))
    state.file <- paste(state.name.sub, "data.csv", sep="_")
    state.file <- paste(datadir, state.file, sep="/")
    
    if (file.exists(state.file)==TRUE) {
        state.agency.data <- read.csv(state.file)
    } else {    
        ag.lst <- get_agencies_and_years(state.id, state.name, datadir, curl)
        
        year.start <- min(ag.lst[[1]])
        year.end <- max(ag.lst[[1]])
        agencies <- ag.lst[[2]]
        
        # Fix typos in agency names from selection list on web form.
        if (state.name == "Texas") {
            agencies$Name <- gsub("Mcallen Police Dept", 
                                  "McAllen Police Dept", 
                                  agencies$Name)
            agencies$Name <- gsub("Mckinney Police Dept", 
                                  "McKinney Police Dept", 
                                  agencies$Name)
            agencies$Name <- gsub("Mclennan County Sheriff Department", 
                                  "McLennan County Sheriff Department", 
                                  agencies$Name)
        }
        
        state.agency.data <- get_state_agency_data(state.id, state.name,
                                                   agencies, 
                                                   year.start, year.end, 
                                                   datadir, curl)
        
        write.csv(state.agency.data, state.file, row.names=FALSE)
        
        # Read the CSV so that the variable types will be set appropriately.
        state.agency.data <- read.csv(state.file, stringsAsFactors = FALSE)
    }
    
    return(state.agency.data)
}



# ---------------------------------------------------------------------------
# Main routine
# ---------------------------------------------------------------------------

# Create the data folder if needed.
datadir <- "data/doj_ucr"
dir.create(file.path(datadir), showWarnings = FALSE, recursive = TRUE)

output.file.name <- "all_us_states_local_agencies_fbi_crime_data.csv"
output.filepath <- paste(datadir, output.file.name, sep="/")

if (file.exists(output.filepath)) {
    all.states.data <- read.csv(output.filepath, stringsAsFactors = FALSE)
} else {
    # Configure curl to use a cookie file and a custom user agent string.
    cookie = 'cookies.txt'
    curl <- getCurlHandle(cookiefile = cookie, cookiejar = cookie,
                          useragent = "Mozilla/5.0")
    
    # Get list of states and state IDs
    states.file.name <- "states.csv"
    states.filepath <- paste(datadir, states.file.name, sep="/")
    
    if (file.exists(states.filepath)) {
        states <- read.csv(states.filepath, stringsAsFactors = FALSE)
    } else {
        # Get and parse web page containing states (and IDs) in "option" tags.
        st <- 'http://www.ucrdatatool.gov/Search/Crime/Local/JurisbyJuris.cfm'
        statedoc <- getURL(st, curl=curl)
        statedoc <- htmlParse(statedoc)
        options <- getNodeSet(xmlRoot(statedoc), "//select[@id='state']/option")
        ids <- sapply(options, xmlGetAttr, "value")
        names <- sapply(options, xmlValue)
        states <- data.frame(ID=ids, Name=names, stringsAsFactors = FALSE)
        states$ID = as.numeric(states$ID)
        write.csv(states, states.filepath, row.names=FALSE)
    }
    
    # Get data for all states
    all.states.data <- do.call("rbind", 
                               lapply(1:nrow(states), 
                                      function(x) get_state_data(
                                          states[x, "ID"], 
                                          states[x, "Name"], 
                                          datadir=datadir, 
                                          curl=curl)))
    
    
    write.csv(all.states.data, output.filepath, row.names=FALSE)
    
    rm(curl)
    ret <- gc(verbose = FALSE)
}