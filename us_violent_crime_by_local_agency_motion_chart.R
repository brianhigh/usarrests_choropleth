# --------------------------------------------------------------------
# Create a Google Motion Chart for Violent Crime Rate (per 100,000
# people) reported by US local law enforcement agencies serving over
# 100,000 people or more for the years 1985 - 2012.
# --------------------------------------------------------------------

# Close connections and clear objects.
closeAllConnections()
rm(list=ls())

# Load packages
library(googleVis)

# Create the data folder if needed.
datadir <- "data/doj_ucr"
dir.create(file.path(datadir), showWarnings = FALSE, recursive = TRUE)

# Read in data file
file.name <- "all_us_states_local_agencies_fbi_crime_data.csv"
filepath <- paste(datadir, file.name, sep="/")
all.states.data <- read.csv(filepath, stringsAsFactors = FALSE)

# Subset by agencies in a particular State
#crime <- all.states.data[all.states.data$State == "Washington",]

# Subset the violent crimes.
violent <- c("Year", "Murder_and_nonnegligent_manslaughter_rate",
             "Forcible_rape_rate", "Robbery_rate", "Aggravated_assault_rate",
             "Violent_Crime_rate", "Agency", "Population")
violent.crime <- all.states.data[, violent]

# Shorten some crime names for shorter plot labels and capitalize them.
violent <- c("Year", "Murder", "Rape", "Robbery", "Assault", "Violent",
             "Agency", "Population")
names(violent.crime) <- violent

# Subset by Population >= 100,000
violent.crime <- violent.crime[violent.crime$Population >= 100000,]

# Remove cases containing NAs
violent.crime <- na.omit(violent.crime)

# Remove duplicate cases of the index variabes: Year and Agency
index <- which(duplicated(violent.crime[,c("Agency", "Year")]))
violent.crime <- violent.crime[-index, ]

# Make the visualization
plot(gvisMotionChart(data = violent.crime, idvar = "Agency", 
                     timevar = "Year", colorvar = "Violent",
                     xvar = "Robbery", yvar = "Assault", sizevar="Population",
                     options=list(width=800, height=600)))
