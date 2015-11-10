## ----choro0, echo=FALSE, eval=TRUE, message=FALSE, warning=TRUE-----------
# See:
# https://stat.ethz.ch/R-manual/R-devel/library/datasets/html/USArrests.html
# https://en.wikipedia.org/wiki/Choropleth_map

# Install packages (if necessary)
for (pkg in c("ggmap", "dplyr")) {
    if (! suppressWarnings(require(pkg, character.only=TRUE)) ) {
        install.packages(pkg, repos="http://cran.fhcrc.org", dependencies=TRUE)
        if (! suppressWarnings(require(pkg, character.only=TRUE)) ) {
            stop(paste0(c("Can't load package: ", pkg, "!"), collapse = ""))
        }
    }
}

## ----choro1, echo=TRUE, eval=TRUE------------------------------------------
# From: https://www3.amherst.edu/~nhorton/r2/examples/advanced.R
library(ggmap); library(dplyr)
USArrests.st = mutate(USArrests, 
                      region=tolower(rownames(USArrests)),
                      murder = cut_number(Murder, 5))
us_state_map = map_data('state')
map_data = merge(USArrests.st, us_state_map, by="region")
map_data = arrange(map_data, order)
head(select(map_data, region, Murder, murder, long, lat, group, order))

## ----choro2, echo=TRUE, eval=TRUE------------------------------------------
# From: https://www3.amherst.edu/~nhorton/r2/examples/advanced.R
p0 = ggplot(map_data, aes(x=long, y=lat, group=group)) +
    geom_polygon(aes(fill = murder)) +
    geom_path(colour='black') +
    theme(legend.position = "bottom", 
          panel.background=element_rect(fill="transparent",
                                        color=NA)) +
    scale_fill_grey(start=1, end =.1) + coord_map();
plot(p0)