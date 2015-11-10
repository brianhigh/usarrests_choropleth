## ----choro1,echo=TRUE, eval=TRUE-----------------------------------------
# See:
# https://www3.amherst.edu/~nhorton/r2/examples.php
# https://www3.amherst.edu/~nhorton/r2/excerpt.php

## ----choro0,echo=TRUE, eval=TRUE-----------------------------------------
library(ggmap); library(dplyr)
USArrests.st = mutate(USArrests, 
                      region=tolower(rownames(USArrests)),
                      murder = cut_number(Murder, 5))
us_state_map = map_data('state')
map_data = merge(USArrests.st, us_state_map, by="region")
map_data = arrange(map_data, order)
head(select(map_data, region, Murder, murder, long, lat, group, order))

## ----choro,echo=TRUE, eval=TRUE------------------------------------------
p0 = ggplot(map_data, aes(x=long, y=lat, group=group)) +
    geom_polygon(aes(fill = murder)) +
    geom_path(colour='black') +
    theme(legend.position = "bottom", 
          panel.background=element_rect(fill="transparent",
                                        color=NA)) +
    scale_fill_grey(start=1, end =.1) + coord_map();
plot(p0)