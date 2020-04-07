
args <- commandArgs(trailingOnly = TRUE)
region <- args[1]

# plot the nytimes data
#source("reshape_nytimes_data.R")

# read in the matrix of cases and deaths
load("reshaped_nytimes_data.RData")

# this we'll have to read in from a text file
fname <- paste0("regions/",region,".txt")
states <- read.table(fname, as.is=TRUE)[,1]


library("ggplot2")
county_map <- map_data("county", region = states)
state_map <- map_data("state", region = states)

    
for(j in (ncol(cases)-3):ncol(cases)){
    county_map$cases <- NA
    county_map$deaths <- NA
    for( k in 1:nrow(st_co) ){
        i1 <- county_map$region == st_co$state[k]
        i2 <- county_map$subregion == st_co$county[k]
        inds <- i1 & i2
        county_map$cases[inds] <- cases[k,j]
        county_map$deaths[inds] <- deaths[k,j]
    }

    p1 <- ggplot() 
    p2 <- geom_polygon( 
        data = county_map,
        aes( x = long, y = lat, group = group, fill = cases), 
        color = NA,
    )
    p5 <- scale_fill_gradient(low = "yellow", high = "red", trans = "log10")
    p3 <- coord_map(projection = "lambert", parameters=c(25,50)) 
    p4 <- theme_void()
    p7 <- ggtitle("Confirmed Coronavirus Cases by County")
    p8 <- labs(caption = "Data Source:The New York Times, based on reports from state and local health agencies")
    p6 <- geom_polygon(
        data = state_map,
        aes(x = long, y = lat, group = group),
        color = "black",
        fill = "white",
        alpha = 0
    )
    fname <- paste0( region, j, ".png" )
    png(fname,width=800,height=600)
    print(p1 + p2 + p5 + p6 + p3 + p4 + p7 + p8)
    dev.off()
}
