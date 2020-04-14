
# plot the nytimes data
#source("reshape_nytimes_data.R")

args <- commandArgs(trailingOnly = TRUE)

# manually define args while testing
# args <- c("AllStates","30")

# define variables from args
days_back <- as.numeric(args[1])
file_stem <- args[2]
states <- args[3:length(args)]


# read in the matrix of cases and deaths
load("reshaped_nytimes_data.RData")

# this we'll have to read in from a text file
#fname <- paste0("regions/",region,".txt")
#states <- read.csv(fname, header=FALSE,as.is=TRUE)[,1]

library("ggplot2")
county_map <- map_data("county", region = states)
state_map <- map_data("state", region = states)

# loads in object county_demog
load("county_demog.RData")

for(j in ((ncol(cases) - days_back):ncol(cases))){
    county_map$cases <- NA
    county_map$deaths <- NA
    for( k in 1:nrow(st_co) ){
        i1 <- county_map$region == st_co$state[k]
        i2 <- county_map$subregion == st_co$county[k]
        inds <- i1 & i2
        # get inds from demographic table
        j1 <- county_demog$state == st_co$state[k]
        # get map inds
        j2 <- county_demog$county == st_co$county[k]
        jj <- which( j1 & j2 )
        per_num <- 1e5
        if( length(jj) != 0 ){
            pop <- county_demog$pop[jj]        
            county_map$cases[inds] <- ceiling(per_num*cases[k,j]/pop)        
            county_map$deaths[inds] <- ceiling(per_num*deaths[k,j]/pop)
        }
    }

    p1 <- ggplot() 
    p2 <- geom_polygon( 
        data = county_map,
        aes( x = long, y = lat, group = group, fill = cases), 
        color = NA
    )
    p3 <- coord_map(projection = "lambert", parameters=c(25,50)) 
    p4 <- theme_void()
    p5 <- scale_fill_viridis_c(limits = c(1,2500))
    p6 <- geom_polygon(
        data = state_map,
        aes(x = long, y = lat, group = group),
        color = "black",
        fill = "white",
        alpha = 0
    )
    p7 <- labs(title = "Confirmed Coronavirus Cases by County")
    p8 <- labs(subtitle = format(all_dates[j], "%b. %d, %Y"))
    p9 <- labs(caption = "Data Source:The New York Times, based on reports from state and local health agencies")
    p10 <- theme( plot.title = element_text(hjust = 0.5, face = "bold"), plot.subtitle = element_text(hjust = 0.5), plot.caption = element_text(hjust = 0.5, face = "italic"))
    fname <- paste0( "plots/", file_stem, "CASES", j, ".png" )
    png(fname,width=800,height=600)
    print(p1 + p2 + p5 + p6 + p3 + p4 + p7 + p8 + p9 + p10 ) 
    dev.off()
}
