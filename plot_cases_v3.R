
# plot the nytimes data
# source("reshape_nytimes_data.R")

args <- commandArgs(trailingOnly = TRUE)
print(args)

# manually define args while testing
args <- c("-125","-65","25","50","all")
library("ggplot2")
library("tidycensus")
library("sf")

# define variables from args
days_back <- 50
stem <- args[5]
#states <- args[3:length(args)]


# read in the matrix of cases and deaths
load("reshaped_nytimes_data_v2.RData")

# read in county_pop and state_map
load("stco_maps.RData")

# grab counties inside of bounding box
bbox <- as.numeric(args[1:4])

bbox_exp <- bbox + c(-3,+3,-3,+3)
check_inside <- function( r ){
    xy <- st_coordinates(r)
    m <- max( sp::point.in.polygon(xy[,1],xy[,2],
        bbox_exp[c(1,2,2,1)],bbox_exp[c(3,3,4,4)] ) )
    return(m==1)
}
is_inside <- rep(NA,nrow(county_pop))
for(j in 1:nrow(county_pop)){
    is_inside[j] <- check_inside(county_pop[j,])
}
county_pop0 <- county_pop[is_inside,]
cases0 <- cases[is_inside,]
deaths0 <- deaths[is_inside,]

# smooth the cases over time
smoother <- function(x){
    y <- x
    n <- length(x)
    y[1] <- mean(x[1:2])
    y[2] <- 0.25*x[1]+0.5*x[2]+0.25*x[3]
    for(j in 3:(n-2)){
        y[j] <- 0.1*(x[j-2]+x[j+2]) + 0.2*(x[j-1]+x[j+1]) + 0.4*x[j]
    }
    y[n-1] <- 0.25*x[n-2]+0.5*x[n-1]+0.25*x[n]
    y[length(x)] <- mean(x[c(n-1,n)])
    return(y)
}
cases1 <- t(apply(cases0,1,smoother))

# do the map projection calculations
target_crs <- '+proj=moll'
target_crs <- sprintf('+proj=laea +lon_0=%f +lat_0=%f',
                      mean(bbox[1:2]), mean(bbox[3:4]) )
county_pop_trans <- st_transform(county_pop0, crs = target_crs)
disp_win_wgs84 <- st_sfc(st_point(bbox[c(1,3)]), st_point(bbox[c(2,4)]),
                         crs = 4326)
disp_win_trans <- st_transform(disp_win_wgs84, crs = target_crs)
disp_win_coord <- st_coordinates(disp_win_trans)

# loop over days, make the plots
for(j in ((ncol(cases) - days_back):(ncol(cases)-1))){

    county_pop_trans$cases <- pmax(0,(cases1[,j]-cases1[,j-1]))/county_pop0$estimate*1e5
    county_pop0$deaths <- deaths0[,j]/county_pop0$estimate*1e5
    print(max(county_pop_trans$cases))
    t1 <- proc.time()
    p1 <- ggplot()
    p2a <- geom_sf(data = county_pop_trans, color = NA, mapping=aes(fill = cases) )
    p2b <- geom_sf(data = state_map, fill = NA, color = rgb(0.4,0.4,0.4) )
    p3 <- coord_sf(xlim = disp_win_coord[,'X'], ylim = disp_win_coord[,'Y'],
        datum = target_crs, expand = FALSE)
    #p3 <- coord_sf(xlim = bbox[1:2], ylim = bbox[3:4], expand = FALSE )
    p4 <- theme_void()
    p5 <- scale_fill_viridis_c(trans = "sqrt", limits = c(0,300))
    p7 <- labs(title = "COVID cases per 100k people per day")
    p8 <- labs(subtitle = format(all_dates[j], "%b. %d, %Y"))
    p9 <- labs(caption = "Data Source: The New York Times, based on reports from state and local health agencies")
    p10 <- theme( plot.title = element_text(hjust = 0.5,face = "bold"), 
        plot.subtitle = element_text(hjust = 0.5), 
        plot.caption = element_text(hjust = 0.5, face = "italic"),
        plot.margin = unit(c(0.01,0.01,0.01,0.01), "npc"),
        legend.title = element_blank(),
        legend.text = element_text(size =12),
        legend.position = "bottom",
        legend.key.width = unit(0.1,"npc"), 
        legend.key.height = unit(0.02,"npc") )
    fname <- paste0( "plots/", stem, "CASES", j, ".png" )
    png(fname,width=600,height=600)
    print(p1+p2a+p2b+p3+p4+p5+p7+p8+p9+p10)
    dev.off()

}
