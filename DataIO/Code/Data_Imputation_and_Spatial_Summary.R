load("../Data/ebike.RData")

library(parallel)
closest.coord <- function(coords, grid){
  return(simplify2array(mclapply(1:nrow(coords), function(i) (which.min((grid[,1]-coords[i,1])^2 + (grid[,2]-coords[i,2])^2)), mc.cores = 5))) 
}

end_missing_id <- which(is.na(df$end_lat))

df <- df[-end_missing_id,]



#na.cts <- sapply(names(df), function(x) sum(is.null(df$x)))

#missing.id.start <- which(df$start_station_id == "")

#unique.stations.id <- unique(df$start_station_id[-missing.id.start])
#unique.stations <- data.frame(unique.stations.id)
#unique.stations$long <- NA
#unique.stations$lat <- NA


unique.stations <- read.csv("../Data/Unique_Stations.csv")



#for(i in 1:length(unique.stations.id)){
#  long <- as.data.frame(table(unique(round(df$start_lng[df$start_station_id == unique.stations.id[i]], 6))), stringsAsFactors =  F)
#  lat <- as.data.frame(table(unique(round(df$start_lat[df$start_station_id == unique.stations.id[i]], 6))), stringsAsFactors = F)
#  name <- as.data.frame(table(unique((df$start_station_name[df$start_station_id == unique.stations.id[i]]))), stringsAsFactors = F)
#  if(max(long$Freq) == 1){
#    unique.stations$long[i] <- mean(as.numeric(long$Var1))
#  }else{
#    unique.stations$long[i] <- as.numeric(unique(long$Var1[long$Freq == max(long$Freq)]))
#  }
#  if(max(lat$Freq) == 1){
#    unique.stations$lat[i] <- mean(as.numeric(lat$Var1))
#  }else{
#    unique.stations$lat[i] <- as.numeric(unique(lat$Var1[lat$Freq == max(lat$Freq)]))
#  }
#  if(max(name$Freq == 1)){
#    unique.stations$name[i] <- name$Var1[1]
#  }else{
#    unique.stations$name[i] <- unique(name$Var[name$Freq == max(name$Freq)])[1]
#  }
#  if(i %%100 == 0){
#    cat(i)
#  }
#}

#write.csv(unique.stations, "../Data/Unique_Stations.csv", row.names = F)


#rm(lat, long, name)


sapply(c(1:14), function(i) sum(df[,i] == ""))


#sapply(c(1:14), function(i) sum(df[,i] == ""))
#start_unique_station_id <- closest.coord(cbind(df$start_lng, df$start_lat), 
#                                         cbind(unique.stations$long, unique.stations$lat))


#start_unique_station_id_1 <- closest.coord(cbind(df$start_lng, df$start_lat), 
#                                         cbind(unique.stations$long, unique.stations$lat))

start_unique_station_id_1 <- closest.coord(cbind(df$start_lng[1:1000000], df$start_lat[1:1000000]), 
                                         cbind(unique.stations$long, unique.stations$lat))
start_unique_station_id_2 <- closest.coord(cbind(df$start_lng[1000001:2000000], df$start_lat[1000001:2000000]), 
                                         cbind(unique.stations$long, unique.stations$lat))
start_unique_station_id_3 <- closest.coord(cbind(df$start_lng[2000001:3000000], df$start_lat[2000001:3000000]), 
                                         cbind(unique.stations$long, unique.stations$lat))
start_unique_station_id_4 <- closest.coord(cbind(df$start_lng[3000001:4000000], df$start_lat[3000001:4000000]), 
                                         cbind(unique.stations$long, unique.stations$lat))
start_unique_station_id_5 <- closest.coord(cbind(df$start_lng[4000001:5000000], df$start_lat[4000001:5000000]), 
                                         cbind(unique.stations$long, unique.stations$lat))
start_unique_station_id_6 <- closest.coord(cbind(df$start_lng[5000001:nrow(df)], df$start_lat[5000001:nrow(df)]), 
                                         cbind(unique.stations$long, unique.stations$lat))




#table(df$end_lat, decreasing = TRUE)

#end_missing_id <- which(is.na(df$end_lat))

#df$end_station_name[end_missing_id]
#df$end_station_id[end_missing_id]

#df[end_missing_id[1:200],]

start_stations_ids <- c(start_unique_station_id_1, start_unique_station_id_2,
                        start_unique_station_id_3, start_unique_station_id_4,
                        start_unique_station_id_5, start_unique_station_id_6)

rm(start_unique_station_id_1, start_unique_station_id_2,
                        start_unique_station_id_3, start_unique_station_id_4,
                        start_unique_station_id_5, start_unique_station_id_6)

end_unique_station_id_1 <- closest.coord(cbind(df$end_lng[1:1000000], df$end_lat[1:1000000]), 
                                         cbind(unique.stations$long, unique.stations$lat))
end_unique_station_id_2 <- closest.coord(cbind(df$end_lng[1000001:2000000], df$end_lat[1000001:2000000]), 
                                         cbind(unique.stations$long, unique.stations$lat))
end_unique_station_id_3 <- closest.coord(cbind(df$end_lng[2000001:3000000], df$end_lat[2000001:3000000]), 
                                         cbind(unique.stations$long, unique.stations$lat))
end_unique_station_id_4 <- closest.coord(cbind(df$end_lng[3000001:4000000], df$end_lat[3000001:4000000]), 
                                         cbind(unique.stations$long, unique.stations$lat))
end_unique_station_id_5 <- closest.coord(cbind(df$end_lng[4000001:5000000], df$end_lat[4000001:5000000]), 
                                         cbind(unique.stations$long, unique.stations$lat))
end_unique_station_id_6 <- closest.coord(cbind(df$end_lng[5000001:nrow(df)], df$end_lat[5000001:nrow(df)]), 
                                         cbind(unique.stations$long, unique.stations$lat))


end_stations_ids <- c(end_unique_station_id_1, end_unique_station_id_2,
                        end_unique_station_id_3, end_unique_station_id_4,
                        end_unique_station_id_5, end_unique_station_id_6)

rm(end_unique_station_id_1, end_unique_station_id_2,
                        end_unique_station_id_3, end_unique_station_id_4,
                        end_unique_station_id_5, end_unique_station_id_6)






marks_table <- as.data.frame(table(start_stations_ids), stringsAsFactors = F)
marks_id_start <- marks_table$start_stations_ids
marks_val_start <- marks_table$Freq

marks_table <- as.data.frame(table(end_stations_ids), stringsAsFactors = F)
marks_id_end <- marks_table$end_stations_ids
marks_val_end <- marks_table$Freq
rm(marks_table)


## Point process modeling
##
library(sf)
library(spatstat)
sh <- st_transform(st_read("../Data/City_Shapes/CityBoundaries.shp"), "+proj=longlat")
chicago_shape <- sh[214,]$geometry
rm(sh)

chicago_coords <- st_coordinates(chicago_shape)[1:6778,1:2]
chicago_coords[nrow(chicago_coords),]
chicago_coords <- rbind(chicago_coords, chicago_coords[1,])

chicago_coords <- chicago_coords[nrow(chicago_coords):1,]
win_chicago <- owin(poly = chicago_coords)
rm(chicago_coords)

# points(unique.stations$long, unique.stations$lat, pch = 19, cex = 0.1)

sub_id <- sample(c(1:nrow(df)), size = 10000)

chicago_stations_start <- as.ppp(ppp(unique.stations$long[as.numeric(marks_id_start)], 
                                  unique.stations$lat[as.numeric(marks_id_start)], 
                                  window = win_chicago, marks = log(marks_val_start)))

chicago_stations_end <- as.ppp(ppp(unique.stations$long[as.numeric(marks_id_end)], 
                                  unique.stations$lat[as.numeric(marks_id_end)], 
                                  window = win_chicago, marks = log(marks_val_end)))
plot(chicago_stations_start)


#tag <- as.character(c(1,2,3,4,5,6,7,8,9,10))
#species <- c("A","A","A","A","B","B","B","C","C","D")
#diameter <- c(50,20,55,30,30,45,15,20,35,45)
#x <- c(9,4,5,14,8,19,9,12,10,2)
#y <- c(6,7,15,16,12,4,19,2,14,9)
#df <- data.frame(tag, species, diameter, x, y)
#species_map <- ppp(df$x, df$y, c(0,20), c(0,20))
#marks(species_map) <- data.frame(m1 = df$species, m2=(df$diameter))

marks_range <- range(c(marks(chicago_stations_start), marks(chicago_stations_end)))

assign_colors <- function(x, marks_val){
  if(x > quantile((marks_val), 0.75)){
    return('gray5')
  }else if(x > quantile((marks_val), 0.5)){
    return('gray30')
  }else if(x > quantile((marks_val), 0.25)){
    return('gray60')
  }else{
    'gray90'
  }
}

marks_col_start <- sapply(log(marks_val_start), function(x) assign_colors(x, marks_val = log(marks_val_start)))
marks_col_end <- sapply(log(marks_val_end), function(x) assign_colors(x, marks_val = log(marks_val_end)))

pdf('../Figures/Chicag_Stations_Colored.pdf', width = 12, height = 6.5)
par(mfrow=c(1,2), cex=0.75, mar=c(1,1,1,2), mgp=c(1.8,0.5,0), bty="L")
plot(win_chicago, main = 'Start')
points(chicago_stations_start$x, chicago_stations_start$y, pch = 19,cex = 0.5,
       main = "Start", col = marks_col_start)
plot(win_chicago, main = 'End')
points(chicago_stations_end$x, chicago_stations_end$y, pch = 19,cex = 0.5,
       main = "End", col = marks_col_end)
legend('bottomleft', pch = 19, col = c('gray90', 'gray60', 'gray30', 'gray5'),
       legend = c('0- 25%', '25% - 50%', '50% - 75%', '75% - 100%'))
dev.off()



pdf('../Figures/Chicago_Stations.pdf', width = 12, height = 6.5)
par(mfrow=c(1,1), cex=0.75, mar=c(1,1,1,2), mgp=c(1.8,0.5,0), bty="L")
plot(win_chicago, main = 'Start')
points(chicago_stations_start$x, chicago_stations_start$y, pch = 19,cex = 0.5,
       main = "Start")
dev.off()


stations <- as.ppp(ppp(unique.stations$long, unique.stations$lat, window = win_chicago))
plot((density(stations, sigma = bw.diggle(stations))),
     main = "Station Density in Chicago")
plot(win_chicago, add = T)

trips <- as.ppp(ppp(df$start_lng, df$start_lat, window = win_chicago))


pdf('../Figures/Chicago_Start_Stations.pdf', width = 6.3, height = 6.5)
par(mfrow=c(1,1), cex=0.75, mar=c(1,1,1,2), mgp=c(1.8,0.5,0), bty="L")
plot(chicago_stations, markrange = marks_range, markscale = 0.001,
     main = "", legend = F)
dev.off()



png('../Figures/Chicago_Start_Stations.png', width = 1630, height = 1650)
par(bg = NA)
plot(chicago_stations, markrange = marks_range, markscale = 0.001,
     main = "", legend = F)
dev.off()


png('../Figures/Chicago_Start_Stations_Colored.png', width = 1630, height = 1650)
par(bg = NA)
plot(chicago_stations, markrange = marks_range, markscale = 0.0005,
     main = "", cols = marks_col, bg = marks_col, legend = F)
legend('bottomleft', pch = 19, col = c('gray', 'green', 'blue', 'red'),
       legend = c('0- 25%', '25% - 50%', '50% - 75%', '75% - 100%'))
dev.off()


png('../Figures/Chicago_Start_Stations_Colored_NO_LEGEND.png', width = 1630, height = 1650)
par(bg = NA)
plot(chicago_stations, markrange = marks_range, markscale = 0.0005,
     main = "", cols = marks_col, bg = marks_col, legend = F)
dev.off()




save.df <- data.frame(long = chicago_stations$x, lat = chicago_stations$y, marks = chicago_stations$marks)

write.csv(save.df, "../Data/chicago_stations_summary.csv")
