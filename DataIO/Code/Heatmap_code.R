library(ggplot2)
library(sf)
library(ggmap)
library(osmdata) 
library(devtools)

# Ensure that day_of_week is a factor with the levels in the right order
df$day_of_week <- factor(df$day_of_week, levels = c("Mon", "Tue", "Wed", "Thu", "Fri", "Sat", "Sun"))

# Create a bar plot of ride count by day of the week
ggplot(df, aes(x = day_of_week)) +
  geom_bar() +
  xlab("Day of the Week") +
  ylab("Number of Rides") +
  ggtitle("Number of Rides per Day of the Week")

ggplot() +
  geom_sf(data = chicago_map) +  # This plots the map
  geom_point(aes(x = start_lng, y = start_lat, color = rideable_type), 
             data = bike_rides, alpha = 0.5) +
  labs(color = "Rideable Type") +
  theme_minimal() +
  ggtitle("Start Locations of Bike Rides in Chicago")

#google map image of chicago - base layer of map 
chicago_map <- get_map(location = 'Chicago', zoom = 10)

ggmap(chicago_map)

pdf('figures/Map.pdf', width = 9.3, height = 6.5)
par(mfrow=c(1,1), cex=0.75, mar=c(1,1,1,2), mgp=c(1.8,0.5,0), bty="L")
ggmap(chicago_map) +
  geom_point(data = df[1:10000,], aes(x = start_lng, y = start_lat), color = "red", size = 0.5, alpha = 0.5)
dev.off()














chicago_map <- get_googlemap(center = c(lon = -87.6298, lat = 41.8781), zoom = 11, scale = 2)

# Assuming your bike rides data is in a data frame called bike_rides
# and you have columns start_lat and start_lng

# Round the lat and lng to cluster nearby locations together
# Adjust the rounding factor to cluster more or less aggressively
df$rounded_start_lat <- round(df$start_lat, digits = 2)
df$rounded_start_lng <- round(df$start_lng, digits = 2)

# Aggregate the data to find the number of rides from each rounded location
aggregated_data <- df %>%
  group_by(rounded_start_lat, rounded_start_lng) %>%
  summarise(number_of_rides = n(), 
            avg_lat = mean(start_lat), 
            avg_lng = mean(start_lng)) %>%
  ungroup()

# Plot the map with the size of the points representing the number of rides
pdf('figures/Map.pdf', width = 9.3, height = 6.5)
par(mfrow=c(1,1), cex=0.75, mar=c(1,1,1,2), mgp=c(1.8,0.5,0), bty="L")
ggmap(chicago_map) +
  geom_point(data = aggregated_data, aes(x = avg_lng, y = avg_lat, size = number_of_rides), 
             color = "red", alpha = 0.5) +
  scale_size_continuous(range = c(1, 10)) + # Adjust the size range as needed
  guides(size = guide_legend(title = "Number of Rides"))
dev.off()

# Create a heatmap layer using stat_density2d
heatmap_layer <- stat_density2d(data = df[sample(1:nrow(df), size = 500000, replace=FALSE),], aes(x = start_lng, y = start_lat, fill = ..level.., alpha = ..level..), linewidth = 0.01, bins = 25, geom = "polygon")
heatmap_layer <- stat_density2d(data = df[sample(1:nrow(df), size = 100000, replace=FALSE),], aes(x = start_lng, y = start_lat, fill = ..level.., alpha = ..level..), geom = "polygon", adjust = 1.5)

# Plot the heatmap over the map
pdf('figures/HeatMap.pdf', width = 6.3, height = 6.5)
par(mfrow=c(1,1), cex=0.75, mar=c(1,1,1,2), mgp=c(1.8,0.5,0), bty="L")
ggmap(chicago_map) +
  heatmap_layer +
  scale_fill_gradient(low = "green", high = "red") +
  scale_alpha(range = c(0.3, 0.8), guide = FALSE) +
  ggtitle("Heatmap of Ride Starts in Chicago") +
  theme(legend.position = "none")
dev.off()
