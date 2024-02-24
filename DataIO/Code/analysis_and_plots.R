library(dplyr)
library(lubridate)
library(ggplot2)
library(scales)

df = load("/Data/ebike.RData")

# Peak hours for bike usesage

head(df)

df_names = colnames(df)

# Check number of NA in dataset
for(i in df_names){
  cat(i,sum(is.na(df[,i])),"\n")
}

# Ensure started_at is in datetime format
df$started_at <- as.POSIXct(df$started_at)

# Extract hour from started_at
df <- df %>%
  mutate(start_hour = hour(started_at))

# Categorize each ride as 'Weekday' or 'Weekend' and extract the start hour
df <- df %>%
  mutate(
    day_type = ifelse(wday(started_at, label = TRUE) %in% c("Sat", "Sun"), "Weekend", "Weekday"),
    start_hour = hour(started_at)
  )

# Aggregate and count rides by hour
peak_hours <- df %>%
  group_by(start_hour) %>%
  summarise(ride_count = n()) %>%
  arrange(desc(ride_count))

# View the results
print(peak_hours)

# Convert 'start_hour' to AM/PM format
peak_hours$formatted_hour <- format(strptime(paste(peak_hours$start_hour, "00"), format = "%H %M"), format = "%I %p")

hours_order <- c("01 AM", "02 AM", "03 AM", "04 AM", "05 AM", "06 AM", "07 AM", "08 AM", "09 AM", "10 AM", "11 AM", "12 PM",
                 "01 PM", "02 PM", "03 PM", "04 PM", "05 PM", "06 PM", "07 PM", "08 PM", "09 PM", "10 PM", "11 PM", "12 AM")

# Convert 'formatted_hour' to a factor with levels in the desired order
peak_hours$formatted_hour <- factor(peak_hours$formatted_hour, levels = hours_order)

# Plot bike usesage by hour
pdf('figures/Bike_Usage_by_Hour.pdf', width = 9.3, height = 6.5)
par(mfrow=c(1,1), cex=0.75, mar=c(1,1,1,2), mgp=c(1.8,0.5,0), bty="L")
ggplot(peak_hours, aes(x = formatted_hour, y = ride_count, fill = formatted_hour)) +
  geom_bar(stat = "identity", show.legend = FALSE) +  # Remove legend for simplicity
  scale_fill_manual(values = rep("#BB0000", length(unique(peak_hours$formatted_hour)))) +  # Use Scarlet for bars
  scale_y_continuous(
    labels = scales::comma_format(scale = 1e-4, suffix = "K"),
    breaks = seq(0, max(peak_hours$ride_count, na.rm = TRUE), by = 50000)
  ) +
  theme_minimal() +
  theme(
    axis.text.x = element_text(angle = 90, hjust = 1, color = "#666666") ) +
  labs(title = "Bike Usage by Hour", x = "Hour of the Day", y = "Number of Rides (Thousands)")
dev.off()

# Group by 'start_hour' and 'member_casual', then count the number of rides
peak_hours_member_casual <- df %>%
  group_by(start_hour, member_casual) %>%
  summarise(ride_count = n(), .groups = 'drop')

# Convert 'start_hour' to AM/PM format
peak_hours_member_casual$formatted_hour <- format(strptime(paste(peak_hours_member_casual$start_hour, "00"), format = "%H %M"), format = "%I %p")

hours_order <- c("01 AM", "02 AM", "03 AM", "04 AM", "05 AM", "06 AM", "07 AM", "08 AM", "09 AM", "10 AM", "11 AM", "12 PM",
                 "01 PM", "02 PM", "03 PM", "04 PM", "05 PM", "06 PM", "07 PM", "08 PM", "09 PM", "10 PM", "11 PM", "12 AM")

# Convert 'formatted_hour' to a factor with levels in the desired order
peak_hours_member_casual$formatted_hour <- factor(peak_hours_member_casual$formatted_hour, levels = hours_order)

# View the result
head(peak_hours_member_casual)

# Assuming 'peak_hours' has columns for 'formatted_hour', 'ride_count', and 'member_casual'
# Ensure 'member_casual' is a factor with levels named appropriately
peak_hours_member_casual$member_casual <- factor(peak_hours_member_casual$member_casual, levels = c("casual", "member"), labels = c("Casual", "Member"))

# Plot bike usesage by hour and rider type
pdf('figures/Bike_Usage_by_Hour_And_Rider_Type.pdf', width = 9.3, height = 6.5)
par(mfrow=c(1,1), cex=0.75, mar=c(1,1,1,2), mgp=c(1.8,0.5,0), bty="L")
ggplot(peak_hours_member_casual, aes(x = start_hour, y = ride_count / 1000, fill = member_casual)) +
  geom_bar(stat = "identity", position = "dodge") +
  scale_fill_manual(values = c("Casual" = "#BB0000", "Member" = "#666666")) +
  scale_y_continuous(breaks = pretty_breaks(n = 5), labels = label_number(suffix = "K")) +
  theme_minimal() +
  labs(title = "Weekend Bike Usage by Hour",
       x = "Hour of the Day",
       y = "Number of Rides (Thousands)",
       fill = "Member Type") +
  theme(axis.text.x = element_text(angle = 90, hjust = 1))
dev.off()

# Group by 'start_hour' and 'member_casual', then count the number of rides
peak_hw <- df %>%
  group_by(day_type, start_hour, member_casual) %>%
  summarise(ride_count = n(), .groups = 'drop')

# Convert 'start_hour' to AM/PM format
peak_hw$formatted_hour <- format(strptime(paste(peak_hw$start_hour, "00"), format = "%H %M"), format = "%I %p")

hours_order <- c("01 AM", "02 AM", "03 AM", "04 AM", "05 AM", "06 AM", "07 AM", "08 AM", "09 AM", "10 AM", "11 AM", "12 PM",
                 "01 PM", "02 PM", "03 PM", "04 PM", "05 PM", "06 PM", "07 PM", "08 PM", "09 PM", "10 PM", "11 PM", "12 AM")

# Convert 'formatted_hour' to a factor with levels in the desired order
peak_hw$formatted_hour <- factor(peak_hw$formatted_hour, levels = hours_order)

# View the result
head(peak_hw)

# Assuming 'peak_hours' has columns for 'formatted_hour', 'ride_count', and 'member_casual'
# Ensure 'member_casual' is a factor with levels named appropriately
peak_hw$member_casual <- factor(peak_hw$member_casual, levels = c("casual", "member"), labels = c("Casual", "Member"))

# Plot bike usesage by hour and rider type Weekday
pdf('figures/Weekday_Bike_Usage_by_Hour_and_Rider_Type.pdf', width = 9.3, height = 6.5)
par(mfrow=c(1,1), cex=0.75, mar=c(1,1,1,2), mgp=c(1.8,0.5,0), bty="L")
ggplot(filter(peak_hw, day_type == "Weekday"), aes(x = formatted_hour, y = ride_count / 1000, fill = member_casual)) +
  geom_bar(stat = "identity", position = "dodge") +
  scale_fill_manual(values = c("Casual" = "#BB0000", "Member" = "#666666")) +
  scale_y_continuous(breaks = pretty_breaks(n = 5), labels = label_number(suffix = "K")) +
  theme_minimal() +
  labs(title = "Weekday Bike Usage by Hour", x = "Hour of the Day", y = "Number of Rides (Thousands)", fill = "Member Type") +
  theme(axis.text.x = element_text(angle = 90, hjust = 1))
dev.off()

# Weekend Plot
pdf('figures/Weekend_Bike_Usage_by_Hour_and_Rider_Type.pdf', width = 9.3, height = 6.5)
par(mfrow=c(1,1), cex=0.75, mar=c(1,1,1,2), mgp=c(1.8,0.5,0), bty="L")
ggplot(filter(peak_hw, day_type == "Weekend"), aes(x = formatted_hour, y = ride_count / 1000, fill = member_casual)) +
  geom_bar(stat = "identity", position = "dodge") +
  scale_fill_manual(values = c("Casual" = "#BB0000", "Member" = "#666666")) +
  scale_y_continuous(breaks = pretty_breaks(n = 5), labels = label_number(suffix = "K")) +
  theme_minimal() +
  labs(title = "Weekend Bike Usage by Hour", x = "Hour of the Day", y = "Number of Rides (Thousands)", fill = "Member Type") +
  theme(axis.text.x = element_text(angle = 90, hjust = 1))
dev.off()