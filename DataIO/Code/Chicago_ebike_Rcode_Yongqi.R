
#######################
#  Load the datasets  # 
#######################

January <- read.csv("C:/Users/yqliu/OneDrive - The Ohio State University/2024Spring/DataIO_Feb2024/Raw data/January.csv")
February <- read.csv("C:/Users/yqliu/OneDrive - The Ohio State University/2024Spring/DataIO_Feb2024/Raw data/February.csv")
March <- read.csv("C:/Users/yqliu/OneDrive - The Ohio State University/2024Spring/DataIO_Feb2024/Raw data/March.csv")
April <- read.csv("C:/Users/yqliu/OneDrive - The Ohio State University/2024Spring/DataIO_Feb2024/Raw data/April.csv")
May <- read.csv("C:/Users/yqliu/OneDrive - The Ohio State University/2024Spring/DataIO_Feb2024/Raw data/May.csv")
June <- read.csv("C:/Users/yqliu/OneDrive - The Ohio State University/2024Spring/DataIO_Feb2024/Raw data/June.csv")
July <- read.csv("C:/Users/yqliu/OneDrive - The Ohio State University/2024Spring/DataIO_Feb2024/Raw data/July.csv")
August <- read.csv("C:/Users/yqliu/OneDrive - The Ohio State University/2024Spring/DataIO_Feb2024/Raw data/August.csv")
September <- read.csv("C:/Users/yqliu/OneDrive - The Ohio State University/2024Spring/DataIO_Feb2024/Raw data/September.csv")
October <- read.csv("C:/Users/yqliu/OneDrive - The Ohio State University/2024Spring/DataIO_Feb2024/Raw data/October.csv")
November <- read.csv("C:/Users/yqliu/OneDrive - The Ohio State University/2024Spring/DataIO_Feb2024/Raw data/November.csv")
December <- read.csv("C:/Users/yqliu/OneDrive - The Ohio State University/2024Spring/DataIO_Feb2024/Raw data/December.csv")

df <- rbind(January, February, March, April, 
            May, June, July, August, 
            September, October, November, December)

dim(df) 
# 5829030      13

colnames(df)
# [1] "ride_id"            "rideable_type"      "started_at"         "ended_at"          
# [5] "start_station_name" "start_station_id"   "end_station_name"   "end_station_id"    
# [9] "start_lat"          "start_lng"          "end_lat"            "end_lng"           
# [13] "member_casual" 


# Check if and where are the missing data 
colSums(is.na(df) | df == "")
#          ride_id      rideable_type         started_at           ended_at start_station_name 
#                0                  0                  0                  0             834545 
# start_station_id   end_station_name     end_station_id          start_lat          start_lng 
#           834677             891757             891898                  0                  0 
#          end_lat            end_lng      member_casual 
#             5961               5961                  0 




###########################################################################
##  Convert the time variables, and calculate the duration of each ride  ##
###########################################################################

# Month, Day of a week, Weekday/Weekend variables # 
Month <- strftime(df$started_at, "%B") 
DayOfWeek <- strftime(df$started_at, "%A") 
Weekday <- ifelse(DayOfWeek == "Saturday" | DayOfWeek == "Sunday", "Weekend", "Weekday")
table(Month, useNA="ifany")
#   April    August  December  February   January      July      June     March       May  November 
#  426590    785932    181806    190445    190301    823488    769204    258678    604827    337735 
# October September 
#  558685    701339
table(DayOfWeek, useNA="ifany")
# Friday    Monday  Saturday    Sunday  Thursday   Tuesday Wednesday 
# 848026    729223    908587    770189    878250    823517    871238
table(Weekday, useNA="ifany")
# Weekday Weekend 
# 4150254 1678776

# Convert the starting and ending time variables into time objects # 
df$started_at <- strptime(df$started_at, format="%Y-%m-%d %H:%M:%S")
df$ended_at <- strptime(df$ended_at, format="%Y-%m-%d %H:%M:%S")

# The duration of each ride # 
duration <- difftime(df$ended_at, df$started_at, units="mins")
summary(as.numeric(duration, units="mins"))
#      Min.   1st Qu.    Median      Mean   3rd Qu.      Max. 
# -10353.35      5.58      9.85     18.71     17.62  41387.25
quantile(as.numeric(duration, units="mins"), c(0.001, seq(0.05, 0.95, 0.05), 0.95, 0.999))
#         0.1%           5%          10%          15%          20%          25%          30%          35%
# 3.333333e-02 2.183333e+00 3.283333e+00 4.100000e+00 4.850000e+00 C5.583333e+00 6.333333e+00 7.133333e+00 
#          40%          45%          50%          55%          60%          65%          70%          75% 
# 7.966667e+00 8.866667e+00 9.850000e+00 1.095000e+01 1.221667e+01 1.368333e+01 1.543333e+01 1.761667e+01 
#          80%          85%          90%          95%          95%        99.9% 
# 2.043333e+01 2.436667e+01 3.046667e+01 4.321667e+01 4.321667e+01 1.162867e+03

quantile(as.numeric(duration, units="mins"), c(seq(0.95, 1, by=0.01)))


sum(duration <= 0 | duration > 2000)  # 1985
sum(duration < 3.333333e-02 | duration > 1.162867e+03) # 7295

sum(duration <= 0) # 567 
sum(duration == 0) # 455
duration[duration < 0]

df_new <- df[duration >= 3.333333e-02 & duration <= 1.162867e+03, ]
df_new$duration <- (as.numeric(duration, units="mins")[duration >= 3.333333e-02 & duration <= 1.162867e+03])
df_new$Weekday <- Weekday[duration >= 3.333333e-02 & duration <= 1.162867e+03]
df_new$Month <- Month[duration >= 3.333333e-02 & duration <= 1.162867e+03]
df_new$DayOfWeek <- DayOfWeek[duration >= 3.333333e-02 & duration <= 1.162867e+03]
df_new$DayOfWeek <- factor(df_new$DayOfWeek, levels=c("Monday", "Tuesday", "Wednesday", "Thursday", 
                                                      "Friday", "Saturday", "Sunday"))

# save(duration, file="C:/Users/yqliu/OneDrive - The Ohio State University/2024Spring/DataIO_Feb2024/duration.RData")


windows()
par(mfrow=c(3,1))

hist(df_new$duration[df_new$duration < 150], xlab="Mins", main="Duration")
abline(v=20, col="red")
abline(v=45, col="green")

hist(df_new$duration[df_new$duration < 150 & df_new$Weekday == "Weekday"], xlab="Mins", main="Weekday Duration", ylim=c(0, 1250000))
abline(v=20, col="red")
abline(v=45, col="green")

hist(df_new$duration[df_new$duration < 150 & df_new$Weekday == "Weekend"], xlab="Mins", main="Weekend Duration", ylim=c(0, 1250000))
abline(v=20, col="red")
abline(v=45, col="green")



## Test -----------------------------------

model1 <- lm(as.numeric(duration, units="mins")[1:10000] ~ Weekday[1:10000] + df$start_station_name[1:10000])
summary(model1)

model2 <- lm(as.numeric(duration, units="mins")[1:10000] ~ df$start_station_name[1:10000] + df$end_station_name[1:10000])
summary(model2)

model3 <- lm(log(df_new$duration) ~ df_new$member_casual + df_new$Weekday + df_new$rideable_type)
summary(model3)

model4 <- glm(duration ~ member_casual + Weekday + Month + rideable_type, family=gaussian(link="log"), data=df_new)
summary(model4)
anova(model4, test="Chi")

## End Test --------------------------------


hist(log(df_new$duration), xlab="Mins", main="Duration")
# After log transformation, duration seems to be Normally distributed. 


# Boxplot of the duration, as a function of member_causal and Day of Week 
df_new_sub <- df_new[df_new$duration < 60,]
library(ggplot2)
plot1 <- ggplot(df_new_sub, aes(x = DayOfWeek, y = duration, fill = member_casual)) +
  geom_boxplot() + 
  scale_fill_manual(values = c("#666666", "#BB0000")) + 
  scale_x_discrete(name ="Days of the Week") 
plot1 


model5 <- glm(duration ~ member_casual + DayOfWeek + rideable_type, family=gaussian(link="log"), data=df_new)
summary(model5)
# Call:
#   glm(formula = duration ~ member_casual + DayOfWeek + rideable_type, 
#       family = gaussian(link = "log"), data = df_new)
# 
# Coefficients:
#   Estimate Std. Error  t value Pr(>|t|)    
# (Intercept)                 3.199143   0.001604 1995.088   <2e-16 ***
#   member_casualmember        -0.464865   0.001462 -317.918   <2e-16 ***
#   DayOfWeekTuesday           -0.048445   0.002647  -18.300   <2e-16 ***
#   DayOfWeekWednesday         -0.063139   0.002629  -24.015   <2e-16 ***
#   DayOfWeekThursday          -0.043884   0.002584  -16.986   <2e-16 ***
#   DayOfWeekFriday            -0.005567   0.002511   -2.217   0.0266 *  
#   DayOfWeekSaturday           0.109576   0.002300   47.640   <2e-16 ***
#   DayOfWeekSunday             0.128197   0.002353   54.478   <2e-16 ***
#   rideable_typedocked_bike    0.748539   0.001743  429.430   <2e-16 ***
#   rideable_typeelectric_bike -0.320655   0.001446 -221.809   <2e-16 ***
#   ---
#   Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1
# 
# (Dispersion parameter for gaussian family taken to be 649.763)
# 
# Null deviance: 4051917741  on 5821734  degrees of freedom
# Residual deviance: 3782705290  on 5821725  degrees of freedom
# AIC: 54226418
# 
# Number of Fisher Scoring iterations: 8

anova(model5, test="Chi")
# Analysis of Deviance Table
# 
# Model: gaussian, link: log
# 
# Response: duration
# 
# Terms added sequentially (first to last)
# 
# 
# Df  Deviance Resid. Df Resid. Dev  Pr(>Chi)    
# NULL                         5821734 4051917741              
# member_casual  1 100390288   5821733 3951527453 < 2.2e-16 ***
# DayOfWeek      6  13209945   5821727 3938317508 < 2.2e-16 ***
# rideable_type  2 155612218   5821725 3782705290 < 2.2e-16 ***  


Newdata <- data.frame(member_casual=rep(c("member", "casual"), 21), 
                      DayOfWeek=rep(rep(c("Monday", "Tuesday", "Wednesday", "Thursday", "Friday", "Saturday", "Sunday"), each=2), 3), 
                      rideable_type=rep(c("classic_bike", "docked_bike", "electric_bike"), each=14))
Newdata$predict_duration <- predict(model5, newdata=Newdata, type="response")



#-------------------------------------------

###################
##  Final Model  ##
###################

model6 <- glm(duration ~ member_casual + DayOfWeek, family=gaussian(link="log"), data=df_new)
summary(model6)
# Call:
#   glm(formula = duration ~ member_casual + DayOfWeek, family = gaussian(link = "log"), 
#       data = df_new)
# 
# Coefficients:
#   Estimate Std. Error  t value Pr(>|t|)    
# (Intercept)          2.990791   0.002079 1438.730   <2e-16 ***
#   member_casualmember -0.510462   0.001425 -358.152   <2e-16 ***
#   DayOfWeekTuesday    -0.056829   0.002842  -19.994   <2e-16 ***
#   DayOfWeekWednesday  -0.073578   0.002821  -26.080   <2e-16 ***
#   DayOfWeekThursday   -0.054617   0.002773  -19.694   <2e-16 ***
#   DayOfWeekFriday     -0.001917   0.002695   -0.711    0.477    
#   DayOfWeekSaturday    0.141973   0.002473   57.398   <2e-16 ***
#   DayOfWeekSunday      0.164789   0.002531   65.096   <2e-16 ***
#   ---
#   Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1
# 
# (Dispersion parameter for gaussian family taken to be 676.5651)
# 
# Null deviance: 4051917741  on 5821734  degrees of freedom
# Residual deviance: 3938317508  on 5821727  degrees of freedom
# AIC: 54461113
# 
# Number of Fisher Scoring iterations: 7


Newdata <- data.frame(member_casual=rep(c("member", "casual"), 7), 
                      DayOfWeek=rep(c("Monday", "Tuesday", "Wednesday", "Thursday", "Friday", "Saturday", "Sunday"), each=2))
Newdata$predict_duration <- predict(model6, newdata=Newdata, type="response")
Newdata
#    member_casual DayOfWeek predict_duration
# 1         member    Monday         11.94519
# 2         casual    Monday         19.90142
# 3         member   Tuesday         11.28529
# 4         casual   Tuesday         18.80198
# 5         member Wednesday         11.09785
# 6         casual Wednesday         18.48969
# 7         member  Thursday         11.31027
# 8         casual  Thursday         18.84360
# 9         member    Friday         11.92231
# 10        casual    Friday         19.86330
# 11        member  Saturday         13.76738
# 12        casual  Saturday         22.93730
# 13        member    Sunday         14.08510
# 14        casual    Sunday         23.46664


df_new_sub$member_casual <- factor(df_new_sub$member_casual, levels = c("casual", "member"), labels = c("Casual", "Member"))
Newdata$member_casual <- factor(Newdata$member_casual, levels = c("casual", "member"), labels = c("Casual", "Member"))

plot2 <- ggplot(df_new_sub, aes(x = DayOfWeek, y = duration, fill = member_casual)) +
  geom_boxplot() + 
  scale_fill_manual(values = c("#666666", "#BB0000")) + 
  scale_x_discrete(name ="Day of the Week") + 
  scale_y_discrete(name ="Duration (mins)") + 
  guides(color = guide_legend(title = "Member Type")) +
  labs(fill = "Member Type") +
  theme_minimal() +
  theme(axis.title.x = element_text(size = 14), # Increase x-axis label font size
        axis.title.y = element_text(size = 14), # Increase y-axis label font size
        legend.title = element_text(size = 14),
        legend.text = element_text(size=14)) +
  geom_point(data = Newdata, aes(x=DayOfWeek, y=predict_duration), color = 'green', 
             position=position_dodge(width=0.75))
plot2 


pdf('C:/Users/yqliu/OneDrive - The Ohio State University/2024Spring/DataIO_Feb2024/Boxplot_with_prediction.pdf', width = 9.3, height = 6.5)
par(mfrow=c(1,1), cex=0.75, mar=c(1,1,1,2), mgp=c(1.8,0.5,0), bty="L")
plot2
dev.off()

save.image("C:/Users/yqliu/OneDrive - The Ohio State University/2024Spring/DataIO_Feb2024/Chicago_ebike_workspace.RData")


