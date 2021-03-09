library(tidyverse)
library(grDevices)
library(hrbrthemes)
library(viridis)

Sys.setlocale("LC_ALL","English")
setwd("E:/Code/R/Push_ups")
# getting all files matching this format: KeepTrack_24_12_2020.csv
file_list <- list.files(paste(getwd(),"/data",sep=""), "\\KeepTrack_[0-9]{2}_[0-9]{2}_[0-9]{4}[.]csv$")
# deleting the "KeepTrack_" and ".csv" from the data name, ex: "24_12_2020" remains
get_current_data <- str_remove(str_remove(file_list, "KeepTrack_"), ".csv")
# remains are being converted to the date format
get_current_data <- as.Date(get_current_data, format = "%d_%m_%Y")
get_current_data <- max(get_current_data) # getting most current date from all files
# creating a new string with our most current date found so we can prepare the file to be read
filename <- paste("KeepTrack_", as.character(get_current_data, "%d_%m_%Y"), ".csv", sep="")
current_data <- read.csv(paste("data/", filename, sep=""), header=FALSE)
# now we only want the data that comes after "Push ups", but before "Rauchen "
rowfilter <- rownames(subset(current_data, current_data$V1 == "Push ups" | current_data$V1 == "Rauchen "))
rowfilter_start = as.numeric(rowfilter[1])+1
rowfilter_end   = as.numeric(rowfilter[2])-1 

current_data <- current_data[rowfilter_start:rowfilter_end, ]
rownames(current_data) <- NULL #resetting our row numbers

current_data$V1 <- as.Date(current_data$V1, "%d.%m.%y") #converting V1 to a date format
current_data$V2 <- as.numeric(rownames(current_data))%%3 #fill v2 with set data
current_data$V2 <- recode(current_data$V2, "1" = "3", "2" = "2", "0" = "1")

# now we are finaly construction a clear data set from all data we parsed
datum   <- as.Date(current_data$V1, "%d.%m.%y")
wochentag <- factor(weekdays(datum), levels = c("Monday", "Tuesday", "Wednesday", "Thursday", "Friday", "Saturday", "Sunday"))
anzahl  <- as.numeric(as.character(current_data$V3))
satz    <- as.ordered(current_data$V2)

# this is the data set we can work with now
push_ups <- data.frame(wochentag, datum, satz, anzahl)

# remove all the data we don't need anymore
rm(current_data, file_list, get_current_data, rowfilter, rowfilter_end, rowfilter_start, datum, anzahl, satz)


'
   This is where we can finally start to work with our cleaned up data set
'


ylim_max = mean(push_ups$anzahl) + (mean(push_ups$anzahl)/100*20)

# Barplot nach wochentagen
p <- ggplot(push_ups[push_ups$wochentag!="Sunday",], aes(x=wochentag, y=anzahl, group=wochentag)) + 
  theme_modern_rc() + #theme that looks nice
  theme(panel.background = element_rect(fill="#141226"),
        plot.background = element_rect(fill="#201e31")
  ) +
  geom_bar(width = 1, alpha = 0.1, fill="#3ab2b2", stat = "summary", fun.y = "mean", aes())  +
  scale_y_continuous(breaks = seq(0,30,by=1.5)) + #show a mark every 1.5 steps
  scale_x_discrete(expand = c(0,0.5)) + #no extra space around the plot
  ggtitle("Average push ups per set by weekday") +
  xlab("") +
  ylab("")
print(p)

#ylim_max = max(push_ups$anzahl)*3 - max(push_ups$anzahl)*3/100*20
## area plot nach sets
#p <- ggplot(push_ups, aes(datum, anzahl, group=satz, fill=satz)) + 
#  theme_modern_rc() + #theme that looks nice
#  theme(panel.background = element_rect(fill="#141226"),
#        plot.background = element_rect(fill="#201e31")
#        ) +
#  ggtitle("Push Ups in 3 sets") + 
#  geom_area(color = alpha("#303030", 0.6),  size=1, alpha=0.6, aes()) +
#  coord_cartesian(ylim = c(4,ylim_max)) + #make the graph start withot space on the bottom
#  scale_y_continuous(breaks = seq(0,ylim_max,by=5)) #show a mark every 10 steps
#print(p)


p <- ggplot(push_ups, aes(datum, anzahl, group=satz)) +
  theme_modern_rc() + #theme that looks nice
  theme(panel.background = element_rect(fill="#141226"),
        plot.background = element_rect(fill="#201e31")
  ) +
  geom_bar(width = 1, alpha = 0.1, fill="#3ab2b2", stat = "summary", fun.y = "mean", aes())  +
  geom_area(color = alpha("#303030", 0.0),  size=0.05, alpha=0.05, aes()) +
  ggtitle("Number of ups by day") +
  xlab("") +
  ylab("Push ups") 
#coord_flip()
print(p)


