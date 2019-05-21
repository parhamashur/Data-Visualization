library(dplyr)
library(tidyr)
library(ggplot2)

DAY <- read.table("~FRPARIS.txt", quote="\"", comment.char="")

# rename variables
names(DAY) <- c("Month", "Day", "Year", "Temp")

# create dataframe that represents 1995-2013 historical data
Past <- DAY %>%
  group_by(Year, Month) %>%
  arrange(Day) %>%
  ungroup() %>%
  group_by(Year) %>%
  mutate(newDay = seq(1, length(Day))) %>%   # label days as 1:365 (will represent x-axis)         
  ungroup() %>%
  filter(Temp != -99 & Year != 2017) %>%     # filter out missing data (identified with '-99' value) & current year data
  group_by(newDay) %>%
  mutate(upper = max(Temp), # identify max value for each day
         lower = min(Temp), # identify min value for each day
         avg = mean(Temp),  # calculate mean value for each day
         se = sd(Temp)/sqrt(length(Temp))) %>%  # calculate standard error of mean
  mutate(avg_upper = avg+(2.101*se),  # calculate 95% CI for mean
         avg_lower = avg-(2.101*se)) %>%  # calculate 95% CI for mean
  ungroup()

Present <- DAY %>%
  group_by(Year, Month) %>%
  arrange(Day) %>%
  ungroup() %>%
  group_by(Year) %>%
  mutate(newDay = seq(1, length(Day))) %>%  # create matching x-axis as historical data
  ungroup() %>%
  filter(Temp != -99 & Year == 2014)  # filter out missing data & select current year data

# create dataframe that represents the lowest temp for each day for the historical data
PastLows <- Past %>%
  group_by(newDay) %>%
  summarise(Pastlow = min(Temp)) # identify lowest temp for each day from 1995-2013

# create dataframe that identifies the days in 2014 in which the temps were lower than all previous 19 years
PresentLows <- Present %>%
  left_join(PastLows) %>%  # merge historical lows to current year low data
  mutate(record = ifelse(Temp<Pastlow, "Y", "N")) %>% # identifies if current year was record low
  filter(record == "Y")  # filter for days that represent current year record lows

# create dataframe that represents the highest temp for each day for the historical data
PastHighs <- Past %>%
  group_by(newDay) %>%
  summarise(Pasthigh = max(Temp))  # identify highest temp for each day from 1995-2013

# create dataframe that identifies the days in 2014 in which the temps were higher than all previous 19 years
PresentHighs <- Present %>%
  left_join(PastHighs) %>%  # merge historical highs to current year low data
  mutate(record = ifelse(Temp>Pasthigh, "Y", "N")) %>% # identifies if current year was record high
  filter(record == "Y")  # filter for days that represent current year record highs


# function to turn y-axis labels into degree formatted values
dgr_fmt <- function(x, ...) {
  parse(text = paste(x, "*degree", sep = ""))
}

# create y-axis variable
a <- dgr_fmt(seq(-20,100, by=10))

# create a small dataframe to represent legend symbol for 2014 Temperature
legend_data <- data.frame(x=seq(175,182),y=rnorm(8,15,2))

p <- ggplot(Past, aes(newDay, Temp)) +
  theme(plot.background = element_blank(),
        panel.grid.minor = element_blank(),
        panel.grid.major = element_blank(),
        panel.border = element_blank(),
        panel.background = element_blank(),
        axis.ticks = element_blank(),
        #axis.text = element_blank(),  
        axis.title = element_blank()) +
  geom_linerange(Past, mapping=aes(x=newDay, ymin=lower, ymax=upper), colour = "wheat2", alpha=.1)

print(p)

p <- p + 
  geom_linerange(Past, mapping=aes(x=newDay, ymin=avg_lower, ymax=avg_upper), colour = "wheat4")

print(p)

p <- p + 
  geom_line(Present, mapping=aes(x=newDay, y=Temp, group=1)) +
  geom_vline(xintercept = 0, colour = "wheat4", linetype=1, size=1)

print(p)

p <- p + 
  geom_hline(yintercept = -20, colour = "white", linetype=1) +
  geom_hline(yintercept = -10, colour = "white", linetype=1) +
  geom_hline(yintercept = 0, colour = "white", linetype=1) +
  geom_hline(yintercept = 10, colour = "white", linetype=1) +
  geom_hline(yintercept = 20, colour = "white", linetype=1) +
  geom_hline(yintercept = 30, colour = "white", linetype=1) +
  geom_hline(yintercept = 40, colour = "white", linetype=1) +
  geom_hline(yintercept = 50, colour = "white", linetype=1) +
  geom_hline(yintercept = 60, colour = "white", linetype=1) +
  geom_hline(yintercept = 70, colour = "white", linetype=1) +
  geom_hline(yintercept = 80, colour = "white", linetype=1) +
  geom_hline(yintercept = 90, colour = "white", linetype=1) +
  geom_hline(yintercept = 100, colour = "white", linetype=1)

print(p)

p <- p + 
  geom_vline(xintercept = 31, colour = "wheat4", linetype=3, size=.5) +
  geom_vline(xintercept = 59, colour = "wheat4", linetype=3, size=.5) +
  geom_vline(xintercept = 90, colour = "wheat4", linetype=3, size=.5) +
  geom_vline(xintercept = 120, colour = "wheat4", linetype=3, size=.5) +
  geom_vline(xintercept = 151, colour = "wheat4", linetype=3, size=.5) +
  geom_vline(xintercept = 181, colour = "wheat4", linetype=3, size=.5) +
  geom_vline(xintercept = 212, colour = "wheat4", linetype=3, size=.5) +
  geom_vline(xintercept = 243, colour = "wheat4", linetype=3, size=.5) +
  geom_vline(xintercept = 273, colour = "wheat4", linetype=3, size=.5) +
  geom_vline(xintercept = 304, colour = "wheat4", linetype=3, size=.5) +
  geom_vline(xintercept = 334, colour = "wheat4", linetype=3, size=.5) +
  geom_vline(xintercept = 365, colour = "wheat4", linetype=3, size=.5) 

print(p)

p <- p +
  coord_cartesian(ylim = c(-20,100)) +
  scale_y_continuous(breaks = seq(-20,100, by=10), labels = a) +
  scale_x_continuous(expand = c(0, 0), 
                     breaks = c(15,45,75,105,135,165,195,228,258,288,320,350),
                     labels = c("January", "February", "March", "April",
                                "May", "June", "July", "August", "September",
                                "October", "November", "December"))

print(p)

p <- p +
  geom_point(data=PresentLows, aes(x=newDay, y=Temp), colour="blue3") +
  geom_point(data=PresentHighs, aes(x=newDay, y=Temp), colour="firebrick3")

print(p)


p <- p +
  ggtitle("Paris's Weather in 2018") +
  theme(plot.title=element_text(face="bold",hjust=.012,vjust=.8,colour="#3C3C3C",size=20)) +
  annotate("text", x = 19, y = 98, label = "Temperature", size=4, fontface="bold")

print(p)

p <- p +
  annotate("text", x = 50, y = 93, 
           label = "Data represents average daily temperatures.", size=3, colour="gray30") +
  annotate("text", x = 51.5, y = 89, 
           label = " Accessible data dates back to January 1, 1995.", size=3, colour="gray30") 
print(p)
