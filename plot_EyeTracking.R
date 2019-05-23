#install.packages("lme4")
#install.packages("ggplot2")
#install.packages("dplyr")
#install.packages("plyr")
#install.packages("stringr")
#install.packages("pbapply")
#install.packages("effsize")
#install.packages("lmerTest")
#install.packages("eyetrackingR")
#devtools::install_github("jwdink/eyetrackingR") ##newest version of eyetrackingR

require(lme4)
library(ggplot2)
library(dplyr)
library(plyr)
library(stringr)
library (pbapply)
library(effsize)
library(lmerTest)
library(eyetrackingR)

###IMPORTING THE DATA SETS###
dataset1<-read.csv("ASHUR_dataset1.csv", header = T)
dataset2<-read.csv("ASHUR_dataset2.csv", header = T)

###making eye-tracking data seperatly for each sampling rate###
dataset.1 <- make_eyetrackingr_data(subset(dataset1,Time1>=0 & Time1<=10000), 
                                   participant_column = "Subject",
                                   trial_column = "Trial",
                                   item_columns = "Item",
                                   time_column = "Time1",
                                   trackloss_column = "Away",
                                   aoi_columns = c('Target', 'Distractor'),
                                   treat_non_aoi_looks_as_missing = F
)

dataset.2 <- make_eyetrackingr_data(subset(dataset2,Time1>=0 & Time1<=10000),
                                   participant_column = "Subject",
                                   trial_column = "Trial",
                                   item_columns = "Item",
                                   time_column = "Time1",
                                   trackloss_column = "Away",
                                   aoi_columns = c('Target', 'Distractor'),
                                   treat_non_aoi_looks_as_missing = F
)


## down-sampling the data - again seperatly for the sampling rates
data1<-make_time_sequence_data(data = subset(dataset.1), 20, aois = "Target",
                               predictor_column ="Condition")


data2<-make_time_sequence_data(data = subset(dataset.2), 20, aois = "Target",
                               predictor_column ="Condition")

##now we re-merge the 2ms and 4ms samples, since both are avaraged across 20ms
data.new<-rbind(data2, data1)


## average the data per item for a t.test
subjs1 <- ddply(subset(data.new), .(Subject, Condition), function(x) mean(x$Prop, na.rm = T))
xtabs(~subjs1$Condition)ddply(subjs1, .(Condition), function(x) mean(x$V1, na.rm = T))

t.test (subjs1$V1~subjs1$Condition, paired= F)


##boxplot
boxplot_conditions <- ggplot(subset(subjs1),aes(x= Condition, y=V1))+
           geom_boxplot(aes(fill=Condition))+
           theme(text = element_text(size = 20)) +
           geom_dotplot(binwidth = 0.005, binaxis='y', stackdir='center', size = 2, fill = "grey") +
           stat_summary(fun.y=mean, geom = "text", label="----", size= 10, color= "white")+
           scale_x_discrete(name = "Condition")+
           scale_y_continuous(name = "Proportion of looks", breaks = seq(0.2, 0.9, 0.1))

boxplot_conditions 

##Delay between execution
Sys.sleep(5) 

##Proportion of looks plot
plot_eyetracking <- plot(subset(data.new, Time>0 & Time<=10000), predictor_column = "Condition", dv = "Prop", model = NULL)+
                    scale_colour_manual(values=c( "red", "blue"))+
                    theme(text = element_text(size = 20)) +
                    coord_cartesian(xlim = c(350, 9500)) +
                    scale_x_continuous(name = "Time (in ms)", breaks = seq(0, 10000, 500))+
                    scale_y_continuous(name = "Proportion of looks towards action video", breaks = seq(0, 1, 0.25), limits = (0:1))
plot_eyetracking
