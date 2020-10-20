##############
#Data visualisation in R, Lecture 4, (c) Jussi Palomäki
##############
### Wide vs. Long data

#Load libraries
library(tidyverse)
library(hrbrthemes)
library(effects)
library(lme4)

#Create dataframe
set.seed(3)
lecture4_data <- data.frame(ID=as.factor(1:10))
lecture4_data_long <- lecture4_data %>% mutate(session1 = runif(nrow(lecture4_data), 0, 1), 
                                               session2 = runif(nrow(lecture4_data), 0, 1.5),
                                               session3 = runif(nrow(lecture4_data), 0, 2),
                                               session4 = runif(nrow(lecture4_data), 0, 2.5),
                                               session5 = runif(nrow(lecture4_data), 0, 3),
                                               session6 = runif(nrow(lecture4_data), 0, 3.5),
                                               session7 = runif(nrow(lecture4_data), 0, 4),
                                               session8 = runif(nrow(lecture4_data), 0, 4.5),
                                               session9 = runif(nrow(lecture4_data), 0, 5),
                                               session10 = runif(nrow(lecture4_data), 0, 5.5)) %>%
  gather(key=session, value=value, session1:session10) #Gather into long format

#Spread() to switch back to wide format
lecture4_data_long %>% spread(session, value)

#Add the value 2 to variable "value" for ID 5 to change intercept for participant 5
lecture4_data_long[which(lecture4_data_long$ID==5),]$value <- lecture4_data_long[which(lecture4_data_long$ID==5),]$value+2 

#Create one outlier
lecture4_data_long[28,3] <- 4.5

#Organize the levels of "session" properly from session1 to session10
lecture4_data_long$session <- as.numeric(factor(lecture4_data_long$session, levels=c("session1", "session2", "session3", "session4", "session5",
                                                                                     "session6", "session7", "session8", "session9", "session10")))
#lecture4_data_long$session <- as.factor(lecture4_data_long$session) #optional

#Initial plot:
my_plot <- ggplot(data=lecture4_data_long, mapping = aes(x=session, y=value)) + geom_point() + geom_smooth(method="lm") + 
  theme_bw() + facet_wrap("ID") + scale_x_continuous(breaks=c(1:10))

my_plot

#Fit both lmer (linear mixed model fitting using the "lme4" library) and lm model to show different fits in the same plot
lmer_lecture4_long <- lmer(value~session + (session|ID), data=lecture4_data_long)
lm_lecture4_long <- lm(value~session, data=lecture4_data_long)

my_plot <- ggplot(lecture4_data_long, aes(session, value)) + geom_point() + 
  geom_smooth(method="lm", se=FALSE, aes(color="Participant-wise linear models")) + 
  geom_line(aes(y=predict(lmer_lecture4_long), color="Linear mixed model fit")) + 
  geom_line(aes(y=predict(lm_lecture4_long), color="Grand linear model fit")) +
  scale_x_continuous(breaks=c(1:10)) +
  ylab("Dependent Variable") + xlab("Session") + labs(color=NULL) + 
  theme_bw() +
  theme(legend.position = c(x=.75, y=.15),
        legend.text = element_text(size=11),
        legend.background = element_rect(fill="white"),
        axis.text.x = element_text(size=8),
        axis.title.x = element_text(size=14),
        axis.title.y = element_text(size=14),
        title = element_text(size=14),
        panel.border = element_blank(),
        panel.grid.minor = element_blank(), 
        axis.line = element_line(colour = "black")) +
  facet_wrap("ID") + labs(title="My first participant-wise plot!")

my_plot

#Create annotation layers for facet-specific annotations
ann_text1 <- data.frame(ID = factor(5, levels=c(1:10)), session=5, value=0.5)
ann_text2 <- data.frame(ID = factor(8, levels=c(1:10)), session=3, value=6)
ann_text3 <- data.frame(ID = factor(4, levels=c(1:10)), session=7.5, value=6)

#A "neat" way to make annotation text angle match the slope of a model fit
#Angle radians is the inverse tangent of slope, then transform radians to degrees
wanted_angle <- atan(coef(lm_lecture4_long)[2]) * 180/pi

#Start annotating
my_plot + geom_text(data = ann_text1, label = "POOR FIT!", size=2.8, angle=wanted_angle, vjust=-0.6,
                    colour="red")

my_plot + geom_text(data = ann_text1, label = "POOR FIT!", size=2.8, angle=wanted_angle, vjust=-0.6,
                    colour="red") +
  geom_rect(data=ann_text2, aes(xmin=2, xmax=4, ymin=3.9, ymax=5.3), alpha=.35, fill="red") +
  geom_text(data = ann_text2, label = "OUTLIER!", size=2.8, colour="red")


my_plot + geom_text(data = ann_text1, label = "POOR FIT!", size=2.8, angle=wanted_angle, vjust=-0.6,
                    colour="red") +
  geom_rect(data=ann_text2, aes(xmin=2, xmax=4, ymin=3.9, ymax=5.3), alpha=.35, fill="red") +
  geom_text(data = ann_text2, label = "OUTLIER!", size=2.8, colour="red") +
  geom_rect(data=ann_text3, aes(xmin=5.5, xmax=10.2, ymin=-0.2, ymax=5), alpha=.35, fill="red") +
  geom_text(data=ann_text3, label="WEIRD!", size=2.8, colour="red")