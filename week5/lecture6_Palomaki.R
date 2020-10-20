############
### LECTURE 5:
### Plotting from models

#Load libraries
library(tidyverse)
library(hrbrthemes)
library(effects)
library(here)
library(emmeans)
library(ggpubr)

#Simulate data
set.seed(2)
lecture5_data <- data.frame(group=c(rep("Group 1", 20), rep("Group 2", 20), rep("Group 3", 20)),
                            y=c(rnorm(20,2,1), rnorm(20, 5, 1), rnorm(20, 8, 1)),
                            gender=c("Female", "Male"),
                            x=c(rnorm(20,2,1), rnorm(20, 5, 1), rnorm(20, 8, 1))) # add x-variable to demonstrate controlling for covariates

#Summarize data
lecture5_data_summary <- lecture5_data %>% group_by(group, gender) %>% 
  dplyr::summarize(mean_y = mean(y), 
                   se_y = sd(y)/sqrt(20),
                   lower = mean_y - 1.96*se_y, #2.5th percentile
                   upper = mean_y + 1.96*se_y) #97.5th percentile note that 1.96 is an approximation from normal distribution

#Obtain estimated marginal means (effect of group*gender)
y_effects1 <- effect("group:gender", lm(y~group*gender, data=lecture5_data))
y_effects1 <- as.data.frame(y_effects1)
y_effects1

#Obtain estimated marginal means (effect of group*gender adjusted for x)
y_effects2 <- effect("group:gender", lm(y~group*gender+x, data=lecture5_data))
y_effects2 <- as.data.frame(y_effects2)
y_effects2

#The same as above but with emmeans()
y_emmeans <- emmeans(lm(y~group*gender+x, data=lecture5_data), specs=c("group", "gender"))
y_emmeans <- as.data.frame(y_emmeans)
y_emmeans

#Plot figures
figure1 <- ggplot(lecture5_data_summary, aes(group, mean_y, fill=gender)) + geom_bar(stat="identity", position=position_dodge(.9)) +
  geom_errorbar(aes(ymin=lower, ymax=upper), position=position_dodge(.9), width=.2, alpha=.5) +
  scale_fill_manual(name=NULL, values=c("salmon", "lightblue")) +
  xlab(NULL) + ylab("This is the DV") + labs(title="Raw data") + theme_bw(base_size=13) +
  geom_text(aes(label=round(mean_y, 2)), position=position_dodge(width=0.9), vjust=-0.25)

figure2 <- ggplot(y_effects1, aes(group, fit, fill=gender)) + geom_bar(stat="identity", position=position_dodge(.9)) +
  geom_errorbar(aes(ymin=lower, ymax=upper), position=position_dodge(.9), width=.2, alpha=.5) +
  scale_fill_manual(name=NULL, values=c("salmon", "lightblue")) +
  xlab(NULL) + ylab(NULL) + labs(title="EMMs, no covariate") + theme_bw(base_size=13) +
  geom_text(aes(label=round(fit, 2)), position=position_dodge(width=0.9), vjust=-0.25)

figure3 <- ggplot(y_effects2, aes(group, fit, fill=gender)) + geom_bar(stat="identity", position=position_dodge(.9)) +
  geom_errorbar(aes(ymin=lower, ymax=upper), position=position_dodge(.9), width=.2, alpha=.5) +
  scale_fill_manual(name=NULL, values=c("salmon", "lightblue")) +
  xlab(NULL) + ylab(NULL) + labs(title="EMMs + covariate") + theme_bw(base_size=13) +
  geom_text(aes(label=round(fit, 2)), position=position_dodge(width=0.9), vjust=-0.25)


#Use ggarrange() to combine multiple ggplot-objects
both_figures <- ggarrange(figure1, figure2, figure3, ncol=3, nrow=1, common.legend=TRUE, legend="bottom")

#Another way to have one common DV name is to remove the y-axis label from all figures, and use annotate_figure():
#both_figures <- annotate_figure(both_figures,
#                                left = text_grob("This is the DV", color = "black", size=14, rot = 90),
#                                fig.lab = NULL, fig.lab.face = NULL)
both_figures

########
#Plotting logistic regressions
########

#Simulate data
set.seed(1)
lecture5_data <- data.frame(ID=as.factor(1:100))
lecture5_data <- lecture5_data %>% dplyr::mutate(dich_dv = c(rep(0, 50), rep(1, 50)),
                                                 trait1 = c(abs(runif(50, 0, 4)+rnorm(50,0,1)), abs(runif(50,3,7)+rnorm(50,0,1))),
                                                 trait2 = abs(trait1*0.5 + rnorm(100, 0, 1)))

#Let's plot it incorrectly! What's wrong with these plots?
ggplot(lecture5_data, aes(trait1, dich_dv)) + geom_point() + geom_smooth(method="lm")
ggplot(lecture5_data, aes(trait2, dich_dv)) + geom_point() + geom_smooth(method="lm") 

#The problem is that the DV values range from 0 to 1, thus skewing any predictions of a _linear model_ below 0 and above 1.
#Solution: force the y-axis to range from -inf to +inf by taking the odds of our DV-values and log-transforming it, i.e. creating log odds (or logits):

#To demonstrate:
log(lecture5_data$dich_dv / (1-lecture5_data$dich_dv))

#Of course, it doesn't really help (yet) to have your DV values at positive or negative infinity.
#Note, also that we can't use the Ordinary Least Squares method to fit a model in this case.
#Thus, the observations (which are now at negative and positive infinity) are "projected" onto a line (linear model) in the log-odds-transformed coordinate system.
#Then, those projected log odds / logit values are summed across observations (or first transformed back into probabilities, in which case they will be multiplied).
#Now we conclude: GIVEN this linear model, the probability of having observed the combination of "0"s and "1"s that we did observe is X %.
#And finally, we do the same again, and again, and again, with different lines.
#The goal is to find out which line MAXIMIZES THE LIKELIHOOD of observing that which we did observe.
#A straight line in a logit coordinate system becomes a squiggly line in the original dichotomous coordinate system.
#See https://www.youtube.com/watch?v=BfKanl1aSG0 for an excellent brief explanation of Maximum Likelihood

#We can use ggplot to do all of the above for us, with just a line of code.
#method = "glm" refers to "generalized linear model", and the "method.args" specifies that we fant a model of the binomial family (dichotomous DV)
ggplot(lecture5_data, aes(trait1, dich_dv)) + geom_point() +
  geom_smooth(method = "glm", method.args = list(family = "binomial")) + theme_bw() +
  xlab("Extroversion") + ylab("P(some event occurring)")

#What if we want more customization? Let's try plotting multiple logistic regression analyses.
#That is, let's plot the effects of two IVs while adjusting for their respective effects.

#In our simulated data, trait1 and trait2 are correlated:
ggplot(lecture5_data, aes(trait1, trait2)) + geom_point() + geom_smooth(method="lm")

#First, we fit a multiple logistic regression model using glm().
#Notice that while both trait1 and trait2 are significant if used alone as predictors, trait2 is no longer significant when controlling for trait1
lecture5_model <- glm(dich_dv ~ trait1+trait2, data=lecture5_data, family="binomial")
summary(lecture5_model)

#Create data frame _with which to predict_ values from our model:
#We create a data frame with an equal number of observations than in the original data
#Values for trait1 range from 0 to 7 to reflect the actual range of the values
#Values for trait2 are manually being held at their mean
#In other words: We want to predict the DV values for observations where trait1 ranges from 0 to 7 while holding trait2 constant
lecture5_pred_frame.1 <- with(lecture5_data, data.frame(trait1 = seq(from=0, to=7, by=0.0705),
                                                        trait2 = rep(mean(lecture5_data$trait2), 100))) 

#Next, we use predict() to predict values based on our logistic regression model, using the new data frame
#type = "link" transforms predictions into logit values, and se.fit=TRUE adds the standard errors of these predicted logit values
#NOTE: The SE and CI have to be calculated on the logit values, NOT the probabilities!
lecture5_preds.1 <- predict(lecture5_model, newdata=lecture5_pred_frame.1, type = "link", se.fit=TRUE)

#Calculate confidence interval manually
critval <- 1.96 ## approx 95% CI
upr <- lecture5_preds.1$fit + (critval * lecture5_preds.1$se.fit) #upper value (97.5%)
lwr <- lecture5_preds.1$fit - (critval * lecture5_preds.1$se.fit) #lower value (2.5%)
fit <- lecture5_preds.1$fit #model fit (predicted value)

#Transform log-odds into probabilities for plotting. 
#The family() function shows the "tools" available inside model-objects, such as glm.
#One of these tools is the linkinv() function, that can be handily used to transform logits into probabilities.
fit_prob <- lecture5_model$family$linkinv(fit)
upr_prob <- lecture5_model$family$linkinv(upr)
lwr_prob <- lecture5_model$family$linkinv(lwr)

#Add everything (trait1-values with which predictions were made, predicted probability, and the upper/lower bounds of the prediction) into a single dataframe
plot_frame.1 <- as.data.frame(cbind(lecture5_pred_frame.1$trait1, fit_prob, upr_prob, lwr_prob))

#######Take a breath.
#Next, we do the exact same thing as above, but this time hold trait1 constant while predicting for scores of trait2
#Create prediction data frame
lecture5_pred_frame.2 <- with(lecture5_data, data.frame(trait2 = seq(from=0, to=7, by=0.0705),
                                                        trait1 = rep(mean(lecture5_data$trait1), 100)))

#Predict
lecture5_preds.2 <- predict(lecture5_model, newdata=lecture5_pred_frame.2, type = "link", se.fit=TRUE)

#Calculate confidence interval
critval <- 1.96 ## approx 95% CI
upr.2 <- lecture5_preds.2$fit + (critval * lecture5_preds.2$se.fit)
lwr.2 <- lecture5_preds.2$fit - (critval * lecture5_preds.2$se.fit)
fit.2 <- lecture5_preds.2$fit

#Transform into probabilities
fit_prob.2 <- lecture5_model$family$linkinv(fit.2)
upr_prob.2 <- lecture5_model$family$linkinv(upr.2)
lwr_prob.2 <- lecture5_model$family$linkinv(lwr.2)

#Add to single dataframe
plot_frame.2 <- as.data.frame(cbind(lecture5_pred_frame.2$trait2, fit_prob.2, upr_prob.2, lwr_prob.2))

#######Take another breath.
#Wrangle to enable plotting
names(plot_frame.1) <- c("x", "fit", "upr", "lwr") #rename columns for trait1 predictions
names(plot_frame.2) <- c("x2", "fit2", "upr2", "lwr2") #rename columns for trait2 predictions

#Combine prediction data frames and turn into long format
plot_frame <- as_tibble(cbind(plot_frame.1, plot_frame.2)) %>% gather(trait, probability, fit, fit2)

#For further plotting purposes, gather the original DV, trait1 and trait2 values
lecture5_datalong <- lecture5_data %>% gather(trait, original_value, trait1, trait2)

#Finalize by binding everything into a tibble
plot_frame <- as_tibble(cbind(plot_frame, lecture5_datalong[,c(2,4)]))

#Plot, option 1
ggplot(plot_frame, aes(x, probability, color=trait)) + geom_line(lwd=0.8) +
  geom_ribbon(data=plot_frame,aes(ymin=lwr,ymax=upr),alpha=0.1, fill="salmon", colour=NA) + #we use geom_ribbon() to visualize 95% confidence band
  geom_ribbon(data=plot_frame,aes(ymin=lwr2,ymax=upr2),alpha=0.1, fill="skyblue", colour=NA) + 
  theme_bw() +
  xlab("Trait score") + ylab("P(some event occurring)") + labs(color="Personality trait") + scale_color_discrete(labels = c("Extroversion","Conscientousness")) +
  #scale_y_continuous(labels = scales::percent) + 
  scale_x_continuous(breaks = c(0,1,2,3,4,5,6,7)) +
  theme(legend.position = c(x=.70, y=.20),
        legend.text = element_text(size=11),
        legend.background = element_rect(fill="transparent"),
        legend.key = element_rect(fill = NA),
        axis.text.x = element_text(size=10),
        axis.text.y = element_text(size=10),
        axis.title.x = element_text(size=14),
        axis.title.y = element_text(size=14),
        title = element_text(size=14),
        panel.border = element_blank(),
        panel.grid.minor = element_blank(), 
        axis.line = element_line(colour = "black")) +
  labs(title="My first adjusted regression plot!")


#Option 2 (black and white with datapoints)
ggplot(plot_frame, aes(x, probability, linetype=trait)) + geom_line(lwd=0.7) +
  geom_point(aes(original_value, dich_dv, shape=trait), position=position_dodge(.05), alpha=.5) + #this makes the plot messy and doesn't make much sense!
  geom_ribbon(data=plot_frame,aes(ymin=lwr,ymax=upr),alpha=0.03, colour=NA) +
  geom_ribbon(data=plot_frame,aes(ymin=lwr2,ymax=upr2),alpha=0.03, colour=NA) + theme_bw() +
  xlab("Trait score") + ylab("P(some event occurring)") + 
  labs(linetype="Personality trait") + 
  labs(shape="Personality trait") +
  scale_linetype_discrete(labels = c("Extroversion","Conscientiousness")) +
  scale_shape_discrete(labels = c("Extroversion", "Conscientiousness")) + #need to be named the same so they are combined in the plot!
  #scale_y_continuous(labels = scales::percent) + 
  scale_x_continuous(breaks = c(0,1,2,3,4,5,6,7)) +
  theme(legend.position = c(x=.70, y=.20),
        legend.text = element_text(size=11),
        legend.background = element_rect(fill="transparent"),
        legend.key = element_rect(fill=NA),
        axis.text.x = element_text(size=10),
        axis.text.y = element_text(size=10),
        axis.title.x = element_text(size=14),
        axis.title.y = element_text(size=14),
        title = element_text(size=14),
        panel.border = element_blank(),
        panel.grid.minor = element_blank(), 
        axis.line = element_line(colour = "black")) +
  guides(linetype=guide_legend(override.aes=list(fill=NA))) + #linetype adds shade around legend keys, color does not apparently(?)
  labs(title="My first adjusted regression plot!")