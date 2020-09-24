# This is the R-Script for the Kaggle Challenge : "FiveThirtyEight Candy Power Ranking Dataset"
# The task is to use a dataset of candies to create a new, ultimative candy that will taste the best. 

# 1. Step: Downloading the dataset from github.
library(readr)
file_path = file.path("/Users/Nicolas/candy-data.csv")
dataset <- read_csv(file_path)
dataset

# Looking at the dataset to get an overview of the structure.
# First check if there is any missing data.
any(is.na(dataset))
# The result is false, so there is no data missing. 

library(tibble)
head(dataset)
glimpse(dataset)
dataset$winpercent <- dataset$winpercent/100

# Besides a general outlook we should also look at the candies we have to deal with. 
ingredients <- dataset[0:10]
datasum <- rowSums(ingredients[0:-1])
datasum
min(datasum)
# With this overview we can see that there are two candies "One dime" and "One quarter" that have absolutly no ingredients.
# It looks like these are the no candies. They could on the other hand be money. 
# Therefore these should be removed. 
data <- dataset[-c(3,4),]
data

# Luckily there are no row values of 7 e.g candies that have all ingredients in them. This would probably also be useless and could be reomved.


# Sorting the dataset by winpercent to see the which candies performed  "best".
dataset_sorted <- data[order(data$winpercent, decreasing = TRUE),]
view(dataset_sorted)

# By looking at the sorted table, it is visible that the ten candies with the
# highest winning-percentage all contain chocolate and have a winning-percentage of over 70%.

# Performing correlation analysis to see if there is a possible connection between different tastes.
variables <- subset(dataset, select = -c(competitorname, winpercent, sugarpercent,pricepercent ))
correlations <- round(cor(variables), 2)

library(ggplot2)
library(ggcorrplot)
ggcorrplot(correlations, hc.order = TRUE, type = "lower", lab = TRUE)

# Above .5 correlations suggest that there might be a connection between different tastes. 
# This has to be keept in mind thoroughout the following processes.

# To determine the influence of each ingredient variable on the winning-percentage
# only tastes will be considered and other factors such as suggar- or price-perccentiles will be excluded. 
# Since the method to collect the data was a  taste-based voting system these information do not add value to the decision.

# Creating a GLM where the variables chocolate, fruity, caramel, peanutyalmondy, nougat, crispedricewafer, hard, bar and 
# pluribus are used to explain the variation of winpercent.

library(wooldridge)
mod <- (glm(winpercent ~ chocolate + fruity + caramel + peanutyalmondy + nougat + crispedricewafer + hard + bar + pluribus, data = dataset))

# Look at the results of the Modell "mod".
summary(mod)
sort(round(mod$coefficients, 3), decreasing = TRUE)

# Visualization of coefficients with respective conf. Intervall
library(dotwhisker)
dwplot(list(mod),  vline = geom_vline(xintercept = 0, colour = "#004DAD", linetype = 2)) +
  theme_bw() + xlab("Coefficient Estimate") + ylab("") +
  ggtitle("Estimated coefficients for the winning-percentage-change") +
  theme(plot.title = element_text(face="bold"),
  legend.position = c(0.007, 0.01),
  legend.justification = c(0, 0), 
  legend.background = element_rect(colour="grey80"),
  legend.title = element_blank())


# Of the different variables only four seem to have a significant impact. The most important taste aspects are
# chocolate, fruity, peanutyalmondy and crispedricewafer.
# For example  if a candy contains chocolate in it, its winning-percentage increases by 20%. 
# If a candy has a fruity taste, the winning-percentage increases by 10%. 
# And if a candy contains nuts, its winning-percentage increases also by 10%.

library(broom)

# Look at the R^2 to see how much of the winning-percentage can be explained by the variables selected variables.
library(rsq)
rsq(mod, adj = FALSE)


# An R^2 of 51% signifies that roughly half of the variance can be explained.
# This is not a good, but still an acceptable outcome, give the variablity in different tastes.

library(lattice)
library(caret)

# The varImp function helps to calculate the respective importance of the features of the modell "mod".
# This shows much the attributes contribute to the model perfromance.
Importance <- as.data.frame(varImp(mod))
Importance <- data.frame(Variables   = rownames(Importance), Overall = Importance$Overall)
Importance[order(Importance$Overall,decreasing = TRUE),]

ggplot(data = Importance, aes(x = reorder(Variables, Overall), y = Overall)) +
geom_pointrange(aes(ymin = 0, ymax = Overall), color = "#004DAD", size = .5) +
theme_linedraw() +
coord_flip() +
labs(x = "", y = "", title = " Variable Importance of Ingredients")

# There it is visible that chocolate is the most important variable in the model.
# Therefore the inclusion of chocolate, can help to determine whether the winning-percentage will be higher or lower. 
# Also peanutyalmondy is a very important variable of the model. 


# Since it was beforehand discovered that there may be correlations between different variables, 
# one has to be cautious about potential interaction effects of the performance of the respective candies.
mod2 <- aov(winpercent ~ chocolate*fruity*caramel*peanutyalmondy*nougat*crispedricewafer*hard*bar*pluribus, data=dataset)
summary(mod2)

# Because of the ANOVA it is visible that caramel and nuts are often used togheter in one type of candy,
# as well as nuts and bars. 
# Therefore when creating a certain candy type it has to be kept in mind, that the respective performance of 
# nuts, caramel or bar underlies certain interaction effects. It has to be assumed that the positive effects of the 
# respective charactersitics only exist because of the interaction. 
# Since this realtionship was discovered, a single interpretation of the effect is difficult. 

# The results of the glm and the observation of interactions leads to the following recommendation: 

# The strongest coefficients in the model are chocolate, fruity, peanutyalmondy, and crispedricewafer.
# Since there is only one candy "Tootsie Pop" that combines chocolate and fruity taste, and the winpercentage of this sweet is
# below 50%, it can only hardly be assumed that the combination of chocolate and fruity will create a promissing candy
# Besides chocolate, also peanutyalmondy and crispedricewafer have a positive effect on the winning-percentage of a candy. 
# Since it was discovered that there are also interaction-effects between peanutyalmondy and caramel, as well as peanutalmondy and bar,
# these characteristics should also be considered. 


# Therefore the recommended candy should contain: 
# chocolate, caramle, peanuts or almonds, should be crispy and bar-shaped!

# One could now use a different modelling approach e.g. a random forest. But the results do not change much, so we can achieve the same results with less work.


# Lastly we transform the data to be able to display win-lose ratios of the respective candy characteristics
library(tidyr)
library(dplyr)
by_choc <- dataset %>%
  group_by(chocolate) %>% 
  summarise(winpercent = mean(winpercent))
t_by_choc <- t(by_choc)
colnames(t_by_choc) <- c("not_contain", "contain")
t_by_choc <- t_by_choc[-1,]
ratio_choc <- t_by_choc
ratio_choc[1] <- t_by_choc[1]/(sum(t_by_choc[1], t_by_choc[2]))
ratio_choc[2] <- t_by_choc[2]/(sum(t_by_choc[1], t_by_choc[2]))
ratio_choc


by_fruit <- dataset %>%
  group_by(fruity) %>%
  summarise(winpercent = mean(winpercent))
t_by_fruit <- t(by_fruit)
colnames(t_by_fruit) <- c("not_contain", "contain")
t_by_fruit <- t_by_fruit[-1,]
ratio_fruit <- t_by_fruit
ratio_fruit[1] <- t_by_fruit[1]/(sum(t_by_fruit[1], t_by_fruit[2]))
ratio_fruit[2] <- t_by_fruit[2]/(sum(t_by_fruit[1], t_by_fruit[2]))
ratio_fruit


by_caramel <- dataset %>%
  group_by(caramel) %>%
  summarise(winpercent = mean(winpercent))
t_by_caramel <- t(by_caramel)
colnames(t_by_caramel) <- c("not_contain", "contain")
t_by_caramel <- t_by_caramel[-1,]
ratio_caramel <- t_by_caramel
ratio_caramel[1] <- t_by_caramel[1]/(sum(t_by_caramel[1], t_by_caramel[2]))
ratio_caramel[2] <- t_by_caramel[2]/(sum(t_by_caramel[1], t_by_caramel[2]))
ratio_caramel

by_nut <- dataset %>%
  group_by(peanutyalmondy) %>%
  summarise(winpercent = mean(winpercent))
t_by_nut <- t(by_nut)
colnames(t_by_nut) <- c("not_contain", "contain")
t_by_nut<- t_by_nut[-1,]
ratio_nut <- t_by_nut
ratio_nut[1] <- t_by_nut[1]/(sum(t_by_nut[1], t_by_nut[2]))
ratio_nut[2] <- t_by_nut[2]/(sum(t_by_nut[1], t_by_nut[2]))
ratio_nut

by_nougat <- dataset %>%
  group_by(nougat) %>%
  summarise(winpercent = mean(winpercent))
t_by_nougat<- t(by_nougat)
colnames(t_by_nougat) <- c("not_contain", "contain")
t_by_nougat<- t_by_nougat[-1,]
ratio_nougat <- t_by_nougat
ratio_nougat[1] <- t_by_nougat[1]/(sum(t_by_nougat[1], t_by_nougat[2]))
ratio_nougat[2] <- t_by_nougat[2]/(sum(t_by_nougat[1], t_by_nougat[2]))
ratio_nougat

by_crisped <- dataset %>%
  group_by(crispedricewafer) %>%
  summarise(winpercent = mean(winpercent))
t_by_crisped <- t(by_crisped)
colnames(t_by_crisped) <- c("not_contain", "contain")
t_by_crisped <- t_by_crisped[-1,]
ratio_crisped <- t_by_crisped
ratio_crisped[1] <- t_by_crisped[1]/(sum(t_by_crisped[1], t_by_crisped[2]))
ratio_crisped[2] <- t_by_crisped[2]/(sum(t_by_crisped[1], t_by_crisped[2]))
ratio_crisped

by_hard <- dataset %>%
  group_by(hard) %>%
  summarise(winpercent = mean(winpercent))
t_by_hard<- t(by_hard)
colnames(t_by_hard) <- c("not_contain", "contain")
t_by_hard <- t_by_hard[-1,]
ratio_hard <- t_by_hard
ratio_hard[1] <- t_by_hard[1]/(sum(t_by_hard[1], t_by_hard[2]))
ratio_hard[2] <- t_by_hard[2]/(sum(t_by_hard[1], t_by_hard[2]))
ratio_hard

by_bar <- dataset %>%
  group_by(bar) %>%
  summarise(winpercent = mean(winpercent))
t_by_bar <- t(by_bar)
colnames(t_by_bar) <- c("not_contain", "contain")
t_by_bar <- t_by_bar[-1,]
ratio_bar <- t_by_bar
ratio_bar[1] <- t_by_bar[1]/(sum(t_by_bar[1], t_by_bar[2]))
ratio_bar[2] <- t_by_bar[2]/(sum(t_by_bar[1], t_by_bar[2]))
ratio_bar

by_pluribus <- dataset %>%
  group_by(pluribus) %>%
  summarise(winpercent = mean(winpercent))
t_by_pluribus <- t(by_pluribus)
colnames(t_by_pluribus) <- c("not_contain", "contain")
t_by_pluribus <- t_by_pluribus[-1,]
ratio_pluribus <- t_by_pluribus
ratio_pluribus[1] <- t_by_pluribus[1]/(sum(t_by_pluribus[1], t_by_pluribus[2]))
ratio_pluribus[2] <- t_by_pluribus[2]/(sum(t_by_pluribus[1], t_by_pluribus[2]))
ratio_pluribus

# Creating a plot of Win-Lose-Ratio of the respective characteristic
ratio <- t(round(rbind(ratio_choc, ratio_fruit, ratio_caramel, ratio_nut, ratio_nougat, ratio_crisped, ratio_hard, ratio_bar, ratio_pluribus),2))
ratioplot <- barplot(ratio, main="Win-Lose-Ratio of the respective characteristic", xlab="Candy characteristic", col=c("gray","#004DAD"), ylab="Win-Lose-Ratio", legend.text = rownames(ratio), args.legend = list(cex = 0.75, x = "topright"))

# Since this plot also shows, that candies with chocolate, nuts and crisped have the best overall Win-Lose-Ratios, we can confirm that this would be the most delicous candy!
