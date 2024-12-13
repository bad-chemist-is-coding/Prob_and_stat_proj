library(readr)
library(tidyverse)
library(ggplot2)
library(plyr)
library(caret)
library(ROCR)
library(performance)
water <- read.csv("water_potability.csv") %>%
  mutate(Potability = recode(Potability, 
                             "0" = "Non-Potable",
                             "1" = "Potable"))
View(water)

head(water)
apply(is.na(water),2,sum)
water<-na.omit(water)
dim(water)
water<-as.data.frame(water)
str(water)
summary(water)
table(water$Potability)

### HISTOGRAM ###

# Assign values from the chart
pH <- water$ph
Hardness <- water$Hardness
Solids <- water$Solids
Chloramines <- water$Chloramines
Sulfate <- water$Sulfate
Conductivity <- water$Conductivity
Organic_Carbon <- water$Organic_carbon
Trihalomethanes <- water$Trihalomethanes
Turbidity <- water$Turbidity
Potability <- as.factor(water$Potability)

## Plot histograms ##

# Plot histogram for pH
hist(
  pH, main = "Frequency of pH histogram graph",
  xlab = "pH", ylab = "Frequency",
  ylim = c(0,600), col = "coral",
  labels = TRUE
)
# Plot histogram for Hardness
hist(
  Hardness, main = "Frequency of Hardness in Water samples histogram graph ",
  xlab = "Hardness", ylab = "Frequency",
  ylim = c(0,600), col = "azure",
  xlim = c(50, 350),
  labels = TRUE
)
# Plot histogram for Solids
hist(
  Solids, main = "Frenquency of Solids in Water samples histogram graph",
  xlab = "Solids", ylab = "Frequency",
  ylim = c(0,600), col = "darkolivegreen1",
  labels = TRUE
)
# Plot histogram for Chloramines
hist(
  Chloramines, main = "Frenquency of Chloramines in Water samples histogram graph",
  xlab = "Chloramines", ylab = "Frequency",
  ylim = c(0,600), col = "darkgoldenrod1",
  xlim = c(0,14),
  labels = TRUE
)
# Plot histogram for Sulfates
hist(
  Sulfate, main = "Frenquency of Sulfate in Water samples histogram graph",
  xlab = "Sulfate", ylab = "Frequency",
  ylim = c(0,1200), col = "cornflowerblue",
  labels = TRUE
)
# Plot histogram for Conductivity
hist(
  Conductivity, main = "Frenquency of Conductivity of Water samples histogram graph",
  xlab = "Conductivity", ylab = "Frequency",
  ylim = c(0,600), col = "chartreuse2",
  labels = TRUE
)
# Plot histogram for Organic Carbon
hist(
  Organic_Carbon, main = "Frenquency of Organic Carbon in Water samples histogram graph",
  xlab = "Organic Carbon", ylab = "Frequency",
  ylim = c(0,600), col = "darksalmon",
  xlim = c(0,30),
  labels = TRUE
)
# Plot histogram for Trihalomethanes
hist(
  Trihalomethanes, main = "Frenquency of Trihalomethanes in Water samples histogram graph",
  xlab = "Trihalomethanes", ylab = "Frequency",
  ylim = c(0,600), col = "deepskyblue",
  xlim = c(0, 140),
  labels = TRUE
)
# Plot histogram for Turbidity
hist(
  Turbidity, main = "Frenquency of Turbidity of Water samples histogram graph",
  xlab = "Turbidity", ylab = "Frequency",
  ylim = c(0,600), col = "cornsilk3",
  xlim = c(1, 7),
  labels = TRUE
)

## POTABLE - NON POTABLE PROPERTIES ##

# Plot pH histogram for potable/non-potable properties
pH_water <- ddply(water, "Potability", summarise, grp.mean=mean(pH))

ggplot(water, aes(x = pH, color=Potability, fill=Potability))+
  geom_histogram(position="identity", alpha = 0.5) +
  geom_vline(data = pH_water, aes(xintercept = grp.mean, color=Potability),
             linetype = "dashed") + 
  scale_x_continuous(breaks=seq(0, 14, by=2)) +
  scale_color_manual(values = c("tomato4",
                                "darkgreen", "brown")) + scale_fill_manual(values =
                                                                             c("rosybrown2", "palegreen","palegreen4")) + 
  labs(title ="Histogram of pH for Potability", x="pH", y="Number of Samples") 

# Plot Hardness histogram for potable/non-potable properties
Hardness_water <- ddply(water, "Potability", summarise, grp.mean1=mean(Hardness, na.rm=TRUE))
ggplot(water, aes(x = Hardness, color=Potability, fill=Potability))+
  geom_histogram(position="identity", alpha = 0.5) +
  geom_vline(data = Hardness_water, aes(xintercept = grp.mean1, color=Potability),
             linetype = "dashed") +
  scale_x_continuous(breaks=seq(0, 500, by=20)) +
  scale_color_manual(values = c("tomato4",
                                "darkgreen", "brown")) + scale_fill_manual(values =
                                                                             c("rosybrown2", "palegreen","palegreen4")) + 
  labs(title ="Histogram of Hardness for Potability", x="Hardness", y="Number of Samples") 


# Plot Solids histogram for potable/non-potable properties
Solids_water <- ddply(water, "Potability", summarise, grp.mean2=mean(Solids, na.rm=TRUE))
ggplot(water, aes(x = Solids, color=Potability, fill=Potability))+
  geom_histogram(position="identity", alpha = 0.5) +
  geom_vline(data = Solids_water, aes(xintercept = grp.mean2, color=Potability),
             linetype = "dashed") +
  scale_x_continuous(breaks=seq(0, 150000, by=10000)) +
  scale_color_manual(values = c("tomato4",
                                "darkgreen", "brown")) + scale_fill_manual(values =
                                                                             c("rosybrown2", "palegreen","palegreen4")) + 
  labs(title ="Histogram of Solids for Potability", x="Solids", y="Number of Samples") 

# Plot Chloramines histogram for potable/non-potable properties
Chloramines_water <- ddply(water, "Potability", summarise, grp.mean3=mean(Chloramines, na.rm=TRUE))
ggplot(water, aes(x = Chloramines, color=Potability, fill=Potability))+
  geom_histogram(position="identity", alpha = 0.5) +
  geom_vline(data = Chloramines_water, aes(xintercept = grp.mean3, color=Potability),
             linetype = "dashed") +
  scale_x_continuous(breaks=seq(0, 14, by=1)) +
  scale_color_manual(values = c("tomato4",
                                "darkgreen", "brown")) + scale_fill_manual(values =
                                                                             c("rosybrown2", "palegreen","palegreen4")) + 
  labs(title ="Histogram of Chloramines for Potability", x="Chloramines", y="Number of Samples") 

# Plot Sulfate histogram for potable/non-potable properties
Sulfate_water <- ddply(water, "Potability", summarise, grp.mean4=mean(Sulfate, na.rm=TRUE))
ggplot(water, aes(x = Sulfate, color=Potability, fill=Potability))+
  geom_histogram(position="identity", alpha = 0.5) +
  geom_vline(data = Sulfate_water, aes(xintercept = grp.mean4, color=Potability),
             linetype = "dashed") +
  scale_x_continuous(breaks=seq(0, 500, by=50)) +
  scale_color_manual(values = c("tomato4",
                                "darkgreen", "brown")) + scale_fill_manual(values =
                                                                             c("rosybrown2", "palegreen","palegreen4")) + 
  labs(title ="Histogram of Sulfate for Potability", x="Sulfate", y="Number of Samples")

# Plot Conductivity histogram for potable/non-potable properties
Conductivity_water <- ddply(water, "Potability", summarise, grp.mean5=mean(Conductivity, na.rm=TRUE))
ggplot(water, aes(x = Conductivity, color=Potability, fill=Potability))+
  geom_histogram(position="identity", alpha = 0.5) +
  geom_vline(data = Conductivity_water, aes(xintercept = grp.mean5, color=Potability),
             linetype = "dashed") +
  scale_x_continuous(breaks=seq(0, 750, by=50)) +
  scale_color_manual(values = c("tomato4",
                                "darkgreen", "brown")) + scale_fill_manual(values =
                                                                             c("rosybrown2", "palegreen","palegreen4")) + 
  labs(title ="Histogram of Conductivity for Potability", x="Conductivity", y="Number of Samples") 

# PLot Organic Carbon histogram for potable/non-potable properties
OrganicCarbon_water <- ddply(water, "Potability", summarise, grp.mean6=mean(Organic_Carbon, na.rm=TRUE))
ggplot(water, aes(x = Organic_Carbon, color=Potability, fill=Potability))+
  geom_histogram(position="identity", alpha = 0.5) +
  geom_vline(data = OrganicCarbon_water, aes(xintercept = grp.mean6, color=Potability),
             linetype = "dashed") +
  scale_x_continuous(breaks=seq(0, 50, by=2)) +
  scale_color_manual(values = c("tomato4",
                                "darkgreen", "brown")) + scale_fill_manual(values =
                                                                             c("rosybrown2", "palegreen","palegreen4")) + 
  labs(title ="Histogram of Organic Carbon for Potability", x="Organic Carbon", y="Number of Samples") 

# Plot Trihalomethanes histogram for potable/non-potable properties
Trihalomethanes_water <- ddply(water, "Potability", summarise, grp.mean7=mean(Trihalomethanes, na.rm=TRUE))
ggplot(water, aes(x = Trihalomethanes, color=Potability, fill=Potability))+
  geom_histogram(position="identity", alpha = 0.5) +
  geom_vline(data = Trihalomethanes_water, aes(xintercept = grp.mean7, color=Potability),
             linetype = "dashed") +
  scale_x_continuous(breaks=seq(0, 300, by=10)) +
  scale_color_manual(values = c("tomato4",
                                "darkgreen", "brown")) + scale_fill_manual(values =
                                                                             c("rosybrown2", "palegreen","palegreen4")) + 
  labs(title ="Histogram of Trihalomethanes for Potability", x="Trihalomethanes", y="Number of Samples") 

# Plot Turbidity histogram for potable/non-potable properties
Turbidity_water <- ddply(water, "Potability", summarise, grp.mean8=mean(Turbidity, na.rm=TRUE))
ggplot(water, aes(x = Turbidity, color=Potability, fill=Potability))+
  geom_histogram(position="identity", alpha = 0.5) +
  geom_vline(data = Turbidity_water, aes(xintercept = grp.mean8, color=Potability),
             linetype = "dashed") +
  scale_x_continuous(breaks=seq(0, 7, by=1)) +
  scale_color_manual(values = c("tomato4",
                                "darkgreen", "brown")) + scale_fill_manual(values =
                                                                             c("rosybrown2", "palegreen","palegreen4")) + 
  labs(title ="Histogram of Turbidity for Potability", x="Turbidity", y="Number of Samples")


water <- read.csv("water_potability.csv")
water <- na.omit(water)
water <- unique(water)
water <- subset(water,
                (ph>=0 & ph<=14) &
                  (Hardness>0) &
                  (Solids>0) &
                  (Chloramines>0) &
                  (Sulfate>0) &
                  (Conductivity>=1e-18 & Conductivity<=10e7)
                &
                  (Organic_carbon>0) &
                  (Trihalomethanes>0) &
                  (Turbidity>0) &
                  (Potability==1 | Potability==0))

set.seed(1)
# Split data
dt <- sort(sample(nrow(water),nrow(water)*0.8))
train <- water[dt,]
test <- water[-dt,]

# Fit a logistic regression model
log_model <- glm(Potability ~ ., data = train, family = "binomial")

# Display the summary of the model
summary(log_model)
# Use the model to predict probabilities on the testing set
predicted_probability <- predict(log_model, newdata = test, type = "response")

# Convert probabilities to binary predictions (0 or 1)
predicted_labels <- ifelse(predicted_probability > 0.5, 1, 0)

# Assuming your actual labels are in the "Potability" column of the testing set
actual_labels <- test$Potability

# Create a confusion matrix to evaluate model performance
confusion_matrix <- table(Actual = actual_labels, Predicted = predicted_labels)

# Calculate accuracy, 95% CI, no-information rate, and p-value
model_performance <- confusionMatrix(data= as.factor(predicted_labels), reference = as.factor(actual_labels))

# Display the model performance summary
print(model_performance)

#Plot ROC curve

ROCRpred <- prediction(predicted_probability, test$Potability) 
ROCRperf <- ROCR::performance(ROCRpred, "tpr", "fpr") 
plot(ROCRperf)

#Calculate AUC Value
as.numeric(performance(ROCRpred, "auc")@y.values)

#Using Hosmer - Lemeshow test to evaluate the model
performance_hosmer(log_model, n_bins = 10)

