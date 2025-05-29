# Assign the 'Total' dataset to a new variable 'MyData' for manipulation
MyData<-Total

# Convert 'Interaction number' to a factor (categorical variable)
MyData$`Interaction number` <- as.factor(MyData$`Interaction number`)
# Convert 'Feeding Method' to a factor
MyData$`Feeding Method` <- as.factor(MyData$`Feeding Method`)

library(dplyr)

#Count of data, Might not be needed now
# Count how many records exist for each 'Feeding Method'
MyData %>% count(MyData$`Feeding Method`)
# Count how many records exist for each 'Interaction Type'
MyData %>% count(MyData$`Interaction Type`)



#Use Total Data for Boxplot 
#
boxplot((Total$Total~ Total$`Interaction Type`* Total$`Feeding Method`),
        at = c(1,2,4,5), # Custom positions for boxplot groups
        las= 3, # Rotate axis labels
        col = c("darkgrey","grey"),
        border = "black",
        horizontal = FALSE,
        notch = FALSE,
        ylab= "Frequency of Competitive Interactions",
        xlab= "Feeding Method - Type of Competition",
        xaxt = "n",)  Don't draw x-axis labels yet
axis(side = 1, at = c(1,2,4,5), # Custom x-axis labels
     labels = c("Scatter - Contest", "Scatter - Scramble", "Target - Contest", "Target - Scramble"), las = 0)

boxplot.stats()
# Get summary stats from boxplot
Summary<-boxplot(Total$Total~ Total$`Interaction Type`* Total$`Feeding Method`)$stats
colnames(Summary)<-c("Scatter - Contest", "Scatter - Scramble", "Target - Contest", "Target - Scramble")
rownames(Summary)<-c("Min","First Quartile","Median","Third Quartile","Maximum")
Summary

# Test significance of interaction between factors
one.way <- aov(Total$Total~ Total$`Interaction Type`* Total$`Feeding Method`)
summary(one.way)


#Mixed Model time
# Install package for mixed models
install.packages("lme4")
library(lme4)
summary(Data) # Show structure of 'Data'

#Fit Mixed Model
Data$ID <- factor(Data$ID) # Ensure 'ID' is a factor (for random effects)


interaction_counts1 <- data.frame(Species = character(),
                                 InteractionType = character(),
                                 Survey = integer(),
                                 Count = numeric(),
                                 stringsAsFactors = FALSE)
# Loop through each unique combination of Species, Interaction Type, and Survey
for (species in unique(Data$`Competitor Species`)) {
  for (interaction_type in unique(Data$`Interaction Type`)) {
    for (survey in unique(Data$`Survey Number`)) {
      # Filter the data based on the current combination of species, interaction type, and survey
      filtered_data <- Data[Data$`Competitor Species` == species & Data$`Interaction Type` == interaction_type & Data$`Survey Number` == survey, ]
      
      # Count the total number of interactions for the current combination
      interaction_count1 <- sum(filtered_data$no)
      
      # Store the count in the interaction_counts data frame
      interaction_counts1 <- rbind(interaction_counts1, data.frame(Species = species,
                                                                 InteractionType = interaction_type,
                                                                 Survey = survey,
                                                                 Count = interaction_count1))
    }
  }
}
print(interaction_counts1)

# Fit Mixed Models & Compare AIC
model <- lmer(ModelData$Count ~ ModelData$`Feeding Method` + ModelData$Species + ModelData$`Interaction Type` + (1|ModelData$Date), data = ModelData)
summary(model)
AIC_value <- AIC(model) # Model fit
AIC_value

# Alternative models
model1 <- lmer(ModelData$Count ~ ModelData$`Feeding Method` + ModelData$Species + (1|ModelData$Date), data = ModelData)
summary(model1)
AIC_value1 <- AIC(model1)
AIC_value1

model2 <- lmer(ModelData$Count ~ ModelData$Species + ModelData$`Interaction Type` + (1|ModelData$Date), data = ModelData)
summary(model2)
AIC_value2 <- AIC(model2)
AIC_value2

## Generalized linear version
model <- glmer(ModelData$Count ~ ModelData$`Feeding Method` + ModelData$Species + ModelData$`Interaction Type` + (1|ModelData$Date), data = ModelData)
#id as random factor
#+ (1|Data$ID),

#Species Frequency Barplots
#Culmative frequency plot
str(Data)
freq <- table(Data$Species) # Count observations by Species
freq

#Histogram type graph
barplot(freq,
        xlab = "Species",
        ylab = "Frequency of Competitive Interactions",
        col = "grey",
        border = "black",
        ylim = c(0, max(freq) * 1.2),  # Increase the y-axis limits by 20%
        las = 1
)
abline(h = seq(0, max(freq) * 1.2, by = 10), lty = "dotted", col = "gray")
#standardize the species (divide by number of individuals)

freq1
barplot(freq1,
        xlab = "Species",
        ylab = "Frequency of Competitive Interactions",
        col = "grey",
        border = "black",
        ylim = c(0, max(freq1) * 1.2),  # Increase the y-axis limits by 20%
        las = 1
)
abline(h = seq(0, max(freq1) * 1.2, by = 10), lty = "dotted", col = "gray")
#standardize the species (divide by number of individuals)

freq <- table(Data$Species)
freq
freq1 <- table(Data$`Competitor Species`)
freq1
freq2 <- table(Adpt$Species)  

filtered_data <- subset(Data, Data$`Competitor Species` == "Seabass" & Data$`Survey Number` %in% 1:5)
filtered_data <- subset(Data, Data$Species == "Seabass" & Data$`Survey Number` %in% 1:5)

# Count the number of competitive interactions
filtered_data
print(filtered_data, n=35)

#For contest interactions
barplot(Adpt$Average.Interactions.C~Adpt$Species,
        xlab = "Species",
        ylab = "Frequency of Competition per Individual (Average)",
        col = "grey",
        border = "black",
        ylim = c(0, max(Adpt$Average.Interactions.C) * 1.4),  # Increase the y-axis limits by 20%
        las = 1
)
abline(h = seq(0, max(Adpt$Average.Interactions.C) * 1.2, by = 5), lty = "dotted", col = "gray")

# For victim species
barplot(Adpt$Average.Interactions.V~Adpt$Species,
        xlab = "Species",
        ylab = "Frequency of Competition per Individual (Average) ",
        col = "grey",
        border = "black",
        ylim = c(0, max(Adpt$Average.Interactions.V) * 1.4),  # Increase the y-axis limits by 20%
        las = 1
)
abline(h = seq(0, max(Adpt$Average.Interactions.V) * 1.2, by = 5), lty = "dotted", col = "gray")



#Culmative frequency
cum_freq <- cumsum(table(Data$Species)) # Victim species
cum_freq
cum_freq <- cum_freq[cum_freq != 2]
#Check for 'NA'
is.na(Data$Species)
#Removes any 'NA'
clean_data <- Data[!is.na(Data$Species), ]
cum_freq <- as.numeric(cum_freq)

#Culmative frequency
cum_freq2 <- cumsum(table(Data$`Competitor Species`))  # Competitor species
cum_freq2
#Check for 'NA'
is.na(Data$`Competitor Species`)
#Removes any 'NA'
clean_data <- Data[!is.na(Data$`Competitor Species`), ]
cum_freq2 <- as.numeric(cum_freq2)
#Adds target into cum_freq2 at (0)

# Add Target manually to correct for missing value
missing_value_pos <- which(labels1 == "Target")  # Find the position of 'E' in the first plot's data
cum_freq2 <- c(cum_freq2[1:(missing_value_pos-1)], 202, cum_freq2[missing_value_pos:length(cum_freq2)])


labels1 <- c("Blonde", "BullHuss ", "CatShark", "Seabass", "Hound", "Target", "Thornback")
# Create a cumulative frequency plot
plot(1:length(cum_freq), cum_freq, type = "b", lwd = 2, col = "blue",
      xlab = "Species", ylab = "Cumulative Frequency", xaxt = "n")
axis(1, at = 1:length(cum_freq), labels = labels1)

labels2 <- c("Blonde", "BullHuss ", "CatShark", "Seabass", "Hound", "Thornback")
lines(1:length(cum_freq2), cum_freq2, type = "b", lwd = 2, col = "red")
grid(lty = "dotted", col = "gray")
legend("topleft", legend = c("Victim Species", "Competitor Species"), lty = 1, pch = NA,
       col = c("blue", "red"))
#boxplot

freq <- table(Data$Species)
freq
freq1 <- table(Data$`Competitor Species`)
freq1

#Frequency per day
# Count the frequency of interactions per day for each competitor
competitor_freq <- table(Data$Date, Data$`Competitor Species`)

species_data <- split(Data, Data$Species)
competitor_data <- split(Data, Data$`Competitor Species`)

# Calculate the frequency of interactions per day for each species
species_freq <- sapply(species_data, function(x) table(x$Date))
species_freq
# Calculate the frequency of interactions per day for each competitor
competitor_freq <- sapply(competitor_data, function(x) table(x$Date))

# Create boxplots for species and competitor frequencies
par(mar = c(5, 4, 4, 2))
par(las = 0)
boxplot(species_freq, 
        at = c(1,3,5,7,9,11,13,15),
        las = 3,
        col = "cornflowerblue",
        border = "black",
        horizontal = FALSE,
        notch = FALSE,
        ylab = "Frequency of Interactions per day",
        xlab = "Species",
        xaxt = "n")


boxplot(competitor_freq,
        at = c(2,4,6,8,10,12,14),
        add = TRUE,
        col = "deeppink",
        border = "black",
        xaxt = "n")


legend_size <- 0.7

legend("topleft", 
       legend = c("Victim Species", "Competitor Species"), 
       fill = c("cornflowerblue", "deeppink"),
       cex = legend_size)

axis(side = 1, at = c(2,4,6,8,10,12,14),
     labels = c("Blonde", "BullHuss ", "CatShark", "Seabass", "Smoothhound", "Target", "Thornback"), las = 0)



layout (matrix(1:1, nc=1))
par(mar = c(5, 4, 4, 2))
par(cex.main = 1)

boxplot(species_freq, 
        las = 3,
        col = "grey",
        border = "black",
        horizontal = FALSE,
        notch = FALSE,
        ylab = "Frequency of Interactions Per Day",
        xlab = "Species",
        xaxt = "n",
        main = "Victim")
axis(side = 1, at = c(1,2,3,4,5,6,7),
     labels = c("Blonde", "BullHuss ", "CatShark", "Seabass", "Smoothhound", "Target", "Thornback"), las = 0)

Summary<-boxplot(species_freq)$stats
colnames(Summary)<-c("Blonde", "BullHuss ", "CatShark", "Seabass", "Smoothhound", "Target", "Thornback")
rownames(Summary)<-c("Min","First Quartile","Median","Third Quartile","Maximum")
Summary

boxplot(competitor_freq, 
        panel.grid = TRUE,
        at = c(1,2,3,4,5,7),
        las = 3,
        col = "grey",
        border = "black",
        horizontal = FALSE,
        notch = FALSE,
        ylab = "Frequency of Interactions Per Day",
        xlab = "Species",
        xaxt = "n",
        main = "Competitor")
axis(side = 1, at = c(1,2,3,4,5,6,7),
     labels = c("Blonde", "BullHuss ", "CatShark", "Seabass", "Smoothhound", "Target", "Thornback"), las = 0)

Summary<-boxplot(competitor_freq)$stats
colnames(Summary)<-c("Blonde", "BullHuss ", "CatShark", "Seabass", "Smoothhound", "Thornback")
rownames(Summary)<-c("Min","First Quartile","Median","Third Quartile","Maximum")
Summary

#should be done after this
