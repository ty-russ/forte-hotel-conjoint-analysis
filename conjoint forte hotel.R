## Marketing Analytics Fundamentals
## Conjoint Analysis - Forte Hotel Design

# This script performs conjoint analysis to identify the most preferred hotel design attributes
# based on customer preferences. It includes segmentation and market share analysis.

# Load required library
library(conjoint)
library(ggplot2)
library(fpc)
library(broom)

# Step 1: Define hotel design profiles using the expand.grid function
hotel <- expand.grid(
  room = c("small suite", "large room", "room office"),
  amenity = c("internet", "speaker phone", "room fax"),
  leisure = c("exercise room", "pool", "exercise + pool"),
  extras = c("shoe shine", "tape library", "fruit cheese", "newspaper"),
  delivery = c("yes", "no")
)
print(hotel)
cat("Total profiles:", dim(hotel)[1], "\n")  # Display the total number of profiles

# Step 2: Load data
preferences <- read.csv("forte_preferences.csv", header = TRUE) 
profiles <- read.csv("forte_profiles.csv", header = TRUE) 
levelnames <- read.csv("forte_levels.csv", header = TRUE) 
simulation1 <- read.csv("forte_simulation1.csv", header = TRUE)  # Existing hotels
simulation2 <- read.csv("forte_simulation2.csv", header = TRUE)  # New hotels

# Confirm data dimensions
cat("Preferences Data Dimensions:", dim(preferences), "\n")
cat("Profiles Data Dimensions:", dim(profiles), "\n")
cat("Level Names Data Dimensions:", dim(levelnames), "\n")
cat("Simulation 1 Data Dimensions:", dim(simulation1), "\n")
cat("Simulation 2 Data Dimensions:", dim(simulation2), "\n")

# Step 3: Calculate part-worth utilities for all respondents
partutilities <- caPartUtilities(y = preferences, x = profiles, z = levelnames)
print(partutilities)

# Step 4: Calculate total utilities for all respondents
totalutilities <- caTotalUtilities(y = preferences, x = profiles)
print(totalutilities)

# Step 5: Analyze feature importance for a specific respondent (e.g., respondent No. 26)
importance <- caImportance(y = preferences[26, ], x = profiles)
cat("Feature Importance for Respondent 26:\n")
print(importance)

# Step 6: Conjoint analysis for a specific respondent and all respondents
cat("Conjoint Analysis for Respondent 26:\n")
Conjoint(preferences[26, ], profiles, levelnames)

cat("Conjoint Analysis for All Respondents:\n")
Conjoint(y = preferences, x = profiles, z = levelnames)

# Step 7: Segmentation of respondents using k-means clustering
cat("K-Means Clustering (2 Segments):\n")
segments_2 <- caSegmentation(preferences, profiles)
print(segments_2$seg)

cat("K-Means Clustering (3 Segments):\n")
segments_3 <- caSegmentation(preferences, profiles, c = 3)
print(segments_3$seg)

# Step 8: Visualize the 2-segment division
cat("Segmentation Summary:\n")
summary(segments_2)

# Plot clustering results
plotcluster(segments_2$util, segments_2$sclu)

# Enhanced clustering visualization using ggplot2
dcf <- discrcoord(segments_2$util, segments_2$sclu)
assignments <- augment(segments_2$segm, dcf$proj[, 1:2])
ggplot(assignments) +
  geom_point(aes(x = X1, y = X2, color = .cluster)) +
  labs(color = "Cluster Assignment", title = "K-Means Clustering Results")

# Step 9: Market share analysis using different simulation profiles
# Uncomment the following lines to perform market share analysis using different models
# ShowAllSimulations(sym = simulation1, y = preferences, x = profiles)
# ShowAllSimulations(sym = simulation2, y = preferences, x = profiles)
# caLogit(simulation1, preferences, profiles)

# End of Script
