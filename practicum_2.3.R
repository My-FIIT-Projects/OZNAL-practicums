# Practicum 2.3

- Train log. regression, SVM or LDA/QDA
- Use ROC curves to determine the best probability threshold



## Hypothesis

# We hypothesize that a player’s class — such as attacker or defender — can be
# predicted based on their technical skills, including attributes like attacking
# and defending abilities, raw power, mentality, and stamina. The question remains:
#which of these attributes should be selected, and how can their relationship to
#a player’s class be effectively evaluated?
  
## Attributes

# Carry out EDA to identify the most suitable predictors for hypothesis.
# Leverage summary statistics, visualizations, and correlation matrices to explore
# the relationships between variables and select those with the highest relevance.
# 

## Model Building

# Utilize logistic regression, discriminant analysis, and support vector machines
# to model the relationship between the dependent variable and the player’s role 
# (e.g. offense, defense). Conduct a thorough comparison of these models, paying 
# close attention to their parameters and making adjustments as needed. Carefully
# assess the performance of each model to determine which is best suited for the
# tasks at hand.


# Implementation

## Libs

library("tidyverse")
library("tidymodels")
library("readr")
library("corrplot") # for visualizing numbers of heatmaps

## Setting working directory

setwd("C:/Users/fmojt/DataScience/OZNAL_practicums/data")
getwd()

## Loading Data

players <- read_csv("./players_22.csv", col_names = TRUE)
View(players)

## Dataset Preparation


### Handling Player Positions

# Player positions are stored in list. We will only the include the first position
# of a player because that is often the most relevant
# players$primary_position <- trimws(sub())

players <- players %>%
  mutate(primary_position = trimws(sub(",.*", "", player_positions)))

summary(players$primary_position)
table(players$primary_position)

### Filtering to Attackers and Defenders

# Player positions are various but generally they can be categorized into:
# a) Attackers (e.g. LF, CF)
# b) Defenders (e.g LWB, CWB)
# c) Midfielders (e.g. LM, LAM)
# d) Goalkeepers

# Offense positions
offense <- c("LF", "CF", "RF", "LW", "RW", "LS", "ST", "RS")

# Defense positions
defense <- c("LWB", "RWB", "LCB", "CB", "RCB", "LB", "RB")

# Midfield positions
midfield <- c("LM", "LAM", "CM", "RM", "RAM", "CAM", "CDM", "DM", "LDM", "RDM")

# Goalkeeper
goalkeeper <- c("GK")


# WE DECIDED TO PREDICT WHETHER PLAYERS ARE DEFENSIVE (0) or OFFENSIVE (1).
# This is because these classes are clearly separable and adding additional
# classes (like Midfielders) would introduce blurred boundaries

target_positions <- c(offense, defense)

### Filtering rows & creating target


players_filtered <- players %>% filter(primary_position %in% target_positions) %>%
  mutate(is_offense = ifelse(primary_position %in% offense, 1, 0))

table(players_filtered$primary_position)
print(paste0("Rows removed:", nrow(players) - nrow(players_filtered)))
print(paste0("Rows now: ", nrow(players_filtered)))

### Filtering Columns

players_col_filtered <- players_filtered %>%
  select(  contains("attacking"),
           contains("defending"),
           contains("shooting"),
           contains("power"),
           contains("mentality"),
           contains("movement"),
           contains("physic"),
           contains('passing'),
           is_offense
  )

str(players_col_filtered)


## EDA

### Removed Features

## CODES: HC (high collinearity), LCwT (low correlation with target)
## CODES: PCS (poor class separability)

# attacking_volleyes (HK with attacking_finishing)  
# attacking_crossing, attacking_heading_accuracy, attacking_short_passing: (LCwT)
# defending_marking_awareness, defending_standing_tackle
# defending_sliding_tackle -> HW
# power_long_shorts, power_shot_power, shooting -> HC
# power_jumping, power_stamina, power_strength -> LCwT
# mentality_composure -> LCwT
# mentality_positioning, mentality_penalties, mentality_interceptions -> HC
# movement_reactions, movement_balance -> LCwT
# movement_acceleration, movement_sprint_speed, physic -> HC
# passing -> HC
# skill_long_passing -> PCS


removed_features <- c('attacking_volleys', 'attacking_crossing',
                      'attacking_heading_accuracy', 'attacking_short_passing',
                      'defending_marking_awareness', 'defending_standing_tackle',
                      'defending_sliding_tackle', 'power_long_shots',
                      'power_shot_power', 'power_jumping', 'power_stamina',
                      'power_strength', 'shooting',
                      'mentality_composure', 'mentality_positioning',
                      'mentality_penalties', 'mentality_interceptions',
                      'movement_reactions', 'movement_balance',
                      'movement_acceleration', 'movement_sprint_speed', 'physic',
                      'skill_long_passing')

players_col_removed <- players_col_filtered %>%
  select(-all_of(removed_features))

### Feature Scaling

scaled_data <- scale(players_col_removed[, -which(names(players_col_removed) == "is_offense")])
colnames(scaled_data)
### Collinearity

players_attacking_cols <- scaled_data %>%
  select(contains("attacking"), contains("defending"), contains("shooting"),
         contains("power"), contains("mentality"), contains("movement"), 
         contains("physic"), contains('passing'), contains("positioning"),
         contains("stamina"), is_offense)

cor_matrix <- cor(players_attacking_cols)
#heatmap(corr_matrix)

corrplot(
  cor_matrix,
  method = "color",
  type = "upper",
  addCoef.col = "black",
  number.cex = 0.5,
  tl.cex = 0.7
)

colnames(players_col_removed)

### Class Separability

# boxplot(attacking_finishing ~ is_offense, data = players_col_removed)
# boxplot(defending ~ is_offense, data = players_col_removed)
players_long <- scaled_data %>%
  pivot_longer(
    cols = -is_offense,
    names_to = "feature",
    values_to = "value"
  )

ggplot(players_long, aes(x = factor(is_offense), y = value)) +
  geom_boxplot() +
  facet_wrap(~ feature, scales = "free") +
  labs(x = "is_offense", y = "value") +
  theme_minimal()

### Normality

# 
# players_long <- scaled_data %>%
#   pivot_longer(cols = -is_offense, names_to = "feature", values_to = "value")

ggplot(players_long, aes(sample = value, color = factor(is_offense))) +
  stat_qq() +
  stat_qq_line() +
  facet_wrap(~ feature, scales = "free") +
  theme_minimal() +
  labs(color = "is_offense")


### Homoscedasticity

### Outliers

### Feature-Target Relationship


