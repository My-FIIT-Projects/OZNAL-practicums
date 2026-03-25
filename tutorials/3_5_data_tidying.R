

# libraries

library(tidyverse)
library(magrittr)

setwd("C:/Users/fmojt/DataScience/OZNAL_practicums/data/original")
getwd()


data <- read_csv("players_22.csv", col_names = TRUE, num_threads = 4)
colnames(data)

data %<>% 
  select(short_name, dob, starts_with("skill")) %>%
  slice(1:5)

data


transposed_v1 <- data %>%
  select(short_name, starts_with("skill")) %>%
  pivot_longer(
    cols = starts_with("skill"),
    names_to = "skill",
    values_to = "value"
  )

transposed_v1

transposed <- data %>%
  select(short_name, starts_with("skill")) %>%
  pivot_longer(
    cols = starts_with("skill"),
    names_to = "skill",
    values_to = "value"
  ) %>%
  pivot_wider(
    names_from = short_name,
    values_from = value
  )

transposed

## Pivot longer

transposed <- pivot_longer(data,              
             !c(short_name, dob), # These columns are not be touched.
             names_to = "skill",  # The key is made for all three skills...
             values_to = "value") # from which values are extracted.
transposed

## Pivot wider

data_reversed <- pivot_wider(transposed,
                             id_cols = !c(short_name, dob),
  names_from = short_name,
  values_from = value
)

data_reversed

# data_reversed <- pivot_wider(
#   transposed,
#   id_cols = si
#   names_from = short_name,
#   values_from = value
# )

data_reversed
