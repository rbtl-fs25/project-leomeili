library(tidyverse)
library(dplyr)
library(googlesheets4)

ewaste_data <- read_sheet("https://docs.google.com/spreadsheets/d/1VcLpSGWubzM7qzZt8LDADRe6cKcU7vAcqSQ0kC9gzeM/edit?gid=1293632712#gid=1293632712")

glimpse(ewaste_data)

write_csv(ewaste_data, "data/raw/ewaste-survey-data.csv")
write_rds(ewaste_data, "data/raw/ewaste-survey-data.rds")


dictionary <- read_sheet("https://docs.google.com/spreadsheets/d/1s58192mwzclzxkkBA5H1orxQBcbTLtAmK_1TWk41jlQ/edit?gid=0#gid=0")
write_csv(dictionary, "data/processed/dictionary.csv")
