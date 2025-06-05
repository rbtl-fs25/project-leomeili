library(tidyverse)
library(dplyr)
library(googlesheets4)

data_raw <- read_sheet("https://docs.google.com/spreadsheets/d/1VcLpSGWubzM7qzZt8LDADRe6cKcU7vAcqSQ0kC9gzeM/edit?gid=1293632712#gid=1293632712")

glimpse(data_raw)
names(data_raw)

#assigning the variables
data_clean1 <- data_raw |> 
  select("date" = "Timestamp",
         "ewaste_amount" = "Do you have electronic waste (e-waste) lying around at home? If yes, approximately how many items?",
         "ewaste_disposal" = "Have you ever disposed of e-waste?",
         "ewaste_recycling" = "Have you ever recycled e-waste?",
         "crm_familiarity" = "Are you aware of the term critical raw material (CRM) and what it stands for?",
         "even_odd" = "Is your birthday an even or odd number (the day of the month)?",
         "article" = "Were you able to open and read the article?",
         "co2_mining" = "For the next question, please indicate how strongly you agree with each of the following sentences stating reasons for recycling e-waste.\n\nThe sentences go as follows:\n\"We should recycle e-waste in order to 'add reason here'.\"\nAn example:\n\"We should recycle e-waste in order to reduce dependence on CRM imports\"\n\nThe scale goes from 1 to 5, where 1 represents 'Strongly disagree' and 5 represents 'Strongly agree'. [Reduce CO2 emissions from mining/sourcing]",
         "crm_recovery" = "For the next question, please indicate how strongly you agree with each of the following sentences stating reasons for recycling e-waste.\n\nThe sentences go as follows:\n\"We should recycle e-waste in order to 'add reason here'.\"\nAn example:\n\"We should recycle e-waste in order to reduce dependence on CRM imports\"\n\nThe scale goes from 1 to 5, where 1 represents 'Strongly disagree' and 5 represents 'Strongly agree'. [Recover critical raw materials (CRM)]",
         "unethical_mining" = "For the next question, please indicate how strongly you agree with each of the following sentences stating reasons for recycling e-waste.\n\nThe sentences go as follows:\n\"We should recycle e-waste in order to 'add reason here'.\"\nAn example:\n\"We should recycle e-waste in order to reduce dependence on CRM imports\"\n\nThe scale goes from 1 to 5, where 1 represents 'Strongly disagree' and 5 represents 'Strongly agree'. [Reduce dependence on ethically controversial mining]",
         "pollution" = "For the next question, please indicate how strongly you agree with each of the following sentences stating reasons for recycling e-waste.\n\nThe sentences go as follows:\n\"We should recycle e-waste in order to 'add reason here'.\"\nAn example:\n\"We should recycle e-waste in order to reduce dependence on CRM imports\"\n\nThe scale goes from 1 to 5, where 1 represents 'Strongly disagree' and 5 represents 'Strongly agree'. [Avoid general pollution]",
         "circecon_local" = "For the next question, please indicate how strongly you agree with each of the following sentences stating reasons for recycling e-waste.\n\nThe sentences go as follows:\n\"We should recycle e-waste in order to 'add reason here'.\"\nAn example:\n\"We should recycle e-waste in order to reduce dependence on CRM imports\"\n\nThe scale goes from 1 to 5, where 1 represents 'Strongly disagree' and 5 represents 'Strongly agree'. [Support a circular economy/local supply chains]",
         "crm_scarcity" = "For the next question, please indicate how strongly you agree with each of the following sentences stating reasons for recycling e-waste.\n\nThe sentences go as follows:\n\"We should recycle e-waste in order to 'add reason here'.\"\nAn example:\n\"We should recycle e-waste in order to reduce dependence on CRM imports\"\n\nThe scale goes from 1 to 5, where 1 represents 'Strongly disagree' and 5 represents 'Strongly agree'. [Reduce dependence on CRM imports/avoid CRM scarcity]",
         "pet" = "For the next question, please indicate how important you consider the recycling of each of the following materials to be.\n\nThe scale goes from 1 to 5, where 1 represents 'Not important at all' and 5 represents 'Extremely important'. [PET]",
         "glass" = "For the next question, please indicate how important you consider the recycling of each of the following materials to be.\n\nThe scale goes from 1 to 5, where 1 represents 'Not important at all' and 5 represents 'Extremely important'. [Glass]",
         "paper_cardboard" = "For the next question, please indicate how important you consider the recycling of each of the following materials to be.\n\nThe scale goes from 1 to 5, where 1 represents 'Not important at all' and 5 represents 'Extremely important'. [Paper/Cardboard]",
         "metal" = "For the next question, please indicate how important you consider the recycling of each of the following materials to be.\n\nThe scale goes from 1 to 5, where 1 represents 'Not important at all' and 5 represents 'Extremely important'. [Metal]",
         "organic_waste" = "For the next question, please indicate how important you consider the recycling of each of the following materials to be.\n\nThe scale goes from 1 to 5, where 1 represents 'Not important at all' and 5 represents 'Extremely important'. [Organic Waste]",
         "electronic_waste" = "For the next question, please indicate how important you consider the recycling of each of the following materials to be.\n\nThe scale goes from 1 to 5, where 1 represents 'Not important at all' and 5 represents 'Extremely important'. [Electronic Waste]",
         "personal_importance" = "How important is it to you personally that you recycle your electronic waste?",
         "systemic_importance" = "From a broader, systemic point of view, how important do you believe e-waste recycling is for society?",
         "likeliness_nextitem" = "How likely are you to recycle your next item of e-waste (e.g. phones, chargers, laptops)?",
         "age" = "How old are you?",
         "gender" = "What is your gender?",
         "degree" = "What is your highest degree?",
         "household" = "What kind of household do you live in?")

  
#adding an id, setting the date and removing duplicate
data_clean2 <- data_clean1 |> 
  mutate(id = seq(1:n())) |> 
  relocate(id) |> 
  mutate(date = as.Date(date)) |> 
  filter(id != 21)

#writing csv's for the processed data
write_csv(data_clean2, "data/processed/ewaste_data-clean.csv")
write_rds(data_clean2, "data/processed/ewaste_data-clean.rds")


##creating a summary and averages for the likert data##

likert_recycling <- c("co2_mining", "crm_recovery", "unethical_mining", "pollution", 
                  "circecon_local", "crm_scarcity")
likert_intentions <- c("likeliness_nextitem", "systemic_importance","personal_importance")
likert_materials <- c("pet", "glass", "paper_cardboard", 
                      "metal", "organic_waste", "electronic_waste")
#these are the three groups of categories that will receive a figure each

#selecting the data for recycling motivation
data_likert_recycling <- data_clean2 |> 
  select(!likert_intentions & !likert_materials)

#pivot to long format and calculating averages
likert_long_recycling <- data_likert_recycling |> 
  pivot_longer(cols = all_of(likert_recycling), 
               names_to = "item", 
               values_to = "score") |> 
  group_by(item, even_odd) |>
  summarise(mean_score = mean(score, na.rm = TRUE), .groups = "drop") |> 
  mutate(item = factor(item, levels = likert_recycling)) |> 
  mutate(item = recode(item,
                       "co2_mining" = "Reducing CO2 emissions\nfrom mining",
                       "crm_recovery" = "Recovering CRM",
                       "unethical_mining" = "Reducing dependence on\nethically controversial mining",
                       "pollution" = "Reducing overall pollution",
                       "circecon_local" = "Supporting a circular economy\nand local supply chains",
                       "crm_scarcity" = "Avoiding CRM scarcity\nthrough reduced dependence")) |> 
  mutate(even_odd = recode(even_odd, "Even" = "Control", "Odd" = "CRM mining\ninfo provided"))

#back to wide for plotting, adding an order and renaming for readability
likert_wide_recycling <- likert_long_recycling |> 
  pivot_wider(names_from = even_odd, values_from = mean_score)

#writing it to the data/final folder, in .rds to preserve the levels of the factor
write_rds(likert_wide_recycling, "data/final/likert_recycling.rds")
write_rds(likert_long_recycling, "data/final/likert_recycling_long.rds")
  

##same process for the data for recycling intention
data_likert_intentions <- data_clean2 |> 
  select(!likert_recycling & !likert_materials)

likert_long_intentions <- data_likert_intentions |> 
  pivot_longer(cols = all_of(likert_intentions), 
               names_to = "item", 
               values_to = "score") |> 
  group_by(item, even_odd) |>
  summarise(mean_score = mean(score, na.rm = TRUE), .groups = "drop") |> 
  mutate(item = factor(item, levels = likert_intentions)) |> 
  mutate(item = recode(item,
                       "systemic_importance" = "How systemically important do you\nconsider e-waste recycling to be?",
                       "likeliness_nextitem" = "How likely are you to recycle\nyour next item of e-waste?",
                       "personal_importance" = "How important is e-waste\nrecycling to you personally?")) |> 
  mutate(even_odd = recode(even_odd, "Even" = "Control", "Odd" = "CRM mining\ninfo provided"))
  
likert_wide_intentions <- likert_long_intentions |> 
  pivot_wider(names_from = even_odd, values_from = mean_score)

write_rds(likert_wide_intentions, "data/final/likert_intentions.rds")
write_rds(likert_long_intentions, "data/final/likert_intentions_long.rds")


##same process for the data for material recycling
data_likert_materials<- data_clean2 |> 
  select(!likert_recycling & !likert_intentions)

likert_long_materials <- data_likert_materials |> 
  pivot_longer(cols = all_of(likert_materials), 
               names_to = "item", 
               values_to = "score") |> 
  group_by(item, even_odd) |>
  summarise(mean_score = mean(score, na.rm = TRUE), .groups = "drop") |> 
  mutate(item = factor(item, levels = likert_materials)) |> 
  mutate(item = recode(item,
                       "pet" = "PET",
                       "glass" = "Glass",
                       "paper_cardboard" = "Paper/Cardboard",
                       "metal" = "Metal",
                       "organic_waste" = "Organic Waste",
                       "electronic_waste" = "Electronic Waste")) |> 
  mutate(even_odd = recode(even_odd, "Even" = "Control", "Odd" = "CRM mining\ninfo provided"))

likert_wide_materials <- likert_long_materials |> 
  pivot_wider(names_from = even_odd, values_from = mean_score)

write_rds(likert_wide_materials, "data/final/likert_materials.rds")
write_rds(likert_long_materials, "data/final/likert_materials_long.rds")


#creating the dataset for the demographics overview
education_summary <- data_clean2 |> 
  select(degree) |> 
  count(degree) |> 
  mutate(percentage = n / sum(n) * 100,
         label = paste0(degree, "\n", round(percentage, 1), "%"))

household_summary <- data_clean2 |> 
  select(household) |> 
  count(household) |> 
  mutate(percentage = n / sum(n) * 100,
         label = paste0(household, "\n", round(percentage, 1), "%"))

  
write_csv(education_summary, "data/final/education.csv")
write_csv(household_summary, "data/final/household.csv")


#creating the dataset for CRM familiarity
crm_familiar_data <- data_clean2 |> 
  select(crm_familiarity) |> 
  drop_na() |>
  count(crm_familiarity) |> 
  mutate(percentage = n / sum(n) * 100,
         label = paste0(crm_familiarity, "\n", round(percentage, 1), "%"))

write_csv(crm_familiar_data, "data/final/CRMfamiliarity.csv")


#creating the dataset for the e-waste amount table
amount_ewaste <- data_clean2 |> 
  select(ewaste_amount) |> 
  count(ewaste_amount)

avg_value <- round(with(amount_ewaste, sum(ewaste_amount * n) / sum(n)), 2)
amount_ewaste <- amount_ewaste |> mutate(Average = NA)
avg_row <- data.frame(ewaste_amount = NA,
                      n = NA,
                      Average = avg_value)
final_amount_ewaste <- bind_rows(amount_ewaste, avg_row)

write_csv(final_amount_ewaste, "data/final/ewaste_amount.csv")
