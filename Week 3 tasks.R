pacman::p_load(
  tidyverse, # the tidyverse framework
  kableExtra,#table
  flextable, #table
  skimr# quickly providing a broad overview of a data frame
)

options(scipen = 999) 

Natural_disasters = read.csv("~/Downloads/收藏/Genius Leon的/School works/Uni yr2/Term 1/Computational Methods/My_CSS_Project_1/Dataset/Natural disasters (EMDAT).csv")

# Inspect the data:
str(Natural_disasters)
glimpse(Natural_disasters)
skim(Natural_disasters)

'''
Natural_disasters %>%
  pivot_wider(
    names_from = Year, # first
    values_from = c(Deaths, Injuries, Homelessness, high_death) # second
  )
However, no need to pivot for this Natural disaster dataset  
''' 


# Select only relevant columns
ND_subset = Natural_disasters %>%
  select(Entity, Year, contains("deaths_all"), contains("injured_all"), contains("homeless_all")) %>%
  rename(Deaths = deaths_all_disasters, Injuries = injured_all_disasters, Homelessness = homeless_all_disasters)

str(ND_subset)




# Now create three tables showing the highest averages of deaths, injuries and homelessness (e.g. top 10)
ND_average = ND_subset %>%
  filter( !Entity %in% c("World", "Soviet Union")) %>% # Remove "World" and "Soviet Union"
  group_by(Entity) %>%
  summarise( average_deaths = mean(Deaths, na.rm = TRUE) %>% round(2),
             average_induries = mean(Injuries, na.rm = TRUE) %>% round(2),
             average_homelessness = mean(Homelessness, na.rm = TRUE) %>% round(2)
             )



# Create 3 tables for top 10 countries of highest average deaths, injuries and homelessness
deaths_ND = ND_average %>%
  arrange(desc(average_deaths)) %>%
  slice(0:10) %>%
  # Apply formatting for the table:
  kable(caption = "Top 10 Countries by Average Deaths") %>%
  kable_styling("striped") %>%
  kable_classic(full_width = FALSE)
deaths_ND


injuries_ND = ND_average %>%
  arrange(desc(average_induries)) %>%
  slice(0:10) %>%
  # Apply formatting for the table:
  kable(caption = "Top 10 Countries by Average Injuries") %>%
  kable_styling("striped") %>%
  kable_classic(full_width = FALSE)
injuries_ND


homelessness_ND = ND_average %>%
  arrange(desc(average_homelessness)) %>%
  slice(0:10) %>%
  # Apply formatting for the table:
  kable(caption = "Top 10 Countries by Average Homelessness") %>%
  kable_styling("striped") %>%
  kable_classic(full_width = FALSE)
homelessness_ND



# Create a new binary variable in the original dataset to show 
# whether the number of deaths by all disasters is higher than 500 in a given year
ND_subset = ND_subset %>%
  mutate(high_death = ifelse(Deaths > 500, TRUE, FALSE))



# Reshape the dataset (selected version) and save it as a separate dataset in your repository
ND_reshaped <- ND_subset %>%
  pivot_wider(
    names_from = Year,       # Specify the columns to pivot; so we are pivoting more rows into columns
    values_from = c(Deaths, Injuries, Homelessness, high_death)  # Specify the values columns
  )

# Save the ND_reshaped data frame as a separate R data set
saveRDS(ND_reshaped, "~/Downloads/收藏/Genius Leon的/School works/Uni yr2/Term 1/Computational Methods/My_CSS_Project_1/Dataset/ND_reshaped.rds")


