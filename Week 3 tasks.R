pacman::p_load(
  tidyverse, # the tidyverse framework
  skimr# quickly providing a broad overview of a data frame
)

Natural_disasters = read.csv("~/Downloads/收藏/Genius Leon的/School works/Uni yr2/Term 1/Computational Methods/My_CSS_Project_1/Dataset/Natural disasters (EMDAT).csv")

# Inspect the data:
str(Natural_disasters)
glimpse(Natural_disasters)

'''
Natural_disasters %>%
  pivot_wider(
    names_from = Year, # first
    values_from = count # second
  )
However, no need to pivot for this Natural disaster dataset  
''' 

# Select only relevant columns
ND_subset = Natural_disasters %>%
  select(Entity, Year, contains("deaths_all"), contains("injured_all"), contains("homeless_all")) %>%
  rename(Deaths = deaths_all_disasters, Injuries = injured_all_disasters, Homelessness = homeless_all_disasters)


# Or we can merge datasets together by this method: 
ND_average = ND_subset %>%
  filter( !Entity %in% c("World", "Soviet Union")) %>%
  summarise( avg_deaths = mean(Deaths, na.rm = TRUE),
             avg_induries = mean(Injuries, na.rm = TRUE),
             avg_homelessness = mean(Homelessness, na.rm = TRUE)
             )

deaths_ND = ND_subset %>%
  select(Entity, Year, Deaths) %>%
  arrange(desc(Deaths)) %>%
  slice(0:10)

injuries_ND = ND_subset %>%
  select(Entity, Year, Injuries) %>%
  arrange(desc(Injuries)) %>%
  slice(0:10)

homelessness_ND = ND_subset %>%
  select(Entity, Year, Homelessness) %>%
  arrange(desc(Homelessness)) %>%
  slice(0:10)

Combined_ND_Top10 = deaths_ND %>%
  left_join(injuries_ND, by = "Entity") %>%
  left_join(homelessness_ND, by = "Entity") %>%
  rename(Deaths_Top10 = Deaths, Injuries_Top10 = Injuries, Homlessness_Top10 = Homelessness) 


# Create a new binary variable in the original dataset to show 
# whether the number of deaths by all disasters is higher than 500 in a given year
ND_subset = ND_subset %>%
  mutate(high_death = ifelse(Deaths > 500, TRUE, FALSE))


