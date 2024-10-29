# Install packages
if (!require("pacman")) {
  install.packages("pacman")
}

pacman::p_load(
  tidyverse, # tidyverse pkgs including purrr
  glue, #combining strings and objects
  gapminder, # dataset
  ggplot2, #plotting
  gridExtra #arranging plots
) 


###################################
### 1. Iteration (if and if_else conditions):
ND_condition <- ND_subset %>%
  mutate(life.35 = 
    if(lifeExp >= 35){
      1
    } else {
      0
    })
ND_condition

# This method does the same thing:
ND_ifelse <- ND_subset %>%
  mutate(life.35 = if_else(lifeExp >= 35, 1, 0))
ND_ifelse

###################################
### 2. Functions:
'''
my_function <- function(x, y)
{
  # do
  # something
  # here
  return(result)
}
'''

### Using function for missing data:
fix_missing <- function(x) 
{
  x[x == -99] <- NA
  x
}

# Apply function to each column (vector) by using a loop (purrr method):
# map() makes a list
# map_lgl() makes a logical vector
# map_int() makes an integer vector
# map_dbl() makes a double vector
# map_chr() makes a character vector
df <- purrr::map_df(df, fix_missing)
df


###################################
# Create a list of data frames with England's biggest cities and their populations
data_list <- list(
  data.frame(City = c("London", "Birmingham", "Manchester"),
             Population = c(8961989, 1141816, 547627)),
  data.frame(City = c("Leeds", "Liverpool", "Newcastle"),
             Population = c(793139, 494814, 148190))
)
# Define the condition for filtering data frames
population_threshold <- 500000
filtered_data <- map(data_list, ~ filter(.x, Population >= population_threshold))
# Note ".x " means to use all columes
filtered_data

# Combine the filtered data frames into a single data frame
combined_data <- reduce(filtered_data, bind_rows)
combined_data


###################################
# The glue() function:
# glue() combines strings and objects:
names <- c("Nikki", "Maria", "Ozan")
fields <- c("Economics", "Demography", "Sociology")
glue("{names} is interested in {fields}.")


create_point_plot <- function(i) 
{
  airquality %>%
    ggplot(aes_string(x = names(airquality)[1], y = names(airquality)[i])) +
    geom_point() +
    labs(
      title = glue("Relationship between Ozone and {names(airquality)[i]}"),
      y = glue("{names(airquality)[i]}")
    )
}



###################################
### Questions:
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

# Or we can use this method:
ND_mean = ND_subset %>%
  select(-Year, -high_death) %>% # Remove all non-numeric columns
  group_by(Entity) %>%
  summarise(across(where(is.numeric), ~ mean(.x, na.rm = TRUE), .names = "average_{.col}")
            )
ND_mean


# We can also use purrr to automate data wrangling task based on the dataset 
#(e.g. summarising data)
ND_all_mean = ND_subset %>%
  select(-Entity, -Year, -high_death) %>%  # Remove non numerical columns 
  map_dbl(mean, na.rm = TRUE)



# Using purrr to automate plotting the trends of deaths, injuries, 
# and homelessness caused by all disasters for 5 countries in the dataset
filtered_ND = ND_subset %>%
  select(-high_death)
  filter(Entity %in% c("China", "Bangladesh", "Canada", "India", "Germany","France"))

ND_plot = function(i) 
{
  filtered_ND %>%
    ggplot(aes_string(x = names(filtered_ND)[2], y = names(filtered_ND)[i])) +
    geom_point() +
    geom_smooth(method = "lm", se = TRUE, color = "blue") +  # Add trend lin
    labs(
      title = glue("Trend of {names(filtered_ND)[i]}"),
      y = glue("{names(filtered_ND)[i]}")
    )
}

# The number 3 below means i starts from column 3 til the total number of the columns in the filtered_ND dataset:
plots_list <- map(3:ncol(filtered_ND), ND_plot)
plots_grid <- gridExtra::grid.arrange(grobs = plots_list, ncol = 2) # Adjust columns as needed
plots_grid




