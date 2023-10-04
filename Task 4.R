# Reading the data file 
library(tidyverse)
library(ggplot2)

raw_file1 <- readLines(con = "https://www.sao.ru/lv/lvgdb/article/UCNG_Table4.txt")

L1 <- # Reading it back, want to remove line 2
  (substr(x = raw_file1, start = 1, stop = 2) == "--") %>% 
  which() %>% 
  min() 

column_headings <- 
  str_split(string = raw_file1[L1-1], pattern = "\\|") %>% 
  unlist() %>% 
  str_trim() # Pulling out column headings

comma_separated_values1 <- 
  raw_file1[(L1+1):length(raw_file1)] %>% 
  gsub("\\|", ",", .) %>% 
  gsub(" ", "", .) # Getting the rest of the data

comma_separated_values_with_names1 <- # Putting it all together
  c(paste(column_headings, collapse = ","),
    comma_separated_values1)  

# Saving
cat(comma_separated_values_with_names1, sep = "\n", 
    file = "formatted_galaxies1.csv")
galaxies1 <- read_csv("formatted_galaxies1.csv")

# Removing unwanted columns
trimmed_gal1 <- galaxies1 %>%
  transmute(Name = name, Velocity = cz)
trimmed_gal <- galaxies %>% 
  transmute(Name = name, Distance = D)

# joining
final <- trimmed_gal %>% inner_join(trimmed_gal1)

# plot for task 1
ggplot(final, aes(x = Distance, y = Velocity)) + 
  geom_point() +  # Add points
  labs(
    title = "Scatterplot of Distance vs. Velocity",
    x = "Distance",
    y = "Velocity"
  ) + 
  theme_minimal() # We see that the farther away the galaxies are, the higher
# is their velocity in general. So yeah, I agree

# the Hubble's constant for task 2
final %>% mutate(Hubble = Velocity/Distance) %>% arrange(Name)



