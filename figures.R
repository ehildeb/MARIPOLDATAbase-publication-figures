# this is the code to create the figures 1, 2, 5, 





# Load necessary packages for data manipulation and visualization
library(readxl)       # For reading Excel files
library(openxlsx)     # Additional tools for Excel file handling
library(countrycode)  # For country code conversions (not explicitly used here)
library(data.table)   # Efficient data manipulation
library(tidyverse)    # Includes ggplot2, dplyr, and other useful tools
library(dplyr)        # For data manipulation
library(stringr)      # For string operations
library(ggplot2)      # For data visualization
library(zoo)          # For working with time-series data
library(tidytext)     # For text mining and manipulation
library("rnaturalearth")
library("rnaturalearthdata")
require(countrycode)
require(maps)


# Set the working directory where the data file is located
setwd("P:/10_ERC_MARIPOLDATA/WP1/Collected Data/4_final data/Aussda database publications")  # Specify the path to your working directory if needed

# Load the dataset from an Excel file
bbnj <- read_excel("10839_da_en_v3_0.xlsx")  # Replace with the actual file path



#################### figure 1



bbnj %>% select(IGC, negotiation_format) %>% table



################### figure 2

# Filter out NAs in the 'date' column before grouping
cumulative_counts <- bbnj %>%
  filter(!is.na(date)) %>%  # Ignore rows with NA in the 'date' column
  group_by(date) %>%
  summarise(Count = n(), .groups = "drop") %>%
  mutate(CumulativeCount = cumsum(Count))

# Filter out NAs in 'date' and 'IGC' when merging with cumulative counts
plot_data <- bbnj %>%
  select(date, IGC) %>%
  filter(!is.na(date), !is.na(IGC)) %>%  # Ignore rows with NA in 'date' or 'IGC'
  distinct() %>%
  right_join(cumulative_counts, by = "date")

# Ensure the 'date' column is of Date type for plotting
plot_data$date <- as.Date(plot_data$date)

# Create the dot plot with cumulative counts
ggplot(plot_data, aes(x = date, y = CumulativeCount, color = IGC,size = Count)) +
  geom_point(shape = 19, alpha = 0.8) +  # Use filled points with transparency
  labs(
    x = "Date",
    y = "Number of Entries",
    color = "IGC"
  ) +
  guides(size = "none") +  # Suppress size legend
  theme_minimal() +
  theme(
    panel.grid = element_blank(),  # Remove grid
    axis.text.x = element_text(angle = 45, hjust = 1, size = 16),  # Larger x-axis text
    axis.text.y = element_text(size = 16),  # Larger y-axis text
    axis.title = element_text(size = 18, face = "bold"),  # Larger and bold axis titles
    legend.title = element_text(size = 16, face = "bold"),  # Larger and bold legend title
    legend.text = element_text(size = 14),  # Larger legend text
    plot.title = element_text(hjust = 0.5, face = "bold", size = 20)  # Center and larger bold title
  ) +
  scale_size_continuous(range = c(3, 10))  # Adjust size of the points for better visibility

################### figure 4


# Filter the data for relevant observations
# Exclude rows where 'double' equals 1
bbnj_ac <- filter(bbnj, double != 1)

# Retain only rows where the negotiation format is either "plenary" or "working group"
bbnj_ac <- filter(bbnj_ac, negotiation_format %in% c("plenary", "working group"))

# Further filter rows where 'type_obs' equals 1
#bbnj_ac <- filter(bbnj_ac, type_obs == 1)

# Count the number of observations per actor (e.g., country)
countries_fa <- bbnj_ac %>%
  group_by(actor) %>%
  dplyr::summarise(count = n())

# Load a world map dataset using rnaturalearth and convert it to an sf object
world <- ne_countries(scale = "medium", returnclass = "sf")

# Standardize the country names in the world map to lowercase for matching purposes
world$sovereignt <- str_to_lower(world$sovereignt)

# Extract a list of unique country names from the world map dataset
countries_list <- unique(world$sovereignt)

# Harmonize actor names in the dataset to match the names in the world map
countries_fa$actor[countries_fa$actor == "usa"] <- 'united states of america'
countries_fa$actor[countries_fa$actor == "uk"] <- 'united kingdom'
countries_fa$actor[countries_fa$actor == "cote divoire"] <- 'ivory coast'
countries_fa$actor[countries_fa$actor == "viet nam"] <- 'vietnam'
countries_fa$actor[countries_fa$actor == "bahamas"] <- 'the bahamas'
countries_fa$actor[countries_fa$actor == "tanzania"] <- 'united republic of tanzania'
countries_fa$actor[countries_fa$actor == "republic of korea"] <- 'south korea'
countries_fa$actor[countries_fa$actor == "cabo verde"] <- 'cape verde'
countries_fa$actor[countries_fa$actor == "syrian arabic republic"] <- 'syria'
countries_fa$actor[countries_fa$actor == "papua neu guinea"] <- 'papua new guinea'
countries_fa$actor[countries_fa$actor == "timor-leste"] <- 'east timor'
countries_fa$actor[countries_fa$actor == "tunesia"] <- 'tunisia'
#Cook Islands and Palestine are listed under other sovereign states in the "world" data set

# Filter for countries that are in the world map dataset or represent the EU
countries_fa <- countries_fa %>% filter(actor %in% countries_list | actor == "eu")

# Add a new column to facilitate merging with the world map dataset
countries_fa <- countries_fa %>% mutate(sovereignt = actor)

# Define a list of European Union (EU) member states
eu <- c("Austria", "Italy", "Belgium", "Latvia", "Bulgaria", "Lithuania", "Croatia", 
        "Luxembourg", "Cyprus", "Malta", "Czech Republic", "Netherlands", "Denmark", "Poland",
        "Estonia", "Portugal", "Finland", "Romania", "France", "Slovakia", "Germany", 
        "Slovenia", "Greece", "Spain", "Hungary", "Sweden", "Ireland", "United Kingdom")

# Convert the list of EU countries to lowercase for consistency
eu <- str_to_lower(eu)

# Merge the world map data with the actor data based on the 'sovereignt' column
world <- merge(world, countries_fa, by = 'sovereignt', all.y = TRUE, all.x = TRUE)

# Update the count for EU member states to reflect the count for "EU"
world$count <- ifelse(world$sovereignt %in% eu, countries_fa$count[countries_fa$actor == "eu"], world$count)

# Rename the count column for better interpretation in the plot
world$Interventions <- world$count

# Plotting the map with intervention counts
ggplot() +
  geom_sf(data = world, aes(fill = count)) +  # Use fill to represent intervention counts
  scale_fill_viridis_c(
    option = "plasma", 
    trans = "sqrt",  # Apply a square root transformation to the fill scale
    name = "Number of \nInterventions",  # Custom legend title
    breaks = c(25, 50, 75, 100, 125),  # Specify the breakpoints for the scale
  ) +
  theme_minimal() +  # Use a minimal theme for clean visualization
  theme(
    panel.grid = element_blank(),  # Remove grid lines
    panel.background = element_rect(fill = "white", color = NA),  # Set background to white
    plot.background = element_rect(fill = "white", color = NA),  # Set plot background to white
    legend.title = element_text(size = 14, face = "bold"),  # Larger legend title
    legend.text = element_text(size = 12),  # Larger legend text
    axis.text = element_blank(),  # Remove axis text
    axis.title = element_blank(),  # Remove axis titles
    plot.title = element_text(hjust = 0.5, face = "bold", size = 18),  # Center and bold title
    plot.subtitle = element_text(hjust = 0.5, size = 14)  # Center subtitle
  )
################### figure 5
# Filter the data to include only specific negotiation formats
df <- filter(bbnj, negotiation_format %in% c("plenary", "working group"))
# Check the representation of 'obo' (observer) groups
table(df$obo)

# Further filter to include specific OBO groups and actors of interest
df <- filter(df, obo %in% c("african group", "caricom", "clam", "psids") | 
               actor %in% c("eu", "china", "usa", "russia"))

# Exclude rows where speaking time exceeds 900 seconds for better data quality
df <- filter(df, time_difference <= 900)

# Replace missing 'obo' values with the corresponding 'actor' value
df$obo <- ifelse(is.na(df$obo), df$actor, df$obo)

# Exclude data for 'g77+china' as it's not part of the analysis
df <- filter(df, obo != "g77+china")

# Rename specific OBO groups and actors for consistency in presentation
current_names <- c("african group", "caricom", "china", "clam", "eu",  
                   "psids", "russia", "usa")  # Current names in the data
renamed_values <- c("AFRICAN GROUP", "CARICOM", "China", "CLAM", "EU", 
                    "PSIDS", "Russia", "USA")  # Desired display names

# Apply renaming using factor levels and labels
df$obo <- factor(df$obo, levels = current_names, labels = renamed_values)

# Summarize the data to calculate the total speaking time by 'obo' and 'package_label'
summary_table <- df %>%
  group_by(obo, package_label) %>%
  summarise(sum_time_difference = sum(time_difference, na.rm = TRUE))

# Calculate the percentage contribution of each package for each OBO
summary_table <- summary_table %>%
  group_by(obo) %>%
  mutate(percentage = (sum_time_difference / sum(sum_time_difference)) * 100)

# Create a stacked bar plot to visualize the speaking time by OBO and package
figure5 <- ggplot(summary_table, aes(x = obo, y = sum_time_difference, fill = package_label)) +
  geom_bar(stat = "identity") +  # Stacked bar plot
  geom_text(aes(label = paste0(round(percentage, 1), "%")), 
            position = position_stack(vjust = 0.5), size = 5, fontface = "bold") +  # Add percentage labels
  labs(
    x = "State / Alliance",  # X-axis label
    y = "Speaking Time in Seconds",  # Y-axis label
    fill = "Package"  # Legend title
  ) +
  theme_minimal() +  # Apply a minimal theme
  theme(
    plot.title = element_text(size = 16, face = "bold", hjust = 0.5),  # Customize plot title
    axis.title.x = element_text(size = 14, face = "bold"),  # Customize x-axis title
    axis.title.y = element_text(size = 14, face = "bold"),  # Customize y-axis title
    axis.text.x = element_text(size = 12, angle = 45, hjust = 1),  # Rotate x-axis labels for readability
    axis.text.y = element_text(size = 12),  # Customize y-axis text
    legend.title = element_text(size = 12, face = "bold"),  # Customize legend title
    legend.text = element_text(size = 12)  # Customize legend text
  )

# Display the plot
figure5


############################## figure 10 


# Filter the dataset to retain only relevant columns and rows
bbnj_filtered <- bbnj %>%
  filter(!is.na(sentiment_constr), !is.na(date), !is.na(IGC), IGC %in% c("1", "2", "3", "4", "5", "5.2", "5.3")) %>%
  mutate(date = as.Date(date))



# Calculate daily mean constructive language score
daily_means <- bbnj_filtered %>%
  group_by(date) %>%
  summarise(
    DailyMean = mean(sentiment_constr, na.rm = TRUE),
    .groups = "drop"
  )


# Calculate mean by IGC
igc_means <- bbnj_filtered %>%
  group_by(IGC) %>%
  summarise(
    IGCMean = mean(sentiment_constr, na.rm = TRUE),
    .groups = "drop"
  )


# Merge daily means with IGC means for plotting
plot_data <- bbnj_filtered %>%
  left_join(daily_means, by = "date") %>%
  left_join(igc_means, by = "IGC")

plot_data$date <- as.Date(plot_data$date)


# Create the plot with all IGC facets in one row
ggplot(plot_data, aes(x = date, y = DailyMean)) +
  geom_point(aes(color = DailyMean),shape = 20, size = 3, alpha = 0.8) + # Daily means represented as color
  geom_hline(data = igc_means, aes(yintercept = IGCMean, linetype = "IGC mean"), color = "darkred", size = 1) +
  #  facet_wrap(~IGC, nrow = 1, scales = "free_x") + # Allow dynamic facet widths
  facet_grid(~IGC, scales = "free_x", space = "free_x") +  
  scale_color_gradient2(
    low = "darkblue", 
    mid = "lightblue",  # Change from white to light gray for better contrast
    high = "blue",  # Make high values slightly less intense
    midpoint = 0, 
    name = "Daily mean"
  ) +
  scale_x_date(
    date_labels = "%Y-%m-%d",
    date_breaks = "day"
  ) +
  labs(
    x = "Date",
    y = "Mean constructive language score",
    linetype = "IGC mean"
  ) +
  theme(
    panel.grid = element_blank(),
    axis.text.x = element_text(angle = 45, hjust = 1, size = 14), # Increase x-axis font size
    axis.text.y = element_text(size = 14), # Increase y-axis font size
    axis.title = element_text(size = 16, face = "bold"), # Increase and bold axis titles
    plot.title = element_text(size = 18, face = "bold", hjust = 0.5), # Larger, centered title
    legend.title = element_text(size = 14, face = "bold"), # Increase legend title font size
    legend.text = element_text(size = 12), # Increase legend text font size
    panel.spacing.x = unit(0.5, "lines")
  )
