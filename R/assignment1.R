#BINF 6890 Assignment #1 --- Nadira Robertson
#Assignment 2 Edits By: Sabrina Saiphoo
#Modified the output plots to have scaled axes
#Adjusted an unused figure to display more information
#Included a statistical test
## Packages used -------
library(sf)
library(maps)
library(vegan)
library(tidyverse)
theme_set(theme_light())

# clean the workspace
rm(list = ls())

# Load file of BOLD data for Emballonuridae

Emballonuridae <- read_tsv(file = "../data/Emballonuridae_BOLD_data.tsv")

# This is used to identify what columns we have available to us. Here we can see that we need subfamily_name, bin_uri, country, lat, lon
names(Emballonuridae)

# Create separate file, remove NA values from data, and count BIN records in each subfamily

embsubbin <- Emballonuridae %>% drop_na(subfamily_name, bin_uri, country, lat, lon)

embsubbin %>%
  count(subfamily_name)

# Unique BIN counts for each subfamily

uniqbin_counts <- embsubbin %>%
  group_by(subfamily_name) %>%
  summarise(BIN_count = n_distinct(bin_uri))
print(uniqbin_counts)

# Graph of unique BIN counts

ggplot(uniqbin_counts, aes(x = subfamily_name, y = BIN_count, fill = subfamily_name)) +
  geom_col() +
  labs(title = "Unique BINS in Emballonuridae subfamilies", x = "Subfamily name", y = "Number Unique of BINs") +
  theme(legend.position = "none")

# Number for BIN samples for each subfamily

samplecount <- embsubbin %>%
  group_by(subfamily_name, bin_uri) %>%
  summarize(BIN_size = n())

# Graph BIN samples for each subfamily. The data for this boxplot appears to be skewed so it is good to modify the yaxis so the data be represented well. 

ggplot(data = samplecount) +
  geom_boxplot(mapping = aes(x = subfamily_name, y = BIN_size, fill = subfamily_name), outlier.color = "red", outlier.fill = "red", outlier.size = 2) +
  theme(legend.position = "none") +
  scale_y_log10() + # here was the addition to scale the y axis
  labs(title = "Distribution of BIN sizes by subfamily", x = "Subfamily", y = "BIN size (# of records)")

# Geographic comparison between subfamilies

geo_counts <- embsubbin %>%
  count(subfamily_name, country)

# Running table() on the country column of geo_counts to identify which countries are listed twice. This would show which countries have both subfamilies.

table(geo_counts$country) #this identifies Kenya and South Sudan


# Graph of bin records per country (Unused)

ggplot(geo_counts, aes(x = country, y = n, fill = subfamily_name)) +
  geom_col(position = "stack") +
  scale_y_sqrt() +# scaled y
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  labs(title = "Number of records per country by subfamily", x = "Country", y = "Number of Records (Scaled by SQRT)")

# Table of country data and plot top 5 (extra)

sort(table(Emballonuridae$"country"), decreasing = TRUE)

plot(sort(table(Emballonuridae$"country"), decreasing = TRUE)[1:5])

# Identify coordinates that have from both subfamilies 

e_data <- embsubbin %>% 
  filter(subfamily_name == "Emballonurinae") %>% 
  distinct(country)

t_data <- embsubbin %>% 
  filter(subfamily_name == "Taphozoinae") %>% 
  distinct(country)

intersect(t_data, e_data)
rm(t_data, e_data)

# Recalculate 3 dataframes to highlight on map

edata_plot <- embsubbin %>%
  filter(subfamily_name == "Emballonurinae") %>% 
  filter(!country %in% c("Kenya", "South Sudan"))

tdata_plot <- embsubbin %>%
  filter(subfamily_name == "Taphozoinae") %>% 
  filter(!country %in% c("Kenya", "South Sudan"))

bdata_plot <- embsubbin %>% 
  filter(country %in% c("Kenya", "South Sudan"))

# Modify the data to fit the map package

tdata_plot_sf <- st_as_sf(tdata_plot, coords = c("lon","lat"), crs = 4326)
edata_plot_sf <- st_as_sf(edata_plot, coords = c("lon","lat"), crs = 4326)
bdata_plot_sf <- st_as_sf(bdata_plot, coords = c("lon","lat"), crs = 4326)

# World map of subfamily locations plot graph command

world <- st_as_sf(maps::map("world", plot = FALSE, fill = TRUE))

emb_map <- bind_rows(
  tdata_plot_sf %>% mutate(Group = "Taphozoinae"),
  edata_plot_sf %>% mutate(Group = "Emballonurinae"),
  bdata_plot_sf %>% mutate(Group = "Overlap (Kenya & South Sudan)")
)

ggplot() +
  geom_sf(data = world, fill = "cornsilk", color = "gray80") +
  geom_sf(data = emb_map, aes(color = Group), size = 2, alpha = 0.9) +
  scale_color_manual(values = c(
    "Emballonurinae" = "skyblue2",
    "Taphozoinae" = "orange1",
    "Overlap (Kenya & South Sudan)" = "darkgreen")) +
  labs(
    title = "Geographic Distribution of Emballonuridae Subfamilies",
    color = "Subfamily") +
  theme_minimal() +
  theme(panel.background = element_rect(fill = "aliceblue", colour = NA))


###Statistical Test#####
# PERMANOVA test used to compare BIN composition of the subfamilies across the countries

# 0/1 Matrix of BINs
bin_matrix <- embsubbin %>%
  distinct(country, subfamily_name, bin_uri) %>% 
  mutate(present = 1) %>%
  pivot_wider(names_from = bin_uri, values_from = present, values_fill = 0)

# Separating the bin_matrix into the country/sub_family and then BINs to use in the PERMANOVA
meta <- bin_matrix[, c("country", "subfamily_name")]
bins_only <- bin_matrix %>% select(-country, -subfamily_name)

# Running the PERMANOVA
permanova_result <- adonis2(bins_only ~ subfamily_name,
                            data = meta,
                            method = "jaccard")
permanova_result




