# BINF 6890 Assignment #1 --- Nadira Robertson
## Packages used -------
library(sf)
library(maps)
library(tidyverse)
theme_set(theme_light())

# Download Emballonuridae data file from BOLD

Emballonuridae <- read_tsv("http://www.boldsystems.org/index.php/API_Public/combined?taxon=Emballonuridae&format=tsv")

# Write file to data folder

write_tsv(Emballonuridae, "../data/Emballonuridae_BOLD_data.tsv")

# Load file of BOLD data for Emballonuridae

Emballonuridae <- read_tsv(file = "../data/Emballonuridae_BOLD_data.tsv")

# Create separate file, remove na values from data, and count BIN records in each subfamily

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

# Graph BIN samples for each subfamily

ggplot(data = samplecount) +
  geom_boxplot(mapping = aes(x = subfamily_name, y = BIN_size, fill = subfamily_name), outlier.color = "red", outlier.fill = "red", outlier.size = 2) +
  theme(legend.position = "none") +
  labs(title = "Distribution of BIN sizes by subfamily", x = "Subfamily", y = "BIN size (# of records)")

# Geographic comparison between subfamilies

geo_counts <- embsubbin %>%
  count(subfamily_name, country)

# Graph of bin records per country (Unused)

ggplot(geo_counts, aes(x = country, y = n, fill = subfamily_name)) +
  geom_col(position = "dodge") +
  labs(title = "Number of records per country by subfamily", x = "Country", y = "Number of Records")

# Table of country data and plot top 5 (extra)

sort(table(Emballonuridae$"country"), decreasing = TRUE)

plot(sort(table(Emballonuridae$"country"), decreasing = TRUE)[1:5])

# World map graph of subfamilies collect sf data for map

emb_sf <- st_as_sf(embsubbin, coords = c("lon", "lat"), crs = 4326)

# World map of subfamily locations plot graph command

world <- st_as_sf(maps::map("world", plot = FALSE, fill = TRUE))

ggplot() +
  geom_sf(data = world, fill = "cornsilk", color = "gray") +
  geom_sf(data = emb_sf, aes(color = subfamily_name), size = 2) +
  labs(title = "Geographic distribution of Emballonuridae Subfamily BINs", color = "Subfamily") +
  theme(panel.background = element_rect(fill = "aliceblue"))

# Unique bins per country for statistical test

uniq_bin_country <- embsubbin %>%
  group_by(country, subfamily_name) %>%
  summarise(BIN_uniqcount = n_distinct(bin_uri))

# Wilcoxon/Mann-Whitney test for comparing unique BINs across countries (unused)

wilcox.test(BIN_uniqcount ~ subfamily_name, data = uniq_bin_country)
