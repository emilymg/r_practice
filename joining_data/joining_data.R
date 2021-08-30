library(scales)

tuesdata <- tidytuesdayR::tt_load('2021-04-06')
forest <- tuesdata$forest
forest_area <- tuesdata$forest_area
brazil_loss <- tuesdata$brazil_loss
soybean_use <- tuesdata$soybean_use
veg_oil <- tuesdata$vegetable_oil

# Join brazil_loss, forest, and forest_area, keeping all brazil_loss joining matches from other 2
brazil_ext <- brazil_loss %>%
  left_join(forest_area, by = c("entity", "code", "year"))

# Confirm joins were successful
glimpse(brazil_ext)

# Add a column that totals hectares lost per year for all reasons
brazil_ext$total_loss_hect <- apply(brazil_ext[,4:14], 1, sum)

# Plot total forest loss per year in Brazil
ggplot(brazil_ext, aes(x = year, y = total_loss_hect/1000000)) +
  geom_col() +
  scale_x_continuous(n.breaks = 7) +
  ggtitle("Total forest loss per year in Brazil (hectares)") +
  xlab("Year") +
  ylab("Forest loss(million hectares)")

# Ignore aggregates (world and continent totals) in forest_area data
forest_area_country <- forest_area %>%
  na.omit() %>% # code for regional and continental aggregates is NA
  filter(entity != "World")

# Look at forest area totals in 1990 vs 2020:  top 20
forest_area_1992 <- forest_area_country %>%
  filter(year == 1992) %>%
  arrange(desc(forest_area)) %>%
  top_n(20)

forest_area_2020 <- forest_area_country %>%
  filter(year == 2020) %>%
  arrange(desc(forest_area)) %>%
  top_n(20)

# Full join individual year tables for comparison plotting
forest_area_compare <- full_join(forest_area_1992, forest_area_2020, by = c("entity", "year", "code", "forest_area"))

# Plot comparison of forest_area for each entity faceted by year of observation
ggplot(forest_area_compare, aes(entity, forest_area)) +
  geom_point() +
  facet_wrap(~year) +
  theme(axis.text.x = element_text(angle = 60, hjust = 1)) +
  xlab("Country") +
  ylab("% World Forest Area")
  
# Anti-join: which results are missing between the 1992 and 2020 tables?
anti_join(forest_area_1992, forest_area_2020, by = "code")
anti_join(forest_area_2020, forest_area_1992, by = "code")

# Semi-join: What observations in forest_area have matches in forest?
forest_semi <- semi_join(forest_area, forest) # net forest conversion observed every 10 years except 2015

# Join forest and forest_area, keep all matching rows
forest_joined <- inner_join(forest, forest_area) %>% 
  filter(entity != "World") #exclude aggregate World datapoints
 
