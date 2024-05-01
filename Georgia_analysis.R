# Install required packages
install.packages("tidyverse")
install.packages("sf")
install.packages('devtools')
devtools::install_github("UrbanInstitute/urbnmapr")
rm(list=ls())
# Load libraries
library(tidyverse)
library(rio)
library(ggplot2)
library(sf)
library(urbnmapr)
library(dplyr)
library(scales)

# 1. Reading the data
college_data <- import("EducationDataPortal_04.03.2024_College (1).csv")
level_of_study <- import("EducationDataPortal_04.03.2024_level_of_study.csv")

# Convert column names to lowercase
colnames(college_data) <- tolower(make.names(colnames(college_data)))
colnames(level_of_study) <- tolower(make.names(colnames(level_of_study)))

# Filter unique institutions in Georgia based on unitid
ga_unique_institutions <- college_data %>%
  distinct(unitid, .keep_all = TRUE) %>%  # Get unique institutions based on unitid
  filter(state_abbr == "GA")  # Filter for institutions in Georgia

ga_map <- urbnmapr::counties %>%  
  filter(state_name == "Georgia")

total_institutions <- nrow(ga_unique_institutions)

# 1. Geographical distribution of educational institutions: 
# Create a data frame with required columns, round off longitude and latitude, and remove duplicates
map_data <- college_data %>%
  mutate(geo_longitude = round(geo_longitude, 2),
         geo_latitude = round(geo_latitude, 2)) %>%
  distinct(unitid, state_abbr, geo_longitude, geo_latitude, .keep_all = TRUE) %>%
  filter(!is.na(geo_longitude) & !is.na(geo_latitude))

# Get the map data for Georgia
ga_map <- urbnmapr::counties %>%
  filter(state_name == "Georgia")

# Plot the map
ggplot() +
  geom_polygon(data = ga_map, mapping = aes(x = long, y = lat, group = group), fill = "lightgray", color = "white", linewidth = 0.3) +
  geom_point(data = map_data, aes(x = geo_longitude, y = geo_latitude), color = "blue", size = 2, alpha = 0.8) +
  coord_map(projection = "albers", lat0 = 32, lat1 = 35) +
  theme_minimal() +
  labs(x = "Longitude",
       y = "Latitude",
       caption = "Data source: Education Data Portal") +
  theme(axis.text = element_text(size = 12),
        axis.title = element_text(size = 14, face = "bold"),
        plot.caption = element_text(size = 10, face = "italic"),
        plot.margin = unit(c(1, 1, 1, 1), "cm"),
        plot.title.position = "plot") +
  ggtitle("Geographical Distribution of Institutions in Georgia") +
  theme(plot.title = element_text(size = 18, face = "bold", hjust = 0.5))
# 2. Institution Types and Control
institution_types_control <- ga_unique_institutions %>%
  group_by(institution_level, inst_control) %>%
  summarise(count = n(), .groups = 'drop')

print(institution_types_control)

total_count <- sum(institution_types_control$count)
print(total_count)

ggplot(institution_types_control, aes(x = inst_control, y = count, fill = institution_level)) +
  geom_bar(stat = "identity", position = "dodge") +
  geom_text(aes(label = count), position = position_dodge(width = 0.9), vjust = -0.5) +
  labs(title = "Institution Types and Control in Georgia", x = "Institution Control", y = "Count") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1),
        axis.text.y = element_text(size = 8))+
  annotate("text", x = Inf, y = Inf, label = paste0("Total Count: ", total_count), hjust = 1.1, vjust = 1.1, size = 3)
# 3. Degree-Granting Institutions 
degree_granting <- ga_unique_institutions %>%
  count(degree_granting)

print(degree_granting)

ggplot(degree_granting, aes(x = degree_granting, y = n, fill = degree_granting)) +
  geom_bar(stat = "identity") +
  geom_text(aes(label = n), vjust = -0.5) +
  labs(title = "Degree-Granting Institutions in Georgia", x = "Degree-Granting Status", y = "Count") +
  theme_minimal()

# 4. Special Focus Institutions: HBCUs
hbcus_count <- sum(ga_unique_institutions$hbcu == "Yes", na.rm = TRUE)
cat("Number of HBCUs in Georgia:", hbcus_count, "\n")

hbcu_data <- ga_unique_institutions %>%
  group_by(hbcu) %>%
  summarise(count = n(), .groups = 'drop') %>%
  mutate(percentage = round(count / sum(count) * 100, 2))

ggplot(hbcu_data, aes(x = "", y = count, fill = hbcu)) +
  geom_bar(width = 1, stat = "identity") +
  coord_polar("y", start = 0) +
  labs(title = "Historically Black Colleges and Universities (HBCUs) in Georgia") +
  geom_text(aes(label = paste0(percentage, "%")), position = position_stack(vjust = 0.5)) +
  theme_minimal() +
  annotate("text", x = 1.5, y = sum(hbcu_data$count) / 2, label = paste0("Total: ", sum(hbcu_data$count)), color = "black", size = 4) +
  annotate("text", x = 1, y = -1, label = paste("HBCUs:", hbcus_count), hjust = 0.2, vjust = -2.5) 

# 5. Top 10 Counties by Number of Institutions
top_counties <- ga_unique_institutions %>%
  group_by(county_name) %>%
  summarise(count = n()) %>%
  arrange(desc(count)) %>%
  head(10)

#ggplot(top_counties, aes(x = reorder(county_name, count), y = count, fill = county_name)) +
ggplot(top_counties, aes(x = count, y = reorder(county_name, count), fill = county_name)) +  
  geom_bar(stat = "identity") +
  labs(title = "Top 10 Counties by Number of Institutions", x = "County", y = "Number of Institutions") +
  theme_minimal() +
  geom_text(aes(label = count), hjust = -0.1, vjust = 0.5, size = 3)+
  scale_fill_discrete(guide = "none")

# 6. Enrollment Trends
enrollment_trends <- level_of_study %>%
  filter(state_abbr == "GA") %>%
  group_by(year, level_of_study) %>%
  summarise(total_enrollment = sum(enrollment_fall, na.rm = TRUE), .groups = 'drop')

print(enrollment_trends)

ggplot(enrollment_trends, aes(x = year, y = total_enrollment, color = level_of_study, linetype = level_of_study)) +
  geom_line(size = 1.2) +
  geom_point(size = 3) +
  #geom_text(data = subset(enrollment_trends, year == max(year)), aes(label = total_enrollment), nudge_y = 10000, size = 3) +
  geom_text(aes(label = total_enrollment), vjust = -0.6 , size = 3) +
  labs(title = "Enrollment Trends by Level of Study in Georgia",
       x = "Year",
       y = "Total Enrollment",
       caption = "Data source: Education Data Portal") +
  scale_color_manual(values = c("First professional" = "firebrick", "Graduate" = "forestgreen", "Undergraduate" = "steelblue")) +
  theme_minimal() +
  theme(legend.position = "bottom",
        axis.text.x = element_text(angle = 45, hjust = 1),
        axis.title = element_text(size = 12, face = "bold"),
        axis.text = element_text(size = 10),
        axis.line = element_line(color = "black", size = 0.5),
        axis.ticks = element_line(color = "black", size = 0.5)) +
  scale_x_continuous(breaks = seq(2019, 2021, by = 1)) +
  scale_y_continuous(labels = scales::comma)
