# Load required libraries
library(dplyr)
library(leaflet)
library(ggplot2)
library(htmlwidgets)
library(htmltools)

# Replace with your published Google Sheet link
sheet_url <- "https://docs.google.com/spreadsheets/d/e/2PACX-1vROsLYRpCh6rUAQFbNcXtTHKpeFFPyWzlQmniXa1DL7BVKeeHkl8-Ml-924kHzpRiUV__q0lD8C10FZ/pub?output=csv"

# Read the data
df <- read.csv(sheet_url)
head(df)
# Calculate group-wise statistics
group_stats <- df %>%
  group_by(Group, Claimed.) %>%
  summarise(Total_Killed = sum(Killed, na.rm = TRUE), .groups = 'drop') %>%
  mutate(Claimed. = ifelse(Claimed. == "Yes", "Claimed", "Not Claimed"))

# Calculate percentage of killings by each group
group_pct <- df %>%
  group_by(Group) %>%
  summarise(Total_Killed = sum(Killed, na.rm = TRUE), .groups = 'drop') %>%
  mutate(Percentage = Total_Killed / sum(Total_Killed) * 100)

# Bar chart for killed by group (claimed/not claimed)
bar_chart <- ggplot(group_stats, aes(x = Group, y = Total_Killed, fill = Claimed.)) +
  geom_bar(stat = "identity", position = "stack") +
  labs(
    title = "Killed by Group (Claimed vs Not Claimed)",
    x = "Group",
    y = "Total Killed",
    fill = "Claimed?"
  ) +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

# Pie chart for group-wise killing percentage
pie_chart <- ggplot(group_pct, aes(x = "", y = Percentage, fill = Group)) +
  geom_bar(stat = "identity", width = 1) +
  coord_polar("y", start = 0) +
  labs(
    title = "Group-Wise Killing Percentage",
    fill = "Group"
  ) +
  theme_void() +
  theme(legend.position = "right")

# Save the charts as images
ggsave("bar_chart.png", bar_chart, width = 6, height = 4)
ggsave("pie_chart.png", pie_chart, width = 6, height = 4)

# Create HTML content for the legend
legend_html <- paste0(
  '<div style="position: fixed; top: 10px; left: 10px; z-index: 1000; background: white; padding: 10px; border: 2px solid grey; border-radius: 5px; box-shadow: 2px 2px 10px rgba(0,0,0,0.5);">',
  '<h3 style="margin: 0;">Group-Wise Statistics</h3>',
  '<h4 style="margin: 5px 0;">Killed by Group (Claimed vs Not Claimed)</h4>',
  '<img src="bar_chart.png" width="300" height="200">',
  '<h4 style="margin: 5px 0;">Group-Wise Killing Percentage</h4>',
  '<img src="pie_chart.png" width="300" height="200">',
  '</div>'
)

# Create leaflet map with legend
m <- leaflet(df) %>%
  addProviderTiles(providers$CartoDB.Positron) %>%
  setView(lng = 90.3563, lat = 23.6850, zoom = 7) %>%
  addMarkers(
    lng = ~Longitude,
    lat = ~Latitude,
    clusterOptions = markerClusterOptions(),
    popup = ~paste0(
      "<b>Group:</b> ", Group, "<br>",
      "<b>Date:</b> ", Date, "<br>",
      "<b>Location:</b> ", Location, "<br>",
      "<b>Target/Event:</b> ", Target.Event, "<br>",
      "<b>Killed:</b> ", Killed, "<br>",
      "<b>Injured:</b> ", Injured, "<br>",
      "<b>Claimed?:</b> ", Claimed., "<br>",
      "<b>Notes:</b> ", Notes
    )
  ) %>%
  addControl(
    html = htmltools::HTML(legend_html),
    position = "topleft"
  )

# Save the map as an HTML file
htmlwidgets::saveWidget(m, file = "map_with_legend.html", selfcontained = TRUE)

# Display the map
m