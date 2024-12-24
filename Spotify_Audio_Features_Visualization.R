# Exploring Trends and Relationships in Spotify Audio Features
# Dataset: Spotify Weekly Top 200 Songs Streaming Data (https://www.kaggle.com/datasets/yelexa/spotify200)
# Spotify Weekly Top 200 Songs Streaming Data


# Required libraries
library(ggcorrplot)
library(ggplot2)
library(dplyr)
library(lubridate)
library(reshape2) 
library(RColorBrewer)


# 1. Data Overview and Cleaning
# Load the data:
spotify <- read.csv(file.choose())
# Cleaning the data:
# List of columns to convert to numeric:
numeric_columns <- c(
  "rank", "artists_num", "peak_rank", "previous_rank", "weeks_on_chart", 
  "streams", "danceability", "energy", "key", "mode", "loudness", 
  "speechiness", "acousticness", "instrumentalness", "liveness", 
  "valence", "tempo", "duration"
)
# Convert to numeric:
for (col in numeric_columns) {
  spotify[[col]] <- as.numeric(spotify[[col]])
}
# Check for NAs introduced during conversion:
colSums(is.na(spotify[numeric_columns]))
# Check for missing values:
colSums(is.na(spotify))
# View rows with missing values in key columns: 
spotify[!complete.cases(spotify), ]  
spotify <- spotify[complete.cases(spotify), ]
# List of character columns to clean:
character_columns <- c("region", "artist_genre", "country", "language", "track_name")
# Remove extra spaces and standardize to title case:
for (col in character_columns) {
  spotify[[col]] <- trimws(spotify[[col]])  # Remove leading/trailing whitespace
  spotify[[col]] <- tolower(spotify[[col]])  # Convert to lowercase for consistency
  spotify[[col]] <- tools::toTitleCase(spotify[[col]])  # Convert to title case
}
# Check unique values for 'region' and 'country':
unique(spotify$region)
unique(spotify$country)
# Remove Duplicate Rows:
spotify <- spotify[!duplicated(spotify), ]
# Filter Out Unwanted Values:
spotify <- spotify[!(spotify$region %in% c("NA", "Global")), ]
spotify <- spotify[!(spotify$artist_genre %in% c("NA", "0")), ]
# Convert to Date format:
spotify$release_date <- as.Date(spotify$release_date, format = "%Y-%m-%d")
spotify$week <- as.Date(spotify$week, format = "%Y-%m-%d")
# Extract year for analysis:
spotify$release_year <- format(spotify$release_date, "%Y")
# Check the structure of the cleaned dataset:
str(spotify)
# Check for missing values again:
colSums(is.na(spotify))
spotify <- spotify[!is.na(spotify$release_date), ]
# Summary of key columns:
summary(spotify[c("rank", "streams", "danceability", "energy", "duration")])
# Preview the first few rows:
head(spotify)
View(spotify)


# 2. Relationships Between Audio Features
# How are different audio features correlated with each other?

# 2.1. Pairwise Correlation Heatmap:
# This plot visualizes the relationships between audio features, with each cell showing a correlation value ranging from -1 (strong negative) to +1 (strong positive).
# Select only numeric columns for correlation:
numeric_columns <- c(
  "rank", "artists_num", "streams", "danceability", "energy", "key", "mode", 
  "loudness", "speechiness", "acousticness", "instrumentalness", "liveness", 
  "valence", "tempo", "duration"
)
spotify_numeric <- spotify[numeric_columns]
# Compute the correlation matrix:
correlation_matrix <- cor(spotify_numeric, use = "complete.obs")
# Reshape the correlation matrix into a long format:
correlation_long <- melt(correlation_matrix)
# Plot the heatmap:
ggplot(correlation_long, aes(x = Var1, y = Var2, fill = value)) +
  geom_tile(color = "white") +  
  scale_fill_gradient2(
    low = "blue", mid = "white", high = "red", midpoint = 0,
    limits = c(-1, 1), name = "Correlation"
  ) +
  geom_text(aes(label = round(value, 2)), size = 5, fontface = "bold", color = "black") +
  labs(
    title = "Correlation Heatmap",
    x = NULL,
    y = NULL
  ) +
  theme_minimal() +
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1, size = 14, face = "bold"),  
    axis.text.y = element_text(size = 14, face = "bold"),  
    plot.title = element_text(hjust = 0.5, size = 18, face = "bold"),  
    legend.title = element_text(size = 12, face = "bold"),  
    legend.text = element_text(size = 10, face = "bold")  
  )
# Results:
# Positive Correlation: energy and loudness (~0.68) which shows tracks with higher energy tend to be louder.
# Negative Correlation: acousticness and energy (~-0.54), which shows songs with high acousticness typically have lower energy.


# 2.2. Energy vs. Acousticness:
# This plot compares songs by acousticness, showing that high acousticness tracks (e.g., acoustic / unplugged) have lower energy, while low acousticness tracks (e.g., electronic) exhibit a broader and higher energy range. It highlights how acoustic characteristics influence a song’s energy.
spotify$acoustic_group <- ifelse(spotify$acousticness > 0.5, "High", "Low")
ggplot(spotify, aes(x = energy, fill = acoustic_group)) +
  geom_density(alpha = 0.5) +
  labs(
    title = "Energy Distribution by Acousticness",
    x = "Energy",
    fill = "Acousticness"
  ) +
  theme(
    axis.text.x = element_text(hjust = 1, size = 14, face = "bold"),  
    axis.text.y = element_text(size = 14, face = "bold"),             
    axis.title.x = element_text(size = 14, face = "bold"),            
    axis.title.y = element_text(size = 14, face = "bold"),            
    plot.title = element_text(hjust = 0.5, size = 16, face = "bold"), 
    legend.title = element_text(size = 14, face = "bold"),            
    legend.text = element_text(size = 14, face = "bold"),             
    panel.grid = element_blank(),                                     
    panel.background = element_blank(),                               
    axis.line = element_line(size = 0.8, color = "black")             
  )


# 2.3. Energy vs. Loudness
# This scatter plot highlights a strong positive correlation between energy and loudness, showing that louder tracks are generally more energetic, reflecting common music production practices to enhance a track's dynamics and appeal.
# Filter out rows with negative energy:
spotify_filtered <- spotify %>% 
  filter(loudness >= -20)
ggplot(spotify_filtered, aes(x = loudness, y = energy)) +
  geom_point(alpha = 0.5, color = "#f23a4d") +
  geom_smooth(method = "lm", color = "blue", size = 2, se = FALSE) + 
  labs(
    title = "Energy vs. Loudness",
    x = "Loudness",
    y = "Energy"
  ) +
  scale_y_continuous(expand = c(0, 0)) +  # Start y-axis at 0
  theme_minimal() +
  theme(
    panel.grid = element_blank(),                           
    axis.line = element_line(size = 1, color = "black"),  
    plot.title = element_text(
      hjust = 0.5, size = 16, face = "bold"                 
    ),
    axis.title.x = element_text(size = 14, face = "bold"),  
    axis.title.y = element_text(size = 14, face = "bold"),  
    axis.text.x = element_text(size = 14, face = "bold"),   
    axis.text.y = element_text(size = 14, face = "bold")    
  )


# 3. Trends Over Time
# How have audio features and song durations changed over time?

# 3.1. Comparison of Metrics Over Time
# This visualization tracks trends in danceability, acousticness, valence, and energy across decades, 
# highlighting how music styles have evolved in complexity and emotion. 
# Ensure release_year is numeric:
spotify$release_year <- as.numeric(as.character(spotify$release_year))
# Aggregate average metrics by Album Release Year:
avg_metrics <- aggregate(
  cbind(danceability, acousticness, liveness, energy) ~ release_year,
  data = spotify,
  FUN = mean,
  na.rm = TRUE
)
# Filter out years before 1940:
avg_metrics <- subset(avg_metrics, release_year >= 1940)
# Convert the data into long format for ggplot:
avg_metrics_long <- reshape2::melt(avg_metrics, id.vars = "release_year",
                                   variable.name = "Metric", value.name = "Average_Value")
# Define a custom color palette for better contrast:
custom_colors <- c("danceability" = "#E41A1C",   
                   "acousticness" = "#377EB8",   
                   "liveness" = "#4DAF4A",       
                   "energy" = "#FFB90F")         
# Plot the comparison:
ggplot(avg_metrics_long, aes(x = release_year, y = Average_Value, color = Metric, group = Metric)) +
  geom_line(size = 1.2) +
  geom_point(size = 2) +
  scale_color_manual(values = custom_colors) + 
  scale_x_continuous(
    breaks = seq(1940, max(avg_metrics$release_year, na.rm = TRUE), by = 10),  
    limits = c(1940, max(avg_metrics$release_year, na.rm = TRUE))             
  ) +
  labs(
    title = "Comparison of Metrics Over Time",
    x = "Year",
    y = "Average Value",
    color = "Metric"
  ) +
  theme_minimal() +
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1, size = 14, face = "bold"),
    axis.text.y = element_text(size = 14, face = "bold"),  
    axis.title.x = element_text(size = 14, face = "bold"),  
    axis.title.y = element_text(size = 14, face = "bold"),  
    plot.title = element_text(hjust = 0.5, size = 18, face = "bold"),
    legend.title = element_text(size = 14, face = "bold"),
    legend.text = element_text(size = 14, face = "bold"),
    panel.grid = element_blank(),  # Remove grid lines
    axis.line.x = element_line(size = 0.8, color = "black"),  
    axis.line.y = element_line(size = 0.8, color = "black")   
  )
# Results:
# The chart shows stable energy levels in recent years, significant fluctuations in acousticness during the 
# mid-20th century, and a gradual upward trend in danceability, reflecting a shift towards rhythm-focused music.


# 3.2. Monthly Streams by Country
# This heatmap tracks monthly streaming activity in the top 10 countries over a two-year period. 
# Extract the month and year from the 'week' column to create a 'month' column:
spotify$month <- format(spotify$week, "%Y-%m")
# Aggregate data to sum up streams for each country and month:
monthly_streams <- aggregate(
  spotify$streams,
  by = list(country = spotify$country, month = spotify$month),
  FUN = sum,
  na.rm = TRUE
)
colnames(monthly_streams) <- c("country", "month", "total_streams")
# Exclude "Global" from the dataset:
monthly_streams <- monthly_streams[monthly_streams$country != "Global", ]
# Identify the top 10 countries by total streams:
total_streams_by_country <- aggregate(
  monthly_streams$total_streams,
  by = list(country = monthly_streams$country),
  FUN = sum
)
colnames(total_streams_by_country) <- c("country", "total_streams")
top_countries <- total_streams_by_country[order(-total_streams_by_country$total_streams), "country"][1:10]
# Filter the dataset for the top 10 countries:
monthly_streams_filtered <- monthly_streams[!is.na(match(monthly_streams$country, top_countries)), ]
# Convert the 'month' column to Date format:
monthly_streams_filtered$month <- as.Date(paste0(monthly_streams_filtered$month, "-01"))
# Remove the specific months 2021-01 and 2022-08:
monthly_streams_filtered <- monthly_streams_filtered[
  !(monthly_streams_filtered$month %in% as.Date(c("2021-01-01", "2022-08-01"))), 
]
# Generate a sequence of months excluding the unwanted ones:
valid_months <- seq(
  from = as.Date("2021-02-01"),
  to = as.Date("2022-07-01"),
  by = "1 month"
)
# Plot the heatmap:
options(scipen = 999)
ggplot(monthly_streams_filtered, aes(x = month, y = country, fill = total_streams)) +
  geom_tile(color = "white") +
  scale_fill_gradient(
    low = "#FFC0CB",   
    high = "red2",     
    name = "Value"
  ) +
  scale_x_date(
    breaks = valid_months,  
    date_labels = "%Y-%m"
  ) +
  labs(
    title = "Monthly Streams by Country",
    x = "Month",
    y = "Country"
  ) +
  theme_minimal() +
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1, size = 14, face = "bold"),  
    axis.text.y = element_text(size = 14, face = "bold"),                         
    axis.title.x = element_text(size = 14, face = "bold"),                       
    axis.title.y = element_text(size = 14, face = "bold"),                        
    plot.title = element_text(hjust = 0.5, size = 16, face = "bold"),             
    legend.title = element_text(size = 14, face = "bold"),                        
    legend.text = element_text(size = 14, face = "bold"),                         
    panel.grid = element_blank(),                                                 
    panel.border = element_blank(),                                               
    axis.line = element_line(size = 0.8, color = "black")                         
  )
# Results:
# Countries like the United States and Brazil dominate streaming activity, particularly during holiday months. 
# Streaming trends are relatively consistent, with slight increases during peak music seasons.


# 3.3. Average Song Duration Over Years
# This line chart tracks changes in the average duration of songs over time. 
# Ensure 'release_year' and 'duration' are numeric:
spotify$release_year <- as.numeric(as.character(spotify$release_year))
spotify$duration <- as.numeric(as.character(spotify$duration))
# Convert duration from milliseconds to minutes:
spotify$duration_minutes <- spotify$duration / 60000
# Aggregate average duration by release year:
avg_duration <- aggregate(
  duration_minutes ~ release_year,
  data = spotify,
  FUN = mean,
  na.rm = TRUE
)
# Filter out years with insufficient data (e.g., before 1940 or extreme outliers):
avg_duration <- subset(avg_duration, release_year >= 1940)
# The plot:
ggplot(avg_duration, aes(x = release_year, y = duration_minutes)) +
  geom_line(color = "#377EB8", size = 1.5) +  
  geom_point(color = "#E41A1C", size = 2) +   
  scale_y_continuous(
    expand = c(0, 0),
    limits = c(0, max(avg_duration$duration_minutes, na.rm = TRUE) * 1.1)  
  ) +
  labs(
    title = "Average Song Duration Over Years",
    x = "Year",
    y = "Average Duration (Minutes)"
  ) +
  theme_minimal() +
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1, size = 14, face = "bold"),  
    axis.text.y = element_text(size = 14, face = "bold"),                          
    axis.title.x = element_text(size = 14, face = "bold"),                         
    axis.title.y = element_text(size = 14, face = "bold"),                        
    plot.title = element_text(hjust = 0.5, size = 18, face = "bold"),              
    panel.grid = element_blank(),                                                 
    axis.line.x = element_line(size = 0.8, color = "black"),                      
    axis.line.y = element_line(size = 0.8, color = "black")                        
  )
# Results:
# Song durations were longer in the 1940s–60s, gradually decreasing in the modern era. 
# Shorter song durations in recent decades reflect changing listener preferences and industry trends, 
# like a focus on concise, attention grabbing tracks.


# 3.4. Top 5 Languages by Song Count
# Ensure the 'language' column is a factor or character:
spotify$language <- as.character(spotify$language)
# Count the number of songs per language:
language_counts <- aggregate(
  uri ~ language, 
  data = spotify, 
  FUN = length
)
colnames(language_counts) <- c("language", "song_count")
# Get the Top 5 Languages by Song Count:
top_languages <- language_counts[order(-language_counts$song_count), ][1:5, ]
# Define custom colors for the top 5 languages:
custom_colors <- c(
  "English" = "#1ED760",    
  "Spanish" = "#377EB8",    
  "Arabic" = "#E41A1C",     
  "German" = "#054A29",     
  "Portuguese" = "#FF7F00"  
)
# Plot the Top 5 Languages by Song Count:
ggplot(top_languages, aes(x = reorder(language, -song_count), y = song_count, fill = language)) +
  geom_bar(stat = "identity", color = "white", width = 0.5) +
  scale_fill_manual(values = custom_colors) +  # Use custom colors for each language
  labs(
    title = "Top 5 Languages by Song Count",
    x = "Language",
    y = "Number of Songs",
    fill = "Language"
  ) +
  theme_minimal() +
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1),
    plot.title = element_text(hjust = 0.5, size = 16, face = "bold"),
    axis.text = element_text(size = 12),
    axis.title = element_text(size = 14),
    legend.position = "none"  # Remove legend for simplicity
  )