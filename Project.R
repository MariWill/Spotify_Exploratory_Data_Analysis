library(ggplot2)
library(dplyr)
spotifydata = read.csv("spotify_data.csv")
head(spotifydata)


spotifydata = subset(spotifydata,released_year %in% c(2022, 2023), select = c(artist_count,artist_name,track_name, released_year,released_day,in_spotify_charts,
                                              in_apple_charts,in_deezer_charts,streams,
                                              in_shazam_charts,bpm,mode,danceability_.,
                                              valence_.,acousticness_.,instrumentalness_.,
                                              liveness_.,speechiness_.) )

spotifydata <- spotifydata %>%
mutate(streams = as.numeric(streams))

summary(spotifydata)


# Plot 1: Number of Tracks Released per Year
ggplot(spotifydata, aes(x=released_year)) + 
  geom_bar(fill="cadetblue", width=0.7) +  
  theme_minimal() + 
  labs(title="Number of Tracks Released per Year", x="Released Year", y="Count of Tracks") +
  theme(axis.text.x = element_text(angle = 90, hjust = 1))  

# Plot 2: Distribution of Musical Modes in Playlist
 ggplot(spotifydata, aes(x=mode)) + 
  geom_bar(fill="cornflowerblue") + 
  theme_minimal() + 
  labs(title="Distribution of Musical Modes in Playlist", x="Musical Mode", y="Count of Tracks")
 
 spotifydata_top10 <- spotifydata %>%
   mutate(streams_in_millions = streams / 1e6) %>%  
   arrange(desc(streams_in_millions)) %>%
   slice(1:10)  
 
 # Plot 3: Top 10 Songs by Streams (in Millions)
 ggplot(spotifydata_top10, aes(x=reorder(track_name, streams_in_millions), y=streams_in_millions)) + 
   geom_bar(stat="identity", fill="dodgerblue") + 
   theme_minimal() + 
   labs(title="Top 10 Songs by Streams (in Millions)", x="Track Name", y="Streams (Millions)") +
   coord_flip() 
 
 # regression model
 
 model <- lm(spotifydata$streams ~ spotifydata$in_spotify_charts + spotifydata$in_apple_charts, data = spotifydata)
 summary(model)
 
 ggplot(spotifydata, aes(x = spotifydata$in_spotify_charts, y = spotifydata$streams)) +
   geom_point() +
   geom_smooth(method = "lm", se = FALSE, color = "darkgreen") +
   labs(title = "Relationship between Spotify Charts and Streams",
        x = "Spotify Charts",
        y = "Streams")
 
 ggplot(spotifydata, aes(x = spotifydata$in_apple_charts, y = spotifydata$streams)) +
   geom_point() +
   geom_smooth(method = "lm", se = FALSE, color = "red") +
   labs(title = "Relationship between Apple Music Charts and Streams",
        x = "Apple Music Charts",
        y = "Streams")

 
