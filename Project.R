#1
spotifydata = read.csv("spotify-2023.csv")
head(spotifydata)

View(spotifydata)

spotifydata = subset(spotifydata, select = -c(artist_count,released_day,in_spotify_charts,
                                              in_apple_charts,in_deezer_charts,streams,
                                              in_shazam_charts,bpm,danceability_.,
                                              valence_.,acousticness_.,instrumentalness_.,
                                              liveness_.,speechiness_.) )

View(spotifydata)

#6
install.packages("ggplot2")
library(ggplot2)
library(dplyr)

ggplot(spotifydata, aes(x=released_year)) + 
  geom_bar(fill="cadetblue", width=0.7) +  
  theme_minimal() + 
  labs(title="Number of Tracks Released per Year", x="Released Year", y="Count of Tracks") +
  theme(axis.text.x = element_text(angle = 90, hjust = 1))  

 ggplot(spotifydata, aes(x=mode)) + 
  geom_bar(fill="cornflowerblue") + 
  theme_minimal() + 
  labs(title="Distribution of Musical Modes in Playlist", x="Musical Mode", y="Count of Tracks")
 
 spotifydata <- spotifydata %>%
   mutate(streams_in_millions = streams / 1e6) %>%  
   arrange(desc(streams_in_millions)) %>%
   slice(1:10)  
 
 ggplot(spotifydata, aes(x=reorder(track_name, streams_in_millions), y=streams_in_millions)) + 
   geom_bar(stat="identity", fill="dodgerblue") + 
   theme_minimal() + 
   labs(title="Top 10 Songs by Streams (in Millions)", x="Track Name", y="Streams (Millions)") +
   coord_flip()  

