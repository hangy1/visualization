---
title: "my digital shadow"
author: "Hang Yin"
date: "2/14/2021"
output: 
  html_document:
    keep_md: yes
    code_folding: hide
---


```r
library(dplyr)
```

```
## 
## Attaching package: 'dplyr'
```

```
## The following objects are masked from 'package:stats':
## 
##     filter, lag
```

```
## The following objects are masked from 'package:base':
## 
##     intersect, setdiff, setequal, union
```

```r
library(jsonlite)
library(lubridate)
```

```
## 
## Attaching package: 'lubridate'
```

```
## The following objects are masked from 'package:base':
## 
##     date, intersect, setdiff, union
```

```r
library(gghighlight)
```

```
## Loading required package: ggplot2
```

```r
library(spotifyr)
library(tidyverse)
```

```
## ── Attaching packages ─────────────────────────────────────── tidyverse 1.3.0 ──
```

```
## ✓ tibble  3.0.5     ✓ purrr   0.3.3
## ✓ tidyr   1.1.2     ✓ stringr 1.4.0
## ✓ readr   1.4.0     ✓ forcats 0.5.0
```

```
## ── Conflicts ────────────────────────────────────────── tidyverse_conflicts() ──
## x lubridate::as.difftime() masks base::as.difftime()
## x lubridate::date()        masks base::date()
## x dplyr::filter()          masks stats::filter()
## x purrr::flatten()         masks jsonlite::flatten()
## x lubridate::intersect()   masks base::intersect()
## x dplyr::lag()             masks stats::lag()
## x lubridate::setdiff()     masks base::setdiff()
## x lubridate::union()       masks base::union()
```

```r
library(knitr)
library(ggplot2)
library(plotly)
```

```
## 
## Attaching package: 'plotly'
```

```
## The following object is masked from 'package:ggplot2':
## 
##     last_plot
```

```
## The following object is masked from 'package:stats':
## 
##     filter
```

```
## The following object is masked from 'package:graphics':
## 
##     layout
```

```r
library(fmsb)
library(factoextra)
```

```
## Welcome! Want to learn more? See two factoextra-related books at https://goo.gl/ve3WBa
```

```r
library(RColorBrewer)
library("GISTools")
```

```
## Loading required package: maptools
```

```
## Loading required package: sp
```

```
## Checking rgeos availability: TRUE
```

```
## Loading required package: MASS
```

```
## 
## Attaching package: 'MASS'
```

```
## The following object is masked from 'package:plotly':
## 
##     select
```

```
## The following object is masked from 'package:dplyr':
## 
##     select
```

```
## Loading required package: rgeos
```

```
## rgeos version: 0.5-5, (SVN revision 640)
##  GEOS runtime version: 3.7.2-CAPI-1.11.2 
##  Linking to sp version: 1.4-2 
##  Polygon checking: TRUE
```

```r
library(tm)
```

```
## Loading required package: NLP
```

```
## 
## Attaching package: 'NLP'
```

```
## The following object is masked from 'package:ggplot2':
## 
##     annotate
```

```r
library(SnowballC)
library("wordcloud")
library(readr)
library(dplyr)
library(e1071)
library(mlbench)
library(makeFlow)

# Connecting spotify API
client_id <- "9161e59c250e4e5eb9704349f3ae353b"
sec_id <- "55cfbe3dcc8f40739bb622a8e3a015f0"
Sys.setenv(SPOTIFY_CLIENT_ID = '9161e59c250e4e5eb9704349f3ae353b')
Sys.setenv(SPOTIFY_CLIENT_SECRET = '55cfbe3dcc8f40739bb622a8e3a015f0')
get_spotify_authorization_code()
spotifyr::get_spotify_access_token()
token <- "BQBEayfPdUYTSGwPacT9lYJHOe-KtbhVKux7OiBUnrZBJB3pclAfvgasi-VsAoI2bQZnZ2rsFs7ejT-2m4I"
```


## Personal Listening History

```r
## total played time(hour) and total artist 
stream_df <- fromJSON("MyData/StreamingHistory0.json", flatten = TRUE)
total_time <- sum(stream_df$msPlayed)/1000/3600 
#366h
total_artist <- length(unique(stream_df$artistName)) 
#724
total_track <- length(stream_df$trackName)  
#8689
unique_track <- length(unique(stream_df$trackName)) 
#2086
track_repeat <- table(stream_df$trackName)
head(sort(table(stream_df$trackName), decreasing = T), n = 10)
```

```
## 
##                                      I Want Your Attention 
##                                                         77 
##                                                       Easy 
##                                                         76 
##                                             Move All Night 
##                                                         72 
##                 You Might Be (feat. Lils) - GoldFish Remix 
##                                                         66 
##                                 Back To You (feat. Kiiara) 
##                                                         63 
##                                           Beat Of My Heart 
##                                                         63 
## 4 Real (feat. Ty Dolla $ign & iLoveMakonnen) - Drezo Remix 
##                                                         59 
##                                                Electrified 
##                                                         59 
##                                                  Feel Love 
##                                                         59 
##                                                  Back To U 
##                                                         56
```


```r
stream_df <-  stream_df %>% 
  filter(endTime < "2021-01-01") %>% 
  mutate(date_time = ymd_hm(endTime, tz = NULL), endTime = NULL) 

stream_df$weekdays <- weekdays(stream_df$date_time)

stream_df <-  stream_df %>%
  mutate(months = month.abb[month(stream_df$date_time)])

breaks <- hour(hm("00:00", "6:00", "12:00", "18:00", "23:59"))
labels <- c("Midnight", "Morning", "Afternoon", "Evening")
stream_df$time_of_day <- cut(x=hour(stream_df$date_time), breaks = breaks, labels = labels, include.lowest=TRUE)

month_max <-stream_df %>%
  group_by(months) %>%
  count(trackName) %>% 
  top_n(1) %>% 
  distinct(months,.keep_all = TRUE) %>%
  arrange(match(months, month.abb))
```

```
## Selecting by n
```

```r
month_time <- stream_df %>%
  group_by(months, time_of_day) %>%
  summarise(time_h = sum(msPlayed)/1000/60/60)
```

```
## `summarise()` has grouped output by 'months'. You can override using the `.groups` argument.
```

```r
weekdays_play <- stream_df %>% 
  group_by(weekdays) %>%
  summarize(hours = sum(msPlayed) / 3600000) %>% 
  arrange(weekdays)
stream_df$quarter <- quarter(stream_df$date_time)
weekdays_play <- stream_df %>% 
  group_by(weekdays, quarter) %>%
  summarize(hours = sum(msPlayed) / 3600000) 
```

```
## `summarise()` has grouped output by 'weekdays'. You can override using the `.groups` argument.
```

```r
view(weekdays_play)

weekdays_play$weekdays <- factor(weekdays_play$weekdays, levels= c("Sunday", "Monday", 
                                         "Tuesday", "Wednesday", "Thursday", "Friday", "Saturday"))
weekdays_play <- weekdays_play[order(weekdays_play$weekdays),]
```

## Plots

![](my_digital_shadow_Yin_files/figure-html/unnamed-chunk-4-1.png)<!-- -->![](my_digital_shadow_Yin_files/figure-html/unnamed-chunk-4-2.png)<!-- -->![](my_digital_shadow_Yin_files/figure-html/unnamed-chunk-4-3.png)<!-- -->

##  Spotify API

```r
# getting my top genre
 artist_genre <-  get_my_top_artists_or_tracks(type = 'artists', time_range = 'long_term', limit = 50) 
 artist_genre <- artist_genre %>%
    rowwise %>% 
    mutate(genres = paste(genres, collapse = ', ')) 
```


```r
# create texting plot 
corpus = Corpus(VectorSource(artist_genre$genres))

#Removing Punctuation
corpus = tm_map(corpus, removePunctuation)
```

```
## Warning in tm_map.SimpleCorpus(corpus, removePunctuation): transformation drops
## documents
```

```r
for (i in c(1:50)) {
  print(corpus[[i]][1])
}
```

```
## $content
## [1] "edm electropop melodic dubstep pop pop dance tropical house"
## 
## $content
## [1] "edm electro house electronic trap electropop future bass pop dance pop edm vapor twitch"
## 
## $content
## [1] "bass trap edm electro house electronic trap pop dance"
## 
## $content
## [1] "belgian edm edm pop pop dance tropical house"
## 
## $content
## [1] "chillwave electropop future bass gauze pop indie dream pop vapor soul vapor twitch"
## 
## $content
## [1] "dubstep edm electro house future bass melodic dubstep pop dance pop edm progressive trance"
## 
## $content
## [1] "chillwave future bass indie dream pop vapor soul vapor twitch"
## 
## $content
## [1] "brostep canadian electronic edm electro house electronic trap"
## 
## $content
## [1] "canadian electronic edm electro house electronic trap electropop future bass indie electropop pop dance pop edm vapor twitch"
## 
## $content
## [1] "asian american hip hop chill rb pop"
## 
## $content
## [1] "chillwave ninja tropical house"
## 
## $content
## [1] "canadian electronic dark clubbing edm electra electro house electronic trap"
## 
## $content
## [1] "kpop kpop boy group"
## 
## $content
## [1] "dance pop edm electro house pop dance tropical house"
## 
## $content
## [1] "indonesian hip hop"
## 
## $content
## [1] "deep tropical house edm electropop house indie poptimism nu disco pop dance pop edm tropical house vapor soul vapor twitch"
## 
## $content
## [1] "chinese hip hop"
## 
## $content
## [1] "brostep edm pop pop dance progressive electro house"
## 
## $content
## [1] "dance pop edm electro house pop pop dance pop edm tropical house"
## 
## $content
## [1] "deep house edm filter house house indie soul nu disco"
## 
## $content
## [1] "dance pop edm electro house pop pop dance tropical house"
## 
## $content
## [1] "brostep dubstep edm electro house electronic trap"
## 
## $content
## [1] "art pop pop"
## 
## $content
## [1] "brostep canadian electronic dubstep edm electro house electronic trap pop dance progressive electro house vapor twitch"
## 
## $content
## [1] "edm electro house house pop dance progressive house tropical house vocal house"
## 
## $content
## [1] "bass house dark clubbing electro house"
## 
## $content
## [1] "edm melodic dubstep pop dance pop edm traprun"
## 
## $content
## [1] "edm electronic trap electropop jamtronica jazztronica livetronica"
## 
## $content
## [1] "kpop"
## 
## $content
## [1] "brostep complextro edm electro house future bass moombahton pop dance progressive electro house"
## 
## $content
## [1] "dubstep edm electro house electronic trap pop dance pop edm"
## 
## $content
## [1] "dirty south rap gangster rap hip hop miami hip hop pop rap rap southern hip hop trap"
## 
## $content
## [1] "bass house brostep edm electro house electronic trap house pop dance vapor twitch"
## 
## $content
## [1] "future bass indie soul vapor soul vapor twitch"
## 
## $content
## [1] "edm electro house electronic trap"
## 
## $content
## [1] "hollywood movie tunes"
## 
## $content
## [1] "bass trap edm electronic trap electropop future bass indie electropop pop edm traprun vapor twitch"
## 
## $content
## [1] "dutch trance edm pop dance progressive house trance"
## 
## $content
## [1] "dark trap emo rap underground hip hop vapor trap"
## 
## $content
## [1] "chinese hip hop"
## 
## $content
## [1] "electronic trap electropop future bass pop edm vapor twitch"
## 
## $content
## [1] "brostep dubstep electro house electronic trap gaming dubstep"
## 
## $content
## [1] "bass trap edm electronic trap electropop future bass pop edm vapor soul vapor twitch"
## 
## $content
## [1] "aussietronica bass trap edm electro house electronic trap electropop future bass pop edm vapor soul vapor twitch"
## 
## $content
## [1] "dfw rap melodic rap rap"
## 
## $content
## [1] "comedy"
## 
## $content
## [1] "bass trap brostep classic dubstep edm electro house electronic trap pop dance"
## 
## $content
## [1] "brostep edm electro house electronic trap future bass pop dance"
## 
## $content
## [1] "breakbeat brostep edm electro house electronic trap glitch hop"
## 
## $content
## [1] "brostep canadian electronic dubstep edm electro house electronic trap gaming dubstep"
```

```r
# create frequency matrix
DTM <- TermDocumentMatrix(corpus)
text_matrix <- as.matrix(DTM)
f <- sort(rowSums(text_matrix),decreasing=TRUE)
text_df <- data.frame(word = names(f),freq=f)
text_df
```

```
##                        word freq
## house                 house   46
## pop                     pop   46
## edm                     edm   43
## trap                   trap   28
## electro             electro   26
## electronic       electronic   25
## dance                 dance   23
## bass                   bass   19
## vapor                 vapor   19
## dubstep             dubstep   12
## electropop       electropop   12
## future               future   12
## twitch               twitch   12
## brostep             brostep   11
## tropical           tropical    9
## hop                     hop    9
## soul                   soul    8
## hip                     hip    8
## rap                     rap    8
## indie                 indie    7
## progressive     progressive    6
## canadian           canadian    5
## melodic             melodic    4
## chillwave         chillwave    3
## trance               trance    3
## dark                   dark    3
## kpop                   kpop    3
## dream                 dream    2
## clubbing           clubbing    2
## deep                   deep    2
## disco                 disco    2
## chinese             chinese    2
## traprun             traprun    2
## gaming               gaming    2
## belgian             belgian    1
## gauze                 gauze    1
## american           american    1
## asian                 asian    1
## chill                 chill    1
## ninja                 ninja    1
## electra             electra    1
## boy                     boy    1
## group                 group    1
## indonesian       indonesian    1
## poptimism         poptimism    1
## filter               filter    1
## art                     art    1
## vocal                 vocal    1
## jamtronica       jamtronica    1
## jazztronica     jazztronica    1
## livetronica     livetronica    1
## complextro       complextro    1
## moombahton       moombahton    1
## dirty                 dirty    1
## gangster           gangster    1
## miami                 miami    1
## south                 south    1
## southern           southern    1
## hollywood         hollywood    1
## movie                 movie    1
## tunes                 tunes    1
## dutch                 dutch    1
## emo                     emo    1
## underground     underground    1
## aussietronica aussietronica    1
## dfw                     dfw    1
## comedy               comedy    1
## classic             classic    1
## breakbeat         breakbeat    1
## glitch               glitch    1
```



```r
# Text graph
set.seed(124)
wordcloud(words = text_df$word, freq = text_df$freq, min.freq = 1, random.order=FALSE, rot.per=0.5, colors= brewer.pal(12,"Dark2"))
```

```
## Warning in brewer.pal(12, "Dark2"): n too large, allowed maximum for palette Dark2 is 8
## Returning the palette you asked for with that many colors
```

![](my_digital_shadow_Yin_files/figure-html/unnamed-chunk-7-1.png)<!-- -->


## 12 months/6 months/3 months prefer audio features

```r
# getting tracks id
year_fav <- get_my_top_artists_or_tracks(type = "tracks", limit = 50, offset = 0,
 time_range = "long_term",authorization = get_spotify_authorization_code(),include_meta_info = FALSE)
 
half_year_fav <- get_my_top_artists_or_tracks(type = "tracks", limit = 25, offset = 0,
time_range = "medium_term", authorization = get_spotify_authorization_code(),
 include_meta_info = FALSE)

recent_fav <- get_my_top_artists_or_tracks(type = "tracks", limit = 13, offset = 0,
time_range = "short_term",authorization = get_spotify_authorization_code(),include_meta_info = FALSE)
```



```r
#getting audio features 
year_fav_feature <- get_track_audio_features(year_fav$id, authorization = get_spotify_access_token())
year_fav_feature <- year_fav_feature[, c("danceability", "loudness", "speechiness", "valence","tempo", "acousticness", "energy")]

half_year_feature <- get_track_audio_features(half_year_fav$id, authorization = get_spotify_access_token())[, c("danceability", "loudness", "speechiness", "valence","tempo", "acousticness", "energy")]

recent_feature <- get_track_audio_features(recent_fav$id, authorization = get_spotify_access_token())[, c("danceability", "loudness", "speechiness", "valence","tempo", "acousticness", "energy")]
```



```r
#max min normalization 
normalize <- function(x) {
  return ((x - min(x)) / (max(x) - min(x)))
}

combined_fav <- rbind(year_fav_feature, half_year_feature, recent_feature)
combined_fav_n <- as.data.frame(lapply(combined_fav, normalize)) 
year_fav_mean <- colMeans(combined_fav_n[1:50,])
half_year_fav_n <- colMeans(combined_fav_n[51:75,])
recent_fav_n <- colMeans(combined_fav_n[76:88,])

combined_fav_feature <- data.frame(rbind(year_fav_mean,half_year_fav_n,recent_fav_n))
color = brewer.pal(3,"Paired")
radarchart(combined_fav_feature,axistype=0 ,maxmin=F,
           pcol=color , plwd=2 , plty=1,
           cglcol="grey", cglty=3, axislabcol="grey", cglwd=0.8,vlcex=0.8 )
legend(x=-1.4, y=1.3, legend = c("12 months", "6 months", "3 months"), bty = "n", pch=20 , col=color , text.col = "black",cex=0.8, pt.cex=0.8, y.intersp = 0.8)
```

![](my_digital_shadow_Yin_files/figure-html/unnamed-chunk-10-1.png)<!-- -->

```r
#title = "Audio feature preference for the past year"
```


```r
playlist1_name <- "chill"
playlist1_uris  <- "4lirpnD9acutQGn1sI8ynJ"
playlist1_features <- get_playlist_audio_features(playlist1_name, playlist1_uris,authorization = get_spotify_access_token())[, c("danceability", "loudness", "speechiness", "valence","tempo", "acousticness", "energy")]
playlist1_features <- data.frame(playlist1_features)
rownames(playlist1_features) <- c(paste ("chill", c(1:48)))
```



```r
#playlist 2
playlist2_name <- "house&techno"
playlist2_uris  <- "1gw8sDn1VF96t1B7QOfPOF"
playlist2_features <- get_playlist_audio_features(playlist2_name, playlist2_uris,authorization = get_spotify_access_token())[, c("danceability", "loudness", "speechiness", "valence","tempo", "acousticness", "energy")]
playlist2_features <- data.frame(playlist2_features)
rownames(playlist2_features) <- c(paste ("H&T", c(1:32)))
```


```r
#playlist 3
playlist3_name <- "hip hop"
playlist3_uris <- "0DsnqxDaX9wTarPom1yGSc"
playlist3_features <- get_playlist_audio_features(playlist3_name, playlist3_uris,authorization = get_spotify_access_token())[, c("danceability", "loudness", "speechiness", "valence","tempo", "acousticness", "energy")]
playlist3_features <- data.frame(playlist3_features)
rownames(playlist3_features) <- c(paste ("hiphop", c(1:23)))
```


```r
#normalize playlists data

combined_full <- as.data.frame(rbind(playlist1_features, playlist2_features, playlist3_features))

combined_full_n <- data.frame(lapply(combined_full,normalize)) %>% `row.names<-`(rownames(combined_full))


chill_mean <- colMeans(combined_full_n[1:49,])

HouseTech_mean <- colMeans(combined_full_n[50:81,])

HipHop_mean <- colMeans(combined_full_n[-23,])

combined_mean <- data.frame(rbind(chill_mean,HouseTech_mean,HipHop_mean ))
```



```r
get_track_audio_features(c("0YDSEyBzPsuQzkuc9Bu8iR", "63PjUXm7NNcqm5OCyRMbr5"))[,c("danceability", "loudness", "speechiness", "valence","tempo", "acousticness", "energy")]
```

```
## # A tibble: 2 x 7
##   danceability loudness speechiness valence tempo acousticness energy
##          <dbl>    <dbl>       <dbl>   <dbl> <dbl>        <dbl>  <dbl>
## 1        0.484    -2.93      0.0408   0.259 150.        0.0534  0.938
## 2        0.547    -6.08      0.0339   0.228  97.5       0.0128  0.566
```


## Radar plot

```r
rownames(combined_mean) <- c("Chill", "House Techno", "Hip Hop")
color_4 <- brewer.pal(4,"Paired")
color4_fill <- add.alpha(color_4,0.3 ) 
radarchart(combined_mean,axistype=0 ,maxmin=F, pcol = color_4, pfcol = color4_fill,plwd=3 , plty=1,
           cglcol="grey", cglty=3, axislabcol="grey", cglwd=0.8,vlcex=0.8 )
legend(x=-1.4, y=1.3, legend = rownames(combined_mean),bty = "n", pch=20,col=color_4,text.col = "black",cex=0.8, pt.cex=0.8)
```

![](my_digital_shadow_Yin_files/figure-html/unnamed-chunk-16-1.png)<!-- -->

```r
#title = "My Playlists Audio Features")
```



```r
#recently liked music list
liked_song <- get_my_saved_tracks(limit =20 )
recent_liked_feat <-  get_track_audio_features(liked_song$track.id, authorization = get_spotify_access_token())[, c("danceability", "loudness", "speechiness", "valence","tempo", "acousticness", "energy")]
recent_liked_feat <- data.frame(recent_liked_feat)
rownames(recent_liked_feat) <- c(paste ("recent_liked", c(1:20)))

recent_n <- as.data.frame(lapply(rbind(recent_liked_feat,combined_full),normalize))

recent_liked_mean <- colMeans(recent_n[1:20,])
recent_combined <- rbind(combined_mean,recent_liked_mean)
rownames(recent_combined) <- c("Chill", "House Techno", "Hip Hop","Recently Liked")

color_4 <- brewer.pal(4,"Paired")
color4_fill <- add.alpha(color_4,0.3 ) 
radarchart(recent_combined, axistype=0 ,maxmin=F, pcol = color_4,plwd=2,   plty=1,cglcol="grey", cglty=3, axislabcol="grey", cglwd=0.8,vlcex=0.8)

legend(x=-1.6, y=1.35, legend = rownames(recent_combined),bty = "n", pch=20,col=color_4,text.col = "black",cex=0.8, pt.cex=0.8,y.intersp = 0.8)
```

![](my_digital_shadow_Yin_files/figure-html/unnamed-chunk-17-1.png)<!-- -->



```r
#weekly discover
playlist_name <- "weekly discovery"
playlist_uris  <- "37i9dQZEVXcUXurUueO6bb"
playlist_disc_features <- get_playlist_audio_features(playlist_name, playlist_uris,authorization = get_spotify_access_token())[1:20, c("danceability", "loudness", "speechiness", "valence","tempo", "acousticness", "energy")]
playlist_disc_features <- data.frame(playlist_disc_features)
rownames(playlist_disc_features) <- c(paste ("discover", c(1:20)))
disc_liked <- rbind(playlist_disc_features, combined_full)
disc_liked_n <- data.frame(lapply(disc_liked,normalize)) %>% `row.names<-`(rownames(disc_liked))

disc_mean <- colMeans(disc_liked_n[1:20,])

disc_combined <- rbind(combined_mean,disc_mean)
rownames(disc_combined) <- c("Chill", "House Techno", "Hip Hop","Discover")

color_4 <- brewer.pal(4,"Paired")
color4_fill <- add.alpha(color_4,0.3 )
radarchart(disc_combined, axistype=0 ,maxmin=F, pcol = color_4, plwd=2 , plty=1,cglcol="grey", cglty=3, axislabcol="grey", cglwd=0.8,vlcex=0.8 )

legend(x= -1.6, y=1.3, legend = rownames(disc_combined),bty = "n", pch=20,col=color_4,text.col = "black",cex=0.8, pt.cex=0.8,y.intersp = 0.8)
```

![](my_digital_shadow_Yin_files/figure-html/unnamed-chunk-18-1.png)<!-- -->

```r
#title = "My Playlists Audio Features")
```


## Conclusion: 
#The timeline analysis matches my daily schedule each period: quarantine,summer break,  work 9-5 onsite, work 9-5 remote.The audio feature analysis matches for my playlists matches the genre style, and also I discovered the spotify weekly discover is based on my long-term music preference. 
# I encountered the difficulty on making clear spider graphs, they need to match my color theme and also easy to read. I don't think I did a great good on that. I also had difficulty connecting my specific listening time to the spotify API, that's why I couldn't analyze my prefer audio features through the day (group by morning, afternoon, night, and midnight).

