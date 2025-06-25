#install.packages("RPostgreSQL")

library(tidyverse)
library(RPostgreSQL)
#install.packages("sqldf")
library(sqldf)
#install.packages("magrittr") 
# package installations are only needed the first time you use it 
#install.packages("dplyr") 
con <- dbConnect(RPostgres::Postgres(), dbname="music_db", user="postgres", host = 'localhost', password="postgres")
tables <- dbGetQuery(con, "select table_name from information_schema.tables where table_schema = 'public'") 
tables
for (i in 1:nrow(tables))
{ temp <- dbGetQuery(con, paste("select * from ", tables[i,1], sep="")) 
assign(tables[i,1], temp) }

library(dplyr)
#install.packages("lubridate")  
library(lubridate)

#CATEGORIA 1: Elemente definitorii pt popularitate

#Cele mai populare genuri muzicale
interogare_1 <- spotify_tracks %>%
  group_by(genre) %>%
  summarize(average_popularity = mean(popularity)) %>%
  arrange(desc(average_popularity))
  slice_head(n = 10)
  select(genre, average_popularity)
  
  
#Clasificarea pieselor in functie de popularitate-criteriu Super Hit
interogare_2 <- spotify_tracks %>%
  mutate(
    popularity_categ = case_when(
      popularity >= 80 ~ "Super Hit",
      popularity >= 50 ~ "Popular",
      TRUE ~ "Less Known"
    )
  )%>%
  filter(popularity >= 80) %>%
  arrange(desc(popularity))
print(interogare2)



#Genurile cu cea mai mare creștere în popularitate
pop_growth <- spotify_tracks %>%
  mutate(year = year(date)) %>%
  group_by(genre, year)%>%
  summarize(average_popularity = mean(popularity)) %>%
  select(genre,year, average_popularity)

interogare_3 <- pop_growth %>%
  group_by(genre) %>%
  summarize(
    max_average_pop = max(average_popularity),
    min_average_pop = min(average_popularity)
  ) %>%
  mutate(diff =  max_average_pop - min_average_pop)%>%
  arrange(desc(diff)) %>%
  select(genre, diff)%>%
  filter(!str_starts(genre, "Children"))%>%
  slice_head(n = 10) 




#Cele mai populare piese din fiecare gen
popular_track_per_genre <- spotify_tracks %>%
  group_by(genre) %>%
  mutate(rank = dense_rank(desc(popularity))) %>%  
  ungroup()

popular_track_per_genre %>%
  filter(rank == 1) %>%  
  select(genre, track_name, artist_name, popularity)
  


#Genurile predominante ale artistilor cei mai ascultati
top_artist <- spotify_tracks %>%
  group_by(artist_name, genre) %>%
  summarize(
    number = n(),  
    average_popularity = mean(popularity, na.rm = TRUE),  
    .groups = "drop"
  ) %>%
  filter(average_popularity > 75)
top_artist %>%
  group_by(genre) %>%
  summarize(number_artists = n(), .groups = "drop") %>%
  arrange(desc(number_artists))


#Categoria 2: Corelații între parametrii

#Corelație între tempo și energy
tempo_energy<- spotify_tracks %>%
  group_by(genre) %>%
  summarize(
    average_energy = mean(energy),
    average_tempo = mean(tempo)
  ) %>%
  arrange(desc(average_energy)) 

print(tempo_energy)

#VIZUALIZARE DATE: Corelația dintre tempo și energy
ggplot(tempo_energy, aes(x = average_tempo, y = average_energy)) +
  geom_point(aes(color = average_energy), size = 4, alpha = 0.7) +
  scale_color_viridis_c(option = "C") +
  theme_minimal() +
  labs(title = "Corelația dintre Tempo și Energy",
       x = "Media Tempo (BPM)",
       y = "Media Energiei",
       color = "Energy Level") +
  theme(plot.title = element_text(hjust = 0.5))





#Corelație între danceability și tempo
dance_tempo<- spotify_tracks %>%
  filter(!grepl("^Childrenâ%", genre))%>%
  group_by(genre) %>%
  summarize(
    average_danceability = mean(danceability),
    average_tempo = mean(tempo)
  ) %>%
  arrange(desc(average_danceability))

print(dance_tempo)
library(ggplot2)
ggplot(dance_tempo, aes(x = average_tempo, y = average_danceability, label = genre)) +
  geom_point(color = "blue", size = 3) +
  geom_text(vjust = -1, hjust = 0.5, size = 4) +  # Adăugăm label-uri pentru genuri
  labs(title = "Relația dintre Danceability și Tempo pe genuri",
       x = "Tempo mediu (BPM)",
       y = "Danceability mediu") +
  theme_minimal()

ggplot(dance_tempo, aes(x = reorder(genre, average_danceability), y = average_danceability)) +
  geom_bar(stat = "identity", fill = "steelblue") +
  coord_flip() +
  labs(title = "Danceability pe genuri muzicale", 
       x = "Gen muzical", 
       y = "Danceability mediu") +
  theme_minimal()

#Corelație între valence și danceability
val_dance <- spotify_tracks %>%
  group_by(genre) %>%
  summarize(
    average_valence = mean(valence),
    average_danceability = mean(danceability)
  ) %>%
  arrange(desc(average_valence))
print(val_dance)
  
#Corelație între loudness și energy
loud_energy <- spotify_tracks %>%
  group_by(genre) %>%
  summarize(
    average_loudness = mean(loudness),
    average_energy = mean(energy)
  ) %>%
  arrange(desc(average_loudness))
print(loud_energy)


#Corelație între speechiness și genre
speech_genre<- spotify_tracks %>%
  group_by(genre) %>%
  summarize(
    average_speech = mean(speechiness),
  ) %>%
  arrange(desc(average_speech))
print(speech_genre)


#Corelație între acousticness și energy
acoust_energy<- spotify_tracks %>%
  group_by(genre) %>%
  summarize(
    average_acoust = mean(acousticness),
    average_energy = mean(energy)
  ) %>%
  arrange(desc(average_acoust))
print(acoust_energy)


#CATEGORIA 3
#Evoluția tempo-ului mediu de-a lungul anilor
evolutie_tempo<- spotify_tracks %>%
  mutate(year = year(date)) %>%
  group_by(year, genre) %>%
  summarize(average_tempo = mean(tempo)) %>%
  arrange(year, desc(average_tempo))
print(evolutie_tempo)

#Evoluția unui gen în topul popularității pentru fiecare an??????????
top_an<- spotify_tracks %>%
  mutate(year = year(date)) %>%
  group_by(year, genre) %>%
  summarize(average_popularity = mean(popularity, na.rm = TRUE), .groups = "drop") %>%
  group_by(year) %>%
  mutate(genre_pop_rank = percent_rank(average_popularity)) %>%
  arrange(year, desc(genre_pop_rank))
print(top_an)

#Variația duratelor pieselor în timp
durata_piese<- spotify_tracks %>%
  mutate(year = year(date)) %>%
  group_by(year) %>%
  summarize(average_dur_min = mean(duration_ms, na.rm = TRUE) / 60000) %>%
  arrange(year)
print(durata_piese)

#Evoluția popularității unui gen muzical în timp -ex. Rock
evolutie_gen<- spotify_tracks %>%
  mutate(year = year(date)) %>%
  group_by(year, genre) %>%
  summarize(average_popularity = mean(popularity)) %>%
  filter(genre == 'Rock')
print(evolutie_gen)
  
#Cele mai populare genuri în fiecare deceniu
gen_pop<- spotify_tracks %>%
  mutate(decade = floor(year(date) / 10) * 10) %>%  
  filter(decade == 1990, !grepl("^Children", genre)) %>%  
  group_by(decade, genre) %>%
  summarize(average_popularity = mean(popularity, na.rm = TRUE), .groups = "drop") %>%
  arrange(desc(average_popularity))








library(ggplot2)
library(ggthemes)  
library(viridis)  

#Vizualizare date: media energiei per gen
ggplot(tempo_energy, aes(x = reorder(genre, average_energy), y = average_energy, fill = average_energy)) +
  geom_col() +
  coord_flip() +  
  scale_fill_viridis_c(option = "C") +
  theme_minimal() +
  labs(title = "Media energiei per gen",
       x = "Gen",
       y = "Media Energiei",
       fill = "Energy Level") +
  theme(plot.title = element_text(hjust = 0.5))





### Instalarea și încărcarea bibliotecilor necesare
install.packages(c("ggplot2", "ggstatsplot", "dplyr", "tidyverse", "tidyr"))
library(ggplot2)
library(ggstatsplot)
library(dplyr)
library(tidyr)



### 1. DISTRIBUȚIA VARIABILELOR MUZICALE
#Histogramă și densitate pentru tempo, loudness, energy, danceability, acousticness, valence
spotify_tracks %>%
  select(tempo, loudness, energy, danceability, acousticness, valence) %>%
  pivot_longer(cols = everything(), names_to = "variable", values_to = "value") %>%
  ggplot(aes(x = value, fill = variable)) +
  geom_histogram(alpha = 0.6, bins = 30, position = "identity") +
  facet_wrap(~ variable, scales = "free") +
  theme_minimal()

### 2. RELAȚII ÎNTRE VARIABILE
#Scatter plot pentru tempo vs. energy
ggplot(spotify_tracks, aes(x = tempo, y = energy)) +
  geom_point(alpha = 0.2) +
  geom_smooth(method = "lm", col = "blue") +
  theme_minimal()

#Scatter plot pentru danceability vs. valence
ggplot(spotify_tracks, aes(x = danceability, y = valence)) +
  geom_point(alpha = 0.5, color = "blue" ) +
  geom_smooth(method = "lm", col = "orange") +
  theme_light()

### 3. TESTE STATISTICE
#Testul Shapiro-Wilk pentru normalitate
shapiro.test (sample(spotify_tracks$tempo,5000))
shapiro.test(sample(spotify_tracks$loudness,5000))

#Corelația între loudness și energy
cor.test(spotify_tracks$loudness, spotify_tracks$energy, method = "spearman")

#Testul Mann-Whitney U pentru diferența între major și minor mode
#compară 2 grupuri independente pe o variabilă continuă
wilcox.test(spotify_tracks$loudness ~ spotify_tracks$mode)

#Ipoteza alternativă este că există o diferență între cele două grupuri
#adică true location shift (schimbarea poziției adevărate a distribuției) nu este egală cu 0
#există o diferență semnificativă în loudness între piesele în major și piesele în minor
#p-value-ul este mult mai mic decât 0.05, putem respinge ipoteza nulă (că nu există diferență între grupuri)

#Testul Kruskal-Wallis pentru danceability între genuri
kruskal.test(danceability ~ genre, data = spotify_tracks)

#df = nr. de grupuri - 1 = 26
#p-value e foarte mic comparativ cu pragul de semnificație 
#genurile muzicale au distribuții diferite ale valorii pentru danceability
# există diferențe semnificative în ceea ce privește danceability între diferitele genuri muzicale
install.packages("FSA")  
library(FSA)

# Run Dunn's test with Bonferroni correction
dunnTest(danceability ~ genre, data = spotify_tracks, method = "bonferroni")


### 4. ASOCIERI ȘI CLASIFICARE
#Testul Chi-pătrat între mode și popularitate ridicată/scăzută
spotify_tracks$popularity_level <- ifelse(spotify_tracks$popularity > median(spotify_tracks$popularity), "High", "Low")
table(spotify_tracks$mode, spotify_tracks$popularity_level)
chisq.test(table(spotify_tracks$mode, spotify_tracks$popularity_level))

#Rezultatul indică o asociere semnificativă între modul pieselor(major/minor) și nivelul lor de popularitate. 
#Piesele în major tind să aibă un nivel mai mare de popularitate (high) comparativ cu piesele în minor, 
#iar această asociere este statistic semnificativă, deoarece p-value-ul este mult mai mic decât nivelul de semnificație de 0.05.


### 5. ANALIZA DURATEI MELODIILOR
#Boxplot pentru durata pieselor per gen
ggplot(spotify_tracks, aes(x = genre, y = duration_ms)) +
  geom_boxplot(fill = "cyan", alpha = 0.6) +
  theme_minimal() +
  coord_flip()

#Trend temporal: cum s-a schimbat durata în timp?
ggplot(spotify_tracks, aes(x = date, y = duration_ms)) +
  geom_point(alpha = 0.3) +
  geom_smooth(method = "lm", col = "purple") +
  theme_minimal()

#Test de mediană pentru durata melodiilor între genuri
kruskal.test(duration_ms ~ genre, data = spotify_tracks)  

#durata pieselor muzicale variază semnificativ între genuri
#se respinge ipoteza nulă





library(ggplot2)
library(dplyr)

dt <- spotify_tracks %>%
  select(energy, loudness, speechiness, liveness, duration_ms, acousticness) %>%
  gather(group, vars)  #datele în format lung


dt$danceability <- spotify_tracks$danceability
dt %>%
  ggplot(aes(x = danceability, y = vars)) +
  geom_point(colour = "darksalmon") +  
  geom_smooth(method = "lm", colour = "dodgerblue4") +
  labs(x = "Danceability", y = "Variables") +
  facet_wrap(~group, scales = "free") +
  theme_bw(14)

install.packages("corrplot")
library(corrplot)
library(dplyr)

#Calcularea corelațiilor Spearman pentru variabilele numerice din 'spotify_tracks'
cor_matrix <- cor(spotify_tracks %>% select_if(is.numeric), method = "spearman")
corrplot(cor_matrix)


install.packages('ggstatsplot') 
library(ggstatsplot)

install.packages("rlang", type = "source")
#Ne interesează să verificăm dacă var popularitate are o distribuție normală
#Ip nulă a testului Shapiro- Wilk este că distribuția e normală. 
shapiro.test(sample(spotify_tracks$popularity, 5000))
#Întrucât p-value - 2.2 * 10 -16, deci mult mai mică decât pragul de semnificație de 0,05, 
#înseamnă că ipoteza nulă este respinsă
#În concluzie, testele în care apare var popularitate vor fi teste neparametrice



#Problema nr. 1: Există vreo asociere între popularitatea pieselor și genul lor?
# var1 = popularitate, nuemrică, fără distr normală, var2 = genre, var nominală cu mai mult de 2 valori dist.
#prin urmare, vom folosi testul neparametric Kruskal-Wallis
#Ip nulă a testului Kruskal-Wallis este că nu există diferențe semnificative de popularitate între genurile muz


ggstatsplot::ggbetweenstats(
  data = spotify_tracks,
  x = genre,
  y = popularity,
  type = 'np'
)


ggstatsplot::ggbetweenstats(
  data = spotify_tracks,
  x = key_,
  y = popularity,
  type = 'np'
)


#Pb.3: Există vreo asociere între popularitate și acousticness/..?
#între popularitate și duration_ms
#Întrucât ambele variabile sunt numerice, și distribuția niciuneia nu este noormală, se va folosi un test de corelație neparametric
#Spearman
#Ip nulă: H0: Nu există corelație între cele 2 variabile (pop nu depinde de durată)

ggstatsplot::ggscatterstats(
  data = spotify_tracks,
  x = duration_ms,
  y = popularity,
  type = 'np'
)
#p-value este 0, înseamnă că ip nulă poate fi respinsă, echivalent cele două variabile sunt corelate
#ro Spearman = mărimea efectului/cât de intensă este corelația dintre cele 2 variabile

library('effectsize')
interpret_phi(0.10)


lm1 <- lm(popularity~duration_ms+danceability+tempo+acousticness+mode_m+speechiness+energy, data=spotify_tracks)

summary(lm1)

ggstatsplot::ggbetweenstats(
  data = spotify_tracks,
  x = mode_m,
  y = popularity,
  type = 'np'
)
interpret_rank_biserial(-0.08)


subset_genres <- spotify_tracks[spotify_tracks$genre %in% c("pop", "rock"), ]
subset_genres <- subset_genres[!is.na(subset_genres$popularity), ]
table(subset_genres$genre)
# Graficul Kruskal-Wallis pe subset
ggstatsplot::ggbetweenstats(
  data = subset_genres,
  x = genre,
  y = popularity,
  type = 'np'
)


ggstatsplot::ggscatterstats(
  data = spotify_tracks,
  x = danceability,
  y = popularity,
  type = "nonparametric"
)


ggstatsplot::ggbetweenstats(
  data = spotify_tracks,
  x = key_,
  y = popularity,
  type = "nonparametric"
)


ggstatsplot::ggscatterstats(
  data = spotify_tracks,
  x = energy,
  y = popularity,
  type = "nonparametric"
)


subset_genres <- spotify_tracks[spotify_tracks$genre %in% c("pop", "rock", "blues", "classical"), ]
subset_genres <- subset_genres[!is.na(subset_genres$popularity), ]

ggstatsplot::ggbetweenstats(
  data = subset_genres,
  x = genre,
  y = popularity,
  type = "nonparametric"
)


library(corrplot)
numeric_cols <- spotify_tracks[, c("popularity", "duration_ms", "danceability", "energy", "tempo", "acousticness", "speechiness", "instrumentalness","valence")]
cor_matrix <- cor(numeric_cols, use = "complete.obs", method = "spearman")
corrplot::corrplot(cor_matrix, method = "color", type = "upper", tl.cex = 0.8)




install.packages("ggstatsplot")  
library(ggstatsplot)
# Crearea datelor ca un data frame
df <- data.frame(
  mode = c(rep("Major", 2), rep("Minor", 2)),
  popularity = rep(c("Ridicată", "Scăzută"), 2),
  count = c(54561, 60214, 31079, 28135)
)
# Transformarea într-un data frame extins (necesar pentru ggbarstats)
expanded_df <- df[rep(1:nrow(df), df$count), 1:2]
# Crearea graficului Chi-pătrat cu ggbarstats
ggbarstats(
  data = expanded_df,
  x = mode,
  y = popularity,
  title = "Asocierea între modul muzical și nivelul de popularitate",
  xlab = "Modul muzical (Major/Minor)",
  ylab = "Nivel de popularitate",
  results.subtitle = TRUE,
  perc.k = 1,
  label = "both", 
  ggtheme = ggplot2::theme_minimal()
)



