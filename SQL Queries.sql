DROP TABLE IF EXISTS spotify_tracks CASCADE;
CREATE TABLE spotify_tracks (
    genre TEXT,
    artist_name TEXT,
    track_name TEXT,
    track_id TEXT PRIMARY KEY,
    popularity INTEGER,
    acousticness FLOAT,
    danceability FLOAT,
	duration_ms INTEGER,
    energy FLOAT,
    instrumentalness FLOAT,
	key_ TEXT,
    liveness FLOAT,
    loudness FLOAT,
	mode_m TEXT,
    speechiness FLOAT,
    tempo FLOAT,
    valence FLOAT,
    date DATE
);

--CATEGORIA 1: Elemente definitorii pt popularitate

--Cele mai populare genuri muzicale
SELECT genre, AVG(popularity) AS average_popularity
FROM spotify_tracks
GROUP BY genre
ORDER BY average_popularity DESC
LIMIT 10;

--Clasificarea pieselor in functie de popularitate-criteriu Super Hit
SELECT track_name, genre, artist_name, popularity,
       CASE 
           WHEN popularity >= 80 THEN 'Super Hit'
           WHEN popularity >= 50 THEN 'Popular'
           ELSE 'Less Known'
       END AS popularity_category
FROM spotify_tracks
WHERE popularity >= 80
ORDER BY popularity DESC;


--Genurile cu cea mai mare creștere în popularitate
WITH pop_growth AS (
    SELECT genre, 
           EXTRACT(YEAR FROM date) AS year, 
           AVG(popularity) AS average_pop
    FROM spotify_tracks
    GROUP BY genre, year
)
SELECT genre, (MAX(average_pop) - MIN(average_pop)) AS pop_increase
FROM pop_growth
WHERE genre NOT LIKE 'Childrenâ%'
GROUP BY genre
ORDER BY pop_increase DESC
LIMIT 10;

--Cele mai populare piese din fiecare gen
WITH popular_track_per_genre AS (
	SELECT genre, track_name, artist_name, popularity,
		RANK() OVER(PARTITION BY genre ORDER BY popularity DESC) AS rank
	FROM spotify_tracks
)
SELECT genre, track_name, artist_name, popularity
FROM popular_track_per_genre
WHERE rank = 1 and genre NOT LIKE 'Childrenâ%';


--Genurile predominante ale artistilor cei mai ascultati
WITH top_artist AS (
    SELECT artist_name, genre, COUNT(track_id) AS nr_songs, 
           AVG(popularity) AS avg_popularity
    FROM spotify_tracks
    GROUP BY artist_name, genre
    HAVING AVG(popularity)>75
)
SELECT genre, COUNT(artist_name) AS nr_top_artists
FROM top_artist
GROUP BY genre
ORDER BY nr_top_artists DESC;


--Cele mai zgomotoase genuri muzicale
SELECT genre, AVG(loudness) AS average_loudness
FROM spotify_tracks
GROUP BY genre
ORDER BY average_loudness DESC;

--CATEGORIA 2: Corelații între parametrii

--Corelatie intre parametrii tempo si energy
SELECT genre, AVG(energy) AS average_energy, AVG(tempo) AS average_tempo
FROM spotify_tracks
GROUP BY genre
ORDER BY average_energy DESC;

SELECT CORR(tempo, energy) AS correlation_tempo_energy
FROM spotify_tracks;

--Corelatie intre parametrii danceability si tempo
SELECT genre, AVG(danceability) AS average_danceability, AVG(tempo) AS average_tempo
FROM spotify_tracks
GROUP BY genre
ORDER BY average_danceability DESC;

--Corelatie intre parametrii valence si danceability
SELECT genre, AVG(valence) AS average_valence, AVG(danceability) AS average_danceability
FROM spotify_tracks
GROUP by genre
ORDER BY average_valence DESC;

--Corelatie intre parametrii loudness si energy
SELECT genre, avg(loudness) AS average_loudness, avg(energy) AS average_energy
FROM spotify_tracks
WHERE genre NOT LIKE 'Childrenâ%'
GROUP BY genre
ORDER BY average_loudness DESC;


--Speechiness vs. genre
SELECT genre, AVG(speechiness) as average_speech
FROM spotify_tracks
GROUP BY genre
ORDER BY average_speech DESC;


--Corelatie intre acousticness si energy
SELECT genre, AVG(acousticness) AS average_acoust, AVG(energy) AS average_energy
FROM spotify_tracks
GROUP BY genre
ORDER BY average_acoust DESC;


--CATEGORIA 3:
--Evolutia tempo-ului mediu de-a lungul anilor
SELECT EXTRACT(YEAR FROM date) AS year, genre, AVG(tempo) AS average_tempo
FROM spotify_tracks
GROUP BY year, genre
ORDER BY year, average_tempo DESC;

--Evolutia unui gen in topul popularitatii pentru fiecare an
SELECT EXTRACT(YEAR FROM date) AS year, genre, AVG(popularity) AS average_popularity,
       PERCENT_RANK() OVER (PARTITION BY EXTRACT(YEAR FROM date) ORDER BY AVG(popularity) DESC) AS genre_popularity_rank
FROM spotify_tracks
GROUP BY year, genre
ORDER BY year, genre_popularity_rank DESC;

--Variația durata pieselor în timp
SELECT EXTRACT(YEAR FROM date) AS year,AVG(duration_ms)/60000 AS average_duration_minutes
FROM spotify_tracks
GROUP BY year
ORDER BY year;


--Evoluția popularității unui gen muzical în timp- ex. Rock
SELECT EXTRACT(YEAR FROM date) AS year, genre, AVG(popularity) AS average_popularity
FROM spotify_tracks
WHERE genre = 'Rock'
GROUP BY year, genre
ORDER BY year;

--Cele mai populare genuri în fiecare deceniu
SELECT 
    (FLOOR(EXTRACT(YEAR FROM date) / 10) * 10) AS decade, 
    genre, 
    AVG(popularity) AS average_popularity
FROM spotify_tracks
WHERE (FLOOR(EXTRACT(YEAR FROM date) / 10) * 10) = 1990 AND genre NOT LIKE 'Childrenâ%'
GROUP BY decade, genre
ORDER BY decade, average_popularity DESC;

--

SELECT genre, AVG(speechiness) AS average_speechiness
FROM spotify_tracks
GROUP BY genre
ORDER BY average_speechiness DESC
LIMIT 10;













