# We load the files:

goals <- read.csv("data/goles_1970_2020.csv", header=TRUE, encoding="UTF-8")
laLiga <- read.csv("data/la-liga.csv", header=TRUE, encoding="UTF-8")

# Filter some seasons (to get the same "first season" in both files):

laLiga <- laLiga[61:968, ]

# View(laLiga)

# Create the object "temporadas"

temporadas <- unique(laLiga$Season)
temporadas

# Rename variables

colnames(laLiga)[3]="team"
colnames(laLiga)[1]="season"
colnames(goals)[2]="team"

# Calculating the points of each team:

laLiga["Points"] <- 3*laLiga["Won"]+laLiga["Draw"]

# Calculating the Winrate (%) of each team:

laLiga$Winrate <- c(0)

for (fila in  1:908){
  laLiga$Winrate[fila] <- round(100*laLiga$Won[fila]/laLiga$Played[fila],2)
}

# Calculating the Loserate (%) of each team:

laLiga$Loserate <- c(0)

for (fila in 1:908){
  laLiga$Loserate[fila] <- round(100*laLiga$Lost[fila]/laLiga$Played[fila],2)
}

# Calculating de Drawrate (%) of each team:

laLiga$Drawrate <- c(0)

for (fila in 1:908){
  laLiga$Drawrate[fila] <- round(100*laLiga$Draw[fila]/laLiga$Played[fila],2)
}

# Calculating year when the season started:

laLiga$year <- c(0)

for (fila in 1:908){
  laLiga$year[fila] <- as.numeric(substr(laLiga$season[fila], 1, 4))
}

# Homogenizing teams names.

laLiga$team <- laLiga$team %>%
  str_replace_all("Elche CF",'Elche')
laLiga$team <- laLiga$team %>%
  str_replace_all("Zaragoza","Real Zaragoza")
laLiga$team <- laLiga$team %>%
  str_replace_all("FC Barcelona","Barcelona")
laLiga$team <- laLiga$team %>%
  str_replace_all("CF Barcelona","Barcelona")
laLiga$team <- laLiga$team %>%
  str_replace_all("Real Gijón","Sporting de Gijon")
laLiga$team <- laLiga$team %>%
  str_replace_all("Sporting Gijón","Sporting de Gijon")
laLiga$team <- laLiga$team %>%
  str_replace_all("Sporting de Gijón","Sporting de Gijon")
laLiga$team <- laLiga$team %>%
  str_replace_all("Sevilla CF","Sevilla")
laLiga$team <- laLiga$team %>%
  str_replace_all("Deportivo de La Coruña","Deportivo")
laLiga$team <- laLiga$team %>%
  str_replace_all("Deportivo de La Coruna","Deportivo")
laLiga$team <- laLiga$team %>%
  str_replace_all("Deportivo La Coruña","Deportivo")
laLiga$team <- laLiga$team %>%
  str_replace_all("Atlético Bilbao","Athletic Club")
laLiga$team <- laLiga$team %>%
  str_replace_all("Athletic Bilbao","Athletic Club")
laLiga$team <- laLiga$team %>%
  str_replace_all("Real Santander","Racing de Santander")
laLiga$team <- laLiga$team %>%
  str_replace_all("Racing Santander","Racing de Santander")
laLiga$team <- laLiga$team %>%
  str_replace_all("Córdoba CF","Cordoba")
laLiga$team <- laLiga$team %>%
  str_replace_all("Córdoba","Cordoba")
laLiga$team <- laLiga$team %>%
  str_replace_all("Almería","Almeria")
laLiga$team <- laLiga$team %>%
  str_replace_all("Alavés","Alaves")
laLiga$team <- laLiga$team %>%
  str_replace_all("CD Logroñés","Logrones")
laLiga$team <- laLiga$team %>%
  str_replace_all("Logroñés","Logrones")
laLiga$team <- laLiga$team %>%
  str_replace_all("Logroñés","Logrones")
laLiga$team <- laLiga$team %>%
  str_replace_all("RCD Español","Espanyol")
laLiga$team <- laLiga$team %>%
  str_replace_all("RCD Espanyol","Espanyol")
laLiga$team <- laLiga$team %>%
  str_replace_all("CD Castellón","Castellon")
laLiga$team <- laLiga$team %>%
  str_replace_all("Castellón","Castellon")
laLiga$team <- laLiga$team %>%
  str_replace_all("CD Málaga","Malaga")
laLiga$team <- laLiga$team %>%
  str_replace_all("Málaga","Malaga")
laLiga$team <- laLiga$team %>%
  str_replace_all("CE Sabadell FC","Sabadell")
goals$team <- goals$team %>%
  str_replace_all("CD Malaga","Malaga")
laLiga$team <- laLiga$team %>%
  str_replace_all("Albacete Balompié","Albacete")
laLiga$team <- laLiga$team %>%
  str_replace_all("Atlético Madrid","Atletico de Madrid")
laLiga$team <- laLiga$team %>%
  str_replace_all("Cádiz CF","Cadiz")
laLiga$team <- laLiga$team %>%
  str_replace_all("Cádiz","Cadiz")
laLiga$team <- laLiga$team %>%
  str_replace_all("Celta Vigo","Celta de Vigo")
laLiga$team <- laLiga$team %>%
  str_replace_all("CD Tenerife","Tenerife")
laLiga$team <- laLiga$team %>%
  str_replace_all("CF Extremadura","Extremadura")
laLiga$team <- laLiga$team %>%
  str_replace_all("CP Mérida","Merida")
laLiga$team <- laLiga$team %>%
  str_replace_all("Mérida","Merida")
laLiga$team <- laLiga$team %>%
  str_replace_all("Gimnàstic","Gimnastic de Tarragona")
laLiga$team <- laLiga$team %>%
  str_replace_all("Granada CF","Granada")
laLiga$team <- laLiga$team %>%
  str_replace_all("Hércules","Hercules")
laLiga$team <- laLiga$team %>%
  str_replace_all("Hercules CF","Hercules")
laLiga$team <- laLiga$team %>%
  str_replace_all("Leganés","Leganes")
laLiga$team <- laLiga$team %>%
  str_replace_all("Mérida","Merida")
laLiga$team <- laLiga$team %>%
  str_replace_all("Real Oviedo","Oviedo")
goals$team <- goals$team %>%
  str_replace_all("Real Oviedo","Oviedo")
laLiga$team <- laLiga$team %>%
  str_replace_all("RCD Mallorca","Mallorca")
laLiga$team <- laLiga$team %>%
  str_replace_all("Real Betis","Betis")
laLiga$team <- laLiga$team %>%
  str_replace_all("Recreativo de Huelva","Recreativo")
goals$team <- goals$team %>%
  str_replace_all("Recreativo de Huelva","Recreativo")
laLiga$team <- laLiga$team %>%
  str_replace_all("Sevilla FC","Sevilla")
laLiga$team <- laLiga$team %>%
  str_replace_all("SD Compostela","Compostela")
laLiga$team <- laLiga$team %>%
  str_replace_all("UD Las Palmas","Las Palmas")
laLiga$team <- laLiga$team %>%
  str_replace_all("UD Salamanca","Salamanca")
laLiga$team <- laLiga$team %>%
  str_replace_all("Valencia CF","Valencia")


# We define a dataset only with the basic stats:

laLiga_basic_params <- laLiga[,c(1,3,26:35)]
# View(laLiga_basic_params)



# For the classification board:

laLiga_season <- c()

## Advanced stats ----

# We join the first dataset to the "goals"'s dataset:

laLiga2 <- laLiga

# Testing that the teams are named in the same way in both datasets:

# sort(unique(laLiga2[,"team"]))
# sort(unique(goals[,"team"]))
# 
# sort(unique(laLiga2[,"team"]))==sort(unique(goals[,"team"]))

laLiga2 <- merge (laLiga2, goals, by = c("season","team"))

# We define a dataset with the advanced stats:

laLiga_advanced_params <- laLiga2[,c(1,2,36:42,45:49)]
#View(laLiga_advanced_params)

# We also calculate the year when the season started each season (point-line chart):

laLiga_advanced_params$year <- c(0)

for (fila in 1:908){
  laLiga_advanced_params$year[fila] <- as.numeric(substr(laLiga_advanced_params$season[fila], 1, 4))
}