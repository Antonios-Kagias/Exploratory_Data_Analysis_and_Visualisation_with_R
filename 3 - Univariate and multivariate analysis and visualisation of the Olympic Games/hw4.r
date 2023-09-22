library(plyr)
library(dplyr)
library(ggplot2)
library(ggridges)
library(tidyverse)
library(plotly)


# Read .csv file
results <- read.csv(file = 'results.csv')

none_results <- results %>%
  filter(Result == "None" & Name != "null") %>%
  select(Event, Year, Name, Medal) %>%
  arrange(Year, Event)
none_results

# ----------------------- FIX SOME OF THE RESULTS -----------------------
results$Result[results$Name == "George ORTON" & results$Event == "400M Hurdles Men" & results$Year == 1900] <- "58.8"
results$Result[results$Name == "Ernst SCHULTZ" & results$Event == "400M Men" & results$Year == 1900] <- "51.5"
results$Result[results$Name == "David HALL" & results$Event == "800M Men" & results$Year == 1900] <- "2:03.8"
results$Result[results$Name == "George POAGE" & results$Event == "400M Hurdles Men" & results$Year == 1904] <- "56.8"
results$Result[results$Name == "John RECTOR" & results$Event == "100M Men" & results$Year == 1908] <- "10.9"
results$Result[results$Name == "Robert KERR" & results$Event == "100M Men" & results$Year == 1908] <- "11.0"
results$Result[results$Name == "Arthur SHAW" & results$Event == "110M Hurdles Men" & results$Year == 1908] <- "15.8"
results$Result[results$Name == "Robert CLOUGHEN" & results$Event == "200M Men" & results$Year == 1908] <- "22.6"
results$Result[results$Name == "Nate CARTMELL" & results$Event == "200M Men" & results$Year == 1908] <- "22.7"
results$Result[results$Name == "Harry HILLMAN" & results$Event == "400M Hurdles Men" & results$Year == 1908] <- "55.3"
results$Result[results$Name == "Leonard Francis TREMEER" & results$Event == "400M Hurdles Men" & results$Year == 1908] <- "57.0"
results$Result[results$Name == "Patrick FLYNN" & results$Event == "3000M Steeplechase Men" & results$Year == 1920] <- "10:21.1"
results$Result[results$Name == "Ernesto AMBROSINI" & results$Event == "3000M Steeplechase Men" & results$Year == 1920] <- "10:32.0"
results$Result[results$Name == "Fanny ROSENFELD" & results$Event == "100M Women" & results$Year == 1928] <- "12.3"
results$Result[results$Name == "Ethel SMITH" & results$Event == "100M Women" & results$Year == 1928] <- "12.3"
results$Result[results$Name == "Kinue HITOMI" & results$Event == "800M Women" & results$Year == 1928] <- "2:17.6"
results$Result[results$Name == "Inga GENTZEL" & results$Event == "800M Women" & results$Year == 1928] <- "2:17.8"
results$Result[results$Name == "Philip EDWARDS" & results$Event == "1500M Men" & results$Year == 1932] <- "3:52.8"
results$Result[results$Name == "Philip EDWARDS" & results$Event == "800M Men" & results$Year == 1932] <- "1:51.5"
results$Result[results$Name == "Ruth BEITIA" & results$Event == "High Jump Women" & results$Year == 2016] <- 1.97
results$Result[results$Name == "Mirela DEMIREVA" & results$Event == "High Jump Women" & results$Year == 2016] <- 1.97
results$Result[results$Name == "Blanka VLASIC" & results$Event == "High Jump Women" & results$Year == 2016] <- 1.97
results$Result[results$Name == "Thomas ROHLER" & results$Event == "Javelin Throw Men" & results$Year == 2016] <- 90.3
results$Result[results$Name == "Julius YEGO" & results$Event == "Javelin Throw Men" & results$Year == 2016] <- 88.24
results$Result[results$Name == "Keshorn WALCOTT" & results$Event == "Javelin Throw Men" & results$Year == 2016] <- 85.38


# ----------------------- NULL NAMES BECAUSE OF TEAM EVENT, e.g. 4X100M Relay -----------------------
null_names <- results %>%
  filter(Result == "None" & Name == "null") %>%
  select(Event, Year, Name, Nationality, Medal, Result) %>%
  arrange(Year, Event)
null_names

results$Result[results$Event == "4X400M Relay Men" & results$Year == 2016
               & results$Medal == "G"] <- "2:57.30"
results$Result[results$Event == "4X400M Relay Men" & results$Year == 2016
               & results$Medal == "S"] <- "2:58.16"
results$Result[results$Event == "4X400M Relay Men" & results$Year == 2016
               & results$Medal == "B"] <- "2:58.49"
results$Result[results$Event == "4X400M Relay Women" & results$Year == 2016
               & results$Medal == "G"] <- "3:19.06"
results$Result[results$Event == "4X400M Relay Women" & results$Year == 2016
               & results$Medal == "S"] <- "3:20.34"
results$Result[results$Event == "4X400M Relay Women" & results$Year == 2016
               & results$Medal == "B"] <- "3:25.88"
results$Result[results$Event == "4X100M Relay Men" & results$Year == 1928
               & results$Medal == "S"] <- "41.200"
results$Result[results$Event == "4X100M Relay Men" & results$Year == 1928
               & results$Medal == "B"] <- "41.800"
results$Result[results$Event == "4X100M Relay Women" & results$Year == 1928
               & results$Medal == "S"] <- "48.800"
results$Result[results$Event == "4X100M Relay Women" & results$Year == 1928
               & results$Medal == "B"] <- "49.000"
results$Result[results$Event == "4X400M Relay Men" & results$Year == 1928
               & results$Medal == "S"] <- "3:14.80"
results$Result[results$Event == "4X400M Relay Men" & results$Year == 1928
               & results$Medal == "B"] <- "3:15.40"
results$Result[results$Event == "4X400M Relay Men" & results$Year == 1908
               & results$Medal == "S"] <- "3:32.40"
results$Result[results$Event == "4X400M Relay Men" & results$Year == 1908
               & results$Medal == "B"] <- "3:32.50"
results$Result[results$Event == "4X100M Relay Men" & results$Year == 1912
               & results$Medal == "G"] <- "42.4"
results$Result[results$Event == "4X100M Relay Men" & results$Year == 1920
               & results$Medal == "S"] <- "42.6"
results$Result[results$Event == "4X100M Relay Men" & results$Year == 1912
               & results$Medal == "S"] <- "42.6"
results$Result[results$Event == "4X100M Relay Men" & results$Year == 1920
               & results$Medal == "B"] <- "42.9"
results$Result[results$Event == "400M Men" & results$Year == 1912
               & results$Medal == "G"] <- "48.2"
results$Result[results$Event == "400M Men" & results$Year == 1912
               & results$Medal == "S"] <- "48.3"
results$Result[results$Event == "400M Men" & results$Year == 1912
               & results$Medal == "B"] <- "48.4"
results$Result[results$Event == "400M Men" & results$Year == 1920
               & results$Medal == "G"] <- "49.6"
results$Result[results$Event == "400M Men" & results$Year == 1920
               & results$Medal == "S"] <- "49.9"
results$Result[results$Event == "400M Men" & results$Year == 1920
               & results$Medal == "B"] <- "50.0"
results$Result[results$Event == "400M Hurdles Men" & results$Year == 1920
               & results$Medal == "G"] <- "54.0"

null_names <- results %>%
  filter(Result == "None" & Name == "null") %>%
  select(Event, Year, Name, Nationality, Medal, Result) %>%
  arrange(Year, Event)
null_names # ----> 0 rows


# ----------------------- NOW DROP ROWS WE CAN'T FIND DATA FOR AND ROWS THAT ARE USELESS -----------------------

na_year <- results %>%
  filter(is.na(Year)) %>%
  select(Event, Year, Name, Medal) %>%
  arrange(Year, Event)
view(na_year) # -------------> 12 rows

none_results <- results %>%
  filter(Result == "None") %>%
  select(Event, Year, Name, Medal) %>%
  arrange(Year, Event)
view(none_results) # -------------> 4 rows

# 16/2406 = 0.66% of our data, we drop these rows

final_results = subset(results, !is.na(results$Year) & results$Result != "None")
view(final_results)

# ----------------------- CHECK VALIDITY OF CERTAIN DATA THAT HAVE FIXED VALUES -----------------------
final_results %>%
  distinct(Gender) # -------------> 2 rows, OK
final_results %>%
  distinct(Medal) # -------------> 3 rows, OK

# ----------------------- SPLIT RESULTS BASED ON EVENT -----------------------
X<-split(final_results, final_results$Event)


# ----------------------- FIX FORMATTING IN RESULTS -----------------------
X$`20Km Race Walk Men`$Result <- gsub("h", ":", X$`20Km Race Walk Men`$Result)
X$`20Km Race Walk Men`$Result <- gsub("^0+", "", X$`20Km Race Walk Men`$Result)
X$`20Km Race Walk Women`$Result <- gsub("h", ":", X$`20Km Race Walk Women`$Result)
X$`20Km Race Walk Women`$Result <- gsub("^0+", "", X$`20Km Race Walk Women`$Result)
X$`4X100M Relay Men`$Result <- gsub("^0:+", "", X$`4X100M Relay Men`$Result)
X$`4X400M Relay Men`$Result <- gsub("^0+", "", X$`4X400M Relay Men`$Result)
X$`4X400M Relay Men`$Result <- gsub(" est", "", X$`4X400M Relay Men`$Result)
X$`4X400M Relay Women`$Result <- gsub("^0+", "", X$`4X400M Relay Women`$Result)
X$`50Km Race Walk Men`$Result <- gsub("h", ":", X$`50Km Race Walk Men`$Result)
X$`50Km Race Walk Men`$Result <- gsub("^0+", "", X$`50Km Race Walk Men`$Result)
X$`800M Men`$Result <- gsub("^0+", "", X$`800M Men`$Result)
X$`800M Women`$Result <- gsub("^0+", "", X$`800M Women`$Result)
X$`Decathlon Men`$Result <- gsub("+P.$", "", X$`Decathlon Men`$Result)
X$`Heptathlon Women`$Result <- gsub("+P.$", "", X$`Heptathlon Women`$Result)
X$`Marathon Men`$Result <- gsub("h", ":", X$`Marathon Men`$Result)
X$`Marathon Men`$Result <- gsub("-", ":", X$`Marathon Men`$Result)
X$`Marathon Men`$Result <- gsub("^0+", "", X$`Marathon Men`$Result)
X$`Marathon Women`$Result <- gsub("h", ":", X$`Marathon Women`$Result)
X$`Marathon Women`$Result <- gsub("-", ":", X$`Marathon Women`$Result)
X$`Marathon Women`$Result <- gsub("^0+", "", X$`Marathon Women`$Result)


# ----------------------- CONVERT TO DOUBLE CERTAIN RESULTS -----------------------
X$`Decathlon Men`$Result <- as.double(X$`Decathlon Men`$Result)
X$`Discus Throw Men`$Result <- as.double(X$`Discus Throw Men`$Result)
X$`Discus Throw Women`$Result <- as.double(X$`Discus Throw Women`$Result)
X$`Hammer Throw Men`$Result <- as.double(X$`Hammer Throw Men`$Result)
X$`Hammer Throw Women`$Result <- as.double(X$`Hammer Throw Women`$Result)
X$`Heptathlon Women`$Result <- as.double(X$`Heptathlon Women`$Result)
X$`High Jump Men`$Result <- as.double(X$`High Jump Men`$Result)
X$`High Jump Women`$Result <- as.double(X$`High Jump Women`$Result)
X$`Javelin Throw Men`$Result <- as.double(X$`Javelin Throw Men`$Result)
X$`Javelin Throw Women`$Result <- as.double(X$`Javelin Throw Women`$Result)
X$`Long Jump Men`$Result <- as.double(X$`Long Jump Men`$Result)
X$`Long Jump Women`$Result <- as.double(X$`Long Jump Women`$Result)
X$`Pole Vault Men`$Result <- as.double(X$`Pole Vault Men`$Result)
X$`Pole Vault Women`$Result <- as.double(X$`Pole Vault Women`$Result)
X$`Shot Put Men`$Result <- as.double(X$`Shot Put Men`$Result)
X$`Shot Put Women`$Result <- as.double(X$`Shot Put Women`$Result)
X$`Triple Jump Men`$Result <- as.double(X$`Triple Jump Men`$Result)
X$`Triple Jump Women`$Result <- as.double(X$`Triple Jump Women`$Result)


# ----------------------- PLOT FOR ALL-TIME MEDALS BY COUNTRY -----------------------
medal_counts <- final_results %>%
  group_by(Nationality, Medal) %>%
  summarize(Count=length(Medal))

medal_levels <- medal_counts %>%
  group_by(Nationality) %>%
  summarize(Total=sum(Count)) %>%
  arrange(Total) %>%
  select(Nationality)

medal_counts$Nationality <- factor(medal_counts$Nationality, levels=medal_levels$Nationality)
medal_counts$Medal <- factor(medal_counts$Medal, levels=c("B", "S", "G"))


all_time_plot <- medal_counts %>%
  ggplot(aes(x=Nationality, y=Count, fill=Medal)) +
  geom_col(color="white") +
  coord_flip() +
  scale_fill_manual(values=c("#967444","#C0C0C0","#FFD700")) +
  theme(axis.text.x = element_text(vjust = 1, hjust = 1)) +
  labs(title = "All-time medals by country",
       x = "Country",
       y = "Total medals")
ggplotly(all_time_plot, height = 2000, width = 1000)


# ----------------------- PLOT FOR COMPARISON BETWEEN RUSSIA, GERMANY, GREAT BRITAIN -----------------------
medals_by_year <- final_results %>%
  select(Nationality, Year, Medal) %>%
  group_by(Nationality, Year) %>%
  summarize(Total=length(Medal))

ger_rus_gbr <- medals_by_year %>%
  filter(Nationality=="GER" | Nationality=="EUA" | Nationality=="FRG" | Nationality=="GDR"
         | Nationality=="RUS" | Nationality=="URS" | Nationality=="EUN"
         | Nationality=="GBR") %>%
  mutate(Nationality = str_replace(Nationality, "EUA", "GER")) %>%
  mutate(Nationality = str_replace(Nationality, "FRG", "GER")) %>%
  mutate(Nationality = str_replace(Nationality, "GDR", "GER")) %>%
  mutate(Nationality = str_replace(Nationality, "URS", "RUS")) %>%
  mutate(Nationality = str_replace(Nationality, "EUN", "RUS"))
ger_rus_gbr

plot_medals_by_year <- ger_rus_gbr %>%
  ggplot(aes(x = Year, y = Total, color = fct_reorder2(Nationality, Year, Total)))+
  geom_line() +
  geom_point() +
  labs(title = "Russia, Germany and Great Britain",
       x = "Year",
       y = "Total medals") +
  theme(legend.position="bottom") +
  theme(legend.title=element_blank())

ggplotly(plot_medals_by_year) %>%
  layout(legend = list(orientation = "h", x = 0.4, y = -0.2, title = list(text = "")))


# ----------------------- PLOT FOR CHANGE IN GOLD MEDAL PLACE FOR SOME SPORTS -----------------------
#100M MEN
op <- options(digits.secs = 2)
X$`100M Men`$Result <- substr(strptime(X$`100M Men`$Result, format = "%OS"), 18, 22)

evolution_100m_men <- X$`100M Men` %>%
  filter(Medal == "G") %>%
  select(Year, Result) %>%
  arrange(Year)

plot_evolution_100m_men <- evolution_100m_men %>%
  ggplot(aes(x = Year, y = as.numeric(Result)))+
  geom_line(color = "#FF6347") +
  geom_point(color = "#B22222") +
  labs(title = "Which time awards a gold medal for '100M Men'",
       x = "Year",
       y = "Time (seconds)") +
  theme(legend.position="bottom") +
  theme(legend.title=element_blank())

ggplotly(plot_evolution_100m_men) %>%
  layout(legend = list(orientation = "h", x = 0.4, y = -0.2, title = list(text = "")))

#100M WOMEN
X$`100M Women`$Result <- substr(strptime(X$`100M Women`$Result, format = "%OS"), 18, 22)

evolution_100m_women <- X$`100M Women` %>%
  filter(Medal == "G") %>%
  select(Year, Result) %>%
  arrange(Year)

plot_evolution_100m_women <- evolution_100m_women %>%
  ggplot(aes(x = Year, y = as.numeric(Result)))+
  geom_line(color = "#008080") +
  geom_point(color = "#000080") +
  labs(title = "Which time awards a gold medal for '100M Women'",
       x = "Year",
       y = "Time (seconds)") +
  theme(legend.position="bottom") +
  theme(legend.title=element_blank())

ggplotly(plot_evolution_100m_women) %>%
  layout(legend = list(orientation = "h", x = 0.4, y = -0.2, title = list(text = "")))


#400M MEN
X$`400M Men`$Result <- substr(strptime(X$`400M Men`$Result, format = "%OS"), 18, 22)

evolution_400m_men <- X$`400M Men` %>%
  filter(Medal == "G") %>%
  select(Year, Result) %>%
  arrange(Year)

plot_evolution_400m_men <- evolution_400m_men %>%
  ggplot(aes(x = Year, y = as.numeric(Result)))+
  geom_line(color = "#FF6347") +
  geom_point(color = "#B22222") +
  labs(title = "Which time awards a gold medal for '400M Men'",
       x = "Year",
       y = "Time (seconds)") +
  theme(legend.position="bottom") +
  theme(legend.title=element_blank())

ggplotly(plot_evolution_400m_men) %>%
  layout(legend = list(orientation = "h", x = 0.4, y = -0.2, title = list(text = "")))

#400M WOMEN
X$`400M Women`$Result <- substr(strptime(X$`400M Women`$Result, format = "%OS"), 18, 22)

evolution_400m_women <- X$`400M Women` %>%
  filter(Medal == "G") %>%
  select(Year, Result) %>%
  arrange(Year)

plot_evolution_400m_women <- evolution_400m_women %>%
  ggplot(aes(x = Year, y = as.numeric(Result)))+
  geom_line(color = "#008080") +
  geom_point(color = "#000080") +
  labs(title = "Which time awards a gold medal for '400M Women'",
       x = "Year",
       y = "Time (seconds)") +
  theme(legend.position="bottom") +
  theme(legend.title=element_blank())

ggplotly(plot_evolution_400m_women) %>%
  layout(legend = list(orientation = "h", x = 0.4, y = -0.2, title = list(text = "")))


#JAVELIN THROW MEN
evolution_javelin_throw_men <- X$`Javelin Throw Men` %>%
  filter(Medal == "G") %>%
  select(Year, Result) %>%
  arrange(Year)

plot_evolution_javelin_throw_men <- evolution_javelin_throw_men %>%
  ggplot(aes(x = Year, y = as.numeric(Result)))+
  geom_line(color = "#FF6347") +
  geom_point(color = "#B22222") +
  labs(title = "How far do you have to throw the javelin for a gold metal in men",
       x = "Year",
       y = "Metres") +
  theme(legend.position="bottom") +
  theme(legend.title=element_blank())

ggplotly(plot_evolution_javelin_throw_men) %>%
  layout(legend = list(orientation = "h", x = 0.4, y = -0.2, title = list(text = "")))

#JAVELIN THROW WOMEN
evolution_javelin_throw_women <- X$`Javelin Throw Women` %>%
  filter(Medal == "G") %>%
  select(Year, Result) %>%
  arrange(Year)

plot_evolution_javelin_throw_women <- evolution_javelin_throw_women %>%
  ggplot(aes(x = Year, y = as.numeric(Result)))+
  geom_line(color = "#008080") +
  geom_point(color = "#000080") +
  labs(title = "How far do you have to throw the javelin for a gold metal in women",
       x = "Year",
       y = "Metres") +
  theme(legend.position="bottom") +
  theme(legend.title=element_blank())

ggplotly(plot_evolution_javelin_throw_women) %>%
  layout(legend = list(orientation = "h", x = 0.4, y = -0.2, title = list(text = "")))


#POLE VAULT MEN
evolution_pole_vault_men <- X$`Pole Vault Men` %>%
  filter(Medal == "G") %>%
  select(Year, Result) %>%
  arrange(Year)

plot_evolution_pole_vault_men <- evolution_pole_vault_men %>%
  ggplot(aes(x = Year, y = as.numeric(Result)))+
  geom_line(color = "#FF6347") +
  geom_point(color = "#B22222") +
  labs(title = "How high do you have to be in pole vault for a gold metal in men",
       x = "Year",
       y = "Metres") +
  theme(legend.position="bottom") +
  theme(legend.title=element_blank())

ggplotly(plot_evolution_pole_vault_men) %>%
  layout(legend = list(orientation = "h", x = 0.4, y = -0.2, title = list(text = "")))

#POLE VAULT WOMEN
evolution_pole_vault_women <- X$`Pole Vault Women` %>%
  filter(Medal == "G") %>%
  select(Year, Result) %>%
  arrange(Year)

plot_evolution_pole_vault_women <- evolution_pole_vault_women %>%
  ggplot(aes(x = Year, y = as.numeric(Result)))+
  geom_line(color = "#008080") +
  geom_point(color = "#000080") +
  labs(title = "How high do you have to be in pole vault for a gold metal in women",
       x = "Year",
       y = "Metres") +
  theme(legend.position="bottom") +
  theme(legend.title=element_blank())

ggplotly(plot_evolution_pole_vault_women) %>%
  layout(legend = list(orientation = "h", x = 0.4, y = -0.2, title = list(text = "")))



