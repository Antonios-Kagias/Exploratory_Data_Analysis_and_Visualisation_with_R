library(tidyverse)


# [ΜΕΡΟΣ Α]


data <- tribble(
  ~happy, ~uptempo, ~blues, ~jazz, ~gospel,
  "yes", "yes", 10, 5, 20,
  "no", "no", NA, 12, 15,
  "yes", "no", 7, 6, 4,
  "no", "yes", 3, NA, NA
)

# Ερώτημα (α)

data_tidy1 <- data %>%
  pivot_longer(c(blues, jazz, gospel), names_to = "genre", values_to = "count") %>%
  arrange(genre)

# Ερώτημα (β)

data_tidy2 <- data %>%
  pivot_longer(c(blues, jazz, gospel), names_to = "genre", values_to = "count", values_drop_na = TRUE)
data_tidy2 <- arrange(data_tidy2, genre)

# Ερώτημα (γ)

# FALSE FALSE
temp1 <- data_tidy2 %>%
  filter(genre != "jazz" & happy == "no") %>%
  select(count) %>%
  sum(na.rm = TRUE)
# FALSE TRUE
temp2 <- data_tidy2 %>%
  filter(genre != "jazz" & happy == "yes") %>%
  select(count) %>%
  sum(na.rm = TRUE)
# TRUE FALSE
temp3 <- data_tidy2 %>%
  filter(genre == "jazz" & happy == "no") %>%
  select(count) %>%
  sum(na.rm = TRUE)
# TRUE TRUE
temp4 <- data_tidy2 %>%
  filter(genre == "jazz" & happy == "yes") %>%
  select(count) %>%
  sum(na.rm = TRUE)

data_tidy3 <- tribble(
  ~jazz, ~happy, ~total,
  "FALSE", "FALSE", temp1,
  "FALSE", "TRUE", temp2,
  "TRUE", "FALSE", temp3,
  "TRUE", "TRUE", temp4
)

# Ερώτημα (δ)

data_tidy2 %>%
  filter(genre == "jazz" & happy == "no") %>%
  select(count) %>%
  sum(na.rm = TRUE)

data_tidy3 %>%
  filter(jazz == TRUE & happy == FALSE) %>%
  select(total)



# [ΜΕΡΟΣ Β]



who <- tidyr::who
view(who)

dict_url <- "https://extranet.who.int/tme/generateCSV.asp?ds=dictionary"
if (!file.exists("dict.csv")) download.file(dict_url, "dict.csv")
dict <- read_csv('dict.csv')
view(dict)

labels <- data.frame(name = colnames(who))
view(labels)

explanations <- semi_join(dict, labels, by=c("variable_name" = "name"))
view(explanations)

# Ερώτημα (1)

ntfc <- select(filter(explanations, dataset == "Notification"), variable_name)
who_new1 <- who %>% 
  pivot_longer(c(ntfc$variable_name), names_to = "notification", values_to = "cases", values_drop_na = TRUE)
who_new1

# Ερώτημα (2)

who_new2 <- who_new1 %>% 
  mutate(notification = str_replace(notification, "newrel", "new_rel"))
who_new2

# Ερώτημα (3)

who_new3 <- who_new2 %>% 
  separate(notification, into = c("new", "type", "sex_age"), sep = "_")
who_new3
who_new4 <- who_new3 %>% 
  separate(sex_age, into = c("sex", "age"), 1)
who_new4

# Ερώτημα (4)

who_new5 <- who_new4 %>% 
  select(!new) %>%
  select(!iso2) %>%
  select(!iso3)
who_new5

# Ερώτημα (5)

who_new6 <- who_new5 %>% 
  select(country, cases) %>%
  group_by(country) %>%
  summarise(count = sum(cases))
who_new6

# Ερώτημα (6)

who_new7 <- who_new5 %>%
  filter(type == "sp") %>%
  group_by(year) %>%
  summarise(cases = max(cases))
who_new7

# Ερώτημα (7)

#only for female
greek_f <- who_new5 %>% 
  filter(country == "Greece") %>%
  filter(sex == "f") %>%
  select(year, cases) %>%
  group_by(year) %>%
  mutate(f = sum(cases)) %>%
  select(!cases) %>%
  unique()
greek_f

#only for male
greek_m <- who_new5 %>% 
  filter(country == "Greece") %>%
  filter(sex == "m") %>%
  select(year, cases) %>%
  group_by(year) %>%
  mutate(m = sum(cases)) %>%
  select(!cases) %>%
  unique()
greek_m

greek <- left_join(greek_f, greek_m)
arrange(greek, desc(rowSums(greek)))

