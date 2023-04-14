library(tidyverse)
library(jsonlite)

download.file("https://atvira.sodra.lt/imones/downloads/2022/monthly-2022.csv.zip", "temp")
unzip("temp")
raw <- read_csv2("monthly-2022.csv")
names(raw) <- c("code", "jarCode", "name", "municipality", "ecoActCode", "ecoActName", "month", "avgWage", "numInsured", "avgWage2", "numInsured2", "tax")

codes <- raw %>%
  group_by(ecoActCode) %>%
  summarise(n = n()) %>%
  arrange(desc(n)) %>%
  na.omit() %>%
  head(22)

raw %>%
  filter(ecoActCode %in% codes$ecoActCode) %>%
  write_csv("lab_sodra.csv")

raw_json <- head(raw) %>%
  toJSON()

write(raw_json, "lab_sodra.json")



# 1 užduotis

library(ggplot2)

data1 = 
  lab_sodra %>% 
  filter(grepl("949900", ecoActCode))

ggplot(data = data1, mapping = aes(x = avgWage)) +
  geom_histogram()



# 2 užduotis

data2 = 
  data1 %>%
  group_by(name) %>%
  summarise(avg_Wage = mean(avgWage, na.rm = T)) %>%
  arrange(desc(avg_Wage))

data22 = data1 %>%
  left_join(data2, by = "name")
data22=unique(data22[, c("name", "avg_Wage")])
data22=data22 %>%
  arrange(desc(avg_Wage)) %>%
  head(5)

data222 =
  data1 %>%
  filter(name %in% data22$name)

ggplot(data = data222, mapping = aes(x = month, y = avgWage, group = name, color = name)) +
  geom_line();




# 3 užduotis

data3 = 
  data222 %>%
  filter(month>=202204) %>%
  group_by (name) %>%
  summarise(insured=sum(numInsured))

ggplot(data = data3, mapping = aes(x = reorder(name, -insured), y = insured, fill = name)) +
  geom_bar(stat = "identity") +
  labs(x = "Name", y = "numInsured", title = "numInsured by Name")
