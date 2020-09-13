#team time!

#read in necessary packages
library(tidyverse)
library(broom)

#read in data
all_plastics <- read_csv("plastic_pollution.csv", guess_max = 50000) %>% 
  rename(lat = Y,
         long = X)
str(all_plastics)

#SE ASIA: 
##  long > 71 < 162
## lat > -16 < 33
## CountryName_FromSource: not Pakistan, China

se_asia <- all_plastics %>% 
  filter(lat < 33, lat > -16, long > 71, long < 162, CountryName_FromSource != "China", CountryName_FromSource != "Australia",CountryName_FromSource != "Pakistan") %>% 
  mutate(region = "southeast asia")
levels(as.factor(se_asia$CountryName_FromSource)) 

#N ASIA:
##  long > 34 < 149
##  lat > 18
## CountryName_FromSource: not Pakistan, kyrgyzstan, 
n_asia <- all_plastics %>% 
  filter(lat > 18 , long > 73, long < 149, CountryName_FromSource != "Pakistan", CountryName_FromSource != "Kyrgyzstan", CountryName_FromSource != "Vietnam", CountryName_FromSource != "Philippines", CountryName_FromSource != "India", CountryName_FromSource != "Myanmar (Burma)", CountryName_FromSource !=  "Nepal") %>% 
  mutate(region = "north asia")

#check countries for errors
levels(as.factor(n_asia$CountryName_FromSource))

#Middle East:
mid_east <- all_plastics %>% 
  filter(lat > 12 , lat < 56, long > 27, long < 80, !CountryName_FromSource %in% c("Egypt", "Romania", "Ukraine", "Bulgaria", "India", "Greece", "Russian Federation", "Russia", "Sri Lanka")) %>% 
  mutate(region = "middle east")

#check countries for errors
levels(as.factor(mid_east$CountryName_FromSource))

#west coast NAmerica
na_west <- all_plastics %>% 
  filter(lat > 15 , lat < 83, long > -171, long < -99, !CountryName_FromSource %in% c("Guatemala")) %>% 
  mutate(region = "west north america")

#check countries for errors
levels(as.factor(na_west$CountryName_FromSource))

#east coast NAmerica
na_east <- all_plastics %>% 
  filter(lat > 15 , lat < 83, long < -53 , long > -99, CountryName_FromSource %in% c("Canada", "Mexico", "United States")) %>% 
  mutate(region = "east north america")

#check countries for errors
levels(as.factor(na_east$CountryName_FromSource))

#Atlantic Islands
atl_isles <- all_plastics %>% 
  filter(lat > 15 , lat < 83, long < -53 , long > -99, !CountryName_FromSource %in% c("Canada", "Mexico", "United States", "Honduras", "Belize", "USA")) %>% 
  mutate(region = "atlantic islands")

#check countries for errors
levels(as.factor(atl_isles$CountryName_FromSource)) #I MAY HAVE MISSED SOME SMALL ISLANDS NORTH OF SOUTH AMERICA -- grouping these with s america, because they're hard to grab with atl isles

#central america
cent_am <- all_plastics %>% 
  filter(lat > 7 , lat < 19, long > -92 , long < -77, !CountryName_FromSource %in% c("Canada", "Colombia", "Mexico", "Jamaica"))  %>% 
  mutate(region = "central america")

#check countries for errors
levels(as.factor(cent_am$CountryName_FromSource))

#west south america
w_south_am <- all_plastics %>% 
  filter(lat > -55 , lat < 13, long > -91 , long < -61, !CountryName_FromSource %in% c("Costa Rica", "Ecuador", "Nicaragua", "Panama")) %>% 
  mutate(region = "west south america")

#check countries for errors
levels(as.factor(w_south_am$CountryName_FromSource))

#east south america
e_south_am <- all_plastics %>% 
  filter(lat > -55 , lat < 13, long > -61 , long < -34, !CountryName_FromSource %in% c("Costa Rica", "Ecuador", "Nicaragua", "Panama")) %>% 
  mutate(region = "east south america")

#check countries for errors
levels(as.factor(e_south_am$CountryName_FromSource))

#europe
eur <- all_plastics %>% 
  filter(lat > 34 , lat < 71, long > -11.5 , long < 40, !CountryName_FromSource %in% c("Russian Federation", "Cyprus", "Russia")) %>% 
  mutate(region = "europe")

#check countries for errors
levels(as.factor(eur$CountryName_FromSource))

#west africa
w_afr <- all_plastics %>% 
  filter(lat > -35 , lat < 38, long > -19 , long < 22, !CountryName_FromSource %in% c("Spain", "Portugal", "Greece", "Western Greece and the Ionian")) %>% 
  mutate(region = "west africa")

#check countries for errors
levels(as.factor(w_afr$CountryName_FromSource))

#east africa
e_afr <- all_plastics %>% 
  filter(lat > -35 , lat < 38, long > 22 , long < 52, !CountryName_FromSource %in% c("Cyprus", "Greece", "Turkey", "Israel", "Saudi Arabia", "Qatar", "Kuwait", "Western Greece and the Ionian", "Sri Lanka", "UK")) %>% 
  mutate(region = "east africa")

#check countries for errors
levels(as.factor(e_afr$CountryName_FromSource))

#Australia, New Zealand and some South Pacific Countries
w_pac <- all_plastics %>% 
  filter(lat > -53 , lat < -8, long > 111 , long < 180, CountryName_FromSource != "Indonesia") %>% 
  mutate(region = "west pacific")

#check countries for errors
levels(as.factor(w_pac$CountryName_FromSource))

#put em all together
plastic_regions <- bind_rows(atl_isles, cent_am, e_afr, e_south_am, eur, mid_east, n_asia, na_west, se_asia, w_afr, w_pac, w_south_am) 

write_csv(plastic_regions, "plastic_regions_data.csv")

#total for each
region_totals <- plastic_regions %>% 
  group_by(region) %>% 
  summarise_each(total = sum())


