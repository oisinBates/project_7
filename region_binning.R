#team time!

#read in necessary packages
library(tidyverse)
library(broom)

#read in data
all_plastics <- read_csv("plastic_pollution.csv", guess_max = 5000) %>% 
  rename(lat = Y,
         long = X)
str(all_plastics)

# #create bins for data
# lat_sum <- all_plastics %>% 
#   group_by(COUNTRY) %>% 
#   summarise(max_lat = max(Y),
#             min_lat = min(Y),
#             med_lat = median(Y),
#             max_long = max(X),
#             min_long = min(X),
#             med_lat = median(Y))


#SE ASIA: 
##  long > 71 < 162
## lat > -16 < 33
## country: not Pakistan, China

se_asia <- all_plastics %>% 
  filter(lat < 33, lat > -16, long > 71, long < 162, COUNTRY != "China", COUNTRY != "Australia",COUNTRY != "Pakistan") %>% 
  mutate(region = "southeast asia")
levels(as.factor(se_asia$COUNTRY)) 

#N ASIA:
##  long > 34 < 149
##  lat > 18
## country: not Pakistan, kyrgyzstan, 
n_asia <- all_plastics %>% 
  filter(lat > 18 , long > 73, long < 149, COUNTRY != "Pakistan", COUNTRY != "Kyrgyzstan", COUNTRY != "Vietnam", COUNTRY != "Philippines", COUNTRY != "India") %>% 
  mutate(region = "north asia")

#check countries for errors
levels(as.factor(n_asia$COUNTRY))

#Middle East:
mid_east <- all_plastics %>% 
  filter(lat > 12 , lat < 56, long > 27, long < 80, !COUNTRY %in% c("Egypt", "Romania", "Ukraine", "Bulgaria", "India", "Greece", "Russian Federation")) %>% 
  mutate(region = "middle east")

#check countries for errors
levels(as.factor(mid_east$COUNTRY))

#west coast NAmerica
na_west <- all_plastics %>% 
  filter(lat > 15 , lat < 83, long > -171, long < -99, !COUNTRY %in% c("Guatemala")) %>% 
  mutate(region = "west north america")

#check countries for errors
levels(as.factor(na_west$COUNTRY))

#east coast NAmerica
na_east <- all_plastics %>% 
  filter(lat > 15 , lat < 83, long < -53 , long > -99, COUNTRY %in% c("Canada", "Mexico", "United States")) %>% 
  mutate(region = "east north america")

#check countries for errors
levels(as.factor(na_east$COUNTRY))

#Atlantic Islands
atl_isles <- all_plastics %>% 
  filter(lat > 15 , lat < 83, long < -53 , long > -99, !COUNTRY %in% c("Canada", "Mexico", "United States", "Honduras", "Belize")) %>% 
  mutate(region = "atlantic islands")

#check countries for errors
levels(as.factor(atl_isles$COUNTRY)) #I MAY HAVE MISSED SOME SMALL ISLANDS NORTH OF SOUTH AMERICA -- grouping these with s america, because they're hard to grab with atl isles

#central america
cent_am <- all_plastics %>% 
  filter(lat > 7 , lat < 19, long > -92 , long < -77, !COUNTRY %in% c("Canada", "Colombia", "Mexico", "Jamaica"))  %>% 
  mutate(region = "central america")

#check countries for errors
levels(as.factor(cent_am$COUNTRY))

#west south america
w_south_am <- all_plastics %>% 
  filter(lat > -55 , lat < 13, long > -91 , long < -61, !COUNTRY %in% c("Costa Rica", "Ecuador", "Nicaragua", "Panama")) %>% 
  mutate(region = "west south america")

#check countries for errors
levels(as.factor(w_south_am$COUNTRY))

#east south america
e_south_am <- all_plastics %>% 
  filter(lat > -55 , lat < 13, long > -61 , long < -34, !COUNTRY %in% c("Costa Rica", "Ecuador", "Nicaragua", "Panama")) %>% 
  mutate(region = "east south america")

#check countries for errors
levels(as.factor(e_south_am$COUNTRY))

#europe
eur <- all_plastics %>% 
  filter(lat > 34 , lat < 71, long > -11.5 , long < 40, !COUNTRY %in% c("Russian Federation", "Cyprus")) %>% 
  mutate(region = "europe")

#check countries for errors
levels(as.factor(eur$COUNTRY))

#west africa
w_afr <- all_plastics %>% 
  filter(lat > -35 , lat < 38, long > -19 , long < 22, !COUNTRY %in% c("Spain", "Portugal", "Greece")) %>% 
  mutate(region = "west africa")

#check countries for errors
levels(as.factor(w_afr$COUNTRY))

#east africa
e_afr <- all_plastics %>% 
  filter(lat > -35 , lat < 38, long > 22 , long < 52, !COUNTRY %in% c("Cyprus", "Greece", "Turkey", "Israel", "Saudi Arabia", "Qatar", "Kuwait")) %>% 
  mutate(region = "east africa")

#check countries for errors
levels(as.factor(e_afr$COUNTRY))

#Australia, New Zealand and some South Pacific Countries
w_pac <- all_plastics %>% 
  filter(lat > -53 , lat < -8, long > 111 , long < 180, COUNTRY != "Indonesia") %>% 
  mutate(region = "west pacific")

#check countries for errors
levels(as.factor(w_pac$COUNTRY))

#put em all together
plastic_regions <- bind_rows(atl_isles, cent_am, e_afr, e_south_am, eur, mid_east, n_asia, na_west, se_asia, w_afr, w_pac, w_south_am) 

write_csv(plastic_regions, "plastic_regions_data.csv")

#total for each
region_totals <- plastic_regions %>% 
  group_by(region) %>% 
  summarise_each(total = sum())


