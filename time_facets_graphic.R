#facet plots for team time

#load in necessary packages
library(tidyverse)
library(lubridate)

#read in data
df <- read_csv("plastic_regions_data.csv", guess_max = 50000) %>% 
  filter(!is.na(region))

#working on grouping by month
df1 <- df %>% 
  mutate (MonthNum = ifelse(MonthNum < 10, paste0("0", MonthNum), MonthNum),
          YearMonth = paste(Year, MonthNum, sep = "-")) 

#add in plastic straw bans
region_bans <- df1 %>% 
  mutate(ban_date = ifelse(COUNTRY == "Brazil", "2018-07-15", 
                           ifelse(COUNTRY == "United Kingdom", "2018-04-15",
                                  ifelse(ISO_CODE == "USWA", "2018-07-01",
                                         ifelse(region == "southeast asia", "2018-01-01",
                                                ifelse(COUNTRY == "Costa Rica", "2017-06-01", NA)))))) %>% 
  filter(!is.na(ban_date))

#get totals by plastic type (hard, soft, marine, other) for each region of interest
#cols of interest: TotalLength_m, TotalVolunteers, TotalClassifiedItems, SUM_ columns
region_sums <- region_bans %>% 
  rename(total_plastic = TotalClassifiedItems_EC2020) %>%
  group_by(YearMonth, COUNTRY, ISO_CODE, region) %>% 
  summarise(hard1 = sum(SUM_Hard_PlasticBeverageBottle),
            hard2 = sum(SUM_Hard_OtherPlasticBottle),
            hard3 = sum(SUM_Hard_BucketOrCrate),
            hard4 = sum(SUM_Hard_Lighter),
            hard5 = sum(SUM_OtherHardPlastic),
            hard6 = sum(SUM_HardSoftLollipopStick_EarBu),
            soft1 = sum(SUM_PlasticOrFoamFoodContainer),
            soft2 = sum(SUM_PlasticOrFoamPlatesBowlsCup),
            soft3 = sum(SUM_Soft_Bag),
            soft4 = sum(SUM_Soft_WrapperOrLabel),
            soft5 = sum(SUM_Soft_Straw),
            soft6 = sum(SUM_Soft_OtherPlastic),
            soft7 = sum(SUM_Soft_CigaretteButts),
            soft8 = sum(SUM_Soft_StringRingRibbon),
            mar1 = sum(Fishing_Net),
            mar2 = sum(SUM_FishingLineLureRope),
            mar3 = sum(Fishing_BuoysAndFloats),
            oth1 = sum(SUM_HardOrSoft_PlasticBottleCap),
            oth2 = sum(SUM_HardSoft_PersonalCareProduc),
            oth3 = sum(SUM_Foam_OtherPlasticDebris),
            oth4 = sum(SUM_OtherPlasticDebris),
            length_collected_km = sum(TotalLength_m/1000)
            ) %>% 
  mutate(tot_hard = sum(hard1, hard2, hard3, hard4, hard5, hard6),
         tot_soft = sum(soft1, soft2, soft3, soft4, soft5, soft6, soft7, soft8),
         tot_mar = sum(mar1, mar2, mar3),
         tot_other = sum(oth1, oth2, oth3, oth4),
         YearMonth = gsub("$", "-01", YearMonth),
         YearMonth = ymd(YearMonth))


#PLOTTING ---------------------------------------------------------------
#plot! basic plot with CPUE over time per region
region_sums %>% 
  filter(COUNTRY == "Brazil") %>% 
  ggplot() +
  geom_point(aes(x = YearMonth, y = tot_hard/(length_collected_km+10), color = "Hard Plastics"), size = 0.75) +
  geom_point(aes(x = YearMonth, y = tot_soft/(length_collected_km+10), color = "Soft Plastics"), size = 0.75) +
  geom_point(aes(x = YearMonth, y = tot_mar/(length_collected_km+10), color = "Marine Plastics"), size = 0.75) +
  geom_point(aes(x = YearMonth, y = tot_other/(length_collected_km+10), color = "Other Plastics"), size = 0.75) +
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),panel.background = element_blank(), axis.ticks.x = element_blank()) +
  ylab("Plastic Density (Items Collected / m Cleanup)") +
  xlab("Date") +
  scale_x_date(date_labels = "%Y %b")+
  geom_vline(aes(xintercept = as.Date("2018-07-15")), col = "red", size = 1) +
  ylim(c(0,750))

region_sums %>% 
  filter(region == "southeast asia") %>% 
  ggplot() +
  geom_point(aes(x = YearMonth, y = tot_hard/(length_collected_km+10), color = "Hard Plastics"), size = 0.75) +
  geom_point(aes(x = YearMonth, y = tot_soft/(length_collected_km+10), color = "Soft Plastics"), size = 0.75) +
  geom_point(aes(x = YearMonth, y = tot_mar/(length_collected_km+10), color = "Marine Plastics"), size = 0.75) +
  geom_point(aes(x = YearMonth, y = tot_other/(length_collected_km+10), color = "Other Plastics"), size = 0.75) +
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),panel.background = element_blank(), axis.ticks.x = element_blank()) +
  ylab("Plastic Density (Items Collected / m Cleanup)") +
  xlab("Date") +
  scale_x_date(date_labels = "%Y %b")+
  geom_vline(aes(xintercept = as.Date("2018-01-01")), col = "red", size = 1.5)+
  ylim(c(0,1000))

