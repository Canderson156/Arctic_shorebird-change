
#load data set of number of shorebirds observed per plot per year
sb_year <- readRDS("Robjects/sb_year.RDS")



#filter to keep only the years where there were repeated observations (only occured in two regions)
sb_rep <- sb_year %>%
  filter(Plot %in% sb_year$Plot[sb_year$Year == 2019]) %>%
  filter(Plot %in% sb_year$Plot[sb_year$Year %in% c(1994, 1995, 1997)])

#rename variables
#group the early surveys since they didn't all happen in the same year
#group the rasmussen plots from different region codes together
#keep the max number of birds observed in that plot at one time
sb_rep2 <- sb_rep %>%
  mutate(time_period = ifelse(Year == 2019, "2019", "1994-97" ),
         region = ifelse(Region_code == 3, "PCI", "Rasmussen")) %>%
  group_by(Plot, time_period, region, Species) %>%
  summarize(n_birds = ceiling(mean(max_birds))) %>%
  ungroup()



#how many plots are in each region

ss <- sb_rep2 %>%
  select(Plot, region) %>%
  distinct() %>%
  group_by(region) %>%
  summarize(n = n())



#same as above, but only REPH and WRSA
REPH <- sb_rep[sb_rep$Species == "REPH",] %>%
  mutate(time_period = ifelse(Year == 2019, "2019", "1994-97" ),
         region = ifelse(Region_code == 3, "PCI", "Rasmussen")) %>%
  group_by(Plot, time_period, region) %>%
  summarize(n_birds = ceiling(mean(max_birds)))

WRSA <- sb_rep[sb_rep$Species == "WRSA",] %>%
  mutate(time_period = ifelse(Year == 2019, "2019", "1994-97" ),
         region = ifelse(Region_code == 3, "PCI", "Rasmussen")) %>%
  group_by(Plot, time_period, region) %>%
  summarize(n_birds = ceiling(mean(max_birds)))


#plots connecting early and late periods for plots. Not sure that this is useful.
ggplot(REPH, aes(y = n_birds, x = time_period)) +
  geom_point(aes(color=region)) +
  geom_line(aes(group = Plot))

ggplot(WRSA, aes(y = n_birds, x = time_period)) +
  geom_point(aes(color=region)) +
  geom_line(aes(group = Plot))

#these box plots are more useful I think
ggplot(REPH, aes(y= n_birds, x = region, color = time_period)) +
  geom_boxplot()

ggplot(WRSA, aes(y= n_birds, x = region, color = time_period)) +
  geom_boxplot()



#summary statistics
REPH %>% 
  group_by(region, time_period) %>% 
  summarize(mean = mean(n_birds), sd = sd(n_birds))
  
WRSA %>% 
  group_by(region, time_period) %>% 
  summarize(mean = mean(n_birds), sd = sd(n_birds))


r1 <- lm(n_birds ~ time_period + region, data = REPH)

anova(r1)

w1 <- lm(n_birds ~ time_period + region, data = WRSA)

anova(w1)


#same as above but for all species

sb_list <- sb_rep2 %>%
  ungroup() %>%
  group_split(Species)

names(sb_list) <- unique(sb_rep$Species)

lms <- lapply(sb_list, function(x) lm(n_birds ~ time_period + region, data = x))

anovas <- lapply(lms, anova)




#plotting the species which had a significant difference

#BASA
# was obsereved in early time_period rasmussen in 4 plots (2,2,2,1). not at all in 2019
BASA <- sb_list[[2]]


ggplot(sb_list[[2]], aes(y= n_birds, x = region, color = time_period)) +
  geom_boxplot()




#SESA 
SESA <- sb_list[[14]]

ggplot(sb_list[[14]], aes(y= n_birds, x = region, color = time_period)) +
  geom_boxplot()


#STSA
# was obsereved in 2019 rasmussen in 4 plots (2,2,2,1). not at all in early plots

STSA <- sb_list[[15]]

ggplot(sb_list[[15]], aes(y= n_birds, x = region, color = time_period)) +
  geom_boxplot()


