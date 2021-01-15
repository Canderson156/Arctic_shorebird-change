
#load data set of number of shorebirds observed per plot per year
sb_year <- readRDS("Robjects/sb_year.RDS")



#filter to keep only the years where there were repeated observations (only occured in two regions)
sb_rep <- sb_year %>%
  filter(Plot %in% sb_year$Plot[sb_year$Year == 2019]) %>%
  filter(Species %notin% c("LESA", "SAND", "SEPL", "WHIM", "WISN"))




#rename variables
#group the early surveys since they didn't all happen in the same year
#group the rasmussen plots from different region codes together
#keep the max number of birds observed in that plot at one time
#re-add the presence variable
#change plot area from km2 to ha



sb_rep2 <- sb_rep %>%
  mutate(time_period = ifelse(Year == 2019, "2019", "1994-97" ),
         region = ifelse(Region_code == 3, "PCI", "Rasmussen")) %>%
  group_by(Plot, time_period, region, Species, Plot_area) %>%
  summarize(n_birds = ceiling(mean(max_birds))) %>%
  ungroup() %>%
  mutate(presence = ifelse(n_birds == 0, FALSE, TRUE)) %>%
  mutate(Plot_area = Plot_area*100)

#how many plots are in each region

ss <- sb_rep2 %>%
  select(Plot, region) %>%
  distinct() %>%
  group_by(region) %>%
  summarize(n = n())


#count and presence for any shorebirds

sb_sb <- sb_rep2 %>%
  select(-Species) %>%
  group_by(Plot, time_period, region, Plot_area) %>%
  summarize(n_birds = sum(n_birds)) %>%
  ungroup() %>%
  mutate(presence = ifelse(n_birds == 0, FALSE, TRUE))







#region specific datasets

sb_ras <- sb_rep2 %>%
  filter(region == "Rasmussen") %>%
  filter(Species %notin% c("RUTU", "REKN"))
  


sb_pci <- sb_rep2 %>%
  filter(region == "PCI") %>%
  filter(Species %notin% c("BASA", "BBSA", "RNPH", "STSA"))

sb_sb_ras <- sb_sb %>%
  filter(region == "Rasmussen")


sb_sb_pci <- sb_sb %>%
  filter(region == "PCI")


#### SPECIES MODEL, REGIONS SEPERATE


########### RASMUSSEN

sb_list_ras <- sb_ras %>%
  ungroup() %>%
  group_split(Species)

names(sb_list_ras) <- unique(sb_ras$Species)

lms_ras <- lapply(sb_list_ras, function(x) glm(n_birds ~ time_period + offset(log(Plot_area)), data = x, family = "poisson"))

lapply(lms_ras, summary)
lapply(lms_ras, function(x) coef(summary(x))[1,1]) #intercepts
lapply(lms_ras, function(x) coef(summary(x))[2,c(1,2,4)]) #coefficient, SE, p


#lapply(sb_list_ras, function(x) sum(x$n_birds)) #== 0 #use to check that all species have been observed in the region

########### PCI


sb_list_pci <- sb_pci %>%
  ungroup() %>%
  group_split(Species)

names(sb_list_pci) <- unique(sb_pci$Species)

lms_pci <- lapply(sb_list_pci, function(x) glm(n_birds ~ time_period + offset(log(Plot_area)), data = x, family = "poisson"))

lapply(lms_pci, summary)
lapply(lms_pci, function(x) coef(summary(x))[1,1])#intercepts
lapply(lms_pci, function(x) coef(summary(x))[2,c(1,2,4)]) #coefficient, SE, p


#lapply(sb_list_pci, function(x) sum(x$n_birds)) #== 0 #use to check that all species have been observed in the region








#### SPECIES MODEL, REGIONS TOGETHER



sb_list <- sb_rep2 %>%
  ungroup() %>%
  group_split(Species)

names(sb_list) <- unique(sb_rep$Species)

lms_both <- lapply(sb_list, function(x) glm(n_birds ~ time_period + region + offset(log(Plot_area)), data = x, family = "poisson"))


lapply(lms_both, function(x) coef(summary(x))[1,1])#intercepts
lapply(lms_both, function(x) coef(summary(x))[2:3,c(1,2,4)]) #coefficient, SE, p



ggplot(sb_list[[13]], aes(y= n_birds, x = region, color = time_period)) +
  geom_boxplot() +
  scale_y_log10()



#### ALL MODEL, REGIONS SEPERATE



########### RASMUSSEN



all_ras <- glmer(n_birds ~ time_period + offset(log(Plot_area)) + (1|Species), data = sb_ras, family = "poisson")



########### PCI

all_pci <- glmer(n_birds ~ time_period + offset(log(Plot_area)) + (1|Species), data = sb_pci, family = "poisson")



#### ALL MODEL, REGIONS TOGETHER


all_both <- glmer(n_birds ~ time_period + offset(log(Plot_area)) + region + (1|Species), data = sb_rep2, family = "poisson")










#################### PLOTS



#change in the absolute number of shorebirds observed between the two time periods
ggplot(sb_sb, aes(y= n_birds, x = region, color = time_period)) +
  geom_boxplot()


#proportion of plots where shorebirds were observed 
table(sb_sb$presence, sb_sb$time_period)[2,]/table(sb_sb$time_period) #3% decrease


table(sb_sb_ras$presence, sb_sb_ras$time_period)[2,]/table(sb_sb_ras$time_period) #3% increase


table(sb_sb_pci$presence, sb_sb_pci$time_period)[2,]/table(sb_sb_pci$time_period) #13% decrease

  
#variation by species


test <- sb_rep2 %>%
  mutate(time_period = ifelse(time_period == "1994-97", 1994, time_period)) %>%
  mutate(time_period = as.numeric(time_period))


test2 <- summarySE(test, measurevar = "n_birds", groupvars = c("time_period", "region", "Species"))


ggplot(test2, aes(y= n_birds, x = time_period, colour = Species)) +
  geom_line(size = 1) +
  geom_errorbar(aes(ymin=n_birds-ci, ymax=n_birds+ci), width=1) +
  facet_wrap(~region) +
  scale_x_continuous(breaks = c(1994, 2019)) +
  scale_y_continuous(breaks = c(0, 2,4,6,8,10))

ggplot(test2, aes(y= n_birds, x = time_period, fill = Species, colour = Species)) +
  geom_line(size = 1) +
  geom_ribbon(aes(ymin=n_birds-ci, ymax=n_birds+ci), alpha=0.1, linetype = "blank") +
  facet_wrap(~region) +
  scale_x_continuous(breaks = c(1994, 2019))

ggplot(test2, aes(y= n_birds, x = time_period, colour = region)) +
  geom_line(size = 1) +
  geom_errorbar(aes(ymin=n_birds-ci, ymax=n_birds+ci), width=1) +
  facet_wrap(~Species) +
  scale_x_continuous(breaks = c(1994, 2019)) +
  scale_y_continuous(breaks = c(0, 2,4,6,8,10))

ggplot(test2, aes(y= n_birds, x = time_period, fill = region, colour = region)) +
  geom_line(size = 1) +
  geom_ribbon(aes(ymin=n_birds-ci, ymax=n_birds+ci), alpha=0.2, linetype = "blank") +
  facet_wrap(~Species) +
  scale_x_continuous(breaks = c(1994, 2019))

