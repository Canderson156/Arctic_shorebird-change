#sb_ra = shorebirds _ regional all


#load data set of number of shorebirds observed per plot per year
#sb_year created in the ch 1 scrips series

sb_year <- readRDS("Robjects/sb_year.RDS")


#filter to keep only the years where there were repeated observations (only occured in two regions)
#keep all plots, observed in one or both time periods
sb_ra <- sb_year %>%
  filter(Plot %in% sb_year$Plot[sb_year$Year == 2019]) %>%
  filter(Species %notin% c("LESA", "SAND", "SEPL", "WHIM", "WISN")) %>%
  filter(Region_name %notin% c("North Archipelago")) %>%
  ungroup()




#rename variables
#group the early surveys since they didn't all happen in the same year
#group the rasmussen plots from different region codes together
#keep the max number of birds observed in that plot at one time
#re-add the presence variable
#change plot area from km2 to ha

sb_ra2 <- sb_ra %>%
  mutate(time_period = ifelse(Year == 2019, "2019", "1994-97"),
         region = ifelse(Region_code == 3, "PCI", "Rasmussen")) %>%
  group_by(Plot, time_period, region, Species, Plot_area) %>%
  summarize(n_birds = ceiling(mean(max_birds))) %>%
  ungroup() %>%
  mutate(presence = ifelse(n_birds == 0, FALSE, TRUE)) %>%
  mutate(Plot_area = Plot_area*100)

#how many plots are in each region

ss_ra <- sb_ra2 %>%
  select(Plot, region) %>%
  distinct() %>%
  group_by(region) %>%
  summarize(n = n())


#count and presence for any shorebirds

sb_sb_ra <- sb_ra2 %>%
  select(-Species) %>%
  group_by(Plot, time_period, region, Plot_area) %>%
  summarize(n_birds = sum(n_birds)) %>%
  ungroup() %>%
  mutate(presence = ifelse(n_birds == 0, FALSE, TRUE))







#region specific datasets

sb_ras <- sb_ra2 %>%
  filter(region == "Rasmussen") %>%
  filter(Species %notin% c("RUTU", "REKN"))
  


sb_pci <- sb_ra2 %>%
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


ras_int <- lapply(lms_ras, function(x) coef(summary(x))[1,1]) #intercepts
ras_int <- data.frame(matrix(unlist(ras_int), nrow=length(ras_int), byrow=T))
ras_coef <- lapply(lms_ras, function(x) coef(summary(x))[2,c(1,2,4)]) #coefficient, SE, p
ras_coef <- data.frame(matrix(unlist(ras_coef), nrow=length(ras_coef), byrow=T))
ras_coef <- cbind(ras_int, ras_coef)
colnames(ras_coef) <- c("intercept", "B", "SE", "p")
ras_coef$Species <- unique(sb_ras$Species)

write.csv(ras_coef, "exported/ras_coef.csv")

#lapply(sb_list_ras, function(x) sum(x$n_birds)) #== 0 #use to check that all species have been observed in the region

########### PCI


sb_list_pci <- sb_pci %>%
  ungroup() %>%
  group_split(Species)

names(sb_list_pci) <- unique(sb_pci$Species)

lms_pci <- lapply(sb_list_pci, function(x) glm(n_birds ~ time_period + offset(log(Plot_area)), data = x, family = "poisson"))

pci_int <- lapply(lms_pci, function(x) coef(summary(x))[1,1]) #intercepts
pci_int <- data.frame(matrix(unlist(pci_int), nrow=length(pci_int), byrow=T))
pci_coef <- lapply(lms_pci, function(x) coef(summary(x))[2,c(1,2,4)]) #coefficient, SE, p
pci_coef <- data.frame(matrix(unlist(pci_coef), nrow=length(pci_coef), byrow=T))
pci_coef <- cbind(pci_int, pci_coef)
colnames(pci_coef) <- c("intercept", "B", "SE", "p")
pci_coef$Species <- unique(sb_pci$Species)

write.csv(pci_coef, "exported/pci_coef.csv")

#lapply(sb_list_pci, function(x) sum(x$n_birds)) #== 0 #use to check that all species have been observed in the region








#### SPECIES MODEL, REGIONS TOGETHER



sb_list <- sb_ra2 %>%
  ungroup() %>%
  group_split(Species)

names(sb_list) <- unique(sb_ra$Species)

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


all_both <- glmer(n_birds ~ time_period + offset(log(Plot_area)) + region + (1|Species), data = sb_ra2, family = "poisson")





### ALL MODEL, INTERACTION


all_int <- glmer(n_birds ~ time_period*region + offset(log(Plot_area)) + (1|Species), data = sb_ra2, family = "poisson")






#################### PLOTS


#for use with log scale
sb_sb2 <- sb_sb %>%
  mutate(n_birds = ifelse(n_birds == 0, 1, n_birds))


#change in the absolute number of shorebirds observed between the two time periods
ggplot(sb_sb, aes(y= n_birds, x = region, color = time_period)) +
  geom_boxplot()

ggplot(sb_sb2, aes(y= n_birds, x = region, color = time_period)) +
  geom_boxplot() +
  scale_y_log10()


#proportion of plots where shorebirds were observed 
table(sb_sb$presence, sb_sb$time_period)[2,]/table(sb_sb$time_period) #3% decrease


table(sb_sb_ras$presence, sb_sb_ras$time_period)[2,]/table(sb_sb_ras$time_period) #3% increase


table(sb_sb_pci$presence, sb_sb_pci$time_period)[2,]/table(sb_sb_pci$time_period) #13% decrease

  
#variation by species


test <- sb_ra2 %>%
  mutate(time_period = ifelse(time_period == "1994-97", 1994, time_period)) %>%
  mutate(time_period = as.numeric(time_period))


test2 <- summarySE(test, measurevar = "n_birds", groupvars = c("time_period", "region", "Species"))
test3 <- summarySE(test, measurevar = "n_birds", groupvars = c("time_period", "Species"))


#Seperated by Region

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
  scale_x_continuous(breaks = c(1994, 2019))  +
  scale_y_continuous(breaks = c(0, 2,4,6,8,10))

#Seperated by species, fixed y axis

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
  scale_x_continuous(breaks = c(1994, 2019))  +
  scale_y_continuous(breaks = c(0, 2,4,6,8,10))

#Seperated by species, free y axis

ggplot(test2, aes(y= n_birds, x = time_period, colour = region)) +
  geom_line(size = 1) +
  geom_errorbar(aes(ymin=n_birds-ci, ymax=n_birds+ci), width=1) +
  facet_wrap(~Species, scales = "free_y") +
  scale_x_continuous(breaks = c(1994, 2019)) +
  scale_y_continuous(breaks = c(0, 0.5,1, 2,4,6,8,10))

ggplot(test2, aes(y= n_birds, x = time_period, fill = region, colour = region)) +
  geom_line(size = 1) +
  geom_ribbon(aes(ymin=n_birds-ci, ymax=n_birds+ci), alpha=0.2, linetype = "blank") +
  facet_wrap(~Species, scales = "free_y") +
  scale_x_continuous(breaks = c(1994, 2019)) +
  scale_y_continuous(breaks = c(0, 0.5,1, 2,4,6,8,10))



###regions grouped

sb_ra3 <- sb_ra2 %>%
  mutate(`log(n_birds+1)` = log(n_birds + 1)) %>%
  mutate(`n_birds+1` = n_birds + 1)

ggplot(sb_ra3, aes(y= `log(n_birds+1)`, x = Species, colour = time_period)) +
  geom_boxplot()
  
ggplot(sb_ra3, aes(y= `n_birds+1`, x = Species, colour = time_period)) +
  geom_boxplot() +
  scale_y_log10()



ggplot(test3, aes(y= n_birds, x = time_period, colour = Species)) +
  geom_line(size = 1) +
  geom_errorbar(aes(ymin=n_birds-ci, ymax=n_birds+ci), width=1) +
  scale_x_continuous(breaks = c(1994, 2019)) +
  scale_y_continuous(breaks = c(0, 2,4,6,8,10))


#figure out a way to manually specify boxplots

ggplot(test3, aes(y= n_birds, x = time_period)) +
  geom_point(size = 1) +
  geom_errorbar(aes(ymin=n_birds-ci, ymax=n_birds+ci), width=1) +
  facet_wrap(~Species, scales = "free_y") +
  scale_x_continuous(breaks = c(1994, 2019)) 

ggplot(test3, aes(y= n_birds, x = time_period)) +
  geom_point(size = 1) +
  geom_errorbar(aes(ymin=n_birds-ci, ymax=n_birds+ci), width=1) +
  facet_wrap(~Species) +
  scale_x_continuous(breaks = c(1994, 2019))  +
  scale_y_log10()




#Is rhere a relationship between STI and change over time per species?

ras_coef$region <- "Rasmussen"
pci_coef$region <- "PCI"

coefs <- rbind(ras_coef, pci_coef)
coefs <- merge(coefs, STI, by.x = "Species", by.y = "code")

#remove intercepts where it seems like the model didn't work

coefs <- coefs %>%
  filter(SE < 5)


ggplot(coefs, aes(x = STI, y = B, cololur = Species)) +
  geom_point()

sti_lm <- lm(B~STI, data = coefs)
summary(sti_lm)

ggplotRegression(lm(B~STI, data = coefs))



#species richness


richness <- sb_ra2 %>%
  filter(n_birds > 0) %>%
  select(Species, region, time_period) %>%
  distinct()

table(richness$region, richness$time_period)

