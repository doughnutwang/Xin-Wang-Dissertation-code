# Data Processing---------------------------------------------------------------


# PA's code------------------------------------------------------------------------------

rm(list=ls())

machine <- 1 # 1 for Xin's; 2 for PAS's

library(tidyverse)

# set working directory
wdir <- ifelse(machine == 1, "/Users/huard/Desktop/Dissertation!!!", "/Users/dbl0ps/Library/CloudStorage/OneDrive-DurhamUniversity/Documents/Current files/_Xin")

setwd(wdir)

# get in required data and check it
animal <- read.csv("data/Animal.csv", stringsAsFactors = F)
dim(animal)

pfiles <- list.files(path = "data", pattern = "Photo")
pdata <- lapply(pfiles, function(f){
  cat(f,"\n")
  dat <- read.csv(sprintf("data/%s",f),stringsAsFactors = F)
})
photo <- bind_rows(pdata)
dim(photo)

options <- read.csv("data/Options.csv", stringsAsFactors = F)
sites <- read.csv("data/Site.csv", stringsAsFactors = F)
udat <- read.csv("data/Upload.csv", stringsAsFactors = F)
psm <- read.csv("data/ProjectSiteMap.csv", stringsAsFactors = F)
UKstd <- read.csv("data/UKstdProjects.csv", stringsAsFactors = F)

# work out which sites we're interested in (only those attached to UK standard projects)
focal.projects <- UKstd %>% filter(UK_standard_or_not == 1)
focal.sites <- psm %>% filter(project_id %in% focal.projects$project_id) %>% select(site_id) %>% unique()

# we're only interested in uploads and pictures from those sites
udat <- udat %>% filter(site_id %in% focal.sites$site_id)
photo <- photo %>% filter(site_id %in% focal.sites$site_id)
# also, we're only interested in pictures with legitimate dates and that have been properly sequenced
photo <- photo %>% filter(sequence_id > 0)
photo <- photo %>% mutate(taken.date = as.Date(taken))
photo <- photo %>% filter(taken.date >= as.Date("2010/01/01"))

# ... and classifications of those pictures (not by anonymous Hancock users though)
animal <- animal %>% filter(photo_id %in% photo$photo_id, !person_id %in% 2470:2474)


# a bit of housekeeping because, at this stage, we no longer need the psm or UKstd
rm(psm, UKstd)

# ------------------------------------------------------------------------------

# get deployment dates for all relevant camera sites

# a really simple approach would just be to say that the first date and the last date
# aren't full days. However, all dates in between are

# so, let's set the first full and last full dates for each upload
udat <- udat %>% mutate(firstFull = as.Date(deployment_date) + 1,
                        lastFull = as.Date(collection_date) - 1)

# let's add an index, so we know which record is which and can see what we lose when 
# we remove dodgy records
udat <- udat %>% mutate(ctr = row_number())

# to be ruthless, we could simply say that if the last date is before the first date,
# or if the timestamp is before either, the user has messed up and we'll ignore their data
udat_clean <- udat %>% filter(timestamp > lastFull & lastFull >= firstFull)

# we can also get rid of those with ridiculously early or late (future) dates
early <- as.Date("2010/01/01")
late <- Sys.Date()
udat_clean <- udat_clean %>% filter(firstFull >= early & lastFull < late)

# we lose a lot of data but mostly cameras that were out for very short periods.

# now, for each site, we want to know which days the camera was active on
# (so that, subsequently, we can work out whether a given species was sighted
# on that day, or not)

# for each upload, we can generate a sequence of the full days:
fullDays <- lapply(1:nrow(udat_clean), function(r){
  site <- udat_clean$site_id[r]
  firstday <- udat_clean$firstFull[r]
  lastday <- udat_clean$lastFull[r]
  fulldates <- seq(firstday, lastday, by=1)
  return(data.frame(site_id=site, date=fulldates))
})
fullDays <- bind_rows(fullDays)                        
fullDays <- fullDays %>% distinct()

# these, then, are the sites and days for which we need to know whether or not 
# a species was seen
if(!dir.exists("output")) dir.create("output")
write.table(fullDays, "output/site-days.csv", row.names = F, sep = ",")

# how many sequences were captured on each of these full days?
seqs <- photo %>% select(site_id,taken.date,sequence_id) %>% distinct()

# now, just take the earliest taken date for any sequence (to avoid sequences that started before 
# midnight and ended after midnight from being allocated to > 1 day)
seqs <- seqs %>% arrange(site_id, sequence_id, taken.date) %>% distinct(site_id, sequence_id, .keep_all = T)
# now, how many sequences are there per site and date?
seqNs <- seqs %>% group_by(site_id, taken.date) %>% summarise(Nseqs = length(unique(sequence_id)))

# we need to find how many of these sequences have been classified
# first, get the sequence_id into the animal table and remove those classified as "Like"
animal <- animal %>% filter(species != 97) %>% left_join(select(photo, photo_id, sequence_id))
seqsClassified <- animal %>% select(sequence_id) %>% distinct()

# get this info into the seqNs table
seqsClassifiedBySite <- seqs %>% filter(sequence_id %in% seqsClassified$sequence_id)
# now we can work out how many have been classified by site and date
seqNsClassified <- seqsClassifiedBySite %>% group_by(site_id, taken.date) %>% summarise(Nclass = length(unique(sequence_id)))

# now we're in a position to work out the proportion classified at each site on each date:
seqNs <- seqNs %>% left_join(seqNsClassified)

# those that meet the 0.95 threshold are worth looking at
seqNs.focal <- seqNs %>% filter(!is.na(Nclass)) 
seqNs.focal <- seqNs.focal %>% filter(Nclass/Nseqs > 0.95)

# ok, on which of those dates and in which of those sites has a badger been classified?
seqsBySite <- seqsClassifiedBySite %>% left_join(seqNs.focal)
# where Nseqs is NA, this is not one of our focal sites or dates, so remove it
seqsBySite <- seqsBySite %>% filter(!is.na(Nseqs))

# --------------------------------------------------------------------------------------

# find the badger classifications
badgers <- animal %>% filter(species == 10) %>% select(sequence_id, species) %>% distinct()
# and work out which of the sequences on the focal dates at the focal sites have / have not been
# classified as badgers
badgers <- seqsBySite %>% left_join(badgers)
badgers <- badgers %>% mutate(badger = ifelse(is.na(species),0,1)) %>% select(-species)

# now, we need to collapse this by date/site combination
badgers <- badgers %>% group_by(site_id, taken.date) %>% summarise(badger = max(badger))

# now, need that in wide format
badgers.wide <- badgers %>% pivot_wider(names_from = taken.date, values_from = badger)

# unite that info with site covariates
focal.sites <- sites %>% filter(site_id %in% badgers.wide$site_id) %>% 
  select(site_id,latitude,longitude,habitat_id,water_id,purpose_id,camera_id,camera_height)

# change covariates into descriptors
focal.sites <- focal.sites %>% 
  left_join(select(options, option_id, option_name), by = c("habitat_id" = "option_id")) %>%
  rename(habitat = option_name) %>%
  transform(habitat = ifelse(is.na(habitat),"Unknown",habitat))

focal.sites <- focal.sites %>% 
  left_join(select(options, option_id, option_name), by = c("water_id" = "option_id")) %>%
  rename(water = option_name) %>%
  transform(water = ifelse(is.na(water),"Unknown",water))

focal.sites <- focal.sites %>% 
  left_join(select(options, option_id, option_name), by = c("purpose_id" = "option_id")) %>%
  rename(purpose = option_name) %>%
  transform(purpose = ifelse(is.na(purpose),"Unknown",purpose))

focal.sites <- focal.sites %>% 
  left_join(select(options, option_id, option_name), by = c("camera_id" = "option_id")) %>%
  rename(camera = option_name) %>%
  transform(camera = ifelse(is.na(camera),"Unknown",camera))

# get rid of less informative columns
focal.sites <- focal.sites %>% select(-c(habitat_id:camera_id))

# make sure that the capture histories and site covariates are in the same order
focal.sites <- arrange(focal.sites, site_id)
badgers.wide <- arrange(badgers.wide, site_id)

# Reorder the badger.wide
badgers.wide <- badgers.wide %>%
  pivot_longer(cols = starts_with("20"),
               names_to = "date_column") %>%
  mutate(year = substr(date_column, 1, 4),
         month = substr(date_column, 6, 7),
         day = substr(date_column, 9, 10)) %>%
  select(-date_column)

badgers.wide <- badgers.wide %>%
  mutate(date = ymd(paste(year, month, day, sep = "-"))) %>%
  select(-year, -month, -day) %>%
  arrange(date)

badgers.wide <- badgers.wide %>%
  pivot_wider(names_from = date, values_from = value)

if(!dir.exists("output")) dir.create("output")
write.table(focal.sites, "output/focal.sites.csv", row.names = F, sep = ",")
write.table(badgers.wide, "output/badger.wide.csv", row.names = F, sep = ",")

# End of PA's code--------------------------------------------------------------


# Do Exploratory Data Analysis

library(tidyverse)

# set working directory
setwd("/Users/huard/Desktop/Dissertation!!!")

# get data on uploads
badger.wide <- read.csv("output/badger.wide.csv", check.names = FALSE, stringsAsFactors = FALSE)
focal.sites <- read.csv("output/focal.sites.csv",check.names = FALSE, stringsAsFactors = FALSE)

# explore the animal appearance during day and month by line plot

library(ggplot2)

# Get data to process - filter out the data that is out of the United Kingdom
EDA_data <- badger.wide

# Explore Badger detection frequency during days
# Mutate data type
non_na_sums <- EDA_data %>%
  summarise(across(starts_with("20"), ~ sum(!is.na(.)), .names = "sum_{.col}"))

# Reshape the data for plotting
non_na_melted <- non_na_sums %>%
  pivot_longer(cols = starts_with("sum_"), 
               names_to = "Date", 
               values_to = "NonNADetections") %>%
  mutate(Year = substr(Date, 5, 8))

frequency_plot <- ggplot(non_na_melted, aes(x = yday(substr(Date, 5, 14)), y = NonNADetections, color = Year)) +
  geom_line() +
  labs(x = "Day of Year", y = "Detection Count", color = "Year",
       title = "(a) Day Frequency of Badgers") +
  theme_minimal() +
  theme(plot.title = element_text(hjust = 0.5)) +
  theme(panel.background = element_rect(fill = "grey90"))

print(frequency_plot)

# Explore Badger detection frequency during months
# Reshape the data for plotting
non_na_melted2 <- non_na_sums %>%
  pivot_longer(cols = starts_with("sum_"), 
               names_to = "Date", 
               values_to = "NonNADetections") %>%
  mutate(Year = substr(Date, 5, 8))

# Convert Date to date format
non_na_melted2$Date <- as.Date(substr(non_na_melted2$Date,5,14))

# Extract year and month from Date
non_na_melted2 <- non_na_melted2 %>%
  mutate(Year = year(Date), Month = month(Date)) %>%
  group_by(Year, Month) %>%
  summarise(MonthlyDetectionCount = sum(NonNADetections))

# Create the frequency plot
frequency_plot2 <- ggplot(non_na_melted2, aes(x = Month, y = MonthlyDetectionCount, color = factor(Year))) +
  geom_line() +
  labs(x = "Month of Year", y = "Detection Count", color = "Year",
       title = "(b) Monthly Frequency of Badgers") +
  scale_x_continuous(breaks = 1:12, labels = month.abb) +
  theme_minimal() +
  theme(plot.title = element_text(hjust = 0.5)) +
  theme(panel.background = element_rect(fill = "grey90"))

print(frequency_plot2)

# explore frequency of detection count, detection count by year, detection count by season

data <- focal.sites %>% left_join(badger.wide)
data <- data %>%
  mutate(detection_count = rowSums(select(., starts_with("20")), na.rm = TRUE))
data_filtered <- data %>%
  filter(detection_count  >= 0 )
count <- sum(data_filtered$detection_count)
count

# frequency of detection counts

ggplot(data_filtered, aes(x = detection_count)) +
  geom_histogram(binwidth = 1, breaks = seq(0, 10, by = 1), color = "white", fill = "#A6C7E2") +
  labs(title = "(a) Frequncy of Detection Counts (All Sites)",
       x = "Detection Count",
       y = "Frequency") +
  theme_minimal() +
  theme(plot.title = element_text(hjust = 0.5)) +
  theme(panel.background = element_rect(fill = "white"))

# detection count by year

long_data <- badger.wide %>%
  pivot_longer(cols = starts_with("20"),  # Select columns starting with "20" (assuming the year)
               names_to = "date",  # New column for date information
               values_to = "detection")
counts_per_year <- long_data %>%
  mutate(year = substr(date, 1, 4)) %>%  # Extract year from the "date" column
  group_by(year) %>%
  summarize(total_detections = sum(detection, na.rm = TRUE))

counts_per_year$year <- as.character(counts_per_year$year)
sum(counts_per_year$total_detections)

# Create a bar plot
ggplot(counts_per_year, aes(x = year, y = total_detections)) +
  geom_bar(stat = "identity", fill = "#A6C7E2") +
  labs(title = "(b) Total Detection Counts of Badgers per Year (Natural Area)",
       x = "Year",
       y = "Total Detections") +
  theme_minimal()+
  theme(plot.title = element_text(hjust = 0.5)) +
  theme(panel.background = element_rect(fill = "white"))


#detection count per month

counts_per_month <- long_data %>%
  mutate(month = substr(date, 6, 7)) %>% 
  group_by(month) %>%
  summarize(total_detections = sum(detection, na.rm = TRUE))
counts_per_month$month <- as.numeric(counts_per_month$month)

counts_per_month$season <- case_when(
  counts_per_month$month %in% c(12, 1, 2) ~ "Winter",
  counts_per_month$month %in% c(3, 4, 5) ~ "Spring",
  counts_per_month$month %in% c(6, 7,8) ~ "Summer",
  counts_per_month$month %in% c(9, 10, 11) ~ "Fall"
)

counts_per_month$season <- factor(counts_per_month$season, levels = c("Spring", "Summer", "Fall", "Winter"))

ggplot(counts_per_month, aes(x = season, y = total_detections)) +
  geom_bar(stat = "identity", fill = "#A6C7E2") +
  labs(title = "(c) Total Detection Counts of Badgers per Season (Natural Area)",
       x = "Season",
       y = "Detection Count") +
  theme_minimal() +
  theme(plot.title = element_text(hjust = 0.5)) +
  theme(panel.background = element_rect(fill = "white"))



# Occupancy modelling-----------------------------------------------------------

# before doing the following, please delete the global environment and start from scratch

library(tidyverse)

# set working directory
setwd("/Users/huard/Desktop/Dissertation!!!")

# get data on uploads
badger.wide <- read.csv("output/badger.wide.csv", check.names = FALSE, stringsAsFactors = FALSE)
focal.sites <- read.csv("output/focal.sites.csv",check.names = FALSE, stringsAsFactors = FALSE)

#(for all the sites for occupancy model)

# define specific study area: urban area and natural area
# for the area : urban area(Durham and Newcasle)

min_lon <- -1.8000
max_lon <- -1.3700
min_lat <- 54.7300
max_lat <- 55.0800
# Filter the data within North West
Urban.focal.sites <- focal.sites %>%
  filter(longitude >= min_lon &
           longitude <= max_lon &
           latitude >= min_lat &
           latitude <= max_lat)

# for the area: natural area(east of scottland mountainside)
min_lon <- -4.1600
max_lon <- -2.3800
min_lat <- 57.110
max_lat <- 57.4900
# Filter the data within North West
Natural.focal.sites <- focal.sites %>%
  filter(longitude >= min_lon &
           longitude <= max_lon &
           latitude >= min_lat &
           latitude <= max_lat)

# --------------------------------------------------------------------------------------

#filter data

Urban.badger.wide <- badger.wide %>% filter(site_id %in% Urban.focal.sites$site_id)
Natural.badger.wide <- badger.wide %>% filter(site_id %in% Natural.focal.sites$site_id)

# ---------------------------------------------------------------------------------

# Draw the sites map for all the sites

# install.packages("leaflet")
library(leaflet)
library(ggplot2)

# sites in urban area
Urban.leaflet_map <- leaflet() %>%
  addProviderTiles("OpenTopoMap")  %>%
  addCircleMarkers(data = Urban.focal.sites, ~longitude, ~latitude,radius = 0.5,
                   popup = ~site_id)
Urban.leaflet_map

# sites in natural area
Natural.leaflet_map <- leaflet() %>%
  addProviderTiles("OpenTopoMap")  %>%
  addCircleMarkers(data = Natural.focal.sites, ~longitude, ~latitude,radius = 0.5,
                   popup = ~site_id)
Natural.leaflet_map

# do occuppancy modelling

library(unmarked)

# Extract the detection non-detection data
y = badger.wide[,-1]
str(y)

# Extract site covariates
for (i in 5:8) focal.sites[,i] <- as.factor(focal.sites[,i])
siteCovs <- focal.sites[,c(2:8)]
str(siteCovs)

# Do correlation analysis
library(corrplot)
cor_matrix <- cor(siteCovs[1:3])
cor_df <- as.data.frame(as.table(cor_matrix))

# Create a correlation heatmap
ggplot(cor_df, aes(Var1, Var2, fill = Freq)) +
  geom_tile() +
  scale_fill_gradient(low = "white", high = "skyblue") +
  theme_minimal() +  
  theme(plot.title = element_text(hjust = 0.5)) +
  theme(panel.background = element_rect(fill = "white"))+
  labs(title = "Correlation Heatmap")

#PA's code----------------------------------------------------------------------

#check sampling data
sampling.dates <- colnames(badger.wide)
sampling.dates <- data.frame(col = 2:dim(badger.wide)[2], date = as.Date(sampling.dates[-1]))

# we can split these up in many ways. First, let's compute some useful variables.
sampling.dates <- sampling.dates %>% mutate(year = year(date),
                                            month = month(date),
                                            day = yday(date))

# we can make a lookup for seasons, to make it quick to add those
seasonLU <- data.frame(month = 1:12, season = c(rep("winter",2),rep("spring",3),rep("summer",3),rep("autumn",3),"winter"))
sampling.dates <- sampling.dates %>% left_join(seasonLU)


#  for spring-------------------------------------------------------------------

# filter out data for spring
springs <- sampling.dates %>% filter(season == "spring" & year >= 2015)

#build occupancy model with no covariates for spring during 2016 to 2022 year
spring.mods <- lapply(2016:2022, function(yr){
  cat("Fitting mod for spring",yr,"...\n")
  # get the column numbers of autumn of that year
  spring.cols <- springs %>% filter(year == yr) %>% select(col) %>% unlist(.) %>% as.integer(.)
  # restrict data of interest to just the relevant columns
  relevant.dat <- badger.wide[,spring.cols]
  # find rows that are all NA
  ncols <- ncol(relevant.dat)
  nNAs <- apply(relevant.dat,1, function(x) length(which(is.na(x))))
  to.remove <- which(nNAs == ncols)
  # remove those rows from the capture history (and, if necessary, the site.covs)
  y <- relevant.dat[-to.remove,]
  umf <-unmarkedFrameOccu(y=as.matrix(y),
                          siteCovs = siteCovs[-to.remove,])
  sprmod <- occu(~ 1 ~ 1, data = umf)
  z1 <- summary(sprmod)$state
  psiLO = as.numeric(z1[1])
  psiLO.se = as.numeric(z1[2])
  psi <- exp(psiLO)/(1+exp(psiLO))
  psi.lo <- exp(psiLO-psiLO.se)/(1+exp(psiLO-psiLO.se))
  psi.hi <- exp(psiLO+psiLO.se)/(1+exp(psiLO+psiLO.se))
  z2 <- summary(sprmod)$det
  detLO = as.numeric(z2[1])
  detLO.se = as.numeric(z2[2])
  det <- exp(detLO)/(1+exp(detLO))
  det.lo <- exp(detLO-detLO.se)/(1+exp(detLO-detLO.se))
  det.hi <- exp(detLO+detLO.se)/(1+exp(detLO+detLO.se))
  return(data.frame(year = yr, nsites = nrow(y),
                    psi,psi.lo,psi.hi,p.psi = as.numeric(z1[4]),
                    det,det.lo,det.hi, p.det = as.numeric(z2[4])))
})
spring.mods <- bind_rows(spring.mods)
spring.mods

#end of PA's code---------------------------------------------------------------

# creat plot to examine the occupancy and detecability of occupancy model
data <- data.frame(
  year = c(2016, 2017, 2018, 2019, 2020, 2021, 2022),
  nsites = c(160, 162, 226, 131, 105, 75, 65),
  psi = c(0.4988963, 0.3215626, 0.3058220, 0.3639829, 0.4073949, 0.4276084, 0.2398177),
  psi.lo = c(0.4382638, 0.2739345, 0.2631724, 0.3083263, 0.3530852, 0.3560218, 0.1805218),
  psi.hi = c(0.5595612, 0.3732150, 0.3520806, 0.4235344, 0.4640658, 0.5023606, 0.3111942),
  p.psi = c(9.855519e-01, 1.066417e-03, 9.351971e-05, 2.548791e-02, 1.043842e-01, 3.327267e-01, 1.316467e-03),
  det = c(0.1320159, 0.1454760, 0.1395442, 0.1217072, 0.2431670, 0.1983338, 0.2918208),
  det.lo = c(0.1190932, 0.1297209, 0.1241864, 0.1084961, 0.2280295, 0.1792825, 0.2618562),
  det.hi = c(0.1461083, 0.1627868, 0.1564619, 0.1362811, 0.2589723, 0.2188697, 0.3237105),
  p.det = c(1.559365e-57, 1.708334e-40, 8.358588e-42, 2.517118e-52, 1.477315e-41, 3.214796e-29, 3.242399e-09)
)

ggplot(data, aes(x = year)) +
  geom_line(aes(y = psi, color = "Psi"), linewidth = 1) +
  geom_line(aes(y = det, color = "Det"), linewidth = 1) +
  geom_ribbon(aes(ymin = psi.lo, ymax = psi.hi, fill = "Psi"), alpha = 0.2) +
  geom_ribbon(aes(ymin = det.lo, ymax = det.hi, fill = "Det"), alpha = 0.2) +
  scale_color_manual(values = c("Psi" = "blue", "Det" = "red")) +
  scale_fill_manual(values = c("Psi" = "blue", "Det" = "red")) +
  labs(title = "(a) Occupancy and Detection Probability in Spring",
       x = "Year",
       y = "Probability") +
  theme_minimal() +
  theme(plot.title = element_text(hjust = 0.5)) +
  theme(panel.background = element_rect(fill = "white"))

# adding covariate

siteCovs <- focal.sites[,c(2:8)]

# examine models with different covariate, compare their AIC and find the best model with 
#covariate habitat

umf <-unmarkedFrameOccu(y=as.matrix(y),
                        siteCovs = siteCovs)
occ <- occu(~ 1 ~ 1, data = umf)
occ1 <- occu(~ 1 ~ habitat, data = umf)
occ2 <- occu(~ 1 ~ habitat+water, data = umf)
occ3 <- occu(~ 1 ~ habitat+water+purpose, data = umf)
occ4 <- occu(~ 1 ~ habitat+water+purpose+camera, data = umf)
occ5 <- occu(~ 1 ~ habitat+water+purpose+camera+camera_height, data = umf)
occ6 <- occu(~ 1 ~ habitat+water+purpose+camera+camera_height+latitude+longitude, data = umf)
occ7 <- occu(~ 1 ~ purpose, data = umf)
occ8 <- occu(~ 1 ~ latitude+longitude, data = umf)
occ
occ1
occ2
occ3
occ4
occ5
occ6
occ7
occ8

# prediction for all habitat for year 2018

cols <- springs %>% filter(year == 2018) %>% select(col) %>% unlist(.) %>% as.integer(.)
y <- badger.wide[,cols]
umf <-unmarkedFrameOccu(y=as.matrix(y),
                        siteCovs = siteCovs)
occ1 <- occu(~ 1 ~ habitat, data = umf)

newdata <- data.frame(habitat = c("coastal - right on the coast, beach",
                                  "woodland - low density forest less than 60% canopy cover",
                                  "Unknown",
                                  "riverbank - right on the riverbank",
                                  "garden - like a backyard garden, probably right next to a residence",
                                  "scrubland - dominated by shrubs, i.e. small to medium woody plants less than 8 m high",
                                  "grassland - dominated by grasses",
                                  "forest - high density forest more than 60% canopy cover",
                                  "residential - houses, apartments, etc.",
                                  "farmland - pasture, etc.",
                                  "swamp - a forested wetland",
                                  "park - recreational place",
                                  "marsh - a wetland dominated by herbaceous, i.e. non-woody plants",
                                  "heath - a kind of scrubland characterised by open, low-growing woody plants less than 2 m high",
                                  "industrial - factories and warehouses",
                                  "bog - a wetland with few/no trees, some shrubs, with lots of peat accumulation",
                                  "rocky - lots of bare rocks with little vegetation",
                                  "commercial - stores and offices"))
Epsi <- predict(occ1, type="state", newdata=newdata)

with(Epsi, {
  plot(1:18, Predicted, xaxt="n", xlim=c(0.5, 18.5),ylim = c(0,1),
       xlab="Habitat",
       ylab=expression(paste("Probability of occurrence (", psi, ")")),
       cex.lab=1.2,
       pch=16, cex=1.5, main = "Probability of Occupancy for Habitat (Spring)")
  axis(1, 1:18, c(habitat = c("coastal",
                              "woodland",
                              "Unknown",
                              "riverbank",
                              "garden",
                              "scrubland",
                              "grassland",
                              "forest",
                              "residential",
                              "farmland",
                              "swamp",
                              "park",
                              "marsh",
                              "heath",
                              "industrials",
                              "bog",
                              "rocky",
                              "commercial")))
  arrows(1:18, lower, 1:18, upper, angle=90, code=3, length=0.05)
})

# test occupancy model with less survey days
springs <- sampling.dates %>% filter(season == "spring" & year >= 2015 & day %in% c(65:80,97:109,128:141))

spring.mods.cov2 <- lapply(2016:2022, function(yr){
  cat("Fitting mod for spring",yr,"...\n")
  # get the column numbers of autumn of that year
  spring.cols <- springs %>% filter(year == yr) %>% select(col) %>% unlist(.) %>% as.integer(.)
  # restrict data of interest to just the relevant columns
  relevant.dat <- badger.wide[,spring.cols]
  # find rows that are all NA
  ncols <- ncol(relevant.dat)
  nNAs <- apply(relevant.dat,1, function(x) length(which(is.na(x))))
  to.remove <- which(nNAs == ncols)
  # remove those rows from the capture history (and, if necessary, the site.covs)
  y <- relevant.dat[-to.remove,]
  umf <-unmarkedFrameOccu(y=as.matrix(y),
                          siteCovs = siteCovs[-to.remove,])
  sprmod <- occu(~ 1 ~ purpose, data = umf)
  print(sprmod)})

# for summer--------------------------------------------------------------------

summers <- sampling.dates %>% filter(season == "summer" & year >= 2015)

summer.mods <- lapply(2016:2022, function(yr){
  cat("Fitting mod for winter",yr,"...\n")
  # get the column numbers of winter of that year
  summer.cols <- summers %>% filter(year == yr) %>% select(col) %>% unlist(.) %>% as.integer(.)
  # restrict data of interest to just the relevant columns
  relevant.dat <- badger.wide[,summer.cols]
  # find rows that are all NA
  ncols <- ncol(relevant.dat)
  nNAs <- apply(relevant.dat,1, function(x) length(which(is.na(x))))
  to.remove <- which(nNAs == ncols)
  # remove those rows from the capture history (and, if necessary, the site.covs)
  y <- relevant.dat[-to.remove,]
  umf <-unmarkedFrameOccu(y=as.matrix(y),
                          siteCovs = siteCovs[-to.remove,])
  sumtmod <- occu(~ 1 ~ 1, data = umf)
  z1 <- summary(sumtmod)$state
  psiLO = as.numeric(z1[1])
  psiLO.se = as.numeric(z1[2])
  psi <- exp(psiLO)/(1+exp(psiLO))
  psi.lo <- exp(psiLO-psiLO.se)/(1+exp(psiLO-psiLO.se))
  psi.hi <- exp(psiLO+psiLO.se)/(1+exp(psiLO+psiLO.se))
  z2 <- summary(sumtmod)$det
  detLO = as.numeric(z2[1])
  detLO.se = as.numeric(z2[2])
  det <- exp(detLO)/(1+exp(detLO))
  det.lo <- exp(detLO-detLO.se)/(1+exp(detLO-detLO.se))
  det.hi <- exp(detLO+detLO.se)/(1+exp(detLO+detLO.se))
  return(data.frame(year = yr, nsites = nrow(y),
                    psi,psi.lo,psi.hi,p.psi = as.numeric(z1[4]),
                    det,det.lo,det.hi, p.det = as.numeric(z2[4])))
})
summer.mods <- bind_rows(summer.mods)
summer.mods

# print plot to examine the occupancy rate and detecability in summer
data <- data.frame(
  year = c(2016, 2017, 2018, 2019, 2020, 2021, 2022),
  nsites = c(47, 60, 131, 135, 42, 55, 38),
  psi = c(0.3618990, 0.1081771, 0.5337149, 0.2014637, 0.4909039, 0.5868607, 0.4459333),
  psi.lo = c(0.2572830, 0.0699801, 0.4690225, 0.1565875, 0.3731558, 0.4878332, 0.3337901),
  psi.hi = c(0.4814774, 0.1635565, 0.5972928, 0.2553078, 0.6096700, 0.6793274, 0.5638645),
  p.psi = c(2.499993e-01, 9.970040e-06, 6.022166e-01, 7.098081e-06, 9.398602e-01, 3.798298e-01, 6.469002e-01),
  det = c(0.06247809, 0.13498205, 0.09565675, 0.17876871, 0.12560458, 0.08942949, 0.23807116),
  det.lo = c(0.05131113, 0.11018088, 0.08574931, 0.15470758, 0.10864553, 0.07908297, 0.21172969),
  det.hi = c(0.07588096, 0.16433483, 0.10657545, 0.20566152, 0.14478087, 0.10098122, 0.26658150),
  p.det = c(1.705978e-38, 9.617997e-16, 6.692758e-78, 1.478251e-18, 3.339632e-32, 6.037147e-67, 1.456715e-14)
)

ggplot(data, aes(x = year)) +
  geom_line(aes(y = psi, color = "Psi"), linewidth = 1) +
  geom_line(aes(y = det, color = "Det"), linewidth = 1) +
  geom_ribbon(aes(ymin = psi.lo, ymax = psi.hi, fill = "Psi"), alpha = 0.2) +
  geom_ribbon(aes(ymin = det.lo, ymax = det.hi, fill = "Det"), alpha = 0.2) +
  scale_color_manual(values = c("Psi" = "blue", "Det" = "red")) +
  scale_fill_manual(values = c("Psi" = "blue", "Det" = "red")) +
  labs(title = "(b) Occupancy and Detection Probability in Summer",
       x = "Year",
       y = "Probability") +
  theme_minimal() +
  theme(plot.title = element_text(hjust = 0.5)) +
  theme(panel.background = element_rect(fill = "white"))

# add covariate
summer.mods.cov2 <- lapply(2016:2022, function(yr){
  cat("Fitting mod for summer",yr,"...\n")
  # get the column numbers of autumn of that year
  summer.cols <- summers %>% filter(year == yr) %>% select(col) %>% unlist(.) %>% as.integer(.)
  # restrict data of interest to just the relevant columns
  relevant.dat <- badger.wide[,summer.cols]
  # find rows that are all NA
  ncols <- ncol(relevant.dat)
  nNAs <- apply(relevant.dat,1, function(x) length(which(is.na(x))))
  to.remove <- which(nNAs == ncols)
  # remove those rows from the capture history (and, if necessary, the site.covs)
  y <- relevant.dat[-to.remove,]
  umf <-unmarkedFrameOccu(y=as.matrix(y),
                          siteCovs = siteCovs[-to.remove,])
  summod <- occu(~ 1 ~ purpose, data = umf)
  print(summod)})

# adding covariates

umf <-unmarkedFrameOccu(y=as.matrix(y),
                        siteCovs = siteCovs)
occ <- occu(~ 1 ~ water, data = umf)
occ1 <- occu(~ 1 ~ habitat, data = umf)
occ2 <- occu(~ 1 ~ habitat+water, data = umf)
occ3 <- occu(~ 1 ~ habitat+water+purpose, data = umf)
occ4 <- occu(~ 1 ~ habitat+water+purpose+camera, data = umf)
occ5 <- occu(~ 1 ~ habitat+water+purpose+camera+camera_height, data = umf)
occ6 <- occu(~ 1 ~ habitat+water+purpose+camera+camera_height+latitude+longitude, data = umf)
occ7 <- occu(~ 1 ~ purpose, data = umf)
occ8 <- occu(~ 1 ~ latitude+longitude, data = umf)
occ
occ1
occ2
occ3
occ4
occ5
occ6
occ7
occ8

# prediction for all habitat

cols <- summers %>% filter(year == 2018) %>% select(col) %>% unlist(.) %>% as.integer(.)
y <- badger.wide[,cols]
umf <-unmarkedFrameOccu(y=as.matrix(y),
                        siteCovs = siteCovs)
occ1 <- occu(~ 1 ~ habitat, data = umf)

newdata <- data.frame(siteCovs=c("habitat"),
                      habitat = c("coastal - right on the coast, beach",
                                  "woodland - low density forest less than 60% canopy cover",
                                  "Unknown",
                                  "riverbank - right on the riverbank",
                                  "garden - like a backyard garden, probably right next to a residence",
                                  "scrubland - dominated by shrubs, i.e. small to medium woody plants less than 8 m high",
                                  "grassland - dominated by grasses",
                                  "forest - high density forest more than 60% canopy cover",
                                  "residential - houses, apartments, etc.",
                                  "farmland - pasture, etc.",
                                  "swamp - a forested wetland",
                                  "park - recreational place",
                                  "marsh - a wetland dominated by herbaceous, i.e. non-woody plants",
                                  "heath - a kind of scrubland characterised by open, low-growing woody plants less than 2 m high",
                                  "industrial - factories and warehouses",
                                  "bog - a wetland with few/no trees, some shrubs, with lots of peat accumulation",
                                  "rocky - lots of bare rocks with little vegetation",
                                  "commercial - stores and offices"))
Epsi <- predict(occ1, type="state", newdata=newdata)

with(Epsi, {
  plot(1:18, Predicted, xaxt="n", xlim=c(0.5, 18.5),ylim = c(0,1),
       xlab="Habitat",
       ylab=expression(paste("Probability of occurrence (", psi, ")")),
       cex.lab=1.2,
       pch=16, cex=1.5, main = "Probability of Occupancy for Habitat (Summer)")
  axis(1, 1:18, c(habitat = c("coastal",
                              "woodland",
                              "Unknown",
                              "riverbank",
                              "garden",
                              "scrubland",
                              "grassland",
                              "forest",
                              "residential",
                              "farmland",
                              "swamp",
                              "park",
                              "marsh",
                              "heath",
                              "industrials",
                              "bog",
                              "rocky",
                              "commercial")))
  arrows(1:18, lower, 1:18, upper, angle=90, code=3, length=0.05)
})

# for autumn

# filter out data for autumn
autumns <- sampling.dates %>% filter(season == "autumn" & year >= 2015)

autumn.mods <- lapply(2016:2022, function(yr){
  cat("Fitting mod for autumn",yr,"...\n")
  # get the column numbers of autumn of that year
  autumn.cols <- autumns %>% filter(year == yr) %>% select(col) %>% unlist(.) %>% as.integer(.)
  # restrict data of interest to just the relevant columns
  relevant.dat <- badger.wide[,autumn.cols]
  # find rows that are all NA
  ncols <- ncol(relevant.dat)
  nNAs <- apply(relevant.dat,1, function(x) length(which(is.na(x))))
  to.remove <- which(nNAs == ncols)
  # remove those rows from the capture history (and, if necessary, the site.covs)
  y <- relevant.dat[-to.remove,]
  umf <-unmarkedFrameOccu(y=as.matrix(y),
                          siteCovs = siteCovs[-to.remove,])
  autmod <- occu(~ 1 ~ 1, data = umf)
  z1 <- summary(autmod)$state
  psiLO = as.numeric(z1[1])
  psiLO.se = as.numeric(z1[2])
  psi <- exp(psiLO)/(1+exp(psiLO))
  psi.lo <- exp(psiLO-psiLO.se)/(1+exp(psiLO-psiLO.se))
  psi.hi <- exp(psiLO+psiLO.se)/(1+exp(psiLO+psiLO.se))
  z2 <- summary(autmod)$det
  detLO = as.numeric(z2[1])
  detLO.se = as.numeric(z2[2])
  det <- exp(detLO)/(1+exp(detLO))
  det.lo <- exp(detLO-detLO.se)/(1+exp(detLO-detLO.se))
  det.hi <- exp(detLO+detLO.se)/(1+exp(detLO+detLO.se))
  return(data.frame(year = yr, nsites = nrow(y),
                    psi,psi.lo,psi.hi,p.psi = as.numeric(z1[4]),
                    det,det.lo,det.hi, p.det = as.numeric(z2[4])))
})
autumn.mods <- bind_rows(autumn.mods)
autumn.mods

data <- data.frame(
  year = c(2016, 2017, 2018, 2019, 2020, 2021, 2022),
  nsites = c(44, 49, 110, 76, 32, 53, 35),
  psi = c(0.3806455, 0.1887810, 0.4923498, 0.3198682, 0.5029009, 0.2061102, 0.3372946),
  psi.lo = c(0.2842932, 0.1213478, 0.4178673, 0.2528864, 0.3784770, 0.1508160, 0.2163245),
  psi.hi = c(0.4874142, 0.2816740, 0.5671733, 0.3952066, 0.6269666, 0.2751093, 0.4841222),
  p.psi = c(0.2646917754, 0.0052028442, 0.9189982730, 0.0218110133, 0.9817626327, 0.0003826788, 0.2696631678),
  det = c(0.06002715, 0.08556325, 0.06307157, 0.16150943, 0.15193673, 0.27104758, 0.13217861),
  det.lo = c(0.04927191, 0.06946771, 0.05463000, 0.14168648, 0.13128398, 0.24311396, 0.10592236),
  det.hi = c(0.07294995, 0.10496742, 0.07271721, 0.18351279, 0.17518326, 0.30091474, 0.16375128),
  p.det = c(1.233941e-39, 9.617968e-26, 6.432563e-70, 1.356078e-26, 5.249789e-24, 1.388789e-11, 6.900671e-14)
)

ggplot(data, aes(x = year)) +
  geom_line(aes(y = psi, color = "Psi"), linewidth = 1) +
  geom_line(aes(y = det, color = "Det"), linewidth = 1) +
  geom_ribbon(aes(ymin = psi.lo, ymax = psi.hi, fill = "Psi"), alpha = 0.2) +
  geom_ribbon(aes(ymin = det.lo, ymax = det.hi, fill = "Det"), alpha = 0.2) +
  scale_color_manual(values = c("Psi" = "blue", "Det" = "red")) +
  scale_fill_manual(values = c("Psi" = "blue", "Det" = "red")) +
  labs(title = "(c) Occupancy and Detection Probability in Autumn",
       x = "Year",
       y = "Probability") +
  theme_minimal() +
  theme(plot.title = element_text(hjust = 0.5)) +
  theme(panel.background = element_rect(fill = "white"))

# add covariate

umf <-unmarkedFrameOccu(y=as.matrix(y),
                        siteCovs = siteCovs)
occ <- occu(~ 1 ~ water, data = umf)
occ1 <- occu(~ 1 ~ habitat, data = umf)
occ2 <- occu(~ 1 ~ habitat+water, data = umf)
occ3 <- occu(~ 1 ~ habitat+water+purpose, data = umf)
occ4 <- occu(~ 1 ~ habitat+water+purpose+camera, data = umf)
occ5 <- occu(~ 1 ~ habitat+water+purpose+camera+camera_height, data = umf)
occ6 <- occu(~ 1 ~ habitat+water+purpose+camera+camera_height+latitude+longitude, data = umf)
occ7 <- occu(~ 1 ~ purpose, data = umf)
occ8 <- occu(~ 1 ~ latitude+longitude, data = umf)
occ
occ1
occ2
occ3
occ4
occ5
occ6
occ7
occ8

# predict for all habitat types

cols <- autumns %>% filter(year == 2018) %>% select(col) %>% unlist(.) %>% as.integer(.)
y <- badger.wide[,cols]
umf <-unmarkedFrameOccu(y=as.matrix(y),
                        siteCovs = siteCovs)
occ1 <- occu(~ 1 ~ habitat, data = umf)

newdata <- data.frame(habitat = c("coastal - right on the coast, beach",
                                  "woodland - low density forest less than 60% canopy cover",
                                  "Unknown",
                                  "riverbank - right on the riverbank",
                                  "garden - like a backyard garden, probably right next to a residence",
                                  "scrubland - dominated by shrubs, i.e. small to medium woody plants less than 8 m high",
                                  "grassland - dominated by grasses",
                                  "forest - high density forest more than 60% canopy cover",
                                  "residential - houses, apartments, etc.",
                                  "farmland - pasture, etc.",
                                  "swamp - a forested wetland",
                                  "park - recreational place",
                                  "marsh - a wetland dominated by herbaceous, i.e. non-woody plants",
                                  "heath - a kind of scrubland characterised by open, low-growing woody plants less than 2 m high",
                                  "industrial - factories and warehouses",
                                  "bog - a wetland with few/no trees, some shrubs, with lots of peat accumulation",
                                  "rocky - lots of bare rocks with little vegetation",
                                  "commercial - stores and offices"))
Epsi <- predict(occ1, type="state", newdata=newdata)

with(Epsi, {
  plot(1:18, Predicted, xaxt="n", xlim=c(0.5, 18.5),ylim = c(0,1),
       xlab="Habitat",
       ylab=expression(paste("Probability of occurrence (", psi, ")")),
       cex.lab=1.2,
       pch=16, cex=1.5, main = "Probability of Occupancy for Habitat (Autumn)")
  axis(1, 1:18, c(habitat = c("coastal",
                              "woodland",
                              "Unknown",
                              "riverbank",
                              "garden",
                              "scrubland",
                              "grassland",
                              "forest",
                              "residential",
                              "farmland",
                              "swamp",
                              "park",
                              "marsh",
                              "heath",
                              "industrials",
                              "bog",
                              "rocky",
                              "commercial")))
  arrows(1:18, lower, 1:18, upper, angle=90, code=3, length=0.05)
})

# for winter

winters <- sampling.dates %>% filter(season == "winter" & year >= 2015)

winter.mods <- lapply(2016:2022, function(yr){
  cat("Fitting mod for winter",yr,"...\n")
  # get the column numbers of winter of that year
  winter.cols <- winters %>% filter(year == yr) %>% select(col) %>% unlist(.) %>% as.integer(.)
  # restrict data of interest to just the relevant columns
  relevant.dat <- badger.wide[,winter.cols]
  # find rows that are all NA
  ncols <- ncol(relevant.dat)
  nNAs <- apply(relevant.dat,1, function(x) length(which(is.na(x))))
  to.remove <- which(nNAs == ncols)
  # remove those rows from the capture history (and, if necessary, the site.covs)
  y <- relevant.dat[-to.remove,]
  umf <-unmarkedFrameOccu(y=as.matrix(y),
                          siteCovs = siteCovs[-to.remove,])
  wintmod <- occu(~ 1 ~ 1, data = umf)
  z1 <- summary(wintmod)$state
  psiLO = as.numeric(z1[1])
  psiLO.se = as.numeric(z1[2])
  psi <- exp(psiLO)/(1+exp(psiLO))
  psi.lo <- exp(psiLO-psiLO.se)/(1+exp(psiLO-psiLO.se))
  psi.hi <- exp(psiLO+psiLO.se)/(1+exp(psiLO+psiLO.se))
  z2 <- summary(wintmod)$det
  detLO = as.numeric(z2[1])
  detLO.se = as.numeric(z2[2])
  det <- exp(detLO)/(1+exp(detLO))
  det.lo <- exp(detLO-detLO.se)/(1+exp(detLO-detLO.se))
  det.hi <- exp(detLO+detLO.se)/(1+exp(detLO+detLO.se))
  return(data.frame(year = yr, nsites = nrow(y),
                    psi,psi.lo,psi.hi,p.psi = as.numeric(z1[4]),
                    det,det.lo,det.hi, p.det = as.numeric(z2[4])))
})
winter.mods <- bind_rows(winter.mods)
winter.mods

data <- data.frame(
  year = c(2016, 2017, 2018, 2019, 2020, 2021, 2022),
  nsites = c(318, 255, 320, 207, 238, 54, 56),
  psi = c(0.2893793, 0.3042874, 0.2578809, 0.2501220, 0.2532256, 0.3448337, 0.5445137),
  psi.lo = c(0.2560127, 0.2636180, 0.2271059, 0.2075600, 0.2204631, 0.2744495, 0.4527684),
  psi.hi = c(0.3251938, 0.3482637, 0.2912547, 0.2981282, 0.2890515, 0.4227510, 0.6333340),
  p.psi = c(9.552443e-08, 3.644062e-05, 2.929369e-10, 5.571886e-06, 2.537989e-09, 5.202229e-02, 6.276023e-01),
  det = c(0.1640682, 0.1909245, 0.2202503, 0.1114815, 0.2329097, 0.2226347, 0.1739337),
  det.lo = c(0.15108411, 0.17539553, 0.20105357, 0.09501985, 0.21617433, 0.20254160, 0.15287827),
  det.hi = c(0.1779342, 0.2074823, 0.2407278, 0.1303842, 0.2505267, 0.2441110, 0.1972140),
  p.det = c(3.636277e-62, 5.875845e-44, 7.086958e-28, 2.224121e-31, 2.710873e-35, 2.218627e-25, 5.322535e-24)
)

ggplot(data, aes(x = year)) +
  geom_line(aes(y = psi, color = "Psi"), linewidth = 1) +
  geom_line(aes(y = det, color = "Det"), linewidth = 1) +
  geom_ribbon(aes(ymin = psi.lo, ymax = psi.hi, fill = "Psi"), alpha = 0.2) +
  geom_ribbon(aes(ymin = det.lo, ymax = det.hi, fill = "Det"), alpha = 0.2) +
  scale_color_manual(values = c("Psi" = "blue", "Det" = "red")) +
  scale_fill_manual(values = c("Psi" = "blue", "Det" = "red")) +
  labs(title = "(d) Occupancy and Detection Probability in Winter",
       x = "Year",
       y = "Probability") +
  theme_minimal() +
  theme(plot.title = element_text(hjust = 0.5)) +
  theme(panel.background = element_rect(fill = "white"))

# add covariate

winter.mods.cov2 <- lapply(2016:2022, function(yr){
  cat("Fitting mod for spring",yr,"...\n")
  # get the column numbers of autumn of that year
  winter.cols <- winters %>% filter(year == yr) %>% select(col) %>% unlist(.) %>% as.integer(.)
  # restrict data of interest to just the relevant columns
  relevant.dat <- badger.wide[,winter.cols]
  # find rows that are all NA
  ncols <- ncol(relevant.dat)
  nNAs <- apply(relevant.dat,1, function(x) length(which(is.na(x))))
  to.remove <- which(nNAs == ncols)
  # remove those rows from the capture history (and, if necessary, the site.covs)
  y <- relevant.dat[-to.remove,]
  umf <-unmarkedFrameOccu(y=as.matrix(y),
                          siteCovs = siteCovs[-to.remove,])
  winmod <- occu(~ 1 ~ purpose, data = umf)
  print(winmod)})

# adding covariates

umf <-unmarkedFrameOccu(y=as.matrix(y),
                        siteCovs = siteCovs)
occ <- occu(~ 1 ~ water, data = umf)
occ1 <- occu(~ 1 ~ habitat, data = umf)
occ2 <- occu(~ 1 ~ habitat+water, data = umf)
occ3 <- occu(~ 1 ~ habitat+water+purpose, data = umf)
occ4 <- occu(~ 1 ~ habitat+water+purpose+camera, data = umf)
occ5 <- occu(~ 1 ~ habitat+water+purpose+camera+camera_height, data = umf)
occ6 <- occu(~ 1 ~ habitat+water+purpose+camera+camera_height+latitude+longitude, data = umf)
occ7 <- occu(~ 1 ~ purpose, data = umf)
occ8 <- occu(~ 1 ~ latitude+longitude, data = umf)
occ
occ1
occ2
occ3
occ4
occ5
occ6
occ7
occ8

# exmine predictions of all type of habitat

cols <- winters %>% filter(year == 2018) %>% select(col) %>% unlist(.) %>% as.integer(.)
y <- badger.wide[,cols]
umf <-unmarkedFrameOccu(y=as.matrix(y),
                        siteCovs = siteCovs)
occ1 <- occu(~ 1 ~ habitat, data = umf)

newdata <- data.frame(siteCovs=c("habitat"),
                      habitat = c("coastal - right on the coast, beach",
                                  "woodland - low density forest less than 60% canopy cover",
                                  "Unknown",
                                  "riverbank - right on the riverbank",
                                  "garden - like a backyard garden, probably right next to a residence",
                                  "scrubland - dominated by shrubs, i.e. small to medium woody plants less than 8 m high",
                                  "grassland - dominated by grasses",
                                  "forest - high density forest more than 60% canopy cover",
                                  "residential - houses, apartments, etc.",
                                  "farmland - pasture, etc.",
                                  "swamp - a forested wetland",
                                  "park - recreational place",
                                  "marsh - a wetland dominated by herbaceous, i.e. non-woody plants",
                                  "heath - a kind of scrubland characterised by open, low-growing woody plants less than 2 m high",
                                  "industrial - factories and warehouses",
                                  "bog - a wetland with few/no trees, some shrubs, with lots of peat accumulation",
                                  "rocky - lots of bare rocks with little vegetation",
                                  "commercial - stores and offices"))
Epsi <- predict(occ1, type="state", newdata=newdata)

with(Epsi, {
  plot(1:18, Predicted, xaxt="n", xlim=c(0.5, 18.5),ylim = c(0,1),
       xlab="Habitat",
       ylab=expression(paste("Probability of occurrence (", psi, ")")),
       cex.lab=1.2,
       pch=16, cex=1.5, main = "Probability of Occupancy for Habitat (Winter)")
  axis(1, 1:18, c(habitat = c("coastal",
                              "woodland",
                              "Unknown",
                              "riverbank",
                              "garden",
                              "scrubland",
                              "grassland",
                              "forest",
                              "residential",
                              "farmland",
                              "swamp",
                              "park",
                              "marsh",
                              "heath",
                              "industrials",
                              "bog",
                              "rocky",
                              "commercial")))
  arrows(1:18, lower, 1:18, upper, angle=90, code=3, length=0.05)
})


# occupancy model with filtered areas to reduce spatial bias--------------------

# ------------------------------------------------------------------------------

# for urban area:

# Extract the detection non-detection data
y = Urban.badger.wide[,-1]
str(y)

# Extract site covariates
for (i in 5:8) Urban.focal.sites[,i] <- as.factor(Urban.focal.sites[,i])
siteCovs <- Urban.focal.sites[,c(2:8)]
str(siteCovs)

# Load data
umf <-unmarkedFrameOccu(y=as.matrix(y),
                        siteCovs = siteCovs)


# -----------------analysis per season---------

Urban.sampling.dates <- colnames(Urban.badger.wide)
Urban.sampling.dates <- data.frame(col = 2:dim(Urban.badger.wide)[2], date = as.Date(Urban.sampling.dates[-1]))

# we can split these up in many ways. First, let's compute some useful variables.

Urban.sampling.dates <- Urban.sampling.dates %>% mutate(year = year(date),
                                                        month = month(date),
                                                        day = yday(date))

# we can make a lookup for seasons, to make it quick to add those
U.seasonLU <- data.frame(month = 1:12, season = c(rep("winter",2),rep("spring",3),rep("summer",3),rep("autumn",3),"winter"))
Urban.sampling.dates <- Urban.sampling.dates %>% left_join(U.seasonLU)

# now we have this info, we can easily extract only the columns of interest.
# as an example, what if we wanted to look at autumn, from 2015 onwards?


# for the full spring - all the detetcion day

U.springs <- Urban.sampling.dates %>% filter(season == "spring" & year >= 2015)

Urban.spring.mods <- lapply(2016:2018, function(yr){
  cat("Fitting mod for autumn",yr,"...\n")
  # get the column numbers of autumn of that year
  spring.cols <- U.springs %>% filter(year == yr) %>% select(col) %>% unlist(.) %>% as.integer(.)
  # restrict data of interest to just the relevant columns
  relevant.dat <- Urban.badger.wide[,spring.cols]
  # find rows that are all NA
  ncols <- ncol(relevant.dat)
  nNAs <- apply(relevant.dat,1, function(x) length(which(is.na(x))))
  to.remove <- which(nNAs == ncols)
  # remove those rows from the capture history (and, if necessary, the site.covs)
  y <- relevant.dat[-to.remove,]
  if (nrow(y) == 0) {
    return(data.frame(year = yr, nsites = 0, psi = NA, psi.lo = NA, psi.hi = NA, p.psi = NA, 
                      det = NA, det.lo = NA, det.hi = NA, p.det = NA))
  }
  umf <-unmarkedFrameOccu(y=as.matrix(y),
                          siteCovs = siteCovs[-to.remove,])
  sprmod <- occu(~ 1 ~ 1, data = umf)
  z1 <- summary(sprmod)$state
  psiLO = as.numeric(z1[1])
  psiLO.se = as.numeric(z1[2])
  psi <- exp(psiLO)/(1+exp(psiLO))
  psi.lo <- exp(psiLO-psiLO.se)/(1+exp(psiLO-psiLO.se))
  psi.hi <- exp(psiLO+psiLO.se)/(1+exp(psiLO+psiLO.se))
  z2 <- summary(sprmod)$det
  detLO = as.numeric(z2[1])
  detLO.se = as.numeric(z2[2])
  det <- exp(detLO)/(1+exp(detLO))
  det.lo <- exp(detLO-detLO.se)/(1+exp(detLO-detLO.se))
  det.hi <- exp(detLO+detLO.se)/(1+exp(detLO+detLO.se))
  return(data.frame(year = yr, nsites = nrow(y),
                    psi,psi.lo,psi.hi,p.psi = as.numeric(z1[4]),
                    det,det.lo,det.hi, p.det = as.numeric(z2[4])))
})
Urban.spring.mods <- bind_rows(Urban.spring.mods)
Urban.spring.mods

# Then, we can change the season in the sampling date to examine occupancy for 
# different season

# for summer

U.summers <- Urban.sampling.dates %>% filter(season == "summer" & year >= 2015)

Urban.summer.mods <- lapply(2016:2022, function(yr){
  cat("Fitting mod for autumn",yr,"...\n")
  # get the column numbers of autumn of that year
  summer.cols <- U.summers %>% filter(year == yr) %>% select(col) %>% unlist(.) %>% as.integer(.)
  # restrict data of interest to just the relevant columns
  relevant.dat <- Urban.badger.wide[,summer.cols]
  # find rows that are all NA
  ncols <- ncol(relevant.dat)
  nNAs <- apply(relevant.dat,1, function(x) length(which(is.na(x))))
  to.remove <- which(nNAs == ncols)
  # remove those rows from the capture history (and, if necessary, the site.covs)
  y <- relevant.dat[-to.remove,]
  if (nrow(y) == 0) {
    return(data.frame(year = yr, nsites = 0, psi = NA, psi.lo = NA, psi.hi = NA, p.psi = NA, 
                      det = NA, det.lo = NA, det.hi = NA, p.det = NA))
  }
  umf <-unmarkedFrameOccu(y=as.matrix(y),
                          siteCovs = siteCovs[-to.remove,])
  summod <- occu(~ 1 ~ 1, data = umf)
  z1 <- summary(summod)$state
  psiLO = as.numeric(z1[1])
  psiLO.se = as.numeric(z1[2])
  psi <- exp(psiLO)/(1+exp(psiLO))
  psi.lo <- exp(psiLO-psiLO.se)/(1+exp(psiLO-psiLO.se))
  psi.hi <- exp(psiLO+psiLO.se)/(1+exp(psiLO+psiLO.se))
  z2 <- summary(summod)$det
  detLO = as.numeric(z2[1])
  detLO.se = as.numeric(z2[2])
  det <- exp(detLO)/(1+exp(detLO))
  det.lo <- exp(detLO-detLO.se)/(1+exp(detLO-detLO.se))
  det.hi <- exp(detLO+detLO.se)/(1+exp(detLO+detLO.se))
  return(data.frame(year = yr, nsites = nrow(y),
                    psi,psi.lo,psi.hi,p.psi = as.numeric(z1[4]),
                    det,det.lo,det.hi, p.det = as.numeric(z2[4])))
})
Urban.summer.mods <- bind_rows(Urban.summer.mods)
Urban.summer.mods


# now we have this info, we can easily extract only the columns of interest.
# as an example, what if we wanted to look at autumn, from 2015 onwards?

U.autumns <- Urban.sampling.dates %>% filter(season == "autumn" & year >= 2015)

# now, we can step through the autumns and look at how variable our inferences
# are across them:

Urban.autumn.mods <- lapply(2016:2022, function(yr){
  cat("Fitting mod for autumn",yr,"...\n")
  # get the column numbers of autumn of that year
  autumn.cols <- U.autumns %>% filter(year == yr) %>% select(col) %>% unlist(.) %>% as.integer(.)
  # restrict data of interest to just the relevant columns
  relevant.dat <- Urban.badger.wide[,autumn.cols]
  # find rows that are all NA
  ncols <- ncol(relevant.dat)
  nNAs <- apply(relevant.dat,1, function(x) length(which(is.na(x))))
  to.remove <- which(nNAs == ncols)
  # remove those rows from the capture history (and, if necessary, the site.covs)
  if (nrow(y) == 0) {
    return(data.frame(year = yr, nsites = 0, psi = NA, psi.lo = NA, psi.hi = NA, p.psi = NA, 
                      det = NA, det.lo = NA, det.hi = NA, p.det = NA))
  }
  y <- relevant.dat[-to.remove,]
  umf <-unmarkedFrameOccu(y=as.matrix(y),
                          siteCovs = siteCovs[-to.remove,])
  autmod <- occu(~ 1 ~ 1, data = umf)
  z1 <- summary(autmod)$state
  psiLO = as.numeric(z1[1])
  psiLO.se = as.numeric(z1[2])
  psi <- exp(psiLO)/(1+exp(psiLO))
  psi.lo <- exp(psiLO-psiLO.se)/(1+exp(psiLO-psiLO.se))
  psi.hi <- exp(psiLO+psiLO.se)/(1+exp(psiLO+psiLO.se))
  z2 <- summary(autmod)$det
  detLO = as.numeric(z2[1])
  detLO.se = as.numeric(z2[2])
  det <- exp(detLO)/(1+exp(detLO))
  det.lo <- exp(detLO-detLO.se)/(1+exp(detLO-detLO.se))
  det.hi <- exp(detLO+detLO.se)/(1+exp(detLO+detLO.se))
  return(data.frame(year = yr, nsites = nrow(y),
                    psi,psi.lo,psi.hi,p.psi = as.numeric(z1[4]),
                    det,det.lo,det.hi, p.det = as.numeric(z2[4])))
})
Urban.autumn.mods <- bind_rows(Urban.autumn.mods)
Urban.autumn.mods

# for winter

U.winters <- Urban.sampling.dates %>% filter(season == "winter" & year >= 2015)

Urban.winter.mods <- lapply(2016:2018, function(yr){
  cat("Fitting mod for autumn",yr,"...\n")
  # get the column numbers of autumn of that year
  winter.cols <- U.winters %>% filter(year == yr) %>% select(col) %>% unlist(.) %>% as.integer(.)
  # restrict data of interest to just the relevant columns
  relevant.dat <- Urban.badger.wide[,winter.cols]
  # find rows that are all NA
  ncols <- ncol(relevant.dat)
  nNAs <- apply(relevant.dat,1, function(x) length(which(is.na(x))))
  to.remove <- which(nNAs == ncols)
  # remove those rows from the capture history (and, if necessary, the site.covs)
  y <- relevant.dat[-to.remove,]
  if (nrow(y) == 0) {
    return(data.frame(year = yr, nsites = 0, psi = NA, psi.lo = NA, psi.hi = NA, p.psi = NA, 
                      det = NA, det.lo = NA, det.hi = NA, p.det = NA))
  }
  umf <-unmarkedFrameOccu(y=as.matrix(y),
                          siteCovs = siteCovs[-to.remove,])
  winmod <- occu(~ 1 ~ 1, data = umf)
  z1 <- summary(winmod)$state
  psiLO = as.numeric(z1[1])
  psiLO.se = as.numeric(z1[2])
  psi <- exp(psiLO)/(1+exp(psiLO))
  psi.lo <- exp(psiLO-psiLO.se)/(1+exp(psiLO-psiLO.se))
  psi.hi <- exp(psiLO+psiLO.se)/(1+exp(psiLO+psiLO.se))
  z2 <- summary(winmod)$det
  detLO = as.numeric(z2[1])
  detLO.se = as.numeric(z2[2])
  det <- exp(detLO)/(1+exp(detLO))
  det.lo <- exp(detLO-detLO.se)/(1+exp(detLO-detLO.se))
  det.hi <- exp(detLO+detLO.se)/(1+exp(detLO+detLO.se))
  return(data.frame(year = yr, nsites = nrow(y),
                    psi,psi.lo,psi.hi,p.psi = as.numeric(z1[4]),
                    det,det.lo,det.hi, p.det = as.numeric(z2[4])))
})
Urban.winter.mods <- bind_rows(Urban.winter.mods)
Urban.winter.mods

# ------------------------------------------------------------------------------

# for natural area:

# Extract the detection non-detection data
y = Natural.badger.wide[,-1]
str(y)

# Extract site covariates
for (i in 5:8) Natural.focal.sites[,i] <- as.factor(Natural.focal.sites[,i])
siteCovs <- Natural.focal.sites[,c(2:8)]
str(siteCovs)

# Load data
umf <-unmarkedFrameOccu(y=as.matrix(y),
                        siteCovs = siteCovs)

Natural.sampling.dates <- colnames(Natural.badger.wide)
Natural.sampling.dates <- data.frame(col = 2:dim(Natural.badger.wide)[2], date = as.Date(Natural.sampling.dates[-1]))

# we can split these up in many ways. First, let's compute some useful variables.

Natural.sampling.dates <- Natural.sampling.dates %>% mutate(year = year(date),
                                                            month = month(date),
                                                            day = yday(date))

# we can make a lookup for seasons, to make it quick to add those
N.seasonLU <- data.frame(month = 1:12, season = c(rep("winter",2),rep("spring",3),rep("summer",3),rep("autumn",3),"winter"))
Natural.sampling.dates <- Natural.sampling.dates %>% left_join(N.seasonLU)

N.springs <- Natural.sampling.dates %>% filter(season == "spring" & year >= 2016)

Natural.spring.mods <- lapply(2016:2018, function(yr){
  cat("Fitting mod for autumn",yr,"...\n")
  # get the column numbers of autumn of that year
  spring.cols <- N.springs %>% filter(year == yr) %>% select(col) %>% unlist(.) %>% as.integer(.)
  # restrict data of interest to just the relevant columns
  relevant.dat <- Natural.badger.wide[,spring.cols]
  # find rows that are all NA
  ncols <- ncol(relevant.dat)
  nNAs <- apply(relevant.dat,1, function(x) length(which(is.na(x))))
  to.remove <- which(nNAs == ncols)
  # remove those rows from the capture history (and, if necessary, the site.covs)
  y <- relevant.dat[-to.remove,]
  if (nrow(y) == 0) {
    return(data.frame(year = yr, nsites = 0, psi = NA, psi.lo = NA, psi.hi = NA, p.psi = NA, 
                      det = NA, det.lo = NA, det.hi = NA, p.det = NA))
  }
  umf <-unmarkedFrameOccu(y=as.matrix(y),
                          siteCovs = siteCovs[-to.remove,])
  sprmod <- occu(~ 1 ~ 1, data = umf)
  z1 <- summary(sprmod)$state
  psiLO = as.numeric(z1[1])
  psiLO.se = as.numeric(z1[2])
  psi <- exp(psiLO)/(1+exp(psiLO))
  psi.lo <- exp(psiLO-psiLO.se)/(1+exp(psiLO-psiLO.se))
  psi.hi <- exp(psiLO+psiLO.se)/(1+exp(psiLO+psiLO.se))
  z2 <- summary(sprmod)$det
  detLO = as.numeric(z2[1])
  detLO.se = as.numeric(z2[2])
  det <- exp(detLO)/(1+exp(detLO))
  det.lo <- exp(detLO-detLO.se)/(1+exp(detLO-detLO.se))
  det.hi <- exp(detLO+detLO.se)/(1+exp(detLO+detLO.se))
  return(data.frame(year = yr, nsites = nrow(y),
                    psi,psi.lo,psi.hi,p.psi = as.numeric(z1[4]),
                    det,det.lo,det.hi, p.det = as.numeric(z2[4])))
})
Natural.spring.mods <- bind_rows(Natural.spring.mods)
Natural.spring.mods


# Same, we can change the season to examine occupancy and detecability for different seasons

# Then, we can change the season in the sampling date to examine occupancy for 
# different season

# for summer

N.summers <- Natural.sampling.dates %>% filter(season == "summer" & year >= 2015)

Natural.summer.mods <- lapply(2016:2022, function(yr){
  cat("Fitting mod for autumn",yr,"...\n")
  # get the column numbers of autumn of that year
  summer.cols <- N.summers %>% filter(year == yr) %>% select(col) %>% unlist(.) %>% as.integer(.)
  # restrict data of interest to just the relevant columns
  relevant.dat <- Natural.badger.wide[,summer.cols]
  # find rows that are all NA
  ncols <- ncol(relevant.dat)
  nNAs <- apply(relevant.dat,1, function(x) length(which(is.na(x))))
  to.remove <- which(nNAs == ncols)
  # remove those rows from the capture history (and, if necessary, the site.covs)
  y <- relevant.dat[-to.remove,]
  if (nrow(y) == 0) {
    return(data.frame(year = yr, nsites = 0, psi = NA, psi.lo = NA, psi.hi = NA, p.psi = NA, 
                      det = NA, det.lo = NA, det.hi = NA, p.det = NA))
  }
  umf <-unmarkedFrameOccu(y=as.matrix(y),
                          siteCovs = siteCovs[-to.remove,])
  summod <- occu(~ 1 ~ 1, data = umf)
  z1 <- summary(summod)$state
  psiLO = as.numeric(z1[1])
  psiLO.se = as.numeric(z1[2])
  psi <- exp(psiLO)/(1+exp(psiLO))
  psi.lo <- exp(psiLO-psiLO.se)/(1+exp(psiLO-psiLO.se))
  psi.hi <- exp(psiLO+psiLO.se)/(1+exp(psiLO+psiLO.se))
  z2 <- summary(summod)$det
  detLO = as.numeric(z2[1])
  detLO.se = as.numeric(z2[2])
  det <- exp(detLO)/(1+exp(detLO))
  det.lo <- exp(detLO-detLO.se)/(1+exp(detLO-detLO.se))
  det.hi <- exp(detLO+detLO.se)/(1+exp(detLO+detLO.se))
  return(data.frame(year = yr, nsites = nrow(y),
                    psi,psi.lo,psi.hi,p.psi = as.numeric(z1[4]),
                    det,det.lo,det.hi, p.det = as.numeric(z2[4])))
})
Natural.summer.mods <- bind_rows(Natural.summer.mods)
Natural.summer.mods


# now we have this info, we can easily extract only the columns of interest.
# as an example, what if we wanted to look at autumn, from 2015 onwards?

N.autumns <- Natural.sampling.dates %>% filter(season == "autumn" & year >= 2015)

# now, we can step through the autumns and look at how variable our inferences
# are across them:

Natural.autumn.mods <- lapply(2016:2022, function(yr){
  cat("Fitting mod for autumn",yr,"...\n")
  # get the column numbers of autumn of that year
  autumn.cols <- N.autumns %>% filter(year == yr) %>% select(col) %>% unlist(.) %>% as.integer(.)
  # restrict data of interest to just the relevant columns
  relevant.dat <- Natural.badger.wide[,autumn.cols]
  # find rows that are all NA
  ncols <- ncol(relevant.dat)
  nNAs <- apply(relevant.dat,1, function(x) length(which(is.na(x))))
  to.remove <- which(nNAs == ncols)
  # remove those rows from the capture history (and, if necessary, the site.covs)
  if (nrow(y) == 0) {
    return(data.frame(year = yr, nsites = 0, psi = NA, psi.lo = NA, psi.hi = NA, p.psi = NA, 
                      det = NA, det.lo = NA, det.hi = NA, p.det = NA))
  }
  y <- relevant.dat[-to.remove,]
  umf <-unmarkedFrameOccu(y=as.matrix(y),
                          siteCovs = siteCovs[-to.remove,])
  autmod <- occu(~ 1 ~ 1, data = umf)
  z1 <- summary(autmod)$state
  psiLO = as.numeric(z1[1])
  psiLO.se = as.numeric(z1[2])
  psi <- exp(psiLO)/(1+exp(psiLO))
  psi.lo <- exp(psiLO-psiLO.se)/(1+exp(psiLO-psiLO.se))
  psi.hi <- exp(psiLO+psiLO.se)/(1+exp(psiLO+psiLO.se))
  z2 <- summary(autmod)$det
  detLO = as.numeric(z2[1])
  detLO.se = as.numeric(z2[2])
  det <- exp(detLO)/(1+exp(detLO))
  det.lo <- exp(detLO-detLO.se)/(1+exp(detLO-detLO.se))
  det.hi <- exp(detLO+detLO.se)/(1+exp(detLO+detLO.se))
  return(data.frame(year = yr, nsites = nrow(y),
                    psi,psi.lo,psi.hi,p.psi = as.numeric(z1[4]),
                    det,det.lo,det.hi, p.det = as.numeric(z2[4])))
})
Natural.autumn.mods <- bind_rows(Natural.autumn.mods)
Natural.autumn.mods

# for winter

N.winters <- Natural.sampling.dates %>% filter(season == "winter" & year >= 2015)

Natural.winter.mods <- lapply(2016:2018, function(yr){
  cat("Fitting mod for autumn",yr,"...\n")
  # get the column numbers of autumn of that year
  winter.cols <- N.winters %>% filter(year == yr) %>% select(col) %>% unlist(.) %>% as.integer(.)
  # restrict data of interest to just the relevant columns
  relevant.dat <- Natural.badger.wide[,winter.cols]
  # find rows that are all NA
  ncols <- ncol(relevant.dat)
  nNAs <- apply(relevant.dat,1, function(x) length(which(is.na(x))))
  to.remove <- which(nNAs == ncols)
  # remove those rows from the capture history (and, if necessary, the site.covs)
  y <- relevant.dat[-to.remove,]
  if (nrow(y) == 0) {
    return(data.frame(year = yr, nsites = 0, psi = NA, psi.lo = NA, psi.hi = NA, p.psi = NA, 
                      det = NA, det.lo = NA, det.hi = NA, p.det = NA))
  }
  umf <-unmarkedFrameOccu(y=as.matrix(y),
                          siteCovs = siteCovs[-to.remove,])
  winmod <- occu(~ 1 ~ 1, data = umf)
  z1 <- summary(winmod)$state
  psiLO = as.numeric(z1[1])
  psiLO.se = as.numeric(z1[2])
  psi <- exp(psiLO)/(1+exp(psiLO))
  psi.lo <- exp(psiLO-psiLO.se)/(1+exp(psiLO-psiLO.se))
  psi.hi <- exp(psiLO+psiLO.se)/(1+exp(psiLO+psiLO.se))
  z2 <- summary(winmod)$det
  detLO = as.numeric(z2[1])
  detLO.se = as.numeric(z2[2])
  det <- exp(detLO)/(1+exp(detLO))
  det.lo <- exp(detLO-detLO.se)/(1+exp(detLO-detLO.se))
  det.hi <- exp(detLO+detLO.se)/(1+exp(detLO+detLO.se))
  return(data.frame(year = yr, nsites = nrow(y),
                    psi,psi.lo,psi.hi,p.psi = as.numeric(z1[4]),
                    det,det.lo,det.hi, p.det = as.numeric(z2[4])))
})
Natural.winter.mods <- bind_rows(Natural.winter.mods)
Natural.winter.mods

