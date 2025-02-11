
# ExtendedMeasurementOrFact:
#   ”Sampling instrument name” oli pakollinen, mutta puuttui -> laitoin ”Flow-through system”


setwd("D:/IFCB/IFCB_codes/IFCB-EMODnet/")

library(data.table)
library(dplyr)
library(openxlsx)

wd <- "csvs/"
csvs <- list.files(wd, pattern = "*.csv")

data <- read.csv2(paste("csvs/", csvs[1], sep = ""), sep = ",", header = TRUE, encoding = "UTF-8")
data <- data.table(data[-1,])

# combine all csvs into one data.table
for (i in 2:length(csvs)){
  temp.data<-read.csv(paste("csvs/", csvs[i], sep = ""), sep = ",", header = TRUE, encoding = "UTF-8")
  temp.data <- temp.data[-1,]
  data<-data.table(rbind(data,temp.data))
}

# sum counts and biovolumes
data_summed <- data %>% group_by(sample_id, object_annotation_category_id) %>% 
  summarise(category_count = n(),
            category_volume = sum(as.numeric(object_biovolume_um3)))

data_cols <- data %>% select(sample_id, object_annotation_category_id, object_annotation_category, object_lat, object_lon, 
                             object_date, object_time, object_depth_max, object_depth_min, object_annotation_status,
                             sample_station, sample_volume_ml)

data_joined <- left_join(data_summed, data_cols, by = c("sample_id" = "sample_id", 
                                                           "object_annotation_category_id" = "object_annotation_category_id")) %>% 
  distinct()


# add aphiaIDs
aphias <- read.csv2("worms_matched_3.csv") %>% select(AphiaID_accepted, ScientificName, ScientificName.1, ScientificName_accepted)

data_joined[data_joined == "centric"] <- "Centrales"
data_joined[data_joined == "pennate"] <- "Pennales"
data_joined[data_joined == "snowella-woronichinia"] <- "cf. Snowella"  #TARKISTA


data_with_aphia <- data_joined %>% left_join(aphias, by = c("object_annotation_category" = "ScientificName"))


library(lubridate)

#data_with_aphia$date_time <- ymd_hms(paste(data_with_aphia$object_date, data_with_aphia$object_time))
data_with_aphia$date_time <- format_ISO8601(as.POSIXct(ymd_hms(paste(data_with_aphia$object_date, data_with_aphia$object_time))), usetz = "Z")




event_data_campaign <- data.table(eventID = "IFCB_Uto_2021", eventDate = "", decimalLatitude = 59.78, decimalLongitude = 21.37, 
                                  institutionCode = "SYKE", datasetName = "IFCB Utö 2021 JERICO-RI Gulf of Finland Pilot Supersite", 
                                  samplingProtocol = "doi: 10.3389/fmars.2022.867695", parentEventID = "", type = "SamplingCampaign", 
                                  maximumDepthInMeters = 5, minimumDepthInMeters = 5)

# create event table
event_data <- data_with_aphia %>% 
  select(eventID = sample_id, eventDate = date_time) %>% 
  distinct() %>% 
  mutate(decimalLatitude = 59.78, decimalLongitude = 21.37, institutionCode = "", datasetName = "", samplingProtocol = "", parentEventID = "IFCB_Uto_2021", type = "Sample",
         maximumDepthInMeters = "", minimumDepthInMeters = "")
event <- rbind(event_data_campaign, event_data, use.names = F)

# create occurrence table
occurrence_data <- data_with_aphia %>% 
  mutate(occurrenceID = case_when(object_annotation_category == "unknown" ~ paste(sample_id, "unknown", sep = "_"),
                                  TRUE ~ paste(sample_id, AphiaID_accepted, sep = "_")), basisOfRecord = "MachineObservation", occurrenceStatus = "Present") %>% 
  mutate(scientificNameID = case_when(object_annotation_category == "unknown" ~ "",
                                      object_annotation_category == "heterocyst" ~ "",
                                      TRUE ~ paste("urn:lsid:marinespecies.org:taxname:", AphiaID_accepted, sep = ""))) %>% 
  select(eventID = sample_id, occurrenceID, basisOfRecord, occurrenceStatus, scientificName = object_annotation_category, scientificNameID) %>% 
  mutate(identificationRemarks = case_when(scientificName == "Snowella" ~ "Class includes both Snowella and Woronichinia",
                                           TRUE ~ "")) %>% 
  mutate(identificationVerificationStatus = "ValidatedByHuman", identificationReferences = "https://github.com/hsosik/ifcb-analysis/wiki/Instructions-for-manual-annotation-of-images",
         associatedMedia = "https://ecotaxa.obs-vlfr.fr/prj/5635") %>% 
  distinct()
occurrence <- occurrence_data

# create measurement table
measurement_data <- data.table(eventID = "IFCB_Uto_2021", measurementType = "Imaging instrument", measurementTypeID = "", measurementValue = "IFCB", 
                               measurementValueID = "http://vocab.nerc.ac.uk/collection/L22/current/TOOL1588/", occurrenceID = "", measurementID = "", 
                               measurementUnit = "", measurementUnitID = "http://vocab.nerc.ac.uk/collection/P06/current/XXXX/")
measurement_data_protocol <- data.table(eventID = "IFCB_Uto_2021", measurementType = "Sampling protocol", measurementTypeID = "http://vocab.nerc.ac.uk/collection/P01/current/SAMPPROT/",
                                        measurementValue = "Water is pumped continuously for the station's flowthrough measurements, from 250 m offshore, with underwater pump (Grundfos SP3A-9N) through a 50-mm black 
                                        PE tube lying at the sea bottom. The inlet for water sampling is located at a depth of ~5 m. Water is distributed through several flow-through sensors, including the IFCB.",
                                        measurementValueID = "", occurrenceID = "", 
                                        measurementID = "", measurementUnit = "", measurementUnitID = "http://vocab.nerc.ac.uk/collection/P06/current/XXXX/")

# calculate biovolume per sample
biovolume_summed <- data_summed %>% group_by(sample_id) %>% 
  summarise(sample_biovolume = round(sum(category_volume), 2))

measurement_sample_biovolume <- biovolume_summed %>% 
  mutate(eventID = sample_id, measurementType = "biovolume", measurementTypeID = "http://vocab.nerc.ac.uk/collection/P01/current/SDBIOL14/", 
         measurementValue = sample_biovolume, measurementValueID = "",
         occurrenceID = "", measurementID = paste(eventID, "bv", sep = "_"), measurementUnit = "um3",
         measurementUnitID = "http://vocab.nerc.ac.uk/collection/P06/current/UMCU/") %>% select(-c(sample_id, sample_biovolume))
  
  
measurement <- rbind(measurement_data, measurement_data_protocol, measurement_sample_biovolume)

for (i in 1:nrow(data_with_aphia)) {
  
  row <- data_with_aphia[i,]
  
  count <- row %>% mutate(measurementType = "count", measurementTypeID = "http://vocab.nerc.ac.uk/collection/P01/current/OCOUNT01/", measurementUnit = "",
                          measurementUnitID = "", measurementValueID = "",
                          occurrenceID = case_when(object_annotation_category == "unknown" ~ paste(sample_id, "unknown", sep = "_"),
                                                   TRUE ~ paste(sample_id, AphiaID_accepted, sep = "_")), 
                          measurementID = paste(occurrenceID, "c", sep = "_")) %>% 
    select(eventID = sample_id, measurementType, measurementTypeID, measurementValue = category_count, occurrenceID, 
           measurementValueID, measurementUnit, measurementUnitID, measurementID)
  
  biovolume <- row %>% mutate(measurementType = "biovolume", measurementTypeID = "http://vocab.nerc.ac.uk/collection/P01/current/SDBIOL14/", measurementUnit = "um3",
                              measurementUnitID = "http://vocab.nerc.ac.uk/collection/P06/current/UMCU/", measurementValueID = "",
                              occurrenceID = case_when(object_annotation_category == "unknown" ~ paste(sample_id, "unknown", sep = "_"),
                                                                      TRUE ~ paste(sample_id, AphiaID_accepted, sep = "_")), 
                              measurementID = paste(occurrenceID, "bv", sep = "_")) %>% 
    select(eventID = sample_id, measurementType, measurementTypeID, measurementValue = category_volume, occurrenceID, 
           measurementValueID, measurementUnit, measurementUnitID, measurementID)
  
  measurement <- rbind(measurement, count, biovolume)
  
}




volume <- data_with_aphia %>% distinct(sample_id, .keep_all = TRUE) %>%  
  mutate(measurementType = "sample volume", measurementTypeID = "http://vocab.nerc.ac.uk/collection/P01/current/VOLXXXXX/", measurementUnit = "ml",
                                      measurementUnitID = "http://vocab.nerc.ac.uk/collection/P06/current/VVML/", measurementValue = 3.8723025999999994, measurementValueID = "", 
                                      occurrenceID = case_when(object_annotation_category == "unknown" ~ paste(sample_id, "unknown", sep = "_"),
                                                               TRUE ~ paste(sample_id, AphiaID_accepted, sep = "_")), 
                                      measurementID = paste(sample_id, "v", sep = "_")) %>%
  select(eventID = sample_id, measurementType, measurementTypeID, measurementValue, occurrenceID, 
         measurementValueID, measurementUnit, measurementUnitID, measurementID)

measurement <- rbind(measurement, volume)




# write tables into excel
l <- list("Event" = event, "Occurrence" = occurrence, "ExtendedMeasurementOrFact" = measurement)
#write.xlsx(l, file = "IFCB_to_EMODnet_280623.xlsx")
