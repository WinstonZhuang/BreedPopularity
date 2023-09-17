library(ggmap)
library(ggrepel)
library(dplyr)
library(ggplot2)
library(shiny)
library(klaR)
citation("ggmap")
data <- read.csv("NYC_Dog_Licensing_Dataset.csv")
data <- subset(data, data$BreedName != "Unknown")
data <- subset(data, data$BreedName != "Not Provided")
data <- subset(data, data$BreedName != "BREED?")
data <- subset(data, data$BreedName != "Unknown/Cross")
data <- subset(data, data$BreedName != "unknown")
data <- subset(data, data$BreedName != "UNKNOWN")
data <- subset(data, data$BreedName != "UNKNOWN BREED?")
data <- subset(data, data$BreedName != "Crossbreed")
data <- subset(data, data$BreedName != "Mixed")
data <- subset(data, data$BreedName != "Unknown (stray)")
data <- subset(data, data$BreedName != "Mix")
data <- subset(data, data$BreedName != "MIX")
data <- subset(data, data$BreedName != "MIXED")
data <- subset(data, data$BreedName != "Mixed Breed")
data <- subset(data, data$BreedName != "MIXED HOUND")
data$Borough <- NA
data$BoroughPop <- NA

Bronx <- c(10451, 10452, 10453, 10454, 10455, 10456, 
           10457, 10458, 10459, 10460, 10461, 10462, 
           10463, 10464, 10465, 10466, 10467, 10468, 
           10469, 10470, 10471, 10472, 10473, 10474, 
           10475)

Brooklyn <- c(11201, 11203, 11204, 11205, 11206, 
              11207, 11208, 11209, 11210, 11211, 
              11212, 11213, 11214, 11215, 11216, 
              11217, 11218, 11219, 11220, 11221, 
              11222, 11223, 11224, 11225, 11226, 
              11228, 11229, 11230, 11231, 11232, 
              11233, 11234, 11235, 11236, 11237, 
              11238, 11239, 11249, 11274)

Manhattan <- c (10001, 10002, 10003, 10004, 10005,
                10006, 10007, 10009, 10010, 10011,
                10012, 10013, 10014, 10016, 10017,
                10018, 10019, 10020, 10021, 10022,
                10023, 10024, 10025, 10026, 10027,
                10028, 10029, 10030, 10031, 10032,
                10033, 10034, 10035, 10036, 10037,
                10038, 10039, 10040, 10044, 10065,
                10069, 10075, 10103, 10110, 10111,
                10112, 10115, 10119, 10128, 10152,
                10153, 10154, 10162, 10165, 10167,
                10168, 10169, 10170, 10171, 10172,
                10173, 10174, 10177, 10271, 10278,
                10279, 10280, 10282)

StatenIsland <- c(10301, 10302, 10303, 10304, 10305,
                  10306, 10307, 10308, 10309, 10310,
                  10311, 10312, 10314)

Queens <-c(11001, 11003, 11004, 11005, 11020, 11021,
           11023, 11024, 11030, 11040, 11042, 11101,
           11102, 11103, 11104, 11105, 11106, 11109,
           11354, 11355, 11356, 11357, 11358, 11360,
           11361, 11362, 11363, 11364, 11365, 11366,
           11367, 11368, 11369, 11370, 11371, 11372,
           11373, 11374, 11375, 11377, 11378, 11379,
           11385, 11411, 11412, 11413, 11414, 11415,
           11416, 11417, 11418, 11419, 11420, 10333, 
           10150, 10159, 10283, 10295, 10101, 11315, 
           11346, 11421, 11422, 11423, 11426, 11427, 
           11428, 11429, 11430, 11432, 11433, 11434, 
           11435, 11436, 11439, 11455, 11691, 11692, 
           11693, 11694, 11697)

issued <- substr(data$LicenseIssuedDate, start = nchar(data$LicenseIssuedDate) -3, stop = nchar(data$LicenseIssuedDate))

choices <- c(unique(sort(issued)))

data2014 <- subset(data, 
                   substr(data$LicenseIssuedDate,
                                start = nchar(data$LicenseIssuedDate) -3,
                                stop = nchar(data$LicenseIssuedDate)) == "2014")

data2015 <- subset(data, 
                   substr(data$LicenseIssuedDate,
                          start = nchar(data$LicenseIssuedDate) -3,
                          stop = nchar(data$LicenseIssuedDate)) == "2015")

data2016 <- subset(data, 
                   substr(data$LicenseIssuedDate,
                          start = nchar(data$LicenseIssuedDate) -3,
                          stop = nchar(data$LicenseIssuedDate)) == "2016")

data2017 <- subset(data, 
                   substr(data$LicenseIssuedDate,
                          start = nchar(data$LicenseIssuedDate) -3,
                          stop = nchar(data$LicenseIssuedDate)) == "2017")

data2018 <- subset(data, 
                   substr(data$LicenseIssuedDate,
                          start = nchar(data$LicenseIssuedDate) -3,
                          stop = nchar(data$LicenseIssuedDate)) == "2018")

data2019 <- subset(data, 
                   substr(data$LicenseIssuedDate,
                          start = nchar(data$LicenseIssuedDate) -3,
                          stop = nchar(data$LicenseIssuedDate)) == "2019")

data2020 <- subset(data, 
                   substr(data$LicenseIssuedDate,
                          start = nchar(data$LicenseIssuedDate) -3,
                          stop = nchar(data$LicenseIssuedDate)) == "2020")

data2021 <- subset(data, 
                   substr(data$LicenseIssuedDate,
                          start = nchar(data$LicenseIssuedDate) -3,
                          stop = nchar(data$LicenseIssuedDate)) == "2021")

for (i in 1:nrow(data2014)) {
  if (data2014[i,5] %in% Bronx) {
    data2014[i,9] <- "Bronx"
    data2014[i,10] <- 1438159
  }
  else if (data2014[i,5] %in% Brooklyn) {
    data2014[i,9] <- "Brooklyn"
    data2014[i,10] <- 2621793
  }
  else if (data2014[i,5] %in% Manhattan) {
    data2014[i,9] <- "Manhattan"
    data2014[i,10] <- 1649114
  }
  else if (data2014[i,5] %in% Queens) {
    data2014[i,9] <- "Queens"
    data2014[i,10] <- 2321580
  }
  else if (data2014[i,5] %in% StatenIsland) {
    data2014[i,9] <- "Staten Island"
    data2014[i,10] <- 473324
  }
}

for (i in 1:nrow(data2015)) {
  if (data2015[i,5] %in% Bronx) {
    data2015[i,9] <- "Bronx"
    data2015[i,10] <- 1446541
  }
  else if (data2015[i,5] %in% Brooklyn) {
    data2015[i,9] <- "Brooklyn"
    data2015[i,10] <- 2640329
  }
  else if (data2015[i,5] %in% Manhattan) {
    data2015[i,9] <- "Manhattan"
    data2015[i,10] <- 1663447
  }
  else if (data2015[i,5] %in% Queens) {
    data2015[i,9] <- "Queens"
    data2015[i,10] <- 2339150
  }
  else if (data2015[i,5] %in% StatenIsland) {
    data2015[i,9] <- "Staten Island"
    data2015[i,10] <- 475948
  }
}

for (i in 1:nrow(data2016)) {
  if (data2016[i,5] %in% Bronx) {
    data2016[i,9] <- "Bronx"
    data2016[i,10] <- 1455444
  }
  else if (data2016[i,5] %in% Brooklyn) {
    data2016[i,9] <- "Brooklyn"
    data2016[i,10] <- 2657924
  }
  else if (data2016[i,5] %in% Manhattan) {
    data2016[i,9] <- "Manhattan"
    data2016[i,10] <- 1677062
  }
  else if (data2016[i,5] %in% Queens) {
    data2016[i,9] <- "Queens"
    data2016[i,10] <- 2341116
  }
  else if (data2016[i,5] %in% StatenIsland) {
    data2016[i,9] <- "Staten Island"
    data2016[i,10] <- 478160
  }
}

for (i in 1:nrow(data2017)) {
  if (data2017[i,5] %in% Bronx) {
    data2017[i,9] <- "Bronx"
    data2017[i,10] <- 1463004
  }
  else if (data2017[i,5] %in% Brooklyn) {
    data2017[i,9] <- "Brooklyn"
    data2017[i,10] <- 2636735
  }
  else if (data2017[i,5] %in% Manhattan) {
    data2017[i,9] <- "Manhattan"
    data2017[i,10] <- 1682664
  }
  else if (data2017[i,5] %in% Queens) {
    data2017[i,9] <- "Queens"
    data2017[i,10] <- 2354054
  }
  else if (data2017[i,5] %in% StatenIsland) {
    data2017[i,9] <- "Staten Island"
    data2017[i,10] <- 479458
  }
}

for (i in 1:nrow(data2018)) {
  if (data2018[i,5] %in% Bronx) {
    data2018[i,9] <- "Bronx"
    data2018[i,10] <- 1479458
  }
  else if (data2018[i,5] %in% Brooklyn) {
    data2018[i,9] <- "Brooklyn"
    data2018[i,10] <- 2582830
  }
  else if (data2018[i,5] %in% Manhattan) {
    data2018[i,9] <- "Manhattan"
    data2018[i,10] <- 1628701
  }
  else if (data2018[i,5] %in% Queens) {
    data2018[i,9] <- "Queens"
    data2018[i,10] <- 2278906
  }
  else if (data2018[i,5] %in% StatenIsland) {
    data2018[i,9] <- "Staten Island"
    data2018[i,10] <- 476179
  }
}

for (i in 1:nrow(data2019)) {
  if (data2019[i,5] %in% Bronx) {
    data2019[i,9] <- "Bronx"
    data2019[i,10] <- 1472654
  }
  else if (data2019[i,5] %in% Brooklyn) {
    data2019[i,9] <- "Brooklyn"
    data2019[i,10] <- 2559903
  }
  else if (data2019[i,5] %in% Manhattan) {
    data2019[i,9] <- "Manhattan"
    data2019[i,10] <- 1628706
  }
  else if (data2019[i,5] %in% Queens) {
    data2019[i,9] <- "Queens"
    data2019[i,10] <- 2253858
  }
  else if (data2019[i,5] %in% StatenIsland) {
    data2019[i,9] <- "Staten Island"
    data2019[i,10] <- 474558
  }
}

for (i in 1:nrow(data2020)) {
  if (data2020[i,5] %in% Bronx) {
    data2020[i,9] <- "Bronx"
    data2020[i,10] <- 1471160
  }
  else if (data2020[i,5] %in% Brooklyn) {
    data2020[i,9] <- "Brooklyn"
    data2020[i,10] <- 2559903
  }
  else if (data2020[i,5] %in% Manhattan) {
    data2020[i,9] <- "Manhattan"
    data2020[i,10] <- 1628701
  }
  else if (data2020[i,5] %in% Queens) {
    data2020[i,9] <- "Queens"
    data2020[i,10] <- 2253858
  }
  else if (data2020[i,5] %in% StatenIsland) {
    data2020[i,9] <- "Staten Island"
    data2020[i,10] <- 474558
  }
}

for (i in 1:nrow(data2021)) {
  if (data2021[i,5] %in% Bronx) {
    data2021[i,9] <- "Bronx"
    data2021[i,10] <- 1468823
  }
  else if (data2021[i,5] %in% Brooklyn) {
    data2021[i,9] <- "Brooklyn"
    data2021[i,10] <- 2559903
  }
  else if (data2021[i,5] %in% Manhattan) {
    data2021[i,9] <- "Manhattan"
    data2021[i,10] <- 1628701
  }
  else if (data2021[i,5] %in% Queens) {
    data2021[i,9] <- "Queens"
    data2021[i,10] <- 2253858
  }
  else if (data2021[i,5] %in% StatenIsland) {
    data2021[i,9] <- "Staten Island"
    data2021[i,10] <- 474558
  }
}

data2014 <- subset(data2014, !is.na(data2014$Borough))
data2015 <- subset(data2015, !is.na(data2015$Borough))
data2016 <- subset(data2016, !is.na(data2016$Borough))
data2017 <- subset(data2017, !is.na(data2017$Borough))
data2018 <- subset(data2018, !is.na(data2018$Borough))
data2019 <- subset(data2019, !is.na(data2019$Borough))
data2020 <- subset(data2020, !is.na(data2020$Borough))
data2021 <- subset(data2021, !is.na(data2021$Borough))

data2014 <- subset(data2014, !is.na(data2014$BreedName))
data2015 <- subset(data2015, !is.na(data2015$BreedName))
data2016 <- subset(data2016, !is.na(data2016$BreedName))
data2017 <- subset(data2017, !is.na(data2017$BreedName))
data2018 <- subset(data2018, !is.na(data2018$BreedName))
data2019 <- subset(data2019, !is.na(data2019$BreedName))
data2020 <- subset(data2020, !is.na(data2020$BreedName))
data2021 <- subset(data2021, !is.na(data2021$BreedName))

sample2014 <- data2014[sample(nrow(data2014),100),]
sample2015 <- data2015[sample(nrow(data2015),100),]
sample2016 <- data2016[sample(nrow(data2016),100),]
sample2017 <- data2017[sample(nrow(data2017),100),]
sample2018 <- data2018[sample(nrow(data2018),100),]
sample2019 <- data2019[sample(nrow(data2019),100),]
sample2020 <- data2020[sample(nrow(data2020),100),]
sample2021 <- data2021[sample(nrow(data2021),100),]

data2014latlon <- geocode(as.character(sample2014$ZipCode), output = "latlon")
sample2014 <- cbind(sample2014, data2014latlon)

nyc <- get_map(location = "new york city", zoom = 10)
ggmap(nyc) + geom_point(data = sample2014, aes(x = lon, y = lat, color = Borough), alpha = 0.5)

data2015latlon <- geocode(as.character(sample2015$ZipCode), output = "latlon")
sample2015 <- cbind(sample2015, data2015latlon)

data2016latlon <- geocode(as.character(sample2016$ZipCode), output = "latlon")
sample2016 <- cbind(sample2016, data2016latlon)

data2017latlon <- geocode(as.character(sample2017$ZipCode), output = "latlon")
sample2017 <- cbind(sample2017, data2017latlon)

data2018latlon <- geocode(as.character(sample2018$ZipCode), output = "latlon")
sample2018 <- cbind(sample2018, data2018latlon)

data2019latlon <- geocode(as.character(sample2019$ZipCode), output = "latlon")
sample2019 <- cbind(sample2019, data2019latlon)

data2020latlon <- geocode(as.character(sample2020$ZipCode), output = "latlon")
sample2020 <- cbind(sample2020, data2020latlon)

data2021latlon <- geocode(as.character(sample2021$ZipCode), output = "latlon")
sample2021 <- cbind(sample2021, data2021latlon)

table2014 <- data2014 %>% group_by(Borough, BreedName) %>%
  summarise(n=n()) %>%
  arrange(desc(n)) %>%
  group_by(Borough) %>%
  arrange(Borough, desc(n)) %>%
  slice_head(n=10)

table2015 <- data2015 %>% group_by(Borough, BreedName) %>%
  summarise(n=n()) %>%
  arrange(desc(n)) %>%
  group_by(Borough) %>%
  arrange(Borough, desc(n)) %>%
  slice_head(n=10)

table2016 <- data2016 %>% group_by(Borough, BreedName) %>%
  summarise(n=n()) %>%
  arrange(desc(n)) %>%
  group_by(Borough) %>%
  arrange(Borough, desc(n)) %>%
  slice_head(n=10)

table2017 <- data2017 %>% group_by(Borough, BreedName) %>%
  summarise(n=n()) %>%
  arrange(desc(n)) %>%
  group_by(Borough) %>%
  arrange(Borough, desc(n)) %>%
  slice_head(n=10)

table2018 <- data2018 %>% group_by(Borough, BreedName) %>%
  summarise(n=n()) %>%
  arrange(desc(n)) %>%
  group_by(Borough) %>%
  arrange(Borough, desc(n)) %>%
  slice_head(n=10)

table2019 <- data2019 %>% group_by(Borough, BreedName) %>%
  summarise(n=n()) %>%
  arrange(desc(n)) %>%
  group_by(Borough) %>%
  arrange(Borough, desc(n)) %>%
  slice_head(n=10)

table2020 <- data2020 %>% group_by(Borough, BreedName) %>%
  summarise(n=n()) %>%
  arrange(desc(n)) %>%
  group_by(Borough) %>%
  arrange(Borough, desc(n)) %>%
  slice_head(n=10)

table2021 <- data2021 %>% group_by(Borough, BreedName) %>%
  summarise(n=n()) %>%
  arrange(desc(n)) %>%
  group_by(Borough) %>%
  arrange(Borough, desc(n)) %>%
  slice_head(n=10)

write.csv(data2014, "data2014.csv", row.names = FALSE)
write.csv(data2015, "data2015.csv", row.names = FALSE)
write.csv(data2016, "data2016.csv", row.names = FALSE)
write.csv(data2017, "data2017.csv", row.names = FALSE)
write.csv(data2018, "data2018.csv", row.names = FALSE)
write.csv(data2019, "data2019.csv", row.names = FALSE)
write.csv(data2020, "data2020.csv", row.names = FALSE)
write.csv(data2021, "data2021.csv", row.names = FALSE)
write.csv(sample2014, "sample2014.csv", row.names = FALSE)
write.csv(sample2015, "sample2015.csv", row.names = FALSE)
write.csv(sample2016, "sample2016.csv", row.names = FALSE)
write.csv(sample2017, "sample2017.csv", row.names = FALSE)
write.csv(sample2018, "sample2018.csv", row.names = FALSE)
write.csv(sample2019, "sample2019.csv", row.names = FALSE)
write.csv(sample2020, "sample2020.csv", row.names = FALSE)
write.csv(sample2021, "sample2021.csv", row.names = FALSE)
write.csv(table2014, "table2014.csv", row.names = FALSE)
write.csv(table2015, "table2015.csv", row.names = FALSE)
write.csv(table2016, "table2016.csv", row.names = FALSE)
write.csv(table2017, "table2017.csv", row.names = FALSE)
write.csv(table2018, "table2018.csv", row.names = FALSE)
write.csv(table2019, "table2019.csv", row.names = FALSE)
write.csv(table2020, "table2020.csv", row.names = FALSE)
write.csv(table2021, "table2021.csv", row.names = FALSE)


