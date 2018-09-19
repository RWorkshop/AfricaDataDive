library(tidyverse)

 pilot_delination <- read_csv(file.choose())

 pilot_delination %>%
   glimpse()


 
 ###Selecting columns based on partial columns names
 pilot_delination2 <- pilot_delination %>%
   select( ends_with("_point_gps_latitude"))%>%
   glimpse()
View(pilot_delination2)

##Transform columns to row

d2 <- data.frame(r1 = names(pilot_delination2), t(pilot_delination2))

pilot_df <- d2 %>%
  select(X1:X6) %>%
  rename_all(Latitude_1 = X1, Latitude_2 = X2, Latiude_3 = X3, Latitude_4 = X4,
         Latitude_5 = X5, Latitude_6 = X6) %>%
  glimpse()
pilot_df <- pilot_df %>%
  select(Latitude_1 = X1, Latitude_2 = X2, Latitude_3 = X3, Latitude_4 = X4,
         Latitude_5 = X5, Latitude_6 = X6) %>%
  glimpse()


##Removing a row_names
rownames(pilot_df) <- c()
View(pilot_df)


##Working with the Longitude part
Longitude_delineation <- pilot_delination %>%
  select(ends_with("point_gps_longitude")) %>%
  glimpse

#3Transforming the row names to columns
d2 <- data.frame(r1 = names(Longitude_delineation), t(Longitude_delineation))
glimpse(d2)

##Removing row values
rownames(d2) <- c()
glimpse(d2)

##
Longitude_df <- d2 %>%
  select(X1:X6) %>%
  glimpse()
View(Longitude_df)

##Renaming the columns
Longitude_df <- Longitude_df %>%
  select(Longitude_1 = X1, Longitude_2 = X2, Longitude_3 = X3,
         Longitude_4 = X4, Longitude_5 = X5, Longitude_6 = X6) %>%
  glimpse()

##mUTATING
Longitude_df <- Longitude_df %>%
  select(Longitude_1:Longitude_6) %>%
  mutate(no = 1:nrow(Longitude_df)) %>%
  glimpse()

##Mutatuing Pilot_df
pilot_df <- pilot_df %>%
  select(Latitude_1:Latitude_6) %>%
  mutate(no = 1:nrow(pilot_df)) %>%
  glimpse()

##Merging both column together
##Outer join
coordinatee <- merge(x = Longitude_df, y = pilot_df, by = "no", all.y = TRUE)

##Removing the merge column and rearranging the data
coordinate <- coordinatee %>%
  select(-no)%>%
  glimpse
coordinate <- coordinate %>%
  select(Latitude_1,Longitude_1,Latitude_2, Longitude_2, Latitude_3, Longitude_3,
         Latitude_4, Longitude_4, Latitude_5,Longitude_5, Latitude_6, Longitude_6) %>%
glimpse

coordinate %>%
  expand.grid(coordinate,Latitude_1, Latitude_2) %>%
  head()



library(tidyr)
pop_wide <- read.csv("http://en.openei.org/doe-opendata/dataset/a7fea769-691d-4536-8ed3-471e993a2445/resource/86c50aa8-e40f-4859-b52e-29bb10166456/download/populationbycountry19802010millions.csv", stringsAsFactors = FALSE, check.names=FALSE)
###3Glimpse of the structure of the data
glimpse(pop_wide)
View(pop_wide)

##Assign column name to the first column
colnames(pop_wide)[1] <- "country"
View(pop_wide)

##Combine the year columns into a single column with separate rows for each
pop_long <- gather(pop_wide, year, population, -country)
View(pop_long)
View(pop_wide)

colnames(Longitude_df)[7] <- c("result")
View(Longitude_df)

##Combine the 
longitude <- gather(Longitude_df, index,longitude, -result )
View(longitude)

colnames(pilot_df)[7] <- c("result")
View(pilot_df)

##Combine the <- 
latitude <- gather(pilot_df, index, latitude, -result)
View(latitude)

new_data <- latitude
new_data$latitude <- as.character.POSIXt(latitude$latitude)

        ###Merging and mutating first
new_data <- latitude
latitude <- latitude %>%
  select(latitude,result) %>%
  mutate(no = 1:nrow(new_data)) %>%
  glimpse()

new_longitude <- longitude

longitude <- longitude %>%
  select(longitude, result) %>%
  mutate(no = 1:nrow(new_longitude)) %>%
  glimpse()

##Merge function
coordinate <- merge(latitude, longitude, by = "no", all.y = TRUE)
colnames(coordinate)

coordinate <- coordinate %>%
  rename(points = no) %>%
  glimpse


View(coordinate)



































