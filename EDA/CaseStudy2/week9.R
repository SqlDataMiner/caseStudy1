## For Week 9
## load thetidyverse
library(tidyverse)

## setup ready to download data
# define the part of the url that is shared by all the historial datasets
url<-"https://www.metoffice.gov.uk/pub/data/weather/uk/climate/stationdata/"
# create a vector containing all of the places for which we want to download data (in no particular order)
places<-c("sheffield","yeovilton","durham","heathrow","newtonrigg","cambridge","bradford","oxford","suttonbonington","waddington","manston","shawbury","rossonwye")
# how many places have we got?
nplace<-length(places)
# append the ending that we know all the files share
places_file<-paste(places,"data.txt",sep="")
fullurl<-paste(url,places_file,sep="")
# since we will be downloading multiple files, it seems sensible to have them all in one directory (make the name here and create the directory when we download the data)
outputdir="MetOfficeData/"
# create the output file names (with the same syntax as the ones we will download)
outputfile=paste(outputdir,places_file,sep="")
# create the directory to hold the downloads
dir.create(outputdir)
## download the data
download.file(fullurl,outputfile,method='libcurl')

## find column headers
start<-vector(length=nplace)
for (i in 1:nplace){
  start[i] <- grep(read_lines(outputfile[1],skip=5,n_max=1),read_lines(outputfile[i]))
}

## extract all the spatial information from line 2 of each file and put into a dataframe
# read in line 2 from all of the files
line2<-lapply(outputfile,read_lines,skip=1,n_max=1)
# extract only the numbers from the the lines
spat_info <- unlist(regmatches(line2, gregexpr('\\(?[-0-9.]+', line2)))
# convert to dataframe
spat_info <- data.frame(matrix(parse_number(spat_info),ncol=5,byrow = T))

## Add variable names
colnames(spat_info)<-c("E","N","lat","long","alt")

## remove E/N and add place names
spat_info <- spat_info %>% select("lat","long","alt")
spat_info <- cbind(place=places, spat_info)

## a list to hold all of the weather data
# since datasets will be read one at a time, let's use a list to hold them
# each place will be one item in the list, and each list item will eventually hold all
# the data for that place, in a dataframe
alldata<-vector("list",length=nplace)
names(alldata)<-places

## function to read data from Met Office files
# function to read in raw Met Office weather and spatial data (in text format),
# tidy it up and return data in a format suitable for EDA
import_MetOffice_data <- function (file_to_read,start_line,spat_info){
  tmp<-read_lines(file_to_read)
  lines_in_file<-length(tmp)

  # remove the header lines
  tmp<-tmp[start_line:lines_in_file]
  # remove the row containing the unit information
  tmp<-tmp[-2]
  # make a new header for sun recorder info, replace the #s by 1s at the end of the relevant row
  tmp[1]<-tmp[1] %>% str_replace("sun","sun sun_recorder")
  tmp <- tmp %>% str_replace_all("#"," 1")
  # replace all --- by NAs and remove all mention of "Provisional" and "*"
  tmp<- tmp %>% str_replace_all("---","NA") %>% str_remove_all("Provisional") %>% str_remove_all("\\*")
  tmp<-read_table2(tmp)
  tmp$sun_recorder<-as.integer(tmp$sun_recorder)
  tmp <- cbind(spat_info,tmp)
  return(tmp)
}

## read data from the rest of locations
for (i in 1:nplace){
  alldata[i] <- lapply(outputfile[i],
                       import_MetOffice_data,
                       start_line=start[i],
                       spat_info[i,])
}
# a quick check to see that all looks OK
lapply(alldata,head)


###########################################################################

### For Week 8

##Getting started with EDA

# convert the list into a dataframe so that we can readily use tidyverse tools to help us
alldata <- do.call(rbind,alldata)

## Sheffield max temp
library(ggplot2)
ggplot(alldata %>% filter(place=="sheffield",mm==8) %>% select(yyyy,tmax))+
  aes(x=yyyy,y=tmax)+
  geom_line()+ labs(y = "Maximum August Temperature (celsius)", x = "Years (CE)")

## map of locations
library(leaflet)
leaflet() %>%
  setView(lng = -2.3, lat = 53.2, zoom = 5.5) %>%
  addProviderTiles(providers$Esri.WorldGrayCanvas) %>%
  addCircleMarkers(lng = spat_info[, "long"],
                   lat= spat_info[, "lat"],
                   popup = spat_info[,"place"],
                   fillColor = )


