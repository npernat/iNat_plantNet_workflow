# Dataset preparations ####

# 0. Packages ####
install.packages("rinat")
install.package("zoo")
install.packages("httr")
install.packages("tidyverse")
install.packages("cli")
devtools::install_github(repo = 'BiologicalRecordsCentre/plantnet')
install.packages("stringr")
install.packages("taxize")
library(rinat)
library(zoo)
library(httr)
library(tidyverse)
library(cli)
library(plantnet)
library(stringr)
library(taxize)

# 1. retrieving images from iNaturalist ####

inat<-get_inat_obs(taxon_name="Isodontia mexicana", maxresults = 10000) 

# get iNaturalist observation of I. mexicana, but it only works for one year at a time
# adapt function to work for more than one year

years<-data.frame(year=c(2007:2021)) # define the years observations should be downloaded
inat_fun<-function(x){get_inat_obs_nostop(taxon_name = "Isodontia mexicana", meta=F,
                                          maxresults = 10000, year=x, quality="research")}
# *I adapted the function so that it won't stop when a result is zero for a year

Isodontia<-apply(years, 1, inat_fun) # loop over years
names(Isodontia)<-as.character(c(2007:2021)) # list of observations, years are list elements
Isodontia_df<-bind_rows(Isodontia) # create a dataframe

# check dataframe
dim(Isodontia_df)
head(Isodontia_df$taxon_id)
names(Isodontia_df)
iso_df<-Isodontia_df[,c(1,5,6,9,10,19,22,37)] # subset of interesting variables to keep
iso_df$SampleID<-c(1:nrow(iso_df)) # create ID number for merging datasets later
# iso_df was exported for the Expert identification


# 2. Feed iNaturalist image URLs in PlantNet App and prepare dataset for merging with expert data ####

# 2.1 create function for plantNet App to run over more than one URL ####
# get the key at https://my.plantnet.org/usage
library(readxl)
isomex_experts <- read_excel("data/isomex_experts.xlsx")
str(isomex_experts)

img_iso<-data.frame(imageURL=isomex_experts$image_url) # create a dataframe with all image URLs
key = "yourAPIkey"
fun_plantnet_all_simp<-function(x){identify(key, imageURL=x, no_reject = 'true',
                                            simplify="TRUE")}

iso_all<-apply(data.frame(img_iso[,1]), 1, fun_plantnet_all_simp) # loop functions over all image URLs


# save results
iso_all_id<-mapply(cbind, iso_all, "SampleID"=1:length(iso_all), SIMPLIFY=F)
iso_all_url<-mapply(cbind, iso_all_id, "imageURL"= img_iso$imageURL)
notfound<-as.vector(which(sapply(iso_all_url, function(x) is.element("Species Not Found", x))))
iso_all_id_cc<-iso_all_url[-c(notfound)] # create a list with complete cases
iso_all_df<-data.frame(do.call(rbind, iso_all_id_cc))
iso_new<-as.data.frame(apply(iso_all_df, 2, unlist))
iso_new$score<-as.numeric(iso_new$score)
iso_new$SampleID<-as.integer(iso_new$SampleID)
str(iso_new)
write.csv2(iso_new, "iso_new.xls") # save for backup

# create second dataset with only the first candidate suggestion
imex<-iso_new # rename
dim(imex)

imex_first<-imex %>% 
  group_by(SampleID) %>% 
  slice_head()
imex_first<-as.data.frame(imex_first)
dim(imex_first)
write.csv2(imex_first, "./data/imex_first.csv")


# 2.2 Extract genus and family name from species candidates ####

# get genus name from package stringr

library(stringr)
imex_first$genus<-word(imex_first$latin_name, 1) # gets the first word aka. the genus 
table(imex_first$genus)

# get family name from package taxize, command plantminer
family<-plantminer(imex_first$latin_name, from="tpl")
head(family)
imex_first$family<-family$family
str(imex_first)

# 3. Merge expert and plantNet dataset ####

# isomex_expert was imported after expert verification of URLs
isomex_experts<-isomex_experts %>% 
  rename(sampleID=SampleID)
imex_joined<-left_join(imex_first, isomex_experts, by="SampleID")
str(imex_joined)
dim(imex_joined)

