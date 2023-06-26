# data analysis

# 
# 0. Packages
install.packages("tidyverse")
install.packages("ggplot2")
install.packages("viridis")
library(tidyverse)
library(ggplot2)
library(viridis)

# 1. characteristics of App and Expert data ####
# App scores and candidate lists
library(tidyverse)

imex %>% 
  count(SampleID) %>% 
  summarize(mean=mean(n), max=max(n), min=min(n), median=quantile(n,0.5))
# a mean of 67 suggestions per image, with the maximum of 164 and a 
# minimum of 1 (for species not found)

imex %>% 
  group_by(SampleID) %>% 
  select(score) %>% 
  drop_na() %>% 
  reframe(score = mean(score)) %>% 
  summarize(mean=mean(score), max=max(score), min=min(score), median=quantile(score,0.5))
# considering all candidate scores, but grouped by sampleID,
# the mean is 0.0185 with a max of 0.333 and a min of 0.00134

imex_joined %>% 
  select(score) %>% 
  drop_na() %>% 
  summarize(mean=mean(score), max=max(score), min=min(score), median=quantile(score,0.5))
# considering only the first candidate scores, the mean is 0.21, max = 0.98 and min = 0.001

imex_joined %>% 
  select(SampleID, score) %>% 
  drop_na() %>% 
  group_by(SampleID) %>% 
  summarize(mean=sum(score)/length(SampleID)) %>% 
  summarize(mean_all=sum(mean)/length(mean))

# Experts identification frequencies
# some characteristics of expert and app identification
# proportion of identifiable/unidentifiable

imex_joined %>% 
  select(species_exp) %>% 
  mutate(spec_exp=case_when(
    species_exp!="unidentifiable"
    & species_exp!="noflower"
    & species_exp!="need_id"
    & species_exp!="noplant"~"identified",
    TRUE ~ as.character(species_exp)
  )) %>% 
  group_by(spec_exp) %>% 
  tally() %>% 
  mutate(prop=n/sum(n)) %>% 
  write.csv2(.,"output_new/species_expert_identification.xls")


imex_joined %>% 
  select(genus_exp) %>% 
  mutate(gen_exp=case_when(
    genus_exp!="unidentifiable"
    & genus_exp!="noflower"
    & genus_exp!="need_id"
    & genus_exp!="noplant"~"identified",
    TRUE ~ as.character(genus_exp)
  )) %>% 
  group_by(gen_exp) %>% 
  tally() %>% 
  mutate(prop=n/sum(n)) %>% 
  write.csv2(.,"output_new/genus_expert_identification.xls")

imex_joined %>% 
  select(family_exp) %>% 
  mutate(fam_exp=case_when(
    family_exp!="unidentifiable"
    & family_exp!="noflower"
    & family_exp!="need_id"
    & family_exp!="noplant"~"identified",
    TRUE ~ as.character(family_exp)
  )) %>% 
  group_by(fam_exp) %>% 
  tally() %>% 
  mutate(prop=n/sum(n)) %>% 
  write.csv2(.,"output_new/family_expert_identification.xls")

# 2. Matching species, genera and family ####

# 2.1 including all candidates
# species 
isomex_250_scores %>% 
  filter(species_exp!="noflower" 
         & species_exp!="need_id"
         & species_exp!="unidentifiable") %>% 
  mutate(agree=case_when(
    latin_name==species_exp~"yes",
    latin_name!=species_exp~"no"))%>% 
  count(agree)%>% 
  mutate(perc=n/sum(n))  

# genus
isomex_250_scores%>%  
  filter(genus_exp!="noflower" 
         & genus_exp!="need_id"
         & genus_exp!="unidentifiable") %>% 
  mutate(agree=case_when(
    genus==genus_exp~"yes",
    genus!=genus_exp~"no"))%>% 
  count(agree)%>% 
  mutate(perc=n/sum(n))  

# family
isomex_250_scores%>% 
  filter(family_exp!="noflower" 
         & family_exp!="need_id"
         & family_exp!="unidentifiable") %>% 
  mutate(agree=case_when(
    family_exp==family~"yes",
    family_exp!=family~"no"))%>% 
  count(agree)%>% 
  mutate(perc=n/sum(n))  

# 2.2. Including only the first candidate (scores) of the App

# Agreement with cutting scores - no threshold 

# species 
imex_joined %>% 
  filter(species_exp!="noflower" 
         & species_exp!="need_id"
         & species_exp!="unidentifiable") %>% 
  mutate(agree=case_when(
    latin_name==species_exp~"yes",
    latin_name!=species_exp~"no"))%>% 
  count(agree)%>% 
  mutate(perc=n/sum(n))  

# genus
imex_joined %>%  
  filter(genus_exp!="noflower" 
         & genus_exp!="need_id"
         & genus_exp!="unidentifiable") %>% 
  mutate(agree=case_when(
    genus==genus_exp~"yes",
    genus!=genus_exp~"no"))%>% 
  count(agree)%>% 
  mutate(perc=n/sum(n))  

# family
imex_joined%>% 
  filter(family_exp!="noflower" 
         & family_exp!="need_id"
         & family_exp!="unidentifiable") %>% 
  mutate(agree=case_when(
    family_exp==family~"yes",
    family_exp!=family~"no"))%>% 
  count(agree)%>% 
  mutate(perc=n/sum(n))  

# Agreements with cutting scores >0.8%
# species 
imex_joined %>% 
  filter(score>=0.80) %>%
  filter(species_exp!="noflower" 
         & species_exp!="need_id"
         & species_exp!="unidentifiable") %>% 
  mutate(agree=case_when(
    latin_name==species_exp~"yes",
    latin_name!=species_exp~"no"))%>% 
  count(agree)%>% 
  mutate(perc=n/sum(n))  
# this shows 4 + 1; however, the total number of observations of scores over 0.8 is n=6
# as "Mentha spicata" was identified right by the experts and the App in two observations
# therefore, the right result is 5 (83.3%).

# genus
imex_joined %>%  
  filter(score>=0.80) %>%
  filter(genus_exp!="noflower" 
         & genus_exp!="need_id"
         & genus_exp!="unidentifiable") %>% 
  mutate(agree=case_when(
    genus==genus_exp~"yes",
    genus!=genus_exp~"no"))%>% 
  count(agree)%>% 
  mutate(perc=n/sum(n))  

# family
imex_joined %>% 
  filter(score>=0.80) %>%
  filter(family_exp!="noflower" 
         & family_exp!="need_id"
         & family_exp!="unidentifiable") %>% 
  mutate(agree=case_when(
    family_exp==family~"yes",
    family_exp!=family~"no"))%>% 
  count(agree)%>% 
  mutate(perc=n/sum(n))  


# Agreements with cutting scores >0.5

# species 
imex_joined %>% 
  filter(score>=0.50) %>% 
  filter(species_exp!="noflower" 
         & species_exp!="need_id"
         & species_exp!="unidentifiable") %>% 
  mutate(agree=case_when(
    latin_name==species_exp~"yes",
    latin_name!=species_exp~"no"))%>% 
  count(agree)%>% 
  mutate(perc=n/sum(n))  

# genus
imex_joined %>%  
  filter(score>=0.50) %>%
  filter(genus_exp!="noflower" 
         & genus_exp!="need_id"
         & genus_exp!="unidentifiable") %>% 
  mutate(agree=case_when(
    genus==genus_exp~"yes",
    genus!=genus_exp~"no"))%>% 
  count(agree)%>% 
  mutate(perc=n/sum(n))  

# family
imex_joined %>% 
  filter(score>=0.50) %>%
  filter(family_exp!="noflower" 
         & family_exp!="need_id"
         & family_exp!="unidentifiable") %>% 
  mutate(agree=case_when(
    family_exp==family~"yes",
    family_exp!=family~"no"))%>% 
  count(agree)%>% 
  mutate(perc=n/sum(n)) 

# Agreements with cutting scores >0.3

# species 
imex_joined %>% 
  filter(score>=0.30) %>%
  filter(species_exp!="noflower" 
         & species_exp!="need_id"
         & species_exp!="unidentifiable") %>% 
  mutate(agree=case_when(
    latin_name==species_exp~"yes",
    latin_name!=species_exp~"no"))%>% 
  count(agree)%>% 
  mutate(perc=n/sum(n))  

# genus
imex_joined %>%  
  filter(score>=0.30) %>%
  filter(genus_exp!="noflower" 
         & genus_exp!="need_id"
         & genus_exp!="unidentifiable") %>% 
  mutate(agree=case_when(
    genus==genus_exp~"yes",
    genus!=genus_exp~"no"))%>% 
  count(agree)%>% 
  mutate(perc=n/sum(n))  

# family
imex_joined %>% 
  filter(score>=0.30) %>%
  filter(family_exp!="noflower" 
         & family_exp!="need_id"
         & family_exp!="unidentifiable") %>% 
  mutate(agree=case_when(
    family_exp==family~"yes",
    family_exp!=family~"no"))%>% 
  count(agree)%>% 
  mutate(perc=n/sum(n)) 

# # Agreements with cutting scores <0.3

# species 
imex_joined %>% 
  filter(score<0.30) %>%
  filter(species_exp!="noflower" 
         & species_exp!="need_id"
         & species_exp!="unidentifiable") %>% 
  mutate(agree=case_when(
    latin_name==species_exp~"yes",
    latin_name!=species_exp~"no"))%>% 
  count(agree)%>% 
  mutate(perc=n/sum(n))  

# Agreements with cutting scores - genus
imex_joined %>%  
  filter(score<0.30) %>%
  filter(genus_exp!="noflower" 
         & genus_exp!="need_id"
         & genus_exp!="unidentifiable") %>% 
  mutate(agree=case_when(
    genus==genus_exp~"yes",
    genus!=genus_exp~"no"))%>% 
  count(agree)%>% 
  mutate(perc=n/sum(n))  

# Agreements with cutting scores - family
imex_joined %>% 
  filter(score<0.30) %>%
  filter(family_exp!="noflower" 
         & family_exp!="need_id"
         & family_exp!="unidentifiable") %>% 
  mutate(agree=case_when(
    family_exp==family~"yes",
    family_exp!=family~"no"))%>% 
  count(agree)%>% 
  mutate(perc=n/sum(n)) 

