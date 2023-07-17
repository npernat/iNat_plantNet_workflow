# Matches and barplots and species list####

# 0. packages ####
# library(tidyvers)
install.packages("viridis")
library(viridis)

# 1. Matching species ####
sm<-imex_region[imex_region$latin_name==imex_region$species_exp,] # matching plant species identifications
table(sm$species_exp)


# create dataframe for plots, score > 0.5
sm_df <- as.data.frame(sm) %>%
  filter(score>=0.5) %>% 
  select(species_exp, continents) %>% 
  group_by(species_exp) %>% 
  count(continents)
sm_df$continents<-factor(sm_df$continents, levels=c("North America", "Europe"))


lbls_sm = paste0(as.character(c(seq(10, 0, -2), seq(2, 18,2)))) # customized labels

tiff("./output/speciesmatches.tiff", units="px",width = 3200,height = 2000,res = 360)
p2<-sm_df %>% 
  mutate(counts=ifelse(continents=="Europe", -n, n)) %>% 
  ggplot(., aes(x=species_exp, y=counts, fill=continents))+
  geom_bar(stat="identity", width=0.25) +
  scale_fill_viridis(discrete = TRUE, begin = 0.1, end = 0.4)+
  geom_point(aes(color=continents), stat="identity", size=2.5, show.legend = F) +
  scale_color_viridis(discrete = TRUE, begin = 0.1, end = 0.4) +
  scale_y_continuous(breaks = seq(-10,18,2), labels=lbls_sm) +
  coord_flip() +
  theme_bw() +
  theme(axis.text.y = element_text(size=8, face="italic"),
        axis.title.y = element_blank(),
        axis.title.x = element_text(size=10),
        axis.text.x = element_text(size=10),
        legend.text = element_text(size=10),        
        legend.position = c(0.75,0.2),
        legend.background = element_rect(colour = "grey", fill="grey90"),
        panel.grid.minor = element_blank()) + 
  labs(y="Number of matches", fill="Observation from")+
  guides(color="none")
dev.off()

# 2. Matching genera ####
gm<-imex_region[imex_region$genus==imex_region$genus_exp,]
str(gm)

gm_df <- as.data.frame(gm) %>%
  filter(score>=0.5) %>% 
  select(genus, continents) %>% 
  group_by(genus) %>% 
  count(continents)

lbls_gm = paste0(as.character(c(seq(10, 0, -2), seq(2, 36,2)))) # customized labels

tiff("./output/genusmatches.tiff", units="px",width = 3200,height = 2000,res = 360)
p1<-gm_df %>% 
  mutate(counts=ifelse(continents=="Europe", -n, n)) %>% 
  ggplot(., aes(x=genus, y=counts, fill=continents))+
  geom_bar(stat="identity", width=0.25) +
scale_fill_viridis(discrete = TRUE, begin = 0.1, end = 0.4)+
  geom_point(aes(color=continents), stat="identity", size=2.5, show.legend = F) +
  scale_color_viridis(discrete = TRUE, begin = 0.1, end = 0.4) +
  scale_y_continuous(breaks = seq(-10,36,2), labels=lbls_gm) +
  coord_flip() +
  theme_bw() +
  theme(axis.text.y = element_text(size=8, face="italic"),
        axis.title.y = element_blank(),
        axis.title.x = element_text(size=10),
        axis.text.x = element_text(size=10),
        legend.text = element_text(size=10),
        legend.position = c(0.75,0.2),
        legend.background = element_rect(colour = "grey", fill="grey90"),
        panel.grid.minor = element_blank()) + 
  labs(y="Number of matches", fill="Observation from")+
  guides(color="none")
dev.off()



# 3. Matching families ####
fm<-imex_region[imex_region$family==imex_region$family_exp,]
str(fm)

fm_df <- as.data.frame(fm) %>%
  filter(score>=0.5) %>% 
  select(family_exp, continents) %>% 
  group_by(family_exp) %>% 
  count(continents)
fm_df$continents<-factor(fm_df$continents, levels=c("North America", "Europe"))
table(fm_df$n)

lbls_fm = paste0(as.character(c(seq(20, 0, -2), seq(2, 42,2)))) # customized labels

tiff("./output/familymatches.tiff", units="px",width = 3200,height = 2000,res = 360)
p3<-fm_df %>% 
  mutate(counts=ifelse(continents=="Europe", -n, n)) %>% 
  ggplot(., aes(x=family_exp, y=counts, fill=continents))+
  geom_bar(stat="identity", width=0.15) +
  scale_fill_viridis(discrete = TRUE, begin = 0.1, end = 0.4)+
  geom_point(aes(color=continents), stat="identity", size=2.5, show.legend = F) +
  scale_color_viridis(discrete = TRUE, begin = 0.1, end = 0.4) +
  scale_y_continuous(breaks = seq(-20,42,2), labels=lbls_fm) +
  coord_flip() +
  theme_bw() +
  theme(axis.text.y = element_text(size=8, face="italic"),
        axis.title.y = element_blank(),
        axis.title.x = element_text(size=10),
        axis.text.x = element_text(size=10),
        legend.text = element_text(size=10),        
        legend.position = c(0.75,0.15),
        legend.background = element_rect(colour = "grey", fill="grey90"),
        panel.grid.minor = element_blank()) + 
  labs(y="Number of matches", fill="Observation from")+
  guides(color="none")
dev.off()

library(cowplot)
tiff("./output/panel_matches.tiff", units="px",width = 3200,height = 5000,res = 360)
plot_grid(p2,p1,p3, nrow=3, labels = c("a", "b", "c"), align = "v")
dev.off()

# 5. Species list (Supporting table 1)
sort(unique(imex_region$species_exp))

# this list does not match with the figures as it also takes into accounts scores < 0.5
speclist<-as.data.frame(imex_region) %>% 
  select(species_exp, latin_name, continents) %>% 
  mutate(agreement = ifelse(species_exp==latin_name, 1, 0)) %>% 
  filter(species_exp!="unidentifiable" & species_exp!="noplant"
         & species_exp!="noflower" & species_exp!="need_id") %>% 
  group_by(species_exp, continents) %>% 
  summarise(matches=sum(agreement)) %>% 
  arrange(desc(species_exp))

spec_counts<-as.data.frame(imex_region) %>% 
  select(species_exp, latin_name, continents) %>% 
  mutate(agreement = ifelse(species_exp==latin_name, 1, 0)) %>% 
  filter(species_exp!="unidentifiable" & species_exp!="noplant"
         & species_exp!="noflower" & species_exp!="need_id") %>% 
  group_by(species_exp, continents) %>% 
  count(species_exp) %>% 
  arrange(desc(species_exp))


speclist_freq_match<-cbind(speclist, spec_counts[c(1,3)])
dim(speclist_freq_match)

write.csv2(speclist_freq_match, "./output/species_list.xls")
dim(speclist_freq_match)
