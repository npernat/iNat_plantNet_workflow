# tree maps

# Flower color and type of interactions, treemaps ####

# 0. packages ####
install.packages("treemapify")
library(treemapify)

# 1. Flower color ####
# chi-square test
iso_col_cont<-as.data.frame.matrix(table(imex_region$continents,imex_region$colour))
names(iso_col_cont)
chisq.test(iso_col_cont[,-c(3:7)])
# significantly different

# create dataframe with flower color frequencies and proportions by continent
color_tm<-as.data.frame(imex_region) %>% 
  select(colour, continents) %>% 
  filter(colour!="unidentifiable" & colour!="noflower" & 
           colour!="need_id" & colour!="noplant" & colour!="NA") %>% 
  droplevels() %>% 
  na.omit(colour) %>% 
  group_by(continents, colour) %>% 
  tally() %>% 
  mutate(percent = round(n / sum(n)*100,1))

# create a treemap
# customized color palette
scale_fill_palette<-c(
  "Europe.blue"= "blue",             
  "Europe.green" = "green" ,           
  "Europe.pink" =  "deeppink1"   ,        
  "Europe.purple" = "purple" ,          
  "Europe.white"  = "white" ,          
  "Europe.white/yellow"= "lemonchiffon" ,     
  "Europe.yellow" =  "yellow" ,        
  "North America.blue"=   "blue"   ,   
  "North America.pink"=  "deeppink1" ,        
  "North America.purple" =  "purple" ,   
  "North America.white" = "white"     ,
  "North America.white/pink" ="lightpink1" ,
  "North America.white/yellow"="lemonchiffon" ,
  "North America.yellow"="yellow")


tiff("./output/color_continents.tiff", units="px",width = 4000,height = 2000,res = 360)
color_tm %>% 
  mutate(group = paste(continents, colour, sep = ".")) %>% # create group variable for customized color scheme
  filter(percent >2) %>% 
  ggplot(., aes(area = percent, label = paste0(colour, "\n", percent,"%"), fill = group)) +
  geom_treemap()+
  geom_treemap_text(colour = "black",
                    place = "centre",
                    size = 15)+
  scale_fill_manual(values = scale_fill_palette) +
  theme(legend.position = "none",
        strip.background=element_rect(fill=viridis(1, begin = 0.1)), # sustomize facet_wrap bar
        strip.text = element_text(color = "white", face = "bold", size=15))+ # customize facet_wrap heading 
  facet_wrap(~continents)
dev.off()


# 2. Types of interaction ####
table(imex_region$interaction) # check data
imex_region$interaction<-gsub("nest_preparing", "nesting", imex_region$interaction)
imex_region$interaction<-gsub("feeding", "visiting", imex_region$interaction)
imex_region$interaction<-gsub("nowasp", "nesting", imex_region$interaction)

# create dataframe with category "others" for single observations  
interaction_tm<-as.data.frame(imex_region) %>% 
  select(interaction) %>% 
  filter(interaction!="unidentifiable" & interaction!="noflower" & 
           interaction!="need_id" & interaction!="noplant" & interaction!="NA") %>% 
  droplevels() %>% 
  na.omit(interaction) %>% 
  count(interaction) %>% 
  mutate(group = case_when(
    interaction == "dead"
    |interaction == "flying"
    |interaction == "mating"
    |interaction == "preyed"
    |interaction == "collection"~ "other",
    TRUE~as.character(interaction))) %>% 
  group_by(group) %>% 
  reframe(n=sum(n)) %>% 
  mutate(percent = round(n / sum(n)*100,1)) 
  
# create treemap
tiff("./output/interaction_continents.tiff", units="px",width = 3000,height = 1000,res = 360)
interaction_tm %>% 
  ggplot(., aes(area = percent, label = paste0(group, "\n", percent, " %"), fill = group)) +
  geom_treemap()+
  geom_treemap_text(place = "centre",
                    size = 15,
                    color="white")+
  scale_fill_manual(values = viridis(6, begin=0.1, end=0.6)) +
  labs(title = "Type of interactions")+
    theme(legend.position = "none")
dev.off()

