# Data cleaning ####

# 1. check consistent use of family names ####
sim_family<-intersect(imex_joined$family, imex_joined$family_exp)
sim_family
dissim_family<-setdiff(union(imex_joined$family, imex_joined$family_exp), sim_family)
sort(dissim_family) # check the spelling of the dissimilar families

# correct spellings
corr_family<-c("apocynaceae"="Apocynaceae", "Caryophillaceae"="Caryophyllaceae",
                      "Mentha"="Lamiaceae", "no plant"="noplant", "Rubus" = "Rosaceae",
                      "unindetifiable"="unidentifiable", "Apiaceaea"="Apiaceae",
                      "Apiaceaeae"="Apiaceae")

imex_joined$family <- ifelse(imex_joined$family %in% names(corr_family), 
                             corr_family[imex_joined$family], imex_joined$family)

imex_joined$family_exp <- ifelse(imex_joined$family_exp %in% names(corr_family), 
                                 corr_family[imex_joined$family_exp], imex_joined$family_exp)

# for some species a "" was given by taxize::plantminer for the family, checking:
sort(unique(imex_joined[which(imex_joined$family==""),]$latin_name))

imex_joined$family[imex_joined$latin_name == "Brugmansia spp."] <- "Solanaceae"
imex_joined$family[imex_joined$latin_name == "Campsis x tagliabuana"] <- "Bignoniaceae"
imex_joined$family[imex_joined$latin_name == "Cassia x nealiae"] <- "Fabaceae"
imex_joined$family[imex_joined$latin_name == "Chrysojasminum fruticans"] <- "Oleaceae"
imex_joined$family[imex_joined$latin_name == "Cynanchum laeve"] <- "Apocynaceae"
imex_joined$family[imex_joined$latin_name == "Dasiphora fruticosa"] <- "Rosaceae"
imex_joined$family[imex_joined$latin_name == "Elymus hystrix"] <- "Poaceae"
imex_joined$family[imex_joined$latin_name == "Equisetum x litorale"] <- "Equisetaceae"
imex_joined$family[imex_joined$latin_name == "Hypericum x desetangsii"] <- "Hypericaceae"
imex_joined$family[imex_joined$latin_name == "Mentha × rotundifolia"] <- "Lamiaceae"
imex_joined$family[imex_joined$latin_name == "Mentha × villosa"] <- "Lamiaceae"
imex_joined$family[imex_joined$latin_name == "Mentha × verticillata"] <- "Lamiaceae"
imex_joined$family[imex_joined$latin_name == "Rosa × damascena"] <- "Rosaceae"
imex_joined$family[imex_joined$latin_name == "Symphoricarpos x chenaultii"] <- "Caprifoliaceae"
imex_joined$family[imex_joined$latin_name == "Tordylium aegyptiacum"] <- "Apiaceae"
imex_joined$family[imex_joined$latin_name == "Sicyos angulata"] <- "Cucurbitaceae"
imex_joined$family[imex_joined$latin_name == "Urtica incisa"] <- "	Urticaceae"

# 2. check consistent use of genus names ####
sim_genus<-intersect(imex_joined$genus, imex_joined$genus_exp)
sim_genus
dissim_genus<-setdiff(union(imex_joined$genus, imex_joined$genus_exp), sim_genus)
sort(intersect(dissim_genus, imex_joined$genus_exp))

corr_genus<-c("need-id"="need_id", "no plant"="noplant", "Euonimus"="Euonymus",
                    "Muehlerbeckia"="Muehlenbeckia", "unindetifiable"="unidentifiable",
                    "Echinaceae"="Echinacea", "Ceanotus"="Ceanothus", "Hydranga"="Hydrangea")
                    
imex_joined$genus <- ifelse(imex_joined$genus %in% names(corr_genus), 
                            corr_genus[imex_joined$genus], imex_joined$genus)

imex_joined$genus_exp <- ifelse(imex_joined$genus_exp  %in% names(corr_genus), 
                                corr_genus[imex_joined$genus_exp ], imex_joined$genus_exp )

# 3. check consistent use of species names ####
sim_species<-intersect(imex_joined$species, imex_joined$species_exp)
sim_species
dissim_species<-setdiff(union(imex_joined$species, imex_joined$species_exp), sim_species)
sort(intersect(dissim_species, imex_joined$species_exp))

sort(unique(imex_joined$latin_name))
sort(unique(imex_joined$species_exp))


# 4. check consistent use of color values given by the experts ####
table(imex_joined$colour)

corr_color<-c("no plant"="noplant","no flower"="noflower","gtreen"="green",
              "white/pinl"="white/pink","whitw/yellow"="white/yellow",
              "green - white"="white/green","Purple"="purple",
              "purple/white"="white/purple" ,"White/yellow"="white/yellow",
              "blue/yellow"="yellow/blue","green/yellow"="yellow/green",
              "pink/yellow"="yellow/pink", "pink/purple"="purple/pink",
              "red-orange"="red/orange", "fuchsia/yellow"="yellow/pink",
              "lilac"="purple", "Purple/yellow"="purple/yellow")
imex_joined$colour <- ifelse(imex_joined$colour %in% names(corr_color), 
                            corr_color[imex_joined$colour], imex_joined$colour)
sort(unique(imex_joined$colour)) 
imex_joined[which(imex_joined$colour=="NA"),]  # NA is other interaction         
corr_species<-c("no plant"="noplant", "Euonimus japonicus"="Euonymus japonicus",
              "Muehlerbeckia complexa"="Muehlenbeckia complexa", "Hydranga"="Hydrangea",
              "green - white"="unidentifiable", "Cirsium arvensae"="Cirsium arvense",
              "Anaphalis margaritaceae"="Anaphalis margaritacea")

which(imex_joined$species_exp=="green - white")
imex_joined[18,]

imex_joined$species_exp <- ifelse(imex_joined$species_exp %in% names(corr_species), 
                             corr_species[imex_joined$species_exp], imex_joined$species_exp)
