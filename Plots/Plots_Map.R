# LOAD LIBRARIES
Packages <- c("tidyverse","plyr","maps")
lapply(Packages, library, character.only = TRUE)

#### BIOGEOGRAPHICAL MAP #######

Heterogeneity.data.map<-Heterogeneity.data.model %>%
  distinct(Effects_group, .keep_all = TRUE) %>%
  mutate(
    Coordinate.X_round2 = round_any(Coordinate.X, 5, ceiling),
    Coordinate.Y_round2 = round_any(Coordinate.Y, 5, ceiling),
    Coordinate.X_round = round_any(Coordinate.X, 10, ceiling),
    Coordinate.Y_round = round_any(Coordinate.Y, 10, ceiling)
  ) %>%
  filter(
    Taxonomic.grouping != "physicalprocess"
  ) %>%
  mutate(
    Taxonomic.grouping = ifelse(Taxonomic.grouping=="mixed", "macroinvertebrates",Taxonomic.grouping)
  ) %>%
  group_by(Coordinate.X_round, Coordinate.Y_round, Taxonomic.grouping) %>%
  dplyr::mutate(
    n_ES = n(),
    n_Study = length(unique(Title)),
    Coordinate.X = Coordinate.X,
    Coordinate.Y=Coordinate.Y
  ) %>% 
  filter(!duplicated(paste(Coordinate.X_round, Coordinate.Y_round)))


ggplot() + 
  geom_polygon(data=map_data("world"), aes(x=long, y=lat, group=group), 
               colour="grey", fill="grey")+
  geom_point(data=Heterogeneity.data.map, 
             aes(x=Coordinate.X, y=Coordinate.Y, size=n_Study),
             alpha=0.6, colour="Red", fill="Red")+ 
  coord_fixed() +
  xlab("") + ylab("")+ 
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(), 
        panel.background = element_rect(fill = 'white', colour = 'white'), 
        axis.line = element_line(colour = "white"),
        axis.ticks=element_blank(), axis.text.x=element_blank(),
        axis.text.y=element_blank())


ggplot(Heterogeneity.data.map, aes(x=Coordinate.Y)) +
  geom_density(fill="grey", col="grey") +
  theme_classic()

ggplot(Heterogeneity.data.map, aes(x=Coordinate.X)) +
  geom_density(fill="grey", col="grey") +
  theme_classic()

#


#### SUMMARY PLOTS ####

# FACET GRAPHS
Het_data_facets<-Heterogeneity.data.model %>% 
  filter ( 
    (Response_metric=="Species_richness" | Response_metric == "Abundance") #&
  ) %>%
  group_by(Heterogeneity.facet) %>%
  dplyr::summarise(
    Title_sum = length(unique(Title)),
    ES_sum = length(unique(Effects_group))
  )

ggplot(Het_data_facets, aes(x=reorder(Heterogeneity.facet, Title_sum, sum), y=Title_sum)) + 
  geom_bar(stat='identity', fill="#003399") + 
  coord_flip() +
  scale_y_reverse()+
  theme_bw()

ggplot(Het_data_facets, aes(x=reorder(Heterogeneity.facet, ES_sum, sum), y=ES_sum)) + 
  geom_bar(stat='identity', fill="#003399", alpha=0.5) + 
  coord_flip() +
  scale_x_discrete(position = "top")+
  theme_bw()

# ORGANISMAL GRAPHS
Het_data_orgs<-Heterogeneity.data.model %>% 
  filter(
    Taxonomic.grouping!="physicalprocess" &
      (Response_metric != "Body size" & Response_metric != "cover" & Response_metric!="Grazing" & 
         Response_metric!="occurrence" & Response_metric != "Predation" & Response_metric != "productivity" &
         Response_metric !="Recruitment")
  ) %>%
  group_by(Taxonomic.grouping) %>%
  dplyr::summarise(
    Title_sum = length(unique(Title)),
    ES_sum = length(unique(Effects_group))
  )

ggplot(Het_data_orgs, aes(x=reorder(Taxonomic.grouping, Title_sum, sum), y=Title_sum)) + 
  geom_bar(stat='identity', fill="#990033") + 
  coord_flip() +
  scale_y_reverse()+
  theme_bw()

ggplot(Het_data_orgs, aes(x=reorder(Taxonomic.grouping, ES_sum, sum), y=ES_sum)) + 
  geom_bar(stat='identity',fill="#990033", alpha=0.5) + 
  coord_flip() +
  scale_x_discrete(position = "top")+
  theme_bw()

# ECO PROCESSES GRAPHS
Het_data_ecop<-Heterogeneity.data.model %>% 
  filter(
    Taxonomic.grouping!="physicalprocess" &
      (Response_metric == "Body size" | Response_metric=="Grazing" |
         Response_metric == "Predation" | Response_metric =="Recruitment")
  ) %>%
  group_by(Response_metric) %>%
  dplyr::summarise(
    Title_sum = length(unique(Title)),
    ES_sum = length(unique(Effects_group))
  )

ggplot(Het_data_ecop, aes(x=reorder(Response_metric, Title_sum, sum), y=Title_sum)) + 
  geom_bar(stat='identity', fill="#006633") + 
  coord_flip() +
  scale_y_reverse()+
  theme_bw()

ggplot(Het_data_ecop, aes(x=reorder(Response_metric, ES_sum, sum), y=ES_sum)) + 
  geom_bar(stat='identity',fill="#006633", alpha=0.5) + 
  coord_flip() +
  scale_x_discrete(position = "top")+
  theme_bw()

#