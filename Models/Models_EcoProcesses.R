# LOAD LIBRARIES
Packages <- c("tidyverse","plyr","lme4","lmerTest","DHARMa","ggeffects","effects")
lapply(Packages, library, character.only = TRUE)

#### ECOLOGICAL PROCESSES - MODELS ####

#grazing
model_graz<-lmer(log(Response_ratio) ~
                   Heterogeneity_01+I(Heterogeneity_01^2)+
                   Het.facet+
                   Heterogeneity.cv+
                   #Spatial.scale.class + # not significant
                   #Taxonomic.grouping + # only one taxonomic group, so not possible to include
                   #Units.class + # rank deficient
                   #Year + # not significant
                   Sample_size + 
                   
                   (1|Effects_group)+
                   (0+(Heterogeneity_01)|Study.included.ID/Effects_group)
                 ,
                 (data_graz<-Heterogeneity.data.model %>% filter ( Response_metric == "Grazing"))
); plot(simulateResiduals(model_graz)); summary(model_graz); plot(allEffects(model_graz))

#predation
model_pred<-lmer(log(Response_ratio) ~
                   Heterogeneity_01+I(Heterogeneity_01^2)+
                   Het.facet+
                   Heterogeneity.cv+
                   Taxonomic.grouping +
                   #Spatial.scale.class + # not significant
                   #Units.class + # rank deficient
                   #Year + # not significant
                   #Sample_size + # not significant
                                        
                   (0+(Heterogeneity_01)|Study.included.ID/Effects_group)+
                   (1|Effects_group)
                 ,
                 (data_pred<-Heterogeneity.data.model %>% filter ( Response_metric == "Predation"))
); plot(simulateResiduals(model_pred)); summary(model_pred); plot(allEffects(model_pred))

#body size
model_Bod<-lmer(log(Response_ratio) ~
                  Heterogeneity_01+I(Heterogeneity_01^2)+
                  Het.facet+
                  Heterogeneity.cv+
                  Taxonomic.grouping +
                  #Spatial.scale.class + # not significant
                  #Units.class + # rank deficient
                  #Year + # not significant
                  #Sample_size + # not significant
                  
                  (0+(Heterogeneity_01)|Study.included.ID/Effects_group)+
                  (1|Effects_group)
                ,
                (data_bod<-Heterogeneity.data.model %>% filter ( Response_metric == "Body size"))
); plot(simulateResiduals(model_Bod)); summary(model_Bod); plot(allEffects(model_Bod))2

#recruitment
model_Rec<-lmer(log(Response_ratio) ~
                  Heterogeneity_01 + I(Heterogeneity_01^2) +
                  Het.facet +
                  Heterogeneity.cv +
                  Heterogeneity_01 * Taxonomic.grouping +
                  #Spatial.scale.class + # not significant
                  #Units.class + # rank deficient
                  #Year + # not significant
                  #Sample_size + # not significant
                  
                  (0+(Heterogeneity_01)|Study.included.ID/Effects_group)+
                  (1|Effects_group)
                ,
                (data_rec<-Heterogeneity.data.model %>% filter (Response_metric == "Recruitment"))
); plot(simulateResiduals(model_Rec)); summary(model_Rec); plot(allEffects(model_Rec))

#
#### ECOLOGICAL PROCESSES - POSTHOC TESTS ####

lsmeans(model_graz, pairwise ~ Het.facet, data=data_graz, adjust = "none", pbkrtest.limit = 8000)
lsmeans(model_Rec, pairwise ~ Het.facet, data=data_rec, adjust = "none", pbkrtest.limit = 8000)
lsmeans(model_pred, pairwise ~ Het.facet, data=data_pred, adjust = "none", pbkrtest.limit = 8000)
lsmeans(model_Bod, pairwise ~ Het.facet, data=data_bod, adjust = "none", pbkrtest.limit = 8000)

lsmeans(model_Rec, pairwise ~ Taxonomic.grouping, data=data_rec, adjust = "none", pbkrtest.limit = 8000)
lsmeans(model_Bod, pairwise ~ Taxonomic.grouping, data=data_bod, adjust = "none", pbkrtest.limit = 8000)

#