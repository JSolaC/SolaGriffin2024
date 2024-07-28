# LOAD LIBRARIES
Packages <- c("tidyverse","plyr","lme4","lmerTest","DHARMa","ggeffects","effects", "emmeans")
lapply(Packages, library, character.only = TRUE)

#### ORGANISMAL GROUPS - MODELS #########

# macroinvertebrates

Macroinv_model<-lmer(log(Response_ratio) ~
                        Heterogeneity_01+I(Heterogeneity_01^2)+
                        Response_metric + # to account for different responses across response metrics
                        Het.facet+ # since there is not enough information for some facets (e.g. feature richness), no interaction should be pursued here
                        Heterogeneity.cv + # this one needs to be on, so that we account for larger effects due to larger hetereogeneity gradients
                        Units.class+
                        #Spatial.scale.class + # not significant
                        #Year + # not significant
                        #Sample_size + # not significant
                        
                        (0+(Heterogeneity_01)|Study.included.ID/Effects_group)+
                        (1|Effects_group)
                      ,
                      (data_macroinv <- Heterogeneity.data.model %>% filter (Taxonomic.grouping == "macroinvertebrates" & (Response_metric=="Abundance" | Response_metric=="Biomass" | Response_metric=="Species diversity" |Response_metric=="Species richness")))
); plot(simulateResiduals(Macroinv_model)); summary(Macroinv_model); plot(allEffects(Macroinv_model))          

#microinvertebrates

Microinv_model<-lmer(log(Response_ratio) ~
                        Heterogeneity_01+I(Heterogeneity_01^2)+
                        Heterogeneity_01*Response_metric+
                        Het.facet+
                        Heterogeneity_01*Heterogeneity.cv+  
                        Heterogeneity_01*Units.class+
                        Spatial.scale.class+
                        Year + 
                        Sample_size + 
                        
                        #(0+(Heterogeneity_01)|Effects_group)
                        (0+(Heterogeneity_01)|Study.included.ID/Effects_group)+
                        (1|Effects_group)
                      ,
                      (data_microinv <-Heterogeneity.data.model %>% filter (Taxonomic.grouping == "microinvertebrates" & (Response_metric=="Abundance" | Response_metric=="Biomass" | Response_metric=="Species diversity" |Response_metric=="Species richness")))
); plot(simulateResiduals(Microinv_model)); summary(Microinv_model); plot(allEffects(Microinv_model))          

#large macroinvertebrates

Macrof_model<-lmer(log(Response_ratio) ~
                      Heterogeneity_01+I(Heterogeneity_01^2)+
                      Response_metric+
                      Het.facet+
                      Units.class+ 
                      #Spatial.scale.class + # not significant
                      Heterogeneity.cv+ 
                      Year + 
                      #Sample_size + # not significant
                      
                      (0+(Heterogeneity_01)|Study.included.ID/Effects_group)+
                      (1|Effects_group)
                    ,
                    (data_macrof<-Heterogeneity.data.model %>% mutate(Het.facet=ifelse(Het.facet=="Substrate 2D amount", "Other facets", Het.facet)) %>% # not enough replicates in substrate 2D amount
                       filter (Taxonomic.grouping == "macrofauna" & (Response_metric=="Abundance" | Response_metric=="Biomass" | Response_metric=="Species diversity" |Response_metric=="Species richness")))
); plot(simulateResiduals(Macrof_model)); summary(Macrof_model); plot(allEffects(Macrof_model))    

#fish

Fish_model<-lmer(log(Response_ratio) ~
                    Heterogeneity_01+I(Heterogeneity_01^2)+
                    Heterogeneity_01*Response_metric+
                    Het.facet+
                    Units.class + 
                    Spatial.scale.class+
                    Heterogeneity.cv+
                    #Year + # not significant
                    #Sample_size + # not significant
                    
                    
                    (0+(Heterogeneity_01)|Study.included.ID/Effects_group)+
                    (1|Effects_group)
                  ,
                  (data_fish<-Heterogeneity.data.model %>% filter (Taxonomic.grouping == "fish" & (Response_metric=="Abundance" | Response_metric=="Biomass" | Response_metric=="Species diversity" |Response_metric=="Species richness"))) #%>% mutate(Response_ratio_log=log(Response_ratio))
); plot(simulateResiduals(Fish_model)); summary(Fish_model); plot(allEffects(Fish_model)) 

#macroalgae

Macroalgae_model<-lmer(log(Response_ratio) ~
                          Heterogeneity_01+I(Heterogeneity_01^2)+
                          Response_metric+
                          Het.facet+
                          Units.class+ 
                          Heterogeneity.cv+
                          #Spatial.scale.class + # not significant
                          Year + 
                          Sample_size + 
                          
                          (0+(Heterogeneity_01)|Study.included.ID/Effects_group)+
                          (1|Effects_group)
                        
                        ,
                        (data_macroalgae<-Heterogeneity.data.model %>% filter ((Taxonomic.grouping == "macroalgae" & (Response_metric=="Abundance" | Response_metric=="Biomass" | Response_metric=="Species diversity" |Response_metric=="Species richness"))))
); plot(simulateResiduals(Macroalgae_model)); summary(Macroalgae_model); plot(allEffects(Macroalgae_model)) 

#microalgae

Microalgae_model<-lmer(log(Response_ratio) ~
                          Heterogeneity_01+I(Heterogeneity_01^2)+
                          Heterogeneity_01*Response_metric+
                          #Units.class + # rank deficient
                          Het.facet +
                          #Spatial.scale.class + # not significant
                          Heterogeneity.cv+
                          #Year + # not significant
                          #Sample_size + # not significant
                          
                          (0+(Heterogeneity_01)|Study.included.ID/Effects_group)+
                          (1|Effects_group)
                        ,
                        (data_microalgae<-Heterogeneity.data.model %>% filter ((Taxonomic.grouping == "microalgae" & (Response_metric=="Abundance" | Response_metric=="Biomass" | Response_metric=="Species diversity" |Response_metric=="Species richness"))) %>% mutate(Study = factor(Study.included.ID)))
); plot(simulateResiduals(Microalgae_model)); summary(Microalgae_model); AIC(Microalgae_model); plot(allEffects(Microalgae_model)) 

#
#### ORGANISMAL GROUPS - POSTHOC TESTS ####

lsmeans(Macroinv_model, pairwise ~ Het.facet, data=data_macroinv, adjust = "none", pbkrtest.limit = 8000)
lsmeans(Microinv_model, pairwise ~ Het.facet, data=data_microinv, adjust = "none", pbkrtest.limit = 8000)
lsmeans(Fish_model, pairwise ~ Het.facet, data=data_fish, adjust = "none", pbkrtest.limit = 8000)
lsmeans(Macrof_model, pairwise ~ Het.facet, data=data_macrof, adjust = "none", pbkrtest.limit = 8000)
lsmeans(Macroalgae_model, pairwise ~ Het.facet, data=data_macroalgae, adjust = "none", pbkrtest.limit = 8000)
lsmeans(Microalgae_model, pairwise ~ Het.facet, data=data_microalgae, adjust = "none", pbkrtest.limit = 8000)

lsmeans(Macroinv_model,pairwise ~ Response_metric, data=data_macroinv, pbkrtest.limit = 3379, adjust = "none")
lsmeans(Microinv_model,pairwise ~ Response_metric, data=data_microinv, pbkrtest.limit = 3379, adjust = "none")
lsmeans(Fish_model,pairwise ~ Response_metric, data=data_fish, pbkrtest.limit = 3379, adjust = "none")
lsmeans(Macrof_model,pairwise ~ Response_metric, data=data_macrof, pbkrtest.limit = 3379, adjust = "none")
lsmeans(Macroalgae_model,pairwise ~ Response_metric, data=data_macroalgae, pbkrtest.limit = 3379, adjust = "none")
lsmeans(Microalgae_model,pairwise ~ Response_metric, data=data_microalgae, pbkrtest.limit = 3379, adjust = "none")


#