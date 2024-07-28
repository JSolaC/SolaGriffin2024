# LOAD LIBRARIES
Packages <- c("tidyverse","plyr","lme4","lmerTest","DHARMa","ggeffects","effects", "emmeans")
lapply(Packages, library, character.only = TRUE)

#### FACET RICHNESS GROUPS - MODELS #####

## Substrate 3D amount

model_3D_R<-lmer(log(Response_ratio) ~
                   
                   #Fixed effects
                   Heterogeneity_01+I(Heterogeneity_01^2)+
                   Heterogeneity_01*Heterogeneity.cv+
                   Units.class+
                   Tax.group+
                   #Spatial.scale.class + # not significant
                   Year + 
                   #Sample_size + # not significant
                   
                   #Random effects
                   (0+(Heterogeneity_01)|Study.included.ID/Effects_group) +
                   (1|Effects_group)
                 
                 ,
                 #dataset
                 (data_3D_R<-Heterogeneity.data.model %>% filter (Response_metric=="Species richness" & Heterogeneity.facet =="Substrate 3D amount"))
                 
); plot(simulateResiduals(model_3D_R)); summary(model_3D_R); plot(allEffects(model_3D_R)) # here I a) visually checked residuals, b) check model output and c) have a visual check on how the output fits the data

## Substrate 2D amount

model_2D_R<-lmer(log(Response_ratio) ~
                   
                   Heterogeneity_01+I(Heterogeneity_01^2)+
                   Heterogeneity.cv+
                   Units.class+
                   #Tax.group +
                   #Spatial.scale.class + # not significant
                   #Year + # not significant
                   Sample_size + 
                   
                   (0+Heterogeneity_01|Study.included.ID/Effects_group)+
                   (1|Effects_group)
                 ,
                 (data_2D_R<-Heterogeneity.data.model %>% filter (Response_metric=="Species richness" & Heterogeneity.facet =="Substrate 2D amount"))
                 
); plot(simulateResiduals(model_2D_R)); summary(model_2D_R); plot(allEffects(model_2D_R))

# Feature size

model_size_R<-lmer(log(Response_ratio) ~
                     
                     Heterogeneity_01+I(Heterogeneity_01^2)+
                     Heterogeneity.cv+
                     Units.class+
                     Heterogeneity_01*Tax.group+
                     #Spatial.scale.class + # not significant
                     #Year + # not significant
                     #Sample_size + # not significant
                     
                     (0+Heterogeneity_01|Study.included.ID/Effects_group)+
                     (1|Effects_group)
                   ,
                   (data_size_R<-Heterogeneity.data.model %>% filter (Response_metric=="Species richness" & Heterogeneity.facet =="Feature size"))
); plot(simulateResiduals(model_size_R));summary(model_size_R); plot(allEffects(model_size_R))

## Feature variation

model_var_R<-lmer(log(Response_ratio) ~
                    
                    Heterogeneity_01+I(Heterogeneity_01^2)+
                    Heterogeneity.cv+
                    Units.class+
                    Heterogeneity_01*Tax.group+
                    #Spatial.scale.class + # not significant
                    #Year + # not significant
                    #Sample_size + # not significant
                    
                    (0+Heterogeneity_01|Study.included.ID/Effects_group)+
                    (1|Effects_group)
                  ,
                  (data_var_R<-Heterogeneity.data.model %>% filter (Response_metric=="Species richness" & Heterogeneity.facet =="Feature variation"))
); plot(simulateResiduals(model_var_R)); summary(model_var_R); plot(allEffects(model_var_R))

# Feature richness

model_rich_R<-lmer(log(Response_ratio) ~
                     
                     Heterogeneity_01+I(Heterogeneity_01^2)+
                     #Tax.group+ # only one level available
                     Heterogeneity_01*Heterogeneity.cv+
                     #Units.class+ # only one level available
                     #Spatial.scale.class + # not enough replication (it is dropped by model even after excluding all other fixed effects)
                     #Year + # not significant
                     #Sample_size + # not significant
                     
                     (0+(Heterogeneity_01)|Study.included.ID/Effects_group)+
                     (1|Study.included.ID)
                   ,
                   (data_rich_R<-Heterogeneity.data.model %>% filter (Response_metric=="Species richness" & Heterogeneity.facet =="Feature richness"))
); plot(simulateResiduals(model_rich_R)); summary(model_rich_R); plot(allEffects(model_rich_R))

# Substrate complexity

model_CX_R<-lmer(log(Response_ratio) ~
                   
                   Heterogeneity_01+I(Heterogeneity_01^2)+
                   Heterogeneity.cv+
                   Heterogeneity_01*Tax.group+
                   Units.class+
                   #Spatial.scale.class + # not significant
                   #Year + # not significant
                   #Sample_size + # not significant
                   
                   (0+Heterogeneity_01|Study.included.ID/Effects_group)+
                   (1|Effects_group)
                 ,
                 (data_CX_R<-Heterogeneity.data.model %>% filter (Response_metric=="Species richness" & Heterogeneity.facet =="Substrate complexity"))
); plot(simulateResiduals(model_CX_R)); summary(model_CX_R); plot(allEffects(model_CX_R))

#

#### FACET ABUNDANCE GROUPS - MODELS #####

## Substrate 3D amount

model_3D_A<-lmer(log(Response_ratio) ~
                   
                   Heterogeneity_01+I(Heterogeneity_01^2)+
                   Heterogeneity_01*Heterogeneity.cv+
                   Tax.group+
                   Heterogeneity_01*Taxonomic.level +
                   Units.class+
                   Year + 
                   #Spatial.scale.class + # not significant
                   Sample_size +
                   
                   (0+(Heterogeneity_01)|Study.included.ID/Effects_group)+
                   (1|Effects_group)
                 ,
                 (data_3D_A<-Heterogeneity.data.model %>% filter (Response_metric=="Abundance" & Heterogeneity.facet =="Substrate 3D amount" & Units.class!="type"))
); plot(simulateResiduals(model_3D_A)); summary(model_3D_A); plot(allEffects(model_3D_A))

## Substrate 2D amount

model_2D_A<-lmer(log(Response_ratio) ~
                   
                   Heterogeneity_01+I(Heterogeneity_01^2)+
                   Tax.group+
                   Heterogeneity_01*Heterogeneity.cv+
                   Units.class +
                   #Taxonomic.level + # not significant
                   #Spatial.scale.class + # not significant
                   #Year + # not significant
                   #Sample_size + # not significant
                   
                   (0+Heterogeneity_01|Study.included.ID/Effects_group)+
                   (1|Effects_group)
                 ,
                 (data_2D_A<-Heterogeneity.data.model %>% filter (Response_metric=="Abundance" & Heterogeneity.facet =="Substrate 2D amount")) 
); plot(simulateResiduals(model_2D_A)); summary(model_2D_A); plot(allEffects(model_2D_A))

## Feature size

model_size_A<-lmer(log(Response_ratio) ~
                     
                     Heterogeneity_01+I(Heterogeneity_01^2)+
                     Heterogeneity_01*Tax.group +
                     Heterogeneity_01*Heterogeneity.cv +
                     #Taxonomic.level + # not significant
                     Units.class +
                     #Spatial.scale.class + # not significant
                     Year + 
                     #Sample_size + # not significant
                     
                     (0+Heterogeneity_01|Study.included.ID/Effects_group)+
                     (1|Effects_group)
                   ,
                   (data_size_A<-Heterogeneity.data.model %>% filter (Response_metric=="Abundance" & Heterogeneity.facet =="Feature size"))
); plot(simulateResiduals(model_size_A));summary(model_size_A); plot(allEffects(model_size_A))

## Feature variation

model_var_A<-lmer(log(Response_ratio) ~
                    
                    Heterogeneity_01 + I(Heterogeneity_01^2) +
                    Tax.group +
                    Heterogeneity.cv +
                    Units.class +
                    #Taxonomic.level + # not significant
                    Spatial.scale.class +
                    #Year + # not significant
                    #Sample_size + # not significant
                    
                    (0+Heterogeneity_01|Study.included.ID/Effects_group) +
                    (1|Effects_group)
                  ,
                  (data_var_A<-Heterogeneity.data.model %>% filter (Response_metric=="Abundance" & Heterogeneity.facet =="Feature variation"))
); plot(simulateResiduals(model_var_A)); summary(model_var_A); plot(allEffects(model_var_A))

## Feature richness

model_rich_A<-lmer(log(Response_ratio) ~
                     
                     Heterogeneity_01+I(Heterogeneity_01^2)+
                     Tax.group+
                     Heterogeneity.cv+
                     #Taxonomic.level + # only one level available
                     #Units.class+ # only one level available
                     #Spatial.scale.class + # only one level
                     #Year + Sample_size + # not significant
                     
                     (0+(Heterogeneity_01)|Study.included.ID/Effects_group)+
                     (1|Effects_group)
                   ,
                   (data_rich_A<-Heterogeneity.data.model %>% filter (Response_metric=="Abundance" & Heterogeneity.facet =="Feature richness"))
); plot(simulateResiduals(model_rich_A)); summary(model_rich_A); AIC(model_rich_A); plot(allEffects(model_rich_A))


# Substrate complexity

model_CX_A<-lmer(log(Response_ratio) ~
                   
                   Heterogeneity_01+I(Heterogeneity_01^2)+
                   Tax.group+
                   Heterogeneity_01*Heterogeneity.cv+
                   Taxonomic.level +
                   Units.class+
                   #Spatial.scale.class + # only one level
                   #Year + # not significant
                   #Sample_size + # not significant
                   
                   (0+Heterogeneity_01|Study.included.ID/Effects_group)+
                   (1|Effects_group)
                 ,
                 (data_CX_A<-Heterogeneity.data.model %>% filter (Response_metric=="Abundance" & Heterogeneity.facet =="Substrate complexity"))
); plot(simulateResiduals(model_CX_A)); summary(model_CX_A); AIC(model_CX_A); plot(allEffects(model_CX_A))


#