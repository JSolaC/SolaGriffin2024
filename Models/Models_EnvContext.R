# LOAD LIBRARIES
Packages <- c("tidyverse","plyr","lme4","lmerTest","DHARMa","effects")
lapply(Packages, library, character.only = TRUE)


#### BIOGEOGRAPHY - MODELS ####

# Facet - Abundance

## 3D

model_3D_A_biogeo<-lmer(log(Response_ratio) ~
                   Heterogeneity_01 + I(Heterogeneity_01^2) +
                   Heterogeneity_01*Heterogeneity.cv +
                   Sub +
                   Tax.group+
                   Units.class +
                   Latitude_abs *
                   Season +
                   Depth +
                   #Spatial.scale.class+ # not significant
                   Year + 
                   #Sample_size + # not significant
                   
                   (0+(Heterogeneity_01)|Study.included.ID/Effects_group)+
                   (1|Effects_group)
                 ,
                 (data_3D_A_biogeo<-Heterogeneity.data.model %>% filter (Heterogeneity.facet =="Substrate 3D amount" & Response_metric=="Abundance"))
); summary(model_3D_A_biogeo); plot(simulateResiduals(model_3D_A_biogeo)); summary(model_3D_A_biogeo); plot(allEffects(model_3D_A_biogeo))

## 2D

model_2D_A_biogeo<-lmer(log(Response_ratio) ~
                   Heterogeneity_01 + I(Heterogeneity_01^2) +
                   Heterogeneity_01*Heterogeneity.cv +
                   Sub +
                   Tax.group +
                   Units.class +
                   Latitude_abs +
                   Season +
                   Depth +
                   #Spatial.scale.class + # not significant
                   #Year + # not significant
                   #Sample_size + # not significant
                   
                   (0+Heterogeneity_01|Study.included.ID/Effects_group)+
                   (1|Effects_group)
                 ,
                 (data_2D_A_biogeo<-Heterogeneity.data.model %>% filter (Heterogeneity.facet =="Substrate 2D amount" & Response_metric=="Abundance")) 
); plot(simulateResiduals(model_2D_A_biogeo)); summary(model_2D_A_biogeo); plot(allEffects(model_2D_A_biogeo))

## size

model_size_A_biogeo<-lmer(log(Response_ratio) ~
                     Heterogeneity_01 + I(Heterogeneity_01^2) +
                     Heterogeneity_01*Heterogeneity.cv +
                     Sub +
                     Tax.group +
                     Units.class +
                     Latitude_abs *
                     Season +
                     Depth +
                     #Spatial.scale.class+ # only one level
                     #Year + # not significant
                     #Sample_size + # not significant
                     
                     (0+Heterogeneity_01|Study.included.ID/Effects_group)+
                     (1|Effects_group)
                   ,
                   (data_size_A_biogeo<-Heterogeneity.data.model %>% filter (Heterogeneity.facet =="Feature size" & Response_metric=="Abundance"))
); plot(simulateResiduals(model_size_A_biogeo));summary(model_size_A_biogeo); plot(allEffects(model_size_A_biogeo))

## variation

model_var_A_biogeo<-lmer(log(Response_ratio) ~
                    Heterogeneity_01 + I(Heterogeneity_01^2) +
                    Sub +
                    Heterogeneity_01*Tax.group +
                    #Units.class + # rank deficient
                    Depth +
                    Latitude_abs *
                    Season +
                    Heterogeneity.cv +
                    #Spatial.scale.class+ # not significant
                    #Year + # not significant
                    #Sample_size + # not significant
                    
                    (0+Heterogeneity_01|Study.included.ID)+
                    (1|Effects_group)
                  ,
                  (data_var_A_biogeo<-Heterogeneity.data.model %>% filter ( Heterogeneity.facet =="Feature variation" & Response_metric=="Abundance"))
); plot(simulateResiduals(model_var_A_biogeo)); summary(model_var_A_biogeo);  plot(allEffects(model_var_A_biogeo))

## richness

model_rich_A_biogeo<-lmer(log(Response_ratio) ~
                     Heterogeneity_01 + I(Heterogeneity_01^2) +
                     #Sub + # rank deficient
                     #Tax.group + only one level
                     #Units.class + # only one level
                     Depth +
                     Latitude_abs +
                     Season +
                     Heterogeneity.cv +
                     #Spatial.scale.class+ # only one level
                     #Year + # not significant
                     #Sample_size + # not significant
                     
                     (0+(Heterogeneity_01)|Study.included.ID/Effects_group)+
                     (1|Effects_group)
                   ,
                   (data_rich_A_biogeo<-Heterogeneity.data.model %>% filter (Heterogeneity.facet =="Feature richness" & Response_metric=="Abundance"))
); plot(simulateResiduals(model_rich_A_biogeo)); summary(model_rich_A_biogeo); plot(allEffects(model_rich_A_biogeo))

# complexity

model_CX_A_biogeo<-lmer(log(Response_ratio) ~
                   Heterogeneity_01 + I(Heterogeneity_01^2) +
                   Sub +
                   Tax.group +
                   #Units.class + # only one level
                   Depth +
                   Latitude_abs +
                   Season +
                   Heterogeneity.cv +
                   #Spatial.scale.class+ # only one level
                   #Year + # not significant
                   #Sample_size + # not significant
                   
                   (0+Heterogeneity_01|Study.included.ID/Effects_group)+
                   (1|Effects_group)
                 ,
                 (data_CX_A_biogeo<-Heterogeneity.data.model %>% filter (Heterogeneity.facet =="Substrate complexity" & Response_metric=="Abundance"))
); plot(simulateResiduals(model_CX_A_biogeo)); summary(model_CX_A_biogeo); plot(allEffects(model_CX_A_biogeo))

# Facets - Richness

## 3D

model_3D_R_biogeo<-lmer(log(Response_ratio) ~
                   Heterogeneity_01 + I(Heterogeneity_01^2) +
                   Sub +
                   Tax.group +
                   Depth +
                   Latitude_abs +
                   Season +
                   Units.class+
                   Heterogeneity.cv +
                   #Spatial.scale.class+ # not significant
                   Year + 
                   #Sample_size + # not significant
                   
                   (0+(Heterogeneity_01)|Study.included.ID/Effects_group)+
                   (1|Effects_group)
                 ,
                 (data_3D_R_biogeo<-Heterogeneity.data.model %>% filter (Heterogeneity.facet =="Substrate 3D amount" & Response_metric=="Species richness"))
                 
); plot(simulateResiduals(model_3D_R_biogeo)); summary(model_3D_R_biogeo); plot(allEffects(model_3D_R_biogeo))

## 2D

model_2D_R_biogeo<-lmer(log(Response_ratio) ~
                   Heterogeneity_01 + I(Heterogeneity_01^2) +
                   Sub +
                   Tax.group +
                   Units.class +
                   Depth +
                   Latitude_abs *
                   Season +
                   Heterogeneity.cv +
                   #Spatial.scale.class+ # not significant
                   #Year + # not significant
                   Sample_size +
                   
                   (0+Heterogeneity_01|Study.included.ID/Effects_group)+
                   (1|Effects_group)
                 ,
                 (data_2D_R_biogeo<-Heterogeneity.data.model %>% filter (Heterogeneity.facet =="Substrate 2D amount" & Response_metric=="Species richness"))
); plot(simulateResiduals(model_2D_R_biogeo)); summary(model_2D_R_biogeo); plot(allEffects(model_2D_R_biogeo))

# size

model_size_R_biogeo<-lmer(log(Response_ratio) ~
                     Heterogeneity_01 + I(Heterogeneity_01^2) +
                     Sub +
                     Tax.group +
                     Units.class +
                     Depth +
                     Latitude_abs +
                     Season +
                     Heterogeneity.cv +
                     #Spatial.scale.class+ # not significant
                     #Year + # not significant
                     #Sample_size + # not significant
                     
                     (0+Heterogeneity_01|Study.included.ID/Effects_group)+
                     (1|Effects_group)
                   ,
                   (data_size_R_biogeo<-Heterogeneity.data.model %>% filter (Heterogeneity.facet =="Feature size" & Response_metric=="Species richness"))
); plot(simulateResiduals(model_size_R_biogeo));summary(model_size_R_biogeo); plot(allEffects(model_size_R_biogeo))

## variation

model_var_R_biogeo<-lmer(log(Response_ratio) ~
                    Heterogeneity_01 + I(Heterogeneity_01^2) +
                    Sub +
                    Tax.group +
                    Units.class +
                    Depth +
                    Latitude_abs +
                    Season +
                    Heterogeneity.cv +
                    Spatial.scale.class+
                    #Year + # not significant
                    #Sample_size + # not significant
                    
                    (0+Heterogeneity_01|Study.included.ID/Effects_group)+
                    (1|Effects_group)
                  ,
                  (data_variation_R_biogeo<-Heterogeneity.data.model %>% filter (Heterogeneity.facet =="Feature variation" & Response_metric=="Species richness"))
); plot(simulateResiduals(model_var_R_biogeo)); summary(model_var_R_biogeo); plot(allEffects(model_var_R_biogeo))

# richness

model_rich_R_biogeo<-lmer(log(Response_ratio) ~
                     Heterogeneity_01 + I(Heterogeneity_01^2) +
                     #Sub + # only one level available
                     #Tax.group + #only one level
                     #Units.class + # only one level
                     Depth +
                     Latitude_abs +
                     Season +
                     Heterogeneity_01*Heterogeneity.cv +
                     #Spatial.scale.class+ # not significant
                     #Year + # not significant
                     #Sample_size + # not significant
                     
                     (0+Heterogeneity_01|Study.included.ID/Effects_group)+
                     (1|Effects_group)
                   ,
                   (data_rich_R_biogeo<-Heterogeneity.data.model %>% filter (Heterogeneity.facet =="Feature richness" & Response_metric=="Species richness"))
); plot(simulateResiduals(model_rich_R_biogeo)); summary(model_rich_R_biogeo); plot(allEffects(model_rich_R_biogeo))

# complexity

model_CX_R_biogeo<-lmer(log(Response_ratio) ~
                   Heterogeneity_01 + I(Heterogeneity_01^2) +
                   Heterogeneity_01*Sub +
                   Tax.group +
                   Units.class +
                   Depth +
                   Latitude_abs +
                   Season +
                   Heterogeneity.cv +
                   #Spatial.scale.class+ # not significant
                   #Year + # not significant
                   #Sample_size + # not significant
                   
                   (0+Heterogeneity_01|Study.included.ID/Effects_group)+
                   (1|Effects_group)
                 ,
                 (data_CX_R_biogeo<-Heterogeneity.data.model %>% filter (Heterogeneity.facet =="Substrate complexity" & Response_metric=="Species richness"))
); plot(simulateResiduals(model_CX_R_biogeo)); summary(model_CX_R_biogeo); plot(allEffects(model_CX_R_biogeo))

#Organismal groups

# macroinvertebrates

Biogeo_macroinv<-lmer(log(Response_ratio) ~
                        Heterogeneity_01 + I(Heterogeneity_01^2) +
                        Heterogeneity_01*Sub +
                        Response_metric+
                        Het.facet+
                        Depth +
                        Latitude_abs * 
                        Season +
                        Heterogeneity.cv +
                        Units.class +
                        #Spatial.scale.class+ # not significant
                        #Year + # not significant
                        #Sample_size + # not significant
                        
                        (0+(Heterogeneity_01)|Study.included.ID/Effects_group)+
                        (1|Effects_group)
                      ,
                      (data_biogeo_macroinv <- Heterogeneity.data.model %>% filter (Taxonomic.grouping == "macroinvertebrates" & (Response_metric=="Abundance" | Response_metric=="Biomass" | Response_metric=="Species diversity" |Response_metric=="Species richness")))
); plot(simulateResiduals(Biogeo_macroinv)); summary(Biogeo_macroinv);  plot(allEffects(Biogeo_macroinv))          

anova(Biogeo_macroinv) # this anova test allows to obtain a statitstic test output for the interactions Latitude_abs*Season

#microinvertebrates

Biogeo_microinv<-lmer(log(Response_ratio) ~
                        Heterogeneity_01 + I(Heterogeneity_01^2) + 
                        Sub +
                        Heterogeneity_01*Response_metric+
                        Het.facet+
                        Units.class +
                        Depth +
                        Latitude_abs * 
                        Season +
                        Heterogeneity_01*Heterogeneity.cv +
                        #Spatial.scale.class+ # not significant
                        Year + 
                        Sample_size + 
                        
                        (0+(Heterogeneity_01)|Study.included.ID/Effects_group)+
                        (1|Effects_group)
                      ,
                      (data_biogeo_microinv <-Heterogeneity.data.model %>% filter (Taxonomic.grouping == "microinvertebrates" & (Response_metric=="Abundance" | Response_metric=="Biomass" | Response_metric=="Species diversity" |Response_metric=="Species richness")))
); plot(simulateResiduals(Biogeo_microinv)); summary(Biogeo_microinv); plot(allEffects(Biogeo_microinv))          

anova(Biogeo_microinv)

#macrofauna

Biogeo_macrof<-lmer(log(Response_ratio) ~
                      Heterogeneity_01 + I(Heterogeneity_01^2) +
                      Sub +
                      Response_metric +
                      Het.facet +
                      Units.class + 
                      Depth +
                      Latitude_abs +
                      Season +
                      Heterogeneity.cv +
                      #Spatial.scale.class+ # not significant
                      #Year + # not significant
                      #Sample_size + # not significant
                      
                      (0+(Heterogeneity_01)|Study.included.ID/Effects_group)+
                      (1|Effects_group)
                    ,
                    (data_biogeo_macrof<-Heterogeneity.data.model %>% filter (Taxonomic.grouping == "macrofauna" & (Response_metric=="Abundance" | Response_metric=="Biomass" | Response_metric=="Species diversity" |Response_metric=="Species richness")))
); plot(simulateResiduals(Biogeo_macrof)); summary(Biogeo_macrof); plot(allEffects(Biogeo_macrof))          

#fish

Biogeo_fish<-lmer(log(Response_ratio) ~
                    Heterogeneity_01 + I(Heterogeneity_01^2) +
                    Sub +
                    Heterogeneity_01*Response_metric+
                    Het.facet+
                    #Units.class + # rank deficient
                    Depth +
                    Latitude_abs +
                    Season +
                    Heterogeneity.cv +
                    #Spatial.scale.class + # not significant
                    #Year + # not significant
                    #Sample_size + # not significant
                    
                    (0+(Heterogeneity_01)|Study.included.ID/Effects_group)+
                    (1|Effects_group)
                  ,
                  (data_biogeo_fish<-Heterogeneity.data.model %>% filter (Taxonomic.grouping == "fish" & (Response_metric=="Abundance" | Response_metric=="Biomass" | Response_metric=="Species diversity" |Response_metric=="Species richness"))) #%>% mutate(Response_ratio_log=log(Response_ratio))
); plot(simulateResiduals(Biogeo_fish)); summary(Biogeo_fish); plot(allEffects(Biogeo_fish)) 

#macroalgae

Biogeo_macroalgae<-lmer(log(Response_ratio) ~
                          Heterogeneity_01 + I(Heterogeneity_01^2) +
                          Sub +
                          Heterogeneity_01*Response_metric+
                          Het.facet+
                          #Units.class + # rank deficient
                          Depth +
                          Latitude_abs +
                          Season +
                          Heterogeneity.cv +
                          #Spatial.scale.class + # not significant
                          Year + 
                          #Sample_size + # not significant
                          
                          (0+(Heterogeneity_01)|Study.included.ID/Effects_group)+
                          (1|Effects_group)
                        
                        ,
                        (data_biogeo_macroalgae<-Heterogeneity.data.model %>% filter ((Taxonomic.grouping == "macroalgae" & (Response_metric=="Abundance" | Response_metric=="Biomass" | Response_metric=="Species diversity" |Response_metric=="Species richness"))))
); plot(simulateResiduals(Biogeo_macroalgae)); summary(Biogeo_macroalgae); plot(allEffects(Biogeo_macroalgae)) 

#microalgae

Biogeo_microalgae<-lmer(log(Response_ratio) ~
                          Heterogeneity_01 + I(Heterogeneity_01^2) +
                          Heterogeneity_01*Sub +
                          Heterogeneity_01*Response_metric+
                          Het.facet+
                          #Units.class + # model is rank deficient
                          #Depth + # convergence issues
                          Latitude_abs +
                          Season +
                          Heterogeneity.cv +
                          #Spatial.scale.class + # not significant
                          #Year + # not significant
                          #Sample_size + # not significant
                          
                          (0+(Heterogeneity_01)|Study.included.ID/Effects_group)+
                          (1|Effects_group)
                        ,
                        (data_biogeo_microalgae<-Heterogeneity.data.model %>% filter ((Taxonomic.grouping == "microalgae" & (Response_metric=="Abundance" | Response_metric=="Biomass" | Response_metric=="Species diversity" |Response_metric=="Species richness"))) %>% mutate(Study = factor(Study.included.ID)))
); plot(simulateResiduals(Biogeo_microalgae)); summary(Biogeo_microalgae); plot(allEffects(Biogeo_microalgae)) 

#
#### BIOGEOGAPHY - POSTHOC TESTS ####

#lsmeans(Biogeo_microalgae, pairwise ~ Depth, data=data_biogeo_microalgae, adjust = "none", pbkrtest.limit = 8000) # model did not converge
lsmeans(Biogeo_macroalgae, pairwise ~ Depth, data=data_biogeo_macroalgae, adjust = "none", pbkrtest.limit = 8000)
lsmeans(Biogeo_fish, pairwise ~ Depth, data=data_biogeo_fish, adjust = "none", pbkrtest.limit = 8000)
lsmeans(Biogeo_macrof, pairwise ~ Depth, data=data_biogeo_macrof, adjust = "none", pbkrtest.limit = 8000) 
lsmeans(Biogeo_microinv, pairwise ~ Depth, data=data_biogeo_microinv, adjust = "none", pbkrtest.limit = 8000)
lsmeans(Biogeo_macroinv, pairwise ~ Depth, data=data_biogeo_macroinv, adjust = "none", pbkrtest.limit = 8000)


#
#### FACET GROUPS - POSTHOC TESTS ####

lsmeans(model_CX_R_biogeo, pairwise ~ Depth, data=data_CX_R_biogeo, adjust = "none", pbkrtest.limit = 3379) 
lsmeans(model_rich_R_biogeo, pairwise ~ Depth, data=data_rich_R_biogeo, adjust = "none", pbkrtest.limit = 3379) 
lsmeans(model_var_R_biogeo, pairwise ~ Depth, data=data_variation_R_biogeo, adjust = "none", pbkrtest.limit = 3379) 
lsmeans(model_size_R_biogeo, pairwise ~ Depth, data=data_size_R_biogeo, adjust = "none", pbkrtest.limit = 3379)
lsmeans(model_2D_R_biogeo, pairwise ~ Depth, data=data_2D_R_biogeo, adjust = "none", pbkrtest.limit = 3379) 
lsmeans(model_3D_R_biogeo, pairwise ~ Depth,  data=data_3D_R_biogeo, adjust = "none", pbkrtest.limit = 3379)

lsmeans(model_CX_A_biogeo, pairwise ~ Depth, data=data_CX_A_biogeo, adjust = "none", pbkrtest.limit = 3379)
lsmeans(model_rich_A_biogeo, pairwise ~ Depth, data=data_rich_A_biogeo, adjust = "none", pbkrtest.limit = 3379) 
lsmeans(model_var_A_biogeo, pairwise ~ Depth, data=data_var_A_biogeo, adjust = "none", pbkrtest.limit = 3379) 
lsmeans(model_size_A_biogeo, pairwise ~ Depth, data=data_size_A_biogeo, adjust = "none", pbkrtest.limit = 3379)
lsmeans(model_2D_A_biogeo, pairwise ~ Depth, data=data_2D_A_biogeo, adjust = "none", pbkrtest.limit = 3379) 
lsmeans(model_3D_A_biogeo, pairwise ~ Depth, data=data_3D_A_biogeo, adjust = "none", pbkrtest.limit = 3379)

#