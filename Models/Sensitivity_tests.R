# LOAD LIBRARIES
library(nlme)

# LOAD FUNCTIONS
Remove_largestudies <- function(dataset){
  
  large_studies <- dataset %>%
    group_by(Title) %>%
    dplyr::summarise(Count=n())
  
  Quantile_95 <- quantile(large_studies$Count, probs=c(0.95))
  
  large_studies_remove <- large_studies %>%
    filter(Count<Quantile_95)
  
  dataset_out <- dataset %>%
    filter(Title %in% large_studies_remove$Title)
  
  return(dataset_out)
  
} # function to remove the 5% largest studies per dataset
Remove_smallHDRS <- function(dataset){
  
  small_HDRS <- dataset %>%
    group_by(Effects_group) %>%
    dplyr::summarise(Count=n()) %>%
    filter(Count>3)
  
  dataset_out <- dataset %>%
    filter(Effects_group %in% small_HDRS$Effects_group)
  
  return(dataset_out)
  
} # function to remove HDRs with <4 data points
Remove_smallHDRS_2 <- function(dataset){
  
  small_HDRS <- dataset %>%
    group_by(Effects_group) %>%
    dplyr::summarise(Count=n()) %>%
    filter(Count>9)
  
  dataset_out <- dataset %>%
    filter(Effects_group %in% small_HDRS$Effects_group)
  
  return(dataset_out)
  
} # function to remove HDRs with <10 data points
Check_data <- function(dataset, dataset_bis){
  
  dataset1 <- dataset %>% 
    mutate(ID="A") %>%
    group_by(ID) %>%
    dplyr::summarise(
      n_HDRs=length(unique(Effects_group)),
      n_data = n()
    )
  
  dataset2 <- dataset_bis %>%
    mutate(ID="A") %>%
    group_by(ID) %>%
    dplyr::summarise(
      n_HDRs=length(unique(Effects_group)),
      n_data = n()
    )
  
  out <- data.frame(
    "HDR"=dataset2$n_HDRs/dataset1$n_HDRs,
    "datapoints"=dataset2$n_data/dataset1$n_data
  )
  
  return(out)
  
}

#
##### TESTING THE EFFECTS OF LARGE STUDIES IN THE MODELS ####

# FACET - RICHNESS

model_3D_R_large <- update(model_3D_R, data=data_3D_R %>% Remove_largestudies())
#plot(simulateResiduals(model_3D_R_large)); summary(model_3D_R_large); #plot(allEffects(model_3D_R_large))
#r.squaredGLMM(model_3D_R_large)
#Check_data(data_3D_R, data_3D_R %>% Remove_largestudies())

model_2D_R_large <- update(model_2D_R, data=data_2D_R %>% Remove_largestudies())
#plot(simulateResiduals(model_2D_R_large)); summary(model_2D_R_large); #plot(allEffects(model_2D_R_large))
#r.squaredGLMM(model_2D_R_large)
#Check_data(data_2D_R, data_2D_R %>% Remove_largestudies())

# FACET - ABUNDANCE

model_3D_A_large <- update(model_3D_A, data=data_3D_A %>% Remove_largestudies())
#plot(simulateResiduals(model_3D_A_large)); summary(model_3D_A_large); #plot(allEffects(model_3D_A_large))
#r.squaredGLMM(model_3D_A_large)
#Check_data(data_3D_A, data_3D_A %>% Remove_largestudies())

model_2D_A_large <- update(model_2D_A, data=data_2D_A %>% Remove_largestudies())
#plot(simulateResiduals(model_2D_A_large)); summary(model_2D_A_large); #plot(allEffects(model_2D_A_large))
#r.squaredGLMM(model_2D_A_large)
#Check_data(data_2D_A, data_2D_A %>% Remove_largestudies())

# ORGANISMAL GROUP

Macroinv_model_large <- update(Macroinv_model, data=data_macroinv %>% Remove_largestudies())
#plot(simulateResiduals(Macroinv_model_large)); summary(Macroinv_model_large); #plot(allEffects(Macroinv_model_large))
#r.squaredGLMM(Macroinv_model_large)
#Check_data(data_macroinv, data_macroinv %>% Remove_largestudies())

Microinv_model_large <- update(Microinv_model, data=data_microinv %>% Remove_largestudies())
#plot(simulateResiduals(Microinv_model_large)); summary(Microinv_model_large); #plot(allEffects(Microinv_model_large))
#r.squaredGLMM(Microinv_model_large)
#Check_data(data_microinv, data_microinv %>% Remove_largestudies())

### BIOGEOGRAPHY

# FACET - RICHNESS

model_3D_R_large_biogeo <- update(model_3D_R_biogeo, data=data_3D_R %>% Remove_largestudies())
#plot(simulateResiduals(model_3D_R_large)); summary(model_3D_R_large); #plot(allEffects(model_3D_R_large))
#r.squaredGLMM(model_3D_R_large)

model_2D_R_large_biogeo <- update(model_2D_R_biogeo, data=data_2D_R %>% Remove_largestudies())
#plot(simulateResiduals(model_2D_R_large)); summary(model_2D_R_large); #plot(allEffects(model_2D_R_large))
#r.squaredGLMM(model_2D_R_large)


# FACET - ABUNDANCE

model_3D_A_large_biogeo <- update(model_3D_A_biogeo, data=data_3D_A %>% Remove_largestudies())
#plot(simulateResiduals(model_3D_A_large)); summary(model_3D_A_large); #plot(allEffects(model_3D_A_large))
#r.squaredGLMM(model_3D_A_large)

model_2D_A_large_biogeo <- update(model_2D_A_biogeo, data=data_2D_A %>% Remove_largestudies())
#plot(simulateResiduals(model_2D_A_large)); summary(model_2D_A_large); #plot(allEffects(model_2D_A_large))
#r.squaredGLMM(model_2D_A_large)

# ORGANISMAL GROUP

Macroinv_model_large_biogeo <- update(Biogeo_macroinv, data=data_macroinv %>% Remove_largestudies())
#plot(simulateResiduals(Macroinv_model_large)); summary(Macroinv_model_large); #plot(allEffects(Macroinv_model_large))
#r.squaredGLMM(Macroinv_model_large)

Microinv_model_large_biogeo <- update(Biogeo_microinv, data=data_microinv %>% Remove_largestudies())
#plot(simulateResiduals(Microinv_model_large)); summary(Microinv_model_large); #plot(allEffects(Microinv_model_large))
#r.squaredGLMM(Microinv_model_large)

#
#### TESTING THE EFFECTS OF EXCLUDING DISCRETE VARIABLES ####

model_3D_R_c <- update(model_3D_R, data=data_3D_R %>% filter(Discrete.or.continuous!="d" & Units.class!="type" & Units.class!="presence/absence"))
#plot(simulateResiduals(model_3D_R_c)); summary(model_3D_R_c); #plot(allEffects(model_3D_R_c))
#r.squaredGLMM(model_3D_R_c)
#Check_data(data_3D_R, data_3D_R %>% filter(Discrete.or.continuous!="d" & Units.class!="type" & Units.class!="presence/absence"))

model_2D_R_c <- update(model_2D_R, data=data_2D_R %>% filter(Discrete.or.continuous!="d" & Units.class!="type" & Units.class!="presence/absence"))
#plot(simulateResiduals(model_2D_R_c)); summary(model_2D_R_c); #plot(allEffects(model_2D_R_c))
#r.squaredGLMM(model_2D_R_c)
#Check_data(data_2D_R, data_2D_R %>% filter(Discrete.or.continuous!="d" & Units.class!="type" & Units.class!="presence/absence"))

model_3D_A_c <- update(model_3D_A, data=data_3D_A %>% filter(Discrete.or.continuous!="d" & Units.class!="type" & Units.class!="presence/absence"))
#plot(simulateResiduals(model_3D_A_c)); summary(model_3D_A_c); #plot(allEffects(model_3D_A_c))
#r.squaredGLMM(model_3D_A_c)
#Check_data(data_3D_A, data_3D_A %>% filter(Discrete.or.continuous!="d" & Units.class!="type" & Units.class!="presence/absence"))

model_2D_A_c <- update(model_2D_A, data=data_2D_A %>% filter(Discrete.or.continuous!="d" & Units.class!="type" & Units.class!="presence/absence"))
#plot(simulateResiduals(model_2D_A_c)); summary(model_2D_A_c); #plot(allEffects(model_2D_A_c))
#r.squaredGLMM(model_2D_A_c)
#Check_data(data_2D_A, data_2D_A %>% filter(Discrete.or.continuous!="d" & Units.class!="type" & Units.class!="presence/absence"))

Macroinv_model_c <- update(Macroinv_model, data=data_macroinv %>% filter(Discrete.or.continuous!="d" & Units.class!="type" & Units.class!="presence/absence"))
#plot(simulateResiduals(Macroinv_model_c)); summary(Macroinv_model_c); #plot(allEffects(Macroinv_model_c))
#r.squaredGLMM(Macroinv_model_c)
#Check_data(data_macroinv, data_macroinv %>% filter(Discrete.or.continuous!="d" & Units.class!="type" & Units.class!="presence/absence"))

Microinv_model_c <- update(Microinv_model, data=data_microinv %>% filter(Discrete.or.continuous!="d" & Units.class!="type" & Units.class!="presence/absence"))
#plot(simulateResiduals(Microinv_model_c)); summary(Microinv_model_c); #plot(allEffects(Microinv_model_c))
#r.squaredGLMM(Microinv_model_c)
#Check_data(data_microinv, data_microinv %>% filter(Discrete.or.continuous!="d" & Units.class!="type" & Units.class!="presence/absence"))

### BIOGEOGRAPHY

model_3D_R_c_biogeo <- update(model_3D_R_biogeo, data=data_3D_R %>% filter(Discrete.or.continuous!="d" & Units.class!="type" & Units.class!="presence/absence"))
#plot(simulateResiduals(model_3D_R_c)); summary(model_3D_R_c); #plot(allEffects(model_3D_R_c))
#r.squaredGLMM(model_3D_R_c)

model_2D_R_c_biogeo <- update(model_2D_R_biogeo, data=data_2D_R %>% filter(Discrete.or.continuous!="d" & Units.class!="type" & Units.class!="presence/absence"))
#plot(simulateResiduals(model_2D_R_c)); summary(model_2D_R_c); #plot(allEffects(model_2D_R_c))
#r.squaredGLMM(model_2D_R_c)

model_3D_A_c_biogeo <- update(model_3D_A_biogeo, data=data_3D_A %>% filter(Discrete.or.continuous!="d" & Units.class!="type" & Units.class!="presence/absence"))
#plot(simulateResiduals(model_3D_A_c)); summary(model_3D_A_c); #plot(allEffects(model_3D_A_c))
#r.squaredGLMM(model_3D_A_c)

model_2D_A_c_biogeo <- update(model_2D_A_biogeo, data=data_2D_A %>% filter(Discrete.or.continuous!="d" & Units.class!="type" & Units.class!="presence/absence"))
#plot(simulateResiduals(model_2D_A_c)); summary(model_2D_A_c); #plot(allEffects(model_2D_A_c))
#r.squaredGLMM(model_2D_A_c)

Macroinv_model_c_biogeo <- update(Biogeo_macroinv, data=data_macroinv %>% filter(Discrete.or.continuous!="d" & Units.class!="type" & Units.class!="presence/absence"))
#plot(simulateResiduals(Macroinv_model_c)); summary(Macroinv_model_c); #plot(allEffects(Macroinv_model_c))
#r.squaredGLMM(Macroinv_model_c)

Microinv_model_c_biogeo <- update(Biogeo_microinv, data=data_microinv %>% filter(Discrete.or.continuous!="d" & Units.class!="type" & Units.class!="presence/absence"))
#plot(simulateResiduals(Microinv_model_c)); summary(Microinv_model_c); #plot(allEffects(Microinv_model_c))
#r.squaredGLMM(Microinv_model_c)

#
##### TESTING THE EFFECTS OF HDRS WITH < 4 DATA POINTS IN THE MODELS ####

# FACET - RICHNESS

model_3D_R_HDRS <- update(model_3D_R, data=data_3D_R %>% Remove_smallHDRS())
#plot(simulateResiduals(model_3D_R_HDRS)); summary(model_3D_R_HDRS); #plot(allEffects(model_3D_R_HDRS))
#r.squaredGLMM(model_3D_R_HDRS)
#Check_data(data_3D_R, data_3D_R %>% Remove_smallHDRS())

model_2D_R_HDRS <- update(model_2D_R, data=data_2D_R %>% Remove_smallHDRS())
#plot(simulateResiduals(model_2D_R_HDRS)); summary(model_2D_R_HDRS); #plot(allEffects(model_2D_R_HDRS))
#r.squaredGLMM(model_2D_R_HDRS)
#Check_data(data_2D_R, data_2D_R %>% Remove_smallHDRS())

# FACET - ABUNDANCE

model_3D_A_HDRS <- update(model_3D_A, data=data_3D_A %>% Remove_smallHDRS())
#plot(simulateResiduals(model_3D_A_HDRS)); summary(model_3D_A_HDRS); #plot(allEffects(model_3D_A_HDRS))
#r.squaredGLMM(model_3D_A_HDRS)
#Check_data(data_3D_A, data_3D_A %>% Remove_smallHDRS())

model_2D_A_HDRS <- update(model_2D_A, data=data_2D_A %>% Remove_smallHDRS())
#plot(simulateResiduals(model_2D_A_HDRS)); summary(model_2D_A_HDRS); #plot(allEffects(model_2D_A_HDRS))
#r.squaredGLMM(model_2D_A_HDRS)
#Check_data(data_2D_A, data_2D_A %>% Remove_smallHDRS())

# ORGANISMAL GROUP

Macroinv_model_HDRS <- update(Macroinv_model, data=data_macroinv %>% Remove_smallHDRS())
#plot(simulateResiduals(Macroinv_model_HDRS)); summary(Macroinv_model_HDRS); #plot(allEffects(Macroinv_model_HDRS))
#r.squaredGLMM(Macroinv_model_HDRS)
#Check_data(data_macroinv, data_macroinv %>% Remove_smallHDRS())

Microinv_model_HDRS <- update(Microinv_model, data=data_microinv %>% Remove_smallHDRS())
#plot(simulateResiduals(Microinv_model_HDRS)); summary(Microinv_model_HDRS); #plot(allEffects(Microinv_model_HDRS))
#r.squaredGLMM(Microinv_model_HDRS)
#Check_data(data_microinv, data_microinv %>% Remove_smallHDRS())

### BIOGEOGRAPHY

# FACET - RICHNESS

model_3D_R_HDRS_biogeo <- update(model_3D_R_biogeo, data=data_3D_R %>% Remove_smallHDRS())
#plot(simulateResiduals(model_3D_R_HDRS)); summary(model_3D_R_HDRS); #plot(allEffects(model_3D_R_HDRS))
#r.squaredGLMM(model_3D_R_HDRS)

model_2D_R_HDRS_biogeo <- update(model_2D_R_biogeo, data=data_2D_R %>% Remove_smallHDRS())
#plot(simulateResiduals(model_2D_R_HDRS)); summary(model_2D_R_HDRS); #plot(allEffects(model_2D_R_HDRS))
#r.squaredGLMM(model_2D_R_HDRS)

# FACET - ABUNDANCE

model_3D_A_HDRS_biogeo <- update(model_3D_A_biogeo, data=data_3D_A %>% Remove_smallHDRS())
#plot(simulateResiduals(model_3D_A_HDRS)); summary(model_3D_A_HDRS); #plot(allEffects(model_3D_A_HDRS))
#r.squaredGLMM(model_3D_A_HDRS)

model_2D_A_HDRS_biogeo <- update(model_2D_A_biogeo, data=data_2D_A %>% Remove_smallHDRS())
#plot(simulateResiduals(model_2D_A_HDRS)); summary(model_2D_A_HDRS); #plot(allEffects(model_2D_A_HDRS))
#r.squaredGLMM(model_2D_A_HDRS)

# ORGANISMAL GROUP

Macroinv_model_HDRS_biogeo <- update(Biogeo_macroinv, data=data_macroinv %>% Remove_smallHDRS())
#plot(simulateResiduals(Macroinv_model_HDRS)); summary(Macroinv_model_HDRS); #plot(allEffects(Macroinv_model_HDRS))
#r.squaredGLMM(Macroinv_model_HDRS)

Microinv_model_HDRS_biogeo <- update(Biogeo_microinv, data=data_microinv %>% Remove_smallHDRS())
#plot(simulateResiduals(Microinv_model_HDRS)); summary(Microinv_model_HDRS); #plot(allEffects(Microinv_model_HDRS))
#r.squaredGLMM(Microinv_model_HDRS)

#
##### TESTING THE EFFECTS OF POTENTIALLY CONFOUNDED STUDY DESIGNS ####

model_3D_R_group <- update(model_3D_R, data=data_3D_R %>% filter(Grouping=="all"))
#plot(simulateResiduals(model_3D_R_group)); summary(model_3D_R_group); #plot(allEffects(model_3D_R_group))
#r.squaredGLMM(model_3D_R_group)
#Check_data(data_3D_R, data_3D_R %>% filter(Grouping=="all"))

model_2D_R_group <- update(model_2D_R, data=data_2D_R %>% filter(Grouping=="all"))
#plot(simulateResiduals(model_2D_R_group)); summary(model_2D_R_group); #plot(allEffects(model_2D_R_group))
#r.squaredGLMM(model_2D_R_group)
#Check_data(data_2D_R, data_2D_R %>% filter(Grouping=="all"))

model_3D_A_group <- update(model_3D_A, data=data_3D_A %>% filter(Grouping=="all"))
#plot(simulateResiduals(model_3D_A_group)); summary(model_3D_A_group); #plot(allEffects(model_3D_A_group))
#r.squaredGLMM(model_3D_A_group)
#Check_data(data_3D_A, data_3D_A %>% filter(Grouping=="all"))

model_2D_A_group <- update(model_2D_A, data=data_2D_A %>% filter(Grouping=="all"))
#plot(simulateResiduals(model_2D_A_group)); summary(model_2D_A_group); #plot(allEffects(model_2D_A_group))
#r.squaredGLMM(model_2D_A_group)
#Check_data(data_2D_A, data_2D_A %>% filter(Grouping=="all"))

Macroinv_model_group <- update(Macroinv_model, data=data_macroinv %>% filter(Grouping=="all"))
#plot(simulateResiduals(Macroinv_model_group)); summary(Macroinv_model_group); #plot(allEffects(Macroinv_model_group))
#r.squaredGLMM(Macroinv_model_group)
#Check_data(data_macroinv, data_macroinv %>% filter(Grouping=="all"))

Microinv_model_group <- update(Microinv_model, data=data_microinv %>% filter(Grouping=="all"))
#plot(simulateResiduals(Microinv_model_group)); summary(Microinv_model_group); #plot(allEffects(Microinv_model_group))
#r.squaredGLMM(Microinv_model_group)
#Check_data(data_microinv, data_microinv %>% filter(Grouping=="all"))

### BIOGEOGRAPHY

model_3D_R_group_biogeo <- update(model_3D_R_biogeo, data=data_3D_R %>% filter(Grouping=="all"))
#plot(simulateResiduals(model_3D_R_group)); summary(model_3D_R_group); #plot(allEffects(model_3D_R_group))
#r.squaredGLMM(model_3D_R_group)

model_2D_R_group_biogeo <- update(model_2D_R_biogeo, data=data_2D_R %>% filter(Grouping=="all"))
#plot(simulateResiduals(model_2D_R_group)); summary(model_2D_R_group); #plot(allEffects(model_2D_R_group))
#r.squaredGLMM(model_2D_R_group)

model_3D_A_group_biogeo <- update(model_3D_A_biogeo, data=data_3D_A %>% filter(Grouping=="all"))
#plot(simulateResiduals(model_3D_A_group)); summary(model_3D_A_group); #plot(allEffects(model_3D_A_group))
#r.squaredGLMM(model_3D_A_group)

model_2D_A_group_biogeo <- update(model_2D_A_biogeo, data=data_2D_A %>% filter(Grouping=="all"))
#plot(simulateResiduals(model_2D_A_group)); summary(model_2D_A_group); #plot(allEffects(model_2D_A_group))
#r.squaredGLMM(model_2D_A_group)

Macroinv_model_group_biogeo <- update(Biogeo_macroinv, data=data_macroinv %>% filter(Grouping=="all"))
#plot(simulateResiduals(Macroinv_model_group)); summary(Macroinv_model_group); #plot(allEffects(Macroinv_model_group))
#r.squaredGLMM(Macroinv_model_group)

Microinv_model_group_biogeo <- update(Biogeo_microinv, data=data_microinv %>% filter(Grouping=="all"))
#plot(simulateResiduals(Microinv_model_group)); summary(Microinv_model_group); #plot(allEffects(Microinv_model_group))
#r.squaredGLMM(Microinv_model_group)

#
##### PLOTTING SENSITIVITY TEST RESULTS ####

Extract_coefs_sensitivity <- function(Full_data, Large_data, HDRS_data, discrete_data, group_data, input_factor){
  
  Factor_selected <- input_factor
  
  Obtain_coefs_summary <- function(model, Factor_selected){
    
    Factor_selected <- input_factor
    
    model_out <- summary(model)$coefficients %>%
      data.frame() %>%
      rownames_to_column(var="Factor") %>%
      filter(Factor==Factor_selected)
    
    return(model_out)
    
  }
  
  out <- bind_rows(
    data.frame("Estimate"=Obtain_coefs_summary(Full_data)$Estimate, "Std_Err"=Obtain_coefs_summary(Full_data)$Std..Error, "Data"="Full"), 
    data.frame("Estimate"=Obtain_coefs_summary(Large_data)$Estimate, "Std_Err"=Obtain_coefs_summary(Large_data)$Std..Error,"Data"="No Large studies"), 
    data.frame("Estimate"=Obtain_coefs_summary(HDRS_data)$Estimate, "Std_Err"=Obtain_coefs_summary(HDRS_data)$Std..Error,"Data"="No Small HDRs"), 
    data.frame("Estimate"=Obtain_coefs_summary(discrete_data)$Estimate, "Std_Err"=Obtain_coefs_summary(discrete_data)$Std..Error,"Data"="No Discrete vars"),
    data.frame("Estimate"=Obtain_coefs_summary(group_data)$Estimate, "Std_Err"=Obtain_coefs_summary(group_data)$Std..Error,"Data"="No Confounded HDRs")
  )
  
  return(out)
  
}
Predict_coefs_sensitivity <- function(Full_data, Large_data, HDRS_data, discrete_data, group_data, Factor){
  
  out <- rbind(
    ggpredict(Full_data, terms=Factor, back_transform = FALSE) %>% mutate(Data="Full"),
    ggpredict(Large_data, terms=Factor, back_transform = FALSE) %>% mutate(Data="No Large studies"),
    ggpredict(HDRS_data, terms=Factor, back_transform = FALSE) %>% mutate(Data="No small HDRs"),
    ggpredict(discrete_data, terms=Factor, back_transform = FALSE) %>% mutate(Data="No Discrete vars"),
    ggpredict(group_data, terms=Factor, back_transform = FALSE) %>% mutate(Data="No Confounded HDRs")
  )
  
  return(out)
  
}

# Heterogeneity effects

Heterogeneity_sensitivity <- rbind(
  
  Extract_coefs_sensitivity(model_3D_R, model_3D_R_large, model_3D_R_HDRS, model_3D_R_c, model_3D_R_group, "Heterogeneity_01") %>% mutate(Test="3D_R"),
  Extract_coefs_sensitivity(model_2D_R, model_2D_R_large, model_2D_R_HDRS, model_2D_R_c, model_2D_R_group, "Heterogeneity_01") %>% mutate(Test="2D_R"),
  Extract_coefs_sensitivity(model_3D_A, model_3D_A_large, model_3D_A_HDRS, model_3D_A_c, model_3D_A_group, "Heterogeneity_01") %>% mutate(Test="3D_A"),
  Extract_coefs_sensitivity(model_2D_A, model_2D_A_large, model_2D_A_HDRS, model_2D_A_c, model_2D_A_group, "Heterogeneity_01") %>% mutate(Test="2D_A"),
  Extract_coefs_sensitivity(Macroinv_model, Macroinv_model_large, Macroinv_model_HDRS, Macroinv_model_c, Macroinv_model_group, "Heterogeneity_01") %>% mutate(Test="Macroinvertebrates"),
  Extract_coefs_sensitivity(Microinv_model, Microinv_model_large, Microinv_model_HDRS, Microinv_model_c, Microinv_model_group, "Heterogeneity_01") %>% mutate(Test="Microinvertebrates"),
  
  Extract_coefs_sensitivity(model_3D_R_biogeo, model_3D_R_large_biogeo, model_3D_R_HDRS_biogeo, model_3D_R_c_biogeo, model_3D_R_group, "Heterogeneity_01") %>% mutate(Test="3D_R_biogeo"),
  Extract_coefs_sensitivity(model_2D_R_biogeo, model_2D_R_large_biogeo, model_2D_R_HDRS_biogeo, model_2D_R_c_biogeo, model_2D_R_group, "Heterogeneity_01") %>% mutate(Test="2D_R_biogeo"),
  Extract_coefs_sensitivity(model_3D_A_biogeo, model_3D_A_large_biogeo, model_3D_A_HDRS_biogeo, model_3D_A_c_biogeo, model_3D_A_group, "Heterogeneity_01") %>% mutate(Test="3D_A_biogeo"),
  Extract_coefs_sensitivity(model_2D_A_biogeo, model_2D_A_large_biogeo, model_2D_A_HDRS_biogeo, model_2D_A_c_biogeo, model_2D_A_group, "Heterogeneity_01") %>% mutate(Test="2D_A_biogeo"),
  Extract_coefs_sensitivity(Biogeo_macroinv, Macroinv_model_large_biogeo, Macroinv_model_HDRS_biogeo, Macroinv_model_c_biogeo, Macroinv_model_group_biogeo, "Heterogeneity_01") %>% mutate(Test="Macroinvertebrates_biogeo"),
  Extract_coefs_sensitivity(Biogeo_microinv, Microinv_model_large_biogeo, Microinv_model_HDRS_biogeo, Microinv_model_c_biogeo, Microinv_model_group_biogeo, "Heterogeneity_01") %>% mutate(Test="Microinvertebrates_biogeo")

  ) %>%
  mutate(
    Test= factor(Test, levels=c("3D_R", "2D_R", "3D_A", "2D_A", "Macroinvertebrates", "Microinvertebrates", 
                                "3D_R_biogeo", "2D_R_biogeo", "3D_A_biogeo", "2D_A_biogeo", "Macroinvertebrates_biogeo", "Microinvertebrates_biogeo"
                                     )),
    Data = factor(Data, levels=c("No Confounded HDRs","No Discrete vars", "No Small HDRs", "No Large studies", "Full"))
         )

ggplot(Heterogeneity_sensitivity, aes(x=Data, y=Estimate, col=Data)) +
  geom_point() +
  geom_errorbar(Heterogeneity_sensitivity, mapping=aes(x=Data, ymin=Estimate-(Std_Err*1.96), ymax=Estimate+(Std_Err*1.96), col=Data),
                alpha=0.3,
                na.rm=TRUE, position=position_dodge(width=0.5),
                width=0.25,
                size=0.5)+
  geom_hline(yintercept = 0, linetype="dashed", alpha=0.3) +
  coord_flip()+
  facet_wrap(~Test, nrow=2) +
  theme_bw() +
  theme(
    legend.position = "none",
    axis.title.x=element_blank(),
    axis.title.y=element_blank(),
    strip.background = element_blank(),
    strip.text.x = element_blank()
  )
  

# Heterogeneity facet

Facet_sensitivity <- rbind(
  Predict_coefs_sensitivity(Biogeo_macroinv, Macroinv_model_large_biogeo, Macroinv_model_HDRS_biogeo, Macroinv_model_c_biogeo, Macroinv_model_group_biogeo, c("Het.facet [all]")) %>% mutate(Test="Macroinvertebrates"),
  Predict_coefs_sensitivity(Biogeo_microinv, Microinv_model_large_biogeo, Microinv_model_HDRS_biogeo, Microinv_model_c_biogeo, Microinv_model_group_biogeo, c("Het.facet [all]")) %>% mutate(Test="Microinvertebrates")
) %>%
  mutate(Data = factor(Data, levels=c("No Confounded HDRs","No Discrete vars", "No small HDRs", "No Large studies", "Full")))

ggplot(Facet_sensitivity, aes(x=Data, y=predicted, col=Data)) +
  geom_point() +
  geom_errorbar(Facet_sensitivity, mapping=aes(x=Data, ymin=predicted-std.error*1.96, ymax=predicted+std.error*1.96, col=Data),
                alpha=0.3,
                na.rm=TRUE, position=position_dodge(width=0.5),
                width=0.25,
                size=0.5)+
  geom_hline(yintercept = 0, linetype="dashed", alpha=0.3) +
  coord_flip()+
  #facet_wrap(~Test*x, nrow=2) +
  facet_grid(Test~x) +
  theme_bw() +
  theme(
    legend.position = "none",
    axis.title.x=element_blank(),
    axis.title.y=element_blank(),
    strip.background = element_blank(),
    strip.text = element_blank()
  )

# Substrate

Substrate_sensitivity <- rbind(
  Predict_coefs_sensitivity(model_3D_R_biogeo, model_3D_R_large_biogeo, model_3D_R_HDRS_biogeo, model_3D_R_c_biogeo, model_3D_R_group_biogeo, c("Sub [all]")) %>% mutate(Test="3D_R"),
  Predict_coefs_sensitivity(model_2D_R_biogeo, model_2D_R_large_biogeo, model_2D_R_HDRS_biogeo, model_2D_R_c_biogeo, model_2D_R_group_biogeo, c("Sub [all]")) %>% mutate(Test="2D_R"),
  Predict_coefs_sensitivity(model_3D_A_biogeo, model_3D_A_large_biogeo, model_3D_A_HDRS_biogeo, model_3D_A_c_biogeo, model_3D_A_group_biogeo, c("Sub [all]")) %>% mutate(Test="3D_A"),
  Predict_coefs_sensitivity(model_2D_A_biogeo, model_2D_A_large_biogeo, model_2D_A_HDRS_biogeo, model_2D_A_c_biogeo, model_2D_A_group_biogeo, c("Sub [all]")) %>% mutate(Test="2D_A"),
  Predict_coefs_sensitivity(Biogeo_macroinv, Macroinv_model_large_biogeo, Macroinv_model_HDRS_biogeo, Macroinv_model_c_biogeo, Macroinv_model_group_biogeo, c("Sub [all]")) %>% mutate(Test="Macroinvertebrates"),
  Predict_coefs_sensitivity(Biogeo_microinv, Microinv_model_large_biogeo, Microinv_model_HDRS_biogeo, Microinv_model_c_biogeo, Microinv_model_group_biogeo, c("Sub [all]")) %>% mutate(Test="Microinvertebrates")
) %>%
  mutate(
    Test = factor(Test, levels = c("3D_R", "2D_R", "3D_A", "2D_A", "Macroinvertebrates", "Microinvertebrates"))
  ) %>%
  mutate(Data = factor(Data, levels=c("No Confounded HDRs","No Discrete vars", "No small HDRs", "No Large studies", "Full")))

ggplot(Substrate_sensitivity, aes(x=Data, y=predicted, col=Data)) +
  geom_point() +
  geom_errorbar(Substrate_sensitivity, mapping=aes(x=Data, ymin=predicted-std.error, ymax=predicted+std.error, col=Data),
                alpha=0.3,
                na.rm=TRUE, position=position_dodge(width=0.5),
                width=0.25,
                size=0.5)+
  geom_hline(yintercept = 0, linetype="dashed", alpha=0.3) +
  coord_flip()+
  facet_wrap(~x*Test, nrow=2) +
  #facet_grid(Test~x, nrow=2) +
  theme_bw() +
  theme(
    legend.position = "none",
    axis.title.x=element_blank(),
    axis.title.y=element_blank(),
    strip.background = element_blank(),
    strip.text.x = element_blank()
  )

# Depth

Depth_sensitivity <- rbind(
  Predict_coefs_sensitivity(model_3D_R_biogeo, model_3D_R_large_biogeo, model_3D_R_HDRS_biogeo, model_3D_R_c_biogeo, model_3D_R_group_biogeo, c("Depth [all]")) %>% mutate(Test="3D_R"),
  Predict_coefs_sensitivity(model_2D_R_biogeo, model_2D_R_large_biogeo, model_2D_R_HDRS_biogeo, model_2D_R_c_biogeo, model_2D_R_group_biogeo, c("Depth [all]")) %>% mutate(Test="2D_R"),
  Predict_coefs_sensitivity(model_3D_A_biogeo, model_3D_A_large_biogeo, model_3D_A_HDRS_biogeo, model_3D_A_c_biogeo, model_3D_A_group_biogeo, c("Depth [all]")) %>% mutate(Test="3D_A"),
  Predict_coefs_sensitivity(model_2D_A_biogeo, model_2D_A_large_biogeo, model_2D_A_HDRS_biogeo, model_2D_A_c_biogeo, model_2D_A_group_biogeo, c("Depth [all]")) %>% mutate(Test="2D_A"),
  Predict_coefs_sensitivity(Biogeo_macroinv, Macroinv_model_large_biogeo, Macroinv_model_HDRS_biogeo, Macroinv_model_c_biogeo, Macroinv_model_group_biogeo, c("Depth [all]")) %>% mutate(Test="Macroinvertebrates"),
  Predict_coefs_sensitivity(Biogeo_microinv, Microinv_model_large_biogeo, Microinv_model_HDRS_biogeo, Microinv_model_c_biogeo, Microinv_model_group_biogeo, c("Depth [all]")) %>% mutate(Test="Microinvertebrates")
) %>%
  mutate(
    Test = factor(Test, levels = c("3D_R", "2D_R", "3D_A", "2D_A", "Macroinvertebrates", "Microinvertebrates"))
  ) %>%
  mutate(Data = factor(Data, levels=c("No Confounded HDRs","No Discrete vars", "No small HDRs", "No Large studies", "Full")))

ggplot(Depth_sensitivity, aes(x=Data, y=predicted, col=Data)) +
  geom_point() +
  geom_errorbar(Depth_sensitivity, mapping=aes(x=Data, ymin=predicted-std.error*1.96, ymax=predicted+std.error*1.96, col=Data),
                alpha=0.3,
                na.rm=TRUE, position=position_dodge(width=0.5),
                width=0.25,
                size=0.5)+
  geom_hline(yintercept = 0, linetype="dashed", alpha=0.3) +
  coord_flip()+
  facet_grid(Test~x) +
  theme_bw() +
  theme(
    legend.position = "none",
    axis.title.x=element_blank(),
    axis.title.y=element_blank(),
    strip.background = element_blank(),
    strip.text = element_blank()
  )

# Latitude

Latitude_sensitivity <- rbind(
  
  Extract_coefs_sensitivity(Biogeo_macroinv, Macroinv_model_large_biogeo, Macroinv_model_HDRS_biogeo, Macroinv_model_c_biogeo, Macroinv_model_group_biogeo, "Latitude_abs") %>% mutate(Test="Macroinvertebrates_biogeo"),
  Extract_coefs_sensitivity(Biogeo_microinv, Microinv_model_large_biogeo, Microinv_model_HDRS_biogeo, Microinv_model_c_biogeo, Microinv_model_group_biogeo, "Latitude_abs") %>% mutate(Test="Microinvertebrates_biogeo")
  
  ) %>%
  mutate(Data = factor(Data, levels=c("No Confounded HDRs","No Discrete vars", "No Small HDRs", "No Large studies", "Full")))

ggplot(Latitude_sensitivity, aes(x=Data, y=Estimate, col=Data)) +
  geom_point() +
  geom_errorbar(Latitude_sensitivity, mapping=aes(x=Data, ymin=Estimate-Std_Err*1.96, ymax=Estimate+Std_Err*1.96, col=Data),
                alpha=0.3,
                na.rm=TRUE, position=position_dodge(width=0.5),
                width=0.25,
                size=0.5)+
  geom_hline(yintercept = 0, linetype="dashed", alpha=0.3) +
  coord_flip()+
  facet_wrap(~Test, nrow=1) +
  theme_bw() +
  theme(
    legend.position = "none",
    axis.title.x=element_blank(),
    axis.title.y=element_blank(),
    strip.background = element_blank(),
    strip.text.x = element_blank()
  )


#
##### TESTING FOR THE SHAPE OF THE RELATIONSHIP ####


# RICHNESS - SUBSTRATE 3D AMOUNT

model_MM<-nlme(log(Response_ratio) ~ Vm*Heterogeneity_01/(K+Heterogeneity_01) * Heterogeneity.cv ,
               fixed=c(Heterogeneity_01~1+Heterogeneity.cv), 
               random = Heterogeneity_01~1|Study.included.ID/Effects_group,
               start = c(Heterogeneity_01= 0, Heterogeneity.cv=0),
               method="ML",
               control=nlmeControl(maxIter = 1000, pnlsMaxIter=1000, msMaxIter = 1000, msVerbose = TRUE),
               data=data_3D_R %>% Remove_smallHDRS_2()
)
summary(model_MM)

model_2P<-nlme(log(Response_ratio) ~ Heterogeneity_2 + Heterogeneity_01 * Heterogeneity.cv ,
               fixed=c(Heterogeneity_01~1+Heterogeneity.cv, Heterogeneity_2~1), 
               random = Heterogeneity_01~1|Study.included.ID/Effects_group,
               method="ML",
               start = c(Heterogeneity_01= 0, Heterogeneity.cv=0, Heterogeneity_2=0),
               control=nlmeControl(maxIter = 1000, pnlsMaxIter=1000, msMaxIter = 1000, msVerbose = TRUE),
               data=data_3D_R %>% Remove_smallHDRS_2()
)
summary(model_2P)


model_L<-nlme(log(Response_ratio) ~ Heterogeneity_01 * Heterogeneity.cv, 
              fixed=c(Heterogeneity_01~1+Heterogeneity.cv), 
              random = Heterogeneity_01~1|Study.included.ID/Effects_group,
              method="ML",
              start = c(Heterogeneity_01= 0, Heterogeneity.cv=0),
              control=nlmeControl(maxIter = 1000, pnlsMaxIter=1000, msMaxIter = 1000, msVerbose = TRUE),
              data=data_3D_R %>% Remove_smallHDRS_2()
)
summary(model_L)

anova(model_MM, model_2P, model_L)

# RICHNESS - SUBSTRATE 2D AMOUNT

model_MM<-nlme(log(Response_ratio) ~ Vm*Heterogeneity_01/(K+Heterogeneity_01) * Heterogeneity.cv ,
               fixed=c(Heterogeneity_01~1+Heterogeneity.cv), 
               random = Heterogeneity_01~1|Study.included.ID/Effects_group,
               method="ML",
               start = c(Heterogeneity_01= 0, Heterogeneity.cv=0),
               control=nlmeControl(maxIter = 1000, pnlsMaxIter=1000, msMaxIter = 1000, msVerbose = TRUE),
               data=data_2D_R %>% Remove_smallHDRS()
)
summary(model_MM)

model_2P<-nlme(log(Response_ratio) ~ Heterogeneity_2 + Heterogeneity_01 * Heterogeneity.cv ,
               fixed=c(Heterogeneity_01~1+Heterogeneity.cv, Heterogeneity_2~1), 
               random = Heterogeneity_01~1|Study.included.ID/Effects_group,
               method = "ML",
               start = c(Heterogeneity_01= 1, Heterogeneity.cv=1, Heterogeneity_2=0),
               control=nlmeControl(maxIter = 1000, pnlsMaxIter=1000, msMaxIter = 100000, msVerbose = TRUE),
               data=data_2D_R %>% Remove_smallHDRS()
)
summary(model_2P)


model_L<-nlme(log(Response_ratio) ~ Heterogeneity_01 * Heterogeneity.cv, 
              fixed=c(Heterogeneity_01~1+Heterogeneity.cv), 
              random = Heterogeneity_01~1|Study.included.ID/Effects_group,
              method="ML",
              start = c(Heterogeneity_01= 0, Heterogeneity.cv=0),
              control=nlmeControl(maxIter = 1000, pnlsMaxIter=1000, msMaxIter = 1000, msVerbose = TRUE),
              data=data_2D_R %>% Remove_smallHDRS()
)
summary(model_L)

anova(model_MM, model_2P, model_L)


# ABUNDANCE - SUBSTRATE 3D AMOUNT

model_MM<-nlme(log(Response_ratio) ~ Vm*Heterogeneity_01/(K+Heterogeneity_01) * Heterogeneity.cv ,
               fixed=c(Heterogeneity_01~1+Heterogeneity.cv), 
               random = Heterogeneity_01~1|Study.included.ID/Effects_group,
               method="ML",
               start = c(Heterogeneity_01= 0, Heterogeneity.cv=0),
               control=nlmeControl(maxIter = 1000, pnlsMaxIter=1000, msMaxIter = 1000, msVerbose = TRUE),
               data=data_3D_A %>% Remove_smallHDRS()
)
summary(model_MM)

model_2P<-nlme(log(Response_ratio) ~ Heterogeneity_2 + Heterogeneity_01 * Heterogeneity.cv ,
               fixed=c(Heterogeneity_01~1+Heterogeneity.cv, Heterogeneity_2~1), 
               random = Heterogeneity_01~1|Study.included.ID/Effects_group,
               method="ML",
               start = c(Heterogeneity_01= 0, Heterogeneity.cv=0, Heterogeneity_2=0),
               control=nlmeControl(maxIter = 1000, pnlsMaxIter=1000, msMaxIter = 1000, msVerbose = TRUE),
               data=data_3D_A %>% Remove_smallHDRS()
)
summary(model_2P)

model_L<-nlme(log(Response_ratio) ~ Heterogeneity_01 * Heterogeneity.cv, 
              fixed=c(Heterogeneity_01~1+Heterogeneity.cv), 
              random = Heterogeneity_01~1|Study.included.ID/Effects_group,
              method="ML",
              start = c(Heterogeneity_01= 0, Heterogeneity.cv=0),
              control=nlmeControl(maxIter = 1000, pnlsMaxIter=1000, msMaxIter = 1000, msVerbose = TRUE),
              data=data_3D_A %>% Remove_smallHDRS()
)
summary(model_L)

anova(model_MM, model_2P, model_L)

# ABUNDANCE - SUBSTRATE 2D AMOUNT

model_MM<-nlme(log(Response_ratio) ~ Vm*Heterogeneity_01/(K+Heterogeneity_01) * Heterogeneity.cv ,
               fixed=c(Heterogeneity_01~1+Heterogeneity.cv), 
               random = Heterogeneity_01~1|Study.included.ID/Effects_group,
               method="ML",
               start = c(Heterogeneity_01= 0, Heterogeneity.cv=0),
               control=nlmeControl(maxIter = 1000, pnlsMaxIter=1000, msMaxIter = 1000, msVerbose = TRUE),
               data=data_2D_A %>% Remove_smallHDRS()
)
summary(model_MM)

model_2P<-nlme(log(Response_ratio) ~ Heterogeneity_2 + Heterogeneity_01 * Heterogeneity.cv ,
               fixed=c(Heterogeneity_01~1+Heterogeneity.cv, Heterogeneity_2~1), 
               random = Heterogeneity_01~1|Study.included.ID/Effects_group,
               method="ML",
               start = c(Heterogeneity_01= 0, Heterogeneity.cv=0, Heterogeneity_2=0),
               control=nlmeControl(maxIter = 1000, pnlsMaxIter=1000, msMaxIter = 1000, msVerbose = TRUE),
               data=data_2D_A %>% Remove_smallHDRS()
)
summary(model_2P)

model_L<-nlme(log(Response_ratio) ~ Heterogeneity_01 * Heterogeneity.cv, 
              fixed=c(Heterogeneity_01~1+Heterogeneity.cv), 
              random = Heterogeneity_01~1|Study.included.ID/Effects_group,
              method="ML",
              start = c(Heterogeneity_01= 0, Heterogeneity.cv=0),
              control=nlmeControl(maxIter = 1000, pnlsMaxIter=1000, msMaxIter = 1000, msVerbose = TRUE),
              data=data_2D_A %>% Remove_smallHDRS()
)
summary(model_L)

anova(model_MM, model_2P, model_L)

# Macroinvertebrates

model_MM<-nlme(log(Response_ratio) ~ Vm*Heterogeneity_01/(K+Heterogeneity_01) * Heterogeneity.cv ,
               fixed=c(Heterogeneity_01~1+Heterogeneity.cv), 
               random = Heterogeneity_01~1|Study.included.ID/Effects_group,
               method="ML",
               start = c(Heterogeneity_01= 0, Heterogeneity.cv=0),
               control=nlmeControl(maxIter = 1000, pnlsMaxIter=1000, msMaxIter = 1000, msVerbose = TRUE),
               data=data_macroinv %>% Remove_smallHDRS()
)
summary(model_MM)

model_2P<-nlme(log(Response_ratio) ~ Heterogeneity_2 + Heterogeneity_01 * Heterogeneity.cv ,
               fixed=c(Heterogeneity_01~1+Heterogeneity.cv, Heterogeneity_2~1), 
               random = Heterogeneity_01~1|Study.included.ID/Effects_group,
               method="ML",
               start = c(Heterogeneity_01= 0, Heterogeneity.cv=0, Heterogeneity_2=0),
               control=nlmeControl(maxIter = 1000, pnlsMaxIter=1000, msMaxIter = 1000, msVerbose = TRUE),
               data=data_macroinv %>% Remove_smallHDRS()
)
summary(model_2P)

model_L<-nlme(log(Response_ratio) ~ Heterogeneity_01 * Heterogeneity.cv, 
              fixed=c(Heterogeneity_01~1+Heterogeneity.cv), 
              random = Heterogeneity_01~1|Study.included.ID/Effects_group,
              method="ML",
              start = c(Heterogeneity_01= 0, Heterogeneity.cv=0),
              control=nlmeControl(maxIter = 1000, pnlsMaxIter=1000, msMaxIter = 1000, msVerbose = TRUE),
              data=data_macroinv %>% Remove_smallHDRS()
)
summary(model_L)

anova(model_MM, model_2P, model_L)

# Microinvertebrates

model_MM<-nlme(log(Response_ratio) ~ Vm*Heterogeneity_01/(K+Heterogeneity_01) * Heterogeneity.cv ,
               fixed=c(Heterogeneity_01~1+Heterogeneity.cv), 
               random = Heterogeneity_01~1|Study.included.ID/Effects_group,
               method="ML",
               start = c(Heterogeneity_01= 0, Heterogeneity.cv=0),
               control=nlmeControl(maxIter = 1000, pnlsMaxIter=1000, msMaxIter = 1000, msVerbose = TRUE),
               data=data_microinv %>% Remove_smallHDRS()
)
summary(model_MM)

model_2P<-nlme(log(Response_ratio) ~ Heterogeneity_2 + Heterogeneity_01 * Heterogeneity.cv ,
               fixed=c(Heterogeneity_01~1+Heterogeneity.cv, Heterogeneity_2~1), 
               random = Heterogeneity_01~1|Study.included.ID/Effects_group,
               method="ML",
               start = c(Heterogeneity_01= 0, Heterogeneity.cv=0, Heterogeneity_2=0),
               control=nlmeControl(maxIter = 1000, pnlsMaxIter=1000, msMaxIter = 1000, msVerbose = TRUE),
               data=data_microinv %>% Remove_smallHDRS()
)
summary(model_2P)

model_L<-nlme(log(Response_ratio) ~ Heterogeneity_01 * Heterogeneity.cv, 
              fixed=c(Heterogeneity_01~1+Heterogeneity.cv), 
              random = Heterogeneity_01~1|Study.included.ID/Effects_group,
              method="ML",
              start = c(Heterogeneity_01= 0, Heterogeneity.cv=0),
              control=nlmeControl(maxIter = 1000, pnlsMaxIter=1000, msMaxIter = 1000, msVerbose = TRUE),
              data=data_microinv %>% Remove_smallHDRS()
)
summary(model_L)

anova(model_MM, model_2P, model_L)

#