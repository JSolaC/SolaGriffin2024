# LOAD LIBRARIES
library(tidyverse)
library(plyr)

#LOAD FUNCTIONS
range0_1 <- function(x,na.rm){(x-min(x, na.rm = na.rm)+0.01)/(max(x, na.rm = na.rm)-min(x, na.rm = na.rm)+0.01)} # transform a given variable into the quasi 0-1 range (to avoid NAs from ratios and log transformations)
z_trans <- function(myVar, na.rm){(myVar - mean(myVar, na.rm = na.rm)) / sd(myVar, na.rm = na.rm)} # z-transform a given variable
inv <- function(x){ ((x - max(x)) * -1) + min(x) } # invert values - when heterogeneity increases as variable decreases
mutate_cond <- function(.data, condition, ..., envir = parent.frame()) {
  condition <- eval(substitute(condition), .data, envir)
  .data[condition, ] <- .data[condition, ] %>% mutate(...)
  .data
}

#load dataset
#Heterogeneity.data <- read.csv(file.choose())
Heterogeneity.data <- read.csv("/Users/jordisola/Library/Mobile Documents/com~apple~CloudDocs/Documents/PhD_thesis/Habitat_metanalysis/Data_analysis/Publication/Heterogeneity_metanalysis_data2.csv")

#Obtain Effect sizes (LnRR: natural logarithm of the response ratio)

Heterogeneity.data.model <- Heterogeneity.data %>%
  
  # For discrete variables, transform categories into a numeric variable
  mutate(
    Heterogeneity.value2 = as.numeric(ifelse(Heterogeneity.value=="min",0,
                                             ifelse(Heterogeneity.value=="mid1",0.25,
                                                    ifelse(Heterogeneity.value=="mid",0.5,
                                                           ifelse(Heterogeneity.value=="mid2",0.75,
                                                                  ifelse(Heterogeneity.value=="max",1,Heterogeneity.value
                                                                  ))))))
  ) %>%
  
  # Compute HDR-based metrics (e.g., Heterogeneity 0-1 metric per HDR)
  split(.$Effects_group)%>% # Create a list separating by Effect Size group
  lapply(function(x) {transform(x, Heterogenenity.cv = sd(Heterogeneity.value2)/mean(Heterogeneity.value2))}) %>% # Quantify the dispersion within each heterogeneity variable (see metadata below for more details)
  lapply(function(x) {transform(x, Heterogeneity_01 = as.numeric(range0_1(z_trans(Heterogeneity.value2, na.rm=T), na.rm=T)))}) %>% #Convert into z-score normalisation + range 0 to 1 (for potential log transformations)
  lapply(function(x) {transform(x, Heterogeneity_01=ifelse(Negative.link=="x",inv(Heterogeneity_01),Heterogeneity_01))}) %>% #In case the increase in a given Heterogeneity variable supposes a decrease in Heterogeneity
  lapply(function(x) {transform(x, Res_min_mean = mean(Response.value[Heterogeneity_01 == min(Heterogeneity_01)]))}) %>% # To measure effect size, we need to know what's the min response value per HDR
  do.call(rbind,.) %>%
  as.data.frame() %>%
  
  # Compute effect size per observation
  mutate(Response_ratio = abs((Response.value+1) / (Res_min_mean+1))) %>% # abs because some variables contained negative values - therefore producing negative ratios that could not be computed with natural logarithm
  
  # Reorganise variables within the dataset to ease exploration of variables (see Metadata below)
  dplyr::select(
    #Study and data identifiers
    Title, Study.included.ID, Effects_group, Year, 
    #Environmental variables
    Coordinate.X, Coordinate.Y, Latitude_abs, Biogeographical.region, Study.system, Depth, Sub, Discrete.or.continuous, Spatial.scale.class, Duration.of.the.study, Season, Taxonomic.grouping, Tax.group, Taxonomic.level,
    #Heterogeneity variables 1
    Heterogeneity.Metric, Heterogeneity.facet, Het.facet, Heterogeneity.value, Heterogeneity.value2, Heterogeneity_01,
    #Heterogeneity variables 2
    Heterogeneity.cv, Heterogeneity.units, Units.class, Negative.link,
    #Response variables
    Response_metric,Response.value, Response_ratio, Response_units, Res_min_mean,
    #Study assessment
    Sample_size, Grouping, Where.did.the.data.come.from., Is.het.the.main.variable..secondary.variable.or.a.covariate., How.did.they.create.the.heterogeneity.treatment., How.many.levels.of.heterogeneity.in.study., Summary.of.study
  ) 

Heterogeneity.data.model <- Heterogeneity.data.model %>%
  mutate(
    Resp.metric = ifelse(Response_metric=="Species richness", "Species diversity", Response_metric),
  ) %>%
  mutate(
    Resp.metric_01 = range0_1(log(Response_ratio), na.rm=T),
    #Resp.metric_sigmoid = sigmoid(log(Response_ratio)),
    Log_Response_ratio = log(Response_ratio),
    Heterogeneity_2 = Heterogeneity_01^2
  ) %>%
  dplyr::group_by(Effects_group) %>%
  dplyr::mutate(Vm=max(Response_ratio)) %>%
  dplyr::mutate(Vm_2=which.min(abs(Response_ratio-(Vm/2)))) %>%
  dplyr::mutate(K=Heterogeneity_01[Response_ratio=Vm_2]) %>%
  mutate(
    Het_MM = Vm*Heterogeneity_01/(K+Heterogeneity_01)
  )
