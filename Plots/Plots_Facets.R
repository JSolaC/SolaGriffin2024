# LOAD LIBRARIES
Packages <- c("tidyverse","ggeffects")
lapply(Packages, library, character.only = TRUE)

# LOAD FUNCTIONS
Extract_het_coefs<-function(input_model){
  
  b<-summary(input_model)$coefficients %>%
    as.data.frame() %>%
    rownames_to_column(var="variable") %>%
    filter(variable=="Heterogeneity_01" | variable=="I(Heterogeneity_01^2)")
  
  return(b)
  
}
Plot_Het<-function(data1, data2, colour_graph,ylim_low,ylim_high){
  
  plot<-ggplot()+
    geom_hline(yintercept=0, linetype="dashed", alpha=0.5)+
    geom_point(data1, mapping=aes(x=Heterogeneity_01, y= log(Response_ratio)), colour="#666666", alpha=0.2)+
    geom_line(data1, mapping = aes(x=Heterogeneity_01, y= predicted, group=paste(Effects_group, Study.included.ID)), colour="#666666", alpha=0.1)+
    geom_ribbon(data2, mapping=aes(x=x, ymin = (conf.low), ymax = (conf.high)),  fill=colour_graph, alpha = 0.1)+
    geom_line(data2, mapping=aes(x=x, y= (predicted)), colour=colour_graph)+

    ylim(ylim_low,ylim_high)+
    theme_bw()+
    theme(legend.position = "none")
  
  return(plot)
}

#### INVERTEBRATES - PREDICTIONS HET ######

data_3D_R$predicted <- predict(model_3D_R, newdata = data_3D_R)
data_2D_R$predicted <- predict(model_2D_R, newdata = data_2D_R)
data_size_R$predicted <- predict(model_size_R, newdata = data_size_R)
data_var_R$predicted <- predict(model_var_R, newdata = data_var_R)
data_rich_R$predicted <- predict(model_rich_R, newdata = data_rich_R)
data_CX_R$predicted <- predict(model_CX_R, newdata = data_CX_R)

Het_3D_R<-ggpredict(model_3D_R, terms = c("Heterogeneity_01 [all]"), back_transform = FALSE)
Het_2D_R<-ggpredict(model_2D_R, terms = c("Heterogeneity_01 [all]"), back_transform = FALSE)
Het_size_R<-ggpredict(model_size_R, terms = c("Heterogeneity_01 [all]"), back_transform = FALSE)
Het_var_R<-ggpredict(model_var_R, terms = c("Heterogeneity_01 [all]"), back_transform = FALSE)
Het_rich_R<-ggpredict(model_rich_R, terms = c("Heterogeneity_01 [all]"), back_transform = FALSE)
Het_CX_R<-ggpredict(model_CX_R, terms = c("Heterogeneity_01 [all]"), back_transform = FALSE)

Plot_Het(data_3D_R, Het_3D_R, "#0066FF",-3.1,4.5)
Plot_Het(data_2D_R, Het_2D_R, "#FF3333",-3.1,4.5)
Plot_Het(data_size_R, Het_size_R, "#00CC33",-3.1,4.5)
Plot_Het(data_var_R, Het_var_R, "#FF9900",-3.1,4.5)
Plot_Het(data_rich_R, Het_rich_R, "#FF66FF",-3.1,4.5)
Plot_Het(data_CX_R, Het_CX_R, "#660066",-3.1,4.5)

data_3D_A$predicted <- predict(model_3D_A, newdata = data_3D_A)
data_2D_A$predicted <- predict(model_2D_A, newdata = data_2D_A)
data_size_A$predicted <- predict(model_size_A, newdata = data_size_A)
data_var_A$predicted <- predict(model_var_A, newdata = data_var_A)
data_rich_A$predicted <- predict(model_rich_A, newdata = data_rich_A)
data_CX_A$predicted <- predict(model_CX_A, newdata = data_CX_A)

Het_3D_A<-ggpredict(model_3D_A, terms = c("Heterogeneity_01 [all]"), back_transform = FALSE)
Het_2D_A<-ggpredict(model_2D_A, terms = c("Heterogeneity_01 [all]"), back_transform = FALSE)
Het_size_A<-ggpredict(model_size_A, terms = c("Heterogeneity_01 [all]"), back_transform = FALSE)
Het_var_A<-ggpredict(model_var_A, terms = c("Heterogeneity_01 [all]"), back_transform = FALSE)
Het_rich_A<-ggpredict(model_rich_A, terms = c("Heterogeneity_01 [all]"), back_transform = FALSE)
Het_CX_A<-ggpredict(model_CX_A, terms = c("Heterogeneity_01 [all]"), back_transform = FALSE)

Plot_Het(data_3D_A, Het_3D_A, "#0066FF",-5.5,9.5)
Plot_Het(data_2D_A, Het_2D_A, "#FF3333",-5.5,9.5)
Plot_Het(data_size_A, Het_size_A, "#00CC33",-5.5,9.5)
Plot_Het(data_var_A, Het_var_A, "#FF9900",-5.5,9.5)
Plot_Het(data_rich_A, Het_rich_A, "#FF66FF",-5.5,9.5)
Plot_Het(data_CX_A, Het_CX_A, "#660066",-5.5,9.5)

#
#### INVERTEBRATES - SUMMARY HET ########

Facet_het_coefs<-rbind(
  Extract_het_coefs(model_3D_R) %>% mutate(Facet="substrate 3D amount", Metric="richness"),
  Extract_het_coefs(model_2D_R) %>% mutate(Facet="substrate 2D amount", Metric="richness"),
  Extract_het_coefs(model_size_R) %>% mutate(Facet="feature size", Metric="richness"),
  Extract_het_coefs(model_CX_R) %>% mutate(Facet="substrate complexity", Metric="richness"),
  Extract_het_coefs(model_var_R) %>% mutate(Facet="feature variation", Metric="richness"),
  Extract_het_coefs(model_rich_R) %>% mutate(Facet="feature richness", Metric="richness"),
  Extract_het_coefs(model_3D_A) %>% mutate(Facet="substrate 3D amount", Metric="abundance"),
  Extract_het_coefs(model_2D_A) %>% mutate(Facet="substrate 2D amount", Metric="abundance"),
  Extract_het_coefs(model_size_A) %>% mutate(Facet="feature size", Metric="abundance"),
  Extract_het_coefs(model_CX_A) %>% mutate(Facet="substrate complexity", Metric="abundance"),
  Extract_het_coefs(model_var_A) %>% mutate(Facet="feature variation", Metric="abundance"),
  Extract_het_coefs(model_rich_A) %>% mutate(Facet="feature richness", Metric="abundance")
) %>%
  group_by(Facet,Metric) %>%
  mutate(
    Facet = factor(Facet, levels=c("feature richness","feature variation","feature size","substrate complexity","substrate 2D amount","substrate 3D amount")),
    Metric = factor(Metric, levels = c("richness","abundance")))


ggplot(Facet_het_coefs %>% filter(variable=="Heterogeneity_01")
       , aes(x=Facet,
         y=Estimate,col=Facet, alpha=Metric))+
  geom_hline(yintercept=0, linetype="dashed")+
  geom_point(position=position_dodge(width=0.3))+
  geom_errorbar(aes(ymin=Estimate-(get('Std. Error')*1.96), ymax=Estimate+(get('Std. Error')*1.96)), width=0.1, position=position_dodge(width=0.3))+
  #scale_color_manual(values=c("black","grey"))+
  scale_color_manual(values=c("substrate 3D amount"="#0066FF","substrate 2D amount"="#FF3333","feature size"="#00CC33","feature variation"="#FF9900","feature richness"="#FF66FF","substrate complexity"="#660066"))+
  scale_alpha_discrete(range=c(1, 0.4))+
  ylab("Slope (x)") + xlab("Heterogeneity facet")+
  coord_flip() + scale_y_continuous(position = "right") +
  theme_bw() + theme(legend.position = 'none')

ggplot(Facet_het_coefs %>% filter(variable=="I(Heterogeneity_01^2)")
       , aes(x=Facet,
         y=Estimate,col=Facet, alpha=Metric))+
  geom_hline(yintercept=0, linetype="dashed")+
  geom_point(position=position_dodge(width=0.3))+
  geom_errorbar(aes(ymin=Estimate-(get('Std. Error')*1.96), ymax=Estimate+(get('Std. Error')*1.96)), width=0.1, position=position_dodge(width=0.3))+
  #scale_color_manual(values=c("black","grey"))+
  scale_color_manual(values=c("substrate 3D amount"="#0066FF","substrate 2D amount"="#FF3333","feature size"="#00CC33","feature variation"="#FF9900","feature richness"="#FF66FF","substrate complexity"="#660066"))+
  scale_alpha_discrete(range=c(1, 0.4))+
  ylab("Slope (x)") + xlab("Heterogeneity facet")+
  coord_flip() + scale_y_continuous(position = "right") +
  theme_bw() + theme(legend.position = 'none')


Facet_het_R2 <- data.frame(rbind(
  data.frame(r.squaredGLMM(model_3D_R), facet="substrate 3D amount", metric="Richness"),
  data.frame(r.squaredGLMM(model_2D_R), facet="substrate 2D amount", metric="Richness"),
  data.frame(r.squaredGLMM(model_size_R), facet="feature size", metric="Richness"),
  data.frame(r.squaredGLMM(model_var_R), facet="feature variation", metric="Richness"),
  data.frame(r.squaredGLMM(model_rich_R), facet="feature richness", metric="Richness"),
  data.frame(r.squaredGLMM(model_CX_R), facet="substrate complexity", metric="Richness"),
  data.frame(r.squaredGLMM(model_3D_A), facet="substrate 3D amount", metric="Abundance"),
  data.frame(r.squaredGLMM(model_2D_A), facet="substrate 2D amount", metric="Abundance"),
  data.frame(r.squaredGLMM(model_size_A), facet="feature size", metric="Abundance"),
  data.frame(r.squaredGLMM(model_var_A), facet="feature variation", metric="Abundance"),
  data.frame(r.squaredGLMM(model_rich_A), facet="feature richness", metric="Abundance"),
  data.frame(r.squaredGLMM(model_CX_A), facet="substrate complexity", metric="Abundance")
  )) %>%
  pivot_longer(
    cols=R2m:R2c,
    values_to ="R2",
    names_to = "Type"
  ) %>%
  mutate(
    facet=factor(facet, c("feature richness", "feature variation", "feature size", "substrate complexity", "substrate 2D amount", "substrate 3D amount")),
    Type=factor(Type, c("R2m", "R2c"))
  )

ggplot(Facet_het_R2 %>% filter(Type=="R2m"), aes(x=facet,y=R2,col=facet, alpha=metric))+
  geom_hline(yintercept=0, linetype="dashed")+
  geom_point(position=position_dodge(width=0.3))+
  scale_color_manual(values=c("substrate 3D amount"="#0066FF","substrate 2D amount"="#FF3333","feature size"="#00CC33","feature variation"="#FF9900","feature richness"="#FF66FF","substrate complexity"="#660066"))+
  scale_alpha_discrete(range=c(1, 0.4))+
  ylab("Slope (x)") + xlab("Heterogeneity facet")+
  coord_flip() + scale_y_continuous(limits=c(0,0.75),position = "right") +
  theme_bw() + 
  theme(
    legend.position = "none",
    axis.title.x=element_blank(),
    axis.title.y=element_blank(),
    strip.background = element_blank(),
    strip.text.x = element_blank()
    )

ggplot(Facet_het_R2 %>% filter(Type=="R2c"), aes(x=facet,y=R2,col=facet, alpha=metric))+
  geom_hline(yintercept=0, linetype="dashed")+
  geom_point(position=position_dodge(width=0.3))+
  scale_color_manual(values=c("substrate 3D amount"="#0066FF","substrate 2D amount"="#FF3333","feature size"="#00CC33","feature variation"="#FF9900","feature richness"="#FF66FF","substrate complexity"="#660066"))+
  scale_alpha_discrete(range=c(1, 0.4))+
  ylab("Slope (x)") + xlab("Heterogeneity facet")+
  coord_flip() + scale_y_continuous(limits=c(0,0.75),position = "right") +
  theme_bw() + 
  theme(
    legend.position = "none",
    axis.title.x=element_blank(),
    axis.title.y=element_blank(),
    strip.background = element_blank(),
    strip.text.x = element_blank()
  )

#