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
Plot_Het<-function(data1, data2, data3, colour_graph,ylim_low,ylim_high){
  
  colour_group <- ifelse(is.null(data1$group), paste(data1$group), paste(data1$group,data1$facet))
  
  plot<-ggplot()+
    geom_hline(yintercept=0, linetype="dashed", alpha=0.5)+
    geom_point(data3, mapping=aes(x=Heterogeneity_01, y= log(Response_ratio)), colour="#666666", alpha=0.2)+
    geom_line(data1, mapping = aes(x=x, y= (predicted), group=colour_group), colour="#666666", alpha=0.1)+
    geom_ribbon(data2, mapping=aes(x=x, ymin = (conf.low), ymax = (conf.high)),  fill=colour_graph, alpha = 0.1)+
    geom_line(data2, mapping=aes(x=x, y= (predicted)), colour=colour_graph)+
    ylim(ylim_low,ylim_high)+
    theme_bw()+
    theme(legend.position = "none")
  
  return(plot)
}

#### ORGANISMAL GROUPS - PREDICTIONS HET #########

Slopes_microinv<-ggpredict(Microinv_model, terms = c("Heterogeneity_01 [all]", "Study.included.ID", "Effects_group"), type="random", back.transform = FALSE)
Slopes_macroinv<-ggpredict(Macroinv_model, terms = c("Heterogeneity_01 [all]", "Study.included.ID","Effects_group"), type="random", back.transform = FALSE)
Slopes_macrof<-ggpredict(Macrof_model, terms = c("Heterogeneity_01 [all]", "Study.included.ID","Effects_group"), type="random", back.transform = FALSE)
Slopes_fish<-ggpredict(Fish_model, terms = c("Heterogeneity_01 [all]", "Study.included.ID","Effects_group"), type="random", back.transform = FALSE)
Slopes_macroal<-ggpredict(Macroalgae_model, terms = c("Heterogeneity_01 [all]", "Study.included.ID","Effects_group"), type="random", back.transform = FALSE)
Slopes_microal<-ggpredict(Microalgae_model, terms = c("Heterogeneity_01 [all]", "Study.included.ID","Effects_group"), type="random", back.transform = FALSE)

Het_microinv<-ggpredict(Microinv_model, terms = c("Heterogeneity_01 [all]"), back.transform = FALSE)
Het_macroinv<-ggpredict(Macroinv_model, terms = c("Heterogeneity_01 [all]"), back.transform = FALSE)
Het_macrof<-ggpredict(Macrof_model, terms = c("Heterogeneity_01 [all]"), back.transform = FALSE)
Het_fish<-ggpredict(Fish_model, terms = c("Heterogeneity_01 [all]"), back.transform = FALSE)
Het_macroal<-ggpredict(Macroalgae_model, terms = c("Heterogeneity_01 [all]"), back.transform = FALSE)
Het_microal<-ggpredict(Microalgae_model, terms = c("Heterogeneity_01 [all]"), back.transform = FALSE)

Plot_Het(Slopes_microinv, Het_microinv, data_microinv, "#F8766D",-10,8)
Plot_Het(Slopes_macroinv, Het_macroinv, data_macroinv, "#B79F00",-10,8)
Plot_Het(Slopes_macrof, Het_macrof, data_macrof, "#00BA38",-10,8)
Plot_Het(Slopes_fish, Het_fish, data_fish, "#00BFC4",-10,8)
Plot_Het(Slopes_macroal, Het_macroal, data_macroalgae, "#619CFF",-10,8)
Plot_Het(Slopes_microal, Het_microal, data_microalgae, "#F564E3",-10,8)

#
#### ORGANISMAL GROUPS - SUMMARY HET ########

Orgs_het_coefs<-rbind(
  Extract_het_coefs(Microinv_model) %>% mutate(Org="microinvertebrates"),
  Extract_het_coefs(Macroinv_model) %>% mutate(Org="macroinvertebrates"),
  Extract_het_coefs(Macrof_model) %>% mutate(Org="macrofauna"),
  Extract_het_coefs(Fish_model) %>% mutate(Org="fish"),
  Extract_het_coefs(Macroalgae_model) %>% mutate(Org="macroalgae"),
  Extract_het_coefs(Microalgae_model) %>% mutate(Org="microalgae")
)%>%
  mutate(
    Org = factor(Org, c("macroalgae", "macroinvertebrates", "macrofauna", "microalgae", "fish", "microinvertebrates"))
  )

ggplot(Orgs_het_coefs %>% filter(variable=="Heterogeneity_01"), 
       aes(x=Org
           ,y=Estimate,col=Org))+
  geom_hline(yintercept=0, linetype="dashed")+
  geom_point()+
  geom_errorbar(aes(x=Org, ymin=Estimate-(get('Std. Error')*1.96), ymax=Estimate+(get('Std. Error')*1.96),col=Org), width=0.2)+
  scale_color_manual(values=c("fish"="#00BFC4","microinvertebrates"="#F8766D","macroinvertebrates"="#B79F00",
                              "macrofauna"="#00BA38","macroalgae"="#619CFF","microalgae"="#F564E3"))+
  ylab("Slope (x)") + xlab("Organismal group")+
  coord_flip() + scale_y_continuous(position = "right") +
  theme_bw() + theme(legend.position = 'none')

ggplot(Orgs_het_coefs %>% filter(variable=="I(Heterogeneity_01^2)"), 
       aes(x=Org,
           y=Estimate,col=Org))+
  geom_hline(yintercept=0, linetype="dashed")+
  geom_point()+
  geom_errorbar(aes(x=Org, ymin=Estimate-(get('Std. Error')*1.96), ymax=Estimate+(get('Std. Error')*1.96),col=Org), width=0.2)+
  scale_color_manual(values=c("fish"="#00BFC4","microinvertebrates"="#F8766D","macroinvertebrates"="#B79F00",
                              "macrofauna"="#00BA38","macroalgae"="#619CFF","microalgae"="#F564E3"))+
  ylab("Slope (x)") + xlab("Organismal group")+
  coord_flip() + scale_y_continuous(position = "right") +
  theme_bw() + theme(legend.position = 'none')

Org_het_R2 <- data.frame(rbind(
  data.frame(r.squaredGLMM(Microinv_model), facet="microinvertebrates"),
  data.frame(r.squaredGLMM(Fish_model), facet="fish"),
  data.frame(r.squaredGLMM(Microalgae_model), facet="microalgae"),
  data.frame(r.squaredGLMM(Macrof_model), facet="large macroinvertebrates"),
  data.frame(r.squaredGLMM(Macroinv_model), facet="macroinvertebrates"),
  data.frame(r.squaredGLMM(Macroalgae_model), facet="macroalgae")
)) %>%
  pivot_longer(
    cols=R2m:R2c,
    values_to ="R2",
    names_to = "Type"
  ) %>%
  mutate(
    facet=factor(facet, c("macroalgae", "macroinvertebrates", "large macroinvertebrates", "microalgae", "fish", "microinvertebrates")),
    Type=factor(Type, c("R2m", "R2c"))
  )

ggplot(Org_het_R2, aes(x=facet,y=R2,col=facet, alpha=Type))+
  geom_hline(yintercept=0, linetype="dashed")+
  geom_point(position=position_dodge(width=0.3))+
  scale_color_manual(values=c("fish"="#00BFC4","microinvertebrates"="#F8766D","macroinvertebrates"="#B79F00", "large macroinvertebrates"="#00BA38","macroalgae"="#619CFF","microalgae"="#F564E3"))+ 
  scale_alpha_discrete(range=c(1, 0.4))+
  ylab("Slope (x)") + xlab("Heterogeneity facet")+
  coord_flip() + scale_y_continuous(position = "right") +
  theme_bw() + 
  theme(
    legend.position = "none",
    axis.title.x=element_blank(),
    axis.title.y=element_blank(),
    strip.background = element_blank(),
    strip.text.x = element_blank()
  )

#
#### ORGANISMAL GROUPS - FACETS ####

# Facet
Macroinv_Facet <- ggpredict(Macroinv_model, terms=c("Het.facet [all]"), back.transform = FALSE)
Microinv_Facet <- ggpredict(Microinv_model, terms=c("Het.facet [all]"), back.transform = FALSE)
Macrofau_Facet <- ggpredict(Macrof_model, terms=c("Het.facet [all]"), back.transform = FALSE)
Fish_Facet <- ggpredict(Fish_model, terms=c("Het.facet [all]"), back.transform = FALSE)
Macroalg_Facet <- ggpredict(Macroalgae_model, terms=c("Het.facet [all]"), back.transform = FALSE)
Microalg_Facet <- ggpredict(Microalgae_model, terms=c("Het.facet"), back.transform = FALSE) # model did not converge

#Facet graph for all facets
Facet_Taxa <- rbind(
  data.frame(Macroinv_Facet,  Taxa="macroinvertebrates"),
  data.frame(Microinv_Facet,  Taxa="microinvertebrates"),
  data.frame(Macrofau_Facet,  Taxa="macrofauna"),
  data.frame(Fish_Facet,  Taxa="fish"),
  data.frame(Macroalg_Facet,  Taxa="macroalgae"),
  data.frame(Microalg_Facet,  Taxa="microalgae")
  ) %>%
  mutate(
    x=factor(x, c("Substrate 3D amount", "Substrate 2D amount", "Other facets"))
    )


ggplot(Facet_Taxa, aes(x=reorder(Taxa,-predicted,mean),
                       y=predicted, col=x))+
  geom_hline(yintercept=0, linetype="dashed", alpha=0.5)+
  geom_point(na.rm=TRUE, position=position_dodge(width=0.8))+
  geom_errorbar(aes(ymin = predicted-std.error, ymax=predicted+std.error), alpha=0.3,
                na.rm=TRUE, position=position_dodge(width=0.8),
                width=0.25,
                size=0.5)+
  scale_color_manual(values=c("Substrate 3D amount"="#0066FF","Substrate 2D amount"="#FF3333","Other facets"="grey"))+
  ylim(-1.5,3)+
  theme_bw()+
  theme(legend.position = "none")

#
#### ORGANISMAL GROUPS - RESPONSE METRICS ####

Macroinv_Response <- ggpredict(Macroinv_model, terms=c("Response_metric"), back.transform = FALSE)
Microinv_Response <- ggpredict(Microinv_model, terms=c("Response_metric"), back.transform = FALSE)
Macrofau_Response <- ggpredict(Macrof_model, terms=c("Response_metric"), back.transform = FALSE)
Fish_Response <- ggpredict(Fish_model, terms=c("Response_metric"), back.transform = FALSE)
Macroalg_Response <- ggpredict(Macroalgae_model, terms=c("Response_metric"), back.transform = FALSE)
Microalg_Response <- ggpredict(Microalgae_model, terms=c("Response_metric"), back.transform = FALSE)

#Response graph for all facets
Response_Taxa <- rbind(
  data.frame(Macroinv_Response,  Taxa="macroinvertebrates"),
  data.frame(Microinv_Response,  Taxa="microinvertebrates"),
  data.frame(Macrofau_Response,  Taxa="macrofauna"),
  data.frame(Fish_Response,  Taxa="fish"),
  data.frame(Macroalg_Response,  Taxa="macroalgae"),
  data.frame(Microalg_Response,  Taxa="microalgae")
)

ggplot(Response_Taxa, aes(x=reorder(Taxa,-predicted,mean), #500x450
                          y=predicted, col=x))+
  geom_hline(yintercept=0, linetype="dashed", alpha=0.5)+
  geom_point(na.rm=TRUE, position=position_dodge(width=0.8))+
  geom_errorbar(aes(ymin = predicted-std.error, ymax=predicted+std.error), alpha=0.3,
                na.rm=TRUE, position=position_dodge(width=0.8),
                width=0.25,
                size=0.5)+
  ylim(-1.5,3)+
  theme_bw()+
  theme(legend.position = "none")#remove legend

#