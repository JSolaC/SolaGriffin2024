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
  
  data1 <- data1 %>%
    rowwise() %>%
    mutate(
      colour_group = paste(group,facet)
    )
  
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

#### ECO PROCESSES - PREDICTIONS HET #####

Slopes_graz<-ggpredict(model_graz, terms = c("Heterogeneity_01 [all]", "Study.included.ID","Effects_group"), type="random", back.transform = FALSE)
Slopes_pred<-ggpredict(model_pred, terms = c("Heterogeneity_01 [all]", "Study.included.ID","Effects_group"), type="random", back.transform = FALSE)
Slopes_Bod<-ggpredict(model_Bod, terms = c("Heterogeneity_01 [all]", "Study.included.ID","Effects_group"), type="random", back.transform = FALSE)
Slopes_Rec<-ggpredict(model_Rec, terms = c("Heterogeneity_01 [all]", "Study.included.ID","Effects_group"), type="random", back.transform = FALSE)

Het_graz<-ggpredict(model_graz, terms = c("Heterogeneity_01 [all]"), back.transform = FALSE)
Het_Pred<-ggpredict(model_pred, terms = c("Heterogeneity_01 [all]"), back.transform = FALSE)
Het_Bod<-ggpredict(model_Bod, terms = c("Heterogeneity_01 [all]"), back.transform = FALSE)
Het_Rec<-ggpredict(model_Rec, terms = c("Heterogeneity_01 [all]"), back.transform = FALSE)

Plot_Het(Slopes_graz, Het_graz, data_graz, "#336600",-4.2,3.5)
Plot_Het(Slopes_pred, Het_Pred, data_pred, "#990000",-4.2,3.5)
Plot_Het(Slopes_Bod, Het_Bod, data_bod, "#006699",-4.2,3.5)
Plot_Het(Slopes_Rec, Het_Rec, data_rec, "#CC9900",-4.2,3.5)

#
#### ECO PROCESSES - SUMMARY HET ########

EP_het_coefs<-rbind(
  Extract_het_coefs(model_graz) %>% mutate(EP="grazing"),
  Extract_het_coefs(model_pred) %>% mutate(EP="predation"),
  Extract_het_coefs(model_Bod) %>% mutate(EP="body size"),
  Extract_het_coefs(model_Rec) %>% mutate(EP="recruitment")
  ) %>%
  mutate(
    EP = factor(EP, c("grazing", "body size", "predation", "recruitment"))
    )

ggplot(EP_het_coefs %>% filter(variable=="Heterogeneity_01"), 
       aes(x=EP,y=Estimate,col=EP))+
  geom_hline(yintercept=0, linetype="dashed")+
  geom_point()+
  geom_errorbar(aes(x=EP, ymin=Estimate-(get('Std. Error')*1.96), ymax=Estimate+(get('Std. Error')*1.96),col=EP),width=0.2)+
  scale_color_manual(values=c("grazing"="#336600","predation"="#990000","body size"="#006699","recruitment"="#CC9900"))+
  scale_fill_manual(values=c("grazing"="#336600","predation"="#990000","body size"="#006699","recruitment"="#CC9900"))+
  ylab("Slope (x)") + xlab("Organismal group")+
  #ylim(-2.2,4.7)+
  coord_flip() + scale_y_continuous(position = "right") +
  theme_bw() + theme(legend.position = 'none')

ggplot(EP_het_coefs %>% filter(variable=="I(Heterogeneity_01^2)"), 
       aes(x=EP,y=Estimate,col=EP))+
  geom_hline(yintercept=0, linetype="dashed")+
  geom_point()+
  geom_errorbar(aes(x=EP, ymin=Estimate-(get('Std. Error')*1.96), ymax=Estimate+(get('Std. Error')*1.96),col=EP),width=0.2)+
  scale_color_manual(values=c("grazing"="#336600","predation"="#990000","body size"="#006699","recruitment"="#CC9900"))+
  scale_fill_manual(values=c("grazing"="#336600","predation"="#990000","body size"="#006699","recruitment"="#CC9900"))+
  ylab("Slope (x)") + xlab("Organismal group")+
  #ylim(-2.2,4.7)+
  coord_flip() + scale_y_continuous(position = "right") +
  theme_bw() + theme(legend.position = 'none')


EcoP_het_R2 <- data.frame(rbind(
  data.frame(r.squaredGLMM(model_graz), facet="grazing"),
  data.frame(r.squaredGLMM(model_pred), facet="predation"),
  data.frame(r.squaredGLMM(model_Bod), facet="body size"),
  data.frame(r.squaredGLMM(model_Rec), facet="recruitment")
)) %>%
  pivot_longer(
    cols=R2m:R2c,
    values_to ="R2",
    names_to = "Type"
  ) %>%
  mutate(
    facet=factor(facet, c("grazing", "body size", "predation", "recruitment")),
    Type=factor(Type, c("R2m", "R2c"))
  )

ggplot(EcoP_het_R2, aes(x=facet,y=R2,col=facet, alpha=Type))+
  geom_hline(yintercept=0, linetype="dashed")+
  geom_point(position=position_dodge(width=0.3))+
  scale_color_manual(values=c("grazing"="#336600","predation"="#990000","body size"="#006699","recruitment"="#CC9900"))+
  scale_alpha_discrete(range=c(1, 0.4))+
  ylab("Slope (x)") + xlab("Heterogeneity facet")+
  coord_flip() + scale_y_continuous(position = "right") +
  theme_bw() + 
  theme(
    legend.position = "none",
    #axis.title.x=element_blank(),
    #axis.title.y=element_blank(),
    strip.background = element_blank(),
    strip.text.x = element_blank()
  )

#
#Facet graph for all facets

Graz_Facet <- ggpredict(model_graz, terms=c("Het.facet [all]"), back.transform = FALSE)
Rec_Facet <- ggpredict(model_Rec, terms=c("Het.facet [all]"), back.transform = FALSE)
Pred_Facet <- ggpredict(model_pred, terms=c("Het.facet [all]"), back.transform = FALSE)
Bod_Facet <- ggpredict(model_Bod, terms=c("Het.facet [all]"), back.transform = FALSE)

Facet_Proc <- rbind(
  data.frame(Graz_Facet,  Process="grazing"),
  data.frame(Rec_Facet,  Process="recruitment"),
  data.frame(Pred_Facet,  Process="predation"),
  data.frame(Bod_Facet,  Process="body size")
) %>%
  mutate(
    x=factor(x, c("Substrate 3D amount", "Substrate 2D amount", "Other facets"))
  )


ggplot(Facet_Proc, aes(x=reorder(Process,-predicted,mean),
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
#Taxa graph for all taxa

#Graz_Facet <- ggpredict(model_graz, terms=c("Taxonomic.grouping [all]"), back.transform = FALSE)
Rec_Facet <- ggpredict(model_Rec, terms=c("Taxonomic.grouping [all]"), back.transform = FALSE)
#Pred_Facet <- ggpredict(model_pred, terms=c("Taxonomic.grouping [all]"), back.transform = FALSE)
Bod_Facet <- ggpredict(model_Bod, terms=c("Taxonomic.grouping [all]"), back.transform = FALSE)

Facet_Proc <- rbind(
  #data.frame(Graz_Facet,  Process="grazing"),
  data.frame(Rec_Facet,  Process="recruitment"),
  #data.frame(Pred_Facet,  Process="predation"),
  data.frame(Bod_Facet,  Process="body size")
) %>%
  mutate(x=ifelse(x=="macrofauna", "large macroinvertebrates", as.character(x))) %>%
  mutate(x=factor(x, c("microinvertebrates","fish","microalgae","large macroinvertebrates","macroinvertebrates","macroalgae")))


ggplot(Facet_Proc, aes(x=reorder(Process,-predicted,mean),
                       y=predicted, col=x))+
  geom_hline(yintercept=0, linetype="dashed", alpha=0.5)+
  geom_point(na.rm=TRUE, position=position_dodge(width=0.8))+
  geom_errorbar(aes(ymin = predicted-std.error, ymax=predicted+std.error), alpha=0.3,
                na.rm=TRUE, position=position_dodge(width=0.8),
                width=0.25,
                size=0.5)+
  scale_color_manual(values=c("fish"="#00BFC4","microinvertebrates"="#F8766D","macroinvertebrates"="#B79F00", "large macroinvertebrates"="#00BA38","macroalgae"="#619CFF","microalgae"="#F564E3"))+ 
  ylim(-1.5,3)+
  theme_bw() #+
  theme(legend.position = "none")

#