# LOAD LIBRARIES
Packages <- c("tidyverse","ggeffects","scales")
lapply(Packages, library, character.only = TRUE)

# LOAD FUNCTIONS
Get_latitude <- function(input_model, input_data, Interaction){
  
  if(Interaction=="No Interaction"){
  
  Latitude_Biogeo<-ggpredict(input_model, terms = c("Latitude_abs [all]"), back.transform = FALSE)
  
  } else {
    
  Latitude_Biogeo_bis<-ggpredict(input_model, terms = c("Latitude_abs [all]", "Season"), back.transform = FALSE) 
  
  Latitude_Biogeo<-Latitude_Biogeo_bis %>%
    #ggpredict provides LnRR values across all latitudes, but data coverage per Season is limited to narrower ranges of latitude. Here, predictions are trimmed to ranges of Latitude for which there is data.
    mutate(
        x = ifelse(group=="autumn" & (x<min(input_data[input_data$Season=="autumn",]$Latitude_abs)|x>max(input_data[input_data$Season=="autumn",]$Latitude_abs)),
                   NA,
                   ifelse(group=="spring" & (x<min(input_data[input_data$Season=="spring",]$Latitude_abs)|x>max(input_data[input_data$Season=="spring",]$Latitude_abs)),
                          NA,
                          ifelse(group=="summer" & (x<min(input_data[input_data$Season=="summer",]$Latitude_abs)|x>max(input_data[input_data$Season=="summer",]$Latitude_abs)),
                                 NA,
                                 ifelse(group=="winter" & (x<min(input_data[input_data$Season=="winter",]$Latitude_abs)|x>max(input_data[input_data$Season=="winter",]$Latitude_abs)),
                                        NA,
                                        ifelse(group=="mixed" & (x<min(input_data[input_data$Season=="mixed",]$Latitude_abs)|x>max(input_data[input_data$Season=="mixed",]$Latitude_abs)),
                                               NA,x)
                                        
                                 ))))
      ) %>%
      filter(!is.na(x))
    
  }

  
  return(Latitude_Biogeo)
  
}
Plot_latitude<-function(input_data,latitude_data){
  
  
  if(length(unique(latitude_data$group))<2) {
    
    latitude_data$colour_group <- "all"
    input_data$colour_group <- "all"
    
  } else {
    
    latitude_data$colour_group <- latitude_data$group
    input_data$colour_group <- input_data$Season
    
  }
  
  
  
  plot<-ggplot()+
    geom_hline(yintercept=0, linetype="dashed", alpha=0.5)+
    geom_point(input_data, mapping=aes(x=Latitude_abs, y=log(Response_ratio), col=colour_group), alpha=0.2)+
    geom_ribbon(latitude_data, mapping=aes(x=x, ymin=(predicted)-std.error , ymax=(predicted)+std.error, group=colour_group, fill=colour_group), alpha=0.2)+
    geom_line(latitude_data,mapping=aes(x=x, y= (predicted), group=colour_group, colour=colour_group))+
    geom_rug(data=input_data,aes(x=Latitude_abs, y=NULL, col=colour_group),sides="b")+
    scale_color_manual(values=c("all"="#000000","autumn"="#990000","mixed"="#999999","spring"="#66CC66","summer"="#FF9900","winter"="#0099FF"))+
    scale_fill_manual(values=c("all"="#000000","autumn"="#990000","mixed"="#999999","spring"="#66CC66","summer"="#FF9900","winter"="#0099FF"))+
    xlim(0,65)+
    ylim(-10,10)+
    theme_bw()+
    theme(legend.position="none")
  
  return(plot)
  
}

#### FUNCTIONAL GROUPS - LATITUDE ########

#Latitude

Latitude_macroinv<-Get_latitude(Biogeo_macroinv, data_biogeo_macroinv, "with Interaction")
Latitude_microinv<-Get_latitude(Biogeo_microinv, data_biogeo_microinv, "with Interaction")
Latitude_fish<-Get_latitude(Biogeo_fish, data_biogeo_fish, "with Interaction")
Latitude_macrof<-Get_latitude(Biogeo_macrof, data_biogeo_macrof, "No Interaction")
Latitude_macroalgae<-Get_latitude(Biogeo_macroalgae, data_biogeo_macroalgae, "No Interaction")
Latitude_microalgae<-Get_latitude(Biogeo_microalgae, data_biogeo_microalgae, "No Interaction")

Plot_latitude(data_biogeo_macroinv, Latitude_macroinv)
Plot_latitude(data_biogeo_microinv, Latitude_microinv)
Plot_latitude(data_biogeo_fish, Latitude_fish)
Plot_latitude(data_biogeo_macrof, Latitude_macrof)
Plot_latitude(data_biogeo_macroalgae, Latitude_macroalgae)
Plot_latitude(data_biogeo_microalgae, Latitude_microalgae)

#
#### FUNCTIONAL GROUPS - PREDICTIONS ####

#Substrate
Macroinv_Sub <- ggpredict(Biogeo_macroinv, terms=c("Sub"), back.transform = FALSE)
Microinv_Sub <- ggpredict(Biogeo_microinv, terms=c("Sub"), back.transform = FALSE)
Macrofau_Sub <- ggpredict(Biogeo_macrof, terms=c("Sub"), back.transform = FALSE)
Fish_Sub <- ggpredict(Biogeo_fish, terms=c("Sub"), back.transform = FALSE)
Macroalg_Sub <- ggpredict(Biogeo_macroalgae, terms=c("Sub"), back.transform = FALSE)
Microalg_Sub <- ggpredict(Biogeo_microalgae, terms=c("Sub"), back.transform = FALSE)

Sub_Taxa <- rbind(
  data.frame(Macroinv_Sub,  Taxa="macroinvertebrates"),
  data.frame(Microinv_Sub,  Taxa="microinvertebrates"),
  data.frame(Macrofau_Sub,  Taxa="macrofauna"),
  data.frame(Fish_Sub,  Taxa="fish"),
  data.frame(Macroalg_Sub,  Taxa="macroalgae"),
  data.frame(Microalg_Sub,  Taxa="microalgae")
) 

ggplot(Sub_Taxa, aes(x=reorder(Taxa,-predicted,mean),
                     y=predicted, col=x))+
  geom_hline(yintercept=0, linetype="dashed", alpha=0.5)+
  geom_point(na.rm=TRUE, position=position_dodge(width=0.5))+
  geom_errorbar(aes(ymin = predicted-std.error, ymax=predicted+std.error), alpha=0.3,
                na.rm=TRUE, position=position_dodge(width=0.5),
                width=0.25,
                size=0.5)+
  scale_color_manual(values=c("Biogenic"="#669933","Rock"="#996600"))+
  ylim(-4.6,4)+
  theme_bw()+
  theme( legend.position = "none")

#Depth
Macroinv_Depth <- ggpredict(Biogeo_macroinv, terms=c("Depth [all]"), back.transform = FALSE)
Microinv_Depth <- ggpredict(Biogeo_microinv, terms=c("Depth [all]"), back.transform = FALSE)
Fish_Depth<-ggpredict(Biogeo_fish, terms = c("Depth [all]"), back.transform = FALSE)
Macrofau_Depth<-ggpredict(Biogeo_macrof, terms = c("Depth [all]"), back.transform = FALSE)
Macroalg_Depth<-ggpredict(Biogeo_macroalgae, terms = c("Depth [all]"), back.transform = FALSE)
#Microalg_Depth<-ggpredict(Biogeo_microalgae, terms = c("Depth [all]"), back.transform = FALSE) # model did not converge

Depth_Taxa <- rbind(
  data.frame(Macroinv_Depth,  Taxa="macroinvertebrates"),
  data.frame(Microinv_Depth,  Taxa="microinvertebrates"),
  data.frame(Macrofau_Depth,  Taxa="macrofauna"),
  data.frame(Fish_Depth,  Taxa="fish"),
  data.frame(Macroalg_Depth,  Taxa="macroalgae")
  #data.frame(Microalg_Depth,  Taxa="microalgae")
) %>%
  mutate(
    x=factor(x, c("bi_high", "bi_low","sb_shallow", "sb_deep"))
  )

ggplot(Depth_Taxa, aes(x=reorder(Taxa,-predicted,mean),
                     y=predicted, col=x))+
  geom_hline(yintercept=0, linetype="dashed", alpha=0.5)+
  geom_point(na.rm=TRUE, position=position_dodge(width=0.5))+
  geom_errorbar(aes(ymin = predicted-std.error, ymax=predicted+std.error), alpha=0.3,
                na.rm=TRUE, position=position_dodge(width=0.5),
                width=0.25,
                size=0.5)+
  #scale_color_manual(values=c("Biogenic"="#669933","Rock"="#996600"))+
  ylim(-4.6,4)+
  theme_bw() +
  theme( legend.position = "none")

#### FACETS - PREDICTIONS ####

Sub_3D_R<-ggpredict(model_3D_R_biogeo, terms=c("Sub [all]"), back.transform = FALSE)
Sub_2D_R<-ggpredict(model_2D_R_biogeo, terms=c("Sub [all]"), back.transform = FALSE)
Sub_size_R<-ggpredict(model_size_R_biogeo, terms=c("Sub [all]"), back.transform = FALSE)
Sub_var_R<-ggpredict(model_var_R_biogeo, terms=c("Sub [all]"), back.transform = FALSE)
#Sub_ID_R<-ggpredict(model_rich_R_biogeo, terms=c("Sub [all]"), back.transform = FALSE) # only one level available in model
Sub_CX_R<-ggpredict(model_CX_R_biogeo, terms=c("Sub [all]"), back.transform = FALSE)

Sub_3D_A<-ggpredict(model_3D_A_biogeo, terms=c("Sub [all]"), back.transform = FALSE)
Sub_2D_A<-ggpredict(model_2D_A_biogeo, terms=c("Sub [all]"), back.transform = FALSE)
Sub_size_A<-ggpredict(model_size_A_biogeo, terms=c("Sub [all]"), back.transform = FALSE)
Sub_var_A<-ggpredict(model_var_A_biogeo, terms=c("Sub [all]"), back.transform = FALSE)
Sub_ID_A<-ggpredict(model_rich_A_biogeo, terms=c("Sub [all]"), back.transform = FALSE) # only one level available in model
Sub_CX_A<-ggpredict(model_CX_A_biogeo, terms=c("Sub [all]"), back.transform = FALSE)

Sub_R <- rbind(
  data.frame(Sub_3D_R,  Taxa="3D"),
  data.frame(Sub_2D_R,  Taxa="2D"),
  data.frame(Sub_size_R,  Taxa="size"),
  data.frame(Sub_var_R,  Taxa="variation"),
  data.frame(Sub_CX_R,  Taxa="CX")
) 

ggplot(Sub_R, aes(x=reorder(Taxa,-predicted,mean),
                     y=predicted, col=x))+
  geom_hline(yintercept=0, linetype="dashed", alpha=0.5)+
  geom_point(na.rm=TRUE, position=position_dodge(width=0.5))+
  geom_errorbar(aes(ymin = predicted-std.error, ymax=predicted+std.error), alpha=0.3,
                na.rm=TRUE, position=position_dodge(width=0.5),
                width=0.25,
                size=0.5)+
  scale_color_manual(values=c("Biogenic"="#669933","Rock"="#996600"))+
  ylim(-4.6,4)+
  theme_bw()+
  theme( legend.position = "none")

Sub_A <- rbind(
  data.frame(Sub_3D_A,  Taxa="3D"),
  data.frame(Sub_2D_A,  Taxa="2D"),
  data.frame(Sub_size_A,  Taxa="size"),
  data.frame(Sub_var_A,  Taxa="variation"),
  #data.frame(Sub_ID_A,  Taxa="richness"),
  data.frame(Sub_CX_A,  Taxa="CX")
) 

ggplot(Sub_A, aes(x=reorder(Taxa,-predicted,mean),
                  y=predicted, col=x))+
  geom_hline(yintercept=0, linetype="dashed", alpha=0.5)+
  geom_point(na.rm=TRUE, position=position_dodge(width=0.5))+
  geom_errorbar(aes(ymin = predicted-std.error, ymax=predicted+std.error), alpha=0.3,
                na.rm=TRUE, position=position_dodge(width=0.5),
                width=0.25,
                size=0.5)+
  scale_color_manual(values=c("Biogenic"="#669933","Rock"="#996600"))+
  #ylim(-4.6,4)+
  theme_bw()#+
  theme( legend.position = "none")

# DEPTH

Depth_3D_R<-ggpredict(model_3D_R_biogeo, terms=c("Depth [all]"), back.transform = FALSE)
Depth_2D_R<-ggpredict(model_2D_R_biogeo, terms=c("Depth [all]"), back.transform = FALSE)
Depth_size_R<-ggpredict(model_size_R_biogeo, terms=c("Depth [all]"), back.transform = FALSE)
Depth_var_R<-ggpredict(model_var_R_biogeo, terms=c("Depth [all]"), back.transform = FALSE)
Depth_ID_R<-ggpredict(model_rich_R_biogeo, terms=c("Depth [all]"), back.transform = FALSE)
Depth_CX_R<-ggpredict(model_CX_R_biogeo, terms=c("Depth [all]"), back.transform = FALSE)

Depth_3D_A<-ggpredict(model_3D_A_biogeo, terms=c("Depth [all]"), back.transform = FALSE)
Depth_2D_A<-ggpredict(model_2D_A_biogeo, terms=c("Depth [all]"), back.transform = FALSE)
Depth_size_A<-ggpredict(model_size_A_biogeo, terms=c("Depth [all]"), back.transform = FALSE)
Depth_var_A<-ggpredict(model_var_A_biogeo, terms=c("Depth [all]"), back.transform = FALSE)
Depth_ID_A<-ggpredict(model_rich_A_biogeo, terms=c("Depth [all]"), back.transform = FALSE)
Depth_CX_A<-ggpredict(model_CX_A_biogeo, terms=c("Depth [all]"), back.transform = FALSE)

Depth_R <- rbind(
  data.frame(Depth_3D_R,  facet = "3D"),
  data.frame(Depth_2D_R, facet = "2D"),
  data.frame(Depth_size_R,  facet = "size"),
  data.frame(Depth_var_R,  facet = "variation"),
  data.frame(Depth_ID_R,  facet = "richness"),
  data.frame(Depth_CX_R,  facet = "CX") )

ggplot(Depth_R, aes(x=reorder(facet,-predicted,mean),
                    y=predicted, col=x))+
  geom_hline(yintercept=0, linetype="dashed", alpha=0.5)+
  geom_point(na.rm=TRUE, position=position_dodge(width=0.5))+
  geom_errorbar(aes(ymin = predicted-std.error, ymax=predicted+std.error), alpha=0.3,
                na.rm=TRUE, position=position_dodge(width=0.5),
                width=0.25,
                size=0.5)+
  scale_color_manual(values=c("bi_high"=hue_pal()(4)[1],"bi_low"=hue_pal()(4)[2],"sb_shallow"=hue_pal()(4)[3],"sb_deep"=hue_pal()(4)[4]))+
  ylim(-4.6,4)+
  theme_bw()#+
  theme(legend.position = "none")


Depth_A <- rbind(
  data.frame(Depth_3D_A,  facet = "3D"),
  data.frame(Depth_2D_A, facet = "2D"),
  data.frame(Depth_size_A,  facet = "size"),
  data.frame(Depth_var_A,  facet = "variation"),
  data.frame(Depth_ID_A,  facet = "richness"),
  data.frame(Depth_CX_A,  facet = "CX") ) %>%
  mutate(x=factor(x,c("bi_high","bi_low","sb_shallow","sb_deep")))


ggplot(Depth_A, aes(x=reorder(facet,-predicted,mean),
                    y=predicted, col=x))+
  geom_hline(yintercept=0, linetype="dashed", alpha=0.5)+
  geom_point(na.rm=TRUE, position=position_dodge(width=0.5))+
  geom_errorbar(aes(ymin = predicted-std.error, ymax=predicted+std.error), alpha=0.3,
                na.rm=TRUE, position=position_dodge(width=0.5),
                width=0.25,
                size=0.5)+
  scale_color_manual(values=c("bi_high"=hue_pal()(4)[1],"bi_low"=hue_pal()(4)[2],"sb_shallow"=hue_pal()(4)[3],"sb_deep"=hue_pal()(4)[4]))+
  #ylim(-4.6,4)+
  theme_bw()+
  theme(legend.position = "none")

#