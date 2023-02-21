
combine_and_plot <- function(effect1, effect2, relevel, diversity) {
  
  combined <- rbind(effect1, effect2)
  
  # Add land-use factors for plotting
  comb_plot_limits <- c("Primary", "YSV", "ISV", "MSV", "Plantation", "Pasture", "Cropland", "Urban") 
  
  # Add specific colours for each order
  group_colours <- c(Diptera = "#FFC300", Lepidoptera = "#ff5733", Hymenoptera = "#c70039", Hemiptera = "#900c3f", Coleoptera = "#581845")
  
  
  # If I don't supply relevel as an argument then do nothing
  if(missing(relevel)) {
    NULL
  } 
  # If I do, then relevel the plot based on the reference group of interest
  else {
    
  if(relevel == "Diptera"){
    combined <- combined %>% mutate(Order = relevel(Order, ref = "Diptera"))
  }
  if(relevel == "Coleoptera"){
    combined <- combined %>% mutate(Order = relevel(Order, ref = "Coleoptera"))
  }
  if(relevel == "Hymenoptera"){
    combined <- combined %>% mutate(Order = relevel(Order, ref = "Hymenoptera"))
  }
  if(relevel == "Hemiptera"){
    combined <- combined %>% mutate(Order = relevel(Order, ref = "Hemiptera"))
  }
  if(relevel == "Lepidoptera"){
    combined <- combined %>% mutate(Order = relevel(Order, ref = "Lepidoptera"))
  }
    
  }
  
    if(diversity == "Abundance"){
      plot <- combined %>%
        ggplot()+
        aes(x = Predominant_land_use, y = Percent_diff, colour = Order, group = Order)+
        geom_hline(yintercept = 0, linewidth = 0.5, color = ("black"), linetype = 1)+
        geom_point(size = 3, position = position_dodge(width = 0.5))+
        geom_linerange(aes(ymin = Percent_lower, ymax = Percent_upper), position = position_dodge(width = 0.5), linewidth = 1)+
        theme_classic()+
        scale_x_discrete(limits=comb_plot_limits)+
        geom_vline(xintercept = 1.5, linetype = 2)+
        geom_vline(xintercept = 2.5, linetype = 2)+
        geom_vline(xintercept = 3.5, linetype = 2)+
        geom_vline(xintercept = 4.5, linetype = 2)+
        geom_vline(xintercept = 5.5, linetype = 2)+
        geom_vline(xintercept = 6.5, linetype = 2)+
        geom_vline(xintercept = 7.5, linetype = 2)+
        theme(axis.text.x = element_text(face= "bold", angle = 45, hjust = 1),
              axis.title.x = element_blank(),
              axis.title.y = element_text(face = "bold"),
              panel.border = element_rect(colour = "black",  fill=NA))+
        xlab("Land use intensity class")+
        ylab("Total abundance difference (%)") +
        scale_colour_manual(values=group_colours) + # this overrides the colours for the groups as above
        guides(color = guide_legend(
          override.aes=list(shape = 15, size = 8)))
    }
    if(diversity == "Species_richness"){
      plot <- combined %>%
        ggplot()+
        aes(x = Predominant_land_use, y = Percent_diff, colour = Order, group = Order)+
        geom_hline(yintercept = 0, linewidth = 0.5, color = ("black"), linetype = 1)+
        geom_point(size = 3, position = position_dodge(width = 0.5))+
        geom_linerange(aes(ymin = Percent_lower, ymax = Percent_upper), position = position_dodge(width = 0.5), linewidth = 1)+
        theme_classic()+
        scale_x_discrete(limits=comb_plot_limits)+
        geom_vline(xintercept = 1.5, linetype = 2)+
        geom_vline(xintercept = 2.5, linetype = 2)+
        geom_vline(xintercept = 3.5, linetype = 2)+
        geom_vline(xintercept = 4.5, linetype = 2)+
        geom_vline(xintercept = 5.5, linetype = 2)+
        geom_vline(xintercept = 6.5, linetype = 2)+
        geom_vline(xintercept = 7.5, linetype = 2)+
        theme(axis.text.x = element_text(face= "bold", angle = 45, hjust = 1),
              axis.title.x = element_blank(),
              axis.title.y = element_text(face = "bold"),
              panel.border = element_rect(colour = "black",  fill=NA))+
        xlab("Land use intensity class")+
        ylab("Species richness difference (%)") +
        scale_colour_manual(values=group_colours) + # this overrides the colours for the groups as above
        guides(color = guide_legend(
          override.aes=list(shape = 15, size = 8))) # this makes the legend symbol a square
      
    }
    
    
  return(plot)
}
