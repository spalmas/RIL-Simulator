#colorblind friendly colors
line.colors <- c("#000000", "#E69F00", "#56B4E9", "#009E73", "#F0E442", "#0072B2", "#D55E00")  

#ggplot parameters 
ggplot_params <- function(){
  theme_classic() + 
    theme(axis.title.x=element_text(colour="black", size = 18, vjust=-0.5),        #sets x axis title size, style, distance from axis #add , face = "bold" if you want bold
          axis.title.y=element_text(colour="black", size = 18, vjust=2),           #sets y axis title size, style, distance from axis #add , face = "bold" if you want bold
          axis.text=element_text(colour="black", size = 16),                       #sets size and style of labels on axes
          plot.title = element_text(hjust=0.02, vjust=-3, face="bold", size=22), #sets the position of the title
          #plot.margin =unit(c(1,1,1,1.5), "lines"),
          legend.position="bottom")
}