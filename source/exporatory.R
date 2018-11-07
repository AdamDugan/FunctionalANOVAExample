


###########################
## Load/Install Packages ##
###########################

pkgs = c("ggplot2","RColorBrewer")
for(p in pkgs) if(p %in% rownames(installed.packages()) == FALSE) {install.packages(p, dependencies = TRUE)}
for(p in pkgs) suppressPackageStartupMessages(library(p, quietly=TRUE, character.only=TRUE))
rm(p, pkgs)





#####################################
## Load the Simulated Data Objects ##
#####################################

load(file = "data/simulateddata.rda")





###################
## Plot the Data ##
###################

## Plot the "true" group-level functions
figure = ggplot() +
  geom_line(data = true.data, aes(x = Time, y = Y_True, color = Group), size = 1.25) +
  geom_point(data = dat, aes(x = Time, y = Y_Sim, group = ID, color = Group), size = 0.9, alpha = 0.3) +
  theme_bw() +
  scale_color_brewer(palette = "Set1") +
  ylab("Y") +
  ggtitle("The 'True' Functions and Simulated Data Points")

## Save the plot
ggsave(filename = "simulateddata.pdf",
       plot = figure,
       device = "pdf",
       path = "figs/",
       dpi = "retina",
       units = "cm",
       width = 8,
       height = 6,
       scale = 3)

## Save the plot as an R object
save(figure, file = "figs/simulateddata.rda")



