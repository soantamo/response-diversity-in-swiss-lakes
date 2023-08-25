###colors R

lakecolors <- read_rds("/home/sophie/Dokumente/Master Thesis R/Master Thesis Analysis/df_final_ne.rds")
lakecolors$Species <- as.factor(lakecolors$Species)

levels(lakecolors$Species)
lakeColors <- setNames(c(
  "#9239F6", "#903495", "#6F3460", "#4A354F", "#D20076", "#FF0076", "#FF4373", 
  "#FF6B58", "#F8B660", "#83DDE0", "#28ADA8", "#3F86BC", "#7A3A9A", "#392682", 
  "#296656", "#569874", "#7EC488", "#76BDCF", "#CE75AD", "#35274A", "#3794BF", 
  "#637394", "#8F526A", "#BC313F", "#E2110F", "#AA352D", "#387C6B", "#20DE8B",
  "#79AF8B", "#AF6AE2", "#966BFF", "#3B9AB2", "#78B7C5", "#E1AF00", "#1B9E77", 
  "#D95F02", "#7570B3", "#E6AB02", "#66A61E", "#E78AC3", "#A6761D", "#666666", 
  "#5BBCD6", "#1B5E20", "#FD6467", "#FAD510", "#FC8D62", "#8DA0CB", "#550307", 
  "#A6D854", "#FFD92F", "#ff4316", "#d15550", "#a2688b", "#747ac5", "#458cff", 
  "#1bff30", "#26e264", "#30c698", "#3ba9cb", "#FFBF7F", "#FF7F00", "#FFFF99", 
  "#B2FF8C", "#32FF00", "#A5EDFF", "#19B2FF", "#CCBFFF", "#654CFF", "#FF99BF", 
  "#E51932", "#E64B35", "#4DBBD5", "#00A087", "#3C5488", "#F39B7F", "#8491B4", 
  "#91D1C2", "#DC0000", "#7E6148", "#B09C85", "#4A6990", "#A73030", "#3B3B3B", 
  "#8F7700", "#003C67", "#17BECF", "#BCBD22", "#7F7F7F", "#E377C2", "#8C564B", 
  "#C7C7C7"
), levels(lakecolors$Species))