## Function for calculating two dimensions of response diversity
resp_div <- function(x, sign_sens = TRUE) {
  
  flag <- TRUE # flag to catch if all values are the same
  
  ## set diversity to zero if all values are the same
  if(length(unique(x)) == 1) {
    div = 0
    flag <- FALSE
  }
  
  ## keep going only if all values are not the same
  if(flag) {
    
    ## stat == range
    if(!sign_sens) {
      
      d <- dist(x, diag = T) # euclidean distance matrix
      Z <- exp(as.matrix(-d)) # similarity matrix
      nspecies <- length(x)
      p=matrix(1/nspecies,nspecies) # relative abundance matrix. Needs to be changed if evenness is of interest
      lenq = 1 # initialises hill number. Need to fix if we want any evenness indices
      qq <- seq(length=lenq, from=0, by=.11)
      
      # Initialise the Zp matrix to zero
      Zp=matrix(0,nspecies,1)
      
      # Compute Zp
      for (i in 1:nspecies){
        for (j in 1:nspecies){
          Zp[i,1]<-Zp[i,1]+Z[i,j]*p[j,1]
        }
      }
      
      # Initialise the Diversity matrix to zero
      Dqz = matrix(0, lenq ,1)
      
      for (iq in 1:lenq)  {
        q<-qq[iq];
        for (zpi in 1:length(Zp[,1])){
          if (Zp[zpi,1]>0)(
            Dqz[iq,1]<-Dqz[iq,1]+ p[zpi,1]*(Zp[zpi,1])^(q-1))
        }
        
        Dqz[iq,1] <- Dqz[iq,1]^(1/(1-q));
      }
      div <- Dqz[iq,1]
    }
    
  }
  
  if(sign_sens) {
    div1 <- max(x) - min(x)
    div2 <- abs(abs(max(x)) - abs(min(x)))
    div <- (div1 - div2) / div1
  }
  
  names(div) <- paste(sign_sens)
  
  div
  
}

# function for standardising range 0-1
range01 <- function(x) {
  (x-min(x))/(max(x)-min(x))} 


lakecolors <- read_rds("/home/sophie/Dokumente/Master Thesis R/Master Thesis Analysis/lakecolors.rds")

levels(lakecolors$Species)
lakeColors <- setNames(c(
  "#9239F6", "#903495", "#6F3460", "#4A354F", "#D20076", "#FF0076", "#FF4373", 
  "#FF6B58", "#F8B660", "#83DDE0", "#28ADA8", "#3F86BC", "#7A3A9A", "#392682", 
  "#296656", "#569874", "#7EC488", "#76BDCF", "#CE75AD", "#35274A", "#3794BF", 
  "#637394", "#8F526A", "#BC313F", "#E2110F", "#AA352D", "#387C6B", "#20DE8B",
  "#79AF8B", "#AF6AE2", "#966BFF", "#3B9AB2", "#78B7C5", "#E1AF00", "#1B9E77", 
  "#D95F02", "#7570B3", "#E6AB02", "#66A61E", "#E78AC3", "#A6761D", "#666666", 
  "#5BBCD6", "#1B5E20", "#FD6467", "#FAD510", "#FC8D62", "#8DA0CB", "#550307", 
  "#A6D854", "#FFD92F"
), levels(lakecolors$Species))


# colors_walen <- c("#1B9E77", "#D95F02", "#7570B3", "#E7298A", "#66A61E", "#E6AB02", "#A6761D",
#             "#666666", "#FF0000", "#5BBCD6", "#02401B", "#FD6467", "#1d1f54", "#FC8D62",
#             "#8DA0CB", "#E78AC3", "#A6D854", "#FFD92F")

