
plot_fix<- function(coords, raw_fix_temp, i, j){
  
  remap_letters<- function(letter, y){ # adjusted y position for easier reading
    letter<- as.character(letter)
    ascenders<- c("b", "d", "f", "h", "i", "k", "l", "t")
    descenders<- c("g", "j", "p", "q", "y")
    punct<- c(",", ".")
    caps<- which(grepl("[A-Z]",letter))
    
    which_asc<- which(is.element(letter, ascenders))
    which_desc<- which(is.element(letter, descenders))
    which_punct<- which(is.element(letter, punct))
    
    y[which_desc]<- y[which_desc]- 2
    y[which_asc]<- y[which_asc]+1
    y[which_punct]<- y[which_punct]-4
    y[caps]<- y[caps]+1
    return(y)
  }
  
  # create new directory for images:
  mainDir <- getwd()
  imgDir <- "img"
  subDir<- paste("s", i, sep= "")
  
  if (!file.exists(imgDir)){
    dir.create(file.path(mainDir, imgDir), showWarnings = FALSE)
  }
  
  if (!file.exists(paste(imgDir, "/", subDir, sep=""))){
    dir.create(file.path(imgDir, subDir), showWarnings = FALSE)
  } 
  
  # create output string
  output= paste(imgDir, "/", subDir, "/", "Sub_", i, "_", "item_", j, ".png", sep="") 
  
  
  # open file for plotting:
  png(filename = output, width = ResX, height = ResY, 
      units = "px", pointsize = 12, bg="white", res = 100)          
  par(mar=c(0,0,0,0)) # set plot margins to 0
  
  
  # create empty plot:
  plot(NA, axes=F, main="", xlab="" ,ylab="", col="black", ylim=c(0, ResY), xlim=c(0, ResX), xaxs="i", yaxs="i")
  
  # convert coordinates to numbers:
  coords$y1<- as.numeric(as.character(coords$y1))
  coords$y2<- as.numeric(as.character(coords$y2))
  
  coords$x1<- as.numeric(as.character(coords$x1))
  coords$x2<- as.numeric(as.character(coords$x2))
  
  # Plot stimuli that appeared on screen:
  rect(coords$x1, ResY-coords$y1, coords$x2, ResY-coords$y2, col= "white", border= "#CFCBCB")
  
  xpos= coords$x1+ (coords$x2- coords$x1)/2
  ypos= ResY-coords$y1- (coords$y2- coords$y1)/2
  
  # correct y pos of some letter for better readibility:
  ypos<- remap_letters(coords$letter, ypos)
  
  # print letters:
  text(xpos, ypos, coords$letter, col= "black")
  
  #symbols(raw_fix$xPos, raw_fix$yPos, circles=rep(0.1, nrow(raw_fix)))
  
  add.alpha <- function(col, alpha=1){
    if(missing(col))
      stop("Please provide a vector of colours.")
    apply(sapply(col, col2rgb)/255, 2, 
          function(x) 
            rgb(x[1], x[2], x[3], alpha=alpha))  
  }
  
  
  # add saccades
 # for (k in 1:nrow(raw_fix_temp)){
 #   if(k==1){ next}
 #   x1<- raw_fix_temp$xPos[k-1]+6
 #   y1<- ResY-raw_fix_temp$yPos[k-1]
    
 #   if(x2>x1){
 #     x2<- raw_fix_temp$xPos[k]-10
 #   } else{
  #    x2<- raw_fix_temp$xPos[k]+10
 #   }
    
 #   y2<- ResY-raw_fix_temp$yPos[k]
    
   # fixPosX<- x1+ (x2-x1)/2
   # fixPosY<- y1+ (y2-y1)/2
    
#    arrows(x1, y1, x2, y2, col = add.alpha("blue",0.2), lty= 1, lwd=2, length=0.10)
    
    #text(fixPosX, fixPosY, raw_fix_temp$fix_num, col= add.alpha("blue",0.3))
 # }
  first<- subset(raw_fix_temp, intrasent_regr==0 & intersent_regr==0)
  second<- subset(raw_fix_temp, intrasent_regr==1 | intersent_regr==1)
  
  # first-pass:
  points(x= first$xPos, y=ResY-first$yPos, pch = 16,  col= add.alpha("green",0.20),
         cex= 0.7*(first$fix_dur/75))
  points(x= first$xPos, y=ResY-first$yPos, pch = 16, cex=0.7, col="green")
  
  # second-pass:
  points(x= second$xPos, y=ResY-second$yPos, pch = 16,  col= add.alpha("orange",0.20),
         cex= 0.7*(second$fix_dur/75))
  points(x= second$xPos, y=ResY-second$yPos, pch = 16, cex=0.7, col="orange")
  
  # fixation numbers:
  text(raw_fix_temp$xPos, ResY-raw_fix_temp$yPos+15, raw_fix_temp$fix_num, col= add.alpha("red",0.6))

  dev.off() # close file
  
} # end of function


