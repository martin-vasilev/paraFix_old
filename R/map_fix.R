
# Martin R. Vasilev, 2016

# V0.1

## The script may seem a bit chaotic because of the large number of additional functions. 
## The reason for this approach is that processing is faster compared to using for loops.


#########
# SETUP #
#########

data<- readLines("data/raw/files.txt")
#data<- readLines("//bournemouth.ac.uk/data/staff/home/mvasilev/Profile/Desktop/Adam/data.txt")

ResX<- 1024
ResY<- 768

#ResX<- 1920
#ResY<- 1080

maxtrial<- 24
#maxtrial<- 3

#filename<- "TEST.asc"
#filename<- "//bournemouth.ac.uk/data/staff/home/mvasilev/Profile/Desktop/Cdiff_data/CD1.asc"

#########################################################################################
#                 LOAD UP REQUIRED FUNCTIONS                                            #
#########################################################################################

get_text<- function(file){ ## extracts the loaded text material                         #
                                                                                     
  start<- which(grepl("DISPLAY TEXT", file))+1 # start point
  end<- which(grepl("START", file))
  
  text<- file[start:end] # subset file
  a= which(gregexpr(pattern ='BUTTON', text)==1)
  
  if(length(a)>0){
    text<- text[-a]
    text<- text[-a] # because one line is removed
  }
  
  input<- which(grepl("INPUT", text)); input<- input[1]-1 # end point
  text<- text[1:input]
  
  #end<- which(grepl("INPUT", file)); end<- end[1]-1 # end point
  #text<- file[start:end] # subset file
  return(text)
}


get_coord<- function(string){ # extracts text coordinates from trial info
  
  # check to make sure there are no button press flags here..
  a= which(gregexpr(pattern ='BUTTON', string)==1)
  
  if(length(a)>0){
    string<- string[-a]
  }

  #if(which(grepl('   ', string))==1){
  #  loc<- as.numeric(gregexpr("CHAR", string)) 
  #  part1<- substr(string, 1, loc+7)
  #  part2<- substr(string, loc+9, nchar(string))
  #  string2<- paste(part1, part2, sep="")
  #}
  #else{
    #out <- data.frame( do.call( rbind, strsplit( string, ' ' ) ) ) 
    out <-  do.call( rbind, strsplit( string, '\t' ) )  
    out<- out[,2]
    out <-  data.frame(do.call( rbind, strsplit( out, ' ' ) ) )
  #}
  
  out<- subset(out, X2!="DELAY") # Remove DELAY 1ms lines
  
  out$X7<- as.numeric(as.character(out$X7))
  out$X8<- as.numeric(as.character(out$X8))
  out$X9<- as.numeric(as.character(out$X9))
  out$X10<- as.numeric(as.character(out$X10))
  out$X11<- as.numeric(as.character(out$X11))
  
  fix_spaces<- function(out){
    out$space<- NULL
    a<- which(out$X6=="") # find position of empty spaces
    out$space<- NA
    out$space[a]<- 1
    # Re-arrange misplaced coords
    out$X7[a]<- out$X8[a]
    out$X8[a]<- out$X9[a]
    out$X9[a]<- out$X10[a]
    out$X10[a]<- out$X11[a]
    out<- out[,-11]
  }
  
  out<- fix_spaces(out)

  
 # clean_MSG<- function(msg){as.numeric(unlist(gsub("[^0-9]", "", unlist(msg)), ""))}
 # out$X1<- clean_MSG(out$X1) # get rid of MSG text
  out<- out[,-c(1,2,3,5)] # remove useless columns
  
  # map sentences:
  map_sent<- function(out){
    sent_bnd<-  which(out$X6=="."| out$X6=="?");
    sent<- NULL
    for(i in 1:length(sent_bnd)){
      if(i==1){
        sent[1:sent_bnd[i]]<- 1
      }
       else{
         sent<- c(sent, c(rep(i, length(sent_bnd[i-1]+1:sent_bnd[i])- length(sent))))
      #  sent[sent_bnd[i-1]+1:sent_bnd[i]]<- i
       }
      if(i==length(sent_bnd)){
        sent<- c(sent, c(rep(i+1, length(sent_bnd[i]+1:nrow(out))- length(sent))))
      }
    }
    out$sent<- sent
    return(out)
  }

  out<- map_sent(out)
  
  # map lines:
  map_line<- function(out){
    line<- NULL
    lines<- sort(unique(out$X8));
   # as.numeric.factor <- function(x) {as.numeric(levels(x))[x]}
    #lines<- as.numeric.factor(lines)
    
    for(i in 1:length(lines)){
      loc_lines<- which(out$X8==lines[i])
      line<- c(line, rep(i, length(loc_lines)))
      out$space[length(line)]<- 2
    }
    out$line<- line
    
    return(out)
  }
  
  out<- map_line(out)
  
  
  # map words:
  map_words<- function(out){
    
    out$word<- NULL
    curr_sent=1
    curr_word=1
    for(i in 1:nrow(out)){
      
      newSent<- curr_sent!= out$sent[i]
      
      if(newSent){
        curr_sent<- curr_sent+1
        curr_word<- 1
      }
      
      out$word[i]<- curr_word
      if(out$X6[i]== ""& !newSent){
        curr_word<- curr_word+1
        out$word[i]<- curr_word
      }
      
    }
    
    return(out)
  }
  
  out<- map_words(out)
  
  # change column names:
  colnames(out)<- c("char", "letter", "x1", "y1", "x2", "y2", "space", "sent",
                    "line", "word")
  
  return(out)
}

# Uses stimuli information to create a coordinate map for each pixel on the screen
coord_map<- function(coords, x=ResX, y= ResY){ 
  #as.numeric.factor <- function(x) {as.numeric(levels(x))[x]}
  #coords$x1<- as.numeric.factor(coords$x1); coords$x2<- as.numeric.factor(coords$x2)
  #coords$y1<- as.numeric.factor(coords$y1); coords$y2<- as.numeric.factor(coords$y2)
  coords$id<- 1:nrow(coords)
  
  map<- data.frame(matrix(NA, nrow = y, ncol = x))
  
  for(i in 1:nrow(coords)){
    map[coords$y1[i]:coords$y2[i],coords$x1[i]:coords$x2[i]]<- coords$id[i]
    
  }
  
  return(map)
}

####################################################

trial_info<- function(file, maxtrial){ # extracts information for processing trials
  ### get trial names:
  ID<- which(grepl('TRIALID', file)); 
  trial_text<- file[ID]
  trials<- substr(trial_text, unlist(gregexpr(pattern =' ',trial_text[1]))[2]+1, nchar(trial_text))
  #trials<- trials[which(letter!="P" &  letter!="F")] # remove practice items and questions
  trials<- gsub(" ", "", trials)
  # sometimes there is an extra empty space that can mess up detection of duplicates
  
  ### get condition:
  I<- unlist(gregexpr(pattern ='I',trials)) # start of item info
  cond<- as.numeric(substr(trials, 2, I-1)) # extract condition number
  
  ### get item: 
  D<- unlist(gregexpr(pattern ='D',trials)) # start of dependent info
  item<- as.numeric(substr(trials, I+1, D-1)) # extract condition number
  depend<- as.numeric(substr(trials, nchar(trials), nchar(trials)))
  
  ### get sequence:
  #seq<- 1:length(trials)
  
  ### get start & end times
  start<- which(grepl('DISPLAY ON', file))
  end <- which(grepl('DISPLAY OFF', file))
  
  duplicated<- trials[duplicated(trials)]
  
  if(length(duplicated)>0){ # if there were aborted trials..
    message(paste(" Diplicated trial", duplicated, "for file:", data[i], "\n"))
    message("Analysing only last attempt at the trial!")
    
    toBeRemoved<- NULL
    
    for(i in 1:length(duplicated)){
      dup_rem<- which(trials==duplicated[i])
      
      for(j in 1:length(dup_rem)){
        if(j!=length(dup_rem)){
          toBeRemoved[length(toBeRemoved)+1]= dup_rem[j]
        }
      } # end of j
    } # end of i
    
    #start<- start[-toBeRemoved]
   # end<- end[-toBeRemoved]
   cond<- cond[-toBeRemoved]
   item<- item[-toBeRemoved]
  # seq<- seq[-toBeRemoved]
   depend<- depend[-toBeRemoved]
   ID<- ID[-toBeRemoved]
  } # end of aborted conditional
  
  trial_db<- data.frame(cond, item, depend, start, end, ID)
  trial_db<- subset(trial_db, depend==0 & item< maxtrial+1)
  trial_db$seq<- 1:nrow(trial_db)

  ###
  
  
 # trials<- trials[which(!is.element(trials, duplicated))]
  
  return(trial_db)
}


parse_fix<- function(file, map, coords, trial_db, i){
  
  get_FIX_stamp<- function(string){as.numeric(substr(string, 1, unlist(gregexpr(pattern ='\t', string))[1]-1))}
  
  loc_blinks<- function(blink_time, s_time){findInterval(blink_time, s_time)+1 }
  # +1 because it affects the next fixation
  
  get_x_pixel<- function(string){
    tab_loc<- unlist(gregexpr(pattern ='\t', string))[3:4]
    sub_str<- substr(string, tab_loc[1], tab_loc[2])
    x<- as.numeric(unlist(gsub("[^0-9.]", "", unlist(sub_str)), ""))
    x<- floor(x)
    
    return(x)
  }
  
  get_y_pixel<- function(string){
    tab_loc<- unlist(gregexpr(pattern ='\t', string))[4:5]
    sub_str<- substr(string, tab_loc[1], tab_loc[2])
    y<- as.numeric(unlist(gsub("[^0-9.]", "", unlist(sub_str)), ""))
    y<- floor(y)
    
    return(y)
  }
  
 # trial_db<- trial_info(file)
  
  
 # for(i in 1:nrow(trial_db)){
    # get position of fixation stamps:
    SFIX_stamps<- which(grepl('SFIX', file[trial_db$start:trial_db$end]))
    EFIX_stamps<- which(grepl('EFIX', file[trial_db$start:trial_db$end]))
    
    if(length(SFIX_stamps)==0 | length(EFIX_stamps)==0){
      # means that there was no complete fixation on this trial (i.e,
      # participant likely pressed end button by mistake)
      raw_fix<- NULL
      return(raw_fix)
      message(sprintf("No fixations in Trial %d: excluded", j))
    }
    
    if(EFIX_stamps[1]<SFIX_stamps[1]){ # removes fixation that triggered gaze box
      EFIX_stamps<- EFIX_stamps[-1]
    }
    
    if(EFIX_stamps[length(EFIX_stamps)]< SFIX_stamps[length(SFIX_stamps)]){
      SFIX_stamps<- SFIX_stamps[-length(SFIX_stamps)]
    } # fixation was not terminated before the end of trial
    
    # get start and end time of fixations:
    s_time<- get_FIX_stamp(file[SFIX_stamps+ trial_db$start])  # start time of fixation
    e_time<- get_FIX_stamp(file[EFIX_stamps+ trial_db$start-2]) # end time of fixation 
    
    # calculate fixation durations:
    fixDur<- e_time- s_time
    
    # get x pixel position:
    x<- get_x_pixel(file[EFIX_stamps+ trial_db$start-1])
    
    # get y pixel position:
    y<- get_y_pixel(file[EFIX_stamps+ trial_db$start-1])
    
    # find blinks:
    blink_stamp<- which(grepl('EBLINK', file[trial_db$start:trial_db$end]))
    blink_time<- get_FIX_stamp(file[blink_stamp+ trial_db$start])-1
    blink_out<- which(blink_time<s_time[1]| blink_time>e_time[length(e_time)])
    
    if(length(blink_out>0)){ # blinks outside time window that is analysed
      blink_time<- blink_time[-blink_out]  # remove them
    }
    
    blink_pos<- loc_blinks(blink_time, s_time)
    blink<- rep(0, length(s_time))
    blink[blink_pos]<-1
    
    #if(length(blink)>length(e_time)){
    #  blink<- blink[-length(blink)]
    #}
    
    # merge into a dataframe:
    fix<- data.frame(s_time, e_time, fixDur, x, y, blink)
    
    #-----------------------------------------------#
    #    map fixations to stimuli on the screen:    #
    #-----------------------------------------------#
    
    loc<- NULL; raw_fix<- NULL; temp<- NULL; sub<- NULL
    s_time<- NULL; e_time<- NULL; xPos<- NULL; yPos<- NULL
    item<- NULL; cond<- NULL; seq<- NULL; fix_num<- NULL; fix_dur<- NULL
    sent<- NULL; line<- NULL; word<- NULL; char_sent<- NULL
    max_sent<- NULL; max_word<- NULL; intersent_regr<- NULL
    intrasent_regr<- NULL; blink<- NULL; outOfBnds<- NULL; outsideText<- NULL
    
    # max word for each sentence:
    curr_sent<- matrix(0, max(coords$sent),2)
    curr_sent[,1]<- c(1:max(coords$sent))
    
    for(j in 1:nrow(fix)){
      
      loc<- map[fix$y[j], fix$x[j]] # locate fixation
      
      if(length(loc)==0){
        outsideText[j]<- 1
      } else{
        outsideText[j]<- 0
      }
      
      # general info:
      sub[j]<- i
      item[j]<- trial_db$item
      cond[j]<- trial_db$cond
      seq[j]<- trial_db$seq
      fix_num[j]<- j
      fix_dur[j]<- fix$fixDur[j]
      
      # info from asc file:
      s_time[j]<- fix$s_time[j];
      e_time[j]<- fix$e_time[j]
      xPos[j]<- fix$x[j];
      yPos[j]<- fix$y[j]
      
      if(xPos[j]<1 | xPos[j]> ResX | yPos[j]< 1 | yPos[j]>ResY){ # fixation is out of bounds
        outOfBnds[j]<- 1
      } else{
        outOfBnds[j]<- 0
      }
      
      # stimuli info:
      if(!is.null(loc)){
        sent[j]<- coords$sent[loc]
        line[j]<- coords$line[loc]
        word[j]<- coords$word[loc]
        char_sent[j]<- as.numeric(as.character(levels(coords$char[loc])[coords$char[loc]]))+1
        # +1 bc Eyetrack counts from 0
      } else{
        sent[j]<- NA; line[j]<- NA; word[j]<- NA; char_sent[j]<- NA
      }

      # saccade stuff:
      if(j==1){
        max_sent[j]<- sent[j]
      } else{
        max_sent[j]<- max_sent[j-1]
        
        if(!is.na(max_sent[j])& !is.na(sent[j]) & sent[j]> max_sent[j]){
          max_sent[j]<- sent[j]
        }
      }
      
      # maximum word:
      if(j==1){
        max_word[j]<- abs(word[j])
        curr_sent[sent[j],2]<- abs(word[j])
      } else{
        max_word[j]<- curr_sent[sent[j],2]
        if(!is.na(word[j])& abs(word[j])>curr_sent[sent[j],2]){
          max_word[j]<- abs(word[j])
          curr_sent[sent[j],2]<- abs(word[j]) # replace new max word
        }
      }
      
      # Regression stuff:
      if(!is.na(max_sent[j])& !is.na(sent[j]) & sent[j]< max_sent[j]){
        intersent_regr[j]<- 1
      } else{
        intersent_regr[j]<- 0
      }
      
      # intra-sentence regressions:
      if(!is.na(word[j])& abs(word[j])<max_word[j]){
        intrasent_regr[j]<- 1
      }else{
        intrasent_regr[j]<- 0
      }
      
      blink[j]<- fix$blink[j]
      
    } # end of j loop
    
    raw_fix<- data.frame(sub,item, cond, seq, s_time, e_time,xPos, yPos, fix_num, fix_dur,
                         sent, max_sent, line, word, max_word, char_sent, intrasent_regr, intersent_regr, blink,
                         outOfBnds, outsideText)
    # remove last fixation on trial (due to participants' making a decision to press the button)
    raw_fix<- raw_fix[-nrow(raw_fix),]
    
    #raw_fix<- rbind(raw_fix, temp)
    
    #rm(item, cond, seq, s_time, e_time,xPos, yPos, fix_num, fix_dur,
    #   sent, max_sent, line, word, max_word, char_sent, intrasent_regr, intersent_regr, blink)
    
 # } # end of i loop
  
  return(raw_fix)
}



###################################################################################################
#                                         Process data here                                       #
###################################################################################################
#

source("plot_fix.R")

raw_fix<- NULL

for (i in 1:length(data)){ # for each subject..
#  i=1; # temporary
 
  cat(sprintf("Processing subject %i", i)); cat("\n")  
  cat(sprintf("Loading data %s ...", data[i])); 
  file<- readLines(data[i]) # load file
  cat(" Done"); cat("\n") 
  trial_db<- trial_info(file, maxtrial) # extract info about trials to be processed
  cat("Trial... ")
  
  for(j in 1:nrow(trial_db)){ # for each item
    
    text<- get_text(file[trial_db$ID[j]:trial_db$start[j]])
    coords<- get_coord(text)
    map<- coord_map(coords, x=ResX, y= ResY)
    raw_fix_temp<- parse_fix(file, map, coords, trial_db[j,], i)
    
    
    raw_fix<- rbind(raw_fix, raw_fix_temp) # plot it
    

  if(length(raw_fix_temp)>1){ # if data was extracted from trial  
  # create picture of fixations:
    plot_fix(coords, raw_fix_temp, i, j)
  }
    cat(toString(j)); cat(" ")
  } # end of item loop
  
  cat("\n DONE \n \n"); 
} # end of subject loop


#text<- get_text(file)
#coords<- get_coord(text)
#map<- coord_map(coords, x=ResX, y= ResY)

#raw_fix<- parse_fix(file, map, coords)

cat("Saving data."); 
save(raw_fix, file= "data/raw_fix.Rda"); cat(".")
write.csv(raw_fix, file= "data/raw_fix.csv"); cat(".")
cat("\n \n All Done!"); 


#fileConn<-file("Fixation_log.txt")
#writeLines(c("Hello","World"), fileConn)
#close(fileConn)
