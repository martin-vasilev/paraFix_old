get_blinks<- function(list_asc = "data/raw/files.txt", maxtrial=24){
  
  trial_info<- function(file, maxtrial, data){ # extracts information for processing trials
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
      message(paste(" Diplicated trial", duplicated, "for file:", data, "\n"))
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
  
  asc<- readLines(list_asc, warn= F)
  
  # file set-up:
  #sink("blinks.txt")
  #cat(paste("sub", "\t", "item", "\t", "cond", "\t", "nblinks"), file="blinks.txt", append=TRUE)
  #cat("\n", file="blinks.txt", append=TRUE)
  
  blinks<- NULL
  temp<- data.frame(sub=NA, item=NA, cond=NA, nblinks=NA, nfix=NA)
 
  for(i in 1:length(asc)){ # for each subject..
    
    cat("\n"); cat(sprintf("Loading data file: %s", asc[i]))
    dataF<- readLines(asc[i]) # load asc file;
    cat(". Done"); cat("\n")
    trial_db<- suppressMessages(trial_info(dataF, maxtrial, asc[i])) # get info about trials)
    
    ntrials<- nrow(trial_db)
    cat(sprintf("Processing trial: "));
    
    for(j in 1:ntrials){
      cat(toString(j)); cat(" ")
      nfix<-NA
      nblinks<-NA
      # sub:
      #cat(toString(i), file="blinks.txt", append=TRUE)
      #cat("\t", file="blinks.txt", append=TRUE)
      
      # item:
      #cat(toString(trial_db$item[j]), file="blinks.txt", append=TRUE)
      #cat("\t", file="blinks.txt", append=TRUE)
      
      # cond:
      #cat(toString(trial_db$cond[j]), file="blinks.txt", append=TRUE)
      
      
      #-------------------#
      # number of blinks: #
      #-------------------#
      get_FIX_stamp<- function(string){as.numeric(substr(string, 1, unlist(gregexpr(pattern ='\t', string))[1]-1))}
      #get_num<- function(string){as.numeric(unlist(gsub("[^0-9]", "", unlist(string)), ""))}
      
      db<- trial_db[j,]
      #start<- get_num(dataF[db$start])
      #end<- get_num(dataF[db$end])
      
      # find blinks:
      blink_stamp<- which(grepl('EBLINK', dataF[db$start:db$end]))
      #blink_time<- get_FIX_stamp(dataF[blink_stamp+ db$start])-1
      #out<- which(blink_time<start)
      
      nblinks= length(blink_stamp)
      #cat(nblinks, file="blinks.txt", append=TRUE)
      
      # new line:
      #cat("\n", file="blinks.txt", append=TRUE)
      
      
      SFIX_stamps<- which(grepl('SFIX', dataF[db$start:db$end]))
      EFIX_stamps<- which(grepl('EFIX', dataF[db$start:db$end]))
      
      if(length(SFIX_stamps)>0 & length(EFIX_stamps)>0){
        if(EFIX_stamps[1]<SFIX_stamps[1]){ # removes fixation that triggered gaze box
          EFIX_stamps<- EFIX_stamps[-1]
        }
        
        if(EFIX_stamps[length(EFIX_stamps)]< SFIX_stamps[length(SFIX_stamps)]){
          SFIX_stamps<- SFIX_stamps[-length(SFIX_stamps)]
        } # fixation was not terminated before the end of trial
        
        # get start and end time of fixations:
        s_time<- get_FIX_stamp(dataF[SFIX_stamps+ db$start])  # start time of fixation
        e_time<- get_FIX_stamp(dataF[EFIX_stamps+ db$start-2]) # end time of fixation 
        
      }
            
      nfix<- length(s_time)
      
      #dataframe:
      temp$sub<- i
      temp$item<- trial_db$item[j]
      temp$cond<- trial_db$cond[j]
      temp$nblinks<- nblinks
      temp$nfix<- nfix
      
      blinks<- rbind(blinks, temp)
    }
    cat("\n")
    
  } # end of subject loop
  #sink()
  #file.show("blinks.txt")
  
  message(paste(round((sum(blinks$nblinks)/ sum(blinks$nfix))*100,3), " % of blinks in the data", sep=""))
  
  return(blinks)
  
}
