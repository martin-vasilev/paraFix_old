
# Martin R Vasilev, 2017

nonfixated<- function(data, list_asc= "data/raw/files.txt",
                      maxtrial=24, bodge=FALSE){
  
  get_text<- function(file){ ## extracts the loaded text material                         #
    
    start<- which(grepl("DISPLAY TEXT", file))+1 # start point
    end<- which(grepl("START", file))
    
    text<- file[start:end] # subset file
    a= which(gregexpr(pattern ='BUTTON', text)==1)
    
    if(length(a)>0){
      text<- text[-a]
      # text<- text[-a] # because one line is removed
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
      
      out$X6<- as.character(out$X6)
      out$word<- NULL
      curr_sent=1
      curr_word=1
      sent_line<- which(out$space==2); sent_line<- sent_line+1

      for(i in 1:nrow(out)){

        newSent<- curr_sent!= out$sent[i]

        out$word[i]<- curr_word
        if(out$X6[i]== ""& !newSent){
          curr_word<- curr_word+1
          out$word[i]<- curr_word
        }


        if(is.element(i, sent_line)){
          if(out$X6[i]!="."){
            curr_word<- curr_word+1
          }
          out$word[i]<- curr_word
        }
        
        if(newSent){
          curr_sent<- curr_sent+1
          curr_word<- 1
          out$word[i]<- curr_word
        }

      }

      return(out)
    }

    out<- map_words(out)

    # change column names:
    colnames(out)<- c("char", "letter", "x1", "y1", "x2", "y2", "space", "sent",
                      "line", "word")

    # map characters per line (for Eye Doctor):
    out$line_char<- NA
    unq_line<- unique(out$line)
    for(i in 1:length(unq_line)){
      line<- which(out$line==unq_line[i])
      out$line_char[line[1]:line[length(line)]]<- 1:length(line)
    }

    return(out)
  }
  
  
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
  
  add<- data[1,]; add[1,]<- NA
  addF<- NULL
  
  for(i in 1:length(unique(data$sub))){ # for each subject..
    
    nitems<- unique(data$item[data$sub==i])# trials that participant saw
    nitems<- sort(nitems)
    
    cat("\n"); cat(sprintf("Loading data file: %s", asc[i]))
    dataF<- readLines(asc[i]) # load asc file;
    cat(". Done"); 
    trial_db<- suppressMessages(trial_info(dataF, maxtrial, asc[i])) # get info about trials)
    
    
    cat("\n"); cat("Processing trial: ")
    for(j in 1:length(nitems)){
      whichDB<- which(trial_db$item== nitems[j])
      text<- get_text(dataF[trial_db$ID[whichDB]:trial_db$start[whichDB]])
      coords<- suppressWarnings(get_coord(text))
      
      if(bodge==TRUE){ # fixes an issue unique to my experiment
        if(nitems[j]==19){
           coords$word[which(coords$char==318)]=11
           coords$word[which(coords$char==319):which(coords$char==372)]= coords$word[which(coords$char==319):which(coords$char==372)]-1
        }
        if(nitems[j]==14){
           coords$word[which(coords$char==261)]=23
           coords$word[which(coords$char==262)]=23
           coords$word[which(coords$char==263):which(coords$char==279)]= coords$word[which(coords$char==263):which(coords$char==279)]-1
        }
      }
      
      
      curr_sent<- matrix(0, max(coords$sent),3)
      curr_sent[,1]<- c(1:max(coords$sent))
      curr_sent[,2]<- 1 # bc sent starts from first word
      max_sent<- NULL
      
      for(k in 1:nrow(curr_sent)){
        max_sent<- max(coords$word[coords$sent==k])
        curr_sent[k,3]<- max_sent
      }
      
      n<- NULL
      set<- NULL
      for(k in 1:nrow(curr_sent)){
        n<- subset(data, sub==i & item==j & sent==k)
        set<- curr_sent[k,2]: curr_sent[k,3]
        not<- which(is.element(set, unique(n$word)))
        not<- set[-not]
        
        if(length(not)>0){
          for(l in 1:length(not)){
            t<- add
            t$sub<- i
            t$item<- nitems[j]
            t$cond<- n$cond[1]
            t$seq<- n$seq[1]
            t$sent<- k
            t$word<- not[l]
            
            addF<- rbind(addF, t)
            
          }# end of l
        }
        
      } # end of k
      
      
      cat(paste(toString(j), " ", sep=''))
    } # end of j
    
  } # end of i

  ## merge dfs:
  data$skipped<- NA
  addF$skipped<- 1
  
  data<- rbind(data, addF)
  
  return(data)
  
} # end of function

