
# Parses data pre-processed in Eye-doctor

parse_da1<- function(list_da1= "C:/Users/Martin Vasilev/Documents/Cdiff_data/data_da1.txt",
                     list_asc= "C:/Users/Martin Vasilev/Documents/Cdiff_data/data_asc.txt",
                     ResX= 1024, ResY= 768, maxtrial=24, bodge=FALSE){
  message(paste("Assuming trial IDs end in", "'", 'D0', "'", sep=''))

  #-----------------------------------------------------------------------------------------#
  #                                  Load up Required functions:                            #
  #-----------------------------------------------------------------------------------------#
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


  #-----------------------------------------------------------------------------------------#
  #                                  Processing starts here:                                #
  #-----------------------------------------------------------------------------------------#

  # read in lists containing data files:
  daF<- readLines(list_da1, warn = F)
  asc<- readLines(list_asc, warn= F)

  if(length(daF)!=length(asc)){stop("Length of .da1 and .asc files does not match!")}

  #-------------------------------------#
  #  Read in da1 data into a dataframe: #
  #-------------------------------------#

  fix<- NULL

  for(i in 1:length(daF)){
    cat("\n") ;cat("Processing data for subject... "); cat(paste(i, " ", sep="")); cat("\n")

    fileDA<- readLines(daF[i], warn=F) # load da1 file
    cat(sprintf("Loading data file: %s", asc[i]))
    dataF<- readLines(asc[i]) # load asc file;
    cat(". Done")
    trial_db<- trial_info(dataF, maxtrial, asc[i]) # get info about trials
    #text<- get_text(file[trial_db$ID[j]:trial_db$start[j]])

    cat("\n"); cat("Mapping fixations for trial: ")

    for(j in 1:length(fileDA)){ # for each item
      string<- fileDA[j]
      da<-data.frame( do.call( rbind, strsplit( string, ' ' ) ) )

      nfix<- as.numeric(as.character(da$X8))
      seq<- as.numeric(as.character(da$X1))
      cond<- as.numeric(as.character(da$X2))
      item<- as.numeric(as.character(da$X3))
      sub<- i

      # find position in asc trial info db:
      whichDB<- which(trial_db$cond== cond & trial_db$item== item)
      # Extract EyeTrack trial text:
      text<- get_text(dataF[trial_db$ID[whichDB]:trial_db$start[whichDB]])
      coords<- suppressWarnings(get_coord(text))
      
      if(bodge==TRUE){ # fixes an issue unique to my experiment
        if(item==19){
           coords$word[which(coords$char==318)]=11
           coords$word[which(coords$char==319):which(coords$char==372)]= coords$word[which(coords$char==319):which(coords$char==372)]-1
        }
        if(item==14){
           coords$word[which(coords$char==261)]=23
           coords$word[which(coords$char==262)]=23
           coords$word[which(coords$char==263):which(coords$char==279)]= coords$word[which(coords$char==263):which(coords$char==279)]-1
        }
      }

      da2<- da[,-c(1:8)]

      fix_dur<- NULL; charX<- NULL; line<- NULL; sacc_len<- NULL
      sent<- NULL; word<- NULL; char_trial<- NULL
      max_sent<- NULL; max_word<- NULL; intersent_regr<- NULL; intrasent_regr<- NULL

      count<- 0

      # max word for each sentence:
      curr_sent<- matrix(0, max(coords$sent),2)
      curr_sent[,1]<- c(1:max(coords$sent))

      for(k in 1:nfix){ # for each fixation

        # info from DA1 file:
        charX[k]<- as.numeric(as.character(da2[,count+1]))+1 # bc Eyetrack counts from 0
        line[k]<- as.numeric(as.character(da2[,count+2]))+1 # bc Eyetrack counts from 0
        fix_dur[k]<- as.numeric(as.character(da2[,count+4]))- as.numeric(as.character(da2[,count+3]))

        ## Eyetrack details:
        rowN<- which(coords$line_char== charX[k] & coords$line== line[k])
        if(length(rowN)==0 & k!=nfix){
          next; # skip iteration
          # issue a warning:
          message(sprintf("Subject %i, item %i, fixation %i is outside of text", i, j, k))
        }
      #  if(length(rowN)==0 & k==nfix){
      #    sub[k]<- NA; cond[k]<- NA; item[k]<- NA; seq[k]<- NA; charX[k]<- NA; char_trial[k]<- NA;
      #    line[k]<- NA; sent[k]<- NA; max_sent[k]<- NA; word[k]<- NA; max_word[k]<- NA;
      #    fix_dur[k]<- NA; intersent_regr[k]<-NA; intrasent_regr[k]<- NA
      #    next; # skip iteration
      #    message(sprintf("Subject %i, item %i, fixation %i is outside of text", i, j, k))
      # }

        sent[k]<- coords$sent[rowN]
        word[k]<- coords$word[rowN]
        char_trial[k]<- as.numeric(as.character(coords$char[rowN]))+1

        count<- count+4

        #---------------
        # saccade stuff:
        #---------------

        # max sentence:
        if(k==1){
          max_sent[k]<- sent[k]
        } else{
          max_sent[k]<- max_sent[k-1]

          if(sent[k]> max_sent[k]){
            max_sent[k]<- sent[k]
          }
        } # end of max sentence

        # maximum word:
        if(k==1){
          max_word[k]<- abs(word[k])
          curr_sent[sent[k],2]<- abs(word[k])
        } else{
          max_word[k]<- curr_sent[sent[k],2]
          if(abs(word[k])>curr_sent[sent[k],2]){
            max_word[k]<- abs(word[k])
            curr_sent[sent[k],2]<- abs(word[k]) # replace new max word
          }
        } # end of max word


        # Regression stuff:
        if(sent[k]< max_sent[k]){
          intersent_regr[k]<- 1
        } else{
          intersent_regr[k]<- 0
        }

        # intra-sentence regressions:
        #if(sent[k]== max_sent[k]){
           if(abs(word[k])<max_word[k]){
              intrasent_regr[k]<- 1
           }else{
              intrasent_regr[k]<- 0
           }
           if(k>1){
             if(word[k]==max_word[k] & max_word[k]==max_word[k-1] & intrasent_regr[k-1]==1){
                intrasent_regr[k]<- 1 # returning to origin of regression (still 2nd-pass)
             }
           }
        #}
        if(intersent_regr[k]==1){
          intrasent_regr[k]<- 0
        }
        
        # saccade length:
        if(k>1){ # no saccade on first fixation
          if(line[k]== line[k-1]){ # not meaningful to calculate length when crossing lines
             sacc_len[k]<- abs(charX[k]- charX[k-1])
          } else{
              sacc_len[k]<- NA
          }
        }


      } # end of k loop

      item<- rep(item, nfix)
      cond<- rep(cond, nfix)
      sub<- rep(sub, nfix)
      seq<- rep(seq, nfix)
      

      fix_temp<- data.frame(sub, cond, item, seq, charX, char_trial, line, sent, max_sent,
                            word, max_word, fix_dur, intersent_regr, intrasent_regr, sacc_len)
      fix<- rbind(fix, fix_temp)
      rm(sub, cond, item, seq, charX, char_trial, line, sent, max_sent, word, max_word, fix_dur,
         intersent_regr, intrasent_regr, sacc_len)

      cat(paste(toString(j), " ", sep=''))

    } # end of j loop



  } # end of i loop

  cat("All done!")

  return(fix)

}

