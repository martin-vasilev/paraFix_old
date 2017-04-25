# Martin R. Vasilev, 2017

# I NEED TO ADD NON-FIXATED WORDS on EACH TRIALS

get_fix<- function(data){
  
  sub<- NULL; item<- NULL; seq<- NULL; cond<- NULL; word<- NULL; o<- NULL; p<- NULL
  nitems<- NULL; n<- NULL; p1<- NULL; p2<- NULL; nfix1<- NULL; nfix2<- NULL; nfixAll<- NULL
  dataN<- NULL; dataT<- NULL; q<- NULL; r<- NULL; sent<- NULL
  
  cat("Processing data for subject... ");
  
  for(i in 1:length(unique(data$sub))){ # for each subect..
    
    nitems<- unique(data$item[data$sub==i])# trials that participant saw
    nitems<- sort(nitems)
    cat(paste(i, " ", sep=""));
    
    for(j in 1: length(nitems)){ # for each item of subect i
      
      n<- subset(data, sub==i & item==nitems[j]) # subset data for subect i & item j
      o<- sort(unique(n$sent))
      
      for(k in 1:length(o)){
        q<- subset(n, sent==o[k])
        r<- sort(unique(q$word))
        
        for(l in 1:length(r)){ # for each word in sentence k
          word[l]<- r[l]  
          sub[l]<- n$sub[1]
          item[l]<- n$item[1]
          seq[l]<- n$seq[1]
          cond[l]<- n$cond[1]
          sent[l]<- o[k]
          
          ### Refixation conditional:
          p<- subset(q, word==r[l])
          p1<- subset(p, intrasent_regr==0)
          p2<- subset(p, intrasent_regr==1)
          
          ## code fixations
          nfix1[l]<- nrow(p1)
          nfix2[l]<- nrow(p2)
          nfixAll[l]<- nrow(p1)+ nrow(p2)
          
        } # end of l
        
        dataT<- data.frame(sub, item, word, seq, sent, cond, nfix1, nfix2, nfixAll)
        sub<- NULL; item<- NULL; seq<- NULL; cond<- NULL; word<- NULL; p<- NULL; sent<- NULL
        p1<- NULL; p2<- NULL; nfix1<- NULL; nfix2<- NULL; nfixAll<- NULL; q<- NULL; r<- NULL
        
        dataN<- rbind(dataN, dataT)
        
      } # end of k

    } # end of j
    
  } # end of i
  
  return(dataN)
}