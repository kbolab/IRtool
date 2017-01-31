#' a class to load and preProcess data
#' 
#' @description  Load and preprocess Data
#'              \itemize{
#'              \item \code{preProcessor() } the costructor
#'              \item \code{load.data.frame( ... ) } Carica il data frame dei referti
#'              \item \code{getData() } restituisce i dati processati
#'              \item \code{apply.filter() } applica un filtro ai referti
#'              }
#' @import stringr tm stringi SnowballC 
#' @export
#' @examples \dontrun{
#' # -----------------------------------------------
#' #  USING THE METHODS of the class
#' # -----------------------------------------------
#' obj.pp <- preProcessor();  
#' 
#' obj.pp$load.data.frame( referti, colonnaTestoReferto="descrizione" )
#' 
#' # Rimuovi i documenti con meno di 10 parole 
#' obj.pp$apply.filter( filter="remove.little.document", param=list("number.of.words" = 10 )  )
#' 
#' # Converti tutti i caratteri in minuscolo
#'  obj.pp$apply.filter(filter="convert.to.lower")
#'  
#'  # Escludi caratteri UTF
#'  obj.pp$apply.filter("remove.nonvalid.UTF8")
#'  
#' 
#' #prendi il risultato
#' aaa <- obj.pp$getData()
#' 
#' # e vediamo che dal numero di documenti inziale
#' print(aaa$referti.in)
#' 
#' # il numero dopo il filtro è effettivamente calato
#' print(aaa$referti.in)
#' }
preProcessor<-function() {
  referti.in<-c()
  referti.out<-c()
  colonnaTesto<-c()
  array.occor<-c()
  stop.char<-c()
  stop.words<-c()
  
  #=================================================================================
  # load.data.frame
  #=================================================================================    
  load.data.frame<-function( data.frame.referti, colonnaTestoReferto="descrizione") {
    referti.in <<- data.frame.referti
    referti.out <<- data.frame.referti
    colonnaTesto <<- colonnaTestoReferto
  }
  #=================================================================================
  # run
  #=================================================================================   
  apply.filter<-function( filter, param="") {
    if(filter=="remove.little.document") {
      for(indice in nrow(referti.out)) {
        lunghezze <-  lapply (referti.out[[colonnaTesto]],  function(x) length(str_split(string = x,pattern = " ")[[1]]))
        lunghezze <- unlist( lunghezze )
        posizioni <- which( lunghezze>param$number.of.words )
        referti.out <<- referti.out[posizioni,]
      }
    }
    if(filter=="convert.to.lower"){
      minuscole <- lapply(X = referti.out[[colonnaTesto]], function(x) str_to_lower(string = x) )
      minuscole <- unlist(minuscole)
      referti.out[[colonnaTesto]] <<- minuscole
    }
    
    if (filter== "remove.nonvalid.UTF8"){
      caratteri <- lapply(X = referti.out[[colonnaTesto]], function(x) iconv(x, "UTF-8", "UTF-8", sub='') )
      caratteri <- unlist(caratteri)
      referti.out[[colonnaTesto]] <<- caratteri
    }
    
    if (filter== "remove.stopwords"){
      referti.out[[colonnaTesto]] <<- lapply(X = referti.out[[colonnaTesto]], function(x) x<-stri_replace_all_fixed(str = x,replacement = " ",stop.char,vectorize_all = FALSE) )
      referti.out[[colonnaTesto]] <<- lapply(X = referti.out[[colonnaTesto]], function(x) { uppa<- str_split(string = x,pattern = c(" "))[[1]]; x<-str_c(uppa[!(uppa %in% stop.words)],collapse = " ")  }  )
    }

    
    if (filter== "remove.numbers"){
      referti.out[[colonnaTesto]] <<- lapply(X = referti.out[[colonnaTesto]], function(x) gsub("\\d", "", x) )
    }
    
    if(filter== "remove.shortWord"){
      referti.out[[colonnaTesto]] <<- lapply(X = referti.out[[colonnaTesto]], function(x) gsub('\\b\\w{1,2}\\b','',x))
    }
    
    if(filter== "stemming"){
      for (i in 1:nrow(referti.out)){
        stringa <- str_split(referti.out[[colonnaTesto]][i], pattern = " ")
        stringa2 <- unlist(stringa)
        radice <- wordStem(words = stringa2, language = 'italian')
        referti.out[[colonnaTesto]][i] <<- paste(radice, sep=" ", collapse=" ")
      }
    }
  }
  #=================================================================================
  # get.td.idf
  #=================================================================================   
  get.tf.idf<-function( ) {
    aaa <- getData()
    tf <- list()
    for(i in seq(1,length(aaa$referti.out[[1]])))  {
      freq.parole <- table(  str_split( string = paste( referti.out[[colonnaTesto]][i] ,collapse  = " "),pattern = " ")   )
      term.freq <- freq.parole[ !(names(freq.parole) %in% c(stop.words,"")) ]
      term.freq <- term.freq/sum(term.freq)
      tf[[i]]<-term.freq
    }
    # calcola il tdf
    
    idf <- table(names(unlist(tf)))
    idf <- 1+log(idf)
    
    tf.idf<-tf
    # for(i in seq(1,length(tf))) {
    #   for(parola in names(tf[[i]] )) {
    #     tf.idf[[i]][parola] <- tf[[i]][parola]* idf[parola]
    #   } 
    # }

    return(list(
      "tf"=tf,
      "idf"=idf,
      "tf.idf"=tf.idf
    ))
  }  
  # a2 <- table(  str_split( string = paste(aa$referti.out$descrizione ,collapse  = " "),pattern = " ")   )
  #=================================================================================
  # getData
  #=================================================================================   
  getData<-function( array.occor.thrs = 5 , keep.old = FALSE) {
    if(keep.old == FALSE ) {
      array.occor <<- table(  str_split( string = paste( referti.out[[colonnaTesto]] ,collapse  = " "),pattern = " ")   )
      array.occor <<- array.occor[which(array.occor > array.occor.thrs)]
      array.occor <- array.occor[ !(names(array.occor) %in% c(stop.words,"") ) ]
    }
    return(  list(
      "referti.out"=referti.out,
      "referti.in"=referti.in,
      "array.occor"=array.occor
    )   )
  }  
  #=================================================================================
  # costructor
  #=================================================================================  
  costructor<-function() {
    referti.in<<-c()
    referti.out<<-c()
    colonnaTesto<<-c()
    array.occor<<-c()
    # stop.char<<-c(".",";",":","'","\"",",","?","!","\n","\r","(",")","[","]")
    stop.char<<-c(".",";",":","'","\"",",","?","!","\n","\r","(",")","[","]","¡","¿","·","^","%","~","+","/","&",
                  ">","<","-","_","=","`","|","@","*","\\","$","}")
    stop.words<<-c("a","e","i","o","il","lo","la","gli","le","di","da","in","su","per","tra","fra","d","un","uno",
                   "una","degli","delle","dei","l","si", "al","ev","x","cc","cm","che","co","in","del",
                   "ed","con","aa","ab")
    
  }
  costructor();
  #================================================================================= 
  return(list(
    "load.data.frame"=load.data.frame,
    "getData"=getData,
    "apply.filter"=apply.filter,
    "get.tf.idf"=get.tf.idf
  ))
}