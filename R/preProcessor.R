#' a class to load and preProcess data
#' 
#' @description  Load and preprocess Data
#'              \itemize{
#'              \item \code{preProcessor() } the costructor
#'              \item \code{load.data.frame( ... ) } Carica il data frame dei referti
#'              \item \code{getData() } restituisce i dati processati
#'              \item \code{apply.filter() } applica un filtro ai referti
#'              }
#' @import stringr tm stringi SnowballC progress
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
  
  ##########################################################################
  # create occorrence table
  #########################################################################
  calculate.occor <- function(array.occor.thrs = 5 , keep.old = FALSE){
    array.occor <<- table(  str_split( string = paste( referti.out[[colonnaTesto]] ,collapse  = " "),pattern = " ")   )
    array.occor <<- array.occor[which(array.occor > array.occor.thrs)]
    array.occor <- array.occor[ !(names(array.occor) %in% c(stop.words,"") ) ]
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
    
    if (filter== "find.replace.synonym" ){
      # Calcola la tabella delle occorrenze per ciascun termine
      table.occor <- calculate.occor()
      table.occor <- as.data.frame(table.occor)
      varNames <- as.character(table.occor$Var1)
      LengthvarNames <- length(varNames)
      similarityJaccard <- array(data = c(0), dim = LengthvarNames*LengthvarNames)
      similarityJaccardNrow <- LengthvarNames
      #load virtual similarity C function
      browser()
      dyn.load("/home/kboaria/PROGETTI/InformationRetrieval/IRtool/R/similarityMatrix.so")
      
      similarityMat <- .C(   "similarityMatrix", as.character (varNames), as.integer (LengthvarNames),
                             as.integer(similarityJaccardNrow), as.array(similarityJaccard))
      
      JaccardMatrix <- array (similarityMat[[4]], dim=c(LengthvarNames,LengthvarNames))
      rownames(JaccardMatrix) <- c(varNames)
      colnames(JaccardMatrix) <- c(varNames)
      
      browser()
      
      rownames(similarity.jaccard) <- c(as.character(table.occor$Var1))
      colnames(similarity.jaccard) <- c(as.character(table.occor$Var1))
      # crea la matrice delle similitudini tramite il coefficiente di jaccard

    }
  }
  #=================================================================================
  # get.td.idf
  # min.rel.thres = la frequenza minima relativa per cui un termine deve comparire fra
  # i documenti per non essere scartatao
  #=================================================================================   
  get.tf.idf<-function( min.rel.thres = .001) {
    
    aaa <- getData()
    tf <- list()
    
    numero.documenti <- length(aaa$referti.out[[1]])
    cat("\nDocuments: ",numero.documenti)
    cat("\n Building tf :\n")
    
    # ---------------------------------------------------------------
    # calcola TF: per ciascun referto calcola la frequenza di ogni singolo termine eliminando 
    # gli spazi
    # ---------------------------------------------------------------
    pb <- progress_bar$new(total = numero.documenti)
    for(i in seq(1,numero.documenti))  {
      freq.parole <- table(  str_split( string = paste( referti.out[[colonnaTesto]][i] ,collapse  = " "),pattern = " ")   )
      term.freq <- freq.parole[ !(names(freq.parole) %in% c(stop.words,"")) ]
      term.freq <- term.freq/sum(term.freq)
      tf[[i]]<-term.freq
      pb$tick()
    }
    cat("Done\n")
    
    cat("Building tf :\n")
    
    # ---------------------------------------------------------------
    # calcola IDF:
    # ---------------------------------------------------------------
    idf <- table(names(unlist(tf)))
    cat("terms before pruning: ",length(idf),"  \n")
    
    # togli quelli che non sono abbastanza frequenti (rumore)
    idf <- idf[which(idf>=min.rel.thres*numero.documenti)]  
    
    # passa al logaritmo
    idf <- 1+log(idf)
    cat("terms after pruning: ",length(idf),"  \n")
    cat("Done \n")
    
    # ---------------------------------------------------------------
    # calcola TD-IDF 
    # ---------------------------------------------------------------
    cat("Building tf-idf :\n")
    pb <- progress_bar$new(total = numero.documenti)
    tf.idf<-tf
    # ora Riduci la lista dei tf ai soli che sono restati nell'idf dopo la scrematura
    for(i in seq(1,numero.documenti))  {
      # pulisci il tf lasciando solo quelli che non sono stati filtrati
      tf[[i]]<-tf[[i]][ names(tf[[i]] ) %in% names(idf) ]
      # calcola il prodotto tf-idf
      nuovoArr <- tapply(c(tf[[i]],idf),names(c(tf[[i]],idf)),prod)
      nuovoArr <- nuovoArr[names(nuovoArr) %in% names(tf[[i]])]
      tf.idf[[i]] <- nuovoArr
      pb$tick()
    }
    cat("Done\n")

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
  getData<-function( ) {
    
      array.occor <<- calculate.occor()
    
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