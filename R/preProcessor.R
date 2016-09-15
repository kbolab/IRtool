#' a class to load and preProcess data
#' 
#' @description  Load and preprocess Data
#'              \itemize{
#'              \item \code{preProcessor() } the costructor
#'              \item \code{load.data.frame( ... ) } Carica il data frame dei referti
#'              \item \code{getData() } restituisce i dati processati
#'              \item \code{apply.filter() } applica un filtro ai referti
#'              }
#' @import stringr tm
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
        browser()
        posizioni <- which( lunghezze>param$number.of.words )
        referti.out <<- referti.out[posizioni,]
      }
    }
  }   
  #=================================================================================
  # getData
  #=================================================================================   
  getData<-function( ) {
    return(  list(
      "referti.out"=referti.out,
      "referti.in"=referti.in
    )   )
  }  
  #=================================================================================
  # costructor
  #=================================================================================  
  costructor<-function() {
    referti.in<<-c()
    referti.out<<-c()
    colonnaTesto<<-c()
  }
  costructor();
  #================================================================================= 
  return(list(
    "load.data.frame"=load.data.frame,
    "getData"=getData,
    "apply.filter"=apply.filter
  ))
}