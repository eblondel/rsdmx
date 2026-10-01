#' @name SDMXCodelists
#' @rdname SDMXCodelists
#' @aliases SDMXCodelists,SDMXCodelists-method
#' 
#' @usage
#' SDMXCodelists(xmlObj, namespaces)
#' 
#' @param xmlObj object of class "XMLInternalDocument derived from XML package
#' @param namespaces object of class "data.frame" given the list of namespace URIs
#' @return an object of class "SDMXCodelists"
#' 
#' @seealso \link{readSDMX}
#' @export
#' 
SDMXCodelists <- function(xmlObj, namespaces){
  new("SDMXCodelists",
      SDMX(xmlObj, namespaces),
      codelists = codelists.SDMXCodelists(xmlObj, namespaces)
  )
}

#get list of SDMXCodelist
#=======================
codelists.SDMXCodelists <- function(xmlObj, namespaces){
  
  codelists <- NULL
  
  messageNsString <- "message"
  if(isRegistryInterfaceEnvelope(xmlObj, FALSE)) messageNsString <- "registry"
  messageNs <- findNamespace(namespaces, messageNsString)
  strNs <- findNamespace(namespaces, "structure")
  
  sdmxVersion <- version.SDMXSchema(xmlObj, namespaces)
  VERSION.21 <- sdmxVersion == "2.1"
  
  codelistsXML <- NULL
  if(VERSION.21){
    codelistsXML <- getNodeSet(xmlObj,
                              "//mes:Structures/str:Codelists/str:Codelist",
                              namespaces = c(mes = as.character(messageNs),
                                             str = as.character(strNs)))
  }else{
    codelistsXML <- getNodeSet(xmlObj,
                              "//mes:CodeLists/str:CodeList",
                              namespaces = c(mes = as.character(messageNs),
                                             str = as.character(strNs)))
  }
  if(!is.null(codelistsXML)){
    codelists <- lapply(codelistsXML, SDMXCodelist, namespaces)
  }
  return(codelists)
}


#as.data.frame
#=============
#'@export
as.data.frame.SDMXCodelists <- function(x, ...,
                                       codelistId = NULL,
                                       codelistAgencyID = NULL,
                                       codelistVersion = NULL,
                                       ignore.empty.slots = TRUE){
  codelist <- NULL
  
  if(length(x@codelists) == 0){
    warning("SDMXCodelists object contains no codelists.")
    return(NULL)
  } else if (length(x@codelists) >= 1){
    if(is.null(codelistId) && is.null(codelistAgencyID) && is.null(codelistVersion)){
      if (length(x@codelists) > 1){
        warning("Using first codelist in SDMXCodelists object: \n
                Specify 'codelistId', 'codelistAgencyID' or 'codelistVersion' arguments for a specific codelist")
      }
      codelist <- x@codelists[[1]]
    }else{
      counfcl_count = 0
      for(cl in x@codelists){
        if(
            (is.null(codelistId)       || cl@id == codelistId)             &&
            (is.null(codelistAgencyID) || cl@agencyID == codelistAgencyID) &&
            (is.null(codelistVersion)  || cl@version == codelistVersion)
          ){
          codelist <- cl
          counfcl_count <- counfcl_count + 1
        }
      }
      if(counfcl_count > 1){
        warning("Multiple matching codelists found in SDMXCodelists object. \n
                 Using last matching codelist.")
      } else if (counfcl_count == 0){
        warning("No matching codelists found in SDMXCodelists object.")
        return(NULL)
      }
    }
  } else {
    stop("Unexpected number of codelists in SDMXCodelists object.")
  }
 
  return(as.data.frame(codelist, ignore.empty.slots=ignore.empty.slots))
}

setAs("SDMXCodelists", "data.frame",
      function(from) as.data.frame.SDMXCodelists(from));
