% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/Class-SDMXRequestParams.R,
%   R/SDMXRequestParams-methods.R
\docType{class}
\name{SDMXRequestParams}
\alias{SDMXRequestParams}
\alias{SDMXRequestParams-class}
\alias{SDMXRequestParams,SDMXRequestParams-method}
\title{Class "SDMXRequestParams"}
\usage{
SDMXRequestParams(regUrl, repoUrl, accessKey,
                   providerId, agencyId, resource, resourceId, version,
                   flowRef, key, start, end, references = NULL, compliant)
}
\arguments{
\item{regUrl}{an object of class "character" giving the base Url of the SDMX service registry}

\item{repoUrl}{an object of class "character" giving the base Url of the SDMX service repository}

\item{accessKey}{an oject of class "character" giving the eventual authentication or subscription
user key (or token) to provide in order to perform the SDMX request. This key may be
mandatory for some service providers.}

\item{providerId}{an object of class "character" giving the provider agency id}

\item{agencyId}{an object of class "character" giving an agency id}

\item{resource}{an object of class "character" giving the type of resource to be queried}

\item{resourceId}{an object of class "character" giving the resource to be queried}

\item{version}{an object of class "character" giving the resource version}

\item{flowRef}{an object of class "character" giving the flowRef to be queried}

\item{key}{an object of class "character" giving the key (SDMX url formatted) to be used for the query}

\item{start}{an object of class "character" giving the start time}

\item{end}{an object of class "character" giving the end time}

\item{references}{an object of class "character" giving the instructions to return (or not) the artefacts referenced by the artefact to be returned}

\item{compliant}{an object of class "logical" indicating if the web-service is compliant with the SDMX REST web-service specifications}
}
\description{
A class to handle a SDMX service request params
}
\section{Slots}{

\describe{
\item{\code{regUrl}}{an object of class "character" giving the base Url of the SDMX service registry}

\item{\code{repoUrl}}{an object of class "character" giving the base Url of the SDMX service repository}

\item{\code{accessKey}}{an object of class "character" indicating the name of request parameter for which
an authentication or subscription user key/token has to be provided to perform requests}

\item{\code{providerId}}{an object of class "character" giving the provider agency Id}

\item{\code{agencyId}}{an object of class "character" giving an agency Id}

\item{\code{resource}}{an object of class "character" giving the type of resource to be queried}

\item{\code{resourceId}}{an object of class "character" giving the resource to be queried}

\item{\code{version}}{an object of class "character" giving the resource version}

\item{\code{flowRef}}{an object of class "character" giving the flowRef to be queried}

\item{\code{key}}{an object of class "character" giving the key (SDMX url formatted) to be used for the query}

\item{\code{start}}{an object of class "character" giving the start time}

\item{\code{end}}{an object of class "character" giving the end time}

\item{\code{references}}{an object of class "character" giving the instructions to return (or not) the artefacts referenced by the artefact to be returned}

\item{\code{compliant}}{an object of class "logical" indicating if the web-service is compliant with the SDMX REST web-service specifications}
}}

\section{Warning}{

This class is not useful in itself, but all SDMX non-abstract classes will 
encapsulate it as slot, when parsing an SDMX-ML document.
}

\examples{
  #how to create a SDMXRequestParams object
  params <- SDMXRequestParams(
   regUrl = "", repoUrl ="", accessKey = NULL,
   providerId = "", agencyId ="", resource = "data", resourceId = "",
   version = "", flowRef = "", key = NULL, start = NULL, end = NULL, 
   references = NULL, compliant = FALSE
  )
}
\author{
Emmanuel Blondel, \email{emmanuel.blondel1@gmail.com}
}
