% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/check_prior_knowledge_network.R
\name{checkPriorKnowledgeNetwork}
\alias{checkPriorKnowledgeNetwork}
\title{Checks prior knowledge network for correct format.}
\usage{
checkPriorKnowledgeNetwork(priorKnowledgeNetwork)
}
\arguments{
\item{priorKnowledgeNetwork}{a network with 3 columns: 
source node ('source'), interaction 
sign ('interaction') and target node('target').}
}
\value{
TRUE if everything is correct. Stops pipeline if not.
}
\description{
Checks prior knowledge network for correct format.
}
\author{
Enio Gjerga, Olga Ivanova 2020-2021
}
\keyword{internal}
