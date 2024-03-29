% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/utils.R
\name{getItemsList}
\alias{getItemsList}
\title{Checks items available for ReportGenerator}
\usage{
getItemsList(items)
}
\arguments{
\item{items}{vector of uploaded filenames.}
}
\value{
a dataframe with the properties of the items
}
\description{
Get the items that the user can choose from in the report generator. The list is loaded from the configuration file
and filtered by the files that have been uploaded.
}
