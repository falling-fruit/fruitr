% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/translation.R
\name{build_common_name_table}
\alias{build_common_name_table}
\title{Build table of common names}
\usage{
build_common_name_table(responses, scientific_names = NULL,
  normalize_languages = FALSE, search_names = FALSE)
}
\arguments{
\item{responses}{(response list) Result of \code{\link{query_sources_about_type}}.}

\item{scientific_names}{(character vector) Scientific names for filtering (?).}

\item{normalize_languages}{(boolean) Whether to normalize language codes.}

\item{search_names}{(boolean) Whether to count search results for each name.}
}
\description{
Build table of common names
}
\seealso{
Other translation functions: \code{\link{build_scientific_name_table}},
  \code{\link{normalize_common_name}},
  \code{\link{parse_sources_about_type}},
  \code{\link{query_sources_about_type}}
}
