% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/helpers.R
\name{subset_search_results}
\alias{subset_search_results}
\title{Subset Search Results}
\usage{
subset_search_results(strings, values, ignore.case = TRUE)
}
\arguments{
\item{strings}{Character vector.}

\item{values}{Numeric vector of the same length as \code{strings}.}

\item{ignore.case}{Whether to ignore case when subsetting strings.}
}
\value{
Values, re-allocated based on the subsetting of their strings.
}
\description{
Allocate values (e.g. search results) based on subsetting of strings (e.g. search strings).
}
\examples{
# Pine: 20, Blue pine: 17
strings <- c("Pine", "Blue pine")
values <- c(20, 17)
subset_search_results(strings, values) # 3, 17
# Pine: 20, Blue pine: 17, White blue pine: 10, White: 20
strings <- c("Pine", "Blue pine", "White blue pine", "White")
values <- c(20, 17, 10, 20)
subset_search_results(strings, values) # 3, 7, 10, 10
# Pine: 20, Blue pine: 17, Blue pine a: 10, Blue pine b: 5, Blue pine b c: 1
strings <- c("Pine", "Blue pine", "Blue pine a", "Blue pine b", "Blue pine b c")
values <- c(20, 17, 10, 5, 1)
subset_search_results(strings, values) # 3, 2, 10, 4, 1
# Pine: 20, Blue: 15, Blue pine: 10, Pine blue: 3
strings <- c("Pine", "Blue", "Blue pine", "Pine blue")
values <- c(20, 15, 10, 3)
subset_search_results(strings, values) # 7, 2, 10, 3
# Pine: 200, Blue pine: 100, White pine: 100
}
\seealso{
Other helper functions: \code{\link{capitalize_words}},
  \code{\link{ch1903_to_wgs84}},
  \code{\link{clean_strings}},
  \code{\link{format_addresses}},
  \code{\link{format_scientific_names}},
  \code{\link{is.empty}}, \code{\link{lvapply}},
  \code{\link{melt_by_listcol}}, \code{\link{na.remove}},
  \code{\link{normalize_language}},
  \code{\link{parse_scientific_names}},
  \code{\link{quotemeta}},
  \code{\link{replace_values_in_list}},
  \code{\link{sp_transform}}, \code{\link{unique_na}}
}
