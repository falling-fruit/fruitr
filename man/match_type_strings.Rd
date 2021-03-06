% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/fallingfruit.R
\name{match_type_strings}
\alias{match_type_strings}
\title{Match Type Strings to Types}
\usage{
match_type_strings(type_strings, types = get_ff_types(pending = FALSE, urls =
  FALSE))
}
\arguments{
\item{type_strings}{Character vector of type strings.}

\item{types}{Falling Fruit types.}
}
\value{
List of Falling Fruit type ids matching each element in \code{type_strings}.
}
\description{
Returns the ids of the Falling Fruit types matching each type string.
}
\examples{
ff_types <- get_ff_types()
match_type_strings("Apple", ff_types)
match_type_strings(c("Apple [Malus domestica]", "Pear [Pyrus]"), ff_types)
}
\seealso{
Other Falling Fruit functions: \code{\link{build_location_descriptions}},
  \code{\link{build_location_description}},
  \code{\link{build_type_strings}},
  \code{\link{expand_category_mask}},
  \code{\link{get_ff_types}},
  \code{\link{normalize_type_strings}},
  \code{\link{parse_type_strings}}
}
