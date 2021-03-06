% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/fallingfruit.R
\name{expand_category_mask}
\alias{expand_category_mask}
\title{Expand Category Mask to Categories}
\usage{
expand_category_mask(mask)
}
\arguments{
\item{mask}{Integer represention of a binary category mask.}
}
\value{
Vector of category names.
}
\description{
Expand Category Mask to Categories
}
\examples{
expand_category_mask(0)
expand_category_mask(3)
}
\seealso{
Other Falling Fruit functions: \code{\link{build_location_descriptions}},
  \code{\link{build_location_description}},
  \code{\link{build_type_strings}},
  \code{\link{get_ff_types}},
  \code{\link{match_type_strings}},
  \code{\link{normalize_type_strings}},
  \code{\link{parse_type_strings}}
}
