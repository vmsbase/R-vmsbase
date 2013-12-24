\name{set_wdgt_sel}
\alias{set_wdgt_sel}
\title{Set Widget Selection - Internal Function}
\usage{
set_wdgt_sel(widget_list, col_vals)
}
\arguments{
  \item{widget_list}{A list of widgets}

  \item{col_vals}{A vector of chararcters}
}
\description{
The \code{set_wdgt_sel} is the internal function that
implements the routine to set the current value to all the
widgets in the list.
}
\details{
This function, with a widgets list and a vector of
characters, set the current selected value to all the
widgets in the list, according to the order in the
character vector provided.
}
\seealso{
\code{\link{get_wdgt_vals}}; \code{\link{set_wdgt_vals}}
}

