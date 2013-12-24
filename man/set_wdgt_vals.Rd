\name{set_wdgt_vals}
\alias{set_wdgt_vals}
\title{Set Widget Values - Internal Function}
\usage{
set_wdgt_vals(widget_list, col_vals)
}
\arguments{
  \item{widget_list}{A list of widgets}

  \item{col_vals}{A vector of chararcters}
}
\description{
The \code{set_wdgt_vals} is the internal function that
implements the routine to set available values to all the
widgets in the list.
}
\details{
This function, with a widgets list and a vector of
characters, assigns the vector of character values to all
the widgets in the list
}
\seealso{
\code{\link{get_wdgt_vals}}; \code{\link{set_wdgt_sel}}
}

