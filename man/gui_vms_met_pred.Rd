\name{gui_vms_met_pred}
\alias{gui_vms_met_pred}
\title{Predict Metier GUI}
\usage{
gui_vms_met_pred(vms_db_name = "")
}
\arguments{
  \item{vms_db_name}{The path of a VMS DataBase}
}
\value{
This function does not return a value.
}
\description{
The \code{gui_vms_met_pred} function implements the
graphical user interface for the Metier Prediction
}
\details{
This function, with a VMS database and a shape file with
harbours points, performs a neural network prediction over
the whole db assigning metier data to vms tracks based on a
training with existing vms-lb match data given by the
database.
}
\references{
free text reference Pointers to the literature related to
this object.
}

