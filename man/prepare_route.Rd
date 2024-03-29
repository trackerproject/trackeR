% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/trackeRdata_plot.R
\name{prepare_route}
\alias{prepare_route}
\title{Prepare a \code{\link{data.frame}} for use in
\code{\link{leaflet_route}} and \code{\link{plot_route}}}
\usage{
prepare_route(x, session = 1, threshold = TRUE, ...)
}
\arguments{
\item{x}{a \code{\link{trackeRdata}} object.}

\item{session}{which session to prepare the
\code{\link{data.frame}} for?}

\item{threshold}{if \code{TRUE} (default), then thresholds are
applied to \code{x} prior to preparing the
\code{\link{data.frame}}.}

\item{...}{Additional arguments to be passed to
\code{\link{threshold}}.}
}
\value{
A \code{data.frame} with variables \code{longitude},
\code{latitude}, \code{speed}, \code{SessionID}, \code{longitude0},
\code{longitude1}, \code{latitude0}, \code{latitude1}. The
observations are ordered according to the timestamp they have in
\code{x}. A suffix of 0 indicates 'start' and a suffix of 1
indicates 'end' at any given observation.
}
\description{
Prepare a \code{\link{data.frame}} for use in
\code{\link{leaflet_route}} and \code{\link{plot_route}}
}
\details{
To be used internally in mapping function and rarely by the user.
}
