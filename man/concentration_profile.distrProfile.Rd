% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/concentrationProfile.R
\name{concentration_profile.distrProfile}
\alias{concentration_profile.distrProfile}
\alias{conProfile}
\alias{concentration_profile.trackeRdata}
\title{Generate training concentration profiles.}
\usage{
\method{concentration_profile}{distrProfile}(object, session = NULL, what = NULL, ...)

\method{concentration_profile}{trackeRdata}(
  object,
  session = NULL,
  what = NULL,
  limits = NULL,
  parallel = FALSE,
  unit_reference_sport = NULL,
  scale = FALSE,
  ...
)
}
\arguments{
\item{object}{An object of class \code{\link{trackeRdata}} or \code{\link{distrProfile}}.}

\item{session}{A numeric vector of the sessions to be used,
defaults to all sessions.}

\item{what}{The variables for which the distribution profiles
should be generated. Defaults to all variables in \code{object}
(\code{what = NULL}).}

\item{...}{Currently not used.}

\item{limits}{A named list of vectors of two numbers to specify the
lower and upper limits for the variables in \code{what}. If
\code{NULL} (default) the limits for the variables in
\code{what} are inferred from \code{object}.}

\item{parallel}{Logical. Should computation be carried out in
parallel? Default is \code{FALSE}.}

\item{unit_reference_sport}{The sport to inherit units from
(default is taken to be the most frequent sport in
\code{object}).}

\item{scale}{Logical. If \code{FALSE} (default) then the integral
of the profiles over the real line matches the session length.}
}
\value{
An object of class \code{conProfile}.

Object:

A named list with one or more components, corresponding to the
value of \code{what}. Each component is a matrix of dimension
\code{g} times \code{n}, where \code{g} is the length of the grids
set in \code{grid} (or 200 if \code{grid = NULL}) and \code{n} is
the number of sessions requested in the \code{session} argument.

Attributes:

Each \code{conProfile} object has the following attributes:

\itemize{

\item \code{sport}: the sports corresponding to the columns of each
list component

\item \code{session_times}: the session start and end times
corresponding to the columns of each list component

\item \code{unit_reference_sport}: the sport where the units have
been inherited from

\item \code{operations}: a list with the operations that have been
applied to the object. See \code{\link{get_operations.distrProfile}}

\item \code{limits}: The variable limits that have been used for the
computation of the concentration profiles.

\item \code{units}: an object listing the units used for the
calculation of distribution profiles. These is the output of
\code{\link{get_units}} on the corresponding
\code{\link{trackeRdata}} object, after inheriting units from
\code{unit_reference_sport}.

}
}
\description{
Generate training concentration profiles.
}
\references{
Kosmidis, I., and Passfield, L. (2015). Linking the Performance of
Endurance Runners to Training and Physiological Effects via
Multi-Resolution Elastic Net. \emph{ArXiv e-print}
arXiv:1506.01388.

Frick, H., Kosmidis, I. (2017). trackeR: Infrastructure for Running
and Cycling Data from GPS-Enabled Tracking Devices in
R. \emph{Journal of Statistical Software}, \bold{82}(7),
1--29. doi:10.18637/jss.v082.i07
}
