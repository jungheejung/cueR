% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/lmer_onefactor_cooksd_fix.R
\name{lmer_onefactor_cooksd_fix}
\alias{lmer_onefactor_cooksd_fix}
\title{Function to compute Cook's distance for a one-factor linear mixed-effects model}
\usage{
lmer_onefactor_cooksd_fix(
  dataframe,
  taskname,
  iv,
  dv,
  subject,
  dv_keyword,
  model_savefname
)
}
\arguments{
\item{dataframe}{A data frame containing the data.}

\item{taskname}{A string indicating the name of the task.}

\item{iv}{The column name that contains the independent variable.}

\item{dv}{A vector of dependent variable names.}

\item{subject}{The column name indicating the subject (random effect).}

\item{dv_keyword}{A string representing the dependent variable for display purposes.}

\item{model_savefname}{The filename where the model summary will be saved.}
}
\value{
A vector of Cook's distance values for each observation in the model.
}
\description{
This function fits a linear mixed-effects model to the specified data
and calculates Cook's distance for the model, which is a measure of
the influence of each observation.
}
\examples{
# Example usage:
# lmer_onefactor_cooksd_fix(dataframe, "Task1", "IV1", "DV1", "Subject", "dv_keyword", "model.txt")
}
