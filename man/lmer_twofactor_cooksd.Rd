% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/lmer_twofactor_cooksd.R
\name{lmer_twofactor_cooksd}
\alias{lmer_twofactor_cooksd}
\title{Run lmer on two factors}
\usage{
lmer_twofactor_cooksd(
  data,
  taskname,
  iv,
  stim_con1,
  stim_con2,
  dv,
  subject_keyword,
  dv_keyword,
  model_savefname,
  effects,
  print_lmer_output
)
}
\arguments{
\item{data}{dataframe}

\item{taskname}{string of task name}

\item{iv}{1st factor with two levels}

\item{stim_con1}{2nd factor with three levels. first contrast}

\item{stim_con2}{2nd factor with three levels. second contrast}

\item{dv}{a string of dependent variable}

\item{subject_keyword}{random factor. e.g. subject or sessions}

\item{dv_keyword}{string of dependent variable. for saving filenames}

\item{model_savefname}{full path of filename to save lmer results}

\item{effects}{options c("random_intercept", "random_slopes", "no_random")}

\item{print_lmer_output}{bool: TRUE if you want the output printed in the Rmd; FALSE if you want the output silent}
}
\value{
cooks d distance
}
\description{
Run lmer on two factors
}
