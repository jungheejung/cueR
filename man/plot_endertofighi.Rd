% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/plot_enderstofighi.R
\name{plot_endertofighi}
\alias{plot_endertofighi}
\title{Plot Within subject and Between subject effects}
\usage{
plot_endertofighi(data, taskname, color_low = "gray", color_high = "black")
}
\arguments{
\item{data}{A dataframe containing the data to be plotted with columns like 'OUTCOME', 'EXPECT_demean', etc.}

\item{taskname}{A character string specifying the task name, used for filtering data and generating plot titles.}

\item{color_low}{The lower end color for gradient (default is "gray").}

\item{color_high}{The higher end color for gradient (default is "black").}
}
\value{
The function does not return a value but produces a grid of plots as a side effect.
}
\description{
This function generates various types of plots for a given task, including demean and cluster-wise means plots.
It allows customization of color gradients for the plots. The function assumes specific column names in the data
and uses custom ggplot formatting functions.
}
\examples{
# Example usage:
plot_endertofighi(df.PVC_center[df.PVC_center$runtype == "runtype-pain", ], "pain", color_low = "red", color_high = "darkred")
# This assumes you already tidied the dataframe in the right format.
# Here's a pipeline:
# 1. First, load the data
#    dataPVC <- cueR::df_load_pvc_beh(datadir,
#                          subject_varkey = subject_varkey,
#                          iv = iv,
#                          exclude = exclude)
# 2. ESSENTIAL: Next, compute within subject and between subject effects
#    df.PVC_center <- cueR::compute_enderstofighi(dataPVC, sub="sub",
#                                   outcome = "event04_actual_angle",expect= "event02_expect_angle",
#                                   ses = "ses", run = "run")
# 3. then `plot_endertofighi`
}
