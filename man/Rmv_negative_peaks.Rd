% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/Rmv_negative_peaks.R
\name{Rmv_negative_peaks}
\alias{Rmv_negative_peaks}
\title{Rmv_negative_peaks}
\usage{
Rmv_negative_peaks(SRmergedfile, S1i, S2i, S3i)
}
\arguments{
\item{SRmergedfile}{A data frame with merged forces and time data extracted from the stress-relaxation section from multiple MACH-1 files}

\item{S1i}{The index of the 1st step}

\item{S2i}{The index of the 2nd step}

\item{S3i}{The index of the 3rd step}
}
\value{
a data frame (SRmergedfile_Clean) with the forces of Stress-Relaxation cleaned from negative peaks
}
\description{
Remove negative peaks around the three stess peaks of the stress relaxation protol
}
