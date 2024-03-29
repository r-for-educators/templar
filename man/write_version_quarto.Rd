% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/write_version_quarto.R
\name{write_version_quarto}
\alias{write_version_quarto}
\title{Function to write the different versions.}
\usage{
write_version_quarto(
  v,
  orig_name,
  orig_dir,
  orig_text,
  sec_info,
  all_info,
  folders,
  to_jupyter = FALSE
)
}
\arguments{
\item{v}{Version list}

\item{orig_name}{Original file name}

\item{orig_dir}{Original file directory}

\item{orig_text}{Original file text}

\item{sec_info}{Sections info}

\item{all_info}{All chunks info}

\item{folders}{Folders to store the new generated files}

\item{to_jupyter}{Should we generate an ipynb copy?}

\item{global_eval}{Should eval option be set back to TRUE?}
}
\value{
Doesn't return anything, but generates the new RMD files.
}
\description{
Function to write the different versions.
}
