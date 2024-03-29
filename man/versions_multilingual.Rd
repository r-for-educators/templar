% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/versions.R
\name{versions_multilingual}
\alias{versions_multilingual}
\title{Knit different versions of a file from chunk and section options}
\usage{
versions_multilingual(
  global_eval = TRUE,
  to_knit = NULL,
  folders = NULL,
  to_jupyter = FALSE
)
}
\arguments{
\item{global_eval}{Logical.}

\item{to_knit}{Character vector specifying which versions to write and knit
into separate files.  If not specified, all versions are produced.}

\item{folders}{List of versions and subfolder to put them in. Use pattern
\code{version_name = folder_name}.  Default is each version in its own folder.}

\item{to_jupyter}{Should a .ipynb document be generated from the python version?}
}
\value{
none
}
\description{
\code{versions_multilingual} is a function that should be included in the setup chunk of
an R Markdown document.  Its purpose is to write, then knit, R Markdown source
files for several versions of a document, specifically when multiple languages
are being used for different versions.
}
