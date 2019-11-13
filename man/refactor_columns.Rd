% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/initial_calcs.R
\name{refactor_columns}
\alias{refactor_columns}
\title{Convert data to a table of factors}
\usage{
refactor_columns(df, dep_var, n_cat = 10, n_quantile = 10,
  avg_type = c("mean", "median"), ignore_cols = NA_character_)
}
\arguments{
\item{df}{dataframe to evaluate}

\item{dep_var}{dependent variable to use (column name)}

\item{n_cat}{for categorical variables, the max number of unique values
to keep. This field feeds the \code{forcats::fct_lump(n = )} argument.}

\item{n_quantile}{for numeric/date fields, the number of quantiles used
to split the data into a factor. Fields that have less than this amount
will not be changed.}

\item{avg_type}{mean or median}

\item{ignore_cols}{columns to ignore from analysis. Good candidates are
fields that have have no duplicate values (primary keys) or fields with
a large proportion of null values}
}
\description{
Convert data to a table of factors
}