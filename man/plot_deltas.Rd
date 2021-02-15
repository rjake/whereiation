% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/plot-deltas.R
\name{plot_deltas}
\alias{plot_deltas}
\title{Visualize difference from avg. by factor (lollipop)}
\usage{
plot_deltas(df, dep_var, ..., avg_type = c("mean", "median"))
}
\arguments{
\item{df}{dataframe to evaluate}

\item{dep_var}{dependent variable to use (column name)}

\item{...}{
  Arguments passed on to \code{\link[=refactor_columns]{refactor_columns}}
  \describe{
    \item{\code{split_on}}{variable to split data / group by}
    \item{\code{id}}{field to use as ID}
    \item{\code{n_cat}}{for categorical variables, the max number of unique values
to keep. This field feeds the \code{forcats::fct_lump(n = )} argument.}
    \item{\code{collapse_by}}{should \code{n_cat} collapse by the distance to the grand
mean \code{"dv"} leaving the extremes as is and grouping factors closer to the
grand mean as "other" or should it use size \code{"n"}}
    \item{\code{n_quantile}}{for numeric/date fields, the number of quantiles used
to split the data into a factor. Fields that have less than this amount
will not be changed.}
    \item{\code{n_digits}}{for numeric fields, the number of digits to keep in the breaks
ex: [1.2345 to 2.3456] will be [1.23 to 2.34] if \code{n_digits = 2}}
    \item{\code{ignore_cols}}{columns to ignore from analysis. Good candidates are
fields that have have no duplicate values (primary keys) or fields with
a large proportion of null values}
  }}

\item{avg_type}{mean or median}
}
\description{
Visualize difference from avg. by factor (lollipop)
}
\examples{
plot_deltas(df = ggplot2::mpg, dep_var = "hwy")
}