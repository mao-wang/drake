% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/api-mapreduce.R
\name{reduce_by}
\alias{reduce_by}
\title{Reduce multiple groupings of targets}
\usage{
reduce_by(plan, ..., prefix = "target", begin = "", op = " + ",
  end = "", pairwise = TRUE, append = TRUE, filter = NULL,
  sep = "_")
}
\arguments{
\item{plan}{workflow plan data frame of prespecified targets}

\item{...}{Symbols, columns of \code{plan} to define target groupings.
A \code{\link[=reduce_plan]{reduce_plan()}} call is applied for each grouping.
Groupings with all \code{NA}s in the selector variables are ignored.}

\item{prefix}{character, prefix for naming the new targets.
Suffixes are generated from the values of the columns
specified in \code{...}.}

\item{begin}{character, code to place at the beginning
of each step in the reduction}

\item{op}{binary operator to apply in the reduction}

\item{end}{character, code to place at the end
of each step in the reduction}

\item{pairwise}{logical, whether to create multiple
new targets, one for each pair/step in the reduction (\code{TRUE}),
or to do the reduction all in one command.}

\item{append}{logical. If \code{TRUE}, the output will include the
original rows in the \code{plan} argument.
If \code{FALSE}, the output will only include the new
targets and commands.}

\item{filter}{an expression like you would pass to \code{dplyr::filter()}.
The rows for which \code{filter} evaluates to \code{TRUE} will be gathered,
and the rest will be excluded from gathering.
Why not just call \code{dplyr::filter()} before \code{gather_by()}?
Because \code{gather_by(append = TRUE, filter = my_column == "my_value")}
gathers on some targets while including all the original targets
in the output. See the examples for a demonstration.}

\item{sep}{character scalar, delimiter for creating the names
of new targets}
}
\value{
A workflow plan data frame.
}
\description{
Perform several calls to \code{\link[=reduce_plan]{reduce_plan()}}
based on groupings from columns in the plan,
and then row-bind the new targets to the plan.
}
\examples{
plan <- drake_plan(
  data = get_data(),
  informal_look = inspect_data(data, mu = mu__),
  bayes_model = bayesian_model_fit(data, prior_mu = mu__)
)
plan <- evaluate_plan(plan, rules = list(mu__ = 1:2), trace = TRUE)
plan
reduce_by(plan, mu___from, begin = "list(", end = ")", op = ", ")
reduce_by(plan, mu__)
reduce_by(plan, mu__, append = TRUE)
reduce_by(plan, mu__, append = FALSE)
reduce_by(plan) # Reduce all the targets.
reduce_by(plan, append = FALSE)
reduce_by(plan, pairwise = FALSE)
# You can filter out the informal_look_* targets beforehand
# if you only want the bayes_model_* ones to be reduced.
# The advantage here is that if you also need `append = TRUE`,
# only the bayes_model_* targets will be reduced, but
# the informal_look_* targets will still be included
# in the output.
reduce_by(
  plan,
  mu___from,
  append = TRUE,
  filter = mu___from == "bayes_model"
)
}
\seealso{
\code{\link[=drake_plan]{drake_plan()}}, \code{\link[=map_plan]{map_plan()}}, \code{\link[=gather_by]{gather_by()}}, \code{\link[=reduce_plan]{reduce_plan()}},
\code{\link[=gather_plan]{gather_plan()}}, \code{\link[=evaluate_plan]{evaluate_plan()}}, \code{\link[=expand_plan]{expand_plan()}}
}
