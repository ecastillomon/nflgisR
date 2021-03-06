% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/helpers_geometry.R
\name{get_delaunay_triangles_def}
\alias{get_delaunay_triangles_def}
\title{Compute Delaunay Triangles for defensive players}
\usage{
get_delaunay_triangles_def(df)
}
\arguments{
\item{df}{df with players_def column}
}
\value{
df with convex_hull_def column with convex hulls
}
\description{
Compute Delaunay Triangles for defensive players
}
\examples{
get_delaunay_triangles_def(df)
}
