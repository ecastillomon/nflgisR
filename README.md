
<!-- README.md is generated from README.Rmd. Please edit that file -->

# nflgisR

nflgisR is a project to bring the tools needed for analyzing NFL
tracking data. While this data is still not fully public, this package
aims to help advancing research on NFL tracking data, and sports
analytics in general. The project aims to provide a simple framework
that can be followed to compute any statistic needed to characterize a
play.

Tracking data is converted to simple features to facilitate computation
of geometric objects. Harnessing two of R’s biggest strenghts: the
tidyverse and sf, nflgisR provides a framework to handle and organize
tracking data in the NFL. This project is totally new, and is open to
collaboration, or extensions to different sports. It is designed to use
the columns provided for the 2020 NFL Data Bowl. If you have a different
dataset, you would have to set the names to match those files or some
minor changes to the code in load\_db. More information regarding
sources is provided in the documentation for load\_db

## Installation

To use this package you can sign into Kaggle and download the data at
[www.kaggle.com/c/nfl-big-data-bowl-2021/data](https://www.kaggle.com/c/nfl-big-data-bowl-2021/data)

This package depends on sf, which could require you to install some
extra libraries for geographic data. There are some tutorials to do
these on <https://thinkr.fr/Installation_spatial_EN.html>

Place all the data at data/

You can build a database to handle the files if you have DBI and
RSQLite, or use csv’s.

``` r
devtools::install_github("ecastillomon/nflgisR")
```

## Getting started

Run load\_db to load all necessary connections. If the DB=sql, it will
lazy load tables using dbplyr. csv mode is much faster but it will be
much more intense on the RAM.

The main function is get\_play\_sf, which will load a vector of gameIds
and playIds. If no playId is given, it will go for whole games. If you
want to make large extractions, you should pass a vector of gameIds and
playId=NULL.

``` r
library(nflgisR)
load_db(src_dir="data",DB="csv",week=5)


sf=get_play_sf(51,2018100400)
sf=get_vornoi_transformations(sf)
```

Once you have plays as nested dataframes, with all the information you
need. Computation between columns becomes very easy. In these example
convex hulls and delaunay triangles are computed for offense and
defense. Checkout how this functions work, so that you can easily add
any transformation you need.

``` r
sf=get_play_sf(51,2018100400) %>% 
  get_closest_players() %>% 
  get_convex_hull_off() %>% 
  get_convex_hull_def() %>% 
  get_delaunay_triangles_def() %>% 
  get_delaunay_triangles_off()
```

If you want to compute play-wide statistics, you can do it in a similar
way, as in the example:

``` r
sf=get_play_sf(51,2018100400) %>% 
  get_play_stats()
sf$play_stats[[1]]
```

To create the database, run create\_db, you will need to pass a src\_dir
and a target\_dir. Database will be created using SQLite, which is very
slow and heavy, so if you find yourself with some time, contact me to
see if we can plug in a faster one.

``` r
create_db(src_dir = "/home/esteban/nflgisR/data/")
```

gganimate is not installed to make the package lighter, but you will
have to install if you don’t already have it already to create gifs.
Plots were used in the submission for my databowl project that can be
found here <https://www.kaggle.com/est092/vornoi-areas>

<img src="https://gitlab.com/est_092/vornoiAreas/-/raw/master/output/VOR-2072.png" alt="hi" class="inline"/>

``` r
plot_vornoi_animation(df)
```

<img src="https://gitlab.com/est_092/vornoiAreas/-/raw/master/output/VA_stacked-2072.png" alt="hi" class="inline"/>

``` r
plot_vornoi_analytics(df)
```

Thanks for using\! This project is meant to be open-source, and any
feedback or contribution will be considered. Feel free to contact me at
[ecastillomon](https://twitter.com/ecastillomon), or at
[patpAItriot](https://twitter.com/patPaitriot) for more football
analytics project. Email: [est092@gmail.com](est092@gmail.com)

Github: [https://github.com/ecastillomon](ecastillomon)
