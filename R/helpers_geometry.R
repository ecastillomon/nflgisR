##Convex Hull Offense
#' Compute convex hull for offensive players
#'
#' @param df
#'
#' @return df with players_off column
#' @export
#' @import sf dplyr
#' @importFrom rlang .data
#'
#' @examples
#' get_convex_hull_off(df)
get_convex_hull_off=function(df){
  df %>% group_by(.data$gameId,.data$playId) %>%
    mutate(convex_hull_off=purrr::map(.data$players_off,purrr::possibly(function(.x){
      # .x=sf_sample$players_off[1]]
      .x=.x %>% tidyr::unnest(.data$tracking_data)
      1:max(.x$frameId) %>%
        purrr::map_dfr(function(z){
          .x %>% filter(.data$frameId==z) %>%st_sf() %>%
            {st_geometry(.) %>% st_union() %>%
                st_convex_hull() %>% st_collection_extract() %>%
                st_sf() %>%
                mutate(area=st_area(st_geometry(.)),frameId=z) %>% tibble()}
        }) }, otherwise = NA)))
}

##Convex Hull Defense
#' Compute convex hull for offensive players
#'
#' @param df df with players_def column
#' @return df with convex_hull_def column with convex hulls
#' @export
#' @import sf dplyr
#' @importFrom rlang .data
#'
#' @examples
#' get_convex_hull_def(df)
get_convex_hull_def=function(df){
  df %>% group_by(.data$gameId,.data$playId) %>%
    mutate(convex_hull_def=purrr::map(.data$players_def,purrr::possibly(function(.x){
      # .x=sf_sample$players_off[1]]
      .x=.x %>% tidyr::unnest(.data$tracking_data)
      1:max(.x$frameId) %>%
        purrr::map_dfr(function(z){
          .x %>% filter(.data$frameId==z) %>%st_sf() %>%
            {st_geometry(.) %>% st_union() %>%
                st_convex_hull() %>% st_collection_extract() %>%
                st_sf() %>%
                mutate(area=st_area(st_geometry(.)),frameId=z) %>% tibble()}
        }) }, otherwise = NA)))
}


##Delaunay Triangles for defensive players
#' Compute Delaunay Triangles for defensive players
#'
#' @param df df with players_def column
#' @return df with convex_hull_def column with convex hulls
#' @export
#' @import sf dplyr
#' @importFrom rlang .data
#'
#' @examples
#' get_delaunay_triangles_def(df)
get_delaunay_triangles_def=function(df){
  df %>% group_by(.data$gameId,.data$playId) %>%
    mutate(delaunay_triangles_def=purrr::map(.data$players_def,purrr::possibly(function(.x){
      # .x=sf_sample$players_off[1]]
      .x=.x %>% tidyr::unnest(tracking_data)
      1:max(.x$frameId) %>%
        purrr::map_dfr(function(z){
          .x %>% filter(frameId==z) %>%st_sf() %>%
            {st_geometry(.) %>% st_union() %>%
                st_triangulate(.) %>%  st_collection_extract() %>%
                st_sf() %>%
                mutate(area=st_area(st_geometry(.)),frameId=z) %>% tibble() }
        }) }, otherwise = NA)))
}

##Delaunay Triangles for offensive players
#' Compute Delaunay Triangles for offensive players
#'
#' @param df df with players_off column
#' @return df with convex_hull_off column with convex hulls
#' @export
#' @import sf dplyr
#' @importFrom rlang .data
#'
#' @examples
#' get_convex_hull_off(df)
get_delaunay_triangles_off=function(df){
  df %>% group_by(.data$gameId,.data$playId) %>%
    mutate(delaunay_triangles_off=purrr::map(.data$players_off,purrr::possibly(function(.x){
      # .x=sf_sample$players_off[1]]
      .x=.x %>% tidyr::unnest(tracking_data)
      1:max(.x$frameId) %>%
        purrr::map_dfr(function(z){
          .x %>% filter(frameId==z) %>%st_sf() %>%
            {st_geometry(.) %>% st_union() %>%
                st_triangulate(.) %>%  st_collection_extract() %>%
                st_sf() %>%
                mutate(area=st_area(st_geometry(.)),frameId=z) %>% tibble() }
        }) }, otherwise = NA)))
}
