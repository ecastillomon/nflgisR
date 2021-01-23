#' get_play_sf
#' Gets a play or a whole game,
#' transforming all coordintates to a simple feature list column, and embedding tracking data into every object in the field
#'  After converting to sf, it will create all combinations of interest between the objects, like all, players, football, players off
#'  and players def. Objects will be repeated and this might not be efficient if only using a certain combination.
#'
#'
#' @param playid
#' @param gameid
#' @return
#' @export
#' @import sf dplyr
#' @importFrom rlang .data
#'
#' @examples df=get_play_sf(playid=NULL,gameid)
get_play_sf=function(playid=NULL,gameid,...){
  get_play(playid,gameid) %>%
    mutate(time=lubridate::as_datetime(.data$time)) %>%
    tidyr::nest(tracking_data=c(.data$time,.data$x,.data$y,.data$s,.data$a,.data$dis,.data$o,.data$dir,.data$event,.data$frameId)) %>%
    mutate(tracking_data=purrr::map(.data$tracking_data,function(x){
      # tidyr::unnest(x,cols=tracking_data) %>%
      st_as_sf(x,coords = c("x", "y"), remove=FALSE) %>%
        ##Velocity
        mutate(dir_rad=.data$dir* pi / 180,
               v_x=sin(.data$dir_rad)*.data$s,
               v_y=cos(.data$dir_rad)*.data$s,
               x_pred=.data$x+.data$v_x,
               y_pred=.data$y+.data$v_y)
    })
    ) %>%
    tidyr::nest(players_all=c(everything(),-colnames(plays_index),-colnames(games), -.data$playDirection) ) %>%
    get_nested_sf()
}

#' get_play
#' Searches for a play without any transformations
#'
#' @param playid if NULL (default), will retrieve the whole game
#' @param gameid
#'
#' @return
#' @export
#' @import dplyr
#' @importFrom rlang .data
#' @examples df=get_play(1946,2018101412)
get_play=function(playid=NULL,gameid ){
    if(is.null(playid)){
      df=plays_index %>%
        filter(.data$gameId%in% gameid )
    } else{
      df=plays_index %>%
        filter(.data$gameId%in% gameid & .data$playId%in% playid)
    }
    df= inner_join(df,df_plays ,by=c("playId","gameId")) %>%
      left_join(games , by=c("gameId")) %>%
      left_join(players %>% select(.data$nflId,.data$height,.data$weight, .data$position_type_1), by=c("nflId")) %>%
      mutate(nflId=coalesce(.data$nflId,9999))

    if("tbl_dbi"%in% class(df_plays)){collect(df)}else{ df}
    # collect()
}





#' get_nested_transformations
#' Gets all combinations of interest between objects
#'
#' @param df with column nflId,playId and gameId, position_type_1
#'  nflId=9999 if football
#'
#' @return
#' @export
#' @import sf dplyr
#' @importFrom rlang .data
#' @examples
get_nested_sf=function(df){
  df %>% mutate(
    ##Get all player data
    players=purrr::map(.data$players_all,function(.x){
      .x %>% filter(!is.na(.data$position_type_1))
    }),
    #Get all player data
    players_off=purrr::map(.data$players_all,function(.x){
      .x %>% filter(.data$position_type_1=="offense")
    }),
    players_def=purrr::map(.data$players_all,function(.x){
      .x %>% filter(.data$position_type_1=="defense")
    }),
    ball=purrr::map(.data$players_all,function(.x){
      .x %>% filter(is.na(.data$position_type_1)) %>%
        mutate(tracking_data=purrr::map(.data$tracking_data,function(x) select(x,-c(.data$v_x,.data$v_y,.data$dir_rad))))
    })
  )
}

