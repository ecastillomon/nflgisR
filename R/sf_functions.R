

field_height <- 160/3
field_width <- 120
field_poly = sf::st_polygon(list(rbind(c(0,0), c(field_width, 0),
                                   c(field_width, field_height),c(0, field_height),
                                   c(0,0))))


#' get_vornoi_transformations
#' Apply all transformations needed to compute Vornoi areas and their areas.
#' For examples of how this can be used check out https://www.kaggle.com/est092/vornoi-areas
#'
#'
#'
#' @param df
#'
#' @return
#' @import sf dplyr
#' @importFrom rlang .data
#' @export
#' @examples
#' get_vornoi_transformations(sf_sample)
get_vornoi_transformations=function(df){
  get_play_boundary(df) %>%
    get_sf_vornoi(.) %>%
    get_play_stats(.) %>%
    get_vornoi_analytics(.)
}


#' get_sf_vornoi
#'
#' @param df
#'
#' @return
#' @import sf dplyr
#' @importFrom rlang .data
#' @examples
#' get_play_boundary(sf_sample) %>% get_sf_vornoi()
get_sf_vornoi=function(df){
  group_by(df,.data$gameId,.data$playId) %>%
    mutate(vornoi_all=purrr::map2(.data$players,.data$play_boundary,purrr::possibly(function(.x,.y){
      # .x=df$players[[1]]
      # .y=df$play_boundary[[1]]
      .x=.x %>% tidyr::unnest(.data$tracking_data)
      1:max(.x$frameId) %>%
        purrr::map_dfr(function(z){
          frame_temp=.x %>% filter(.data$frameId==z) %>%st_sf()
          st_geometry(frame_temp) %>% st_union() %>%
            st_voronoi(., st_sfc(.y)) %>% st_collection_extract() %>%
            st_sf() %>%  ## mutate(index=row_number()) %>%
            st_join(frame_temp %>% select(.data$nflId), st_contains) %>%
            st_intersection(.y) %>%
            mutate(area =st_area(st_geometry(.)),frameId=z) %>%
            tibble() %>% st_sf()
          # mutate(nflId=frame_temp$nflId )
        })

    }, otherwise = NA))) %>%
    ungroup()
}

#' get_play_stats
#' Applies a series of transformations to the each frame to tag important information
#' @param df
#'
#' @return
#' @export
#' @import sf
#' @import dplyr
#' @examples
#' get_play_stats(sf_sample)
get_play_stats=function(df){
  group_by(df,.data$gameId,.data$playId) %>%
    mutate(play_stats=purrr::map(.data$players_all,purrr::possibly(function(.x){
      # .x=sf_sample$players_all[[1]]
      .x=.x %>% tidyr::unnest(.data$tracking_data)
      .x %>% group_by(.data$frameId,.data$event) %>% arrange(.data$event) %>% filter(row_number()==1) %>% ungroup() %>%
        select(.data$frameId,.data$event) %>%
        mutate(pass_event=case_when(grepl("pass_forward",.data$event)~"start",
                                    grepl("pass_(arrived|outcome)",.data$event)~"end",
                                    TRUE~"other")  ,
               event_2=ifelse(.data$event=="None",.data$event,"Other"),
               event=case_when( grepl("interception",.data$event,.data$event,ignore.case = TRUE)~"interception",
                                grepl("touchdown",.data$event,ignore.case = TRUE)~"touchdown",TRUE~.data$event),
               pass_start=min(.data$frameId[grepl("start",.data$pass_event)],na.rm = TRUE),
               pass_end=min(.data$frameId[grepl("end",.data$pass_event)],na.rm = TRUE),
               pass_event=ifelse(.data$pass_event=="end" & .data$frameId!=.data$pass_end,"other",.data$pass_event),
               play_start= min(.data$frameId[grepl("ball_snap",.data$event)]),
               complete=!any(grepl("\\wcomplete|interception|sack",.data$event)),
               inair=.data$frameId>.data$pass_start & .data$frameId<.data$pass_end,
               pass_event=case_when(grepl("other",.data$pass_event) & .data$inair~"air",
                                    grepl("other",.data$pass_event) & !is.finite(.data$pass_start) & grepl("Other",.data$event_2)~"sack",
                                    grepl("other",.data$pass_event) & !.data$inair & .data$frameId>.data$pass_end~"aftercatch",
                                    TRUE~.data$pass_event) )

    }, otherwise = NA)))
}


#' get_play_boundary
#'
#' @param df
#'
#' @return
#' @export
#' @import sf
#' @import dplyr
#' @examples
#' get_play_boundary(sf_sample)
get_play_boundary=function(df){
  group_by(df,.data$gameId,.data$playId) %>%
    mutate(play_boundary=purrr::map2(.data$absoluteYardlineNumber,.data$playDirection,purrr::possibly(function(.x,.y){
      if(.y=="right"){
        st_polygon(list(rbind(c(.x,0), c(field_width, 0),
                              c(field_width, field_height),c(.x, field_height),
                              c(.x,0))))
      }else{
        st_polygon(list(rbind(c(0,0), c(.x, 0),
                              c(.x, field_height),c(0, field_height),
                              c(0,0))))
      }
    }, otherwise = NA)))
}

#' get_vornoi_analytics
#' @param df
#'
#' @return
#' @import sf
#' @import dplyr
#' @examples
#' get_play_boundary(sf_sample) %>% get_sf_vornoi() %>% get_play_stats() %>% get_vornoi_analytics()
get_vornoi_analytics=function(df){
  mutate(df,vornoi_stats_players=purrr::map2(.data$vornoi_all,.data$play_stats,purrr::possibly(function(.x,.y){
      df_limit=.y %>% filter(.data$pass_event=="start" | .data$pass_event=="sack")
      .x %>%st_drop_geometry() %>%  filter(.data$frameId>=df_limit$play_start & .data$frameId<=df_limit$frameId) %>%
        group_by(.data$nflId) %>%  arrange(.data$frameId) %>%
        summarise(area_first=first(.data$area), area_last=last(.data$area), area_mean=mean(.data$area),area_max=max(.data$area)) %>% ungroup() %>%
        mutate(area_gained_perc=.data$area_last/.data$area_first,area_gained_net=.data$area_last-.data$area_first,
               area_optim_perc=.data$area_max/.data$area_first, area_optim_net=.data$area_max-.data$area_mean)
    }, otherwise = NA)),
    vornoi_stats_team=purrr::map2(.data$vornoi_stats_players,.data$players,purrr::possibly(function(.x,.y){
      .x %>% left_join(.y,by=c("nflId")) %>% group_by(.data$position_type_1) %>%
        summarise(player_max=  .data$nflId[.data$area_max==max(.data$area_max)],
                  player_max_win=  .data$nflId[.data$area_gained_net==max(.data$area_gained_net)],
                  player_max_loss=  .data$nflId[.data$area_gained_net==min(.data$area_gained_net)],
                  area_first=sum(.data$area_first), area_last=sum(.data$area_last),
                  area_mean=mean(.data$area_mean),area_max=max(.data$area_max)
        ) %>% ungroup() %>%
        mutate(area_gained_perc=.data$area_last/.data$area_first,area_gained_net=.data$area_last-.data$area_first,
               area_optim_perc=.data$area_max/.data$area_first, area_optim_net=.data$area_max-.data$area_mean)
    }, otherwise = NA)))
}
