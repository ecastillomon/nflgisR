#' get_closest_players
#'
#' @param df a dataframe built with get_play_sf
#'
#' @return df with an extra column distance_all with each player's distance to each opposing player
#' @export
#' @import sf dplyr
#' @importFrom rlang .data
#' @examples
get_closest_players=function(df){
  group_by(df,.data$gameId,.data$playId) %>%
    mutate(distance_all=purrr::map(.data$players_all,purrr::possibly(function(.x){
      # .x=df$players_all[[1]]
      df_temp=.x %>% tidyr::unnest(.data$tracking_data) %>% st_sf() %>%
        group_by(.data$frameId,.data$nflId) %>% arrange(.data$event) %>% filter(row_number()==1)  %>%ungroup()
      df_temp%>% group_by(.data$frameId,.data$nflId) %>%
        select(.data$frameId,.data$nflId) %>%
        # head(1) %>% ungroup()->.x
        dplyr::group_modify(function(.x,.y){
          frame=.y$frameId
          y=df_temp %>% filter(.data$frameId==frame)
          ids=y$nflId %>% as.character()
          st_distance(.x,y) %>%  data.frame() %>%
            purrr::set_names(ids) %>% tidyr::pivot_longer(everything(),names_to="nflId") %>%
            arrange(value) %>% mutate(type=paste0("closest_",row_number())) %>%
            tidyr::pivot_wider(names_from = type,values_from=c(.data$nflId,.data$value), names_glue="{type}-{.value}") %>%
            bind_cols(.x,.) %>% st_drop_geometry()
        }) %>%
        ungroup()
    }, otherwise = NA))) %>%
    ungroup()
}
