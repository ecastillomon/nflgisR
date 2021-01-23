
#' get_vornoi_animation
#' Will need to have gganimate installed
#'
#' @param df
#' @param file_dir directory were file will be created, will create a dir for the game, and will try to put the plots there
#'
#' @return
#' @export
#' @import sf dplyr ggplot2
#' @importFrom rlang .data
#'
plot_vornoi_animation=function(df,file_dir="output/"){
  if (!requireNamespace("gganimate", quietly = TRUE)) {
    print("gganimate packages are needed, please install")
    break()
  }
  df_plot_stats=df %>% head(1) %>% pull(.data$play_stats) %>% .[[1]]
  df_plot=df %>% head(1) %>% pull(.data$players_all) %>% .[[1]] %>%
    tidyr::unnest(.data$tracking_data) %>% st_sf()%>%
    left_join(df_plot_stats , by=c("frameId")) %>% mutate(frameId=as.integer(.data$frameId))
  # %>% filter(nflId!=9999)
  df_boundary=df %>% head(1) %>% pull(.data$play_boundary) %>% .[[1]]
  home=df %>% pull(.data$homeTeamAbbr) %>% head(1)
  away=df %>% pull(.data$visitorTeamAbbr) %>% head(1)
  desc=df %>% pull(.data$playDescription) %>% head(1)
  down=df %>% pull(.data$down) %>% head(1)
  togo=df %>% pull(.data$yardsToGo) %>% head(1)
  pass_result=df %>% pull(.data$passResult) %>% head(1)
  gm_id=df %>% pull(.data$gameId) %>% head(1)
  pl_id=df %>% pull(.data$playId) %>% head(1)
  play_length = length(unique(df_plot$frameId))
  play_title = paste0(away," at ", home, " ",down," & ",togo," [",pass_result,"]" , ", ",desc)
  file_dir=paste0(file_dir,gm_id,"/")
  system(paste0("mkdir -p ",file_dir ))

  df_football=df_plot %>% filter(nflId==9999)
  df_plot_vor=df %>% head(1) %>% pull(.data$vornoi_all)%>% .[[1]]%>% mutate(frameId=as.integer(.data$frameId)) %>%
    left_join(df_plot%>% st_drop_geometry() %>% distinct(.data$nflId, .data$jerseyNumber,.data$position_type_1,
                                                         .data$displayName,.data$pass_start,.data$pass_end,.data$play_start) ,by=c("nflId"))
  play_area=df_plot_vor %>% filter(.data$frameId==1) %>% summarise(area=sum(.data$area)) %>% pull(.data$area)

  play_frames=ggplot()+
    geom_sf(aes(geometry=geometry,fill=position_type_1,group=frameId),alpha=.2,data=df_plot_vor)+
    geom_sf(aes(group=frameId),shape=3,data=df_football)+
    geom_sf(data=df_plot %>% filter(!is.na(.data$position_type_1)),aes(color=position_type_1,shape=position_type_1,group=frameId),size=3, alpha=.5)+
    geom_text(aes(x=x,y=y,label=jerseyNumber,group=frameId), data=df_plot%>% filter(!is.na(position_type_1)),vjust=-1,size=3)+
    geom_segment(aes(x=x,y=y,xend=x+v_x,yend=y+v_y,color=position_type_1,group=frameId),data=df_plot,arrow = arrow(type = "closed",
                                                                                                                   length = unit(0.05, "inches")), alpha=.3,linetype=2)+
    theme_classic()+
    labs(
      title = play_title,  subtitle = paste0(" Frame: {floor(frame*play_length/250)}"),  caption = "Source: nflgisR" ) +
    gganimate::transition_states(frameId,transition_length = 2, state_length = 1,
                      wrap = FALSE)
  play_anim = tryCatch({
    gganimate::animate(
      play_frames,fps = 50, nframe = 250, width = 1500, height =  720, end_pause = 10,
      width=1280,res = 144)
  },error=function(e){
    gganimate::animate(
      play_frames,fps = 50, nframe = 250, width = 1500,height =  720,
      res = 144)
  })
  gganimate::anim_save(paste0(file_dir,"vornoi-",pl_id,".gif") , play_anim )
  play_anim
}


#' plot_vornoi_analytics
#'
#' @param df
#' @param file_dir directory were file will be created, will create a dir for the game, and will try to put the plots there
#' @param type types of plots to make, will default to all, but you can pass a vector of: "players","stacked","team","all"
#'
#'
#' @return
#' @export
#' @import dplyr ggplot2
#' @importFrom rlang .data
#'
plot_vornoi_analytics=function(df,file_dir="output/",type="all"){
  if (!requireNamespace("gganimate", quietly = TRUE)) {
    print("gganimate packages are needed, please install")
    break()
  }
  df_plot_stats=df %>% head(1) %>% pull(.data$play_stats) %>% .[[1]]
  df_plot=df %>% head(1) %>% pull(.data$players_all) %>% .[[1]] %>%
    tidyr::unnest(.data$tracking_data) %>% st_sf()%>%
    left_join(df_plot_stats , by=c("frameId")) %>% mutate(frameId=as.integer(.data$frameId))
  # %>% filter(nflId!=9999)
  df_boundary=df %>% head(1) %>% pull(.data$play_boundary) %>% .[[1]]
  home=df %>% pull(.data$homeTeamAbbr) %>% head(1)
  away=df %>% pull(.data$visitorTeamAbbr) %>% head(1)
  desc=df %>% pull(.data$playDescription) %>% head(1)
  down=df %>% pull(.data$down) %>% head(1)
  togo=df %>% pull(.data$yardsToGo) %>% head(1)
  pass_result=df %>% pull(.data$passResult) %>% head(1)
  gm_id=df %>% pull(.data$gameId) %>% head(1)
  pl_id=df %>% pull(.data$playId) %>% head(1)
  play_length = length(unique(df_plot$frameId))
  play_title = paste0(away," at ", home, " ",down," & ",togo," [",pass_result,"]" , ", ",desc)
  file_dir=paste0(file_dir,gm_id,"/")
  system(paste0("mkdir -p ",file_dir ))

  df_football=df_plot %>% filter(nflId==9999)
  df_plot_vor=df %>% head(1) %>% pull(.data$vornoi_all)%>% .[[1]]%>% mutate(frameId=as.integer(.data$frameId)) %>%
    left_join(df_plot%>% st_drop_geometry() %>% distinct(.data$nflId, .data$jerseyNumber,.data$position_type_1,
                                                         .data$displayName,.data$pass_start,.data$pass_end,.data$play_start) ,by=c("nflId"))
  play_area=df_plot_vor %>% filter(.data$frameId==1) %>% summarise(area=sum(.data$area)) %>% pull(.data$area)
  if("stacked" %in% type |type=="all"){
    ##Stacked
    df_plot_vor %>% mutate(displayName=paste0(.data$jerseyNumber,"\n",.data$displayName)) %>%
      # semi_join(df_plot_stats %>% filter(frameId>=pass_start & frameId<=pass_end), by=c("frameId")) %>%
      {ggplot()+
          geom_area(data=.,aes(x=frameId,y=area, fill=displayName), position = position_stack())+
          geom_text(data=distinct(.,position_type_1,play_start), aes(x=play_start,y=.9*play_area,label="Ball Snap"),angle=90,size=3, hjust=-.01,vjust=1.5)+
          geom_text(data=distinct(.,position_type_1,pass_start), aes(x=pass_start,y=.9*play_area,label="Pass Thrown"),angle=90,size=3, hjust=-.01,vjust=1.5)+
          geom_text(data=distinct(.,position_type_1,pass_end), aes(x=pass_end,y=.9*play_area,label="Pass Arrived"),angle=90,size=3, hjust=-.01,vjust=1.5)+
          geom_vline(data=distinct(.,position_type_1,pass_start,pass_start,pass_end), aes(xintercept=pass_start),linetype=3)+
          geom_vline(data=distinct(.,position_type_1,pass_end,pass_start,pass_end), aes(xintercept=pass_end),linetype=3)+
          geom_vline(data=distinct(.,position_type_1,play_start,pass_start,pass_end), aes(xintercept=play_start),linetype=1)+
          geom_vline(data=distinct(.,position_type_1,pass_start,pass_start,pass_end), aes(xintercept=pass_start),linetype=3)+
          geom_vline(data=distinct(.,position_type_1,pass_end,pass_start,pass_end), aes(xintercept=pass_end),linetype=3)+
          theme_bw()+ labs(
            title = play_title,
            caption = "Source: nflgisR"
          ) +
          facet_grid(.~position_type_1)}+
      ggsave(paste0(file_dir,"VA_stacked-",pl_id,".png"),width = 15,height = 7 )
  }
  if("players" %in% type |type=="all"){
    ##Players
    df_plot_vor %>% mutate(displayName=paste0(jerseyNumber,"\n",displayName)) %>%
      # semi_join(df_plot_stats %>% filter(frameId>=pass_start & frameId<=pass_end), by=c("frameId")) %>%
      {ggplot()+
          geom_area(data=.,aes(x=frameId,y=area, fill=position_type_1))+
          geom_vline(data=distinct(.,displayName,play_start,pass_start,pass_end), aes(xintercept=play_start),linetype=1)+
          geom_vline(data=distinct(.,displayName,pass_start,pass_start,pass_end), aes(xintercept=pass_start),linetype=3)+
          geom_vline(data=distinct(.,displayName,pass_end,pass_start,pass_end), aes(xintercept=pass_end),linetype=3)+
          facet_grid(.~displayName)}+ggsave(paste0(file_dir,"VA-players-",pl_id,".png"),width = 15,height = 7 )
   }
  if("teams" %in% type |type=="all"){

    df_plot_vor %>%group_by(position_type_1,frameId) %>%
      summarise(area=sum(area),play_start=first(play_start),pass_start=first(pass_start), pass_end=first(pass_end)) %>%ungroup() %>%
      {ggplot()+
          geom_area(data=.,aes(x=frameId,y=area, fill=position_type_1),position = position_stack())+
          geom_vline(data=., aes(xintercept=play_start),linetype=1)+
          geom_vline(data=., aes(xintercept=pass_start),linetype=3)+
          geom_vline(data=., aes(xintercept=pass_end),linetype=3)+
          geom_vline(data=distinct(.,position_type_1,pass_end,pass_start,pass_end), aes(xintercept=pass_end),linetype=3)+
          theme_bw()+ labs(
            title = play_title,
            caption = "Source: NFL Data Bowl 2020"
          ) }+ggsave(paste0(file_dir,"VA-team-",pl_id,".png"),width = 15,height = 7 )
    }
}
