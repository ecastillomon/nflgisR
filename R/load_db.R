#' load_db
#'
#' @param src_dir specify the working directory of the database or csvs with players, play index, games and tracking data
#' @param DB csv or sql , if sql is needed run create_db first
#' @param week number to load when csv or NULL to fetch all
#'
#'  Will load all necessary dataframes to get information neccesary.
#'  When ran with csv, will load everything in memory, so only one week is loaded on default (1), this means default
#'  behaviour will look for week1.csv . If ran with parameter week=NULL, all files with prefix "week" available will be loaded.
#'  If the SQLite database is built (recommended), all connections are loaded using lazy loading, and will be much less hard on the RAM.
#'
#'
#'
#'  If experimenting with data not from databowl, here are the minimum requirements in each, for it to work without modifications.
#'  Other sports could work, but you would have to look for the ball identifiers, and player positions and modify accordingly
#'  Plays Index: gameId, playid
#'  Players: nflId, position
#'  Games: gameId,  homeTeamAbbr, visitorTeamAbbr, week
#'  Tracking Data: time, x, y,  a, dis, o, dir, event, nflId, displayName, jerseyNumber,  frameId, team, gameId, playId, playDirection
#'
#'  plays_index %>% filter(.data$gameId%in% gameid & playId%in% playid)
#' @return all connections loaded as lazy dataframes when run with DB, all databases loaded from csv.
#' @import dplyr
#' @export
#' @examples
load_db=function(src_dir="data", DB="csv", week=1){
  if(DB=="sql"){
    con = DBI::dbConnect(RSQLite::SQLite(), paste0(src_dir,"data/nflgisR.db"))
    df_plays= tbl(con, "df_plays")
    games=tbl(con, "games")
    plays_index=tbl(con,"plays_index")
    players=tbl(con,"players")
  }else{
    load_plays(src_dir)
    load_games(src_dir)
    load_players(src_dir)
    load_tracking_data(src_dir=src_dir,week)
  }

}




#' load_plays
#'
#' @param src_dir specify the working directory of the necesary files
#' Point to directory with play index
#'
#'
#' @return all connections loaded as lazy dataframes
#' @importFrom vroom vroom
#' @importFrom rlang .data
#'
#' @examples
#' load_plays("../roboShutdown/data")
load_plays=function(src_dir="data"){
  plays_index<<-vroom::vroom(paste0(src_dir,"/","plays.csv"))
  }

#' load_games
#'
#' @param src_dir specify the working directory of the necesary files
#' Point to directory with game index
#'
#'
#' @return game index
#' @importFrom vroom vroom
#' @importFrom rlang .data
#'
#' @examples
#' load_games("../roboShutdown/data")
load_games=function(src_dir="data"){
  games<<-vroom::vroom(paste0(src_dir,"/","games.csv"))
}

#' load_players
#'
#' @param src_dir specify the working directory of the necesary files

#' @return players_index
#' @import dplyr
#' @importFrom vroom vroom
#' @importFrom rlang .data
#'
#' @examples
#' load_players("../roboShutdown/data")
load_players=function(src_dir="data"){
  players<<-vroom::vroom(paste0(src_dir,"/","players.csv")) %>%
    mutate(position_type_1=case_when(grepl("CB|SS|MLB|FS|OLB|DE|LB|ILB|DB|^S$|NT|DT",.data$position)~"defense",
                                     grepl("WR|QB|TE|RB|FB|HB|LB|ILB|DB|^S$|NT|DT",.data$position)~"offense",
                                     grepl("^K$|^P$|LS",.data$position)~"special teams"))
}




#' load_tracking_data
#'
#' @param src_dir specify the working directory of the necesary files
#' @param week week to be loaded, can be any week in the folder or all .
#' @param file_prefix default prefix to tracking data filenames, default is week like the original 2020 databowl files
#' Week name must look like this: week[0-9]+ , example:week1.csv
#' if you want to use more than week at a time, you should look into creating a database and using dbplyr
#' If you plan on using different play by play data, you should make sure it has the following columns: nflId, GameId,playId
#'
#' @return all connections loaded as lazy dataframes
#' @importFrom vroom vroom
#' @importFrom rlang .data
#'
#' @examples
#' load_tracking_data()
load_tracking_data=function(src_dir="data",week=1, file_prefix="week"){
  df_plays<<-list.files(src_dir) %>%
    {if(is.null(week)){ .[grepl(file_prefix,.)] }
      else{paste0(src_dir,"/",file_prefix,week,".csv")}} %>%
    purrr::map_dfr(function(x){
      vroom::vroom(paste0(x))
    })

}
