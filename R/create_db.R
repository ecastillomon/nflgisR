
#'
#' @examples
#' create_db
#'
#' @param src_dir
#' @param target_dir
#' @param target_name
#'
#' @return
#' @export
#' @import dplyr
#' @importFrom vroom vroom
#' @importFrom rlang .data
#'
#' @examples
#' create_db()
create_db=function(src_dir="data",target_dir="data",target_name="nflgisR"){

  if (!requireNamespace("DBI", quietly = TRUE) |(!requireNamespace("RSQLite", quietly = TRUE))) {
    print("DBI and RSQLite packages are needed, please install")
    break()
  }
  if (!dir.exists(target_dir)) {
    dir.create(target_dir)
  }
  df_all=list.files(src_dir) %>%
    .[grepl("week",.)] %>%
    purrr::map_dfr(function(x){
      vroom::vroom(paste0(src_dir,"/",x))
    })
  con <- DBI::dbConnect(RSQLite::SQLite(), paste0(target_dir,"/",target_name,".db"))

  DBI::dbWriteTable(con,"df_plays",df_all, overwrite=TRUE)

  plays=vroom::vroom(paste0(src_dir,"/","plays.csv"))
  DBI::dbWriteTable(con,"plays_index",plays)
  games=vroom::vroom(paste0(src_dir,"/","games.csv"))
  DBI::dbWriteTable(con,"games",games)
  players=vroom::vroom(paste0(src_dir,"/","players.csv")) %>%
    mutate(position_type_1=case_when(grepl("CB|SS|MLB|FS|OLB|DE|LB|ILB|DB|^S$|NT|DT",.data$position)~"defense",
                                     grepl("WR|QB|TE|RB|FB|HB|LB|ILB|DB|^S$|NT|DT",.data$position)~"offense",
                                     grepl("^K$|^P$|LS",.data$position)~"special teams"))
  DBI::dbWriteTable(con,"players",players,overwrite=TRUE)
}
