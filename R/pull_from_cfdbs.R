#'Data pull from SVDBS
#'
#'larger description of data pull
#'
#'@param uid character string. Username of individuals oracle account
#'
#'@return data.frame
#'\item{survey}{raw data pull from SVDBS}
#'
#' @export

pull_from_cfdbs<- function(uid) {
  
 # makes a connection
  nova <- connect_to_database("nova",uid=uid)
  qry <- c(
    "select link, docn, subtrip, port, state, year, permit, month, day, 
         negear, negear2, toncl1, area, cf_ntrips, mainspp, 
    mesh_in, cf_df, cf_da, trplndlb
    from connection to oracle
    (select link, docn, subtrip, port, state, year, permit, month, day, 
    negear, negear2, toncl1, area, ntrips cf_ntrips, mainspp,
    mesh mesh_in, df cf_df, da cf_da, trplndlb
    from cfdett&sysparm.AA) ;"
  )
  
  commdata <- RODBC::sqlQuery(nova, qry)

  return(commdata)
}
