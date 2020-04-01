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

age_pull_from_svdbs<- function(uid) {
  
 # makes a connection
  sole <- connect_to_database("sole",uid=uid)
  qry <- c(
    "select u.cruise6 cruise6,stratum,tow,station,svspp,sex,length,age
from    union_fscs_svbio u, svdbs_cruises c
    where   u.cruise6 = c.cruise6 and
    season = 'FALL' and
    purpose_code = 10 and
    svspp in ('013','015','023','026','028','032','072','073','074','075','076',
'077','078','102','103','104','105','106','107','108','121','131','135','141',
    '143','145','155','164','193','197') and 
    age is NOT NULL);"
  )
  
  AgeSurvey <- RODBC::sqlQuery(sole, qry)

  return(AgeSurvey)
}
