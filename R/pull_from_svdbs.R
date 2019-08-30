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

pull_from_svdbs<- function(uid) {
  
 # makes a connection
  sole <- connect_to_database("sole",uid=uid)
  qry <- c(
    "select b.cruise6,b.stratum,b.tow,b.station,
  s.est_year year,season, est_month month,est_day day,
  substr(est_time,1,2)||substr(est_time,4,2) time,
  round(substr(beglat,1,2) + (substr(beglat,3,7)/60),6) beglat,
  round(((substr(beglon,1,2) + (substr(beglon,3,7)/60)) * -1), 6) beglon,
  setdepth,surftemp, bottemp,
  b.svspp,logged_species_name, sex,length,age,maturity,indid,indwt,stom_volume,stom_wgt, expcatchwt, expcatchnum
  from union_fscs_svbio b, union_fscs_svcat p, union_fscs_svsta s, svdbs_cruises c
  where
  b.svspp in ('013','015','023','026','028','032','072','073','074','075','076','077','078','102','103','104','105','106','107','108','121','131','135','141','143','145','155','164','193','197') and
  (b.cruise6=s.cruise6) and
  (c.cruise6=b.cruise6) and
  (p.cruise6=c.cruise6) and
  (p.stratum=b.stratum) and
  (b.stratum=s.stratum) and
  (p.station=b.station) and
  (b.station=s.station) and
  (p.svspp=b.svspp) and
  (p.tow=b.tow) and
  (b.tow=s.tow) ;"
  )
  
  survey <- RODBC::sqlQuery(sole, qry)

  return(survey)
}
