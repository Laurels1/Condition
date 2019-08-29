#' Conncet to an internal NEFSC database.
#'
#'A utility function enabling the user to connect to an internal database provided the user has permissions and
#'posesses a valid username and password.
#'
#' For this to work, you need an oracle client installed.
#' Tested with Oracle instantClient_12_2 installed
#' Note: if you use 64 bit Rstudio then you need a 64 bit client
#' Note: if you use 32 bit Rstudio then you need a 32 bit client
#'
#'
#' @param server  name of the server
#' @param uid  username of person with permissions
#' @return Object of class "RODBC" with attibutes found in \code{\link{RODBC}} . This object can be passed as an argument to
#' most functions in the cfdbs package. Failure results in termination with an error message
#'
#' @section Warning:
#' 3 failed attempts and you will be locked out of the system.
#' @seealso \code{\link{RODBC}}
#'
#' @examples
#' \dontrun{
#' connectToDatabase(server="name_of_server",uid="individuals_username")
#'}
#' @export
#'

connect_to_database  <-  function(server,uid){
  # calls function for user to enter password
  pwd <- get_pwd(server)

  # connects to DB and catches errors and warnings
    chan <- tryCatch(
      {
        RODBC::odbcConnect(server,uid=uid,pwd=pwd)
        #message(paste0("Successfully connected to Database: ",server))
      }, warning=function(w) {
        if (grepl("logon denied",w)) {message("logon to server failed - Check username and password")}
        if (grepl("locked",w)) {message("logon to server failed - Account may be locked")}
        message(paste0("Can not Connect to Database: ",server))
        return()
      }, error=function(e) {
        message(paste0("Terminal error: ",e))
        return()
      }, finally = {

      }
    )
  # returns
  return(chan)
}

#getpassword

get_pwd <- function(server){
  pwd <- getPass::getPass(msg=paste0("Enter your password for ",server,":"),forcemask = FALSE)
  return(pwd)
}
