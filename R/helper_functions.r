
sfc_as_cols <- function(x, names = c("x","y")) {
  stopifnot(inherits(x,"sf") && inherits(sf::st_geometry(x),"sfc_POINT"))
  ret <- sf::st_coordinates(x)
  ret <- tibble::as_tibble(ret)
  stopifnot(length(names) == ncol(ret))
  x <- x[ , !names(x) %in% names]
  ret <- setNames(ret,names)
  dplyr::bind_cols(x,ret)
}


dfa_mod <- function(dat, m, R, covariate, just_testing = TRUE, data_wide = TRUE, ...)  {

  
  if(just_testing) {
    cntl.list <- list(minit = 200, maxit = 5200, allow.degen = TRUE, conv.test.slope.tol = 0.5)
    # cntl.list = list(minit = 200, 
    #                  maxit = 40000, allow.degen = TRUE,
    #                  abstol = 0.0001, conv.test.slope.tol = 0.5)
  }
  if(!just_testing){
    cntl.list <- list(minit = 200, maxit = 60000, allow.degen = FALSE,
                      abstol = 0.0001, conv.test.slope.tol = 0.1)
  }
  if(!data_wide){
    dat <- dat %>% 
      tidyr::pivot_wider(names_from = YEAR, values_from = cond) %>%
      tibble::column_to_rownames(var = "common_name") %>% 
      as.matrix
  }
  
  mod_list <- list(m = m, R = R)
  
  if(covariate == "none"){
    m1 <- MARSS(dat,
                model = mod_list,
                form = "dfa",
                z.score = FALSE,
                control = cntl.list)
  }
  if(covariate == "season_f"){

    m1 <- MARSS(dat,
                model = mod_list,
                form = "dfa",
                z.score = FALSE,
                covariates = season_f,
                control = cntl.list)
  }
  return(m1)
}




Z_maker <- function(n, m, nsites = 1) {
  if(!n%%nsites == 0)
    stop("nsites is not a multiple of n")
  n <- n/nsites
  
  # Set up default Z
  Z <- matrix(list(), nrow = n, ncol = m)
  # insert row (i) & col (j) indices
  for (i in seq(n)) {
    Z[i, ] <- paste("Z", i, seq(m), sep = "")
  }
  # set correct i,j values in Z to numeric 0
  if (m > 1) {
    for (i in 1:(m - 1)) {
      Z[i, (i + 1):m] <- 0
    }
  }
  
  return(do.call(rbind, replicate(nsites, Z, simplify = FALSE)))
}
