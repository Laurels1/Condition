
sfc_as_cols <- function(x, names = c("x","y")) {
  stopifnot(inherits(x,"sf") && inherits(sf::st_geometry(x),"sfc_POINT"))
  ret <- sf::st_coordinates(x)
  ret <- tibble::as_tibble(ret)
  stopifnot(length(names) == ncol(ret))
  x <- x[ , !names(x) %in% names]
  ret <- setNames(ret,names)
  dplyr::bind_cols(x,ret)
}


dfa_mod <- function(dat, m, R, cov_v = NA, just_testing = TRUE, data_wide = TRUE, safe = FALSE, ...)  {

  
  if(just_testing) {
    # cntl.list <- list(minit = 200, maxit = 5200, allow.degen = TRUE, conv.test.slope.tol = 0.5)
    cntl.list = list(minit = 200,
                     maxit = 40000, allow.degen = TRUE,
                     abstol = 0.0001, conv.test.slope.tol = 0.5,
                     safe = safe)
  }
  if(!just_testing){
    cntl.list <- list(minit = 200, maxit = 60000, allow.degen = FALSE,
                      abstol = 0.0001, conv.test.slope.tol = 0.1,
                      safe = safe)
  }
  if(!data_wide){
    dat <- dat %>% 
      tidyr::pivot_wider(names_from = YEAR, values_from = cond) %>%
      tibble::column_to_rownames(var = "common_name") %>% 
      as.matrix
  }
  
  mod_list <- list(m = m, R = R)
  
  if(is.na(cov_v)){
    m1 <- MARSS(dat,
                model = mod_list,
                form = "dfa",
                z.score = FALSE,
                control = cntl.list)
  }
  if(!is.na(cov_v)){

    cov_v <- as.vector(cov_v)
    
    
    m1 <- MARSS(dat,
                model = mod_list,
                form = "dfa",
                z.score = FALSE,
                covariates = cov_v,
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



dfa_plot <- function(object, EPU = NULL) {
  
  marss_obj <- MARSSparamCIs(object)
  
  name_code <- rownames(object$marss$data)
  m <- nrow(object$states.se)
  
  if(!is.null(EPU)){
    title_name <- switch(EPU,
                         GOM = "Gulf of Maine",
                         MAB = "Mid-Atlantic Bight",
                         SS = "Scotian Shelf",
                         GB = "Georges Bank")
  }
  if(is.null(EPU)){
    title_name = ""
  }

  # the rotation matrix for the Z
  z <- coef(marss_obj, type = "Z")
  H.inv <- varimax(z)$rotmat
  
  # Get the Z, upZ, lowZ
  z.low <- coef(marss_obj, type = "Z", what="par.lowCI")
  z.up <- coef(marss_obj, type = "Z", what="par.upCI")
  
  z.rot <- z %*% H.inv
  z.rot.up <- z.up %*% H.inv
  z.rot.low <- z.low %*% H.inv

  
  factor_df <- data.frame(name_code = rep(name_code, m),
                          trend = as.factor(rep(paste0("Trend ", 1:m), each = length(name_code))),
                          Z = as.vector(z),
                          Zup = as.vector(z.up),
                          Zlow = as.vector(z.low)) %>%
    mutate(spp = gsub("_",  " ", name_code),
           spp = ifelse(grepl("america?|atlant?|acadia?", spp),
                        stringr::str_to_sentence(spp),
                        spp),
           shape_id = 19, #ifelse(Zup < 0 | Zlow > 0, 19, 1),
           # fill_id = ifelse(Zup < 0 | Zlow > 0, "black", "grey70"),
           # fill_id = ifelse(trend == "Trend 1", "#264CFF", "#FF420E"), 
           # color_id = fill_id,
           alpha_id = ifelse(Zup < 0 | Zlow > 0, .9, .8),
    ) %>% 
    arrange(desc(spp)) %>% 
    mutate(spp = factor(spp, levels = unique(spp)))
  
  
  color_ramp <- RColorBrewer::brewer.pal(n = m, name = "Set1")
  names(color_ramp) <- levels(factor_df$trend)
  scale_colour_manual(values = color_ramp)
  
  
  fplot <-  ggplot(data = factor_df,
                   aes(x = spp,
                       ymin = Zlow,
                       ymax = Zup,
                       y = Z)) +
    geom_hline(yintercept = 0, color = "black") +
    geom_hline(yintercept = 0.2, color = "grey60", linetype = "dashed") +
    geom_hline(yintercept = -0.2, color = "grey60", linetype = "dashed") +
    geom_pointrange(aes(shape = shape_id, fill = trend, color = trend, alpha = alpha_id), show.legend = FALSE)+ #, position = position_dodge(width = 0.5)) +
    # facet_wrap(~trend) +
    coord_flip() +
    scale_shape_identity() +
    scale_fill_manual(values = color_ramp) +
    scale_color_manual(values = color_ramp) +
    labs(title = title_name,
         x = "", y = "factor loadings") +
    theme_minimal()
  
  colnames(object$marss$data)[1] <- 1991 ### THIS IS WRONG, FIX IT
  
  trend_df <- tsSmooth(object, type = "xtT", interval = "confidence", 
                       level = .95) %>% 
    mutate(
      trend = gsub(pattern = "^X", "Trend ", .rownames),
      year = rep(as.numeric(colnames(object$marss$data)), m))#,
      # fill_id = ifelse(trend == "Trend 1", "#264CFF", "#FF420E"), 
      # color_id = fill_id)
  
  
  tplot <- ggplot(data = trend_df,
                  aes(x = year,
                      ymin = .conf.low,
                      ymax = .conf.up,
                      y = .estimate)) +
    geom_hline(yintercept = 0, color = "black", alpha = 0.5) +
    geom_line(aes(color = trend)) +
    geom_ribbon(aes(fill = trend), alpha = 0.6, show.legend = FALSE)+ #, position = position_dodge(width = 0.5)) +
    # facet_wrap(~ trend, ncol = m) +
    labs(x = "", y = "Estimate") +
    scale_fill_manual(values = color_ramp) +
    scale_color_manual(values = color_ramp) +
    theme_minimal()
  
  tfplot <- fplot/tplot + plot_layout(heights = c(3, 1))
  #   
  return(tfplot)
  
}

loadings_df <- function(object){

  marss_obj <- MARSSparamCIs(object)
  name_code <- rownames(object$marss$data)
  m <- nrow(object$states.se)
  
  # if(!is.null(EPU)){
  #   title_name <- switch(EPU,
  #                        GOM = "Gulf of Maine",
  #                        MAB = "Mid-Atlantic Bight",
  #                        SS = "Scotian Shelf",
  #                        GB = "Georges Bank")
  # }
  # if(is.null(EPU)){
  #   title_name = ""
  # }
  # 
  # the rotation matrix for the Z
  z <- coef(marss_obj, type = "Z")
  H.inv <- varimax(z)$rotmat
  
  # Get the Z, upZ, lowZ
  z.low <- coef(marss_obj, type = "Z", what="par.lowCI")
  z.up <- coef(marss_obj, type = "Z", what="par.upCI")
  
  z.rot <- z %*% H.inv
  z.rot.up <- z.up %*% H.inv
  z.rot.low <- z.low %*% H.inv
  
  factor_df <- data.frame(name_code = rep(name_code, m),
                          trend = rep(paste0("Trend ", 1:m), each = length(name_code)),
                          Z = as.vector(z),
                          Zup = as.vector(z.up),
                          Zlow = as.vector(z.low)) %>%
    mutate(spp = gsub("_",  " ", name_code),
           spp = ifelse(grepl("america?|atlant?|acadia?", spp),
                        stringr::str_to_sentence(spp),
                        spp)
           
           # shape_id = 19, #ifelse(Zup < 0 | Zlow > 0, 19, 1),
           # fill_id = ifelse(Zup < 0 | Zlow > 0, "black", "grey70"),
           # fill_id = ifelse(trend == "Trend 1", "#264CFF", "#FF420E"), 
           # color_id = fill_id,
           # alpha_id = ifelse(Zup < 0 | Zlow > 0, .9, .8),
    ) %>% 
    arrange(desc(spp)) %>% 
    mutate(spp = factor(spp, levels = unique(spp)))
  return(factor_df)
}


