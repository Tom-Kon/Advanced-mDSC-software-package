freqdiffFunc <- function(reactiveInputs) {
  
  dCpRHF <- reactiveInputs$dCpRHF
  dCpTHF <- reactiveInputs$dCpTHF
  ARHF <- reactiveInputs$ARHF
  ATHF <- reactiveInputs$ATHF
  BRHF <- reactiveInputs$BRHF
  BTHF <- reactiveInputs$BTHF
  DRHF <- reactiveInputs$DRHF
  DTHF <- reactiveInputs$DTHF
  TTHF <- reactiveInputs$TTHF
  TRHF <- reactiveInputs$TRHF
  k <- reactiveInputs$k
  kcorr <- reactiveInputs$kcorr
  
  intlimithigher <- reactiveInputs$intlimithigher
  intlimitlower <- reactiveInputs$intlimitlower
  
  
  
  xmin <- reactiveInputs$xmin
  xmax <- reactiveInputs$xmax
  
  x0 <- 0.5*(TTHF+ TRHF)
  m <- 0.5*(TRHF - TTHF)
  S2 <- DTHF-BTHF
  S1 <- DRHF-BRHF
  
  km <- k*m

  if (length(xmin) > 0 && length(xmax) > 0) {
    x_list <- seq(xmin, xmax, length.out = 10000)
  } else {
    x_list <- seq(TTHF - 50, TRHF + 50, length.out = 10000)
  }
  
  a <- exp(k*x0)*(S2*exp(k*m)-S1*exp(-k*m))
  b <- exp(k*x0)*(dCpTHF*exp(k*m)-dCpRHF*exp(-k*m)+ x0*(exp(k*m)*S2 - exp(-k*m)*S1) + m*(exp(k*m)*S2+exp(-k*m)*S1))
  c <- exp(-2*k*m)
  v <- exp(k*(x0+m))
  # missing_area_num <- dCpTHF-dCpRHF+S1*TRHF+S2*TTHF
  # print(missing_area_num)
  # 
  # missing_area_func <- function(x) {
  #   missing_area_num/(1+exp(-k*(x-x0+m)))*(1+exp(-k*(x-x0-m)))
  # } 
  # 
  # int_missing <- integrate(missing_area_func, intlimitlower, intlimithigher)
  # print(int_missing)
  # 
    
  
  y_full_func <- function(x) {
    (ATHF +BTHF * (x - TTHF) +(dCpTHF + (x - TTHF) * (DTHF - BTHF)) /(1 + exp(-k * (x - TTHF)))) - (ARHF + BRHF * (x - TRHF) +(dCpRHF + (x - TRHF) * (DRHF - BRHF)) /(1 + exp(-k * (x - TRHF))))
  }
  
  y_simple_func <- function(x) {
    (a*x+b)/(exp(k*x)*(1+c*v*exp(-k*x))*(1+v*exp(-k*x)))
  }
  
  
  y_full <- y_full_func(x_list)
  
  base_corr_x1 <- which.min(abs(x_list-intlimitlower))
  base_corr_x2 <- which.min(abs(x_list-intlimithigher))

  base_corr_y1 <- y_full[base_corr_x1]
  base_corr_y2 <- y_full[base_corr_x2]
  base_corr_slope <- (base_corr_y2-base_corr_y1)/(intlimithigher-intlimitlower)
  base_corr_intercept <- y_full[base_corr_x1] - base_corr_slope*intlimitlower

  baseline <- base_corr_slope*x_list + base_corr_intercept
  # baseline <- base_corr_y1+(base_corr_y2-base_corr_y1)/(1+exp(-kcorr*(x_list-x0)))
  
  y_simple <- y_simple_func(x_list)
    
  int_y_full <- integrate(y_full_func, intlimitlower, intlimithigher)
  int_y_simple <- integrate(y_simple_func, intlimitlower, intlimithigher)
  
  y_full_baselinecorr_func <- function(x) {
    (ATHF +BTHF * (x - TTHF) +(dCpTHF + (x - TTHF) * (DTHF - BTHF)) /(1 + exp(-k * (x - TTHF)))) - (ARHF + BRHF * (x - TRHF) +(dCpRHF + (x - TRHF) * (DRHF - BRHF)) /(1 + exp(-k * (x - TRHF))))-(base_corr_y1+(base_corr_y2-base_corr_y1)/(1+exp(-kcorr*(x-x0))))
  }

  
  
  y_full_basecorr <- y_full_baselinecorr_func(x_list)
  int_y_full_basecorr <- integrate(y_full_baselinecorr_func, intlimitlower, intlimithigher)
  
  
  
  int_derived <- b*log(c)/(k*v*(c-1)) + a*(0.5*log(c)^2+log(c)*log(v))/(v*k^2*(c-1))
  int_derived_simple <- 2*m*(dCpTHF+S2*m)
  int_derived_supersimple <- 2*m*dCpTHF
  
  param <- data.frame(
    dCpRHF = dCpRHF, 
    dCpTHF = dCpTHF, 
    ARHF = ARHF, 
    ATHF = ATHF, 
    BRHF = BRHF, 
    BTHF = BTHF, 
    DRHF = DRHF, 
    DTHF = DTHF, 
    TTHF = TTHF, 
    TRHF = TRHF, 
    k = k,
    km = km,
    intlimitlower = intlimitlower,
    intlimithigher = intlimithigher
  )
  
  int_res <- data.frame(
    int_y_full = int_y_full$value,
    int_y_full_basecorr = int_y_full_basecorr$value,
    int_y_simple = int_y_simple$value, 
    int_derived = int_derived,
    int_derived_simple = int_derived_simple,
    int_derived_supersimple = int_derived_supersimple
  )
  
  int_res_dev <- data.frame(
    dev_y_full = abs(int_y_full_basecorr$value-int_y_full$value)/int_y_full_basecorr$value*100,
    dev_y_baseline = "0",
    dev_y_simple = abs(int_y_full_basecorr$value-int_y_simple$value)/int_y_full_basecorr$value*100,
    dev_int_derived = abs(int_y_full_basecorr$value-int_derived)/int_y_full_basecorr$value*100,
    dev_int_derived_simple = abs(int_y_full_basecorr$value-int_derived_simple)/int_y_full_basecorr$value*100,
    dev_int_derived_supersimple = abs(int_y_full_basecorr$value-int_derived_supersimple)/int_y_full_basecorr$value*100,
    S1 = S1,
    S2 = S2,
    frac = dCpTHF/dCpRHF
  )
  
  print(int_res)
  
  currentRes <- list(
    param = param, int_res = int_res, int_res_dev = int_res_dev
  )

  
  resdf <- data.frame(
    x_list = x_list,
    y_full = y_full,
    y_full_basecorr = y_full_basecorr,
    y_simple = y_simple,
    intlimithigher = intlimithigher,
    intlimitlower = intlimitlower,
    baseline
  )

  reslist <- list(resdf = resdf, currentRes = currentRes)

  return(reslist)
}






freqdiffgraphFunc <- function(res, id) {
  
  p <- ggplot(res) +
    geom_line(
      aes(x = x_list, y = y_full),
      color = "blue",
      linewidth = 1.3
    ) +
    geom_line(
      aes(x = x_list, y = y_simple),
      color = "red",
      linewidth = 1.3
    ) +
    geom_line(
      aes(x = x_list, y = y_full_basecorr),
      color = "green",
      linewidth = 1.3
    ) +
    geom_line(
      aes(x = x_list, y = baseline),
      color = "green",
      linewidth = 1.3
    ) +
    geom_vline(xintercept = res$intlimitlower, linetype = "dotted") +
    geom_vline(xintercept = res$intlimithigher, linetype = "dotted") +
    labs(
      title = "LOESS smoothed Total heat flow",
      x = "Temperature (°C)",
      y = "Total heat flow (W/g)"
    ) +
    theme_minimal(base_size = 18)
  
  p <- ggplotly(
    p,
    source = id
  )
  
  p <- event_register(
    p,
    "plotly_relayout"
  )
  
  p
}