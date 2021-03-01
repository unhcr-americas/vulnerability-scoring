varSelection <- function(data, miss_thr=2/3, gamma_thr=0.7, beta_thr=c(-3,3), m=10, crit_thr=0.95,
                         Theta_range=c(-15, 15), qsel=0.25, ...) {
  
  #data: dataset (dichotomous variable must be 0/1 coded)
  #miss_thr = threshold for missing values (default 2/3)
  #gamma_thr = threshold for item discrimination (default 0.7)
  #beta_thr = threshold for beta parameters (default [-3,3])
  #m = max no. of categories for discrete variables, otherwise it is considered as continuous
  #crit_thr = threshold for "critical items": obs. response rate concentrated in a certain category (def. 0.95)
  #Theta_range = latent trait range (standard normal distributed) for evaluating the item/test information (def. [-15,15])
  #qsek = quantile for the selection based on item information proportion on the whole test information (def. 0.25 = 1st quartile)
  #... = argument for 'mirt' function within IRT selection
  
  library(mirt)
  
  nvar <- ncol(data) #no. of variables
  n <- nrow(data) #no. of statistical units (households)
  

  out <- data.frame(ID=1:nvar, name=names(data), stringsAsFactors=FALSE)
  miss <- apply(data, 2, function(x) sum(1*is.na(x)))
  miss_prop <- round(miss/n, 6)
  ncat <- apply(data, 2, function(x) length(unique(na.omit(x))))
  mmin <- apply(data, 2, min, na.rm=TRUE)
  mmax <- apply(data, 2, max, na.rm=TRUE)
  mmean <- apply(data, 2, mean, na.rm=TRUE)
  out <- data.frame(out, miss_prop, ncat, min=mmin, max=mmax, mean=mmean)
  rownames(out) <- NULL
  # browser()
  # check for dichotomous data
  flag <- (ncat == 2 & (mmin != 0 | mmax != 1))
  if (any(flag)) stop("Check dichotomous data: they must be 0/1 coded")
  
  # preliminary selection: missing values and continuous variables
  cat("*****************Preliminary selection*****************\n")
  cat("- Missing proportion threshold", miss_thr, "\n")
  cat("- Variables with number of response categories greater than", m, "are removed\n")
  cat("......\n")
  cont <- ifelse(out$ncat > m | out$max > m, 1, 0)
  z <- which(out$miss_prop <= miss_thr & cont != 1)
  out$cont <- cont
  out$miss <- 1*(out$miss_prop >= miss_thr)
  out2 <- out[z, ]
  
  # critical items
  data_sub1 <- subset(data, select=out2$ID)
  tab <- apply(data_sub1, 2, function(x) prop.table(table(x, useNA="always")))
  crit_item <- sapply(tab, function(x) 1*(max(x)>=crit_thr))
  out2 <- data.frame(out2, crit_item)
  nvar_prel <- nrow(out2)
  cat("End preliminary selection\n")
  cat("- ", nvar_prel, "variables out of", nvar, "are retained\n")
  cat("- ", sum(crit_item), "are \"critical\" items\n")
  # browser()
  # model-based selection
  cat("\n*****************IRT selection*****************\n")
  modIRT <- mirt(data_sub1, model=1, itemtype="graded", ...)
  params <- coef(modIRT, simplify=TRUE, IRTpars=TRUE)$items
  out2$gamma <- params[, 1] #discrim parameters
  beta <- params[, 2:ncol(params)] #"difficulty" parameter(s)
  colnames(beta) <- paste("beta", 1:ncol(beta), sep="")
  out2 <- data.frame(out2, beta)
  
  #item information 
  Theta <- matrix(seq(Theta_range[1], Theta_range[2], by = .1))
  item_info <- matrix(NA, nrow=length(Theta), ncol=nvar_prel)
  for (j in 1:nvar_prel) {
    extr <- extract.item(modIRT, item=j)
    item_info[, j] <- iteminfo(extr, Theta)
   }  
  item_infotot <- colSums(item_info)
  
  # test information
  test_info <- testinfo(modIRT, Theta)
  test_infotot <- sum(test_info)
  out2$prop_info <- item_infotot/test_infotot*100
  
  # sel on gamma
  out2$gamma_sel <- ifelse(out2$gamma >= gamma_thr, "keep", "drop")
  
  # sel on betas
  beta_med <- apply(beta, 1, median, na.rm=TRUE)
  out2$beta_sel <- ifelse(beta_med >= beta_thr[1] & beta_med <= beta_thr[2], "keep", "drop")
  
  # sel on information (prop >= 1/n.items)
  out2$info_sel <- ifelse(out2$prop_info >= (1/nvar_prel)*100, "keep", "drop")
  
  # sel on information (prop >= quantile)
  q <- quantile(out2$prop_info, qsel)
  out2$info_selQ <- ifelse(out2$prop_info >= q, "keep", "drop")
  
  # Final proposal: at least 2 criteria out of 4
  sel <- subset(out2, select=c("gamma_sel", "beta_sel", "info_sel", "info_selQ"))
  nkeep <- apply(sel, 1, function(x) sum(1*(x == "keep")))
  final_sel <- rep(NA, nvar_prel)
  final_sel <- ifelse(nkeep < 2, "drop", final_sel)
  final_sel <- ifelse(nkeep > 2, "keep", final_sel)
  final_sel <- ifelse(nkeep == 2 & out2$crit_item == 1, "drop", final_sel)
  final_sel <- ifelse(nkeep == 2 & out2$crit_item == 0, "keep", final_sel)
  
  out2$final_sel <- final_sel
  cat("End IRT selection\n")

  return(list(prel=out, final=out2))
  
}
