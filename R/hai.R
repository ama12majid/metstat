varians1pop <- function(data, var.pop, alpha){
  #karakteristik
  n <- length(data)
  s2 <- var(data)
  #statistik uji
  chishit <- (n-1)*s2/var.pop
  #titik kritis
  chisq1 <- qchisq(1-alpha/2, n-1)
  chisq2 <- qchisq(1-alpha, n-1)
  chisq3 <- qchisq(alpha, n-1)
  #uji
  if (chishit<=chisq1){
    note1 <- "H0 not rejected"
  }else{
    note1 <- "H0 rejected"
  }

  if (chishit<=chisq2){
    note2 <- "H0 not rejected"
  }else{
    note2 <- "H0 rejected"
  }

  if (chishit>=chisq3){
    note3 <- "H0 not rejected"
  }else{
    note3 <- "H0 rejected"
  }

  print("Varian Test 1 Populations ========")
  cat("Population varians= ", var.pop, ". Sample= ", n, "\n")
  cat("H0: var = ", var.pop, "========== chis.hit > chis(alpha/2)", "\n")
  cat("Chi-Square.obs= ", chishit, ". Critical Point= ", chisq1, ". Decision= ", note1, "\n")
  cat("H0: var > ", var.pop, "========== chis.hit > chis(alpha)", "\n")
  cat("Chi-Square.obs= ", chishit, ". Critical Point= ", chisq2, ". Decision= ", note2, "\n")
  cat("H0: var < ", var.pop, "========== chis.hit < chis(1-alpha)", "\n")
  cat("Chi-Square.obs= ", chishit, ". Critical Point= ", chisq3, ". Decision= ", note3, "\n")
}

var2pop <- function(data1, data2, sigma1, sigma2, alpha){
  n1 <- length(data1)
  n2 <- length(data2)
  var1 <- var(data1)
  var2 <- var(data2)
  #statistik uji
  fobs <- var1/var2*sigma2/sigma1
  #f
  f1 <- 1/(qf(alpha, n1-1, n2-1))
  f2 <- qf(1-alpha, n1-1, n2-2)
  f3 <- qf(alpha, n1-1, n2-1)
  #uji
  if (fobs>f1){note1 <- "H0 rejected"  }else{note1 <- "H0 not rejected"}
  if (fobs>f2){note2 <- "H0 rejected"  }else{note2 <- "H0 not rejected"}
  if (fobs>f3){note3 <- "H0 rejected"  }else{note3 <- "H0 not rejected"}
  # note
  print("Varian test 2 population ===============")
  cat("Varian pop 1= ", sigma1, ". Varian pop 2= ", sigma2, "\n")
  cat("H0: var1 = var ========== chis.hit > chis(alpha/2)", "\n")
  cat("Fobs= ", fobs, ". Critical Point= ", f1, ". Decision= ", note1, "\n")
  cat("H0: var1 > var2 ========== chis.hit > chis(alpha)", "\n")
  cat("Fobs= ", fobs, ". Critical Point= ", f2, ". Decision= ", note2, "\n")
  cat("H0: var1 < var2 ========== chis.hit < chis(1-alpha)", "\n")
  cat("Fobs= ", fobs, ". Critical Point= ", f3, ". Decision= ", note3, "\n")
}
