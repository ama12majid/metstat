# sample >30, var pop
mean1pop <- function(data, mean.pop, var.pop, alpha){
  xbar <- mean(data)
  n <- length(data)
  zhit <- (xbar-mean.pop)/(sqrt(var.pop/n))
  zalpha1 <- qnorm(1-alpha/2)
  zalpha2 <- qnorm(1-alpha)
  if (abs(zhit)<=zalpha1){
      note1 <- "H0 not rejected"
      }else{
      note1 <- "H0 rejected"
      }
  if (zhit<=zalpha2){
    note2 <- "H0 not rejected"
  }else{
    note2 <- "H0 rejected"
  }
  if (zhit>=-zalpha2){
    note3 <- "H0 not rejected"
  }else{
    note3 <- "H0 rejected"
  }
  print("Mean Test 1 Population (Sample >=30) ========")
  cat("Population mean= ", mean.pop, ". Population varian= ", var.pop, ". Sample= ", n, "\n")
  cat("H0: mu = ", mean.pop, "==========", "\n")
  cat("Zobs= ", zhit, ". Critical Point= ", zalpha1, ". Decision= ", note1, "\n")
  cat("H0: mu > ", mean.pop, "==========", "\n")
  cat("Zobs= ", zhit, ". Critical Point= ", zalpha2, ". Decision= ", note2, "\n")
  cat("H0: mu < ", mean.pop, "==========", "\n")
  cat("Zobs= ", zhit, ". Critical Point= ", zalpha2, ". Decision= ", note3, "\n")
}
# sample >30, var pop not knowed
mean1pop.s <- function(data, mean.pop, alpha){
  xbar <- mean(data)
  n <- length(data)
  s <- sqrt(var(data))
  thit <- (xbar-mean.pop)/(s/sqrt(n))
  t1 <- qt(1-alpha/2, n-1)
  t2 <- qt(1-alpha, n-1)
  if (abs(thit)<=t1){
    note1 <- "H0 not rejected"
  }else{
    note1 <- "H0 rejected"
  }
  if (thit<=t2){
    note2 <- "H0 not rejected"
  }else{
    note2 <- "H0 rejected"
  }
  if (thit>=-t2){
    note3 <- "H0 not rejected"
  }else{
    note3 <- "H0 rejected"
  }
  print("Mean Test 1 Population (<30 samples) ========")
  cat("Population mean= ", mean.pop, ". Sample= ", n, "\n")
  cat("H0: mu = ", mean.pop, "==========", "\n")
  cat("tobs= ", thit, ". Critical Point= ", t1, ". Decision= ", note1, "\n")
  cat("H0: mu > ", mean.pop, "==========", "\n")
  cat("tobs= ", thit, ". Critical Point= ", t2, ". Decision= ", note2, "\n")
  cat("H0: mu < ", mean.pop, "==========", "\n")
  cat("tobs= ", thit, ". Critical Point= ", t2, ". Decision= ", note3, "\n")
}

mean2pop <- function(data1, data2, dif, var.pop1, var.pop2, alpha){
  # karakteristik
  xbar1 <- mean(data1)
  xbar2 <- mean(data2)
  n1 <- length(data1)
  n2 <- length(data2)
  if (n1<30 | n2<30){
    print("Sample Less than 30")
  }else{
    # statistik uji
    zhit <- ((xbar1-xbar2)-dif)/sqrt((var.pop1/n1)+((var.pop2)/n2))
    # kritis
    z1 <- qnorm(1-alpha/2)
    z2 <- qnorm(1-alpha)
    #uji
    if (zhit>z1){
      note1 <- "H0 rejected"
    }else{
      note1 <- "H0 not rejected"
    }

    if (zhit>z2){
      note2 <- "H0 rejected"
    }else{
      note2 <- "H0 not rejected"
    }

    if (zhit<-z2){
      note3 <- "H0 rejected"
    }else{
      note3 <- "H0 not rejected"
    }
    #note
    print("Mean Test 2 Population (var 1 var 2 mentioned) ========")
    cat("Varian Pop1= ", var.pop1, ". Varian Pop2= ", var.pop2, "\n")
    cat("H0: mu - mu2 = ", dif, "==========", "\n")
    cat("Zobs= ", zhit, ". Critical Point= ", z1, ". Decision= ", note1, "\n")
    cat("H0: mu - mu2 > ", dif, "==========", "\n")
    cat("Zobs= ", zhit, ". Critical Point= ", z2, ". Decision= ", note2, "\n")
    cat("H0: mu - mu2 < ", dif, "==========", "\n")
    cat("Zobs= ", zhit, ". Critical Point= ", z2, ". Decision= ", note3, "\n")
  }
}

mean2pop.s <- function(data1, data2, dif, alpha){
  # karakteristik
  xbar1 <- mean(data1)
  xbar2 <- mean(data2)
  n1 <- length(data1)
  n2 <- length(data2)
  var1 <- var(data1)
  var2 <- var(data2)
  # cek data
  if (n1<30 | n2<30){
  }else{
    # statistik uji
    zhit <- ((xbar1-xbar2)-dif)/sqrt((var1/n1)+((var2)/n2))
    # kritis
    z1 <- qnorm(1-alpha/2)
    z2 <- qnorm(1-alpha)
    #uji
    if (zhit>z1){
      note1 <- "H0 rejected"
    }else{
      note1 <- "H0 not rejected"
    }

    if (zhit>z2){
      note2 <- "H0 rejected"
    }else{
      note2 <- "H0 not rejected"
    }

    if (zhit<-z2){
      note3 <- "H0 rejected"
    }else{
      note3 <- "H0 not rejected"
    }
    #note
    print("Mean Test 2 Population (var not know) ========")
    cat("Varian Sample 1= ", var1, ". Varian Sample 2= ", var2, "\n")
    cat("H0: mu - mu2 = ", dif, "==========", "\n")
    cat("Zobs= ", zhit, ". Critical Point= ", z1, ". Decision= ", note1, "\n")
    cat("H0: mu - mu2 > ", dif, "==========", "\n")
    cat("Zobs= ", zhit, ". Critical Point= ", z2, ". Decision= ", note2, "\n")
    cat("H0: mu - mu2 < ", dif, "==========", "\n")
    cat("Zobs= ", zhit, ". Critical Point= ", z2, ". Decision= ", note3, "\n")
  }
}


mean2sample <- function(data1, data2, dif, alpha){
  xbar1 <- mean(data1)
  xbar2 <- mean(data2)
  n1 <- length(data1)
  n2 <- length(data2)
  s1 <- var(data1)
  s2 <- var(data2)
  v <- n1+n2-2
  if (n1>=30 & n2>=30){
    print("Sample more than 30")
  }else{
    #thit
    sp <- sqrt(((n1-1)*s1+(n2-1)*s2)/(n1+n2-2))
    thit <- ((xbar1-xbar2)-dif)/(sp*sqrt(1/n1+1/n2))
    #kritis
    t1 <- qt(1-alpha/2,v)
    t2 <- qt(1-alpha, v)
    #uji
    if (abs(thit)>t1){
      note1 <- "H0 rejected"
    }else{
      note1 <- "H0 not rejected"
    }

    if (thit>t2){
      note2 <- "H0 rejected"
    }else{
      note2 <- "H0 not rejected"
    }

    if (thit<-t2){
      note3 <- "H0 rejected"
    }else{
      note3 <- "H0 not rejected"
    }
    #note
    print("Mean Test 2 Population (var1 = var2, sample<30) ========")
    cat("H0: mu - mu2 = ", dif, "==========", "\n")
    cat("tobs= ", thit, ". Critical Point= ", t1, ". Decision= ", note1, "\n")
    cat("H0: mu - mu2 > ", dif, "==========", "\n")
    cat("tobs= ", thit, ". Critical Point= ", t2, ". Decision= ", note2, "\n")
    cat("H0: mu - mu2 < ", dif, "==========", "\n")
    cat("tobs= ", thit, ". Critical Point= ", t2, ". Decision= ", note3, "\n")
  }
}

mean2sample.s <- function(data1, data2, dif, alpha){
  xbar1 <- mean(data1)
  xbar2 <- mean(data2)
  n1 <- length(data1)
  n2 <- length(data2)
  s1 <- var(data1)
  s2 <- var(data2)
  v <- (s1/n1+s2/n2)^2/((s1/n1)^2/(n1-1)+(s2/n2)^2/(n2-1))
  if (n1>=30 & n2>=30){
    print("Sample more than 30")
  }else{
    #thit
    thit <- ((xbar1-xbar2)-dif)/sqrt(s1/n1+s2/n2)
    #kritis
    t1 <- qt(1-alpha/2,v)
    t2 <- qt(1-alpha, v)
    #uji
    if (abs(thit)>t1){
      note1 <- "H0 rejected"
    }else{
      note1 <- "H0 not rejected"
    }

    if (thit>t2){
      note2 <- "H0 rejected"
    }else{
      note2 <- "H0 not rejected"
    }

    if (thit<-t2){
      note3 <- "H0 rejected"
    }else{
      note3 <- "H0 not rejected"
    }
    #note
    print("Mean Test 2 Population (var1 not equal with var2, sample<30) ========")
    cat("H0: mu - mu2 = ", dif, "==========", "\n")
    cat("tobs= ", thit, ". Critical Point= ", t1, ". Decision= ", note1, "\n")
    cat("H0: mu - mu2 > ", dif, "==========", "\n")
    cat("tobs= ", thit, ". Critical Point= ", t2, ". Decision= ", note2, "\n")
    cat("H0: mu - mu2 < ", dif, "==========", "\n")
    cat("tobs= ", thit, ". Critical Point= ", t2, ". Decision= ", note3, "\n")
  }
}

mean2sample.sigma <- function(data1, data2, dif, sigma1, sigma2, alpha){
  xbar1 <- mean(data1)
  xbar2 <- mean(data2)
  n1 <- length(data1)
  n2 <- length(data2)
  if (n1>=30 & n2>=30){
    print("Sample more than 30")
  }else{
    #thit
    thit <- ((xbar1-xbar2)-dif)/sqrt(sigma1/n1+sigma2/n2)
    #kritis
    t1 <- qnorm(1-alpha/2)
    t2 <- qnorm(1-alpha)
    #uji
    if (thit>t1){
      note1 <- "H0 rejected"
    }else{
      note1 <- "H0 not rejected"
    }

    if (thit>t2){
      note2 <- "H0 rejected"
    }else{
      note2 <- "H0 not rejected"
    }

    if (thit<-t2){
      note3 <- "H0 rejected"
    }else{
      note3 <- "H0 not rejected"
    }
    #note
    print("Mean Test 2 Population (var1 not equal with var2, sample<30) ========")
    cat("Varians Pop1 = ", sigma1, ". Varians Pop2= ", sigma2, "\n")
    cat("H0: mu - mu2 = ", dif, "==========", "\n")
    cat("Zobs= ", thit, ". Critical Point= ", t1, ". Decision= ", note1, "\n")
    cat("H0: mu - mu2 > ", dif, "==========", "\n")
    cat("Zobs= ", thit, ". Critical Point= ", t2, ". Decision= ", note2, "\n")
    cat("H0: mu - mu2 < ", dif, "==========", "\n")
    cat("Zobs= ", thit, ". Critical Point= ", t2, ". Decision= ", note3, "\n")
  }
}

meancoupled <- function(data1, data2, dif, alpha){
  n1 <- length(data1)
  n2 <- length(data2)
  if (n1==n2){
    d <- data1-data2
    dbar <- mean(d)
    sd <- var(d)
    #statistik uji
    thit <- (dbar-dif)/(sqrt(sd/n1))
    #t
    t1 <- qt(1-alpha/2, n1-1)
    t2 <- qt(1-alpha, n1-1)
    #Uji
    if (abs(thit)>t1){
      note1 <- "H0 rejected"
    }else{
      note1 <- "H0 not rejected"
    }
    if (thit>t2){
      note2 <- "H0 rejected"
    }else{
      note2 <- "H0 not rejected"
    }
    if (thit<-t2){
      note3 <- "H0 rejected"
    }else{
      note3 <- "H0 not rejected"
    }
    #note
    print("Mean Test 2 Coupled Population (n1=n2) ========")
    cat("H0: mu - mu2 = ", dif, "==========", "\n")
    cat("tobs= ", thit, ". Critical Point= ", t1, ". Decision= ", note1, "\n")
    cat("H0: mu - mu2 > ", dif, "==========", "\n")
    cat("tobs= ", thit, ". Critical Point= ", t2, ". Decision= ", note2, "\n")
    cat("H0: mu - mu2 < ", dif, "==========", "\n")
    cat("tobs= ", thit, ". Critical Point= ", t2, ". Decision= ", note3, "\n")
  }
}
