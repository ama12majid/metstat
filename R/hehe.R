#uji proporsi (data biner, 1 berhasil, 0 gagal)

proporsi1populasi <- function(data, p0, alpha){
  n <- length(data)
  if (n>=30){
    # karaktaeristik
    pcap <- mean(data)
    zhit <- (pcap-p0)/sqrt(p0*(1-p0)/n)
    z1 <- qnorm(1-alpha/2)
    z2 <- qnorm(1-alpha)
    if (abs(zhit)<=z1){
      note1 <- "H0 not rejected"
    }else{
      note1 <- "H0 rejected"
    }
    if (zhit<=z2){
      note2 <- "H0 not rejected"
    }else{
      note2 <- "H0 rejected"
    }
    if (zhit>=-z2){
      note3 <- "H0 not rejected"
    }else{
      note3 <- "H0 rejected"
    }
    print("Uji proporsi 1 sampel besar")
    cat("P0= ", p0, "\n")
    cat("H0: p = ", p0, "==========", "\n")
    cat("Zobs= ", zhit, ". Critical Point= ", z1, ". Decision= ", note1, "\n")
    cat("H0: p > ", p0, "==========", "\n")
    cat("Zobs= ", zhit, ". Critical Point= ", z2, ". Decision= ", note2, "\n")
    cat("H0: p < ", p0, "==========", "\n")
    cat("Zobs= ", zhit, ". Critical Point= ", z2, ". Decision= ", note3, "\n")
  }else{
    print <- "Data minimal 30 sampel"
  }
}

proporsi2populasi <- function(data1, data2, dif, alpha){
  n1 <- length(data1)
  n2 <- length(data2)
  pcap1 <- mean(data1)
  pcap2 <- mean(data2)
  x1 <- sum(data1)
  x2 <- sum(data2)
  if (dif==0){
    #statistik uji
    pcap <- (x1+x2)/(n1+n2)
    zhit <- (pcap1 - pcap2)/sqrt(pcap*(1-pcap)*(1/n1+1/n2))
  }else{
    zhit <- ((pcap1 - pcap2)-dif)/sqrt((p1*(1-p1)/n1)+(p2*(1-p2)/n2))
  }
  z1 <- qnorm(1-alpha/2)
  z2 <- qnorm(1-alpha)
  if (abs(zhit)<=z1){
    note1 <- "H0 not rejected"
  }else{
    note1 <- "H0 rejected"
  }
  if (zhit<=z2){
    note2 <- "H0 not rejected"
  }else{
    note2 <- "H0 rejected"
  }
  if (zhit>=-z2){
    note3 <- "H0 not rejected"
  }else{
    note3 <- "H0 rejected"
  }
  print("Uji proporsi 1 sampel besar")
  cat("H0: p1-p2 = ", dif, "==========", "\n")
  cat("Zobs= ", zhit, ". Critical Point= ", z1, ". Decision= ", note1, "\n")
  cat("H0: p1-p2 > ", dif, "==========", "\n")
  cat("Zobs= ", zhit, ". Critical Point= ", z2, ". Decision= ", note2, "\n")
  cat("H0: p1-p2 < ", dif, "==========", "\n")
  cat("Zobs= ", zhit, ". Critical Point= ", z2, ". Decision= ", note3, "\n")
}
