METSTAT
========
Package ini merupakan hasil komputerisasi dari uji-uji statistik yang ada pada matakuliah metode statistika 2 di Sekolah Tinggi Ilmu Statistik.

Instalasi Package
========
Package ini dapat kamu install dengan memasukkan command ini di R:
```{r}
install.packages("devtools")
devtools::install_github("ama12majid/metstat)
'''

Fungsi dalam Package
===============
Dalam package ini terdapat beberapa fungis untuk melakukan analisis seperti: 

Uji Rata-Rata 1 Populasi dengan Sampel Besar:
```{r}
mean1pop(data, mean.pop, var.pop, alpha)
```

Uji Rata-Rata 1 Populasi dengan Sampel Kecil:
```{r}
mean1pop.s(data, mean.pop, alpha)
```

Pengujian Rata-Rata 2 Populasi Sampel Besar (Varians diketahui):
```{r}
mean2pop(data1, data2, dif, var.pop1, var.pop2, alpha)
```

Pengujian hipotesis rata-rata 2 populasi sampel besar (varians tidak diketahui, var1=var2):
```{r}
mean2pop.s(data1, data2, dif, alpha)
```

Pengujian rata-rata 2 populasi, dengan jumlah sampel kecil, varians sama:
```{r}
mean2sample(data1, data2, dif, alpha)
```

Pengujian hipotesis rata-rata 2 populasi degan sampel berukuran kecil, dengan asumsi varians populasi untuk kedua populasi berbeda:
```{r}
mean2sample.s(data1, data2, dif, alpha)
```

Pengujian hipotesis rata-rata 2 populasi degan sampel berukuran kecil, dengan asumsi varians populasi untuk kedua populasi diketahui:
```{r}
mean2sample.sigma(data1, data2, dif, sigma1, sigma2, alpha)
```

Pengujian hipotesis rata-rata 2 populasi berpasangan:
```{r}
meancoupled(data1, data2, dif, alpha)
```

Pengujian hipotesis proporsi 1 populasi:
```{r}
proporsi1populasi(data, p0, alpha)
```

Pengujian proporsi 2 populasi:
```{r}
proporsi2populasi(data1, data2, dif, alpha)
```

Pengujian hipotesis varians 1 populasi:
```{r}
varians1pop(data, var.pop, alpha)
```

Pengujian hipotesis varians 2 populasi:
```{r}
var2pop(data1, data2, sigma1, sigma2, alpha)
```


Info lebih lanjut kunjungi blog [ini](http://letme-escape.blogspot.co.id) atau bisa email ke <dramateneigersmansa1@gmail.com>
