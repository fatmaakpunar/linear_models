#Örnek 3.7.2  Dişli çarklarda kullanılmak üzere 616 naylon ve çelikten oluşan bir kompozit geliştirilmektedir. 
#Dökme demir dişlilerdeki gürültü seviyesi (x) ile yeni malzemeden yapılmış özdeş dişlilerdeki gürültü seviyesi (y) arasındaki ilişkiyi incelemek amacıyla bir çalışma yürütülmektedir. 
#Mevcut veriler şu şekildedir:
#y <- 74,81,107,90,64
#x <- 75,80,110,93,65
y <- matrix(c(74, 81, 107, 90, 64), nrow = 5, ncol = 1)
x1 <- c(75,80,110,93,65)
x <- cbind(1, x1)
#Basit bir doğrusal regresyon modeli varsayılmaktadır. Bu veriler için,
xtx <- t(x) %*% x
xtx_inv <- solve(xtx)
b <- xtx_inv %*% t(x) %*% y
#manuel
y_hat <- x %*% b
# b. Artıkları (hataları) hesaplama (y - y_hat)
residuals <- y - y_hat
# c. Artık Kareler Toplamını (SS_Res) bulma
SS_Res <- sum(residuals^2)
# d. Serbestlik derecesine bölme (n - p)
n <- length(y)
p <- 2 # beta_0, beta_1
s_squared_manual <- SS_Res / (n - p)
s <- sqrt(s_squared_manual)
#%95'lik bir güven bandı oluşturmak için, verilerin aralığı olan 65 ile 110 arasında yer alan x değerlerini isteğe bağlı olarak seçeriz. Bu noktalardan biri olarak bar{x}'in seçilmesi gelenekseldir. 
#t(x*) için bar{x} = 84.6
z <- matrix(c(1, 84.6), nrow=2, ncol=1)
#güven aralığı formülü için 
confidence_interval <- t(z) %*% xtx_inv %*% z
t_kritik <- abs(qt(0.025, df = 3))
#Dökme demirden yapılmış benzer dişliler 84.6 desibel gürültü seviyesi ürettiğinde, kompozit malzemeyle yapılmış dişliler için ortalama gürültü seviyesi üzerindeki %95'lik güven sınırları şu şekildedir:
lower_bound <- (t(z) %*% b) - (t_kritik %*% s %*% confidence_interval)
upper_bound <- (t(z) %*% b) + (t_kritik %*% s %*% confidence_interval)
