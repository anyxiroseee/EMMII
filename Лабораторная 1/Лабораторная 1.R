library("stats")
#Построить график процесса AR(1) для различных значений параметра θ

AR = function(n, θ){
  x = array()
  x[1] = 0
  for (k in 2 : n) {
  x[k] = θ * x[k-1] + rnorm(n, 0, 1)
  }
  return(x)
}

AR1 = AR(100, 0.36)
plot(AR1, xlab = "n", ylab = "θ", type = "l", main = "|θ| < 1")

AR2 = AR(100, 1)
plot(AR2, xlab = "n", ylab = "θ", type = "l", main = "|θ| = 1")

AR3 = AR(100, 1.36)
plot(AR3, xlab = "n", ylab = "θ", type = "l", main = "|θ| > 1")

#Оценить параметр |θ| ≤ 1 по МНК

# Σxk-1 * xk / Σ(xk-1)^2 - оценка МНК параметра авторегрессии
MNK = function(n, AR) {
  a = b = 0
  for (k in 2 : n) {
    a = a + (AR[k - 1] * AR[k])
    b = b + ((AR[k - 1]) ^ 2)
  }
  return (a / b)
}

#|θ| < 1
MNK(100, AR1)
#|θ| = 1
MNK(100, AR2)

#Оценка МП параметра θ процесса AR(1)

MP = function(θ) {
  sum = 0
  for (k in 2 : n) {
    sum = sum + (AR1[k] - θ * AR1[k-1]) ^ 2
  }
  return (sum)
}

##Оценки МНК и МП для параметра θ в случае гауссовского шума совпадают:
n = 100
optimize(f = MP, interval = c(-100,100))$minimum
MNK(100, AR1)


#МНК-оценки для объема выборки k = 10, 11, ..., n (n = 1000)

AR4 = AR(1000, 0.28)

#для k = 10
MNK10 = array(dim = 10)
MNK10[1] = 0
for (k in 2 : 10) {
  MNK10[k] = MNK(k, AR4)
}

plot(MNK10, xlab = "n", ylab = "θ", type = 'l', main = "k = 10")

#для k = 11
MNK11 = array(dim = 11)
MNK11[1] = 0
for (k in 2 : 11) {
  MNK11[k] = MNK(k, AR4)
}
plot(MNK11, xlab = "n", ylab = "θ", type = 'l', main = "k = 11")

#для k = 10...1000
MNKK = array(dim = 1000 - 9)
for (k in 10 : 1000){
  MNKK[k - 9] = MNK(k, AR4)}

plot(MNKK, xlab = "n", ylab = "θ", type = 'l', main = "k = 10, 11, ..., n")

#Свойство состоятельности выполняется, т.к
#при увеличении объема выборки оценка сходится к истинному значению.
#На 1000м шаге значение оценки ближе к истинному (θ = 0.28), чем на 10м шаге
#Начиная с 450 шага (см. график) отклонения от истинного значения незначительные, поэтому
#n = 450 в некотором смысле можно считать оптимальным объемом выборки.


#График устойчивого процесса AR(2)
#Т.е. 2 параметра, следующее наблюдение будет зависить от двух предыдущих, умноженных на константу (некоторый шум)

AR2 = function(n, θ1, θ2){
  x = array()
  x[1] = 0
  x[2] = 1
  for (k in 3 : n) {
    x[k] = θ1 * x[k-1] + θ2 * x[k-2] + rnorm(n, 0, 1)
  }
  return(x)
}

AR21 = AR2(100, 0.5, 0.33)
plot(AR21, xlab = "n", ylab = "θ", type = "l", main = "AR(2)")

arima(AR21, order = c(2,0,0), include.mean = FALSE)
