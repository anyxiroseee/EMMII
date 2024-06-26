library("tseries")

#1. Построить график стационарного процесса {hn} и график волатильности
#{σn} процесса GARCH(1,0) из n = 1000 наблюдений.

GARCH10 = function(a0, a1) {
  n = 1000 #количество наблюдений
  #векторы для сохранения значений процесса и волатильности
  h = numeric(n)
  σ = numeric(n)
  e = rnorm(n, 0, 1) #случайные ошибки
  σ[1] = 1  #начальное значение волатильности
  h[1] = 1 #начальное значение процесса
  
  #процесс GARCH(1,0)
  for (i in 2:n) {
    σ[i] = a0 + a1 * (h[i-1])^2
    h[i] = sqrt(σ[i]) * e[i]
  }
  
  par(mfrow=c(2,1))
  plot(h, type='l', main='Стационарный процесс {h_n} GARCH(1,0)', col='seagreen')
  plot(σ, type='l', main='Волатильность {σ_n} GARCH(1,0)', col='blue')
  
  return(h)
}

#генерация GARCH(1,0) с параметрами а0 = 0.9, а1 = 0.5
а0 = 0.9
а1 = 0.5
set.seed(123)
h_n = GARCH10 (а0, а1) #стационарность достигается при 0 < a1 < 1

#2. Оценить a0 и a1 с помощью МНК путем преобразования процесса ARCH(1) ≡ GARCH(1, 0) к процессу авторегрессии первого порядка.

n = 1000
a0 = 0.9
a1 = 0.5

#МНК для а1
MNK_a1 = function(h) {
  sum_x1 = sum(h[-1]^2)
  sum_y1 = sum(h[-length(h)]^2)
  sum_x2 = sum(h[-1]^4)
  sum_y2 = sum(h[-1]^2 * h[-length(h)]^2)
  
  numerator = (sum_y2 / length(h) - (sum_x1 * sum_y1) / (length(h)^2))
  denominator = (sum_x2 / length(h) - (sum_x1 / length(h))^2)
  
  return(numerator / denominator)
}

#МНК для а0
MNK_a0 = function(h, a1) {
  sum_x1 = sum(h[-1]^2)
  sum_y1 = sum(h^2)
  
  return((sum_y1 / length(h)) - a1 * (sum_x1 / length(h)))
}

estimated_a1 = MNK_a1(h_n); estimated_a1
estimated_a0 = MNK_a0(h_n, estimated_a1); estimated_a0

#3. При помощи функции garch()
# Функция 𝑔𝑎𝑟𝑐ℎ() определяется как 𝑔𝑎𝑟𝑐ℎ(𝑥,𝑜𝑟𝑑𝑒𝑟 = 𝑐(𝑞, 𝑝), 𝑠𝑡𝑎𝑟𝑡 = 𝑐(𝑎′0, 𝑎′1, . . . , 𝑎′𝑝, 𝑏′1, . . . , 𝑏′𝑞), . . .)
garch(h_n, order = c(0, 1), series = NULL, start = c(a0, a1))

#4. Построить график стационарного процесса GARCH(3,0) из n = 1100 наблюдений. 
#Разделить процесс на обучающую и тестовую выборки в отношении 10:1. 
#Оценить вектор параметров (a0, a1, a2, a3)′ на обучающей выборке, используя функцию garch() 
#Затем вычислить последовательность прогнозов на 1 шаг {h n + 1|n} на тестовой выборке и наложить прогнозы 
#на график процесса.

GARCH30 = function(p) {
  n = 1100
  e = rnorm(n, 0, 1)
  h = numeric(n)
  h[1] = 0
  σ = numeric(n)
  
  σ[2] = p[1]
  h[2] = sqrt(σ[2]) * e[1]
  σ[3] = p[1] + p[2] * (h[2]^2) + p[3] * (h[1]^2)
  h[3] = sqrt(σ[3]) * e[2]
  σ[4] = p[1] + p[2] * (h[3]^2) + p[3] * (h[2]^2) + p[4] * (h[1]^2)
  h[4] = sqrt(σ[4]) * e[3]
  
  for (i in 4:n) {
    σ[i] = p[1] + p[2] * (h[i-1]^2) + p[3] * (h[i-2]^2) + p[4] * (h[i-3]^2)
    h[i] = sqrt(σ[i]) * e[i]
  }
  
  plot(h, type = 'l', main = "График стационарного процесса GARCH(3,0)")
  return(h)
}

p = c(0.3, 0.2, 0.5, 0.1) #вектор параметров (а0, а1, а2, а3)

#моделирование процесса GARCH(3,0) и построение графика
h_data = GARCH30(p)

#разделение процесса на обучающую и тестовую выборки
n_train = 1000
n_test = 100
h_train = h_data[1:n_train]
h_test = h_data[(n_train + 1):(n_train + n_test)]

#оценка параметров модели на обучающей выборке
estimated_p30 = garch(h_train, order = c(0, 3), start = p)$coef
estimated_p30

#вычисление последовательности прогнозов на тестовой выборке
predict_GARCH30 = function(h_test, h_train, p) {
  h_predict = numeric(length = length(h_test))
  for (i in 1:length(h_test)) {
    if (i <= 3) {
      h_predict[i] = p[1]
    } else {
      #вычисление прогноза с использованием абсолютного значения корня из прогноза
      h_predict[i] = sqrt(abs(p[1] + p[2] * (h_test[i-1]^2) + p[3] * (h_test[i-2]^2) + p[4] * (h_test[i-3]^2)))
    }
  }
  return(c(h_train, h_predict))
}

#вычисление последовательности прогнозов на тестовой выборке и наложение их на график процесса
h_predict = predict_GARCH30(h_test, h_train, estimated_p30)
lines(h_predict, type = 'l', col = "red")

# 5. Построить стационарный процесс GARCH(1,1), из n = 1000 наблюдений и оценить его параметры (a0, a1, b1)′ по выборке {hn}, используя garch().

GARCH11 = function(a0, a1, b1, n) {
  e = rnorm(n, 0, 1)  
  h = numeric(n)  
  h[1] = 1 
  σ = numeric(n)
  σ[1] = 1  
  for (i in 2:n) {
    σ[i] = a0 + a1 * (h[i-1])^2 + b1 * σ[i-1]  # волатильность
    h[i] = sqrt(σ[i]) * e[i]  # значение процесса
  }
  return(h)
}

a0 = 1
a1 = 0.5 #стационарность достигается при 0 < a1 + b1 < 1
b1 = 0.3  
n = 1000

#генерация GARCH(1,1) и оценка его параметров
h_data = GARCH11(a0, a1, b1, n)
estimated_p11 = garch(h_data, order = c(1, 1), start = c(a0, a1, b1))$coef
estimated_p11
