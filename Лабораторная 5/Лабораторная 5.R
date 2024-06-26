#2. Дискретный аналог броуновского движения
set.seed(123)
δ = 0.0001   #т.к. √∆ = 0.01
n = 1000
k = seq(0, n, by = 1)
e = rnorm(n + 1, 0, sqrt(δ)) 
B = 0

for (k in 1:n) {
  B[k + 1] = B[k] + e[k] 
}

plot(seq(0, δ * 1000, δ), B, type = 'l', main = "Диск. броуновское (процесс 8)", col = "blue")

#3. Ансамбль 200 реализаций процесса
plot(seq(0, δ * 1000, δ), B, type = 'l', ylim = c(-1, 1), col = "lightblue", main = "Ансамбль 200 реализаций:")

for (k in 1:200) {
  e = rnorm(n + 1, 0, sqrt(δ)) 
  for (k in 1:n) {
    B[k + 1] = B[k] + e[k] 
  }
  lines(seq(0, δ * 1000, δ), B, type = 'l', col = "lightblue")
}

#4. Ограничить ансамбль реализаций по правилу трех сигм
#ДИ: P(︁−3√𝑘∆ ≤ 𝐵𝑘∆ ≤ 3√𝑘∆)︁= 0.997, 𝑘 ≥ 0

kδ = seq(0, 1000) * δ
up = 3 * sqrt(kδ)  #верхняя граница ДИ
low = -3 * sqrt(kδ)  #нижняя граница ДИ

lines(seq(0, δ * 1000, δ), up, type = 'l', col = "red")
lines(seq(0, δ * 1000, δ), low, type = 'l', col = "red")

legend("topright", legend = c("Доверительный интервал"), col = c("red"), bty = "n", lwd = 1)

# 5. Процесс 9 - решение уравнения St (интег. ур-е, определяющее геом. броун движение) 
#с н.у S0 (не зависит от броуновского движения)
S = numeric(length = n + 1)
S0 = 1
a = 0.5
σ = 0.9
δ = 0.0001
B = 0
k = seq(0, n, by = 1)

for (k in 1:n) {
  B[k + 1] = B[k] + e[k+1]
}
for (k in 1:(n+1)) {
  S[k] = S0 * exp((a - σ^2 / 2) * k * δ + σ * B[k]) #(a - σ^2 / 2) - локальный снос, σ^2 - диффузия
}

plot(seq(0, δ * 1000, δ), S, type = 'l', col = "blue", main = "Процесс 9:")

#6. Ансамбль 200 реализаций процесса
plot(seq(0, δ * 1000, δ), B, type = 'l', ylim = c(-1, 1), col = "lightblue", main = "Ансамбль 200 реализаций:")

for (k in 1:200) {
  e = rnorm(n + 1, 0, sqrt(δ)) 
  for (k in 1:n) {
    B[k + 1] = B[k] + e[k] 
    S[k+1] = S0 * exp((a - σ^2 / 2) * (k + 1) * δ + σ * B[k + 1])
  }
  lines(seq(0, δ * 1000, δ), B, type = 'l', col = "lightblue")
}
