bisseccao <- function(f, a, b, precisao) {
  if (f(a) * f(b) >= 0) stop("Os pontos inicial e final devem ter sinais diferentes")
  
  diferenca = b - a
  aa = a 
  bb = b 
  metade = 0
  
  if (diferenca <= precisao) {
    return((aa + bb)/2)
  }
  
  metade <- (aa + bb)/2
  
  if (f(metade) == 0) {
    return(metade)
  } else if (f(aa) * f(metade) < 0) {
    return(bisseccao(f, aa, metade, precisao))
  } else {
    return(bisseccao(f, metade, bb, precisao))
  }
}

# Teste 1: Encontrando raiz de x - 2
f1 <- function(x) x - 2
resultado1 <- bisseccao(f1, 0, 3, precisao = 0.01)
print(paste("Raiz = 2?", resultado1))

# Teste 2: Encontrando raiz de x² - 4
f2 <- function(x) x^2 - 4
resultado2 <- bisseccao(f2, 0, 3, precisao = 0.01)
print(paste("Raiz = 2?", resultado2))

# Teste 3: Encontrando raiz de x - 1
f3 <- function(x) x - 1
resultado3 <- bisseccao(f3, 0, 2, precisao = 0.01)
print(paste("Raiz = 1?", resultado3))
