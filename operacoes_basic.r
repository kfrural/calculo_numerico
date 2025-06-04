calculadora <- function(num1, num2, operacao) {
  if (!is.numeric(num1) | !is.numeric(num2)) stop("Os valores devem ser numéricos")
  if ((operacao %in% c("divisao", "/")) & (num2 == 0)) stop("Erro: Divisão por zero não é permitida")
  switch(operacao,
         soma = num1 + num2,
         subtracao = num1 - num2,
         multiplicacao = num1 * num2,
         divisao = num1 / num2,
         porcentagem = (num2 / num1) * 100,
         "+" = num1 + num2,
         "-" = num1 - num2,
         "*" = num1 * num2,
         "/" = num1 / num2,
         stop("Operação inválida")
  )
}

print(calculadora(10, 2, "soma"))
print(calculadora(10, 2, "subtracao"))
print(calculadora(10, 2, "multiplicacao"))
print(calculadora(10, 2, "divisao"))
print(calculadora(10, 2, "porcentagem"))

print(calculadora(10, 2, "+"))
print(calculadora(10, 2, "-"))
print(calculadora(10, 2, "*"))
print(calculadora(10, 2, "/"))

comparar <- function(num1, num2) {
    if (!is.numeric(num1) | !is.numeric(num2)) stop("Os valores devem ser numéricos")
    if (num1 > num2) return(" o  num 1 eh maior")
    if (num1 < num2) return(" o num 2 eh menor")
    return("iguais")
}

print(comparar(10, 2))
print(comparar(2, 10))
print(comparar(5, 5))

#comparar trs valores
comparar3 <- function(num1, num2, num3) {
    if(!is.numeric(num1) | !is.numeric(num2) | !is.numeric(num3)) stop("valor tem que ser numerico")
    if (num1 > num2 & num1 > num3) return("o num1 eh o maior")
    if (num2 > num1 & num2 > num3) return("o num2 eh o maior")
    if (num3 > num2 & num3 > num1) return("o num3 eh o maior")
    return("iguais")
}

print(comparar3(10, 2, 4))
print(comparar3(2, 10, 1))
print(comparar3(2, 10, 11))
print(comparar3(5, 5, 5))
