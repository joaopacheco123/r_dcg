

# Vamos criar um objeto (variável) do tipo numérico, com nome "x".
x <- 5

print(x)


# Caso deseje, um novo valor pode ser atribuído à "x":
x <- "oi"
print(x)

# Note que "x" pode receber o valor de uma outra variável:
y <- 10

x <- y

print(x)

# Ou uma operação matemática:
x <- y + 1

print(x)

##################### COMENTE SEU CÓDIGO SEMPRE QUE POSSÍVEL #######################


# Imprimindo no console o valor de y
print(y)


# No R há diferenciação entre letras maiúsculas e minúsculas.
print(Y)

# Ambiente ->>>>>>>>>>

# Para remover variáveis da aba ambiente
rm(x)

# Caso deseje remover todas
rm(list = ls())

############
#Operadores#
############

# Podem ser utilizados diretamente com as variáveis

z <- 2
y <- 4

# Soma
1 + 1
z + y

# Multiplicação
z * y

# Exponenciação
z ** y

# O operador de igualdade é dessa forma!!!
z == y

# Diferença
z != y

# Negação. 
!(z == y)

# Utilize parenteses em operações mais complexas
a <- 5 * 2 + 3
b <- 5 * (2 + 3)

a == b

# OU
TRUE | FALSE

#######
#Loops#
#######

# Utilizamos principalmente o for loop
for(i in 1:10) {
  print(i)
}

# Criando um vetor 
nomes <- c("joao", "mauricio", "joao pedro")

# O loop pode iterar por vetores, listas, etc
for(nome in nomes) {
  print(nome)
}

# Operações podem ser feitas dentro dos loops
for(i in 1:10) {
  print(i + 1)
}

###########
#If - else#
###########

i <- 1

# Avalia se condição é verdadeira
if(i == 2) {
  print("i tem o valor de 2")
} else {
  print("Essa condição é falsa!")
}


# Também pode ser utilizado dentro de um loop:

print(nomes)
  
# For loop com if 
for(nome in nomes) {
  # Avalia se a condição é verdadeira
  if(nome == "mauricio") {
    print(paste({nome}, "deu migué na aula!"))
  } 
  # Caso a condição seja falsa
  else {
    print(paste({nome}, "está presente"))
  }
}


######### 
#Funções#
#########

#  "Tools" -> "Global Options" -> "Code" -> "Display" -> "Highlight function calls"

# Já utilizamos funções anteriormente
print(z)

# Podemos criar nossa própria função
somar <- function(x, y) {
  return(x + y)
}

# Testando função
somar(1,2)

# Podemos salvar o resultado da função em uma nova variável
resultado <- somar(1,2)

#########
#Pacotes#
#########

## A função abaixo instala pacotes
install.packages("psych")

## Para instalar com as dependências
install.packages("psych", dependencies = TRUE)

## Carregando o pacote psych
library(psych)

## Utilizando funções sem carregar o pacote
# Isso também pode ser útil quando mais de um pacote possuir funções com mesmo nome.
psych::fa()

# Ajuda

## Pedindo ajuda com uma função 
help(mean) 

## A mesma função também pode ser utilizada para pacotes
help(psych)

## O help search irá buscar páginas relacionadas ao termo inserido
help.search("linear regression")

## Escrever "?" funciona como help() 
?mean  

## Escrever "??" funciona como help.search()
??mean

## Selecionar função e apertar a tecla f1
mean()

## A função example traz exemplos de uso
example("mean")

# Valorize as mensagens de erro!

## As mensagens de erro são feitas para ajudar o programador, não somente pra frustrá-lo.
print(j)

