--Aluno: André Gustavo da Rosa Ribeiro

--1 - Escreva uma função para o cálculo dos números da sequência de Fibonacci, utilizando Haskell.
fibonacci :: Int -> Int
fibonacci x
  | x==0 = 0
  | x==1 = 1
  | otherwise = fibonacci(x-1) + fibonacci(x-2)

--2 Um dos primeiros algoritmos documentados é o algoritmo para o cálculo do Maior Divisor Comum (MDC) de Euclides publicado por volta do ano 300 AC. Podemos simplificar este algoritmo dizendo que dados dois inteiros A e B, o MDC entre eles será dado pelo valor absoluto de A se B=0 e pelo MDC entre B e o resto da divisão de A por B se B>0. Escreva uma função para o cálculo do MDC entre dois números inteiros positivos, usando o algoritmo de Euclides conforme apresentado aqui, utilizando Haskell

mdc :: Int -> Int -> Int
mdc x y
  | y == 0 = abs x
  | otherwise = mdc y (x`mod`y)

--3 Escreva uma função recursiva que dado um número inteiro n, devolva a soma dos dígitos deste número. Exemplo: dado 1234 a função deverá devolver 10. Utilizando Haskell e recursividade.

somaDigitos :: Int -> Int -> Int
somaDigitos num soma
  | length (show num) == 1 = soma + num
  | otherwise = somaDigitos (read (tail(show num)) :: Int) (soma+(read ([head(show num)]) :: Int))


--4 Escreva uma função que devolva a soma de todos os números menores que 10000 que sejam múltiplos de 3 ou 5

somaMil :: Int -> Int -> Int
somaMil num soma
  | num == 10000 = soma
  | num `rem` 3 == 0 || num `rem` 5 == 0 = somaMil (num+1) (soma+num)
  | otherwise =  somaMil (num+1) (soma)

--5 Escreva uma função que, recebendo uma lista de inteiros, apresente a diferença entre a soma dos quadrados e o quadrado da soma destes inteiros, usando recursividade.

func :: [Int] -> Int -> Int -> Int
func lista sum1 sum2
  | length lista == 0 = sum1 - (sum2*sum2)
  | otherwise = func (tail lista) (sum1+((head lista)*(head lista))) (sum2+(head lista))


--6 O Crivo de Eratóstenes não é o melhor algoritmo para encontrar números primos. Crie uma função que implemente o Crivo de Euler (Euler’s Sieve) para encontrar todos os números primos menores que um determinado inteiro dado.
testarPrimo :: Int -> [Int] -> Bool
testarPrimo num listaPrimos
  | num < 2 = False
  | num == 2 = True
  | listaPrimos == [] = True
  | num `rem`2==0 = False
  | ((num `rem` (head listaPrimos)==0) && ((head listaPrimos) /= num)) = False
  | otherwise = testarPrimo num (tail listaPrimos)

listarPrimos :: Int -> [Int] -> [Int]
listarPrimos numOriginal listaOutput
  | numOriginal < 2 = listaOutput
  | testarPrimo (numOriginal-1) [2..numOriginal-1] == True = listarPrimos (numOriginal-1) ((numOriginal-1) : listaOutput)
  | otherwise = listarPrimos (numOriginal-1) listaOutput


-- 7 Nem só de Fibonacci vivem os exemplos de recursão. Escreva uma função que devolva todos os números de uma sequência de Lucas (2, 1, 3, 4, 7, 11, 18, 29, 47, 76, 123) menores que um inteiro dado
lucasCalcular :: Int -> Int
lucasCalcular x
  | x==0 = 2
  | x==1 = 1
  | otherwise = lucasCalcular(x-1) + lucasCalcular(x-2)

lucasLista :: Int -> Int -> [Int] -> [Int]
lucasLista x lucasN listaOutput
  | x == 1 = []
  | x == 2 = [1]
  | lucasCalcular lucasN > x = listaOutput
  | otherwise = lucasLista x (lucasN+1) (listaOutput ++ [(lucasCalcular lucasN)]  )

--8 Escreva uma função, chamada aoContrario em Haskel para reverter uma lista. Dado [1,2,3] devolva [3,2,1].

aoContrario :: [Int] -> [Int] -> [Int]
aoContrario listaInput listaOutput
  | length listaInput == 0 = listaOutput
  | otherwise = aoContrario (tail listaInput) ((head listaInput) : listaOutput)

--9 Escreva uma função chamada somaRecursiva que recebe dois valores inteiros e devolve o produto destes valores sem usar o operador de multiplicação.

somaRecursiva:: Int -> Int -> Int -> Int
somaRecursiva x y output
  | y == 1 = output + x
  | otherwise = somaRecursiva x (y-1) (output+x)

--10 Escreva uma função chamada comprimento que receba uma lista de inteiros e devolva o comprimento desta lista. Observe que você não pode usar nenhuma função que já calcule o comprimento de uma lista
comprimento :: [Int] -> Int -> Int
comprimento lista tamanho
  | lista == [] = tamanho
  | otherwise = comprimento (tail lista) (tamanho+1)

main = do
--Q1
  putStrLn $ "Func.1: entrada: 7 resultado:" ++ show (fibonacci 7)
  putStrLn ""
--Q2
  putStrLn $ "Func.2: entrada: 4 ; 8 resultado:" ++ show (mdc 4 8)
  putStrLn $ "Func.2: entrada: 3 ; 0 resultado:" ++ show (mdc 3 0)
  putStrLn ""

--Q3
  putStrLn $ "Func.3: entrada: 123 ; 0 resultado:" ++ show (somaDigitos 123 0)
  putStrLn ""

--Q4
  putStrLn $ "Func.4: entrada: 0 ; 0 resultado:" ++ show (somaMil 0 0)
  putStrLn ""

--Q5
  putStrLn $ "Func.5: entrada: [2,3,5] ; 0 ; 0 resultado:" ++ show (func [2,3,5] 0 0)
  putStrLn ""

--Q6
  putStrLn $ "Func.6: entrada: 11 ; [] resultado:" ++ show (listarPrimos 11 [])
  putStrLn ""

--Q7
  putStrLn $ "Func.7: entrada: 1 ; 0 ; [] resultado:" ++ show (lucasLista 1 0 [])
  putStrLn $ "Func.7: entrada: 2 ; 0 ; [] resultado:" ++ show (lucasLista 2 0 [])
  putStrLn $ "Func.7: entrada: 5 ; 0 ; [] resultado:" ++ show (lucasLista 5 0 [])
  putStrLn ""

--Q8
  putStrLn $ "Func.8: entrada: [1,2,3]; [] resultado:" ++ show (aoContrario [1,2,3] [])
  putStrLn ""
--Q9
  putStrLn $ "Func.9: entrada: 4 ; 2 ; 0 resultado:" ++ show (somaRecursiva 4 2 0)

  putStrLn ""
--Q10
  putStrLn $ "Func.10: entrada:[1,2,3,4,5] ; 0 resultado:" ++ show (comprimento [1,2,3,4,5] 0)
