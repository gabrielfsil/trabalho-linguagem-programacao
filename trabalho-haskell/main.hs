-- Gabriel Fernandes Silva - 202064562C
-- João Pedro de Carvalho Lima - 201965150AC


import System.Random
import Data.List
import Data.Char

-- Função para configurar se usará dígitos repetidos ou não
configurarRepeticao :: Bool -> [Int] -> [Int]
configurarRepeticao False xs = nub xs
configurarRepeticao True xs = xs

-- | 'gerarSegredo' é uma função que gera um segredo aleatório com 4 dígitos com base na escolha de permitir
-- ou não dígitos repetidos, representada pelo argumento 'repetir'.
--
-- A função utiliza a função 'configurarRepeticao' para configurar a lista de dígitos disponíveis de acordo com 'repetir'.
-- Em seguida, utiliza uma geração aleatória de números usando a biblioteca 'System.Random'.
-- O segredo gerado é uma lista de 4 dígitos selecionados aleatoriamente a partir da lista de possibilidades.
--
-- Exemplo de uso:
--
-- >>> gerarSegredo False
-- [1,2,3,4] -- Um segredo com dígitos únicos, sem repetição
--
-- >>> gerarSegredo True
-- [2,2,4,5] -- Um segredo com dígitos permitindo repetição
--
-- A função utiliza a biblioteca 'System.Random' e 'newStdGen' para gerar números aleatórios.
--
-- Função para gerar segredo aleatório com 4 dígitos de acordo com a função anterior
gerarSegredo :: Bool -> IO [Int]
gerarSegredo repetir = do
  gen <- newStdGen
  let
    segredo = take 4 $ configurarRepeticao repetir $ randomRs (0,9) gen
  if length segredo < 4 then
    gerarSegredo repetir
  else
    return segredo

-- Função para coletar a entrada do usuário, somente os primeiros 4 dígitos
coletarEntrada :: IO [Int]
coletarEntrada = do
  putStrLn "Digite os 4 primeiros dígitos:"
  entrada <- getLine
  let entradaValida = take 4 $ map digitToInt $ filter isDigit entrada
  return entradaValida

-- Função para validar a entrada do usuário
validarEntrada :: [Int] -> Bool
validarEntrada entrada = length entrada == 4

-- Função para gerar a resposta com símbolos em ordem aleatória
gerarResposta :: [Int] -> [Int] -> IO [Int]
gerarResposta segredo entrada = do
  shuffledResposta <- shuffle resposta
  return shuffledResposta
  where
    resposta = verificaResultado segredo entrada -- [1,1,1,1] | [1,0,-1,-1]

-- Função para verificar se o usuário acertou
verificarPosicao :: Eq a => [a] -> [a] -> Bool
verificarPosicao [] [] = True
verificarPosicao (x:xs) (y:ys) = x == y && verificarPosicao xs ys
verificarPosicao _ _ = False

-- | 'verificarPosicaoNumero' é uma função que recebe uma lista 'lista', um 'numero' e uma 'posicao'.
-- Ela verifica se o 'numero' está na 'lista' na 'posicao' especificada e retorna um valor inteiro
-- que representa a ocorrência do 'numero' na 'lista' naquela posição.

-- Se o 'numero' estiver na 'lista' na 'posicao' correta, a função retorna '1' (O).
-- Se o 'numero' estiver na 'lista', mas não na 'posicao' correta, a função retorna '0' (-).
-- Se o 'numero' não estiver na 'lista' em nenhuma posição, a função retorna '-1' (X).

-- Esta função é utilizada internamente pela função 'verificarPosicaoEOcorrencia'.

-- Exemplo de uso:

-- >>> verificarPosicaoNumero [1, 2, 3, 4] 3 2
-- 1

-- >>> verificarPosicaoNumero [1, 2, 3, 4] 4 1
-- 0

-- >>> verificarPosicaoNumero [1, 2, 3, 4] 7 0
-- -1

-- A função utiliza a operação '!!' para acessar o elemento da 'lista' na 'posicao' especificada.
-- Ela compara esse elemento com o 'numero' e retorna o valor correspondente conforme as condições acima.

-- Função que verifica a posição e a ocorrência do número na lista
verificarPosicaoNumero :: Eq a => [a] -> a -> Int -> Int
verificarPosicaoNumero lista numero posicao
  | lista !! posicao == numero = 1 -- Número na posição correta
  | otherwise = if numero `elem` lista then 0 else -1 -- Número na posição errada ou não na lista

-- | 'verificarPosicaoEOcorrencia' é uma função auxiliar que recebe duas listas, 'segredo' e 'entrada', 
-- onde 'segredo' representa o segredo a ser adivinhado e 'entrada' é o palpite do jogador. 
-- A função retorna uma lista de inteiros que representa as ocorrências de dígitos da 'entrada' no 'segredo',
-- onde cada elemento da lista corresponde à posição do dígito na 'entrada' em relação ao 'segredo'. 
-- Se um dígito da 'entrada' estiver no 'segredo' na mesma posição, é representado por '1' (O), 
-- se estiver no 'segredo', mas em uma posição diferente, é representado por '0' (-),
-- e se não estiver no 'segredo' em nenhuma posição, é representado por '-1' (X).

-- Exemplo de uso:

-- >>> verificarPosicaoEOcorrencia [1, 2, 3, 4] [4, 3, 2, 1]
-- [0, 0, 0, 0]

-- >>> verificarPosicaoEOcorrencia [1, 2, 3, 4] [1, 2, 5, 6]
-- [1, 1, -1, -1]

-- >>> verificarPosicaoEOcorrencia [1, 2, 3, 4] [5, 6, 7, 8]
-- [-1, -1, -1, -1]

-- A função utiliza 'zip' para emparelhar cada elemento da 'entrada' com sua posição e chama 'verificarPosicaoNumero'
-- para verificar a posição e a ocorrência de cada dígito.


-- Função auxiliar para verificar ocorrência
verificarPosicaoEOcorrencia :: Eq a => [a] -> [a] -> [Int]
verificarPosicaoEOcorrencia segredo entrada = [verificarPosicaoNumero segredo num pos | (num,pos) <- zip entrada [0..]]

-- Função para verificar resultado
verificaResultado :: [Int] -> [Int] -> [Int]
verificaResultado segredo entrada = if verificarPosicao segredo entrada 
    then 
        [1,1,1,1]
    else 
        verificarPosicaoEOcorrencia segredo entrada

-- Função para embaralhar uma lista
shuffle :: [a] -> IO [a]
shuffle xs = do
  gen <- newStdGen
  return $ fst $ unzip $ sortOn snd $ zip xs (randoms gen :: [Int])

-- Função principal
main :: IO ()
main = do
  putStrLn "Deseja gerar um segredo com repetição? (s para sim, qualquer outra para não)"
  repetir <- getLine
  putStrLn "Bem-vindo ao jogo Mastermind!"
  segredo <- gerarSegredo (repetir == "s")
  jogar 8 segredo

-- Função para jogar o jogo
jogar :: Int -> [Int] -> IO () -- Número de tentativas restantes e segredo para ser descoberto
jogar tentativas segredo
  | tentativas == 0 = do
    putStrLn ("Suas tentativas acabaram! O segredo era: " ++ show segredo)
  | otherwise = do
    -- Solicita que o jogador insira uma entrada
    entrada <- coletarEntrada
    -- Verifica se a entrada do jogador é válida
    if validarEntrada entrada then do
      -- Gera a resposta com base na entrada do jogador e no segredo
      resposta <- gerarResposta segredo entrada
      -- Imprime a resposta no formato adequado (O, -, ou X)
      putStrLn $ "Resposta: " ++ intercalate " " (
        map (\x -> 
          if x == 1 then 
            "O" 
          else 
            if x == 0 then 
              "-" 
            else 
              "X") 
              resposta)
      -- Verifica se o jogador adivinhou o segredo completamente
      if all (== 1) resposta then
        putStrLn ("Parabéns! Você acertou o segredo!O segredo era: " 
        ++ show segredo)
      else
        -- Se o jogador não acertou o segredo, continua o jogo com tentativas restantes
        jogar (tentativas - 1) segredo
    else do
      -- Se a entrada do jogador não for válida, exibe uma mensagem de erro
      putStrLn "Entrada inválida. Digite 4 dígitos válidos."
      -- Continua o jogo com as mesmas tentativas e segredo
      jogar tentativas segredo