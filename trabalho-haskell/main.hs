-- Gabriel Fernandes Silva - 202064562C
-- João Pedro de Carvalho Lima - 201965150AC


import System.Random
import Data.List
import Data.Char

-- Função para configurar se usará dígitos repetidos ou não
configurarRepeticao :: Bool -> [Int]
configurarRepeticao False = nub [0..9]
configurarRepeticao True = [0..9]

-- Função para gerar segredo aleatório com 4 dígitos de acordo com a função anterior
gerarSegredo :: Bool -> IO [Int]
gerarSegredo repetir = do
  gen <- newStdGen
  let possibilidades = configurarRepeticao repetir
      segredo = take 4 $ randomRs (0, 9) gen
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
    resposta = verificaResultado segredo entrada

-- Função para verificar se o usuário acertou
verificarPosicao :: Eq a => [a] -> [a] -> Bool
verificarPosicao [] [] = True
verificarPosicao (x:xs) (y:ys) = x == y && verificarPosicao xs ys
verificarPosicao _ _ = False

-- Função que verifica a posição e a ocorrência do número na lista
verificarPosicaoNumero :: Eq a => [a] -> a -> Int -> Int
verificarPosicaoNumero lista numero posicao
  | lista !! posicao == numero = 1 -- Número na posição correta
  | otherwise = if numero `elem` lista then 0 else -1 -- Número na posição errada ou não na lista

-- Função auxiliar para verificar ocorrência
verificarPosicaoEOcorrencia :: Eq a => [a] -> [a] -> [Int]
verificarPosicaoEOcorrencia lista1 lista2 = [verificarPosicaoNumero lista1 num pos | (num, pos) <- zip lista2 [0..]]

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
      putStrLn $ "Resposta: " ++ intercalate " " (map (\x -> if x == 1 then "O" else if x == 0 then "-" else "X") resposta)
      -- Verifica se o jogador adivinhou o segredo completamente
      if all (== 1) resposta then
        putStrLn ("Parabéns! Você acertou o segredo!O segredo era: " ++ show segredo)
      else
        -- Se o jogador não acertou o segredo, continua o jogo com tentativas restantes
        jogar (tentativas - 1) segredo
    else do
      -- Se a entrada do jogador não for válida, exibe uma mensagem de erro
      putStrLn "Entrada inválida. Digite 4 dígitos válidos."
      -- Continua o jogo com as mesmas tentativas e segredo
      jogar tentativas segredo