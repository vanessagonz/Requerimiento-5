-- Requerimiento 5: ciclos-por-tiempo
-- Calcula cuántos ciclos completos del semáforo ocurren en cierta cantidad de minutos.
ciclosPorTiempo :: Int -> Int
ciclosPorTiempo minutos =
  let duracionCicloSegundos = 90 + 120 + 6
      totalSegundos = minutos * 60
  in totalSegundos `div` duracionCicloSegundos
main :: IO ()
main = do
  let minutos = 10
  print (ciclosPorTiempo minutos)
  
