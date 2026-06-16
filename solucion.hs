-- ============================================================
-- ARCHIVO: solucion.hs
-- DESCRIPCIÓN: Sistema de Semáforos Inteligentes
--              Fase 3 — Estudio Comparativo: Haskell
--              Reimplementación de transicion, timer y
--              ciclosPorTiempo con clasificación taxonómica
-- ============================================================


-- ============================================================
-- TIPOS: Algebraic Data Types (ADT)
-- Modela los estados válidos del semáforo.
-- El compilador impide usar cualquier valor fuera de este
-- conjunto en tiempo de compilación (type safety).
-- ============================================================

data Color
  = EnRojo
  | EnAmarillo
  | EnVerde
  deriving (Show, Eq)

data Accion
  = CambiarA Color
  | AccionPorDefecto
  deriving (Show, Eq)

type ResultadoTransicion = (Color, Accion)


-- ============================================================
-- FUNCIÓN: transicion
-- NATURALEZA: Pura — mismo input siempre produce el mismo
--             output; sin efectos secundarios ni estado mutable
-- ESTRATEGIA: Pattern Matching sobre el ADT Color
-- IMPACTO: No destructiva
-- ============================================================
-- Entrada: colorActual (Color), cambiarA (Color)
-- Salida:  ResultadoTransicion — par (estado, acción)
-- Comportamiento: devuelve AccionPorDefecto si la transición
--                 no es válida según el ciclo semafórico
-- ============================================================
transicion :: Color -> Color -> ResultadoTransicion
transicion EnRojo     EnVerde    = (EnRojo,     CambiarA EnVerde)
transicion EnVerde    EnAmarillo = (EnVerde,    CambiarA EnAmarillo)
transicion EnAmarillo EnRojo     = (EnAmarillo, CambiarA EnRojo)
transicion estadoActual _        = (estadoActual, AccionPorDefecto)


-- ============================================================
-- FUNCIÓN: duracionCiclo (auxiliar pura)
-- NATURALEZA: Pura
-- ESTRATEGIA: Composición aritmética directa
-- IMPACTO: No destructiva
-- ============================================================
duracionRojo, duracionAmarillo, duracionVerde :: Int
duracionRojo     = 90
duracionAmarillo = 6
duracionVerde    = 120

duracionCiclo :: Int
duracionCiclo = duracionRojo + duracionAmarillo + duracionVerde


-- ============================================================
-- FUNCIÓN: timer
-- NATURALEZA: Pura — dado un timestamp Unix, siempre retorna
--             el mismo Color; sin efectos secundarios
-- ESTRATEGIA: Guardas (guards) sobre el resultado de mod
-- IMPACTO: No destructiva
-- ============================================================
-- Entrada: timestamp (Int) — tiempo Unix en segundos
-- Salida:  Color activo en ese instante
-- Comportamiento: calcula la posición dentro del ciclo
--                 actual con mod y aplica los umbrales
-- ============================================================
timer :: Int -> Color
timer timestamp
  | pos < duracionRojo                          = EnRojo
  | pos < duracionRojo + duracionAmarillo       = EnAmarillo
  | otherwise                                   = EnVerde
  where
    pos = timestamp `mod` duracionCiclo


-- ============================================================
-- FUNCIÓN: ciclosPorTiempo
-- NATURALEZA: Pura — dado el mismo número de minutos, siempre
--             retorna el mismo resultado; sin efectos secundarios
-- ESTRATEGIA: Composición con duracionCiclo + división entera
-- IMPACTO: No destructiva
-- ============================================================
-- Entrada: minutos (Int) — duración en minutos (>= 0)
-- Salida:  Int — cantidad de ciclos completos en ese período
-- ============================================================
ciclosPorTiempo :: Int -> Int
ciclosPorTiempo minutos =
  let duracionCicloSegundos = duracionRojo + duracionAmarillo + duracionVerde
      totalSegundos         = minutos * 60
  in  totalSegundos `div` duracionCicloSegundos


-- ============================================================
-- FUNCIÓN: main — Ejemplos de uso (equivalente al Req. 7)
-- NATURALEZA: Impura — efecto secundario: imprime en pantalla
-- ESTRATEGIA: Secuenciación monádica con do-notation (IO)
-- IMPACTO: No destructiva sobre datos; efecto en consola
-- ============================================================
main :: IO ()
main = do

  putStrLn "=== TRANSICION ==="
  print $ transicion EnRojo     EnVerde       -- (EnRojo,    CambiarA EnVerde)
  print $ transicion EnVerde    EnAmarillo    -- (EnVerde,   CambiarA EnAmarillo)
  print $ transicion EnAmarillo EnRojo        -- (EnAmarillo,CambiarA EnRojo)
  print $ transicion EnRojo     EnRojo        -- (EnRojo,    AccionPorDefecto)

  putStrLn "\n=== TIMER ==="
  putStrLn $ "t=0   => " ++ show (timer 0)    -- EnRojo
  putStrLn $ "t=89  => " ++ show (timer 89)   -- EnRojo   (último segundo)
  putStrLn $ "t=90  => " ++ show (timer 90)   -- EnAmarillo
  putStrLn $ "t=95  => " ++ show (timer 95)   -- EnAmarillo (último segundo)
  putStrLn $ "t=96  => " ++ show (timer 96)   -- EnVerde
  putStrLn $ "t=215 => " ++ show (timer 215)  -- EnVerde  (último segundo)
  putStrLn $ "t=216 => " ++ show (timer 216)  -- EnRojo   (ciclo nuevo)

  putStrLn "\n=== CICLOS-POR-TIEMPO ==="
  putStrLn $ "Duracion ciclo : " ++ show duracionCiclo ++ " segundos"
  putStrLn $ "10 min => " ++ show (ciclosPorTiempo 10) ++ " ciclo(s)"
  putStrLn $ "15 min => " ++ show (ciclosPorTiempo 15) ++ " ciclo(s)"
  putStrLn $ "60 min => " ++ show (ciclosPorTiempo 60) ++ " ciclo(s)"
  putStrLn $ "3  min => " ++ show (ciclosPorTiempo 3)  ++ " ciclo(s)"
  putStrLn $ "1  min => " ++ show (ciclosPorTiempo 1)  ++ " ciclo(s)"
