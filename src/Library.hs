module Library where
import PdePreludat

-- Auto
data Auto = Auto {
    color :: String,
    velocidad:: Number,
    distanciaRecorrida :: Number
} deriving Show

instance Eq Auto where {
    (==) unAuto otroAuto = color unAuto == color otroAuto
}

-- Carrera
type Carrera = [Auto]
type PowerUp = (Auto -> Carrera -> Carrera)
type Color = String
type Posicion = (Number,Color)
type Evento = (Carrera -> Carrera)


-- estanCerca :: Auto -> Auto -> Bool
-- estanCerca unAuto otroAuto =
--     (unAuto /= otroAuto) && abs (distanciaRecorrida unAuto - distanciaRecorrida otroAuto) < 10

estanCerca :: Auto -> Auto -> Bool
estanCerca unAuto otroAuto = 
    ((unAuto /= otroAuto &&) . (<10) . abs . (distanciaRecorrida unAuto -) . distanciaRecorrida) otroAuto

vaTranquilo :: Auto -> Carrera -> Bool
vaTranquilo auto carrera = 
    (noTengaNingunAutoCerca auto carrera &&) . leVaGanandoATodos auto $ carrera

noTengaNingunAutoCerca :: Auto -> Carrera -> Bool
noTengaNingunAutoCerca auto = not . any (estanCerca auto)

leVaGanandoATodos :: Auto -> Carrera -> Bool
leVaGanandoATodos auto = not . any (`leVaGanando` auto)

leVaGanandoATodos' :: Auto -> Carrera -> Bool
leVaGanandoATodos' auto = all (leVaGanando auto)

leVaGanando :: Auto -> Auto -> Bool
leVaGanando unAuto otroAuto = 
    ((unAuto /= otroAuto &&) . (< distanciaRecorrida unAuto) . distanciaRecorrida) otroAuto

autoEnCarrera :: Auto -> Carrera -> Auto 
autoEnCarrera auto = head . filter (== auto)

puestoDeUnAuto :: Auto -> Carrera -> Number
puestoDeUnAuto auto = (+1) . length . filter (`leVaGanando` auto)

correrDurante :: Number -> Auto -> Auto
correrDurante tiempo auto = auto{
    distanciaRecorrida = distanciaRecorrida auto + tiempo * velocidad auto
}


-- bajarVelocidadEn :: Number -> Auto -> Auto 
-- bajarVelocidadEn cuanto auto = auto{
--     velocidad = modificador (velocidad auto - cuanto)
-- }

bajarVelocidadEn :: Number -> Auto -> Auto 
bajarVelocidadEn = alterarVelocidad . flip (-)


modificador :: Number -> Number
modificador = max 0

alterarVelocidad :: (Number -> Number) -> Auto ->Auto
alterarVelocidad funcion auto = 
    cambiarVelocidad (funcion . velocidad $ auto) auto

-- duplicarVelocidad :: Auto -> Auto
-- duplicarVelocidad auto = auto {
--     velocidad = velocidad auto * 2
-- }

duplicarVelocidad :: Auto -> Auto
duplicarVelocidad = alterarVelocidad (*2)

cambiarVelocidad :: Number -> Auto -> Auto
cambiarVelocidad velocidad auto = auto {
    velocidad = modificador velocidad 
}


afectarALosQueCumplen :: (a -> Bool) -> (a -> a) -> [a] -> [a]
afectarALosQueCumplen criterio efecto lista
  = (map efecto . filter criterio) lista ++ filter (not.criterio) lista

terremoto :: PowerUp
terremoto auto  = afectarALosQueCumplen (estanCerca auto) (bajarVelocidadEn 50)

miguelitos :: Number -> PowerUp
miguelitos cuanto auto = afectarALosQueCumplen (`leVaGanando` auto) (bajarVelocidadEn cuanto)

jetPack :: Number -> PowerUp
jetPack tiempo auto = 
    afectarALosQueCumplen (==auto) 
    (cambiarVelocidad (velocidad auto) . correrDurante tiempo . duplicarVelocidad)

obtenerPosiciones :: Carrera -> [Posicion]
obtenerPosiciones carrera = 
    map (\auto -> (puestoDeUnAuto auto carrera, color auto)) carrera

simularCarrera :: Carrera -> [Evento] -> [Posicion]
simularCarrera carrera = obtenerPosiciones . foldl (flip ($)) carrera 

correnTodos :: Number -> Evento
correnTodos = map . correrDurante 

usaPowerUp :: PowerUp -> Color -> Evento
usaPowerUp powerUp colorAuto carrera = 
    powerUp (buscarPorColor colorAuto carrera) carrera

buscarPorColor :: Color -> Carrera -> Auto
buscarPorColor colorAuto = head . filter ((== colorAuto) . color)