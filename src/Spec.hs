module Spec where
import PdePreludat
import Library
import Test.Hspec

-- rojo
rojo = Auto "rojo" 120 0
-- azul
azul = Auto "azul" 120 0
-- verde
verde = Auto "verde" 120 0
-- blanco
blanco = Auto "blanco" 120 0
--negro
negro = Auto "negro" 120 0

-- carreraTranquilaParaElRojo
carreraTranquilaParaElRojo = [rojo {distanciaRecorrida = 100}, azul, blanco]
-- carreraNoTranquilaParaElRojo
carreraNoTranquilaParaElRojo = [rojo {distanciaRecorrida = 100}, azul{distanciaRecorrida = 95}, blanco]
-- carreraInicial
carreraInicial = [rojo, azul, verde, blanco, negro]

-- carreraAvanzada
carreraAvanzada = [rojo, azul{distanciaRecorrida = 95}, blanco{distanciaRecorrida = 30}]

-- eventos
eventos = [correnTodos 30,
           usaPowerUp (jetPack 3) "azul",
           usaPowerUp terremoto "blanco",
           correnTodos 30,
           usaPowerUp (miguelitos 20) "blanco",
           usaPowerUp (jetPack 6) "negro",
           correnTodos 10]

correrTests :: IO ()
correrTests = hspec $ do
 describe "Test de estan cerca" $ do
  it "se cumple si son distintos y su distancia es menor a 10" $ do
    rojo `shouldSatisfy` estanCerca azul {distanciaRecorrida = 5}
  it "no se cumple si son iguales" $ do
    rojo `shouldNotSatisfy` estanCerca rojo {distanciaRecorrida = 5}
  it "no se cumple si esta lejos" $ do
    rojo `shouldNotSatisfy` estanCerca azul {distanciaRecorrida = 100}
  it "no se cumple si la distancia es 10" $ do
    rojo `shouldNotSatisfy` estanCerca rojo {distanciaRecorrida = 10}

 describe "Test de va tranquilo" $ do
  it "se cumple si no tiene ningún auto cerca y les va ganando a todos en una carrera" $ do
    carreraTranquilaParaElRojo `shouldSatisfy` vaTranquilo (autoEnCarrera rojo carreraTranquilaParaElRojo)
  it "no se cumple si tiene algun auto cerca" $ do
    carreraNoTranquilaParaElRojo `shouldNotSatisfy` vaTranquilo (autoEnCarrera rojo carreraNoTranquilaParaElRojo)
  it "no se cumple si no va ganando" $ do
    carreraAvanzada `shouldNotSatisfy` vaTranquilo (autoEnCarrera rojo carreraAvanzada)

 describe "Test de puesto de un auto" $ do
  it "esta en primer puesto si le va ganando a todos" $ do
    puestoDeUnAuto (autoEnCarrera rojo carreraTranquilaParaElRojo) carreraTranquilaParaElRojo `shouldBe` 1
  it "esta en ultimo puesto si no le gana a nadie" $ do
    puestoDeUnAuto (autoEnCarrera rojo carreraAvanzada) carreraAvanzada `shouldBe` 3

 describe "Test de correr" $ do
  it "si un auto corre por un tiempo, su distancia recorrida aumenta" $ do
    distanciaRecorrida (correrDurante 1 rojo) `shouldBe` 120
  it "si un auto corre 0 segundos, su distancia recorrida no cambia" $ do
    distanciaRecorrida (correrDurante 0 rojo) `shouldBe` 0

 describe "Test de modificar velocidad" $ do
  it "su velocidad no dismuye mas que 0" $ do
    velocidad (bajarVelocidadEn 1000 rojo) `shouldBe` 0
  it "su velocidad dismuye en el numero que se indica" $ do
    velocidad (bajarVelocidadEn 100 rojo) `shouldBe` 20

 describe "Test Power Ups" $ do
  it "test de terremoto" $ do
    velocidad (autoEnCarrera azul (terremoto (autoEnCarrera rojo carreraNoTranquilaParaElRojo) carreraNoTranquilaParaElRojo)) `shouldBe` 70
  it "test de miguelitos" $ do
    velocidad (autoEnCarrera azul (miguelitos 100 (autoEnCarrera rojo carreraAvanzada) carreraAvanzada)) `shouldBe` 20
  it "test de jetPack: la velocidad inicial es igual a la final" $ do
    velocidad (autoEnCarrera azul (jetPack 100 azul carreraInicial)) `shouldBe` 120
  it "test de jetPack: la distancia recorrida aumenta" $ do
    distanciaRecorrida (autoEnCarrera azul (jetPack 2 azul carreraInicial)) `shouldBe` 480


 describe "Test 4" $ do
  it "test" $ do
    1 `shouldBe` 1
 describe "Test 4" $ do
  it "test" $ do
    1 `shouldBe` 1
 describe "Test 4" $ do
  it "test" $ do
    1 `shouldBe` 1

