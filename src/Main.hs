module Main where
import Graphics.Gloss
import Graphics.Gloss.Interface.Pure.Game
-- import Graphics.Gloss.Geometry.Angle
import Graphics.Gloss.Data.Vector
import Graphics.Gloss.Geometry.Line
import Prelude hiding (Down)
import Data.List (partition)

import Aritmetiikka
import Talot
import Hemmot
import Kopteri

alkutilanne :: PeliTilanne
alkutilanne = 
      GameOn
            (Peli 
               0 
               (luoKopteri (0,0))
               [Talo 800 500 700, Talo 100 400 (-1000)]
               [Hemmo (700, 800), Hemmo(900, 800), Hemmo ((-1000),100), Hemmo (-800, 600), Hemmo (-500, 5000)]
               -- Yksi hemmoista tarkoituksella piilossa pilvessä
               
               -- Ja yksi hemuli ufojen kynsissä ihan hevon kuikassa.
               -- Leikin tuolla pelikuvan seurauksella.
            )


main :: IO ()
main = play
         (InWindow "Choplifter" (1000,700) (400,400))
         (light blue)
         24
         alkutilanne
         piirräPeliTilanne
         reagoiPeliTilanne
         päivitäPeliTilanne


reagoiPeliTilanne :: Event -> PeliTilanne -> PeliTilanne
reagoiPeliTilanne tapahtuma pelitilanne
  = case pelitilanne of
      GameOver cl -> GameOver cl
      GameOn cl  -> GameOn (reagoi tapahtuma cl)


reagoi :: Event -> Choplifter -> Choplifter
reagoi tapahtuma peli
   = case tapahtuma of
       EventKey (Char 'w') Down _ _ -> kopterille (muutaTehoa 2) peli
       EventKey (Char 's') Down _ _ -> kopterille (muutaTehoa (-2)) peli 
       EventKey (Char 'a') Down _ _ -> kopterille (kallista (-8)) peli
       EventKey (Char 'd') Down _ _ -> kopterille (kallista 8) peli
       _ -> peli


päivitäPeliTilanne :: Float -> PeliTilanne -> PeliTilanne
päivitäPeliTilanne aikaEdellisestä pelitilanne
  = case pelitilanne of
      GameOver peli -> GameOver peli
      GameOn peli   -> case törmääköTaloon (kopteriTörmäysviivat (cl_kopteri peli)) (cl_talot peli) of
                       Nothing -> GameOn (päivitäPeliä aikaEdellisestä peli)
                       Just Roottori -> GameOver peli
                       Just Laskuteline
                          | onkoHyväLaskeutuminen (cl_kopteri peli) 
                                      -> GameOn (päivitäPeliä aikaEdellisestä
                                                (kopterille laskeudu peli))
                          | otherwise -> GameOver peli

kopterille :: (Kopteri -> Kopteri) -> Choplifter -> Choplifter
kopterille f peli = peli{cl_kopteri = f (cl_kopteri peli)}

päivitäPeliä :: Float -> Choplifter -> Choplifter
päivitäPeliä aikaEdellisestä edellinenTila 
     = case edellinenTila of
         Peli aika kopteri talot hemmot
          -> let              
              paikka = kop_paikka kopteri
              nouseekoKyytiin hemmo = magV (hemmo_sijainti hemmo #- (kop_paikka kopteri)) < 50
              (hemmotKopteriin,hemmotUlkona) = partition nouseekoKyytiin hemmot
             in Peli (aika + aikaEdellisestä)                     
                     (päivitäKopteria aikaEdellisestä (genericLength hemmotKopteriin) kopteri)
                     talot
                     (map (päivitäHemmoa (flip korkeusKohdassa edellinenTila) 
                                         paikka)
                          hemmotUlkona)


törmääköTaloon :: ((Point,Point),(Point,Point)) -> [Talo] -> Maybe TörmäysKohta
törmääköTaloon törmäysviivat talot = fmap maximum1 (nonEmpty (mapMaybe törmääköYhteen talot))
   where 
    törmääköYhteen talo
      = let
          ((ala1,ala2),(ylä1,ylä2)) = törmäysviivat 
          (va,oy) = nurkkaPisteet talo
        in case (not (segClearsBox ala1 ala2 va oy), not (segClearsBox ylä1 ylä2 va oy)) of
             (True,False) -> Just Laskuteline
             (False,False) -> Nothing
             _ -> Just Roottori


piirräPeliTilanne :: PeliTilanne -> Picture
piirräPeliTilanne pelitilanne
   = case pelitilanne of
       GameOver cl -> piirräPeli cl <> translate (-300) 0 (color orange (text "GAME OVER"))
       GameOn cl   -> piirräPeli cl                       


piirräPeli :: Choplifter -> Picture
piirräPeli peli =  let                      
                      -- apuviivaAla = color red (line [va,oa])
                      -- apuviivaYlä = color red (line [va1,oa1])
                      
                      aika = cl_aika peli
                      talot = cl_talot peli

                      kopterikuva = piirräKopteri aika (cl_kopteri peli)

                      hemmoKuvat = map (piirräHemmo aika) (cl_hemmot peli)
                      taloKuvat = map piirräTalo talot
                      
                      pisteOtsikko = translate (-1400) 1200
                                    (scale 0.8 0.8
                                    (text "PELASTETUT HEMULIT:"))
                      pisteViesti = translate (-1400) 1050                                    
                                    (text (show (kop_hemmojaKyydissä (cl_kopteri peli))))
                      ufoViesti = translate (-700) 5500
                                  (text "PELASTA HEMULI ABDUKTIOLTA!!")   

                      peliKuva = kopterikuva                      
                                  <> maa
                                  <> pilvi1
                                  <> pilvi2
                                  <> ufo1
                                  <> ufo2
                                  <> pictures taloKuvat
                                  <> pictures hemmoKuvat
                                  <> pisteOtsikko
                                  <> pisteViesti
                                  <> ufoViesti
                                  -- <> apuviivaAla
                                  -- <> apuviivaYlä
                   in scale 0.25 0.25 (translate (0 - (fst(kop_paikka (cl_kopteri peli)))) (0 - (snd(kop_paikka (cl_kopteri peli)))) peliKuva)       
                                   -- (translate 0 (-180) peliKuva)
                                   



data PeliTilanne = GameOver Choplifter | GameOn Choplifter

data Choplifter
  = Peli
    {
      cl_aika :: Float              --  Aika pelin alusta
     
     ,cl_kopteri :: Kopteri         --  Kopterin tiedot

     ,cl_talot :: [Talo]            --  Esteet pelissä
     
     ,cl_hemmot :: [Hemmo]          --  Pelihahmot
    }




korkeusKohdassa :: Float -> Choplifter -> Float
korkeusKohdassa kohta peli = 
   maybe 0 maximum1 . nonEmpty . map (osuukoTaloon kohta) . cl_talot $ peli


maa :: Picture
maa = color green (translate 0 (-500) (rectangleSolid 5000 1000))

pilvi1 :: Picture
pilvi1 = color white (translate (-500) 1000 (circleSolid 100))
        <> color white (translate (-400) 1050 (circleSolid 100))
        <> color white (translate (-300) 970 (circleSolid 100))
        <> color white (translate (-200) 980 (circleSolid 100))
        <> color white (translate (-420) 1020 (circleSolid 100))

pilvi2 :: Picture
pilvi2 = color white (translate (-800) 700 (circleSolid 90))
        <> color white (translate (-700) 750 (circleSolid 80))
        <> color white (translate (-750) 620 (circleSolid 120))
        <> color white (translate (-650) 600 (circleSolid 110))
        <> color white (translate (-850) 550 (circleSolid 100))

ufo1 :: Picture
ufo1 = color green (translate (-500) 4500 (circleSolid 90))
        <> color black (translate (-550) 4450 (circleSolid 20))
        <> color red (translate (-450) 4550 (circleSolid 20))

ufo2 :: Picture
ufo2 = color green (translate (-100) 5300 (circleSolid 100))
        <> color red (translate (-50) 5250 (circleSolid 35))
        <> color black (translate (-150) 5350 (circleSolid 25))




