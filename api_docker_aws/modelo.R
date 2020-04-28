library(plumber)

modelo <- lm(Ozone ~ Wind + Temp + Month, data = airquality)

#* @apiTitle Exemplo de API [modelo linear]

#* Retorna a distancia percorrida em funcao da velocidade
#* @param Wind Velocidade do vento (mph)
#* @param Temp Temperautura (degrees F)
#* @param Month Mes (1-12)
#* @post /modelo

function(Wind, Temp, Month) {
  
  Wind  <- as.double(Wind)
  Temp  <- as.double(Temp)
  Month <- as.double(Month)
  
  resultado <- predict(modelo, list(Wind = Wind,
                                    Temp = Temp,
                                    Month = Month))
  
  return(resultado)
  
}

