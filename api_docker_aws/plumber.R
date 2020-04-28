#
# This is a Plumber API. You can run the API by clicking
# the 'Run API' button above.
#
# Find out more about building APIs with Plumber here:
#
#    https://www.rplumber.io/
#

library(plumber)

#* @apiTitle Exemplo de API [modelo linear]

#* Echo back the input
#* @param msg The message to echo
#* @get /echo
function(msg = "") {
    list(msg = paste0("The message is: '", msg, "'"))
}

#* Plot a histogram
#* @png
#* @get /plot
function() {
    rand <- rnorm(100)
    hist(rand)
}

#* Return the sum of two numbers
#* @param a The first number to add
#* @param b The second number to add
#* @post /sum
function(a, b) {
    as.numeric(a) + as.numeric(b)
}


modelo <- lm(Ozone ~ Wind + Temp + Month, data = airquality)


#* Retorna a distancia percorrida em funcao da velocidade
#* @param Wind Velocidade do vento (mph)
#* @param Temp Temperautura (degrees F)
#* @param Month Mes (1-12)
#* @post /modelo
function(Wind, Temp, Month){
  
  Wind <- as.double(Wind)
  Temp <- as.double(Temp)
  Month <- as.double(Month)
  
  resultado <- predict(modelo, list(Wind=Wind, Temp=Temp, Month=Month))
  
  return(resultado)
  
}


