Bandt.Pompe <- function(elements, dimension, delay, size){
  
  dyn.load("BandtPompe.so")
  
  probability <- .Call("BandtPompe", elements, dimension, delay, size)
  
  return (probability)
}

