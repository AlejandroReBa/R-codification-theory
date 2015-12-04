#Create by Alejandro Reyes
#Creative Commons. Apache 2.0
GeneralEntropy <- function(probabilityVector){ #Entropy for any length vector
		vectorLength <- length(probabilityVector)
		probabilitySum <- 0
		res <- 0
		notSense <-0
	
		#if vectorLength = 0, throw an error

		if(vectorLength == 1){
			vectorLength <-2
			probabilityVector <-c(probabilityVector[1],1-probabilityVector[1])
		}
	
		for (i in 1:vectorLength){
			if(probabilityVector[i] < 0){
				stop("Probability can't be negative")
			}else if (probabilityVector[i] == 1){
				notSense <- 1
			}
				probabilitySum <- probabilitySum + probabilityVector[i];
			}
		
	
		if(probabilitySum != 1){
			stop("Sum of probability must be 1")
		}
	
		if (notSense == 0){
			for(i in 1:vectorLength){
				if(probabilityVector[i] != 0) #no tengo en cuenta probabilidades = 0
					res <- res + probabilityVector[i]*log2(1/probabilityVector[i]);
			}
		}else{ #if any probability value is 1 doesn't exist entropy --> equal 0
			res <- 0;
		}
	
		return (res)
	}
