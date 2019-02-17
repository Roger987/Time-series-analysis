#include <R.h>
#include <Rinternals.h>
#include <Rmath.h>


int equals(int* pattern, int*symbols, int size){

	int i, j, aux = 0;
	for(i = 0; i < size; i++){
		if(pattern[i] == symbols[i]){
			aux++;
		}
	}
	if(aux == size){
		return 1;
	}
	else{
		return 0 ;
	}
}

SEXP TimeOrderedImputation(SEXP Rseries, SEXP Rdimension, SEXP Rdelay, SEXP Rpatterns, SEXP Relements, SEXP Rsymbols){

		int dimFat = 1, n, i, j, k, seriesize, dimension, delay, aux = 0;
		int* symbols_aux;
		int* patterns_aux;
		double* elements_aux;
	    SEXP Rprobability;

	    Rdimension = coerceVector(Rdimension, INTSXP);
	    Rdelay = coerceVector(Rdelay, INTSXP);
	    Rpatterns = coerceVector(Rpatterns, INTSXP);
	    Rsymbols = coerceVector(Rsymbols, INTSXP);
	    Relements = coerceVector(Relements, REALSXP);

	    dimension = INTEGER(Rdimension)[0];
	    delay = INTEGER(Rdelay)[0];
	    seriesize = length(Rseries);
	    n = dimension;

	    elements_aux = REAL(Relements);
	    symbols_aux = INTEGER(Rsymbols);
	    patterns_aux = INTEGER(Rpatterns);

		while(1 < n)
		{
			dimFat = n*dimFat;
			n--;
		}

		int **patterns = (int**) malloc((seriesize - (dimension - 1)*delay) * sizeof(int*));
		double **elements = (double**) malloc((seriesize - (dimension - 1)*delay) * sizeof(double*));
		for(i = 0; i < (seriesize - (dimension - 1)*delay); i++){
			patterns[i] = (int*) malloc(dimension * sizeof(int));
			elements[i] = (double*) malloc(dimension * sizeof(double));
		}

		int **symbols = (int**) malloc(dimFat * sizeof(int*));
		for(i = 0; i < dimFat; i++){
			symbols[i] = (int*) malloc(dimension * sizeof(int));
		}

		for(i = 0; i < seriesize - ((dimension - 1)*delay); i++){
			for(j = 0; j < dimension; j++){
				patterns[i][j] = patterns_aux[aux];
				elements[i][j] = elements_aux[aux];
				aux++;
			}
		}
		aux = 0;
		for(i = 0; i < dimFat; i++){
			for(j = 0; j < dimension; j++){
				symbols[i][j] = symbols_aux[aux];
				aux++;
			}
		}

	    PROTECT(Rprobability = allocVector(REALSXP, dimFat));
	    for(i = 0; i < dimFat; i++){
	        REAL(Rprobability)[i] = 0;
	    }

	    for(i = 0; i < seriesize - ((dimension - 1)*delay); i++){
        	for(j = 0; j < dimFat; j++){
            	if(equals(patterns[i], symbols[j], dimension)){
                	REAL(Rprobability)[j]++;
            	}
        	}
		}

		for(i = 0; i < dimFat; i++){
			REAL(Rprobability)[i] = REAL(Rprobability)[i]/(seriesize - (dimension - 1)*delay);
		}

		UNPROTECT(1);

    	return Rprobability;

}