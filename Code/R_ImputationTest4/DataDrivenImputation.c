#include <R.h>
#include <Rinternals.h>
#include <Rmath.h>

int indx;

void permute(int* element, int init, int end, int* array) { 
    
    int i, aux;

    if(init == end){
        for(i = 0; i <= end; i++){ 
            array[indx] = element[i];
            indx++;
        }   
    }

    else{ 

        for (i = init; i <= end; i++) { 
            aux = element[init];
            element[init] = element[i];
            element[i] = aux;
            permute(element, init + 1, end, array); 
            aux = element[init];
            element[init] = element[i];
            element[i] = aux;
        } 
    } 
} 

int** myPermute(int* pattern, int* vec_aux, int dimension, int size, int sizeFat){

    int i, j = 0;
    
    int* array2 = (int*) malloc(size * sizeof(int));
    for(i = 0; i < dimension; i++){
        if(vec_aux[i]){
            array2[j] = pattern[i];
            j++;
        }
    }

    int* dup = (int*) malloc((sizeFat*j) * sizeof(int));

    indx = 0;
    permute(array2, 0, size - 1, dup);//

    indx = 0;

    int **matrixFinal = (int**) malloc(sizeFat * sizeof(int*));
    for(i = 0; i < sizeFat; i++){
        matrixFinal[i] = (int*) malloc(dimension * sizeof(int));
    }

    int aux = 0;
    for(i = 0; i < sizeFat; i++){
        int* elementAux = vec_aux;
        for(j = 0; j < dimension; j++){
            if(elementAux[j]){
                matrixFinal[i][j] = dup[aux];
                aux++;
            }
            else{
                matrixFinal[i][j] = pattern[j];
            }
        }
    }
    
    return matrixFinal;
}

int check_duplicated(double *pattern, int size){
	int i, j, aux = 0;
	for(i = 0; i < size; i++){
		for(j = i+1; j < size; j++){
			if(pattern[i] == pattern[j]){
				aux++;
				return 0;
				break;
			}
		}
	}
	if(aux == 0){
		return 1;
	}
}

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

SEXP DataDrivenImputation(SEXP Rseries, SEXP Rdimension, SEXP Rdelay, SEXP Rpatterns, SEXP Relements, SEXP Rsymbols, SEXP RCompleteCaseprob){


        int dimFat = 1, n, i, j, k, seriesize, dimension, delay, aux = 0, duplicated_elements = 0;
        int* symbols_aux;
        int* patterns_aux;
        double* elements_aux;
        double* CompleteCaseprob;
        SEXP Rprobability;

        Rdimension = coerceVector(Rdimension, INTSXP);
        Rdelay = coerceVector(Rdelay, INTSXP);
        Rpatterns = coerceVector(Rpatterns, INTSXP);
        Rsymbols = coerceVector(Rsymbols, INTSXP);
        Relements = coerceVector(Relements, REALSXP);
        RCompleteCaseprob = coerceVector(RCompleteCaseprob, REALSXP);

        dimension = INTEGER(Rdimension)[0];
        delay = INTEGER(Rdelay)[0];
        seriesize = length(Rseries);
        n = dimension;

        elements_aux = REAL(Relements);
        symbols_aux = INTEGER(Rsymbols);
        patterns_aux = INTEGER(Rpatterns);
        CompleteCaseprob = REAL(RCompleteCaseprob);

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

            if(check_duplicated(elements[i], dimension)){
                for(j = 0; j < dimFat; j++){
                    if(equals(patterns[i], symbols[j], dimension)){
                        REAL(Rprobability)[j]++;
                    }
                }
            }
            else{

                double w = 0;
                int size = 0, sizeFat = 1;

                int* vec_aux = (int*) malloc(dimension * sizeof(int));
                for(j = 0; j < dimension; j++){
                    vec_aux[j] = 0;
                }

                for(j = 0; j < dimension; j++){
                    for(k = j+1; k < dimension; k++){
                        if(elements[i][j] == elements[i][k]){
                            vec_aux[j] = 1;
                            vec_aux[k] = 1;
                        }
                    }
                }

                for(j = 0; j < dimension; j++){
                    if(vec_aux[j]){
                        size++;
                    }
                }

                n = size;

                while(1 < n)
                {
                    sizeFat = n*sizeFat;
                    n--;
                }

                int** permutation_patterns = myPermute(patterns[i], vec_aux, dimension, size, sizeFat);
                
                for(j = 0; j < sizeFat; j++){
                    for(k = 0; k < dimFat; k++){
                        if(equals(permutation_patterns[j], symbols[k], dimension)){
                            w += CompleteCaseprob[k];
                        }
                    }
                }

                for(j = 0; j < sizeFat; j++){
                    for(k = 0; k < dimFat; k++){
                        if(equals(permutation_patterns[j], symbols[k], dimension)){
                            if(CompleteCaseprob[k]/w > 0){
                                REAL(Rprobability)[k] += CompleteCaseprob[k]/w;

                            }
                        }
                    }
                }


            }

    	}

        for(i = 0; i < dimFat; i++){
            REAL(Rprobability)[i] = REAL(Rprobability)[i]/((seriesize - (dimension - 1)*delay));
        }

        UNPROTECT(1);

    	return Rprobability;

}