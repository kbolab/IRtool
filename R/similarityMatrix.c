#include <stdlib.h>  
#include <stdio.h>
#include <string.h>
# include <math.h>

#define MAX_STRING_LEN 80

void similarityMatrix(char **varNames, int *LengthvarNames, int *similarityJaccardNrow, double *similarityJaccard){
  
  int i;
  int length1;
  int length2;
  int k;
  int j;
  double similarity;
  char *array1;
  char *array2;
//  double sqsum1;
//  double sqsum2;
//  double prod;
//  int delim;
  int indice;
  int parametro;
  int uguali;
  int disuguali;
  double unione;
  int posizione;
    
//  i = 3;
//  k = 0;
//  j = 4;
  
  for (j=0; j<*LengthvarNames; j++)
  {
    //printf("%d\n", j);
    
    for (i=0; i<*LengthvarNames; i++)
    {
      //printf("%d\n", i);
      
      uguali = 0;
      unione = 0;
      disuguali = 0;
      similarity = 0;
      length1 = 0;
      length2 = 0;
      indice = 0;
      array1 = varNames[j];
      array2 = varNames[i];
      length1 = strlen(varNames[j]);
      length2 = strlen(varNames[i]);
      if (length1>=length2)
        indice = length1;
      else
        indice= length2;
      
      for (k=0; k<indice; k++){
        if (array1[k] == array2[k])
          uguali++;
        if (array1[k] != array2[k])
          disuguali++;
        unione = uguali+disuguali;
        similarity = 1-(uguali/unione);
      }
        parametro = j*(*similarityJaccardNrow)+i;
        similarityJaccard[parametro]=similarity;
        
//        printf("%d\n", j);
//        printf("%d\n", i);
//        printf("%f\n", *similarityJaccard);
        
    }
  }
//  printf("%d\n", i);
//  printf("%d\n", j);
  

//  printf("%d\n", uguali);
//  printf("%f\n", unione);
//  printf("%.2f\n", similarity);
  
  
//  return;


}
  