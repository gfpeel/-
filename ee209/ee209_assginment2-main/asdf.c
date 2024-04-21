#include <assert.h> /* to use assert() */
#include <stdio.h>
#include <stdlib.h> /* for strtol() */
#include <string.h>
#include <strings.h>
#include "str.h"
char *StrCopy(char *pcDest, const char* pcSrc);
size_t StrGetLength(const char* pcSrc);
char *StrCopy(char *pcDest, const char* pcSrc);

int main() {
    /*
    char arr1[20] = "asdf";
    char arr2[20] = "qqqq";
    char arr3[20] = "asdf";
    char arr4[20] = "qqqq";

    StrConcat(arr1, arr2);
    if(StrConcat(arr1, arr2) == NULL) {
        printf("NULL\n");
    }
    else{
        printf("%s\n", StrConcat(arr1, arr2));
    }
    if(strcat(arr3, arr4) == NULL) {
        printf("NULL\n");
    }
    else{
        printf("%s\n", strcat(arr3, arr4));
    }
    */
    char a[10] = "-1213";
    printf("%ld", strtol(a, NULL, 10));
    
    //printf("%ld\n", StrGetLength(arr1));

    return 0;
}

char *StrConcat(char *pcDest, const char* pcSrc)
{
  size_t lenth = strlen(pcDest);
  strcpy(pcDest + lenth, pcSrc);
  
  return pcDest;
}

char *StrCopy(char *pcDest, const char* pcSrc)
{
  
  assert(pcSrc); /* NULL address, 0, and FALSE are identical. */
  assert(pcDest); /* NULL address, 0, and FALSE are identical. */

  char *cpyDest = pcDest;

  for(; *pcSrc != '\0'; pcSrc++) {
    *cpyDest++ = *pcSrc;
  }
  *cpyDest = '\0';

  return pcDest;
}

size_t StrGetLength(const char* pcSrc)
{
  const char *pcEnd;
  assert(pcSrc); /* NULL address, 0, and FALSE are identical. */
  pcEnd = pcSrc;
	
  while (*pcEnd) /* null character and FALSE are identical. */
    pcEnd++;

  return (size_t)(pcEnd - pcSrc);
}