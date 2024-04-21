#include <assert.h> /* to use assert() */
#include <stdio.h>
#include <stdlib.h> /* for strtol() */
#include <string.h>
#include <strings.h>
#include "str.h"

/* Your task is: 
   1. Rewrite the body of "Part 1" functions - remove the current
      body that simply calls the corresponding C standard library
      function.
   2. Write appropriate comment per each function
*/

/* Part 1 */
/*------------------------------------------------------------------------*/
size_t StrGetLength(const char* pcSrc)
{
  const char *pcEnd;
  assert(pcSrc); /* NULL address, 0, and FALSE are identical. */
  pcEnd = pcSrc;
	
  while (*pcEnd) /* null character and FALSE are identical. */
    pcEnd++;

  return (size_t)(pcEnd - pcSrc);
}
/*------------------------------------------------------------------------*/
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
/*------------------------------------------------------------------------*/
int StrCompare(const char* pcS1, const char* pcS2)
{
    assert(pcS1); /* NULL address, 0, and FALSE are identical. */
    assert(pcS2); /* NULL address, 0, and FALSE are identical. */

    while(1) {
        /* (*pcS1, *pcS2) == ('\0', '\0') */
        if (*pcS1=='\0' && *pcS2=='\0') {
            break;
        }
        /* (*pcS1, *pcS2) == ( , '\0') */
        else if (*pcS2 == '\0') {
            if (*pcS1 != '\0') {
                break;
            }
            pcS1++;
        }
        /* (*pcS1, *pcS2) == ('\0', ) */
        else if (*pcS1 == '\0') {
            if (*pcS2 != '\0') {
                break;
            }
            pcS2++;
        }
        /* (*pcS1, *pcS2) == ( , ) */
        else {
            if ((*pcS1) != (*pcS2)) {
                break;
            }
            pcS1++;
            pcS2++;
        }
    }
    return (*pcS1)-(*pcS2);
}
/*------------------------------------------------------------------------*/
char *StrFindChr(const char* pcHaystack, int c)
{
  assert(pcHaystack); /* NULL address, 0, and FALSE are identical. */

    char* ptr = (char*) pcHaystack;
    while(*ptr != '\0') {
        if(*ptr == c) {
            break;
        }
        ptr++;
    }
  
    return (*ptr == '\0' && c != '\0') ? NULL : ptr;
}
/*------------------------------------------------------------------------*/
char *StrFindStr(const char* pcHaystack, const char *pcNeedle)
{
  assert(pcHaystack); /* NULL address, 0, and FALSE are identical. */
  assert(pcNeedle); /* NULL address, 0, and FALSE are identical. */

  if (*pcNeedle == '\0') {
    return (char*) pcHaystack;
  }
  const char* cpyh;
  const char* cpyn;
  while (*pcHaystack != '\0') {
    cpyh = pcHaystack;
    cpyn = pcNeedle;
    while ((*cpyn != '\0') && (*cpyh == *cpyn)) {
      cpyh++;
      cpyn++;
    }
    if (*cpyn == '\0') {
      return (char*) pcHaystack;
    }
    pcHaystack++;
  }

  return NULL;
}
/*------------------------------------------------------------------------*/
char *StrConcat(char *pcDest, const char* pcSrc)
{
  assert(pcDest); /* NULL address, 0, and FALSE are identical. */
  assert(pcSrc); /* NULL address, 0, and FALSE are identical. */
  
  size_t lenth = StrGetLength(pcDest);
  StrCopy(pcDest + lenth, pcSrc);
  
  return pcDest;
}

/*------------------------------------------------------------------------*/
long int StrToLong(const char *nptr, char **endptr, int base)
{
  assert(nptr); /* NULL address, 0, and FALSE are identical. */

  /* handle only when base is 10 */
  if (base != 10) return 0;

  /* TODO: fill this function */
  long int num = 0;
  while (*nptr == ' ' || *nptr == '\t' || *nptr == '\n' || *nptr == '\r' || *nptr == '\v' || *nptr == '\f') {
    nptr++;
  }

  if (*nptr == '-') {
    nptr++;
    while (47 < *nptr && *nptr < 58) {
      if (num < LONG_MIN / 10 || num * (10) < LONG_MIN + (*nptr - 48)) {
        num = LONG_MIN;
        break;
      }
      num =  num * (10) - (*nptr - 48);
      nptr++;
    }
  }
  else {
    if (*nptr == '+') {
      nptr++;
    }
    while (47 < *nptr && *nptr < 58) {
      if (num > LONG_MAX / 10 || num * 10 > LONG_MAX - (*nptr - 48)) {
        num = LONG_MAX;
        break;
      }
      num =  num * (10) + (*nptr - 48);
      nptr++;
    }
  }
  
  while (47 < *nptr && *nptr < 58) {
    nptr++;
  }
  if(endptr != NULL) {
    *endptr = (char*) nptr;
  }

  return num;
}

/*------------------------------------------------------------------------*/
int StrCaseCompare(const char *pcS1, const char *pcS2)
{
  assert(pcS1); /* NULL address, 0, and FALSE are identical. */
  assert(pcS2); /* NULL address, 0, and FALSE are identical. */

  /* TODO: fill this function */
  char c1;
  char c2;

      while (*pcS1 && *pcS2) {
        c1 = *pcS1;
        c2 = *pcS2;
        if ('A' - 1 < c1 && c1 < 'Z' + 1) {
          c1 = c1 + ('a'-'A');
        }
        if ('A' - 1 < c2 && c2 < 'Z' + 1) {
          c2 = c2 + ('a'-'A');
        }
        if (c1 != c2) {
          return c1 - c2;
        } 
        pcS1++;
        pcS2++;
    }
  return *pcS1 - *pcS2;
}