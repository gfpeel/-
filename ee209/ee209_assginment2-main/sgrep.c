#include <stdio.h>
#include <stdlib.h>
#include <string.h> /* for skeleton code */
#include <unistd.h> /* for getopt */
#include "str.h"

#define MAX_STR_LEN 1023

#define FALSE 0
#define TRUE  1

/*
 * Fill out your own functions here (If you need)
 */

/*--------------------------------------------------------------------*/
/* PrintUsage()
   print out the usage of the Simple Grep Program                     */
/*--------------------------------------------------------------------*/
void
PrintUsage(const char* argv0)
{
  const static char *fmt =
	  "Simple Grep (sgrep) Usage:\n"
	  "%s pattern [stdin]\n";

  printf(fmt, argv0);
}
/*-------------------------------------------------------------------*/
/* SearchPattern()
   Your task:
   1. Do argument validation
   - String or file argument length is no more than 1023
   - If you encounter a command-line argument that's too long,
   print out "Error: pattern is too long"

   2. Read the each line from standard input (stdin)
   - If you encounter a line larger than 1023 bytes,
   print out "Error: input line is too long"
   - Error message should be printed out to standard error (stderr)

   3. Check & print out the line contains a given string (search-string)

   Tips:
   - fgets() is an useful function to read characters from file. Note
   that the fget() reads until newline or the end-of-file is reached.
   - fprintf(sderr, ...) should be useful for printing out error
   message to standard error

   NOTE: If there is any problem, return FALSE; if not, return TRUE  */
/*-------------------------------------------------------------------*/
int
SearchPattern(const char *pattern)
{
  char buf[MAX_STR_LEN + 2];

  /*
   *  TODO: check if pattern is too long
   */
  if (StrGetLength(pattern) > 1023) {
    fprintf(stderr, "Error: pattern is too long\n");
    return FALSE;
  }
  char* star_ptr1;
  char* star_ptr2;
  char* cpystart;
  char cpypattern[1024];
  char* find_init_ptr;
  int print;



  /* Read one line at a time from stdin, and process each line */
  while (fgets(buf, sizeof(buf), stdin)) {

    /* TODO: check the length of an input line */
    if (StrGetLength(buf) > 1023) {
    fprintf(stderr, "Error: input line is too long\n");
    return FALSE;
  }
    /* TODO: fill out this function */
    print = 0;

    if (StrFindChr(pattern, '*') == NULL) {
      if (StrFindStr(buf, pattern) != NULL) {
        printf("%s", buf);
      }
    }
    else {
      star_ptr1 = StrFindChr(pattern, '*');
      *star_ptr1 = '\0';
      StrCopy(cpypattern, pattern);
      *star_ptr1 = '*';
      if ((find_init_ptr = StrFindStr(buf, cpypattern)) != NULL) {
        find_init_ptr++;
        print = 1;
      }
      
      /* pattern != "...*" */
      /* pattern == "*asdf*" */
      while (print && (*(star_ptr1 + 1) != '\0')
          && ((star_ptr2 = StrFindChr(star_ptr1 + 1, '*')) != NULL)) {

        /* cpy "asdf" */
        cpystart = star_ptr1 + 1;
        *star_ptr2 = '\0';
        StrCopy(cpypattern, cpystart);
        *star_ptr2 = '*';
        star_ptr1 = star_ptr2;
        
        if ((find_init_ptr = StrFindStr(find_init_ptr, cpypattern)) == NULL) {
          print = 0;
        }
        find_init_ptr++;
      }
      if (print && (*(star_ptr1 + 1) != '\0')) {
        cpystart = star_ptr1 + 1;
        StrCopy(cpypattern, cpystart);   

        if ((find_init_ptr = StrFindStr(find_init_ptr, cpypattern)) == NULL) {
          print = 0;
        }
      }

      if (print) {
        printf("%s", buf);
      }
    }
  }


  return TRUE;
}
/*-------------------------------------------------------------------*/
int
main(const int argc, const char *argv[])
{
  /* Do argument check and parsing */
  if (argc < 2) {
	  fprintf(stderr, "Error: argument parsing error\n");
	  PrintUsage(argv[0]);
	  return (EXIT_FAILURE);
  }

  return SearchPattern(argv[1]) ? EXIT_SUCCESS:EXIT_FAILURE;
}
