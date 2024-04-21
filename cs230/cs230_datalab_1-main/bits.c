/* 
 * CS:APP Data Lab 
 * 
 * <Donghyun Lee cs20230490>
 * 
 * bits.c - Source file with your solutions to the Lab.
 *          This is the file you will hand in to your instructor.
 *
 * WARNING: Do not include the <stdio.h> header; it confuses the dlc
 * compiler. You can still use printf for debugging without including
 * <stdio.h>, although you might get a compiler warning. In general,
 * it's not good practice to ignore compiler warnings, but in this
 * case it's OK.  
 */

#if 0
/*
 * Instructions to Students:
 *
 * STEP 1: Read the following instructions carefully.
 */

You will provide your solution to the Data Lab by
editing the collection of functions in this source file.

INTEGER CODING RULES:
 
  Replace the "return" statement in each function with one
  or more lines of C code that implements the function. Your code 
  must conform to the following style:
 
  int Funct(arg1, arg2, ...) {
      /* brief description of how your implementation works */
      int var1 = Expr1;
      ...
      int varM = ExprM;

      varJ = ExprJ;
      ...
      varN = ExprN;
      return ExprR;
  }

  Each "Expr" is an expression using ONLY the following:
  1. Integer constants 0 through 255 (0xFF), inclusive. You are
      not allowed to use big constants such as 0xffffffff.
  2. Function arguments and local variables (no global variables).
  3. Unary integer operations ! ~
  4. Binary integer operations & ^ | + << >>
    
  Some of the problems restrict the set of allowed operators even further.
  Each "Expr" may consist of multiple operators. You are not restricted to
  one operator per line.

  You are expressly forbidden to:
  1. Use any control constructs such as if, do, while, for, switch, etc.
  2. Define or use any macros.
  3. Define any additional functions in this file.
  4. Call any functions.
  5. Use any other operations, such as &&, ||, -, or ?:
  6. Use any form of casting.
  7. Use any data type other than int.  This implies that you
     cannot use arrays, structs, or unions.

 
  You may assume that your machine:
  1. Uses 2s complement, 32-bit representations of integers.
  2. Performs right shifts arithmetically.
  3. Has unpredictable behavior when shifting an integer by more
     than the word size.

EXAMPLES OF ACCEPTABLE CODING STYLE:
  /*
   * pow2plus1 - returns 2^x + 1, where 0 <= x <= 31
   */
  int pow2plus1(int x) {
     /* exploit ability of shifts to compute powers of 2 */
     return (1 << x) + 1;
  }

  /*
   * pow2plus4 - returns 2^x + 4, where 0 <= x <= 31
   */
  int pow2plus4(int x) {
     /* exploit ability of shifts to compute powers of 2 */
     int result = (1 << x);
     result += 4;
     return result;
  }

FLOATING POINT CODING RULES

For the problems that require you to implent floating-point operations,
the coding rules are less strict.  You are allowed to use looping and
conditional control.  You are allowed to use both ints and unsigneds.
You can use arbitrary integer and unsigned constants.

You are expressly forbidden to:
  1. Define or use any macros.
  2. Define any additional functions in this file.
  3. Call any functions.
  4. Use any form of casting.
  5. Use any data type other than int or unsigned.  This means that you
     cannot use arrays, structs, or unions.
  6. Use any floating point data types, operations, or constants.


NOTES:
  1. Use the dlc (data lab checker) compiler (described in the handout) to 
     check the legality of your solutions.
  2. Each function has a maximum number of operators (! ~ & ^ | + << >>)
     that you are allowed to use for your implementation of the function. 
     The max operator count is checked by dlc. Note that '=' is not 
     counted; you may use as many of these as you want without penalty.
  3. Use the btest test harness to check your functions for correctness.
  4. Use the BDD checker to formally verify your functions
  5. The maximum number of ops for each function is given in the
     header comment for each function. If there are any inconsistencies 
     between the maximum ops in the writeup and in this file, consider
     this file the authoritative source.

/*
 * STEP 2: Modify the following functions according the coding rules.
 * 
 *   IMPORTANT. TO AVOID GRADING SURPRISES:
 *   1. Use the dlc compiler to check that your solutions conform
 *      to the coding rules.
 *   2. Use the BDD checker to formally verify that your solutions produce 
 *      the correct answers.
 */


#endif
/* Copyright (C) 1991-2018 Free Software Foundation, Inc.
   This file is part of the GNU C Library.

   The GNU C Library is free software; you can redistribute it and/or
   modify it under the terms of the GNU Lesser General Public
   License as published by the Free Software Foundation; either
   version 2.1 of the License, or (at your option) any later version.

   The GNU C Library is distributed in the hope that it will be useful,
   but WITHOUT ANY WARRANTY; without even the implied warranty of
   MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
   Lesser General Public License for more details.

   You should have received a copy of the GNU Lesser General Public
   License along with the GNU C Library; if not, see
   <http://www.gnu.org/licenses/>.  */
/* This header is separate from features.h so that the compiler can
   include it implicitly at the start of every compilation.  It must
   not itself include <features.h> or any other header that includes
   <features.h> because the implicit include comes before any feature
   test macros that may be defined in a source file before it first
   explicitly includes a system header.  GCC knows the name of this
   header in order to preinclude it.  */
/* glibc's intent is to support the IEC 559 math functionality, real
   and complex.  If the GCC (4.9 and later) predefined macros
   specifying compiler intent are available, use them to determine
   whether the overall intent is to support these features; otherwise,
   presume an older compiler has intent to support these features and
   define these macros by default.  */
/* wchar_t uses Unicode 10.0.0.  Version 10.0 of the Unicode Standard is
   synchronized with ISO/IEC 10646:2017, fifth edition, plus
   the following additions from Amendment 1 to the fifth edition:
   - 56 emoji characters
   - 285 hentaigana
   - 3 additional Zanabazar Square characters */
/* We do not support C11 <threads.h>.  */

/* 
 * isAscii - return 1 if 0x30 <= x <= 0x39 (ASCII codes for characters '0' to '9')
 *   Example: isAscii(0x35) = 1.
 *            isAscii(0x3a) = 0.
 *            isAscii(0x05) = 0.
 *   Legal ops: ! ~ & ^ | + << >>
 *   Max ops: 15
 *   Rating: 3
 */
int isAscii(int x) {
  //0x30==00110000, 0x39==00111001
  //if it is bigger then 0x30
  int con_1 = 0x2f;
  int does_big = (con_1 + (~x + 1)) >> 31;

  //if it is smaller then 0x39
  int con_2 = 0x3a;
  int does_small = (x + (~con_2 + 1)) >> 31;

  //retrun 1 only if both val is 0xfff... 
  int result = (does_big & does_small) & 1;
  return result;
}

/* 
 * fourthBits - return word with every fourth bit (starting from the LSB) set to 1
 *   Legal ops: ! ~ & ^ | + << >>
 *   Max ops: 8
 *   Rating: 1
 */
int fourthBits(void) {
  int modul_bit = 0x11;
  modul_bit |= (modul_bit<<8);
  modul_bit |= (modul_bit<<16);

  return modul_bit;
}

/*
 * countOneBits - returns count of number of 1's in word
 *   Examples: countOneBits(5) = 2, countOneBits(7) = 3
 *   Legal ops: ! ~ & ^ | + << >>
 *   Max ops: 40
 *   Rating: 4
 */
int countOneBits(int x) {
  //m = 0x01010101
  int m = 0x01;
  int result_count = 0;
  m |= m << 8;
  m |= m << 16;
  //0x11111111(4bit < 32) => use 8bit(0x01010101)
  
  result_count = (x & m);
  result_count += x >> 1 & m;
  result_count += x >> 2 & m;
  result_count += x >> 3 & m;
  result_count += x >> 4 & m;
  result_count += x >> 5 & m;
  result_count += x >> 6 & m;
  result_count += x >> 7 & m;

  result_count += result_count >> 16;
  result_count += result_count >> 8;
  result_count &= 0xff;

  return result_count;
}

/* 
 * countPattern - returns the number of found "1111" in the given number x
 *   Legal ops: ! ~ & ^ | + << >>
 *   Max ops: 80
 *   Rating: 6
 */
int countPattern(int x) {
  //also use 8bits piece
  //piece = 0x0f0f0f0f
  int piece = 0;
  int m = 0;
  int result = 0;
  int sign_bit = 0;
  int second_bit = 0;
  int fault_count = 0;

  piece = 0x0f;
  piece |= piece << 8;
  piece |= piece << 16;
  //m = 0x01010101
  m = 0x01;
  m |= m << 8;
  m |= m << 16;

  //max => 0x80808080
  result = ((x & piece) + m) & piece << 4;
  result += ((x >> 1 & piece) + m) & piece << 4;
  result += ((x >> 2 & piece) + m) & piece << 4;
  result += ((x >> 3 & piece) + m) & piece << 4;
  result += ((x >> 4 & piece) + m) & piece << 4;
  result += ((x >> 5 & piece) + m) & piece << 4;
  result += ((x >> 6 & piece) + m) & piece << 4;
  result += ((x >> 7 & piece) + m) & piece << 4;

  //sign bit
  sign_bit = (x >> 31 & 1);
  second_bit = (x >> 30 & sign_bit);
  fault_count = sign_bit + second_bit + (x >> 29 & second_bit);
  
  result = (result >> 4) & ~(0xf << 28);
  result += result >> 16;
  result += result >> 8;
  result &= 0xff;
  result += ~fault_count + 1;


  return result;
}

/* 
 * subOverflowFree - Determine if can compute x-y without overflow
 *   Example: subOverflowFree(0x80000000,0x80000000) = 1,
 *            subOverflowFree(0x80000000,0x70000000) = 0, 
 *   Legal ops: ! ~ & ^ | + << >>
 *   Max ops: 20
 *   Rating: 3
 */
int subOverflowFree(int x, int y) {
  int minus_y = ~y + 1;
  //"sign of (x-y) == sign of x" or "sign of x == sign of y"
  int result =  (!(((x + minus_y) ^ x) >> 31)) | (!(((x ^ y)) >> 31));
  return result;
}

/*
 * mulSevenSixteenth - multiply by 7/16 rounding toward 0, avoiding overflow.
 *    Examples: mulSevenSixteenth(22) = 3
 *    mulSevenSixteenth(0x40000000) = 469762048 (no overflow)

 *    Legal ops: ! ~ & ^ | + << >>
 *    Max ops: 25
 *    Rating: 4
 */
int mulSevenSixteenth(int x) {
  int lastnum = ((x & 0x0f) + ((x << 1) & 0x0f) + ((x << 2) & 0x0f));

  int round = !!(lastnum & 0x0f); //round: 0 => no need to round, 1 => need to round
  int sign = x >> 31; //pos: -0, neg: +1

  int result = ((x >> 4) + (x >> 3) + (x >> 2)) + (lastnum >> 4)  + (round & sign);

  return result;
}

/* 
 * sm2tc - Convert from sign-magnitude to two's complement
 *   where the MSB is the sign bit
 *   Example: sm2tc(0x80000005) = -5.
 *   Legal ops: ! ~ & ^ | + << >>
 *   Max ops: 15
 *   Rating: 4
 */
int sm2tc(int x) {
  int mag = x & ~(1 << 31);//sign: 0=>pos, 1=>neg
  int sign = x >> 31;
  int result = (mag & ~sign) + ((~mag +1) & sign);
  return result;
}

/* 
 * float_abs - Return bit-level equivalent of absolute value of f for
 *   floating point argument f.
 *   Both the argument and result are passed as unsigned int's, but
 *   they are to be interpreted as the bit-level representations of
 *   single-precision floating point values.
 *   When argument is NaN, return argument..
 *   Legal ops: Any integer/unsigned operations incl. ||, &&. also if, while
 *   Max ops: 10
 *   Rating: 4
 */
unsigned float_abs(unsigned uf) {
  int sign_bit = ~(1 << 31);
  while((((uf >> 23) + 1) >> 9) && uf << 9) return uf; // nan -> uf
  return uf & sign_bit; // otherwise -> abs(uf)
}

/* 
 * integer_to_float - Return bit-level equivalent of expression (float) x
 *   Result is returned as unsigned int, but
 *   it is to be interpreted as the bit-level representation of a
 *   single-precision floating point values.
 *   Legal ops: Any integer/unsigned operations incl. ||, &&. also if, while
 *   Max ops: 30
 *   Rating: 6
 */
unsigned integer_to_float(int x) {
  unsigned int abs_x = x;
  int sign = (x >> 31);
  int i = 0;
  unsigned int x_copy = 0;
  unsigned int float_bit = 0;
  int shift = 0;
  int round = 0;
  int exp_bit = 0;
  unsigned int result = 0;

  abs_x = (abs_x ^ sign) + (sign & 1);

  x_copy = abs_x;
  while(x_copy){
    x_copy = x_copy >> 1;
    i++;
  }

  while(i){
    float_bit = 0;
    shift = (i - 24);
    round = 0;
    if ((24 - i) >> 31) {
      float_bit = abs_x >> shift;
      //rounding
      
      if (abs_x & (1 << (shift - 1))) {
        if ((shift >> 1) && (abs_x << (34 + (~ shift)))) round = 1;
        if (abs_x & (1 << shift)) round = 1;
      }
      
    }
    else float_bit = abs_x << (- shift);

    float_bit ^= (1 << 23);

    exp_bit = 126 + i;

    sign = sign << 31;

    result = sign + (exp_bit << 23) + float_bit + round;
    return result;
  }
  return 0;
}

/* 
 * real_quarter - Return bit-level equivalent of expression 0.25*f for
 *   floating point argument f.
 *   Both the argument and result are passed as unsigned int's, but
 *   they are to be interpreted as the bit-level representation of
 *   single-precision floating point values.
 *   When argument is NaN, return argument
 *   Legal ops: Any integer/unsigned operations incl. ||, &&. also if, while
 *   Max ops: 30
 *   Rating: 8
 */
  unsigned real_quarter(unsigned uf) {
    unsigned int exp_bit = (uf << 1 >> 24);
    unsigned int f = 0;
    int round = 0;
    int i = 0;
    unsigned int result;

    if ((exp_bit - 3) >> 31){ // exp <= 2
      f = uf << 9 >> 9;
      f += (1 << 23);
      round = 0;
      i = 0;
      if ((exp_bit - 2) >> 31){
        if (exp_bit - 1){
          //exp == 0
          f ^= (1 << 23);
        }
        //exp == 1
        i++;
      }
      //exp == 2
      i++;

      if (f & i) {
       if (i ^ 1) {
        if (f & 1) round = 1;
       }
       if (f & (1 << i)) round = 1;
      }
      f = f >> i;

      result = (uf & (1 << 31)) + f + round;
      return result;
    }
    /*1. exp 2 -> exp = 0, f = 1.11111... -> 0.11111,1...
    2. exp 1 -> exp = 0, f = 1.11111... -> 0.01111,11...
    3. exp 0 -> exp = 0, f = 0.11111... -> 0.00111,11...*/
    if (exp_bit ^ 0xff) return uf - (1 << 24); // not nan
    return uf; // nan
  }