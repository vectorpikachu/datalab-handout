/*
 * CS:APP Data Lab
 *
 * <Please put your name and userid here>
 * Hangzhou Lyu 2200013126
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
/* Copyright (C) 1991-2022 Free Software Foundation, Inc.
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
   <https://www.gnu.org/licenses/>.  */
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
/*
 * bitXnor - ~(x^y) using only ~ and |
 *   Example: bitXnor(6, -5) = 2
 *   Legal ops: ~ |
 *   Max ops: 7
 *   Rating: 1
 */
int bitXnor(int x, int y)
{
  // 我不知道为啥输入不了中文,所以中文都是我从Windows复制来的，除非我用英语说不清楚会用中文
  // ~(x^y) using truth table: res = ~x&~y | x&y = ~(x|y) | ~(~x|~y)
  return ~(x | y) | ~(~x | ~y);
}
/*
 * bitConditional - x ? y : z for each bit respectively
 *   Example: bitConditional(0b00110011, 0b01010101, 0b00001111) = 0b00011101
 *   Legal ops: & | ^ ~
 *   Max ops: 4
 *   Rating: 1
 */
int bitConditional(int x, int y, int z)
{
  // truth table:
  // x = 0 y = 0 z = 1 res = 1
  // x = 0 y = 1 z = 1 res = 1
  // x = 1 y = 1 z = 0 res = 1
  // x = 1 y = 1 z = 1 res = 1
  // otherwsie, res = 0
  // so, res = ~x&~y&z | ~x&y&z | x&y&~z | x&y&z = ~x&z | x&y
  return (~x & z) | (x & y);
}
/*
 * byteSwap - swaps the nth byte and the mth byte
 *  Examples: byteSwap(0x12345678, 1, 3) = 0x56341278
 *            byteSwap(0xDEADBEEF, 0, 2) = 0xDEEFBEAD
 *  You may assume that 0 <= n <= 3, 0 <= m <= 3
 *  Legal ops: ! ~ & ^ | + << >>
 *  Max ops: 16
 *  Rating: 2
 */
int byteSwap(int x, int n, int m)
{
  // first, get the bits of nth byte and mth byte
  int nth_bits = n << 3;
  int mth_bits = m << 3;
  // second, get the two bytes needed
  int nth_byte = x >> nth_bits;
  int mth_byte = x >> mth_bits;
  // third, swap == y = y^x, x = x^y, y = x^y
  int nmXor = (nth_byte ^ mth_byte) & 0xFF;
  return x ^ (nmXor << nth_bits) ^ (nmXor << mth_bits);
}
/*
 * logicalShift - shift x to the right by n, using a logical shift
 *   Can assume that 0 <= n <= 31
 *   Examples: logicalShift(0x87654321,4) = 0x08765432
 *   Legal ops: ! ~ & ^ | + << >>
 *   Max ops: 16
 *   Rating: 3
 */
int logicalShift(int x, int n)
{
  // if n >= 0, (x >> n) ^ 000000000000
  // if n < 0, (x >> n) ^ 111111000000 (logical shift, the top digit will also be zero!!!!!)
  // x >> 31 -> sign of x
  // sign << 31 the first bit is sign, others were zeros
  // sign << 31 >> n 31~31-n are sign
  // but x >> n, 31-n~0 will be x'
  // so sign should be 31~31-n+1, left shift << 1
  return (x >> n) ^ ((((x >> 31) << 31) >> n) << 1);
}
/*
 * cleanConsecutive1 - change any consecutive 1 to zeros in the binary form of x.
 *   Consecutive 1 means a set of 1 that contains more than one 1.
 *   Examples cleanConsecutive1(0x10) = 0x10
 *            cleanConsecutive1(0xF0) = 0x0
 *            cleanConsecutive1(0xFFFF0001) = 0x1
 *            cleanConsecutive1(0x4F4F4F4F) = 0x40404040
 *   Legal ops: ! ~ & ^ | + << >>
 *   Max ops: 16
 *   Rating: 4
 */
int cleanConsecutive1(int x)
{
  // 把x错位一下 1 bit
  // like 1111101
  //       1111101
  //       1111000
  //      1111000
  //      1111100
  // just kill the only one, left with consecutive ones (losing 1 bit)
  // so we need to add that bit
  // if sign is here
  // 1 111101
  // 1 111110
  // 1 111100
  // 1 111000
  // 1 111100
  // no problem! wrong here!
  // when 1000000
  // misplacement = 1000000
  // and the final if x ^ 10000000, res = 0000000
  // so we should left shift first
  int misplacement = x & (x << 1);
  return x ^ (misplacement | (misplacement >> 1));
}
/*
 * leftBitCount - returns count of number of consective 1's in
 *     left-hand (most significant) end of word.
 *   Examples: leftBitCount(-1) = 32, leftBitCount(0xFFF0F0F0) = 12
 *   Legal ops: ! ~ & ^ | + << >>
 *   Max ops: 50
 *   Rating: 4
 */
int leftBitCount(int x)
{
  // from web
  // x >> n == 0xFFFFFFFF, means n~31 are all 1, and the answer is 31-n = 31 + ~n + 1
  int flag = !(~x);
  int cnt = 0;
  // if x >> 16 == 0xFFFFFFFF, which means we can try >> 8, which means we should let cnt = 0, don't increment
  // when x >> 16 == 0xFFFFFFFF, cnt = 0, when x >> 16 != 0xFFFFFFFF, cnt = 16, we need to >> 16+more to get 0xFFFFFFFF
  cnt += (!!(~(x >> 16))) << 4;
  cnt += (!!(~(x >> (cnt + 8)))) << 3;
  cnt += (!!(~(x >> (cnt + 4)))) << 2;
  cnt += (!!(~(x >> (cnt + 2)))) << 1;
  cnt += (!!(~(x >> (cnt + 1))));
  // but when x = 0x80000000, cnt will be 30, because when cnt = 31, x >> cnt will be 0xFFFFFFFF
  // other situation is the same, when we get cnt, actually we need cnt + 1, that's why the answer is 32 - (cnt+1)
  // but problem arises when encountered with 0xFFFFFFFF and 0xFFFFFFFE, their cnts are both 0
  return 32 + ~cnt + flag;
  // 38 ops
}
/*
 * counter1To5 - return 1 + x if x < 5, return 1 otherwise, we ensure that 1<=x<=5
 *   Examples counter1To5(2) = 3,  counter1To5(5) = 1
 *   Legal ops: ~ & ! | + << >>
 *   Max ops: 15
 *   Rating: 2
 */
int counter1To5(int x)
{
  // first, judge wether x < 5, x < 5 , j = 0xFFFFFFFF, x == 5, j =0x0
  int xXor5 = (x & (~5)) | (~x & 5);
  int j = ((!!(xXor5)) << 31) >> 31;
  // when j == 0xFFFFFFFF, x & j = x, when j = 0x0, x & j = 0
  return (x & j) + 1;
  // 7 ops
}
/*
 * sameSign - return 1 if x and y have same sign, and 0 otherwise
 *   Examples sameSign(0x12345678, 0) = 1, sameSign(0xFFFFFFFF,0x1) = 0
 *   Legal ops: ! ~ & ! ^ | + << >>
 *   Max ops: 5
 *   Rating: 2
 */
int sameSign(int x, int y)
{
  int j = 1 << 31; // to judge whether the sign bit is zero
  return !((x ^ y) & j);
  // 4 ops
}
/*
 * satMul3 - multiplies by 3, saturating to Tmin or Tmax if overflow
 *  Examples: satMul3(0x10000000) = 0x30000000
 *            satMul3(0x30000000) = 0x7FFFFFFF (Saturate to TMax)
 *            satMul3(0x70000000) = 0x7FFFFFFF (Saturate to TMax)
 *            satMul3(0xD0000000) = 0x80000000 (Saturate to TMin)
 *            satMul3(0xA0000000) = 0x80000000 (Saturate to TMin)
 *  Legal ops: ! ~ & ^ | + << >>
 *  Max ops: 25
 *  Rating: 3
 */
int satMul3(int x)
{
  // if x + x saturate, the sigh will change, we can use x ^ (x + x) to get whether saturate, if do, sign will be 1, signMask will be 0xFFFFFFFF
  int add2x = x + x;
  int add3x = add2x + x;
  int signMask = ((x ^ add2x) | (x ^ add3x)) >> 31; // check if saturate
  // if x's sign is 1, overflow will be 0x80000000
  // if x's sign is 0, overflow will be 0x7FFFFFFF
  // if 3*x not overflow, signMask will be all 0, signMask | add3x = add3x, signMask & (...) = 0, add3x ^ 0 = add3x
  // if overflow, signMask will be all 1, signMask | add3x = all 1, x>>31 = all x's sign, if x >= 0, 11...1 & (00..00 ^ 1000..0) = 1..0
  // if x < 0 , 11...1 & (111..1 ^ 1..00) = 01111...11 = 0x7FFFFFFF
  // but after 1111..1 ^ , the answer will be reversed, 0x80000000 -> 0x7FFFFFFF, 0x7FFFFFFF -> 0x80000000
  return (signMask | add3x) ^ (signMask & ((x >> 31) ^ (1 << 31)));
  // 12 ops
}
/*
 * isGreater - if x > y  then return 1, else return 0
 *   Example: isGreater(4,5) = 0, isGreater(5,4) = 1
 *   Legal ops: ! ~ & ^ | + << >>
 *   Max ops: 24
 *   Rating: 3
 */
int isGreater(int x, int y)
{
  // check the sign bit of x - y = x + ~y + 1, in all, check x + ~y
  // if x > y, x + ~y = x - y - 1 >= 0, sign bit is 0
  // if x <= y, x + ~y = x - y - 1 < 0, sign bit is 1
  int xSign = x >> 31;
  int ySign = y >> 31;
  // if x&y 's sign are same, x - y - 1 won't be overflow, -1 - Tmin - 1 = 0xFFFFFFFF + 0x7FFFFFFF = 0x00000000
  // Tmax - 0 -1
  int cmpSameSign = (xSign ^ ySign) | ((x + (~y)) >> 31); // equal and 0 res = 0 = greater
  // if not same , x's sign = 0, y's sign = 1, res = 1,  x's sign = 1, y's sign = 0, res = 0
  // according to the truth table, res = !x & y
  int cmpDiffSign = (!xSign) & ySign; //  we use 1 = greater, 0 = otherwise
  // res = !cmpSameSign | cmpDiffSgih
  return (!cmpSameSign) | cmpDiffSign;
  // 11 ops
}
/*
 * subOK - Determine if can compute x-y without overflow
 *   Example: subOK(0x80000000,0x80000000) = 1,
 *            subOK(0x80000000,0x70000000) = 0,
 *   Legal ops: ! ~ & ^ | + << >>
 *   Max ops: 20
 *   Rating: 3
 */
int subOK(int x, int y)
{
  // when the signs are the same, OK
  int sameSign = x ^ y;
  // when the signs are different, x - y = x + ~y + 1, if overflow, x-y 's sign will be different from x
  int res = x + (~y + 1);
  int diffSign = res ^ x;
  return !((sameSign & diffSign) >> 31);
}
/*
 * trueFiveEighths - multiplies by 5/8 rounding toward 0,
 *  avoiding errors due to overflow
 *  Examples: trueFiveEighths(11) = 6
 *            trueFiveEighths(-9) = -5
 *            trueFiveEighths(0x30000000) = 0x1E000000 (no overflow)
 *  Legal ops: ! ~ & ^ | + << >>
 *  Max ops: 25
 *  Rating: 4
 */
int trueFiveEighths(int x)
{
  // to avoid overflow, we need to /8 first
  int quotient = x >> 3;
  int remainder = x & 7; // positive
  // x = 8 * quotient + remainder
  // x * 5/8 = 5 * quotien + remainder * 5 / 8
  int fiveQuotient = quotient + (quotient << 2);
  int fiveRemainder = remainder + (remainder << 2);
  // here, the fiveRemainder is positive, when x < 0, incorrect
  // if x < 0, the real answer = fiveQuotient + (fiveRemainder + 7 ) >> 3
  // if x >= 0, the real answer = fiveQuotient + fiveRemainder >> 3;
  int xSignMask = (x >> 31) & 7;
  return fiveQuotient + ((fiveRemainder + xSignMask) >> 3);
  // 11 ops
}
/*
 * float_half - Return bit-level equivalent of expression 0.5*f for
 *   floating point argument f.
 *   Both the argument and result are passed as unsigned int's, but
 *   they are to be interpreted as the bit-level representation of
 *   single-precision floating point values.
 *   When argument is NaN, return argument
 *   Legal ops: Any integer/unsigned operations incl. ||, &&. also if, while
 *   Max ops: 30
 *   Rating: 4
 */
unsigned float_half(unsigned uf)
{
  // s(31) exp(30-23) frac(22-0)
  int sMask = 0x80000000;
  int expMask = 0x7F800000;
  int fracMask = 0x007FFFFF;
  int s = uf & sMask;
  int exp = uf & expMask;
  int frac = uf & fracMask;
  // to get the last two bits of uf, if last two bits are 11, we need to let truncating = 1
  int truncating = !((uf & 3) ^ 3);
  int s_frac = uf & 0x807FFFFF;
  // Special NaN & Inf
  if (exp == 0x7F800000)
    return uf;
  // Denormalized exp = 0
  if (exp == 0x00000000)
  {
    // the exp won't change, s won't change
    // but consider the last two bits, if 11, /2 we get 1 1(should be truncated), we get 1 0 0(truncating)
    int modifiedFrac = frac >> 1;
    return s | (modifiedFrac + truncating);
  }
  // Normalized
  // but when exp = 1, the E = exp - 127 = -126, 1.frac / 2 =0.(1+frac/2), which means exp should be 0
  if (exp == 0x00800000)
  {
    int modifiedFrac = (frac >> 1) | 0x00400000;
    return s | (modifiedFrac + truncating);
  }
  // other normalized values, exp -- , frac and s won't change
  return (((exp >> 23) - 1) << 23) | s_frac;
}
/*
 * float_i2f - Return bit-level equivalent of expression (float) x
 *   Result is returned as unsigned int, but
 *   it is to be interpreted as the bit-level representation of a
 *   single-precision floating point values.
 *   Legal ops: Any integer/unsigned operations incl. ||, &&. also if, while
 *   Max ops: 30
 *   Rating: 4
 */
unsigned float_i2f(int x)
{
  // s(31) exp(30-23) frac(22-0)
  int exp = 0;
  int frac = 0;
  int fSign = (x >> 31) & 1;
  int bitIdx = 30;
  int leftShiftx = 0;
  int remainder = 0;
  int truncating = 0;
  if (x == 0)
    return x;
  
  if (x == 0x80000000)
  {
    // when x is TMin, we need 32 bits 100000000000 highest bit 31
    // exp = 31 + 127 = 158
    exp = 158;
  }
  else
  {
    if (fSign)
      x = -x;
    
    while (!(x >> bitIdx))
      bitIdx--;
    // bitIndex indicates the E of x
    exp = bitIdx + 127;
    // we need a 0-22 frac, so we need to transform 0-bitIdx x into 0-22 frac, right shift will cause the loss of info, so left shift
    // plus, abandon the first one
    leftShiftx = x << (31 - bitIdx);
    frac = (leftShiftx >> 8) & 0x007FFFFF;
    // then the truncating problem
    remainder = leftShiftx & 0xFF;                                      // the last 8 bits of x
    truncating = remainder > 0x80 || (remainder == 0x80 && (frac & 1)); // to the even
    frac += truncating;
    // but like 11111..1 1111 0000, we should add more exp
    if (frac >> 23) {
      frac = frac & 0x007FFFFF;
      exp++;
    }
  }
  return (fSign << 31) | (exp << 23) | frac;
}
/*
 * float64_f2i - Return bit-level equivalent of expression (int) f
 *   for 64 bit floating point argument f.
 *   Argument is passed as two unsigned int, but
 *   it is to be interpreted as the bit-level representation of a
 *   double-precision floating point value.
 *   Notice: uf1 contains the lower part of the f64 f
 *   Anything out of range (including NaN and infinity) should return
 *   0x80000000u.
 *   Legal ops: Any integer/unsigned operations incl. ||, &&. also if, while
 *   Max ops: 20
 *   Rating: 4
 */
int float64_f2i(unsigned uf1, unsigned uf2)
{
  // TMax = 2^31-1, TMin = -2^31
  // uf2 s(64) exp(62-52) frac(51-32) uf1(31-0)
  unsigned int sign_mask = 0x80000000u;
  unsigned int exp_mask = 0x7FF00000u;
  unsigned int frac_mask = 0x000FFFFFu;
  unsigned int sign = uf2 & sign_mask;
  int exp = (uf2 & exp_mask) >> 20;
  unsigned int frac = uf2 & frac_mask;
  int biased_exp = 0;

  // 处理特殊情况
  if (exp == 0x7FF) {
    return 0x80000000u; // NaN 或 Infinity
  } else if (exp == 0) {
    return 0; // 非规格化数或零
  }

  // 将 frac 转换为整数
  frac = frac | 0x00100000u; // 添加隐藏的 1
  frac = (frac << 11) | (uf1 >> 21);
  biased_exp = exp - 1023;
  if (biased_exp >= 31) {
    // 超出整数范围
    return 0x80000000u;
  } else if (biased_exp < 0) {
    return 0;
  } else {
    frac = frac >> (31 - biased_exp);
  }

  // 改变符号位
  // neagative the 2's implement shoulbe be ~frac + 1
  if (sign) // negative
    return sign | (~frac + 1);
  return sign | frac;
}
/*
 * float_negpwr2 - Return bit-level equivalent of the expression 2.0^-x
 *   (2.0 raised to the power -x) for any 32-bit integer x.
 *
 *   The unsigned value that is returned should have the identical bit
 *   representation as the single-precision floating-point number 2.0^-x.
 *   If the result is too small to be represented as a denorm, return
 *   0. If too large, return +INF.
 *
 *   Legal ops: Any integer/unsigned operations incl. ||, &&. Also if, while
 *   Max ops: 20
 *   Rating: 4
 */
unsigned float_negpwr2(int x)
{
  // the min number is 2^(-149)
  if (x > 149)
    return 0;
  // the max number is (2 - e) * 2^127
  if (x < -127)
    return 0x7F800000;
  if (x <= 149 && x >= 127) {
    // these are denorms
    return 1 << (149 - x);
  }
  // theese are norms
  return (127 - x) << 23;
}
