/*****************************************************************************
 *                                                                           *
 *                         GNAT COMPILER COMPONENTS                          *
 *                                                                           *
 *                               S J L J . C                                 *
 *                                                                           *
 *                                 B o d y                                   *
 *                                                                           *
 *                                                                           *
 *                      Copyright (C) 2008, AdaCore                          *
 *                                                                           *
 * GNAT is free software;  you can  redistribute it  and/or modify it under  *
 * terms of the  GNU General Public License as published  by the Free Soft-  *
 * ware  Foundation;  either version 2,  or (at your option) any later ver-  *
 * sion.  GNAT is distributed in the hope that it will be useful, but WITH-  *
 * OUT ANY WARRANTY;  without even the  implied warranty of MERCHANTABILITY  *
 * or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU General Public License  *
 * for  more details.  You should have  received  a copy of the GNU General  *
 * Public License  distributed with GNAT;  see file COPYING.  If not, write  *
 * to  the  Free Software Foundation,  51  Franklin  Street,  Fifth  Floor,  *
 * Boston, MA 02110-1301, USA.                                               *
 *                                                                           *
 * GNAT is maintained by AdaCore (http://www.adacore.com)                    *
 *                                                                           *
 ****************************************************************************/

/* Provides a setjmp longjmp mechanism for use with runtimes where exception
   propagation is not available. Note that we use __builtin setjmp and longjmp
   versions, that are not available from Ada, thus this C version */

typedef int jmp_buf[5];

static jmp_buf JMPBUF[5];
static int insetjmp = 0;

int mysetjmp (void (*next ()))
{
  int ret;
  if (!__builtin_setjmp (JMPBUF[insetjmp])) {
    insetjmp++;
    next();
    insetjmp--;
    return 0;
  }
  return 1;
}

void mylongjmp (void)
{
  insetjmp--;
  if (insetjmp >= 0)
    __builtin_longjmp (JMPBUF[insetjmp], 1);
  //  else
  //    abort();
}
