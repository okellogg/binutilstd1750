/* Opcode table header for MIL-STD-1750A and MIL-STD-175B
   Copyright 1989, 1991, 1992, 1993, 1994, 1995 Free Software Foundation.

This file is part of GDB, GAS, and the GNU binutils.

GDB, GAS, and the GNU binutils are free software; you can redistribute
them and/or modify them under the terms of the GNU General Public
License as published by the Free Software Foundation; either version
1, or (at your option) any later version.

GDB, GAS, and the GNU binutils are distributed in the hope that they
will be useful, but WITHOUT ANY WARRANTY; without even the implied
warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See
the GNU General Public License for more details.

You should have received a copy of the GNU General Public License
along with this file; see the file COPYING.  If not, write to the Free
Software Foundation, 59 Temple Place - Suite 330, Boston, MA
02111-1307, USA.  */

/* These are used as bit flags for the arch field in the m1750_opcode
   structure.  */
#define	_m1750_undef  0
#define m1750a  0x000         /* 1750A with no options */
#define m1750b1 0x001         /* 1750B with option 1 */
#define m1750b2 0x002         /* 1750B with option 2 */
#define m1750b3 0x004         /* 1750B with option 3 */
#define mmu     0x008         /* 1750A or B with expnded memory support */

#define ma31750 0x00f         /* the ma31750 has all the above + extr BIFs */

/* end of m1750-opcode.h */
