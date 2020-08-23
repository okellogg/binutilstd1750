/* BFD library support routines for architectures.
   Copyright (C) 1990, 91, 92, 93, 94 Free Software Foundation, Inc.
   Hacked by Steve Chamberlain of Cygnus Support.

This file is part of BFD, the Binary File Descriptor library.

This program is free software; you can redistribute it and/or modify
it under the terms of the GNU General Public License as published by
the Free Software Foundation; either version 2 of the License, or
(at your option) any later version.

This program is distributed in the hope that it will be useful,
but WITHOUT ANY WARRANTY; without even the implied warranty of
MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
GNU General Public License for more details.

You should have received a copy of the GNU General Public License
along with this program; if not, write to the Free Software
Foundation, Inc., 59 Temple Place - Suite 330, Boston, MA 02111-1307, USA.  */

#include "bfd.h"
#include "sysdep.h"
#include "libbfd.h"

int bfd_default_scan_num_mach();


#define N(name, print,d,next)  \
{  16, 16, 16, bfd_arch_m1750, name, "m1750",print,2,d,bfd_default_compatible,bfd_default_scan, next, }

/* 
 * Names of supported architecture and variants.
 */

static const bfd_arch_info_type arch_info_struct[] =
{ 
  N(0x0000,"m1750a",true, &arch_info_struct[1]),
  N(0x8000,"m1750am",false, &arch_info_struct[2]),
  N(0x1000,"m1750b1",false, &arch_info_struct[3]),
  N(0x2000,"m1750b2",false, &arch_info_struct[4]),
  N(0x3000,"m1750b3",false, &arch_info_struct[5]),
  N(0x4000,"m1750b4",false, &arch_info_struct[6]),
  N(0x5000,"m1750b5",false, &arch_info_struct[7]),
  N(0x6000,"m1750b6",false, &arch_info_struct[8]),
  N(0x7000,"m1750b7",false, &arch_info_struct[9]),
  N(0x9000,"m1750b9",false, &arch_info_struct[10]),
  N(0xa000,"m1750ba",false, &arch_info_struct[11]),
  N(0xb000,"m1750bb",false, &arch_info_struct[12]),
  N(0xc000,"m1750bc",false, &arch_info_struct[13]),
  N(0xd000,"m1750bd",false, &arch_info_struct[14]),
  N(0xe000,"m1750be",false, &arch_info_struct[15]),
  N(0xf000,"m1750bf",false, &arch_info_struct[16]),
  N(0x0000,"m1750b",false, 0),
};

const bfd_arch_info_type bfd_m1750_arch =
  N(0x0000,"m1750a",false, &arch_info_struct[0]);

