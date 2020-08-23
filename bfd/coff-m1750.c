/* BFD back-end for MIL-STD-1750 COFF binaries.
   Copyright 1990, 91, 92, 93, 94, 95, 1996 Free Software Foundation, Inc.
   Written by Chris Nettleton.
   Based on coff-m68k, written by Cygnus Support.

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
#include "obstack.h"
#include "coff/m1750.h"
#include "coff/internal.h"
#include "libcoff.h"

/* Clean up namespace.  */
#define m1750coff_howto_table	_bfd_m1750coff_howto_table
#define m1750_rtype2howto	_bfd_m1750coff_rtype2howto
#define m1750_howto2rtype	_bfd_m1750coff_howto2rtype
#define m1750_reloc_type_lookup	_bfd_m1750coff_reloc_type_lookup

#define COFF_DEFAULT_SECTION_ALIGNMENT_POWER (2)


#ifdef ONLY_DECLARE_RELOCS
extern reloc_howto_type m1750coff_howto_table[];
#else
reloc_howto_type m1750coff_howto_table[] = 
{
  /*
   * 0: R_M1750_IMMW16
   * Absolute 16-bit word address
   */
  HOWTO (R_M1750_IMMW16,              /* type */             
         1,                           /* rightshift */                           
         1,                           /* size (0 = byte, 1 = short, 2 = long) */ 
         16,                          /* bitsize */                   
         false,                       /* pc_relative */                          
         0,                           /* bitpos */                               
         complain_overflow_dont,      /* complain_on_overflow */
         0,                           /* special_function */                     
         "WORD16",                    /* name */
         true,                        /* partial_inplace */                      
         0xffff,                      /* src_mask */                             
         0xffff,                      /* dst_mask */                             
         false),                      /* pcrel_offset */
  
  /*
   * 1: R_M1750_PCREL8
   * 8 bit PC word relative offset, relative to start of instruction
   * and always in the second byte
   */
  HOWTO (R_M1750_PCREL8,              /* type */
         0,                           /* rightshift */
         1,                           /* size (0 = byte, 1 = short, 2 = long) */
         8,                           /* bitsize */
         true,                        /* pc_relative */
         0,                           /* bitpos */
         complain_overflow_signed,    /* complain_on_overflow */
         0,                           /* special_function */
         "ICR8",                      /* name */
         true,                        /* partial_inplace */
         0xff,                        /* src_mask */
         0xff,                        /* dst_mask */
         false),                      /* pcrel_offset */

  /*
   * 2: R_M1750_HIGH16
   * Absolute 16-bit word address
   */
  HOWTO (R_M1750_HIGH16,              /* type */             
         17,                          /* rightshift */                           
         1,                           /* size (0 = byte, 1 = short, 2 = long) */ 
         16,                          /* bitsize */                   
         false,                       /* pc_relative */                          
         0,                           /* bitpos */                               
         complain_overflow_dont,      /* complain_on_overflow */
         0,                           /* special_function */                     
         "HIGH16",                    /* name */
         true,                        /* partial_inplace */                      
         0xffff,                      /* src_mask */                             
         0xffff,                      /* dst_mask */                             
         false),                      /* pcrel_offset */

  /*
   * 3: R_M1750_LOW16
   * Absolute 16-bit word address
   */
   HOWTO (R_M1750_LOW16,              /* type */             
         1,                           /* rightshift */                           
         1,                           /* size (0 = byte, 1 = short, 2 = long) */ 
         16,                          /* bitsize */                   
         false,                       /* pc_relative */                          
         0,                           /* bitpos */                               
         complain_overflow_dont,      /* complain_on_overflow */
         0,                           /* special_function */                     
         "LOW16",                     /* name */
         true,                        /* partial_inplace */                      
         0xffff,                      /* src_mask */                             
         0xffff,                      /* dst_mask */                             
         false),                      /* pcrel_offset */

  /*
   * 4: R_RELBYTE
   * 8-bit byte address
   */
  HOWTO (R_RELBYTE,                   /* type */
         0,                           /* rightshift */                           
         1,                           /* size (0 = byte, 1 = short, 2 = long) */ 
         8,                           /* bitsize */                   
         false,                       /* pc_relative */                          
         0,                           /* bitpos */                               
         complain_overflow_bitfield,  /* complain_on_overflow */
         0,                           /* special_function */                     
         "RELBYTE",                   /* name */
         true,                        /* partial_inplace */                      
         0xffffffff,                  /* src_mask */                             
         0xffffffff,                  /* dst_mask */                             
         false),                      /* pcrel_offset */

  /*
   * 5: R_RELWORD
   * 32-bit byte address
   */
  HOWTO (R_RELWORD,                   /* type */
         1,                           /* rightshift */                           
         1,                           /* size (0 = byte, 1 = short, 2 = long) */ 
         16,                          /* bitsize */                   
         false,                       /* pc_relative */                          
         0,                           /* bitpos */                               
         complain_overflow_bitfield,  /* complain_on_overflow */
         0,                           /* special_function */                     
         "RELWORD",                   /* name */
         true,                        /* partial_inplace */                      
         0xffffffff,                  /* src_mask */                             
         0xffffffff,                  /* dst_mask */                             
         false),                      /* pcrel_offset */

  /*
   * 6: R_RELLONG
   * 32-bit byte address
   */
  HOWTO (R_RELLONG,                   /* type */
         0,                           /* rightshift */                           
         2,                           /* size (0 = byte, 1 = short, 2 = long) */ 
         32,                          /* bitsize */                   
         false,                       /* pc_relative */                          
         0,                           /* bitpos */                               
         complain_overflow_bitfield,  /* complain_on_overflow */
         0,                           /* special_function */                     
         "RELLONG",                   /* name */
         true,                        /* partial_inplace */                      
         0xffffffff,                  /* src_mask */                             
         0xffffffff,                  /* dst_mask */                             
         false),                      /* pcrel_offset */

};
#endif /* not ONLY_DECLARE_RELOCS */

#ifndef BADMAG
#define BADMAG(x) M1750BADMAG(x)
#endif
#define M1750 1		/* Customize coffcode.h */

/* Turn a howto into a reloc number */

#ifdef ONLY_DECLARE_RELOCS
extern void m1750_rtype2howto PARAMS ((arelent *internal, int relocentry));
extern int m1750_howto2rtype PARAMS ((reloc_howto_type *));
extern reloc_howto_type *m1750_reloc_type_lookup
  PARAMS ((bfd *, bfd_reloc_code_real_type));
#else
void
m1750_rtype2howto(internal, relocentry)
     arelent *internal;
     int relocentry;
{
  switch (relocentry) 
    {
    case R_M1750_IMMW16:
      internal->howto = m1750coff_howto_table + 0; 
      break;
    case R_M1750_PCREL8:
      internal->howto = m1750coff_howto_table + 1; 
      break;
    case R_M1750_HIGH16:
      internal->howto = m1750coff_howto_table + 2; 
      break;
    case R_M1750_LOW16:
      internal->howto = m1750coff_howto_table + 3; 
      break;
   case R_RELBYTE:
      internal->howto = m1750coff_howto_table + 4; 
      break;
   case R_RELWORD:
      internal->howto = m1750coff_howto_table + 5; 
      break;
   case R_RELLONG:
   case R_M1750_IMM32:
      internal->howto = m1750coff_howto_table + 6; 
      break;
    default:
      internal->howto = m1750coff_howto_table + 0; 
    }
}

int 
m1750_howto2rtype (internal)
     reloc_howto_type *internal;
{
  return internal->type;

  if (internal->pc_relative) 
  {
    switch (internal->bitsize) 
    {
     case 32: return R_PCRLONG;
     case 16: return R_PCRWORD;
     case 8: return R_PCRBYTE;
    }
  }
  else 
  {
    switch (internal->bitsize) 
     {
      case 32: return R_RELLONG;
      case 16: return R_RELWORD;
      case 8: return R_RELBYTE;
     }
  }
  return R_RELLONG;    
}

reloc_howto_type *
m1750_reloc_type_lookup (abfd, code)
     bfd *abfd;
     bfd_reloc_code_real_type code;
{
  switch (code)
    {
    default:			return NULL;
    case BFD_RELOC_M1750_IMM16W:	return m1750coff_howto_table + 0;
    case BFD_RELOC_M1750_PCREL8W:	return m1750coff_howto_table + 1;
    case BFD_RELOC_HI16:		return m1750coff_howto_table + 2;
    case BFD_RELOC_LO16:		return m1750coff_howto_table + 3;
    case BFD_RELOC_32:			return m1750coff_howto_table + 4;
    case BFD_RELOC_8:			return m1750coff_howto_table + 5;
    case BFD_RELOC_16:			return m1750coff_howto_table + 6;
    }
  /*NOTREACHED*/
}

#endif /* not ONLY_DECLARE_RELOCS */

#define RTYPE2HOWTO(internal, relocentry) \
  m1750_rtype2howto(internal, (relocentry)->r_type)

#define SELECT_RELOC(external, internal) \
  external.r_type = m1750_howto2rtype(internal);

#define coff_bfd_reloc_type_lookup m1750_reloc_type_lookup

#define coff_relocate_section _bfd_coff_generic_relocate_section

#include "coffcode.h"

const bfd_target 
#ifdef TARGET_SYM
  TARGET_SYM =
#else
  m1750coff_vec =
#endif
{
#ifdef TARGET_NAME
  TARGET_NAME,
#else
  "coff-m1750",			/* name */
#endif
  bfd_target_coff_flavour,
  BFD_ENDIAN_BIG,		/* data byte order is big */
  BFD_ENDIAN_BIG,		/* header byte order is big */

  (HAS_RELOC | EXEC_P |		/* object flags */
   HAS_LINENO | HAS_DEBUG |
   HAS_SYMS | HAS_LOCALS | WP_TEXT | D_PAGED),

  (SEC_HAS_CONTENTS | SEC_ALLOC | SEC_LOAD | SEC_RELOC), /* section flags */
#ifdef NAMES_HAVE_UNDERSCORE
  '_',
#else
  0,				/* leading underscore */
#endif
  '/',				/* ar_pad_char */
  15,				/* ar_max_namelen */
  bfd_getb64, bfd_getb_signed_64, bfd_putb64,
     bfd_getb32, bfd_getb_signed_32, bfd_putb32,
     bfd_getb16, bfd_getb_signed_16, bfd_putb16, /* data */
  bfd_getb64, bfd_getb_signed_64, bfd_putb64,
     bfd_getb32, bfd_getb_signed_32, bfd_putb32,
     bfd_getb16, bfd_getb_signed_16, bfd_putb16, /* hdrs */

 {_bfd_dummy_target, coff_object_p, /* bfd_check_format */
   bfd_generic_archive_p, _bfd_dummy_target},
 {bfd_false, coff_mkobject, _bfd_generic_mkarchive, /* bfd_set_format */
   bfd_false},
 {bfd_false, coff_write_object_contents, /* bfd_write_contents */
   _bfd_write_archive_contents, bfd_false},

     BFD_JUMP_TABLE_GENERIC (coff),
     BFD_JUMP_TABLE_COPY (coff),
     BFD_JUMP_TABLE_CORE (_bfd_nocore),
     BFD_JUMP_TABLE_ARCHIVE (_bfd_archive_coff),
     BFD_JUMP_TABLE_SYMBOLS (coff),
     BFD_JUMP_TABLE_RELOCS (coff),
     BFD_JUMP_TABLE_WRITE (coff),
     BFD_JUMP_TABLE_LINK (coff),
     BFD_JUMP_TABLE_DYNAMIC (_bfd_nodynamic),

  COFF_SWAP_TABLE
 };


