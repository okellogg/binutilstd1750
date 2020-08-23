/* This file is tc-m1750.h

   Copyright (C) 1996
   Free Software Foundation, Inc.

   This file is part of GAS, the GNU Assembler.

   GAS is free software; you can redistribute it and/or modify
   it under the terms of the GNU General Public License as published by
   the Free Software Foundation; either version 2, or (at your option)
   any later version.

   GAS is distributed in the hope that it will be useful,
   but WITHOUT ANY WARRANTY; without even the implied warranty of
   MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
   GNU General Public License for more details.

   You should have received a copy of the GNU General Public License
   along with GAS; see the file COPYING.  If not, write to
   the Free Software Foundation, 59 Temple Place - Suite 330, Boston, MA 02111-1307, USA.  */

#define TC_M1750 1

#define TARGET_BYTES_BIG_ENDIAN 1

#define COFF_MAGIC M1750MAGIC
#define BFD_ARCH bfd_arch_m1750        /* for non-BFD_ASSEMBLER */
#define TARGET_ARCH bfd_arch_m1750     /* BFD_ASSEMBLER */
#define COFF_FLAGS F_AR32W
#define TC_COUNT_RELOC(x) ((x)->fx_addsy||(x)->fx_subsy)
#define TC_COFF_SET_MACHINE(HDRS)     tc_headers_hook (HDRS)

#define TC_COFF_FIX2RTYPE(fixP) tc_coff_fix2rtype(fixP)
#define TC_COFF_SIZEMACHDEP(frag) tc_coff_sizemachdep(frag)
extern int tc_coff_sizemachdep PARAMS ((struct frag *));

#define tc_comment_chars m1750_comment_chars
extern const char *m1750_comment_chars;

#define tc_crawl_symbol_chain(a)	{;}	/* not used */
#define tc_aout_pre_write_hook(x)	{;}	/* not used */

#define LISTING_WORD_SIZE 2	        /* A word is 2 bytes */
#define LISTING_LHS_WIDTH 2	        /* One word on the first line */
#define LISTING_LHS_WIDTH_SECOND 2	/* One word on the second line */
#define LISTING_LHS_CONT_LINES 64       /* And 64 lines max (needed for .data) */
#define LISTING_HEADER "MIL-STD-1750 GAS "

#if !defined (REGISTER_PREFIX_OPTIONAL)
#if defined (M1750COFF) 
#define LOCAL_LABEL(name) (name[0] == '.' \
			   && (name[1] == 'L' || name[1] == '.'))
#define FAKE_LABEL_NAME ".L0\001"
#define REGISTER_PREFIX_OPTIONAL 0
#else
#define REGISTER_PREFIX_OPTIONAL 1
#endif
#endif /* not def REGISTER_PREFIX and not def OPTIONAL_REGISTER_PREFIX */

#ifdef BFD_ASSEMBLER

#define tc_frob_symbol(sym,punt) \
    if (S_GET_SEGMENT (sym) == reg_section) punt = 1

#define NO_RELOC BFD_RELOC_NONE

#else
#define NO_RELOC 0
#endif

#define DIFF_EXPR_OK

extern void m1750_init_after_args PARAMS ((void));
#define tc_init_after_args m1750_init_after_args

extern int m1750_parse_long_option PARAMS ((char *));
#define md_parse_long_option m1750_parse_long_option

/* Don't warn on word overflow; it happens on %hi relocs.  */
#undef WARN_SIGNED_OVERFLOW_WORD
 
#define md_operand(x)

/*
  Defined as 32 for 1750, yes this is correct
*/
#define TARGET_WORD_SIZE 32
#define TARGET_ARCH bfd_arch_m1750

struct relax_type;

extern const struct relax_type md_relax_table[];
#define TC_GENERIC_RELAX_TABLE md_relax_table

#define NEED_FX_R_TYPE 1

/* end of tc-m1750.h */
