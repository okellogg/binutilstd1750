/* c-m1750.c -- Assembler for the m1750 family
   Copyright (C) 1987, 91, 92, 93, 94, 95, 1996 Free Software Foundation, Inc.

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
   along with GAS; see the file COPYING.  If not, write to the Free
   Software Foundation, 59 Temple Place - Suite 330, Boston, MA
   02111-1307, USA.  */

#include <ctype.h>
#include "as.h"
#include "obstack.h"
#include "subsegs.h"

#include "opcode/m1750.h"

/* 
 * Relocations and fixups

 There are two different cases where an instruction or data
 directive operand requires relocation, or fixup.

 1. Branch instructions, such as 'blt', take an 8-bit signed word
 offset. The formula for computing the offset is this:

 offset = (destination - pc) / 2

 Branch instructions never branch to a label not declared
 locally, so the actual offset can be computed by the assembler.

 2. Jump instructions, such as 'jc 15,addr', and memory reference 
 instructions such as 'l r1,addr', may refer to a label declared
 in another module, in which case a relocatable object code
 element is generated. If the label is declared locally then
 a fixup is made. In both cases, the value place in the instruction
 is a word address. i.e. the byte address dived by two.

 The j* instructions, jgt, jlt and so on, will be coded as the 
 equivalent brach instruction if the destination is within range.
 Otherwise they will be expanded to the equivalent conditional
 or unconditional jump instruction, which gets a segment-relative
 address that is relocated by the linker.

 */


/*
 * This string holds the chars that always start a comment. If the
 * pre-processor is disabled, these aren't very useful. The macro
 * tc_comment_chars points to this. We use this, rather than the usual
 * comment_chars, so that the --bitwise-or option will work.
 */
const char *m1750_comment_chars = "!";


/*
 * This array holds the chars that only start a comment at the beginning
 * of a line.  If the line seems to have the form '# 123 filename' .line
 * and .file directives will appear in the pre-processed output. Note that
 * input_file.c hand checks for '#' at the beginning of the first line of
 * the input file. This is because the compiler outputs #NO_APP at the
 * beginning of its output. Also note that comments like this one will
 * always work.
 */
const char line_comment_chars[] = "!#";
const char line_separator_chars[] = "";


/*
 * Chars that can be used to separate mant from exp in floating point
 * numbers. 
 */
CONST char EXP_CHARS[] = "eE";


/*
 * Chars that mean this number is a floating point constant, as in
 * "0f12.456" or "0d1.2345e12".
 *
 * ...Also be aware that MAXIMUM_NUMBER_OF_CHARS_FOR_FLOAT may have to be
 * changed in read.c. Ideally it shouldn't have to know about it at all,
 * but nothing is ideal around here.
 */
CONST char FLT_CHARS[] = "rRsSfFdDxXeE";

/*
 * Size of relocation record
 */
const int md_reloc_size = 8;


/*
 * We have the capability to make the register prefix optional. By default
 * the register prefix is unused since all high-level langauge defined
 * symbols begin with '_', and the reason for using the register prefix is
 * invalid. However if the macro REGISTER_PREFIX_OPTIONAL is defined when
 * compiling this file, and the user sets the assembly-time option
 * --register-prefix-optional then the prefix is optional, and will be
 * ignored if used.
 */
#ifdef REGISTER_PREFIX_OPTIONAL
int flag_reg_prefix_optional = REGISTER_PREFIX_OPTIONAL;
#else
int flag_reg_prefix_optional;
#endif


/* 
 * Whether --register-prefix-optional was used on the command line.
 */
static int reg_prefix_optional_seen;


/*
 * Stuff to do with defining which architectural options were are
 * assembling for. The 1750A is the default.
 */
static int current_architecture = 0;


extern int coff_flags;

void
tc_headers_hook (headers)
     object_headers *headers;
{
  coff_flags |= current_architecture;

  headers->filehdr.f_magic = M1750MAGIC;
  headers->aouthdr.magic = OMAGIC;
}




extern char *input_line_pointer;


static void s_bss PARAMS ((int));
static void s_even PARAMS ((int));
static void s_proc PARAMS ((int));
static void m1750_rdata PARAMS ((int));

/* These are the back-ends for the various machine dependent pseudo-ops.  */
void demand_empty_rest_of_line ();


static void
s_data2 (ignore)
     int ignore;
{
  subseg_set (data_section, 2);
  demand_empty_rest_of_line ();
}

static void
s_bss (ignore)
     int ignore;
{
  /* We don't support putting frags in the BSS segment, we fake it
     by marking in_bss, then looking at s_skip for clues.  */

  subseg_set (bss_section, 0);
  demand_empty_rest_of_line ();
}

static void
s_even (ignore)
     int ignore;
{
  register int temp;
  register long temp_fill;

  temp = 1;			/* JF should be 2? */
  temp_fill = get_absolute_expression ();
  if (!need_pass_2)		/* Never make frag if expect extra pass. */
    frag_align (temp, (int) temp_fill, 0);
  demand_empty_rest_of_line ();
}


/*
 * This table describes all the machine specific pseudo-ops the assembler
 * has to support. The fields are:
 * 
 * 1: pseudo-op name without dot
 * 2: function to call to execute this pseudo-op
 * 3: Integer arg to pass to the function
 */
const pseudo_typeS md_pseudo_table[] =
{
  {"bss", s_bss, 0},
  {"even", s_even, 0},
  {"skip", s_space, 0},
  {"align", s_align_bytes, 0},
  {"noopt", s_ignore, 0},
  {"optim", s_ignore, 0},
  {"rdata", m1750_rdata, 0},
  {"rodata", m1750_rdata, 0},
  {"sbam", float_cons, 'a'},	/* single binary angular measurement */
  {"dbam", float_cons, 'b'},	/* double binary angular measurement */
  {0, 0, 0}
};


void
md_show_usage (FILE * stream)
{
  fprintf (stream, "\
-Ab1                    allow 1750B option 1 instructions\n\
-Ab2                    allow 1750B option 1 instructions\n\
-Ab3                    allow 1750B option 1 instructions\n\
-Ammu                   support expanded memory\n\
-Ano-b1                 do not assemble option 1 instructions\n");
  fprintf (stream, "\
-Ano-b2                 do not assemble option 1 instructions\n\
-Ano-b3                 do not assemble option 1 instructions\n\
-Ano-mmu                no expanded memory\n\
-A1750a                 -Ano-b1 -Ano-b2 -Ano-b3 -Ano-mmu\n\
-A1750b                 -A-b1 -A-b2 -A-b3 -Ano-mmu\n\
-Ama31750               -Ano-b1 -A-b2 -A-b3 -Ano-mmu\n\
-Amas281                -Ano-b1 -Ano-b2 -Ano-b3 -Ano-mmu\n");
}

static void
m1750_rdata (xxx)
     int xxx;
{
  char *save_line = input_line_pointer;
  static char section[] = ".rodata\n";

  /* Just pretend this is .section .rodata */
  input_line_pointer = section;
  obj_coff_section (xxx);

  input_line_pointer = save_line;
}

/* 
 * Round up a section size to the appropriate boundary. (Always an even
 * number of bytes on the 1750.)
 */
valueT
md_section_align (segment, size)
     segT segment;
     valueT size;
{
  if (size & 0x1)
    return size + 1;
  else
    return size;
}

/* FIXME: find out what this does */
long
md_pcrel_from (fixP)
     fixS *fixP;
{
  int adjust;

  /* Because fx_pcrel_adjust is a char, and may be unsigned, we store
     -1 as 64.  */
  adjust = fixP->fx_pcrel_adjust;
  return fixP->fx_where + fixP->fx_frag->fr_address - adjust;
}

void
md_number_to_chars (buf, val, n)
     char *buf;
     valueT val;
     int n;
{
  number_to_chars_bigendian (buf, val, n);
}

/*
 * --------------------------------------------------------------------
 * --------------------------------------------------------------------
 */

/* 
 * This table desribes how you change sizes for the various types of variable
 *  size expressions.  This version only supports two kinds. */

/* Note that calls to frag_var need to specify the maximum expansion
   needed; this is currently 2 bytes */

#define ENCODE_RELAX_STATE(type,size) \
  ((relax_substateT)((type<<2) | (size)))

#define SIZE_FROM_RELAX_STATE(s) \
    ( (((s) & 0x3) == BYTE ? 1 : (((s) & 0x3) == WORD ? 2 : 4)) )

/* types */
#define COND_JUMP 1		/* conditional jump */
#define UNCOND_JUMP 2		/* unconditional jump */
/* sizes */
#define BYTE 0
#define WORD 1
#define DWORD 2
#define UNKNOWN_SIZE 3

const relax_typeS md_relax_table[] =
{
/* The fields are:
   1) most positive reach of this state,
   2) most negative reach of this state,
   3) how many bytes this mode will add to the size of the current frag
   4) which index into the table to try if we can't fit into this one.
 */

  /* this block is not used */
  {1, 1, 0, 0},
  {1, 1, 0, 0},
  {1, 1, 0, 0},
  {1, 1, 0, 0},

  /* FIXME: -255 should be -256, but this resuts in an error :
   * Error: Value of -257 too large for field of 1 bytes at 0x501 
   */

  /* this block is for conditional jumps */
  {253, -255, 0, ENCODE_RELAX_STATE (COND_JUMP, WORD)},
  {65534, -65536, 2, ENCODE_RELAX_STATE (COND_JUMP, DWORD)},
  {0, 0, 4, 0},
  {1, 1, 0, 0},

  /* this block is for unconditional jumps */
  {253, -255, 0, ENCODE_RELAX_STATE (UNCOND_JUMP, WORD)},
  {65534, -65536, 2, ENCODE_RELAX_STATE (UNCOND_JUMP, DWORD)},
  {0, 0, 3, 0},
  {1, 1, 0, 0},

};


symbolS *
md_undefined_symbol (char *name)
{
  return 0;
}


/*
 * md_parse_option
 * 
 * Invocation line includes a switch not recognized by the base assembler.
 * See if it's a processor-specific option.  These are:
 *     -Ab1
 *     -Ab2
 *     -Ab3
 *     -Ammu
 *     -Ano-b1
 *     -Ano-b2
 *     -Ano-b3
 *     -Ano-mmu
 * 
 *     -A1750a     means -Ano-b1 -Ano-b2 -Ano-b3 -Ano-mmu
 *     -A1750b     means -A-b1 -A-b2 -A-b3 -Ano-mmu
 *     -Ama31750   means -Ano-b1 -A-b2 -A-b3 -Ano-mmu
 *     -Amas281    means -Ano-b1 -Ano-b2 -Ano-b3 -Ano-mmu
 */

CONST char *md_shortopts = "SA:k";

struct option md_longopts[] =
{
#define OPTION_REGISTER_PREFIX_OPTIONAL (OPTION_MD_BASE + 1)
  {"register-prefix-optional", no_argument, NULL,
   OPTION_REGISTER_PREFIX_OPTIONAL},
  {NULL, no_argument, NULL, 0}
};
size_t md_longopts_size = sizeof (md_longopts);

int
md_parse_option (c, arg)
     int c;
     char *arg;
{
  switch (c)
    {
    case 'A':
      {
	if (strcmp (arg, "b1") == 0)
	  {
	    current_architecture |= F_M1750B1;
	  }
	else if (strcmp (arg, "b2") == 0)
	  {
	    current_architecture |= F_M1750B2;
	  }
	else if (strcmp (arg, "b3") == 0)
	  {
	    current_architecture |= F_M1750B3;
	  }
	else if (strcmp (arg, "expanded") == 0)
	  {
	    current_architecture |= F_M1750MMU;
	  }
	else if (strcmp (arg, "mmu") == 0)
	  {
	    current_architecture |= F_M1750MMU;
	  }
	else if (strcmp (arg, "no-b1") == 0)
	  {
	    current_architecture &= ~F_M1750B1;
	  }
	else if (strcmp (arg, "no-b2") == 0)
	  {
	    current_architecture &= ~F_M1750B2;
	  }
	else if (strcmp (arg, "no-b3") == 0)
	  {
	    current_architecture &= ~F_M1750B3;
	  }
	else if (strcmp (arg, "no-mmu") == 0)
	  {
	    current_architecture &= ~F_M1750MMU;
	  }
	else if (strcmp (arg, "1750a") == 0)
	  {
	    current_architecture &= ~F_M1750B1;
	    current_architecture &= ~F_M1750B2;
	    current_architecture &= ~F_M1750B3;
	    current_architecture &= ~F_M1750MMU;
	  }
	else if (strcmp (arg, "1750b") == 0)
	  {
	    current_architecture |= F_M1750B1;
	    current_architecture |= F_M1750B2;
	    current_architecture |= F_M1750B3;
	    current_architecture &= ~F_M1750MMU;
	  }
	else if (strcmp (arg, "mas281") == 0)
	  {
	    current_architecture &= ~F_M1750B1;
	    current_architecture &= ~F_M1750B2;
	    current_architecture &= ~F_M1750B3;
	    current_architecture &= ~F_M1750MMU;
	  }
	else if (strcmp (arg, "ma31750") == 0)
	  {
	    current_architecture &= ~F_M1750B1;
	    current_architecture |= F_M1750B2;
	    current_architecture |= F_M1750B3;
	    current_architecture &= ~F_M1750MMU;
	  }
	else
	  {
	    as_bad ("bad architecture %s", arg);
	    return 0;
	  }
      }
      break;

    case OPTION_REGISTER_PREFIX_OPTIONAL:
      flag_reg_prefix_optional = 1;
      reg_prefix_optional_seen = 1;
      break;

    default:
      return 0;
    }

  return 1;
}


/*
 * --------------------------------------------------------------------
 * Stuff to do with fixups and relaxing
 * --------------------------------------------------------------------
 */

/*
 * -------------------------------------------------------------------------
 * md_convert_frag();
 *
 * Called after relax() is finished.
 * In:  Address of frag.
 *      fr_type == rs_machine_dependent.
 *      fr_subtype is what the address relaxed to.
 *
 * Out: Any fixSs and constants are set up.
 *      Caller will turn frag into a ".space 0".
 * -------------------------------------------------------------------------
 */

#ifndef BFD_ASSEMBLER

void
md_convert_frag (headers, sec, fragP)
     object_headers *headers;
     segT sec;
     fragS *fragP;
{
  md_convert_frag_1 (fragP);
}

#else

void
md_convert_frag (abfd, sec, fragP)
     bfd *abfd;
     segT sec;
     fragS *fragP;
{
  md_convert_frag_1 (fragP);
}
#endif




int
md_convert_frag_1 (fragP)
     fragS *fragP;
{
  register unsigned char *opcode;
  unsigned char *where_to_put_displacement = NULL;
  unsigned int target_address;
  unsigned int opcode_address;
  unsigned int extension = 0;
  int displacement_from_opcode_start;

  opcode = (unsigned char *) fragP->fr_opcode;

  /* Address we want to reach in file space. */
  target_address = S_GET_VALUE (fragP->fr_symbol) + fragP->fr_offset;
/*
   target_address += fragP->fr_symbol->sy_frag->fr_address;
 */

  /* Address opcode resides at in file space. */
  opcode_address = fragP->fr_address;

  /* Displacement from opcode start to fill into instruction. */
  displacement_from_opcode_start = target_address - opcode_address;

  switch (fragP->fr_subtype)
    {
    case ENCODE_RELAX_STATE (UNCOND_JUMP, BYTE):
    case ENCODE_RELAX_STATE (COND_JUMP, BYTE):
      /* don't have to change opcode */
      extension = 0;		/* 1 opcode + 1 displacement */
      where_to_put_displacement = &opcode[1];
      md_number_to_chars (
			   (char *) where_to_put_displacement,
			   (valueT) (displacement_from_opcode_start >> 1),
			   SIZE_FROM_RELAX_STATE (fragP->fr_subtype));
      break;

    case ENCODE_RELAX_STATE (UNCOND_JUMP, WORD):
    case ENCODE_RELAX_STATE (COND_JUMP, WORD):
      /* ls 8 bits of opcode are already set to condition code
         and RX = 0 */
      fragP->fr_opcode[0] = 0x70;	/* the JC instruction */

      fix_new (fragP,
	       fragP->fr_fix,
	       2,
	       fragP->fr_symbol,
	       fragP->fr_offset,
	       0,
	       R_M1750_IMMW16);
//               NO_RELOC);

      fragP->fr_fix += 2;

//      extension = 2;       /* 2 bytes absolute address */
      //      where_to_put_displacement = &opcode[2];
      //      md_number_to_chars (
      //        (char *) where_to_put_displacement,
      //        (valueT) (target_address >> 1),
      //        SIZE_FROM_RELAX_STATE (fragP->fr_subtype));

      break;

    default:
      BAD_CASE (fragP->fr_subtype);
      break;
    }
  fragP->fr_fix += extension;
}


/* Force truly undefined symbols to their maximum size, and generally set up
   the frag list to be relaxed
 */
int
md_estimate_size_before_relax (fragP, segment)
     register fragS *fragP;
     segT segment;
{
  return 0;
}

/*
 * Convert a fixup into a BFD relocation type. FIXME!!!
 */
short
tc_coff_fix2rtype (fixP)
     fixS *fixP;
{
  if (fixP->fx_r_type != 0)
    {
      return fixP->fx_r_type;
    }
  else
    {
      return (fixP->fx_pcrel ?
	      (fixP->fx_size == 1 ? R_PCRBYTE :
	       fixP->fx_size == 2 ? R_PCRWORD :
	       R_PCRLONG) :
	      (fixP->fx_size == 1 ? R_RELBYTE :
	       fixP->fx_size == 2 ? R_M1750_IMMW16 :
	       R_RELLONG));
    }


  switch (fixP->fx_r_type)
    {
    case R_M1750_IMMW16:
      return BFD_RELOC_M1750_IMM16W;

    case R_M1750_PCREL8:
      return BFD_RELOC_M1750_PCREL8W;

    case R_M1750_HIGH16:
      return BFD_RELOC_HI16;

    case R_M1750_LOW16:
      return BFD_RELOC_LO16;

    case R_M1750_IMM32:
      return BFD_RELOC_32;

    case R_RELBYTE:
      return BFD_RELOC_8;

    case R_RELWORD:
      return BFD_RELOC_16;

    case R_RELLONG:
      return BFD_RELOC_32;

    case NO_RELOC:
      return BFD_RELOC_M1750_PCREL8W;

    default:
      BAD_CASE (fixP->fx_r_type);
    }
}

static void
md_apply_fix_2 (fixP, val)
     fixS *fixP;
     offsetT val;
{

  /* This is unnecessary but it convinces the native rs6000 compiler
     to generate the code we want.  */
  char *buf = fixP->fx_frag->fr_literal;
  buf += fixP->fx_where;
  /* end ibm compiler workaround */


  switch (fixP->fx_r_type)
    {
    case R_M1750_IMMW16:
      if (val < -0x1ffffe || val > 0x0001fffe)
	as_bad_where (fixP->fx_file, fixP->fx_line, "value out of range");

      *buf++ = (val >> 9);
      *buf++ = (val >> 1);
      break;

    case R_M1750_PCREL8:
      if (val < -256 || val > 255)
	as_bad_where (fixP->fx_file, fixP->fx_line, "value out of range");

      val += 1;
      *buf++ = (val >> 1);
      break;

    case R_M1750_HIGH16:
      *buf++ = (val >> 25);
      *buf++ = (val >> 17);
      break;

    case R_M1750_LOW16:
      *buf++ = (val >> 9);
      *buf++ = (val >> 1);
      break;

    case R_M1750_IMM32:
      *buf++ = (val >> 25);
      *buf++ = (val >> 17);
      *buf++ = (val >> 9);
      *buf++ = (val >> 1);
      break;

    case R_RELBYTE:
      *buf++ = val;
      break;

    case R_RELWORD:
      *buf++ = (val >> 8);
      *buf++ = val;
      break;

    case R_RELLONG:
      *buf++ = (val >> 24);
      *buf++ = (val >> 16);
      *buf++ = (val >> 8);
      *buf++ = val;
      break;

//    case NO_RELOC:
      //      break;

    case 0:
      switch (fixP->fx_size)
	{
	case 1:
	  *buf++ = val;
	  break;
	case 2:
	  *buf++ = (val >> 9);
	  *buf++ = (val >> 1);
	  break;
	case 4:
	  *buf++ = (val >> 24);
	  *buf++ = (val >> 16);
	  *buf++ = (val >> 8);
	  *buf++ = val;
	  break;
	default:
	  BAD_CASE (fixP->fx_size);
	}
      break;

    default:
      BAD_CASE (fixP->fx_r_type);
    }
}


#ifdef BFD_ASSEMBLER
int
md_apply_fix (fixP, valp)
     fixS *fixP;
     valueT *valp;
{
  md_apply_fix_2 (fixP, (addressT) * valp);
  return 1;
}
#else
void 
md_apply_fix (fixP, val)
     fixS *fixP;
     long val;
{
  md_apply_fix_2 (fixP, (addressT) val);
}
#endif


/*
 * --------------------------------------------------------------------
 * --------------------------------------------------------------------
 */

void
m1750_init_after_args ()
{
}

char *
parse_exp (char *s, expressionS * op)
{
  char *save = input_line_pointer;
  char *new;

  if (!s)
    {
      return s;
    }

  input_line_pointer = s;
  expression (op);
  new = input_line_pointer;
  input_line_pointer = save;
  return new;
}

/*
 * 1750A and 1750B addressing modes
 */
enum addressing_mode
{
  mode_r,			/* register * register */
  mode_rs,			/* register * immediate shift count */
  mode_rb,			/* register * immediate bit number */
  mode_d,			/* register * 16-bit direct address */
  mode_dl,			/* register * 32-bit direct address */
  mode_d0,			/* implicit register * 16-bit direct address */
  mode_dx,			/* register * i16-bit direct address + index */
  mode_i,			/* register * indirect */
  mode_i0,			/* implicit regisre * indirect */
  mode_ix,			/* register * indirect indexed */
  mode_im_imx,			/* register * 16-bit immediate plus optional index (IM,IMX) */
  mode_im,			/* register * 16-bit immediate */
  mode_isp,			/* register * immediate short positive, range 1 .. 16 */
  mode_isn,			/* register * immediate short negative, range -1 .. -16 */
  mode_icr,			/* instructions counter relative (branch instructions only) */
  mode_b,			/* based, i.e. default register then base reg and offset */
  mode_bx,			/* based, i.e. default register then base reg + index reg */
  mode_s,			/* special */
  mode_dc,			/* small constant * direct address */
  mode_dc1,			/* small constant * direct address, but encoded const - 1 */
  mode_ic,			/* small constant * indirect address */
  mode_jmp,			/* special mode for J* instructions, mode_icr or mode_d */
  mode_jc,			/* for conditional jump instructions */
  mode_xio,			/* special mode for XIO instructions */
  illegal
};

/*
 * 1750A, 1750B and MA31750 XIO mnemonics and codes
 * Model indicates as follows:
 * 'A' mandatory on 1750A and 1750B
 * 'a' optional on 1750A and 1750B
 * 'B' mandatory on 1750B
 * 'b' optional on 1750B
 * '3' user-defined for mA31750 in 1750A or 1750B mode
 * '4' user-defined for mA31750 in 1750B mode
 */
static const struct xio_entry
  {
    char *mnem;			/* lower case mnemonic */
    unsigned cmd;		/* 16-bit command */
    unsigned mask;		/* bit set where command bit is significant */
    char Model;			/* indicated which 1750 variant the cmd is supported by */
  }
xio_table[] =
{
  {
    "bit", 0x840d, 0xffff, '3'
  }
  ,				/* Run built in test */
  {
    "cc", 0x8402, 0xffff, '3'
  }
  ,				/* Console command */
  {
    "ci", 0xc000, 0xffff, 'a'
  }
  ,				/* Console data input */
  {
    "clc", 0x4001, 0xffff, 'a'
  }
  ,				/* Clear Console */
  {
    "clir", 0x2001, 0xffff, 'A'
  }
  ,				/* Clear interrupt request */
  {
    "co", 0x4000, 0xffff, 'a'
  }
  ,				/* Console data output */
  {
    "dmad", 0x4007, 0xffff, 'a'
  }
  ,				/* Direct memory access disable */
  {
    "dmae", 0x4006, 0xffff, 'a'
  }
  ,				/* Direct memory access enable */
  {
    "dsbl", 0x2003, 0xffff, 'A'
  }
  ,				/* Disable interrupts */
  {
    "dsur", 0x4005, 0xffff, 'a'
  }
  ,				/* Disable start-up ROM */
  {
    "enbl", 0x2002, 0xffff, 'A'
  }
  ,				/* Enable interrupts */
  {
    "esur", 0x4004, 0xffff, 'a'
  }
  ,				/* Enable start-up ROM */
  {
    "go", 0x400b, 0xffff, 'a'
  }
  ,				/* Reset trigger-go */
  {
    "icw", 0x840c, 0xffff, '3'
  }
  ,				/* Read internal config. word */
  {
    "ita", 0xc00a, 0xffff, 'a'
  }
  ,				/* Input timer A */
  {
    "itar", 0xc002, 0xffff, '4'
  }
  ,				/* Input timer A reset reg. */
  {
    "itb", 0xc00e, 0xffff, 'a'
  }
  ,				/* Input timer B */
  {
    "itbr", 0xc00f, 0xffff, '4'
  }
  ,				/* Input timer B reset reg. */
  {
    "itgi", 0x840e, 0xffff, '3'
  }
  ,				/* Input trigger-go reset reg. */
  {
    "lmp", 0x5000, 0xff00, 'a'
  }
  ,				/* Load memory protect RAM */
  {
    "los", 0x0406, 0xffff, '3'
  }
  ,				/* Load OAS reg. */
  {
    "lxmp", 0x4000, 0xf000, 'b'
  }
  ,				/* Load ext. mem. protect RAM */
  {
    "mpen", 0x4003, 0xffff, 'a'
  }
  ,				/* Memory protect enable */
  {
    "od", 0x2008, 0xffff, 'a'
  }
  ,				/* Write output discrete reg. */
  {
    "ota", 0x400a, 0xffff, 'a'
  }
  ,				/* Output timer A */
  {
    "otar", 0x4002, 0xffff, 'b'
  }
  ,				/* Output timer A reset reg. */
  {
    "otb", 0x400e, 0xffff, 'a'
  }
  ,				/* Output timer B */
  {
    "otbr", 0x400f, 0xffff, 'b'
  }
  ,				/* Output timer B reset reg. */
  {
    "otgr", 0x040e, 0xffff, '3'
  }
  ,				/* Output trigger-go reg. */
  {
    "pi", 0x8000, 0xf000, 'a'
  }
  ,				/* Programmed input */
  {
    "po", 0x0000, 0xf000, 'a'
  }
  ,				/* Programmed output */
  {
    "rcfr", 0xa00f, 0xffff, 'A'
  }
  ,				/* Read and clear fault reg. */
  {
    "rcs", 0xc001, 0xffff, 'a'
  }
  ,				/* Read console status */
  {
    "rcw", 0x8410, 0xffff, '3'
  }
  ,				/* Read external configuration */
  {
    "rdi", 0xa009, 0xffff, 'a'
  }
  ,				/* Read discrete input. */
  {
    "rdor", 0xa008, 0xffff, 'a'
  }
  ,				/* Read output discrete reg. */
  {
    "rdow", 0xa008, 0xffff, 'b'
  }
  ,				/* Read output discrete reg. */
  {
    "rfmk", 0xa006, 0xffff, 'B'
  }
  ,				/* Read fault mask */
  {
    "rfr", 0x8401, 0xffff, '3'
  }
  ,				/* Read fault reg. (no clear) */
  {
    "ric1", 0xa001, 0xffff, 'a'
  }
  ,				/* Read input/output interrupt code, level 1. */
  {
    "ric2", 0xa002, 0xffff, 'a'
  }
  ,				/* Read input/output interrupt code, level 2. */
  {
    "ripr", 0xd100, 0xff00, 'a'
  }
  ,				/* Read instruction page reg. */
  {
    "rlp", 0x8404, 0xffff, '3'
  }
  ,				/* Read linkage pointer */
  {
    "rmfa", 0x8407, 0xffff, '3'
  }
  ,				/* Read memory fail address */
  {
    "rmfp", 0x8408, 0xffff, '3'
  }
  ,				/* Read memory fail page */
  {
    "rmfs", 0xa00d, 0xffff, 'a'
  }
  ,				/* Read memory fault state */
  {
    "rmk", 0xa000, 0xffff, 'A'
  }
  ,				/* Read interrupt mask */
  {
    "rmp", 0xd000, 0xff00, 'a'
  }
  ,				/* Read memory protect RAM */
  {
    "rns", 0x200a, 0xffff, 'a'
  }
  ,				/* Reset normal power up line */
  {
    "ropr", 0xd200, 0xff00, 'a'
  }
  ,				/* Read operand page reg. */
  {
    "ros", 0x8406, 0xffff, '3'
  }
  ,				/* Read OAS reg. */
  {
    "rpbs", 0xa00c, 0xffff, 'b'
  }
  ,				/* Read page bank select */
  {
    "rpi", 0x2004, 0xffff, 'A'
  }
  ,				/* Reset pending interrupt */
  {
    "rpir", 0xa004, 0xffff, 'A'
  }
  ,				/* Read pending interrupt reg. */
  {
    "rps", 0x8405, 0xffff, '3'
  }
  ,				/* Read processor status */
  {
    "rsw", 0xa00e, 0xffff, 'A'
  }
  ,				/* Read status word */
  {
    "rxmp", 0xc000, 0xf000, 'b'
  }
  ,				/* Read ext. mem. protect RAM */
  {
    "sfmk", 0x2006, 0xffff, 'B'
  }
  ,				/* Set fault mask */
  {
    "sfr", 0x0401, 0xffff, '3'
  }
  ,				/* Set fault reg. */
  {
    "smk", 0x2000, 0xffff, 'A'
  }
  ,				/* Set interrupt mask */
  {
    "spi", 0x2005, 0xffff, 'A'
  }
  ,				/* Set pending interrupt reg. */
  {
    "tah", 0x4009, 0xffff, 'a'
  }
  ,				/* Timer A halt */
  {
    "tas", 0x4008, 0xffff, 'a'
  }
  ,				/* Timer A start */
  {
    "tbh", 0x400d, 0xffff, 'a'
  }
  ,				/* Timer B halt */
  {
    "tbs", 0x400c, 0xffff, 'a'
  }
  ,				/* Timer B start */
  {
    "tpio", 0xa00b, 0xffff, 'a'
  }
  ,				/* Test Programmed Output */
  {
    "wipr", 0x5100, 0xff00, 'a'
  }
  ,				/* Write instruction page reg. */
  {
    "wopr", 0x5200, 0xff00, 'a'
  }
  ,				/* Write operand page reg. */
  {
    "wpbs", 0x200f, 0xffff, 'b'
  }
  ,				/* Write page bank select */
  {
    "wsw", 0x200e, 0xffff, 'A'
  }
  ,				/* Write status word */
};


void
insert_reg (char *regname, int regnum)
{
  /*
   * Insert the given register in the symbol table, both in upper case
   * and in lower case.
   */
  char buf[100];
  int i;

#ifdef REGISTER_PREFIX
  if (!flag_reg_prefix_optional)
    {
      buf[0] = REGISTER_PREFIX;
      strcpy (buf + 1, regname);
      regname = buf;
    }
#endif

  symbol_table_insert (symbol_new (regname, reg_section, regnum,
				   &zero_address_frag));

  for (i = 0; regname[i]; i++)
    buf[i] = islower (regname[i]) ? toupper (regname[i]) : regname[i];
  buf[i] = '\0';

  symbol_table_insert (symbol_new (buf, reg_section, regnum,
				   &zero_address_frag));
}




/*
 * 1750A and 1750B opcode mnemonics and codes. Note that
 * 'A' means 1750A; 'B' means 1750B. All the 1750A instructions
 * are valid on the 1750B. All the instructions marked 'B' are
 * illegal on the 1750A. The three 1750B options, option 1, option 2
 * and option 3, are marked '1', '2' and '3'. 'M' instructions require
 * the MMU.
 * Instructions marked 'X' are not supported at all, and must be
 * translated by the assembler into an appropriate sequence, possibly
 * depending on the 17050A/B option.
 */
static const struct opcode_entry
{
  char *mnem;
  enum addressing_mode mode;
  unsigned Code;
  char a_or_b;
}
opcode_table[] =
{
  {
    "A", mode_d, 0XA000, 'A'
  }
  ,
  {
    "AB", mode_b, 0X1000, 'A'
  }
  ,
  {
    "ABS", mode_r, 0XA400, 'A'
  }
  ,
  {
    "ABX", mode_bx, 0X4040, 'A'
  }
  ,
  {
    "AIM", mode_im, 0X4A01, 'A'
  }
  ,
  {
    "AISP", mode_isp, 0XA200, 'A'
  }
  ,
  {
    "AND", mode_d, 0XE200, 'A'
  }
  ,
  {
    "ANDB", mode_b, 0X3400, 'A'
  }
  ,
  {
    "ANDM", mode_im, 0X4A07, 'A'
  }
  ,
  {
    "ANDR", mode_r, 0XE300, 'A'
  }
  ,
  {
    "ANDX", mode_bx, 0X40E0, 'A'
  }
  ,
  {
    "AR", mode_r, 0XA100, 'A'
  }
  ,
  {
    "ATAN", mode_s, 0X4E60, '1'
  }
  ,
  {
    "BEX", mode_s, 0X7700, 'A'
  }
  ,
  {
    "BEZ", mode_icr, 0X7500, 'A'
  }
  ,
  {
    "BGE", mode_icr, 0X7B00, 'A'
  }
  ,
  {
    "BGT", mode_icr, 0X7900, 'A'
  }
  ,
  {
    "BIF", mode_s, 0X4F00, 'A'
  }
  ,
  {
    "BLE", mode_icr, 0X7800, 'A'
  }
  ,
  {
    "BLT", mode_icr, 0X7600, 'A'
  }
  ,
  {
    "BNZ", mode_icr, 0X7A00, 'A'
  }
  ,
  {
    "BPT", mode_s, 0XFFFF, 'A'
  }
  ,
  {
    "BR", mode_icr, 0X7400, 'A'
  }
  ,
  {
    "C", mode_d, 0XF000, 'A'
  }
  ,
  {
    "CB", mode_b, 0X3800, 'A'
  }
  ,
  {
    "CBL", mode_d, 0XF400, 'A'
  }
  ,
  {
    "CBX", mode_bx, 0X40C0, 'A'
  }
  ,
  {
    "CIM", mode_im, 0X4A0A, 'A'
  }
  ,
  {
    "CISN", mode_isn, 0XF300, 'A'
  }
  ,
  {
    "CISP", mode_isp, 0XF200, 'A'
  }
  ,
  {
    "COS", mode_s, 0X4E20, '1'
  }
  ,
  {
    "CR", mode_r, 0XF100, 'A'
  }
  ,
  {
    "D", mode_d, 0XD400, 'A'
  }
  ,
  {
    "DA", mode_d, 0XA600, 'A'
  }
  ,
  {
    "DABS", mode_r, 0XA500, 'A'
  }
  ,
  {
    "DAR", mode_r, 0XA700, 'A'
  }
  ,
  {
    "DB", mode_b, 0X1C00, 'A'
  }
  ,
  {
    "DBX", mode_bx, 0X4070, 'A'
  }
  ,
  {
    "DC", mode_d, 0XF600, 'A'
  }
  ,
  {
    "DCR", mode_r, 0XF700, 'A'
  }
  ,
  {
    "DD", mode_d, 0XD600, 'A'
  }
  ,
  {
    "DDR", mode_r, 0XD700, 'A'
  }
  ,
  {
    "DECM", mode_dc1, 0XB300, 'A'
  }
  ,
  {
    "DIM", mode_im, 0X4A05, 'A'
  }
  ,
  {
    "DISN", mode_isn, 0XD300, 'A'
  }
  ,
  {
    "DISP", mode_isp, 0XD200, 'A'
  }
  ,
  {
    "DL", mode_d, 0X8600, 'A'
  }
  ,
  {
    "DLB", mode_b, 0X0400, 'A'
  }
  ,
  {
    "DLBX", mode_bx, 0X4010, 'A'
  }
  ,
  {
    "DLI", mode_i, 0X8800, 'A'
  }
  ,
  {
    "DLR", mode_r, 0X8700, 'A'
  }
  ,
  {
    "DM", mode_d, 0XC600, 'A'
  }
  ,
  {
    "DMR", mode_r, 0XC700, 'A'
  }
  ,
  {
    "DNEG", mode_r, 0XB500, 'A'
  }
  ,
  {
    "DR", mode_r, 0XD500, 'A'
  }
  ,
  {
    "DS", mode_d, 0XB600, 'A'
  }
  ,
  {
    "DSAR", mode_r, 0X6E00, 'A'
  }
  ,
  {
    "DSCR", mode_r, 0X6F00, 'A'
  }
  ,
  {
    "DSLC", mode_rs, 0X6800, 'A'
  }
  ,
  {
    "DSLL", mode_rs, 0X6500, 'A'
  }
  ,
  {
    "DSLR", mode_r, 0X6D00, 'A'
  }
  ,
  {
    "DSR", mode_r, 0XB700, 'A'
  }
  ,
  {
    "DSRA", mode_rs, 0X6700, 'A'
  }
  ,
  {
    "DSRL", mode_rs, 0X6600, 'A'
  }
  ,
  {
    "DST", mode_d, 0X9600, 'A'
  }
  ,
  {
    "DSTB", mode_b, 0X0C00, 'A'
  }
  ,
  {
    "DSTI", mode_i, 0X9800, 'A'
  }
  ,
  {
    "DSTX", mode_bx, 0X4030, 'A'
  }
  ,
  {
    "DV", mode_d, 0XD000, 'A'
  }
  ,
  {
    "DVIM", mode_im, 0X4A06, 'A'
  }
  ,
  {
    "DVR", mode_r, 0XD100, 'A'
  }
  ,
  {
    "EATA", mode_s, 0X4E70, '1'
  }
  ,
  {
    "ECOS", mode_s, 0X4E30, '1'
  }
  ,
  {
    "EEXP", mode_s, 0X4EB0, '1'
  }
  ,
  {
    "EFA", mode_d, 0XAA00, 'A'
  }
  ,
  {
    "EFAR", mode_r, 0XAB00, 'A'
  }
  ,
  {
    "EFC", mode_d, 0XFA00, 'A'
  }
  ,
  {
    "EFCR", mode_r, 0XFB00, 'A'
  }
  ,
  {
    "EFD", mode_d, 0XDA00, 'A'
  }
  ,
  {
    "EFDR", mode_r, 0XDB00, 'A'
  }
  ,
  {
    "EFIX", mode_r, 0XEA00, 'A'
  }
  ,
  {
    "EFL", mode_d, 0X8A00, 'A'
  }
  ,
  {
    "EFLR", mode_r, 0XEE00, 'X'
  }
  ,
  {
    "EFLT", mode_r, 0XEB00, 'A'
  }
  ,
  {
    "EFM", mode_d, 0XCA00, 'A'
  }
  ,
  {
    "EFMR", mode_r, 0XCB00, 'A'
  }
  ,
  {
    "EFS", mode_d, 0XBA00, 'A'
  }
  ,
  {
    "EFSR", mode_r, 0XBB00, 'A'
  }
  ,
  {
    "EFST", mode_d, 0X9A00, 'A'
  }
  ,
  {
    "ELN", mode_s, 0X4ED0, '1'
  }
  ,
  {
    "ESCO", mode_s, 0X4E50, '1'
  }
  ,
  {
    "ESIN", mode_s, 0X4E10, '1'
  }
  ,
  {
    "ESQR", mode_s, 0X4E90, '1'
  }
  ,
  {
    "EXP", mode_s, 0X4EA0, '1'
  }
  ,
  {
    "FA", mode_d, 0XA800, 'A'
  }
  ,
  {
    "FAB", mode_b, 0X2000, 'A'
  }
  ,
  {
    "FABS", mode_r, 0XAC00, 'A'
  }
  ,
  {
    "FABX", mode_bx, 0X4080, 'A'
  }
  ,
  {
    "FAR", mode_r, 0XA900, 'A'
  }
  ,
  {
    "FC", mode_d, 0XF800, 'A'
  }
  ,
  {
    "FCB", mode_b, 0X3C00, 'A'
  }
  ,
  {
    "FCBX", mode_bx, 0X40D0, 'A'
  }
  ,
  {
    "FCR", mode_r, 0XF900, 'A'
  }
  ,
  {
    "FD", mode_d, 0XD800, 'A'
  }
  ,
  {
    "FDB", mode_b, 0X2C00, 'A'
  }
  ,
  {
    "FDBX", mode_bx, 0X40B0, 'A'
  }
  ,
  {
    "FDR", mode_r, 0XD900, 'A'
  }
  ,
  {
    "FIX", mode_r, 0XE800, 'A'
  }
  ,
  {
    "FLT", mode_r, 0XE900, 'A'
  }
  ,
  {
    "FM", mode_d, 0XC800, 'A'
  }
  ,
  {
    "FMB", mode_b, 0X2800, 'A'
  }
  ,
  {
    "FMBX", mode_bx, 0X40A0, 'A'
  }
  ,
  {
    "FMR", mode_r, 0XC900, 'A'
  }
  ,
  {
    "FNEG", mode_r, 0XBC00, 'A'
  }
  ,
  {
    "FS", mode_d, 0XB800, 'A'
  }
  ,
  {
    "FSB", mode_b, 0X2400, 'A'
  }
  ,
  {
    "FSBX", mode_bx, 0X4090, 'A'
  }
  ,
  {
    "FSR", mode_r, 0XB900, 'A'
  }
  ,
  {
    "INCM", mode_dc1, 0XA300, 'A'
  }
  ,
  {
    "J", mode_jmp, 0X74f0, 'A'
  }
  ,
  {
    "JC", mode_jc, 0X7000, 'A'
  }
  ,
  {
    "JCI", mode_jc, 0X7100, 'A'
  }
  ,
  {
    "JEZ", mode_jmp, 0X7520, 'A'
  }
  ,
  {
    "JGE", mode_jmp, 0X7B60, 'A'
  }
  ,
  {
    "JGT", mode_jmp, 0X7940, 'A'
  }
  ,
  {
    "JLE", mode_jmp, 0X7830, 'A'
  }
  ,
  {
    "JLT", mode_jmp, 0X7610, 'A'
  }
  ,
  {
    "JNZ", mode_jmp, 0X7A50, 'A'
  }
  ,
  {
    "JS", mode_d, 0X7200, 'A'
  }
  ,
  {
    "L", mode_d, 0X8000, 'A'
  }
  ,
  {
    "LB", mode_b, 0X0000, 'A'
  }
  ,
  {
    "LBX", mode_bx, 0X4000, 'A'
  }
  ,
  {
    "LBY", mode_r, 0XBF00, '3'
  }
  ,
  {
    "LBYI", mode_r, 0XAF00, '3'
  }
  ,
  {
    "LDL", mode_s, 0XCD00, '2'
  }
  ,
  {
    "LDS", mode_s, 0XDD00, '2'
  }
  ,
  {
    "LEFL", mode_s, 0XCE00, '2'
  }
  ,
  {
    "LEFS", mode_s, 0XDE00, '2'
  }
  ,
  {
    "LI", mode_i, 0X8400, 'A'
  }
  ,
  {
    "LIM", mode_im_imx, 0X8500, 'A'
  }
  ,
  {
    "LISN", mode_isn, 0X8300, 'A'
  }
  ,
  {
    "LISP", mode_isp, 0X8200, 'A'
  }
  ,
  {
    "LLB", mode_d, 0X8C00, 'A'
  }
  ,
  {
    "LLBI", mode_i, 0X8E00, 'A'
  }
  ,
  {
    "LLIM", mode_dl, 0X8500, 'A'
  }
  ,
  {
    "LM", mode_dc, 0X8900, 'A'
  }
  ,
  {
    "LN", mode_s, 0X4EC0, '1'
  }
  ,
  {
    "LR", mode_r, 0X8100, 'A'
  }
  ,
  {
    "LSJS", mode_dl, 0X7E00, 'M'
  }
  ,
  {
    "LSL", mode_s, 0XCC00, '2'
  }
  ,
  {
    "LSS", mode_s, 0XDC00, '2'
  }
  ,
  {
    "LST", mode_d0, 0X7D00, 'A'
  }
  ,
  {
    "LSTI", mode_i0, 0X7C00, 'A'
  }
  ,
  {
    "LUB", mode_d, 0X8B00, 'A'
  }
  ,
  {
    "LUBI", mode_i, 0X8D00, 'A'
  }
  ,
  {
    "LURS", mode_s, 0X7F00, 'M'
  }
  ,
  {
    "M", mode_d, 0XC400, 'A'
  }
  ,
  {
    "MB", mode_b, 0X1800, 'A'
  }
  ,
  {
    "MBX", mode_bx, 0X4060, 'A'
  }
  ,
  {
    "MIM", mode_im, 0X4A03, 'A'
  }
  ,
  {
    "MISN", mode_isn, 0XC300, 'A'
  }
  ,
  {
    "MISP", mode_isp, 0XC200, 'A'
  }
  ,
  {
    "MOV", mode_s, 0X9300, 'A'
  }
  ,
  {
    "MR", mode_r, 0XC500, 'A'
  }
  ,
  {
    "MS", mode_d, 0XC000, 'A'
  }
  ,
  {
    "MSIM", mode_im, 0X4A04, 'A'
  }
  ,
  {
    "MSR", mode_r, 0XC100, 'A'
  }
  ,
  {
    "N", mode_d, 0XE600, 'A'
  }
  ,
  {
    "NEG", mode_r, 0XB400, 'A'
  }
  ,
  {
    "NIM", mode_im, 0X4A0B, 'A'
  }
  ,
  {
    "NOP", mode_s, 0XFF00, 'A'
  }
  ,
  {
    "NR", mode_r, 0XE700, 'A'
  }
  ,
  {
    "OR", mode_d, 0XE000, 'A'
  }
  ,
  {
    "ORB", mode_b, 0X3000, 'A'
  }
  ,
  {
    "ORBX", mode_bx, 0X40F0, 'A'
  }
  ,
  {
    "ORIM", mode_im, 0X4A08, 'A'
  }
  ,
  {
    "ORR", mode_r, 0XE100, 'A'
  }
  ,
  {
    "POPM", mode_r, 0X8F00, 'A'
  }
  ,
  {
    "PSHM", mode_r, 0X9F00, 'A'
  }
  ,
  {
    "RB", mode_dc, 0X5300, 'A'
  }
  ,
  {
    "RBI", mode_ic, 0X5500, 'A'
  }
  ,
  {
    "RBR", mode_rb, 0X5400, 'A'
  }
  ,
  {
    "RVBR", mode_r, 0X5C00, 'A'
  }
  ,
  {
    "S", mode_d, 0XB000, 'A'
  }
  ,
  {
    "SAR", mode_r, 0X6B00, 'A'
  }
  ,
  {
    "SB", mode_dc, 0X5000, 'A'
  }
  ,
  {
    "SBB", mode_b, 0X1400, 'A'
  }
  ,
  {
    "SBBX", mode_bx, 0X4050, 'A'
  }
  ,
  {
    "SBI", mode_ic, 0X5200, 'A'
  }
  ,
  {
    "SBR", mode_rb, 0X5100, 'A'
  }
  ,
  {
    "SBY", mode_r, 0XDF00, '3'
  }
  ,
  {
    "SBYI", mode_r, 0XCF00, '3'
  }
  ,
  {
    "SCOS", mode_s, 0X4E40, '1'
  }
  ,
  {
    "SCR", mode_r, 0X6C00, 'A'
  }
  ,
  {
    "SFBS", mode_r, 0X9500, '3'
  }
  ,
  {
    "SIM", mode_im, 0X4A02, 'A'
  }
  ,
  {
    "SIN", mode_s, 0X4E00, '1'
  }
  ,
  {
    "SISP", mode_isp, 0XB200, 'A'
  }
  ,
  {
    "SJS", mode_d, 0X7E00, 'A'
  }
  ,
  {
    "SLBI", mode_i, 0X9E00, 'A'
  }
  ,
  {
    "SLC", mode_rs, 0X6300, 'A'
  }
  ,
  {
    "SLL", mode_rs, 0X6000, 'A'
  }
  ,
  {
    "SLR", mode_r, 0X6A00, 'A'
  }
  ,
  {
    "SOJ", mode_d, 0X7300, 'A'
  }
  ,
  {
    "SQRT", mode_s, 0X4E80, '1'
  }
  ,
  {
    "SR", mode_r, 0XB100, 'A'
  }
  ,
  {
    "SRA", mode_rs, 0X6200, 'A'
  }
  ,
  {
    "SRL", mode_rs, 0X6100, 'A'
  }
  ,
  {
    "SRM", mode_d, 0X9700, 'A'
  }
  ,
  {
    "ST", mode_d, 0X9000, 'A'
  }
  ,
  {
    "STB", mode_b, 0X0800, 'A'
  }
  ,
  {
    "STBX", mode_bx, 0X4020, 'A'
  }
  ,
  {
    "STC", mode_dc, 0X9100, 'A'
  }
  ,
  {
    "STCI", mode_ic, 0X9200, 'A'
  }
  ,
  {
    "STI", mode_i, 0X9400, 'A'
  }
  ,
  {
    "STLB", mode_d, 0X9C00, 'A'
  }
  ,
  {
    "STM", mode_dc, 0X9900, 'A'
  }
  ,
  {
    "STUB", mode_d, 0X9B00, 'A'
  }
  ,
  {
    "SUBI", mode_i, 0X9D00, 'A'
  }
  ,
  {
    "SVBR", mode_r, 0X5A00, 'A'
  }
  ,
  {
    "TB", mode_dc, 0X5600, 'A'
  }
  ,
  {
    "TBI", mode_ic, 0X5800, 'A'
  }
  ,
  {
    "TBR", mode_rb, 0X5700, 'A'
  }
  ,
  {
    "TSB", mode_d, 0X5900, 'A'
  }
  ,
  {
    "TVBR", mode_r, 0X5E00, 'A'
  }
  ,
  {
    "UA", mode_d, 0XAE00, '3'
  }
  ,
  {
    "UAR", mode_r, 0XAD00, '3'
  }
  ,
  {
    "UC", mode_d, 0XFD00, '3'
  }
  ,
  {
    "UCIM", mode_im, 0X4A00, '3'
  }
  ,
  {
    "UCR", mode_r, 0XFC00, '3'
  }
  ,
  {
    "URS", mode_s, 0X7F00, 'A'
  }
  ,
  {
    "US", mode_d, 0XBE00, '3'
  }
  ,
  {
    "USR", mode_r, 0XBD00, '3'
  }
  ,
  {
    "VIO", mode_d, 0X4900, 'A'
  }
  ,
  {
    "XBR", mode_s, 0XEC00, 'A'
  }
  ,
  {
    "XIO", mode_xio, 0X4800, 'A'
  }
  ,
  {
    "XOR", mode_d, 0XE400, 'A'
  }
  ,
  {
    "XORM", mode_im, 0X4A09, 'A'
  }
  ,
  {
    "XORR", mode_r, 0XE500, 'A'
  }
  ,
  {
    "XWR", mode_r, 0XED00, 'A'
  }
  ,
};

static int
get_opcode (int *code, enum addressing_mode *mode, int *a_or_b, char *mnem)
{
  /*
   * If the given string is a 1750 opcode mnemonic return the code 
   * otherwise return -1. Use binary chop to find matching entry.
   */

  int l = 0;
  int r = sizeof (opcode_table) / sizeof (struct opcode_entry) - 1;

  do
    {
      int mid = (l + r) / 2;
      int ans = strcmp (mnem, opcode_table[mid].mnem);

      if (ans < 0)
	r = mid - 1;
      else if (ans > 0)
	l = mid + 1;
      else
	{
	  *code = opcode_table[mid].Code;
	  *mode = opcode_table[mid].mode;
	  *a_or_b = opcode_table[mid].a_or_b;

	  return 0;
	}
    }
  while (l <= r);

  return -1;
}

void
md_begin ()
{
  /*
   * This function is called once at the begining of assembly, before any
   * other in this module. 
   */

  static const struct reg_name_struct
  {
    char *name;
    unsigned char number;
  }
  reg_table[] =
  {
    { "r0", 0x0 } ,
    { "r1", 0x1 } ,
    { "r2", 0x2 } ,
    { "r3", 0x3 } ,
    { "r4", 0x4 } ,
    { "r5", 0x5 } ,
    { "r6", 0x6 } ,
    { "r7", 0x7 } ,
    { "r8", 0x8 } ,
    { "r9", 0x9 } ,
    { "r10", 0xA } ,
    { "r11", 0xB } ,
    { "r12", 0xC } ,
    { "r13", 0xD } ,
    { "r14", 0xE } ,
    { "r15", 0xF } ,
  };

  static const struct cc_struct
    {
      char *mnem;
      unsigned char code;
    }
  cc_table[] =
  {
    { "all", 0x7 } ,
    { "ceq", 0xA } ,
    { "cez", 0xA } ,
    { "cge", 0xE } ,
    { "cgt", 0xC } ,
    { "cle", 0xB } ,
    { "clt", 0x9 } ,
    { "cnz", 0xD } ,
    { "cy", 0x8 } ,
    { "eq", 0x2 } ,
    { "ez", 0x2 } ,
    { "ge", 0x6 } ,
    { "gez", 0x6 } ,
    { "gt", 0x4 } ,
    { "gtz", 0x4 } ,
    { "le", 0x3 } ,
    { "lez", 0x3 } ,
    { "lz", 0x1 } ,
    { "ne", 0x5 } ,
    { "nz", 0x5 } ,
    { "uc", 0xF } ,
    { "lt", 0x1 } ,
    { "m", 0x1 } ,
    { "np", 0x3 } ,
    { "gz", 0x4 } ,
    { "p", 0x4 } ,
    { "nm", 0x6 }
  };

  /*
   * Copy the names of the registers into the assember's symbol table
   * where they may be found by the function 'expresion'.
   */
  int i;

  for (i = 0; i < sizeof (reg_table) / sizeof (struct reg_name_struct); i++)
    {
      insert_reg (reg_table[i].name, reg_table[i].number);
    }

  /*
   * Copy names fo condition codes in to the assembler's symbol table
   * where they may be found by the function 'expression'. Make the register
   * number distinctive.
   */
  for (i = 0; i < sizeof (cc_table) / sizeof (struct cc_struct); i++)
    {
      insert_reg (cc_table[i].mnem, cc_table[i].code + 32);
    }

  /*
   * Copy the xio names into the register table
   */
  for (i = 0; i < sizeof (xio_table) / sizeof (struct xio_entry); i++)
    {
      insert_reg (xio_table[i].mnem, xio_table[i].cmd);
    }
}


int
is_a_register (expressionS * e)
{
  /* Predicate for an expression that is a normal register */

  return (e->X_op == O_register) && (e->X_add_number >= 0) && (e->X_add_number <= 15);
}


int
is_a_cc (expressionS * e)
{
  /* Predicate for an expression that is a condition code.
     Note that condition codes are held as registers 32 .. 47 */

  return (e->X_op == O_register) && (e->X_add_number >= 32) && (e->X_add_number < 48);
}

int
is_a_xio_cmd (expressionS * e)
{
  /* Predicate for an expression that is an XIO command. 
     Note that commands are held as registers */

  return (e->X_op == O_register);
}


void
md_assemble (str)
     char *str;
{
  /*
   * This is the main function here. It takes a line of assembly language
   * source code and assembles it. Note, labels and pseudo ops have already
   * been removed, so too has leading white space.
   */

  char *er;
  int cnt;
  char mnem[10];
  int opcode;
  enum addressing_mode amode;
  int a_or_b;

  char *output;
  expressionS e1, e2, e3;

  /* Initialize the three operands */
  e1.X_op = O_absent;
  e2.X_op = O_absent;
  e3.X_op = O_absent;

  /* Drop leading whitespace (probably not required) tbd */
  while (*str == ' ')
    str++;

  /* get opcode mnemonic and make sure it's in upper case */
  cnt = 0;
  memset (mnem, '\0', 10);
  while (isalnum (*str) && cnt < 10)
    {
      if (islower (*str))
	mnem[cnt++] = toupper (*str++);
      else
	mnem[cnt++] = *str++;
    }

  if (get_opcode (&opcode, &amode, &a_or_b, mnem) < 0)
    {
      as_bad ("Unknown opcode `%s'", mnem);
      return;
    }

  /* Check the instruction is valid for the current architecture */
  switch (a_or_b)
    {
    case 'A':
      /* 1750A instructions are always valid */
      break;
    case 'B':
      /* we have no 1750B instructions that are not in options 1, 2 or 3 */
      break;
    case '1':
      if (!(current_architecture & F_M1750B1))
	{
	  as_bad ("1750B option 1 required");
	  return;
	}
      break;
    case '2':
      if (!(current_architecture & F_M1750B2))
	{
	  as_bad ("1750B option 2 required");
	  return;
	}
      break;
    case '3':
      if (!(current_architecture & F_M1750B3))
	{
	  as_bad ("1750B option 3 required");
	  return;
	}
      break;
    }

  /* skip white space */
  while (isspace (*str))
    str++;

  /* parse zero, one, two or three operands */
  str = parse_exp (str, &e1);
  while (isspace (*str))
    str++;
  if (e1.X_op != O_absent && *str == ',')
    {
      str++;
      while (isspace (*str))
	str++;
      str = parse_exp (str, &e2);
      while (isspace (*str))
	str++;
      if (e2.X_op != O_absent && *str == ',')
	{
	  str++;
	  while (isspace (*str))
	    str++;
	  str = parse_exp (str, &e3);
	}
    }

  if (a_or_b == 'X')
    {
      if (opcode == 0xEE00)
	{
	  /* extended float load register, becomes two instructions     
	     whose order dependes on the register numbers
	   */
	  if (is_a_register (&e1) && is_a_register (&e2) && e3.X_op == O_absent)
	    {
	      int r1 = e1.X_add_number;
	      int r2 = e2.X_add_number;

	      if (r1 > 13 || r2 > 13)
		{
		  as_bad ("register must be r0 .. r13");
		}
	      else if (r1 < r2)
		{
		  output = frag_more (4);

		  output[0] = 0x87;	/* DLR */
		  output[1] = (r1 << 4) + r2;
		  output[2] = 0x81;	/* LR */
		  output[3] = ((r1 + 2) << 4) + (r2 + 2);
		}
	      else
		{
		  output = frag_more (4);

		  output[0] = 0x81;	/* LR */
		  output[1] = ((r1 + 2) << 4) + (r2 + 2);
		  output[2] = 0x87;	/* DLR */
		  output[3] = (r1 << 4) + r2;
		}
	    }
	  else
	    as_bad ("operands wrong for mode R instruction");

	}

      return;
    }

  /* switch off long call if expanded memory not specified */
  if (opcode == 0X7E00 && (!(current_architecture & F_M1750MMU)) && amode == mode_dl)
    amode = mode_d;

  switch (amode)
    {
    case mode_r:
      /* Register mode,
         1st operand must be a register,
         2nd operand must be a register
       */
      if (is_a_register (&e1) && is_a_register (&e2) && e3.X_op == O_absent)
	{
	  output = frag_more (2);
	  output[0] = (opcode >> 8) & 0xff;
	  output[1] = (e1.X_add_number << 4) + e2.X_add_number;
	}
      else
	as_bad ("operands wrong for mode R instruction");
      break;

    case mode_b:
      /* Based mode
         1st operand must be a base register (i.e. r12, r13, r14, r15)
         2nd operand must be a consant in the range 0 .. 255
       */
      if (is_a_register (&e1) && e2.X_op == O_constant && e3.X_op == O_absent)
	{
	  output = frag_more (2);
	  output[0] = ((opcode >> 8) & 0xfc) + (e1.X_add_number - 12);
	  if (e2.X_add_number < 0 || e2.X_add_number >= 256)
	    as_bad ("constant must be in the range 0 .. 255");
	  output[1] = e2.X_add_number & 0xff;
	}
      else
	as_bad ("operands wrong for based mode instruction");
      break;

    case mode_bx:
      /* Based indexed mode
         1st operand must be a base register
         2nd operand must be an index register
       */
      if (is_a_register (&e1) && is_a_register (&e2) && e3.X_op == O_absent)
	{
	  output = frag_more (2);
	  output[0] = ((opcode >> 8) & 0xfc) + (e1.X_add_number - 12);
	  if (e2.X_add_number == 0)
	    as_bad ("register r0 cannot be used as an index register");
	  output[1] = (opcode & 0xf0) + (e2.X_add_number & 0xf);
	}
      else
	as_bad ("operands wrong for indexed based mode instruction");
      break;

    case mode_rs:
      /* R mode for shift instructions with immediate places,
         1st operand must be a register,
         2nd operand must be a constant
       */
      if (is_a_register (&e1) && e2.X_op == O_constant && e3.X_op == O_absent)
	{
	  output = frag_more (2);
	  output[0] = (opcode >> 8) & 0xff;
	  if (e2.X_add_number <= 0 || e2.X_add_number > 16)
	    {
	      as_bad ("permitted range of shift count is 1 .. 16");
	      output[1] = 0;
	    }
	  else
	    output[1] = ((e2.X_add_number - 1) << 4) + (e1.X_add_number);
	}
      else
	as_bad ("operands wrong for %s instruction", mnem);
      break;

    case mode_rb:
      /* R mode for bit set and test instructions with immediate bit number,
         1st operand must be a constant, range 0 .. 15
         2nd operand must be a register,
       */
      if (e1.X_op == O_constant && is_a_register (&e2) && e3.X_op == O_absent)
	{
	  output = frag_more (2);
	  output[0] = (opcode >> 8) & 0xff;
	  if (e1.X_add_number < 0 || e1.X_add_number >= 16)
	    {
	      as_bad ("bit number must be in the range 0 .. 15");
	      output[1] = 0;
	    }
	  else
	    output[1] = (e1.X_add_number << 4) + (e2.X_add_number);
	}
      else
	as_bad ("operands wrong for %s instruction", mnem);
      break;

    case mode_d0:
    case mode_i0:
      /* Direct and indirect modes, as use by load status instruction
         1st operand must be a 16-bit word address, possibly resolved at link time
         Optional 2nd operand must be an index register
       */
      if ((e1.X_op == O_constant || e1.X_op == O_symbol)
	  && (e2.X_op == O_absent || is_a_register (&e2)))
	{
	  output = frag_more (4);
	  output[0] = (opcode >> 8) & 0xff;

	  if (e2.X_op == O_absent)
	    output[1] = 0;	/* RX = 0 */
	  else if (is_a_register (&e2))
	    {
	      if (e2.X_add_number == 0)
		as_bad ("register r0 cannot be used as an index register");
	      output[1] = e2.X_add_number & 0xf;
	    }

	  if (e1.X_op == O_constant)
	    {
	      output[2] = (e1.X_add_number >> 8) & 0xff;
	      output[3] = (e1.X_add_number >> 0) & 0xff;
	    }
	  else
	    {
	      fix_new_exp (
			    frag_now,	/* which frag */
			    output - frag_now->fr_literal + 2,	/* where in that frag */
			    2,	/* size */
			    &e1,	/* address of expression */
			    0,	/* zero for not pc relative */
			    R_M1750_IMMW16);	/* relocation type */
	    }
	}
      else
	as_bad ("operands wrong for LST instruction");
      break;

    case mode_d:
    case mode_dx:
    case mode_i:
    case mode_ix:
    case mode_im_imx:
      /* Direct and indirect modes, 
         1st operand must be a register,
         2nd operand must be a 16-bit word address, possibly resolved at link time
         Optional 3rd operand must be an index register
       */
      if (is_a_register (&e1)
	  && (e2.X_op == O_constant || e2.X_op == O_symbol)
	  && (e3.X_op == O_absent || is_a_register (&e3)))
	{
	  output = frag_more (4);
	  output[0] = (opcode >> 8) & 0xff;

	  if (e3.X_op == O_absent)
	    output[1] = (e1.X_add_number & 0xf) << 4;	/* RX = 0 */
	  else if (e3.X_op == O_register)
	    {
	      if (e3.X_add_number == 0)
		as_bad ("register r0 cannot be used as an index register");
	      output[1] = ((e1.X_add_number & 0xf) << 4) + (e3.X_add_number & 0xf);
	    }

	  if (e2.X_op == O_constant)
	    {
	      output[2] = (e2.X_add_number >> 8) & 0xff;
	      output[3] = (e2.X_add_number >> 0) & 0xff;
	    }
	  else
	    {
	      fix_new_exp (
			    frag_now,	/* which frag */
			    output - frag_now->fr_literal + 2,	/* where in that frag */
			    2,	/* size */
			    &e2,	/* address of expression */
			    0,	/* zero for not pc relative */
			    R_M1750_IMMW16);	/* relocation type */
	    }
	}
      else
	as_bad ("operands wrong for mode D,DX,I,IX instruction");
      break;

    case mode_dl:
      /* Direct long mode, 
         1st operand must be a register,
         2nd operand must be a 32-bit word address, possibly resolved at link time
       */
      if (opcode == 0X7E00)
	{
	  if (is_a_register (&e1)
	      && (e2.X_op == O_constant || e2.X_op == O_symbol)
	      && (e3.X_op == O_absent || is_a_register (&e3)))
	    {
	      output = frag_more (10);
	      output[0] = 0x85;	/* LIM */
	      output[1] = 0xb0;	/* R11 */
	      output[2] = 0x00;	/* MS 8 bits of address (top 7 will be zero) */
	      output[3] = 0x00;	/* next 8 bits of address */
	      output[4] = 0x85;	/* LIM */
	      output[5] = 0xC0;	/* R12 */
	      output[6] = 0x00;	/* next 8 bits of address */
	      output[7] = 0x00;	/* LS 8 bits of address */
	      output[8] = 0x77;	/* BEX */
	      output[9] = 0x00;	/* N = 0 */

	      if (e2.X_op == O_constant)
		{
		  output[2] = (e2.X_add_number >> 24) & 0xff;
		  output[3] = (e2.X_add_number >> 16) & 0xff;
		  output[6] = (e2.X_add_number >> 8) & 0x0ff;
		  output[7] = (e2.X_add_number >> 0) & 0x0ff;
		}
	      else
		{
		  fix_new_exp (
				frag_now,	/* which frag */
				output - frag_now->fr_literal + 2,	/* where in that frag */
				2,	/* size */
				&e2,	/* address of expression */
				0,	/* zero for not pc relative */
				R_M1750_HIGH16);	/* relocation type */

		  fix_new_exp (
				frag_now,	/* which frag */
				output - frag_now->fr_literal + 6,	/* where in that frag */
				2,	/* size */
				&e2,	/* address of expression */
				0,	/* zero for not pc relative */
				R_M1750_LOW16);		/* relocation type */
		}
	    }
	  else
	    as_bad ("operands wrong for long SJS instruction");
	}
      else if (opcode == 0X8500)
        {
          /* long load literal */
          if (is_a_register (&e1)
              && (e2.X_op == O_constant || e2.X_op == O_symbol)
              && (e3.X_op == O_absent))
            {
              output = frag_more (8);
              output[0] = 0x85; /* LIM */
              output[1] = e1.X_add_number << 4; 
              output[2] = 0x00; /* MS 8 bits of address (top 7 will be zero) */
              output[3] = 0x00; /* next 8 bits of address */
              output[4] = 0x85; /* LIM */
              output[5] = (e1.X_add_number + 1) << 4;
              output[6] = 0x00; /* next 8 bits of address */
              output[7] = 0x00; /* LS 8 bits of address */

              if (e2.X_op == O_constant)
                {
                  output[2] = (e2.X_add_number >> 24) & 0xff;
                  output[3] = (e2.X_add_number >> 16) & 0xff;
                  output[6] = (e2.X_add_number >> 8) & 0x0ff;
                  output[7] = (e2.X_add_number >> 0) & 0x0ff;
                }
              else
                {
                  fix_new_exp (
                                frag_now,       /* which frag */
                                output - frag_now->fr_literal + 2,      /* where in that frag */
                                2,      /* size */
                                &e2,    /* address of expression */
                                0,      /* zero for not pc relative */
                                R_M1750_HIGH16);        /* relocation type */

                  fix_new_exp (
                                frag_now,       /* which frag */
                                output - frag_now->fr_literal + 6,      /* where in that frag */
                                2,      /* size */
                                &e2,    /* address of expression */
                                0,      /* zero for not pc relative */
                                R_M1750_LOW16);         /* relocation type */
                }
            }
          else
            as_bad ("operands wrong for long LIM instruction");
        }
      break;

    case mode_dc:
    case mode_ic:
    case mode_dc1:
      /* Direct mode, but RA is a small constant
         1st operand must be constant in the range 0 .. 15 (mode_dc)
         1st operand must be constant in the range 1 .. 16 (mode dc1)
         2nd operand must be a constant or a symbol
         Optional 3rd operand must be an index register
       */
      if (e1.X_op == O_constant
	  && (e2.X_op == O_constant || e2.X_op == O_symbol)
	  && (e3.X_op == O_absent || is_a_register (&e3)))
	{
	  output = frag_more (4);
	  output[0] = (opcode >> 8) & 0xff;

	  if (amode == mode_dc || amode == mode_ic)
	    {
	      if (e1.X_add_number < 0 || e1.X_add_number > 15)
		as_bad ("constant must be in range 0 .. 15");
	      output[1] = (e1.X_add_number & 0xf) << 4;		/* RA = constant, RX = 0 */
	    }
	  else
	    {
	      if (e1.X_add_number < 1 || e1.X_add_number > 16)
		as_bad ("constant must be in range 1 .. 16");
	      output[1] = ((e1.X_add_number - 1) & 0xf) << 4;	/* RA = constant, RX = 0 */
	    }

	  if (e3.X_op == O_register)
	    {
	      if (e3.X_add_number == 0)
		as_bad ("register r0 cannot be used as an index register");
	      output[1] += (e3.X_add_number & 0xf);
	    }

	  if (e2.X_op == O_constant)
	    {
	      output[2] = (e2.X_add_number >> 8) & 0xff;
	      output[3] = (e2.X_add_number >> 0) & 0xff;
	    }
	  else
	    {
	      fix_new_exp (
			    frag_now,	/* which frag */
			    output - frag_now->fr_literal + 2,	/* where in that frag */
			    2,	/* size */
			    &e2,	/* address of expression */
			    0,	/* zero for not pc relative */
			    R_M1750_IMMW16);	/* relocation type */
	    }
	}
      else
	as_bad ("operands wrong for mode D,DX,I,IX instruction");
      break;

    case mode_im:
      /* Immediate mode,
         1st operand must be a register
         2nd operand must be a 16-bit constant or link-time constant
       */
      if (is_a_register (&e1)
	  && (e2.X_op == O_constant || e2.X_op == O_symbol)
	  && e3.X_op == O_absent)
	{
	  output = frag_more (4);
	  output[0] = (opcode >> 8) & 0xff;
	  output[1] = ((e1.X_add_number & 0xf) << 4) + (opcode & 0xf);

	  if (e2.X_op == O_constant)
	    {
	      output[2] = (e2.X_add_number >> 8) & 0xff;
	      output[3] = (e2.X_add_number >> 0) & 0xff;
	    }
	  else
	    {
	      fix_new_exp (
			    frag_now,	/* which frag */
			    output - frag_now->fr_literal + 2,	/* where in that frag */
			    2,	/* size */
			    &e2,	/* address of expression */
			    0,	/* zero for not pc relative */
			    R_M1750_IMMW16);	/* relocation type */
	    }
	}
      else
	as_bad ("operands wrong for mode IM instruction");
      break;

    case mode_isp:
      /* Immediate short positive, 
         1st operand must be a register,
         2nd operand must be a literal in the range 1 .. 16
       */
      if (is_a_register (&e1) && e2.X_op == O_constant && e3.X_op == O_absent)
	{
	  output = frag_more (2);
	  output[0] = (opcode >> 8) & 0xff;

	  if (e2.X_add_number <= 0 || e2.X_add_number > 16)
	    {
	      as_bad ("constant out of range for mode ISP instruction");
	      output[1] = (e1.X_add_number & 0xf) << 4;
	    }
	  else
	    output[1] = ((e1.X_add_number & 0xf) << 4) + (e2.X_add_number - 1);
	}
      else
	as_bad ("operands wrong for mode ISP instruction");
      break;

    case mode_isn:
      /* Immediate short negative,
         1st operand must be a register,
         2nd operand must be a literal in the range 1 .. 16
       */
      if (is_a_register (&e1) && e2.X_op == O_constant && e3.X_op == O_absent)
	{
	  output = frag_more (2);
	  output[0] = (opcode >> 8) & 0xff;

	  if (e2.X_add_number < 0 || e2.X_add_number > 16)
	    {
	      as_bad ("constant out of range for mode ISN instruction");
	      output[1] = (e1.X_add_number & 0xf) << 4;
	    }
	  else
	    output[1] = ((e1.X_add_number & 0xf) << 4) + (e2.X_add_number - 1);
	}
      else
	as_bad ("operands wrong for %s instruction", mnem);
      break;

    case mode_icr:
      /* Instruction counter relative. Used by branch instructions.
         1st operand must be a label or a small constant
       */
      if ((e1.X_op == O_symbol || e1.X_op == O_constant)
	  && e2.X_op == O_absent && e3.X_op == O_absent)
	{
	  output = frag_more (2);
	  output[0] = (opcode >> 8);
	  output[1] = (opcode & 0xff);

	  if (e1.X_op == O_symbol)
	    {
	      fix_new_exp (
			    frag_now,	/* which frag */
			    output - frag_now->fr_literal + 1,	/* where in that frag */
			    1,	/* size */
			    &e1,	/* the expression */
			    1,	/* pc relative = true */
			    R_M1750_PCREL8);	/* relocation */
	    }
	  else
	    {
	      if (e1.X_add_number < -128 || e1.X_add_number > 127)
		as_bad ("branch operand out of range");
	      output[1] = e1.X_add_number & 0xff;
	    }
	}
      else if (e1.X_op == O_constant && e2.X_op == O_absent && e3.X_op == O_absent)
	{
	  output = frag_more (2);
	  if (e2.X_add_number >= -128 || e2.X_add_number < 127)
	    {
	      output = frag_more (2);
	      output[0] = (opcode >> 8) & 0xff;
	      output[1] = (e2.X_add_number & 0x1ff) / 2;
	    }
	  else
	    as_bad ("range of %s instruction is -128 to +127 words", mnem);
	}
      else
	as_bad ("operands wrong for %s instruction", mnem);
      break;

    case mode_jmp:
      /* Instruction counter relative
         1st operand must be a label
       */
      if (e1.X_op == O_symbol && e2.X_op == O_absent && e3.X_op == O_absent)
	{
	  frag_wane (frag_now);
	  frag_new (0);
	  output = frag_more (2);
	  output[0] = (opcode >> 8) & 0xff;	/* the ble etc code */
	  output[1] = (opcode & 0xff);	/* the jc cc */

	  /* This is a relaxable insn, so we do some special handling */

	  output = frag_var (
			      rs_machine_dependent,	/* relax_stateT type */
			      2,	/* grow by max_chars */
			      2,	/* var */
			      ENCODE_RELAX_STATE (COND_JUMP, BYTE),	/* relax_substateT subtype */
			      e1.X_add_symbol,	/* symbolS *symbol */
			      e1.X_add_number,	/* offset */
			      output);	/* char *opcode */
	}
      else
	as_bad ("operands wrong for jump instruction");
      break;

    case mode_jc:
      /* mode for conditional jump instructions
         jc number,symbol[,index-register]
         jc condition_code,symbol[,index-register]
       */
      if ((e1.X_op == O_constant || is_a_cc (&e1))
	  && (e2.X_op == O_symbol || e2.X_op == O_constant)
	  && (e3.X_op == O_absent || is_a_register (&e3)))
	{
	  int code;

	  code = e1.X_add_number & 0xf;

	  output = frag_more (4);
	  output[0] = (opcode >> 8) & 0xff;	/* the jc or jci */

	  if (e3.X_op == O_absent)
	    output[1] = code << 4;	/* RX = 0 */
	  else if (e3.X_op == O_register)
	    {
	      if (e3.X_add_number == 0)
		as_bad ("register r0 cannot be used as an index register");
	      output[1] = (code << 4) + (e3.X_add_number & 0xf);
	    }

	  if (e2.X_op == O_constant)
	    {
	      output[2] = (e2.X_add_number >> 8) & 0xff;
	      output[3] = (e2.X_add_number >> 0) & 0xff;
	    }
	  else
	    {
	      fix_new_exp (
			    frag_now,	/* which frag */
			    output - frag_now->fr_literal + 2,	/* where in that frag */
			    2,	/* size */
			    &e2,	/* address of expression */
			    0,	/* zero for not pc relative */
			    R_M1750_IMMW16);	/* relocation type */
	    }
	}
      else
	{
	  as_bad ("badly formed jump instruction");
	  return;
	}
      break;

    case mode_xio:
      /* XIO instructions. 
         1st operand must be a register
         2nd operand must be an XIO code, or a constant
         3rd operand is optional and must be an index register if present
       */
      if (is_a_register (&e1)
	  && (e2.X_op == O_constant || is_a_xio_cmd (&e2))
	  && (e3.X_op == O_absent || is_a_register (&e3)))
	{
	  output = frag_more (4);
	  output[0] = (opcode >> 8) & 0xff;

	  if (e3.X_op == O_absent)
	    output[1] = (e1.X_add_number & 0xf) << 4;	/* RX = 0 */
	  else if (e3.X_op == O_register)
	    {
	      if (e3.X_add_number == 0)
		as_bad ("register r0 cannot be used as an index register");
	      if (e1.X_add_number > 15)
		as_bad ("First operand must be a general register");
	      output[1] = ((e1.X_add_number & 0xf) << 4) + (e3.X_add_number & 0xf);
	    }

	  output[2] = (e2.X_add_number >> 8) & 0xff;
	  output[3] = (e2.X_add_number >> 0) & 0xff;
	}
      else
	as_bad ("operands wrong for XIO instruction");
      break;

    case mode_s:
      /* special mode, each instruction different */
      switch (opcode >> 8)
	{
	case 0x4E:
	  /* 1750B math instructions, only valid if option 1 set */
	  if (is_a_register (&e1) && e2.X_op == O_absent && e3.X_op == O_absent)
	    {
	      output = frag_more (2);
	      output[0] = (opcode >> 8) & 0xff;
	      output[1] = (opcode & 0xf0) + (e1.X_add_number & 0xf);
	    }
	  else
	    as_bad ("operands wrong for 1750B math instruction");
	  break;

	case 0x4F:
	  /* BIF instruction, takes 8-bit operand */
	  if (e1.X_op == O_constant && e2.X_op == O_absent && e3.X_op == O_absent)
	    {
	      output = frag_more (2);
	      output[0] = (opcode >> 8) & 0xff;
	      output[1] = (e1.X_add_number & 0xff);
	    }
	  else
	    as_bad ("operands wrong for BIF instruction");
	  break;

	case 0xCC:
	case 0xCD:
	case 0xCE:
	case 0xDC:
	case 0xDD:
	case 0xDE:
	  /* 1750B option 2 instructions - long loads and stores */
	  if (is_a_register (&e1) && is_a_register (&e2) && e3.X_op == O_constant)
	    {
	      output = frag_more (4);
	      output[0] = (opcode >> 8) & 0xff;
	      output[1] = ((e1.X_add_number & 0xf) << 4) + (e2.X_add_number & 0xf);
	      output[2] = (e3.X_add_number >> 8) & 0xff;
	      output[3] = e3.X_add_number & 0xff;
	    }
	  else
	    as_bad ("operands wrong for 1750B long load/store instruction");
	  break;

	case 0x77:
	  /* BEX instruction */
	  if (e1.X_op == O_constant && e2.X_op == O_absent && e3.X_op == O_absent)
	    {
	      output = frag_more (2);
	      output[0] = (opcode >> 8) & 0xff;

	      if (e1.X_add_number >= 0 && e1.X_add_number < 16)
		output[1] = e1.X_add_number;
	      else
		{
		  as_bad ("bex operand must in range 0 .. 15");
		  output[1] = 0;
		}
	    }
	  else
	    as_bad ("operands wrong for bex instruction");
	  break;

	case 0x7F:
	  /* URS/LURS instruction */
	  if (is_a_register (&e1) && e2.X_op == O_absent && e3.X_op == O_absent)
	    {
	      if (a_or_b == 'M' && (current_architecture & F_M1750MMU))
		{
		  /* long URS instruction */
		  output = frag_more (2);
                  output[0] = 0x77; /* BEX */
                  output[1] = 0x02; /* N = 2 */
		}
	      else
		{
		  /* regular URS instruction */
		  output = frag_more (2);
		  output[0] = (opcode >> 8) & 0xff;
		  output[1] = (e1.X_add_number & 0xf) << 4;
		}
	    }
	  else
	    as_bad ("operands wrong for urs instruction");
	  break;

	case 0xEC:
	  /* XBR instruction
	   * 1st operand must be a register
	   */
	  if (is_a_register (&e1) && e2.X_op == O_absent && e3.X_op == O_absent)
	    {
	      output = frag_more (2);
	      output[0] = (opcode >> 8) & 0xff;
	      output[1] = (e1.X_add_number & 0xf) << 4;
	    }
	  else
	    as_bad ("operands wrong for xbr instruction");
	  break;

	case 0x93:
	  /* MOV instruction 
	   * 1st operand must be a register
	   * 2nd operand must be a register
	   */
	  if (is_a_register (&e1) && is_a_register (&e2) && e3.X_op == O_absent)
	    {
	      output = frag_more (2);
	      output[0] = (opcode >> 8) & 0xff;
	      output[1] = ((e1.X_add_number & 0xf) << 4) + (e2.X_add_number & 0xf);
	    }
	  else
	    as_bad ("operands wrong for mov instruction");
	  break;

	case 0xFF:
	  /* NOP and BPT instructions 
	   * No operands
	   */
	  if (e1.X_op == O_absent && e2.X_op == O_absent && e3.X_op == O_absent)
	    {
	      output = frag_more (2);
	      output[0] = (opcode >> 8) & 0xff;
	      output[1] = opcode & 0xff;
	    }
	  else
	    as_bad ("operands wrong for bpt or nop instruction");
	  break;

	default:
	  output = frag_more (2);
	  output[0] = (opcode >> 8) & 0xff;
	  break;
	}
      break;

    default:
      /* illegal mode */
      break;
    }
}

#ifndef BFD_ASSEMBLER
/*ARGSUSED */
void
tc_coff_symbol_emit_hook (ignore)
     symbolS *ignore;
{
}

int
tc_coff_sizemachdep (frag)
     fragS *frag;
{
  switch (frag->fr_subtype & 0x3)
    {
    case BYTE:
      return 1;
    case WORD:
      return 2;
    case DWORD:
      return 4;
    default:
      abort ();
      return 0;
    }
}
#endif

