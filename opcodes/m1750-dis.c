/*
 * m1750-dis.c
 * 
 * Copyright (c) 1996, Chris Nettleton Software
 *
 * The authors hereby grant permission to use, copy, modify, distribute, 
 * and license this software and its documentation for any purpose, 
 * provided that existing copyright notices are retained in all copies 
 * and that this notice is included verbatim in any distributions. No 
 * written agreement, license, or royalty fee is required for any of the 
 * authorized uses. Modifications to this software may be copyrighted by 
 * their authors and need not follow the licensing terms described here, 
 * provided that the new terms are clearly indicated on the first page 
 * of each file where they apply. 
 *
 * $Log$
 */

#include <stdlib.h>
#include <stdio.h>
#include <ctype.h>
#include <getopt.h>

#include "dis-asm.h"
#include "opcode/m1750.h"

/* Maximum length of an instruction.  */
#define MAXLEN 6

#include <setjmp.h>

struct private
{
  /* Points to first byte not fetched.  */
  bfd_byte *max_fetched;
  bfd_byte the_buffer[MAXLEN];
  bfd_vma insn_start;
  jmp_buf bailout;
};

/* Make sure that bytes from INFO->PRIVATE_DATA->BUFFER (inclusive)
   to ADDR (exclusive) are valid.  Returns 1 for success, longjmps
   on error.  */
#define FETCH_DATA(info, addr) \
  ((addr) <= ((struct private *)(info->private_data))->max_fetched \
   ? 1 : fetch_data ((info), (addr)))

static int
fetch_data (info, addr)
     struct disassemble_info *info;
     bfd_byte *addr;
{
  int status;
  struct private *priv = (struct private *)info->private_data;
  bfd_vma start = priv->insn_start + (priv->max_fetched - priv->the_buffer);

  status = (*info->read_memory_func) (start,
                                      priv->max_fetched,
                                      addr - priv->max_fetched,
                                      info);
  if (status != 0)
    {
      (*info->memory_error_func) (status, start, info);
      longjmp (priv->bailout, 1);
    }
  else
    priv->max_fetched = addr;
  return 1;
}


/*
 * 1750A and 1750B addressing modes
 */
enum addressing_mode {
  mode_r, 
  mode_nr,  /* for bit set/clear/test instructions */
  mode_d, mode_dc, mode_dc2, mode_dx, mode_i, mode_ic,
  mode_ix, mode_im, mode_imx, mode_isp,
  mode_isn, mode_icr, mode_b, mode_bx,
  mode_s, 
  mode_s1,    /* use by 1750B option1 instructions */
  mode_s2,    /* use by 1750B option2 instructions */
  mode_ir, illegal};


/*
 * Names of condition codes, used in conditional jump instructions
 */
static const char *cc_names [] = {
  "0",   "lt",  "eq",  "le",     /* 0 .. 3 */
  "gt",  "ne",  "ge",  "all",    /* 4 .. 7 */
  "cy",  "clt", "ceq", "cle",    /* 8 .. 11 */
  "cgt", "cnz", "cge", "uc"      /* 12 ..15 */
};


/*
 * I/O Command Summary, from MA31750 data sheet, Figure 20c ordered by
 * code. This table was generated automatically.
 */
static const struct xio_record {
  char *mnemonic;       /* 2, 3 or 4 char mnemonic */
  unsigned code;        /* 16-bit code from user manual */
  unsigned mask;
  char model;           /* 'a' for 1750a or b, 'b' for 1750B only */
                        /* 'm' for mmu */
} xio_commands [] = {
  {"itbr",   0xc00f, 0xffff, 'z'},  /* Input timer B reset reg. */
  {"itar",   0xc002, 0xffff, 'z'},  /* Input timer A reset reg. */
  {"wpbs",   0x200f, 0xffff, 'b'},  /* Write page bank select */
  {"rpbs",   0xa00c, 0xffff, 'b'},  /* Read page bank select */
  {"rdow",   0xa008, 0xffff, 'b'},  /* Read output discrete reg. */
  {"otbr",   0x400f, 0xffff, 'b'},  /* Output timer B reset reg. */
  {"otar",   0x4002, 0xffff, 'b'},  /* Output timer A reset reg. */
  {"od",     0x2008, 0xffff, 'a'},  /* Write output discrete reg. */
  {"tbs",    0x400c, 0xffff, 'a'},  /* Timer B start */
  {"tbh",    0x400d, 0xffff, 'a'},  /* Timer B halt */
  {"tas",    0x4008, 0xffff, 'a'},  /* Timer A start */
  {"tah",    0x4009, 0xffff, 'a'},  /* Timer A halt */
  {"tpio",   0xa00b, 0xffff, 'a'},  /* Test Programmed Output */
  {"go",     0x400b, 0xffff, 'a'},  /* Reset trigger-go */
  {"rns",    0x200a, 0xffff, 'a'},  /* Reset normal power up line */
  {"rdor",   0xa008, 0xffff, 'a'},  /* Read output discrete reg. */
  {"rmfs",   0xa00d, 0xffff, 'a'},  /* Read memory fault state */
  {"ric2",   0xa002, 0xffff, 'a'},  /* Read input/output interrupt code, level 2. */
  {"ric1",   0xa001, 0xffff, 'a'},  /* Read input/output interrupt code, level 1. */
  {"rdi",    0xa009, 0xffff, 'a'},  /* Read discrete input. */
  {"rcs",    0xc001, 0xffff, 'a'},  /* Read console status */
  {"otb",    0x400e, 0xffff, 'a'},  /* Output timer B */
  {"ota",    0x400a, 0xffff, 'a'},  /* Output timer A */
  {"mpen",   0x4003, 0xffff, 'a'},  /* Memory protect enable */
  {"itb",    0xc00e, 0xffff, 'a'},  /* Input timer B */
  {"ita",    0xc00a, 0xffff, 'a'},  /* Input timer A */
  {"esur",   0x4004, 0xffff, 'a'},  /* Enable start-up ROM */
  {"dsur",   0x4005, 0xffff, 'a'},  /* Disable start-up ROM */
  {"dmae",   0x4006, 0xffff, 'a'},  /* Direct memory access enable */
  {"dmad",   0x4007, 0xffff, 'a'},  /* Direct memory access disable */
  {"co",     0x4000, 0xffff, 'a'},  /* Console data output */
  {"ci",     0xc000, 0xffff, 'a'},  /* Console data input */
  {"clc",    0x4001, 0xffff, 'a'},  /* Clear Console */
  {"sfmk",   0x2006, 0xffff, 'B'},  /* Set fault mask */
  {"rfmk",   0xa006, 0xffff, 'B'},  /* Read fault mask */
  {"wsw",    0x200e, 0xffff, 'A'},  /* Write status word */
  {"spi",    0x2005, 0xffff, 'A'},  /* Set pending interrupt reg. */
  {"smk",    0x2000, 0xffff, 'A'},  /* Set interrupt mask */
  {"rpi",    0x2004, 0xffff, 'A'},  /* Reset pending interrupt */
  {"rsw",    0xa00e, 0xffff, 'A'},  /* Read status word */
  {"rpir",   0xa004, 0xffff, 'A'},  /* Read pending interrupt reg. */
  {"rmk",    0xa000, 0xffff, 'A'},  /* Read interrupt mask */
  {"rcfr",   0xa00f, 0xffff, 'A'},  /* Read and clear fault reg. */
  {"enbl",   0x2002, 0xffff, 'A'},  /* Enable interrupts */
  {"dsbl",   0x2003, 0xffff, 'A'},  /* Disable interrupts */
  {"clir",   0x2001, 0xffff, 'A'},  /* Clear interrupt request */
  {"sfr",    0x0401, 0xffff, '3'},  /* Set fault reg. */
  {"bit",    0x840d, 0xffff, '3'},  /* Run built in test */
  {"rps",    0x8405, 0xffff, '3'},  /* Read processor status */
  {"rmfp",   0x8408, 0xffff, '3'},  /* Read memory fail page */
  {"rmfa",   0x8407, 0xffff, '3'},  /* Read memory fail address */
  {"rlp",    0x8404, 0xffff, '3'},  /* Read linkage pointer */
  {"icw",    0x840c, 0xffff, '3'},  /* Read internal config. word */
  {"rfr",    0x8401, 0xffff, '3'},  /* Read fault reg. (no clear) */
  {"rcw",    0x8410, 0xffff, '3'},  /* Read external configuration */
  {"ros",    0x8406, 0xffff, '3'},  /* Read OAS reg. */
  {"otgr",   0x040e, 0xffff, '3'},  /* Output trigger-go reg. */
  {"los",    0x0406, 0xffff, '3'},  /* Load OAS reg. */
  {"itgi",   0x840e, 0xffff, '3'},  /* Input trigger-go reset reg. */
  {"cc",     0x8402, 0xffff, '3'},  /* Console command */
  {"wopr",   0x5200, 0xff00, 'a'},  /* Write operand page reg. */
  {"wipr",   0x5100, 0xff00, 'a'},  /* Write instruction page reg. */
  {"ropr",   0xd200, 0xff00, 'a'},  /* Read operand page reg. */
  {"rmp",    0xd000, 0xff00, 'a'},  /* Read memory protect RAM */
  {"ripr",   0xd100, 0xff00, 'a'},  /* Read instruction page reg. */
  {"lmp",    0x5000, 0xff00, 'a'},  /* Load memory protect RAM */
  {"rxmp",   0xc000, 0xf000, 'b'},  /* Read ext. mem. protect RAM */
  {"lxmp",   0x4000, 0xf000, 'b'},  /* Load ext. mem. protect RAM */
  {"po",     0x0000, 0xf000, 'a'},  /* Programmed output */
  {"pi",     0x8000, 0xf000, 'a'},  /* Programmed input */
};


const struct xio_record* 
get_xio_record (unsigned code)
{
  /*
   * Given a 16-bit xio command code, return the address of the
   * record in the above table. Return NULL if no record
   * is found.
   */
  int len = sizeof (xio_commands) / sizeof (struct xio_record);

  int i = 0;

  while (i < len)
    {
      if ((code & xio_commands [i].mask) == 
          (xio_commands [i].code & xio_commands [i].mask))
        return &xio_commands [i];

      i++;
    }

  return NULL;
}

/*
 * In the following data structures, we have divided the 1750 instruction 
 * set into groups, where the number of the group is taken from the
 * most significant four bits of the first word of the instruction. Note
 * that some groups are irregular, and are handled in a special manner.
 */
struct {
  char *mnemonic;
  enum addressing_mode mode;
} group0123 [] = {
  {"LB   ", mode_b   },  /* 00 */
  {"DLB  ", mode_b   },  /* 04 */
  {"STB  ", mode_b   },  /* 08 */
  {"DSTB ", mode_b   },  /* 0C */

  {"AB   ", mode_b   },  /* 10 */
  {"SBB  ", mode_b   },  /* 14 */
  {"MB   ", mode_b   },  /* 18 */
  {"DB   ", mode_b   },  /* 1C */

  {"FAB  ", mode_b   },  /* 20 */
  {"FSB  ", mode_b   },  /* 24 */
  {"FMB  ", mode_b   },  /* 28 */
  {"FDB  ", mode_b   },  /* 2C */

  {"ORB  ", mode_b   },  /* 30 */
  {"ANDB ", mode_b   },  /* 34 */
  {"CB   ", mode_b   },  /* 38 */
  {"FCB  ", mode_b   }   /* 3C */
};

struct {
  char *mnemonic;
  enum addressing_mode mode;
} group4 [] = {
  {"DATA ", illegal  },  /* 40 */
  {"DATA ", illegal  },  /* 41 */
  {"DATA ", illegal  },  /* 42 */
  {"DATA ", illegal  },  /* 43 */
  {"DATA ", illegal  },  /* 44 */
  {"DATA ", illegal  },  /* 45 */
  {"DATA ", illegal  },  /* 46 */
  {"DATA ", illegal  },  /* 47 */
  {"XIO  ", mode_imx },  /* 48 */
  {"VIO  ", mode_d   },  /* 49 */
  {"DATA ", illegal  },  /* 4A  -- but see the following table */
  {"DATA ", illegal  },  /* 4B */
  {"DATA ", illegal  },  /* 4C */
  {"DATA ", illegal  },  /* 4D */
  {"DATA ", illegal  },  /* 4E */
  {"BIF  ", mode_s   },  /* 4F */
};

struct {
  char *mnemonic;
  enum addressing_mode mode;
} group4a [] = {
  {"UCIM ", mode_im  },  /* 4A 0 */
  {"AIM  ", mode_im  },  /* 4A 1 */
  {"SIM  ", mode_im  },  /* 4A 2 */
  {"MIM  ", mode_im  },  /* 4A 3 */
  {"MSIM ", mode_im  },  /* 4A 4 */
  {"DIM  ", mode_im  },  /* 4A 5 */
  {"DVIM ", mode_im  },  /* 4A 6 */
  {"ANDM ", mode_im  },  /* 4A 7 */
  {"ORIM ", mode_im  },  /* 4A 8 */
  {"XORM ", mode_im  },  /* 4A 9 */
  {"CIM  ", mode_im  },  /* 4A A */
  {"NIM  ", mode_im  },  /* 4A B */
  {"DATA ", illegal  },  /* 4A C */
  {"DATA ", illegal  },  /* 4A D */
  {"DATA ", illegal  },  /* 4A E */
  {"DATA ", illegal  }   /* 4A E */
};

struct {
  char *mnemonic;
  enum addressing_mode mode;
} group4x [] = {
  {"LBX  ", mode_bx  },  /* 4X 0 */
  {"DLBX ", mode_bx  },  /* 4X 1 */
  {"STBX ", mode_bx  },  /* 4X 2 */
  {"DSTX ", mode_bx  },  /* 4X 3 */
  {"ABX  ", mode_bx  },  /* 4X 4 */
  {"SBBX ", mode_bx  },  /* 4X 5 */
  {"MBX  ", mode_bx  },  /* 4X 6 */
  {"DBX  ", mode_bx  },  /* 4X 7 */
  {"FABX ", mode_bx  },  /* 4X 8 */
  {"FSBX ", mode_bx  },  /* 4X 9 */
  {"FMBX ", mode_bx  },  /* 4X A */
  {"FDBX ", mode_bx  },  /* 4X B */
  {"CBX  ", mode_bx  },  /* 4X C */
  {"FCBX ", mode_bx  },  /* 4X D */
  {"ANDX ", mode_bx  },  /* 4X E */
  {"ORBX ", mode_bx  },  /* 4X F */
};

struct {
  char *mnemonic;
  enum addressing_mode mode;
} group4e [] = {
  {"SIN  ", mode_s1  },  /* 4E 0 */
  {"ESIN ", mode_s1  },  /* 4E 1 */
  {"COS  ", mode_s1  },  /* 4E 2 */
  {"ECOS ", mode_s1  },  /* 4E 3 */
  {"SCOS ", mode_s1  },  /* 4E 4 */
  {"ESCO ", mode_s1  },  /* 4E 5 */
  {"ATAN ", mode_s1  },  /* 4E 6 */
  {"EATA ", mode_s1  },  /* 4E 7 */
  {"SQRT ", mode_s1  },  /* 4E 8 */
  {"ESQR ", mode_s1  },  /* 4E 9 */
  {"EXP  ", mode_s1  },  /* 4E A */
  {"EEXP ", mode_s1  },  /* 4E B */
  {"LN   ", mode_s1  },  /* 4E C */
  {"ELN  ", mode_s1  },  /* 4E D */
  {"DATA ", illegal  },  /* 4E E */
  {"DATA ", illegal  },  /* 4E F */
};

struct {
  char *mnemonic;
  enum addressing_mode mode;
} group5 [] = {
  {"SB   ", mode_dc  },  /* 50 */
  {"SBR  ", mode_nr  },  /* 51 */
  {"SBI  ", mode_ic  },  /* 52 */
  {"RB   ", mode_dc  },  /* 53 */
  {"RBR  ", mode_nr  },  /* 54 */
  {"RBI  ", mode_ic  },  /* 55 */
  {"TB   ", mode_dc  },  /* 56 */
  {"TBR  ", mode_nr  },  /* 57 */
  {"TBI  ", mode_ic  },  /* 58 */
  {"TSB  ", mode_d   },  /* 59 */
  {"SVBR ", mode_r   },  /* 5A */
  {"DATA ", illegal  },  /* 5B */
  {"RVBR ", mode_r   },  /* 5C */
  {"DATA ", illegal  },  /* 5D */
  {"TVBR ", mode_r   },  /* 5E */
  {"DATA ", illegal  }   /* 5F */
};

struct {
  char *mnemonic;
  enum addressing_mode mode;
} group6 [] = {
  {"SLL  ", mode_ir  },  /* 60 */
  {"SRL  ", mode_ir  },  /* 61 */
  {"SRA  ", mode_ir  },  /* 62 */
  {"SLC  ", mode_ir  },  /* 63 */
  {"DATA ", illegal  },  /* 64 */
  {"DSLL ", mode_ir  },  /* 65 */
  {"DSRL ", mode_ir  },  /* 66 */
  {"DSRA ", mode_ir  },  /* 67 */
  {"DSLC ", mode_ir  },  /* 68 */
  {"DATA ", illegal  },  /* 69 */
  {"SLR  ", mode_r   },  /* 6A */
  {"SAR  ", mode_r   },  /* 6B */
  {"SCR  ", mode_r   },  /* 6C */
  {"DSLR ", mode_r   },  /* 6D */
  {"DSAR ", mode_r   },  /* 6E */
  {"DSCR ", mode_r   }   /* 6F */
};

struct {
  char *mnemonic;
  enum addressing_mode mode;
} group7 [] = {
  {"JC   ", mode_d   },  /* 70  special format */
  {"JCI  ", mode_i   },  /* 71    ..      ..   */
  {"JS   ", mode_d   },  /* 72 */
  {"SOJ  ", mode_d   },  /* 73 */
  {"BR   ", mode_icr },  /* 74 */
  {"BEZ  ", mode_icr },  /* 75 */
  {"BLT  ", mode_icr },  /* 76 */
  {"BEX  ", mode_s   },  /* 77 */
  {"BLE  ", mode_icr },  /* 78 */
  {"BGT  ", mode_icr },  /* 79 */
  {"BNZ  ", mode_icr },  /* 7A */
  {"BGE  ", mode_icr },  /* 7B */
  {"LSTI ", mode_i   },  /* 7C */
  {"LST  ", mode_d   },  /* 7D */
  {"SJS  ", mode_d   },  /* 7E */
  {"URS  ", mode_s   }   /* 7F */
};

struct {
  char *mnemonic;
  enum addressing_mode mode;
} group8 [] = {
  {"L    ", mode_d   },  /* 80 */
  {"LR   ", mode_r   },  /* 81 */
  {"LISP ", mode_isp },  /* 82 */
  {"LISN ", mode_isn },  /* 83 */
  {"LI   ", mode_i   },  /* 84 */
  {"LIM  ", mode_imx },  /* 85 */
  {"DL   ", mode_d   },  /* 86 */
  {"DLR  ", mode_r   },  /* 87 */
  {"DLI  ", mode_i   },  /* 88 */
  {"LM   ", mode_dc  },  /* 89 */
  {"EFL  ", mode_d   },  /* 8A */
  {"LUB  ", mode_d   },  /* 8B */
  {"LLB  ", mode_d   },  /* 8C */
  {"LUBI ", mode_i   },  /* 8D */
  {"LLBI ", mode_i   },  /* 8E */
  {"POPM ", mode_s   }   /* 8F */
};

struct {
  char *mnemonic;
  enum addressing_mode mode;
} group9 [] = {
  {"ST   ", mode_d   },  /* 90 */
  {"STC  ", mode_dc  },  /* 91 */
  {"STCI ", mode_dc  },  /* 92 */
  {"MOV  ", mode_s   },  /* 93 */
  {"STI  ", mode_i   },  /* 94 */
  {"SFBS ", mode_r   },  /* 95 */
  {"DST  ", mode_d   },  /* 96 */
  {"SRM  ", mode_d   },  /* 97 */
  {"DSTI ", mode_i   },  /* 98 */
  {"STM  ", mode_dc  },  /* 99 */
  {"EFST ", mode_d   },  /* 9A */
  {"STUB ", mode_d   },  /* 9B */
  {"STLB ", mode_d   },  /* 9C */
  {"SUBI ", mode_i   },  /* 9D */
  {"SLBI ", mode_i   },  /* 9E */
  {"PSHM ", mode_s   }   /* 9F */
};

struct {
  char *mnemonic;
  enum addressing_mode mode;
} group10 [] = {
  {"A    ", mode_d   },  /* A0 */
  {"AR   ", mode_r   },  /* A1 */
  {"AISP ", mode_isp },  /* A2 */
  {"INCM ", mode_dc2 },  /* A3 */
  {"ABS  ", mode_r   },  /* A4 */
  {"DABS ", mode_r   },  /* A5 */
  {"DA   ", mode_d   },  /* A6 */
  {"DAR  ", mode_r   },  /* A7 */
  {"FA   ", mode_d   },  /* A8 */
  {"FAR  ", mode_r   },  /* A9 */
  {"EFA  ", mode_d   },  /* AA */
  {"EFAR ", mode_r   },  /* AB */
  {"FABS ", mode_r   },  /* AC */
  {"UAR  ", mode_r   },  /* AD */
  {"UA   ", mode_d   },  /* AE */
  {"LBYI ", mode_s   }   /* AF */
};

struct {
  char *mnemonic;
  enum addressing_mode mode;
} group11 [] = {
  {"S    ", mode_d   },  /* B0 */
  {"SR   ", mode_r   },  /* B1 */
  {"SISP ", mode_isp },  /* B2 */
  {"DECM ", mode_dc2 },  /* B3 */
  {"NEG  ", mode_r   },  /* B4 */
  {"DNEG ", mode_r   },  /* B5 */
  {"DS   ", mode_d   },  /* B6 */
  {"DSR  ", mode_r   },  /* B7 */
  {"FS   ", mode_d   },  /* B8 */
  {"FSR  ", mode_r   },  /* B9 */
  {"EFS  ", mode_d   },  /* BA */
  {"EFSR ", mode_r   },  /* BB */
  {"FNEG ", mode_r   },  /* BC */
  {"USR  ", mode_r   },  /* BD */
  {"US   ", mode_d   },  /* BE */
  {"LBY  ", mode_s   }   /* BF */
};

struct {
  char *mnemonic;
  enum addressing_mode mode;
} group12 [] = {
  {"MS   ", mode_d   },  /* C0 */
  {"MSR  ", mode_r   },  /* C1 */
  {"MISP ", mode_isp },  /* C2 */
  {"MISN ", mode_isn },  /* C3 */
  {"M    ", mode_d   },  /* C4 */
  {"MR   ", mode_r   },  /* C5 */
  {"DM   ", mode_d   },  /* C6 */
  {"DMR  ", mode_r   },  /* C7 */
  {"FM   ", mode_d   },  /* C8 */
  {"FMR  ", mode_r   },  /* C9 */
  {"EFM  ", mode_d   },  /* CA */
  {"EFMR ", mode_r   },  /* CB */
  {"LSL  ", mode_s2  },  /* CC */
  {"LDL  ", mode_s2  },  /* CD */
  {"LEFL ", mode_s2  },  /* CE */
  {"SBYI ", mode_s   }   /* CF */
};

struct {
  char *mnemonic;
  enum addressing_mode mode;
} group13 [] = {
  {"DV   ", mode_d   },  /* D0 */
  {"DVR  ", mode_r   },  /* D1 */
  {"DISP ", mode_isp },  /* D2 */
  {"DISN ", mode_isn },  /* D3 */
  {"D    ", mode_d   },  /* D4 */
  {"DR   ", mode_r   },  /* D5 */
  {"DD   ", mode_d   },  /* D6 */
  {"DDR  ", mode_r   },  /* D7 */
  {"FD   ", mode_d   },  /* D8 */
  {"FDR  ", mode_r   },  /* D9 */
  {"EFD  ", mode_d   },  /* DA */
  {"EFDR ", mode_r   },  /* DB */
  {"LSS  ", mode_s2  },  /* DC */
  {"LDS  ", mode_s2  },  /* DD */
  {"LEFS ", mode_s2  },  /* DE */
  {"SBY  ", mode_s   }   /* DF */
};

struct {
  char *mnemonic;
  enum addressing_mode mode;
} group14 [] = {
  {"OR   ", mode_d   },  /* E0 */
  {"ORR  ", mode_r   },  /* E1 */
  {"AND  ", mode_d   },  /* E2 */
  {"ANDR ", mode_r   },  /* E3 */
  {"XOR  ", mode_d   },  /* E4 */
  {"XORR ", mode_r   },  /* E5 */
  {"N    ", mode_d   },  /* E6 */
  {"NR   ", mode_r   },  /* E7 */
  {"FIX  ", mode_r   },  /* E8 */
  {"FLT  ", mode_r   },  /* E9 */
  {"EFIX ", mode_r   },  /* EA */
  {"EFLT ", mode_r   },  /* EB */
  {"XBR  ", mode_s   },  /* EC */
  {"XWR  ", mode_r   },  /* ED */
  {"DATA ", illegal  },  /* EE */
  {"DATA ", illegal  }   /* EF */
};

struct {
  char *mnemonic;
  enum addressing_mode mode;
} group15 [] = {
  {"C    ", mode_d   },  /* F0 */
  {"CR   ", mode_r   },  /* F1 */
  {"CISP ", mode_isp },  /* F2 */
  {"CISN ", mode_isn },  /* F3 */
  {"CBL  ", mode_d   },  /* F4 */
  {"DATA ", illegal  },  /* F5 */
  {"DC   ", mode_d   },  /* F6 */
  {"DCR  ", mode_r   },  /* F7 */
  {"FC   ", mode_d   },  /* F8 */
  {"FCR  ", mode_r   },  /* F9 */
  {"EFC  ", mode_d   },  /* FA */
  {"EFCR ", mode_r   },  /* FB */
  {"UCR  ", mode_r   },  /* FC */
  {"UC   ", mode_d   },  /* FD */
  {"DATA ", illegal  },  /* FE */
  {"BPT  ", mode_s   },  /* FF */
  {"NOP  ", mode_s   }   /* FF */
};


int
print_insn_m1750 (addr, info)
     bfd_vma addr;
     disassemble_info *info;
{
  /* 
   * Print the m1750 instruction at address addr in debugged memory,
   * on info->stream. Returns length of the instruction, in bytes.
   */
  int size = 0;
  unsigned word1, word2;
  int lab;
  int i;

  /* to be filled in by code later in this function */
  char *mnemonic = "???";
  enum addressing_mode mode = illegal;

  int top4, next4;

  /* stuff copied from m68k-dis.c */
  struct private priv;
  bfd_byte *buffer = priv.the_buffer;
  fprintf_ftype save_printer = info->fprintf_func;
  void (*save_print_address) PARAMS((bfd_vma, struct disassemble_info*))
    = info->print_address_func;

  info->private_data = (PTR) &priv;
  priv.max_fetched = priv.the_buffer;
  priv.insn_start = addr;
  if (setjmp (priv.bailout) != 0)
    /* Error return.  */
    return -1;

  info->insn_info_valid = 1;        /* We do return this info */
  info->insn_type = dis_nonbranch;  /* Assume non branch insn */
  info->branch_delay_insns = 0;     /* Assume no delay */
  info->target = 0;                 /* Assume no target known */

  /* get first 16-bit word */
  FETCH_DATA (info, buffer + 2);
  word1 = buffer [0] << 8;
  word1 += buffer [1];
  size += 2;

  /* the second 16-bit word is optional, gets filled in later
     if required */

  /* decode top 4 bits */
  top4 = (word1 & 0xf000) >> 12;
  next4 = (word1 & 0x0f00) >> 8;

  switch (top4) 
    {
    case 0:
    case 1:
    case 2:
    case 3:
      /*
       * groups 0, 1, 2, and 3 are all mode_b instructions, the
       * instructions a not indexable, and use bits 4,5 as an 
       * opcode extension
       */ 
      mnemonic = group0123 [((top4 & 0x3) << 2) + (next4 >> 2)].mnemonic;
      mode = mode_b;
      break;

    case 4:
       /*
        * group 4A are immediate instructions, using ls 4 bits
        * of word1 as an opcode extension
        */ 

      if (next4 >= 0 && next4 <= 3) 
        {
          /* 4X instructions (have I got this right?) */
          mnemonic = group4x [(word1 & 0xf0) >> 4].mnemonic;
          mode = group4x [(word1 & 0xf0) >> 4].mode;
        }
      else if (next4 == 8) 
        {
          /* xio instruction */
          mnemonic = group4 [next4].mnemonic;
          mode = group4 [next4].mode;
        }
      else if (next4 == 9) 
        {
          /* vio instruction */
          mnemonic = group4 [next4].mnemonic;
          mode = group4 [next4].mode;
        }
      else if (next4 == 10) 
        {
          /* 4A (imml) instructions */
          mnemonic = group4a [word1 & 0xf].mnemonic;
          mode = group4a [word1 & 0xf].mode;
        }
      else if (next4 == 14) 
        {
          /* 4E (math) instructions */
          mnemonic = group4e [(word1 & 0xf0) >> 4].mnemonic;
          mode = group4e [(word1 & 0xf0) >> 4].mode;
        }
      else if (next4 == 15) 
        {
          /* 4E (BIF) instructions */
          mnemonic = "BIF";
          mode = mode_s;
        }
      else
        {
          mnemonic = "?";
          mode = illegal;
        }
      break;

    case 5:
      mnemonic = group5 [next4].mnemonic;
      mode = group5 [next4].mode;
      break;

    case 6:
      mnemonic = group6 [next4].mnemonic;
      mode = group6 [next4].mode;
      break;

    case 7:
      mnemonic = group7 [next4].mnemonic;
      mode = group7 [next4].mode;
      break;

    case 8:
      mnemonic = group8 [next4].mnemonic;
      mode = group8 [next4].mode;
      break;

    case 9:
      mnemonic = group9 [next4].mnemonic;
      mode = group9 [next4].mode;
      break;

    case 10:
      mnemonic = group10 [next4].mnemonic;
      mode = group10 [next4].mode;
      break;

    case 11:
      mnemonic = group11 [next4].mnemonic;
      mode = group11 [next4].mode;
      break;

    case 12:
      mnemonic = group12 [next4].mnemonic;
      mode = group12 [next4].mode;
      break;

    case 13:
      mnemonic = group13 [next4].mnemonic;
      mode = group13 [next4].mode;
      break;

    case 14:
      mnemonic = group14 [next4].mnemonic;
      mode = group14 [next4].mode;
      break;

    case 15:
      if (((next4) == 15) && (word1 & 0xf) == 0)
        mnemonic = group15 [16].mnemonic;
      else
        mnemonic = group15 [next4].mnemonic;
      mode = group15 [next4].mode;
      break;
    }

  /* convert mnemonic to lower case */
  {
    char mnemonic2 [20];  /* more than enough for a 1750 mnemonic */
    int len = strlen (mnemonic);

    strcpy (mnemonic2, mnemonic);
    for (i = 0; i < len; i++)
      if (isupper (mnemonic2 [i]))
        mnemonic2 [i] = tolower (mnemonic2 [i]);

    (*info->fprintf_func) (info->stream, "%-7s", mnemonic2);
  }

  /* act on address mode */
  switch (mode) 
    {
      case illegal:
        {
          (*info->fprintf_func) (info->stream, "#%04x", word1);
        }
        break;

      case mode_r:
        {
          /* two registers in ls 8 bits of word 1 */
          int ra = (word1 & 0xf0) >> 4;
          int rb = (word1 & 0xf);

          (*info->fprintf_func) (info->stream, "r%d,r%d", ra, rb);
        }
        break;

      case mode_nr:
        {
          /* small literal and register in ls 8 bits of word 1 */
          int b = (word1 & 0xf0) >> 4;
          int rb = (word1 & 0xf);

          (*info->fprintf_func) (info->stream, "%d,r%d", b, rb);
        }
        break;

      case mode_ir:
        /*
         * mode_ir is invented *here* for shift instructions
         * that have the number of places where the first
         * register should be.
         */ 
        {
          /* two registers in ls 8 bits of word 1 */
          int n = (word1 & 0xf0) >> 4;
          int rb = (word1 & 0xf);

          (*info->fprintf_func) (info->stream, "r%d,%d", rb, n + 1);
        }
        break;

      case mode_d:
      case mode_dc:
      case mode_dc2:
      case mode_dx:
      case mode_i:
      case mode_ic:
      case mode_ix:
        {
          /* Memory direct, or indirect */
          int ra = (word1 & 0xf0) >> 4;
          int rx = (word1 & 0xf);

          FETCH_DATA (info, buffer + 4);
          word2 = buffer [2] << 8;
          word2 += buffer [3];
          size += 2;

          /* special symbols if this is a JC or JCI instruction */
          if (top4 == 7 && (next4 == 0 || next4 == 1)) {

            if (rx == 0)
              {
                info->target = word2 << 1;
                (*info->fprintf_func) (info->stream, "%s,", cc_names [ra]);
                (*info->print_address_func) (info->target, info);
              } 
            else
              (*info->fprintf_func) (info->stream, "%s,0x%04x,r%d", cc_names [ra], word2, rx);

          /* special address format for jump instructions  JS, SOJ, SJS */
          } else if (top4 == 7 && (next4 == 2 || next4 == 2 || next4 == 14)) {

            if (rx == 0)
              {
                (*info->fprintf_func) (info->stream, "r%d,", ra);
                (*info->print_address_func) (word2 << 1, info);
              } 
            else
              (*info->fprintf_func) (info->stream, "r%d,0x%04x,r%d", ra, word2, rx);

          /* STC is a special case where ra is a literal */
          } else if (mode == mode_dc || mode == mode_ic) {
            if (rx == 0)
              {
                (*info->fprintf_func) (info->stream, "%d,", ra);
                (*info->print_address_func) (word2 << 1, info);
              } 
            else
              (*info->fprintf_func) (info->stream, "%d,%d,r%d", ra, word2, rx);

          /* LST or LSTI instruction */
          } else if (top4 == 7 && (next4 == 0xc || next4 == 0xd) && ra == 0) {

            if (rx == 0)
              {
                (*info->fprintf_func) (info->stream, "%d,", ra);
                (*info->print_address_func) (word2 << 1, info);
              }
            else
              (*info->fprintf_func) (info->stream, "%d,r%d", word2, rx);

          } else if (mode == mode_dc2) {

            if (rx == 0)
              {
                (*info->fprintf_func) (info->stream, "%d,", ra + 1);
                (*info->print_address_func) (word2 << 1, info);
              }
            else
              (*info->fprintf_func) (info->stream, "%d,%d,r%d", ra + 1, word2, rx);

          /* simple numeric format for non-jumps */
          } else {

            if (rx == 0)
              {
                (*info->fprintf_func) (info->stream, "r%d,", ra);
                (*info->print_address_func) (word2 << 1, info);
              }
            else
              (*info->fprintf_func) (info->stream, "r%d,%d,r%d", ra, word2, rx);
          }
        }
        break;

      case mode_im:
        {
          /* immediate long, has an opcode extension field */
          int ra = (word1 & 0xf0) >> 4;

          FETCH_DATA (info, buffer + 4);
          word2 = buffer [2] << 8;
          word2 += buffer [3];
          size += 2;

          (*info->fprintf_func) (info->stream, "r%d,%d", ra, word2);
        }
        break;

      case mode_imx:
        {
          /* indexable, indexed if rx /= 0 */
          int ra = (word1 & 0xf0) >> 4;
          int rx = (word1 & 0xf);

          FETCH_DATA (info, buffer + 4);
          word2 = buffer [2] << 8;
          word2 += buffer [3];
          size += 2;

          if (top4 == 4 && next4 == 8) 
            {
            /* this is an xio, special format for literal */
            const struct xio_record* x = get_xio_record (word2);
            char mn [80];

            if (x->mask == 0xffff)
              sprintf (mn, "%s", x->mnemonic);
            else if (x->mask == 0xfff0)
              sprintf (mn, "%s+0x%01x", x->mnemonic, x->code - (x->code & x->mask));
            else if (x->mask == 0xff00)
              sprintf (mn, "%s+0x%02x", x->mnemonic, x->code - (x->code & x->mask));
            else if (x->mask == 0xf000)
              sprintf (mn, "%s+0x%03x", x->mnemonic, x->code - (x->code & x->mask));
            else
              sprintf (mn, "%s+0x%04x", x->mnemonic, x->code - (x->code & x->mask));

            if (rx == 0)
              (*info->fprintf_func) (info->stream, "r%d,%s", ra, mn);
            else
              (*info->fprintf_func) (info->stream, "r%d,%s,r%d", ra, mn, rx);

          } else {
             /* just a regular immediate, display literal as a decimal
                number */
            if (rx == 0)
              (*info->fprintf_func) (info->stream, "r%d,%d", ra, word2);
            else
              (*info->fprintf_func) (info->stream, "r%d,%d,r%d", ra, word2, rx);
          }
        }
        break;

      case mode_isp:
      case mode_isn:
        {
          /*
           * Immediate short positive, ls 4 bits give immediate value,
           * range 1 .. 16, Immediate short negative, ls 4 bits 
           * range -1 .. -16
           */ 
          int ra = (word1 & 0xf0) >> 4;
          int i = (word1 & 0xf);

          (*info->fprintf_func) (info->stream, "r%d,%d", ra, i + 1);
        }
        break;

      case mode_icr:
        {
          /* Instruction counter relative (signed word displacement) */
          int d = (word1 & 0xff);

          if (d & 0x80)
            d |= 0xffffff00;
  
          info->target = addr + d * 2;
          (*info->fprintf_func) (info->stream, "%d", d);
        }
        break;

      case mode_b:
        {
          /* Base relative, unsigned word displacment */
          int d = word1 & 0xff;
          int br = (word1 & 0x300) >> 8;

          (*info->fprintf_func) (info->stream, "r%d,%d", br + 12, d);
        }
        break;

      case mode_bx:
        {
          /* Base relative, indexed and not indexed */
          int ocx = (word1 & 0xf0) >> 4;
          int rx = (word1 & 0xf);
          int br = (word1 & 0x300) >> 8;

          if (rx == 0)
            (*info->fprintf_func) (info->stream, "r%d", br + 12);
          else
            (*info->fprintf_func) (info->stream, "r%d,r%d", br + 12, rx);
        }
        break;

      case mode_s:
        {
          /* special addressing mode */
          int s1 = (word1 & 0xf0) >> 4;
          int s2 = (word1 & 0xf);
          int top8 = (word1 >> 8) & 0x00FF;

          if (word1 == 0xffff)
            {
              /* Breakpoint instruction */
              (*info->fprintf_func) (info->stream, "");
            }
          else if (word1 == 0xff00)
            {
              /* nop instruction */
              (*info->fprintf_func) (info->stream, "");
            }
          else if (top8 == 0xEC)
            {
              /* XBR instruction */
              (*info->fprintf_func) (info->stream, "r%d", s1);
            }
          else if (top8 == 0x8F ||
              top8 == 0x9F ||
              top8 == 0x93)
            {
              /* popm pshm move */
              (*info->fprintf_func) (info->stream, "r%d,r%d", s1, s2);
            }
          else if (top8 == 0x7F)
            {
              /* urs, ls 4 bits should be zero */
              if ((word1 & 0xf) == 0)
                (*info->fprintf_func) (info->stream, "r%d", s1);
              else
                (*info->fprintf_func) (info->stream, "r%d,%d", s1, word1 & 0xf);
            }
          else if ((word1 & 0xfff0) == 0x7700)
            {
              /* BEX instruction */
              (*info->fprintf_func) (info->stream, "%d", word1 & 0xff);
            }
          else if (top8 == 0xAF || top8 == 0xBF ||top8 == 0xCF || top8 == 0xDF )
            {
              /* LBY etc */
              (*info->fprintf_func) (info->stream, "r%d,r%d", s1, s2);
            }
          else if (top8 == 0x4F)
            {
              /* BIF instruction */
              (*info->fprintf_func) (info->stream, "%d", word1 & 0xfF);
            }
          else
            {
              /* other special instruction */
              (*info->fprintf_func) (info->stream, "%d,%d", s1, s2);
            }
        }
        break;

      case mode_s1:
        {
          int rb = (word1 & 0xf);

          (*info->fprintf_func) (info->stream, "r%d", rb);
        }
        break;

      case mode_s2:
        {
          int ra = (word1 & 0xf0) >> 4;
          int rb = (word1 & 0xf);

          FETCH_DATA (info, buffer + 4);
          word2 = buffer [2] << 8;
          word2 += buffer [3];
          size += 2;

          (*info->fprintf_func) (info->stream, "r%d,r%d,0x%04X", ra, rb, word2);
        }
      break;
    }

  return size;
}

