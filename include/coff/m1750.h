/*
 * File m1750.h
 *
 * Defines coff external file structure for 1750-format COFF
 *
 * 1750-specific code Copyright (c) 1996 Chris Nettleton Software
 * Based on GNU M68k format coff header
 */

#ifndef GNU_COFF_M1750_H
#define GNU_COFF_M1750_H 1

/********************** FILE HEADER **********************/

struct external_filehdr {
	char f_magic[2];	/* magic number			*/
	char f_nscns[2];	/* number of sections		*/
	char f_timdat[4];	/* time & date stamp		*/
	char f_symptr[4];	/* file pointer to symtab	*/
	char f_nsyms[4];	/* number of symtab entries	*/
	char f_opthdr[2];	/* sizeof(optional hdr)		*/
	char f_flags[2];	/* flags			*/
};


/*
 * MIL-STD-1750 Magic Numbers
 *
 * The are two 1750 standards, MIL-STD-1750A and MIL-STD-1750B
 * The 1750B is upward compatible with the 1750A, and has several
 * optional features indicated by flags. (we use the MS 4 bits of
 * the COFF flags word)
 */
#define	M1750MAGIC	0333
#define OMAGIC          M1750MAGIC

#define M1750BADMAG(x) ((x).f_magic != M1750MAGIC) 

/*
 * MIL-STD-1750 Architecture Options Flags
 * 
 * All 1750s support the 1750A instruction set. The 1750B has
 * three options, known here as B1, B2 and B3. Both the A and
 * the B may have a memory management unit (MMU).
 */

#define F_M1750TYPE      (0xf000)

#define F_M1750B1        (0x1000)
#define F_M1750B2        (0x2000)
#define F_M1750B3        (0x4000)
#define F_M1750MMU       (0x8000)


#define	FILHDR	struct external_filehdr
#define	FILHSZ	20

/********************** AOUT "OPTIONAL HEADER" **********************/


typedef struct 
{
  char  magic[2];               /* type of file                         */
  char  vstamp[2];              /* version stamp                        */
  char  tsize[4];               /* text size in bytes, padded to FW bdry*/
  char  dsize[4];               /* initialized data "  "                */
  char  bsize[4];               /* uninitialized data "   "             */
  char  entry[4];               /* entry pt.                            */
  char  text_start[4];          /* base of text used for this file */
  char  data_start[4];          /* base of data used for this file */
}
AOUTHDR;

#define AOUTSZ 28
#define AOUTHDRSZ 28

/********************** SECTION HEADER **********************/


struct external_scnhdr {
	char		s_name[8];	/* section name			*/
	char		s_paddr[4];	/* physical address, aliased s_nlib */
	char		s_vaddr[4];	/* virtual address		*/
	char		s_size[4];	/* section size			*/
	char		s_scnptr[4];	/* file ptr to raw data for section */
	char		s_relptr[4];	/* file ptr to relocation	*/
	char		s_lnnoptr[4];	/* file ptr to line numbers	*/
	char		s_nreloc[2];	/* number of relocation entries	*/
	char		s_nlnno[2];	/* number of line number entries*/
	char		s_flags[4];	/* flags			*/
};

/*
 * names of "special" sections
 */
#define _TEXT	".text"
#define _DATA	".data"
#define _RDATA	".rdata"
#define _BSS	".bss"
#define _COMMENT ".comment"

#define	SCNHDR	struct external_scnhdr
#define	SCNHSZ	40


/********************** LINE NUMBERS **********************/

/* 1 line number entry for every "breakpointable" source line in a section.
 * Line numbers are grouped on a per function basis; first entry in a function
 * grouping will have l_lnno = 0 and in place of physical address will be the
 * symbol table index of the function name.
 */
struct external_lineno {
	union {
		char l_symndx[4];	/* function name symbol index, iff l_lnno == 0*/
		char l_paddr[4];	/* (physical) address of line number	*/
	} l_addr;
	char l_lnno[2];	/* line number		*/
};


#define	LINENO	struct external_lineno
#define	LINESZ	6


/********************** SYMBOLS **********************/

#define E_SYMNMLEN	8	/* # characters in a symbol name	*/
#define E_FILNMLEN	14	/* # characters in a file name		*/
#define E_DIMNUM	4	/* # array dimensions in auxiliary entry */

struct external_syment 
{
  union {
    char e_name[E_SYMNMLEN];
    struct {
      char e_zeroes[4];
      char e_offset[4];
    } e;
  } e;
  char e_value[4];
  char e_scnum[2];
  char e_type[2];
  char e_sclass[1];
  char e_numaux[1];
};



#define N_BTMASK	(017)
#define N_TMASK		(060)
#define N_BTSHFT	(4)
#define N_TSHIFT	(2)
  

union external_auxent {
	struct {
		char x_tagndx[4];	/* str, un, or enum tag indx */
		union {
			struct {
			    char  x_lnno[2]; /* declaration line number */
			    char  x_size[2]; /* str/union/array size */
			} x_lnsz;
			char x_fsize[4];	/* size of function */
		} x_misc;
		union {
			struct {		/* if ISFCN, tag, or .bb */
			    char x_lnnoptr[4];	/* ptr to fcn line # */
			    char x_endndx[4];	/* entry ndx past block end */
			} x_fcn;
			struct {		/* if ISARY, up to 4 dimen. */
			    char x_dimen[E_DIMNUM][2];
			} x_ary;
		} x_fcnary;
		char x_tvndx[2];		/* tv index */
	} x_sym;

	union {
		char x_fname[E_FILNMLEN];
		struct {
			char x_zeroes[4];
			char x_offset[4];
		} x_n;
	} x_file;

	struct {
		char x_scnlen[4];			/* section length */
		char x_nreloc[2];	/* # relocation entries */
		char x_nlinno[2];	/* # line numbers */
	} x_scn;

        struct {
		char x_tvfill[4];	/* tv fill value */
		char x_tvlen[2];	/* length of .tv */
		char x_tvran[2][2];	/* tv range */
	} x_tv;		/* info about .tv section (in auxent of symbol .tv)) */


};

#define	SYMENT	struct external_syment
#define	SYMESZ	18	
#define	AUXENT	union external_auxent
#define	AUXESZ	18



/********************** RELOCATION DIRECTIVES **********************/


struct external_reloc {
  char r_vaddr[4];
  char r_symndx[4];
  char r_type[2];
#ifdef M1750_COFF_OFFSET
  char r_offset[4];
#endif

};


#define RELOC struct external_reloc

#ifdef M1750_COFF_OFFSET
#define RELSZ 14
#else
#define RELSZ 10
#endif

#endif /* GNU_COFF_M1750_H */
