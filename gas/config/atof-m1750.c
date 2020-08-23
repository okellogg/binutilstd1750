/*
 * binutils/gas/config/atof-m1750.c
 * 
 * Exact atof for MIL-STD-1750
 * 
 * Copyright (c) 1996, Chris Nettleton Software
 * 
 * The authors hereby grant permission to use, copy, modify, distribute,
 * and license this software and its documentation for any purpose,
 * provided that existing copyright notices are retained in all copies and
 * that this notice is included verbatim in any distributions. No written
 * agreement, license, or royalty fee is required for any of the
 * authorized uses. Modifications to this software may be copyrighted by
 * their authors and need not follow the licensing terms described here,
 * provided that the new terms are clearly indicated on the first page of
 * each file where they apply.
 * 
 * $log$
 */

#include <ctype.h>
#include <stdlib.h>
#include <string.h>


#include "as.h"

/*
 * The 1750 has two floating point formats, known as "floating" and
 * "extended precision floating". They are 32 and 48 bits (2 and 3 words)
 * in size resp. The 1750B also supports two fixed-point formats, "single
 * precision binary angular measurement" and "double precision binary
 * angular measurement",  which are 16 and 32 bits (1 and 2 words) in size
 * resp.
 */



/*
 * uarith.c
 *
 * Universal Arithmetic Package
 *
 * Copyright (c) 1992, Chris Nettleton, Burlington MA
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
 * Written 11-Oct-92  CCFN 
 */

/*
 * This is the structure definition for a universal integer. Each integer
 * consists of the basic structure with one-byte for the value plus as
 * many further bytes as necessary. The first byte is the  least
 * significant. 
 */
#define ui_base (32768)
#define UI_BASE_TYPE unsigned short
#define ui_log2base (15)

struct ui_type {
  unsigned int nan;
  char sign;                /* '+' or '-' */
  int len;                  /* length of value */
  UI_BASE_TYPE value [1];
};

typedef struct ui_type *ui_ptr;

/*
 * Universal reals are held as a ratio of two universal integers.
 * Zero is represented 0/1. Normalized universal reals have 
 * greatest common divisor (n, d) = 1.
 */
struct ur_type {
  ui_ptr n, d;
};

typedef struct ur_type *ur_ptr;


/* Use these to check for storage leaks */
extern long ui_alloc_cnt;
extern long ui_free_cnt;


ui_ptr ui_new (int len);

int ui_is_one (ui_ptr u);

extern long ui_alloc_cnt;
extern long ui_free_cnt;

ui_ptr ui_zero ();

ui_ptr ui_one ();

ui_ptr ui_nan ();



int ui_width (ui_ptr ui);

int ui_n2 (int r1, int n1, int r2);

ui_ptr ui_copy (ui_ptr ui);

ui_ptr ui_from_int (int i);

int ui_int (ui_ptr ui);

ui_ptr ui_from_string (int base, char *s, int exponent);

ui_ptr ui_from_decimal (char *s, int exponent);

ui_ptr ui_pow_int (ui_ptr left, int right);

ui_ptr ui_neg (ui_ptr left);

ui_ptr ui_abs (ui_ptr left);

ui_ptr ui_add (ui_ptr left, ui_ptr right);

ui_ptr ui_sub (ui_ptr left, ui_ptr right);

ui_ptr ui_mul (ui_ptr left, ui_ptr right);

ui_ptr ui_div (ui_ptr left, ui_ptr right);

ui_ptr ui_mod (ui_ptr left, ui_ptr right);

ui_ptr ui_rem (ui_ptr left, ui_ptr right);

ui_ptr ui_pow (ui_ptr left, ui_ptr right);

int ui_eq (ui_ptr left, ui_ptr right);

int ui_ne (ui_ptr left, ui_ptr right);

int ui_lt (ui_ptr left, ui_ptr right);

int ui_gt (ui_ptr left, ui_ptr right);

int ui_ge (ui_ptr left, ui_ptr right);

int ui_le (ui_ptr left, ui_ptr right);

void ui_delete (ui_ptr ui);

int ui_is_zero (ui_ptr u);

ur_ptr ur_normalize (ui_ptr n, ui_ptr d);

ur_ptr ur_zero ();

ur_ptr ur_one ();

ur_ptr ur_new (ui_ptr n, ui_ptr d);

ur_ptr ur_copy (ur_ptr u);

ur_ptr ur_from_int (int n, int d);

ur_ptr ur_from_ui (ui_ptr ui);

ur_ptr ur_from_string (int base, char *s, int exponent);

ur_ptr ur_from_decimal (char *s, int exponent);

ur_ptr ur_abs (ur_ptr left);

ur_ptr ur_neg (ur_ptr left);

ui_ptr ur_int (ur_ptr left);

ur_ptr ur_frac (ur_ptr left);

ur_ptr ur_add (ur_ptr left, ur_ptr right);

ur_ptr ur_sub (ur_ptr left, ur_ptr right);

ur_ptr ur_mul (ur_ptr left, ur_ptr right);

ur_ptr ur_div (ur_ptr left, ur_ptr right);

ur_ptr ur_pow (ur_ptr left, ui_ptr right);

int ur_eq (ur_ptr left, ur_ptr right);

int ur_ne (ur_ptr left, ur_ptr right);

int ur_lt (ur_ptr left, ur_ptr right);

int ur_gt (ur_ptr left, ur_ptr right);

int ur_ge (ur_ptr left, ur_ptr right);

int ur_le (ur_ptr left, ur_ptr right);

void ur_split (int *sign, ur_ptr *mantissa, int *exp, ur_ptr ur, int r);

void ur_convert (unsigned char *buff, int n, int r, ur_ptr ur);

void ur_delete (ur_ptr ur);

int ur_is_zero (ur_ptr u);

/* Use these to check for storage leaks */
long ui_alloc_cnt = 0L;
long ui_free_cnt = 0L;


int 
ui_width_int (int n)
  /*
    -------------------------------------------------------------------
    Compute the width in bits of the given number.  
    Examples: width (1) = 1, width (100) = 7.
    -------------------------------------------------------------------
  */
{
  int d = n;
  int w = 0;

  while (d) 
    {
      w ++; d >>= 1;
    }

  return w;
} /* ui_width_int */



int 
ui_n2 (int r1, int n1, int r2)
  /*
    -------------------------------------------------------------------
    Given a number that has n1 significant digits when expressed to
    radix r1, compute number of significant digits to radix r2. This
    version achieves an approximate result that is either correct or
    slightly high. n2 = ceil (n1 * log (r2) / log (r1))
    -------------------------------------------------------------------
  */
{
  int w1, w2;

  assert (r1 >= 2 && r1 <= ui_base);
  assert (r2 >= 2 && r2 <= ui_base);

  if (n1 == 0)
    return 0;
  else 
    {
      w1 = ui_width_int (r1 * r1 - 1);
      w2 = ui_width_int (r2 * r2) - 1;

      return (n1 * w1 + w2 - 1) / w2; 
    }
} /* ui_n2 */



int 
ui_width (ui_ptr u)
  /*
    -------------------------------------------------------------------
    Return the number of significant bits in u. 
    -------------------------------------------------------------------
  */
{
  int d;
  int w = 0;

  if (u->len > 1)
    w += (u->len - 1) * ui_log2base;

  d =  u->value [u->len - 1];
  while (d) 
    {
      d >>= 1; 
      w++;
    }

  return w;
} /* ui_width */



ui_ptr 
ui_new (int len)
  /*
    -------------------------------------------------------------------
    Create a new zero universal integer with the given number of 
    digits, and return its address.
    -------------------------------------------------------------------
  */
{
  ui_ptr ui;

  ui = malloc (sizeof (struct ui_type) + (len * sizeof (UI_BASE_TYPE)) - 1);
  assert (ui != NULL);

  if (ui == NULL) 
    {
      return NULL;
    }

  ui->nan = 0;
  ui->len = len;
  ui->sign = '+';
  if (len > 0)
    memset (&ui->value, 0,(len * sizeof (UI_BASE_TYPE)));

  ui_alloc_cnt++;
  return ui;
} /* ui_new */



void 
ui_delete (ui_ptr ui)
  /*
    -------------------------------------------------------------------
    Free the storage assocaited with the given ui.
    -------------------------------------------------------------------
  */
{
  if (ui != NULL) 
    {
      ui_free_cnt++;
      free (ui);
    }
} /* ui_delete */



ui_ptr 
ui_copy (ui_ptr u)
  /*
    -------------------------------------------------------------------
    Create and return a copy of the given ui.
    -------------------------------------------------------------------
  */
{
  ui_ptr ui = ui_new (u->len);

  ui->nan = u->nan;
  ui->sign = u->sign;
  if (u->len > 0)
    memcpy (&ui->value, &u->value, (u->len * sizeof (UI_BASE_TYPE)));

  return ui;
} /* ui_copy */



void 
ui_normalize (ui_ptr ui)
  /*
    -------------------------------------------------------------------
    Set the number of significant digits for the given ui. By 
    convention zero has the sign set to +ve.
    -------------------------------------------------------------------
 */
{
  int n;

  assert (!ui->nan);

  /* compute numbers of significant digits */
  n = ui->len;
  while (n > 0 && ui->value [n - 1] == 0)
    n--;
  ui->len = n;
  if (n == 0)
    ui->sign = '+';

} /* normalize */



int 
ui_int (ui_ptr ui)
  /*
    -------------------------------------------------------------------
    Convert the given universal integer into a host integer. The value
    of the universal integer must be in the range -32768 .. 32767
    otherwise the result will be incorrect.
    -------------------------------------------------------------------
  */
{
  int ans = 0;
  int j;

  for (j = 0; j < ui->len; j++)
    ans = ans * ui_base + ui->value [j];

  return ans;
} /* ui_int */



void 
ui_add_int (ui_ptr ui, int i)
  /*
    -------------------------------------------------------------------
    Add the given integer to the given universal integer.
    -------------------------------------------------------------------
 */
{
  int j;

  if (i != 0) {
    int sum = ui->value [0] + i;
    int carry = sum / ui_base;

    ui->value [0] = sum % ui_base;
    assert (ui->len > 0);
    
    for (j = 1; j < ui->len; j++) 
      {
	int sum = ui->value [j] + carry;

	ui->value [j] = sum % ui_base;
	carry = sum / ui_base;
      }
    assert (carry == 0);
  }

} /* ui_add_int */



void 
ui_mul_int (ui_ptr ui, int i)
  /*
    -------------------------------------------------------------------
    Multiply the given universal integer by the given int. 
    Range (0 .. ui_base - 1)
    -------------------------------------------------------------------
 */
{
  int j;
  int carry = 0;

  for (j = 0; j < ui->len; j++) 
    {
      int product = ui->value [j] * i + carry;

      ui->value [j] = product % ui_base;
      carry = product / ui_base;
    }
  assert (carry == 0);

} /* ui_mul_int */



int 
ui_div_int (ui_ptr ui, int i)
  /*
    -------------------------------------------------------------------
    Divide the given universal integer by the given single digit int.
    Note that int is in the range 0 .. ui_base-1. Return the remainder.  
    -------------------------------------------------------------------
 */
{
  assert (i > 0);

  if (i > 1) 
    {
      int j;
      int remainder = 0;

      for (j = ui->len - 1; j >= 0; j--) 
	{
	  int temp = remainder * ui_base + ui->value [j];

	  ui->value [j] = temp / i;
	  remainder = temp % i;
	} 
      return remainder;
    }
  return 0;

} /* ui_div_int */



ui_ptr 
ui_pow_int (ui_ptr u, int v)
  /*
    -------------------------------------------------------------------
    Raise u to the power v, return a new ui.
    -------------------------------------------------------------------
  */
{
  ui_ptr ans = ui_one ();
  ui_ptr temp = ui_copy (u);
  ui_ptr temp2;

  assert (v >= 0); 

  /* use square and shift */
  while (v != 0) 
    {
      if (v & 1) 
	{
	  temp2 = ui_mul (ans, temp);
	  ui_delete (ans);
	  ans = temp2;      
	}
      v >>= 1;
      if (v == 0)
	break;
      temp2 = ui_mul (temp, temp);
      ui_delete (temp);
      temp = temp2;
    }

  ui_delete (temp);
  ui_normalize (ans);

  return ans;
} /* ui_pow_int */



/*
    -------------------------------------------------------------------
    Some useful constants
    -------------------------------------------------------------------
*/
static struct ui_type ui_const_zero = {0, '+',0,{0}};
static struct ui_type ui_const_one = {0, '+',1,{1}};
static struct ui_type ui_const_nan = {1, 0,0,{0}};

ui_ptr ui_zero () 
{ 
  return ui_copy (&ui_const_zero); 
};

ui_ptr ui_one () 
{ 
  return ui_copy (&ui_const_one); 
};

ui_ptr ui_nan () 
{ 
  return ui_copy (&ui_const_nan); 
};



int 
ui_is_zero (ui_ptr u)
  /*
    -------------------------------------------------------------------
    Return true if the given ui = 0
    -------------------------------------------------------------------
  */
{
  return (!u->nan) && (u->len == 0);
} /* ui_is_zero */

  

int 
ui_is_one (ui_ptr u)
  /*
    -------------------------------------------------------------------
    Return true if the given ui = +1
    -------------------------------------------------------------------
  */
{
  return !u->nan && u->len == 1 && u->value [0] == 1 && u->sign == '+';
} /* ui_is_one */

  

ui_ptr 
ui_from_int (int i)
  /*
    -------------------------------------------------------------------
    Convert the given host integer to universal, and return the result 
    as a new ui.
    -------------------------------------------------------------------
  */
{
  long l0 = (i < 0? -i: i);
  long l;
  int d;      /* number of significant digits in i */
  ui_ptr ans;
  int j;

  /* compute d */
  l = l0;
  d = 0;
  while (l > 0) 
    {
      l /= ui_base;
      d++;
    }

  if (d == 0)
    d = 1;

  /* allocate space for ui */
  ans = ui_new (d);

  /* compute ans */
  l = l0;
  j = 0;
  while (l > 0) 
    {
      ans->value [j++] = l % ui_base;
      l /= ui_base;
    }

  if (i < 0)
    ans->sign = '-';

  ui_normalize (ans);
  return ans;
} /* ui_from_int */



ui_ptr 
ui_from_string (int radix, char *s, int exponent)
  /*
    -------------------------------------------------------------------
    Convert the given digit string to universal, and return the result 
    as a new ui. Base is in the range 2 .. 16; exponent is in the range
    0 .. 32767. Note the string may contain underscores and a decimal
    point.
    -------------------------------------------------------------------
  */
{
  int len = ui_n2 (radix, strlen (s),ui_base);
  ui_ptr ans = ui_new (len);

  while (*s != '\0') 
    {
      int d = -1;

      if (isdigit (*s))
	d = *s - '0';
      else if (radix > 10 && isupper (*s)) 
	d = *s - 'A' + 10;
      else if (radix > 10 && islower (*s))
	d = *s - 'a' + 10;
      else if (*s == '_' || *s == '.')
	d = -1;
      else
	break;

      if (d >= 0) 
	{
	  ui_mul_int (ans, radix);
	  ui_add_int (ans, d);
	}
      s++;
    }

  if (exponent != 0) 
    {
      ui_ptr b = ui_from_int (radix);
      ui_ptr e = ui_pow_int (b, exponent);
      ui_ptr new_ans = ui_mul (ans, e);

      ui_delete (ans);
      ui_delete (b);
      ui_delete (e);
      ans = new_ans;
    }

  ui_normalize (ans);

  return ans;
} /* ui_from_string */



ui_ptr 
ui_from_decimal (char *s, int exponent)
  /*
    -------------------------------------------------------------------
    Convert the given decimal digit string to universal, and return the 
    result as a new ui. 
    -------------------------------------------------------------------
  */
{
  int len = ui_n2 (10, strlen (s),ui_base);
  ui_ptr ans = ui_new (len);

  while (1) 
    {
      if (isdigit (*s)) 
	{
	  ui_mul_int (ans, 10);
	  ui_add_int (ans, *s - '0');

	} 
      else if (*s != '_' && *s != '.')
	break;

      s++;
    }

  if (exponent != 0) 
    {
      ui_ptr b = ui_from_int (10);
      ui_ptr e = ui_pow_int (b, exponent);
      ui_ptr new_ans = ui_mul (ans, e);

      ui_delete (ans);
      ui_delete (b);
      ui_delete (e);
      ans = new_ans;
    }

  ui_normalize (ans);

  return ans;
} /* ui_from_decimal */



ui_ptr 
ui_neg (ui_ptr left)
  /*
    -------------------------------------------------------------------
    Compute -left, and return the result as a new ui.
    -------------------------------------------------------------------
  */
{
  ui_ptr ans;

  if (left->nan)
    return ui_nan ();

  ans = ui_copy (left);
  if (left->len > 0)
    ans->sign = (left->sign == '+'? '-': '+');

  return ans;
} /* ui_neg */



ui_ptr 
ui_abs (ui_ptr left)
  /*
    -------------------------------------------------------------------
    Compute abs left, and return the result as a new ui.
    -------------------------------------------------------------------
  */
{
  ui_ptr ans;

  if (left->nan)
    return ui_nan ();

  ans = ui_copy (left);
  ans->sign = '+';

  return ans;
} /* ui_abs */



ui_ptr 
ui_add (ui_ptr left, ui_ptr right)
  /*
    -------------------------------------------------------------------
    Compute left + right, and return the result as a new ui.
    -------------------------------------------------------------------  
  */
{
  int maxlen = (left->len > right->len? left->len : right->len);
  ui_ptr ans;

  if (left->nan || right->nan)
    return ui_nan ();

  if (left->len == 0)
    return ui_copy (right);
  else if (right->len == 0)
    return ui_copy (left);

  ans = ui_new (maxlen + 1);
  if (left->sign == right->sign) 
    {
      int carry = 0;
      int i;

      for (i = 0; i < maxlen; i++) 
	{
	  int a = (i < left->len? left->value [i] : 0);
	  int b = (i < right->len? right->value [i] : 0);
	  int sum = a + b + carry;

	  ans->value [i] = sum % ui_base;
	  carry = sum / ui_base;
	}
      ans->value [i] = carry;
      ans->sign = left->sign;

    } 
  else 
    {
      int carry = 0;
      int i;

      for (i = 0; i < maxlen; i++) 
	{
	  int a = (i < left->len? left->value [i] : 0);
	  int b = (i < right->len? right->value [i] : 0);
	  int sum = a - b + carry;

	  if (sum < 0) 
	    {
              sum += ui_base;
              carry = -1;
	    } 
	  else
            carry = 0;

	  ans->value [i] = sum % ui_base;
	}
      ans->sign = left->sign;

      if (carry < 0) 
	{
	  carry = 0;
	  for (i = 0; i < maxlen; i++) 
	    {
              int sum = - ans->value [i] + carry;

              if (sum < 0) 
        	{
        	  sum += ui_base;
        	  carry = -1;
        	} 
      	      else
                carry = 0;

              ans->value [i] = sum % ui_base;
	    }
	  ans->sign = right->sign;
	}
    }

  ui_normalize (ans);
  return ans;
} /* ui_add */



ui_ptr 
ui_sub (ui_ptr left, ui_ptr right)
  /*
    -------------------------------------------------------------------
    Compute left - right, and return the result as a new ui.
    -------------------------------------------------------------------
  */
{
  int maxlen = (left->len > right->len? left->len : right->len);
  ui_ptr ans;

  if (left->nan || right->nan)
    return ui_nan ();

  if (left->len == 0)
    return ui_neg (right);
  else if (right->len == 0)
    return ui_copy (left);

  ans = ui_new (maxlen + 1);
  if (left->sign != right->sign) 
    {
      int carry = 0;
      int i;

      for (i = 0; i < maxlen; i++) 
	{
	  int a = (i < left->len? left->value [i] : 0);
	  int b = (i < right->len? right->value [i] : 0);
	  int sum = a + b + carry;

	  ans->value [i] = sum % ui_base;
	  carry = sum / ui_base;
	}
      ans->value [i] = carry;
      ans->sign = left->sign;

    } 
  else 
    {
      int carry = 0;
      int i;

      for (i = 0; i < maxlen; i++) 
	{
	  int a = (i < left->len? left->value [i] : 0);
	  int b = (i < right->len? right->value [i] : 0);
	  int sum = a - b + carry;

	  if (sum < 0) 
	    {
              sum += ui_base;
              carry = -1;
	    } 
	  else
            carry = 0;

	  ans->value [i] = sum % ui_base;
	}
      ans->sign = left->sign;

      if (carry < 0) 
	{
	  carry = 0;
	  for (i = 0; i < maxlen; i++) 
	    {
              int sum = - ans->value [i] + carry;
              if (sum < 0) 
        	{
        	  sum += ui_base;
        	  carry = -1;
        	} 
              else
        	carry = 0;

              ans->value [i] = sum % ui_base;
	    }
	  ans->sign = (left->sign == '+'? '-': '+');
	}
    }

  ui_normalize (ans);
  return ans;
} /* ui_sub */



ui_ptr 
ui_mul (ui_ptr left, ui_ptr right)
  /*
    -------------------------------------------------------------------
    Compute left * right, and return the result as a new ui.
    -------------------------------------------------------------------
  */
{
  ui_ptr ans;
  int i, j;

  if (left->nan || right->nan)
    return ui_nan ();

  if (ui_is_zero (left) || ui_is_zero (right))
    return ui_zero ();

  ans = ui_new (left->len + right->len);
  if (left->sign == right->sign)
    ans->sign = '+';
  else
    ans->sign = '-';

  for (i = 0; i < left->len; i++) 
    {
      int carry = 0;

      for (j = 0; j < right->len; j++) 
	{
	  int product = 
            left->value [i] * right->value [j] + ans->value [i + j] + carry;

	  ans->value [i + j] = product % ui_base;
	  carry = product / ui_base;
	}
      ans->value [i + j] = carry;
    }

  ui_normalize (ans);
  return ans;
} /* ui_mul */



static void 
long_divide (ui_ptr q, ui_ptr uu, ui_ptr u, ui_ptr v)
  /*
    -------------------------------------------------------------------
    Compute u / v, and return the quotient in q, and the remainder in r
    un >= vn, and vn >= 2
    -------------------------------------------------------------------
 */
{
  int un;     /* number of sigificant digits in u */
  int vn;     /* number of sigificant digits in v */
  ui_ptr vv;
  int j;
  int carry;
  int v1;
  int d;
  int uj;
  int qn;
 
  un = u->len;
  vn = v->len;
  assert (un >= vn && vn >= 2); 

  /* Step 1, normalize u and v, into uu and vv */
  v1 = v->value [vn - 1];
  d = ui_base / (v1 + 1);

  un++;

  vv = ui_copy (v);
  ui_mul_int (vv, d);

  if (u->len > 0)
    memcpy (uu->value, u->value, (u->len * sizeof (UI_BASE_TYPE)));
  ui_mul_int (uu, d);

  /* begin long division */
  qn = un - vn + 1;
  uj = 0;
  for (j = qn - 1; j >= 0; j--) 
    {
      int uindex = un - qn + j;
      int uj1 = uu->value [uindex];
      int uj2 = uu->value [uindex - 1];
      int v1 = vv->value [vn - 1];
      int v2 = vv->value [vn - 2];
      int qd = (uj * ui_base + uj1) / v1;
      int k;
      int ms_digit;

      if (qd > 0) 
	{
	  /* correct for two high */
	  while (v2 * qd > 
              (uj * ui_base + uj1 - qd * v1) * 
        	ui_base + uj2)
            qd--;

	  /* multiply and subtract */
	  carry = 0;
	  for (k = vn - 1; k >= 0; k--) 
	    {
              int temp = uu->value [uindex - k] - 
                	 qd * vv->value [vn - 1 - k] + carry;

              if (temp >= 0) 
        	{
        	  uu->value [uindex - k] = temp % ui_base;
        	  carry = temp / ui_base;
        	} 
              else 
                {
        	  carry = - ((ui_base - 1 - temp) / ui_base);
        	  uu->value [uindex - k] = temp - carry * ui_base;
                }
	    }
	  ms_digit = uu->value [uindex + 1] + carry;

	  /* test remainder, add back if remainder -ve */
	  if (ms_digit < 0) 
	    {
              carry = 0;
              for (k = vn - 1; k >= 0; k--) 
        	{
        	  int temp = uu->value [uindex - k] + vv->value [vn - 1 - k] + carry;

        	  uu->value [uindex - k] = temp % ui_base;
        	  carry = temp / ui_base;
        	}
            ms_digit += carry;
            qd--;
	  }

	  uu->value [uindex + 1] = ms_digit;
	}

      q->value [j] = qd;
      uj = uu->value [uindex];
    }

  /* correct remainder by dividing by d */
  if (d > 1)
    ui_div_int (uu, d);

  ui_delete (vv);

} /* long_divide */



ui_ptr 
ui_div (ui_ptr u, ui_ptr v)
  /*
    -------------------------------------------------------------------
    Compute u / v, and return the quotient as a new ui.
    -------------------------------------------------------------------
 */
{
  if (u->nan || v->nan)
    return ui_nan ();

  if (v->len == 0) 
    {
      /* divisor zero, return nan */
      return ui_nan ();

    } 
  else if (u->len < v->len) 
    {
      /* divisor greater than dividend, quotient is zero */
      return ui_zero ();

    } 
  else if (v->len == 1) 
    {
      /* use short division */
      ui_ptr q = ui_copy (u);

      ui_div_int (q, v->value [0]);
      q->sign = (u->sign == v->sign? '+' : '-');
      ui_normalize (q);
      return q;

    } 
  else 
    {
      /* use long division */
      ui_ptr q = ui_new (u->len - v->len + 1);
      ui_ptr r = ui_new (u->len + 1);

      long_divide (q, r,u,v);
      q->sign = (u->sign == v->sign? '+' : '-');

      ui_delete (r);
      ui_normalize (q);
      return q;
    }
} /* ui_div */



ui_ptr 
ui_mod (ui_ptr u, ui_ptr v)
  /*
    -------------------------------------------------------------------
    Compute u mod v, and return the result as a new ui.
    -------------------------------------------------------------------
  */
{
  if (u->nan || v->nan)
    return ui_nan ();

  if (v->len == 0) 
    {
      /* divisor zero, return nan */
      return ui_nan ();

    } 
  else if (u->len < v->len) 
    {
      /* divisor greater than dividend, remainder is u */
      ui_ptr r = ui_copy (u);

      ui_normalize (r);
      if (u->sign != v->sign && r->len > 0) 
	{
	  ui_ptr ans = ui_add (r, v);
	  ui_delete (r);
	  return ans;
	} 
      else
        return r;

    } 
  else if (v->len == 1) 
    {
      /* use short division */
      ui_ptr q = ui_copy (u);
      ui_ptr r = ui_new (1);

      r->value [0] = ui_div_int (q, v->value [0]);
      r->sign = u->sign;
      ui_delete (q);
      ui_normalize (r);

      if (u->sign != v->sign && r->len > 0) 
        {
	  ui_ptr ans = ui_add (r, v);
	  ui_delete (r);
	  return ans;
	} 
      else
	return r;
    } 
  else 
    {
      /* use long division */
      ui_ptr q = ui_new (u->len - v->len + 1);
      ui_ptr r = ui_new (u->len + 1);

      long_divide (q, r,u,v);

      r->sign = u->sign;

      ui_delete (q);
      ui_normalize (r);

      if (u->sign != v->sign && r->len > 0) 
	{
	  ui_ptr ans = ui_add (r, v);
	  ui_delete (r);
	  return ans;
	} 
      else
        return r;
    }
} /* ui_mod */



ui_ptr 
ui_rem (ui_ptr u, ui_ptr v)
  /*
    -------------------------------------------------------------------
    Compute u rem v, and return the result as a new ui.
    -------------------------------------------------------------------
  */
{
  if (u->nan || v->nan)
    return ui_nan ();

  if (v->len == 0) 
    {
      /* divisor zero, return nan */
      return ui_nan ();
    } 
  else if (u->len < v->len) 
    {
      /* divisor greater than dividend, remainder is u */
      return ui_copy (u);

    } 
  else if (v->len == 1) 
    {
      /* use short division */
      ui_ptr q = ui_copy (u);
      ui_ptr r = ui_new (1);

      r->value [0] = ui_div_int (q, v->value [0]);
      r->sign = u->sign;
      ui_delete (q);
      ui_normalize (r);
      return r;

    } 
  else 
    {
      /* use long division */
      ui_ptr q = ui_new (u->len - v->len + 1);
      ui_ptr r = ui_new (u->len + 1);

      long_divide (q, r,u,v);
      r->sign = u->sign;

      ui_delete (q);
      ui_normalize (r);
      return r;
    }
} /* ui_rem */



ui_ptr 
ui_pow (ui_ptr left, ui_ptr right)
  /*
    -------------------------------------------------------------------
    Compute left ** right, and return the result as a new ui.
    -------------------------------------------------------------------
  */
{
  if (left->nan || right->nan)
    return ui_nan ();

  else if (right->sign != '+')
    return ui_nan ();

  else if (right->len == 0)
    return ui_one ();

  else if (right->len == 1) 
    return ui_pow_int (left, right->value [0]);

  else if (right->len == 2) 
    return ui_pow_int (left, right->value [1] * ui_base + right->value [0]);

  else
    return ui_nan ();

} /* ui_pow */



int 
ui_eq (ui_ptr left, ui_ptr right)
  /*
    -------------------------------------------------------------------
    Compare left and right. 
    If left = right then return true else false
    -------------------------------------------------------------------
  */
{
  int i;

  assert (!left->nan && !right->nan);

  if (left->sign != right->sign)
    return 0;

  if (left->len != right->len)
    return 0;

  for (i = left->len - 1; i >= 0; i--)
    if (left->value [i] != right->value [i])
      return 0;

  return 1; 
} /* ui_eq */



int 
ui_lt (ui_ptr left, ui_ptr right)
  /*
    -------------------------------------------------------------------
    Compare left and right. 
    If left < right then return true else false
    -------------------------------------------------------------------
  */
{
  assert (!left->nan && !right->nan);

  if (left->sign == '-' && right->sign == '+')
    return 1;
  if (left->sign == '+' && right->sign == '-')
    return 0;

  if (left->sign == '+') 
    {
      if (left->len < right->len)
	return 1;
    } 
  else 
    {
      if (left->len > right->len)
	return 1;
    }

  {
    ui_ptr temp = ui_sub (left, right);
    int sign = temp->sign;

    ui_delete (temp);
    return (sign == '-');     
  }

  return 0; 
} /* ui_lt */



int 
ui_ne (ui_ptr left, ui_ptr right)
  /*
    -------------------------------------------------------------------
    Compare left and right. 
    If left /= right then return true else false
    -------------------------------------------------------------------
  */
{
  return !ui_eq (left, right);
} /* ui_ne */



int 
ui_gt (ui_ptr left, ui_ptr right)
  /*
    -------------------------------------------------------------------
    Compare left and right. 
    If left > right then return true else false
    -------------------------------------------------------------------
  */
{
  assert (!left->nan && !right->nan);

  if (left->sign == '+' && right->sign == '-')
    return 1;
  if (left->sign == '-' && right->sign == '+')
    return 0;

  if (left->sign == '+') 
    {
      if (left->len > right->len)
	return 1;
    } 
  else 
    {
      if (left->len < right->len)
	return 1;
    }

  {
    ui_ptr temp = ui_sub (right, left);
    int sign = temp->sign;

    ui_delete (temp);
    return (sign == '-');     
  }

  return 0; 
} /* ui_gt */



int 
ui_ge (ui_ptr left, ui_ptr right)
  /*
    -------------------------------------------------------------------
    Compare left and right. 
    If left >= right then return true else false
    -------------------------------------------------------------------
  */
{
  return !ui_lt (left, right);
} /* ui_ge */



int 
ui_le (ui_ptr left, ui_ptr right)
  /*
    -------------------------------------------------------------------
    Compare left and right. 
    If left <= right then return true else false
    -------------------------------------------------------------------
  */
{
  return !ui_gt (left, right);
} /* u_le */


ur_ptr 
ur_new (ui_ptr n, ui_ptr d)
  /*
    -------------------------------------------------------------------
    Create a new universal real with the given numerator and
    denominator. 
    -------------------------------------------------------------------
  */
{
  ur_ptr ans;

  ans = malloc (sizeof (struct ur_type));
  assert (ans != NULL);

  ans->n = n;
  ans->d = d;

  return ans;
} /* ur_new */



ur_ptr 
ur_normalize (ui_ptr n, ui_ptr d)
  /*
    -------------------------------------------------------------------
    Create a new universal real with the given numerator and
    denominator. Use Euclid's algorithm to normalize the answer.
    -------------------------------------------------------------------
  */
{
  ui_ptr u, v;
  ur_ptr ans;

  if (n->nan || d->nan) 
    {
      return ur_new (ui_nan (), ui_nan ());
    }

  if (ui_is_zero (n)) 
    {
      return ur_zero ();
    }

  u = ui_copy (n);
  v = ui_copy (d);

  /* Use Euclid's algorithm to find the greatest common divisor
     of numerator and denominator 
  */
  while (v->len > 0) 
    {
      ui_ptr r;

      if (u->len < v->len) 
	{
	  ui_ptr temp = u;
	  u = v;
	  v = temp;
	}
      r = ui_mod (u, v);
      ui_delete (u);
      u = v;
      v = r;
    }

  /* Divide both numerator and denominator by the GCD (in u)
  */
  if (ui_is_one (u))
    ans = ur_new (ui_copy (n), ui_copy (d));
  else
    ans = ur_new (ui_div (n, u), ui_div (d, u));

  /* If the ur is negative, put the -ve sign on the numerator
  */
  if (n->sign != d->sign) 
    {
      ans->n->sign = '-';
      ans->d->sign = '+';
    }

  ui_delete (u);
  ui_delete (v);

  return ans;
} /* ur_normalize */



void 
ur_delete (ur_ptr ur)
  /*
    -------------------------------------------------------------------
    Delete the given universal real    
    -------------------------------------------------------------------
  */
{
  if (ur != NULL) 
    {
      ui_delete (ur->n);
      ui_delete (ur->d);
      free (ur);
    }
} /* ur_delete */



ur_ptr 
ur_copy (ur_ptr ur)
  /*
    -------------------------------------------------------------------
    Create a new universal real from the given ur.
    -------------------------------------------------------------------
  */
{
  ur_ptr ans;

  ans = ur_new (ui_copy (ur->n), ui_copy (ur->d));

  return ans;
} /* ur_copy */



ur_ptr 
ur_zero ()
  /*
    -------------------------------------------------------------------
    Return the value zero as a universal real.
    -------------------------------------------------------------------
  */
{
  ui_ptr n = ui_zero ();
  ui_ptr d = ui_one ();
  ur_ptr ans;

  ans = ur_new (n, d);

  return ans;
} /* ur_zero */



ur_ptr 
ur_one ()
  /*
    -------------------------------------------------------------------
    Return the value one as a universal real.
    -------------------------------------------------------------------
  */
{
  ui_ptr n = ui_one ();
  ui_ptr d = ui_one ();
  ur_ptr ans;

  ans = ur_new (n, d);

  return ans;
} /* ur_one */



ur_ptr 
ur_from_int (int n, int d)
  /*
    -------------------------------------------------------------------
    Create a new ur from the given integer values
    -------------------------------------------------------------------
  */
{
  ui_ptr un = ui_from_int (n);
  ui_ptr ud = ui_from_int (d);
  ur_ptr ans;

  ans = ur_normalize (un, ud);
  ui_delete (un);
  ui_delete (ud);

  return ans;
} /* ur_from_int */



ur_ptr 
ur_from_ui (ui_ptr ui)
  /*
    -------------------------------------------------------------------
    Create a new ur from the given ui
    -------------------------------------------------------------------
  */
{
  return ur_new (ui_copy (ui), ui_one ());
} /* ur_from_int */



ur_ptr 
ur_from_string (int radix, char *s, int exponent)
  /*
    -------------------------------------------------------------------
    Convert the given digit string to universal, and return the result 
    as a new ur. Radix is in the range 2 .. 16 exponent is in the range
    0 .. 32767.
    -------------------------------------------------------------------
  */
{
  ur_ptr ur;

  if (exponent >= 0) 
    {
      ur = ur_new (ui_from_string (radix, s, exponent), ui_one ());
    } 
  else 
    {
      ui_ptr n = ui_from_string (radix, s, 0);
      ui_ptr t1 = ui_from_int (radix);
      ui_ptr d = ui_pow_int (t1, -exponent);

      ur = ur_normalize (n, d);
      ui_delete (n);
      ui_delete (t1);
      ui_delete (d);
    }

  return ur;
} /* ur_from_string */



ur_ptr 
ur_from_decimal (char *s, int exponent)
  /*
    -------------------------------------------------------------------
    Convert the given decimal digit string to universal, and return the 
    result as a new ur. 
    -------------------------------------------------------------------
  */
{
  ur_ptr ur;

  if (exponent >= 0) 
    {
      ur = ur_new (ui_from_decimal (s, exponent), ui_one ());
    } 
  else 
    {
      ui_ptr n = ui_from_decimal (s, 0);
      ui_ptr t1 = ui_from_int (10);
      ui_ptr d = ui_pow_int (t1, -exponent);

      ur = ur_normalize (n, d);
      ui_delete (n);
      ui_delete (t1);
      ui_delete (d);
    }

  return ur;
} /* ur_from_decimal */



ur_ptr 
ur_abs (ur_ptr left)
/*
    -------------------------------------------------------------------
    Compute abs left and return the result as a new ur.
    -------------------------------------------------------------------
*/
{
  ur_ptr ans;

  ans = ur_copy (left);
  ans->n->sign = '+';

  return ans;
} /* ur_abs */



ur_ptr 
ur_neg (ur_ptr left)
/*
    -------------------------------------------------------------------
    Compute -left and return the result as a new ur.
    -------------------------------------------------------------------
*/
{
  ur_ptr ans;

  ans = ur_copy (left);
  if (ans->n->sign == '+')
    ans->n->sign = '-';
  else
    ans->n->sign = '+';

  return ans;
} /* ur_neg */



ui_ptr 
ur_int (ur_ptr left)
/*
    -------------------------------------------------------------------
    Return the integer part of left as a new ui.
    -------------------------------------------------------------------
*/
{
  ui_ptr ans;

  ans = ui_div (left->n, left->d);

  return ans;
} /* ur_int */



ur_ptr 
ur_frac (ur_ptr left)
/*
    -------------------------------------------------------------------
    Return the fractional part of left as a new ur.
    -------------------------------------------------------------------
*/
{
  ui_ptr rem;
  ur_ptr ans;

  rem = ui_rem (left->n, left->d);
  ans = ur_normalize (rem, left->d);

  ui_delete (rem);

  return ans;
} /* ur_frac */



ur_ptr 
ur_add (ur_ptr left, ur_ptr right)
  /*
    -------------------------------------------------------------------
    Compute left + right, and return the result as a new ur.
    -------------------------------------------------------------------
  */
{
  ur_ptr ans;

  if (ui_eq (left->d, right->d)) 
    {
      ui_ptr n = ui_add (left->n, right->n);

      ans = ur_normalize (n, left->d);

      ui_delete (n);

    } 
  else 
    {
      ui_ptr d = ui_mul (left->d, right->d);
      ui_ptr n1 = ui_mul (left->n, right->d);
      ui_ptr n2 = ui_mul (left->d, right->n);
      ui_ptr n = ui_add (n1, n2);

      ans = ur_normalize (n, d);

      ui_delete (d);
      ui_delete (n1);
      ui_delete (n2);
      ui_delete (n);
    }

  return ans;
} /* ur_add */



ur_ptr 
ur_sub (ur_ptr left, ur_ptr right)
  /*
    -------------------------------------------------------------------
    Compute left - right, and return the result as a new ur.
    -------------------------------------------------------------------
  */
{
  ur_ptr ans;

  if (ui_eq (left->d, right->d)) 
    {
      ui_ptr n = ui_sub (left->n, right->n);

      ans = ur_normalize (n, left->d);

      ui_delete (n);

    } 
  else 
    {
      ui_ptr d = ui_mul (left->d, right->d);
      ui_ptr n1 = ui_mul (left->n, right->d);
      ui_ptr n2 = ui_mul (left->d, right->n);
      ui_ptr n = ui_sub (n1, n2);

      ans = ur_normalize (n, d);

      ui_delete (d);
      ui_delete (n1);
      ui_delete (n2);
      ui_delete (n);
    }

  return ans;
} /* ur_sub */



ur_ptr 
ur_mul (ur_ptr left, ur_ptr right)
  /*
    -------------------------------------------------------------------
    Compute left * right, and return the result as a new ur.
    -------------------------------------------------------------------
  */
{
  ui_ptr n, d;
  ur_ptr u1, u2;
  ur_ptr ans;

  u1 = ur_normalize (left->n, right->d);
  u2 = ur_normalize (right->n, left->d);

  n = ui_mul (u1->n, u2->n);
  d = ui_mul (u1->d, u2->d);
  ans = ur_new (n, d);

  ur_delete (u1);
  ur_delete (u2);

  return ans;
} /* ur_mul */



ur_ptr 
ur_div (ur_ptr left, ur_ptr right)
  /*
    -------------------------------------------------------------------
    Compute left / right, and return the result as a new ur.
    -------------------------------------------------------------------
  */
{
  ui_ptr n, d;
  ur_ptr u1, u2;
  ur_ptr ans;

  u1 = ur_normalize (left->n, right->n);
  u2 = ur_normalize (right->d, left->d);

  n = ui_mul (u1->n, u2->n);
  d = ui_mul (u1->d, u2->d);
  ans = ur_new (n, d);

  ur_delete (u1);
  ur_delete (u2);

  return ans;
} /* ur_div */



ur_ptr 
ur_pow (ur_ptr left, ui_ptr right)
  /*
    -------------------------------------------------------------------
    Compute left ** right, and return the result as a new ur.
    -------------------------------------------------------------------
  */
{
  ui_ptr n, d;
  ur_ptr ans;

    if (ui_is_zero (right)) 
    {
      ans = ur_copy (left);

    } 
  else if (right->sign == '+') 
    {
      n = ui_pow (left->n, right);
      d = ui_pow (left->d, right);

      ans = ur_normalize (n, d);
      ui_delete (d);
      ui_delete (n);

    } 
  else 
    {
      ui_ptr abs_right = ui_abs (right);

      n = ui_pow (left->n, abs_right);
      d = ui_pow (left->d, abs_right);

      ans = ur_normalize (d, n);
      ui_delete (d);
      ui_delete (n);
      ui_delete (abs_right);
    }

  return ans;
} /* ur_pow */



int 
ur_eq (ur_ptr left, ur_ptr right)
  /*
    -------------------------------------------------------------------
    Compare left and right. 
    If left = right then return true else false
    -------------------------------------------------------------------
  */
{
  int ans;
  
  ans = ui_eq (left->n, right->n) && ui_eq (left->d, right->d);

  return ans;
} /* ur_eq */



int 
ur_ne (ur_ptr left, ur_ptr right)
  /*
    -------------------------------------------------------------------
    Compare left and right. 
    If left /= right then return true else false
    -------------------------------------------------------------------
  */
{
  return !ur_eq (left, right);
} /* ur_ne */



int 
ur_lt (ur_ptr left, ur_ptr right)
  /*
    -------------------------------------------------------------------
    Compare left and right. 
    If left < right then return true else false
    -------------------------------------------------------------------
  */
{
  ur_ptr diff = ur_sub (left, right);
  int ans;

  ans = (diff->n->sign == '-');
  ur_delete (diff);

  return ans;
} /* ur_lt */



int 
ur_gt (ur_ptr left, ur_ptr right)
  /*
    -------------------------------------------------------------------
    Compare left and right. 
    If left > right then return true else false
    -------------------------------------------------------------------
  */
{
  ur_ptr diff = ur_sub (right, left);
  int ans;

  ans = (diff->n->sign == '-');
  ur_delete (diff);

  return ans;
} /* ur_gt */



int 
ur_ge (ur_ptr left, ur_ptr right)
  /*
    -------------------------------------------------------------------
    Compare left and right. 
    If left >= right then return true else false
    -------------------------------------------------------------------
  */
{
  return !ur_lt (left, right);
} /* ur_ge */



int 
ur_le (ur_ptr left, ur_ptr right)
  /*
    -------------------------------------------------------------------
    Compare left and right. 
    If left <= right then return true else false
    -------------------------------------------------------------------
  */
{
  return !ur_gt (left, right);
} /* ur_le */


void 
ur_convert (unsigned char *buff, int n, int r, ur_ptr ur)
  /*
    -------------------------------------------------------------------
    Convert the given universal real to a digit string in the given 
    buffer. Parameter n gives the number of digits to convert, 
    parameter r gives the radix to be used.
    -------------------------------------------------------------------
  */
{
  ui_ptr temp1, temp2, temp3, temp4, ui;
  int j;
  int nonzero;

  /* Multiply the given ur by r**n, then get the integer part
  */
  temp1 = ui_from_int (r);
  temp2 = ui_from_int (n);
  temp3 = ui_pow (temp1, temp2);
  temp4 = ui_mul (ur->n, temp3);
  ui = ui_div (temp4, ur->d);

  /* check given ui for zero
  */
  nonzero = (ui->len > 0);

  /* compute digit [j], start with ls digit */
  for (j = 0; j < n && nonzero; j++) 
    {
      int i;
      int rem = 0;

      nonzero = 0;
      for (i = ui->len - 1; i >= 0; i--) 
	{
	  int dividend = ui->value [i] + (rem * ui_base);
	  int quotient = dividend / r;

	  rem = dividend % r;
	  ui->value [i] = quotient;
	  if (quotient != 0)
            nonzero = 1;
	}
      buff [j] = rem;
    }

  /* Append trailing zeros
  */
  while (j < n)
    buff [j++] = 0;

  /* Reverse digits so ms digit is first 
  */
  for (j = 0; j < n / 2; j++) 
    {
      int temp = buff [j];
      buff [j] = buff [n - j - 1];
      buff [n - j - 1] = temp;
    }

  /* Tidy up
  */
  ui_delete (ui);
  ui_delete (temp1);
  ui_delete (temp2);
  ui_delete (temp3);
  ui_delete (temp4);
} /* ur_convert */



int 
ur_is_zero (ur_ptr u)
  /*
    -------------------------------------------------------------------
    Check if the given ur is zero.
    -------------------------------------------------------------------
  */
{
  return (!u->n->nan) && (u->n->len == 0);
}



void 
ur_split (int *sign, ur_ptr *mantissa, int *exp, ur_ptr ur, int r)
  /*
    -------------------------------------------------------------------
    Get the normalized Ada sign, mantissa, and exponent, for futher
    information see Ada LRM 3.5.7/4.  Parameter r gives the radix.
    -------------------------------------------------------------------
 */
{
  ui_ptr uiradix, uione;
  ur_ptr urradix, urone;
  int exp2, expr;
  ur_ptr urmantissa;

  assert (r >= 2 && r <= 256);

  if (ui_is_zero (ur->n)) {
    *sign = '+';
    *mantissa = ur_zero ();
    *exp = 0;
    return;
  }

  uione = ui_one ();
  urone = ur_one ();
  uiradix = ui_from_int (r);
  urradix = ur_new (uiradix, uione);
  
  /* Get approx exponent as power of two 
  */
  exp2 = ui_width (ur->n) - ui_width (ur->d);

  /* Convert to exponent to power of radix (approx)
  */
  if (exp2 > 0)
    expr = ui_n2 (2, exp2, r);
  else if (exp2 < 0)
    expr = -ui_n2 (2, -exp2, r);
  else
    expr = 0;   

  /* Compute approx mantissa from given ui
  */
  if (expr != 0) 
    {
      ur_ptr factor;

      if (expr > 0)
	factor = ur_new (ui_one (), ui_pow_int (uiradix, expr));
      else
	factor = ur_new (ui_pow_int (uiradix, -expr), ui_one ());

      urmantissa = ur_mul (ur, factor);
      ur_delete (factor);

    } 
  else 
    {
      urmantissa = ur_copy (ur);
    }

  /* Set sign, and set mantissa +ve
  */
  if (urmantissa->n->sign == '+')
    *sign = 1;
  else
    *sign = -1;
  urmantissa->n->sign = '+';

  /* Now adjust mantissa so its value lies between 1/radix and 1.0
  */
  /* First reduce value if >= 1.0, (ie num >= den)
  */
  while (ui_ge (urmantissa->n, urmantissa->d)) 
    {
      ur_ptr temp;

      temp = ur_div (urmantissa, urradix);
      ur_delete (urmantissa);
      urmantissa = temp;
      expr++;
    }

  /* Second increase value if < 1.0 / radix, (ie num * radix < den)
  */
  while (1) 
    {
      ui_ptr t = ui_mul (urmantissa->n, uiradix);
      int done = ui_ge (t, urmantissa->d);
      ur_ptr temp;

      ui_delete (t);
      if (done)
	break;

      temp = ur_mul (urmantissa, urradix);
      ur_delete (urmantissa);
      urmantissa = temp;
      expr--;
    }

  /* Return the mantissa and exponent */
  *mantissa = urmantissa;
  *exp = expr;

  /* Tidy up
  */
  ur_delete (urradix);
  ur_delete (urone);

} /* ur_split */


/* 
 * Turn a string in 'input_line_pointer' into a floating point constant of type
 * 'type', and store the appropriate bytes in '*litP'. An error message is 
 * returned, or NULL if OK.
 */
char *
md_atof (type, litP, sizeP)
     char type;
     char *litP;
     int *sizeP;
{
  int size;  /* size of 1750 rep in bytes */
  int fixed = 0;

  char *p = input_line_pointer;
  char *p1;           /* where the number starts */

  int exponent = 0;   /* default exponent is zero */
  int sign = 1;       /* default sign is +ve */
  int idigs = 0;      /* number of integer part digits */
  int fdigs = 0;      /* number of fraction part digits */

  ur_ptr ur;          /* the number read in */

  long int word1, word2, word3;
  ur_ptr mantissa;
  int i;
  unsigned char bits [40];

  switch (type)
    {
    case 'f':
    case 'F':
    case 's':
    case 'S':
      size = 4;   /* single precision floating */
      fixed = 0;
      break;

    case 'd':
    case 'D':
    case 'r':
    case 'R':
      size = 6;   /* extended precision floating */
      fixed = 0;
      break;

    case 'a':
    case 'A':
      size = 2;   /* single precision binary angular measurement */
      fixed = 1;
      break;

    case 'b':
    case 'B':
      size = 4;   /* double precision binary angular measurement */
      fixed = 1;
      break;

    default:
      return "invalid floating type";
    }

  /* skip white space */
  while (isspace (*p))
    p++;

  /* get optional sign */
  if (*p == '+')
    p++;
  else if (*p == '-')
    {
      p++;
      sign = -1;
    }

  /* get optional integer part */
  p1 = p;
  if (isdigit (*p))
    {
      while (isdigit (*p))
        {
          p++;
          idigs++;
        }
    }

  /* get optional fraction part */
  if (*p == '.')
    {
      p++;
      while (isdigit (*p))
        {
          p++;
          fdigs++;
        }
    }

  /* get optional exponent */
  if (*p == 'e' || *p == 'E')
    {
      int esign = 1;
      int e = 0;

      p++;

      if (*p == '+')
        p++;
      else if (*p == '-')
        {
          p++;
          esign = -1;
        }

      if (isdigit (*p))
        {
          while (isdigit (*p))
            {
              if (e < 256)
                e = e * 10 + *p - '0';
              p++;
            }
        }

      exponent = e * esign;
    }

  input_line_pointer = p;

  /* The number cannot be completely optional, check for digs */
  if (idigs + fdigs == 0)
    return "";

  if (sign < 0)
    {
      ur_ptr temp;

      temp = ur_from_string (10, p1, exponent - fdigs);
      ur = ur_neg (temp);
      ur_delete (temp);
    }
  else
    ur = ur_from_string (10, p1, exponent - fdigs);

  /* now convert to 1750 format */
  if (ur_is_zero (ur))
    {
      word1 = 0x0000;
      word2 = 0x0000;
      word3 = 0x0000;
    }
  else if (fixed)
    {
      /* scale +/- 180 degrees to +/- 0.5 */
      ur_ptr scale_factor = ur_from_int (1, 360);
      ur_ptr temp1 = ur_mul (ur, scale_factor);
      ur_ptr temp2 = ur_frac (temp1);

      /* form 2's complement */
      ur_ptr one = ur_one ();
      ur_ptr two = ur_add (one, one);
      ur_ptr temp3 = ur_add (temp2, two);

      /* discard integer part */
      ur_ptr temp4 = ur_frac (temp3);

      /* get ms 32 bits of fraction */
      ur_convert (bits, 32, 2, temp4);

      word1 = 0x0000;
      for (i = 0; i < 16; i++)
        word1 = (word1 << 1) | bits [i];

      word2 = 0x0000;
      for (i = 16; i < 32; i++)
        word2 = (word2 << 1) | bits [i];

      /* tidy up */
      ur_delete (ur);
      ur_delete (temp1);
      ur_delete (temp2);
      ur_delete (one);
      ur_delete (two);
      ur_delete (temp3);
      ur_delete (temp4);
    }
  else
    {
      /* Floating point type */

      /* split given number into mantissa in the range 0.0 .. 0.999...,
         a sign -1, 0 or +1, and a radix 2 exponent */
      ur_split (&sign, &mantissa, &exponent, ur, 2);
      ur_delete (ur);

      /* round to 39/23 bits by adding 2**(-40) / 2**(-24) */
      if (sign != 0)
        {  
          ur_ptr one = ur_one ();
          ur_ptr temp = mantissa;
          ui_ptr radix = ui_from_int (2);
          int mbits = (strchr ("dD", type)? 39 : 23);
          ur_ptr rounding_bit = ur_new (ui_one (), ui_pow_int (radix, mbits + 1));
          ur_ptr half = ur_new (ui_one (), ui_copy (radix));

          mantissa = ur_add (temp, rounding_bit);

          /* shift right if carry out */
          if (ur_ge (mantissa, one))
            {
              ur_ptr two = ur_add (one, one);
              ur_ptr temp2 = mantissa;
 
              mantissa = ur_div (temp2, two);
              exponent++;

              ur_delete (temp2);
              ur_delete (two);
            }

          ur_delete (rounding_bit);
          ui_delete (radix);
          ur_delete (temp);
          ur_delete (one);
          ur_delete (half);
        }

      /* get 39 bits of mantissa */
      ur_convert (bits, 39, 2, mantissa);
      ur_delete (mantissa);

      if (exponent > 127)
        {
          /* complain then encode the largest value */
          as_warn ("floating point value out of range");

          if (sign < 0)
            {
              word1 = 0x8000;
              word2 = 0x007f;
              word3 = 0x0000;
            }
          else
            {
              word1 = 0x7fff;
              word2 = 0xff7f;
              word3 = 0xffff;
            }
        }
      else if (exponent < -128)
        {
          /* complain then encode the smallest value */
          as_warn ("floating point value out of range");

          if (sign < 0)
            {
              word1 = 0x8000;
              word2 = 0x0080;
              word3 = 0x0000;
            }
          else
            {
              word1 = 0x7fff;
              word2 = 0xff80;
              word3 = 0xffff;
            }
        }
      else
        {
          /* build a three-word floating point number */
          word1 = 0x0000;

          for (i = 0; i <= 14; i++)
            word1 = (word1 << 1) + bits [i];

          word2 = 0x0000;
          for (i = 15; i <= 22; i++)
            word2 = (word2 << 1) + bits [i];
          word2 = word2 << 8;

          word3 = 0x0000;
          for (i = 23; i <= 38; i++)
            word3 = (word3 << 1) + bits [i];
    
          /* for 2's complement if number is negative */
          if (sign < 0)
            {
              int carry = 0;
              int sum;

              if (strchr ("dD", type))
                {
                  sum = -word3;
                  word3 = sum & 0xffff;
                  carry = sum >> 16;
                }

              sum = -word2 + carry;
              word2 = sum & 0xff00;
              carry = sum >> 16;

              sum = -word1 + carry;
              word1 = sum & 0xffff;
              carry = sum >> 16;

            }

          if ((word1 & 0xc000) == 0xc000)
            {
              word1 = 0x8000;
              word2 = 0x0000;
              word3 = 0x0000;
              exponent--;
            }

          word2 |= exponent & 0xff;
        }
    }

  /* Now output the number */
  md_number_to_chars (litP, word1, 2);
  litP += 2;
  md_number_to_chars (litP, word2, 2);
  litP += 2;
  if (strchr ("dD", type))
    {
      md_number_to_chars (litP, word3, 2);
      litP += 2;
    }

  /* Finish up and return OK */
  *sizeP = size;
  return NULL;
}

