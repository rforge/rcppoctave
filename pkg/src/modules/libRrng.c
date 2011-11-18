/*
 * This file contains the portions of R source code related to the default
 * random number generator (Mersenne-Twitter).
 *
 *  R : A Computer Language for Statistical Data Analysis
 *  Copyright (C) 1995, 1996  Robert Gentleman and Ross Ihaka
 *  Copyright (C) 1997--2011  The R Development Core Team
 *
 *  This program is free software; you can redistribute it and/or modify
 *  it under the terms of the GNU General Public License as published by
 *  the Free Software Foundation; either version 2 of the License, or
 *  (at your option) any later version.
 *
 *  This program is distributed in the hope that it will be useful,
 *  but WITHOUT ANY WARRANTY; without even the implied warranty of
 *  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 *  GNU General Public License for more details.
 *
 *  You should have received a copy of the GNU General Public License
 *  along with this program; if not, a copy is available at
 *  http://www.r-project.org/Licenses/
 */

#include <stdio.h>
#include <stdlib.h>
#include <stdint.h>
#include <math.h>
#include <limits.h>

#include "libRrng.h"

/* Normal generator is not actually set here but in nmath/snorm.c */
#define RNG_DEFAULT MERSENNE_TWISTER
#define N01_DEFAULT INVERSION


//#include <R_ext/Rdynload.h>
//
//static DL_FUNC User_unif_fun, User_unif_nseed,
//	User_unif_seedloc;
//typedef void (*UnifInitFun)(Int32);
//
//UnifInitFun User_unif_init = NULL; /* some picky compilers */
//
//DL_FUNC  User_norm_fun = NULL; /* also in ../nmath/snorm.c */
//
//
static RNGtype RNG_kind = RNG_DEFAULT;
//extern N01type N01_kind; /* from ../nmath/snorm.c */
static N01type N01_kind = INVERSION;
//
///* typedef unsigned int Int32; in Random.h */
//
///* .Random.seed == (RNGkind, i_seed[0],i_seed[1],..,i_seed[n_seed-1])
// * or           == (RNGkind) or missing  [--> Randomize]
// */
//
typedef struct {
    RNGtype kind;
    N01type Nkind;
    char *name; /* print name */
    int n_seed; /* length of seed vector */
    Int32 *i_seed;
} RNGTAB;


static Int32 dummy[MAX_SEED_LENGTH];
static
RNGTAB RNG_Table[] =
{
/* kind Nkind	  name	           n_seed      i_seed */
    { WICHMANN_HILL,        BUGGY_KINDERMAN_RAMAGE, "Wichmann-Hill",	     3,	dummy},
    { MARSAGLIA_MULTICARRY, BUGGY_KINDERMAN_RAMAGE, "Marsaglia-MultiCarry",  2,	dummy},
    { SUPER_DUPER,          BUGGY_KINDERMAN_RAMAGE, "Super-Duper",	     2,	dummy},
    { MERSENNE_TWISTER,     BUGGY_KINDERMAN_RAMAGE, "Mersenne-Twister",  1+624,	dummy},
    { KNUTH_TAOCP,          BUGGY_KINDERMAN_RAMAGE, "Knuth-TAOCP",       1+100,	dummy},
    { USER_UNIF,            BUGGY_KINDERMAN_RAMAGE, "User-supplied",         0,	dummy},
    { KNUTH_TAOCP2,         BUGGY_KINDERMAN_RAMAGE, "Knuth-TAOCP-2002",  1+100,	dummy},
    { LECUYER_CMRG,         BUGGY_KINDERMAN_RAMAGE, "L'Ecuyer-CMRG",         6,	dummy},
};


#define d2_32	4294967296./* = (double) */
#define i2_32m1 2.328306437080797e-10/* = 1/(2^32 - 1) */
#define KT      9.31322574615479e-10 /* = 2^-30 */

#define I1 (RNG_Table[RNG_kind].i_seed[0])
#define I2 (RNG_Table[RNG_kind].i_seed[1])
#define I3 (RNG_Table[RNG_kind].i_seed[2])

static void Randomize(RNGtype kind);
static double MT_genrand(void);
static Int32 KT_next(void);
//static void RNG_Init_R_KT(Int32);
static void RNG_Init_KT2(Int32);
#define KT_pos (RNG_Table[KNUTH_TAOCP].i_seed[100])

static double fixup(double x)
{
    /* ensure 0 and 1 are never returned */
    if(x <= 0.0) return 0.5*i2_32m1;
    if((1.0 - x) <= 0.0) return 1.0 - 0.5*i2_32m1;
    return x;
}


double unif_rand(void)
{
    double value;

    switch(RNG_kind) {

    case WICHMANN_HILL:
	I1 = I1 * 171 % 30269;
	I2 = I2 * 172 % 30307;
	I3 = I3 * 170 % 30323;
	value = I1 / 30269.0 + I2 / 30307.0 + I3 / 30323.0;
	return fixup(value - (int) value);/* in [0,1) */

    case MARSAGLIA_MULTICARRY:/* 0177777(octal) == 65535(decimal)*/
	I1= 36969*(I1 & 0177777) + (I1>>16);
	I2= 18000*(I2 & 0177777) + (I2>>16);
	return fixup(((I1 << 16)^(I2 & 0177777)) * i2_32m1); /* in [0,1) */

    case SUPER_DUPER:
	/* This is Reeds et al (1984) implementation;
	 * modified using __unsigned__	seeds instead of signed ones
	 */
	I1 ^= ((I1 >> 15) & 0377777); /* Tausworthe */
	I1 ^= I1 << 17;
	I2 *= 69069;		/* Congruential */
	return fixup((I1^I2) * i2_32m1); /* in [0,1) */

    case MERSENNE_TWISTER:
	return fixup(MT_genrand());

//    case KNUTH_TAOCP:
    case KNUTH_TAOCP2:
	return fixup(KT_next() * KT);

//    case USER_UNIF:
//	return *((double *) User_unif_fun());

    case LECUYER_CMRG:
    {
	/* Based loosely on the GPL-ed version of
	   http://www.iro.umontreal.ca/~lecuyer/myftp/streams00/c2010/RngStream.c
	   but using int_least64_t, which C99 guarantees.
	*/
	int k;
	int_least64_t p1, p2;

#define II(i) (RNG_Table[RNG_kind].i_seed[i])
#define m1    4294967087
#define m2    4294944443
#define normc  2.328306549295727688e-10
#define a12     (int_least64_t)1403580
#define a13n    (int_least64_t)810728
#define a21     (int_least64_t)527612
#define a23n    (int_least64_t)1370589

	p1 = a12 * (unsigned int)II(1) - a13n * (unsigned int)II(0);
	/* p1 % m1 would surely do */
	k = p1 / m1;
	p1 -= k * m1;
	if (p1 < 0.0) p1 += m1;
	II(0) = II(1); II(1) = II(2); II(2) = p1;

	p2 = a21 * (unsigned int)II(5) - a23n * (unsigned int)II(3);
	k = p2 / m2;
	p2 -= k * m2;
	if (p2 < 0.0) p2 += m2;
	II(3) = II(4); II(4) = II(5); II(5) = p2;

	return ((p1 > p2) ? (p1 - p2) : (p1 - p2 + m1)) * normc;
    }
    default:
	error(_("unif_rand: unimplemented RNG kind %d"), RNG_kind);
	return -1.;
    }
}

/* we must mask global variable here, as I1-I3 hide RNG_kind
   and we want the argument */
static void FixupSeeds(RNGtype RNG_kind, int initial)
{
/* Depending on RNG, set 0 values to non-0, etc. */

    int j, notallzero = 0;

    /* Set 0 to 1 :
       for(j = 0; j <= RNG_Table[RNG_kind].n_seed - 1; j++)
       if(!RNG_Table[RNG_kind].i_seed[j]) RNG_Table[RNG_kind].i_seed[j]++; */

    switch(RNG_kind) {
    case WICHMANN_HILL:
	I1 = I1 % 30269; I2 = I2 % 30307; I3 = I3 % 30323;

	/* map values equal to 0 mod modulus to 1. */
	if(I1 == 0) I1 = 1;
	if(I2 == 0) I2 = 1;
	if(I3 == 0) I3 = 1;
	return;

    case SUPER_DUPER:
	if(I1 == 0) I1 = 1;
	/* I2 = Congruential: must be ODD */
	I2 |= 1;
	break;

    case MARSAGLIA_MULTICARRY:
	if(I1 == 0) I1 = 1;
	if(I2 == 0) I2 = 1;
	break;

    case MERSENNE_TWISTER:
	if(initial) I1 = 624;
	 /* No action unless user has corrupted .Random.seed */
	if(I1 <= 0) I1 = 624;
	/* check for all zeroes */
	for (j = 1; j <= 624; j++)
	    if(RNG_Table[RNG_kind].i_seed[j] != 0) {
		notallzero = 1;
		break;
	    }
	if(!notallzero) Randomize(RNG_kind);
	break;

//    case KNUTH_TAOCP:
    case KNUTH_TAOCP2:
	if(KT_pos <= 0) KT_pos = 100;
	/* check for all zeroes */
	for (j = 0; j < 100; j++)
	    if(RNG_Table[RNG_kind].i_seed[j] != 0) {
		notallzero = 1;
		break;
	    }
	if(!notallzero) Randomize(RNG_kind);
	break;
//    case USER_UNIF:
//	break;
    case LECUYER_CMRG:
	/* first set: not all zero, in [0, m1)
	   second set: not all zero, in [0, m2) */
    {
	unsigned int tmp;
	int allOK = 1;
	for (j = 0; j < 3; j++) {
	    tmp = RNG_Table[RNG_kind].i_seed[j];
	    if(tmp != 0) notallzero = 1;
	    if (tmp >= m1) allOK = 0;
	}
	if(!notallzero || !allOK) Randomize(RNG_kind);
	for (j = 3; j < 6; j++) {
	    tmp = RNG_Table[RNG_kind].i_seed[j];
	    if(tmp != 0) notallzero = 1;
	    if (tmp >= m2) allOK = 0;
	}
	if(!notallzero || !allOK) Randomize(RNG_kind);
    }
    break;
    default:
	error(_("FixupSeeds: unimplemented RNG kind %d"), RNG_kind);
    }
}

//extern double BM_norm_keep; /* ../nmath/snorm.c */
static double BM_norm_keep = 0.0;

static void RNG_Init(RNGtype kind, Int32 seed)
{
    int j;

    BM_norm_keep = 0.0; /* zap Box-Muller history */

    /* Initial scrambling */
    for(j = 0; j < 50; j++)
	seed = (69069 * seed + 1);
    switch(kind) {
    case WICHMANN_HILL:
    case MARSAGLIA_MULTICARRY:
    case SUPER_DUPER:
    case MERSENNE_TWISTER:
	/* i_seed[0] is mti, *but* this is needed for historical consistency */
	for(j = 0; j < RNG_Table[kind].n_seed; j++) {
	    seed = (69069 * seed + 1);
	    RNG_Table[kind].i_seed[j] = seed;
	}
	FixupSeeds(kind, 1);
	break;
//    case KNUTH_TAOCP:
//	RNG_Init_R_KT(seed);
//	break;
    case KNUTH_TAOCP2:
	RNG_Init_KT2(seed);
	break;
    case LECUYER_CMRG:
	for(j = 0; j < RNG_Table[kind].n_seed; j++) {
	    seed = (69069 * seed + 1);
	    while(seed >= m2) seed = (69069 * seed + 1);
	    RNG_Table[kind].i_seed[j] = seed;
	}
	break;
//    case USER_UNIF:
//	User_unif_fun = R_FindSymbol("user_unif_rand", "", NULL);
//	if (!User_unif_fun) error(_("'user_unif_rand' not in load table"));
//	User_unif_init = (UnifInitFun) R_FindSymbol("user_unif_init", "", NULL);
//	if (User_unif_init) (void) User_unif_init(seed);
//	User_unif_nseed = R_FindSymbol("user_unif_nseed", "", NULL);
//	User_unif_seedloc = R_FindSymbol("user_unif_seedloc", "",  NULL);
//	if (User_unif_seedloc) {
//	    int ns = 0;
//	    if (!User_unif_nseed) {
//		warning(_("cannot read seeds unless 'user_unif_nseed' is supplied"));
//		break;
//	    }
//	    ns = *((int *) User_unif_nseed());
//	    if (ns < 0 || ns > 625) {
//		warning(_("seed length must be in 0...625; ignored"));
//		break;
//	    }
//	    RNG_Table[kind].n_seed = ns;
//	    RNG_Table[kind].i_seed = (Int32 *) User_unif_seedloc();
//	}
//	break;
    default:
	error(_("RNG_Init: unimplemented RNG kind %d"), kind);
    }
}

unsigned int TimeToSeed(void); /* datetime.c */

static void Randomize(RNGtype kind)
{
/* Only called by  GetRNGstate() when there is no .Random.seed */
    RNG_Init(kind, TimeToSeed());
}

//static void GetRNGkind(SEXP seeds)
//{
//    /* Load RNG_kind, N01_kind from .Random.seed if present */
//    int tmp, *is;
//    RNGtype newRNG; N01type newN01;
//
//    if (isNull(seeds))
//	seeds = findVarInFrame(R_GlobalEnv, R_SeedsSymbol);
//    if (seeds == R_UnboundValue) return;
//    if (!isInteger(seeds)) {
//	if (seeds == R_MissingArg) /* How can this happen? */
//	    error(_(".Random.seed is a missing argument with no default"));
//	error(_(".Random.seed is not an integer vector but of type '%s'"),
//		type2char(TYPEOF(seeds)));
//    }
//    is = INTEGER(seeds);
//    tmp = is[0];
//    if (tmp == NA_INTEGER)
//	error(_(".Random.seed[1] is not a valid integer"));
//    newRNG = (RNGtype) (tmp % 100);
//    newN01 = (N01type) (tmp / 100);
//    if (newN01 < 0 || newN01 > KINDERMAN_RAMAGE)
//	error(_(".Random.seed[0] is not a valid Normal type"));
//    switch(newRNG) {
//    case WICHMANN_HILL:
//    case MARSAGLIA_MULTICARRY:
//    case SUPER_DUPER:
//    case MERSENNE_TWISTER:
//    case KNUTH_TAOCP:
//    case KNUTH_TAOCP2:
//    case LECUYER_CMRG:
//	break;
//    case USER_UNIF:
//	if(!User_unif_fun)
//	    error(_(".Random.seed[1] = 5 but no user-supplied generator"));
//	break;
//    default:
//	error(_(".Random.seed[1] is not a valid RNG kind (code)"));
//    }
//    RNG_kind = newRNG; N01_kind = newN01;
//    return;
//}


void GetRNGstate()
{
//    /* Get  .Random.seed  into proper variables */
//    int len_seed;
//    SEXP seeds;
//
//    /* look only in the workspace */
//    seeds = findVarInFrame(R_GlobalEnv, R_SeedsSymbol);
//    if (seeds == R_UnboundValue) {
//	Randomize(RNG_kind);
//    } else {
//	GetRNGkind(seeds);
//	len_seed = RNG_Table[RNG_kind].n_seed;
//	/* Not sure whether this test is needed: wrong for USER_UNIF */
//	if(LENGTH(seeds) > 1 && LENGTH(seeds) < len_seed + 1)
//	    error(_(".Random.seed has wrong length"));
//	if(LENGTH(seeds) == 1 && RNG_kind != USER_UNIF)
//	    Randomize(RNG_kind);
//	else {
//	    int j, *is = INTEGER(seeds);
//	    for(j = 1; j <= len_seed; j++)
//		RNG_Table[RNG_kind].i_seed[j - 1] = is[j];
	    FixupSeeds(RNG_kind, 0);
//	}
//    }
}
//
//void PutRNGstate()
//{
//    /* Copy out seeds to  .Random.seed  */
//    int len_seed, j;
//    SEXP seeds;
//
//    if (RNG_kind < 0 || RNG_kind > LECUYER_CMRG ||
//	N01_kind < 0 || N01_kind > KINDERMAN_RAMAGE) {
//	warning("Internal .Random.seed is corrupt: not saving");
//	return;
//    }
//
//    len_seed = RNG_Table[RNG_kind].n_seed;
//
//    PROTECT(seeds = allocVector(INTSXP, len_seed + 1));
//
//    INTEGER(seeds)[0] = RNG_kind + 100 * N01_kind;
//    for(j = 0; j < len_seed; j++)
//	INTEGER(seeds)[j+1] = RNG_Table[RNG_kind].i_seed[j];
//
//    /* assign only in the workspace */
//    defineVar(R_SeedsSymbol, seeds, R_GlobalEnv);
//    UNPROTECT(1);
//}

static void RNGkind(RNGtype newkind)
{
/* Choose a new kind of RNG.
 * Initialize its seed by calling the old RNG's unif_rand()
 */
    if (newkind == -1) newkind = RNG_DEFAULT;
    switch(newkind) {
    case WICHMANN_HILL:
    case MARSAGLIA_MULTICARRY:
    case SUPER_DUPER:
    case MERSENNE_TWISTER:
//    case KNUTH_TAOCP:
//    case USER_UNIF:
    case KNUTH_TAOCP2:
    case LECUYER_CMRG:
	break;
    default:
	error(_("RNGkind: unimplemented RNG kind %d"), newkind);
    }
    GetRNGstate();
    RNG_Init(newkind, unif_rand() * UINT_MAX);
    RNG_kind = newkind;
//    PutRNGstate();
}

static void Norm_kind(N01type kind)
{
    if (kind == -1) kind = N01_DEFAULT;
    if (kind < 0 || kind > KINDERMAN_RAMAGE)
	error(_("invalid Normal type in RNGkind"));
//    if (kind == USER_NORM) {
//	User_norm_fun = R_FindSymbol("user_norm_rand", "", NULL);
//	if (!User_norm_fun) error(_("'user_norm_rand' not in load table"));
//    }
    GetRNGstate(); /* might not be initialized */
    if (kind == BOX_MULLER)
	BM_norm_keep = 0.0; /* zap Box-Muller history */
    N01_kind = kind;
//    PutRNGstate();
}


/*------ .Internal interface ------------------------*/
#define asInteger(x) x
#define isNull(x) (x==NULL)

void do_RNGkind (RNGtype* rng, N01type* norm)
{
//    SEXP ans, rng, norm;
//
//    checkArity(op,args);
    GetRNGstate(); /* might not be initialized */
//    PROTECT(ans = allocVector(INTSXP, 2));
//    INTEGER(ans)[0] = RNG_kind;
//    INTEGER(ans)[1] = N01_kind;
//    rng = CAR(args);
//    norm = CADR(args);
//    GetRNGkind(R_NilValue); /* pull from .Random.seed if present */
    if(!isNull(rng)) { /* set a new RNG kind */
	RNGkind((RNGtype) asInteger(*rng));
    }
    if(!isNull(norm)) { /* set a new normal kind */
	Norm_kind((N01type) asInteger(*norm));
    }
//    UNPROTECT(1);
//    return ans;
}


void do_setseed (Int32 seed, RNGtype* skind, N01type* nkind)
{
//    SEXP skind, nkind;
//    int seed;
//
//    checkArity(op,args);
//    seed = asInteger(CAR(args));
//    if (seed == NA_INTEGER)
//	error(_("supplied seed is not a valid integer"));
//    skind = CADR(args);
//    nkind = CADDR(args);
//    GetRNGkind(R_NilValue); /* pull RNG_kind, N01_kind from
//			       .Random.seed if present */
    if (!isNull(skind)) RNGkind((RNGtype) asInteger(*skind));
    if (!isNull(nkind)) Norm_kind((N01type) asInteger(*nkind));
    RNG_Init(RNG_kind, (Int32) seed); /* zaps BM history */
//    PutRNGstate();
//    return R_NilValue;
}


///* S COMPATIBILITY */
//
///* The following entry points provide compatibility with S. */
///* These entry points should not be used by new R code. */
//
//void seed_in(long *ignored)
//{
//    GetRNGstate();
//}
//
//void seed_out(long *ignored)
//{
//    PutRNGstate();
//}

/* ===================  Mersenne Twister ========================== */
/* From http://www.math.keio.ac.jp/~matumoto/emt.html */

/* A C-program for MT19937: Real number version([0,1)-interval)
   (1999/10/28)
     genrand() generates one pseudorandom real number (double)
   which is uniformly distributed on [0,1)-interval, for each
   call. sgenrand(seed) sets initial values to the working area
   of 624 words. Before genrand(), sgenrand(seed) must be
   called once. (seed is any 32-bit integer.)
   Integer generator is obtained by modifying two lines.
     Coded by Takuji Nishimura, considering the suggestions by
   Topher Cooper and Marc Rieffel in July-Aug. 1997.

   Copyright (C) 1997, 1999 Makoto Matsumoto and Takuji Nishimura.
   When you use this, send an email to: matumoto@math.keio.ac.jp
   with an appropriate reference to your work.

   REFERENCE
   M. Matsumoto and T. Nishimura,
   "Mersenne Twister: A 623-Dimensionally Equidistributed Uniform
   Pseudo-Random Number Generator",
   ACM Transactions on Modeling and Computer Simulation,
   Vol. 8, No. 1, January 1998, pp 3--30.
*/

/* Period parameters */
#define N 624
#define M 397
#define MATRIX_A 0x9908b0df   /* constant vector a */
#define UPPER_MASK 0x80000000 /* most significant w-r bits */
#define LOWER_MASK 0x7fffffff /* least significant r bits */

/* Tempering parameters */
#define TEMPERING_MASK_B 0x9d2c5680
#define TEMPERING_MASK_C 0xefc60000
#define TEMPERING_SHIFT_U(y)  (y >> 11)
#define TEMPERING_SHIFT_S(y)  (y << 7)
#define TEMPERING_SHIFT_T(y)  (y << 15)
#define TEMPERING_SHIFT_L(y)  (y >> 18)

static Int32 *mt = dummy+1; /* the array for the state vector  */
static int mti=N+1; /* mti==N+1 means mt[N] is not initialized */

/* Initializing the array with a seed */
static void
MT_sgenrand(Int32 seed)
{
    int i;

    for (i = 0; i < N; i++) {
	mt[i] = seed & 0xffff0000;
	seed = 69069 * seed + 1;
	mt[i] |= (seed & 0xffff0000) >> 16;
	seed = 69069 * seed + 1;
    }
    mti = N;
}

/**
 * Extract the RNG seed as .Random.seed: the first element encodes the RNG and
 * Normal kind.
 *
 * @param rseed output seed (must be at least 626 long)
 */
int do_getseed(int* rseed){

	int nseed = RNG_Table[RNG_kind].n_seed;
	for(int j = 0; j < nseed; j++) {
		//printf("%i ",RNG_Table[RNG_kind].i_seed[j-1]);
		rseed[j + 1]  = RNG_Table[RNG_kind].i_seed[j];
	}
	//printf("\n");
	rseed[0] = RNG_kind + 100 * N01_kind;
	return nseed;
}

/* Initialization by "sgenrand()" is an example. Theoretically,
   there are 2^19937-1 possible states as an intial state.
   Essential bits in "seed_array[]" is following 19937 bits:
    (seed_array[0]&UPPER_MASK), seed_array[1], ..., seed_array[N-1].
   (seed_array[0]&LOWER_MASK) is discarded.
   Theoretically,
    (seed_array[0]&UPPER_MASK), seed_array[1], ..., seed_array[N-1]
   can take any values except all zeros.                             */

static double MT_genrand(void)
{
    Int32 y;
    static Int32 mag01[2]={0x0, MATRIX_A};
    /* mag01[x] = x * MATRIX_A  for x=0,1 */

    mti = dummy[0];

    if (mti >= N) { /* generate N words at one time */
	int kk;

	if (mti == N+1)   /* if sgenrand() has not been called, */
	    MT_sgenrand(4357); /* a default initial seed is used   */

	for (kk = 0; kk < N - M; kk++) {
	    y = (mt[kk] & UPPER_MASK) | (mt[kk+1] & LOWER_MASK);
	    mt[kk] = mt[kk+M] ^ (y >> 1) ^ mag01[y & 0x1];
	}
	for (; kk < N - 1; kk++) {
	    y = (mt[kk] & UPPER_MASK) | (mt[kk+1] & LOWER_MASK);
	    mt[kk] = mt[kk+(M-N)] ^ (y >> 1) ^ mag01[y & 0x1];
	}
	y = (mt[N-1] & UPPER_MASK) | (mt[0] & LOWER_MASK);
	mt[N-1] = mt[M-1] ^ (y >> 1) ^ mag01[y & 0x1];

	mti = 0;
    }

    y = mt[mti++];
    y ^= TEMPERING_SHIFT_U(y);
    y ^= TEMPERING_SHIFT_S(y) & TEMPERING_MASK_B;
    y ^= TEMPERING_SHIFT_T(y) & TEMPERING_MASK_C;
    y ^= TEMPERING_SHIFT_L(y);
    dummy[0] = mti;

    return ( (double)y * 2.3283064365386963e-10 ); /* reals: [0,1)-interval */
}

/*
   The following code was taken from earlier versions of
   http://www-cs-faculty.stanford.edu/~knuth/programs/rng.c-old
   http://www-cs-faculty.stanford.edu/~knuth/programs/rng.c
*/


#define long Int32
#define ran_arr_buf       R_KT_ran_arr_buf
#define ran_arr_cycle     R_KT_ran_arr_cycle
#define ran_arr_ptr       R_KT_ran_arr_ptr
#define ran_arr_sentinel  R_KT_ran_arr_sentinel
#define ran_x             dummy

#define KK 100                     /* the long lag */
#define LL  37                     /* the short lag */
#define MM (1L<<30)                 /* the modulus */
#define TT  70   /* guaranteed separation between streams */
#define mod_diff(x,y) (((x)-(y))&(MM-1)) /* subtraction mod MM */
#define is_odd(x)  ((x)&1)          /* units bit of x */
static void ran_array(long aa[],int n)    /* put n new random numbers in aa */
{
  register int i,j;
  for (j=0;j<KK;j++) aa[j]=ran_x[j];
  for (;j<n;j++) aa[j]=mod_diff(aa[j-KK],aa[j-LL]);
  for (i=0;i<LL;i++,j++) ran_x[i]=mod_diff(aa[j-KK],aa[j-LL]);
  for (;i<KK;i++,j++) ran_x[i]=mod_diff(aa[j-KK],ran_x[i-LL]);
}
#define QUALITY 1009 /* recommended quality level for high-res use */
static long ran_arr_buf[QUALITY];
static long ran_arr_sentinel=(long)-1;
static long *ran_arr_ptr=&ran_arr_sentinel; /* the next random number, or -1 */

static long ran_arr_cycle(void)
{
  ran_array(ran_arr_buf,QUALITY);
  ran_arr_buf[KK]=-1;
  ran_arr_ptr=ran_arr_buf+1;
  return ran_arr_buf[0];
}

/* ===================  Knuth TAOCP  2002 ========================== */

/*    This program by D E Knuth is in the public domain and freely copyable.
 *    It is explained in Seminumerical Algorithms, 3rd edition, Section 3.6
 *    (or in the errata to the 2nd edition --- see
 *        http://www-cs-faculty.stanford.edu/~knuth/taocp.html
 *    in the changes to Volume 2 on pages 171 and following).              */

/*    N.B. The MODIFICATIONS introduced in the 9th printing (2002) are
      included here; there's no backwards compatibility with the original. */


static void ran_start(long seed)
{
  register int t,j;
  long x[KK+KK-1];              /* the preparation buffer */
  register long ss=(seed+2)&(MM-2);
  for (j=0;j<KK;j++) {
    x[j]=ss;                      /* bootstrap the buffer */
    ss<<=1; if (ss>=MM) ss-=MM-2; /* cyclic shift 29 bits */
  }
  x[1]++;              /* make x[1] (and only x[1]) odd */
  for (ss=seed&(MM-1),t=TT-1; t; ) {
    for (j=KK-1;j>0;j--) x[j+j]=x[j], x[j+j-1]=0; /* "square" */
    for (j=KK+KK-2;j>=KK;j--)
      x[j-(KK-LL)]=mod_diff(x[j-(KK-LL)],x[j]),
      x[j-KK]=mod_diff(x[j-KK],x[j]);
    if (is_odd(ss)) {              /* "multiply by z" */
      for (j=KK;j>0;j--)  x[j]=x[j-1];
      x[0]=x[KK];            /* shift the buffer cyclically */
      x[LL]=mod_diff(x[LL],x[KK]);
    }
    if (ss) ss>>=1; else t--;
  }
  for (j=0;j<LL;j++) ran_x[j+KK-LL]=x[j];
  for (;j<KK;j++) ran_x[j-LL]=x[j];
  for (j=0;j<10;j++) ran_array(x,KK+KK-1); /* warm things up */
  ran_arr_ptr=&ran_arr_sentinel;
}
/* ===================== end of Knuth's code ====================== */

static void RNG_Init_KT2(Int32 seed)
{
    ran_start(seed % 1073741821);
    KT_pos = 100;
}

static Int32 KT_next(void)
{
    if(KT_pos >= 100) {
	ran_arr_cycle();
	KT_pos = 0;
    }
    return ran_x[(KT_pos)++];
}

//static void RNG_Init_R_KT(Int32 seed)
//{
//    SEXP fun, sseed, call, ans;
//    fun = findVar1(install(".TAOCP1997init"), R_BaseEnv, CLOSXP, FALSE);
//    if(fun == R_UnboundValue)
//	error("function '.TAOCP1997init' is missing");
//    PROTECT(sseed = ScalarInteger(seed % 1073741821));
//    PROTECT(call = lang2(fun, sseed));
//    ans = eval(call, R_GlobalEnv);
//    memcpy(dummy, INTEGER(ans), 100*sizeof(int));
//    UNPROTECT(2);
//    KT_pos = 100;
//}

double norm_rand(void){
	#define BIG 134217728 /* 2^27 */
	/* unif_rand() alone is not of high enough precision */
	double u1 = unif_rand();
	u1 = (int)(BIG*u1) + unif_rand();
	return qnorm5(u1/BIG, 0.0, 1.0, 1, 0);
}

double exp_rand(void)
{
    /* q[k-1] = sum(log(2)^k / k!)  k=1,..,n, */
    /* The highest n (here 16) is determined by q[n-1] = 1.0 */
    /* within standard precision */
    const static double q[] =
    {
	0.6931471805599453,
	0.9333736875190459,
	0.9888777961838675,
	0.9984959252914960,
	0.9998292811061389,
	0.9999833164100727,
	0.9999985691438767,
	0.9999998906925558,
	0.9999999924734159,
	0.9999999995283275,
	0.9999999999728814,
	0.9999999999985598,
	0.9999999999999289,
	0.9999999999999968,
	0.9999999999999999,
	1.0000000000000000
    };

    double a = 0.;
    double u = unif_rand();    /* precaution if u = 0 is ever returned */
    while(u <= 0. || u >= 1.) u = unif_rand();
    for (;;) {
	u += u;
	if (u > 1.)
	    break;
	a += q[0];
    }
    u -= 1.;

    if (u <= q[0])
	return a + u;

    int i = 0;
    double ustar = unif_rand(), umin = ustar;
    do {
	ustar = unif_rand();
	if (umin > ustar)
	    umin = ustar;
	i++;
    } while (u > q[i]);
    return a + umin * q[0];
}

# define R_FINITE(x)    isfinite(x)

/* From rgamma.c */

/*
 *  Mathlib : A C Library of Special Functions
 *  Copyright (C) 1998 Ross Ihaka
 *  Copyright (C) 2000--2008 The R Development Core Team
 *
 *  This program is free software; you can redistribute it and/or modify
 *  it under the terms of the GNU General Public License as published by
 *  the Free Software Foundation; either version 2 of the License, or
 *  (at your option) any later version.
 *
 *  This program is distributed in the hope that it will be useful,
 *  but WITHOUT ANY WARRANTY; without even the implied warranty of
 *  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 *  GNU General Public License for more details.
 *
 *  You should have received a copy of the GNU General Public License
 *  along with this program; if not, a copy is available at
 *  http://www.r-project.org/Licenses/
 *
 *  SYNOPSIS
 *
 *    #include <Rmath.h>
 *    double rgamma(double a, double scale);
 *
 *  DESCRIPTION
 *
 *    Random variates from the gamma distribution.
 *
 *  REFERENCES
 *
 *    [1] Shape parameter a >= 1.  Algorithm GD in:
 *
 *	  Ahrens, J.H. and Dieter, U. (1982).
 *	  Generating gamma variates by a modified
 *	  rejection technique.
 *	  Comm. ACM, 25, 47-54.
 *
 *
 *    [2] Shape parameter 0 < a < 1. Algorithm GS in:
 *
 *	  Ahrens, J.H. and Dieter, U. (1974).
 *	  Computer methods for sampling from gamma, beta,
 *	  poisson and binomial distributions.
 *	  Computing, 12, 223-246.
 *
 *    Input: a = parameter (mean) of the standard gamma distribution.
 *    Output: a variate from the gamma(a)-distribution
 */

#define repeat for(;;)

double rgamma(double a, double scale)
{
/* Constants : */
    const static double sqrt32 = 5.656854;
    const static double exp_m1 = 0.36787944117144232159;/* exp(-1) = 1/e */

    /* Coefficients q[k] - for q0 = sum(q[k]*a^(-k))
     * Coefficients a[k] - for q = q0+(t*t/2)*sum(a[k]*v^k)
     * Coefficients e[k] - for exp(q)-1 = sum(e[k]*q^k)
     */
    const static double q1 = 0.04166669;
    const static double q2 = 0.02083148;
    const static double q3 = 0.00801191;
    const static double q4 = 0.00144121;
    const static double q5 = -7.388e-5;
    const static double q6 = 2.4511e-4;
    const static double q7 = 2.424e-4;

    const static double a1 = 0.3333333;
    const static double a2 = -0.250003;
    const static double a3 = 0.2000062;
    const static double a4 = -0.1662921;
    const static double a5 = 0.1423657;
    const static double a6 = -0.1367177;
    const static double a7 = 0.1233795;

    /* State variables [FIXME for threading!] :*/
    static double aa = 0.;
    static double aaa = 0.;
    static double s, s2, d;    /* no. 1 (step 1) */
    static double q0, b, si, c;/* no. 2 (step 4) */

    double e, p, q, r, t, u, v, w, x, ret_val;

    if (!R_FINITE(a) || !R_FINITE(scale) || a < 0.0 || scale <= 0.0) {
	if(scale == 0.) return 0.;
	ML_ERR_return_NAN;
    }

    if (a < 1.) { /* GS algorithm for parameters a < 1 */
	if(a == 0)
	    return 0.;
	e = 1.0 + exp_m1 * a;
	repeat {
	    p = e * unif_rand();
	    if (p >= 1.0) {
		x = -log((e - p) / a);
		if (exp_rand() >= (1.0 - a) * log(x))
		    break;
	    } else {
		x = exp(log(p) / a);
		if (exp_rand() >= x)
		    break;
	    }
	}
	return scale * x;
    }

    /* --- a >= 1 : GD algorithm --- */

    /* Step 1: Recalculations of s2, s, d if a has changed */
    if (a != aa) {
	aa = a;
	s2 = a - 0.5;
	s = sqrt(s2);
	d = sqrt32 - s * 12.0;
    }
    /* Step 2: t = standard normal deviate,
               x = (s,1/2) -normal deviate. */

    /* immediate acceptance (i) */
    t = norm_rand();
    x = s + 0.5 * t;
    ret_val = x * x;
    if (t >= 0.0)
	return scale * ret_val;

    /* Step 3: u = 0,1 - uniform sample. squeeze acceptance (s) */
    u = unif_rand();
    if (d * u <= t * t * t)
	return scale * ret_val;

    /* Step 4: recalculations of q0, b, si, c if necessary */

    if (a != aaa) {
	aaa = a;
	r = 1.0 / a;
	q0 = ((((((q7 * r + q6) * r + q5) * r + q4) * r + q3) * r
	       + q2) * r + q1) * r;

	/* Approximation depending on size of parameter a */
	/* The constants in the expressions for b, si and c */
	/* were established by numerical experiments */

	if (a <= 3.686) {
	    b = 0.463 + s + 0.178 * s2;
	    si = 1.235;
	    c = 0.195 / s - 0.079 + 0.16 * s;
	} else if (a <= 13.022) {
	    b = 1.654 + 0.0076 * s2;
	    si = 1.68 / s + 0.275;
	    c = 0.062 / s + 0.024;
	} else {
	    b = 1.77;
	    si = 0.75;
	    c = 0.1515 / s;
	}
    }
    /* Step 5: no quotient test if x not positive */

    if (x > 0.0) {
	/* Step 6: calculation of v and quotient q */
	v = t / (s + s);
	if (fabs(v) <= 0.25)
	    q = q0 + 0.5 * t * t * ((((((a7 * v + a6) * v + a5) * v + a4) * v
				      + a3) * v + a2) * v + a1) * v;
	else
	    q = q0 - s * t + 0.25 * t * t + (s2 + s2) * log(1.0 + v);


	/* Step 7: quotient acceptance (q) */
	if (log(1.0 - u) <= q)
	    return scale * ret_val;
    }

    repeat {
		
	/* Step 8: e = standard exponential deviate
	 *	u =  0,1 -uniform deviate
	 *	t = (b,si)-double exponential (laplace) sample */
	e = exp_rand();
	u = unif_rand();
	u = u + u - 1.0;
	if (u < 0.0)
	    t = b - si * e;
	else
	    t = b + si * e;
	/* Step	 9:  rejection if t < tau(1) = -0.71874483771719 */
	if (t >= -0.71874483771719) {
	    /* Step 10:	 calculation of v and quotient q */
	    v = t / (s + s);
	    if (fabs(v) <= 0.25)
		q = q0 + 0.5 * t * t *
		    ((((((a7 * v + a6) * v + a5) * v + a4) * v + a3) * v
		      + a2) * v + a1) * v;
	    else
		q = q0 - s * t + 0.25 * t * t + (s2 + s2) * log(1.0 + v);
	    /* Step 11:	 hat acceptance (h) */
	    /* (if q not positive go to step 8) */
	    if (q > 0.0) {
		w = expm1(q);
		/*  ^^^^^ original code had approximation with rel.err < 2e-7 */
		/* if t is rejected sample again at step 8 */
		if (c * fabs(u) <= w * exp(e - 0.5 * t * t))
		    break;
	    }
	}
    } /* repeat .. until  `t' is accepted */
    x = s + 0.5 * t;
    return scale * x * x;
}

#if 0

int main(void)
 {
    int j;

    // you can seed with any uint32, but the best are odds in 0..(2^32 - 1)

    set_seed(123);

    // print the first 2,002 random numbers seven to a line as an example

    for(j=0; j<6; j++)
        printf(" %10lf", unif_rand());
    printf("\n");
        
    set_seed(123);
    for(j=0; j<6; j++)
        printf(" %.10lf", unif_rand());

	printf("\n");
    return(EXIT_SUCCESS);
 }


#endif
