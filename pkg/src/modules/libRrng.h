#ifndef __LIB_R_RNG_H
#define __LIB_R_RNG_H

/* From R_ext/Random.h */
typedef enum {
    WICHMANN_HILL,
    MARSAGLIA_MULTICARRY,
    SUPER_DUPER,
    MERSENNE_TWISTER,
    KNUTH_TAOCP,
    USER_UNIF,
    KNUTH_TAOCP2,
    LECUYER_CMRG
} RNGtype;

/* Different kinds of "N(0,1)" generators :*/
typedef enum {
    BUGGY_KINDERMAN_RAMAGE,
    AHRENS_DIETER,
    BOX_MULLER,
    USER_NORM,
    INVERSION,
    KINDERMAN_RAMAGE
} N01type;

#ifdef __cplusplus
extern "C" {
#endif

void GetRNGstate(void);
void PutRNGstate(void);

double unif_rand(void);
/* These are also defined in Rmath.h */
double norm_rand(void);
double exp_rand(void);

typedef unsigned int Int32;

// Export other functions for setting/getting the RNG state
void do_setseed (Int32 seed, RNGtype* skind, N01type* nkind);
int do_getseed(int* seed);

// Export other stat functions
double rgamma(double a, double scale);

#ifdef __cplusplus
}
#endif

#define MAX_SEED_LENGTH 625

#define IEEE_754 1
#define ISNAN(x)     (isnan(x)!=0)
#define ML_POSINF	1.0/0.0
#define ML_NEGINF	-1.0/0.0
#define ML_NAN 0.0/0.0

#define ML_ERR_return_NAN { return ML_NAN; }

/* Use 0.5 - p + 0.5 to perhaps gain 1 bit of accuracy */
#define R_D_Lval(p)	(lower_tail ? (p) : (0.5 - (p) + 0.5))	/*  p  */
#define R_D_Cval(p)	(lower_tail ? (0.5 - (p) + 0.5) : (p))	/*  1 - p */

#define R_DT_qIv(p)	(log_p ? (lower_tail ? exp(p) : - expm1(p)) \
			       : R_D_Lval(p))
#define R_DT_CIv(p)	(log_p ? (lower_tail ? -expm1(p) : exp(p)) \
			       : R_D_Cval(p))

#define R_Q_P01_boundaries(p, _LEFT_, _RIGHT_)		\
    if(p < 0 || p > 1)				\
	    return ML_NAN;				\
	if(p == 0)					\
	    return lower_tail ? _LEFT_ : _RIGHT_;	\
	if(p == 1)					\
	    return lower_tail ? _RIGHT_ : _LEFT_;	\

double qnorm5(double p, double mu, double sigma, int lower_tail, int log_p)
{
    double p_, q, r, val;

#ifdef IEEE_754
    if (ISNAN(p) || ISNAN(mu) || ISNAN(sigma))
	return p + mu + sigma;
#endif
    R_Q_P01_boundaries(p, ML_NEGINF, ML_POSINF);

    if(sigma  < 0)	return ML_NAN;
    if(sigma == 0)	return mu;

    p_ = R_DT_qIv(p);/* real lower_tail prob. p */
    q = p_ - 0.5;

#ifdef DEBUG_qnorm
    REprintf("qnorm(p=%10.7g, m=%g, s=%g, l.t.= %d, log= %d): q = %g\n",
	     p,mu,sigma, lower_tail, log_p, q);
#endif


/*-- use AS 241 --- */
/* double ppnd16_(double *p, long *ifault)*/
/*      ALGORITHM AS241  APPL. STATIST. (1988) VOL. 37, NO. 3

        Produces the normal deviate Z corresponding to a given lower
        tail area of P; Z is accurate to about 1 part in 10**16.

        (original fortran code used PARAMETER(..) for the coefficients
         and provided hash codes for checking them...)
*/
    if (fabs(q) <= .425) {/* 0.075 <= p <= 0.925 */
        r = .180625 - q * q;
	val =
            q * (((((((r * 2509.0809287301226727 +
                       33430.575583588128105) * r + 67265.770927008700853) * r +
                     45921.953931549871457) * r + 13731.693765509461125) * r +
                   1971.5909503065514427) * r + 133.14166789178437745) * r +
                 3.387132872796366608)
            / (((((((r * 5226.495278852854561 +
                     28729.085735721942674) * r + 39307.89580009271061) * r +
                   21213.794301586595867) * r + 5394.1960214247511077) * r +
                 687.1870074920579083) * r + 42.313330701600911252) * r + 1.);
    }
    else { /* closer than 0.075 from {0,1} boundary */

	/* r = min(p, 1-p) < 0.075 */
	if (q > 0)
	    r = R_DT_CIv(p);/* 1-p */
	else
	    r = p_;/* = R_DT_Iv(p) ^=  p */

	r = sqrt(- ((log_p &&
		     ((lower_tail && q <= 0) || (!lower_tail && q > 0))) ?
		    p : /* else */ log(r)));
        /* r = sqrt(-log(r))  <==>  min(p, 1-p) = exp( - r^2 ) */
#ifdef DEBUG_qnorm
	REprintf("\t close to 0 or 1: r = %7g\n", r);
#endif

        if (r <= 5.) { /* <==> min(p,1-p) >= exp(-25) ~= 1.3888e-11 */
            r += -1.6;
            val = (((((((r * 7.7454501427834140764e-4 +
                       .0227238449892691845833) * r + .24178072517745061177) *
                     r + 1.27045825245236838258) * r +
                    3.64784832476320460504) * r + 5.7694972214606914055) *
                  r + 4.6303378461565452959) * r +
                 1.42343711074968357734)
                / (((((((r *
                         1.05075007164441684324e-9 + 5.475938084995344946e-4) *
                        r + .0151986665636164571966) * r +
                       .14810397642748007459) * r + .68976733498510000455) *
                     r + 1.6763848301838038494) * r +
                    2.05319162663775882187) * r + 1.);
        }
        else { /* very close to  0 or 1 */
            r += -5.;
            val = (((((((r * 2.01033439929228813265e-7 +
                       2.71155556874348757815e-5) * r +
                      .0012426609473880784386) * r + .026532189526576123093) *
                    r + .29656057182850489123) * r +
                   1.7848265399172913358) * r + 5.4637849111641143699) *
                 r + 6.6579046435011037772)
                / (((((((r *
                         2.04426310338993978564e-15 + 1.4215117583164458887e-7)*
                        r + 1.8463183175100546818e-5) * r +
                       7.868691311456132591e-4) * r + .0148753612908506148525)
                     * r + .13692988092273580531) * r +
                    .59983220655588793769) * r + 1.);
        }

	if(q < 0.0)
	    val = -val;
        /* return (q >= 0.)? r : -r ;*/
    }
    return mu + sigma * val;
}


/* From datetime.c */
/* needed on Windows to avoid redefinition of tzname as _tzname */
#define _NO_OLDNAMES
#include <time.h>
#undef _NO_OLDNAMES

#ifdef Win32
#define gmtime R_gmtime
#define localtime R_localtime
#define mktime R_mktime
extern struct tm*  gmtime (const time_t*);
extern struct tm*  localtime (const time_t*);
extern time_t mktime (struct tm*);
#else
#include "sys/time.h"
#endif

#ifdef HAVE_UNISTD_H
#include <unistd.h> /* for getpid */
#endif

unsigned int TimeToSeed(void)
{
    unsigned int seed, pid = getpid();
#if defined(HAVE_CLOCK_GETTIME) && defined(CLOCK_REALTIME)
    {
	struct timespec tp;
	clock_gettime(CLOCK_REALTIME, &tp);
	seed = ((uint64_t) tp.tv_nsec << 16) ^ tp.tv_sec;
    }
#elif defined(HAVE_GETTIMEOFDAY)
    {
	struct timeval tv;
	gettimeofday (&tv, NULL);
	seed = ((uint64_t) tv.tv_usec << 16) ^ tv.tv_sec;
    }
#else
    /* C89, so must work */
    seed = (Int32) time(NULL);
#endif
    seed ^= (pid <<16);
    return seed;
}

/* Redefinitions from errors.c */
#define warning printf
#define error printf

/* From Defn.h */
#define _(String) (String)

#endif
