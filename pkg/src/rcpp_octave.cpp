/* Copyright (c) 1996-2009 by John W. Eaton.
   Copyright (c) 2003 by Paul Kienzle.
   Copyright (c) 2010 by Albert Graef.
   Copyright (c) 2011 by Renaud Gaujoux.

   This file is part of the R package RcppOctave.

   RcppOctave is free software: you can redistribute it and/or modify it
   under the terms of the GNU General Public License as published by the Free
   Software Foundation, either version 3 of the License, or (at your option)
   any later version.

   RcppOctave is distributed in the hope that it will be useful, but WITHOUT
   ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or
   FITNESS FOR A PARTICULAR PURPOSE.

   Please see the accompanying COPYING file for the precise license terms. The
   GPL are also be read online at http://www.gnu.org/licenses/.
*/

#include "rcpp_octave.h"

// R includes
#include <R.h>
#include <Rinternals.h>
#include <R_ext/Rdynload.h>

// Octave includes
#include <octave/config.h>
#include <octave/input.h>

#include <octave/pt-all.h>
#include <iostream>
#include <octave/symtab.h>
#include <octave/parse.h>
#include <octave/unwind-prot.h>
#include <octave/toplev.h>
#include <octave/error.h>
#include <octave/quit.h>
#include <octave/variables.h>
#include <octave/sighandlers.h>
#include <octave/sysdep.h>
#include <octave/str-vec.h>

// STD includes
#include <string.h>
using namespace std;

static bool OCTAVE_INITIALIZED = false;

/**
 * Global variable to hold verbosity status.
 */
bool RCPP_OCTAVE_VERBOSE = false;


/* Octave embedded interpreter.
 * This is a mix between embedded versions of Octave from:
 * 1) pure-octave by Albert Graef:
 * (source: http://docs.pure-lang.googlecode.com/hg/pure-octave.html) 
 * It is described as a "heavily hacked version of octave_embed by 
 * Paul Kienzle (http://wiki.octave.org/wiki.pl?OctaveEmbedded) which 
 * in turn is based on Octave's toplevel".
 *
 * 2) SHOGUN
 * (source: http://shogun-toolbox.org/trac/browser/src/octave/)
 */

octave_value octave_feval(const string& fname, const octave_value_list& args, int nres=-1, const std::vector<string>* output_names=NULL);
inline octave_value octave_feval(const string& fname, const octave_value_list& args, const std::vector<string>& output_names){
	return octave_feval(fname, args, output_names.size(), &output_names);
}

/**
 * Toggle verbosity for RcppOctave calls.
 */
SEXP octave_verbose(SEXP value){
	bool bval = Rcpp::as<bool>(value);
	bool res = RCPP_OCTAVE_VERBOSE;
	RCPP_OCTAVE_VERBOSE = bval;
	return( Rcpp::wrap(res) );
}

bool octave_session(bool start=true){

	VERBOSE_LOG("Octave interpreter: %s\n", OCTAVE_INITIALIZED ? "on" : "off");
	if( start && !OCTAVE_INITIALIZED ){
		VERBOSE_LOG("Starting Octave interpreter\n");
		// instantiate the Octave interpreter
		int narg = 4;
		string_vector argv(narg);
		argv(0) = "Roctave";
		argv(1) = "--quiet";
		// Try avoid corruption of R console with Octave console outputs
		// [suggested by Albert Graef]
		argv(2) = "--no-line-editing";
		argv(3) = "--no-history";
		if( !octave_main(narg, argv.c_str_vec(), true /*embedded*/) )
			error("Failed to start Octave interpreter");

		OCTAVE_INITIALIZED = true;
		bind_internal_variable("crash_dumps_octave_core", false);

	}
	else if( !start && OCTAVE_INITIALIZED ){
		if( RCPP_OCTAVE_VERBOSE )
			Rprintf("Terminating Octave interpreter\n");
		// terminate interpreter
		do_octave_atexit();
		OCTAVE_INITIALIZED = false;
	}
	VERBOSE_LOG("Octave interpreter: %s\n", OCTAVE_INITIALIZED ? "on" : "off");

	return true;
}

SEXP octave_start(SEXP verbose){

	if( !isNull(verbose) )
		RCPP_OCTAVE_VERBOSE = Rcpp::as<bool>(verbose);
	return Rcpp::wrap(octave_session(true));
}

SEXP octave_end(){
	return Rcpp::wrap(octave_session(false));
}

void R_init_RcppOctave(DllInfo *info)
{
	/* Register routines, allocate resources. */
	octave_start(R_NilValue);
}

void R_unload_RcppOctave(DllInfo *info)
{
	/* Release resources. */
	octave_end();
}

/**
 * Recover from an exception.
 * It restores some Octave static variables into a no-error state.
 */
extern void recover_from_exception(void)
{
#if OCTAVE_API_VERSION_NUMBER >= 45
#else
  // This isn't supported in the latest Octave versions. We simply leave this
  // disabled for now, which means that you'll have to use 'unwind_protect'
  // explicitly in your Octave code in order to handle Octave exceptions.
  // XXXFIXME: This might leak memory in some cases??
  unwind_protect::run_all ();
#endif

  can_interrupt = true;
  octave_interrupt_immediately = 0;
  octave_interrupt_state = 0;
  octave_signal_caught = 0;
#if OCTAVE_API_VERSION_NUMBER >= 37
  octave_exception_state = octave_no_exception;
#else
  octave_allocation_error = 0;
#endif
  octave_restore_signal_mask ();
  octave_catch_interrupts ();
}

typedef std::vector<string> std_vector;
SEXP octave_feval(SEXP fname, SEXP args, SEXP output, SEXP unlist=R_NilValue){

	using namespace Rcpp;
	BEGIN_RCPP

	bool do_unlist = isNull(unlist) ? true : as<bool>(unlist);

	octave_value out;
	if( TYPEOF(output) == STRSXP ){
		out = octave_feval(Rcpp::as<string>(fname)
						, Rcpp::as<octave_value_list>(args)
						, Rcpp::as<std_vector>(output));
	}else{
		out = octave_feval(Rcpp::as<string>(fname)
						, Rcpp::as<octave_value_list>(args)
						, Rcpp::as<int>(output));
	}

	// unlist result if requested
	if( do_unlist ){
		if( out.is_cs_list() ){ // unnamed list

			octave_value_list ol = out.list_value();
			if ( ol.length() == 1){
				VERBOSE_LOG("octave_feval - Unlisting unnamed output\n");
				return Rcpp::wrap(ol(0));
			}

		}else if( out.is_map() ){ // named list

			octave_map m = out.map_value();
			if (m.nfields() == 1){
				const string_vector& keys = m.fieldnames();
				VERBOSE_LOG("octave_feval - Unlisting named output '%s'\n", keys[0].c_str());
				return Rcpp::wrap(m.contents(keys[0])(0));
			}

		}else{
			// throw an R error
			std::ostringstream err;
			err << R_PACKAGE_NAME" - could not unlist the result of function `"
					<< Rcpp::as<string>(fname).c_str()
					<< "`: unsupported returned value type [" << out.type_name().c_str() << "].";
			error(err.str().c_str());
		}
	}

	return Rcpp::wrap(out);

	END_RCPP
}

int getOutnames(const string& fname, std::vector<string>& onames){

	// Lookup this name in the symbol tables
	octave_value fdef = symbol_table::find(fname);
	onames.clear();

	VERBOSE_LOG("octave_feval - Check if `%s` has an internal symbol table ... ", fname.c_str());
	if( !fdef.is_user_function() ){
		VERBOSE_LOG("NO\n");
		return -1;
	}
	VERBOSE_LOG("YES\n");

	VERBOSE_LOG("octave_feval - Check if output names are detectable ... ");
	octave_user_function *f = (octave_user_function*)fdef.function_value();
	if( f == NULL ){
		VERBOSE_LOG("NO\n");
		return -1;
	}
	tree_parameter_list *rl = f->return_list ();
	if( rl == NULL ){
		VERBOSE_LOG("NO\n");
		return -1;
	}

	int nres = rl->length();
	VERBOSE_LOG("YES [%i]\n", nres);

	// limit number of output variables
	if( nres == 0 ) return 0;

	VERBOSE_LOG("octave_feval - Output name(s):");
	onames.reserve(nres);
	tree_parameter_list::iterator rlp = rl->begin();
	for(int i=0; rlp != rl->end(); rlp++)
	{
		tree_identifier *rid = (*rlp)->ident();
		if (rid)
		{
			// retrieve the variable names
			onames.push_back(rid->name());
			VERBOSE_LOG(" '%s'", onames[i].c_str());
			++i;
		}
	}
	VERBOSE_LOG("\n");

	return nres;
}

octave_value octave_feval(const string& fname, const octave_value_list& args, int nres, const std::vector<string>* output_names) {

	VERBOSE_LOG("octave_feval - Calling Octave function `%s` with %i argument(s)\n", fname.c_str(), args.length());

	// update time stamps to reload modified functions if necessary
	Vlast_prompt_time.stamp();

	octave_save_signal_mask();
	if (octave_set_current_context) {
#if defined (USE_EXCEPTIONS_FOR_INTERRUPTS)
		panic_impossible()
#else
	#if OCTAVE_API_VERSION_NUMBER >= 45
	#else
		//XXX FIXME XXX
		unwind_protect::run_all ();
	#endif
		raw_mode(0);
		Rprintf("\n");
		octave_restore_signal_mask();
#endif
	}

	can_interrupt = true;
	octave_catch_interrupts();
	octave_initialized = true;

	try {

#if OCTAVE_API_VERSION_NUMBER >= 37
#else
		symbol_table* old_symb_tab=curr_sym_tab;
		curr_sym_tab = top_level_sym_tab;
#endif
		reset_error_handler();

		// extract the output names
		std::vector<string> autonames;
		if( output_names != NULL )
			nres = output_names->size();
		else{
			int n_output = getOutnames(fname, autonames);
			// set default (max) number of result if necessary and possible
			VERBOSE_LOG("octave_feval - Requested number of return value(s): ");
			if( nres < 0 ){
				nres = n_output == -1 ? 1 : n_output;
				VERBOSE_LOG("%i [default]\n", nres);
			}else if( n_output > nres ){ // the user requested less than the maximum output
				VERBOSE_LOG("%i [truncate]\n", nres);
				autonames.clear();
			}else{
				VERBOSE_LOG("%i\n", nres);
				autonames.clear();
			}
			output_names = &autonames;
		}
		const std::vector<string>& onames = *output_names;

		VERBOSE_LOG("octave_feval - Calling feval now ... ");
		octave_value_list out = feval(fname, args, nres);
		if ( !error_state ){
			VERBOSE_LOG("OK\noctave_feval - Result has %i elements\n", out.length());

			// reduce the number of result elements if necessary
			if( nres < out.length() ){
				VERBOSE_LOG("octave_feval - Limit result to %i elements\n", nres);
				out = out.slice(0, nres);
			}

			// directly return the result if no output names are available
			if( onames.size() == 0 )
				return out;
			else{ // return the result as a map
				int n = onames.size();
				if( n != out.length() ){
					warning("Dropping names due to inconsistent lengths");
					return out;
				}

				VERBOSE_LOG("octave_feval - Set output name(s):");
				// add output names
				octave_map m(dim_vector(n, 1));
				for (int i=0; i<n; ++i){
					const string& s = onames[i];
					VERBOSE_LOG(" '%s'", s.c_str());
					if( s[0] == '\0' ){
						error(R_PACKAGE_NAME"octave_feval - empty output name.");
					}
					m.setfield(s, out(i));
				}
				VERBOSE_LOG("\n");
				return octave_value(m);
			}
		} else {
			VERBOSE_LOG("ERROR\n");
			VERBOSE_LOG(R_PACKAGE_NAME" - error in Octave function `%s`.\n", fname.c_str());
		}

#if OCTAVE_API_VERSION_NUMBER >= 37
#else
		curr_sym_tab = old_symb_tab;
#endif

	} catch	(octave_interrupt_exception){
		Rprintf(R_PACKAGE_NAME" - Caught Octave exception: interrupt\n");
		recover_from_exception();
		Rprintf("\n");
		//error_state = -2;
	}
	catch (std::bad_alloc)
	{
		Rprintf(R_PACKAGE_NAME" - Caught Octave exception: bad_alloc\n");
		recover_from_exception();
		Rprintf("\n");
		//error_state = -3;
	}

	octave_restore_signal_mask();
	octave_initialized = false;


	// throw an R error
	std::ostringstream err;
	err << R_PACKAGE_NAME" - error in Octave function `" << fname.c_str() << "`.";
	error(err.str().c_str());

	return octave_value_list();
}

#if 0
	int main(void){

		octave_feval(R_NilValue, R_NilValue, R_NilValue);

		octave_end();

		return 0;
	}
#endif
