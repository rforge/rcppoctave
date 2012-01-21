## This Makefile was borrowed from the RcppGSL package
## Copyright (C) 2011 Romain François and Edd Eddelbuettel
## It was modifed by Renaud Gaujoux to make it more generic and to generate the 
## fake vignettes on the fly.
## Copyright (C) 2011 Renaud Gaujoux

## There is an old bug in texidvi that makes it not swallow the ~
## marker used to denote whitespace. This is actually due to fixing
## another bug whereby you could not run texidvi on directory names
## containing a tilde (as we happen to do for Debian builds of R
## alpha/beta/rc releases). The 'tilde' bug will go away as it
## reportedly has been squashed upstream but I am still bitten by it
## on Ubuntu so for now Dirk will insist on pdflatex and this helps.
AUTHOR_USER=renaud
R_PACKAGE=RcppOctave

##---------------------------------------------------------------------
## Everything below this should be generic and work for any package provided that
## they have the following directory inst/doc setting:
## - inst/doc/vignettes: contains the Rnw files for normal vignettes
## - inst/doc/unitTests: contains an R file <PKGNAME>-unitTests.R and an .Rnw file
## that run the unit tests and generate the unit test vignette respectively
##---------------------------------------------------------------------

whoami=$(shell whoami)
RNW_SRCS = $(notdir $(wildcard vignettes/*.Rnw))
PDF_OBJS=$(RNW_SRCS:.Rnw=.pdf)

# add unit tests if necessary
ifneq ("$(wildcard unitTests)","")
PDF_OBJS:=$(R_PACKAGE)-unitTests.pdf $(PDF_OBJS)
endif

ifneq (,$(findstring $(AUTHOR_USER),$(whoami)))
LOCAL_MODE=1
endif

ifeq (${R_HOME},)
NOT_CHECKING=1
TMP_INSTALL_DIR := $(shell mktemp -d)
export R_LIBS=$(shell pwd)/../../../lib
endif

# Define command for temporary installation (used when make is directly called,
# i.e. when not in check/build/INSTALL)
ifdef NOT_CHECKING
define do_install
	# Installing the package in temporary library directory $(TMP_INSTALL_DIR)
	-$(RPROG) CMD INSTALL -l "$(TMP_INSTALL_DIR)" ../../.
	@if test ! -d "$(TMP_INSTALL_DIR)/$(R_PACKAGE)"; then \
	echo "ERROR: Temporary installation failed."; \
	echo "# Removing temporary library directory $(TMP_INSTALL_DIR)"; \
	rm -rf $(TMP_INSTALL_DIR); \
	exit 1; else \
	echo "# Package successfully installed"; fi;
	@echo "# Adding temporary library '$(TMP_INSTALL_DIR)' to R_LIBS"
	$(eval R_LIBS := $(TMP_INSTALL_DIR):$(R_LIBS))
endef
else
define do_install
endef	
endif

all: init $(PDF_OBJS) do_clean
	@echo "# All vignettes are uptodate"

init:
	# Generating vignettes for package $(R_PACKAGE)
	# Detected vignettes: $(PDF_OBJS)
ifdef LOCAL_MODE
	# Mode: Local Development
else
	# Mode: Production
endif

clean:
	rm -fr *.tex *.bbl *.blg *.aux *.out *.log 

clean-all: clean
	rm -fr *.pdf *.Rnw unitTests-results

setvars:
ifeq (${R_HOME},)
R_HOME=	$(shell R RHOME)
endif
RPROG=	$(R_HOME)/bin/R
RSCRIPT=$(R_HOME)/bin/Rscript

do_clean:
	@-rm -rf $(TMP_INSTALL_DIR);

# Generate .pdf from .tex
%.pdf: %.tex
	$(eval VIGNETTE_BASENAME := $(shell basename $@ .pdf))
	# Generating pdf $@ from $<
ifdef LOCAL_MODE
	pdflatex $(VIGNETTE_BASENAME)
else
	$(RSCRIPT) -e "tools::texi2dvi( '$(VIGNETTE_BASENAME).tex', pdf = TRUE, clean = FALSE )"
endif
	-bibtex $(VIGNETTE_BASENAME)
ifdef LOCAL_MODE
	pdflatex $(VIGNETTE_BASENAME)
	pdflatex $(VIGNETTE_BASENAME)
else
	$(RSCRIPT) -e "tools::texi2dvi( '$(VIGNETTE_BASENAME).tex', pdf = TRUE, clean = TRUE )"
	# copy generated pdf file into package check directory so that it
	# is not lost after all the hard generation work!
	# R CMD check re-builds the vignette in <CHECKDIR>/vign_test/<PKG>/inst/doc
	# so we copy the pdf up 4 levels into <CHECKDIR>
	cp $(VIGNETTE_BASENAME).pdf ../../../../
endif
	# Create fake vignette file ./$(VIGNETTE_BASENAME).Rnw
	@echo "\\documentclass[10pt]{article}\n%\\VignetteIndexEntry{$(VIGNETTE_BASENAME)}\n\
\usepackage{url}\n\usepackage[colorlinks]{hyperref}\n\n\\\\begin{document}\n\end{document}\n" > $(VIGNETTE_BASENAME).Rnw
	# Remove temporary LaTeX files (but keep the .tex)
	rm -fr $(VIGNETTE_BASENAME).toc $(VIGNETTE_BASENAME).log \
	$(VIGNETTE_BASENAME).bbl $(VIGNETTE_BASENAME).blg \
	$(VIGNETTE_BASENAME).aux $(VIGNETTE_BASENAME).out

# Generate a .tex file for a single vignette: generate from the 
# actual .Rnw file, stored in the subdirectory 'vignettes'
%.tex: vignettes/%.Rnw
	$(do_install)	
	# Generating vignette $@ from $<
	# Using R_LIBS: $(R_LIBS)
	$(RSCRIPT) --vanilla -e "require(highlight); driver <- HighlightWeaveLatex(boxes = TRUE); Sweave( '$<', driver = driver ); "

ifneq ("$(wildcard unitTests)","")
%-unitTests.pdf: unitTests/doUnit.R
	$(do_install)
	$(eval VIGNETTE_BASENAME := $(shell basename $@ .pdf))
	# Generating vignette for unit tests: $@
	# Using R_LIBS: $(R_LIBS)
	rm -fr unitTests-results/*
	$(RSCRIPT) --vanilla -e "source('unitTests/doUnit.R'); doUnit('$(R_PACKAGE)', '$(R_PACKAGE)-unitTests.Rnw');"
	$(RPROG) CMD Sweave $(R_PACKAGE)-unitTests.Rnw
	$(RSCRIPT) --vanilla -e "tools::texi2dvi( '$(R_PACKAGE)-unitTests.tex', pdf = TRUE, clean = TRUE )"
	rm -fr $(R_PACKAGE)-unitTests.tex
endif

#	$(eval VIGNETTE_BASENAME := $(shell basename $< .Rnw))
#	$(eval R_LIBS := $(TMP_INSTALL_DIR):$(R_LIBS))
#	@echo $(R_LIBS)
#	@echo "# Generating vignette $(VIGNETTE_BASENAME) from $<"
#	$(RSCRIPT) --vanilla -e "require(highlight); driver <- HighlightWeaveLatex(boxes = TRUE); Sweave( '$<', driver = driver ); "
