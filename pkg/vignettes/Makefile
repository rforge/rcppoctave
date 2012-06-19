## This Makefile use the Makefile provided by the package pkgmaker
## Copyright (C) 2012 Renaud Gaujoux

ifneq (${R_HOME},)
RPROG=$(R_HOME)/bin/R
RSCRIPT=$(R_HOME)/bin/Rscript
else 
RPROG=R
RSCRIPT=Rscript
endif

MK=$(shell $(RSCRIPT) --vanilla -e "pkgmaker::vignetteMakefile('renaud', 'RcppOctave')")

include $(MK)
