## This Makefile use the Makefile provided by the package pkgmaker
## Copyright (C) 2012 Renaud Gaujoux

ifneq (${R_HOME},)
RSCRIPT=$(R_HOME)/bin/Rscript
else 
RSCRIPT=`echo R`script
endif

.PHONY: src/RJwrapper.Rnw

MK=$(shell $(RSCRIPT) --vanilla -e "pkgmaker::vignetteMakefile('RcppOctave', skip='EddelbuettelGaujoux.Rnw')")

include $(MK)

