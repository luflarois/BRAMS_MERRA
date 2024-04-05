REV=1.0
EXE=../bin/correlation.$(REV)

SRCINCLUDES=-I$(SRC_PATH)
### IMPORTANT ###
#*** dont forget to use 'make clean' after changing any of these options ***

FC           =gfortran
FC_OPTS      = -O3 -ffree-form -ffree-line-length-none -fbacktrace -fbounds-check
FLOADER      =gfortran

