M=	$(PWD)/../lib
sys=$(shell uname -s)
ifndef MEX
MEX:=	mex
endif
ifndef MEXD
MEXD:=	mex7
endif
ifndef MATLAB
MATLAB=$(subst MATLAB=,,$(shell matlab -e | grep MATLAB= ))
endif
ifeq ($(sys),SunOS)
#Solaris:
ifeq ($(bit),64)
#CFLAGS=-fast -mt -KPIC -DANSI_C -I$(MATLAB)/extern/include -xarch=v9a
CFLAGS=-fast -mt -KPIC -DANSI_C -I$(MATLAB)/extern/include -m64 -xarch=sparcvis
CC=	cc -m64 -xarch=sparcvis
M=	$(PWD)/../lib/sparcv9
MEXOPTS= CC="cc -m64 -xarch=sparcvis" FC="f90  -m64 -xarch=sparcvis" -f $(MATLAB)/bin/f90opts.sh
else
CFLAGS=-fast -mt -KPIC -DANSI_C -I$(MATLAB)/extern/include
MEXOPTS=-f $(MATLAB)/bin/f90opts.sh
endif
LDFLAGS= -dy -G -h libguisdap.so -z text
FC=f95
else
#Linux
CFLAGS=-O3 -fPIC -ansi -march=native -D_GNU_SOURCE -pthread -DANSI_C -I$(MATLAB)/extern/include
PLFLAGS= -Wall -g -O4 -march=native -shared -fPIC -lfftw3f

MEXOPTS=FC=gfortran-5 GCC=gcc-5 -compatibleArrayDims
CC=gcc-5
LDFLAGS= -shared
endif

th=$(shell if [ -f /usr/include/pthread.h ]; then echo 1;else echo 0;fi)

#The rest
IRILIB=	-L$M -liri
MSISLIB=	-L$M -lmsis
GUPLIB=	-L$M -lguisdap
ifeq ($(th),1)
CFLAGS:=	$(CFLAGS) -DGUPTHREAD
GUPLIB:=	$(GUPLIB) -lpthread
endif


all: iri msis geomag lib mex install

iri:
	$(MEX) $(MEXOPTS) iri.F $(IRILIB)

geomag:
	$(MEX) $(MEXOPTS) geomag.F $(IRILIB)

msis:
	$(MEX) $(MEXOPTS) msis.F $(MSISLIB)

lib:
	$(CC) plwin.c -o $M/plwin.so $(PLFLAGS)
	cp plwin.h $M
	$(COMPILE.c) mrqmndiag_worker.c dirthe_worker.c GULIPS_addm_worker.c GULIPS_cov_worker.c GULIPS_invR_worker.c GULIPS_mul_worker.c spec_worker.c transf_worker.c addr_covar_worker.c
	$(LD) -o $M/libguisdap.so $(LDFLAGS) *.o
	$(AR) cr $M/libguisdap.a *.o
	$(RM) *.o

mex:
	$(MEX) $(MEXOPTS) -DANSI_C mrqmndiag.c $(GUPLIB)
	$(MEX) $(MEXOPTS) -DANSI_C dirthe.c $(GUPLIB)
	$(MEX) $(MEXOPTS) -DANSI_C spec.c $(GUPLIB)
	$(MEX) $(MEXOPTS) -DANSI_C transf.c $(GUPLIB)
	$(MEX) $(MEXOPTS) -DANSI_C addr_covar.c $(GUPLIB)

install:
	mv *.mex* ../${MEXD}
