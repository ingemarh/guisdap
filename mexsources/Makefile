sys=$(shell uname -s)
ifndef MATLAB
MATLAB:=	/opt/matlab6
endif
ifeq ($(sys),SunOS)
#Solaris:
CFLAGS=-fast -mt -KPIC -DANSI_C -I$(MATLAB)/extern/include
MEXOPTS=-f $(MATLAB)/bin/f90opts.sh
else
#Linux
CFLAGS=-O3 -fPIC -ansi -D_GNU_SOURCE -pthread -DANSI_C -I$(MATLAB)/extern/include
MEXOPTS=
endif

th=$(shell if [ -f /usr/include/pthread.h ]; then echo 1;else echo 0;fi)

#The rest
M=	$(PWD)/../lib
IRILIB=	-L$M -liri
MSISLIB=	-L$M -lmsis
GUPLIB=	-L$M -lguisdap
mex_Directory = ../mex6
ifeq ($(th),1)
CFLAGS:=	$(CFLAGS) -DGUPTHREAD
GUPLIB:=	$(GUPLIB) -lpthread
endif

all: iri msis lib mex6 install

iri:
	mex $(MEXOPTS) iri.f $(IRILIB)

msis:
	mex $(MEXOPTS) msis.f $(MSISLIB)

lib:
	$(COMPILE.c) mrqmndiag_worker.c dirthe_worker.c GULIPS_addm_worker.c GULIPS_cov_worker.c GULIPS_invR_worker.c GULIPS_mul_worker.c spec_worker.c transf_worker.c addr_covar_worker.c
	$(LD) -o $M/libguisdap.so -dy -G -h libguisdap.so -z text *.o
	$(AR) cr $M/libguisdap.a *.o
	$(RM) *.o

mex6:
	mex -DANSI_C mrqmndiag.c $(GUPLIB)
	mex -DANSI_C dirthe.c $(GUPLIB)
	mex -DANSI_C spec.c $(GUPLIB)
	mex -DANSI_C transf.c $(GUPLIB)
	mex -DANSI_C addr_covar.c $(GUPLIB)

install:
	mv *.mex* ${mex_Directory}
