sys=$(shell uname -s)
ifndef MEX
MEX:=	mex
endif
ifndef MEXD
MEXD:=	mex7
endif
ifeq ($(sys),SunOS)
#Solaris:
ifndef MATLAB
MATLAB:=	/opt/matlab7
endif
CFLAGS=-fast -mt -KPIC -DANSI_C -I$(MATLAB)/extern/include
MEXOPTS=-f $(MATLAB)/bin/f90opts.sh
LDFLAGS= -dy -G -h libguisdap.so -z text
else
#Linux
CFLAGS=-O3 -fPIC -ansi -D_GNU_SOURCE -pthread -DANSI_C -I$(MATLAB)/extern/include
MEXOPTS=
LDFLAGS= -shared
endif

th=$(shell if [ -f /usr/include/pthread.h ]; then echo 1;else echo 0;fi)

#The rest
M=	$(PWD)/../lib
IRILIB=	-L$M -liri
MSISLIB=	-L$M -lmsis
GUPLIB=	-L$M -lguisdap
ifeq ($(th),1)
CFLAGS:=	$(CFLAGS) -DGUPTHREAD
GUPLIB:=	$(GUPLIB) -lpthread
endif

all: iri msis lib mex install

iri:
	$(MEX) $(MEXOPTS) iri.f $(IRILIB)

geomag:
	$(MEX) $(MEXOPTS) geomag.f $(IRILIB)

msis:
	$(MEX) $(MEXOPTS) msis.f $(MSISLIB)

lib:
	$(COMPILE.c) mrqmndiag_worker.c dirthe_worker.c GULIPS_addm_worker.c GULIPS_cov_worker.c GULIPS_invR_worker.c GULIPS_mul_worker.c spec_worker.c transf_worker.c addr_covar_worker.c
	$(LD) -o $M/libguisdap.so $(LDFLAGS) *.o
	$(AR) cr $M/libguisdap.a *.o
	$(RM) *.o

mex:
	$(MEX) -DANSI_C mrqmndiag.c $(GUPLIB)
	$(MEX) -DANSI_C dirthe.c $(GUPLIB)
	$(MEX) -DANSI_C spec.c $(GUPLIB)
	$(MEX) -DANSI_C transf.c $(GUPLIB)
	$(MEX) -DANSI_C addr_covar.c $(GUPLIB)

install:
	mv *.mex* ../${MEXD}
