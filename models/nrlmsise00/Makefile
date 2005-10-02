L=	../../lib/
MLIST=	nrlmsise00.f
N=	libmsis

A=	.a
S=	.so
ML=	$L$N

sys=$(shell uname -s)
ifeq ($(sys),SunOS)
FFLAGS = -xO3 -KPIC -dalign -mt -silent -e
FC=f90
LDFLAGS= -dy -G -h $N$S -z text
else
ifeq ($(FC),f95) 
FC=gfortran
FFLAGS = -O3 -fPIC -c -static
else
FC=g77
FFLAGS = -O3 -fPIC -c -static -finit-local-zero
endif
LDFLAGS= -shared
endif

libmsis:
	@$(MAKE) clean
	$(COMPILE.f) $(MLIST)
	$(RM) $(ML)$A $(ML)$S
	$(LD) -o $(ML)$S $(LDFLAGS) *.o
	$(AR) cr $(ML)$A *.o
	@$(MAKE) clean

clean:
	$(RM) *.o
