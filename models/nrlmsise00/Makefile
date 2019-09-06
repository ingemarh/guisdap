L=	../../lib/
MLIST=	nrlmsise00.f
N=	libmsis
GFORT=	gfortran-6

A=	.a
S=	.so
ML=	$L$N

sys=$(shell uname -s)
ifeq ($(sys),SunOS)
ifeq ($(bit),64)
ML=	$Lsparcv9/$N
#FFLAGS = -xO3 -KPIC -dalign -mt -silent -e -xarch=v9a
FFLAGS = -xO3 -KPIC -dalign -mt -silent -e -m64 -xarch=sparcvis
else
FFLAGS = -xO3 -KPIC -dalign -mt -silent -e
endif
FC=f90
LDFLAGS= -dy -G -h $N$S -z text
else
ifeq ($(FC),g77) 
FC=g77
FFLAGS = -O3 -fPIC -c -static -finit-local-zero
else
FFLAGS = -O3 -fPIC -c -fno-automatic
FC=$(GFORT)
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
