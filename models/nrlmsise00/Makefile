L=	../../lib/
MLIST=	nrlmsise00.f
N=	libmsis

sys=$(shell uname -s)
ifeq ($(sys),SunOS)
FFLAGS = -KPIC -dalign -mt -silent -e
FC=f90
else
FFLAGS = -O4 -fPIC -NC198 -c -static -finit-local-zero
endif

A=	.a
S=	.so
ML=	$L$N

libiri:
	@$(MAKE) clean
	$(COMPILE.f) $(MLIST)
	$(RM) $(ML)$A $(ML)$S
	$(LD) -o $(ML)$S -dy -G -h $N$S -z text *.o
	$(AR) cr $(ML)$A *.o
	@$(MAKE) clean

clean:
	$(RM) *.o
