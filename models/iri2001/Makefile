L=	../../lib/
MLIST=	irisub.f irifun.f iritec.f iridreg.f igrf.f cira.f
N=	libiri

A=	.a
S=	.so
ML=	$L$N

sys=$(shell uname -s)
ifeq ($(sys),SunOS)
FFLAGS = -xO3 -KPIC -dalign -mt -silent -e
FC=f90
LDFLAGS= -dy -G -h $N$S -z text
else
FFLAGS = -O3 -fPIC -c -static -finit-local-zero -ffixed-line-length-132
LDFLAGS= -shared
endif

libiri:
	@$(MAKE) clean
	$(COMPILE.f) $(MLIST)
	$(RM) $(ML)$A $(ML)$S
	$(LD) -o $(ML)$S $(LDFLAGS) *.o
	$(AR) cr $(ML)$A *.o
	@$(MAKE) clean

clean:
	$(RM) *.o
