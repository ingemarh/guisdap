L=	../../lib/
MLIST=	irisub.f irifun.f iritec.f iridreg.f igrf.f cira.f
N=	libiri

sys=$(shell uname -s)
ifeq ($(sys),SunOS)
FFLAGS = -O -KPIC -dalign -mt -silent -e
FC=f90
else
FFLAGS = -O3 -fPIC -c -static -finit-local-zero -ffixed-line-length-132
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
