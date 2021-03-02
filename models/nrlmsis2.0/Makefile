L=	../../lib/
MLIST=  matface.F90 alt2gph.F90 msis_constants.F90 msis_init.F90 msis_gfn.F90 msis_tfn.F90 msis_dfn.F90 msis_calc.F90 msis_gtd8d.F90
N=	libmsis2
GFORT=	gfortran-6

A=	.a
S=	.so
ML=	$L$N

FFLAGS = -fPIC -c -fno-automatic -O3 -cpp -DDBLE -ffast-math
FC=$(GFORT)
LDFLAGS= -shared

libmsis:
	@$(MAKE) clean
	$(COMPILE.f) $(MLIST)
	$(RM) $(ML)$A $(ML)$S
	$(LD) -o $(ML)$S $(LDFLAGS) *.o
	$(AR) cr $(ML)$A *.o
	@$(MAKE) clean

clean:
	$(RM) *.o
