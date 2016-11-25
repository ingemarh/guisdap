L=	../../lib/
MLIST=	cira.for igrf.f iridreg.for iriflip.for irifun.f irisub.f iritec.for
N=	libiri
ALIB=
EXF=

A=	.a
S=	.so
ML=	$L$N

sys=$(shell uname -s)
ifeq ($(sys),SunOS)
ifeq ($(bit),64)
ML=	$Lsparcv9/$N
#FFLAGS =-xarch=v9a
EXF =-m64 -xarch=sparcvis
endif
FFLAGS =-xO3 -KPIC -dalign -mt -silent -e $(EXF)
FC=f90
LDFLAGS= -dy -G -h $N$S -z text
else
ifeq ($(FC),g77)
EXF =-static -finit-local-zero -ffixed-line-length-132
elseifeq ($(FC),g95)
EXF =-fno-second-underscore -fstatic -fmultiple-save
ALIB=/usr/lib/g95/i586-mandriva-linux-gnu/4.0.4/libf95.a /usr/lib/libc_nonshared.a
else
FC=gfortran-4.8
EXF = -fno-automatic -fno-align-commons
endif
FFLAGS =-O2 -fPIC -c $(EXF)
LDFLAGS=-shared
endif

libiri:
	@$(MAKE) clean
	$(COMPILE.f) $(MLIST)
	$(RM) $(ML)$A $(ML)$S
	$(LD) -o $(ML)$S $(LDFLAGS) *.o $(ALIB)
	$(AR) cr $(ML)$A *.o $(ALIB)
	@$(MAKE) clean

clean:
	$(RM) *.o
