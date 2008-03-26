SHELL=/bin/sh
NULL=true
#------------------------------------------------------------------------------
# Directory locations.
#------------------------------------------------------------------------------

SOURCEDIR=./source
HELPDIR=./compile
INSTALLDIR=.
GUPDIR=../../lib
EXAMPLE=./example

#------------------------------------------------------------------------------
# macros.
#------------------------------------------------------------------------------

MAKE=make
AR=ar
OS=
ENV=
RANLIB=ranlib
LANG=
IDL=idl

#------------------------------------------------------------------------------
# Macros for sunos.
#------------------------------------------------------------------------------

SHAREDEXT_sunos=so
NONSHAREDEXT_sunos=a
AROPTIONS_sunos=-r

FOPTIONS_SHARED_sunos_sparc32=-xlang=f77 -G
FOPTIONS_NONSHARED_sunos_sparc32=-c -xlang=f77
FC_sunos_sparc32=f95
PIC_sunos_sparc32=-KPIC
LDOPTIONS_sunos_sparc32=
WRAPPER_sunos_sparc32=32
LIBNAME_sunos_sparc32=sunos_sparc

FOPTIONS_SHARED_sunos_sparc64=-xlang=f77 -G -xarch=generic64
FOPTIONS_NONSHARED_sunos_sparc64=-c -xlang=f77 -xarch=generic64
FC_sunos_sparc64=f95
PIC_sunos_sparc64=-KPIC
LDOPTIONS_sunos_sparc64=
WRAPPER_sunos_sparc64=64
LIBNAME_sunos_sparc64=sunos_sparcV9

FOPTIONS_SHARED_sunos_gnu32=-fno-second-underscore -w -m32
FOPTIONS_NONSHARED_sunos_gnu32=-c -fno-second-underscore -w -m32
FC_sunos_gnu32=g77
PIC_sunos_gnu32=
LDOPTIONS_sunos_gnu32=-shared
WRAPPER_sunos_gnu32=32
LIBNAME_sunos_gnu32=sunos_sparc

FOPTIONS_SHARED_sunos_gnu64=-fno-second-underscore -w -m64
FOPTIONS_NONSHARED_sunos_gnu64=-c -fno-second-underscore -w -m64
FC_sunos_gnu64=g77
PIC_sunos_gnu64=
LDOPTIONS_sunos_gnu64=-shared
WRAPPER_sunos_gnu64=64
LIBNAME_sunos_gnu64=sunos_sparcV9

#------------------------------------------------------------------------------
# Macros for Linux32.
#------------------------------------------------------------------------------

SHAREDEXT_linux32=so
NONSHAREDEXT_linux32=a
AROPTIONS_linux32=-r

FOPTIONS_SHARED_linux32_intel32=-Bstatic
FOPTIONS_NONSHARED_linux32_intel32=-c -assume 2underscores
FC_linux32_intel32=ifort
PIC_linux32_intel32=
LDOPTIONS_linux32_intel32=-shared
WRAPPER_linux32_intel32=32
LIBNAME_linux32_intel32=linux_x86

FOPTIONS_SHARED_linux32_gnu32=-mno-align-double -fno-second-underscore -w -m32
FOPTIONS_NONSHARED_linux32_gnu32=-c -mno-align-double -w -m32
FC_linux32_gnu32=g77
FC_linux32_gnu32=gfortran
PIC_linux32_gnu32=-fPIC
LDOPTIONS_linux32_gnu32=-shared
WRAPPER_linux32_gnu32=32
LIBNAME_linux32_gnu32=linux_x86

#------------------------------------------------------------------------------
# Macros for Linux64.
#------------------------------------------------------------------------------

SHAREDEXT_linux64=so
NONSHAREDEXT_linux64=a
AROPTIONS_linux64=-r

FOPTIONS_SHARED_linux64_intel32=-Bstatic
FOPTIONS_NONSHARED_linux64_intel32=-c -assume 2underscores
FC_linux64_intel32=ifort
PIC_linux64_intel32=
LDOPTIONS_linux64_intel32=-shared
WRAPPER_linux64_intel32=32
LIBNAME_linux64_intel32=linux_x86

FOPTIONS_SHARED_linux64_gnu32=-mno-align-double -fno-second-underscore -w -m32
FOPTIONS_NONSHARED_linux64_gnu32=-c -mno-align-double -w -m32
FC_linux64_gnu32=g77
PIC_linux64_gnu32=-fPIC
LDOPTIONS_linux64_gnu32=-shared
WRAPPER_linux64_gnu32=32
LIBNAME_linux64_gnu32=linux_x86

FOPTIONS_SHARED_linux64_gnu64=-mno-align-double -fno-second-underscore -w -m64
FOPTIONS_NONSHARED_linux64_gnu64=-c -mno-align-double -w -m64
FC_linux64_gnu64=g77
FC_linux64_gnu64=gfortran-4.3
PIC_linux64_gnu64=-fPIC
LDOPTIONS_linux64_gnu64=-shared
WRAPPER_linux64_gnu64=64
LIBNAME_linux64_gnu64=linux_x86_64

#------------------------------------------------------------------------------
# Macros for Windows.
#------------------------------------------------------------------------------

SHAREDEXT_win32=dll
NONSHAREDEXT_win32=a
AROPTIONS_win32=-r

FOPTIONS_SHARED_win32_cygwin32=-mno-cygwin -I%IDLINC% -Wl,--add-stdcall-alias
FOPTIONS_NONSHARED_win32_cygwin32=-c -mno-cygwin -shared -mno-align-double -fno-second-underscore -w
FC_win32_cygwin32=g77
PIC_win32_cygwin32=
LDOPTIONS_win32_cygwin32=-shared
WRAPPER_win32_cygwin32=32
LIBNAME_win32_cygwin32=Win32_x86

#------------------------------------------------------------------------------
# Macros for Mach.
#------------------------------------------------------------------------------

SHAREDEXT_mach=dylib
NONSHAREDEXT_mach=a
AROPTIONS_mach=rc

FOPTIONS_SHARED_mach_gnu32=-w
FOPTIONS_NONSHARED_mach_gnu32=-w
FC_mach_gnu32=g95
PIC_mach_gnu32=
LDOPTIONS_mach_gnu32=-dynamic
WRAPPER_mach_gnu32=32
LIBNAME_mach_gnu32=mach

#------------------------------------------------------------------------------
# Compile/link entire distribution.
#------------------------------------------------------------------------------

all.help:
	@more $(HELPDIR)/Help.all

all: all.$(OS).$(ENV)

all..:
	echo "Missing OS and ENV variables"

all.linux32.intel32: all.build
all.linux32.gnu32: all.build
all.linux64.intel32: all.build
all.linux64.gnu32: all.build
all.linux64.gnu64: all.build
all.win32.cygwin32: all.build
all.mach.gnu32: all.build
all.sunos.sparc32: all.build
all.sunos.sparc64: all.build
all.sunos.gnu32: all.build
all.sunos.gnu64: all.build


all.build:
	@cd $(SOURCEDIR); \
cp wrappers_$(WRAPPER_$(OS)_$(ENV)).inc wrappers.inc;\
echo "Building non-sharable object \
onera_desp_lib_$(LIBNAME_$(OS)_$(ENV)).$(NONSHAREDEXT_$(OS))"; \
$(FC_$(OS)_$(ENV)) $(FOPTIONS_NONSHARED_$(OS)_$(ENV)) *.f;\
$(AR) $(AROPTIONS_$(OS)) liboneradesp_$(LIBNAME_$(OS)_$(ENV)).$(NONSHAREDEXT_$(OS)) *.o;\
$(RANLIB) liboneradesp_$(LIBNAME_$(OS)_$(ENV)).$(NONSHAREDEXT_$(OS));\
echo "";\
echo "Building sharable object \
onera_desp_lib_$(LIBNAME_$(OS)_$(ENV)).$(SHAREDEXT_$(OS))"; \
$(FC_$(OS)_$(ENV)) $(FOPTIONS_SHARED_$(OS)_$(ENV)) $(PIC_$(OS)_$(ENV)) -o onera_desp_lib_$(LIBNAME_$(OS)_$(ENV)).$(SHAREDEXT_$(OS)) \
*.f $(LDOPTIONS_$(OS)_$(ENV))

#------------------------------------------------------------------------------
# Install files.
#------------------------------------------------------------------------------

install.help:
	@more $(HELPDIR)/Help.install

install: install.lib

install.lib:
	@if [ -f $(SOURCEDIR)/onera_desp_lib_*.so ] ; then \
	   cp $(SOURCEDIR)/onera_desp_lib_*.so $(INSTALLDIR)/matlab;\
	   cp $(SOURCEDIR)/onera_desp_lib_*.so $(INSTALLDIR);\
	   cp $(SOURCEDIR)/onera_desp_lib_*.so $(GUPDIR)/onera_desp_lib.so;\
	 else \
	   $(NULL) ; \
	 fi
	@if [ -f $(SOURCEDIR)/onera_desp_lib_*.dll ] ; then \
	   cp $(SOURCEDIR)/onera_desp_lib_*.dll $(INSTALLDIR)/matlab;\
	   cp $(SOURCEDIR)/onera_desp_lib_*.dll $(INSTALLDIR);\
	   cp $(SOURCEDIR)/onera_desp_lib_*.dll $(GUPDIR)/onera_desp_lib.dll;\
	 else \
	   $(NULL) ; \
	 fi
	@if [ -f $(SOURCEDIR)/onera_desp_lib_*.dylib ] ; then \
	  @cp $(SOURCEDIR)/onera_desp_lib_*.dylib $(INSTALLDIR)/matlab;\
	   cp $(SOURCEDIR)/onera_desp_lib_*.dll $(INSTALLDIR);\
	 else \
	   $(NULL) ; \
	 fi
	@if [ -f $(SOURCEDIR)/liboneradesp_*.a ] ; then \
	   cp $(SOURCEDIR)/liboneradesp_*.a $(INSTALLDIR);\
	 else \
	   $(NULL) ; \
	fi


#------------------------------------------------------------------------------
# Test distribution.
#------------------------------------------------------------------------------

test.help:
	@more $(HELPDIR)/Help.test

test: test.$(LANG)

test.IDL:
	@cd $(EXAMPLE); $(IDL) idl_test.pro 

#------------------------------------------------------------------------------
# Clean/purge.
#------------------------------------------------------------------------------

clean:
	@-rm -f core
	@-rm -f $(SOURCEDIR)/core
	@-rm -f $(SOURCEDIR)/*.o
	@-rm -f $(SOURCEDIR)/*.so
	@-rm -f $(SOURCEDIR)/*.a
	@-rm -f $(SOURCEDIR)/*.dll
	@-rm -f $(SOURCEDIR)/*.dylib

purge:
	@-rm -f *~
	@-rm -f *#
	@-rm -f $(SOURCEDIR)/*~
	@-rm -f $(SOURCEDIR)/*#
