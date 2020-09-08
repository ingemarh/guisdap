#cmake_minimum_required(VERSION 3.16)

#set(CMAKE_C_COMPILER /usr/bin/gcc-6)
#set(CMAKE_Fortran_COMPILER /usr/bin/gfortran-6)
#set(CMAKE_Fortran_LINK_EXECUTABLE gfortran-6)
#enable_language(Fortran)

project(libguisdap)

option(GUPTHREAD "Build with pthreads" ON)
#add_compile_options(-O4 -Wall -march=native)

add_library(guisdap SHARED)

target_sources(guisdap PRIVATE
	mrqmndiag_worker.c dirthe_worker.c GULIPS_addm_worker.c GULIPS_cov_worker.c GULIPS_invR_worker.c GULIPS_mul_worker.c spec_worker.c transf_worker.c addr_covar_worker.c
)


target_compile_definitions(guisdap PRIVATE ANSI_C)

if (GUPTHREAD)
	target_compile_definitions(guisdap PRIVATE GUPTHREAD)

	target_link_libraries(guisdap PUBLIC
		pthread
	)
endif()


add_library(plwin SHARED
)
target_sources(plwin PRIVATE
	plwin.c
)
target_link_libraries(plwin PUBLIC fftw3f pthread)


add_library(alt_decoder SHARED
)
target_sources(alt_decoder PRIVATE
	alt_decoder.c
)
target_link_libraries(alt_decoder PUBLIC pthread)


add_library(clutter SHARED
)
target_sources(clutter PRIVATE
	clutter.c
)
target_link_libraries(clutter PUBLIC fftw3f pthread)

SET_TARGET_PROPERTIES(plwin clutter alt_decoder PROPERTIES PREFIX "")
#set_target_properties(plwin clutter alt_decoder guisdap PROPERTIES LIBRARY_OUTPUT_DIRECTORY lib)
file(GLOB PUBLIC_H "[a-z]*.h")
file(INSTALL ${PUBLIC_H} DESTINATION ${CMAKE_LIBRARY_OUTPUT_DIRECTORY})
file(GLOB PUBLIC_TXT "[a-z]*.txt")
file(INSTALL ${PUBLIC_TXT} DESTINATION ${SHARE_DIRECTORY}/doc)
#file(COPY ${CMAKE_CURRENT_SOURCE_DIR}/*.[txt|h] DESTINATION ${LIBRARY_OUTPUT_DIRECTORY}/)


#install(TARGETS guisdap plwin EXPORT guisdap)

#install(EXPORT guisdap DESTINATION lib/cmake/)

#find_package(Matlab REQUIRED MAIN_PROGRAM MX_LIBRARY)
#find_library (LIBIRI libiri.so /opt/guisdap9/lib)
#include_directories( ${Matlab_INCLUDE_DIRS} )
#add_link_options(-nostartfiles)
#matlab_add_mex( NAME iri EXECUTABLE SRC iri.F LINK_TO ${LIBIRI} gfortran mat m)
