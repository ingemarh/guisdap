project(libguisdap)

option(GUPTHREAD "Build with pthreads" ON)
#add_compile_options(-O4 -Wall -march=native)

list(APPEND CMAKE_MODULE_PATH "${CMAKE_CURRENT_SOURCE_DIR}/findFFTW")
find_package(FFTW REQUIRED)
if (FFTW_FOUND)
    message(STATUS "FFTW_INCLUDE_DIRS: ${FFTW_INCLUDE_DIRS}")
else ()
    message(FATAL_ERROR "FFTW not found!")
endif ()

add_library(guisdap SHARED)

target_sources(guisdap PRIVATE
	mrqmndiag_worker.c dirthe_worker.c GULIPS_addm_worker.c GULIPS_cov_worker.c GULIPS_invR_worker.c GULIPS_mul_worker.c spec_worker.c transf_worker.c addr_covar_worker.c
)

target_compile_definitions(guisdap PRIVATE ANSI_C)

if (GUPTHREAD)
	target_compile_definitions(guisdap PRIVATE GUPTHREAD)
	target_link_libraries(guisdap PUBLIC pthread)
endif()

add_library(plwin SHARED)
target_sources(plwin PRIVATE plwin.c get_tid.c)
target_link_libraries(plwin PUBLIC FFTW::Float pthread)

add_library(alt_decoder SHARED)
target_sources(alt_decoder PRIVATE alt_decoder.c get_tid.c)
target_link_libraries(alt_decoder PUBLIC pthread)

add_library(clutter SHARED)
target_sources(clutter PRIVATE clutter.c get_tid.c)
target_link_libraries(clutter PUBLIC FFTW::Float pthread)

SET_TARGET_PROPERTIES(plwin clutter alt_decoder PROPERTIES PREFIX "")
#set_target_properties(plwin clutter alt_decoder guisdap PROPERTIES LIBRARY_OUTPUT_DIRECTORY lib)
file(GLOB PUBLIC_H "[a-z]*.h")
file(INSTALL ${PUBLIC_H} DESTINATION ${CMAKE_LIBRARY_OUTPUT_DIRECTORY})
file(GLOB PUBLIC_TXT "[a-z]*.txt")
file(INSTALL ${PUBLIC_TXT} DESTINATION ${SHARE_DIRECTORY}/doc)
#file(COPY ${CMAKE_CURRENT_SOURCE_DIR}/*.[txt|h] DESTINATION ${LIBRARY_OUTPUT_DIRECTORY}/)
