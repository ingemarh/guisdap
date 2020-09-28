project(libiri)

#string(APPEND CMAKE_Fortran_FLAGS " -fno-automatic -fno-align-commons")

file(GLOB IRI_SRC "*.f*")
add_library(iri SHARED)
target_sources(iri PRIVATE ${IRI_SRC})

#file(GLOB PUBLIC_ASC "*.asc")
#file(GLOB PUBLIC_DAT "[di]grf*.dat" "mcsat*.dat")
file(GLOB PUBLIC_DAT "mcsat*.dat")
file(INSTALL ${PUBLIC_DAT} DESTINATION ${SHARE_DIRECTORY}/iri)
