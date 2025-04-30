project(irbem)

#string(APPEND CMAKE_Fortran_FLAGS " -mno-align-double -fno-second-underscore -w")
string(APPEND CMAKE_Fortran_FLAGS " -fno-second-underscore -w")

#file(GLOB IRBEM_SRC "../myOwnMagField*.f *.f*")
file(GLOB IRBEM_SRC "*.[fc]*")
add_library(onera_desp_lib SHARED)
target_sources(onera_desp_lib PRIVATE ${IRBEM_SRC})
SET_TARGET_PROPERTIES(onera_desp_lib PROPERTIES PREFIX "")
