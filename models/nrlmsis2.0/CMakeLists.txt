project(libmsis2)
add_library(msis2 SHARED)
target_sources(msis2 PRIVATE matface.F90 alt2gph.F90 msis_constants.F90 msis_init.F90 msis_gfn.F90 msis_tfn.F90 msis_dfn.F90 msis_calc.F90 msis_gtd8d.F90)
