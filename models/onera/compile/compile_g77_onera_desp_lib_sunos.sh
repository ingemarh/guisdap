g77 -c -fno-second-underscore -w ../source/*.f
ar -r liboneradesp_sunos.a *.o
ranlib liboneradesp_sunos.a
#
g77 -c -fno-second-underscore -w ../source/*.f
ar -r libBL.a CoordTrans.o sgp4*.o AFRL_CRRES_models.o AE8_AP8.o heliospheric_transformation.o internal_field.o Tsyganenko04.o Alexeev2000.o calcul_Lstar_o.o dipole.o igrf.o init_nouveau.o mead.o field_line_tracing.o Pfitzer_q.o Olson_Pfitzer_dyn.o Tsyganenko87l.o Tsyganenko87s.o Tsyganenko89.o Tsyganenko96.o Tsyganenko01.o t01_s.o Ostapenko.o find_bm.o loc_equator.o trace_drift_shell.o date_util.o
ranlib libBL.a
g77 -o onera_desp_lib_sunos.so onera_desp_lib.o -L. -lBL -shared
rm *.o libBL.a
cp onera_desp_lib_sunos.so ../matlab/onera_desp_lib.so
mv liboneradesp_sunos.a onera_desp_lib_sunos.so ..
