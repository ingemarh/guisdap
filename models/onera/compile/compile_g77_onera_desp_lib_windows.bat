g77 -c -mno-cygwin -shared -mno-align-double -fno-second-underscore -w ../source/*.f
ar -r libBL.a CoordTrans.o sgp4*.o AFRL_CRRES_models.o AE8_AP8.o heliospheric_transformation.o internal_field.o Tsyganenko04.o Alexeev2000.o calcul_Lstar_o.o dipole.o igrf.o init_nouveau.o field_line_tracing.o mead.o Pfitzer_q.o Olson_Pfitzer_dyn.o Tsyganenko87l.o Tsyganenko87s.o Tsyganenko89.o Tsyganenko96.o Tsyganenko01.o t01_s.o Ostapenko.o find_bm.o loc_equator.o trace_drift_shell.o date_util.o
ar -r liboneradesp.a *.o
ranlib libBL.a
ranlib liboneradesp.a
g77 -mno-cygwin -I%IDLINC% -Wl,--add-stdcall-alias -shared -o onera_desp_lib_windows.dll onera_desp_lib.o -L. -lBL
rm *.o libBL.a
cp onera_desp_lib_windows.dll ../matlab/onera_desp_lib.dll
mv liboneradesp.a onera_desp_lib_windows.dll ..
