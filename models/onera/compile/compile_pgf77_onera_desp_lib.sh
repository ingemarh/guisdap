pgf77 -c -Mnodalign ../source/*.f
ar -r libBL.a CoordTrans.o sgp4*.o AFRL_CRRES_models.o AE8_AP8.o heliospheric_transformation.o internal_field.o Tsyganenko04.o Alexeev2000.o calcul_Lstar_o.o Dipole.o IGRF.o init_nouveau.o Mead.o Pfitzer_q.o Olson_Pfitzer_dyn.o Tsyganenko87l.o Tsyganenko87s.o Tsyganenko89.o Tsyganenko96.o Tsyganenko01.o T01_S.o Ostapenko.o find_bm.o loc_equator.o trace_drift_shell.o date_util.o
ar -r liboneradesp.a *.o
ranlib libBL.a
ranlib liboneradesp.a
pgf77 -o onera_desp_lib.so onera_desp_lib.o -L. -lBL -shared
rm *.o libBL.a
