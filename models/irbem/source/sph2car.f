
      REAL*4 FUNCTION sph2car(argc, argv)
      INCLUDE 'wrappers.inc'

       j = loc(argc)                    ! Obtains the number of arguments (argc)
                                       ! Because argc is passed by VALUE.

c  Call subroutine geo2gsm1, converting the IDL parameters to standard FORTRAN
c  passed by reference arguments.
c
c  subroutine geo2gsm: 6 arguments
      call SPH_CAR(%VAL(argv(1)), %VAL(argv(2)), %VAL(argv(3)),
     &  %VAL(argv(4)))

      sph2car = 9.9

      RETURN
      END
