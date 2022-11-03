       subroutine matface(lp,p)
       use msis_init, only          : msisinit
       integer, intent(in) :: lp
       character(lp), intent(in) :: p

       call msisinit(parmpath=p,parmfile='msis20.parm')

       endsubroutine
