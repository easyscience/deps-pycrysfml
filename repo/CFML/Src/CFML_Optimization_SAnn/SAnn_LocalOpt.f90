 Submodule (CFML_Simulated_Annealing) SAnn_LocalOpt
  implicit none
   contains
    !!----
    !!---- Module Subroutine Local_Optim(Model_Functn,n,x,f,low,high,bound)
    !!----    integer,                      intent(in)      :: n
    !!----    real(kind=cp), dimension(:),  intent(in out)  :: x
    !!----    real(kind=cp),                intent(out)     :: f
    !!----    real(kind=cp), dimension(:),  intent(in)      :: low,high
    !!----    integer, dimension(:),        intent(in)      :: bound
    !!----    Interface
    !!----       Subroutine Model_Functn(v,cost)
    !!----          real,dimension(:),    intent( in):: v
    !!----          real,                 intent(out):: cost
    !!----       End Subroutine Model_Functn
    !!----    End Interface
    !!----
    !!----   (Public)
    !!----   Unirandi subroutine
    !!----   T.  Csendes,  ”Nonlinear  parameter  estimation  by  global  optimization, efficiency  and  reliability,
    !!----   Acta Cybernetica,  vol.  8,  no.  4,  pp.  361–370, 1988.
    !!----   Adapted by J. Rodriguez-Carvajal to F90
    !!----
    !!---- Update: April - 2008
    !!
    Module Subroutine Local_Optim(Model_Functn,n,x,f,low,high,bound)
       !---- Arguments ----!
       integer,                      intent(in)      :: n
       real(kind=cp), dimension(:),  intent(in out)  :: x
       real(kind=cp),                intent(out)     :: f
       real(kind=cp), dimension(:),  intent(in)      :: low,high
       integer, dimension(:),        intent(in)      :: bound

       Interface
          Subroutine Model_Functn(v,cost)
             Use CFML_GlobalDeps, only: Cp
             real(kind=cp),dimension(:),    intent( in):: v
             real(kind=cp),                 intent(out):: cost
          End Subroutine Model_Functn
       End Interface

       !---- Local variables ----!
       integer                               :: i,itest, irndm, nfev
       real(kind=cp)                         :: h, deltf, eps, a, f1,relcon, maxfn
       real(kind=cp), dimension(n)           :: x1
       real(kind=cp), dimension(100,n)       :: r
       real(kind=cp), parameter              :: zero=0.0, onen3=0.001, half=0.5, one=1.0, two=2.0

       h = onen3
       deltf = one                 !initial step length
       itest = 0
       nfev = 0
       relcon=0.00001
       eps =  0.00001
       maxfn=10000
       CALL Model_Functn(x, f) !starting value of the function
       !write(*,*) "  f=",f
       do_outm: do
          call random_number(r)              !  Evaluate 100 random vectors r(100,n)
          irndm = 0
          do_intm: do
            irndm = irndm+1
            IF (irndm > 100) cycle do_outm
            a = zero                            !Select a random vector having norm
            DO  i=1,n                           !less or equal to 0.5
              r(irndm,i) = r(irndm,i)-half
              a = a+r(irndm,i)*r(irndm,i)
            END DO
            IF (a <= zero) cycle do_intm
            a = SQRT(a)
            !IF (a > half) cycle do_intm
            r(irndm,1:n) = r(irndm,1:n)/a       !New trial point
            x1(1:n) = x(1:n)+h*r(irndm,1:n)
            call Boundary_Cond(n,x1,low,high,bound)
            CALL Model_Functn(x1, f1)
           ! write(*,*) "  f1-1=",f1
            nfev = nfev+1
            IF (f1 >= f) then
               IF (nfev > maxfn) exit do_outm
               h = -h                           !Step in the opposite direction
               x1(1:n) = x(1:n)+h*r(irndm,1:n)
               call Boundary_Cond(n,x1,low,high,bound)
               CALL Model_Functn(x1, f1)
              ! write(*,*) "  f1-2=",f1
               nfev = nfev+1
               IF (f1 >= f) then
                  IF (nfev > maxfn) exit do_outm
                  itest = itest+1
                  IF (itest < 2) cycle do_intm
                  h = h*half                    !Decrease step length
                  itest = 0
                  IF (deltf < eps) exit do_outm !Relative convergence test for the
                                                !objective function
                  IF (ABS(h)-relcon < 0.0) THEN !Convergence test for the step length
                    exit  do_outm
                  ELSE
                    cycle do_intm
                  END IF
               END IF
            END IF

            do
               x(1:n) = x1(1:n)
               deltf = (f-f1)/ABS(f1)
               f = f1
               h = h*two                   ! Increase step length
               x1(1:n) = x(1:n)+h*r(irndm,1:n)
               call Boundary_Cond(n,x1,low,high,bound)
               CALL Model_Functn(x1, f1)
               nfev = nfev+1
               IF (f1 >= f) exit
            end do
            IF (nfev > maxfn) exit do_outm  ! Check tolerance maxfn
            h = ABS(h*half)                 ! Decrease step length
          End do do_intm
       End do do_outm

    End Subroutine Local_Optim

 End Submodule SAnn_LocalOpt