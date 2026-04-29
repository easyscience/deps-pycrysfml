
 Submodule(CFML_gSpaceGroups) gS_Symm_Symbols
   implicit none
   real(kind=cp), parameter :: eps_symm  = 0.0002_cp
   contains

    !!----
    !!---- Function Axes_Rotation(R) Result(N)
    !!----    integer, dimension(3,3), intent  (in) :: r    !  In -> Rotation part of Symmetry Operator
    !!----    integer                               :: n    ! Out -> Orden of the Rotation Part
    !!----
    !!----    Determine the orden of rotation (valid for all bases). Return a zero
    !!----    if any error occurs.
    !!----
    !!---- Update: February - 2005
    !!
    Function Axes_Rotation(r) Result(n)
       !---- Arguments ----!
       integer, dimension(3,3), intent (in) :: r
       integer                              :: n

       !---- Local Variables ----!
       integer :: det,itr

       n=0

       det=Determ3D(r)
       itr=trace(r)
       select case (itr)
          case (-3)
             if (det == -1) n=-1

          case (-2)
             if (det == -1) n=-6

          case (-1)
             if (det == -1) n=-4
             if (det ==  1) n= 2

          case ( 0)
             if (det == -1) n=-3
             if (det ==  1) n= 3

          case ( 1)
             if (det == -1) n=-2
             if (det ==  1) n= 4

          case ( 2)
             if (det ==  1) n= 6

          case ( 3)
             if (det ==  1) n= 1
       end select

    End Function Axes_Rotation

    !!----
    !!---- Subroutine Get_String_Resolv(T,X,Ix,Symb)
    !!----    real(kind=cp), dimension(3), intent( in) :: t      !  In -> Traslation part
    !!----    real(kind=cp), dimension(3), intent( in) :: x      !  In -> real part of variable
    !!----    integer,       dimension(3), intent( in) :: ix     !  In -> Frags: 1:x, 2:y, 3:z
    !!----    character (len=*),           intent(out) :: symb   ! Out -> String
    !!----
    !!----    Returning a string for point, axes or plane give as
    !!----    written in fractional form from Resolv_sist procedures.
    !!----
    !!---- Update: February - 2005
    !!
    Subroutine Get_String_Resolv(t,x,ix,symb)
       !---- Arguments ----!
       real(kind=cp), dimension(3),      intent( in) :: t
       real(kind=cp), dimension(3),      intent( in) :: x
       integer,       dimension(3),      intent( in) :: ix
       character (len=*),                intent(out) :: symb

       !---- Local Variables ----!
       character(len=60) :: car
       integer           :: i, np, npos
       real(kind=cp),dimension(3) :: xx

       !---- Main ----!
       xx=x
       do i=1,3
          car = String_Fraction_2Dig(x(i))
          np=index(car,"1/2")
          if (np > 0) then
             xx=2.0*x
             exit
          end if
       end do

       symb=" "
       npos=1
       do i=1,3
          !---- Only t value ----!
          if (abs(xx(i)) <= eps_symm) then
             car=String_Fraction_2Dig(t(i))
             car=adjustl(car)
             if (car(1:1) == "+") car=car(2:)
             np=len_trim(car)
             if (i < 3) then
                symb(npos:)=car(1:np)//", "
                npos=npos+np+2
             else
                symb(npos:)=car(1:np)
             end if
             cycle
          end if

          car=String_Fraction_2Dig(xx(i))
          car=adjustl(car)
          if (abs(abs(xx(i)) - 1.0) <= eps_symm) then
             if (car(1:2) == "+1") car=car(3:)
             if (car(1:2) == "-1") car(2:)=car(3:)
          else
             if (car(1:1) == "+") car=car(2:)
          end if
          np=len_trim(car)
          symb(npos:)=car(1:np)
          npos=npos+np
          select case (ix(i))
             case (1)
                symb(npos:)="x"
             case (2)
                symb(npos:)="y"
             case (3)
                symb(npos:)="z"
          end select
          npos=npos+1
          if (abs(t(i)) > 0.0 ) then
             car=String_Fraction_2Dig(t(i))
             car=adjustl(car)
             np=len_trim(car)
             symb(npos:)=car(1:np)
             npos=npos+np
          end if
          if (i < 3) then
             symb(npos:)=", "
             npos=npos+2
          end if

       end do
       symb=pack_string(symb)

    End Subroutine Get_String_Resolv

    !!----
    !!---- Function Symmetry_Symbol(S,T) Result(Symbol)
    !!----    integer, dimension(3,3),     intent( in) :: s
    !!----    real(kind=cp), dimension(3), intent( in) :: t
    !!----    character (len=:),   allocatable         :: symbol
    !!----
    !!----
    !!----    Obtain the symbol of the symmetry element corresponding to operator (S,T) in 3D
    !!----
    !!---- Update: February - 2005, converted in submodule function March 2020
    !!
    Module Function Symmetry_Symbol(S,T) Result(Symbol)
       !---- Arguments ----!
       integer,       dimension(3,3),    intent( in) :: s
       real(kind=cp), dimension(3),      intent( in) :: t
       character (len=:), allocatable                :: Symbol

       !---- Local variables ----!
       character (len=80)      :: carsym,symb
       character (len=1)       :: signo
       integer                 :: i, n, npos
       integer, dimension(3)   :: ix1, ix2, ix3
       integer, dimension(3,3) :: w
       integer, dimension(3,3), parameter :: identidad = reshape([1, 0, 0, &
                                                                   0, 1, 0, &
                                                                   0, 0, 1],[3,3])
       real(kind=cp)                    :: rnum
       real(kind=cp), dimension(3)      :: t0,t1,t2,t3
       real(kind=cp), dimension(3)      :: x1,x2,x3
       real(kind=cp), dimension(3)      :: p0,p1,p2,p3
       real(kind=cp), dimension(3,3)    :: ww

       !---- Initialize ----!
       symb=" "
       n=axes_rotation(s)
       !t0=mod(t+10.0_cp,1.0_cp)  !Attempt to use the given translation
       t0=t                       !of the symmetry operator
       x1 =0.0
       ix1=0
       call Clear_Error()

       select case (n)
          case (1) ! Traslation or identity
             if (sum(abs(t)) <= 3.0*eps_symm) then
                symb(1:1) ="1"
             else
                symb(1:3)="t ("
                npos=4
                call get_string_resolv(t0,x1,ix1,carsym)
                symb(npos:)=carsym(1:len_trim(carsym))//")"
             end if

          case (:-3) ! Rotoinversion
             !---- Inversion point ----!
             w=s-identidad
             call resolv_sist_3x3(w,-t0,t3,x3,ix3)

             !---- Axes rotation ----!
             w=matmul(s,s)-identidad
             t1=matmul(real(s),t0)+t0
             call resolv_sist_3x3(w,-t1,t2,x2,ix2)

             !---- Sense of rotation ----!
             !---- P0, P1 ----!
             p0=0.0
             p1=1.0
             do i=1,3
                if (ix2(i) == 0) then
                   p0(i)=t2(i)
                   p1(i)=t2(i)
                else
                   p0(i)=t2(i)+x2(i)*p0(ix2(i))
                   p1(i)=t2(i)+x2(i)*p1(ix2(i))
                end if
             end do

             !---- P2 ----!
             do i=1,3
                if (p1(i) > 0.0 ) exit
             end do
             select case (i)
                case (1)
                   p2(3)=0.5*p1(3)
                   p2(2)=0.7*p1(2)
                   p2(1)=-(p2(2)*p1(2) + p2(3)*p1(3))/p1(1)

                case (2)
                   p2(1)=0.5*p1(1)
                   p2(3)=0.7*p1(3)
                   p2(2)=-(p2(1)*p1(1) + p2(3)*p1(3))/p1(2)

                case (3)
                   p2(1)=0.5*p1(1)
                   p2(2)=0.7*p1(2)
                   p2(3)=-(p2(1)*p1(1) + p2(2)*p1(2))/p1(3)
             end select
             do i=1,3
                if (abs(p2(i) - p0(i)) <= eps_symm) p2(i)=p2(i)*p2(i)+0.5*real(i)
             end do

             !---- P3 ----!
             p3=matmul(real(s),p2)+t0
             ww(1,:)=p1-p0
             ww(2,:)=p2-p0
             ww(3,:)=p3-p0
             rnum=Determ3D(ww)
             if (rnum > 0.0) then
                signo="-"
             else
                signo="+"
             end if

             !---- Determine the final symbol ----!
             write(unit=symb,fmt="(i2)") n
             symb=adjustl(symb)
             npos=len_trim(symb)
             npos=npos+1
             symb(npos:npos)=signo
             npos=npos+2
             call get_string_resolv(t2,x2,ix2,carsym)
             symb(npos:)=carsym(1:len_trim(carsym))//";"
             npos=len_trim(symb)+2
             call get_string_resolv(t3,x3,ix3,carsym)
             symb(npos:)=carsym(1:len_trim(carsym))

          case (-2)  ! Reflection or glide reflection
             t1=matmul(s,t0)+t0
             if (t1(1) <= eps_symm .and. t1(2) <= eps_symm .and. &
                 t1(3) <= eps_symm) then        ! Pure Reflection

                !----Mirror Plane ----!
                w=s-identidad
                call resolv_sist_3x3(w,-t0,t3,x3,ix3)
                symb(1:2)="m "
                npos=3
                call get_string_resolv(t3,x3,ix3,carsym)
                symb(npos:)=carsym(1:len_trim(carsym))
             else                          ! Glide Reflection
                t3=0.5*t1
                w=s-identidad
                t1=t0-t3
                call resolv_sist_3x3(w,-t1,t2,x2,ix2)

                !---- Determine the final symbol ----!
                symb(1:2)="g "

                !---- a: (1/2, 0, 0) ----!
                if ( (abs(t3(1) - 0.5) <= eps_symm) .and. (abs(t3(2)) <= eps_symm) .and. &
                     (abs(t3(3)) <= eps_symm) ) then
                   symb(1:2)="a "
                end if

                !---- b: (0, 1/2, 0) ----!
                if ( (abs(t3(2) - 0.5) <= eps_symm) .and. (abs(t3(1)) <= eps_symm) .and. &
                     (abs(t3(3)) <= eps_symm) ) then
                   symb(1:2)="b "
                end if

                !---- c: (0, 0, 1/2) ----!
                if ( (abs(t3(3) - 0.5) <= eps_symm) .and. (abs(t3(2)) <= eps_symm) .and. &
                     (abs(t3(1)) <= eps_symm) ) then
                   symb(1:2)="c "
                end if

                !---- n: ( 1/2, 1/2, 0); (0, 1/2, 1/2); (1/2, 0, 1/2) ----!
                !---- n: ( 1/2, 1/2, 1/2) ----!
                !---- n: (-1/2, 1/2, 1/2); (1/2, -1/2, 1/2); (1/2, 1/2, -1/2) ----!
                if ( (abs(t3(1) - 0.5) <= eps_symm) .and. (abs(t3(2) - 0.5) <= eps_symm) .and. &
                     (abs(t3(3)) <= eps_symm) ) then
                   symb(1:2)="n "
                end if
                if ( (abs(t3(2) - 0.5) <= eps_symm) .and. (abs(t3(3) - 0.5) <= eps_symm) .and. &
                     (abs(t3(1)) <= eps_symm) ) then
                   symb(1:2)="n "
                end if
                if ( (abs(t3(1) - 0.5) <= eps_symm) .and. (abs(t3(3) - 0.5) <= eps_symm) .and. &
                     (abs(t3(2)) <= eps_symm) ) then
                   symb(1:2)="n "
                end if
                if ( (abs(t3(1) - 0.5) <= eps_symm) .and. (abs(t3(2) - 0.5) <= eps_symm) .and. &
                     (abs(t3(3) - 0.5) <= eps_symm) ) then
                   symb(1:2)="n "
                end if
                if ( (abs(t3(1) + 0.5) <= eps_symm) .and. (abs(t3(2) - 0.5) <= eps_symm) .and. &
                     (abs(t3(3) - 0.5) <= eps_symm) ) then
                   symb(1:2)="n "
                end if
                if ( (abs(t3(1) - 0.5) <= eps_symm) .and. (abs(t3(2) + 0.5) <= eps_symm) .and. &
                     (abs(t3(3) - 0.5) <= eps_symm) ) then
                   symb(1:2)="n "
                end if
                if ( (abs(t3(1) - 0.5) <= eps_symm) .and. (abs(t3(2) - 0.5) <= eps_symm) .and. &
                     (abs(t3(3) + 0.5) <= eps_symm) ) then
                   symb(1:2)="n "
                end if

                !---- d: ( 1/4,+-1/4, 0); (0, 1/4,+-1/4); (+-1/4, 0, 1/4) ----!
                !---- d: ( 1/4, 1/4,+-1/4); (+-1/4, 1/4, 1/4); (1/4,+-1/4, 1/4) ----!
                !---- d: (-1/4, 1/4,+-1/4); (+-1/4,-1/4, 1/4); (1/4,+-1/4,-1/4) ----!
                p3=t3
                p3=mod(p3+10.0_cp,1.0_cp)
                do i=1,3
                   if (p3(i) > 0.5) p3(i)=p3(i) -1.0
                end do
                if ( (abs(p3(1) - 0.25) <= eps_symm) .and. (abs(abs(p3(2)) - 0.25) <= eps_symm) .and. &
                     (abs(p3(3)) <= eps_symm) ) then
                   symb(1:2)="d "
                end if
                if ( (abs(p3(2) - 0.25) <= eps_symm) .and. (abs(abs(p3(3)) - 0.25) <= eps_symm) .and. &
                     (abs(p3(1)) <= eps_symm) ) then
                   symb(1:2)="d "
                end if
                if ( (abs(p3(3) - 0.25) <= eps_symm) .and. (abs(abs(p3(1)) - 0.25) <= eps_symm) .and. &
                     (abs(p3(2)) <= eps_symm) ) then
                   symb(1:2)="d "
                end if
                if ( (abs(p3(1) - 0.25) <= eps_symm) .and. (abs(abs(p3(3)) - 0.25) <= eps_symm) .and. &
                     (abs(p3(2) - 0.25) <= eps_symm) ) then
                   symb(1:2)="d "
                end if
                if ( (abs(p3(2) - 0.25) <= eps_symm) .and. (abs(abs(p3(1)) - 0.25) <= eps_symm) .and. &
                     (abs(p3(3) - 0.25) <= eps_symm) ) then
                   symb(1:2)="d "
                end if
                if ( (abs(p3(1) - 0.25) <= eps_symm) .and. (abs(abs(p3(2)) - 0.25) <= eps_symm) .and. &
                     (abs(p3(3) - 0.25) <= eps_symm) ) then
                   symb(1:2)="d "
                end if
                if ( (abs(p3(1) + 0.25) <= eps_symm) .and. (abs(abs(p3(3)) - 0.25) <= eps_symm) .and. &
                     (abs(p3(2) - 0.25) <= eps_symm) ) then
                   symb(1:2)="d "
                end if
                if ( (abs(p3(2) + 0.25) <= eps_symm) .and. (abs(abs(p3(1)) - 0.25) <= eps_symm) .and. &
                     (abs(p3(3) - 0.25) <= eps_symm) ) then
                   symb(1:2)="d "
                end if
                if ( (abs(p3(3) + 0.25) <= eps_symm) .and. (abs(abs(p3(2)) - 0.25) <= eps_symm) .and. &
                     (abs(p3(1) - 0.25) <= eps_symm) ) then
                   symb(1:2)="d "
                end if
                npos=3

                !---- Glide Part ----!
                if ( symb(1:1) == "n" .or. symb(1:1) == "d" .or. &
                     symb(1:1) == "g" ) then
                   symb(npos:)="("
                   npos=npos+1
                   x1 =0.0
                   ix1=0
                   call get_string_resolv(t3,x1,ix1,carsym)
                   symb(npos:)=carsym(1:len_trim(carsym))//")"
                   npos=len_trim(symb)+2
                end if

                !---- Location of Glide Plane ----!
                call get_string_resolv(t2,x2,ix2,carsym)
                symb(npos:)=carsym(1:len_trim(carsym))
             end if

          case (-1)  ! Inversion
             t1=0.5*t0
             symb(1:3)="-1 "
             npos=4
             x1 =0.0
             ix1=0
             call get_string_resolv(t1,x1,ix1,carsym)
             symb(npos:)=carsym(1:len_trim(carsym))

          case (2:)  ! Rotation / Screw Rotation
             w=identidad
             t1=t0
             do i=1,n-1
                w=matmul(w,s)
                t1=t1+matmul(w,t0)
             end do
             if (abs(t1(1)) <= eps_symm .and. abs(t1(2)) <= eps_symm &
                 .and. abs(t1(3)) <= eps_symm) then              ! Pure rotation

                !---- Rotations axes ----!
                w=s-identidad
                call resolv_sist_3x3(w,-t0,t2,x2,ix2)

                !---- Sense of rotation ----!
                !---- P0, P1 ----!
                p0=0.0
                p1=1.0
                do i=1,3
                   if (ix2(i) == 0) then
                      p0(i)=t2(i)
                      p1(i)=t2(i)
                   else
                      p0(i)=t2(i)+x2(i)*p0(ix2(i))
                      p1(i)=t2(i)+x2(i)*p1(ix2(i))
                   end if
                end do

                !---- P2 ----!
                do i=1,3
                   if (p1(i) > 0.0 ) exit
                end do
                select case (i)
                   case (1)
                      p2(3)=0.5*p1(3)
                      p2(2)=0.7*p1(2)
                      p2(1)=-(p2(2)*p1(2) + p2(3)*p1(3))/p1(1)

                   case (2)
                      p2(1)=0.5*p1(1)
                      p2(3)=0.7*p1(3)
                      p2(2)=-(p2(1)*p1(1) + p2(3)*p1(3))/p1(2)

                   case (3)
                      p2(1)=0.5*p1(1)
                      p2(2)=0.7*p1(2)
                      p2(3)=-(p2(1)*p1(1) + p2(2)*p1(2))/p1(3)
                end select
                do i=1,3
                   if (abs(p2(i) - p0(i)) <= eps_symm) p2(i)=p2(i)*p2(i)+0.5*real(i)
                end do

                !---- P3 ----!
                p3=matmul(real(s),p2)+t0
                ww(1,:)=p1-p0
                ww(2,:)=p2-p0
                ww(3,:)=p3-p0

                rnum=Determ3D(ww)
                if (rnum > 0.0) then
                   signo="+"
                else
                   signo="-"
                end if

                !---- Determine the final symbol ----!
                write(unit=symb,fmt="(i2)") n
                symb=adjustl(symb)
                npos=len_trim(symb)
                if ( n /= 2) then
                   npos=npos+1
                   symb(npos:)=signo
                end if
                npos=npos+2
                call get_string_resolv(t2,x2,ix2,carsym)
                symb(npos:)=carsym(1:len_trim(carsym))
             else                     ! Screw Rotation
                t3=(1.0/real(n))*t1
                w=s-identidad
                t1=t0-t3
                call resolv_sist_3x3(w,-t1,t2,x2,ix2)

                !---- Sense of rotation ----!
                !---- P0, P1 ----!
                p0=0.0
                p1=1.0
                do i=1,3
                   if (ix2(i) == 0) then
                      p0(i)=t2(i)
                      p1(i)=t2(i)
                   else
                      p0(i)=t2(i)+x2(i)*p0(ix2(i))
                      p1(i)=t2(i)+x2(i)*p1(ix2(i))
                   end if
                end do

                !---- P2 ----!
                do i=1,3
                   if (p1(i) > 0.0 ) exit
                end do
                select case (i)
                   case (1)
                      p2(3)=0.5*p1(3)
                      p2(2)=0.7*p1(2)
                      p2(1)=-(p2(2)*p1(2) + p2(3)*p1(3))/p1(1)

                   case (2)
                      p2(1)=0.5*p1(1)
                      p2(3)=0.7*p1(3)
                      p2(2)=-(p2(1)*p1(1) + p2(3)*p1(3))/p1(2)

                   case (3)
                      p2(1)=0.5*p1(1)
                      p2(2)=0.7*p1(2)
                      p2(3)=-(p2(1)*p1(1) + p2(2)*p1(2))/p1(3)
                end select
                do i=1,3
                   if (abs(p2(i) - p0(i)) <= eps_symm) p2(i)=p2(i)*p2(i)+0.5*real(i)
                end do

                !---- P3 ----!
                p3=matmul(real(s),p2)+t0
                ww(1,:)=p1-p0
                ww(2,:)=p2-p0
                ww(3,:)=p3-p0
                rnum=Determ3D(ww)
                if (rnum > 0.0) then
                   signo="+"
                else
                   signo="-"
                end if

                !---- Determine the final symbol ----!
                write(unit=symb,fmt="(i2)") n
                symb=adjustl(symb)
                npos=len_trim(symb)
                if ( n /= 2) then
                   npos=npos+1
                   symb(npos:npos)=signo
                end if
                npos=npos+2

                !---- Screw Part ----!
                symb(npos:)="("
                npos=npos+1
                x1 =0.0
                ix1=0
                call get_string_resolv(t3,x1,ix1,carsym)
                symb(npos:)=carsym(1:len_trim(carsym))//")"
                npos=len_trim(symb)+2
                call get_string_resolv(t2,x2,ix2,carsym)
                symb(npos:)=carsym(1:len_trim(carsym))
             end if

       end select

       Symbol=trim(symb)

    End Function Symmetry_Symbol

    !!----
    !!---- Module Subroutine SearchOp(Sim,I1,I2,Isl)
    !!----    integer , dimension(3,3), Intent(in)  :: sim      !  In -> Rotational part of a symmetry operator
    !!----    integer ,                 Intent(in)  :: i1       !  In -> i1=1,  i2=24  if not hexagonal  (matrices of m3m )
    !!----    integer ,                 Intent(in)  :: i2       !  In -> i1=25, i2=36  if     hexagonal  (matrices of 6/mmm)
    !!----    integer ,                 Intent(out) :: Isl      ! Out -> Index of the matrix Mod6(Isl,:,:)=sim.
    !!----                                                               This index allow to get the corresponding tabulated symmetry symbol.
    !!----
    !!---- Update: February - 2005
    !!
    Module Function SearchOp(Sim,I1,I2) Result(Isl)
       !---- Arguments ----!
       integer , dimension(3,3), Intent(in) :: sim
       integer , Intent(in)                 :: i1,i2
       integer                              :: Isl

       !---- Local variables ----!
       integer               :: iss,ipass,j,k,im

       iss=1
       ipass=0
       call clear_Error()
       do
          ipass=ipass+1
          imdo:  do im=i1,i2
             Isl=0
             do j=1,3
                do k=1,3
                   if (sim(j,k) /= iss*Mod6(im,j,k)) cycle imdo
                end do
             end do
             Isl=iss*im
             exit
          end do imdo

          if (Isl /= 0) return

          if (ipass >=2 ) then
             Err_CFML%Msg=" Try to re-write your S.O. using a rotational part"
             if (i1 == 1 .and.  i2 == 24) then
                Err_CFML%Msg=trim(Err_CFML%Msg)//" identical to a S.O. of the space group P m -3 m"
             else if(i1 == 25 .and.  i2 == 36) then
                Err_CFML%Msg=trim(Err_CFML%Msg)//" identical to a S.O. of the space group P 6/m m m"
             else
                Err_CFML%Msg=trim(Err_CFML%Msg)//" identical to a S.O. of the space group P m -3 m or P 6/m m m"
             end if
             err_CFML%Ierr=1
             return
          end if
          iss=-1
       end do

    End Function SearchOp

    !!----
    !!---- Module Subroutine Read_SymTrans_Code(Code,N,Tr)
    !!----    character (len=*),          intent( in) :: Code
    !!----    integer,                    intent(out) :: N
    !!----    real(kind=cp),dimension(3), intent(out) :: Tr
    !!----
    !!----    Read a Code string for reference the symmetry operator and the
    !!----    Traslation applied.
    !!--<<        _2.555     : N_Op = 2, Tr=( 0.0, 0.0, 0.0)
    !!----        _3.456     : N_Op = 3, Tr=(-1.0, 0.0, 1.0)
    !!-->>
    !!----
    !!---- Update: April - 2005
    !!
    Module Subroutine Read_SymTrans_Code(Code,N,Tr)
       !---- Arguments ----!
       character (len=*),          intent( in) :: Code
       integer,                    intent(out) :: N
       real(kind=cp),dimension(3), intent(out) :: Tr

       !---- Local variables ----!
       character(len=20) :: car
       integer          :: i,j,k,n_ini,n_end,nt

       N=1
       Tr=0.0
       if (len_trim(code) <= 0) return

       car=adjustl(code)
       n_ini=index(car,"_")
       n_ini=n_ini+1

       !---- Found Number of Symmetry Operator ----!
       n_end=index(car,".")
       if (n_end ==0) n_end=len_trim(car)+1
       read (unit=car(n_ini:n_end-1),fmt=*) n

       !---- Found the Traslation ----!
       n_ini=index(car,".")
       if (n_ini /= 0) then
          n_ini=n_ini+1
          n_end=len_trim(car)
          read (unit=car(n_ini:n_end),fmt=*) nt
          i=nt/100
          j=mod(nt,100)/10
          k=nt-(i*100+j*10)
          i=i-5
          j=j-5
          k=k-5
          tr(1)=real(i)
          tr(2)=real(j)
          tr(3)=real(k)
       end if

    End Subroutine Read_SymTrans_Code

    Module Subroutine Write_SymTrans_Code(N,Tr,Codec,ChCode,Rcode)
       !---- Arguments ----!
       integer,                    intent(in)  :: N
       real(kind=cp),dimension(3), intent(in)  :: Tr
       character (len=*),optional, intent(out) :: Codec     !As proposed by RJA
       character (len=*),optional, intent(out) :: ChCode    !Like the old subroutine compatible with Read_SymTrans_Code
       real(kind=cp),    optional, intent(out) :: Rcode     !symcode as a real number

       !---- Local Variables ----!
       character(len=3)      :: car
       integer, dimension(3) :: i
       character(len=10)     :: tcode,code

       Code=" "
       if (N <= 0)  return
       if (N > 196) return !Up to 196 symmetry operators

       car="   "

       !---- Number of the Symmetry Operator ----!
       write(unit=car,fmt="(i3)") n
       car=adjustl(car)
       Code = trim(car)//'_'
       car="   "
       !---- Traslation Part ----!
       i=5+nint(tr)
       if (all(i == 5) .and. n ==1) then
          !>Atom at original position and no translations:  changed to code='.' RJA
          Code="."
          if(present(Rcode))  Rcode=1.555
          if(present(Codec))  Codec=code
          if(present(ChCode)) ChCode=" "
       else
          write(unit=car(1:1),fmt="(i1)") i(1)
          write(unit=car(2:2),fmt="(i1)") i(2)
          write(unit=car(3:3),fmt="(i1)") i(3)
          code=trim(code)//trim(car)
          tcode=code(1:index(code,'_')-1)//'.'//trim(car)
          if(present(Codec)) Codec=code
          if(present(Rcode)) read(tcode,'(f8.3)') rcode
          if(present(ChCode)) ChCode="_"//trim(tcode)
       end if

    End Subroutine Write_SymTrans_Code

 End Submodule gS_Symm_Symbols