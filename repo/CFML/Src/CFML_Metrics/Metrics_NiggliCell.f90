!!----
!!----
!!----
Submodule (CFML_Metrics) Metrics_NiggliCell
 Implicit none
 Contains
    !!----
    !!---- NIGGLI_CELL_ABC
    !!----    Calculates the Niggli cell when the input is a vector contatining
    !!----    cell parameters.
    !!----    Calls the subroutine Niggli_Cell_Nigglimat for
    !!----    the effective calculations
    !!----
    !!---- 10/04/2019
    !!
    Module Subroutine Niggli_Cell_ABC(VCell,Niggli_Point,Cell,Trans)
       !---- Arguments ----!
       real(kind=cp),dimension(6),              intent(in out) :: VCell
       real(kind=cp),dimension(5),    optional, intent(   out) :: Niggli_Point
       class(Cell_G_Type),            optional, intent(   out) :: Cell
       real(kind=cp), dimension(3,3), optional, intent(   out) :: Trans

       !---- Local variables ----!
       real(kind=cp), dimension(2,3) :: n_mat
       type(Cell_G_Type)             :: celda

       n_mat(1,1)=VCell(1)*VCell(1)
       n_mat(1,2)=VCell(2)*VCell(2)
       n_mat(1,3)=VCell(3)*VCell(3)
       n_mat(2,1)=VCell(2)*VCell(3)*cosd(VCell(4))
       n_mat(2,2)=VCell(1)*VCell(3)*cosd(VCell(5))
       n_mat(2,3)=VCell(1)*VCell(2)*cosd(VCell(6))

       if (present(Niggli_Point)) then
          if (present(trans)) then
             call Niggli_Cell_Mat(n_mat,Niggli_Point,celda,trans)
          else
             call Niggli_Cell_Mat(n_mat,Niggli_Point,celda)
          end if

       else if(present(trans)) then
          call Niggli_Cell_Mat(n_mat,cell=celda,trans=trans)

       else
          call Niggli_Cell_Mat(n_mat,cell=celda)
       end if
       if (Err_CFML%IErr /= 0) return

       !> Reconstruct the new cell (Niggli Cell)
       VCell(1) = sqrt(n_mat(1,1))
       VCell(2) = sqrt(n_mat(1,2))
       VCell(3) = sqrt(n_mat(1,3))
       VCell(4) = acosd(n_mat(2,1)/(VCell(2)*VCell(3)))
       VCell(5) = acosd(n_mat(2,2)/(VCell(1)*VCell(3)))
       VCell(6) = acosd(n_mat(2,3)/(VCell(1)*VCell(2)))

       !> The exactly type of cell is unknown
       if (present(cell)) then
          call Set_Crystal_Cell(Vcell(1:3),Vcell(4:6), Cell)
       end if

    End Subroutine Niggli_Cell_ABC

    !!----
    !!---- NIGGLI_CELL_PARAMS
    !!----    Calculates the Niggli cell when the input is the list of cell parameters
    !!----    provided as six scalars.
    !!----
    !!----    Calls the subroutine Niggli_Cell_Nigglimat for the effective calculations
    !!----
    !!---- 10/04/2019
    !!
    Module Subroutine Niggli_Cell_Params(A,B,C,Alpha,Beta,Gamma,Niggli_Point,Cell,Trans)
       !---- Arguments ----!
       real(kind=cp),                           intent(in out)  :: a,b,c,alpha,beta,gamma
       real(kind=cp), dimension(5),   optional, intent(   out)  :: Niggli_Point
       class(Cell_G_Type),            optional, intent(   out)  :: Cell
       real(kind=cp), dimension(3,3), optional, intent(   out)  :: Trans

       !--- Local variables ---!
       type(Cell_G_Type)             :: celda
       real(kind=cp), dimension(2,3) :: n_mat

       !> Init
       if ( alpha+beta < gamma+1.0_cp  .or. alpha+gamma < beta+1.0_cp .or. beta+gamma < alpha+1.0_cp) then
          Err_CFML%IErr=1
          Err_CFML%Msg="NIGGLI_CELL@METRICS: The provided angles cannot set a unit cell!"
          return
       end if

       call Set_Crystal_Cell((/a,b,c/),(/alpha,beta,gamma/), Celda)
       if (Err_CFML%IErr /= 0) return

       n_mat(1,1)=Celda%GD(1,1); n_mat(1,2)=Celda%GD(2,2); n_mat(1,3)=Celda%GD(3,3)
       n_mat(2,1)=Celda%GD(2,3); n_mat(2,2)=Celda%GD(1,3); n_mat(2,3)=Celda%GD(1,2)

       if (present(Niggli_Point)) then
          if (present(trans)) then
             call Niggli_Cell_Mat(n_mat,Niggli_Point,celda,trans)
          else
             call Niggli_Cell_Mat(n_mat,Niggli_Point,celda)
          end if

       else if(present(trans)) then
          call Niggli_Cell_Mat(n_mat,cell=celda,trans=trans)

       else
          call Niggli_Cell_Mat(n_mat,cell=celda)
       end if
       if (Err_CFML%IErr /= 0) return

       a=celda%cell(1); b=celda%cell(2); c=celda%cell(3)
       alpha=celda%ang(1); beta=celda%ang(2); gamma=celda%ang(3)

       if (present(cell)) then
          call Set_Crystal_Cell([a,b,c],[alpha,beta,gamma], Cell)
       end if

    End Subroutine Niggli_Cell_Params

    !!----
    !!---- NIGGLI_CELL_TYPE
    !!----    Calculates the Niggli cell when the input is an object of type Cell_Type
    !!----
    !!----    Calls the subroutine Niggli_Cell_Nigglimat for the effective calculations
    !!----
    !!---- 10/04/2019
    !!
    Module Subroutine Niggli_Cell_Type(Cell,Niggli_Point,Celln,Trans)
       !---- Arguments ----!
       class(Cell_G_Type),                      intent(in out ) :: cell
       real(kind=cp),dimension(5),    optional, intent(   out)  :: Niggli_Point
       class(Cell_G_Type),            optional, intent(   out)  :: celln
       real(kind=cp), dimension(3,3), optional, intent(   out)  :: trans

       !--- Local variables ---!
       type(Cell_G_Type)               :: celda
       real(kind=cp), dimension(2,3)   :: n_mat

       !> Init
       celda=cell
       n_mat(1,1)=Celda%GD(1,1); n_mat(1,2)=Celda%GD(2,2); n_mat(1,3)=Celda%GD(3,3)
       n_mat(2,1)=Celda%GD(2,3); n_mat(2,2)=Celda%GD(1,3); n_mat(2,3)=Celda%GD(1,2)

       if (present(Niggli_Point)) then
          if (present(trans)) then
             call Niggli_Cell_Mat(n_mat,Niggli_Point,celda,trans)
          else
             call Niggli_Cell_Mat(n_mat,Niggli_Point,celda)
          end if

       else if(present(trans)) then
          call Niggli_Cell_Mat(n_mat,cell=celda,trans=trans)
       else
          call Niggli_Cell_Mat(n_mat,cell=celda)
       end if
       if (Err_CFML%IErr /= 0) return

       call Set_Crystal_Cell(celda%cell,celda%ang,cell)

       !> Problem with the type of celln
       if (present(celln)) then
          call Set_Crystal_Cell(celda%cell,celda%ang,celln)
       end if

    End Subroutine Niggli_Cell_Type

    !!----
    !!---- NIGGLI_CELL_VECT
    !!----    Calculates the Niggli cell when the input is given as three vectors
    !!----    in Cartesian components. A test of linear indenpendency is performed.
    !!----
    !!----    Calls the subroutine Niggli_Cell_Nigglimat for the effective calculations
    !!----
    !!---- 10/04/2019
    !!
    Module Subroutine Niggli_Cell_Vect(Vec1,Vec2,Vec3,Niggli_Point,Cell,Trans)
       !---- Arguments ----!
       real(kind=cp),dimension(3),                intent(in)     :: Vec1, Vec2, Vec3
       real(kind=cp),dimension(5),      optional, intent(out)    :: Niggli_Point
       class(Cell_G_Type),              optional, intent(out)    :: cell
       real(kind=cp), dimension(3,3),   optional, intent(out)    :: trans

       !--- Local variables ---!
       type(Cell_G_Type)             :: celda
       real(kind=cp), dimension(2,3) :: n_mat
       real(kind=cp)                 :: det

       !> Init
       det=Determ_V(Vec1,Vec2,Vec3)
       if (abs(det) < 0.0001) then
          Err_CFML%IErr=1
          ERR_CFML%Msg="NIGGLI_CELL@METRICS: The three input vectors aren't linearly independent!"
          return
       end if

       n_mat(1,1)=dot_product(Vec1,Vec1)
       n_mat(1,2)=dot_product(Vec2,Vec2)
       n_mat(1,3)=dot_product(Vec3,Vec3)

       n_mat(2,1)=dot_product(Vec2,Vec3)
       n_mat(2,2)=dot_product(Vec1,Vec3)
       n_mat(2,3)=dot_product(Vec1,Vec2)

       if (present(Niggli_Point)) then
          if (present(trans)) then
             call Niggli_Cell_Mat(n_mat,Niggli_Point,celda,trans)
          else
             call Niggli_Cell_Mat(n_mat,Niggli_Point,celda)
          end if

       else if(present(trans)) then
          call Niggli_Cell_Mat(n_mat,cell=celda,trans=trans)
       else
          call Niggli_Cell_Mat(n_mat,cell=celda)
       end if
       if (Err_CFML%IErr /= 0) return

       !> We don't know which type of cell was used.
       if (present(cell)) then
          call Set_Crystal_Cell(celda%cell,celda%ang, Cell)
       end if

    End Subroutine Niggli_Cell_Vect

    !!----
    !!---- NIGGLI_CELL_MAT
    !!----    Calculates the Niggli cell when the input is the Niggli Matrix
    !!----    (part of the metrics) of a primitive cell.
    !!----    Applies the scalar algorithm of I. Krivy and B. Gruber,
    !!----    Acta Cryst A32, 297 (1976)
    !!----
    !!----    If Trans is present, Cell should also be present.
    !!----
    !!---- 10/04/2019
    !!
    Module Subroutine Niggli_Cell_Mat(N_Mat,Niggli_Point,Cell,Trans)    !Scalar algorithm
       !---- Arguments ----!
       real(kind=cp),dimension(2,3),              intent(in out) :: N_mat
       real(kind=cp),dimension(5),      optional, intent(out)    :: Niggli_Point
       class(Cell_G_Type),              optional, intent(out)    :: cell
       real(kind=cp), dimension(3,3),   optional, intent(out)    :: trans

       !--- Local variables ---!
       type(Cell_G_Type)             :: Celda
       real(kind=cp)                 :: A,B,C,u,v,w,epss
       real(kind=cp), dimension(3)   :: cel,ang
       real(kind=cp), dimension(3,3) :: trm
       logical                       :: ok
       integer                       :: iu,iv,iw, ncount ! ncount is the counter no more that Numiter=100
                                                         ! iterations are permitted. In case of exhausting
                                                         ! the iteration Err_Crys=.true. but the current
                                                         ! cell is output anyway

       real(kind=cp), parameter       :: EPR=0.0001      !Relative epsilon
       integer, parameter             :: NUMITER=100

       ! N is a Niggli cell of L if  (i) it is as Buerger cell of L and
       !                            (ii) |90-alpha| + |90-beta| + |90-gamma| -> maximum
       !                  / a.a  b.b  c.c \       /  s11  s22  s33 \
       !   Niggli matrix  |               |   =   |                |
       !                  \ b.c  a.c  a.b /       \  s23  s13  s12 /
       !
       ! I. Krivy and B. Gruber, Acta Cryst A32, 297 (1976)
       ! Krivy-Gruber algorithms safely implemented (suggestion of Ralf Grosse-Kunsleve)
       ! R.W. Grosse-Kunstleve, N. K. Sauter and P. D. Adams, Acta Cryst A60, 1-6 (2004)
       ! Epsilon: e
       !    x < y -> x < y-e;    x > y -> y < x-e
       !   x <= y -> .not. y < x-e;   x >= y -> .not. x < y-e
       !   x == y -> .not. (x < y-e .or. y < x-e)
       !

       !> Init
       A=n_mat(1,1)
       B=n_mat(1,2)
       C=n_mat(1,3)
       u=2.0*n_mat(2,1)
       v=2.0*n_mat(2,2)
       w=2.0*n_mat(2,3)
       epss=epr*(A*B*C)**(1.0/6.0)
       ncount=0
       ok=.true.

       if (present(trans)) then
          !> Construct the Celda from its Niggli parameters
          cel(1) = sqrt(A)
          cel(2) = sqrt(B)
          cel(3) = sqrt(C)
          ang(1) = acosd(u/(cel(2)*cel(3)*2.0))
          ang(2) = acosd(v/(cel(1)*cel(3)*2.0))
          ang(3) = acosd(w/(cel(1)*cel(2)*2.0))
          call Set_Crystal_Cell(cel,ang, Celda)
       end if

       do
          ncount=ncount+1
          if (ncount > NUMITER) then
             ok=.false.
             exit
          end if

          !> if(A > B .or. ( A == B  .and. abs(u) > abs(v)) ) then  ! A1
          if (B < A-epss .or. ( .not.( A < B-epss .or. B < A-epss)  .and. &
              abs(v) < abs(u)-epss ) ) then  ! A1
             call swap(A,B)
             call swap(u,v)
          end if

          !>if(B > C .or. ( B == C .and. abs(v) > abs(w)) ) then  ! A2
          if (C < B-epss .or. ( .not.( C < B-epss .or. B < C-epss) .and. &
              abs(w) < abs(v)-epss) ) then  ! A2
             call swap(B,C)
             call swap(v,w)
             cycle
          end if

          !> if (u*v*w > 0.0) then                                 ! A3
          iu=1; iv=1; iw=1
          if ( u < -epss) iu=-1
          if ( v < -epss) iv=-1
          if ( w < -epss) iw=-1
          if (abs(u) < epss) iu=0
          if (abs(v) < epss) iv=0
          if (abs(w) < epss) iw=0
          if (iu*iv*iw > 0) then                                      ! A3
             u=abs(u)
             v=abs(v)
             w=abs(w)
          else                                                        ! A4
             u=-abs(u)
             v=-abs(v)
             w=-abs(w)
          end if

          !> if( abs(u) > B .or. ( u == B .and. 2.0*v < w) .or. ( u == -B .and. w < 0.0)) then  ! A5
          if (B < abs(u)-epss .or. ( .not.(u < B-epss .or. B < u-epss) .and. &
              2.0*v < w-epss) .or. ( .not.(u < -B-epss .or. -B < u-epss) .and. &
              w < -epss)) then  ! A5
             iu=1
             if ( u < -epss) iu=-1
             C = B+C - u * iu
             v =  v  - w * iu
             u = u - 2.0*B*iu
             cycle
          end if

          !> if( abs(v) > A .or. ( v == A .and. 2.0*u < w) .or. ( v == -A .and. w < 0.0)) then  ! A6
          if (A < abs(v)-epss .or. (.not. (v < A-epss .or. A < v-epss) .and. &
              2.0*u < w-epss) .or. ( .not.( v < -A-epss .or. -A < v-epss) .and. &
              w < -epss)) then  ! A6
             iv=1
             if( v < -epss) iv=-1
             C = A+C - v * iv
             u =  u  - w * iv
             v = v - 2.0*A*iv
             cycle
          end if

          !> if( abs(w) > A .or. ( w == A .and. 2.0*u < v) .or. ( w == -A .and. v < 0.0)) then  ! A7
          if (A < abs(w)-epss .or. ( .not. (w < A-epss .or. A < w-epss) .and. &
             2.0*u < v-epss) .or. ( .not. (w < -A-epss .or. -A < w-epss) .and. &
             v < -epss)) then  ! A7
             iw=1
             if( w < -epss) iw=-1
             B = A+B - w * iw
             u =  u  - v * iw
             w = w - 2.0*A*iw
             cycle
          end if

          !> if(u+v+w+A+B < 0.0 .or. (u+v+w+A+B == 0.0 .and. 2.0*(A+v)+w > 0.0 )) then  ! A8
          if (u+v+w+A+B < -epss .or. ( abs(u+v+w+A+B) < epss .and. 2.0*(A+v)+w > epss )) then  ! A8
             C=A+B+C+u+v+w
             u=2.0*B+u+w
             v=2.0*A+v+w
             cycle
          end if

          exit
       end do

       !> Reconstruct the new Niggli matrix
       n_mat(1,1)=A; n_mat(1,2)=B; n_mat(1,3)=C
       n_mat(2,1)=0.5*u; n_mat(2,2)=0.5*v; n_mat(2,3)=0.5*w

       if (.not. ok) then
          Err_CFML%IErr=1
          Err_CFML%Msg="NIGGLI_CELL_MAT@METRICS: The limit of iterations has been reached!"
          return
       end if

       if (present(Niggli_point)) then
          Niggli_point(1)= A/C
          Niggli_point(2)= B/C
          Niggli_point(3)= u/C
          Niggli_point(4)= v/C
          Niggli_point(5)= w/C
       end if

       if (present(cell)) then
          !> Reconstruct the new cell (Niggli Cell)
          cel(1) = sqrt(A)
          cel(2) = sqrt(B)
          cel(3) = sqrt(C)
          ang(1) = acosd(u/(cel(2)*cel(3)*2.0))
          ang(2) = acosd(v/(cel(1)*cel(3)*2.0))
          ang(3) = acosd(w/(cel(1)*cel(2)*2.0))
          call Set_Crystal_Cell(cel,ang, Cell)

          if (present(trans)) then
             trm=Get_Transfm_Matrix(celda,cell)
             if (Err_CFML%IErr==0) then
                trans=trm
             else
                trans=identity
             end if
          end if
       end if

    End Subroutine Niggli_Cell_Mat

End Submodule Metrics_NiggliCell
