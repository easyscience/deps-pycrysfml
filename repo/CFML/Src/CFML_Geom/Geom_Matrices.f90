 Submodule (CFML_Geom) Geom_Matrices
    implicit none
   contains

    !!----
    !!---- Pure Module Function Matrix_Phithechi(Phi,Theta,Chi,Code) Result(M)
    !!----    real(kind=cp),                intent(in) :: Phi
    !!----    real(kind=cp),                intent(in) :: Theta
    !!----    real(kind=cp),                intent(in) :: Chi
    !!----    character(len=*), optional,   intent(in) :: Code
    !!----    real(kind=cp), dimension(3,3)            :: M    ! Put -> Active Rotation Matrix
    !!----
    !!----    Calculate the active rotation matrix corresponding to the composition
    !!----    of a positive rotation around z of angle Chi, followed by a positive rotation
    !!----    of angle Theta around the y-axis and a subsequent positive rotation of angle Phi
    !!----    around z. "Positive" means counter-clockwise.
    !!----    The matrix is M = Rz(Phi) . Ry(Theta) . Rz(Chi)
    !!----    The colums represent the components of the unitary vectors {u,v,w} that
    !!----    may be considered as an alternative orthonormal frame to the canonical {i,j,k}.
    !!----    Applying the matrix M to a point in {i,j,k} gives another point in {i,j,k} obtained
    !!----    by the successive application of the three rotations given above. The transpose
    !!----    (inverse) of the M-matrix, when applied to a point in {i,j,k}, gives the coordinates
    !!----    of the same point referred to the frame {u,v,w}. This transpose matrix corresponds
    !!----    to a passive (change or Cartesian frame) rotation leaving the points in the same
    !!----    position with respect to the  {i,j,k} frame.
    !!----    The matrix M when applied to a column vector containing the coordinates of a point
    !!----    with respect to the {u,v,w} frame provides the coordinates of the same point with
    !!----    respect to the {i,j,k} frame.
    !!----    If Code =="R" or Blank or not present then the input angles are given in radians.
    !!----    If Code =="D" then the input angles are given in degrees (Phi, Theta, Chi).
    !!----
    !!---- Update: February - 2005
    !!
    Pure Module Function Matrix_Phithechi(Phi,Theta,Chi,Code) Result(Mt)
       !---- Arguments ----!
       real(kind=cp),                intent(in) :: Phi
       real(kind=cp),                intent(in) :: Theta
       real(kind=cp),                intent(in) :: Chi
       character(len=*), optional,   intent(in) :: Code
       real(kind=cp), dimension(3,3)            :: Mt

       !---- Local Variables ----!
       real(kind=cp) :: p,t,c

       if (present(code)) then
          select case (code(1:1))
             case("D","d") ! degrees
                p=Phi*to_rad
                t=Theta*to_rad
                c=Chi*to_rad
             case default ! radians
                p=Phi
                t=Theta
                c=Chi
          end select
       else
          !---- radians ----!
          p=Phi
          t=Theta
          c=Chi
       end if
       Mt(1,1)= cos(p)*cos(t)*cos(c)-sin(p)*sin(c)    !
       Mt(2,1)= sin(p)*cos(t)*cos(c)+cos(p)*sin(c)    !  u
       Mt(3,1)=-sin(t)*cos(c)                         !
       Mt(1,2)=-cos(p)*cos(t)*sin(c)-sin(p)*cos(c)    !
       Mt(2,2)=-sin(p)*cos(t)*sin(c)+cos(p)*cos(c)    !  v
       Mt(3,2)= sin(t)*sin(c)                         !
       Mt(1,3)= cos(p)*sin(t)                         !
       Mt(2,3)= sin(p)*sin(t)                         !  w
       Mt(3,3)= cos(t)                                !

    End Function Matrix_Phithechi

    !!----
    !!---- Pure Module Function Matrix_Rx(Ang,Code) Result(M)
    !!----    real(kind=cp),                      intent(in) :: Ang
    !!----    character(len=*), optional,         intent(in) :: Code
    !!----    real(kind=cp), dimension(3,3)                  :: M    ! Put -> Active Rotation Matrix
    !!----
    !!----    Calculate the active rotation matrix corresponding to the positive rotation
    !!----    of an angle Phi around the x-axis. The transpose matrix corresponds to a
    !!----    passive rotation that changes the orthogonal system to {u,v,w} leaving the point
    !!----    at the same position w.r.t. the canonical {i,j,k} frame.
    !!----    If Code =="R" or Blank or not present then the input angle is given in radians.
    !!----    If Code =="D" then the input angle is given in degrees.
    !!----
    !!---- Update: February - 2005
    !!
    Pure Module Function Matrix_Rx(Ang,Code) Result(Mt)
       !---- Arguments ----!
       real(kind=cp),               intent(in) :: Ang
       character(len=*), optional,  intent(in) :: Code
       real(kind=cp), dimension(3,3)           :: Mt

       !---- Local Variables ----!
       real(kind=cp) :: p

       if (present(code)) then
          select case (code(1:1))
             case("D","d") ! degrees
                p=Ang*to_rad
             case default ! radians
                p=Ang
          end select
       else
          !---- radians ----!
          p=Ang
       end if
       Mt(1,1)= 1.0        !              1  0  0
       Mt(2,1)= 0.0        !  u           0  c -s     Rx
       Mt(3,1)= 0.0        !              0  s  c
       Mt(1,2)= 0.0        !
       Mt(2,2)= cos(p)     !  v
       Mt(3,2)= sin(p)     !
       Mt(1,3)= 0.0        !
       Mt(2,3)=-sin(p)     !  w
       Mt(3,3)= cos(p)     !

    End Function Matrix_Rx

    !!----
    !!---- Pure Module Function Matrix_Ry(Ang,Code) Result(M)
    !!----    real(kind=cp),                      intent(in) :: Ang
    !!----    character(len=*), optional,         intent(in) :: Code
    !!----    real(kind=cp), dimension(3,3)                  :: M    ! Put -> Active Rotation Matrix
    !!----
    !!----    Calculate the active rotation matrix corresponding to the positive rotation
    !!----    of an angle Phi around the y-axis. The transpose matrix corresponds to a
    !!----    passive rotation that changes the orthogonal system to {u,v,w} leaving the point
    !!----    at the same position w.r.t. the canonical {i,j,k} frame.
    !!----    If Code =="R" or Blank or not present then the input angle is given in radians.
    !!----    If Code =="D" then the input angle is given in degrees.
    !!----
    !!---- Update: February - 2005
    !!
    Pure Module Function Matrix_Ry(Ang,Code) Result(Mt)
       !---- Arguments ----!
       real(kind=cp),               intent(in) :: Ang
       character(len=*), optional,  intent(in) :: Code
       real(kind=cp), dimension(3,3)           :: Mt

       !---- Local Variables ----!
       real(kind=cp) :: p

       if (present(code)) then
          select case (code(1:1))
             case("D","d") ! degrees
                p=Ang*to_rad
             case default ! radians
                p=Ang
          end select
       else
          !---- radians ----!
          p=Ang
       end if
       Mt(1,1)= cos(p)  !             c  0  s
       Mt(2,1)= 0.0     !  u          0  1  0      Ry
       Mt(3,1)=-sin(p)  !            -s  0  c
       Mt(1,2)= 0.0     !
       Mt(2,2)= 1.0     !  v
       Mt(3,2)= 0.0     !
       Mt(1,3)= sin(p)  !
       Mt(2,3)= 0.0     !  w
       Mt(3,3)= cos(p)  !

    End Function Matrix_Ry

    !!----
    !!---- Pure Module Function Matrix_Rz(Ang,Code) Result(M)
    !!----    real(kind=cp),                      intent(in) :: Ang
    !!----    character(len=*), optional,         intent(in) :: Code
    !!----    real(kind=cp), dimension(3,3)                  :: M    ! Put -> Active Rotation Matrix
    !!----
    !!----    Calculate the active rotation matrix corresponding to the positive rotation
    !!----    of an angle Phi around the z-axis. The transpose matrix corresponds to a
    !!----    passive rotation that changes the orthogonal system to {u,v,w} leaving the point
    !!----    at the same position w.r.t. the canonical {i,j,k} frame.
    !!----    If Code =="R" or Blank or not present then the input angle is given in radians.
    !!----    If Code =="D" then the input angle is given in degrees.
    !!----
    !!---- Update: February - 2005
    !!
    Pure Module Function Matrix_Rz(Ang,Code) Result(Mt)
       !---- Arguments ----!
       real(kind=cp),               intent(in) :: Ang
       character(len=*), optional,  intent(in) :: Code
       real(kind=cp), dimension(3,3)           :: Mt

       !---- Local Variables ----!
       real(kind=cp) :: p

       if (present(code)) then
          select case (code(1:1))
             case("D","d") ! degrees
                p=Ang*to_rad
             case default ! radians
                p=Ang
          end select
       else
          !---- radians ----!
          p=Ang
       end if
       Mt(1,1)= cos(p)  !                 c  -s  0
       Mt(2,1)= sin(p)  !  u              s   c  0    Rz
       Mt(3,1)= 0.0     !                 0   0  1
       Mt(1,2)=-sin(p)  !
       Mt(2,2)= cos(p)  !  v
       Mt(3,2)= 0.0     !
       Mt(1,3)= 0.0     !
       Mt(2,3)= 0.0     !  w
       Mt(3,3)= 1.0     !

    End Function Matrix_Rz

    !!---- Pure Module Function Set_Rotation_Matrix(ang) Result(Rot)
    !!----   real(kind=cp), dimension(3),   intent( in) :: ang
    !!----   real(kind=cp), dimension(3,3)              :: Rot
    !!----
    !!----  Subroutine calculating the rotation matrix Rot corresponding to
    !!----  the application (active rotations) of the following succesive rotations:
    !!----
    !!----  Rot = Rx(ang(3)) . Ry(ang(2)) . Rz(ang(1))
    !!----
    !!----    Created: October 2009  (JRC)
    !!----    Updated: March 2013 (JRC)
    !!----
    Pure Module Function Set_Rotation_Matrix(ang) Result(Rot)
      real(kind=cp), dimension(3),   intent( in) :: ang
      real(kind=cp), dimension(3,3)              :: Rot
      !Local variables
      real(kind=cp), dimension(3,3) :: Rx,Ry,Rz
      Rx=Matrix_Rx(ang(1),"D")
      Ry=Matrix_Ry(ang(2),"D")
      Rz=Matrix_Rz(ang(3),"D")
      Rot=Matmul(Rx,matmul(Ry,Rz))
    End Function Set_Rotation_Matrix

    !!---- Subroutine Get_Matrix_moving_v_to_u(v,u,R,w,ang)
    !!----   real(kind=cp), dimension(3),           intent(in)  :: v,u   !Starting and final vectors
    !!----   real(kind=cp), dimension(3,3),         intent(out) :: R     !Rotation matrix moving v to u:  u=Rv
    !!----   real(kind=cp), optional,               intent(out) :: ang   !angle between the two vectors
    !!----   real(kind=cp), optional,dimension(3),  intent(out) :: w     !axis normal to plane of the two vectors
    !!----
    !!----   Subroutine to get the orthogonal matrix that rotates a vector v
    !!----   to orient it along the vector u. Makes use of Cross_Product and
    !!----   Rot_Gibbs_Matrix (Gibbs matrix)
    !!----
    !!----    Created: February 2010 (JRC)
    !!----    Updated: March 2013 (JRC)
    !!----
    !!
    Module Subroutine Get_Matrix_moving_v_to_u(v,u,R,w,ang)
      real(kind=cp), dimension(3),           intent(in)  :: v,u
      real(kind=cp), dimension(3,3),         intent(out) :: R
      real(kind=cp), optional,               intent(out) :: ang
      real(kind=cp), optional,dimension(3),  intent(out) :: w
      !--- Local variables ---!
      integer                        :: i,iu,iv
      real(kind=cp), parameter       :: ep=1.0e-5_cp
      real(kind=cp)                  :: mv,mu,mvu,phi,c
      logical                        :: co_linear
      real(kind=cp), dimension(3)    :: vu
      integer, dimension(1)          :: im
      real(kind=cp), parameter, dimension(3,3):: ident=reshape([ 1.0_cp,0.0_cp,0.0_cp, &
                                                                 0.0_cp,1.0_cp,0.0_cp, &
                                                                 0.0_cp,0.0_cp,1.0_cp],[3,3])

      if(present(ang)) ang=0.0
      if(present(w))   w=0.0
      !First determine if the two input vectors are co-linear
      im=maxloc(abs(v))
      iv=im(1)
      im=maxloc(abs(u))
      iu=im(1)
      co_linear=.true.
      if(iu == iv) then ! may be co-linear
        if(abs(u(iu)) > ep) then
          c=v(iv)/u(iu)
          do i=1,3
            if(abs( v(i)-c*u(i) ) > ep ) then
               co_linear=.false.
               exit
            end if
          end do
        end if
      else
        co_linear=.false.
      end if
      if(co_linear) then
        mvu=v(iv)*u(iu)
        if(mvu < 0.0) then   !opposed vectors
          R=-ident
        else                 !parallel vectors
          R=ident
        end if
      else
        ! non co-linear
        vu=Cross_Product(v,u)      !Rotation axis
        mv=sqrt(dot_product(v,v))
        mu=sqrt(dot_product(u,u))
        phi=dot_product(u,v)/mv/mu
        phi=acosd(phi)        !Angle between the two input vectors
        R=Rot_Gibbs_Matrix(vu,phi)  !Gibbs matrix
        if(present(ang)) ang=phi
        if(present(w)) w=vu
      end if
    End Subroutine Get_Matrix_moving_v_to_u

 End Submodule Geom_Matrices