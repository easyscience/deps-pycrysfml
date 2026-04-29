!!----
!!----
!!----
!!
 SubModule (CFML_geom) Geom_Angles
    implicit none
   Contains
    !!----
    !!---- Pure Module Function Angle_Dihedral(U,V,W) Or (Ri,Rj,Rk,Rn)   Result(Angle)
    !!----    real(kind=cp), dimension(3), intent( in) :: u       !  In -> Vector 1
    !!----    real(kind=cp), dimension(3), intent( in) :: v       !  In -> Vector 2
    !!----    real(kind=cp), dimension(3), intent( in) :: w       !  In -> Vector 3
    !!----    or
    !!----    real(kind=cp), dimension(3), intent( in) :: ri      !  In -> Vector position ri
    !!----    real(kind=cp), dimension(3), intent( in) :: rj      !  In -> Vector position rj
    !!----    real(kind=cp), dimension(3), intent( in) :: rk      !  In -> Vector position rk
    !!----    real(kind=cp), dimension(3), intent( in) :: rl      !  In -> Vector position rn
    !!----    real(kind=cp)                            :: angle   ! Out -> Dihedral angle
    !!----
    !!----    Calculates the dihedral angle between planes "u-v" and "v-w", where vectors U,V,W
    !!----    are given in cartesian components.
    !!----    Calculates the dihedral angle corresponding to the four points (ri,rj,rk,rn)
    !!----    given in cartesian components. The definition used for the dihedral angle
    !!----    is the following:
    !!--<<
    !!----    Phi(i,j,k,n) = acos { (rij x rjk) (rjk x rkn) / |rij x rjk| / |rjk x rkn| }
    !!----
    !!----    with this definition the sign of Phi is positive if the vector product
    !!----    (rij x rjk) x (rjk x rkn) is in the same direction as rjk, and negative if
    !!----    the direction is opposite.
    !!-->>
    !!----
    !!---- Update: February - 2005
    !!

    !!--++
    !!--++ Pure Module Function Angle_Dihedral_Ijkn(Ri,Rj,Rk,Rn) Result(Angle)
    !!--++    real(kind=cp), dimension(3), intent( in) :: ri       !  In -> Vector position ri
    !!--++    real(kind=cp), dimension(3), intent( in) :: rj       !  In -> Vector position rj
    !!--++    real(kind=cp), dimension(3), intent( in) :: rk       !  In -> Vector position rk
    !!--++    real(kind=cp), dimension(3), intent( in) :: rl       !  In -> Vector position rn
    !!--++    real(kind=cp)                            :: angle    ! Out -> Dihedral angle
    !!--++
    !!--++    (OVERLOADED)
    !!--++    Calculates the dihedral angle corresponding to the four points (ri,rj,rk,rn)
    !!--++    given in cartesian components. The definition used for the dihedral angle
    !!--++    is the following:
    !!--++
    !!--++    Phi(i,j,k,n) = acos { (rij x rjk) (rjk x rkn) / |rij x rjk| / |rjk x rkn| }
    !!--++
    !!--++    with this definition the sign of Phi is positive if the vector product
    !!--++    (rij x rjk) x (rjk x rkn) is in the same direction as rjk, and negative if
    !!--++    the direction is opposite.
    !!--++
    !!--++ Update: May - 2020
    !!
    Pure Module Function Angle_Dihedral_Ijkn(ri,rj,rk,rn) result(angle)
       !---- Arguments ----!
       real(kind=cp), dimension(3), intent( in) :: ri,rj,rk,rn
       real(kind=cp)                            :: angle
       angle=Angle_Dihedral_Uvw(rj-ri ,rk-rj, rn-rk )
    End Function Angle_Dihedral_Ijkn

    !!--++
    !!--++ Pure Module Function Angle_Dihedral_Uvw(U,V,W) Result(Angle)
    !!--++    real(kind=cp), dimension(3), intent( in) :: u       !  In -> Vector 1
    !!--++    real(kind=cp), dimension(3), intent( in) :: v       !  In -> Vector 2
    !!--++    real(kind=cp), dimension(3), intent( in) :: w       !  In -> Vector 3
    !!--++    real(kind=cp)                            :: angle   ! Out -> Dihedral angle
    !!--++
    !!--++    (OVERLOADED)
    !!--++    Calculates the dihedral angle between planes u-v and v-w
    !!--++    Vectors u,v,w are given in cartesian components.
    !!--++
    !!--++ Update: February - 2005
    !!
    Pure Module Function Angle_Dihedral_Uvw(u,v,w) result(angle)
       !---- Argument ----!
       real(kind=cp), dimension(3), intent( in) :: u,v,w
       real(kind=cp)                            :: angle

       !---- Local variables ----!
       real(kind=cp)               :: uvmod, vwmod, sig
       real(kind=cp), dimension(3) :: uv,vw

       angle=0.0_cp

       uv=cross_product(u,v)
       vw=cross_product(v,w)
       sig = -sign(1.0_cp, dot_product(cross_product(uv,vw),v))
       uvmod=sqrt(dot_product(uv,uv))
       vwmod=sqrt(dot_product(vw,vw))
       if (uvmod < eps .or. vwmod < eps) return
       angle=acosd(dot_product(uv,vw)/uvmod/vwmod)*sig

    End Function Angle_Dihedral_Uvw

    !!----
    !!---- Pure Module Function Angle_Mod(X) Result (Y)
    !!----     real(kind=cp),               intent(in) :: x
    !!----                  or
    !!----     real(kind=cp), dimension(:), intent(in) :: x
    !!----
    !!----     Calculates the angle [-pi,pi)
    !!----
    !!---- Update: February - 2005
    !!

    !!--++
    !!--++ Pure Module Function Angle_Modn(Angle) Result(AngMod)
    !!--++    real(kind=cp), intent(in) :: Angle    !  In/Out -> Angle
    !!--++
    !!--++    (OVERLOADED)
    !!--++    Transforms angle in radians between -pi and +pi
    !!--++
    !!--++ Update: February - 2005
    !!
    Pure Module Function Angle_ModN(Angle) Result(AngMod)
       !---- Arguments ----!
       real(kind=cp), intent(in) :: Angle
       real(kind=cp)             :: AngMod

       AngMod=mod(angle+6.0*pi,2.0*pi)
       if (angmod > pi) angmod=angmod-2.0*pi

    End Function Angle_ModN

    !!--++
    !!--++ Pure Module Function Angle_Modv(V_Angle) Result(VAngMod)
    !!--++    real(kind=cp), dimension(:), intent(in) :: V_Angle
    !!--++
    !!--++    (OVERLOADED)
    !!--++    Transforms angles in radians between -pi and +pi
    !!--++
    !!--++ Update: February - 2005
    !!
    Pure Module Function Angle_ModV(V_Angle) Result(VAngMod)
       !---- Arguments ----!
       real(kind=cp), dimension(:),intent(in) :: V_Angle
       real(kind=cp), dimension(size(V_Angle)):: VAngMod

       !---- Local Variables ----!
       integer :: i

       VAngMod=mod(V_Angle+6.0*pi,2.0*pi)
       do i=1,size(V_Angle)
          if (VAngMod(i) > pi) VAngMod(i)=VAngMod(i)-2.0*pi
       end do

    End Function Angle_ModV

    !!----
    !!---- Pure Module Function Angle_Uv(U,V,G) Result(Angle)
    !!----    integer/real(kind=cp), dimension(:), intent( in)     :: u      !  In -> Vector 1
    !!----    integer/real(kind=cp), dimension(:), intent( in)     :: v      !  In -> Vector 2
    !!----    real(kind=cp), dimension(:,:), intent( in), optional :: g      !  In -> Metric tensor
    !!----    real(kind=cp)                                        :: angle  ! Out -> Angle
    !!----
    !!----    Calculates the angle between vectors u and v given in cartesian
    !!----    components. If g is not given cartesian components are assumed.
    !!----
    !!---- Update: February - 2005
    !!

    !!--++
    !!--++ Pure Module Function Angle_UvI(Ui,Vi,G) Result(Angle)
    !!--++    integer, dimension(:),                   intent(in) :: ui      !  In -> Vector 1
    !!--++    integer, dimension(:),                   intent(in) :: vi      !  In -> Vector 2
    !!--++    real(kind=cp), dimension(:,:), optional, intent(in) :: g       !  In -> Metric tensor
    !!--++    real(kind=cp)                                       :: angle   ! Out -> Angle
    !!--++
    !!--++    (OVERLOADED)
    !!--++    Calculates the angle between vectors u and v given in cartesians
    !!--++    or fractional components. If g is not given cartesian components
    !!--++    are assumed.
    !!--++
    !!--++ Update: February - 2005
    !!
    Pure Module Function Angle_UvI(Ui,Vi,G) Result(Angle)
       !---- Argument ----!
       integer, dimension(:),   intent( in)                 :: ui
       integer, dimension(:),   intent( in)                 :: vi
       real(kind=cp), dimension(:,:), intent( in), optional :: g   !metric tensor
       real(kind=cp)                                        :: angle

       !---- Local variables ----!
       real(kind=cp)                      :: umod, vmod
       real(kind=cp), dimension(size(ui)) :: u
       real(kind=cp), dimension(size(vi)) :: v

       angle=0.0

       u=real(ui)
       v=real(vi)

       if (present(g)) then
          umod = sqrt(dot_product(u,matmul(g,u)))
          vmod = sqrt(dot_product(v,matmul(g,v)))
          if (umod < eps .or. vmod < eps) return
          angle=acosd(dot_product(u,matmul(g,v))/umod/vmod)
       else
          umod=sqrt(dot_product(u,u))
          vmod=sqrt(dot_product(v,v))
          if (umod < eps .or. vmod < eps) return
          angle=acosd(dot_product(u,v)/umod/vmod)
       end if

    End Function Angle_uvi

    !!--++
    !!--++ Pure Module Function Angle_Uvr(U,V,G) Result(Angle)
    !!--++    real(kind=cp), dimension(:), intent( in)             :: u      !  In -> Vector 1
    !!--++    real(kind=cp), dimension(:), intent( in)             :: v      !  In -> Vector 2
    !!--++    real(kind=cp), dimension(:,:), intent( in), optional :: g      !  In -> Metric tensor
    !!--++    real(kind=cp)                                        :: angle  ! Out -> Angle
    !!--++
    !!--++    (OVERLOADED)
    !!--++    Calculates the angle between vectors u and v given in cartesian
    !!--++    or fractional components. If g is not given cartesian components
    !!--++    are assumed.
    !!--++
    !!--++ Update: February - 2005
    !!
    Pure Module Function Angle_UvR(u,v,g) result(angle)
       !---- Argument ----!
       real(kind=cp), dimension(:),   intent( in)           :: u
       real(kind=cp), dimension(:),   intent( in)           :: v
       real(kind=cp), dimension(:,:), intent( in), optional :: g   !metric tensor
       real(kind=cp)                                        :: angle

       !---- Local variables ----!
       real(kind=cp)   :: umod, vmod

       angle=0.0

       if (present(g)) then
          umod = sqrt(dot_product(u,matmul(g,u)))
          vmod = sqrt(dot_product(v,matmul(g,v)))
          if (umod < eps .or. vmod < eps) return
          angle=acosd(dot_product(u,matmul(g,v))/umod/vmod)
       else
          umod=sqrt(dot_product(u,u))
          vmod=sqrt(dot_product(v,v))
          if (umod < eps .or. vmod < eps) return
          angle=acosd(dot_product(u,v)/umod/vmod)
       end if

    End Function Angle_uvr

    !!----
    !!---- Subroutine Angle_and_Sigma(Cellp,DerM,x1,x0,x2,s1,s0,s2,ang,s)
    !!----    Type(Cell_G_Type),         intent(in)  :: Cellp         ! Cell object
    !!----    real(kind=cp), dimension(3,3,6), intent(in)  :: DerM          ! Matrix of derivatives of Cellp%Cr_Orth_cel
    !!----    real(kind=cp), dimension(3),     intent(in)  :: x0,x1,x2      ! Three points in fractional coordinates and sigmas
    !!----    real(kind=cp), dimension(3),     intent(in)  :: s0,s1,s2      ! Sigmas of the three points
    !!----    real(kind=cp),                   intent(out) :: ang,s         ! Angle and sigma
    !!----
    !!---- Update: October - 2016
    !!
    Module Subroutine Angle_and_Sigma(Cellp,DerM,x1,x0,x2,s1,s0,s2,ang,s)
       !---- Arguments ----!
       Type(Cell_G_Type),         intent(in)  :: Cellp         ! Cell object
       real(kind=cp), dimension(3,3,6), intent(in)  :: DerM          ! Matrix of derivatives of Cellp%Cr_Orth_cel
       real(kind=cp), dimension(3),     intent(in)  :: x0,x1,x2      ! Three points in fractional coordinates and sigmas, X0 is central
       real(kind=cp), dimension(3),     intent(in)  :: s0,s1,s2      ! Sigmas of the three points
       real(kind=cp),                   intent(out) :: ang,s         ! Angle and sigma

       !---- Local variables ----!
       real(kind=cp) :: d1,d2,d12,sa1,sa2,sa12
       real(kind=cp) :: cang12
       real(kind=cp) :: srel1,srel2

       !> Init values
       call clear_error()

       ang=0.0
       s=0.0

       !> Distances
       call distance_and_sigma(Cellp,DerM,x1,x0,s1,s0,d1,sa1)
       call distance_and_sigma(Cellp,DerM,x2,x0,s2,s0,d2,sa2)
       call distance_and_sigma(Cellp,DerM,x1,x2,s1,s2,d12,sa12)
       if (d1 <= 0.0001 .or. d2 <= 0.0001 .or. d12 <= 0.0001) then
          Err_CFML%Ierr=1
          Err_CFML%Msg="Some of the distances between atoms are zero! "
          return
       end if

       !> Angles
       cang12=0.5_cp*(d1/d2+d2/d1-d12*d12/d1/d2)
       ang=ACOSd(cang12)

       !---- Alternative calculation of angles' sigmas ----!
       srel1=(sa1/d1)**2
       srel2=(sa2/d2)**2
       s=SQRT(srel1+srel2+(sa12*d12/d1/d2)**2)*to_deg

    End Subroutine Angle_and_Sigma

    !!----
    !!----  Subroutine Get_Anglen_Axis_From_RotMat(R,axis,angle)
    !!----    real(kind=cp), dimension(3,3), intent(in) :: R             !Input orthogonal matrix
    !!----    real(kind=cp), dimension(3),   intent(out):: axis          !Non normalized rotation axis
    !!----    real(kind=cp),                 intent(out):: angle         !Angle of rotation
    !!----
    !!----  Subroutine to obtain the axis and angle of rotation corresponding to
    !!----  an input orthogonal matrix. A Cartesian frame is assumed
    !!----
    !!---- Update: January - 2011
    !!----
    Module Subroutine Get_Anglen_Axis_From_RotMat(R,axis,angle)
      Real(kind=cp), dimension(3,3), intent(in) :: R
      Real(kind=cp), dimension(3),   intent(out):: axis
      Real(kind=cp),                 intent(out):: angle
      !--- Local variables ---!
      Real(kind=cp) :: va

      va=(R(1,1)+R(2,2)+R(3,3)-1.0_cp)*0.5_cp
      if(va < -1.0_cp) va=-1.0_cp
      if(va >  1.0_cp) va= 1.0_cp
      angle= acosd(va)
      if(abs(abs(angle)-180.0_cp) < epsi) then
         axis= (/                sqrt(R(1,1)+1.0_cp), &
                sign(1.0_cp,R(1,2))*sqrt(R(2,2)+1.0_cp), &
                sign(1.0_cp,R(1,3))*sqrt(R(3,3)+1.0_cp) /)
      else
         axis= (/  R(2,3)-R(3,2), &
                   R(3,1)-R(1,3), &
                   R(1,2)-R(2,1) /)
      end if
    End Subroutine Get_Anglen_Axis_From_RotMat

    !!----
    !!----  Subroutine Get_Euler_From_Fract(X1,X2,X3,Mt,Phi,Theta,Chi,Eum,Code)
    !!----    real(kind=cp),           dimension(3),   intent (in) :: x1,x2,x3
    !!----    real(kind=cp),           dimension(3,3), intent (in) :: M !Matrix transforming to Cartesian coordinates
    !!----    real(kind=cp),                           intent(out) :: theta,phi,chi
    !!----    real(kind=cp), optional, dimension(3,3), intent(out) :: EuM
    !!----    character(len=*), optional,              intent (in) :: Code
    !!----
    !!----  Subroutine to obtain the Euler angles (2nd setting) of a Cartesian frame having
    !!----  as origin the point x3, the z-axis along x1-x3 and the "xz" plane coincident with
    !!----  the plane generated by the two vectors (x2-x3,x1-x3). The
    !!----
    !!---- Update: February - 2005
    !!
    Module Subroutine Get_Euler_From_Fract(X1,X2,X3,Mt,Phi,Theta,Chi,Eum,Code)
       !---- Arguments ----!
       real(kind=cp),           dimension(3),   intent (in) :: x1,x2,x3
       real(kind=cp),           dimension(3,3), intent (in) :: Mt
       real(kind=cp),                           intent(out) :: theta,phi,chi
       real(kind=cp), optional, dimension(3,3), intent(out) :: EuM
       character(len=*), optional,              intent (in) :: Code

       !---- Local variables ----!
       real(kind=cp), dimension(3)   :: u,v,w
       real(kind=cp), dimension(3,3) :: rot

!  U = ( cosPhi cosTheta cosChi - sinPhi sinChi,   sinPhi cosTheta cosChi+cosPhi sinChi,  -sinTheta cosChi)
!  V = (-sinPhi cosChi   - cosPhi cosTheta sinChi, cosPhi cosChi -sinPhi cosTheta sinChi,  sinTheta sinChi)
!  W = ( cosPhi sinTheta, sinPhi sinTheta,  cosTheta)
!
!     This corresponds to Euler angles defined in the following way:
!
!     In the starting position the cartesian frame (u,v,w) coincides with the crystallographic
!     cartesian frame (e1//a, e2 in the a-b plane and e3= e1 x e2). First a rotation Chi around
!     the e3 axis is applied, then a rotation Theta around the e2 axis and finally a rotation Phi
!     around e3. The total rotation matrix is
!
!          R(Phi,Theta,Chi) = R(e3,Phi) R(e2,Theta) R(e3,Chi) = [[ u, v, w]]
!
!     The columns of the active rotation matrix are the components of the unitary vectors u,v,w.

       w=matmul(Mt,x1-x3)
       w=w/sqrt(dot_product(w,w))
       u=matmul(Mt,x2-x3)
       u=u/sqrt(dot_product(u,u))
       v=cross_product(w,u)
       v=v/sqrt(dot_product(v,v))
       u=cross_product(v,w) !already normalized
       rot(:,1)=u; rot(:,2)=v;  rot(:,3)=w  !Matrix Rot ([u,v,w] columns)
       if (present(EuM)) EuM=rot
       if (present(Code)) then
          call get_PhiTheChi(rot,phi,theta,chi,Code)
       else
          call get_PhiTheChi(rot,phi,theta,chi)
       end if

    End Subroutine Get_Euler_From_Fract

    !!----
    !!---- Subroutine Get_OmegaChiPhi(Mt,Omega,Chi,Phi,Code)
    !!----    real(kind=cp), dimension(3,3),intent(in)  :: Mt
    !!----    real(kind=cp),                intent(out) :: Omega
    !!----    real(kind=cp),                intent(out) :: Chi
    !!----    real(kind=cp),                intent(out) :: Phi
    !!----    character(len=*), optional,   intent(in)  :: Code
    !!----
    !!----    Calculate the Euler Angles corresponding to an orthogonal matrix
    !!----    The definition of the Euler angles in this case correspond to the
    !!----    rotation matrix of Busing and Levy for diffractometry obtained from
    !!----    the composition of a rotation around z of angle Phi, followed by a
    !!----    rotation of angle Chi around the y-axis and a subsequent rotation of angle
    !!----    Omega around z.
    !!----    The matrix is supposed to be of the form: M = Rz(Omega).Ry(Chi).Rz(Phi)
    !!----    If Code =="R" or not present then the output angles are provided in radians.
    !!----    If Code =="D" then the output angles are provided in degrees.
    !!----    A checking of the input matrix is given before calculating the angles.
    !!----    The user must check the logical variable "ERR_RotMat" after calling this
    !!----    subroutine. If ERR_RotMat=.true. it means that the input matrix is not orthogonal.
    !!----    The obtained rotations should be interpreted as changes of reference systems, the
    !!----    angles correspond to the motor settings to put a reciprocal vector in Cartesian
    !!----    coordinates w.r.t. the L-system (all angles equal to zero) in the position given
    !!----    by the active rotation matrix Mt:  z4= Mt z1.
    !!----
    !!---- Updated: March - 2013
    !!
    Module Subroutine Get_OmegaChiPhi(Mt,Omega,Chi,Phi,Code)  !Conventional Euler angles of diffractometry
       !---- Arguments ----!
       real(kind=cp), dimension(3,3),intent(in)  :: Mt
       real(kind=cp),                intent(out) :: Omega
       real(kind=cp),                intent(out) :: Chi
       real(kind=cp),                intent(out) :: Phi
       character(len=*), optional,   intent(in)  :: Code

       !---- Local Variables ----!
       real(kind=cp), dimension(3,3):: MTT
       real(kind=cp), parameter, dimension(3,3) :: &
                      identity = reshape ( (/1.0,0.0,0.0, 0.0,1.0,0.0, 0.0,0.0,1.0/),(/3,3/))

       MTT=transpose(Mt)
       MTT=matmul(MTT,Mt)-identity
       if (sum(abs(MTT)) > 5.0*eps) then
          Err_CFML%Ierr=1
          Err_CFML%Msg=" Error in Get_OmegaChiPhi ... the input matrix is not orthogonal! "
          return
       end if
       if (abs(Mt(3,3)-1.0) < eps) then  !M(3,3)=cos(Chi)=1
          Chi=0.0
          Omega=0.0                       ! Omega and Phi have the same axis, we select Omega=0
          !Phi=acos(Mt(1,1))              ! M(1,1)=cos(Omega)cos(Chi)cos(Phi)-sin(Omega)sin(Phi)
          Phi=atan2(Mt(1,2),Mt(1,1))      ! M(1,2)=cos(Omega)cos(Chi)sin(Phi)+sin(Omega)cos(Phi)
       else if(abs(Mt(3,3)+1.0) < eps) then  !M(3,3)=cos(Chi)=-1
          Chi=pi
          Omega=0.0                       ! Omega and Phi have the same axis, we select Omega=0
          !Phi=acos(-Mt(1,1))             ! We use also the elements (11) and (12)
          Phi=atan2(-Mt(1,2),Mt(1,1))
       else
          !Chi=acos(Mt(3,3))  !Better use the relation below (In BL there is an error in eqn 48 for omega)
          Omega=atan2(-Mt(2,3),Mt(1,3))       !M(1,3)=  cos(Omega)sin(Chi)   M(2,3)= -sin(Omega)sin(Chi)
          Phi=atan2(-Mt(3,2),-Mt(3,1))        !M(3,1)= -sin(Chi)cos(Phi)     M(3,2)= -sin(Chi)sin(Phi)
          Chi=atan2( Sqrt(Mt(3,1)*Mt(3,1)+Mt(3,2)*Mt(3,2)), Mt(3,3) )
       end if
       if (present(Code)) then
          if (code(1:1)=="D" .or. code(1:1)=="d") then
             Phi=Phi*to_deg
             Omega=Omega*to_deg
             Chi=Chi*to_deg
          end if
       end if

    End Subroutine Get_OmegaChiPhi

    !!----
    !!---- Subroutine Get_PhiTheChi(Mt,Phi,Theta,Chi,Code)
    !!----    real(kind=cp), dimension(3,3),intent(in)  :: Mt
    !!----    real(kind=cp),                intent(out) :: Phi
    !!----    real(kind=cp),                intent(out) :: Theta
    !!----    real(kind=cp),                intent(out) :: Chi
    !!----    character(len=*), optional,   intent(in)  :: Code
    !!----
    !!----    Calculate the Euler Angles corresponding to an orthogonal matrix
    !!----    The definition of the Euler angles in this case correspond to the
    !!----    active rotation matrix obtained from the composition of a rotation
    !!----    around z of angle Chi, followed by a rotation of angle Theta
    !!----    around the y-axis and a subsequent rotation of angle Phi around z.
    !!----    The matrix is supposed to be of the form: M = Rz(Phi).Ry(Theta).Rz(Chi)
    !!----    If Code =="R" or not present then the output angles are provided in radians.
    !!----    If Code =="D" then the output angles are provided in degrees.
    !!----    A checking of the input matrix is given before calculating the angles.
    !!----    The user must check the logical variable "Err_CFML%" after calling this
    !!----    subroutine. If Err_CFML%Ierr=1 it means that the input matrix is not orthogonal.
    !!----
    !!---- Update: February - 2005
    !!
    Module Subroutine Get_PhiTheChi(Mt,Phi,Theta,Chi,Code)
       !---- Arguments ----!
       real(kind=cp), dimension(3,3),intent(in)  :: Mt
       real(kind=cp),                intent(out) :: Phi
       real(kind=cp),                intent(out) :: Theta
       real(kind=cp),                intent(out) :: Chi
       character(len=*), optional,   intent(in)  :: Code

       !---- Local Variables ----!
       real(kind=cp), dimension(3,3):: MTT
       real(kind=cp), parameter, dimension(3,3) :: &
                      identity = reshape ( (/1.0,0.0,0.0, 0.0,1.0,0.0, 0.0,0.0,1.0/),(/3,3/))

       MTT=transpose(Mt)
       MTT=matmul(MTT,Mt)-identity
       if (sum(abs(MTT)) > 5.0*eps) then
          Err_CFML%Ierr=1
          Err_CFML%Msg=" Error in Get_PhiTheChi ... the input matrix is not orthogonal! "
          return
       end if
       if (abs(Mt(3,3)-1.0) < eps) then  !M(3,3)=cos(Theta)
          Theta=0.0
          Phi=0.0
          Chi=acos(Mt(1,1))               !M(1,1)=cos(Phi)cos(Theta)cos(Chi)-sin(Phi)sin(Chi)
       else if(abs(Mt(3,3)+1.0) < eps) then
          Theta=pi
          Phi=0.0
          Chi=acos(-Mt(1,1))
       else
          Theta=acos(Mt(3,3))
          Phi=atan2(Mt(2,3),Mt(1,3))     !M(1,3)=cos(Phi)sin(Theta)  M(2,3)=sin(phi)sin(Theta)
          Chi=atan2(Mt(3,2),-Mt(3,1))    !M(3,1)= -sin(Theta)cos(Chi)   M(3,2)= sin(Theta)sin(Chi)
       end if
       if (present(Code)) then
          if (code(1:1)=="D" .or. code(1:1)=="d") then
             Phi=Phi*to_deg
             Theta=Theta*to_deg
             Chi=Chi*to_deg
          end if
       end if

    End Subroutine Get_PhiTheChi

    !!----
    !!---- Subroutine Torsion_and_Sigma(Cellp,x1,x2,x3,x4,sx1,sx2,sx3,sx4,tor,s)
    !!----    Type(Cell_G_Type),         intent(in)  :: Cellp            ! Cell object
    !!----    real(kind=cp), dimension(3),     intent(in)  :: x1,x2,x3,x4      ! Three points in fractional coordinates and sigmas
    !!----    real(kind=cp), dimension(3),     intent(in)  :: sx1,sx2,sx3,sx4  ! Sigmas of the three points
    !!----    real(kind=cp),                   intent(out) :: tor,s            ! Torsion angle and sigma
    !!----
    !!----    From Acta Cryst A28, 1972, 213-215
    !!----    Version from Parst97
    !!----
    !!---- Update: October - 2016
    !!
    Module Subroutine Torsion_and_Sigma(Cellp, x1,x2,x3,x4,sx1,sx2,sx3,sx4,tor,s)
       !---- Arguments ----!
       Type(Cell_G_Type),               intent(in)  :: Cellp         ! Cell object
       real(kind=cp), dimension(3),     intent(in)  :: x1,x2,x3,x4       ! Three points in fractional coordinates and sigmas, X0 is central
       real(kind=cp), dimension(3),     intent(in)  :: sx1,sx2,sx3,sx4   ! Sigmas of the three points
       real(kind=cp),                   intent(out) :: tor,s             ! Angle and sigma

       !---- Local Variables ----!
       integer                       :: i,j
       real(kind=cp), dimension(3,3) :: dlt,m
       real(kind=cp), dimension(4)   :: ds
       real(kind=cp), dimension(3)   :: xc1,xc2,xc3,xc4
       real(kind=cp), dimension(3)   :: sc1,sc2,sc3,sc4
       real(kind=cp), dimension(3)   :: dst
       real(kind=cp)                 :: cf1,cf2,sf1,sf2,p
       real(kind=cp)                 :: stau,ctau,tau
       real(kind=cp)                 :: a1,a2,s1,s2,s3,s4

       !> Init
       call Clear_Error()

       tor=0.0
       s=0.0

       !> Cartesian coordinates
       xc1 = matmul(Cellp%Cr_Orth_cel,x1)
       xc2 = matmul(Cellp%Cr_Orth_cel,x2)
       xc3 = matmul(Cellp%Cr_Orth_cel,x3)
       xc4 = matmul(Cellp%Cr_Orth_cel,x4)

       sc1 = matmul(abs(Cellp%Cr_Orth_cel),sx1)
       sc2 = matmul(abs(Cellp%Cr_Orth_cel),sx2)
       sc3 = matmul(abs(Cellp%Cr_Orth_cel),sx3)
       sc4 = matmul(abs(Cellp%Cr_Orth_cel),sx4)

       dlt(1,:)=xc2-xc1
       dlt(2,:)=xc2-xc3
       dlt(3,:)=xc4-xc3

       do i=1,3
          dst(i)=sqrt(dlt(i,1)**2+dlt(i,2)**2+dlt(i,3)**2)
          if (dst(i) <=0.0001) then
             Err_CFML%IErr=1
             Err_CFML%Msg="Some of the distances between atoms are zero! "
             return
          end if
       end do

       do i=1,3
          do j=1,3
             m(i,j)=dlt(i,j)/dst(i)
          end do
       end do

       cf1=m(1,1)*m(2,1) + m(1,2)*m(2,2) + m(1,3)*m(2,3)
       cf2=m(2,1)*m(3,1) + m(2,2)*m(3,2) + m(2,3)*m(3,3)
       if (abs(cf1) > 0.9999 .or. abs(cf2) > 0.9999) then
          Err_CFML%IErr=1
          Err_CFML%Msg="Problem in Torsion and Sigma routine!. Please check!"
          return
       end if

       sf1=sqrt(1.0-cf1**2)
       sf2=sqrt(1.0-cf2**2)
       p=sf1*sf2
       if (abs(p) < 1.0e-5) then
          Err_CFML%IErr=1
          Err_CFML%Msg="Problem in Torsion and Sigma routine!. Please check!"
          return
       end if

       stau= ( m(3,1)*( m(2,2)*m(1,3)-m(1,2)*m(2,3) )  + &
               m(3,2)*( m(1,1)*m(2,3)-m(1,3)*m(2,1) )  + &
               m(3,3)*( m(2,1)*m(1,2)-m(1,1)*m(2,2) ) ) / p

       ctau= ( (m(2,2)*m(1,3)-m(1,2)*m(2,3)) * (m(3,2)*m(2,3)-m(2,2)*m(3,3)) + &
               (m(2,3)*m(1,1)-m(2,1)*m(1,3)) * (m(3,3)*m(2,1)-m(3,1)*m(2,3)) + &
               (m(2,1)*m(1,2)-m(1,1)*m(2,2)) * (m(3,1)*m(2,2)-m(2,1)*m(3,2)) ) / p

       tau=atan2(stau,ctau)*to_deg
       if (tau > 180.0) tau=tau-360.0

       ds(1)=(1.0/3.0)*(sc1(1)**2+sc1(2)**2+sc1(3)**2)
       ds(2)=(1.0/3.0)*(sc2(1)**2+sc2(2)**2+sc2(3)**2)
       ds(3)=(1.0/3.0)*(sc3(1)**2+sc3(2)**2+sc3(3)**2)
       ds(4)=(1.0/3.0)*(sc4(1)**2+sc4(2)**2+sc4(3)**2)
       ds=sqrt(ds)

       s1=(ds(1)/(dst(1)*sf1))**2
       a1=(dst(2)-dst(1)*cf1)/(dst(1)*sf1)
       a2=(dst(2)-dst(3)*cf2)/(dst(3)*sf2)
       s2=(ds(2)/dst(2))**2*(a1**2-2.*a1*(cf2/sf2)*ctau+(cf2/sf2)**2)
       s3=(ds(3)/dst(2))**2*(a2**2-2.*a2*(cf1/sf1)*ctau+(cf1/sf1)**2)
       s4=(ds(4)/(dst(3)*sf2))**2
       if ( (s1+s2+s3+s4) < 0.0) then
          Err_CFML%IErr=1
          Err_CFML%Msg="Problem in Torsion and Sigma routine!. Please check!"
          return
       end if

       s=sqrt(s1+s2+s3+s4)*to_deg
       tor=tau

    End Subroutine Torsion_and_Sigma

 End SubModule Geom_Angles
