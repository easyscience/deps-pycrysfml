 Submodule (CFML_Maths) Math_Polyhedron_Volume
  implicit none

   contains
    !!----
    !!---- Module Function Polyhedron_Volume(Nv,Vert,Cent) Result(vol)
    !!----    integer,                       intent(in) :: Nv       ! Vertices Number
    !!----    real(kind=cp), dimension(:,:), intent(in) :: Vert     ! Cartesian coordinates of vertices
    !!----    real(kind=cp), dimension(3),   intent(in) :: Cent     ! Cartesian coordinates a central point
    !!----
    !!---- This procedure calculate the volume of polyhedral with Nv vertices.
    !!---- It is based on volcal program of L. W. FINGER.
    !!---- Adapted by Javier Gonzalez Platas
    !!----
    !!---- Update: February - 2010
    !!
    Module Function Polyhedron_Volume(NV,Vert,Cent) Result(vol)
       !---- Arguments ----!
       integer,                       intent(in) :: Nv       ! Number of Vertices
       real(kind=cp), dimension(:,:), intent(in) :: Vert     ! Cartesian coordinates of atoms
       real(kind=cp), dimension(3),   intent(in) :: Cent     ! Cartesian coordinates of Central atom
       real(kind=cp)                             :: vol
       !---- Local Variables ----!
       integer                       :: i,j,k,l,i1,j1
       real(kind=cp)                 :: z,z0,area,factor
       real(kind=cp),dimension(6)    :: vxyz
       real(kind=cp),dimension(3)    :: d
       real(kind=cp),dimension(3,Nv) :: Atm_cart

       vol=0.0
       call Clear_Error()

       if (nv <= 3) then
          Err_CFML%Ierr=1
          Err_CFML%Msg='The number of vertices for polyhedron volume is less than 4'
          return
       end if

       do i=1,nv
          Atm_cart(:,i)=Vert(:,i)- Cent
       end do

       do i=1,nv-2
          i1=i+1
          do j=i1,nv-1
             j1=j+1
             vxyz(1:3)=Atm_cart(:,j)-Atm_cart(:,i)
        loop:do k=j1,nv
                vxyz(4:6)=Atm_cart(:,k)-Atm_cart(:,i)
                d(1)=vxyz(2)*vxyz(6)-vxyz(5)*vxyz(3)
                d(2)=vxyz(4)*vxyz(3)-vxyz(1)*vxyz(6)
                d(3)=vxyz(1)*vxyz(5)-vxyz(4)*vxyz(2)
                area=0.5*sqrt(d(1)*d(1)+d(2)*d(2)+d(3)*d(3))
                z0=0.5*(Atm_cart(1,i)*d(1)+Atm_cart(2,i)*d(2)+Atm_cart(3,i)*d(3))/area

                ! check for and avoid plane through origin
                if (abs(z0) < 1.0e-5) cycle
                factor = 3.0
                do l=1,nv
                   if(l==i .or. l==j .or. l==k) cycle

                   ! calculate distance of point l from plane of ijk
                   z=0.5*((Atm_cart(1,i)-Atm_cart(1,l))*d(1)+ &
                          (Atm_cart(2,i)-Atm_cart(2,l))*d(2)+ &
                          (Atm_cart(3,i)-Atm_cart(3,l))*d(3))/area

                   ! z and z0 must have the same sign
                   if (z * z0 < -0.001) cycle loop
                   if (abs(z * z0) < 0.001)then
                      ! if more than 3 corners on this face, the area will be counted twice.
                      ! change factor to handle this case.
                     factor = 6.0
                   end if
                end do

                ! all points on same side,  thus ijk are face
                ! Direction Cosines Of Plane Normal
                d=d/(2.0*area)

                vol=vol+area*abs(z0)/factor

             end do loop
          end do
       end do

    End Function Polyhedron_Volume

    !!----
    !!---- Module Subroutine Get_Centroid_Coord(Cn,Atm_Cart,Centroid,Baricenter)
    !!----    integer,                       intent(in) :: Cn          ! Coordination Number
    !!----    real(kind=cp), dimension(:,:), intent(in) :: Atm_Cart    ! Cartesian coordinates of atoms
    !!----    real(kind=cp), dimension(3),   intent(out):: Centroid    ! Centroid
    !!----    real(kind=cp), dimension(3),   intent(out):: Baricenter  ! Baricenter
    !!----
    !!---- Procedure to calculate Centroid and BariCenter of a pPolyhedron according to
    !!---- Tonci Balic-Zunic (Acta Cryst. B52, 1996, 78-81; Acta Cryst. B54, 1998, 766-773)
    !!---- Centroid is here different from Baricentre and it is defined in the above reference.
    !!----
    !!---- Update: February - 2010
    !!
    Module Subroutine Get_Centroid_Coord(Cn,Atm_Cart,Centroid,Baricenter)
       !---- Arguments ----!
       integer,                       intent(in) :: Cn          ! Coordination Number
       real(kind=cp), dimension(:,:), intent(in) :: Atm_Cart    ! Cartesian coordinates of atoms, gathered as: (1:3,1:Cn)
       real(kind=cp), dimension(3),   intent(out):: Centroid    ! Centroid
       real(kind=cp), dimension(3),   intent(out):: Baricenter  ! Baricenter

       !---- Local variables ----!
       real(kind=cp), dimension(4)   :: plane1,plane2,plane3
       real(kind=cp), dimension(3)   :: p0,p1,p2,p3,u,v,r,t
       real(kind=cp), dimension(3,3) :: w, w1
       real(kind=cp)                 :: d,umod,vmod,rmod,d1
       real(kind=cp)                 :: sx, sy, sz, sx2, sy2, sz2, sx3, sy3, sz3
       real(kind=cp)                 :: sxy, sxz, syz, sxy2, sxz2
       real(kind=cp)                 :: sx2y, sx2z, syz2, sy2z
       integer                       :: i

       call Clear_Error()
       centroid=0.0
       baricenter=0.0

       p1=Atm_Cart(1:3,1)
       p2=Atm_Cart(1:3,2)
       p3=Atm_Cart(1:3,3)

       select case (cn)
          case (:2)
             err_CFML%Ierr=1
             err_CFML%Msg='Centroid calculation needs 3 vertices as minimum'
             return

          case (3)
             !---- Plane 1: Defined with those 3 Points ----!
             call Get_Plane_From_3Points(p1, p2, p3, &
                                        plane1(1), plane1(2), plane1(3), plane1(4))
             r=plane1(1:3)
             rmod=norm2(r)
             if (abs(rmod) <= 0.0001) then
                err_CFML%Ierr=1
                err_CFML%Msg='Imposible to define a Plane with the three given points '
                return
             end if
             r=r/rmod

             !---- Vectors ----!
             u=p2-p1
             umod=norm2(u)
             if (abs(umod) <= 0.0001) then
                err_CFML%Ierr=1
                err_CFML%Msg='Check your points! Seems that two of them are equal'
                return
             end if

             v=p3-p1
             vmod=norm2(v)
             if (abs(vmod) <= 0.0001) then
                err_CFML%Ierr=1
                err_CFML%Msg='Check your points! Seems that two of them are equal'
                return
             end if

             !---- Plane 2 ----!
             p0=p1+0.5*u
             u=u/umod
             plane2(1:3)=u
             plane2(4)=-( plane2(1)*p0(1)+plane2(2)*p0(2)+plane2(3)*p0(3) )

             !---- Plane 3 ----!
             p0=p1+0.5*v
             v=v/vmod
             plane3(1:3)=v
             plane3(4)=-( plane3(1)*p0(1)+plane3(2)*p0(2)+plane3(3)*p0(3) )

             !---- Centroid ----!
             w(1,1:3)=plane1(1:3)
             w(2,1:3)=plane2(1:3)
             w(3,1:3)=plane3(1:3)
             d=determ3D(w)

             if (abs(d) <= 0.0001) then
                err_CFML%Ierr=1
                err_CFML%Msg='Determinant is singular to calculate Centroid point'
                return
             end if

             w(1:3,1)=[-plane1(4),-plane2(4), -plane3(4)]
             d1=determ3D(w)
             centroid(1)=d1/d

             w(1,1:3)=plane1(1:3)
             w(2,1:3)=plane2(1:3)
             w(3,1:3)=plane3(1:3)
             w(1:3,2)=[-plane1(4),-plane2(4), -plane3(4)]
             d1=determ3D(w)
             centroid(2)=d1/d

             w(1,1:3)=plane1(1:3)
             w(2,1:3)=plane2(1:3)
             w(3,1:3)=plane3(1:3)
             w(1:3,3)=[-plane1(4),-plane2(4), -plane3(4)]
             d1=determ3D(w)
             centroid(3)=d1/d

             sx =0.0; sy =0.0; sz =0.0
             do i=1,3
                sx=sx+Atm_Cart(1,i)
                sy=sy+Atm_Cart(2,i)
                sz=sz+Atm_Cart(3,i)
             end do

          case (4:)
             sx =0.0; sy =0.0; sz =0.0
             sx2=0.0; sy2=0.0; sz2=0.0
             sx3=0.0; sy3=0.0; sz3=0.0
             sxy=0.0; sxz=0.0; syz=0.0
             sxy2=0.0; sxz2=0.0
             sx2y=0.0; sx2z=0.0
             syz2=0.0; sy2z=0.0
             do i=1,cn
                sx=sx+Atm_Cart(1,i)
                sy=sy+Atm_Cart(2,i)
                sz=sz+Atm_Cart(3,i)

                sx2=sx2+Atm_Cart(1,i)*Atm_Cart(1,i)
                sy2=sy2+Atm_Cart(2,i)*Atm_Cart(2,i)
                sz2=sz2+Atm_Cart(3,i)*Atm_Cart(3,i)

                sx3=sx3+Atm_Cart(1,i)*Atm_Cart(1,i)*Atm_Cart(1,i)
                sy3=sy3+Atm_Cart(2,i)*Atm_Cart(2,i)*Atm_Cart(2,i)
                sz3=sz3+Atm_Cart(3,i)*Atm_Cart(3,i)*Atm_Cart(3,i)

                sxy=sxy+Atm_Cart(1,i)*Atm_Cart(2,i)
                sxz=sxz+Atm_Cart(1,i)*Atm_Cart(3,i)
                syz=syz+Atm_Cart(2,i)*Atm_Cart(3,i)

                sxy2=sxy2+Atm_Cart(1,i)*Atm_Cart(2,i)*Atm_Cart(2,i)
                sxz2=sxz2+Atm_Cart(1,i)*Atm_Cart(3,i)*Atm_Cart(3,i)

                sx2y=sx2y+Atm_Cart(2,i)*Atm_Cart(1,i)*Atm_Cart(1,i)
                sx2z=sx2z+Atm_Cart(3,i)*Atm_Cart(1,i)*Atm_Cart(1,i)

                syz2=syz2+Atm_Cart(2,i)*Atm_Cart(3,i)*Atm_Cart(3,i)
                sy2z=sy2z+Atm_Cart(3,i)*Atm_Cart(2,i)*Atm_Cart(2,i)
             end do

             w(1,1)=sx2 - (sx**2)/real(cn)
             w(1,2)=sxy - (sx*sy)/real(cn)
             w(1,3)=sxz - (sx*sz)/real(cn)
             t(1)=0.5*(sx3 + sxy2 + sxz2 - ((sx2*sx + sy2*sx + sz2*sx)/real(cn)))

             w(2,1)=sxy - (sx*sy)/real(cn)
             w(2,2)=sy2 - (sy**2)/real(cn)
             w(2,3)=syz - (sy*sz)/real(cn)
             t(2)=0.5*(sx2y + sy3 + syz2 - ((sx2*sy + sy2*sy + sz2*sy)/real(cn)))

             w(3,1)=sxz - (sx*sz)/real(cn)
             w(3,2)=syz - (sy*sz)/real(cn)
             w(3,3)=sz2 - (sz**2)/real(cn)
             t(3)=0.5*(sx2z + sy2z + sz3 - ((sx2*sz + sy2*sz + sz2*sz)/real(cn)))

             d=determ3D(w)
             if (abs(d) <= 0.0001) then
                err_CFML%Ierr=1
                err_CFML%Msg='Determinant is singular to calculate Centroid point'
                return
             end if

             w1=w
             w1(:,1)=t
             d1=determ3D(w1)
             centroid(1)=d1/d

             w1=w
             w1(:,2)=t
             d1=determ3D(w1)
             centroid(2)=d1/d

             w1=w
             w1(:,3)=t
             d1=determ3D(w1)
             centroid(3)=d1/d
       end select

       baricenter=[ sx/real(cn), sy/real(cn), sz/real(cn) ]

       return
    End Subroutine Get_Centroid_Coord

    !!----
    !!---- Module Subroutine Get_Plane_from_3Points(P1,P2,P3,A,B,C,D)
    !!----    real(kind=cp), dimension(3), intent(in) :: P1
    !!----    real(kind=cp), dimension(3), intent(in) :: P2
    !!----    real(kind=cp), dimension(3), intent(in) :: P3
    !!----    real(kind=cp),               intent(out):: A
    !!----    real(kind=cp),               intent(out):: B
    !!----    real(kind=cp),               intent(out):: C
    !!----    real(kind=cp),               intent(out):: D
    !!----
    !!----    Caculate the implicit form of a Plane in 3D as
    !!----    A * X + B * Y + C * Z + D = 0
    !!----
    !!---- Update: July - 2005
    !!
    Module Subroutine Get_Plane_from_3Points(P1, P2, P3, A, B, C, D)
       !---- Arguments ----!
       real(kind=cp), dimension(3), intent(in) :: P1
       real(kind=cp), dimension(3), intent(in) :: P2
       real(kind=cp), dimension(3), intent(in) :: P3
       real(kind=cp),               intent(out):: A
       real(kind=cp),               intent(out):: B
       real(kind=cp),               intent(out):: C
       real(kind=cp),               intent(out):: D

       !---- Local Variables ----!
       real(kind=cp) :: r

       a = ( p2(2) - p1(2) ) * ( p3(3) - p1(3) ) &
           - ( p2(3) - p1(3) ) * ( p3(2) - p1(2) )

       b = ( p2(3) - p1(3) ) * ( p3(1) - p1(1) ) &
           - ( p2(1) - p1(1) ) * ( p3(3) - p1(3) )

       c = ( p2(1) - p1(1) ) * ( p3(2) - p1(2) ) &
           - ( p2(2) - p1(2) ) * ( p3(1) - p1(1) )

       r=sqrt(a**2 + b**2 + c**2)
       a=a/r
       b=b/r
       c=c/r

       d = - p2(1) * a - p2(2) * b - p2(3) * c

       return
    End Subroutine Get_Plane_from_3Points

 End Submodule Math_Polyhedron_Volume
