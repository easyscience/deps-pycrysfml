!!----
!!---- SUBMODULE CFML_Math_3D
!!----
!!----
!!
Submodule (CFML_Maths) Maths_SistCoord_Changes
 implicit none
 Contains
    !!----
    !!---- GET_CART_FROM_SPHER
    !!----    Determine the Cartesian coordinates from spherical coordinates.
    !!----    Theta is the azimutal angle
    !!----
    !!---- 04/04/2019
    !!
    Pure Module Function Get_Cart_from_Spher(SphCoord,Mode) Result(CarCoord)
       !---- Arguments ----!
       real(kind=cp), dimension(3), intent( in) :: SphCoord ! Coordinates (R,Theta,Phi)
       character(len=*), optional,  intent( in) :: mode     ! If "D" the angles are in degrees, otherwise radians is considered
       real(kind=cp), dimension(3)              :: CarCoord ! Cartesian coordinates

       !---- Local Variables ----!
       real(kind=cp) :: ph,th

       th=SphCoord(2)
       ph=SphCoord(3)
       if (present(mode)) then
          if (mode(1:1) == "D" .or. mode(1:1) == "d") then
             th=SphCoord(2)*TO_RAD
             ph=SphCoord(3)*TO_RAD
          end if
       end if
       CarCoord(1)=SphCoord(1)*cos(ph)*sin(th)
       CarCoord(2)=SphCoord(1)*sin(ph)*sin(th)
       CarCoord(3)=SphCoord(1)*cos(th)

    End Function Get_Cart_from_Spher

    !!----
    !!---- GET_CART_FROM_CYLIN
    !!----    Determine the Cartesian coordinates from cylindrical coordinates.
    !!----
    !!---- 04/04/2019
    !!
    Pure Module Function Get_Cart_from_Cylin(CylCoord,Mode) Result(CarCoord)
       !---- Arguments ----!
       real(kind=cp), dimension(3), intent( in) ::  CylCoord ! Coordinates rho,phi,zeta
       character(len=*), optional,  intent( in) ::  mode     ! "D" angles in degrees, otherwise in radians
       real(kind=cp), dimension(3)              ::  CarCoord ! Cartesian coordinates

       !---- Local Variables ----!
       real(kind=cp) :: ph

       ph=CylCoord(2)
       if (present(mode)) then
          if (mode(1:1) == "D" .or. mode(1:1) == "d") ph=CylCoord(2)*TO_RAD
       end if
       CarCoord(1)=CylCoord(1)*cos(ph)
       CarCoord(2)=CylCoord(1)*sin(ph)
       CarCoord(3)=CylCoord(3)

    End Function Get_Cart_from_Cylin

    !!----
    !!---- GET_CYLIN_FROM_CART
    !!----    Determine the cylindrical coordinates from Cartesian coordinates.
    !!----    The components of the CylCoord vector are [rho,phi,z]
    !!----
    !!---- 04/04/2019
    !!
    Pure Module Function Get_Cylin_from_Cart(CarCoord, Mode) Result(CylCoord)
       !---- Arguments ----!
       real(kind=cp), dimension(3),intent(in) ::  CarCoord   ! Cartesian coordinatates
       character(len=*), optional, intent(in) ::  mode
       real(kind=cp), dimension(3)            ::  CylCoord   ! Cylindrical coordinates

       !---- Local Variables ----!
       integer :: j

       CylCoord(3)=CarCoord(3)
       if( abs(CarCoord(2)) > epss .or. abs(CarCoord(1)) > epss) then
          CylCoord(2)=atan2(CarCoord(2),CarCoord(1))
       else
          CylCoord(2)= 0.0_cp
       end if

       CylCoord(1)=0.0_cp
       do j=1,2
          CylCoord(1)=CylCoord(1)+CarCoord(j)*CarCoord(j)
       end do
       CylCoord(1)=sqrt(CylCoord(1))

       if (present(mode)) then
          if (mode(1:1) == "D" .or. mode(1:1) == "d") CylCoord(2)=CylCoord(2)*TO_DEG
       end if

    End Function Get_Cylin_from_Cart

    !!----
    !!---- GET_SPHER_FROM_CART
    !!----    Determine the spherical coordinates from rectangular coordinates
    !!----    The components of the SphCoord vectors are [r,theta,phi]
    !!----
    !!---- 04/04/2019
    !!
    Pure Module Function Get_Spher_from_Cart(CarCoord,mode) Result(SphCoord)
       !---- Arguments ----!
       real(kind=cp), dimension(3), intent(in) :: CarCoord ! Cartesian
       character(len=*), optional,  intent(in) :: mode
       real(kind=cp), dimension(3)             :: SphCoord ! Spherical

       !---- Local Variables ----!
       integer :: j

       SphCoord(1)=0.0_cp
       do j=1,3
          SphCoord(1)=SphCoord(1)+CarCoord(j)*CarCoord(j)
       end do
       SphCoord(1)=sqrt(SphCoord(1))

       if (SphCoord(1) > 0.0_cp) then
          SphCoord(2)=CarCoord(3)/SphCoord(1)
          if (abs(SphCoord(2)) > 1.0_cp) then
             SphCoord(2)=sign(1.0_cp,SphCoord(2))
          end if
          SphCoord(2)=acos(SphCoord(2))
          if (abs(SphCoord(2)) < epss .or. abs(SphCoord(2)-pi) < epss) then
             SphCoord(3)=0.0_cp
          else
             SphCoord(3)=atan2(CarCoord(2),CarCoord(1))
          end if
       else
          SphCoord(2)=0.0_cp
          SphCoord(3)=0.0_cp
       end if
       if (present(mode)) then
          if (mode(1:1) == "D" .or. mode(1:1) == "d") then
             SphCoord(2)=SphCoord(2)*TO_DEG
             SphCoord(3)=SphCoord(3)*TO_DEG
          end if
       end if

    End Function Get_Spher_from_Cart

    !!----
    !!---- GET_SPHER_FROM_CYLIN
    !!----    Determine the spheric coordinates from cylinder coordinates
    !!----
    !!---- 04/04/2019
    !!
    Pure Module Function Get_Spher_from_Cylin(CylCoord,mode) Result(SphCoord)
       !---- Arguments ----!
       real(kind=cp), dimension(3), intent(in) :: CylCoord ! Cylinder
       character(len=*), optional,  intent(in) :: mode
       real(kind=cp), dimension(3)             :: SphCoord ! Spherical

       !---- Local Variables ----!
       real(kind=cp), dimension(3) :: CarCoord

       if (present(mode)) then
          CarCoord=Get_Cart_from_Cylin(CylCoord,mode)
          SphCoord=Get_Spher_from_Cart(CarCoord,mode)
       else
          CarCoord=Get_Cart_from_Cylin(CylCoord)
          SphCoord=Get_Spher_from_Cart(CarCoord,mode)
       end if

    End Function Get_Spher_from_Cylin

    !!----
    !!---- GET_CYLIN_FROM_SPHER
    !!----    Determine the spheric coordinates from cylinder coordinates
    !!----
    !!---- 04/04/2019
    !!
    Pure Module Function Get_Cylin_from_Spher(SphCoord,mode) Result(CylCoord)
       !---- Arguments ----!
       real(kind=cp), dimension(3), intent(in) :: SphCoord ! Cylinder
       character(len=*), optional,  intent(in) :: mode
       real(kind=cp), dimension(3)             :: CylCoord ! Spherical

       !---- Local Variables ----!
       real(kind=cp), dimension(3) :: CarCoord

       if (present(mode)) then
          CarCoord=Get_Cart_from_Spher(SphCoord,mode)
          CylCoord=Get_Cylin_from_Cart(CarCoord,mode)
       else
          CarCoord=Get_Cart_from_Spher(SphCoord,mode)
          CylCoord=Get_Cylin_from_Cart(CarCoord,mode)
       end if

    End Function Get_Cylin_from_Spher

End Submodule Maths_SistCoord_Changes
