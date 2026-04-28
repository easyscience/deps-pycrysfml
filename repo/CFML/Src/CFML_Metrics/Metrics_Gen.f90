Submodule (CFML_Metrics) Metrics_Gen
 Implicit none
 Contains
    !!----
    !!---- SIGMAV_FROM_CELL
    !!----    Calculates the standard deviation of the unit cell volume
    !!----    from the standard deviations of cell parameters.
    !!----    The input variable is of type Cell_Type.
    !!----
    !!----    It is assumed that there is no correlation (covariance terms) between
    !!----    the standard deviations of the different cell parameters.
    !!----
    !!---- 10/04/2019
    !!
    Pure Module Function SigmaV_From_Cell(Cell) Result(sigma)
       !---- Arguments ----!
       class(Cell_G_Type), intent(in) :: Cell      ! Cell Parameters
       real(kind=cp)                  :: sigma     ! Sigma

       !--- Local variables ---!
       integer                     :: i
       real(kind=cp)               :: cr1, cr2
       real(kind=cp)               :: q,ca,cb,cc,vc,sa,sb,sc
       real(kind=cp), dimension(3) :: var_ang

       !> Init
       sigma=0.0_cp

       cr1=sum(abs(cell%scell))
       cr2=sum(abs(cell%sang))
       if (cr1 < EPS .and. cr2 < EPS ) return

       vc=0.0_cp
       do i=1,3
          q=cell%scell(i)/cell%cell(i)
          vc=vc+q*q
       end do

       if (cr2 > EPS) then
          ca=cosd(cell%ang(1)) ;  sa=sind(cell%ang(1))
          cb=cosd(cell%ang(2)) ;  sb=sind(cell%ang(2))
          cc=cosd(cell%ang(3)) ;  sc=sind(cell%ang(3))
          q=1.0-ca*ca-cb*cb-cc*cc+2.0*ca*cb*cc

          var_ang = (cell%sang * TO_RAD)**2/q
          vc=vc+ (ca-cb*cc)*(ca-cb*cc)*sa*sa * var_ang(1)
          vc=vc+ (cb-ca*cc)*(cb-ca*cc)*sb*sb * var_ang(2)
          vc=vc+ (cc-ca*cb)*(cc-ca*cb)*sc*sc * var_ang(3)
       end if

       sigma=cell%vol*sqrt(vc)

    End Function SigmaV_From_Cell

    !!----
    !!---- VOLUME_FROM_CELL
    !!---- Calculate the volume of a Cell
    !!----
    !!---- 10/04/2019
    !!
    Pure Module Function Volume_from_Cell(cell,ang) Result(Vol)
       !---- Arguments ----!
       real(kind=cp), dimension(3), intent(in) :: cell
       real(kind=cp), dimension(3), intent(in) :: ang
       real(kind=cp)                           :: vol

       !---- Local Variables ----!
       integer       :: i
       real(kind=cp) :: p,s,cs

       !> Init
       p=1.0_cp; s=1.0_cp

       do i=1,3
          cs=cosd(ang(i))
          p=p*cs
          s=s-cs*cs
       end do
       vol=sqrt(abs(s + 2.0_cp*p))

       do i=1,3
          vol=vol*cell(i)
       end do

    End Function Volume_from_Cell

    !!----
    !!---- GET_METRICS
    !!----    Constructs the metric tensor
    !!----
    !!---- 10/04/2019
    !!
    Pure Module Function Get_Metrics(cell,ang) Result(G)
       !---- Arguments ----!
       real(kind=cp), dimension(3)  , intent(in ) :: cell  ! Cell Parameters
       real(kind=cp), dimension(3)  , intent(in ) :: ang
       real(kind=cp), dimension(3,3)              :: G     ! Metric Tensor

       !---- Local Variables ----!
       integer :: i

       G(1,2)= cell(1)*cell(2)*cosd(ang(3))
       G(1,3)= cell(1)*cell(3)*cosd(ang(2))
       G(2,3)= cell(2)*cell(3)*cosd(ang(1))

       do i=1,3
          G(i,i)= cell(i)*cell(i)
       end do

       G(2,1)=G(1,2)
       G(3,1)=G(1,3)
       G(3,2)=G(2,3)

    End Function Get_Metrics

    !!--++
    !!--++ GET_CRYST_ORTHOG_MATRIX
    !!--++    Obtains the matrix giving the crystallographic basis in
    !!--++    direct space in terms of a Cartesian basis. The output matrix
    !!--++    can be directly used for transforming crystallographic components
    !!--++    to Cartesian components of the components of a vector considered
    !!--++    as a column vector:   XC = CrystOrt X.
    !!--++
    !!--++    If CartType is not present, or if it is not equal to 'A',
    !!--++    the cartesian system is defined as:
    !!--++          z // c; y is in the bc-plane; x is y ^ z
    !!--++    a = (a sinbeta singamma*, -a sinbeta cosgamma*, a cosbeta )
    !!--++    b = (         0         ,     b sinalpha      , b cosalpha)
    !!--++    c = (         0         ,         0           , c         )
    !!--++
    !!--++    If CartType = 'A', the Cartesian system is defined as:
    !!--++         x // a; y is in the ab-plane; z is x ^ z
    !!--++    a = (       a   ,         0           ,       0             )
    !!--++    b = ( b cosgamma,    b singamma       ,       0             )
    !!--++    c = (  c cosbeta, -c sinbeta cosalpha*, c sinbeta sinalpha* )
    !!--++
    !!--++    The output matrix is the tranposed of the above one(s) so that the
    !!--++    matrix can directly be used for transforming "components" given
    !!--++    in a crystallographic basis to "components" in cartesian basis
    !!--++    when the components are used as "column" vectors.
    !!--++
    !!--++      [a] = C [e] , In [a],[e] basis vectors are in column form
    !!--++      (a) = (e) CT, In (a),(e) basis vectors are in row form
    !!--++      CrystOrt = CT  => (a) = (e) CystOrt, in ITC: (a) = (e) P
    !!--++
    !!--++    Remember that  C.CT = GD (direct cell metrics)
    !!--++
    !!--++
    !!--++      Xc = CrystOrt X (Xc Cartesian components, X crystallographic components)
    !!--++
    !!--++ 10/04/2019
    !!
    Module Function Get_Cryst_Orthog_Matrix(Cell, Ang, CarType) Result(Mat)
       !---- Arguments ----!
       real(kind=cp), dimension(3  ), intent (in ) :: cell,ang   ! Cell Parameters
       character(len=*), optional,    intent (in ) :: CarType    ! Type of Cartesian axes
       real(kind=cp), dimension(3,3)               :: Mat        ! Conversion matrix

       !---- Local Variables ----!
       character(len=:), allocatable :: car
       real(kind=cp)                 :: cosgas, singas, cosbes, sinbes


       !> Init and checks
       Car="            "
       if (present(CarType)) then
          Car=U_case(adjustl(CarType))

          !> Check for valid input
          if (len_trim(Car) == 2) then  !> two symbols input
             if (Car /= 'CA' .and. Car /= 'AC' .and. Car /= 'BC' .and. Car /= 'BA') then
                Err_CFML%IErr=-1  ! Warning
                Err_CFML%Msg="GET_CRYST_ORTHOG_MATRIX@METRICS: Invalid CarType. Reset to default! "
                Car='CA'     !default: c//Z, a*//X
             end if

          else !> one symbol input

             select case(Car(1:1))
                case('C')
                    Car(2:2)='A'

                case('A')
                    Car(2:2)='C'

                case('B')
                    Car(2:2)='C'        !defaults to c* // Z for this case

                case default
                    Car='CA'            !default because invalid first character
                end select
          end if
       end if
       if (len_trim(Car) == 0) Car='CA' !> default: c//Z, a*//X

       !> Setting of matrix
       Select Case(Car)
          case('CA')           ! This is the default c//Z, a*//X
             !  Transponse of the following matrix:
             !    a = (a sinbeta singamma*, -a sinbeta cosgamma*, a cosbeta )
             !    b = (         0         ,     b sinalpha      , b cosalpha)
             !    c = (         0         ,         0           , c         )
             cosgas =(cosd(ang(1))*cosd(ang(2))-cosd(ang(3)))/(sind(ang(1))*sind(ang(2)))
             singas = sqrt(1.0_cp-cosgas**2)
             Mat(1,1) = cell(1)*sind(ang(2))*singas
             Mat(1,2) = 0.0_cp
             Mat(1,3) = 0.0_cp
             Mat(2,1) =-cell(1)*sind(ang(2))*cosgas
             Mat(2,2) = cell(2)*sind(ang(1))
             Mat(2,3) = 0.0_cp
             Mat(3,1) = cell(1)*cosd(ang(2))
             Mat(3,2) = cell(2)*cosd(ang(1))
             Mat(3,3) = cell(3)

           case('AC')  !This is the alternate case in the version prior to 2019.
             ! Incorrectly labelled in 2019 as x//a and Y // b*  => AB!!!!
             !  It is really x//a and Z // c* => AC
             !  Transponse of the following matrix:
             !    a = (       a   ,         0           ,       0             )
             !    b = ( b cosgamma,    b singamma       ,       0             )
             !    c = (  c cosbeta, -c sinbeta cosalpha*, c sinbeta sinalpha* )
             cosgas =(cosd(ang(3))*cosd(ang(2))-cosd(ang(1)))/(sind(ang(3))*sind(ang(2))) ! This is actually cos(alpha*) but called cosgas!!
             singas = sqrt(1.0-cosgas**2)
             Mat(1,1) = cell(1)
             Mat(1,2) = cell(2)*cosd(ang(3))
             Mat(1,3) = cell(3)*cosd(ang(2))
             Mat(2,1) = 0.0
             Mat(2,2) = cell(2)*sind(ang(3))
             Mat(2,3) =-cell(3)*sind(ang(2))*cosgas
             Mat(3,1) = 0.0
             Mat(3,2) = 0.0
             Mat(3,3) = cell(3)*sind(ang(2))*singas

          case('BC')  ! This is Carpenter orientation with b // Y, c* // Z, coded by RJA
             cosbes=(cosd(ang(1))*cosd(ang(3)) - cosd(ang(2)))/(sind(ang(1))*sind(ang(3)))
             sinbes=sqrt(1.0_cp-cosbes**2)
             Mat(1,1)=cell(1)*sind(ang(3))
             Mat(2,1)=cell(1)*cosd(ang(3))
             Mat(3,1)=0.0_cp
             Mat(1,2)=0.0_cp
             Mat(2,2)=cell(2)
             Mat(3,2)=0.0_cp
             Mat(1,3)=-1.0_cp*cell(3)*sind(ang(1))*cosbes
             Mat(2,3)=cell(3)*cosd(ang(1))
             Mat(3,3)=cell(3)*sind(ang(1))*sinbes

          case('BA') ! Angel and Brown with Y // b and  X // a*
             cosbes=(cosd(ang(1))*cosd(ang(3)) - cosd(ang(2)))/(sind(ang(1))*sind(ang(3)))
             sinbes=sqrt(1.0_cp-cosbes**2)
             Mat(1,1)=cell(1)*sind(ang(3))*sinbes
             Mat(2,1)=cell(1)*cosd(ang(3))
             Mat(3,1)= -1.0_cp*cell(1)*sind(ang(3))*cosbes
             Mat(1,2)=0.0_cp
             Mat(2,2)=cell(2)
             Mat(3,2)=0.0_cp
             Mat(1,3)=0.0_cp
             Mat(2,3)=cell(3)*cosd(ang(1))
             Mat(3,3)=cell(3)*sind(ang(1))

       End Select

    End Function Get_Cryst_Orthog_Matrix

    !!----
    !!---- SET_CELL
    !!----    Constructs the object "Cell"
    !!----
    !!----    This function is the subroutine set_crystal_cell transformed
    !!----    into a function. It has been introduced because it is much
    !!----    better suited to be called from Python this way.
    !!----
    !!----    WARNING: This function crashes at runtime in DEBUG mode if
    !!----    some of the pointers pointing to an array, i.e., ptr_vscell
    !!----    or ptr_vsang, is null when passed to set_crystal_cell.
    !!----
    !!---- 23/09/2024
    !!
    Module Function Set_Cell(vcell,vang,carttype,vscell,vsang) result(cell)
      !---- Arguments ----!
      real(kind=cp), dimension(3),           intent(in) :: vcell    ! Cell parameters a,b,c
      real(kind=cp), dimension(3),           intent(in) :: vang     ! Cell parameters alpha,beta,gamma
      character(len=*),            optional, intent(in) :: carttype ! Cartesian coordinate system: 'CA' | 'BC' | 'BA' | 'CB'
      real(kind=cp), dimension(3), optional, intent(in) :: vscell   ! Standard deviations of vcell
      real(kind=cp), dimension(3), optional, intent(in) :: vsang    ! Standard deviations of vang
      type(Cell_G_Type)                                 :: cell     ! Crystal cell

      ! Local variables
      character (len=2),           target  :: carttype_
      real(kind=cp), dimension(3), target  :: vscell_
      real(kind=cp), dimension(3), target  :: vsang_
      character(len=:),            pointer :: ptr_carttype => null()
      real(kind=cp), dimension(:), pointer :: ptr_vsang => null()
      real(kind=cp), dimension(:), pointer :: ptr_vscell => null()

      if (present(carttype)) then
         carttype_ = carttype
         ptr_carttype => carttype_
      end if
      if (present(vscell)) then
         vscell_ = vscell
         ptr_vscell => vscell_
      end if
      if (present(vsang)) then
         vsang_ = vsang
         ptr_vsang => vsang_
      end if

      call set_crystal_cell(vcell,vang,cell,ptr_carttype,ptr_vscell,ptr_vsang)

   End Function Set_Cell

    !!----
    !!---- RECIPROCAL_CELL
    !!----    Calculates the reciprocal lattice vectors
    !!----
    !!---- 10/04/2019
    !!
    Pure Module Subroutine Reciprocal_Cell(cell,ang,rcell,rang,rVol)
       !---- Arguments ----!
       real(kind=cp), dimension(3), intent(in ) :: cell,ang
       real(kind=cp), dimension(3), intent(out) :: rcell,rang
       real(kind=cp),               intent(out) :: rvol

       !---- Local Variables ----!
       real(kind=cp)  :: vol

       !> Init
       rcell=0.0_cp; rang=0.0_cp; rvol=0.0_cp     ! Initializing return values
       rvol=0.0_cp

       vol=Volume_from_Cell(cell,ang)
       if (vol <= EPS .or. (Err_CFML%IErr /= 0)) return

       rvol=1.0_cp/vol

       rcell(1)=cell(2)*Cell(3)*sind(ang(1))*rvol
       rcell(2)=cell(3)*Cell(1)*sind(ang(2))*rvol
       rcell(3)=cell(1)*Cell(2)*sind(ang(3))*rvol
       rang(1)=(cosd(ang(2))*cosd(ang(3))-cosd(ang(1)))/(sind(ang(2))*sind(ang(3)))
       rang(2)=(cosd(ang(1))*cosd(ang(3))-cosd(ang(2)))/(sind(ang(1))*sind(ang(3)))
       rang(3)=(cosd(ang(2))*cosd(ang(1))-cosd(ang(3)))/(sind(ang(2))*sind(ang(1)))
       rang=acosd(rang)

    End Subroutine Reciprocal_Cell

    !!----
    !!---- SET_CRYSTAL_CELL
    !!----    Constructs the object "Cell"
    !!----
    !!----    Valid for CELL_TYPE, CELL_G_TYPE, CELL_LS_TYPE, CELL_GLS_TYPE
    !!----
    !!---- 10/04/2019
    !!
    Module Subroutine Set_Crystal_Cell(VCell,VAng,Cell,Cartype,Vscell,Vsang)
       !---- Arguments ----!
       real(kind=cp), dimension(3),         intent(in)  :: Vcell, Vang    ! Cell parameters
       class(Cell_G_Type),               intent(in out) :: Cell           ! Cell Object
       character (len=*),          optional,intent(in ) :: CarType        ! Orientation in Cartesian
       real(kind=cp), dimension(3),optional,intent(in ) :: Vscell, Vsang  ! Standard deviations

       !> a,b,c
       cell%cell=vcell
       cell%scell=0.0_cp
       if (present(Vscell)) cell%scell=Vscell

       !> angles
       cell%ang=vang
       where(cell%ang < EPS) cell%ang =90.0_cp
       cell%sang=0.0_cp
       if (present(Vsang)) cell%sang=Vsang

       !> Volume
       cell%vol=Volume_from_Cell(Vcell, Vang)
       cell%svol=0.0_cp
       if (cell%vol <= EPS) then
          Err_CFML%IErr=1
          Err_CFML%Msg="SET_CRYSTAL_CELL@METRICS: Negligible volume for the current cell parameters!"
          return
       end if
       cell%svol=sigmaV_from_Cell(cell)

       !> Reciprocal
       call reciprocal_cell(Vcell,Vang,cell%rcell,cell%rang,cell%rvol)

       !> GD and GR
       cell%GD=Get_metrics(Vcell,Vang)
       cell%GR=Get_metrics(cell%rcell,cell%rang)

       !> Cartesian conversion
       if (present(Cartype)) then
          cell%cartType=adjustl(u_case(cartype))
       else
          cell%CartType="CA"
       end if
       Cell%Cr_Orth_cel=Get_Cryst_Orthog_matrix(Vcell,Vang,cell%CartType)
       Cell%Orth_Cr_cel=inverse_matrix(Cell%Cr_Orth_cel)
       if (Err_CFML%IErr ==1) then
          Err_CFML%Msg="SET_CRYSTAL_CELL@METRICS: Probably wrong cell parameters. Please, check it!"
          return
       end if

       !> Busing-Levy matrix component
       !(it corresponds to the transpose of Orth_Cr_cel when Celda%CartType="CA")
       if (cell%CartType == "CA") then
          cell%bl_m=Transpose(Cell%Orth_Cr_cel)
          cell%inv_bl_m=Transpose(Cell%Cr_Orth_cel)

       else
          Cell%bl_m(1,1)=cell%rcell(1)
          Cell%bl_m(1,2)=cell%rcell(2)*cosd(cell%rang(3))
          Cell%bl_m(1,3)=cell%rcell(3)*cosd(cell%rang(2))
          Cell%bl_m(2,2)=cell%rcell(2)*sind(cell%rang(3))
          Cell%bl_m(2,3)=-(cell%rcell(3)*sind(cell%rang(2))*cosd(cell%ang(1)))
          Cell%bl_m(3,3)=1.0_cp/cell%cell(3)
          Cell%bl_m(2,1)=0.0_cp
          Cell%bl_m(3,1)=0.0_cp
          Cell%bl_m(3,2)=0.0_cp
          Cell%Inv_bl_M=inverse_matrix(Cell%bl_M)
          if (Err_CFML%IErr ==1) then
             Err_CFML%Msg="SET_CRYSTAL_CELL@METRICS: Wrong cell parameters. Please, check it!"
             return
          end if
       end if

    End Subroutine Set_Crystal_Cell

    !!----
    !!---- GET_CRYST_FAMILY
    !!----    Obtain the Crystal Family, Symbol and System from cell parameters
    !!----
    !!---- 10/04/2019
    !!----
    Module Subroutine Get_Cryst_Family(Cell, Family, Symbol, System)
       !---- Arguments ----!
       class(Cell_G_Type),     intent(in ) :: Cell
       character(len=*),       intent(out) :: Family
       character(len=*),       intent(out) :: Symbol
       character(len=*),       intent(out) :: System

       !---- Local variables ----!
       integer, dimension(3) :: icodp, icoda
       integer               :: n1,n2

       !> Init
       Family=" "
       Symbol=" "
       System=" "

       icodp=0; icoda=0

       !> a
       icodp(1)=1

       !> b
       if (abs(cell%cell(2)-cell%cell(1)) <= EPS) then
          icodp(2)=icodp(1)
       else
          icodp(2)=2
       end if

       !> c
       if (abs(cell%cell(3)-cell%cell(1)) <= EPS) then
          icodp(3)=icodp(1)
       else
          icodp(3)=3
       end if

       !> alpha
       icoda(1)=1

       !> beta
       if (abs(cell%ang(2)-cell%ang(1)) <= EPS) then
          icoda(2)=icoda(1)
       else
          icoda(2)=2
       end if

       !>gamma
       if (abs(cell%ang(3)-cell%ang(1)) <= EPS) then
          icoda(3)=icoda(1)
       else
          icoda(3)=3
       end if

       n1=count(icoda==icoda(1))  ! angles
       n2=count(icodp==icodp(1))  ! parameters

       select case (n1)
          case (1) ! All angles are differents
             Family="Triclinic"
             Symbol ="a"
             System ="Triclinic"

          case (2) ! two angles are equal
             if (icoda(1) == icoda(2)) then
                if (abs(cell%ang(3)-120.0) <= EPS) then
                   if (icodp(1)==icodp(2)) then
                      !> Hexagonal
                      Family="Hexagonal"
                      Symbol ="h"
                      System ="Hexagonal"
                   end if
                else
                   !> Monoclinic
                   Family="Monoclinic"
                   Symbol ="m"
                   System ="Monoclinic"
                end if

             else
                !> Monoclic b-unique setting
                if (abs(cell%ang(1)-90.0) <= EPS) then
                   Family="Monoclinic"
                   Symbol ="m"
                   System ="Monoclinic"
                end if
             end if

          case (3) ! all angles are equals
             if (abs(cell%ang(1) - 90.000) <= EPS) then
                select case (n2)
                   case (1)
                      !> Orthorhombic
                      Family="Orthorhombic"
                      Symbol ="o"
                      System ="Orthorhombic"

                   case (2)
                      !> Tetragonal
                      if (icodp(1)==icodp(2)) then
                         Family="Tetragonal"
                         Symbol ="t"
                         System ="Tetragonal"
                      end if

                   case (3)
                      !> Cubic
                      Family="Cubic"
                      Symbol ="c"
                      System ="Cubic"
                end select

             else
                if (n2 == 3) then
                   !> Hexagonal with rhombohedral axes
                   Family="Hexagonal"
                   Symbol ="h"
                   System ="Trigonal"
                end if
             end if

       end select ! n

       !> Error?
       if (len_trim(Family) <= 0) then
          Err_CFML%IErr=1
          Err_CFML%Msg="GET_CRYST_FAMILY@METRICS: Error obtaining the Crystal Family!"
       end if

    End Subroutine Get_Cryst_Family

    !!----
    !!---- GET_DERIV_ORTH_CELL
    !!----    Subroutine to get derivative matrix of the transformation matrix
    !!----    to orthogonal frame. Useful for calculations of standard deviations
    !!----    of distances and angles. The specialised subroutine calculating
    !!----    sigmas of distances "distance_and_sigma" is in Atom_mod.
    !!----
    !!----    The output matricx "Deriv_Orthcell" are the derivatives of, with
    !!----    respect to a(1),b(2),c(3),alpha(4),beta(5) and gamma(6) of the
    !!----    matrix   "Cell%Cr_Orth_cel".
    !!----
    !!---- 10/04/2019
    !!
    Module Function Get_Deriv_Orth_Cell(Cell,Cartype) Result(De_Orthcell)
       !---- Arguments ----!
       class(Cell_G_Type),              intent(in ) :: cell
       character(len=*), optional,      intent(in ) :: CarType
       real(kind=cp), dimension(3,3,6)              :: De_Orthcell

       !---- Local Variables ----!
       real(kind=cp)    ::  ca,cb,cg,sa,sb,sg,f,g, fa,fb,fc,ga,gb,gc
       character(len=2) :: car

       !> Init
       De_Orthcell=0.0_cp

       ca=cosd(cell%ang(1))
       cb=cosd(cell%ang(2))
       cg=cosd(cell%ang(3))
       sa=sind(cell%ang(1))
       sb=sind(cell%ang(2))
       sg=sind(cell%ang(3))

       !> Init and checks
       car=" "
       if (present(CarType)) then
          Car=U_case(adjustl(CarType))

          !> Check for valid input
          if (len_trim(Car) == 2) then  ! two symbols input
             if (Car /= 'CA' .and. Car /= 'AB' .and. Car /= 'BC' .and. Car /= 'BA') then
                Err_CFML%Ierr=1
                Err_CFML%Msg="GET_DERIV_ORTH_CELL@METRICS: Invalid CarType! "
                return
             end if
             if (Car == 'BC' .or. Car == 'BA') then
                Err_CFML%Ierr=1
                Err_CFML%Msg="GET_DERIV_ORTH_CELL@METRICS: CarType not supported! "
                return
             end if

          else ! one symbol input
             select case(Car(1:1))
                case('C')
                   Car(2:2)='A'

                case('A')
                   Car(2:2)='B'

                case('B')
                   Err_CFML%Ierr=1
                   Err_CFML%Msg="GET_DERIV_ORTH_CELL@METRICS: CarType not supported! "
                   return

                case default
                   Car='CA'            !> default because invalid first character
             end select
          end if
       end if

       if (len_trim(Car) == 0) Car='CA' !> default: c//Z, a*//X

       select case (car)
          case ('AB') ! x//a was original alternate setting
             f=(ca-cb*cg)/sg    !-cosgas*sinbeta
             g=SQRT(sb*sb-f*f)  ! singas*sinbeta
             fa=-sa/sg          ! df/dalpha
             fb=sb*cg/sg        ! df/dbeta
             fc=cb/sg**2        ! df/dgamma
             ga=-f*fa/g         ! dg/dalpha
             gb=(sb*cb-f*fb)/g  ! dg/dbeta
             gc=f/g*fc          ! dg/dgamma

             ! M: Transponse of the following matrix:
             !    a = (       a   ,         0           ,       0             )
             !    b = ( b cosgamma,    b singamma       ,       0             )
             !    c = (  c cosbeta, -c sinbeta cosalpha*, c sinbeta sinalpha* )

             !
             !        (   a         b*cg        c*cb )
             !    M = (   0         b*sg        c*f  )
             !        (   0          0          c*g  )
             !
             !           (   1      0      0 )
             !  dM_da =  (   0      0      0 )
             !           (   0      0      0 )
             de_Orthcell(1,1,1) = 1.0

             !           (   0      cg     0 )
             !  dM_db =  (   0      sg     0 )
             !           (   0      0      0 )
             de_Orthcell(1,2,2) = cg
             de_Orthcell(2,2,2) = sg

             !
             !            (   0          0          cb )
             !  dM_dc =   (   0          0          f  )
             !            (   0          0          g  )
             de_Orthcell(1,3,3) = cb
             de_Orthcell(2,3,3) = f
             de_Orthcell(3,3,3) = g

             !
             !             (   0          0           0   )
             ! dM_dalpha=  (   0          0          c*fa )
             !             (   0          0          c*ga )
             !
             de_Orthcell(2,3,4) = cell%cell(3)*fa
             de_Orthcell(3,3,4) = cell%cell(3)*ga

             !
             !             (   0          0         -c*sb )
             ! dM_dbeta =  (   0          0          c*fb )
             !             (   0          0          c*gb )
             !
             de_Orthcell(1,3,5) = -cell%cell(3)*sb
             de_Orthcell(2,3,5) =  cell%cell(3)*fb
             de_Orthcell(3,3,5) =  cell%cell(3)*gb

             !
             !              (   0        -b*sg         0   )
             ! dM_dgamma =  (   0         b*cg        c*fc )
             !              (   0          0          c*gc )
             !
             de_Orthcell(1,2,6) = -cell%cell(2)*sg
             de_Orthcell(2,2,6) =  cell%cell(2)*cg
             de_Orthcell(2,3,6) =  cell%cell(3)*fc
             de_Orthcell(3,3,6) =  cell%cell(3)*gc

          case ('CA')
             !
             !  By default, the cartesian frame is such as z//c
             !  Transponse of the following matrix:
             !    a = (a sinbeta singamma*, -a sinbeta cosgamma*, a cosbeta )
             !    b = (         0         ,     b sinalpha      , b cosalpha)
             !    c = (         0         ,         0           , c         )

             !         ( a sinbeta singamma*          0             0 )
             !    M =  (-a sinbeta cosgamma*      b sinalpha        0 )
             !         ( a cosbeta                b cosalpha        c )

             f=(cg-ca*cb)/sa    !-sinbeta . cosgamma*
             g=SQRT(sb*sb-f*f)  ! sinbeta . singamma*
             fa= cb/sa**2       ! df/dalpha
             fb=sb*ca/sa        ! df/dbeta
             fc=-sb/sa          ! df/dgamma
             ga=-f*fa/g         ! dg/dalpha
             gb=(sb*cb-f*fb)/g  ! dg/dbeta
             gc=f/g*fc          ! dg/dgamma

             !         ( a*g        0         0 )
             !    M =  ( a*f      b*sa        0 )
             !         ( a*cb     b*ca        c )

             !
             !           (   g       0      0 )
             !  dM_da =  (   f       0      0 )
             !           (   cb      0      0 )
             de_Orthcell(1,1,1) = g
             de_Orthcell(1,2,1) = f
             de_Orthcell(1,3,1) = cb

             !           (   0      0      0 )
             !  dM_db =  (   0      sa     0 )
             !           (   0      ca     0 )
             de_Orthcell(1,2,2) = sa
             de_Orthcell(3,2,2) = ca

             !
             !            (   0      0      0  )
             !  dM_dc =   (   0      0      0  )
             !            (   0      0      1  )
             de_Orthcell(3,3,3) = 1

             !
             !             ( a*ga         0          0 )
             ! dM_dalpha=  ( a*fa       -b*ca        0 )
             !             (   0         b*sa        0 )
             !
             de_Orthcell(1,1,4) = cell%cell(1)*ga
             de_Orthcell(2,1,4) = cell%cell(1)*fa
             de_Orthcell(2,2,4) =-cell%cell(2)*ca
             de_Orthcell(3,2,4) = cell%cell(2)*sa

             !
             !             (  a*gb        0         0 )
             ! dM_dbeta =  (  a*fb        0         0 )
             !             ( -a*sb        0         0 )
             !
             de_Orthcell(1,1,5) = cell%cell(1)*gb
             de_Orthcell(2,1,5) = cell%cell(1)*fb
             de_Orthcell(3,1,5) =-cell%cell(1)*sb

             !
             !              (  a*gc     0      0   )
             ! dM_dgamma =  (  a*fc     0      0   )
             !              (   0       0      0   )
             !
             de_Orthcell(1,1,6) = cell%cell(1)*gc
             de_Orthcell(2,1,6) = cell%cell(1)*fc

       end select

    End Function Get_Deriv_Orth_Cell

    Module Subroutine Change_Setting_Cell_Symb(Cell,sett,Celln)
       !---- Arguments ----!
       class(Cell_G_Type),            intent( in)     :: Cell
       character(len=*),              intent (in)     :: sett
       class(Cell_G_Type),            intent(out)     :: Celln
       !
       real(kind=cp), dimension(3,3) :: Mat
       call Get_Transf(sett,Mat)
       if (err_cfml%ierr ==0) call Change_Setting_Cell(Cell,Mat,Celln)

    End Subroutine Change_Setting_Cell_Symb
    !!----
    !!---- SUBROUTINE CHANGE_SETTING_CELL_MAT
    !!----
    !!---- Calculates a new cell giving the transformation matrix.
    !!---- The input matrix can be given as the S-matrix in International
    !!---- Tables or its transposed (default) that corresponds to the matrix
    !!---- relating formal column matrices containing the basis vectors.
    !!----
    !!---- Update: February - 2005
    !!
    Module Subroutine Change_Setting_Cell_Mat(Cell,Mat,Celln,Matkind)
       !---- Arguments ----!
       class(Cell_G_Type),            intent( in)   :: Cell
       real(kind=cp), dimension (3,3),intent( in)   :: Mat
       class(Cell_G_Type),            intent(out)   :: Celln
       character(len=*),  optional,   intent (in)   :: Matkind

       !--- Local variables ---!
       integer                       :: i
       real(kind=cp), dimension(3)   :: cellv,angl
       real(kind=cp), dimension(3,3) :: S,GN,ST

       if (present(matkind)) then
          if (matkind(1:2) == "it" .or. matkind(1:2) == "IT" ) then
             S=Mat
             ST=transpose(Mat)
          else
             S=transpose(Mat)
             ST=Mat
          end if
       else
          S=transpose(Mat)
          ST=Mat
       end if

       !> Get the new metric tensor
       !> GDN= Mat GD MatT  or GDN= ST GD S
       GN=matmul(ST,matmul(Cell%GD,S))

       !> Calculate new cell parameters from the new metric tensor
       do i=1,3
          cellv(i)=sqrt(GN(i,i))
       end do
       angl(1)=acosd(GN(2,3)/(cellv(2)*cellv(3)))
       angl(2)=acosd(GN(1,3)/(cellv(1)*cellv(3)))
       angl(3)=acosd(GN(1,2)/(cellv(1)*cellv(2)))

       call Set_Crystal_Cell(cellv,angl,Celln)

    End Subroutine Change_Setting_Cell_Mat

    !!----
    !!---- GET_PRIMITIVE_CELL
    !!----    Subroutine for getting the primitive cell from a centred cell
    !!----    On input Lat_type is the lattice type: P,A,B,C,I,R or F
    !!----    Centred_cell is the Crystal_Cell_Type of the input lattice
    !!----    The subroutine calculates the transformation matric "transfm"
    !!----
    !!---- 10/04/2019
    !!
    Module Subroutine Get_Primitive_Cell(Lat_Type,C_Cell,P_Cell,Transfm)
       !---- Arguments ----!
       character(len=*),              intent(in)  :: lat_type    ! Lattice type
       class(Cell_G_Type),            intent(in)  :: c_cell      ! Input Cell Object
       class(Cell_G_Type),            intent(out) :: p_cell      ! Output Cell Object
       real(kind=cp), dimension(3,3), intent(out) :: transfm     ! Transformation Matrix between Cell objects

       !---- Local variables ----!
       integer                       :: i
       real(kind=cp), dimension(3)   :: celp,celang
       real(kind=cp), dimension(3,3) :: cart,metric
       character(len=1)              :: lat

       lat=adjustl(lat_type)
       Select Case(lat)
          case("a","A")
             transfm= reshape([1.0,0.0,0.0,  0.0,0.5,0.5,  0.0,-0.5,0.5],[3,3])

          case("b","B")
             transfm= reshape([0.5,0.0,0.5,  0.0,1.0,0.0, -0.5, 0.0,0.5],[3,3])

          case("c","C")
             transfm= reshape([0.5,0.5,0.0, -0.5,0.5,0.0,  0.0, 0.0,1.0],[3,3])

          case("i","I")
             transfm= reshape([1.0,0.0,0.0,  0.0,1.0,0.0,  0.5, 0.5,0.5],[3,3])

          case("r","R")
             transfm= reshape([2.0/3.0, 1.0/3.0, 1.0/3.0,  &
                               -1.0/3.0, 1.0/3.0, 1.0/3.0,  &
                               -1.0/3.0,-2.0/3.0, 1.0/3.0],[3,3])

          case("f","F")
             transfm= reshape([0.5,0.0,0.5,  0.5,0.5,0.0,  0.0, 0.5,0.5],[3,3])

          case default  !assumed primitive
             transfm= reshape([1.0,0.0,0.0,  0.0,1.0,0.0,  0.0,0.0,1.0],[3,3])
       End Select

       select type (C_Cell)
          class is (Cell_G_Type)
             transfm=transpose(transfm)
             cart=matmul(transfm,transpose(C_Cell%Cr_Orth_cel))
             metric=matmul(cart,transpose(cart))

             !> Calculate new cell parameters from the new metric tensor
             do i=1,3
                Celp(i)=sqrt(metric(i,i))
             end do

             celang(1)=acosd(metric(2,3)/(celp(2)*celp(3)))
             celang(2)=acosd(metric(1,3)/(celp(1)*celp(3)))
             celang(3)=acosd(metric(1,2)/(celp(1)*celp(2)))

          class default
             celp=c_cell%cell
             celang=c_cell%ang
       end select
       call Set_Crystal_Cell(celp,celang,p_cell)

    End Subroutine Get_Primitive_Cell

    !!----
    !!---- GET_TRANSFM_MATRIX
    !!----    Get the transformation matrix between two
    !!----    primitive unit cells (the range of indices is fixed to -2 to 2)
    !!----
    !!---- 10/04/2019
    !!
    Module Function Get_Transfm_Matrix(cella,cellb,tol) Result(Trm)
       !---- Arguments ----!
       class(Cell_G_Type),            intent(in) :: cella  ! Cell object
       class(Cell_G_Type),            intent(in) :: cellb  ! Cell object
       real(kind=cp), optional,       intent(in) :: tol    ! Tolerance
       real(kind=cp), dimension(3,3)             :: trm    ! Transformation matrix

       !---- Local variables ----!
       type(Cell_G_Type)       :: Cellt
       integer,dimension(3,3)  :: Nu
       integer, parameter      :: NLIM=2
       integer                 :: j,i1,i2,i3,i4,i5,i6,i7,i8,i9
       real(kind=cp)           :: tolt
       Logical                 :: ok

       !> Init
       tolt=0.3
       if(present(tol)) tolt=tol
       Trm=0.0_cp

       ok=.false.

       dox: do i1=-NLIM,NLIM                     !         |i1  i4  i7|
          do i2=-NLIM,NLIM                       !    Nu = |i2  i5  i8|
             do i3=-NLIM,NLIM                    !         |i3  i6  i9|
                do i4=-NLIM,NLIM
                   do i5=-NLIM,NLIM
                      do i6=-NLIM,NLIM
                         do i7=-NLIM,NLIM
                            do i8=-NLIM,NLIM
                               do i9=-NLIM,NLIM
                                  j=i1*i5*i9+i4*i8*i3+i2*i6*i7-i3*i5*i7-i8*i6*i1-i2*i4*i9     !determinant (much faster than calling determ_A)
                                  if ( j /= 1) cycle

                                  Nu=reshape([i1,i2,i3,i4,i5,i6,i7,i8,i9],[3,3])
                                  Trm=real(Nu)
                                  call Change_Setting_Cell(Cella,Trm,Cellt)
                                  if (Sum(abs(Cellt%cell(:)-Cellb%cell(:)))+Sum(abs(Cellt%ang(:)-Cellb%ang(:))) < tolt  ) then
                                     ok=.true.
                                     exit dox
                                  end if
                               end do    !i9
                            end do     !i8
                         end do      !i7
                      end do       !i6
                   end do        !i5
                end do         !i4
             end do          !i3
          end do           !i2
       end do  dox       !i1

       if (.not. ok) then
          Err_CFML%Ierr=1
          Err_CFML%Msg="GET_TRANSF_MATRIX@METRICS: Error in Transformation Matrix between Cells!"
       end if

    End Function Get_Transfm_Matrix

    !!----
    !!---- Module Function Get_Basis_From_UVW(dmin,u,cell,mode) Result(ZoneB)
    !!----
    !!----  Function to construct ZA of type Zone_Axis. This subroutine picks up two reciprocal
    !!----  lattice vectors satisfying the equation
    !!----                            hu+kv+lw=0
    !!----  The two reciprocal lattice vectors have no coprime factors and
    !!----  constitute the basis of a reciprocal lattice plane. They are
    !!----  obtained as the shortest two reciprocal lattice vectors satisfying
    !!----  the above equation. If mode is provided and mode="R", we interpret
    !!----  that the input zone axis is a reciprocal lattice vector and what we
    !!----  obtain is the basis of a direct plane in terms of lattice vectors.
    !!----
    !!----  If mode="R", dmin corresponds n(uvw)max
    !!----
    !!---- 11/04/2019, Updated June 2020
    !!
    Module Function Get_Basis_From_UVW(dmin,u,cell,mode) Result(ZoneB)
       !--- Arguments ---!
       real(kind=cp),              intent(in) :: dmin      ! Minimum d-spacing (smax=1/dmin)
       integer, dimension(3),      intent(in) :: u         ! Zone axis indices
       class(Cell_G_Type),         intent(in) :: cell      ! Cell object
       character(len=*), optional, intent(in) :: mode
       type (Zone_Axis_Type)                  :: ZoneB     ! !Object containing u and basis vector in the plane

       !--- Local Variables ---!
       integer                        :: n,ik,il,um,iv,i1,i2,i,coun01,coun02,coun1,coun2
       integer                        :: kmin,kmax,lmin,lmax
       integer,dimension(3)           :: au,h,mu
       real(kind=cp), dimension(2)    :: rm
       real(kind=cp), dimension(3,3)  :: mat
       integer,dimension(3,2)         :: bas
       real(kind=cp)                  :: rv,s2max
       logical                        :: ok

       !> Init
       ZoneB%nlayer=0
       ZoneB%uvw=u

       if (present(mode)) then
          Mat=cell%GD
       else
          Mat=cell%GR
       end if

       ok=.false.

       au=abs(u)
       um=3*maxval(au)
       i=maxloc(au,dim=1)
       iv=u(i)
       mu=u
       if (iv < 0) then
          mu=-u
          iv=-iv
       end if

       Select Case (i)
          Case(1)
             i1=2; i2=3

          Case(2)
             i1=1; i2=3

          Case(3)
             i1=1; i2=2
       End Select

       rm(1)=100000.0; rm(2)=rm(1)
       bas(:,1) = [71,121, 113]
       bas(:,2) = [117, 91,-111]

       if (present(mode)) then    ! Direct
          s2max=dmin*dmin   !here dmin is really n_max
          kmax=nint(dmin/Cell%cell(i1)+1.0)
          lmax=nint(dmin/Cell%cell(i2)+1.0)
          kmax=min(um,kmax)
          lmax=min(um,lmax)
          !mat=cell%GD

       else
          s2max=1.0/(dmin*dmin)
          kmax=nint(Cell%cell(i1)/dmin+1.0)
          lmax=nint(Cell%cell(i2)/dmin+1.0)
          kmax=min(um,kmax)
          lmax=min(um,lmax)
          !mat=cell%gr
       end if

       kmin=-kmax; lmin=-lmax
       coun1=0; coun2=0
       do ik=kmax,kmin,-1
          do il=lmax,lmin,-1
             if (ik == 0 .and. il == 0) cycle
             n=-ik*mu(i1)-il*mu(i2)
             if (mod(n,iv) == 0) then               !n is multiple of iv
                h(i)= n/iv ; h(i1)=ik ; h(i2) = il  !h is solution of hu+kv+lw=0
                rv=dot_product(real(h,kind=cp),matmul(mat,real(h,kind=cp)))
                if (rv > s2max  .or. rv < 1.0e-20) cycle
                if (rv < rm(1)) then
                   if (.not. co_linear(bas(:,1),h,3) ) then
                      bas(:,2)=bas(:,1)
                      rm(2) = rm(1)
                      if (coun1 >=1) coun2=coun2+1
                   end if
                   bas(:,1)=h
                   rm(1) = rv
                   coun1=coun1+1

                else if (rv < rm(2) .and. .not. co_linear(bas(:,1),h,3) ) then
                   bas(:,2)=h
                   rm(2) = rv
                   coun2=coun2+1
                end if
             end if
          end do
       end do

       ZoneB%rx=bas(:,1)
       ZoneB%ry=bas(:,2)
       if (coun1 >= 1 .and. coun2 >=1) ok=.true.

       coun01=0; coun02=0; coun1=0; coun2=0
       do i=1,3
          if (ZoneB%rx(i) < 0) coun1=coun1+1
          if (ZoneB%ry(i) < 0) coun2=coun2+1
          if (ZoneB%rx(i) == 0) coun01=coun01+1
          if (ZoneB%ry(i) == 0) coun02=coun02+1
       end do
       if (coun1 >= 2 .or. (coun1 == 1 .and. coun01 == 2)) ZoneB%rx=-ZoneB%rx
       if (coun2 >= 2 .or. (coun2 == 1 .and. coun02 == 2)) ZoneB%ry=-ZoneB%ry

       if (.not. ok) then
          err_CFML%IErr=1
          Err_CFML%Msg="GET_BASIS_FROM_UVW@METRICS: Problems in this procedure!"
       end if

    End Function Get_Basis_From_Uvw

    !!----
    !!---- GET_TWOFOLD_AXES
    !!----    Subroutine for getting the possible two-fold axes (within an
    !!----    angular tolerance tol) existing in the lattice generated by the
    !!----    unit cell "Celln". Strictly independent two-fold axes are stored
    !!----    in the variable "twofold" that is of type Twofold_Axes_Type
    !!----    The output order of the two-fold axes is ascending in their
    !!----    modulus. Shorter vectors appears before longer ones.
    !!----    The conditions for a reciprocal or direct row to be a two-fold
    !!----    axis are discussed by Y. Le Page in J.Appl.Cryst. 15, 255 (1982).
    !!----
    !!---- 10/04/2019
    !!
    Module Function Get_TwoFold_Axes(Cell,Tol) Result(Twofold)
       !---- Arguments ----!
       class(Cell_G_Type),      intent (in) :: Cell     ! Cell object
       real(kind=cp),           intent (in) :: Tol      ! angular tolerance in degrees
       Type(twofold_axes_type)              :: Twofold

       !---- Local variables ----!
       integer                        :: i,j,n,m, ih,ik,il,iu,iv,iw,imax,ntwo
       real(kind=cp), dimension(3)    :: dv, rv, a, b, c, as, bs, cs, cross
       real(kind=cp), dimension(  12) :: maxes,crossa
       integer, dimension(  12)       :: dota,ind
       real(kind=cp), dimension(3,12) :: caxes
       integer, dimension(3,12)       :: dtw,rtw
       integer, dimension(3)          :: v,h
       real(kind=cp)                  :: dot,crossm

       maxes=0.0; crossa=0.0; dota=0; caxes=0.0; dtw=0; rtw=0
       a=Cell%Cr_Orth_cel(:,1)
       b=Cell%Cr_Orth_cel(:,2)
       c=Cell%Cr_Orth_cel(:,3)
       twofold%a=a
       twofold%b=b
       twofold%c=c
       as=cross_product(b,c)/Cell%Vol !Reciprocal lattice vectors in
       bs=cross_product(c,a)/Cell%Vol !Cartesian components
       cs=cross_product(a,b)/Cell%Vol
       ntwo=0
       imax=2   !Is inough if the input cell is the Buerger or Niggli cell

       do_iu: do iu=imax, 0,-1
          do iv=imax,-imax,-1
             do iw=imax,-imax,-1
                v=[iu,iv,iw]
                if (.not. Co_Prime(v,2)) cycle

                do ih=imax,0,-1
                   do ik=imax,-imax,-1
                      do_il:do il=imax,-imax,-1
                         h=[ih,ik,il]
                         if (.not. Co_Prime(h,2)) cycle

                         n=abs(ih*iu+ik*iv+il*iw)
                         if ( n == 2 .or. n == 1) then
                            dv=real(iu,kind=cp)*a+real(iv,kind=cp)*b+real(iw,kind=cp)*c
                            rv=real(ih,kind=cp)*as+real(ik,kind=cp)*bs+real(il,kind=cp)*cs
                            cross=cross_product(dv,rv)
                            dot=sqrt(dot_product(cross,cross))
                            crossm=atand(dot/real(n,kind=cp))
                            if (abs(crossm) <= tol) then
                               do m=1,ntwo
                                  if (determ_V([17,41,71],v,dtw(:,m) ) == 0) cycle do_il
                               end do
                               ntwo=ntwo+1
                               dtw(:,ntwo)= v
                               dv=v(1)*a+v(2)*b+v(3)*c
                               caxes(:,ntwo)=dv
                               maxes(ntwo)=sqrt(dot_product(dv,dv))
                               rtw(:,ntwo)= h
                               dota(ntwo)=n
                               crossa(ntwo)=crossm
                            end if
                            if (ntwo == 12) exit do_iu
                         end if
                      end do do_il
                   end do
                end do
             end do
          end do
       end do do_iu

       ind=sort(maxes,ntwo)
       do i=1,ntwo
          j=ind(i)
          twofold%dtwofold(:,i)= dtw(:,j)
          twofold%caxes(:,i)= caxes(:,j)
          twofold%maxes(i)= maxes(j)
          twofold%rtwofold(:,i)= rtw(:,j)
          twofold%dot(i)= dota(j)
          twofold%cross(i)= crossa(j)
       End do
       twofold%ntwo=ntwo
       twofold%tol=tol

    End Function Get_TwoFold_Axes

    !!----
    !!---- GET_CONVENTIONAL_CELL
    !!----  This subroutine provides the "conventional" (or quasi! being still tested )
    !!----  from the supplied object "twofold" that has been obtained from a previous
    !!----  call to Get_TwoFold_Axes. The conventional unit cell can be deduced from
    !!----  the distribution of two-fold axes in the lattice. The cell produced in this
    !!----  procedure applies some rules for obtaining the conventional cell, for instance
    !!----  in monoclinic lattices (a single two-fold axis) the two-fold axis is along
    !!----  b and the final cell is right handed with a <= c and beta >= 90. It may be
    !!----  A,C or I centred. The convertion to the C-centred setting in the A and I
    !!----  centring, is not attempted. The angular tolerance for accepting a two-fold
    !!----  axis, or higher order axes, as such has been previously set into twofold%tol
    !!----  component. The output Tr-matrix is the transpose of the IT convention.
    !!----  It corresponds to the transformation between formal column matrices containing
    !!----  the basis vectors.
    !!----  The tolerance for comparing distances in angstroms told is optional.
    !!----- By default the used tolerance is 0.2 angstroms.
    !!----
    !!---- 10/04/2019
    !!----
    Module Subroutine Get_Conventional_Cell(Twofold,Cell,Tr,Message,told,lattice)
       !---- Arguments ----!
       Type(Twofold_Axes_Type), intent(in)  :: Twofold
       class(Cell_G_Type),      intent(out) :: Cell
       integer, dimension(3,3), intent(out) :: tr
       character(len=*),        intent(out) :: message
       real(kind=cp), optional, intent(in)  :: told
       character(len=2), optional, intent(out) :: lattice

       !---- Local variables ----!
       integer, dimension(1)          :: ix
       integer, dimension(2)          :: ab
       integer, dimension(3)          :: rw,h1,h2
       integer, dimension(66)         :: inp
       integer, dimension(3,48)       :: row
       real(kind=cp), dimension(3)    :: u,v1,v2,v3,a,b,c,vec,vi,vj,vk
       real(kind=cp), dimension(48)   :: mv
       real(kind=cp), dimension(66)   :: ang
       integer                        :: iu,iv,iw,nax,i,j,k,m,namina,naminb,naminc,ia
       real(kind=cp)                  :: dot,ep,domina,dominb,dominc,aij,aik,ajk
       real(kind=cp)                  :: delt,tola
       logical                        :: ok,hexap, hexac

       !> Init
       a=twofold%a; b=twofold%b; c=twofold%c
       delt=twofold%tol
       ep=cosd(90.0-delt)
       domina=9.0e+30; dominc=domina
       ab=0; mv=0.0; ang=0.0; row=0; inp=0

       tr=reshape([1,0,0,0,1,0,0,0,1],[3,3])
       Call Set_Crystal_Cell([1.0_cp,1.0_cp,1.0_cp],[90.0_cp,90.0_cp,90.0_cp],Cell)
       message=" "

       ok=.true.
       tola=0.2
       if(present(told)) tola=told

       Select Case(twofold%ntwo)
          Case (1)    !> Monoclinic n-2foldaxes=1
             v2=twofold%caxes(:,1)
             u = v2/twofold%maxes(1)
             tr(2,:)=twofold%dtwofold(:,1)
             nax=0
             do iu=-3,3
                do iv=-3,3
                   do_iw: do iw=0,3
                      rw=[iu,iv,iw]
                      !> if(iu == 0 .and. iv == 0 .and. iw == 0) cycle
                      if (.not. Co_Prime(rw,3)) cycle
                      vec=real(iu,kind=cp)*a+real(iv,kind=cp)*b+real(iw,kind=cp)*c
                      dot=sqrt(dot_product(vec,vec))
                      vec=vec/dot
                      if (abs(dot_product(u,vec)) < ep) then
                         do m=1,nax
                            if (co_linear(rw,row(:,m),3)) cycle do_iw
                         end do
                         nax=nax+1
                         row(:,nax) = rw
                         mv(nax) = dot
                         if (dot < domina) then
                            domina=dot
                            namina=nax
                            tr(1,:)=rw
                            v1=real(iu,kind=cp)*a+real(iv,kind=cp)*b+real(iw,kind=cp)*c
                         end if
                      end if
                   end do do_iw
                end do
             end do

             do i=1,nax
                if (i == namina) cycle
                if (mv(i) < dominc) then
                   dominc=mv(i)
                   naminc=i
                end if
             end do
             tr(3,:)=row(:,naminc)
             v3=row(1,naminc)*a+row(2,naminc)*b+row(3,naminc)*c

             !> Length of the three basis vectors should be stored in mv(1),mv(2),mv(3)
             mv(1)=sqrt(dot_product(v1,v1))
             mv(2)=sqrt(dot_product(v2,v2))
             mv(3)=sqrt(dot_product(v3,v3))

             !>The two shortest vectors perpendicular to the primary twofold axis have been found
             !> and the transformation matrix has been constructed
             namina=determ(tr,3)
             if (namina < 0) then   !right handed system
                tr(2,:)=-tr(2,:)
                v2=-v2
                namina=-namina
             end if

             !> Test if beta is lower than 90 in such a case invert c and b
             dominb=dot_product(v1/mv(1),v3/mv(3))
             if (dominb > 0.0) then  !angle beta < 90
                tr(2,:)=-tr(2,:)
                v2=-v2
                tr(3,:)=-tr(3,:)
                v3=-v3
             end if

             Select Case (namina)
                Case(1)
                   message="Monoclinic, primitive cell"
                   if (present(lattice)) lattice="mP"

                Case(2)
                   rw=matmul([0,1,1],tr)
                   if (.not. co_prime(rw,3)) then
                      message="Monoclinic, A-centred cell"
                      if (present(lattice)) lattice="mA"

                   else
                      rw=matmul([1,1,1],tr)
                      if (.not. co_prime(rw,3)) then
                         message="Monoclinic, I-centred cell"
                         if (present(lattice)) lattice="mI"
                      else
                         rw=matmul([1,1,0],tr)
                         if(.not. co_prime(rw,3)) message="Monoclinic, C-centred cell"
                         if (present(lattice)) lattice="mC"
                      end if
                   end if

                Case(3:)
                   message="Error in monoclinic cell"
                   err_CFML%IErr=1
                   err_CFML%Msg="GET_CONVENTIONAL_CELL@METRICS: Error for monoclinic cell!"
                   return
             End Select

          Case (3)    !> Orthorhombic/Trigonal n-2foldaxes=3
             u(1:3)=twofold%maxes(1:3)
             ix=minloc(u)
             namina=ix(1)
             ix=maxloc(u)
             naminc=ix(1)
             if (naminc == namina) then
                namina=1; naminb=2; naminc=3
             else
                do i=1,3
                   if(i == namina) cycle
                   if(i == naminc) cycle
                   naminb=i
                   exit
                end do
             end if
             tr(1,:) = twofold%dtwofold(:,namina)
             tr(2,:) = twofold%dtwofold(:,naminb)
             tr(3,:) = twofold%dtwofold(:,naminc)
             v1 = twofold%caxes(:,namina)
             v2 = twofold%caxes(:,naminb)
             v3 = twofold%caxes(:,naminc)
             mv(1)=twofold%maxes(namina)
             mv(2)=twofold%maxes(naminb)
             mv(3)=twofold%maxes(naminc)

             !> Check the system by verifying that the two-fold axes form 90 (orthorhombic)
             !> or 120 degrees (Trigonal)
             domina=dot_product(v2/mv(2),v3/mv(3))
             dominb=dot_product(v1/mv(1),v3/mv(3))
             dominc=dot_product(v1/mv(1),v2/mv(2))

             if (abs(domina) < ep .and. abs(dominb) < ep .and. abs(dominc) < ep) then !orthorhombic
                namina=determ(tr,3)
                if (namina < 0) then
                   tr(3,:)=-tr(3,:)
                   v3=-v3
                   namina=-namina
                end if
                Select Case (namina)
                   Case(1)
                      message="Orthorhombic, Primitive cell"
                      if (present(lattice)) lattice="oP"

                   Case(2)
                      rw=matmul([0,1,1],tr)
                      if (.not. co_prime(rw,3)) then
                         message="Orthorhombic, A-centred cell"
                         if (present(lattice)) lattice="oA"
                      else
                         rw=matmul([1,1,1],tr)
                         if (.not. co_prime(rw,3)) then
                            message="Orthorhombic, I-centred cell"
                            if (present(lattice)) lattice="oI"
                         else
                            rw=matmul([1,1,0],tr)
                            if (.not. co_prime(rw,3)) then
                               message="Orthorhombic, C-centred cell"
                               if (present(lattice)) lattice="oC"
                            else
                               rw=matmul([1,0,1],tr)
                               if (.not. co_prime(rw,3)) message="Orthorhombic, B-centred cell"
                               if (present(lattice)) lattice="oB"
                            end if
                         end if
                      end if

                   Case(3:)
                      message="Orthorhombic, F-centred cell"
                      if (present(lattice)) lattice="oF"
                End Select

             else !Rhombohedral/Trigonal

                !> In the Trigonal system the two-fold axes are in the plane perpendicular to
                !> the three-fold axis, and valid a,b, vectors can be chosen among any two two-fold
                !> axes forming an angle of 120 degrees
                !> verify that 1 and 2 form 120
                ang(1)=acosd(domina)    !2-3
                ang(2)=acosd(dominb)    !1-3
                ang(3)=acosd(dominc)    !1-2
                dot=1.0
                iu=1
                j=0
                do i=1,3
                   if (abs(ang(i)-120.0) < delt) then
                      j=i
                      exit
                   end if
                end do

                if ( j == 0) then
                   do i=1,3
                      if (abs(ang(i)-60.0) < delt) then
                         j=i
                         dot=-1.0
                         iu=-1
                         exit
                      end if
                   end do
                End if

                if ( j == 0) then
                   message="Trigonal/Rhombohedral test failed! Supply only one two-fold axis"
                   err_CFML%IErr=1
                   err_CFML%flag=.true.
                   err_CFML%Msg="GET_CONVENTIONAL_CELL@METRICS: "//trim(message)
                   return
                else
                   Select Case (j)
                      case(1)
                         vi=v2
                         vj=dot*v3
                         h1=tr(2,:); h2=iu*tr(3,:)
                         tr(3,:)=tr(1,:)
                         tr(1,:)=h1
                         tr(2,:)=h2

                      case(2)
                         vi=v1
                         vj=dot*v3
                         h2=iu*tr(3,:)
                         tr(3,:)=tr(2,:)
                         tr(2,:)=h2

                      case(3)
                         vi=v1
                         vj=dot*v2
                         tr(2,:)=iu*tr(2,:)

                   End Select

                   v1 = vi
                   v2 = vj
                   mv(1)=sqrt(dot_product(v1,v1))
                   mv(2)=sqrt(dot_product(v2,v2))
                   vi=v1/mv(1)
                   vj=v2/mv(2)

                   ok=.false.

                   do_iu: do iu=-3,3
                      do iv=-3,3
                         do iw=0,3
                            rw=[iu,iv,iw]
                            if (.not. Co_prime(rw,3)) cycle
                            vec=real(iu)*a+real(iv)*b+real(iw)*c
                            dot=sqrt(dot_product(vec,vec))
                            vec=vec/dot
                            if (abs(dot_product(vi,vec)) < ep  .and. abs(dot_product(vj,vec)) < ep) then
                               tr(3,:)=rw
                               ok=.true.
                               exit do_iu
                            end if
                         end do
                      end do
                   end do do_iu

                   If (ok) then
                      namina=determ(tr,3)
                      if (namina < 0) then
                         tr(3,:)=-tr(3,:)
                         namina=-namina
                      end if
                      v3 = tr(3,1)*a+tr(3,2)*b+tr(3,3)*c
                      mv(3)=sqrt(dot_product(v3,v3))
                      Select Case (namina)
                         case(1)
                            message="Primitive hexagonal cell"
                            if (present(lattice)) lattice="hP"
                         case(3)
                            rw=matmul([2,1,1],tr)
                            if (.not. co_prime(rw,3)) then
                               message="Rhombohedral, obverse setting cell"
                            if (present(lattice)) lattice="hR"
                            else
                               message="Rhombohedral, reverse setting cell"
                            if (present(lattice)) lattice="hR"
                            end if
                      End Select

                   Else
                      message="Trigonal/Rhombohedral test failed! Supply only one two-fold axis"
                      err_CFML%IErr=1
                      err_CFML%flag=.true.
                      err_CFML%Msg="GET_CONVENTIONAL_CELL@METRICS: "//trim(message)
                      return
                   End if
                End if !j==0
             End if  !orthorhombic test

          Case (5)    !> Tetragonal n-2foldaxes=5
             m=0
             inp=0
             mv(1:5)=twofold%maxes(1:5)
             do i=1,twofold%ntwo-1
                vi=twofold%caxes(:,i)/twofold%maxes(i)
                do j=i+1,twofold%ntwo
                   vj=twofold%caxes(:,j)/twofold%maxes(j)
                   m=m+1
                   ang(m)=acosd(dot_product(vi,vj))
                   if (abs(ang(m)-45.0) < delt .or. abs(ang(m)-135.0) < delt) then
                      inp(i)=1
                      inp(j)=1
                      if (mv(i) > mv(j)) then
                         ia=j
                      else
                         ia=i
                      end if
                      if (ab(1) == 0) then
                         ab(1) = ia
                      else
                         ab(2) = ia
                      end if
                   end if
                end do
             end do

             !> Determination of the c-axis (that making 90 degree with all the others)
             ix=minloc(inp)
             naminc=ix(1)

             !> The two axes forming a,b are those of indices ab(1) and ab(2)
             namina=ab(1)
             naminb=ab(2)
             if (namina == 0 .or. naminb == 0) then
                message="Basis vectors a-b not found!"
                err_CFML%IErr=1
                err_CFML%flag=.true.
                err_CFML%Msg="GET_CONVENTIONAL_CELL@METRICS: "//trim(message)
                return
             else
                tr(1,:) = twofold%dtwofold(:,namina)
                tr(2,:) = twofold%dtwofold(:,naminb)
                tr(3,:) = twofold%dtwofold(:,naminc)
                v1 = twofold%caxes(:,namina)
                v2 = twofold%caxes(:,naminb)
                v3 = twofold%caxes(:,naminc)
                mv(1)=twofold%maxes(namina)
                mv(2)=twofold%maxes(naminb)
                mv(3)=twofold%maxes(naminc)
                namina=determ(tr,3)
                if (namina < 0) then
                   tr(3,:)=-tr(3,:)
                   v3=-v3
                   namina=-namina
                end if

                Select Case (namina)
                   Case(1)
                      message="Tetragonal, Primitive cell"
                      if (present(lattice)) lattice="tP"
                   Case(2)
                      message="Tetragonal, I-centred cell"
                      if (present(lattice)) lattice="tI"
                   Case(3:)
                      message="Error in tetragonal cell"
                      err_CFML%IErr=1
                      err_CFML%flag=.true.
                      err_CFML%Msg="GET_CONVENTIONAL_CELL@METRICS: "//trim(message)
                      return
                End Select
             end if

          Case (7)    !Hexagonal n-2foldaxes=7

             m=0
             inp=0
             mv(1:7)=twofold%maxes(1:7)
             hexap=.false.;  hexac=.false.

             !>Search tha a-b plane
             do_ii:do i=1,twofold%ntwo-1
                vi=twofold%caxes(:,i)/twofold%maxes(i)
                do j=i+1,twofold%ntwo
                   vj=twofold%caxes(:,j)/twofold%maxes(j)
                   aij=acosd(dot_product(vi,vj))
                   if (abs(aij-120.0) < delt) then
                      if (abs(mv(i)-mv(j)) < tola .and. .not. hexap ) then
                         rw(1)=i; rw(2)=j
                         u(1)=mv(i); u(2)=mv(j)
                         hexap=.true.
                         exit do_ii
                      end if
                   end if
                end do
             end do do_ii

             if (hexap) then ! Search the c-axis, it should be also a two-fold axis!
                             ! because Op(6).Op(6).Op(6)=Op(2)
                v1 = twofold%caxes(:,rw(1))
                v2 = twofold%caxes(:,rw(2))
                vj=v1/u(1)
                vk=v2/u(2)
                do i=1,twofold%ntwo
                   vi=twofold%caxes(:,i)/twofold%maxes(i)
                   aij=acosd(dot_product(vi,vj))
                   aik=acosd(dot_product(vi,vk))
                   if (abs(aij-90.0) < delt .and. abs(aik-90.0) < delt ) then
                      rw(3)=i
                      u(3)= mv(i)
                      hexac=.true.
                      exit
                   end if
                end do
             else
                err_CFML%IErr=1
                err_CFML%flag=.true.
                err_CFML%Msg="GET_CONVENTIONAL_CELL@METRICS: Error in Hexagonal n-2fold axes=7"
                return
             end if

             if (hexac) then
                do i=1,3
                   tr(i,:) = twofold%dtwofold(:,rw(i))
                   mv(i)=u(i)
                end do
                v3 = twofold%caxes(:,rw(3))
                namina=determ(tr,3)
                if (namina < 0) then
                   tr(3,:)=-tr(3,:)
                   v3=-v3
                   namina=-namina
                end if

                Select Case (namina)
                   Case(1)
                      message="Hexagonal, Primitive cell"
                      if (present(lattice)) lattice="hP"
                   Case(2:)
                      message="Hexagonal, centred cell? possible mistake"
                      if (present(lattice)) lattice="hX"
                End Select

             else
                message="The c-axis of a hexagonal cell was not found!"
                err_CFML%IErr=1
                err_CFML%flag=.true.
                err_CFML%Msg="GET_CONVENTIONAL_CELL@METRICS: "//trim(message)
                return
             end if

          Case (9)   !>Cubic n-2foldaxes=9
             m=0
             inp=0
             mv(1:9)=twofold%maxes(1:9)
             do_i:do i=1,twofold%ntwo-2
                vi=twofold%caxes(:,i)/twofold%maxes(i)
                do j=i+1,twofold%ntwo-1
                   vj=twofold%caxes(:,j)/twofold%maxes(j)
                   do k=j+1,twofold%ntwo
                      vk=twofold%caxes(:,k)/twofold%maxes(k)
                      aij=acosd(dot_product(vi,vj))
                      aik=acosd(dot_product(vi,vk))
                      ajk=acosd(dot_product(vj,vk))
                      if (abs(aij-90.0) < delt .and. abs(aik-90.0) < delt .and. abs(ajk-90.0) < delt ) then
                         if (abs(mv(i)-mv(j)) < tola .and. abs(mv(i)-mv(k)) < tola .and. abs(mv(j)-mv(k)) < tola ) then
                            rw(1)=i; rw(2)=j; rw(3)=k
                            u(1)=mv(i); u(2)=mv(j); u(3)=mv(k)
                            exit do_i
                         end if
                      end if
                   end do
                end do
             end do do_i

             do i=1,3
                tr(i,:) = twofold%dtwofold(:,rw(i))
                mv(i)=u(i)
             end do
             v1 = twofold%caxes(:,rw(1))
             v2 = twofold%caxes(:,rw(2))
             v3 = twofold%caxes(:,rw(3))
             namina=determ(tr,3)
             if (namina < 0) then
                tr(3,:)=-tr(3,:)
                v3=-v3
                namina=-namina
             end if

             Select Case (namina)
                Case(0)
                  message="Pseudo-cubic but tolerance too small ... "
                  err_CFML%IErr=1
                  err_CFML%flag=.true.
                  err_CFML%Msg="GET_CONVENTIONAL_CELL@METRICS: "//trim(message)
                  return
                Case(1)
                   message="Cubic, Primitive cell"
                   if (present(lattice)) lattice="cP"

                Case(2)
                   rw=matmul([0,1,1],tr)
                   if (.not. co_prime(rw,3)) then
                      message="Cubic, A-centred cell"
                      if (present(lattice)) lattice="cA"
                   else
                      rw=matmul([1,1,1],tr)
                      if (.not. co_prime(rw,3)) then
                         message="Cubic, I-centred cell"
                         if (present(lattice)) lattice="cI"
                      else
                         rw=matmul([1,1,0],tr)
                         if (.not. co_prime(rw,3)) then
                            message="Cubic, C-centred cell"
                            if (present(lattice)) lattice="cC"
                         else
                            rw=matmul([1,0,1],tr)
                            if (.not. co_prime(rw,3)) message="Cubic, B-centred cell"
                            if (present(lattice)) lattice="cB"
                         end if
                      end if
                   end if

                Case(3:)
                  message="Cubic, F-centred cell"
                  if (present(lattice)) lattice="cF"

             End Select

          case default
             write(unit=message,fmt="(a,i3)") "Wrong number of two-fold axes! ",twofold%ntwo
             err_CFML%IErr=1
             err_CFML%flag=.true.
             err_CFML%Msg="GET_CONVENTIONAL_CELL@METRICS: "//trim(message)
             return

       End Select

      !> Calculation of the new cell
      ang(1)=acosd(dot_product(v2/mv(2),v3/mv(3)))
      ang(2)=acosd(dot_product(v1/mv(1),v3/mv(3)))
      ang(3)=acosd(dot_product(v1/mv(1),v2/mv(2)))
      Call Set_Crystal_Cell(mv(1:3),ang(1:3),Cell)

    End Subroutine Get_Conventional_Cell

    !!----
    !!---- CART_VECTOR
    !!----    Convert a vector in crystal space to cartesian components
    !!----    The value of code has been extended to use also the Busing-Levy
    !!----    Cartesian system as reference also for direct and reciprocal space.
    !!----    Codes:
    !!----    The Cartesian frame is that defined by the setting of the "Cell" object
    !!----         D: The components are given with respect to basis (a,b,c)
    !!----         R: The components are given with respect to basis (a*,b*,c*)
    !!----        BL: The components are given with respect to basis (a*,b*,c*) but
    !!----            the Cartesian frame is that defined by Busing and Levy
    !!----       BLD: The components are given with respect to basis (a,b,c) but
    !!----            the Cartesian frame is that defined by Busing and Levy
    !!----
    !!---- 10/04/2019
    !!
    Pure Module Function Cart_Vector(Mode,V,Cell) Result(Vc)
       !---- Arguments ----!
       character(len=*),            intent(in) :: mode      !  D: Direct, R: Reciprocal, BL or BLD
       real(kind=cp), dimension(3), intent(in) :: v         !  Vector
       class(Cell_G_Type),          intent(in) :: Cell      !  Cell object
       real(kind=cp), dimension(3)             :: vc        !

       !---- Local variable ----!
       character(len=1) :: car

       !> Init
       car=adjustl(mode)
       Vc=0.0_cp

       select case (car)
          case("d","D")
             vc = matmul(cell%Cr_Orth_cel,v)  !Direct conversion to Cartesian frame

          case ("r","R")
             vc = matmul(cell%GR,v)            !Converts to direct space
             vc = matmul(cell%Cr_Orth_cel,vc)  !Converts to Cartesian frame

          case ("bl","BL")
             vc = matmul(cell%BL_M,vc) !Direct conversion to BL Cartesian frame

          case ("bld","BLD")
             vc = matmul(cell%GD,v)   !Converts to reciprocal space
             vc = matmul(cell%BL_M,vc)!Converts to BL Cartesian frame
       end select

    End Function Cart_Vector

    !!----
    !!---- CART_U_VECTOR
    !!----    Convert a vector in crystal space to unitary cartesian components
    !!----
    !!---- 10/04/2019
    !!
    Pure Module Function Cart_U_Vector(Mode,V,Cell) Result(Vc)
       !---- Arguments ----!
       character(len=*),            intent(in) :: Mode   ! Options, D, R, BL, BLD
       real(kind=cp), dimension(3), intent(in) :: v      ! Vector
       class(Cell_G_Type),          intent(in) :: Cell   ! Cell object
       real(kind=cp), dimension(3)             :: vc

       !---- Local Variables ----!
       real(kind=cp) :: vmod

       !> Init
       vc=0.0_cp

       vc=cart_vector(Mode,v,cell)
       vmod=sqrt(dot_product(vc,vc))
       if (vmod > epsilon(0.0_cp)) vc=vc/vmod

    End Function Cart_U_Vector

    !!----
    !!---- Rot_Gibbs_Matrix(U, PHI, CELDA)
    !!----    Returns the matrix (Gibbs matrix) of the active rotation of "phi" degrees
    !!----    along the "U" direction: R v = v', the vector v is tranformed to vector v'
    !!----    keeping the reference frame unchanged.
    !!----
    !!----    If one wants to calculate the components of the vector "v" in a rotated
    !!----    reference frame it suffices to invoke the function using "-phi".
    !!----    If "Celda" is present, "U" is in "Celda" coordinates,
    !!----    if not "U" is in cartesian coordinates.
    !!----
    !!---- 10/04/2019
    !!
    Pure Module Function Rot_Gibbs_Matrix(V,Phi,Cell) Result(Mat)
       !---- Argument ----!
       real(kind=cp), dimension(3),      intent(in) :: V     ! Direction vector
       real(kind=cp),                    intent(in) :: phi   ! Angle in Degrees of rotation around V
       class(Cell_G_Type), optional,     intent(in) :: cell  ! Cell object
       real(kind=cp), dimension(3,3)                :: Mat   ! Gibbs Matrix corresponding to the rotation around V of an angle Phi

       !---- Local variables ----!
       real(kind=cp)               :: c, s, umc, umod
       real(kind=cp), dimension(3) :: vc

       !> Init
       if (present(cell)) then
          vc= matmul(cell%cr_orth_cel,v)
       else
          vc=v
       end if

       umod=sqrt(dot_product(vc,vc))
       if (umod < tiny(1.0_cp)) then
          vc=[0.0_cp,0.0_cp,1.0_cp]
       else
          vc= vc/umod
       end if

       c= cosd(phi)
       s= sind(phi)
       umc = 1.0-c
       Mat(1,1)= c + umc*vc(1)**2
       Mat(1,2)= umc*vc(1)*vc(2) - s*vc(3)
       Mat(1,3)= umc*vc(1)*vc(3) + s*vc(2)

       Mat(2,1)= umc*vc(2)*vc(1) + s*vc(3)
       Mat(2,2)= c + umc*vc(2)**2
       Mat(2,3)= umc*vc(2)*vc(3) - s*vc(1)

       Mat(3,1)= umc*vc(3)*vc(1) - s*vc(2)
       Mat(3,2)= umc*vc(3)*vc(2) + s*vc(1)
       Mat(3,3)= c + umc*vc(3)**2

    End Function Rot_Gibbs_Matrix

    !!----
    !!---- STRAIN_FROM_CELL
    !!----    Calculates the strain from cell described by T0 to cell described by T1
    !!----    Coded from equations given by Zotov, Acta Cryst. (1990). A46, 627-628
    !!----
    !!----    Ported from WinStrain (RJA): February - 2019
    !!----
    !!---- 19/04/2019
    !!
    Module Function Strain_from_Cell(Itype,T0,T1) Result(strain)
       !---- Arguments ----!
       integer,                       intent(in) :: itype  ! Strain type
       real(kind=cp), dimension(3,3), intent(in) :: T0     ! CR_Orth_Cel for chosen axial system for the starting state
       real(kind=cp), dimension(3,3), intent(in) :: T1     ! CR_Orth_Cel for chosen axial system for the final state
       real(kind=cp), dimension(3,3)             :: Strain ! calculated cell strain

       !--- Local variables ---!
       integer                       :: i, j
       real(kind=cp), dimension(3,3) :: s0, s1, sinv, w1, w2

       !> Init
       strain=0.0_cp
       do i=1,3
          strain(i,i)=0.1        ! safety
       end do

       !> Original literature is written in terms of S matrices:
       !> Zotov, Acta Cryst. (1990). A46, 627-628
       !> These are the transpose of CR_Orth_Cel
       s0=transpose(t0)
       s1=transpose(t1)

       select case (itype)
          case (1) ! Eulerian finite
             Sinv=Inverse_Matrix(S1)
             if (Err_CFML%IErr /=0) return

             w1=matmul(sinv,s0)
             w2=transpose(w1)
             strain=matmul(w1,w2)
             do i=1,3
                do j=1,3
                   strain(i,j)= 0.5_cp*(strain(i,j)-2.0_cp*w1(i,j)-2.0_cp*w1(j,i))
                end do
             end do
             do i=1,3
                strain(i,i)=strain(i,i)+1.5_cp
             end do

          case (2) ! Eulerian infinitesimal
             Sinv=Inverse_Matrix(S1)
             if (Err_CFML%IErr /=0) return

             w1=matmul(sinv,s0)        !
             do i=1,3
                do j=1,3
                   strain(i,j)= -0.5_cp*(w1(i,j)+w1(j,i))
                end do
             end do
             do i=1,3
                strain(i,i)=strain(i,i)+1.0_cp
             end do

          case (3) ! Lagrangian finite
             Sinv=Inverse_Matrix(S0)
             if (Err_CFML%IErr /=0) return

             w1=matmul(sinv,s1)
             w2=transpose(w1)        !
             strain=matmul(w1,w2)
             do i=1,3
                do j=1,3
                   strain(i,j)=0.5_cp*strain(i,j)
                end do
             end do
             do i=1,3
                strain(i,i)=strain(i,i)-0.5_cp
             end do

          case (4) ! Lagrangian infinitesimal
             Sinv=Inverse_Matrix(S0)
             if (Err_CFML%IErr /=0) return

             w1=matmul(sinv,s1)         !
             do i=1,3
                do j=1,3
                   strain(i,j)=0.5_cp*(w1(i,j)+w1(j,i))
                end do
             end do
             do i=1,3
                strain(i,i)=strain(i,i)-1.0_cp
             end do

       end select

    End Function Strain_from_Cell

    !!----
    !!---- Subroutine Find_Lowindex_Dir
    !!----
    !!----
    !!---- 19/01/2021
    !!
    Module Subroutine Find_Lowindex_Dir(Cvec,Cell,Space,Irange,Ind,Ang)
       !---- Arguments ----!
       real(kind=cp), dimension(3), intent(in)  :: Cvec    ! Cartesian vector to be indexed on cell
       type(cell_G_type),           intent(in)  :: Cell    ! the cell params
       character(len=*),            intent(in)  :: Space   ! R or D for recip or direct space
       integer,                     intent(in)  :: Irange  ! maximum value of index to test, will run 0 to +irange for first index, -irange to +irange for others
       real(kind=cp),dimension(3),  intent(out) :: Ind     ! indices of closest direction
       real(kind=cp),               intent(out) :: Ang     ! residual angle to nearest direction (always >=0 and <90)

       !---- Local variables ----!
       integer                     :: i,j,k
       real(kind=cp),dimension(3)  :: vec      !Cartesian vector of direction to be tested
       real(kind=cp)               :: ang_temp
       logical                     :: invert

       !> init
       ind=0.0_cp
       ang=180.0_cp

       !> loop over possible indices
       do i=0,irange
          do j=-1*irange,irange
             do k=-1*irange,irange
                if (index(U_case(space),'D') > 0)then
                   vec=Cart_U_Vector('D',real((/i,j,k/)),cell)
                else
                   vec=Cart_U_Vector('R',real((/i,j,k/)),cell)
                end if
                ang_temp=abs(acosd(dot_product(vec,Cvec)))
                invert=.false.
                if (ang_temp > 90.0_cp)then
                    ang_temp=180._cp-ang_temp
                    invert=.true.
                end if

                if (ang_temp < ang)then
                    ang=ang_temp
                    ind=(/i,j,k/)
                    if(invert)ind=-1*ind
                end if
             end do
          end do
       end do

    End Subroutine Find_Lowindex_Dir

    !!----
    !!---- Subroutine  Calc_Paxes_Angles
    !!----
    !!----    Calculates angles between principal axes and the cell and Cartesian axes
    !!----    No calculation of esd's here
    !!----
    !!---- 19/01/2021
    !!
    Module Subroutine Calc_Paxes_Angles(x,c,index_range)
       !---- Arguments ----!
       type(Strain_Tensor_Type), intent(inout) :: X
       type(cell_G_type),        intent(in)    :: c            ! reference cell to which angles are calculated
       integer,                  intent(in)    :: index_range  ! max index for hunting nearest directions

       !---- Local Variables ----!
       integer                     :: i,j
       real(kind=cp), dimension(3) :: vec

       x%cart_ang(1:3,1:3,1)=acosd(x%evec(1:3,1:3))        ! Angles to Cartesian axes. Trivial, but needed for calculating errors
       x%cart_ang(1:3,1:3,2)=0._cp                         ! esds

       !> Calc angles from Eigenvectors to unit-cell axes
       x%cell_ang(1:3,1:3,1)=0._cp

       !> Angles to the crystal axes
       do i=1,3        ! set the crystal axis
          vec=0.
          vec(i)=1.0
          do j=1,3        ! loop over eigenvectors
             x%cell_ang(i,j,1)=acosd(dot_product(Cart_U_Vector('D',Vec,c),X%Evec(1:3,j)))      ! angle from evector j to cell vector i in real space
             x%cell_ang(i,j,3)=acosd(dot_product(Cart_U_Vector('R',Vec,c),X%Evec(1:3,j)))      ! angle from evector j to cell vector i in recip space
          end do
       end do

       !> Find closest low-index plane normals and lattice vectors to each evec
       do i= 1,3       ! loop over evecs
          call find_lowindex_dir(X%Evec(1:3,i),C,'D',index_range,X%dir_close(i,2,1:3), X%dir_close(i,2,4))
          call find_lowindex_dir(X%Evec(1:3,i),C,'R',index_range,X%dir_close(i,1,1:3), X%dir_close(i,1,4))
       end do

     End Subroutine Calc_Paxes_Angles

End Submodule Metrics_Gen