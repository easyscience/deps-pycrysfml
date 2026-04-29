SubModule (CFML_gSpaceGroups) gS_Get_LattType
   implicit none
   Contains

   !!----
   !!---- GET_LATTICE_TYPE
   !!----
   !!---- Returns the lattice type symbol from lattice centring vectors
   !!----
   !!---- 22/04/2019
   !!
   Module Function Get_Lattice_Type_L(N, Latc) Result(lattyp)
      !---- Arguments ----!
      integer,                        intent( in) :: N
      type(rational), dimension(:,:), intent( in) :: Latc
      character(len=1)                            :: lattyp

      !---- Local variables ----!
      integer :: i,j,nlat
      logical :: latt_p, latt_a, latt_b, latt_c, latt_i, latt_r, &
                 latt_s, latt_h, latt_f, latt_x
      integer,        dimension(10)   :: latt_given
      type(rational), dimension(3,10) :: lattice

      !> Init
      lattyp=" "
      lattice(:,1)  = [ 0//1,1//2,1//2 ]
      lattice(:,2)  = [ 1//2,0//1,1//2 ]
      lattice(:,3)  = [ 1//2,1//2,0//1 ]
      lattice(:,4)  = [ 1//2,1//2,1//2 ]
      lattice(:,5)  = [ 2//3,1//3,1//3 ]
      lattice(:,6)  = [ 1//3,2//3,2//3 ]
      lattice(:,7)  = [ 1//3,2//3,1//3 ]
      lattice(:,8)  = [ 2//3,1//3,2//3 ]
      lattice(:,9)  = [ 2//3,1//3,0//1 ]
      lattice(:,10) = [ 1//3,2//3,0//1 ]

      !> Check for primitive and non conventional centrings
      nlat   = 0
      latt_p = .true.
      do i = 1 , N
          if (.not. Rational_Is_Integer(latc(1:3,i))) then
             latt_p = .false.
             nlat   = nlat + 1
          end if
      end do
      if (latt_p) then    ! primitive
         lattyp="P"
         return
      end if

      if (nlat > 3) then  !non conventional centring
         lattyp="X"
         !Err_CFML%Ierr = 1
         !Err_CFML%Msg = "Get_Lattice_Type@SPACEG: Number of centring vectors > 3."
         !return
      end if

      latt_a=.false.
      latt_b=.false.
      latt_c=.false.
      latt_i=.false.
      latt_r=.false.
      latt_s=.false. ! Rombohedral,reverse setting
      latt_h=.false. ! Hexagonally centred
      latt_f=.false.
      latt_x=.false.

      do i = 1 , N
         latt_given(:) = 0
         do j = 1 , 10
            if (Rational_Equal(latc(1:3,i),lattice(1:3,j))) then
               latt_given(j) = 1
               select case (j)
                  case (1)
                     latt_a=.true.

                  case (2)
                     latt_b=.true.

                  case (3)
                     latt_c=.true.

                  case (4)
                     latt_i=.true.

                  case (5,6)
                     latt_r=.true.

                  case (7,8)
                     latt_s=.true.

                  case (9,10)
                     latt_h=.true.
               end select
               exit
            end if
         end do

         if (sum(latt_given) == 0) then
            latt_x = .true.
            exit
         end if
      end do

      !> Lattice Type
      if (latt_x) then
         lattyp  = "X"
         return
      end if

      if ( (latt_a .and. latt_b .and. latt_c) .or. (latt_a .and. latt_b) .or. &
           (latt_a .and. latt_c) .or. (latt_b .and. latt_c) ) then
         latt_f=.true.
         latt_a=.false.
         latt_b=.false.
         latt_c=.false.
         latt_p=.false.
         latt_i=.false.
      end if

      if (latt_a) lattyp="A"
      if (latt_b) lattyp="B"
      if (latt_c) lattyp="C"
      if (latt_i) lattyp="I"
      if (latt_r) lattyp="R"
      if (latt_s) lattyp="S"
      if (latt_h) lattyp="H"
      if (latt_f) lattyp="F"

      return
   End Function Get_Lattice_Type_L

   !!----
   !!---- GET_LATTICE_TYPE_FROM_MAT
   !!----
   !!---- The M matrix transforms a primitive basis to a standard
   !!---- basis. The columns of the inverse of the M matrix contain
   !!---- the primitive vectors of the lattice expressed in the
   !!---- standard basis.
   !!----
   !!---- 22/04/2019
   !!
   Module Function Get_Lattice_Type_from_MAT(M) Result(lattyp)
      !---- Arguments ----!
      type(rational), dimension(3,3), intent(in)  :: M
      character(len=1)                            :: lattyp

      !---- Local variables ----!
      integer                        :: i,j,n,nLatticePoints,nCentringVectors
      type(rational)                 :: det
      type(rational), dimension(3)   :: t
      type(rational), dimension(3,3) :: Minv,Maux
      type(rational), dimension(3,3) :: latc
      logical                        :: newt
      logical,        dimension(3)   :: isOrigin
      logical                        :: pout

      !> ==== Debug ====
      pout = .false.
      pout = (pout .or. CFML_DEBUG)
      !> ===============

      !> Init
      Call Clear_Error()
      lattyp=" "

      Minv=Rational_Inverse_Matrix(M)
      det = rational_determ(Minv)
      det = (1//1) / det

      if (mod(det%Numerator,det%Denominator) /= 0) then
         Err_CFML%Ierr = 1
         Err_CFML%Msg = "Get_Lattice_Type_From_Mat@SPACEG: Inverse of the determinant is not integer"
         return
      end if

      nLatticePoints = det%Numerator / det%Denominator
      if ( nLatticePoints > 1 ) then
         if (pout) write(*,'(8x,a,i2,1x,a)') " => Standard lattice is non primitive.",&
                   nLatticePoints,"lattice points in the unit cell"

         nCentringVectors = 0
         Maux             = Minv ! Maux stores lattice points inside the unit cell
         isOrigin         = .True.
         do i = 1 , 3
            do j = 1 , 3
               if (mod(Maux(i,j)%Numerator,Maux(i,j)%Denominator) == 0) then ! integer component
                  Maux(i,j) = 0 // 1

               else ! fractional component
                  n = Maux(i,j)%Numerator / Maux(i,j)%Denominator
                  Maux(i,j) = Maux(i,j) - (n//1)
                  if (Maux(i,j) < (0//1)) Maux(i,j) = Maux(i,j) + (1//1)
                  isOrigin(j) = .False.
               end if
            end do
         end do

         do i = 1 , 3
            if ( .not. isOrigin(i) ) then
               t = Maux(:,i) ! t is a centring vector
               newt = .true.
               do j = 1 , nCentringVectors
                  if (Rational_Co_Linear(t,latc(:,j))) newt = .false.
               end do
               if (newt) then
                  nCentringVectors = nCentringVectors + 1
                  latc(:,nCentringVectors) = t
               end if
            end if
         end do

         if (nCentringVectors > 0) then
            lattyp=Get_Lattice_Type(nCentringVectors,latc)

         else
            lattyp = "P"
         end if
         if (Err_CFML%Ierr /= 0) return
         if (pout) write(*,'(12x,2a)') "Lattice type: ",lattyp

      else
         if (pout) write(*,'(8x,a)') " => Standard lattice is primitive "
         lattyp = "P"
      end if
   End Function Get_Lattice_Type_from_MAT

   !!----
   !!---- GET_MAGNETIC_LATTICE_TYPE
   !!----
   !!---- Returns the magnetic type symbol from lattice and antilattice translations.
   !!----
   !!---- 22/04/2019
   !!
   Module Subroutine Get_Magnetic_Lattice(G)
      !---- Arguments ----!
      type(spg_type), intent(in out) :: G

      !---- Local variables ----!
      integer :: i,j
      logical :: latt_p, latt__a, latt__b, latt__c, &
                         latt_a,  latt_b,  latt_c, latt_i, latt_r
      integer,          dimension(9)   :: latt_given
      type(rational),   dimension(3,9) :: lattice

      !> Init
      Call Clear_Error()

      G%shu_lat(:)=" "
      if (G%num_lat > 0) then
         G%shu_lat(1)=Get_Lattice_Type(G%num_Lat,G%Lat_tr)

      else
          G%shu_lat(1) = "P"
      end if
      if (Err_CFML%Ierr /= 0)    return
      if (G%num_aLat == 0) return

      lattice(:,1)  = [ 1//2,0//1,0//1 ]
      lattice(:,2)  = [ 0//1,1//2,0//1 ]
      lattice(:,3)  = [ 0//1,0//1,1//2 ]
      lattice(:,4)  = [ 0//1,1//2,1//2 ]
      lattice(:,5)  = [ 1//2,0//1,1//2 ]
      lattice(:,6)  = [ 1//2,1//2,0//1 ]
      lattice(:,7)  = [ 1//2,1//2,1//2 ]
      lattice(:,8)  = [ 2//3,1//3,5//6 ]
      lattice(:,9)  = [ 1//3,2//3,1//6 ]

      latt_p  = .false.
      latt__a = .false.
      latt__b = .false.
      latt__c = .false.
      latt_a  = .false.
      latt_b  = .false.
      latt_c  = .false.
      latt_i  = .false.
      latt_r  = .false.

      do i = 1 , G%num_aLat
         latt_given(:) = 0
         do j = 1 , 9
            if (Rational_Equal(G%aLat_tr(1:3,i),lattice(1:3,j))) then
               latt_given(j) = 1
               select case (j)
                  case (1)
                     latt__a = .true.
                  case (2)
                     latt__b = .true.
                  case (3)
                     latt__c = .true.
                  case (4)
                     latt_a  = .true.
                  case (5)
                     latt_b  = .true.
                  case (6)
                     latt_c  = .true.
                  case (7)
                     latt_i  = .true.
                  case (8,9)
                     latt_r  = .true.
               end select
               exit
            end if
         end do
         if (sum(latt_given) == 0) then
            G%shu_lat(2) = "Z"
            exit
         end if
      end do

      if (G%shu_lat(2) == " ") then
         G%shu_lat(2) = "Z"
         select case (G%shu_lat(1))
            case ("P")
               if (sum(latt_given) > 1) then
                  Err_CFML%Ierr = 1
                  Err_CFML%Msg = "Get_Magnetic_Lattice_Type@SPACEG: Primitive lattice with more than one anti-translation."
                  return
               end if

               if (latt__a) then
                  G%shu_lat(2) = "a"
                  return

               else if (latt__b) then
                  G%shu_lat(2) = "b"
                  return

               else if (latt__c) then
                  G%shu_lat(2) = "c"
                  return

               else if (latt_a) then
                  G%shu_lat(2) = "A"
                  return

               else if (latt_b) then
                  G%shu_lat(2) = "B"

               else if (latt_c) then
                  G%shu_lat(2) = "C"
                  return

               else if (latt_i) then
                  G%shu_lat(2) = "I"
               end if

            case ("A")
               if (latt__a .or. latt_i) then
                  G%shu_lat(2) = "a"

               else if (latt__b .or. latt__c) then
                  G%shu_lat(2) = "b"

               else if (latt_b .or. latt_c) then
                  G%shu_lat(2) = "B"
               end if

            case ("C")
               if (latt__a .or. latt__b) then
                  G%shu_lat(2) = "a"

               else if (latt__c .or. latt_i) then
                  G%shu_lat(2) = "c"

               else if (latt_a .or. latt_b) then
                  G%shu_lat(2) = "A"
               end if

            case ("I")
               if (latt__a .or. latt_a) then
                  G%shu_lat(2) = "a"

               else if (latt__b .or. latt_b) then
                  G%shu_lat(2) = "b"

               else if (latt__c .or. latt_c) then
                  G%shu_lat(2) = "c"
               end if

            case ("F")
               if (latt__a .or. latt__b .or. latt__c .or. latt_i) G%shu_lat(2) = "S"

            case ("R")
               if (latt__c .or. latt_r) G%shu_lat(2) = "I"
         end select
      end if

      if (G%shu_lat(2) == "Z") then
         Err_CFML%Ierr = 1
         Err_CFML%Msg = "Get_Magnetic_Lattice_Type@SPACEG: Unable to identify lattice type."
      end if
   End Subroutine Get_Magnetic_Lattice

   !!----
   !!---- GET_LATTICE_TYPE_FROM_GENER
   !!----    Return the Lattice type symbol from  a list of generators
   !!----
   !!---- 26/05/2019
   !!
   Module Function Get_Lattice_Type_from_Gener(Ngen,Gen) Result(Latt)
      !---- Arguments ----!
      integer,                        intent(in) :: Ngen
      character(len=*), dimension(:), intent(in) :: Gen
      character(len=:), allocatable              :: Latt

      !---- Local Variables ----!
      integer                                     :: n, nt
      type(Symm_Oper_Type)                        :: Op
      type(rational), dimension(3,3)              :: Identidad
      type(rational), dimension(:,:), allocatable :: Tr


      !> Init
      Latt=" "
      if (ngen <=0 ) return

      call Rational_Identity_Matrix(identidad)

      nt=0
      do n=1,ngen
         Op=Get_Op_from_Symb(gen(n))
         if (Op%Time_Inv /= 1) cycle
         if (.not. Rational_Equal(Op%Mat(1:3,1:3),identidad)) cycle
         if (.not. Rational_Is_NullVector(Op%Mat(1:3,4))) nt=nt+1
      end do

      if (nt ==0) then
         Latt="P"
         return
      end if

      if (allocated(Tr)) deallocate(Tr)
      allocate(Tr(3,nt))
      nt=0
      do n=1,ngen
         Op=Get_Op_from_Symb(gen(n))
         if (Op%Time_Inv /= 1) cycle
         if (.not. Rational_Equal(Op%Mat(1:3,1:3),identidad)) cycle
         if (rational_Is_NullVector(Op%Mat(1:3,4))) cycle
         nt=nt+1
         Tr(:,nt)=Op%Mat(1:3,4)
      end do
      Latt=Get_Lattice_Type_L(Nt, Tr)

   End Function Get_Lattice_Type_from_Gener

End SubModule gS_Get_LattType