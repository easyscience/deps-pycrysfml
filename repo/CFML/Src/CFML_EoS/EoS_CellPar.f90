!!----
!!----
!!----
SubModule (CFML_EoS) EoS_CellPar
   implicit none
   Contains

   !!----
   !!---- GET_PARAMS_CELL
   !!----    returns full cell parameters in crystal_cell_type for input P and T
   !!----
   !!---- Date: 04/02/2021
   !!---- Revision: OK
   !!
   Module Function Get_Params_Cell(P, T, Cell_Eos, Cartype) Result(Cell)
      !---- Arguments ----!
      real(kind=cp),              intent(in) :: p,t
      type(eos_cell_type),        intent(in) :: cell_eos
      character(len=2), optional, intent(in) :: cartype    ! orientation
      type(cell_G_type)                      :: cell       !cell params, metric tensor at this P,T

      !---- Local Variables ----!
      integer                     :: i,j,k
      real(kind=cp)               :: v,arg
      real(kind=cp),dimension(3)  :: abc,ang
      character(len=2)            :: ctype


      !> init
      abc=10._cp
      ctype='  '
      if (present(cartype))ctype=U_case(cartype)

      !> check if all needed eos are loaded
      call Eos_Cell_Loaded_Check(cell_eos)
      if (err_CFML%Flag) return

      !> Get cell edges
      call clear_error()
      do i=1,3
         abc(i)=get_Volume_axis(P,T,cell_eos,i)
         if (err_CFML%Flag) return
      end do

      !> get the angles
      ang=90._cp
      select case(U_case(cell_eos%system(1:4)))
         case('TRIG','HEXA')
            ang(3)=120._cp

         case('MONO')
            if (cell_eos%eosang%iangle == 0)then
               v=get_Volume_axis(P,T,cell_eos,0)
               i=cell_eos%unique
               arg=V/product(abc)
               if (arg > 0.999999_cp)then
                  ang(i)=90._cp
               else
                  ang(i)=asind(arg)
                  if (cell_eos%obtuse(i))ang(i)=180.0_cp-ang(i)
               end if

            else
               ang(cell_eos%unique)=get_angle_poly(p,t,cell_eos%eosang,cell_eos%unique)
            end if

         case('TRIC')
            if (cell_eos%eosang%iangle == 0)then
               !all eos for V,abc, and d's present
               v=get_volume(p,t,cell_eos%eos(0))
               do i=1,3
                  j=mod(i,3)+1
                  k=mod(j,3)+1
                  arg=V/abc(j)/abc(k)/get_volume(p,t,cell_eos%eos(i+3))
                  if (arg > 0.999999_cp)then
                     ang(i)=90._cp
                  else
                     ang(i)=asind(arg)
                     if (cell_eos%obtuse(i))ang(i)=180.0_cp-ang(i)
                  end if
               end do
            else
               do i=1,3
                  ang(i)=get_angle_poly(p,t,cell_eos%eosang,i)
               end do
            end if
      end select

      !> Set the metric tensor
      call Set_Crystal_Cell (abc, Ang, Cell, cartype=ctype)

   End Function Get_Params_Cell

   !!----
   !!---- ISOTROPIC_CELL
   !!----
   !!---- returns .true. if crystal system is isotropic or cubic
   !!----
   !!---- Date: 23/02/2021
   !!---- Revision: OK
   !!
   Module Function Isotropic_Cell(cell_eos) result(isotropic)
      !---- Arguments ----!
      type(eos_cell_type),intent(in)      :: cell_eos
      logical                             :: isotropic

      !---- Local Variables ----!
      character(len=len(cell_eos%system)) :: sys

      isotropic=.true.

      sys=U_case(cell_eos%system)
      if (index(sys,'ISOT') > 0) return
      if (index(sys,'CUB')  > 0) return

      isotropic=.false.

   End Function Isotropic_Cell

   !!----
   !!---- EOS_CELL_LOADED_CHECK
   !!----
   !!---- Subroutine to check whether required eos are available for cell calculations
   !!---- If not, issues a warning
   !!----
   !!---- Date: 09/09/2020
   !!---- Revision: OK
   !!
   Module Subroutine Eos_Cell_Loaded_Check(Cell_Eos)
      !---- Arguments ----!
      type(eos_cell_type), intent(in) :: cell_eos

      !---- Variables ----!
      integer :: i, isum

      select case(U_case(cell_eos%system(1:4)))
         case('CUBI','ISOT')
            if (cell_eos%loaded(0) == 0)then
               call set_error(-1,'No eos loaded for cubic system, so no calculations possible')
               return
            end if

         case('ORTH','TRIG','HEXA','TETR')
            !>check for all eos present in some form
            do i = 0,3
               if (cell_eos%loaded(i) > 0)cycle
               if (index(U_case(cell_eos%system(1:4)),'ORTH') == 1)then
                  call set_error(-1,'Orthorhombic crystal system, but not enough EoS loaded to do calculations')
               else
                  call set_error(-1,'Uniaxial crystal system, but not enough EoS loaded to do calculations')
               end if
               return
            end do

         case('MONO')
            !> check for all eos present in some form
            isum=0
            do i = 0,3
               if (cell_eos%loaded(i) > 0)isum=isum+1
            end do

            if (isum < 4)then        !This takes care of angle poly because if present and 3 eos, the 4th is set as calculated
               call set_error(-1,'Monoclinic crystal system, but not enough EoS loaded to do calculations')
               return
            end if

            !> check for unique flag
            if (cell_eos%unique == 0)then
               call set_error(-1, 'Monoclinic crystal system, but unique axis not defined')
               return
            end if

            if (cell_eos%eosang%iangle > 0)then
               isum=0
               do i = 0,3
                  if (cell_eos%loaded(i) == 1)isum=isum+1
               end do
               if (isum == 4)then
                  call set_error(-1,'More than required EoS are loaded with angles: either delete angle polynomial or 1 EoS')
                  return
               end if
            end if

         case('TRIC')
            if (cell_eos%eosang%iangle == 0)then
               !> require all
               if (sum(cell_eos%loaded(0:6)) < 7)then
                  call set_error(-1,'Triclinic crystal system, but not enough EoS loaded to do calculations')
                  return
               end if

            else
               isum=0
               do i = 0,3
                  if (cell_eos%loaded(i) == 1)isum=isum+1
               end do
               if (isum == 4)then
                  call set_error(-1,'More than required EoS are loaded with angles: either delete angle polynomial or 1 EoS')
                  return

               else if(isum < 3)then
                  call set_error(-1, 'Triclinic crystal system, but not enough EoS loaded to do calculations')
                  return
               end if
            end if

         case default
            call set_error(-1,'Unrecognised Crystal System')
            return

      end select

      call clear_error()

   End Subroutine Eos_Cell_Loaded_Check

   !!----
   !!---- SET_CELL_TYPES
   !!----
   !!---- Subroutine to set info, eosc and flags inside eos_cell_type
   !!----
   !!---- Date: 25/09/2020
   !!---- Revision: OK
   !!
   Module Subroutine Set_Cell_Types(Cell_Eos)
      !---- Arguments ----!
      type(eos_cell_type), intent(in out) :: cell_eos

      !---- Local Variables ----!
      integer :: i

      !> Init
      cell_eos%cout='N'
      cell_eos%loaded=0        ! clear and reset

      select case(U_case(cell_eos%system(1:4)))
         case('TRIC')
            if (cell_eos%eosang%iangle >  0)then
               cell_eos%n=3
               cell_eos%inputlist='(a,b,c,V,Ang)'
            else
               cell_eos%n=6
               cell_eos%inputlist='(a,b,c,d100,d010,d001,V)'
            end if

         case('MONO')
            cell_eos%n=3
            cell_eos%inputlist='(a,b,c,V)'
            if (cell_eos%eosang%iangle >  0)cell_eos%inputlist='(a,b,c,V,Ang)'

         case('ISOT')
            cell_eos%n= 0
            cell_eos%inputlist='V'

         case default
            cell_eos%n=3
            cell_eos%inputlist='(a,b,c,V)'
      end select

      !> set all eos%system to cell_eos%system
      if (len_trim(cell_eos%system) > 0)then
         cell_eos%eos(0:6)%system=cell_eos%system
         cell_eos%eosang%system=cell_eos%system
      end if

      !> initial testing of eos to see if present
      do i=0,cell_eos%n
         if (cell_eos%eos(i)%imodel /= 0 .or.  cell_eos%eos(i)%itherm /= 0)cell_eos%loaded(i)=1
      end do

      !> now check to see if we can calculate missing
      call clear_error()

      select case(U_case(cell_eos%system(1:4)))
         case('TRIC')
            !requires all 7 eos or only 3 of 4 if angle poly
            if (cell_eos%eosang%iangle > 0)then        !angle poly
               !angle poly should be loaded: only need 3 of 4 eos Vabc
               if (sum(cell_eos%loaded(0:3)) == 3)then
                  do i=0,3
                     if (cell_eos%loaded(i) == 0)then
                        cell_eos%loaded(i)=3
                        exit
                     end if
                  end do
               end if
            end if

         case('MONO')
            !Set the unique axis flags
            if (index('ABC',U_case(cell_eos%unique_label)) > 0)then
               cell_eos%unique=index('ABC',U_case(cell_eos%unique_label))
            else
               i=index(cell_eos%system,'-')-1
               if (i > 0)cell_eos%unique=index('ABC',U_case(cell_eos%system(i:i)))
            end if

            ! no symmetry equivs, so loaded is either 1 or 0, until here. V,a,b,c are all required unless angle poly set
            if (cell_eos%eosang%iangle > 0)then        !angle poly
               !angle poly should be loaded: only need 3 of 4 eos Vabc
               if (sum(cell_eos%loaded(0:3)) == 3)then
                  do i=0,3
                     if (cell_eos%loaded(i) == 0)then
                        cell_eos%loaded(i)=3
                        exit
                     end if
                  end do
               end if
            end if

         case('ORTH')
            ! no symmetry equivs, so loaded is either 1 or 0, until here
            if (sum(cell_eos%loaded(0:3)) == 3)then
               do i=0,3
                  if (cell_eos%loaded(i) == 0)then
                     cell_eos%loaded(i)=3
                     exit
                  end if
               end do
            end if

         case('TETR','TRIG','HEXA')
            if (cell_eos%loaded(1) == 0 .and. cell_eos%loaded(2) == 1)then     ! move b to a
               cell_eos%eos(1)=cell_eos%eos(2)
               cell_eos%loaded(1)=1
            end if

            if (cell_eos%loaded(1) == 1)then                          ! make b = a
               cell_eos%loaded(2)=2
               cell_eos%eos(2)=cell_eos%eos(1)
            end if

            if (cell_eos%loaded(0) == 0 .and. cell_eos%loaded(1) == 1 .and. cell_eos%loaded(3) == 1) cell_eos%loaded(0)=3   !V from a and c
            if (cell_eos%loaded(1) == 0 .and. cell_eos%loaded(0) == 1 .and. cell_eos%loaded(3) == 1) cell_eos%loaded(1)=3   ! a from V and c
            if (cell_eos%loaded(3) == 0 .and. cell_eos%loaded(0) == 1 .and. cell_eos%loaded(1) == 1) cell_eos%loaded(3)=3   ! C from a and V

         case('CUBI','ISOT') !
            !> first move b or c loaded to a
            if (cell_eos%loaded(2) == 1)then
               cell_eos%eos(1)=cell_eos%eos(2)
               cell_eos%loaded(1)=1

            else if(cell_eos%loaded(3) == 1)then
               cell_eos%eos(1)=cell_eos%eos(3)
               cell_eos%loaded(1)=1
            end if

            !> now set dependencies
            if (cell_eos%loaded(0) == 1)then              ! V loaded
               cell_eos%loaded(1:3)=3                     ! calc a,b,c from V

            else if(cell_eos%loaded(1) == 1)then          ! a loaded
               cell_eos%loaded(0)=3
               cell_eos%loaded(2:3)=2
               cell_eos%eos(2)=cell_eos%eos(1)
               cell_eos%eos(3)=cell_eos%eos(1)
            end if

      end select

      !> Update flags and eosc
      call init_eos_type(cell_eos%eosc)      ! clears eosc
      do i=0,cell_eos%n
         if (cell_eos%loaded(i) == 1)then
            cell_eos%eosc=cell_eos%eos(i)
            exit
         end if
      end do
      cell_eos%eosc%imodel=1                 ! dummies for i/o control
      cell_eos%eosc%itherm=1
      do i=0,cell_eos%n
         if (cell_eos%loaded(i) == 1)then
            if (cell_eos%eos(i)%imodel /= 0)then
               cell_eos%cout(i,1)='Y'

            else
               cell_eos%eosc%imodel=0
            end if

            if (cell_eos%eos(i)%itherm /= 0)then
               cell_eos%cout(i,2)='Y'

            else
               cell_eos%eosc%itherm=0
            end if

            if (cell_eos%cout(i,1) == 'Y' .and. cell_eos%cout(i,2) == 'Y')cell_eos%cout(i,3)='Y'
         end if
      end do

   End Subroutine Set_Cell_Types

End SubModule EoS_CellPar