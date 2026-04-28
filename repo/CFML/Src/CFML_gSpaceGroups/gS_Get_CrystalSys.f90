!!----
!!----
!!----
!!----
SubModule (CFML_gSpaceGroups) gS_Get_CrystalSys
   implicit none
   Contains
   !!----
   !!---- GET_CRYSTAL_SYSTEM_STR
   !!----
   !!---- 13/05/2019
   !!
   Module Function Get_Crystal_System_Str(Ops, nops) Result(Str)
      !---- Arguments ----!
      type(Symm_Oper_Type), dimension(:), intent(in) :: Ops    ! Reduced operators (Numops)
      integer,                            intent(in) :: NOps   ! Numops
      character(len=:), allocatable                  :: Str

      !---- Local variables ----!
      integer :: nrot_1, nrot_1b
      integer :: nrot_2, nrot_2b
      integer :: nrot_3, nrot_3b
      integer :: nrot_4, nrot_4b
      integer :: nrot_6, nrot_6b
      integer :: i,n,ndet

      !> Init
      N=0
      Str=" "
      if (Nops ==0) then
         err_CFML%Ierr=1
         err_CFML%Msg="Get_Crystal_System_Num@GSPACEGROUPS: The number of symmetry operators is zero!"
         return
      end if

      nrot_1  = 0
      nrot_2  = 0
      nrot_3  = 0
      nrot_4  = 0
      nrot_6  = 0
      nrot_1b = 0
      nrot_2b = 0
      nrot_3b = 0
      nrot_4b = 0
      nrot_6b = 0

      do i=1, Nops
         ndet= Get_Rotation_Order(Ops(i)%Mat(1:3,1:3))
         select case (ndet)
            case (-6)
               nrot_6b=nrot_6b +1
            case (-4)
               nrot_4b=nrot_4b +1
            case (-3)
               nrot_3b=nrot_3b +1
            case (-2)
               nrot_2b=nrot_2b +1
            case (-1)
               nrot_1b=nrot_1b +1
            case ( 1)
               nrot_1 =nrot_1  +1
            case ( 2)
               nrot_2 =nrot_2  +1
            case ( 3)
               nrot_3 =nrot_3  +1
            case ( 4)
               nrot_4 =nrot_4  +1
            case ( 6)
               nrot_6 =nrot_6  +1
            case default
               err_CFML%Ierr=1
               err_CFML%Msg="Get_Crystal_System_Num@GSPACEGROUPS: Problems in Rotation order calculation!"
               return
         end select
      end do

      !> Cubic
      if ( (nrot_3 + nrot_3b == 8) ) then
         n=7
         str=SYS_CRY(n)
         return
      end if

      !> Hexagonal
      if ( (nrot_6 + nrot_6b == 2) ) then
         n=6
         str=SYS_CRY(n)
         return
      end if

      !> Trigonal
      if ( (nrot_3 + nrot_3b == 2) ) then
         n=5
         str=SYS_CRY(n)
         return
      end if

      !> Tetragonal
      if ( (nrot_4 + nrot_4b == 2) ) then
         n=4
         str=SYS_CRY(n)
         return
      end if

      !> Orthorhombic
      if ( (nrot_2 + nrot_2b == 3) ) then
         n=3
         str=SYS_CRY(n)
         return
      end if

      !> Monoclinic
      if ( (nrot_2 + nrot_2b == 1)  ) then
         n=2
         str=SYS_CRY(n)
         return
      end if

      !> Triclinic
       n=1
       str=SYS_CRY(n)

   End Function Get_Crystal_System_Str

   !!----
   !!---- GET_CRYSTAL_SYSTEM_FROM_LAUE
   !!----
   !!----
   !!
   Module Function Get_Crystal_System_from_Laue(Laue) Result(Str)
      !---- Arguments ----!
      character(len=*), intent(in)  :: Laue
      character(len=:), allocatable :: Str

      !---- Local Variables ----!
      character(len=:), allocatable :: car
      integer                       :: n

      car=pack_string(l_case(Laue))
      select case (trim(car))
         case ("m-3m","m-3","m3","m3m")
            n=7
         case ("6/mmm","6/m")
            n=6
         case ("-3m","-3","-3r","-3mr","-3m1","-31m")
            n=5
         case ("4/mmm","4/m")
            n=4
         case ("mmm")
            n=3
         case ("2/m")
            n=2
         case default
            n=1
      end select
      str=SYS_CRY(n)
   End Function Get_Crystal_System_from_Laue

End SubModule gS_Get_CrystalSys