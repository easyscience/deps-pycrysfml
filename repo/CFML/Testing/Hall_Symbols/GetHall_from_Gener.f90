!!----
!!---- Program: Recover Magnetic_Hall symbols
!!----
!!---- JGP May2019
!!
Program Magnetic_Hall
   !---- Use Modules ----!
   Use CFML_Symmetry_Tables
   Use CFML_gSpaceGroups
   Use CFML_Strings, only: pack_string

   !---- Variables ----!
   implicit none

   character(len=50)                            :: str_BNS, str_Hall, Str_tmp
   character(len=:), allocatable                :: generators
   character(len=60), dimension(:), allocatable :: gen
   integer                                      :: i, n, ngen
   integer, dimension(3)                        :: shift

   !> Magnetic Symmetry Symbols (Proposed)
   call set_Shubnikov_info()

   call execute_command_line("cls")
   print*,'-----------------------------------------------------'
   print*,' Testing Magnetic Symmetry Groups using Hall symbols'
   print*,'-----------------------------------------------------'

   !> Main
   do n=1,1651   !19,66,103
      select case (n)
         case (1)
            print*,'---- Triclinic Space groups ----'
         case (8)
            print*,' '
            print*,'---- Monoclinic Space groups ----'
         case (99)
            print*,' '
            print*,'---- Orthorrhombic Space groups ----'
         case (661)
            print*,' '
            print*,'---- Tetragonal Space groups ----'
         case (1231)
            print*,' '
            print*,'---- Trigonal Space groups ----'
         case (1339)
            print*,' '
            print*,'---- Hexagonal Space groups ----'
         case (1503)
            print*,' '
            print*,'---- Cubic Space groups ----'
      end select


      !> Info from Database
      str_BNS =adjustl(shubnikov_info(n)%BNS)
      str_Hall=adjustl(shubnikov_info(n)%MHall)

      !> Magnetic Hall symbol
      ngen=0
      gen=" "
      call Get_Generators(str_Hall, gen, ngen)

      !> Test routine
      select case (n)
         case (1259:1261)
            shift=[0,0,1]
            Str_tmp=Get_Hall_from_Generators(Ngen, Gen, shift)
         case (1262)
            shift=[0,0,-1]
            Str_tmp=Get_Hall_from_Generators(Ngen, Gen, shift)
         case (1267:1269)
            shift=[0,0,-1]
            Str_tmp=Get_Hall_from_Generators(Ngen, Gen, shift)
         case (1270)
            shift=[0,0,1]
            Str_tmp=Get_Hall_from_Generators(Ngen, Gen, shift)
         case (1386:1390)
            shift=[0,0,-1]
            Str_tmp=Get_Hall_from_Generators(Ngen, Gen, shift)
         case (1391:1400)
            shift=[0,0,1]
            Str_tmp=Get_Hall_from_Generators(Ngen, Gen, shift)
         case (1401:1407)
            shift=[0,0,-1]
            Str_tmp=Get_Hall_from_Generators(Ngen, Gen, shift)
         case (1408:1409)
            shift=[0,0,1]
            Str_tmp=Get_Hall_from_Generators(Ngen, Gen, shift)
         case default
            Str_tmp=Get_Hall_from_Generators(Ngen, Gen)
      end select
      generators=" "
      do i=1,ngen
        generators=trim(generators)//trim(gen(i))//";"
      end do
      generators=generators(1:len_trim(generators)-1)
      if (trim(pack_string(Str_tmp)) /= trim(pack_string(str_Hall))) then
         write(unit=*,fmt='(i6,t18,a,t35,a,t60,a,t85,a)') n, trim(str_BNS), trim(str_Hall), trim(Str_tmp),  'ERROR'
      else
         write(unit=*,fmt='(i6,t18,a,t35,a,t60,a,t85,a)') n, trim(str_BNS), trim(str_Hall), trim(Str_tmp),  "OK  -> "//trim(generators)
      end if

   end do

End Program Magnetic_Hall
