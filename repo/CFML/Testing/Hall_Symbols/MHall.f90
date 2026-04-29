!!----
!!---- Program: Test_Magnetic_Hall_Symbols
!!----
!!---- JGP/NAK/JRC August2020
!!
Program Test_Magnetic_Hall_Symbols
   !---- Use Modules ----!
   Use CFML_Globaldeps,   only: cp, err_CFML, clear_error
   Use CFML_gSpaceGroups, only: SpG_Type, Get_Generators, Get_Hall_from_Generators, Change_Setting_Generators,&
                                Init_SpaceGroup, Group_Constructor, Identify_Group, Write_SpaceGroup_Info, &
                                Get_MagPG_from_BNS, ISO_to_Jones_Notation
   Use CFML_Strings,      only: pack_string

   !---- Variables ----!
   implicit none

   character(len=180)                           :: input_Hall,input_string
   character(len=80)                            :: str_Hall
   character(len=60), dimension(:), allocatable :: gen
   character(len=:), allocatable                :: generators, setting
   integer                                      :: i,j,k,ngen,d, narg
   type(SpG_Type)                               :: SpG
   real(kind=cp)                                :: start, fin
   logical                                      :: gen_given, time_reversal

    narg=COMMAND_ARGUMENT_COUNT()
    gen_given=.false.
    if (narg /= 0) then
       call GET_COMMAND_ARGUMENT(1, input_Hall)
    end if

   !> Main
   do
      call clear_error()
      write(*,"(a)")'  -------------------------------'
      write(*,"(a)")'  - MHall: Testing Hall symbols -'
      write(*,"(a)")'  -------------------------------'
      if (narg == 0) then
        write(*,"(a)",advance="no") " => Enter the magnetic Hall symbol or a list of generators in Jones'faithful notation: "
        read(*,"(a)") input_Hall
        if(len_trim(input_Hall) == 0) exit
      end if
      call cpu_time(start)
      i=index(input_Hall,"x")
      j=index(input_Hall,"y")
      k=index(input_Hall,"z")


      if(i /= 0 .and. j /= 0 .and. k /= 0) then !List of generators, generate the Hall symbol
        gen_given=.true.
        !Check the aspect of generators, does it contains (x,-y,z)' forms?, and convert them to Jones Faithful notation
        if(index(input_Hall,")") /= 0 .and. index(input_Hall,"(") /= 0) call ISO_to_jones_notation(input_Hall)
        call Get_Generators(input_hall, d, gen, ngen,time_reversal)
        generators=" "
        do i=1,ngen
          generators=trim(generators)//trim(gen(i))//";"
        end do
        generators=generators(1:len_trim(generators)-1)
        generators=pack_string(generators)
        input_string=generators
        write(*,"(a,a)") " => Input generators: ",trim(input_string)
        input_hall = Get_Hall_from_Generators(Ngen, Gen)
        write(*,"(a,a)") " => Deduced Hall symbol from generators: ",trim(input_hall)
      end if

      !> Magnetic Hall symbol
      i=index(input_Hall,":")
      if(i /= 0) then
        str_Hall=input_Hall(1:i-1)
        setting=adjustl(input_Hall(i+1:))
      else
        str_Hall=input_Hall
        setting=" "
      end if
      ngen=0
      call Get_Generators(str_Hall, Gen, ngen)
      if (Err_CFML%Ierr /= 0) then
         write(*,"(a)")'    --->'//trim(Err_CFML%Msg)
         if(narg /= 0 ) then
           exit
         else
           cycle
         end if
      end if

      generators=" "
      do i=1,ngen
        generators=trim(generators)//trim(gen(i))//";"
      end do
      generators=generators(1:len_trim(generators)-1)
      generators=pack_string(generators)
      if(gen_given .and. trim(input_string) /= trim(generators)) then
         write(*,"(a/a)") " => The obtained generators: "//trim(generators), &
                          "    do not correspond to the input generators: "//trim(input_string)
         write(*,"(a)") " => An error has occurred in interpreting the symmetry element of a generator"
         cycle
      else
         write(*,"(a)") " => Obtained generators: "//trim(generators)
      end if

      if(len_trim(setting) /= 0) then
        write(*,"(a)") " => Followed by a change of basis: "//trim(setting)
        call Change_Setting_Generators(setting,ngen,gen)
        if (Err_CFML%Ierr /= 0) then
           write(*,"(a)")'    --->'//trim(Err_CFML%Msg)
           if(narg /= 0 ) then
             exit
           else
             cycle
           end if
        end if
        generators=" "
        do i=1,ngen
          generators=trim(generators)//trim(gen(i))//";"
        end do
        generators=generators(1:len_trim(generators)-1)
        write(*,"(a)") " => Newly obtained generators: "//trim(generators)
      end if


      !!> Constructor
      call Init_SpaceGroup(SpG)
      call Group_Constructor(gen,SpG)
      if (Err_CFML%Ierr /= 0) then
         write(*,"(a)")'    --->'//trim(Err_CFML%Msg)
         if(narg /= 0 ) then
           exit
         else
           cycle
         end if
      end if
      !
      !!> Identify group
      call Identify_Group(Spg)
      if(Err_CFML%Ierr == 1) then
         write(unit=*,fmt="(a)") "  WARNING: "//Err_CFML%Msg
         call clear_error()
      end if
      if(len_trim(Spg%mag_pg) == 0) Spg%mag_pg= Get_MagPG_from_BNS(Spg%bns_symb,Spg%mag_type)
      if(len_trim(Spg%Hall)   == 0) then
        if(len_trim(setting) /= 0) then
          Spg%Hall= input_Hall
        else
          Spg%Hall= str_Hall
        end if
      end if
      call Write_SpaceGroup_Info(SpG)
      call cpu_time(fin)
      write(*,"(/,a,f12.3,a)") " => Total CPU_TIME for this calculation: ",fin-start," seconds"
      if (narg /= 0) exit
   end do

End Program Test_Magnetic_Hall_Symbols
