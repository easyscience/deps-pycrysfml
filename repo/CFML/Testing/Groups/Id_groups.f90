 Program Test_Identify_Groups
    !---- Use Modules ----!
    use CFML_Globaldeps, only: cp, err_cfml,Set_CFML_DEBUG
    use CFML_Symmetry_Tables
    use CFML_gSpaceGroups
    implicit none

    character(len=256)                  :: generatorList
    class(Spg_Type), allocatable        :: Grp
    real(kind=cp) :: start, fin

    !> Init

    call set_CFML_DEBUG(.true.)
    do
       write(*,'(a)') "x,-y,z;x,y,z+1/2;-x,-y,-z;x+1/2,y,z    <-- The generators of a Crystallographic 3D group"
       write(*,'(a)') "x,-y,z,t,1;x,y,z,t+1/2,-1              <-- The generators of a Magnetic SuperSpace group"
       write(*,'(a)') "x1,-x2,x3,x4,1;x1,x2,x3,x4+1/2,-1      <-- The generators of a Magnetic SuperSpace group"
       write(*,'(/,a)',advance='no') " => Introduce generators the group as indicated above: "
       read(*,'(a)') generatorList
       if (len_trim(generatorList) == 0) exit

       call CPU_TIME(start)
       call Set_SpaceGroup(generatorList,Grp)
       if (Err_CFML%Ierr /= 0) then
          write(*,'(/,4x,a)') trim(Err_CFML%Msg)
          cycle
       else
          call Write_SpaceGroup_Info(Grp)
       end if

       call CPU_TIME(fin)
       write(*,"(a,f12.3,a)") "CPU_TIME for this calculation: ",fin-start," seconds"
    end do

End Program Test_Identify_Groups