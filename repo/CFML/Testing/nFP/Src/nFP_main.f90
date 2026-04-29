
Program nFP_main
   Use CFML_Strings,      only: U_Case
   Use CFML_Utilities
   Use CFML_Powder
   Use CFML_DiffPatt
   use nFP_Globals
   use nFP_read_files
   use nFP_Reflections
   use nFP_Simulation
   use nFP_Optimization

     !---- Variables ----!
     implicit none

     integer                :: i,nfree
     integer                :: lun
     character(len=1024)    :: fname
     character(len=132)     :: filcod
     logical                :: existe=.false.
     character(len=:),allocatable   :: cfl_file, buffer_cifs
     integer                        :: narg
     Logical                        :: arggiven=.false.
     real(kind=cp)                  :: T_ini,T_fin, T_Time, GoF

     !---- Arguments on the command line ----!
     narg=command_argument_count()

     if (narg > 0) then
        call get_command_argument(1,fname)
        arggiven=.true.
     end if


     write(unit=*,fmt="(/,/,6(a,/))")                                                     &
          "                     ------ PROGRAM GDS_FULLPROF ------"                     , &
          "                       ---- Version 0.0 May-2025----"                        , &
          "    **********************************************************************"  , &
          "    * General Diffraction System FullProf (Simulation and Optimization)  *"  , &
          "    **********************************************************************"  , &
          "                          (JRC/NAK/JGP- May-2025 )"
     write(unit=*,fmt=*) " "

     if (.not. arggiven) then
        write(unit=*,fmt="(a)", advance='no') " => Name of the input CFL/CIF file: "
        read(unit=*,fmt="(a)") fname
        if(len_trim(fname) == 0) stop
     end if

    i=index(fname,".",back=.true.)
    filcod=fname(1:i-1)
     open(newunit=lun,file=trim(filcod)//".log", status="replace",action="write")

    !> Initialize the clock
     call cpu_time(T_Ini)
     T_Time=0.0

     i=index(fname,".",back=.true.)
     call Set_CFML_DEBUG(.true.)
     if(i == 0) then
        write(unit=*,fmt="(a)") " => The extension of the file should be included in the name! "
        write(unit=*,fmt="(a)") " => The GDS_FullProf program is aborted! "
        stop
     else
     	  ! If a CIF file is provided, only simulation is allowed,
     	  ! unless the name of the CIF file is inside the CFL file
     	  if(U_Case(fname(i+1:i+3)) == "CFL") then
     	  	Simulation=.false.  !Unless a simulation is required inside the CFL file
     	  	cfl_file=trim(fname)
          inquire(file=cfl_file,exist=existe)
          if (.not. existe) then
             write(unit=*,fmt="(a)") " File: "//trim(cfl_file)//" doesn't exist!"
             stop
          end if
          !***********************************************************
          !Reading the input file to set up the global variables
          call nFP_read_CFL(cfl_file,lun)
          !***********************************************************
     	  else  !Assumed a CIF or mCIF file
     	  	Simulation=.true.
     	  	buffer_cifs=trim(fname)
          inquire(file=buffer_cifs,exist=existe)
          if (.not. existe) then
             write(unit=*,fmt="(a)") " File: "//trim(buffer_cifs)//" doesn't exist!"
             stop
          end if
          call nFP_read_CIFs(buffer_cifs)
     	  end if
     end if
     !Once the input file has been read, all the actions are known

     !Generate reflections for all conditions and patterns

    !***********************************************************
     Call nFP_gen_reflections(lun)
    !***********************************************************

     !if(Simulation) then
     	 !Just do the calculation and stop
    !***********************************************************
     	 Call nFP_Simulate(lun)
    !***********************************************************
    	 if(Err_CFML%Ierr /= 0) then
     	  write(*,"(a)") " ==> ERROR: "//Err_CFML%Msg
       end if
     !else
     	 !Perform the optimization prodedures
     !	 Call nFP_Optimize()
     !end if

     do i=1,N_patterns
       nfree=67
       call Calc_Rfactors(Pat(i)%Pdat,Excl(i),nfree,Chi_sqr(i),R_exp(i),R_wpatt(i),R_patt(i),Gof,.true.) !sigmas
       write(*,"(a,i3)")    "  Pattern #",i
       write(*,"(a,2F14.4)") "      Chi2=",Chi_sqr(i), Calc_Chi2(Pat(i)%Pdat,Excl(i),nfree)
       write(*,"(a,F14.4)") "    R_patt=",R_patt(i)
       write(*,"(a,F14.4)") "     R_exp=",R_exp(i)
       write(*,"(a,F14.4)") "   R_wpatt=",R_wpatt(i)
       write(*,"(a,F14.4)") "       GoF=",GoF
     end do

    !> ---- End Program ----
    call cpu_time(T_fin)
    T_fin=T_fin-T_ini
    T_time=T_time + T_fin

    T_ini=T_fin/60.0
    T_fin=int(T_ini)
    T_ini=(T_ini-T_fin)*60.0

    write(unit=*,fmt='(a,i3,a,f8.4,a)')       " => CPU-time: ",nint(T_fin)," minutes",T_ini," seconds"
    T_ini=T_time/60.0
    T_fin=int(T_ini)
    T_ini=(T_ini-T_fin)*60.0
    write(unit=*,fmt='(a,i3,a,f8.4,a)')       " => TOTAL CPU-time: ",nint(T_fin)," minutes",T_ini," seconds"

    if (CFML_DEBUG) then
       write(unit=lun,fmt='(/,a)')            " => Normal End of program: GDS_FULLPROF "
       write(unit=lun,fmt='(a,i3,a,f8.4,a)')  " => CPU-time: ",nint(T_fin)," minutes",T_ini," seconds"
       close(unit=lun)
    end if



End Program nFP_main