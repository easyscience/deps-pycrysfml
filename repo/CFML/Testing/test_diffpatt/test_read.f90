 Program Test_Read
  use CFML_GlobalDeps
  use CFML_DiffPatt
  use CFML_Strings
  type(DiffPat_E_type) :: diff
  integer :: narg,ier,mode,i
  character(len=180) :: filename,filebuf
  character(len=10)  :: modem
    narg=COMMAND_ARGUMENT_COUNT()
    if(narg > 0) then
       call GET_COMMAND_ARGUMENT(1,filebuf)
    else 
       write(*,"(a)",advance="no") " => Enter the full name of the buffer file: "
       read(*,"(a)") filebuf
    end if
  open(unit=1,file=trim(filebuf),status="old",action="read",position="rewind")
   
  do     
    read(unit=1,fmt=*,iostat=ier) mode, filename
    if(ier /= 0) exit
    write(*,"(a,i5)") " => Reading file "//trim(filename)//" with format ",mode
    Select Case (mode)
          case(0)
            modem="DEFAULT"
          case(1)
            modem="D1AOLD"
          case(2)
            modem="D1BOLD"
          case(3)
            modem="D1B"
          case(4)
            modem="NLS"
          case(5)
            modem="G41"
          case(6)
            modem="D2B"
          case(7)
            modem="3T2"
          case(8)
            modem="DMC"
          case(9,10)
            modem="XYSIGMA"
          case(12)
            modem="GSASTOF"
    End Select
    call Read_Pattern(filename,diff,modem)    
    if(Err_CFML%Ierr /= 0) then
      write(*,"(a)") trim(Err_CFML%Msg)
      if(Err_CFML%nl /= 0) then
        do i=1, Err_CFML%nl
          write(unit=*,fmt="(a)") trim(Err_CFML%txt(i))
        end do
      end if
      exit
    else 
      write(*,"(a,i6,a)") " => Diffraction pattern from: "//trim(filename)//" read correctly with", diff%npts, " points"
      do i=diff%npts/3,diff%npts/3+10
        write(*,"(i8,4f18.4)") i,diff%x(i),diff%y(i),diff%sigma(i),diff%ycalc(i)
      end do      
    end if
  end do
 End Program Test_Read