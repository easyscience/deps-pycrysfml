 Program Correct_data
  use CFML_GlobalDeps
  use CFML_DiffPatt
  use CFML_Strings
  use CFML_Random
  type(DiffPat_E_type) :: diff
  integer :: narg,ier,mode,i,ncells,j
  integer, dimension(10) :: cell
  character(len=180) :: filename,filebuf,filecorr
  character(len=10)  :: modem
  character(len=10),dimension(2)  :: cond
    narg=COMMAND_ARGUMENT_COUNT()
    if(narg > 0) then
       call GET_COMMAND_ARGUMENT(1,filebuf)
    else 
       write(*,"(a)",advance="no") " => Enter the full name of the buffer file: "
       read(*,"(a)") filebuf
    end if
  open(unit=1,file=trim(filebuf),status="old",action="read",position="rewind")
  cond(1)="Neutrons"
  cond(2)="2Theta"  
  do     
    read(unit=1,fmt=*,iostat=ier) mode, filename, ncells, cell(1:ncells)
    if(ier /= 0) exit
    write(*,"(a,i5)") " => Reading file "//trim(filename)//" with format ",mode
    write(*,"(a,i5,a,10i6)") " => Correcting ",ncells," cells:",cell(1:ncells)
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
      diff%kindRad=cond(1) 
      diff%ScatVar =cond(2) 
      j=index(filename,".",back=.true.) 
      filecorr=filename(1:j)//"xys" 
      do i=2,diff%npts
        do j=1,ncells
          if(i == cell(j)) then
            counts=0.5*(diff%y(i-1)+diff%y(i+1))
            diff%y(i)=random_poisson(counts)
            exit
          end if
        end do
      end do
      write(*,"(a)") " => Writing Corrected Diffraction pattern: "//trim(filecorr) 
      call Write_Pattern(filecorr, diff, "XYS")     
    end if
  end do
 End Program Correct_data