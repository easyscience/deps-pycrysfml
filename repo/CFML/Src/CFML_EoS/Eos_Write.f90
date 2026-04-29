!!----
!!----
!!----
SubModule (CFML_EoS) EoS_Write_Info
   implicit none
   Contains

   !!----
   !!---- WRITE_EOS_DATAFILE
   !!----   General routine to Write Data in a Lun iunit
   !!----
   !!---- 17/07/2015
   !!
   Module Subroutine Write_EoS_DataFile(Dat,lun)
      !---- Arguments ----!
      type (eos_data_list_type), intent(in) :: dat  ! data structure
      integer,                   intent(in) :: lun  ! Unit to write the information

      !---- Variables ----!
      integer              :: ierr,i,j,k
      character(len=128)   :: text

      !> set up the labels in order
      integer, parameter           :: INI=4, IEND=21
      character(len=5),dimension(INI:IEND) :: lab=['T    ','sigT ','P    ','sigP ','V    ','sigV ',  &
                                                   'A    ','sigA ','B    ','sigB ','C    ','sigC ',  &
                                                   'ALPHA','sigAL','BETA ','sigBE','GAMMA','sigGA']

      !>
      !> assume that unit is connected and open.
      !>

      !> Write header info
      write(unit=lun,fmt='(a,a)',iostat=ierr) 'TITLE ',trim(dat%title)
      write(unit=lun,fmt='(a)',iostat=ierr)    '#'
      write(unit=lun,fmt='(a)',iostat=ierr)    '#  Data file written by CFML eos module'
      write(unit=lun,fmt='(a)',iostat=ierr)    '#'

      !> Crystal system
      if (len_trim(dat%system) > 0)then
         write(unit=lun,fmt='(a,a)',iostat=ierr)  'SYSTEM ',trim(dat%system)
         write(unit=lun,fmt='(a)',iostat=ierr)    '#'
      end if

      !> Original Tscale of data is not known, write out Tscale K if T data present
      if (dat%ic_dat(4) == 1)then
         write(unit=lun,fmt='(a)',iostat=ierr)  'TSCALE  K'
         write(unit=lun,fmt='(a)',iostat=ierr)    '#'
      end if

      !> Scales
      if (len_trim(dat%Pscale_name) > 0)then
         write(unit=lun,fmt='(a,a)',iostat=ierr)  'PSCALE ',trim(dat%Pscale_name)
         write(unit=lun,fmt='(a)',iostat=ierr)    '#'
      end if
      if (len_trim(dat%Vscale_name) > 0)then
         write(unit=lun,fmt='(a,a)',iostat=ierr)  'VSCALE ',trim(dat%Vscale_name)
         write(unit=lun,fmt='(a)',iostat=ierr)    '#'
      end if
      if (len_trim(dat%Lscale_name) > 0)then
         write(unit=lun,fmt='(a,a)',iostat=ierr)  'LSCALE ',trim(dat%Lscale_name)
         write(unit=lun,fmt='(a)',iostat=ierr)    '#'
      end if

      !> Datatype: we assume that all data are the same type: responsibility of calling program
      select case(dat%eosd(1)%xtype)
      case(1)
         write(unit=lun,fmt='(a)',iostat=ierr)  'DATATYPE MODULI ISOTHERMAL'
         write(unit=lun,fmt='(a)',iostat=ierr)    '#'
      case(2)
         write(unit=lun,fmt='(a)',iostat=ierr)  'DATATYPE MODULI ADIABATIC'
         write(unit=lun,fmt='(a)',iostat=ierr)    '#'
      end select


      !> build format line
      text='FORMAT 1'
      do i=ini,iend
         if (dat%ic_dat(i) ==1) text=trim(text)//' '//lab(i)
      end do
      write(unit=lun,fmt='(a)',iostat=ierr)    trim(text)

      !> write the data: all data written out
      do i=1,dat%n        ! loop over data points
         if (dat%ic_dat(4) == 1) write(unit=lun,fmt='(f12.6)',iostat=ierr,advance='no') dat%eosd(i)%t
         if (dat%ic_dat(5) == 1) write(unit=lun,fmt='(f12.6)',iostat=ierr,advance='no') dat%eosd(i)%sigt
         if (dat%ic_dat(6) == 1) write(unit=lun,fmt='(f12.6)',iostat=ierr,advance='no') dat%eosd(i)%p
         if (dat%ic_dat(7) == 1) write(unit=lun,fmt='(f12.6)',iostat=ierr,advance='no') dat%eosd(i)%sigp
         if (dat%ic_dat(8) == 1) write(unit=lun,fmt='(f12.6)',iostat=ierr,advance='no') dat%eosd(i)%v
         if (dat%ic_dat(9) == 1) write(unit=lun,fmt='(f12.6)',iostat=ierr,advance='no') dat%eosd(i)%sigv

         !> small loop over cell edges
         do j=1,3
            k=8+2*j
            if (dat%ic_dat(k) == 1)   write(unit=lun,fmt='(f12.6)',iostat=ierr,advance='no') dat%eosd(i)%cell(j)
            if (dat%ic_dat(k+1) == 1) write(unit=lun,fmt='(f12.6)',iostat=ierr,advance='no') dat%eosd(i)%sigc(j)
         end do

         !> small loop over cell angles
         do j=1,3
            k=14+2*j
            if (dat%ic_dat(k) == 1)   write(unit=lun,fmt='(f12.6)',iostat=ierr,advance='no') dat%eosd(i)%ang(j)
            if (dat%ic_dat(k+1) == 1) write(unit=lun,fmt='(f12.6)',iostat=ierr,advance='no') dat%eosd(i)%siga(j)
         end do
         write(unit=lun,fmt='(1x)',iostat=ierr)          ! forces new line
      end do
   End Subroutine Write_Eos_Datafile

   !!----
   !!---- SUBROUTINE WRITE_EOS_FILE
   !!----
   !!---- General routine to Write EoS in a Lun iunit
   !!----
   !!---- Update: 17/07/2015
   !!
   Module Subroutine Write_Eos_File(Eos, Lun)
      !---- Arguments ----!
      type (EoS_Type),intent(in)   :: Eos ! EoS object
      integer,        intent(in)   :: lun ! Unit

      !---- Variables ----!
      character(len=12)            :: stext
      character(len=1024)          :: text
      integer                      :: ierr,i,j,k
      real(kind=cp)                :: valp
      real(kind=cp),dimension(10)  :: p

      !>
      !> assume that unit is connected and open.
      !>

      !> Header info written to file from calling program
      write(unit=lun,fmt='(a)',iostat=ierr) ' '
      write(unit=lun,fmt='(a)',iostat=ierr) ' EosFit parameter file'
      write(unit=lun,fmt='(a)',iostat=ierr) ' '
      write(unit=lun,fmt='(a)',iostat=ierr) ' This is a fixed-format file. If you edit this file, make sure you do not move '// &
                                            'anything!'
      write(unit=lun,fmt='(a)',iostat=ierr) ' It is safer to change the parameters by loading the file to EosFit, and saving '// &
                                            'the file after making changes'
      write(unit=lun,fmt='(a)',iostat=ierr) ' _______________________________________________________________________________'// &
                                            '_____________________________'
      write(unit=lun,fmt='(a)',iostat=ierr) ' '


      !> title and comments
      write(unit=lun,fmt='(a,a)',iostat=ierr) 'Title =',trim(eos%title)
      write(unit=lun,fmt='(a,a)',iostat=ierr) 'Savedate =',trim(eos%savedate)

      do i=1,size(eos%doc)
         if (len_trim(eos%doc(i)) > 0)then
            write(unit=lun,fmt='(a)') 'Comment ='//trim(eos%doc(i))
         end if
      end do
      write(unit=lun,fmt='(a)',iostat=ierr) ' '

      !> Crystal system
      write(unit=lun,fmt='(a,a)',iostat=ierr) 'System =',trim(eos%system)

      !> eos type
      text=',  ('//trim(eos%model)//')'
      if (eos%imodel == 0) text=',  (none)'
      write(unit=lun,fmt='(a,i3,a)',iostat=ierr) 'Model =',eos%imodel,trim(text)
      write(unit=lun,fmt='(a,i3)',iostat=ierr) 'Order =',eos%iorder

      text=',  ('//trim(eos%tmodel)//')'
      if (eos%itherm == 0)text=',  (none)'
      write(unit=lun,fmt='(a,i3,a,a,a)',iostat=ierr) 'Thermal =',eos%itherm,trim(text)

      text=',  ('//trim(eos%cmodel)//')'
      if (eos%icross == 0)text=',  (none)'
      write(unit=lun,fmt='(a,i3,a,a,a)',iostat=ierr) 'Cross =',eos%icross,trim(text)

      text=',  ('//trim(eos%tranmodel)//')'
      if (eos%itran == 0)text=',  (none)'
      write(unit=lun,fmt='(a,i3,a,a,a)',iostat=ierr) 'Trans =',eos%itran,trim(text)

      text=',  ('//trim(eos%smodel)//')'
      if (eos%ishear == 0)text=',  (none)'
      write(unit=lun,fmt='(a,i3,a,a,a)',iostat=ierr) 'Shear =',eos%ishear,trim(text)

      write(unit=lun,fmt='(a,i3)',iostat=ierr) 'Angles =',eos%iangle

      write(unit=lun,fmt='(a,f10.5)',iostat=ierr) 'Pref =',eos%pref
      write(unit=lun,fmt='(a,f10.5)',iostat=ierr) 'Tref =',eos%tref
      write(unit=lun,fmt='(a,a)',iostat=ierr) 'Pscale =',trim(eos%pscale_name)
      write(unit=lun,fmt='(a,a)',iostat=ierr) 'Vscale =',trim(eos%vscale_name)

      do i=1,2
         text=',  ('//trim(eos%oscmodel(i))//')'
         if (eos%iosc(i) == 0)text=',  (none)'
         write(unit=lun,fmt='(a,i1,a,i3,a)',iostat=ierr) 'Osc',i+1,' =',eos%iosc(i),trim(text)
      end do

      if (eos%linear)then
         write(unit=lun,fmt='(a)',iostat=ierr) 'Type = Linear'
         if (len_trim(eos%LinearDir) == 0)then
            write(unit=lun,fmt='(a)') 'Direction = Unknown'
         else
            write(unit=lun,fmt='(a,a)') 'Direction =',trim(adjustl(eos%LinearDir))
         end if

      else if(eos%iangle == 0)then
         write(unit=lun,fmt='(a)',iostat=ierr) 'Type = Volume'

      else
         write(unit=lun,fmt='(a)',iostat=ierr) 'Type = Angles'
      end if

      write(unit=lun,fmt='(a,f10.5)',iostat=ierr) 'Stoich =',eos%stoich
      if (eos%density0 > tiny(0.)) then
         write(unit=lun,fmt='(a,f10.5)',iostat=ierr) 'Density0 =',eos%density0
      end if

      write(unit=lun,fmt='(a)',iostat=ierr) ' '

      !> Eos parameters
      if (eos%iangle == 0)then
         !> Normal Eos parameters
         do i=1,n_eospar
            valp=eos%params(i)*eos%factor(i)
            if (abs(valp) < 1.0E7_cp)then
               text=string_real(valp,precision(valp)+2)
            else
               write(text,'(''    Inf'')')
            end if

            if (eos%iuse(i) == 0) then
               write(unit=lun,fmt='(a,i2,a12,5a)')'Param =',i,text(1:12)
            else
               write(unit=lun,fmt='(a,i2,a12,5a)')'Param =',i,text(1:12),'     (',eos%parname(i),',  ',&
                     trim(eos%comment(i)),')'
            end if
         end do

      else
         !> angle polynomial to be written into space for params(1:30)
         do i=1,3      ! loop over angles
            p(1)=eos%angpoly(i,0,1)
            p(2:4)=eos%angpoly(i,1,1:3)      !P terms
            p(5:7)=eos%angpoly(i,2,1:3)      !T terms
            p(8:10)=eos%angpoly(i,3,1:3)      !PT terms
            do k=1,10
               j=10*(i-1)+k
               text=string_real(p(k),precision(p(k))+2)
               write(unit=lun,fmt='(a,i2,a12)')'Param =',j,text(1:12)
            end do
         end do

      end if

      !> VCV: stored as scaled values for precision
      write(unit=lun,fmt='(a)',iostat=ierr) ' '
      write(unit=lun,fmt='(a)',iostat=ierr) '  Variance-Covariance matrix='
      do i=1,n_eospar
         text=''
         do j=1,n_eospar
            if (abs(eos%vcv(i,j)) > tiny(0.0))then
               write(stext,fmt='(e12.5)')eos%vcv(i,j )*eos%factor(i)*eos%factor(j)
               text=trim(text)//stext
            else              !   123456789012
               text=trim(text)//' 0.00000E+00'
            end if
         end do
         write(unit=lun,fmt='(a)',iostat=ierr)trim(text)
      end do
      write(unit=lun,fmt='(a)',iostat=ierr) ' '
      write(unit=lun,fmt='(a)',iostat=ierr) ' '

      !>Cv table
      if (eos%itherm == 9)then
         write(unit=lun,fmt='(a)',iostat=ierr)'  Heat Capacity='
         do j=1,size(eos%cv_table,dim=1)
            write(unit=lun,fmt='(2f10.5)',iostat=ierr)eos%cv_table(j,1:2)
         end do
      end if

   End Subroutine Write_Eos_File

   !!----
   !!---- SUBROUTINE WRITE_EOSCAL
   !!----
   !!----   Subroutine to write the calculated parameters of an eos to file at a series of PT points
   !!----   NO  header info is printed here.
   !!----
   !!----   Therefore the program header and write_info_eos have to be called first before calling this routine
   !!----   Then write_eoscal_header is called from here
   !!----
   !!----   Change: 06/10/2015 to make write_eoscal_header private, and change name from write_eoscal_file
   !!----   Change: 12/12/2017 created eoscal_text so that errors and values are printed when error state
   !!----   Change: 19/12/2018 added error flag to return to calling program, if warning or error on at least one calc
   !!----   Change: 9/2020 added handling of calculated directions in unit cell
   !!----
   !!---- Date: 17/07/2015
   !!
   Module Subroutine Write_Eoscal(Pmin, Pmax, Pstep, Tmin, Tmax, Tstep, Tscale_In, Eos, Lun, &
                                  Nprint, Eoscal_ERR, Cell_Eos, Axis)
      !---- Arguments ----!
      real(kind=cp),                 intent(in)     ::  pmin, pmax, pstep   ! P to calculate properties
      real(kind=cp),                 intent(in)     ::  tmin,tmax,tstep     ! T to calculate properties
      character(len=*),              intent(in)     ::  tscale_in           ! Name of the Tscale for output, either C or K
                                                                            ! If Pstep or Tstep  < tiny(0.0) then only Pmin (or Tmin) calculated
      type(EoS_Type),                intent(in)     ::  eos                 ! Eos
      integer,                       intent(in)     :: lun                  ! logical unit for printing
      integer,                       intent(out)    :: nprint               ! Number of data printed
      logical,                       intent(out)    :: eoscal_err           ! error flag
      type(EoS_Cell_Type), optional, intent(in out) :: cell_eos
      type(axis_type),     optional, intent(in out) :: axis

      !---- Local variable ----!
      real(kind=cp)           :: p,t,tout      ! The p and T of each calculation
      real(kind=cp)           :: pst,tst  ! local copy of tstep and pstep
      character(len=255)      :: text     ! local text variable
      character(len=1)        :: tscale   ! local name of tscale
      logical                 :: loop_p   ! loop indicator .true. for inner loop of calcs over P
      integer                 :: cellcase ! indicator for type of eos or cell_eos

      !> init
      nprint=0    ! output counter
      eoscal_err=.false.
      cellcase=0

      if (present(axis) .and. present(cell_eos))then
         cell_eos%eosc%linear = .true.       !safety for using write_eoscal_header
         if (axis%ieos == -2)then
            cellcase = 1        !general drection
         else
            cellcase = 3        !third-axis from others
         end if
      end if

      !> Tscale for output: C or K
      if (len_trim(tscale_in) == 0)then
         tscale='K'
      else
         tscale=U_case(tscale_in)
         if (tscale /= 'K' .and. tscale /='C')tscale='K'
      end if

      !> Write file header
      if (cellcase ==0)then
         call write_eoscal_header(eos,lun,tscale)
      else
         call write_eoscal_cell_header(axis,lun,tscale)
      end if

      !> copy Pstep/Tstep
      tst=tstep
      pst=pstep

      !> set up loop control variables
      if (abs(pst) > tiny(0.0))then       ! inner loop over P
         loop_p=.true.
         if (abs(tst) < tiny(0.0))then   ! no outerloop
            tst=10.*max((tmax-tmin),1.0)       ! set tstep big enough to stop loop
         end if
      else
         loop_p=.false.                  ! inner loop over T
         if (abs(pst) < tiny(0.0))then   ! no outerloop
            pst=10.*max((pmax-pmin),1.0)       ! set pstep big enough to stop loop
         end if
      end if

      !> Initialise loop variables
      p=pmin
      t=tmin

      !> Start of outer loop
      outer: do
         !> reset inner loop variable to start value
         if (loop_p)then
            p=pmin
         else
            t=tmin
         end if

         inner: do
            if (cellcase == 0)then
               call clear_error()
               call physical_check(eos,Pin=p,Tin=T)
               if (err_CFML%Flag) then
                  tout=t
                  if (tscale =='C')tout=tout-273.16
                  text=trim(string_real(p,6))//'  '//trim(string_real(t,6))//' :   '//trim(err_CFML%Msg)
                  write(lun,'(a)')trim(text)
                  eoscal_err=.true.

               else
                  text=eos_cal_text(p,t,Tscale_In,Eos)
                  write(lun,'(a)')trim(text)      ! This way we get to see the calculated values even if error in calcs with valid eos
                  if (err_CFML%Flag)then
                     write(lun,'(a)')'   *****WARNING:   '//trim(err_CFML%Msg)
                     eoscal_err=.true.
                  end if
               end if

            else
               call clear_error()
               text= eoscal_text_direction(P,T,Tscale_in,cell_eos,axis)
               write(lun,'(a)')trim(text)      ! This way we get to see the calculated values even if error in calcs with valid eos
               if (err_CFML%Flag) then
                  write(lun,'(a)')'   *****WARNING:   '//trim(err_CFML%Msg)
                  eoscal_err=.true.
               end if

            end if
            nprint=nprint+1

            !> Now increment inner loop variable and test for completion
            if (loop_p) then
               p=p+pst                           ! inner loop over p
               if (p > pmax+0.99_cp*pst)exit inner
            else
               t=t+tst                           ! inner loop over t
               if (t > tmax+0.99_cp*tst)exit inner
            end if
         end do inner

         write(lun,'(/)')        ! blank line to help some plotting programs

         !> Now increment outer loop variable and test for completion
         if (loop_p)then
            t=t+tst                           ! outer loop over t
            if (t > tmax+0.99_cp*tst)exit outer
         else
            p=p+pst                           ! outer loop over p
            if (p > pmax+0.99_cp*pst)exit outer
         end if
      end do outer

   End Subroutine Write_Eoscal

   !!--++
   !!--++ SUBROUTINE WRITE_EOSCAL_CELL_HEADER
   !!--++
   !!--++ Subroutine that print information on iout unit for calculated directions without own eos
   !!--++
   !!--++ Date: 15/09/2020
   !!
   Module Subroutine Write_Eoscal_Cell_Header(Axis, Lun, Tscale_In)
      !---- Arguments ----!
      type(axis_type),  intent(in) :: axis
      integer,          intent(in) :: lun         ! logical unit for printing
      character(len=*), intent(in) :: tscale_in   ! Scale for Temp

      !---- Local Variables ----!
      character(len=1)     :: tscale
      character(len=255)   :: head     ! local text variable for column headers

      !> alpha scale
      write(lun,'(//)')
      write(lun,'("  Note that values of alpha are multiplied by a factor of 10^5")')

      !> tscale for output: C or K
      if (len_trim(tscale_in) == 0) then
         tscale='K'
      else
         tscale=U_case(tscale_in)
         if (tscale /= 'K' .and. tscale /='C')tscale='K'
      end if

      !> create column header
      if (axis%ieos == 0) then
         write(head,'("   Press   Temp",a1,"   Volume    V/V0T       K    Kprime    dK/dT    alpha")' ) Tscale
      else
         write(head,'("   Press   Temp",a1,"   Length    L/L0T       M    Mprime    dM/dT    alpha")' ) Tscale
      end if

      !> Write header
      write(lun,'(/a)')trim(head)

   End Subroutine Write_Eoscal_Cell_Header

   !!----
   !!---- SUBROUTINE WRITE_EOSCAL_HEADER
   !!----
   !!---- Subroutine that print information on iout unit
   !!----
   !!---- Date: 17/07/2015
   !!
   Module Subroutine Write_Eoscal_Header(Eos, Lun, Tscale_In, Headout)
      !---- Arguments ----!
      type(EoS_Type),            intent(in)  :: eos         ! Eos information
      integer,                   intent(in)  :: lun         ! logical unit for printing
      character(len=*),          intent(in)  :: tscale_in   ! Scale for Temp
      character(len=*),optional, intent(out) :: Headout

      !---- Local Variables ----!
      character(len=1)     :: tscale
      character(len=255)   :: head     ! local text variable for column headers

      !> alpha scale
      write(lun,'(//)')
      if (eos%itherm /= 0) then
         write(lun,'("  Note that values of alpha are multiplied by a factor of ",f5.1,"x10^5")')eos%alphafactor/1.0E5
      end if

      !> Cp, Cv units
      if (eos%imodel > 0 .and. eos%itherm > 0 .and. .not. eos%linear)then
         if (VscaleMGD(eos) )then
            write(lun,'("  Heat capacities in J/mol/K provided K0 in kbar or GPa")')

         else
            write(lun,'("  Heat capacities are in units that depend on the volume and pressure units")')
         end if
      end if
      write(lun,'(//)')

      if (eos%imodel == 1 .or. eos%imodel == 5 .or. eos%imodel == 6) then
         write(lun,'("  Do not forget: Normalised Pressure and strain not defined for ",a," Eos")')trim(eos%model)

      else if(eos%itherm /= 0)then
         write(lun,'("  Normalised Pressure (NP) and finite strain (f) are defined relative to V at P=0 and same T")')

      else
         write(lun,'("  Normalised Pressure is NP and finite strain is f")')
      end if

      !> tscale for output: C or K
      if (len_trim(tscale_in) == 0) then
         tscale='K'
      else
         tscale=U_case(tscale_in)
         if (tscale /= 'K' .and. tscale /='C')tscale='K'
      end if

      !> create column header
      if (eos%linear) then
         write(head,'(" Press   Temp",a1,"     Length  esdL       L/L0T  esd(L/L0T)  M      esdM  Mprime ", &
             &"esdMp   Mpp  esdMpp  f         esdf       NP      esdNP   dM/dT    esddK   alpha  esda")' ) Tscale
      else
         write(head,'(" Press   Temp",a1,"     Volume  esdV       V/V0T  esd(V/V0T)  K      esdK  Kprime ", &
             &"esdKp   Kpp  esdKpp  f         esdf       NP      esdNP   dK/dT    esddK   alpha  esda")' ) Tscale
      end if
      if (eos%itran > 0)head=trim(head)//'   spstrain'

      if (eos%density0 > tiny(0.0)) head=trim(head)//'  density  esdden'

      if (eos%pthermaleos .and. eos%itran ==0)head=trim(head)//'  Ptherm'

      if (eos%itherm > 0 .and. abs(eos%params(18)) > tiny(0.0)) then
         if (eos%linear)then
            head=trim(head)//'    Ms    Gamma'
         else
            head=trim(head)//'    Ks    Gamma'
         end if
      end if

      if (eos%itherm == 7)head=trim(head)//' DebyeT'
      if (eos%itherm == 8)head=trim(head)//'    EinT'

      if(eos%itherm /= 0 .and. eos%imodel /= 0 .and. (abs(eos%params(18)) >tiny(0._cp) .or. eos%osc_allowed))then
            if(VscaleMGD(Eos) .and. .not. eos%linear)head=trim(head)//'    Cp      Cv'
      endif


      !> Write header
      if (present(headout))then
         headout=trim(head)

      else
         write(lun,'(/a)')trim(head)
      end if

   End Subroutine Write_Eoscal_Header

   !!--++
   !!--++ Subroutine Write_Info_Angle_Poly
   !!--++
   !!--++ Date: 08/02/2021
   !!
   Module Subroutine Write_Info_Angle_Poly(EoS, Iang, Iout)
      !---- Arguments ----!
      type(eos_type),    intent(in) :: eos     ! eos type with angles polynomial
      integer, optional, intent(in) :: iang    ! The angle number to print, If missing prints all
      integer, optional, intent(in) :: iout    ! Logical unit

      !---- Local Variables ----!
      integer           :: lun,i,ia,angflag
      character(len=180):: ltext
      character(len=20) :: sign,var

      !> check for angle to print
      angflag=0
      if (present(iang))then
         if (iang < 1 .or. iang > 3)return        !illegal angl number
         angflag=iang
      end if

      !> Unit to print the information
      lun=6
      if (present(iout)) lun=iout

      if (eos%iangle == 0) return              !no polynomial

      do ia=1,3
         if (angflag > 0 .and. ia /= angflag)cycle
         ltext='     '//celllabel(ia+3)//'='//trim(string_real(eos%angpoly(ia,0,1),7))

         if (eos%iangle == 1)then         !polynomial
            do i=1,N_ANGPOLY            ! P terms
               if (abs(eos%angpoly(ia,1,i)) > tiny(0._cp))then
                  sign='+'
                  if (eos%angpoly(ia,1,i) < 0._cp) sign=' '     !string_real sets sign if <0
                  var='P'
                  if (i > 1)write(unit=var,fmt='("P^",i1)')i
                  ltext=trim(ltext)//' '//trim(sign)//trim(adjustl(string_real(eos%angpoly(ia,1,i),8)))//trim(var)
               end if
            end do

            do i=1,N_ANGPOLY
               if (abs(eos%angpoly(ia,2,i)) > tiny(0._cp))then
                  sign='+'
                  if (eos%angpoly(ia,2,i) < 0._cp)sign=' '     !string_real sets sign if <0
                  var='T'
                  if (i > 1)write(unit=var,fmt='("T^",i1)')i
                  ltext=trim(ltext)//' '//trim(sign)//trim(adjustl(string_real(eos%angpoly(ia,2,i),8)))//trim(var)
               end if
            end do

            do i=1,N_ANGPOLY
               if (abs(eos%angpoly(ia,3,i)) > tiny(0._cp))then
                  sign='+'
                  if (eos%angpoly(ia,3,i) < 0._cp)sign=' '     !string_real sets sign if <0
                  var='PT'
                  if (i == 2)var='P^2T'
                  if (i == 3)var='PT^2'
                  ltext=trim(ltext)//' '//trim(sign)//trim(adjustl(string_real(eos%angpoly(ia,3,i),8)))//trim(var)
               end if
            end do
         end if

         !>Write out the out the info
         write(unit=lun,fmt='(a)')trim(ltext)
      end do

   End Subroutine Write_Info_Angle_Poly

   !!----
   !!---- SUBROUTINE WRITE_INFO_EOS
   !!----
   !!---- Subroutine that print information on iout unit
   !!----
   !!---- Date: 17/07/2015
   !!
   Module Subroutine Write_Info_Eos(EoS, Iout)
      !---- Arguments ----!
      type(Eos_Type),    intent(in) :: Eos   ! EoS object
      integer, optional, intent(in) :: iout  ! Logical unit

      !---- Local Variables ----!
      character(len=30) :: line,string
      integer           :: i,lun


      !> Unit to print the information
      lun=6
      if (present(iout)) lun=iout

      !> Header / Title
      write(unit=lun,fmt='(a)') ' '
      write(unit=lun,fmt='(a)') ' '
      write(unit=lun,fmt='(a)') '  EOS Information'
      write(unit=lun,fmt='(a)') '-------------------'
      write(unit=lun,fmt='(a)') ' '
      write(unit=lun,fmt='(a)') '    Title: '//trim(EoS%title)
      write(unit=lun,fmt='(a)') 'Eos Saved: '//trim(EoS%savedate)

      !> Doc Information
      do i=1,size(EoS%doc)
         if (len_trim(EoS%doc(i)) > 0) then
            write(unit=lun,fmt='(a)') '  Comment: '//trim(EoS%doc(i))
         end if
      end do
      write(unit=lun,fmt='(a)') ' '

      !> Crystal system
      write(unit=lun,fmt='(a)') '   System: '//trim(EoS%system)
      if (EoS%linear) then
         write(unit=lun,fmt='(a)') '    Class: Linear'
         if (len_trim(EoS%LinearDir) == 0)then
            write(unit=lun,fmt='(a)') 'Direction: Unknown'

         else
            write(unit=lun,fmt='(a,a)') 'Direction: ',trim(adjustl(EoS%LinearDir))
         end if

      else if (EoS%iangle == 0)then
         write(unit=lun,fmt='(a)') '    Class: Volume'

      else
         write(unit=lun,fmt='(a)') '    Class: Angles'
      end if

      if (EoS%imodel /= 0 .or. EoS%iangle /= 0) then
         if (len_trim(EoS%Pscale_name) > 0)write(unit=lun,fmt='(a)') '   Pscale: '//trim(EoS%Pscale_name)
      end if

      if (EoS%imodel /= 0) then
         if (len_trim(EoS%Vscale_name) > 0)write(unit=lun,fmt='(a)') '   Vscale: '//trim(EoS%Vscale_name)
         write(unit=lun,fmt='(a,t27,f8.3)') '   Stoichiometry: ',EoS%stoich

         !> Reference Density
         if (EoS%density0 > tiny(0.0)) then
            write(unit=lun,fmt='(a,t27,f8.3)') '   Reference density: ',EoS%density0
         end if

         write(unit=lun,fmt='(a)') ' '
         write(unit=lun,fmt='(a)') '  Compressibility'
         write(unit=lun,fmt='(a)') '-------------------'
         write(unit=lun,fmt='(a)') ' '
         write(unit=lun,fmt='(a)') '    Model: '//trim(EoS%model)
         if (EoS%imodel > 0) then                ! no output if no p eos: all done in write_info_eos_thermal
            write(unit=lun,fmt='(a,i2)') '    Order: ',EoS%iorder

            !> Pressure Parameters
            write(unit=lun,fmt='(a,t27,f8.3)') '   Pressure of reference: ',EoS%pref
            write(unit=lun,fmt='(a)') ' '

            do i=1,5
               if (EoS%iuse(i) /= 0) then
                  line=string_numstd(EoS%params(i)*EoS%factor(i),EoS%esd(i)*EoS%factor(i))
                  string=' '
                  select case(EoS%iuse(i))
                     case (2)
                        string=' [FIXED VALUE]'
                     case (3)
                        string=' [IMPLIED VALUE]'
                  end select
                  write(unit=lun,fmt='(3x,a5,": ",a,T30,":",a)') &
                       trim(EoS%parname(i)),trim(line),trim(EoS%comment(i))//trim(string)
               end if
            end do
            write(unit=lun,fmt='(a)') ' '
         end if
      end if

      !> Thermal EOS
      if (EoS%itherm > 0) call write_info_eos_thermal(EoS,lun)

      !>Extra oscillators
      if (EoS%iosc(1) > 0) call write_info_eos_oscillator(EoS,lun)

      !> Cross terms
      if (EoS%icross > 0) call write_info_eos_cross(EoS,lun)

      !> Transition
      if (EoS%itran > 0) call write_info_eos_transition(EoS,lun)

      !> Shear
      if (EoS%ishear > 0) call write_info_eos_shear(EoS,lun)

      !> Group scales
      if(sum(EoS%iuse(51:59)) > 0) call write_info_eos_groupscales(EoS,lun)

      !> Angle polynomial
      if (EoS%iangle > 0) call Write_Info_Angle_Poly(EoS,iout=lun)

      !> End
      write(unit=lun,fmt='(a)') ' '

   End Subroutine Write_Info_Eos

   !!----
   !!---- SUBROUTINE WRITE_INFO_EOS_CELL
   !!----
   !!---- Subroutine that prints information about the list of eos in cell_eos on iout unit
   !!----
   !!---- Date: 09/09/2020
   !!
   Module Subroutine Write_Info_Eos_Cell_Type(Cell_Eos, Iout)
      !---- Arguments ----!
      type(eos_cell_type), intent(in out) :: cell_eos   !must be inout to allow the flags to be changed
      integer, optional,   intent(in)     :: iout       ! Logical unit

      !---- Local Variables ----!
      character(len=110) :: ltext
      integer            :: i,lun

      !> Unit to print the information
      lun=6
      if (present(iout)) lun=iout

      !>Update all the flags and pointers
      call set_cell_types(cell_eos)

      write(ltext,'(110(''_''))')
      write(unit=lun,fmt='(a)')ltext
      write(unit=lun,fmt='(a)')'     The crystal system is '//trim(cell_eos%system)

      !>Angle information
      select case(U_case(cell_eos%system(1:4)))
         case('MONO')
            if (cell_eos%unique < 1 .or. cell_eos%unique > 3)then
               write(unit=lun,fmt='(''     Monoclinic unique axis has not been set'')')
            else
               if (cell_eos%eosang%iangle > 0)then
                  write(unit=lun,fmt='(''     Monoclinic angle defined by polynomial:'')')
                  call write_info_angle_poly(cell_eos%eosang,cell_eos%unique,lun)

               else
                  write(unit=lun,fmt='(''     Monoclinic angle defined eos of cell edges and volume:'')')
                  if (cell_eos%obtuse(cell_eos%unique))then
                     write(unit=lun,fmt='(''     Monoclinic angle '',a,'' is set obtuse (>90deg)'')')celllabel(cell_eos%unique+3)

                  else
                     write(unit=lun,fmt='(''     Monoclinic angle '',a,'' is set acute (<90deg)'')')celllabel(cell_eos%unique+3)
                  end if
               end if
            end if

         case('TRIC')
            if (cell_eos%eosang%iangle > 0)then
               write(unit=lun,fmt='(''     Triclinic angles defined by polynomials:'')')
               do i=1,3
                  call write_info_angle_poly(cell_eos%eosang,i,lun)
               end do

            else
               write(unit=lun,fmt='(''     Triclinic angles defined by eos of cell edges, volume, and d-spacings:'')')
               do i=1,3
                  if (cell_eos%obtuse(i))then
                     write(unit=lun,fmt='(''     Triclinic angle '',a,'' is set obtuse (>90deg)'')')celllabel(i+3)

                  else
                     write(unit=lun,fmt='(''     Triclinic angle '',a,'' is set acute  (<90deg)'')')celllabel(i+3)
                  end if
               end do
            end if

         case default
            if (cell_eos%eosang%iangle> 0)then
               write(unit=lun,fmt='(''     Polynomials for angles have been loaded but the crystal system has fixed angles'')')
               write(unit=lun,fmt='(''     The angle polynomials will be ignored unless you change the system to triclinic or monoclinic'')')
            end if

      end select

      write(unit=lun,fmt='(a)')ltext

      write(unit=lun,fmt='(a)')'  The loaded EoS are:                                         PV    VT    PVT  PSCALE      VSCALE'
      do i=0,cell_eos%n
         select case(cell_eos%loaded(i))
            case default
               write(unit=lun,fmt='(2x,a4,a)')axislabel(i),':  No eos loaded'

            case(1)
               write(unit=lun,fmt='(2x,a4,a,a,3x,3(5x,a1),t80,a,t91,a)')axislabel(i),': ',cell_eos%eos(i)%title(1:47), &
                     cell_eos%cout(i,1:3),trim(cell_eos%eos(i)%Pscale_name),trim(' '//cell_eos%eos(i)%Vscale_name)

            case(2)
               write(unit=lun,fmt='(2x,a4,a)')axislabel(i),':  No eos loaded but set by symmetry'

            case(3)
               write(unit=lun,fmt='(2x, a4,a)')axislabel(i),':  No eos loaded but will be calculated from others'

            case(4)
               write(unit=lun,fmt='(2x,a4,a)')axislabel(i),':  Monoclinic unique axis'
         end select
      end do

      write(unit=lun,fmt='(a)')ltext

      write(unit=lun,fmt='(a)') ' '

   End Subroutine Write_Info_Eos_Cell_Type

   !!--++
   !!--++ WRITE_INFO_EOS_CROSS
   !!--++
   !!--++ PRIVATE
   !!--++ Subroutine that print information on iout unit
   !!--++
   !!--++ Date: 11/07/2016
   !!
   Module Subroutine Write_Info_Eos_Cross(Eos, Iout)
      !---- Arguments ----!
      type(Eos_Type),    intent(in) :: Eos
      integer, optional, intent(in) :: iout

      !---- Local Variables ----!
      character(len=30) :: line,string
      integer           :: i,lun

      !> Check
      if (Eos%icross ==0) return

      !> Unit to print the information
      lun=6
      if (present(iout)) lun=iout

      write(unit=lun,fmt='(a)') ' '
      write(unit=lun,fmt='(a)') ' '
      write(unit=lun,fmt='(a)') '      P-T cross-terms'
      write(unit=lun,fmt='(a)') '---------------------------'
      write(unit=lun,fmt='(a)') ' '
      write(unit=lun,fmt='(a)') '   Model: '//trim(eos%cmodel)

      do i=8,9
         if (eos%iuse(i) /= 0) then
            line=string_numstd(eos%params(i)*eos%factor(i),eos%esd(i)*eos%factor(i))     ! include scaling
            string=' '
            select case(eos%iuse(i))
               case(2)
                  string=' [FIXED VALUE]'
               case(3)
                  string=' [IMPLIED VALUE]'
            end select
            write(unit=lun,fmt='(3x,a5,'': '',a,T30,'':'',a)') &
                 trim(eos%parname(i)),trim(line),trim(eos%comment(i))//trim(string)
         end if
      end do
      write(unit=lun,fmt='(a)') ' '
   End Subroutine Write_Info_Eos_Cross

   !!--++
   !!--++ SUBROUTINE WRITE_INFO_EOS_GROUPSCALES
   !!--++
   !!--++ Subroutine that print information on iout unit
   !!--++
   !!--++ Date: 23/03/2020 RJA
   !!
   Module Subroutine Write_Info_Eos_GroupScales(EoS, Iout)
      !---- Arguments ----!
      type(Eos_Type),    intent(in) :: Eos   ! EoS object
      integer, optional, intent(in) :: iout  ! Logical unit

      !---- Local Variables ----!
      character(len=30) :: line,string
      integer           :: i,lun

      !> Init
      lun=6
      if (present(iout)) lun=iout

      write(unit=lun,fmt='(a)') ' '
      write(unit=lun,fmt='(a)') '  Scale factors for data groups'
      write(unit=lun,fmt='(a)') '---------------------------------'

      do i=51,59
         if (EoS%iuse(i) /= 0) then
            line=string_numstd(EoS%params(i)*EoS%factor(i),EoS%esd(i)*EoS%factor(i))
            string=' '
            select case(EoS%iuse(i))
               case(2)
                  string=' [FIXED VALUE]'
            end select
            write(unit=lun,fmt='(3x,a5,": ",a,T30,":",a)') &
                  trim(EoS%parname(i)),trim(line),trim(EoS%comment(i))//trim(string)
         end if
      end do

   End Subroutine Write_Info_Eos_GroupScales

   !!--++
   !!--++ SUBROUTINE WRITE_INFO_EOS_OSCILLATOR
   !!--++
   !!--++ Subroutine that print information on iout unit
   !!--++
   !!--++ Date: 09/03/2020 RJA
   !!
   Module Subroutine Write_Info_Eos_Oscillator(EoS, Iout)
      !---- Arguments ----!
      type(Eos_Type),    intent(in) :: Eos     ! EoS object
      integer, optional, intent(in) :: iout    ! Logical unit

      !---- Local Variables ----!
      character(len=30) :: line,string
      integer           :: i,lun,j

      !> Check
      if (EoS%iosc(1) == 0) return  !if no first oscillator, there is no second
      if (.not. EoS%osc_allowed)return

      !> Init
      lun=6
      if (present(iout)) lun=iout

      write(unit=lun,fmt='(a)') ' '
      write(unit=lun,fmt='(a)') '  Additional Oscillators'
      write(unit=lun,fmt='(a)') '--------------------------'

      do j=1,2
         write(unit=lun,fmt='(a)') ' '
         write(unit=lun,fmt='(a,i1,a)') 'Oscillator ',j+1,': '//trim(EoS%oscmodel(j))
         write(unit=lun,fmt='(a)') ' '

         do i=35+5*j,39+5*j
            if (EoS%iuse(i) /= 0) then
               line=string_numstd(EoS%params(i)*EoS%factor(i),EoS%esd(i)*EoS%factor(i))
               string=' '
               select case(EoS%iuse(i))
                  case(2)
                     string=' [FIXED VALUE]'

                  case(3)
                     string=' [IMPLIED VALUE]'
               end select
               write(unit=lun,fmt='(3x,a5,": ",a,T30,":",a)') &
                     trim(EoS%parname(i)),trim(line),trim(EoS%comment(i))//trim(string)
            end if
         end do
      end do

   End Subroutine Write_Info_Eos_Oscillator

   !!--++
   !!--++ WRITE_INFO_EOS_SHEAR
   !!--++
   !!--++ PRIVATE
   !!--++ Subroutine that print information on iout unit
   !!--++
   !!--++ 11/07/2016
   !!
   Module Subroutine Write_Info_Eos_Shear(Eos,Iout)
      !---- Arguments ----!
      type(Eos_Type),    intent(in) :: Eos
      integer, optional, intent(in) :: iout

      !---- Local Variables ----!
      character(len=30) :: line,string
      integer           :: i,lun

      !> Check
      if(Eos%ishear ==0) return

      !> Unit to print the information
      lun=6
      if (present(iout)) lun=iout

      write(unit=lun,fmt='(a)') ' '
      write(unit=lun,fmt='(a)') ' '
      write(unit=lun,fmt='(a)') '  Shear modulus Information'
      write(unit=lun,fmt='(a)') '---------------------------'
      write(unit=lun,fmt='(a)') ' '
      write(unit=lun,fmt='(a)') '   Model: '//trim(eos%smodel)

      do i=30,39
         if (eos%iuse(i) /= 0) then
            line=string_numstd(eos%params(i)*eos%factor(i),eos%esd(i)*eos%factor(i))     ! include scaling
            string=' '
            select case(eos%iuse(i))
                case(2)
                string=' [FIXED VALUE]'
                case(3)
                string=' [IMPLIED VALUE]'
            end select
            write(unit=lun,fmt='(3x,a5,'': '',a,T30,'':'',a)') &
                 trim(eos%parname(i)),trim(line),trim(eos%comment(i))//trim(string)
         end if
      end do
      write(unit=lun,fmt='(a)') ' '

   End Subroutine Write_Info_Eos_Shear

   !!--++
   !!--++ SUBROUTINE WRITE_INFO_EOS_THERMAL
   !!--++
   !!--++ Subroutine that print information on iout unit
   !!--++
   !!--++ Date: 17/07/2015
   !!
   Module Subroutine Write_Info_Eos_Thermal(EoS, Iout)
      !---- Arguments ----!
      type(Eos_Type),    intent(in) :: Eos   ! EoS object
      integer, optional, intent(in) :: iout  ! Logical unit

      !---- Local Variables ----!
      character(len=30) :: line,string
      integer           :: i,lun,is

      !> Init
      lun=6
      if (present(iout)) lun=iout

      is=10                            ! If pmodel present, it was already reported
      if (EoS%imodel == 0) is=1     ! if no pmodel report all params here


      write(unit=lun,fmt='(a)') ' '
      write(unit=lun,fmt='(a)') '  Thermal Expansion'
      write(unit=lun,fmt='(a)') '-------------------'
      write(unit=lun,fmt='(a)') ' '
      write(unit=lun,fmt='(a)') '   Model: '//trim(EoS%tmodel)

      if (EoS%osc_allowed .and. EoS%params(14) > 0.5_cp)then
          if(eos%itherm == 6)then
            write(unit=lun,fmt='(a)') '           (this is a q-compromise EoS)'
          else
            write(unit=lun,fmt='(a)') '           with q-compromise'
          endif
      endif

      write(unit=lun,fmt='(a)') ' '

      write(unit=lun,fmt='(a,f8.2,a)') '   Temperature of reference: ',EoS%tref,' K'
      write(unit=lun,fmt='(a)') ' '

      do i=is,19
         if (EoS%iuse(i) /= 0) then
            line=string_numstd(eos%params(i)*eos%factor(i),eos%esd(i)*eos%factor(i))     ! include scaling
            string=' '
            select case(EoS%iuse(i))
               case(2)
                  string=' [FIXED VALUE]'

               case(3)
                  string=' [IMPLIED VALUE]'
            end select
            write(unit=lun,fmt='(3x,a5,": ",a,T30,":",a)') &
                  trim(EoS%parname(i)),trim(line),trim(EoS%comment(i))//trim(string)
         end if
      end do

      !> extra stuff if additional oscilators
      if (sum(EoS%iosc) > 0. .and. EoS%osc_allowed)then
         write(unit=lun,fmt='(a)') ' '
         write(unit=lun,fmt='(a,f6.3,a)') '   This oscillator models ',1.0_cp-EoS%params(40)-EoS%params(45),' of the total modes'
      end if

   End Subroutine Write_Info_Eos_Thermal

   !!--++
   !!--++ SUBROUTINE WRITE_INFO_EOS_TRANSITION
   !!--++
   !!--++ Subroutine that print information on iout unit
   !!--++
   !!--++ Date: 17/07/2015
   !!
   Module Subroutine Write_Info_Eos_Transition(EoS, Iout)
      !---- Arguments ----!
      type(Eos_Type),    intent(in) :: EoS  ! EoS object
      integer, optional, intent(in) :: iout    ! Logical Unit

      !---- Local Variables ----!
      character(len=30) :: line,string
      integer           :: i,lun

      !> Check
      if(EoS%itran == 0)return

      !> Init
      lun=6
      if (present(iout)) lun=iout

      write(unit=lun,fmt='(a)') ' '
      write(unit=lun,fmt='(a)') ' '
      write(unit=lun,fmt='(a)') '  Phase Transition '
      write(unit=lun,fmt='(a)') '-------------------'
      write(unit=lun,fmt='(a)') ' '
      write(unit=lun,fmt='(a)') '   Model: '//trim(EoS%tranmodel)
      write(unit=lun,fmt='(a)') ' '

      do i=20,29
         if (EoS%iuse(i) /= 0) then
            line=string_numstd(eos%params(i)*eos%factor(i),eos%esd(i)*eos%factor(i))     ! include scaling
            string=' '
            select case(EoS%iuse(i))
               case (2)
                  string=' [FIXED VALUE]'
               case (3)
                  string=' [IMPLIED VALUE]'
            end select
            write(unit=lun,fmt='(3x,a5,": ",a,T30,":",a)') &
                  trim(EoS%parname(i)),trim(line),trim(EoS%comment(i))//trim(string)
         end if
      end do

   End Subroutine Write_Info_Eos_Transition

   !!----
   !!---- FUNCTION EOS_CAL_TEXT
   !!----
   !!----   Subroutine to write the calculated parameters of an eos to file at one P T point
   !!----   NO  header info is printed here.
   !!----
   !!----   Normally called from Write_eoscal
   !!----
   !!---- Date: 16/02/2021
   !!
   Module Function EoS_Cal_Text(P, T, Tscale_In, Eos) Result(Text)
      !---- Arguments ----!
      real(kind=cp),     intent(in)  :: p                   ! P to calculate properties
      real(kind=cp),     intent(in)  :: t                   ! T to calculate properties
      character(len=*),  intent(in)  :: tscale_in           ! Name of the Tscale for output, either C or K
      type(EoS_Type),    intent(in)  :: eos                 ! Eos
      character(len=:),  allocatable :: text                ! character string with results

      !---- Local variable ----!
      integer,parameter       :: nout=21
      character(len=1)        :: tscale   ! local name of tscale
      integer,dimension(nout) :: ip=(/6,6,9,8,6,5,  5, 9, 7, 7,    5,  9, 7,7,6,6,6,6,6,6,6/) ! format for output
      integer                 :: i

      real(kind=cp),dimension(6)   :: parvals
      real(kind=cp),dimension(6)   :: esd
      real(kind=cp),dimension(nout):: parout,esdout
      real(kind=cp)                :: v0,fp,fs,agt

      !> Init
      text=" "

      !> Tscale for output: C or K
      if (len_trim(tscale_in) == 0)then
         tscale='K'
      else
         tscale=U_case(tscale_in)
         if (tscale /= 'K' .and. tscale /='C')tscale='K'
      end if

      call clear_error()
      esd=0.0_cp
      esdout=0.0_cp
      parout=0.0_cp

      !> Now do the calculations at P,T
      Parvals= EoS_Cal(P,T,eos)
      if (sum(eos%vcv) > tiny(0.0_cp)) esd= eos_cal_esd(P,T,eos)

      !> build ouput value array
      V0=Get_Volume(0.0,T,Eos)

      parout(1)=p
      parout(2)=t
      if (tscale =='C')parout(2)=parout(2)-273.16
      parout(3)=parvals(1)*eos%factor(1)      ! v
      esdout(3)=esd(1)*eos%factor(1)
      parout(4)=parvals(1)/v0                 ! v/V0 at this T
      esdout(4)=esdout(3)/v0

      !> convert  V,K,Kp,Kpp to output values
      do i=2,4
         parout(i+3)=parvals(i)*eos%factor(i)
         esdout(i+3)=esd(i)*eos%factor(i)
      end do

      !>deal with f-F:
      if (abs(p) < tiny(0.0) ) then
         call ffcal_eos(p,t,eos,fp,fs)      ! because F not defined numerically at P=0
         parout(9)=FP
         esdout(9)=0.0_cp
      else
         call ffcal_dat_esd(parvals(1),esd(1),V0,0.0_cp,P,0.0_cp,Eos, &          ! only esd input is esd(V) at this P
              parout(9),esdout(9),parout(8),esdout(8))
      end if

      !> dK/dT
      parout(10)=parvals(5)*eos%factor(8)
      esdout(10)=esd(5)*eos%factor(8)

      !> handle alpha
      parout(11)=parvals(6)*eos%alphafactor
      esdout(11)=esd(6)*eos%alphafactor

      !> spon strain
      if (eos%itran > 0) parout(12)=Get_Transition_Strain(P,T,Eos)

      !> density
      if (eos%density0 > tiny(0.0)) then
         parout(13)=eos%density0*eos%params(1)/parvals(1)
         parout(14)=parout(13)*esd(1)/parvals(1)
      end if

      !> Thermal pressure
      if (eos%pthermaleos .and. eos%itran ==0) parout(15)=p-get_pressure(parvals(1),eos%tref,eos)

      !> Report adiabatic properties
      if (eos%itherm > 0 .and. abs(eos%params(18)) > tiny(0.0)) then
         parout(19)=Get_Grun_th(p,t,eos)             !Gruneisen for Kt--> Ks
         agt=parvals(6)*parout(19)*T        ! Get_Grun knows about linear
         if (eos%linear) agt=3.0_cp*agt
         parout(18)=(1.0_cp+agt)*parvals(2) ! Ks/Ms
      end if

      !> MGD EoS parameters:
      if (eos%itherm == 7 .or. eos%itherm == 8) then
          parout(17)=get_DebyeT(parvals(1),eos)      !Debye or Einstein T
      end if

      !> Cp and CV - write these provided there is a thermal and eos model and non-zero gamma0
          if(eos%itherm /= 0 .and. eos%imodel /= 0 .and. (abs(eos%params(18)) >tiny(0._cp) .or. eos%osc_allowed))then
              if(VscaleMGD(Eos) .and. .not. eos%linear)then
                 parout(20)=get_cp(P,T,Eos)
                 parout(21)=get_cv(P,T,Eos)
              endif
      end if

      !> output this datum: dynamic formatting to text string

      !> pressure (no esd)
      text=trim(string_real(parout(1),ip(1)))

      !> T value (no esd)
      text=trim(text)//'  '//trim(string_real((parout(2)),ip(2)))

      !> other params
      do i=3,11
         text=trim(text)//'  '//trim(string_real(parout(i),ip(i)))//' '//trim(string_real(esdout(i),ip(i)))
      end do

      if (eos%itran > 0) text=trim(text)//'  '//trim(string_real(parout(12),ip(12)))    ! spontaneous strain

      if (eos%density0 > tiny(0.0)) &
         text=trim(text)//'  '//trim(string_real(parout(13),ip(13)))//' '//trim(string_real(parout(14),ip(14)))

      if (eos%pthermaleos .and. eos%itran ==0)text=trim(text)//'  '//trim(string_real(parout(15),ip(15)))

      if (eos%itherm > 0 .and. abs(eos%params(18)) > tiny(0.)) &
         text=trim(text)//'  '//trim(string_real(parout(18),ip(18)))//'  '//trim(string_real(parout(19),ip(19)))
         !>Cp and CV - write these provided there is a thermal and eos model and non-zero gamma0

      if (eos%itherm == 7 .or.  eos%itherm == 8) &
         text=trim(text)//'  '//trim(string_real(parout(17),ip(17)))

      if (eos%itherm /= 0 .and. eos%imodel /= 0 .and. (abs(eos%params(18)) >tiny(0._cp) .or. eos%osc_allowed))then
              if(VscaleMGD(Eos) .and. .not. eos%linear) &
                text=trim(text)//'  '//trim(string_real(parout(20),ip(20)))//'  '//trim(string_real(parout(21),ip(21)))
      end if

   End Function EoS_Cal_Text

   !!--++
   !!--++ FUNCTION EOSCAL_TEXT_DIRECTION
   !!--++
   !!--++   Subroutine to write the calculated parameters of calculated direction to file at one PT point
   !!--++   NO  header info is printed here.
   !!--++
   !!--++   Normally called from Write_eoscal
   !!--++
   !!--++   Date: 09/2020
   !!
   Module Function Eoscal_Text_Direction(P, T, Tscale_In, Cell_eos, Axis) Result(Text)
      !---- Arguments ----!
      real(kind=cp),      intent(in)  :: p                   !P to calculate properties
      real(kind=cp),      intent(in)  :: t                   !T to calculate properties
      character(len=*),   intent(in)  :: tscale_in           ! Name of the Tscale for output, either C or K
      type(axis_type),    intent(in)  :: axis                ! The direction
      type(EoS_Cell_Type),intent(in)  :: cell_eos            ! The eos for all the cell
      character(len=:), allocatable   :: text                ! character string with results

      !---- Local variable ----!
      integer,parameter         :: nout=8
      character(len=1)          :: tscale   ! local name of tscale
      integer,dimension(nout)   :: ip=(/6,6,8,8,6,6,7,7/) ! format for output
      integer                   :: i

      real(kind=cp),dimension(6)   :: parvals
      real(kind=cp),dimension(nout):: parout
      real(kind=cp)                :: v0

      !> Init
      Text=" "

      !> Tscale for output: C or K
      if (len_trim(tscale_in) == 0)then
         tscale='K'
      else
         tscale=U_case(tscale_in)
         if (tscale /= 'K' .and. tscale /='C')tscale='K'
      end if

      call clear_error()
      parout=0.0_cp

      !> Now do the calculations at P,T
      if (axis%ieos == -2)then
         Parvals= get_props_general(P,T,cell_eos,axis)
         V0=get_Volume_general(0.0_cp,T,cell_eos,axis)
      else
         Parvals= get_props_third(P,T,cell_eos,axis%ieos)
         V0=get_volume_third(0.0_cp,T,cell_eos,axis%ieos)
      end if

      !> build ouput value array
      parout(1)=p
      parout(2)=t
      if (tscale =='C')parout(2)=parout(2)-273.16
      parout(3)=parvals(1)*cell_eos%eosc%factor(1)      ! v
      parout(4)=parvals(1)/v0                 ! v/V0 at this T

      parout(5)=parvals(2)*cell_eos%eosc%factor(2)   !K or M
      parout(6)=parvals(3)*cell_eos%eosc%factor(3)   !Kp or Mp
      parout(7)=parvals(5)*cell_eos%eosc%factor(5)   !dK/dT or dM/dT
      parout(8)=parvals(6)*cell_eos%eosc%factor(10)   !alpha

      !> output this datum: dynamic formatting to text string

      !> No esd's
      do i=1,nout
         text=trim(text)//'  '//trim(string_real(parout(i),ip(i)))
      end do

   End Function Eoscal_Text_Direction

End SubModule EoS_Write_Info