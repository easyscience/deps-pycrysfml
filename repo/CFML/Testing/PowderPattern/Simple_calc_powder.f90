 !!----
 !!---- PROGRAM:  Simple_Calculation_of_Powder_Patterns
 !!----
 !!---- Simple program for calculating powder patterns by reading a CIF or a CFL file
 !!----
 !!---- If a CIF file is read there is no control on powder pattern parameters. A standard
 !!---- pattern corresponding to CuKa X-ray radiation is calculated. The parameters of this
 !!---- powder pattern are described by the components of the object PPC (powder pattern constants)
 !!---- Default Powder Diffraction Pattern
 !!----    stlmax=0.6; PPC%Title="Default Powder Pattern "
 !!----    PPC%U=0.0002; PPC%V=-0.0002; PPC%W=0.012; PPC%LAMBDA=1.54056; PPC%X=0.0015
 !!----    PPC%Thmin=1.00; PPC%step=0.05;  PPC%Thmax= int(2.0*asind(stlmax*1.54056)); PPC%job=0
 !!----    PPC%Ls=1900.0; nf=30; PPC%bkg=50.0
 !!----    powfile="powder_pattern.dat"
 !!----
 !!---- If a CFL is provided the directives for calculating the powder pattern must be provided
 !!---- before the list of the atoms. The following directives can be given:
 !!----
 !!----   TITLE  whatever set of characters identifying the job
 !!----   CELL   a b c alpha beta gamma (four real values, cell parameters)
 !!----   SPGR   SpgSymbol    (Symbol (HM/Hall) or number of the space group)
 !!----   UVWX   u v w x    (four real values defining the resolution of the instrument TCH Pseudo-Voigt)
 !!----   LAMBDA  lambda    (1 real value, wavelength)
 !!----   JOBTYPE   X-rays  (or Neutrons, if the first character is not N or n, "X-rays" is the default)
 !!----   PATTERN  Tmin  step Tmax  (intial angle, step and final angle for calculations of powder pattern)
 !!----   SIZE_L   Lorentzian_Size   (one reals in angstroms)
 !!----   POWFILE  name_of_powder_diffraction_data_file
 !!----   BACKGD   bkg   (1 real, level of background in counts)
 !!----
 !!----   ATOM  Label  ChemSymb   x    y    z    Biso   Occ
 !!----
 !!---- The number of ATOM directives is not limited. The items are 2 strings and five reals.
 !!----
 !!---- The program uses CrysFML and a module called Gen_Powder_Pattern where the subroutine for
 !!---- calculating the powder diffraction pattern is stored
 !!----
 Module Gen_Powder_Pattern
    !---- Use Modules ----!
    use CFML_GlobalDeps, only: to_Deg
    use CFML_Maths,      only: locate
    use CFML_Reflections,only: RefList_Type, Srefl_type
    use CFML_DiffPatt,   only: DiffPat_Type, DiffPat_E_Type, allocate_pattern
    use CFML_Profiles,   only: PseudoVoigt

    !---- Variables ----!
    implicit none

    private

    public  :: calc_powder_pattern
    private :: TCH

    Type, public :: PowPat_CW_Conditions
       character(len=140) :: title
       integer :: job
       real    :: Lambda, U, V, W, X, Ls
       real    :: Thmin, Thmax, step
       real    :: scalef,bkg
    End Type PowPat_CW_Conditions

 Contains
    !!----
    !!---- Pure Subroutine TCH(Hg,Hl,Fwhm,Eta)
    !!----
    !!---- Calculation of eta and FWHM of the pV-function for the
    !!---- T-C-H representation.
    !!----
    !!
    Pure Subroutine TCH(Hg,Hl,Fwhm,Eta)
       !---- Arguments ----!
       real, intent(in)  :: hg
       real, intent(in)  :: hl
       real, intent(out) :: fwhm
       real, intent(out) :: eta

       !---- Variables ----!
       real, parameter :: o1= 2.69269, o2=2.42843, o3=4.47163, o4= 0.07842
       real, parameter :: e1= 1.36603, e2=0.47719, e3=0.11116
       real            :: ctl, tlr

       ! There is no exception handling because it is supposed to be
       ! perfomed before calling TCH
       ctl=hg**5.0+o1*hg**4.0*hl+o2*hg**3.0*hl**2.0+o3*hg**2.0*hl**3.0+  &
           o4*hg*hl**4.0+hl**5.0
       fwhm=ctl**0.2
       tlr = hl/fwhm
       eta = max(1.0e-06, e1*tlr-e2*tlr*tlr+e3*tlr**3.0)  !eta
    End Subroutine TCH

    Subroutine Calc_Powder_Pattern(Ppc,Hkl,Pat,irad)
       Type(PowPat_CW_Conditions),     intent(in)  :: PPC
       type(RefList_Type),             intent(in)  :: hkl
       Type(DiffPat_E_Type),           intent(out) :: Pat
       integer,                        intent(in)  :: irad
       !--- Local Variables ----!
       integer :: i,j,npts,i1,i2
       real    :: Intens,Bragg,Hl,Hg, ss,cs,tt,th1,th2,LorentzF, Y,eta,fwhm,chw

       npts=(PPC%Thmax-PPC%Thmin)/PPC%step + 1.02
       call allocate_pattern(Pat,npts)
       Pat%Title=adjustl(Trim(PPC%title))
       i=len_trim(Pat%Title)
       write(unit=Pat%Title(i+2:),fmt="(a,f7.4,f7.1)") " => lambda,Ls: ", &
                  PPC%Lambda,PPC%Ls

       Pat%ScatVar="2-Theta"
       Pat%instr="Calculated Pattern"
       Pat%xmin= PPC%Thmin
       Pat%xmax= PPC%Thmax
       Pat%ymin= 0.0
       Pat%ymax=0.0
       Pat%scal=1.0
       Pat%monitor=0.0
       Pat%step=PPC%step
       Pat%Tsample=300.0
       Pat%Tset=300.0
       Pat%npts=npts
       Pat%ct_step=.true.
       Pat%wave=[PPC%Lambda,PPC%Lambda,0.0,0.0,0.0]
       chw=15.0
       do i=1,npts
          Pat%x(i)=Pat%xmin+real(i-1)*Pat%step
       end do

       Y= to_deg*PPC%Lambda/PPC%Ls
      Select Type(ref => hkl%ref)
       type is (Srefl_type)
       do i=1,hkl%nref
          ss=PPC%Lambda*ref(i)%S
          cs=sqrt(abs(1.0-ss*ss))
          tt=ss/cs
          LorentzF=0.5/(ss*ss*cs)
          Bragg=2.0*asind(ss)
          HG=sqrt(tt*(PPC%U*tt+PPC%V)+PPC%W)
          HL=PPC%X*tt + Y/cs
          call TCH(hg,hl,fwhm,eta)
          Select Case(nint(eta*10.0))
             Case(:2)
                chw=25.0
             case(3:5)
                chw=45.0
             case(6:7)
                chw=60.0
             case(8:)
                chw=90.0
          End Select

          th1=Bragg-chw*fwhm
          th2=Bragg+chw*fwhm
          i1=Locate(Pat%x,th1,npts)
          i2=Locate(Pat%x,th2,npts)
          i1=max(i1,1)
          i2=min(i2,npts)
          Intens= LorentzF *ref(i)%mult * ref(i)%Fc(irad)**2 * PPC%Scalef
          do j=i1,i2
             Pat%ycalc(j)=Pat%ycalc(j)+ PseudoVoigt( Pat%x(j)-Bragg, [fwhm,eta ] ) * Intens
          end do
       end do
      End Select

    End Subroutine Calc_Powder_Pattern

  End Module Gen_Powder_Pattern

  !!----
  !!----  Program Calculation_of_Powder_Patterns
  !!----
  !!----
  !!---- Update: June - 2009
  !!
  Program Simple_Calculation_of_Powder_Patterns
     !---- Use Modules ----!
     use CFML_GlobalDeps,         only: to_Deg, Err_CFML
     use CFML_Atoms,              only: AtList_Type, Allocate_Atom_List,Write_Atom_List
     use CFML_Metrics,            only: Cell_G_type, set_Crystal_Cell,write_crystal_cell
     use CFML_Reflections,        only: RefList_Type,H_uni,get_maxnumref,Initialize_RefList
     Use CFML_gSpaceGroups,       only: SpG_Type, Set_SpaceGroup, &
                                        Write_SpaceGroup_Info
     Use CFML_Structure_Factors,  only: Write_Structure_Factors, Structure_Factors,&
                                        Init_Structure_Factors
     use CFML_DiffPatt,           only: DiffPat_E_Type
     use CFML_IOForm,             only: Read_Xtal_Structure
     use CFML_Strings,            only: file_type,u_case

     use Gen_Powder_Pattern

     !---- Variables ----!
     implicit none

     integer                :: i,maxnumref,ier,mult,nf
     integer                :: lun=1,lp=2
     real                   :: stlmax,tini,tfin,tim,ftim=1.0
     character(len=132)     :: line,powfile,filcod
     character(len=3)       :: mode
     character(len=8)       :: units="seconds",radiation

     class(Cell_G_Type),  allocatable :: cell
     class(SPG_Type),     allocatable :: SpG
     Type(AtList_Type)                :: A
     type(RefList_Type)             :: hkl
     Type(DiffPat_E_Type)           :: Pat
     Type(PowPat_CW_Conditions)     :: PPC
     Type(file_type)                :: fich_cfl
     integer                        :: narg, irad !=1 XRA =2 NEU
     Logical                        :: esta, arggiven=.false.


     !---- Arguments on the command line ----!
     narg=command_argument_count()

     if (narg > 0) then
        call get_command_argument(1,filcod)
        arggiven=.true.
     end if


     write(unit=*,fmt="(/,/,6(a,/))")                                                     &
          "            ------ PROGRAM SIMPLE POWDER PATTERN CALCULATION  ------"        , &
          "                   ---- Version 0.1 December-2022----"                       , &
          "    **********************************************************************"  , &
          "    * Calculates powder diffraction pattern from a *.CFL or a *.CIF file *"  , &
          "    **********************************************************************"  , &
          "                           (JRC- December-2022 )"
     write(unit=*,fmt=*) " "

     if (.not. arggiven) then
        write(unit=*,fmt="(a)", advance='no') " => Code of the file xx.cfl (give xx): "
        read(unit=*,fmt="(a)") filcod
        if(len_trim(filcod) == 0) stop
     end if
     Mode="CFL"
     i=index(filcod,".cfl")
     if(i /= 0) filcod=filcod(1:i-1)
     inquire(file=trim(filcod)//".cfl",exist=esta)
     if ( .not. esta) then
        write(unit=*,fmt="(a)") " File: "//trim(filcod)//".cif (or .cfl) does'nt exist!"
        stop
     end if
     call Read_Xtal_Structure(trim(filcod)//".cfl",Cell,SpG,A,Ftype=fich_cfl)

    if (err_CFML%Ierr /= 0) then
       write(unit=*,fmt="(a)") trim(err_CFML%msg)
    else
       open(unit=lun,file=trim(filcod)//".powder", status="replace",action="write")
       write(unit=lun,fmt="(/,/,6(a,/))")                                                 &
          "            ------ PROGRAM SIMPLE POWDER PATTERN CALCULATION  ------"        , &
          "                    ---- Version 0.1 April-2009----"                         , &
          "    **********************************************************************"  , &
          "    * Calculates powder diffraction pattern from a *.CFL or a *.CIF file *"  , &
          "    **********************************************************************"  , &
          "                          (JRC- April 2009 )"
       ! Calculate a default Powder Diffraction Pattern
       stlmax=0.6; PPC%Title="Default Powder Pattern"
       PPC%U=0.0002; PPC%V=-0.0002; PPC%W=0.012; PPC%LAMBDA=1.54056; PPC%X=0.0015
       PPC%Thmin=1.00; PPC%step=0.05;  PPC%Thmax= int(2.0*asind(stlmax*1.54056)); PPC%job=0
       PPC%Ls=1900.0;  nf=30; PPC%bkg=50.0
       powfile="powder_pattern.dat"
       units=" seconds"
       tim=0.0

       ! Write initial structure information in the .powder file
       call Write_Crystal_Cell(Cell,lun)
       call Write_SpaceGroup_Info(SpG,lun)
       call Write_Atom_List(A,Iunit=lun)

       ! Look for calculation conditions in the CFL file that are provided before the list of atoms
       if (mode == "CFL") then
          do i=1,fich_cfl%nlines
             line=adjustl(u_case(fich_cfl%line(i)%str))
             if(line(1:4) == "ATOM") exit

             Select Case(Trim(line(1:7)))
                Case("TITLE")
                   PPC%title= adjustl(fich_cfl%line(i)%str(6:))

                Case("UVWX")
                   read(unit=line(7:),fmt=*,iostat=ier) PPC%U,PPC%V,PPC%W,PPC%X
                   if (ier /= 0) then
                      PPC%U=0.02; PPC%V=-0.02; PPC%W=0.12; PPC%X=0.0015
                   end if

                Case("LAMBDA")
                   read(unit=line(7:),fmt=*,iostat=ier) PPC%LAMBDA
                   if(ier /= 0) PPC%LAMBDA=1.56

                Case("BACKGD")
                   read(unit=line(7:),fmt=*,iostat=ier) PPC%bkg
                   if(ier /= 0) PPC%bkg=20.0

                Case("JOBTYPE")
                   radiation = adjustl(fich_cfl%line(i)%str(8:))
                   if (radiation(1:1) == "N" .or. radiation(1:1) == "n") then
                      PPC%job=1
                      write(*,*) " => Neutrons Job ...."
                   else
                      PPC%job=0
                      write(*,*) " => X-rays Job ...."
                   end if

                Case("PATTERN")
                   read(unit=line(8:),fmt=*,iostat=ier) PPC%Thmin, PPC%step,  PPC%Thmax
                   if (ier /= 0) then
                      PPC%Thmin=1.00; PPC%step=0.05;  PPC%Thmax= 120.0
                   end if

                Case("SIZE_L")
                   read(unit=line(8:),fmt=*,iostat=ier) PPC%Ls
                   if (ier /= 0) then
                      PPC%Ls=1900.0
                   end if
                   write(*,*) " => Lorentzian size: ", PPC%Ls

                Case("POWFILE")
                   powfile=adjustl(fich_cfl%line(i)%str(8:))
                   if (len_trim(powfile) == 0) then
                      powfile="powder_pattern.dat"
                   end if
                   write(*,*) " => Powder pattern file: "//trim(powfile)

             End Select
          end do

       End if

    ! Calculate sinTheta/Lambda max from 2Thetamax     PPC%Thmax= int(2.0*asind(stlmax*1.56))
    stlmax=sind(min((PPC%Thmax+10.0)*0.5,90.0))/PPC%lambda

    end if !if error

    if (PPC%job == 0) then      !X-rays
       write(unit=lun,fmt="(/,a)")  " => CALCULATION OF X-RAY POWDER DIFFRACTION PATTERN "
       PPC%title=Trim(PPC%title)//"; X-RAYS: "
    else
       write(unit=lun,fmt="(/,a)")  " => CALCULATION OF NEUTRON POWDER DIFFRACTION PATTERN"
       PPC%title=Trim(PPC%title)//"; NEUTRONS: "
    end if
    write(unit=lun,fmt="(  a,4f10.5)")  " => Resolution parameters UVWX: ",PPC%U,PPC%V,PPC%W,PPC%X
    write(unit=lun,fmt="(  a, f10.5)")  " => Lambda: ",PPC%lambda
    write(unit=lun,fmt="(  a, f10.5,a)")" => Background level: ",PPC%bkg," counts"
    write(unit=lun,fmt="(  a,2f10.2)")  " => Lorentzian size: ",PPC%Ls
    write(unit=lun,fmt="(  a,3f10.5)")  " => 2Theta range and step: ",PPC%Thmin,PPC%step,PPC%Thmax
    write(unit=lun,fmt="(  a,3f10.5)")  " => Maximum sin(Theta)/Lambda (for generating reflections): ",stlmax

    ! Now calculate a powder diffraction pattern
    ! First generate reflections and calculate structure factors
    Mult=2*SpG%NumOps

       MaxNumRef = get_maxnumref(stlmax,Cell%Vol,multip=Mult)
       !call Initialize_RefList(MaxNumRef, hkl, "srefl")
       call cpu_time(tini)
       call H_Uni(Cell,SpG,.true.,0.0,stlmax,"s",MaxNumRef,hkl,Ref_typ="srefl")
       call cpu_time(tfin)
       tim=tim+ tfin-tini
       write(*,"(a,i8)") "  => Total number of generated reflections is ",hkl%nref
       write(unit=lun,fmt="(a,i9)") " => Total number of generated reflections is ",hkl%nref

       if (PPC%job == 1) then      !Neutrons
          call Init_Structure_Factors(hkl,A,Spg,mode="NUC",lun=lun)
       else if(PPC%job == 0) then !Xrays
          call Init_Structure_Factors(hkl,A,Spg,mode="XRA",lambda=PPC%lambda,lun=lun)
       end if

       call cpu_time(tini)
       write(*,*) " => Calculating structure factors ..."
       if (PPC%job == 1) then      !Neutrons
          call Structure_Factors(hkl,A,SpG,mode="NUC")
       else if(PPC%job == 0) then !X-rays
          call Structure_Factors(hkl,A,SpG,mode="XRA",lambda=PPC%lambda)
       end if
       call cpu_time(tfin)
       tim=tim+ tfin-tini
       write(unit=lun,fmt="(a,f15.3,a)") " => CPU-time used for Structure_Factors: ",(tfin-tini)*ftim,units

       if (err_CFML%Ierr /= 0) then
          write(*,*) " => Error in calculations of Structure Factors"
          write(*,*) " => "//trim(ERR_CFML%Msg)
          stop
      end if

      if (radiation(1:1) == "N") then
         call Write_Structure_Factors(hkl,lun,mode="NUC")
         irad=2
      else
         call Write_Structure_Factors(hkl,lun,mode="XRA")
         irad=1
      end if

      call cpu_time(tini)
      PPC%Scalef=cell%RVol
      call Calc_powder_pattern(PPC,hkl,Pat,irad)
      call cpu_time(tfin)
      tim=tim+ tfin-tini
      write(*,*) " => CPU-time used for Calc_powder_pattern: ",(tfin-tini)*ftim,units
      write(unit=lun,fmt="(a,f15.3,a)") " => CPU-time used for Calc_powder_pattern: ",(tfin-tini)*ftim,units
      write(*,*) " => CPU-time for all calculations: ",tim*ftim,units
      write(unit=lun,fmt="(a,f15.3,a)") " => CPU-time for all calculations: ",tim*ftim,units

      open(unit=lp,file=trim(powfile),status="replace",action="write")
        write(unit=lp,fmt="(a)") "!"//trim(Pat%title)
        write(unit=lp,fmt="(3f10.4)") Pat%xmin,Pat%step,Pat%xmax
        write(unit=lp,fmt="(8f16.4)") Pat%ycalc+PPC%bkg
      close(unit=lun)

  End Program Simple_Calculation_of_Powder_Patterns

