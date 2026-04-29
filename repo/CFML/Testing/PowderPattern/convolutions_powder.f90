 !!----
 !!---- PROGRAM:  Similarity_of_Powder_Patterns
 !!----
 !!---- Simple program for calculating powder patterns by reading a CIF file
 !!----
 !!---- If a CIF file is read there is no control on powder pattern parameters. A standard
 !!---- pattern corresponding to D2B with lambda=1.594 is calculated. The parameters of this
 !!---- powder pattern are described by the components of the object PPC (powder pattern constants)
 !!---- Default Powder Diffraction Pattern
 !!----    stlmax=0.6; PPC%Title="Default D2B-Powder Pattern"
 !!----    PPC%U=0.009687; PPC%V=-0.035561; PPC%W=0.049284; PPC%LAMBDA=1.594; PPC%X=0.008905
 !!----    PPC%Thmin=1.00; PPC%step=0.05;  PPC%Thmax= int(2.0*asind(stlmax*1.594)); PPC%job=1
 !!----    PPC%Ls=1900.0; nf=30; PPC%bkg=50.0
 !!----    powfile="powder_pattern.dat"
 !!----
 !!---- The program uses CrysFML and a module called Gen_Powder_Pattern where the subroutine for
 !!---- calculating the powder diffraction pattern is stored
 !!----
 Module Gen_Powder_Pattern
    !---- Use Modules ----!
    use CFML_GlobalDeps, only: to_Deg, cp
    use CFML_Maths,      only: locate
    use CFML_Reflections,only: RefList_Type, Srefl_type
    use CFML_DiffPatt,   only: DiffPat_Type, DiffPat_E_Type, allocate_pattern
    use CFML_Profiles,   only: PseudoVoigt, hat, Back_To_Back_Exp
    use CFML_FFT,        only: direct_convol

    !---- Variables ----!
    implicit none

    private

    public  :: calc_powder_pattern, PowPat_Similarity, Write_PRF, PowPat_WCC, convol_peaks_arr, convol_fft
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

    Subroutine Calc_Powder_Pattern(Ppc,Hkl,Pat)
       !---- Argument ----!
       Type(PowPat_CW_Conditions),     intent(in)  :: PPC
       type(RefList_Type),             intent(in)  :: hkl
       Type(DiffPat_E_Type),           intent(out) :: Pat

       !--- Local Variables ----!
       integer       :: i,j,k,npts,i1,i2,np,nph
       real(kind=cp) :: Intens,Bragg,Hl,Hg, ss,cs,tt,th1,th2,LorentzF, Y,eta,fwhm,chw,x
       real(kind=cp) :: alfa=14 , beta=3
       type(points_interval_type) :: interval
       real(kind=cp), dimension(:), allocatable :: convolved, pv, hat_f
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
                chw=5.0
             case(3:5)
                chw=10.0
             case(6:7)
                chw=15.0
             case(8:)
                chw=25.0
          End Select

          th1=Bragg-chw*(fwhm+1.0)
          th2=Bragg+chw*(fwhm+1.0)

          i1=Locate(Pat%x,th1,npts)
          i2=Locate(Pat%x,th2,npts)
          i1=max(i1,1)
          i2=min(i2,npts)
          np=i2-i1+1
          !nph=np/2
          if(allocated(pv)) deallocate(pv)
          if(allocated(hat_f)) deallocate(hat_f)
          allocate(pv(np),hat_f(np))
           pv=0.0; hat_f=0.0
          k=0
          do j=i1,i2
            k=k+1
            x=Pat%x(j)-Bragg
            pv(k)=PseudoVoigt( x , [fwhm,eta ] )
            hat_f(k)=Back_To_Back_Exp(x,[alfa,beta])
          end do
          convolved=direct_convol(pv, hat_f)
          Intens= LorentzF *ref(i)%mult * ref(i)%Fc**2 * PPC%Scalef
          !k=(np-1)/2
          k=0
          do j=i1,i2
             k=k+1
             Pat%ycalc(j)=Pat%ycalc(j)+ convolved(k) * Intens
          end do
       end do
       Pat%ymax=maxval(Pat%ycalc)
       Pat%ymin=minval(Pat%ycalc)
      End Select

    End Subroutine Calc_Powder_Pattern


    Function PowPat_Similarity(Pat1,Pat2) result(similarity)
      Type(DiffPat_E_Type), intent(in) :: Pat1,Pat2
      real(kind=cp) :: similarity
      integer :: i,j,n
      real(kind=cp) :: sum1,sum2,sim
      real(kind=cp), dimension(Pat1%npts) :: integ1,integ2
      n=Pat1%npts
      sum1=sum(Pat1%ycalc(1:n))
      sum2=sum(Pat2%ycalc(1:n))
      integ1=0.0_cp
      integ2=0.0_cp
      do i=1,n
        integ1(i) = sum(Pat1%ycalc(1:i))/sum1
        integ2(i) = sum(Pat2%ycalc(1:i))/sum2
      end do
      similarity= 100.0*(1.0 - sum(abs(integ1-integ2))/real(n))
    End Function PowPat_Similarity

    ! Calculation of the Cross-correlation function
    Function cross_corr(f,g) result(fg) !
      real(kind=cp), dimension(:), intent(in) :: f
      real(kind=cp), dimension(:), intent(in) :: g

      real(kind=cp), dimension(:), allocatable  :: fg,fm,gm
      integer :: i ,j, nf, ng, n
      nf=size(f); ng=size(g)
      n= min(ng,nf)
      allocate(fg(-n+1:n-1),fm(-n+1:n-1),gm(-n+1:n-1))
      fm(-n+1:-1)=0.0_cp; fm(0:n-1)=1000.0*f(1:n)/sum(f(1:n)) !Normalization by 1.0/area x 1000
      gm(-n+1:-1)=0.0_cp; gm(0:n-1)=1000.0*g(1:n)/sum(g(1:n))

      do i=-n+1,n-1
        fg(i) = 0.0_cp
        do j=-n+1,n-1
           if(j+i < -n+1) cycle
           if(j+i > n-1) cycle
           fg(i)=fg(i) + fm(j) * gm(j+i)
        end do
      end do
    End Function cross_corr

    ! Similarity between two powder patterns based on weighted cross-correlation
    ! function between two patterns. The similarity is between 0 and 100 and it is
    ! obtained as a single value after weigthed integration with a triangular function of base -L,L.
    !
    Function PowPat_WCC(Pat1,Pat2,L,prt) result(similarity)
      Type(DiffPat_E_Type), intent(in) :: Pat1,Pat2
      integer,              intent(in) :: L
      logical, optional,    intent(in) :: prt
      real(kind=cp) :: similarity
      integer :: i,j,n,lc
      real(kind=cp) :: sum11,sum22,sum12, sm, w, ttheta
      real(kind=cp), dimension(-Pat1%npts+1:Pat1%npts-1) :: c12,c11,c22

      c11=cross_corr(Pat1%ycalc,Pat1%ycalc)
      c22=cross_corr(Pat2%ycalc,Pat2%ycalc)
      c12=cross_corr(Pat1%ycalc,Pat2%ycalc)
      sum11=0.0 ; sum22=0.0 ; sum12=0.0
      sm=1.0/real(L)
      do i=-L,L
        w=1.0 - abs(real(i)*sm)  !Triangular weight
        sum11=sum11+ w * c11(i)
        sum22=sum22+ w * c22(i)
        sum12=sum12+ w * c12(i)
      end do
      similarity= 100.0*sum12/sqrt(sum11*sum22)

      if(present(prt)) then
        if(prt) then
          n=Pat1%npts
          open(newunit=lc,file="correlations.xys", status="replace",action="write")
          write(lc,"(a)") "! Multi-column correlations  c12, c11, c22"
          write(lc,"(a)") "! -2theta_max  -  2theta_max"
          write(lc,"(a,2f12.5)") "!", (-n+1)*Pat1%step, (n-1)*Pat1%step
          write(lc,"(a)") "!"
          write(lc,"(a)") "!   Testing correlation functions"
          write(lc,"(a)") "!"
          do i=-n+1,n-1
            ttheta= i*Pat1%step
            write(lc,"(4f18.5)") ttheta,c12(i),c11(i),c22(i)
          end do
          close(unit=lc)
        end if
      end if

    End Function PowPat_WCC

    Subroutine Write_PRF(fileprf,lambda,Pat,hkl)
       character(len=*),        intent(in)  :: fileprf
       real(kind=cp),           intent(in)  :: lambda
       Type(DiffPat_E_Type),    intent(in)  :: Pat
       Type(RefList_Type),      intent(in)  :: hkl

       integer :: i,j,i_prf
       character(len=*),parameter :: tb=char(9)
       character (len=50) :: forma1
       real :: ymax,scl

       open(newunit=i_prf,file=trim(fileprf),status="replace",action="write")
       ymax=Pat%ymax
       scl=1.0
       do
         if(ymax < 1.0e6) exit !on exit we have the appropriate value of scl
         scl=scl*0.1
         ymax=ymax*scl
       end do
       if(ymax < 100.0) then
        forma1='(f12.4,4(a,f8.4))'
       else if(ymax < 1000.0) then
        forma1='(f12.4,4(a,f8.3))'
       else if(ymax < 10000.0) then
        forma1='(f12.4,4(a,f8.2))'
       else if(ymax < 100000.0) then
        forma1='(f12.4,4(a,f8.1))'
       else
        forma1='(f12.4,4(a,f8.0))'
       end if

       !write(*,"(3f12.4,a,f12.4,a)") ymaxini, ymax, scl, " Pat%ymax * scl:",ymaxini*scl, " Format: "//trim(forma1)
       write(i_prf,'(a)') trim(Pat%Title)  !//"  CELL:"
                                !  N_phases, N_points, Lamda1,Lambda2,zero,shift1,shif2,Ixunit
       write(i_prf,'(I3,I7,5f12.5,i5)')1,Pat%npts,lambda,lambda,0.0,0.0,0.0,0
       write(i_prf,'(17i6)')  hkl%Nref, 0 , 0
       write(i_prf,'(15a)')' 2Theta',tb,'Yobs',tb,'Ycal',tb,  &
        'Yobs-Ycal',tb,'Backg',tb,'Posr',tb,'(hkl)',tb,'K'
       !dd=(y(i,n_pat)-yc(i,n_pat))*scl
       do  i=1,Pat%npts
         write(i_prf,forma1) Pat%x(i),tb,Pat%ycalc(i)*scl,tb,Pat%ycalc(i)*scl,tb, -ymax/4.0,tb,0.0
       end do
       !Writing reflections
       do j=1,hkl%Nref
         !iposr=-(k-1)*ideltr
           write(i_prf,'(f12.4,9a,i8,a,3i3,a,2i3)')  2.0*asind(hkl%Ref(j)%s*Lambda), &
               tb,'        ',tb,'        ',tb,'        ',  &
               tb,'        ',tb,0, tb//'(',hkl%Ref(j)%h,')'//tb,hkl%Ref(j)%imag,1
       end do
      close(unit=i_prf)
    End Subroutine Write_PRF

  End Module Gen_Powder_Pattern

  !!----
  !!----  Program Calculation_of_Powder_Patterns
  !!----
  !!----
  !!---- Update: June - 2009
  !!
  Program Similarity_of_Powder_Patterns
     !---- Use Modules ----!
     use CFML_GlobalDeps,         only: to_Deg, Err_CFML, cp
     use CFML_Atoms,              only: AtList_Type,Write_Atom_List
     use CFML_Profiles,           only: hat
     use CFML_Metrics,            only: Cell_G_type
     use CFML_Reflections,        only: RefList_Type,H_uni,get_maxnumref,Initialize_RefList, Gener_Reflections
     Use CFML_gSpaceGroups,       only: SpG_Type,Write_SpaceGroup_Info
     Use CFML_Structure_Factors,  only: Structure_Factors,Init_Structure_Factors, &
                                        SF_init_opMatTr, SF_Clear_init_symOP
     use CFML_DiffPatt,           only: DiffPat_E_Type
     use CFML_IOForm,             only: Read_Xtal_Structure
     use Gen_Powder_Pattern
     use CFML_FFT,                only: points_interval_type

     !---- Variables ----!
     implicit none
     real(kind=cp)          :: stlmax,tini,tfin,ftim=1000.0, similarity,x
     character(len=132)     :: line,powfile,filcod
     character(len=20)      :: aux_str
     character(len=14)      :: units=" milli-seconds"

     Type(Cell_G_Type)                 :: cell
     class(SPG_Type), allocatable      :: SpG
     Type(AtList_Type)                 :: A
     Type(RefList_Type)                :: hkl
     Type(DiffPat_E_Type),dimension(2) :: Pat
     Type(PowPat_CW_Conditions)        :: PPC
     type(points_interval_type)        :: interval

     integer   :: narg, ip, Mult, MaxNumRef,i,k,ier,lp, Lwidth
     Logical   :: esta
     real(kind=cp), dimension(-25:25) :: hat_prof
     real(kind=cp), dimension(50)     :: mhat_prof
     real(kind=cp), dimension(:),allocatable :: y_conv

     !---- Arguments on the command line ----!
     narg=command_argument_count()

     write(unit=*,fmt="(/,/,4(a,/))")                                                     &
          "            ------ SIMILARITY OF POWDER DIFFRACTION PATTERNS  ------"        , &
          "                    ---- Version 0.1 April-2023----"                         , &
          "    **********************************************************************"  , &
          "                           (JRC- April-2023 )"
     if (narg < 2) stop " Two names of CIF files have to be provided!"

     stlmax=0.4; PPC%Thmin=2.00; PPC%U=0.0002; PPC%V=-0.0002; PPC%W=0.012; PPC%LAMBDA=1.54056; PPC%X=0.0015
     PPC%step=0.05; PPC%Ls=1900.0;  PPC%bkg=0.0; PPC%job=0
     Lwidth=30
     if (narg > 2) then
        call get_command_argument(3,aux_str)
        read(aux_str,*,iostat=ier) PPC%Ls
        if(ier /= 0) PPC%Ls=1900.0
        if (narg > 3) then
           call get_command_argument(4,aux_str)
           read(aux_str,*,iostat=ier) stlmax
           if(ier /= 0) stlmax=0.4
        end if
        if (narg > 4) then
           call get_command_argument(5,aux_str)
           read(aux_str,*,iostat=ier) Lwidth
           if(ier /= 0) Lwidth= 30
        end if
     end if

     call cpu_time(tini)
     do ip=1,2
        call get_command_argument(ip,filcod)
        write(unit=*,fmt=*) " "
        i=index(filcod,".cif")
        if(i /= 0) then
          filcod=filcod(1:i-1)
        end if
        inquire(file=trim(filcod)//".cif",exist=esta)
        if (esta) then
          call Read_Xtal_Structure(trim(filcod)//".cif",Cell,SpG,A)
          call SF_Clear_init_symOP()
          call SF_init_opMatTr(SpG)
          !call Write_SpaceGroup_Info(SpG)
          !call Write_Atom_List(A)
        else
          write(*,*) " The file "//trim(filcod)//".cif"//" does not exist!"
          stop
        end if

        if (err_CFML%Ierr /= 0) then
           write(unit=*,fmt="(a)") trim(err_CFML%Msg)
           stop
        else
          ! Calculate a default Powder Diffraction Pattern
          PPC%Title="Powder Pattern of Structure provided in: "//trim(filcod)//".cif"
          PPC%Thmax= int(2.0*asind(stlmax*1.54056))
          powfile=trim(filcod)//".dat"
          !! Calculate sinTheta/Lambda max from 2Thetamax
          !stlmax=sind(min((PPC%Thmax+10.0)*0.5,90.0))/PPC%lambda
        end if !if error

        write(unit=*,fmt="(/,a)")  " => CALCULATION OF X-RAY POWDER DIFFRACTION PATTERN "
        PPC%title=Trim(PPC%title)//"; X-RAYS: "

        ! Now calculate a powder diffraction pattern
        ! First generate reflections and calculate structure factors

        !Mult=2*SpG%NumOps
        !MaxNumRef = get_maxnumref(stlmax,Cell%Vol,mult=Mult)
        !call Initialize_RefList(MaxNumRef, hkl, "srefl")
        !call H_Uni(Cell,SpG,.true.,0.0,stlmax,"s",MaxNumRef,hkl)

        call Gener_Reflections(Cell,0.0,stlmax,hkl,SpG,Unique=.true.,Friedel=.true.,Ref_typ='srefl')

        write(*,"(a,i8)") " => Total number of generated reflections: ",hkl%nref
        call Init_Structure_Factors(hkl,A,Spg,mode="XRA",lambda=PPC%lambda)
        write(*,"(a,i2,a)") " => Calculating structure factors for structure# ",ip," -> "//trim(filcod)//".cif"
        call Structure_Factors(hkl,A,SpG,mode="XRA",lambda=PPC%lambda)
        if (err_CFML%Ierr /= 0) then
           write(*,"(a)") " => Error in calculations of Structure Factors"
           write(*,"(a)") " => "//trim(ERR_CFML%Msg)
           stop
        end if
        PPC%Scalef=cell%RVol
        write(*,"(a,i2)") " => Calculating powder pattern ",ip
        call Calc_powder_pattern(PPC,hkl,Pat(ip))
        call Write_PRF(trim(filcod)//".prf",PPC%lambda,Pat(ip),hkl)

        open(newunit=lp,file=trim(powfile),status="replace",action="write")
          write(unit=lp,fmt="(a)") "!"//trim(Pat(ip)%title)
          write(unit=lp,fmt="(3f12.5)") Pat(ip)%xmin,Pat(ip)%step,Pat(ip)%xmax
          write(unit=lp,fmt="(8f16.4)") Pat(ip)%ycalc+PPC%bkg
        close(unit=lp)
     end do
     !Now calculate the similarity of the powder diffraction patterns
     similarity=PowPat_Similarity(Pat(1),Pat(2))
     write(*,"(a,f9.5)") " => Similarity(%) -  Hofmann = ",similarity
     similarity=PowPat_WCC(Pat(1),Pat(2),Lwidth)
     write(*,"(a,f9.5)") " => Similarity(%) - DeGelder = ",similarity
     call cpu_time(tfin)
     write(unit=*,fmt="(/,a,f15.3,a)") " => CPU-time used: ",(tfin-tini)*ftim,units
     !
     ! Testing convolutions
     !
     !allocate(y_conv(Pat(1)%npts))
     !! Creating the kernel with a function Hat of width 1 degree
     !
     !hat_prof=0.0
     !do i=-25,25
     !  x= i*Pat(1)%step
     !  hat_prof(i) = hat(x,[0.5]) !Hat function of width=1.5 degree
     !end do
     !!y_conv=direct_convol(Pat(1)%ycalc,hat_prof)
     !interval%np=Pat(1)%npts
     !interval%low=ppc%thmin; interval%high=ppc%thmax
     !y_conv=convol_fft(Pat(1)%ycalc,hat_prof,interval)
     !open(newunit=lp,file="convolved.dat",status="replace",action="write")
     !  write(unit=lp,fmt="(a)") "!"//trim(Pat(1)%title)//" Convolved with a Hat function"
     !  !write(unit=lp,fmt="(3f12.5)") Pat(1)%xmin-25*Pat(1)%step,Pat(1)%step,Pat(1)%xmax-25*Pat(1)%step
     !  write(unit=lp,fmt="(3f12.5)") Pat(1)%xmin,Pat(1)%step,Pat(1)%xmax
     !  write(unit=lp,fmt="(8f16.4)") y_conv+PPC%bkg
     !close(unit=lp)

  End Program Similarity_of_Powder_Patterns