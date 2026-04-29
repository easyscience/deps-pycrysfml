Module nFP_Reflections
   Use CFML_GlobalDeps,                only: CP, PI, TPI, Err_CFML, Clear_Error
   Use CFML_gSpaceGroups,              only: Spg_Type,kvect_info_type, SuperSpaceGroup_type, Allocate_KVector
   Use CFML_Maths,                     only: Trace, Sort, Equal_vector, locate
   Use CFML_Metrics,                   only: Cell_G_Type
   Use CFML_Strings,                   only: l_case, u_case
   Use CFML_Rational
   Use CFML_DiffPatt,                  only: PowPatt_CW_Conditions_Type, PowPatt_TOF_Conditions_Type
   Use CFML_IOForm,                    only: Powder_attributes_Type
   use CFML_Profiles,                  only: Get_FWHM_Eta, calc_pseudo_voigt, pseudovoigt, Init_Prof_Val
   use CFML_Reflections
   use CFML_Powder
   use nFP_Globals
   implicit none
   private
   public :: nFP_gen_Reflections,Reflections_Contribution_CW_ini,&
             Profile_Contribution_CW

   !integer :: N_phases
   !integer :: N_patterns, N_,CW_patt, N_TOF_patt
   !type(Phase_Type),       dimension(:), allocatable :: Ph        !> Phases
   !type(Pattern_Type),     dimension(:), allocatable :: Pat       !> Patterns
   !type, extends(RefList_Type), public :: RefP_type         !> This is a property of a phase and the patterns to which it contributes
   !    integer                                    :: N_patt !> Number of patterns to which the reflection list contributes
   !    integer, dimension(:,:,:),     allocatable :: ptr    !> (2,Nref,N_pat), Contributions are in the phase structure, calculated using wdt*FWHM for each pattern
   !                                                         !> index 1(2) is the minimum(maximum) points in the scattering variable of pattern n_pat
   !    real(kind=cp), dimension(:,:), allocatable :: pos    !> (Nref,N_patt)
   !end type RefP_type
   !type(RefP_type),   dimension(:),   allocatable :: Ref  ! to be allocated as (N_phases)

   contains

    !Module Subroutine Write_Info_RefList(Reflex, Iunit, Mode,kinfo)
    !  !---- Arguments ----!
    !  type(RefList_Type),              intent(in) :: Reflex
    !  integer,               optional, intent(in) :: Iunit
    !  character(len=*),      optional, intent(in) :: Mode
    !  type(kvect_info_type), optional, intent(in) :: kinfo
   Subroutine nFP_gen_Reflections(lun)
      integer, optional, intent(in) :: lun
      integer       :: i,j,k,MaxRef, N_patt, n, n_pat
      real(kind=cp) :: sintlmax, dsp
      real(kind=cp), dimension(:), allocatable :: wdt
      logical :: twowaves

      !Calculate maximum sinTheta/Lambda
      sintlmax=sintl_max()
      if(allocated(RL)) deallocate(RL)
      allocate(RL(N_Phases))
      write(*,*) " Maximum sintheta/Lambda =",sintlmax
      twowaves=.false.

      do i=1,N_Phases

        Select Type(spg => Ph(i)%SpG)

         Type is (SPG_Type)
           if(Ph(i)%mag) then
              if(Ph(i)%mag_only) then
                call Gener_Reflections(Ph(i)%Cell,0.0,sintlmax,RL(i),spg,i,MagExt=.true.,Unique=.true.,mag_only=.true.,Friedel=.true.,Ref_typ='MRefl')
              else
                call Gener_Reflections(Ph(i)%Cell,0.0,sintlmax,RL(i),spg,i,MagExt=.true.,Unique=.true.,Friedel=.true.,Ref_typ='MRefl')
              end if
           else
              if(spg%NumSpg < 230 .and. spg%NumSpg > 0) then  !Bad calculation check wiht CrysFML
                MaxRef=Get_MaxNumRef(SinTLMax, Ph(i)%cell%Vol, 0.0_cp, multip=spg%Multip)
                call H_Uni(Ph(i)%cell, spg, .true., 0.0, sintlmax, "s", MaxRef, RL(i),Ref_typ='SRefl')
              else
               call Gener_Reflections(Ph(i)%Cell,0.0,sintlmax,RL(i),spg,i,Unique=.true.,Friedel=.true.,Ref_typ='SRefl')
              end if
           end if

         type is (SuperSpaceGroup_Type)
           if(Ph(i)%mag) then
              if(Ph(i)%mag_only) then
                call Gener_Reflections(Ph(i)%Cell,0.0,sintlmax,RL(i),spg,i,MagExt=.true.,Unique=.true.,mag_only=.true.,Friedel=.true.,Ref_typ='MRefl',kout=Ph(i)%kvec)
              else
                call Gener_Reflections(Ph(i)%Cell,0.0,sintlmax,RL(i),spg,i,MagExt=.true.,Unique=.true.,Friedel=.true.,Ref_typ='MRefl',kout=Ph(i)%kvec)
              end if
           else
              call Gener_Reflections(Ph(i)%Cell,0.0,sintlmax,RL(i),spg,i,Unique=.true.,Friedel=.true.,Ref_typ='SRefl',kout=Ph(i)%kvec)
           end if
        End Select

        !call Write_Info_RefList(RL(i), lun)
        !Select Type( r => RL(i)%Ref)
        !  type is(Refl_Type)
        !    write(*,"(a,i3,a)") " => Reflections of phase #",i," have been allocated as type: Refl_Type"
        !  type is(SRefl_Type)
        !    write(*,"(a,i3,a)") " => Reflections of phase #",i," have been allocated as type: SRefl_Type"
        !  type is(MRefl_Type)
        !    write(*,"(a,i3,a)") " => Reflections of phase #",i," have been allocated as type: MRefl_Type"
        !End Select
      end do

      !Determine now the contributions to the patterns
      allocate(wdt(N_patterns))
      wdt=5.0_cp
      do i=1,N_patterns
        Select Type (tw => Pat(i)%cond)
          Class is (PowPatt_CW_Conditions_Type)
             wdt(i)=max(5.0,tw%wdt)
          Class is (PowPatt_TOF_Conditions_Type)
             wdt(i)=max(5.0,tw%wdt)
        End Select
        !write(*,"(a,i3,a,f12.5)") "  Pattern #",i,"   Wdt=",wdt(i)
      end do

      do i=1,N_Phases
        N_patt=Ph(i)%Ncontr; n=RL(i)%Nref
        RL(i)%N_patt=N_patt
        RL(i)%i_ph=i
        !Write(*,"(a,i3)") " Working in nFP_gen_Reflections for phase #",i

        do j=1,Ph(i)%Ncontr  !Check if there is a pattern with 2 wavelengths (in such a case the number of reflection is duplicated)
           n_pat= Ph(i)%patterns(j) !number of the pattern to which the phase contributes
           Select Type (tw => Pat(n_pat)%cond)
             Class is (PowPatt_CW_Conditions_Type)
                if(tw%twowaves) then
                  n=n*2 !duplicate the number of reflections
                  twowaves=.true.
                  exit
                end if
           End Select
           !write(*,"(a,i6,f12.5)") " n_pat,wdt(n_pat): ", n_pat,wdt(n_pat)
        end do

        if(allocated(RL(i)%ptr))     deallocate(RL(i)%ptr)
        if(allocated(RL(i)%pos))     deallocate(RL(i)%pos)
        if(allocated(RL(i)%HG))      deallocate(RL(i)%HG)
        if(allocated(RL(i)%HL))      deallocate(RL(i)%HL)
        if(allocated(RL(i)%FWHM))    deallocate(RL(i)%FWHM)
        if(allocated(RL(i)%ETA))     deallocate(RL(i)%ETA)
        if(allocated(RL(i)%patts))   deallocate(RL(i)%patts)
        if(allocated(RL(i)%corr))    deallocate(RL(i)%corr)
        allocate(RL(i)%ptr(2,n,N_patt),RL(i)%pos(n,N_patt),RL(i)%HG(n,N_patt),RL(i)%HL(n,N_patt),RL(i)%corr(n,N_patt))
        allocate(RL(i)%FWHM(n,N_patt),RL(i)%ETA(n,N_patt),RL(i)%patts(N_patt))
        RL(i)%ptr=0;  RL(i)%pos = 0.0_cp; RL(i)%HG=0.0_cp;  RL(i)%HL=0.0_cp
        RL(i)%FWHM=0.0_cp; RL(i)%ETA=0.0_cp
        RL(i)%patts=Ph(i)%patterns

        call Init_Prof_Val()  !This initializes the table for Gauss-Legendre integration

        do j=1, Ph(i)%Ncontr
           n_pat= Ph(i)%patterns(j) !number of the pattern to which the phase contributes
           if(Pat(n_pat)%sample /= "P") cycle

           if(Pat(n_pat)%mode == "CW") then
             call Reflections_Contribution_CW_ini(i,j,RL(i),wdt(n_pat))
           else if(Pat(n_pat)%mode == "TF") then

              do k=1,RL(i)%Nref  !Calculate the position of the reflection for each pattern and the contributing points
                dsp=2.0_cp/RL(i)%Ref(k)%s
                RL(i)%pos(k,j)=dsp*Pat(n_pat)%Pdat%wave(1)
              end do

           end if

        end do !j=1, Ph(i)%Ncontr

        if(present(lun)) call Write_RefList(RL(i), lun)

      end do !i=1,N_Phases

   End Subroutine nFP_gen_Reflections

   Subroutine Reflections_Contribution_CW_ini(iph,ipat,RL,wdt)
      integer,                     intent(in)     :: iph,ipat
      type(RefP_type),             intent(in out) :: RL !It has been partially filled before
      real(kind=cp),               intent(in)     :: wdt
      !--- Local variables ---!
      integer :: k,kn,NRef,npts,n_pat
      real(kind=cp)               :: pt, sint, cost, HG, HL, fwhm, eta, plor1, &
                                     pos_left, pos_right, plor, pref_corr, ratio
      real(kind=cp), dimension(2) :: lambda
      logical :: twowaves

      Nref=RL%Nref
      n_pat= Ph(iph)%patterns(ipat)
      npts=Pat(n_pat)%Pdat%npts
      Select Type(cond => Pat(n_pat)%cond)
        Type is(PowPatt_CW_Conditions_Type)
           twowaves=cond%twowaves
           lambda=cond%Lambda
           ratio=cond%ratio
      End Select

      !Write(*,"(a,f12.5)") "  WDT: ",wdt
      !Write(*,"(a)") " Num_Ref  n_pat   Position   Lorentz-Abs      Pref       Corr"

      associate(xp => Pat(n_pat)%Pdat%x)

        do k=1,Nref  !Calculate the position of the reflection for each pattern and the contributing points
          sint=RL%Ref(k)%s * Pat(n_pat)%PDat%wave(1)
          if(sint > 1.0_cp) cycle
          cost=sqrt(abs(1.0-sint*sint))
          pt = 2.0_cp * asind( sint )
          RL%pos(k,ipat)=pt
          Call Get_HG_HL_CW(iph,n_pat,k,1,HG,HL)
          ! Calculate the FWHM for each reflection and complete the type
          Call Get_FWHM_Eta(HG,HL,fwhm,eta)
          pos_left =pt-fwhm*wdt
          pos_right=pt+fwhm*wdt
          RL%ptr(1,k,ipat)=max(1,locate(xp,pos_left,npts))
          RL%ptr(2,k,ipat)=min(npts,locate(xp,pos_right,npts))
          RL%HG(k,ipat)=HG
          RL%HL(k,ipat)=HL
          RL%FWHM(k,ipat)=fwhm
          RL%ETA(k,ipat)=eta
          !Calculation of CORR for the current reflection
          plor1=Lorentz_abs_CW(sint,cost,pt,0.0,"N","DBS","HEWAT")

          plor=Powder_Lorentz_IntegInt_CW("N",0.0,0.0,0.0,sint*sint,cost)
          pref_corr=1.0_cp
          if(Ph(iph)%Pow(n_pat)%n_pref > 0) then
            call Preferred_orientation("MAX_MD",Ph(iph)%Pow(n_pat)%n_pref, &
                      Ph(iph)%Pow(n_pat)%axes_pref, &
                      Ph(iph)%Cell%gr,Rot_Mats(iph), &
                      Ph(iph)%Pow(n_pat)%pref,RL%Ref(k),pref_corr)
          end if
          RL%corr(k,ipat)=plor1*pref_corr
          !write(*,"(i6,2(a,i3),f12.5,2i6)")k," ipat",ipat," n_pat",n_pat,RL%pos(k,ipat),RL%ptr(1:2,k,ipat)
          !write(*,"(i8,i7,5f12.5)")k,n_pat,RL%pos(k,ipat),plor,plor1,pref_corr,RL%corr(k,ipat)

          if(twowaves) then !Store the second wavelength in the second half of the array
             sint=RL%Ref(k)%s * lambda(2)
             if(sint > 1.0_cp) cycle
             cost=sqrt(abs(1.0-sint*sint))
             kn=k+Nref
             pt=2.0_cp * asind( sint )
             RL%pos(kn,ipat)=pt
             Call Get_HG_HL_CW(iph,n_pat,kn,2,HG,HL)
             pos_left =pt-fwhm*wdt
             pos_right=pt+fwhm*wdt
             RL%ptr(1,kn,ipat)=max(1,locate(xp,pos_left,npts))
             RL%ptr(2,kn,ipat)=min(npts,locate(xp,pos_right,npts))
             RL%HG(kn,ipat)=HG
             RL%HL(kn,ipat)=HL
             RL%FWHM(kn,ipat)=fwhm
             RL%ETA(kn,ipat)=eta
             !Calculation of CORR for the current reflection
             plor=Lorentz_abs_CW(sint,cost,pt,&
                                 0.0,"N","DBS","HEWAT")
             pref_corr=1.0_cp
             if(Ph(iph)%Pow(n_pat)%n_pref > 0) then
               call Preferred_orientation("MAX_MD",Ph(iph)%Pow(n_pat)%n_pref, &
                         Ph(iph)%Pow(n_pat)%axes_pref, &
                         Ph(iph)%Cell%gr,Rot_Mats(iph), &
                         Ph(iph)%Pow(n_pat)%pref,RL%Ref(kn),pref_corr)
             end if
             RL%corr(kn,ipat)=plor*pref_corr*ratio ! Apply ratio
          end if
        end do
      end associate
   End Subroutine Reflections_Contribution_CW_ini

   Subroutine Profile_Contribution_CW(iph,ipat,RL,yp)
      integer,                     intent(in)     :: iph,ipat
      type(RefP_type),             intent(in)     :: RL !It has been totally filled before
      real(kind=cp), dimension(:), intent(out)    :: yp
      !--- Local variables ---!
      integer       :: j,i1,i2,k,kn,NRef,n_pat,irad,npts
      real(kind=cp) :: pos,Bragg, fwhm, eta, asym1,asym2, scalef, corr, intens, ratio
      real(kind=cp), dimension(2) :: Lambda
      real(kind=cp), dimension(:), allocatable :: y
      logical :: twowaves

      Nref=RL%Nref
      n_pat= Ph(iph)%patterns(ipat)
      Select Type(cond => Pat(n_pat)%cond)
        Type is(PowPatt_CW_Conditions_Type)
           asym1=cond%asym1
           asym2=cond%asym2
           twowaves=cond%twowaves
           lambda=cond%Lambda
           ratio=cond%ratio
      End Select

      scalef=Ph(iph)%scale_factors(ipat)
      yp=0.0_cp

      Select Case(Pat(n_pat)%radiation)
        case("X") ; irad=1
        case("N") ; irad=2
        case("E") ; irad=3
        Case Default ; irad=2
      End Select

      npts=Pat(n_pat)%Pdat%npts

      do k=1,Nref  !Calculate the position of the reflection for each pattern and the contributing points
        i1=RL%ptr(1,k,ipat)
        if(i1 == 0) cycle !The reflection is not contributing to pattern n_pat
        i2=RL%ptr(2,k,ipat)
        Bragg=RL%pos(k,ipat)
        fwhm=RL%fwhm(k,ipat)
        eta=RL%eta(k,ipat)
        corr=RL%corr(k,ipat)
        !Calculate the profile of the reflection
        Select Type (rf => RL%Ref)
           class is (Srefl_type)
             intens = scalef * corr * rf(k)%mult * rf(k)%fc(irad)**2
        End Select
        if (asym1 > 0.000001_cp) then
            if (allocated(y)) deallocate(y)
            allocate(y(i1:i2)); y=0.0_cp
            call calc_pseudo_voigt(Pat(n_pat)%Pdat%x(i1:i2),y(i1:i2),bragg,eta,fwhm,asym1,asym2)
            yp(i1:i2) = yp(i1:i2) + y(i1:i2) * intens
        else
            do j = i1 , i2
                yp(j) = yp(j) + pseudovoigt(Pat(n_pat)%Pdat%x(j) - bragg, [fwhm,eta]) * intens
            end do
        end if
        if(twowaves) then
           kn=k+Nref
           Bragg=RL%pos(kn,ipat)
           fwhm=RL%fwhm(kn,ipat)
           eta=RL%eta(kn,ipat)
           i1=RL%ptr(1,kn,ipat)
           i2=RL%ptr(2,kn,ipat)
           corr=RL%corr(kn,ipat)
           !Calculate the profile of the reflection
           Select Type (rf => RL%Ref)
              class is (Srefl_type)
                intens = scalef * corr * rf(kn)%mult * rf(kn)%fc(irad)**2 * ratio
           End Select
           if (asym1 > 0.000001_cp) then
               if (allocated(y)) deallocate(y)
               allocate(y(i1:i2)); y=0.0_cp
               call calc_pseudo_voigt(Pat(n_pat)%Pdat%x(i1:i2),y(i1:i2),bragg,eta,fwhm,asym1,asym2)
               yp(i1:i2) = yp(i1:i2) + y(i1:i2) * intens
           else
               do j = i1 , i2
                   yp(j) = yp(j) + pseudovoigt(Pat(n_pat)%Pdat%x(j) - bragg, [fwhm,eta]) * intens
               end do
           end if
        end if
      end do

      do j=1,Pat(n_pat)%PDat%npts
        pos=Pat(n_pat)%PDat%x(j)
        do k=1,Excl(n_pat)%num_excl
           if(pos >= Excl(n_pat)%Exc(k)%mina .and. pos <= Excl(n_pat)%Exc(k)%maxb) then
             yp(j)=0.0
             Exit
           end if
        end do
      end do

   End Subroutine Profile_Contribution_CW

   Subroutine Get_HG_HL_CW(nph,npat,nrf,iwav,HG,HL)
     integer,       intent(in) :: npat,nph,nrf,iwav
     real(kind=cp), intent(out):: HG,HL

     !--- Local variables ---!
     integer       :: j
     real(kind=cp) :: sinth,costh,tanth,costh2,lambda,frac_gsz, frac_lstr,isosz,isostr, &
                      u,v,w,x,y,z,dst2,dsiz,HG2,isosize_broad, isostr_broad,sq
     real(kind=cp), dimension(3)  :: hkl
     real(kind=cp), dimension(15) :: par
     j=Pat(npat)%irf
     lambda=Pat(npat)%Pdat%wave(iwav)
     sinth=RL(nph)%Ref(nrf)%s*lambda
     sq=4.0_cp*(RL(nph)%Ref(nrf)%s)**2 ! 1/d^2
     hkl=RL(nph)%Ref(nrf)%hr
     !write(*,*) hkl
     costh=sqrt(1.0_cp-sinth*sinth)
     costh2=costh*costh
     tanth=sinth/costh
     isosz=0.0;  dsiz=0.0; isosize_broad=0.0; par=0.0
     isostr=0.0; dst2=0.0;  isostr_broad=0.0
     frac_gsz=0.0; frac_lstr=0.0
     isosz=Ph(nph)%pow(npat)%iso_size
     if(isosz > 1.0) then
       frac_gsz=Ph(nph)%pow(npat)%Gauss_iso_size_frac
       isosize_broad=180.0_cp*lambda/isosz/PI !division by costh below
     end if
     isostr_broad=Ph(nph)%pow(npat)%iso_strain
     if(isostr > 0.0) then
       frac_lstr=Ph(nph)%pow(npat)%Lorentz_iso_strain_frac
     end if
     call Calc_Anisotropic_Strain("QUARTIC_FORM","P6/mmm","CW", par, hkl, sq, dst2)
     u=0.0_cp; v=0.0_cp; w=0.0_cp; x=0.0_cp; y=0.0_cp; z=0.0_cp

     Select Type( cd => Pat(npat)%cond)
       Class is (PowPatt_CW_Conditions_Type)
          u=cd%u
          v=cd%v
          w=cd%w
          x=cd%x
          y=cd%y
     End Select

     If(j > 0 ) Then
        u= u + cw_irf(j)%u_i(iwav)
        v= v + cw_irf(j)%v_i(iwav)
        w= w + cw_irf(j)%w_i(iwav)
        x= x + cw_irf(j)%x_i(iwav)
        y= y + cw_irf(j)%y_i(iwav)
        z= z + cw_irf(j)%z_i(iwav)
     End if
        HG2=((u + (dst2 + isostr_broad) * (1.0_cp - frac_lstr)**2)  * tanth + v) *tanth + w +  (frac_gsz*(isosize_broad+dsiz)/costh)**2
        HG=sqrt(HG2)
        HL= (x+ sqrt(dst2)*frac_lstr)* tanth + (y+(dsiz+isosize_broad)*(1.0_cp - frac_gsz))/costh
   End Subroutine Get_HG_HL_CW

   !Function for determining sinTheta/Lambda min and max for all the patterns
   Function sintl_max() result(sintlmax)
     real(kind=cp) :: sintlmax
     integer       :: i
     real(kind=cp) :: xm,sintl
     sintlmax=0.0
     do i=1,N_patterns
       write(*,*) "  xmax:", Pat(i)%PDat%xmax, " wave1",Pat(i)%PDat%wave(1), Pat(i)%mode
       xm=Pat(i)%PDat%xmax*1.05
       Select Case(Pat(i)%mode)
         Case("CW")
              sintl=sind(xm*0.5_cp)/Pat(i)%PDat%wave(1)
         Case("TF")
              sintl=1.0/(2.0_cp*xm*Pat(i)%PDat%wave(1))
       End Select
       if(sintl > sintlmax) sintlmax=sintl
     end do
   End Function sintl_max

   Subroutine Write_RefList(Rf,lun)
     type(RefP_type), intent(in) :: Rf
     integer,         intent(in) :: lun
     !--- Local variables ---!
     integer :: i,n_pat,n_patts,n_refl
     character(len=10) :: fm

     n_patts=Rf%N_patt
     n_refl=Rf%nref
     fm="(a,   i3)"
     write(fm(4:6),"(i3)") n_patts
     write(lun,"(/a)")         " ================================================================================"
     write(lun,"(a,i3,a,i6)")  "  LIST of Reflections generated  for  PHASE# : ",Rf%i_ph, "  Number of reflections: ",Rf%nref
     write(lun,fm)        "  The Reflections contribute to the patterns : ",Rf%patts(1:n_patts)
     write(lun,"(/a)")    " NumRef         Hr        Kr        Lr     Mult Imag SinTh/Lambda"
     do n_pat=1,n_patts
       write(lun,"(a,i3)")"                Position   Ini   Fin   Gauss-HG    Lorentz-HL     FWHM        ETA   for pattern #",Rf%patts(n_pat)
     end do
     write(lun,"(a)")     " ======================================================================================================"


     do i=1,n_refl
       write(lun,"(i8,tr4,3f10.4,2i5,f13.5)") i, Rf%Ref(i)%hr, Rf%Ref(i)%Mult, Rf%Ref(i)%imag, Rf%Ref(i)%s
       do n_pat=1,n_patts
         if(Rf%ptr(1,i,n_pat) /= 0) then
           write(lun,"(tr14,f10.4,2i6,4f12.4)") Rf%pos(i,n_pat),Rf%ptr(1:2,i,n_pat),Rf%HG(i,n_pat),Rf%HL(i,n_pat),Rf%FWHM(i,n_pat),Rf%ETA(i,n_pat)
         else
           write(lun,"(a,i3)") "         -> Reflection non-contributing to pattern #",Rf%patts(n_pat)
         end if
       end do
     end do

   End Subroutine Write_RefList


End Module nFP_Reflections