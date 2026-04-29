!
!   !!!!! W A R N I N G !!!!!! THIS PROGRAM IS STILL NOT WORKING!
!
Module Flipping_Ratios

 use CFML_GlobalDeps
 use CFML_Math_3D,                  only: Veclength
 use CFML_crystallographic_symmetry,only: space_group_type, Write_SpaceGroup, get_stabilizer
 use CFML_Atom_TypeDef,             only: Atom_List_Type, Write_Atom_List, MAtom_list_Type, &
                                          Get_Atom_2nd_Tensor_Ctr
 use CFML_crystal_metrics,          only: Crystal_Cell_Type, Write_Crystal_Cell
 use CFML_Reflections_Utilities,    only: Hkl_s
 use CFML_IO_Formats,               only: Readn_set_Xtal_Structure,err_form_mess,err_form,file_list_type
 use CFML_Propagation_vectors,      only: K_Equiv_Minus_K
 use CFML_Magnetic_Symmetry
 use CFML_Magnetic_Structure_Factors
 use CFML_SXTAL_Geom,               only: z1frnb
 use CFML_String_Utilities,         only: U_case
 use CFML_Structure_Factors,        only: Calc_hkl_StrFactor
 use CFML_Extinction_Correction,    only: Correct_FlippingRatios
 implicit none

 integer, public :: i_flip=1

  contains


    !!---- Subroutine Calc_Induced_MsF_MiV(cell,MField,dir_MField,Mh)
    !!----    !---- Arguments ----!
    !!----    type(Crystal_Cell_type),    intent(in)     :: Cell
    !!----    real(kind=cp),              intent(in)     :: MField
    !!----    real(kind=cp),dimension(3), intent(in)     :: dir_MField
    !!----    type(MagH_Type),            intent(in out) :: Mh
    !!----
    !!----  This subroutine completes the object Mh when the tensorial magnetic structure
    !!----  factor has been previously calculated.
    !!----
    !!----  Created: June 2014 (JRC)
    !!----
    Subroutine Calc_Induced_MsF_MiV(cell,MField,dir_MField,Mh)
       !---- Arguments ----!
       type(Crystal_Cell_type),    intent(in)     :: Cell
       real(kind=cp),              intent(in)     :: MField
       real(kind=cp),dimension(3), intent(in)     :: dir_MField
       type(MagH_Type),            intent(in out) :: Mh
       !--- Local variables ---!
       real(kind=cp)                  :: s
       real(kind=cp),    dimension(3) :: u_vect,er,ed
       complex(kind=cp), dimension(3) :: Mc, MiV

       !
       u_vect = MField * dir_MField / Veclength(Cell%Cr_Orth_cel,dir_MField)
       Mh%MsF=matmul(Mh%TMsF,u_vect)  !Magnetic structure factor from TMsF tensor
       !---- Calculation of the Magnetic Interaction vector ----!
       s  = 2.0*Mh%s            !1/d=r*, M = M// + Mp   => Mp = M - M// = M - (M.e)e
       er = Mh%h/s              !unitary vector referred to the reciprocal basis
       ed = matmul(cell%GR,er)  !  "        "       "             direct    "
       Mc  = Mh%MsF / Cell%cell                !Magnetic structure factor in basis {a,b,c}
       MiV = Mc - dot_product(er,Mc) * ed      !Magnetic interaction vector in basis {a,b,c}
       Mh%MiV  =  MiV * Cell%cell              !Magnetic interaction vector in basis {e1,e2,e3}
       Mh%MiVC = matmul(Cell%Cr_Orth_cel,MiV)  !Magnetic interaction vector in Cartesian components
       Mh%sqMiV= dot_product(Mh%MiVC, Mh%MiVC)
       return
    End Subroutine Calc_Induced_MsF_MiV

    Subroutine readn_set_MsF(fliptyp)
      integer, intent(in) :: fliptyp
      real(kind=cp), dimension(3) :: h,hc
      real(kind=cp)     :: q, flipping_ratio, sigma, omeg,gam,nu,  &
                           sn, AN,BN,AM,BM,yp,ym,ypm, Valmub=0.2695 !0.5*(gamma_N * r0)
      character(len=30) :: fmm
      complex(kind=cp)  :: fc
      integer           :: i,ier,numor
       !Reading flipping ratio file

       do
          if(fliptyp == 1) then
            read(unit=i_flip,fmt="(i8,6f8.3,2f10.2)",iostat=ier)  numor,h(:), omeg, gam, nu,  flipping_ratio, sigma
          else
            read(unit=i_flip,fmt=*,iostat=ier)  h(:),flipping_ratio, sigma,i,q
          end if
          if(ier /= 0) exit
          !Calculation of q=sin^2(alpha) if needed
          if(ubgiven) then
              hc=matmul(ub,h)
              q = 1.0-hc(3)*hc(3)/dot_product(hc,hc)  !sin^2(alpha) = 1 - (h(3)/|h|)^2
          else
              if(fliptyp == 1) then
                call z1frnb(lambda,gam,omeg,nu,hc)
                q=1.0-hc(3)*hc(3)/dot_product(hc,hc)
              end if
          end if
          sn=hkl_s(h,cell)
          sn=sn*sn
          call simpleFlipr("X",iext,extc,q,polarup,polardown,h,sn,cell,A,SpG,flr,fn,fmag)
          !Now solve the equation to get Fm
          !Approximations:
          !ppp=1/2((1+p+)yp+(1-p+)ym)    ppm=1/2((1+p+)yp-(1-p+)ym)
          !mpp=1/2((1-p-)yp+(1+p-)ym)    mpm=1/2((1-p-)yp-(1+p-)ym)
          !General expression for flipping ratio
          !R=I+/I-
          !I+= (NN*+ MM*q2)ppp + 2(AN.AM + BN.BM)q.ppm + MM*ypm q(1-q)
          !I-= (NN*+ MM*q2)mpp + 2(AN.AM + BN.BM)q.mpm + MM*ypm q(1-q)
          ! Particular case for a centrosymmetric structure M*=M, N*=N, B=0 AN=N,AM=M
          !I+= (N2+ M2.q2)ppp + 2.N.M.q.ppm + M2 ypm q(1-q)
          !I-= (N2+ M2.q2)mpp + 2.N.M.q.mpm + M2 ypm q(1-q)
          ! Case of no extinction: yp=1,ym=1, ypm=1
          ! ppp=1, mpp=1, ppm=p+  mpm=-p-
          !I+= (N2+ M2.q2) + 2.N.M.q.p+ + M2 q(1-q)
          !I-= (N2+ M2.q2) - 2.N.M.q.p- + M2 q(1-q)


          !     (1+ g2.q2)ppp + 2.g.q.ppm + g2 ypm q(1-q)
          !R= ------------------------------------------------
          !     (1+ g2.q2)mpp + 2.g.q.mpm + g2 ypm q(1-q)
          !
          !
       end do
      write(unit=lun,fmt="(/,a,/)") "   H   K   L   Mult  SinTh/Lda    |Fc|       Phase        F-Real      F-Imag      Num"
      do i=1, hkl%nref
         sn=hkl%ref(i)%s * hkl%ref(i)%s
         call Calc_StrFactor("P","N",i,sn,A,Spg,sf2,fc=fc)
         write(unit=lun,fmt="(3i4,i5,5f12.5,i8,f12.5)") hkl%ref(i)%h, hkl%ref(i)%mult, &
              hkl%ref(i)%S, hkl%ref(i)%Fc, hkl%ref(i)%Phase, real(fc), aimag(fc), i, sqrt(sf2)
      end do
    End Subroutine readn_set_MsF

    Subroutine simpleFlipr(mode,iext,extc,q,polarp,polarm,hn,sn,cellp,Atm,SpGr,flr,fn,fmag)
      !  Standard calculation of flipping ratios for simple
      !  magnetic form factors.
      character(len=1),                   intent(in) :: mode
      integer,                            intent(in) :: iext
      real(kind=cp), dimension(6),        intent(in) :: extc
      real(kind=cp), dimension(3),        intent(in) :: hn
      real(kind=cp),                      intent(in) :: sn,polarp, polarm,q
      type(Crystal_Cell_Type),            intent(in) :: cellp
      type(Atom_List_Type),               intent(in) :: Atm
      type(Space_Group_Type),             intent(in) :: SpGr
      real(kind=cp),                      intent(out):: flr
      complex,                   optional,intent(out):: fn,fmag
      !-----------------------------------------------
      !   L o c a l   V a r i a b l e s
      !-----------------------------------------------
      integer :: i, j, k, n, ni, ii, irr, mni
      real(kind=cp), dimension(11)        :: xi
      real(kind=cp), dimension(Atm%natoms) :: otr, oti, frc, frs, fic, fis
      real(kind=cp), dimension(Atm%natoms) :: motr, moti, mfrc, mfrs, mfic, mfis !magnetic part
      real(kind=cp), dimension(3)          :: t,h
      real(kind=cp), dimension(3,3)        :: sm
      real(kind=cp) :: fr, fi, ffr, ffi, cosr, cosi, sinr, sini, scosr, scosi, &
                       ssinr, ssini, a1, a2, a3, a4, b1, b2, b3, b4, temp, scal
      real(kind=cp) :: av,bv, x, yy, arg, arg2, exparg
      real(kind=cp) :: mav, mbv          ! real and imaginary part of the magnetic structure factor.
      real(kind=cp) :: mfr,mfi,mffr,mffi !magnetic form factors
      real(kind=cp) :: ma1, ma2, ma3, ma4, mb1, mb2, mb3, mb4
      real(kind=cp) :: mcosr, mcosi, msinr, msini, mscosr, mscosi, &
                       mssinr, mssini
      real(kind=cp) :: MAG,NUC                ! Magnetic and Nuclear squared structure factors
      real(kind=cp) :: AAlpha, Iplus, Iminus  ! Intermediate variables used in the FR calculus.
      Logical       :: magnetic
      !--------------------------------------------------------------------------
      !   L o c a l   V a r i a b l e s  R e l a t e d  t o  E x t i n c t i o n
      !--------------------------------------------------------------------------
      real(kind=cp) :: DIpyp, DIpym, DIpypm,&  ! Intermediate variables used calculus of FR derivatives.
                       DImyp, DImym, DImypm    ! with respect to extinction parameters
      real(kind=cp) :: yp, ym, ypm, ppp, ppm, mpp, mpm  !! Extinction correction + polar-corrections
      !-----------------------------------------------
      !  SN: (sintheta/Lambda)**2
      n=Atm%natoms
      a1=0.0    ;   ma1=0.0
      a2=0.0    ;   ma2=0.0
      a3=0.0    ;   ma3=0.0
      a4=0.0    ;   ma4=0.0
      b1=0.0    ;   mb1=0.0
      b2=0.0    ;   mb2=0.0
      b3=0.0    ;   mb3=0.0
      b4=0.0    ;   mb4=0.0
      mav=0.0
      mbv=0.0

      !---------------------------
      DO i=1,n           !loop over atoms
      !---------------------------
        magnetic=.false.
        ffr= 0.0                  !
        ffi= 0.0                  !
        mfr= 0.0                  ! We start reseting all form factors
        mfi= 0.0                  !
        mffr= 0.0                 !
        mffi= 0.0                 !

        IF(ntyp(ipiof) == 'DIPO' .or. ntyp(ipiof) == 'QUAD' .or. ntyp(ipiof) == 'HXAP' .or. &
           ntyp(ipiof) == 'OCTU' .or. ntyp(ipiof) == 'DISP' .or. ntyp(ipiof) == 'MULT' ) THEN
                magnetic=.true.  !Magnetic atoms are given with special form factors
                mffr= 1.0        ! Isotropic magnetic form factor is just set to 1
                mffr= 0.0
        END IF

        xi(1:3)=Atm%atom(i)%x
        xi(4)=Atm%atom(i)%biso
        xi(5)=Atm%atom(i)%occ
        xi(6:11)=Atm%atom(i)%U(1:6)
        temp=EXP(-xi(4)*sn)   !exp{-Bi (sintheta/Lambda)^2}

        ni=Atm%atom(i)%ind(1)

        IF(ni > 0) THEN                ! Nuclear contribution
            ffi=dfpp(ni,n_pat)         ! Imaginary Fermi length
            ffr=dfp(ni,n_pat)          ! Fermi Length
        ELSE                           ! Nuclear and Magnetic contribution
            ffi=dfpp(-ni,n_pat)        ! Imaginary Fermi length
            ffr=dfp(-ni,n_pat)         ! Fermi Length
            if(ntyp(ipiof) == 'MPOL') then
               mfr= 1.0                ! Especial magnetic form factor is just set to 1
               mfi= 0.0
               mni=-ptr(ipiof,2,n_pat)
               !This is a magnetic atom with simple form factor (spin+orbital contribution): mu {<j0>+C2<j2>+C4<j4>+C6<j6>}
               !Nevertheless, magnetic is kept to be .false. in order to avoid the calculation of multipolar terms
               !in subroutine mag_ffactor called below if magnetic=.true. .
               CALL mpol_ffactor(n_pat,mni,ipiof,sn,mffr)
            end if
        END IF

        ! Loop over symmetry operators
        ! Set indices for applying selected symmetry operators
        ! Identity is always applied and must be the first sym.op.

        scosr=0.0 ; mscosr=0.0        !
        scosi=0.0 ; mscosi=0.0        !  We just reset all the needed quantities.
        ssinr=0.0 ; mssinr=0.0        !
        ssini=0.0 ; mssini=0.0        !

        !+++++++++++++++++++++++++
        DO  irr=1,irl   !Loop over symmetry operators
        !+++++++++++++++++++++++++
          sm(:,:)=Spgr(iph)%Symop(irr)%Rot(:,:)
             t(:)=SpGr(iph)%Symop(irr)%tr(:)
          x=0.0
          DO  ii=1,3
            x=t(ii)*hn(ii)+x
            yy=0.0
            DO  j=1,3
              yy= hn(j)*sm(j,ii)+yy
            END DO
            h(ii)=yy
          END DO
          arg=x
          DO j=1,3
            arg=h(j)*xi(j)+arg
          END DO
          arg=tpi*arg
          arg2=h(1)*h(1)*xi(13)+h(2)*h(2)*xi(14)+ h(3)*h(3)*xi(15)+        &
               2.0*h(1)*h(2)*xi(16)+ 2.0*h(1)*h(3)*xi(17)+2.0*h(2)*h(3)*xi(18)
          exparg=EXP(-arg2)
          fr=1.0
          fi=0.0
          !Nuclear contribution if read_strf == 0
          cosr=COS(arg)*exparg*fr   !fr*cos{2pi(hT Rs rj+ts)}*exp(-{hTRsBetaj RsTh})
          cosi=COS(arg)*exparg*fi   !fi*cos{2pi(hT Rs rj+ts)}*exp(-{hTRsBetaj RsTh})
          sinr=SIN(arg)*exparg*fr   !fr*sin{2pi(hT Rs rj+ts)}*exp(-{hTRsBetaj RsTh})
          sini=SIN(arg)*exparg*fi   !fi*sin{2pi(hT Rs rj+ts)}*exp(-{hTRsBetaj RsTh})

          scosr=scosr+cosr          !FRC= SIG fr(j,s)cos{2pi(hT Rs rj+ts)}*Ta(s)
          scosi=scosi+cosi          !FIC= SIG fi(j,s)cos{2pi(hT Rs rj+ts)}*Ta(s)
          if(icent == 1) then
           ssinr=ssinr+sinr          !FRS= SIG fr(j,s)sin{2pi(hT Rs rj+ts)}*Ta(s)
           ssini=ssini+sini          !FIS= SIG fi(j,s)sin{2pi(hT Rs rj+ts)}*Ta(s)
          end if

          !Magnetic contribution

          mcosr=COS(arg)*exparg*mfr   !fr*cos{2pi(hT Rs rj+ts)}*exp(-{hTRsBetaj RsTh})
          mcosi=COS(arg)*exparg*mfi   !fi*cos{2pi(hT Rs rj+ts)}*exp(-{hTRsBetaj RsTh})
          msinr=SIN(arg)*exparg*mfr   !fr*sin{2pi(hT Rs rj+ts)}*exp(-{hTRsBetaj RsTh})
          msini=SIN(arg)*exparg*mfi   !fi*sin{2pi(hT Rs rj+ts)}*exp(-{hTRsBetaj RsTh})

          mscosr=mscosr+mcosr          !FRC= SIG fr(j,s)cos{2pi(hT Rs rj+ts)}*Ta(s)
          mscosi=mscosi+mcosi          !FIC= SIG fi(j,s)cos{2pi(hT Rs rj+ts)}*Ta(s)

          if(icent == 1) then
           mssinr=mssinr+msinr          !FRS= SIG fr(j,s)sin{2pi(hT Rs rj+ts)}*Ta(s)
           mssini=mssini+msini          !FIS= SIG fi(j,s)sin{2pi(hT Rs rj+ts)}*Ta(s)
          end if

        !+++++++++++++++++++++++++
        END DO     !over symmetry operators  !  END LOOP SYMM.OP.
        !+++++++++++++++++++++++++

        frc(i)=scosr    !Components of geometrical struture factor of atom i
        fis(i)=ssini    !all components contribute to real(kind=cp) and imaginary parts
        frs(i)=ssinr    !in the most general case.
        fic(i)=scosi

        otr(i)=ffr*xi(5)*temp     ! (f0+Deltaf')*OCC*Tiso
        oti(i)=ffi*xi(5)*temp     !     Deltaf" *OCC*Tiso

        !-----CALCULATE A AND B OF F
        a1 = a1 + otr(i)*frc(i)    ! components of A  and B
        a4 = a4 + oti(i)*fic(i)    ! A(h) = a1 - a2 - a3 - a4
        b1 = b1 + oti(i)*frc(i)    ! B(h) = b1 - b2 + b3 + b4
        b4 = b4 + otr(i)*fic(i)    !

        if(icent == 1) then
          a2 = a2 + otr(i)*fis(i)
          a3 = a3 + oti(i)*frs(i)
          b2 = b2 + oti(i)*fis(i)
          b3 = b3 + otr(i)*frs(i)
        end if

        mfrc(i)=mscosr    !Components of geometrical struture factor of atom i
        mfis(i)=mssini    !all components contribute to real(kind=cp) and imaginary parts
        mfrs(i)=mssinr    !in the most general case.
        mfic(i)=mscosi

        motr(i)=mffr*xi(5)*temp     ! mu*fr(mag)*OCC*Tiso
        moti(i)=mffi*xi(5)*temp     ! mu*fi(mag) *OCC*Tiso


        !-----CALCULATE Am AND Bm OF Fm
        ma1 = ma1 + motr(i)*mfrc(i)    ! components of A  and B
        ma4 = ma4 + moti(i)*mfic(i)    ! Am(h) = ma1 - ma2 - ma3 - ma4
        mb1 = mb1 + moti(i)*mfrc(i)    ! Bm(h) = mb1 - mb2 + mb3 + mb4
        mb4 = mb4 + motr(i)*mfic(i)    !

        if(icent == 1) then
          ma2 = ma2 + motr(i)*mfis(i)
          ma3 = ma3 + moti(i)*mfrs(i)
          mb2 = mb2 + moti(i)*mfis(i)
          mb3 = mb3 + motr(i)*mfrs(i)
        end if

      !---------------------------
      END DO  !over atoms
      !---------------------------

        av=icent*(a1-a2-a3-a4)        !real part of the nuclear structure factor
        bv=icent*(b1-b2+b3+b4)        !imaginary part of the nuclear structure factor
        mav=icent*(ma1-ma2-ma3-ma4)   !real part of the magnetic structure factor
        mbv=icent*(mb1-mb2+mb3+mb4)   !imaginary part of the magnetic structure factor

        mav= scal*mav
        mbv= scal*mbv

        q= gam(nn,n_pat)
        AAlpha= q * ( av*mav + bv*mbv )
        MAG= mav * mav + mbv * mbv
        NUC=  av *  av +  bv *  bv

        ! Extinction correction

        if (iext /= 0) then
           call Correct_FlippingRatios(iext,Lambda,q,extc,sn,hn,Av,Bv,mav,mbv,yp,ym,ypm)

           ppp=((1.0+polarp)*yp+(1.0-polarp)*ym)*0.5 !+Pp
           ppm=((1.0+polarp)*yp-(1.0-polarp)*ym)*0.5 !+Pm
           mpp=((1.0-polarm)*yp+(1.0+polarm)*ym)*0.5 !-Pp
           mpm=((1.0-polarm)*yp-(1.0+polarm)*ym)*0.5 !-Pm

           Iplus=  (NUC+q*q*MAG)*ppp  + 2.0*q*(av*mav+ bv*mbv)*ppm  +(1.0-q)*q*MAG*ypm
           Iminus= (NUC+q*q*MAG)*mpp  + 2.0*q*(av*mav+ bv*mbv)*mpm  +(1.0-q)*q*MAG*ypm

        else

           Iplus=  NUC + 2.0*polarp* AAlpha+ q*MAG
           Iminus= NUC - 2.0*polarm* AAlpha+ q*MAG

        end if

        ! The following lines are valid if input data are flipping ratios
        ! Flipping ratio is calculated and stored in flr
        flr=Iplus/Iminus
       !-----Calculate the phase of the magnetic structure factor in radians
       if(jfou(n_pat) /= 0) then
        phasen(nn,n_pat)=0.0
        if(icent == 1) then
          if (abs(mag) < 0.00001) then
            arg=0.0
          else
            arg= mbv/sqrt(mag)
          end if
          if(abs(arg) > 1.0) arg=sign(1.0_cp,arg)
          if (abs(mag) >= 0.000001) phasen(nn,n_pat)=asin(arg)
          if (mav < 0.0) phasen(nn,n_pat)=pi-phasen(nn,n_pat)
          if (phasen(nn,n_pat) < 0.0) phasen(nn,n_pat)=phasen(nn,n_pat)+tpi
        else
          if(mav < 0.0) phasen(nn,n_pat) = pi
        end if
       end if

    End Subroutine simpleFlipr

    Subroutine Set_Form_Factors(Atm,Nuc_species,Mag_species,fermi_lengths,mcoeff,ok,mess,lun)
      use CFML_Scattering_Chemical_Tables
      type(Atom_List_Type),                      intent(in out):: Atm
      integer,                                   intent(out)   :: Nuc_species, Mag_species
      real(kind=cp), dimension(:),  allocatable, intent(out)   :: fermi_lengths
      real(kind=cp), dimension(:,:),allocatable, intent(out)   :: mcoeff
      logical,                                   intent(out)   :: ok
      character(len=*),                          intent(out)   :: mess
      integer,                      optional,    intent(in)    :: lun
      !---- Local variables ----!
      character(len=4)                        :: symbcar
      integer                                 :: i,k,n,m
      character(len=4), dimension(atm%natoms) :: symb
      real(kind=cp),    dimension(atm%natoms) :: bs
      real(kind=cp)                           :: b
      call set_chem_info()
       !---- Getting Fermi Lengths of atoms ----!
       symb="    "
       bs=0.0
       n=0
       ok=.true.
       do i=1,atm%natoms
          symbcar=u_case(atm%atom(i)%chemsymb)
          call Get_Fermi_Length(symbcar,b)
          if (abs(b) < 0.0001) then
             ok=.false.
             Mess="The Fermi Length of Species "//symbcar//" was not found"
             return
          else
             if(any(symb == symbcar)) cycle
             n=n+1
             symb(n)=symbcar
             bs(n) = b
          end if
       end do
       if(allocated(fermi_lengths)) deallocate(fermi_lengths)
       allocate(fermi_lengths(n))
       Nuc_species=n
       fermi_lengths=bs(1:n)
       m=0
       do i=1,atm%natoms
         symbcar=u_case(atm%atom(i)%chemsymb)
         if(u_case(trim(atm%atom(i)%SfacSymb)) == "MPOL") m=m+1
         do j=1,Nuc_species
           if(symbcar == symb(j)) then
             atm%atom(i)%ind(1)=j
             exit
           end if
         end do
       end do
       Mag_species=m

       !---- Printing Information ----!
       if (present(lun)) then
          write(unit=lun,fmt="(/,a)")  "  INFORMATION FROM TABULATED NEUTRON SCATTERING FACTORS"
          write(unit=lun,fmt="(a,/)")  "  ==================================================="
          write(unit=lun,fmt="(a)")    "  FERMI LENGTHS "
          write(unit=lun,fmt="(a,i3)") "   Number of chemically different species: ",n
          write(unit=lun,fmt="(/,a)")  "   Atom     Fermi Length [10^(-12) cm]"
          do k=1,n
             write(unit=lun,fmt="(a,F15.6)")  "     "//symb(k), bs(k)
          end do
          write(unit=lun,fmt="(/,/)")
       end if
       call Remove_chem_info()

    End Subroutine Set_Form_Factors

    Subroutine Mpol_Ffactor(coeff,ac,sn,mffr)
       real(kind=cp), dimension(4), intent(in) :: coeff
       real(kind=cp), dimension(:), intent(in) :: ac
       real(kind=cp), intent(in) :: sn
       real(kind=cp), intent(out):: mffr
       ! Local variables
       integer :: i
       real(kind=cp)    :: j0,j2,j4,j6
       j0=ac(7); j2=ac(14); j4=ac(21); j6=ac(28)
       do i=1,5,2
         j0=j0+ac(i)*EXP(-ac(i+1)*sn)      !<j0>      value for Q=H+k
         j2=j2+ac(i+7)*EXP(-ac(i+8)*sn)    !<j2>/sn value for Q=H+k
         j4=j4+ac(i+14)*EXP(-ac(i+15)*sn)  !<j4>/sn value for Q=H+k
         j6=j6+ac(i+21)*EXP(-ac(i+22)*sn)  !<j6>/sn value for Q=H+k
       end do
       j2=j2*sn  !<j2> value for Q=H+k
       j4=j4*sn  !<j4> value for Q=H+k
       j6=j6*sn  !<j6> value for Q=H+k
       mff= pn*coeff(1)*j0 + coeff(2)*j2 + coeff(3)*j4 + coeff(4)*j6
       return
    End Subroutine Mpol_Ffactor

End Module Flipping_Ratios

Program MagRef
 use CFML_GlobalDeps
 use CFML_Math_3D,                  only: Veclength
 use CFML_crystallographic_symmetry,only: space_group_type, Write_SpaceGroup, get_stabilizer
 use CFML_Atom_TypeDef,             only: Atom_List_Type, Write_Atom_List, MAtom_list_Type, &
                                          Get_Atom_2nd_Tensor_Ctr
 use CFML_crystal_metrics,          only: Crystal_Cell_Type, Write_Crystal_Cell
 use CFML_Reflections_Utilities,    only: Hkl_s
 use CFML_IO_Formats,               only: Readn_set_Xtal_Structure,err_form_mess,err_form,file_list_type
 use CFML_Propagation_vectors,      only: K_Equiv_Minus_K
 use CFML_Magnetic_Symmetry
 use CFML_Magnetic_Structure_Factors
 use Flipping_Ratios

 implicit none

 type (file_list_type)       :: fich_cfl
 type (Space_Group_Type)     :: SpG
 type (MagSymm_k_Type)       :: MGp
 type ( Atom_list_Type)      :: A
 type (MAtom_list_Type)      :: Am
 type (Crystal_Cell_Type)    :: Cell
 type (MagH_Type)            :: Mh

 character(len=256)           :: line,filcod,fileflip,mess
 character(len=1)             :: sig
 real(kind=cp)                :: sn,sf2,lambda,polarup,polardown
 real(kind=cp), dimension(3)  :: u_vect
 integer                      :: Num, lun=1, ier,i,j,m,ih,ik,il,iv, n_ini, n_end, fliptyp
 complex(kind=cp)             :: fc
 real(kind=cp), dimension(6)  :: extc
 real(kind=cp), dimension(3,3):: ub

 integer                     :: narg,iext,ity
 Logical                     :: esta, arggiven=.false.,flipp=.false.,ext=.false.,ubgiven=.false.,ok
 integer                     :: Nuc_species, Mag_species
 real(kind=cp), dimension(:,:),allocatable :: mcoeff !(28,Mag_species)
 real(kind=cp), dimension(:),  allocatable :: fermi_lengths !(Nuc_species)
 real(kind=cp), dimension(:),  allocatable :: fermi_lengths !(Nuc_species)

      !---- Arguments on the command line ----!
      narg=COMMAND_ARGUMENT_COUNT()

      if(narg > 0) then
              call GET_COMMAND_ARGUMENT(1,filcod)
              arggiven=.true.
      end if

      write(unit=*,fmt="(/,/,7(a,/))")                                                 &
           "              ------ P r o g r a m     M a g R e f  ------"               , &
           "                    ---- Version 0.3 June-2014 ----"                    , &
           "    ******************************************************************"  , &
           "    * Calculates magnetic structure factors, and magnetic interaction*"  , &
           "    * vectors from magnetic structures by reading a *.CFL file       *"  , &
           "    * Calculates also Induced Magnetic Structure Factor Tensors      *"  , &
           "    ******************************************************************"  , &
           "                            (JRC- June 2014)"
    write(unit=*,fmt=*) " "

     if(.not. arggiven) then
       write(unit=*,fmt="(a)",advance="no") " => Code of the file xx.cfl (give xx): "
       read(unit=*,fmt="(a)") filcod
       if(len_trim(filcod) == 0) stop
     end if
     i=index(filcod,".",back=.true.)
     if( i /= 0) filcod=filcod(1:i-1)

     open(unit=lun,file=trim(filcod)//".cal", status="replace",action="write")
     write(unit=lun,fmt="(/,/,7(a,/))")                                                &
           "              ------ P r o g r a m     M a g R e f  ------"               , &
           "                    ---- Version 0.3 June-2014 ----"                    , &
           "    ******************************************************************"  , &
           "    * Calculates magnetic structure factors, and magnetic interaction*"  , &
           "    * vectors from magnetic structures by reading a *.CFL file       *"  , &
           "    * Calculates also Induced Magnetic Structure Factor Tensors      *"  , &
           "    ******************************************************************"  , &
           "                            (JRC- June 2014)"

     inquire(file=trim(filcod)//".cfl",exist=esta)
     if( .not. esta) then
       write(unit=*,fmt="(a)") " File: "//trim(filcod)//".cfl does'nt exist!"
       stop
     end if
     call Readn_set_Xtal_Structure(trim(filcod)//".cfl",Cell,SpG,A,Mode="CFL",file_list=fich_cfl)

     If(err_form) then
       write(unit=*,fmt="(a)") trim(err_form_mess)
     else

     !Test of the type "file_list_type" by writing at the end of the file
     write(unit=lun,fmt="(/,a)") "    =========================="
     write(unit=lun,fmt="( a )") "    Text of the input CFL file"
     write(unit=lun,fmt="(a,/)") "    =========================="
     write(unit=*,fmt="(/,a)")   "    =========================="
     write(unit=*,fmt="( a )")   "    Text of the input CFL file"
     write(unit=*,fmt="(a,/)")   "    =========================="
     do i=1,fich_cfl%nlines
       write(unit=lun,fmt="(a,i5,a)") " Line:",i,"  "//fich_cfl%line(i)
       write(unit=*,fmt="(a,i5,a)") " Line:",i,"  "//fich_cfl%line(i)
     end do


       call Write_Crystal_Cell(Cell,lun)
       call Write_SpaceGroup(SpG,lun,full=.true.)
       call Write_Atom_List(A,lun=lun)
       !Search for flipping ratio files in order to extract phased observed magnetic structure factors
       lambda=0.8
       fliptyp=1
       polarup=1.0 ; polardown=1.0
       do i=1,fich_cfl%nlines
         line=adjustl(fich_cfl%line(i))
         if(U_Case(line(1:4)) == "FLIP") then
           fileflip=adjustl(line(5:))
           flipp=.true.
         end if
         if(U_Case(line(1:6)) == "LAMBDA") then
           read(unit=line(7:),fmt=*,iostat=ier) lambda
           if(ier /= 0) lambda=0.8
         end if
         if(U_Case(line(1:7)) == "POLARUD") then
           read(unit=line(8:),fmt=*,iostat=ier) polarup,polardown
           if(ier /= 0) then
             polarup=1.0 ; polardown=1.0
           end if
         end if
         if(U_Case(line(1:4)) == "EXTI") then
           read(unit=line(5:),fmt=*,iostat=ier) extc, iext
           if(ier /= 0) then
              read(unit=line(5:),fmt=*,iostat=ier) extc(1)
              if(ier /= 0) extc(:) = 0.0
              iext=1
           end if
           if(any(extc > 0.0001)) ext=.true.
         end if
         if(flipp .and. ext) exit
       end do

       !If flipp is true, extract only observed magnetic structure factors
       if(flipp) then
         if(SpG%centred /= 2) then
           write(unit=*,fmt="(a)") " => The given space group has no inversion centre at the origin!"
           write(unit=*,fmt="(a)") " => Phased magnetic structure factors cannot be obtained!"
           stop
         end if
         !  Set nuclear and magnetic scattering factors coefficients for all the atoms
         !  in the structure
         Call Set_Form_Factors(A,Nuc_species,Mag_species,fermi_lengths,mcoeff,ok,mess)

         open(newunit=i_flip,file=fileflip,status="old",action="read",iostat=ier,position="rewind")
         if(ier/=0) then
           write(unit=*,fmt="(a)") " => File: "//trim(fileflip)//" not found!"
           stop
         end if
         !Detect the type of file *.int or other
         if(index(fileflip,".int") /= 0) fliptyp=2
         if(fliptyp == 2) then

           read(unit=i_flip,fmt="(a)") line
           read(unit=i_flip,fmt="(a)") line
           read(unit=i_flip,fmt="(a)") line
           read(unit=line,fmt=*) lambda,ity, i, polarup,polardown,j
           if(ity /= 2) then
             write(unit=*,fmt="(a)") " => The file: "//trim(fileflip)//" doesn't contain flipping ratios!"
             stop
           end if
           if(j /= 0) then
             read(unit=i_flip,fmt=*,iostat=ier) ub(1,:)
             read(unit=i_flip,fmt=*,iostat=ier) ub(2,:)
             read(unit=i_flip,fmt=*,iostat=ier) ub(3,:)
             if(ier == 0) ubgiven=.true.
           end if
           !Now the position for reading is the good one for getting hkl,flip and sflip
         end if
         call readn_set_MsF(fliptyp)
         stop
       end if

       n_ini=1
       n_end=fich_cfl%nlines
       call Readn_Set_Magnetic_Structure(fich_cfl,n_ini,n_end,MGp,Am)
       if(err_MagSym) then
         write(unit=*,fmt="(a)") "   "//err_MagSym_Mess
         stop
       end if
       if(Am%suscept) then
          call Write_Magnetic_Structure(lun,MGp,Am,cell=Cell)
       else
          call Write_Magnetic_Structure(lun,MGp,Am)
       end if

      do
         if(Am%suscept) then
           write(unit=*,fmt="(a)",advance="no") &
           " => Enter a reflections as 3 integers -> (h,k,l) : "
           read(unit=*,fmt=*) ih,ik,il
           m=1
         else
           write(unit=*,fmt="(a)",advance="no") &
           " => Enter a magnetic reflection as 4 integers -> (h,k,l,m)=H+sign(m)*k(abs(m)): "
           read(unit=*,fmt=*) ih,ik,il,m
         end if
         if( m == 0 .or. abs(ih)+abs(ik)+abs(il) == 0 ) exit
         !construct partially the object Mh
         j=sign(1,m)
         sig="+"
         if( j == -1) sig="-"
         Mh%signp=real(-j)  ! sign "+" for H-k and "-" for H+k
         iv=abs(m)
         Mh%num_k=iv
         Mh%h= real((/ih,ik,il/)) - Mh%signp*MGp%kvec(:,iv)
         Mh%s = hkl_s(Mh%h,Cell)
         Mh%keqv_minus=K_Equiv_Minus_K(MGp%kvec(:,iv),MGp%latt)

         if(Am%suscept) then
            !write(unit=*,fmt="(a,i2,a)",advance="no") &
            !" => Enter the strength(in Tesla) and direction of applied magnetic field: "
            !read(unit=*,fmt=*) Am%MagField, Am%dir_MField
            call Calc_Induced_Sk(cell,SpG,Am%MagField,Am%dir_MField,Am,6)
            call Calc_Magnetic_Strf_Tensor(SpG,Am,Mh)
            write(unit=lun,fmt="(/,a,3i4,a)")  "  Reflection: (",ih,ik,il,") "
            write(unit=*,  fmt="(/,a,3i4,a)")  "  Reflection: (",ih,ik,il,") "
            write(unit=lun,fmt="(a)")          "  Real part of Tensorial Magnetic Structure Factor: "
            write(unit=*,  fmt="(a)")          "  Real part of Tensorial Magnetic Structure Factor: "
            do i=1,3
              write(unit=lun,fmt="(a,3f12.5)") "       ",real(Mh%TMsF(i,:))
              write(unit=*  ,fmt="(a,3f12.5)") "       ",real(Mh%TMsF(i,:))
            end do
            write(unit=lun,fmt="(a)")          "  Imaginary part of Tensorial Magnetic Structure Factor: "
            write(unit=*  ,fmt="(a)")          "  Imaginary part of Tensorial Magnetic Structure Factor: "
            do i=1,3
              write(unit=lun,fmt="(a,3f12.5)") "       ",aimag(Mh%TMsF(i,:))
              write(unit=*  ,fmt="(a,3f12.5)") "       ",aimag(Mh%TMsF(i,:))
            end do
            call Calc_Induced_MsF_MiV(cell,Am%MagField,Am%dir_MField,Mh)
            write(unit=lun,fmt="(a,2(3f8.4,a))") "  Magnetic Structure   Factor : (",real(Mh%MsF),")+i(",aimag(Mh%MsF),") "
            write(unit=lun,fmt="(a,2(3f8.4,a))") "  Magnetic Interaction Vector : (",real(Mh%MiV),")+i(",aimag(Mh%MiV),") "
            write(unit=*,  fmt="(a,2(3f8.4,a))") "  Magnetic Structure   Factor : (",real(Mh%MsF),")+i(",aimag(Mh%MsF),") "
            write(unit=*,  fmt="(a,2(3f8.4,a))") "  Magnetic Interaction Vector : (",real(Mh%MiV),")+i(",aimag(Mh%MiV),") "
            write(unit=lun,fmt="(a,f12.5 )")     "  Square of Mag. int.  Vector : ",Mh%sqMiV
            write(unit=*  ,fmt="(a,f12.5 )")     "  Square of Mag. int.  Vector : ",Mh%sqMiV

         else

            !Calculate magnetic structure factor and magnetic interaction vector
            call Calc_Magnetic_StrF_MiV(Cell,MGp,Am,Mh)
            write(unit=lun,fmt="(/,a,3i4,a,3f8.4,a)") "  Reflection: (",ih,ik,il,") "//sig//" (",MGp%kvec(:,iv),")"
            write(unit=lun,fmt="(a,3f8.4,a)")         "              (",Mh%h,")"
            write(unit=*,  fmt="(/,a,3i4,a,3f8.4,a)") "  Reflection: (",ih,ik,il,") "//sig//" (",MGp%kvec(:,iv),")"
            write(unit=*,  fmt="(a,3f8.4,a)")         "              (",Mh%h,")"
            write(unit=lun,fmt="(a,2(3f8.4,a))") "  Magnetic Structure   Factor : (",real(Mh%MsF),")+i(",aimag(Mh%MsF),") "
            write(unit=lun,fmt="(a,2(3f8.4,a))") "  Magnetic Interaction Vector : (",real(Mh%MiV),")+i(",aimag(Mh%MiV),") "
            write(unit=*,  fmt="(a,2(3f8.4,a))") "  Magnetic Structure   Factor : (",real(Mh%MsF),")+i(",aimag(Mh%MsF),") "
            write(unit=*,  fmt="(a,2(3f8.4,a))") "  Magnetic Interaction Vector : (",real(Mh%MiV),")+i(",aimag(Mh%MiV),") "
            write(unit=lun,fmt="(a,f12.5 )")     "  Square of Mag. int.  Vector : ",Mh%sqMiV
            write(unit=*  ,fmt="(a,f12.5 )")     "  Square of Mag. int.  Vector : ",Mh%sqMiV
         end if
       end do


       write(unit=*,fmt="(a)") " Normal End of program: MagRef "
       write(unit=*,fmt="(a)") " Results in File: "//trim(filcod)//".cal"
     end if

     close(unit=lun)


End Program MagRef

