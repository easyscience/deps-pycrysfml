 Module Cw_Diffraction_Pv
   use CFML_GlobalDeps, only: cp
   use CFML_Optimization_LSQ, only: Max_Free_par, LSQ_State_Vector_type, LSQ_Conditions_type, LSQ_Data_Type
   Use CFML_Profiles, only: init_prof_val, prof_val
   implicit none
   private

   !Public subroutines
   public:: Sum_PV_peaks, set_nampar,CWL_sub_profile, powder_patt_der, powder_patt_nder, powder_patt_odr

   !Global public variables
   integer,      public, parameter         :: nbac=100    !maximum number of background parameters
   integer,      public, parameter         :: ngl =9      !maximum number of global parameters
   integer,      public, parameter         :: npeaks=(Max_Free_par-(nbac+ngl))/4
   integer,      public, parameter         :: npe_sub=15
   real(kind=cp),public, parameter         :: rad=57.29577951
   real(kind=cp),private,dimension(npeaks) :: Intens,ri2,dt2,t2
   real(kind=cp),public ,dimension(npeaks) :: fwhm1,fwhm2,eta1,eta2
   real(kind=cp),public ,dimension(npeaks) :: der_u,der_v,der_w,der_z,der_x
   real(kind=cp),public ,dimension(npeaks) :: der_u2,der_v2,der_w2,der_z2,der_x2
   real(kind=cp),public, dimension(nbac)   :: bac_pos,bac_int
   integer,public                          :: inter,itype,jobtyp,icont,npeakx
   integer,public                          :: n_ba     ! number of points to define the background
   real(kind=cp),public                    :: rla1,rla2,rla,ratio
   character (len=132),public              :: title
   character (len=80),public               :: filecode,filedat !codes of input data files
   real(kind=cp),public                    :: hg, hl, h, eta, chi2, chiold=1.0e30
   integer, private                        :: jstart
   logical, public                         :: use_asymm=.false., use_hps=.false.


   Type(LSQ_State_Vector_type),    public :: vs  !State vector containing pv, code, vs%nampar,etc..
   Type(LSQ_Conditions_type ),save,public :: c   !conditions of the algorithm
   Type(LSQ_Data_Type),            public :: d   !Data to be refined (set in the main program)




  contains


   subroutine powder_patt_odr(n,m,np,nq,ldn,ldm,ldnp,beta,xplusd,ifixb,  &
                              ifixx,ldifx,ideval,f,fjacb,fjacd,istop)
     Integer,                              Intent(In)    :: n       !Number of observations
     Integer,                              Intent(In)    :: m       !Number of colums of array X+D (n,m)
     Integer,                              Intent(In)    :: np      !Number of function parameters
     Integer,                              Intent(In)    :: nq      !Number of responses per observation
     Integer,                              Intent(In)    :: ldn     !Leading dimension >= n
     Integer,                              Intent(In)    :: ldm     !Leading dimension >= m
     Integer,                              Intent(In)    :: ldnp    !Leading dimension >= np
     Real(Kind=cp),Dimension(np),          Intent(In)    :: beta    !Current values of parameters
     Real(Kind=cp),Dimension(ldn,m),       Intent(In)    :: xplusd  !Current Values of the explanatory variables X+D
     Integer,      Dimension(np),          Intent(In)    :: ifixb   !0 fixed, 1 varied
     Integer,                              Intent(In)    :: ldifx   !leading dimension of array ifixx
     Integer,      Dimension(ldifx,m),     Intent(In)    :: ifixx   !=0
     Integer,                              Intent(In)    :: ideval
     Real(Kind=cp),Dimension(ldn,nq),      Intent(In Out):: f
     Real(Kind=cp),Dimension(ldn,ldnp,nq), Intent(Out)   :: fjacb
     Real(Kind=cp),Dimension(ldn,ldm,nq),  Intent(Out)   :: fjacd
     Integer,                              Intent(In Out):: istop

     !Local variables
     integer                     :: i,j,no
     real(kind=cp)               :: xval,yval,resid
     type(LSQ_State_Vector_type) :: lvs
     Real(Kind=cp),Dimension(np) :: der



     fjacd=0.0
     lvs=vs                   !Set the local state vector
     do i=1,lvs%np
       if(ifixb(i) == 0) cycle
       lvs%pv(i)=beta(i)      !Update the state vector with the input free parameters
     end do

     if(mod(ideval,10) >= 1) then
          chi2=0.0
          do i=1,n
            call Sum_PV_Peaks(i,d%x(i),d%yc(i),lvs)
            f(i,1)= d%yc(i)
            resid= (d%y(i)-d%yc(i))/d%sw(i)
            chi2=chi2+resid*resid
          end do
          chi2=chi2/real(n-np)
          if(chi2 <= chiold) then
            c%nfev=c%nfev+1
            chiold=chi2
          end if
     end if

     if(mod(ideval/10,10) >= 1) then
          do i=1,n
            call Sum_PV_Peaks(i,d%x(i),d%yc(i),lvs,.true.)
            no=0
            do j=1,lvs%np
               fjacb(i,j,1)=lvs%dpv(j)
            end do
          end do
          c%njev=c%njev+1
     end if

   End subroutine powder_patt_odr

   subroutine powder_patt_der(m,n,x,fvec,fjac,iflag)
     Integer,                      Intent(In)    :: m, n
     Real(Kind=cp),Dimension(:),   Intent(In)    :: x
     Real(Kind=cp),Dimension(:),   Intent(In Out):: fvec
     Real(Kind=cp),Dimension(:,:), Intent(Out)   :: fjac
     Integer,                      Intent(In Out):: iflag

     !Local variables
     integer                     :: i,j,no
     real(kind=cp)               :: xval,yval
     type(LSQ_State_Vector_type) :: lvs
     Real(Kind=cp),Dimension(n)  :: der
     lvs=vs                 !Set Final value of Marquardt F-Lambdathe local state vector
     no=0
     do i=1,lvs%np
       if(lvs%code(i) == 0) cycle
       no=no+1
       lvs%pv(i)=x(no)      !Update the state vector with the input free parameters
     end do

     Select Case (iflag)

        case(1)
          chi2=0.0
          do i=1,m
            call Sum_PV_Peaks(i,d%x(i),d%yc(i),lvs)
            fvec(i)= (d%y(i)-d%yc(i))/d%sw(i)
            chi2=chi2+fvec(i)*fvec(i)
          end do
          chi2=chi2/real(m-n)
          if(chi2 <= chiold) then
            c%nfev=c%nfev+1
            !write(unit=*,fmt="(a,i6,a,F14.6)") " => Iteration number: ",c%nfev, "      Chi2=",chi2
            chiold=chi2
          end if

        case(2)

          do i=1,m
            call Sum_PV_Peaks(i,d%x(i),d%yc(i),lvs,.true.)
            no=0
            do j=1,lvs%np
               if(lvs%code(j) == 0) cycle
               no=no+1
               der(no)=lvs%dpv(j)       !Update the state vector with the input free parameters
            end do
            fjac(i,1:n)= der(1:n) * (-1.0/d%sw(i))
          end do
          c%njev=c%njev+1
     End Select

   End subroutine powder_patt_der

   subroutine powder_patt_nder(m,n,x,fvec,iflag)
     Integer,                   Intent(In)    :: m, n
     Real(Kind=cp),Dimension(:),Intent(In)    :: x
     Real(Kind=cp),Dimension(:),Intent(In Out):: fvec
     Integer,                   Intent(In Out):: iflag

     !Local variables
     integer                     :: i,j,no
     real(kind=cp)                        :: xval,yval
     type(LSQ_State_Vector_type) :: lvs

     lvs=vs                 !Set the local state vector
     no=0
     do i=1,lvs%np
       if(lvs%code(i) == 0) cycle
       no=no+1
       lvs%pv(i)=x(no)      !Update the state vector with the input free parameters
     end do
     do i=1,m
       call Sum_PV_Peaks(i,d%x(i),d%yc(i),lvs)
       fvec(i)= (d%y(i)-d%yc(i))/d%sw(i)
     end do
     c%nfev=c%nfev+1
   End Subroutine powder_patt_nder

   Subroutine set_nampar(n_ba,npeak,vs)
     integer,                    intent(in)    :: n_ba,npeak
     type(LSQ_State_Vector_type),intent(in out):: vs
     integer :: i,j
     ! Subroutine setting the names of all possible parameters in the model
     vs%nampar(:)= " "
     vs%nampar(1)="Ka2/Ka1-ratio"
     vs%nampar(2)="Asym-1(S_L)"
     vs%nampar(3)="Asym-2(D_L)"
     vs%nampar(4)="U-Caglioti"
     vs%nampar(5)="V-Caglioti"
     vs%nampar(6)="W-Caglioti"
     vs%nampar(7)="Z-parameter"
     vs%nampar(8)="Eta0-PV"
     vs%nampar(9)="X-PV"
     if(n_ba <= 9) then
       do j=1,n_ba
         write(unit=vs%nampar(9+j),fmt="(a,i1)")   "background_",j
       end do
     else
       do j=1,9
         write(unit=vs%nampar(9+j),fmt="(a,i1)")   "background_",j
       end do
       do j=10,n_ba
         write(unit=vs%nampar(9+j),fmt="(a,i2)")   "background_",j
       end do
     end if
     j=9+n_ba+1
     if(npeak <= 9) then
       do i=1,npeak
         write(unit=vs%nampar(j),  fmt="(a,i1)")   "Bragg_Pos_",i
         write(unit=vs%nampar(j+1),fmt="(a,i1)")   "Intensity_",i
         write(unit=vs%nampar(j+2),fmt="(a,i1)")   "Shf_FWHM_",i
         write(unit=vs%nampar(j+3),fmt="(a,i1)")   "Shf_EtaPV_",i
        j=j+4
       end do
     else
       do i=1,9
         write(unit=vs%nampar(j),  fmt="(a,i1)")   "Bragg_Pos_",i
         write(unit=vs%nampar(j+1),fmt="(a,i1)")   "Intensity_",i
         write(unit=vs%nampar(j+2),fmt="(a,i1)")   "Shf_Gamma_",i
         write(unit=vs%nampar(j+3),fmt="(a,i1)")   "Shf_EtaPV_",i
        j=j+4
       end do
       do i=10,npeak
         write(unit=vs%nampar(j),  fmt="(a,i2)")   "Bragg_Pos_",i
         write(unit=vs%nampar(j+1),fmt="(a,i2)")   "Intensity_",i
         write(unit=vs%nampar(j+2),fmt="(a,i2)")   "Shf_FWHM_",i
         write(unit=vs%nampar(j+3),fmt="(a,i2)")   "Shf_EtaPV_",i
        j=j+4
       end do
     end if
   End Subroutine set_nampar

   !!----
   !!---- Subroutine Sum_Pv_Peaks(I,Xval,Ycalc,Vsa,CalDer)
   !!----
   !!---- Asymmetric peak function modelled by convolving Pseudo-Voigt with axial divergence
   !!---- P are parameters. YCALC is the value returned to the main program
   !!---- If CalDer is present the function calculates the analytical derivatives
   !!---- Vsa%Pv(1)  to Vsa%Pv(9) are global parameters: Ratio, Asym1, Asym2, U,V,W,Z,eta0,X
   !!---- Vsa%Pv(10) to Vsa%Pv(ngl+n_a) are background parameters
   !!---- Jstart= ngl+ n_ba +1
   !!---- Vsa%Pv(jstart),Vsa%Pv(jstar+1),Vsa%Pv(jstar+2),Vsa%Pv(jstar+3)... : PeakPosition,
   !!---- Intensity, Shft-FWHM, Shft-Eta
   !!---- Linear interpolation of low and high background
   !!---- To avoid repetitive calculations, the values which cannot variate
   !!---- are stored in intermediate arrays
   !!---- Some derivatives are calculated at the same time as the function
   !!----
   !!---- Update: August -2009
   !!
   Subroutine Sum_PV_Peaks(I,Xval,Ycalc,Vsa,CalDer)
      !---- Arguments ----!
      integer,                    intent(in)    :: i
      real(kind=cp),              intent(in)    :: xval
      real(kind=cp),              intent(out)   :: ycalc
      Type(LSQ_State_Vector_type),intent(in out):: Vsa
      logical,optional,           intent(in)    :: calder

      !---Local variables ---!
      integer                        :: ncount,j,npea,l,ib,ib1,ib2,i1,i2
      real(kind=cp)                  :: tet,spv,s2der,asder1,asder2,etader,tth,tang,bgr, &
                                        dprdt,dprdg,dprde,dprds,dprdd,profil, ss1,v1,  &
                                        ss2,v2,del1,del2,gamma1,gamma2,tn,tn2,tet2
      real(kind=cp)                  :: uder,vder,wder,zder,xder

      v2 = 0

     IF (i == 1) then    !Making intermediate calculations that are not depending on the
                          !particular observation. This is the reason why only for i=1
         npea=0
         ratio=1.0/(1.0+Vsa%Pv(1))
         jstart=ngl+n_ba+1

         DO j=jstart,vsa%np,4
            npea=npea+1
            if(Vsa%Pv(j+1) < 0.0) Vsa%Pv(j+1)=0.1
            Intens(npea)=Vsa%Pv(j+1)*ratio
            tet=Vsa%Pv(j)/2.0/rad
            tn=tan(tet)
            eta1(npea)=Vsa%Pv(8)+Vsa%Pv(9)*Vsa%Pv(j)+Vsa%Pv(j+3)
            Hg=sqrt((Vsa%Pv(4)*tn+Vsa%Pv(5))*tn+Vsa%Pv(6))
            fwhm1(npea)=Hg+Vsa%Pv(7)/cos(tet)+Vsa%Pv(j+2)      !FWHM calculated for all peaks

            ! Derivatives
            if (present(Calder)) then
               der_u(npea)=0.0
               der_v(npea)=0.0
               der_w(npea)=0.0
               if (Hg > 0.0001) then
                  der_u(npea)=0.5*tn*tn/Hg     !4
                  der_v(npea)=0.5*tn/Hg        !5
                  der_w(npea)=0.5/Hg           !6
               end if
               der_z(npea)=0.0
               if (Vsa%Pv(7) > 0.000001) der_z(npea)=1.0/cos(tet) !7
               der_X(npea)=Vsa%Pv(j)       !9
            end if

            ! K-alpha2 contribution
            IF (jobtyp == 1 .or. jobtyp == 3) THEN
               tet2=ASIN(rla*SIN(tet))
               t2(npea)=2.0*rad*tet2
               tn2=tan(tet2)
               eta2(npea)=Vsa%Pv(8)+Vsa%Pv(9)*t2(npea)+Vsa%Pv(j+3)
               dt2(npea)=rla*COS(tet)/SQRT(1.0-(rla*SIN(tet))**2)
               ri2(npea)=Intens(npea)*Vsa%Pv(1)
               Hg=sqrt((Vsa%Pv(4)*tn2+Vsa%Pv(5))*tn2+Vsa%Pv(6))
               fwhm2(npea)=Hg+Vsa%Pv(7)/cos(tet2)+Vsa%Pv(j+2)      !FWHM calculated for all peaks

               ! Derivatives
               if (present(CalDer)) then
                  der_u2(npea)=0.0
                  der_v2(npea)=0.0
                  der_w2(npea)=0.0
                  if (Hg > 0.0001) then
                     der_u2(npea)=0.5*tn2*tn2/Hg     !4
                     der_v2(npea)=0.5*tn2/Hg        !5
                     der_w2(npea)=0.5/Hg           !6
                  end if
                  der_z2(npea)=0.0
                  if (Vsa%Pv(7) > 0.000001) der_z2(npea)=1.0/cos(tet2) !7
                  der_X2(npea)=t2(npea)       !9
               end if
            ELSE
               t2(npea)=Vsa%Pv(j)
               ri2(npea)=0.0
               dt2(npea)=1.0
               fwhm2(npea)=0.0
            END IF
         END DO
     end if  !i=1

     ! Calculation of the function
     ! Initialize derivatives
     spv=0.0; s2der=0.0; asder1=0.0; asder2=0.0; etader=0.0
     uder=0.0; vder=0.0; wder=0.0; zder=0.0; xder=0.0; tth=xval
     vsa%dpv(1:vsa%np)=0.0

     ! Calculation of the background
     ! tang=FLOAT(i-1)/FLOAT(no-1)
     i1=1
     i2=n_ba
     ib1=ngl+1
     ib2=ib1+1
     do ib=1,n_ba-1
        if (tth >= bac_pos(ib) .and. tth <= bac_pos(ib+1)) then
           i1=ib
           i2=ib+1
           ib1=ngl+i1
           ib2=ib1+1
           exit
        end if
     end do
     if(abs(bac_pos(i2)-bac_pos(i1)) < 0.01) then
        tang=0.0
     else
        tang=(tth-bac_pos(i1))/(bac_pos(i2)-bac_pos(i1))
     end if
     bgr=Vsa%pv(ib1)+(Vsa%pv(ib2)-Vsa%pv(ib1))*tang
     l=0

     Do j=jstart,vsa%np,4
        l=l+1
        del1=tth-Vsa%pv(j)
        gamma1=fwhm1(l)
        eta=eta1(l)
        IF (eta < 0.000001) eta=0.000001
        v1=2.0*del1/gamma1
        ss1=0.0
        ss2=0.0
        IF (abs(v1) < 80.0) then
           call prof_val( eta,gamma1,Vsa%pv(2),Vsa%pv(3),tth ,Vsa%pv(j), &
                          dprdt, dprdg,dprde,dprds,dprdd,profil,use_asymm,use_hps)
           ss1=Intens(l)*profil
           if (present(CalDer)) then
              Vsa%dpv(j)=Intens(l)*dprdt           !Derivative w.r.t. 2theta = p(j)
              Vsa%dpv(j+1)=ss1/Vsa%pv(j+1)         !Derivative w.r.t. Integrated intensity
              Vsa%dpv(j+2)=Intens(l)*dprdg         !Derivative w.r.t. FWHM
              Vsa%dpv(j+3)=Intens(l)*dprde         !Derivative w.r.t. Eta
              asder1=asder1+Intens(l)*dprds        !Derivative w.r.t. S_L
              asder2=asder2+Intens(l)*dprdd        !Derivative w.r.t. S_D
              etader=etader+Vsa%dpv(j+3)
              xder=xder+Vsa%dpv(j+3)*der_x(l)
              uder=uder+Vsa%dpv(j+2)*der_u(l)
              vder=vder+Vsa%dpv(j+2)*der_v(l)
              wder=wder+Vsa%dpv(j+2)*der_w(l)
              zder=zder+Vsa%dpv(j+2)*der_z(l)
           end if
        end if

        ! Kalpha2 contribution
        IF (.not.(jobtyp == 2) .and. .not. (jobtyp==4)) then
           del2=tth-t2(l)
           gamma2=fwhm2(l)
           v2=2.0*del2/gamma2
           eta=eta2(l)
           IF (abs(v2) <= 80.0) then
              call prof_val( eta,gamma2,Vsa%pv(2),Vsa%pv(3),tth ,t2(l),&
                            dprdt,dprdg,dprde,dprds,dprdd,profil,use_asymm,use_hps)
              ss2=ri2(l)*profil
              if (present(Calder)) then
                 Vsa%dpv(j)=Vsa%dpv(j)+ri2(l)*dprdt*dt2(l)  !Derivative w.r.t. 2theta = p(j)
                 Vsa%dpv(j+1)=Vsa%dpv(j+1)+ss2/Vsa%pv(j+1)  !Derivative w.r.t. Integrated intensity
                 Vsa%dpv(j+2)=Vsa%dpv(j+2)+ri2(l)*dprdg     !Derivative w.r.t. FWHM
                 Vsa%dpv(j+3)=Vsa%dpv(j+3)+ri2(l)*dprde     !Derivative w.r.t. Eta
                 asder1=asder1+ri2(l)*dprds                 !Derivative w.r.t. S_L
                 asder2=asder2+ri2(l)*dprdd                 !Derivative w.r.t. S_D
                 etader=etader+Vsa%dpv(j+3)
                 xder=xder+Vsa%dpv(j+3)*der_x2(l)
                 uder=uder+Vsa%dpv(j+2)*der_u2(l)
                 vder=vder+Vsa%dpv(j+2)*der_v2(l)
                 wder=wder+Vsa%dpv(j+2)*der_w2(l)
                 zder=zder+Vsa%dpv(j+2)*der_z2(l)
                 s2der=s2der+ss2
              end if
           end if
        end if
        spv=spv+ss1+ss2
     End Do
     ycalc=bgr+spv    ! bgr: Background / spv: function

     ! Assign derivatives to array Vsa%dpv
     If (present(Calder)) then
        Vsa%dpv(ib1)=1.0-tang
        Vsa%dpv(ib2)=tang
        IF (jobtyp == 1 .or. jobtyp == 3) THEN   ! K-alpha2 contribution
           Vsa%dpv(1)=-(ycalc-bgr)*ratio+s2der/Vsa%pv(1)
        ELSE
           Vsa%dpv(1)=0.0
        END IF
        Vsa%dpv(2)=asder1
        Vsa%dpv(3)=asder2
        Vsa%dpv(4)=uder
        Vsa%dpv(5)=vder
        Vsa%dpv(6)=wder
        Vsa%dpv(7)=zder
        Vsa%dpv(8)=etader
        Vsa%dpv(9)=xder
     End If
   End Subroutine Sum_PV_Peaks

   !-----------------------------------------
   ! Calculation of individual peak profiles

   Subroutine Cwl_Sub_Profile(Vsa,d,npks,ysub)
     Type(LSQ_State_Vector_type),         intent(in)     :: Vsa
     Type(LSQ_Data_type),                 intent(in)     :: d
     integer,                             intent(in)     :: npks
     real(kind=cp),dimension(d%nobs,npks),intent(out)    :: ysub
     !
     integer       :: i,j,l,jstart,i1,i2,ib,ib1,ib2
     real(kind=cp) :: spv,tth,tang,bgr, &
                      ss1,v1,dprdt,dprdg,dprde,dprds,dprdd,profil,  &
                      ss2,v2,del1,del2,gamma1,gamma2

     ratio=1.0/(1.0+Vsa%Pv(1))
     jstart=ngl+n_ba+1

     !   Calculation of the function
     l=0
     Do j=jstart,Vsa%np,4  ! loop over peaks
       l=l+1
       if(l > npks) exit
       Do i=1,d%nobs     ! loop over points
         spv=0.0
         tth=d%x(i)
         i1=1
         i2=n_ba
         do ib=1,n_ba-1
           if(tth >= bac_pos(ib) .and. tth <= bac_pos(ib+1)) then
             i1=ib
             i2=ib+1
             ib1=ngl+i1
             ib2=ib1+1
            exit
           end if
         end do
         if(abs(bac_pos(i2)-bac_pos(i1)) < 0.01) then
            tang=0.0
         else
            tang=(tth-bac_pos(i1))/(bac_pos(i2)-bac_pos(i1))
         end if
         bgr=Vsa%Pv(ib1)+(Vsa%Pv(ib2)-Vsa%Pv(ib1))*tang


         del1=tth-Vsa%Pv(j)
         gamma1=fwhm1(l)
         eta=eta1(l)
         IF(eta < 0.000001) eta=0.000001
         ss1=0.0
         ss2=0.0
         v1=2.0*del1/gamma1
         If(abs(v1) < 25.0) then
           call prof_val( eta,gamma1,Vsa%Pv(2),Vsa%Pv(3),tth ,Vsa%Pv(j),      &
                        dprdt, dprdg,dprde,dprds,dprdd,profil,use_asymm,use_hps)
           ss1=Intens(l)*profil
         End If

        ! ligne suivante corrigee le 05 juin 2002
         If(jobtyp == 1 .or. jobtyp==3)  Then  ! K-alpha2 contribution
          del2=tth-t2(l)
          gamma2=fwhm2(l)
          eta=eta2(l)
          v2=2.0*del2/gamma2
          If(abs(v2) < 25.0) then
              call prof_val( eta,gamma2,Vsa%Pv(2),Vsa%Pv(3),tth ,t2(l),      &
                             dprdt,dprdg,dprde,dprds,dprdd,profil,use_asymm,use_hps)
              ss2=ri2(l)*profil
          End If
         end if
         spv=spv+ss1+ss2
         ysub(i,L)=bgr+spv
       End Do  !i points
     End Do    !j peaks
   End Subroutine Cwl_Sub_Profile

 End Module Cw_Diffraction_Pv
