 Module rL_Int_Mod
   use CFML_GlobalDeps,       only: cp
   use CFML_Optimization_LSQ, only: Max_Free_par, LSQ_State_Vector_type, LSQ_Conditions_type, LSQ_Data_Type
   Use CFML_Profiles,         only: Pseudovoigt_Der
   Use CFML_Strings,          only: pack_string
   implicit none
   private

   !Public subroutines
   public:: Sum_PV_peaks, set_nampar, powder_patt_odr, Back_Chebychev

   !Global public variables
   integer,      public, parameter         :: nbac=100    !maximum number of background parameters
   integer,      public, parameter         :: ngl =5      !maximum number of global parameters
   integer,      public, parameter         :: npeaks=(Max_Free_par-(nbac+ngl))/4
   real(kind=cp),private,dimension(npeaks) :: Intens
   real(kind=cp),public ,dimension(npeaks) :: fwhm,eta
   real(kind=cp),public ,dimension(npeaks) :: der_g0H,der_g1H,der_g2H
   real(kind=cp),public ,dimension(npeaks) :: der_g0Eta,der_g1Eta
   real(kind=cp),public, dimension(nbac)   :: bac_pos,bac_int
   integer,public                          :: npeakx
   integer,public                          :: n_ba     ! number of points to define the background
   character (len=132),public              :: title
   character (len=80),public               :: filecode,filedat !codes of input data files
   real(kind=cp),public                    :: chi2, chiold,x_ini,x_fin
   integer, private                        :: jstart
   logical, public                         :: poly_back !Polynomial background.


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
     integer                     :: i,j
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
            do j=1,lvs%np
               fjacb(i,j,1)=lvs%dpv(j)
            end do
          end do
          c%njev=c%njev+1
     end if

   End subroutine powder_patt_odr

   Subroutine set_nampar(n_ba,npeak,vs)
     integer,                    intent(in)    :: n_ba,npeak
     type(LSQ_State_Vector_type),intent(in out):: vs
     integer :: i,j
     ! Subroutine setting the names of all possible parameters in the model
     vs%nampar(:)= " "
     vs%nampar(1)="g0_Hpv"
     vs%nampar(2)="g1_Hpv"
     vs%nampar(3)="g2_Hpv"
     vs%nampar(4)="g0_EtaPV"
     vs%nampar(5)="g1_EtaPV"
     do j=1,n_ba
       write(unit=vs%nampar(ngl+j),fmt="(a,i3)")   "background_",j
     end do
     j=ngl+n_ba+1
     do i=1,npeak
       write(unit=vs%nampar(j),  fmt="(a,i3)")   "Peak_Pos_",i
       write(unit=vs%nampar(j+1),fmt="(a,i3)")   "Intensity_",i
       write(unit=vs%nampar(j+2),fmt="(a,i3)")   "Shf_FWHM_",i
       write(unit=vs%nampar(j+3),fmt="(a,i3)")   "Shf_Eta_",i
       j=j+4
     end do
     do i=ngl+1,ngl+n_ba+4*npeak
       vs%nampar(i)=pack_string(vs%nampar(i))
     end do
   End Subroutine set_nampar

   Subroutine Back_Chebychev(x,x1,x2,n,par,bk,der)
      Real(kind=cp),                         intent(in)  :: x,x1,x2
      Integer,                               intent(in)  :: n
      Real(kind=cp), dimension(:),           intent(in)  :: par
      Real(kind=cp),                         intent(out) :: bk
      Real(kind=cp), dimension(:), optional, intent(out) :: der
      !--- Local variables ---!
      Real(kind=cp) :: thx,rj,derv,c
      integer :: j

      bk=par(1)
      if(present(der)) then
        der(1:n)= 0.0
        der(1)  = 1.0
      end if
      thx=2.0*(x-0.5*(x1+x2))/(x2-x1)
      if(thx < -1.0) thx=-1.0
      if(thx >  1.0) thx= 1.0
      c=acos(thx)
      do j=1,n-1
        rj=real(j,kind=cp)
        derv=cos(rj*c)
        bk=bk+par(j+1)*derv
        if(present(der)) der(j+1)=der(j+1)+derv
      end do
   End Subroutine Back_Chebychev
   !!----
   !!---- Subroutine Sum_Pv_Peaks(I,Xval,Ycalc,Vsa,CalDer)
   !!----
   !!---- Peak shape modelled by Pseudo-Voigt function
   !!---- Ycalc is the value returned to the main program
   !!---- If CalDer is present the function calculates the analytical derivatives
   !!---- Vsa%Pv(1)  to Vsa%Pv(5) are global parameters: g0-Hpv, g1-Hpv,g2-Hpv, g0-EtaPV, g1-EtaPV
   !!---- Vsa%Pv(6) to Vsa%Pv(ngl+n_ba) are background parameters
   !!---- Jstart= ngl+ n_ba +1
   !!---- Vsa%Pv(jstart),Vsa%Pv(jstar+1),Vsa%Pv(jstar+2),Vsa%Pv(jstar+3)... : PeakPosition,
   !!---- Intensity, Shft-FWHM, Shft-Eta
   !!---- Linear interpolation of low and high background or Chebychev polynomial
   !!---- To avoid repetitive calculations, the values which cannot variate
   !!---- are stored in intermediate arrays
   !!---- Some derivatives are calculated at the same time as the function
   !!----
   !!---- Update: October -2021
   !!
   Subroutine Sum_PV_Peaks(I,Xval,Ycalc,Vsa,CalDer)
     !---- Arguments ----!
     integer,                    intent(in)    :: i
     real(kind=cp),              intent(in)    :: xval
     real(kind=cp),              intent(out)   :: ycalc
     Type(LSQ_State_Vector_type),intent(in out):: Vsa
     logical,optional,           intent(in)    :: calder

     !---Local variables ---!
     integer                        :: j,npea,l,ib,ib1,ib2,i1,i2
     real(kind=cp)                  :: spv,etader,tang,bgr,derx,derH,derEta,profil,ss,v, &
                                       del,pos,etav,fwh
     real(kind=cp)                  :: g0Hder,g1Hder,g2Hder,g0Etader,g1Etader
     real(kind=cp), dimension(3)    :: par_der
     real(kind=cp), dimension(24)   :: parv,deriv  !Background parameters for Chebychev polynomials

     if (i == 1) then    !Making intermediate calculations that are not depending on the
                         !particular observation. This is the reason why only for i=1
         npea=0
         jstart=ngl+n_ba+1

         Do j=jstart,vsa%np,4
            npea=npea+1
            pos=Vsa%Pv(j)
            Intens(npea)=Vsa%Pv(j+1)
            fwh=Vsa%Pv(1) + pos*(Vsa%Pv(2) + pos*Vsa%Pv(3))
            fwhm(npea)=fwh + Vsa%Pv(j+2)             !FWHM calculated for all peaks
            eta(npea)=Vsa%Pv(4) + Vsa%Pv(5)*pos + Vsa%Pv(j+3)

            ! Derivatives
            if (present(Calder)) then
                 der_g0H(npea)=0.0
                 der_g1H(npea)=0.0
                 der_g2H(npea)=0.0
               der_g0Eta(npea)=0.0
               der_g1Eta(npea)=0.0
               if (fwh > 0.00001) then
                  der_g0H(npea)=1.0            !1
                  der_g1H(npea)=pos            !2
                  der_g2H(npea)=pos*pos        !3
               end if
               if (eta(npea) > 0.00001) then
                  der_g0Eta(npea) = 1.0        !4
                  der_g1Eta(npea) = pos        !5
               end if
            end if
         End Do
     end if  !i=1

     ! Calculation of the function
     ! Initialize derivatives
     spv=0.0 !Sum of profiles
     g0Hder=0.0; g1Hder=0.0; g2Hder=0.0; g0Etader=0.0; g1Etader=0.0
     vsa%dpv(1:vsa%np)=0.0  !Nullify all derivatives


     if(poly_back) then
        parv(1:n_ba)=Vsa%Pv(ngl+1:ngl+n_ba)
        if(present(Calder)) then
           call Back_Chebychev(xval,x_ini,x_fin,n_ba,parv(1:n_ba),bgr,deriv(1:n_ba))
           Vsa%dpv(ngl+1:ngl+n_ba) = deriv(1:n_ba)
        else
           call Back_Chebychev(xval,x_ini,x_fin,n_ba,parv(1:n_ba),bgr)
        end if
     else
        ! Calculation of the background
        i1=1
        i2=n_ba
        ib1=ngl+1
        ib2=ib1+1
        do ib=1,n_ba-1
           if (xval >= bac_pos(ib) .and. xval <= bac_pos(ib+1)) then
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
           tang=(xval-bac_pos(i1))/(bac_pos(i2)-bac_pos(i1))
        end if
        bgr=Vsa%pv(ib1)+(Vsa%pv(ib2)-Vsa%pv(ib1))*tang
     end if

     l=0
     Do j=jstart,vsa%np,4
        l=l+1
        del=xval-Vsa%pv(j)
        fwh=fwhm(l)
        etav=eta(l)
        IF (etav < 0.0) etav=0.0
        v=2.0*del/fwh
        ss=0.0
        IF (abs(v) < 80.0) then
           call Pseudovoigt_Der(del,[fwh,etav],profil,par_der)
           derx=par_der(1);  derH=par_der(2); derEta=par_der(3)
           ss=Intens(l)*profil
           if (present(CalDer)) then
              Vsa%dpv(j)=Intens(l)*derx            !Derivative of ycalc w.r.t. position = p(j)
              Vsa%dpv(j+1)=ss/Vsa%pv(j+1)          !Derivative of ycalc w.r.t. Integrated intensity
              Vsa%dpv(j+2)=Intens(l)*derH          !Derivative of ycalc w.r.t. FWHM
              Vsa%dpv(j+3)=Intens(l)*derEta        !Derivative of ycalc w.r.t. Eta
              g0Hder=g0Hder+Vsa%dpv(j+2)*der_g0H(l)   !Adding derivatives w.r.t. global parameters
              g1Hder=g1Hder+Vsa%dpv(j+2)*der_g1H(l)
              g2Hder=g2Hder+Vsa%dpv(j+2)*der_g2H(l)
              g0Etader=g0Etader+Vsa%dpv(j+3)*der_g0Eta(l)
              g1Etader=g1Etader+Vsa%dpv(j+3)*der_g1Eta(l)
           end if
        end if
        spv=spv+ss
     End Do
     ycalc=bgr+spv    ! bgr: Background / spv: function

     ! Assign derivatives to array Vsa%dpv
     If (present(Calder)) then
        if(.not. poly_back) then
          Vsa%dpv(ib1)=1.0-tang
          Vsa%dpv(ib2)=tang
        end if
        Vsa%dpv(1)=g0Hder
        Vsa%dpv(2)=g0Etader
        Vsa%dpv(3)=g1Etader
        Vsa%dpv(4)=g0Etader
        Vsa%dpv(5)=g1Etader
     End If
   End Subroutine Sum_PV_Peaks


 End Module rL_Int_Mod
