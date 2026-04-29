 Module rL_kvInt_Mod
   use CFML_GlobalDeps,       only: cp
   use CFML_Optimization_LSQ, only: Max_Free_par, LSQ_State_Vector_type, LSQ_Conditions_type, LSQ_Data_Type
   Use CFML_Profiles,         only: Pseudovoigt_Der
   Use CFML_Strings,          only: pack_string
   Use CFML_Maths,            only: sort
   implicit none
   private

   !Public subroutines
   public:: Sum_PV_peaks, set_nampar, profile_patt_odr, profile_patt_LVM, Back_Chebychev, gen_peaks, Allocate_Globals, &
            Set_weakness, Sigma_Back, Update_Peaks

   !Global public variables
   character(len=132),public          :: title
   character(len=80), public          :: filecode,filedat !codes of input data files
   integer,      public, parameter    :: ngl = 22     !maximum number of global parameters
   integer,      public, parameter    :: nbac= 24     !maximum number of background parameters
   integer,      public               :: npeakx,jsc
   integer,      public               :: n_ba         ! number of background parameters
   integer,      public               :: nkvec        ! number of propagation vectors
   real(kind=cp),public               :: chi2, chiold,x_ini,x_fin, threshold=3.0
   integer,      public,dimension(:,:),allocatable :: hkl   !(3+nkvec,npeakx) !Allocated in generation of peaks
   integer,      public,dimension(:),  allocatable :: nharm !( nkvecx)        !Allocated after reading CFL
   real(kind=cp),public,dimension(:,:),allocatable :: kvec  !(3,nkvec)        !Allocated after reading CFL
   real(kind=cp),public,dimension(:),  allocatable :: peak_pos,corr_pp  !(npeakx) !Allocated in generation of peaks and Allocate_Globals, ideal and corrected peak positions
   real(kind=cp),public,dimension(:),  allocatable :: Intens    !(npeakx) All items below need a call to Allocate_Globals
   real(kind=cp),public,dimension(:),  allocatable :: fwhmf,etaf,fwhms,etas,fwhm,eta,sfwhm,seta
   real(kind=cp),public,dimension(:),  allocatable :: der_g0H,der_g1H,der_g2H
   real(kind=cp),public,dimension(:),  allocatable :: der_g0Eta,der_g1Eta
   real(kind=cp),public,dimension(:),  allocatable :: der_z0,der_z1,der_z2
   real(kind=cp),public,dimension(:,:),allocatable :: der_k
   logical,      public,dimension(:),  allocatable :: satellite,weak_dyn  !Allocated in generation of peaks and Allocate_Globals

   Type(LSQ_State_Vector_type),      public :: vs  !State vector containing pv, code, vs%nampar,etc..
   Type(LSQ_Conditions_type ),save,  public :: c   !conditions of the algorithm
   Type(LSQ_Data_Type),              public :: d   !Data to be refined (set in the main program)

  contains

   !!---- Subroutine Gen_Peaks(h_ini,h_fin,js,nkv,kv,nhar,n,h,peakp,satel)
   !!----
   !!----  This subroutine generates the indices of reflections in the scan
   !!----  It is valid only for scans along one of the common components of the propagation vectors
   !!----
   Subroutine Gen_Peaks(h_ini,h_fin,js,nkv,kv,nhar,n,h,peakp,satel)
     real(kind=cp), dimension(3),              intent(in)  :: h_ini,h_fin ! Real indices of the initial an final scan
     integer,                                  intent(in)  :: js          ! Component varied in the scan
     integer,                                  intent(in)  :: nkv         ! Number of modulation vectors
     real(kind=cp), dimension(3,nkv),          intent(in)  :: kv          ! Modulation vectors
     integer,       dimension(  nkv),          intent(in)  :: nhar        ! Maximum harmonic index
     integer,                                  intent(out) :: n           ! Number of generated peaks
     integer,     dimension(:,:),  allocatable,intent(out) :: h           ! Indices of reflections
     real(kind=cp), dimension(:),  allocatable,intent(out) :: peakp       ! Peak positions in r.l.u. along "js"
     logical,       dimension(:),  allocatable,intent(out) :: satel       ! Satellite indicator
     !
     !--- Local variables ---!
     integer       :: i,j,k,L, ini, fin, nf, ns, np, isc
     real(kind=cp) :: pos, x1,x2
     integer, dimension(:), allocatable :: ind

     x1=h_ini(js); x2=h_fin(js)
     ini=nint(x1); fin=nint(x2)
     !Number of fundamental and satellite peaks
     nf=fin-ini+1
     ns=0
     if(nkv > 0) then
        do i=1,nkv
          ns=ns + 2 * nhar(i)
        end do
        ns = nf * ns
     end if
     np=ns+nf
     if(allocated(h)) deallocate(h)
     if(allocated(peakp)) deallocate(peakp)
     if(allocated(satel)) deallocate(satel)
     if(allocated(ind)) deallocate(ind)
     allocate(h(3+nkv,np))   !The allocation here may be higher than the final number of peaks "n"
     allocate(peakp(np),satel(np), ind(np))
     peakp=0.0; satel=.false.
     h=0
     peakp=0.0
     n=0
     do i=ini,fin
       pos=real(i)
       if(pos > x1 .and. pos < x2) then
         n=n+1
         peakp(n) = pos
         do j=1,3
           h(j,n)= nint(h_ini(j))
           if(j == js) then
             h(js,n)=i
           end if
         end do
       end if
       if(nkv > 0) then
         do L = 1,nkv
            do k = -nhar(L),nhar(L)
               if(k == 0) cycle
               pos=real(i) + k * kv(js,L)
               if(pos > x1 .and. pos < x2) then
                 n=n+1
                 peakp(n) = pos
                 h(3+L,n)=k
                 satel(n)=.true.
               end if
            end do
         end do
       end if
     end do
     !Reorder the peaks to make them increasing in jsc coordinate

     ind=sort(peakp,n)
     open(newunit=isc,status="scratch",form="unformatted",action="readwrite")
     do i=1,n
       j=ind(i)
       write(isc) peakp(j),h(:,j),satel(j)
     end do
     rewind(unit=isc)
     do i=1,n
      read(isc) peakp(i),h(:,i),satel(i)
     end do
     close(isc)
   End Subroutine Gen_Peaks

   Subroutine Allocate_Globals(n)
     integer, intent(in) :: n
     if(allocated(Intens))  deallocate(Intens)
     allocate(Intens(n))
     Intens=0.0
     if(allocated(fwhmf))  deallocate(fwhmf)
     if(allocated(fwhm))  deallocate(fwhm)
     if(allocated(sfwhm))  deallocate(sfwhm)
     if(allocated(eta))  deallocate(eta)
     if(allocated(seta))  deallocate(seta)
     allocate(fwhmf(n),fwhm(n),eta(n),sfwhm(n),seta(n))
     fwhmf=0.0; fwhm=0.0; eta=0.0; sfwhm=0.0; seta=0.0
     if(allocated(fwhms))  deallocate(fwhms)
     allocate(fwhms(n))
     fwhmf=0.0
     if(allocated(etaf))  deallocate(etaf)
     allocate(etaf(n))
     etaf=0.0
     if(allocated(etas))  deallocate(etas)
     allocate(etas(n))
     etas=0.0
     if(allocated(der_g0H))  deallocate(der_g0H)
     allocate(der_g0H(n))
     der_g0H=0.0
     if(allocated(der_g1H))  deallocate(der_g1H)
     allocate(der_g1H(n))
     der_g1H=0.0
     if(allocated(der_g2H))  deallocate(der_g2H)
     allocate(der_g2H(n))
     der_g2H=0.0
     if(allocated(der_g0Eta))  deallocate(der_g0Eta)
     allocate(der_g0Eta(n))
     der_g0Eta=0.0
     if(allocated(der_g1Eta))  deallocate(der_g1Eta)
     allocate(der_g1Eta(n))
     der_g1Eta=0.0
     if(allocated(corr_pp))  deallocate(corr_pp)
     allocate(corr_pp(n))
     corr_pp=peak_pos(1:n)
     if(allocated(der_z0))  deallocate(der_z0)
     allocate(der_z0(n))
     der_z0=0.0
     if(allocated(der_z1))  deallocate(der_z1)
     allocate(der_z1(n))
     der_z1=0.0
     if(allocated(der_z2))  deallocate(der_z2)
     allocate(der_z2(n))
     der_z2=0.0
     if(allocated(der_k))  deallocate(der_k)
     allocate(der_k(9,n))
     der_k=0.0
     if(allocated(weak_dyn))  deallocate(weak_dyn)
     allocate(weak_dyn(n))
     weak_dyn=.false.
   End Subroutine Allocate_Globals

   Subroutine profile_patt_odr(n,m,np,nq,ldn,ldm,ldnp,beta,xplusd,ifixb,  &
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
     real(kind=cp)               :: resid
     type(LSQ_State_Vector_type) :: lvs

     fjacd=0.0
     !To avoid warnings
        if(istop == 987654) write(*,*) "This is unbelivable!"
        if(ifixb(1) == 987 .and. ifixx(1,1) == 654 .and. xplusd(1,1) == 456) write(*,*) "This is also unbelivable!!"
     !end doing nothing to avoid warnings
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

   End Subroutine profile_patt_odr

   Subroutine profile_patt_LVM(m,n,x,fvec,fjac,iflag)
     Integer,                      Intent(In)    :: m, n
     Real(Kind=cp),Dimension(:),   Intent(In)    :: x
     Real(Kind=cp),Dimension(:),   Intent(In Out):: fvec
     Real(Kind=cp),Dimension(:,:), Intent(Out)   :: fjac
     Integer,                      Intent(In Out):: iflag

     !Local variables
     integer                     :: i,j,no
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

   End Subroutine profile_patt_LVM

   Subroutine set_nampar(n_ba,npeak,vs)
     integer,                    intent(in)    :: n_ba,npeak
     type(LSQ_State_Vector_type),intent(in out):: vs
     integer :: i,j
     ! Subroutine setting the names of all possible parameters in the model
     vs%nampar(:) = " "
     vs%nampar(1) ="Zero0"           !Constant zero
     vs%nampar(2) ="Zero1"           !First order zero
     vs%nampar(3) ="Zero2"           !Second order zero
     vs%nampar(4) ="g0f_Hpv"         !Fundamental reflections FWHM parameters
     vs%nampar(5) ="g1f_Hpv"
     vs%nampar(6) ="g2f_Hpv"
     vs%nampar(7) ="g0s_Hpv"         !Satellite reflections FWHM parameters
     vs%nampar(8) ="g1s_Hpv"
     vs%nampar(9) ="g2s_Hpv"
     vs%nampar(10)="g0f_EtaPV"       !Fundamental reflections Eta parameters
     vs%nampar(11)="g1f_EtaPV"
     vs%nampar(12)="g0s_EtaPV"       !Satellite reflections Eta parameters
     vs%nampar(13)="g1s_EtaPV"
     vs%nampar(14)="k1_x"            !Up to three propagation vectors for fixing positions of satellites
     vs%nampar(15)="k1_y"
     vs%nampar(16)="k1_z"
     vs%nampar(17)="k2_x"
     vs%nampar(18)="k2_y"
     vs%nampar(19)="k2_z"
     vs%nampar(20)="k3_x"
     vs%nampar(21)="k3_y"
     vs%nampar(22)="k3_z"

     do j=1,n_ba
       write(unit=vs%nampar(ngl+j),fmt="(a,i3)")   "background_",j
     end do
     j=ngl+n_ba+1
     do i=1,npeak
       write(unit=vs%nampar(j),  fmt="(a,i3)")   "Shf_Peak_Pos_",i
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

      thx=2.0*(x-0.5*(x1+x2))/(x2-x1)
      if(thx < -1.0) thx=-1.0
      if(thx >  1.0) thx= 1.0
      c=acos(thx)
      bk=0.0
      do j=1,n
        rj=real(j-1,kind=cp)
        derv=cos(rj*c)
        bk=bk+par(j)*derv
        if(present(der)) der(j)=derv
      end do
   End Subroutine Back_Chebychev

   Function Sigma_Back(x,x1,x2,n,sig_par) result(sigmab)
     real(kind=cp),               intent(in) :: x,x1,x2
     integer,                     intent(in) :: n
     real(kind=cp), dimension(n), intent(in) :: sig_par
     real(kind=cp)                           :: sigmab
      !--- Local variables ---!
      Real(kind=cp) :: thx,rj,derv,c
      integer :: j

      sigmab=0.0
      thx=2.0*(x-0.5*(x1+x2))/(x2-x1)
      if(thx < -1.0) thx=-1.0
      if(thx >  1.0) thx= 1.0
      c=acos(thx)
      do j=1,n
        rj=real(j-1,kind=cp)
        derv=cos(rj*c)
        sigmab=sigmab+(derv*sig_par(j))**2
      end do
      sigmab=sqrt(sigmab)
   End Function Sigma_Back

   !!----
   !!---- Subroutine Sum_Pv_Peaks(I,Xval,Ycalc,Vsa,CalDer)
   !!----
   !!---- Peak shape modelled by Pseudo-Voigt function. Peak positions Peak_Pos and indices hkl(3+nkvec,n) are calculated
   !!---- externally to this subroutine. From peak positions the corrected peak positions (applying zero-shifts and local
   !!---- shifts) are calculated for the first point i.
   !!---- Ycalc is the value returned to the main program
   !!---- If CalDer is present the function calculates the analytical derivatives
   !!---- Vsa%Pv(1)  to Vsa%Pv(3) are global parameters controly the zero shif
   !!---- Vsa%Pv(4)  to Vsa%Pv(6): FWHM parameters of fundamental reflections g0f-Hpv, g1f-Hpv,g2f-Hpv
   !!---- Vsa%Pv(7)  to Vsa%Pv(9): FWHM parameters of  satellite  reflections g0s-Hpv, g1s-Hpv,g2s-Hpv
   !!---- Vsa%Pv(10), Vsa%Pv(11) : Eta parameters of fundamental reflections  g0f_EtaPV, g1f_EtaPV
   !!---- Vsa%Pv(12), Vsa%Pv(13) : Eta parameters of  satellite  reflections  g0s_EtaPV, g1s_EtaPV
   !!---- Vsa%Pv(14) to Vsa%Pv(22) are the components k1x,k1y,k1z,k2x,k2y,k2z,k3x,k3y,k3z are the components of k-vectors
   !!     Vsa%Pv(23) to Vsa%Pv(ngl+n_ba) are background parameters of the Chebychev poynomial
   !!---- Jstart= ngl+ n_ba +1
   !!---- Vsa%Pv(jstart),Vsa%Pv(jstar+1),Vsa%Pv(jstar+2),Vsa%Pv(jstar+3)... : PeakPosition,
   !!---- To avoid repetitive calculations, the values which cannot vary in succesive points
   !!---- are stored in intermediate arrays
   !!---- Some derivatives are calculated at the same time as the function and some variables
   !!---- (Npeax,n_ba, ngl, Peak_pos, jsc, hkl, satellite, etc.) are accessible from host association.
   !!----
   !!---- Update: November -2021
   !!
   Subroutine Sum_PV_Peaks(I,Xval,Ycalc,Vsa,CalDer)
     !---- Arguments ----!
     integer,                    intent(in)    :: i
     real(kind=cp),              intent(in)    :: xval
     real(kind=cp),              intent(out)   :: ycalc
     Type(LSQ_State_Vector_type),intent(in out):: Vsa
     logical,optional,           intent(in)    :: calder

     !---Local variables ---!
     integer                        :: j,npea,L,k
     real(kind=cp)                  :: sYciL,bgr,derx,derH,derEta,profil,YciL,v, &
                                       del,pos,etav,fwh
     real(kind=cp)                  :: g0Hfder,g1Hfder,g2Hfder,g0Etafder,g1Etafder,z0der,z1der,z2der
     real(kind=cp)                  :: g0Hsder,g1Hsder,g2Hsder,g0Etasder,g1Etasder
     real(kind=cp), dimension(9)    :: kder
     real(kind=cp), dimension(3)    :: par_der
     real(kind=cp), dimension(2)    :: par
     real(kind=cp), dimension(24)   :: parv,deriv  !Background parameters for Chebychev polynomials
     integer                        :: jstart,ini

     if (i == 1) then    !Making intermediate calculations that are not depending on the
                         !particular observation. This is the reason why only for i=1

         !Update ideal positions of satellite peaks

         if(nkvec > 0) then
           do j=1,npeakx                   !For each peak
             if(all(hkl(4:,j) == 0)) cycle !Extra dimensions with indices 0 => fundamental reflection
             ini=10                        !Starting index to calculate the number of the proper parameter
             do L=1,nkvec
               ini=ini+3                   !For k1, ini=13, for k2, ini=16, for k3, ini=19
               k=hkl(3+L,j)
               if(k /= 0) then
                  peak_pos(j)=peak_pos(j)-k*kvec(jsc,L) + k * Vsa%Pv(ini+jsc) ! Vsa%Pv(ini+jsc)=kvec(jsc,L)
               end if
             end do
           end do
         end if

         jstart=ngl+n_ba+1
         npea=0
         Do j=jstart,vsa%np,4
            npea=npea+1
            pos= peak_pos(npea)
            corr_pp(npea) = pos + Vsa%Pv(j) + Vsa%Pv(1) + pos*(Vsa%Pv(2)+Vsa%Pv(3)*pos)
            Intens(npea)=Vsa%Pv(j+1)
            if(satellite(npea)) then
              fwh=Vsa%Pv(7) + pos*(Vsa%Pv(8) + pos*Vsa%Pv(9))        !Global Satellite reflections FWHM
              fwhms(npea)= fwh + Vsa%Pv(j+2)                         !FWHM calculated for all satellite peaks
              etas(npea) = Vsa%Pv(12) + Vsa%Pv(13)*pos + Vsa%Pv(j+3) !Global eta for fundamental peaks
            else
              fwh=Vsa%Pv(4) + pos*(Vsa%Pv(5) + pos*Vsa%Pv(6))        !Global Fundamental reflections FWHM
              fwhmf(npea)= fwh + Vsa%Pv(j+2)                         !FWHM calculated for all fundamental peaks
              etaf(npea) = Vsa%Pv(10) + Vsa%Pv(11)*pos + Vsa%Pv(j+3) !Global eta for fundamental peaks
            end if
            ! Derivatives
            if (present(Calder)) then
                 der_g0H(npea)=0.0
                 der_g1H(npea)=0.0
                 der_g2H(npea)=0.0
               der_g0Eta(npea)=0.0
               der_g1Eta(npea)=0.0
               der_k(:,npea) = 0.0

               der_Z0(npea) = 1.0        !1  Derivatives of positions w.r.t. zero-shift parameters
               der_Z1(npea) = pos        !2
               der_Z2(npea) = pos*pos    !3
               !The values of the following derivatives do not depend on the fundamental or satellite character of the reflection
               der_g0H(npea)=1.0        !4,7     !Derivatives of FWHM w.r.t. fwhm parameters
               der_g1H(npea)=pos        !5,8
               der_g2H(npea)=pos*pos    !6,9
               der_g0Eta(npea) = 1.0    !10,12    !Derivatives of ETA w.r.t. eta parameters
               der_g1Eta(npea) = pos    !11,13

               if(satellite(npea)) then
                  !Propagation vectors
                  ini=10                        !Starting index to calculate the number of the proper parameter
                  do L=1,nkvec
                    ini=ini+3                   !For k1, ini=13, for k2, ini=16, for k3, ini=19
                    k=hkl(3+L,npea)
                    if(k /= 0) then
                        !write(*,*) " ini+jsc = ",ini+jsc, ini+jsc-13
                       if(Vsa%code(ini+jsc) == 1 ) der_k(ini+jsc-13,npea) =  k  !derivative of peak position with respect to k-vector
                    end if
                  end do
               end if
            end if
         End Do
     end if  !i=1

     ! Calculation of the function
     ! Initialize derivatives
     sYciL=0.0 !Sum of profile contributions to point "I" of value xval
     !Cumulants of derivatives for each point
     g0Hfder=0.0; g1Hfder=0.0; g2Hfder=0.0; g0Etafder=0.0; g1Etafder=0.0
     g0Hsder=0.0; g1Hsder=0.0; g2Hsder=0.0; g0Etasder=0.0; g1Etasder=0.0
     z0der=0.0; z1der=0.0; z2der=0.0; kder=0.0
     vsa%dpv(1:vsa%np)=0.0  !Nullify all derivatives

     !Calculation of background "bgr" and derivatives "deriv"
     parv(1:n_ba)=Vsa%Pv(ngl+1:ngl+n_ba)
     if(present(Calder)) then
        call Back_Chebychev(xval,x_ini,x_fin,n_ba,parv(1:n_ba),bgr,deriv(1:n_ba))
        Vsa%dpv(ngl+1:ngl+n_ba) = deriv(1:n_ba)
     else
        call Back_Chebychev(xval,x_ini,x_fin,n_ba,parv(1:n_ba),bgr)
     end if

     j=ngl+n_ba+1
     Do L=1,npeakx
        del=xval-corr_pp(L)
        if(satellite(L)) then
          fwh=fwhms(L)
          etav=etas(L)
        else
          fwh=fwhmf(L)
          etav=etaf(L)
        end if
        IF (etav < 0.0) etav=0.0
        v=2.0*del/fwh
        YciL=0.0
        IF (abs(v) < 80.0) then
           par=[fwh,etav]
           if(present(Calder)) then
             call Pseudovoigt_Der(del,par,profil,par_der)
           else
             call Pseudovoigt_Der(del,par,profil)
           end if
           YciL=Intens(L)*profil !Contribution to ycalc of peak L
           !write(*,"(a,i5,a,i3,a,f12.4)") "  YciL for point #",i," and peak ",L," -> ",YciL
           if (present(CalDer)) then
             derx=Intens(L)*par_der(1);  derH=Intens(L)*par_der(2); derEta=Intens(L)*par_der(3) !derivatives of YciL w.r.t pos,H, eta
             if(Vsa%code(1)   == 1) z0der=z0der + derx * der_z0(L)   !First calculation of derivatives independent of the peak nature (F or S)
             if(Vsa%code(2)   == 1) z1der=z1der + derx * der_z1(L)
             if(Vsa%code(3)   == 1) z2der=z2der + derx * der_z2(L)

             if(Vsa%code(j)   == 1) Vsa%dpv(j)  = derx                !Derivative of ycalc w.r.t. shift of position = p(j)
             if(Vsa%code(j+1) == 1) Vsa%dpv(j+1)= profil              !Derivative of ycalc w.r.t. Integrated intensity
             if(Vsa%code(j+2) == 1) Vsa%dpv(j+2)= derH                !Derivative of ycalc w.r.t. shift of FWHM
             if(Vsa%code(j+3) == 1) Vsa%dpv(j+3)= derEta              !Derivative of ycalc w.r.t. lshift of Eta
             !write(*,"(a,2i5,6f14.5,i5)") " Result of Pseudovoigt_Der: ",i,L,intens(L),del,fwh,etav,profil,Vsa%dpv(j+1),Vsa%code(j+1)
             !Adding derivatives w.r.t. global parameters (this depends on F/S)
             if(satellite(L)) then
                 if(Vsa%code(7)  == 1) g0Hsder  = g0Hsder   + derH  * der_g0H(L)
                 if(Vsa%code(8)  == 1) g1Hsder  = g1Hsder   + derH  * der_g1H(L)
                 if(Vsa%code(9)  == 1) g2Hsder  = g2Hsder   + derH  * der_g2H(L)
                 if(Vsa%code(12) == 1) g0Etasder= g0Etasder + derEta* der_g0Eta(L)
                 if(Vsa%code(13) == 1) g1Etasder= g1Etasder + derEta* der_g1Eta(L)
                  kder(:) = kder(:)   +  derx * der_k(:,L)
             else
                 if(Vsa%code(4)  == 1) g0Hfder  = g0Hfder  + derH  * der_g0H(L)
                 if(Vsa%code(5)  == 1) g1Hfder  = g1Hfder  + derH  * der_g1H(L)
                 if(Vsa%code(6)  == 1) g2Hfder  = g2Hfder  + derH  * der_g2H(L)
                 if(Vsa%code(10) == 1) g0Etafder= g0Etafder+ derEta* der_g0Eta(L)
                 if(Vsa%code(11) == 1) g1Etafder= g1Etafder+ derEta* der_g1Eta(L)
             end if
           end if
        end if
        j=j+4
        sYciL=sYciL+YciL  !Contribution to ycalc at point i of all peaks
     End Do
     ycalc=bgr+sYciL    !  Background + sYciL

     ! Assign derivatives to array Vsa%dpv
     If (present(Calder)) then
        Vsa%dpv(1) = z0der       !     "Zero0"           !Constant zero
        Vsa%dpv(2) = z1der       !     "Zero1"           !First order zero
        Vsa%dpv(3) = z2der       !     "Zero2"           !Second order zero
        Vsa%dpv(4) = g0Hfder     !     "g0f_Hpv"         !Fundamental reflections FWHM parameters
        Vsa%dpv(5) = g1Hfder     !     "g1f_Hpv"
        Vsa%dpv(6) = g2Hfder     !     "g2f_Hpv"
        Vsa%dpv(7) = g0Hsder     !     "g0s_Hpv"         !Satellite reflections FWHM parameters
        Vsa%dpv(8) = g1Hsder     !     "g1s_Hpv"
        Vsa%dpv(9) = g2Hsder     !     "g2s_Hpv"
        Vsa%dpv(10)= g0Etafder   !     "g0f_EtaPV"       !Fundamental reflections Eta parameters
        Vsa%dpv(11)= g1Etafder   !     "g1f_EtaPV"
        Vsa%dpv(12)= g0Etasder   !     "g0s_EtaPV"       !Satellite reflections Eta parameters
        Vsa%dpv(13)= g1Etasder   !     "g1s_EtaPV"
        Vsa%dpv(14:22)=  kder    !     "k1_x", "k1_y", "k1_z", "k2_x", "k2_y", "k2_z"  ....
     End If
   End Subroutine Sum_PV_Peaks

   Function Ymax_peak(L) result(YmaxP)
      integer,intent(in) :: L
      real(kind=cp)      :: YmaxP
      !
      real(kind=cp):: fwh,etav,profil
      if(satellite(L)) then
        fwh=fwhms(L)
        etav=etas(L)
      else
        fwh=fwhmf(L)
        etav=etaf(L)
      end if
      call Pseudovoigt_Der(0.0,[fwh,etav],profil)
      YmaxP=Intens(L)*profil !Maximum contribution to ycalc of peak L
   End Function Ymax_peak

   Subroutine Set_weakness()
     integer :: L
     real(kind=cp) :: ymax,sbgr
     do L=1,npeakx
        ymax=Ymax_peak(L)
        sbgr=Sigma_Back(corr_pp(L),x_ini,x_fin,n_ba,vs%spv(ngl+1:ngl+n_ba))
        if(ymax > threshold*sbgr) then
          weak_dyn(L) = .false.
        else
          weak_dyn(L) = .true.
        end if
     end do
   End Subroutine Set_weakness

   Subroutine Update_Peaks()
     integer       :: i,j
     real(kind=cp) :: pos,fwh,sfwh,et,set
     j=ngl+n_ba+1
     Do i=1,npeakx
        pos= peak_pos(i)
        corr_pp(i) = pos + Vs%Pv(j) + Vs%Pv(1) + pos*(Vs%Pv(2)+Vs%Pv(3)*pos)
        Intens(i)=Vs%Pv(j+1)
        if(Intens(i) < 0.1 .and. vs%spv(j+1) <= 0.1 ) then   !sigma obtained by LSQ
          vs%spv(j+1)= Sigma_Back(corr_pp(i),x_ini,x_fin,n_ba,vs%spv(ngl+1:ngl+n_ba)) / threshold
          if(vs%spv(j+1) < 0.1) vs%spv(j+1)=2.5
        else if(vs%spv(j+1) <= 0.01) then
            vs%spv(j+1)=sqrt(vs%pv(j+1))
        end if
        if(satellite(i)) then
          fwh=Vs%Pv(7) + pos*(Vs%Pv(8) + pos*Vs%Pv(9))     !Global Satellite reflections FWHM
          fwhms(i)= fwh + Vs%Pv(j+2)                       !FWHM calculated for all satellite peaks
          etas(i) = Vs%Pv(12) + Vs%Pv(13)*pos + Vs%Pv(j+3) !Global eta for satellite peaks
          fwhm(i)=fwhms(i)
          sfwhm(i)=sqrt(Vs%sPv(7)**2 + (pos*Vs%sPv(8))**2 + (pos*pos*Vs%sPv(9))**2 + vs%spv(j+2)**2)
          eta(i) = etas(i)
          seta(i)=sqrt(Vs%sPv(12)**2+ (pos*Vs%sPv(13))**2 + vs%spv(j+3)**2)
        else
          fwh=Vs%Pv(4) + pos*(Vs%Pv(5) + pos*Vs%Pv(6))     !Global Fundamental reflections FWHM
          fwhmf(i)= fwh + Vs%Pv(j+2)                       !FWHM calculated for all fundamental peaks
          etaf(i) = Vs%Pv(10) + Vs%Pv(11)*pos + Vs%Pv(j+3) !Global eta for fundamental peaks
          fwhm(i)=fwhmf(i)
          sfwhm(i)=sqrt(Vs%sPv(4)**2 + (pos*Vs%sPv(5))**2 + (pos*pos*Vs%sPv(6))**2 + vs%spv(j+2)**2)
          eta(i) = etaf(i)
          seta(i)=sqrt(Vs%sPv(10)**2+ (pos*Vs%sPv(11))**2 + vs%spv(j+3)**2)
        end if
        j=j+4
     End do
   End Subroutine Update_Peaks

 End Module rL_kvInt_Mod
