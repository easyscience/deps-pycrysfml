!!----
!!----
!!----
SubModule (CFML_EoS) EoS_TransESD

   implicit none

   Contains

   !!--++
   !!--++ TRANSFORM_ESD
   !!--++
   !!--++ New generalised version for V,K,Kp,Kpp,dK/dT and alpha
   !!--++
   !!--..  NOTE:
   !!--..       the value of 0.001 was chosen to balance non-linearity of
   !!--..       equations (which implies small value) against the arithmatic
   !!--..       precision (wants big value).
   !!--..
   !!--..       This value gives same esd's as double-precision eosfit5
   !!--++
   !!--++ Date: 02/08/2013
   !!--++ Revision: OK
   !!
   Module Function Transform_Esd(P, T, Eos) Result(Esd)
      !---- Arguments ----!
      real(kind=cp),   intent(in)              :: p        ! Pressure at which to calculate parameter esds
      real(kind=cp),   intent(in)              :: t        ! Temperature at which to calculate parameter esds
      type (EoS_Type), intent(in)              :: eos      ! The EoS parameters and vcv
      real(kind=cp),dimension(N_EOSPAR)        :: esd      ! The esd's of Eos parameters at this P and T

      !---- Local Variables ----!
      real(kind=cp), parameter                    :: FACTOR=0.01        ! shift factor for calculating derivatives
      real(kind=cp),dimension(N_EOSPAR,N_EOSPAR)  :: d                  ! cross derivatives of parameters
      real(kind=cp),dimension(-2:2,6)             :: par
      real(kind=cp)                               :: shift              ! shift applied to parameter
      type (eos_type)                             :: eost               ! local copy of input eos
      integer                                     :: i,j,k

      !> initialisation
      d=0.0_cp
      esd=0.0_cp

      !> loop to calculate d(param_i at P,T)/d(param_j at Pref,Tref)
      do j=1,10
         if (EoS%iuse(j) == 1) then
            do k=-2,2,1
               eost=EoS                                                     !reset eos params
               if (abs(EoS%params(j)) > tiny(0.0_cp)) then
                  shift=factor*EoS%params(j)
                  if (j == 10) shift=10.0*shift                    ! alpha0
               else
                  shift=1.0_cp/EoS%factor(j)                    ! shift to a parameter with zero value
                  if (j == 5) shift=0.002                          ! dK/dT has print factor 1, but typical value -.02
               end if

               eost%params(j)=EoS%params(j)+float(k)*shift    ! apply shift to a parameter
               Par(k,1:6)= EoS_Cal(P,T,Eost)                 ! calc resulting parvals
            end do
            d(1:6,j)=(par(-2,1:6)+8.0_cp*(par(1,1:6)-par(-1,1:6))-par(2,1:6))/(12.0_cp*shift) ! derivative to second order approximation
         end if
      end do

      !> d(1:6,j) contains the derivatives in order V,K0,Kp,Kpp,dK/dT,alpha0 wrt param(j) at Pref,Tref
      !  now switch these to 10 for alpha0 and ignore any other alpha coeffs
      d(10,1:10)=d(6,1:10)
      d(6,1:10)=0.0_cp

      !> Now calculate esd-squared array = vcv(i,i). The input vcv is already linear for linear eos!
      do k=1,N_EOSPAR
         do i=1,N_EOSPAR
            if (EoS%iref(i) == 0) cycle ! because vcv(i,j) will be 0
            do j=1,N_EOSPar
               if (EoS%iref(j) == 0) cycle ! because vcv(i,j) will be 0
               esd(k)=esd(k)+EoS%vcv(i,j)*d(k,i)*d(k,j)
            end do
         end do
      end do

      !> Final: extra trap June 2016 in case round off leaves esd(i)^2 < 0
      do i=1,N_EOSPAR
          if (esd(i) > tiny(0.0_cp) ) then
             esd(i)=sqrt(esd(i))
          else
             esd(i)=0.0_cp
          end if
      end do

   End Function Transform_Esd

End Submodule EoS_TransESD