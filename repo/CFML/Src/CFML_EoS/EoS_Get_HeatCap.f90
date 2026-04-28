!!----
!!----
!!----
SubModule (CFML_EoS) EoS_Get_HeatCap
   implicit none

   Contains

   !!----
   !!---- GET_CP
   !!----
   !!---- Calculates the heat capacity at constant pressure by the Gruenesien relation
   !!----
   !!---- If problem, like no gamma0, or  linear eos, default return is Cp=0.
   !!----
   !!---- Date: 01/04/2020
   !!---- Revision: OK
   !!
   Module Function Get_Cp(P, T, EoS) result(C)
      !---- Arguments ----!
      real(kind=cp),  intent(in) :: P,T
      type(Eos_Type), intent(in) :: EoS     ! Eos Parameters
      real(kind=cp)              :: C       ! Heat capacity in J/mol/K if V is molar

      !---- Local Variables ----!
      real(kind=cp) :: v,k,al,gamma2,cv,gamma

      !> Default
      C=0.0_cp

      !> checks
      if (abs(eos%params(18)) < tiny(0.0_cp)) return          ! gamma=0. therefore Cp undefined

      if (eos%imodel == -1) then !ptv table does not support Grueneisen or Cp
         call set_error(-1, 'EoS from PTV table does not support Cp calculations')
         return
      end if

      !> get V and K and into SI units
      v=get_volume(p,t,eos)

      k=k_cal(v,t,eos,p=p)
      if (index(U_case(eos%pscale_name),'GPA') > 0)  k=k*1.0E9
      if (index(U_case(eos%pscale_name),'KBAR') > 0) k=k*1.0E8

      select case(eos%itherm)
         case(7, 8, 9)  !> Calculation from Cv, avoiding alpha, which would be recursive
            v=get_volume(p,t,eos)
            gamma2=get_grun_th(P,T,Eos)**2.0_cp
            ! gamma2=get_grun_V(V,Eos)**2.0_cp

            if (VscaleMGD(eos)) v=v*1.0E-6     !V now in m3/mol
            cv=get_cv(p,t,eos)
            c=(1.0_cp + gamma2*Cv*T/k/v)*Cv

         case default   !from Cp=(1+alpha.gamma.T)Cv
            al=Alpha_Cal(P,T,eos)
            gamma=Get_Grun_th(P,T,eos)
            c=(1.0_cp + al*gamma*T)*get_cv(p,t,eos)
      end select

   End Function Get_Cp

   !!----
   !!---- GET_CV
   !!----
   !!---- Calculates the heat capacity at constant volume by the Gruenesien relation for most EoS
   !!----  for those that are oscillator-based, does explicit calculation from oscillator equations
   !!---- If problem, like no gamma0, or  linear eos, default return is Cv=0.
   !!----
   !!---- Date: 01/04/2020
   !!---- Revision: OK
   !!
   Module Function Get_Cv(P, T, Eos, j) result(Cv)
      !---- Arguments ----!
      real(kind=cp),     intent(in) :: P,T
      type(Eos_Type),    intent(in) :: EoS   ! Eos Parameters
      integer, optional, intent(in) :: j     ! which oscillator: then Pthermal only calculates for this one
      real(kind=cp)                 :: Cv    ! Heat capacity in J/mol/K if V is molar

      !---- Local Variables ----!
      integer                      :: i,jo
      real(kind=cp),dimension(0:2) :: Cvpart
      real(kind=cp)                :: v,k,thetaD,gammaV,x

      !> init
      jo=-1     !calculate all:
      if (present(j)) then
         if (j > -1 .and. j < 3) jo=j
      end if
      cvpart=0.0_cp
      Cv=0.0_cp

      v=get_volume(p,t,eos)

      select case(eos%itherm)
         case (-1)
            if (eos%imodel == -1) then !ptv table does not support Grueneisen or Cp
               call set_error(-1,'EoS from PTV table does not support Cv calculations')
               return
            end if

         case (7) !> MGD
            thetaD=get_DebyeT(V,Eos)
            x=thetaD/t
            if (x < huge(0.))  &
               cvpart(0)=3.0_cp*eos%params(13)*8.314_cp * (4.0_cp*debye(3,x) -3.0_cp*x/(exp(x)-1))
               ! no scaling, units are in R=8.314 J/mol/K

         case (8) !> Einstein
            x=get_DebyeT(V,Eos)/t
            if (x < 20)     &        !corresponds to 0.05ThetaE where Cv < 0.00002 J/mol/K
               cvpart(0)=3.0_cp*eos%params(13)*8.314_cp * x**2._cp * exp(x)/(exp(x)-1)**2._cp
               ! no scaling, units are in R=8.314 J/mol/K

         case (9) ! Cv Table - just do look-up
            ! Cv for this model is independent of P
            i=locate(eos%cv_table(1:,1),T,size(eos%cv_table,dim=1))     ! i is the index to the entry with biggest T < Trequest
            if (i > Ubound(eos%cv_table,dim=1)-1) then
               call set_error(-1,'Request to Cv table with T > Tmax')
               i=Ubound(eos%cv_table,dim=1)-1

            else if (i < 1) then
               call set_error(-1,'Request to Cv table with T < Tmin')
               i=1
            end if
            cvpart(0)=eos%cv_table(i,2) + (eos%cv_table(i+1,2) - eos%cv_table(i,2))/(eos%cv_table(i+1,1) - eos%cv_table(i,1))*(T - eos%cv_table(i,1))

         case default !> Cv = alpha.Kt/gamma/V
            if (abs(eos%params(18)) < tiny(0.)) return          ! gamma=0. therefore Cv undefined
            k=k_cal(v,t,eos,p=p)
            cv=Alpha_Cal(P,T,eos)*k/Get_Grun_th(p,t,eos) * v
            ! scaling when getting Cv from other params
            ! if (index(U_case(eos%pscale_name),'GPA') > 0)  factor=1.0E9
            ! if (index(U_case(eos%pscale_name),'KBAR') > 0) factor=1.0E8
            ! if (VscaleMGD(eos)) factor=factor*1.0E-6     !test for cm3/mol or equivalent in eos%vscale_name
            cv=cv/EPthermal_factor(eos)
            return        ! this approach not compatible with mode calculations
      end select

      !> Extra oscillators: only allowed in combination with models 6,7,8
      if (eos%osc_allowed .and. sum(eos%iosc) > 0) then
         cvpart(0)=(1._cp-eos%params(40)-eos%params(45))*cvpart(0)     ! partial contribution main oscillator

         do i=1,2
            select case(eos%iosc(i))
               case(0)
                  cycle

               case(1) !> DEBYE
                  thetaD=get_DebyeT(V,Eos,i)
                  gammaV=get_grun_V(V,Eos,i)
                  x=thetaD/t
                  if (x < huge(0.))  &
                     cvpart(i)=eos%params(35+5*i)*3.0_cp*eos%params(13)*8.314_cp * (4.0_cp*debye(3,x) -3.0_cp*x/(exp(x)-1))

               case(2) !> Einstein
                  x=get_DebyeT(V,Eos,i)/t
                  if (x < 20)     &        !corresponds to 0.05ThetaE where Cv < 0.00002 J/mol/K
                     cvpart(i)=eos%params(35+5*i)*3.0_cp*eos%params(13)*8.314_cp * x**2._cp * exp(x)/(exp(x)-1)**2._cp

            end select
         end do
      end if

      !> Now return requested part of pth:
      if (jo == -1)then
         cv=sum(cvpart)
      else
         cv=cvpart(jo)
      end if

   End Function Get_Cv

End SubModule EoS_Get_HeatCap