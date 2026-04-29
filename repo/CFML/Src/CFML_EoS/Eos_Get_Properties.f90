!!----
!!----
!!----
SubModule (CFML_EoS) EoS_Get_Property
   implicit none
   Contains

   !!----
   !!---- FUNCTION GET_PROPERTY_X
   !!----
   !!---- Returns the ...
   !!----
   !!---- Date: 16/03/2017
   !!
   Module Function Get_Property_X(P, T, Eos, Xtype) Result(Val)
      !---- Arguments ----!
      real(kind=cp),     intent(in) :: P       ! Pressure
      real(kind=cp),     intent(in) :: T       ! Temperature
      type(Eos_Type),    intent(in) :: EoS     ! Eos Parameter
      integer, optional, intent(in) :: xtype   ! =0 when X=V, =1 for X=K (isothermal)
      real(kind=cp)                 :: Val

      !---- Local Variables ----!
      integer       :: itype
      real(kind=cp) :: vol, agt

      !> Init
      itype=0               ! default
      if (present(xtype)) itype=xtype

      select case(itype)
         case(0)                 ! Volume
            val=Get_Volume(P,T,Eos)

         case(1)                 ! Isothermal modulus
            vol=Get_Volume(P,T,Eos)
            val=K_Cal(Vol,T,Eos,p=p)

         case(2)                 ! Adiabatic modulus
            vol=Get_Volume(P,T,Eos)
            agt=Alpha_Cal(P,T,Eos)*get_grun_th(P,T,eos)*T        ! Get_Grun knows about linear
            if (eos%linear) agt=3.0_cp*agt
            val=(1.0_cp+agt)*K_Cal(Vol,T,Eos,p=p)

         case default
            val=0.0_cp
      end select

   End Function Get_Property_X

   !!----
   !!---- FUNCTION GET_PROPS_GENERAL
   !!----
   !!---- Returns elastic properties input P,T for a general direction axis in cell_eos
   !!---- Use Eos_Cal when the eos for an axis is known
   !!----
   !!---- Date: 09/09/2020
   !!
   Module Function Get_Props_General(P, T, Cell_Eos, Axis) Result(Parvals)
      !---- Arguments ----!
      real(kind=cp),      intent(in)  :: p,T
      type(eos_cell_type),intent(in)  :: cell_eos
      type(axis_type),    intent(in)  :: axis
      real(kind=cp), dimension(6)     :: parvals

      !---- Local Variables ----!

      !>init
      parvals=0.0_cp

      parvals(1)=get_Volume_general(P,T,cell_eos,axis)      ! length
      parvals(2)=get_mod_general(P,T,cell_eos,axis)         ! M
      parvals(3)=get_modp_general(P,T,cell_eos,axis)        ! Kp
      parvals(5)=get_dMdT_general(P,T,cell_eos,axis)        ! dK/dT
      parvals(6)=get_alpha_general(P,T,cell_eos,axis)       ! alpha

   End Function Get_Props_General

   !!----
   !!---- FUNCTION GET_PROPS_THIRD
   !!----
   !!---- Returns elastic properties input P,T for principal axis ieos in cell_eos
   !!---- when it can calculated from other eos
   !!---- Use Eos_Cal when the eos for an axis is known
   !!----
   !!---- Date: 09/09/2020
   !!----
   Module Function Get_Props_Third(P, T, Cell_Eos, Ieos) Result(Parvals)
      !---- Arguments ----!
      real(kind=cp),             intent(in)  :: p,T
      type(eos_cell_type),       intent(in)  :: cell_eos
      integer,                   intent(in)  :: ieos     ! the axis (1,2,3) or V (0) to be calculated
      real(kind=cp), dimension(6)            :: parvals

      !---- Local Variables ----!

      !>init
      parvals=0.0_cp

      parvals(1)=get_volume_third(P,T,cell_eos,ieos)       ! V
      parvals(2)=get_mod_third(P,T,cell_eos,ieos)          ! K
      parvals(3)=get_modp_third(P,T,cell_eos,ieos)         ! Kp
      parvals(5)=get_dMdT_third(P,T,cell_eos,ieos)         ! dK/dT
      parvals(6)=get_alpha_third(P,T,cell_eos,ieos)        ! alpha

   End Function Get_Props_Third

   !!--++
   !!--++ FUNCTION GET_PROPS_PTVTABLE
   !!--++
   !!--++ Returns the requested property at two of P,T,V from an eos loaded as a table of values
   !!--++ The volume values in the table are expected to be scaled to V=1.0 at Pref, Tref
   !!--++ and the true V0 is in eospar%params(1)
   !!--++
   !!--++ Date: 14/10/16
   !!
   Module Function Get_Props_PTVTable(P, T, V, Eos, Res) Result(Val)
      !---- Arguments ----!
      real(kind=cp),    intent(in) :: P       ! Pressure
      real(kind=cp),    intent(in) :: T       ! Temperature
      real(kind=cp),    intent(in) :: V       ! Volume
      type(Eos_Type),   intent(in) :: EoS     ! Eos Parameter
      character(len=*), intent(in) :: Res     ! Parameter requested for calculation (P,T, or V)
      real(kind=cp)                :: Val

      !---- Local Variables ----!
      logical                   :: pmin,pmax,tmin,tmax      !.true. if requested PT lays off table in direction indicated by name
      integer                   :: i,j
      real(kind=cp)             :: VV0,tt,vm,vp,km,kp,va,vb,dvdp,dvdt
      type(Eos_Type)            :: eosm,eosp,eosv     ! Eos for local Murn at Tminus and Tplus
      character(len=10)         :: Var
      !character(len=60)         :: text

      !>Init
      Val=0.0_cp

      pmin=.false.
      pmax=.false.
      tmin=.false.
      tmax=.false.

      VV0=V/EoS%params(1)        ! table values are all V/V0

      !> Determine what is the request: P,T,V, K, KP, AL
      Var=Res
      Var=U_Case(adjustl(Var))

      !> Check valid request
      if (EoS%imodel/= -1) then
         call set_error(1,' ')
         write(err_CFML%Msg,'(''Request to get_props_pvttable with invalid imodel #'',i5)')EoS%imodel
         return
      end if

      !> Find upper-corner coords for P,T if in table (works because P and T always ascend) else set error flags
      ! Only test if P supplied in argument, not if P requested from T and V
      if (Var(1:1) /= 'P') then
         if (p < EoS%table%pmin) then
            pmin=.true.
            i=1
         else if(p > EoS%table%pmax) then
            pmax=.true.
            i=EoS%table%np
         else
            !> Inside the table
            do i=1,EoS%table%np
               if (p+0.0001 < EoS%table%ptv(i,1,1)) exit       !0.0001 required to avoid rounding error if p comes from steps in pvcal
            end do
            if (i > EoS%table%np)i=EoS%table%np
         end if
      end if

      if (Var(1:1) /= 'T') then                         ! Only test if T supplied in argument, not if T requested from P and V
         if (t < EoS%table%tmin) then
            tmin=.true.
            j=1
         else if(t > EoS%table%tmax) then
            tmax=.true.
            j=EoS%table%nt
         else
            !> Inside the table
            do j=1,EoS%table%nt
               if (t < EoS%table%ptv(1,j,2) ) exit
            end do
            if(j > EoS%table%nt)j=EoS%table%nt
         end if

      else      ! T is requested, V must be present, and the row of P is already known
         if (VV0 < eos%table%ptv(i,1,3))then
            tmin=.true.       !V is smaller than V at minT, so T is lower than the table edge. Assmes alpha > 0
         else if(VV0 > eos%table%ptv(i,EoS%table%nt,3))then
            tmax=.true.
         else
            !> Inside the table
            do j=1,EoS%table%nt
               if (VV0 < EoS%table%ptv(i,j,3) ) exit
            end do
            if (j > EoS%table%nt)j=EoS%table%nt
         end if
      end if

      if (Var(1:1) == 'P') then                         ! P requested from T and V: we already know j, from the T
         if (VV0 > eos%table%ptv(1,j,3)) then
            pmin=.true.       !V is bigger than V at minP, so P is lower than the table edge.
         else if(VV0 < eos%table%ptv(EoS%table%np,j,3)) then
            pmax=.true.
         else
            !> Inside the table
            do i=1,EoS%table%np -1
               if (VV0 > EoS%table%ptv(i,j,3) ) exit
            end do
            if (i > EoS%table%np)i=EoS%table%np
         end if
      end if

      !> Now we have either outside table flags set true, or we have the i and j values
      !  Set the i and j to min/max if off the edges
      if (pmin) i=1
      if (pmax) i=EoS%table%np

      if (tmin) j=1
      if (tmax) j=Eos%table%nt

      !> Now set warnings and errors for off table
      if ( (pmin .or. pmax) .and. (tmin .or. tmax) ) then       !off diagonally from a corner. No calculation
         call set_error(1,' ')
         select case(Var(1:1))
            case default
               write(err_CFML%Msg,&
                    '(''PTV table request for P = '',f6.2,'' at T ='',f6.1,'' Outside table'')') p, t
               val=eos%table%ptv(i,j,3) ! This is V, will be wrong for K or alpha, but should not matter

           case('P')
              write(err_CFML%Msg,&
                    '(''PTV table request for V = '',f8.3,'' at T ='',f6.1,'' Outside table'')')v,t
              val=eos%table%ptv(i,j,1)

           case('T')
              write(err_CFML%Msg,&
                    '(''PTV table request for P = '',f6.2,'' at V ='',f8.3,'' Outside table'')')p,v
              val=eos%table%ptv(i,j,2)
         end select
         return

      else if(pmin .or. pmax .or. tmin .or. tmax) then
         call set_error(-1,' ')
         select case(Var(1:1))
            case default
               write(err_CFML%Msg,&
                    '(''PTV table request for P = '',f6.2,'' at T ='',f6.1,'' outside one table edge: linear extrapolation made'')')p,t

            case('P')
               write(err_CFML%Msg,&
                    '(''PTV table request for V = '',f8.3,'' at T ='',f6.1,'' outside one table edge: linear extrapolation made'')')v,t

            case('T')
               write(err_CFML%Msg,&
                    '(''PTV table request for P = '',f6.2,'' at V ='',f8.3,'' outside one table edge: linear extrapolation made'')')p,v
         end select
      end if

      !> New version of 12 May 2022: Do extrapolation for off one edge by seperate code
      if (pmin .or. pmax) then        !beyond edge in pressure
         if (pmin) i=2

         if (Var(1:1) /= 'T') then
            tt=(t-EoS%table%ptv(1,j-1,2))/(EoS%table%ptv(1,j,2)-EoS%table%ptv(1,j-1,2))  !tt is (T-Tm)/(Tp-Tm)
            vm=tt*(EoS%table%ptv(i-1,j,3)-EoS%table%ptv(i-1,j-1,3)) +EoS%table%ptv(i-1,j-1,3)  !V at T and Pm
            vp=tt*(EoS%table%ptv(i,j,3)-EoS%table%ptv(i,j-1,3)) +EoS%table%ptv(i,j-1,3)        !V at T and Pp
            dvdp=(vp-vm)/(eos%table%ptv(i,j,1)-eos%table%ptv(i-1,j,1))       !dV/dP
         end if

         Select case(Var(1:3))
            case('V  ') !from P and T
               val=vp+(p-eos%table%ptv(i,j,1))*dvdp
               val=val*EoS%params(1)

            case('P  ') !from V and T
               val = (vv0-vp)/dvdp +  eos%table%ptv(i,j,1)

            case('T  ')    !from P and V
               Vm=EoS%table%ptv(i-1,j-1,3)+(P-EoS%table%ptv(i-1,j-1,1))*(EoS%table%ptv(i-1,j-1,3)-EoS%table%ptv(i,j-1,3))/(EoS%table%ptv(i-1,j-1,1)-EoS%table%ptv(i,j-1,1)) !V at P and T-
               Vp=EoS%table%ptv(i-1,j,3)+(P-EoS%table%ptv(i-1,j,1))*(EoS%table%ptv(i-1,j,3)-EoS%table%ptv(i,j,3))/(EoS%table%ptv(i-1,j,1)-EoS%table%ptv(i,j,1)) !V at P and T+
               val=EoS%table%ptv(i,j-1,2) + (VV0-Vm)*(EoS%table%ptv(i,j,2)-EoS%table%ptv(i,j-1,2))/(Vp-Vm)  !Interpolate in T

            case('K  ') !input must be p and t
               val=-vp/dvdp - p +  eos%table%ptv(i,j,1)

            case('KP ')
               val=-1.0

            case('AL ') !approximated as alpha at edge
               if (pmin) i=1
               Vp=EoS%table%ptv(i,j,3)
               Vm=EoS%table%ptv(i,j-1,3)
               val=2.0_cp*(Vp-Vm)/(EoS%table%ptv(i,j,2)-EoS%table%ptv(i,j-1,2))/(Vm+Vp)

         end select

      else if (tmin .or. tmax) then        !beyond edge in temperature
         if (tmin) j=2
         EoSM= Murn_Ptv_table(i-1,j-1,EoS)       !EoS at edge if T < Tmin, or one below edge if T > Tmax
         EoSp= Murn_Ptv_table(i-1,j,EoS)         !Eos at edge if T > Tmax, or one above edge if T < Tmin but use this as reference always

         Select case(Var(1:3))
            case('V  ') !from P and T
               Vm=get_volume(p-EoS%table%ptv(i-1,j-1,1),eosm%tref,eosm)    !V at P and T(j)
               Vp=get_volume(p-EoS%table%ptv(i-1,j,1),eosp%tref,eosp)      !V at P and T(j-1)
               dvdt=(vp-vm)/(eos%table%ptv(i,j,2)-eos%table%ptv(i,j-1,2))       !dV/dT
               val=vp+(t-eos%table%ptv(i,j,2))*dvdt
               val=val*EoS%params(1)

            case('P  ') !from V and T
               Vm=EoS%table%ptv(i-1,j,3)+(T-EoS%table%ptv(i-1,j,2))*(EoS%table%ptv(i-1,j,3)-EoS%table%ptv(i-1,j-1,3))/(EoS%table%ptv(i-1,j,2)-EoS%table%ptv(i-1,j-1,2)) !V at T and P-
               Vp=EoS%table%ptv(i,j,3)+(T-EoS%table%ptv(i,j,2))*(EoS%table%ptv(i,j,3)-EoS%table%ptv(i,j-1,3))/(EoS%table%ptv(i,j,2)-EoS%table%ptv(i,j-1,2))      !V at T and P+
               val=EoS%table%ptv(i-1,j,1) + (VV0-Vm)*(EoS%table%ptv(i,j,1)-EoS%table%ptv(i-1,j,1))/(Vp-Vm)  !Interpolate in T

            case('T  ')    !from P and V
               Vm=get_volume(p-EoS%table%ptv(i-1,j-1,1),eosm%tref,eosm)    !V at P and T(j)
               Vp=get_volume(p-EoS%table%ptv(i-1,j,1),eosp%tref,eosp)      !V at P and T(j-1)
               dvdt=(vp-vm)/(eos%table%ptv(i,j,2)-eos%table%ptv(i,j-1,2))       !dV/dT at P
               val = (vv0-vp)/dvdt + eos%table%ptv(i,j,2)

            case('K  ') !input must be p and t
               km=get_k(p-EoS%table%ptv(i-1,j-1,1),eosm%tref,eosm)      ! K at Tminus
               kp=get_k(p-EoS%table%ptv(i-1,j,1),eosp%tref,eosp)          ! K at Tplus
               tt=(t-EoS%table%ptv(1,j-1,2))/(EoS%table%ptv(1,j,2)-EoS%table%ptv(1,j-1,2))
               val=(kp-km)*tt + km

            case('KP ')
               km=get_kp(p-EoS%table%ptv(i-1,j-1,1),eosm%tref,eosm)      ! Kp at Tminus
               kp=get_kp(p-EoS%table%ptv(i-1,j,1),eosp%tref,eosp)          ! Kp at Tplus
               tt=(EoS%table%ptv(1,j,2)-EoS%table%ptv(1,j-1,2))/(EoS%table%ptv(1,j,2)-EoS%table%ptv(1,j-1,2))
               val=(kp-km)*tt + km

            case('AL ') !approximated as alpha at edge
               Vm=get_volume(p-EoS%table%ptv(i-1,j-1,1),eosm%tref,eosm)    !V at P and T(j)
               Vp=get_volume(p-EoS%table%ptv(i-1,j,1),eosp%tref,eosp)      !V at P and T(j-1)
               dvdt=(vp-vm)/(eos%table%ptv(i,j,2)-eos%table%ptv(i,j-1,2))       !dV/dT
               val= dvdt/vp

         end select

      else !within table
         !> Get the Murngahan EoS from Pminus at Tminus and Tplus, but set as if a T=Tref
         !> Must then call Eos functions with Tref as argument
         if (Var(1:1) /= 'P') then       ! cannot do this if P being requested
            EoSM= Murn_Ptv_table(i-1,j-1,EoS)
            EoSp= Murn_Ptv_table(i-1,j,EoS)
         end if

         select case(Var(1:3))
            case('V  ')
               !> Murnaghan interpolation on P axis, followed by linear in T
               Vm=get_volume(p-EoS%table%ptv(i-1,j-1,1),eosm%tref,eosm)
               Vp=get_volume(p-EoS%table%ptv(i-1,j,1),eosp%tref,eosp)
               val=Vm+(Vp-Vm)*(t-EoS%table%ptv(1,j-1,2))/(EoS%table%ptv(1,j,2)-EoS%table%ptv(1,j-1,2))  ! linear T
               val=val*EoS%params(1)

            case('P  ')
               tt=(t-EoS%table%ptv(1,j-1,2))/(EoS%table%ptv(1,j,2)-EoS%table%ptv(1,j-1,2))
               do i=1,EoS%table%np
                  vb=va
                  va=tt*(EoS%table%ptv(i,j,3)-EoS%table%ptv(i,j-1,3)) +EoS%table%ptv(i,j-1,3)      ! volume at each P row for target T
                  if (va < vv0) exit
               end do

               !> Set up Murn in Eosm
               call Init_EoS_Type(Eosv)
               eosv%imodel=1
               call set_eos_names(eosv)      ! sets names for params
               eosv%title='Murnaghan for interpolation'
               eosv%params(1)=vb
               EoSM= Murn_Ptv_table(i-1,j-1,EoS)
               EoSP= Murn_Ptv_table(i-1,j,EoS)
               eosv%params(2)=eosm%params(2)+(eosp%params(2)-eosm%params(2))*tt
               eosv%params(3)=eosm%params(3)+(eosp%params(3)-eosm%params(3))*tt
               val=get_pressure(vv0,eosv%tref,eosv)+EoS%table%ptv(i-1,j,1)

            case('T  ')
               Vm=get_volume(p-EoS%table%ptv(i-1,j-1,1),eosm%tref,eosm)   !V at T(j-1) and P
               Vp=get_volume(p-EoS%table%ptv(i-1,j,1),eosp%tref,eosp)     !V at T(j) and P
               Val=EoS%table%ptv(i,j-1,2)  + (VV0-Vm)*(EoS%table%ptv(i,j,2)-EoS%table%ptv(i,j-1,2))/(Vp-Vm)    !Linear interpolation in T at P

            case('K  ') !input must be p and t
               km=get_k(p-EoS%table%ptv(i-1,j-1,1),eosm%tref,eosm)      ! K at Tminus
               kp=get_k(p-EoS%table%ptv(i-1,j,1),eosp%tref,eosp)          ! K at Tplus
               tt=(t-EoS%table%ptv(1,j-1,2))/(EoS%table%ptv(1,j,2)-EoS%table%ptv(1,j-1,2))
               val=(kp-km)*tt + km

            case('KP ')
               tt=(t-EoS%table%ptv(1,j-1,2))/(EoS%table%ptv(1,j,2)-EoS%table%ptv(1,j-1,2))
               val=eosm%params(3)+(eosp%params(3)-eosm%params(3))*tt

            case('AL ')
               !> Murnaghan interpolation on P axis, followed by linear in T
               Vm=get_volume(p-EoS%table%ptv(i-1,j-1,1),eosm%tref,eosm)
               Vp=get_volume(p-EoS%table%ptv(i-1,j,1),eosp%tref,eosp)
               val=2.0_cp*(Vp-Vm)/(EoS%table%ptv(i-1,j,2)-EoS%table%ptv(i-1,j-1,2))/(Vm+Vp)

            case default
               call set_error(1,' ')
               write(err_CFML%Msg,'(''Request for '',a1,'' to get_props_pvttable not valid'')')Var(1:1)

         end select
      end if

   End Function Get_Props_PTVTable

End SubModule EoS_Get_Property