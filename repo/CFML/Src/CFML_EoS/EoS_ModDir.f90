!!----
!!----
!!----
SubModule (CFML_EoS) EoS_ModDir
   implicit none
   Contains

   !!----
   !!---- FUNCTION GET_MOD_AXIS
   !!----
   !!---- Returns the value of modulus of principal axis (ieos) in unit
   !!---- cell in cell_eos at P,T
   !!----
   !!---- Call this Function directly when the calling routine  knows that
   !!---- the direction is a principal axis
   !!----
   !!---- Date: 09/09/2020
   !!
   Module Function Get_Mod_Axis(P, T, Cell_eos, Ieos) result(Modu)
      !---- Arguments ----!
      real(kind=cp),      intent(in)  :: p,T
      type(eos_cell_type),intent(in)  :: cell_eos
      integer,            intent(in)  :: ieos      !axis indicator, as in axis_type%ieos
      real(kind=cp)                   :: modu      !returned modulus

      !---- Local Variables ----!

      !>init
      modu=0.0_cp

      select case(cell_eos%loaded(ieos))
         case(1)
            modu=get_k(p,t,cell_eos%eos(ieos))

         case(2) ! sym equiv. Always uses eos(1) for a-axis
            modu=get_k(p,t,cell_eos%eos(1))

         case(3)
            modu=get_mod_third(p,T,cell_eos,ieos)

         case(4)
            modu=get_k(p,t,cell_eos%eos(cell_eos%unique))
      end select

      return
   End Function Get_Mod_Axis

   !!----
   !!---- FUNCTION GET_MOD_CELL
   !!----
   !!---- Returns the value of modulus of any axis in unit cell in cell_eos at P,T
   !!---- Call this Function when the calling routine does not know if the
   !!---- direction is a principal axis or not
   !!----
   !!---- If a principal direction is requested, only axis%ieos is required
   !!---- axis%v and axis%atype only used if axis%ieos=-2
   !!----
   !!---- Date: 09/09/2020
   !!
   Module Function Get_Mod_Cell(P, T, Cell_eos, Axis) Result(Modu)
      !---- Arguments ----!
      real(kind=cp),      intent(in)  :: p,T
      type(eos_cell_type),intent(in)  :: cell_eos
      type(axis_type),    intent(in)  :: axis
      real(kind=cp)                   :: modu !returned modulus

      !---- Local Variables ----!

      !> init
      modu=0.0_cp

      select case(axis%ieos)      !invalid numbers just return
         case(0:6)   !principal direction for which eos exists, or can be calculated
            modu=get_mod_axis(p,t,cell_eos,axis%ieos)

         case(-2)   !general direction
            modu=get_mod_general(p,T,cell_eos,axis)
      end select

      return
   End Function Get_Mod_Cell

   !!--++
   !!--++ FUNCTION GET_MOD_GENERAL
   !!--++
   !!--++ Returns the value of modulus of any axis in unit cell in cell_eos at P,T
   !!--++
   !!--++ Date: 09/09/2020
   !!
   Module Function Get_Mod_General(P, T, Cell_eos, Axis) Result(Modu)
      !---- Arguments ----!
      real(kind=cp),      intent(in)  :: p,T
      type(eos_cell_type),intent(in)  :: cell_eos
      type(axis_type),    intent(in)  :: axis
      real(kind=cp)                   :: modu

      !---- Local Variables ----!
      integer         :: i
      real(kind=cp)   :: k,kmax,pstep,pcal,vm,vp

      !> for spline
      integer,parameter              :: NSTEP=11   !must be odd
      integer                        :: imid
      real(kind=cp),dimension(NSTEP) :: x,y,dy !,d2y

      !> find largest linear modulus of axes
      kmax=tiny(0._cp)
      do i=1,3
         k=get_mod_axis(P,T,cell_eos,i)
         if (k > kmax) kmax=k
      end do

      !> now do pstep as kmax/1000.  Seems good compromise from tests - also depends on nstep
      pstep=kmax/500.

      !> approximate modulus
      vm=get_Volume_cell(P-0.5_cp,T,cell_eos,axis)
      vp=get_Volume_cell(P+0.5_cp,T,cell_eos,axis)
      K=(vm+vp)/2.0_cp/(vm-vp)
      pstep=K/500._cp  !should give delv of ca 1%

      pcal=p-int(NSTEP/2)*pstep
      do i=1,NSTEP
         x(i)=pcal
         y(i)=get_Volume_cell(Pcal,T,cell_eos,axis)
         pcal=pcal+pstep
      end do

      !call Second_Derivative(x, y, NSTEP, d2y)
      dy=First_Derivative(x, y, NSTEP)

      imid=int(NSTEP/2) + 1
      Modu=-1.0*y(imid)/dy(imid)

   End Function  Get_Mod_General

   !!--++
   !!--++ FUNCTION GET_MOD_THIRD
   !!--++
   !!--++ Returns the value of modulus of a principal axis ieos in unit
   !!--++ cell in cell_eos at P,T when it can be calculated from others
   !!--++
   !!--++ Date: 09/09/2020
   !!
   Module Function Get_Mod_Third(P, T, Cell_eos, Ieos) Result(Modu)
      !---- Arguments ----!
      real(kind=cp),       intent(in) :: p,T
      type(eos_cell_type), intent(in) :: cell_eos
      integer,             intent(in) :: ieos     ! the modulus of the axis (1,2,3) or V (0) to be calculated
      real(kind=cp)                   :: modu

      !---- Local Variables ----!
      integer       :: i
      real(kind=cp) :: beta_ang

      !>init
      modu=0.0_cp

      !>safety check: if mono or triclinic, should only be called if angle poly used
      if (U_case(cell_eos%system(1:4)) == 'TRIC' .or. U_case(cell_eos%system(1:3)) == 'MONO')then
         if (cell_eos%eosang%iangle == 0) then
            call set_error(1,'Get_Mod_Third called for mono or triclinic, without angle poly set')
         end if
      end if

      select case(U_case(cell_eos%system(1:4)))
         case('TRIC','MONO','ORTH')
            beta_ang=Get_Angle_Volfactor_Deriv(P,T,cell_eos,'P')/Get_Angle_Volfactor(P,T,cell_eos)  !1/A dA/dP
            select case(ieos)
               case(0)
                  modu=  1.0_cp/(1.0_cp/Get_K(P,T,cell_eos%eos(1))+1.0_cp/Get_K(P,T,cell_eos%eos(2))+ &
                         1.0_cp/Get_K(P,T,cell_eos%eos(3))- beta_ang)

               case default
                  modu=  1.0_cp/Get_K(P,T,cell_eos%eos(0))
                  do i=1,3
                     if (i == ieos)cycle
                     modu= modu - 1.0_cp/Get_K(P,T,cell_eos%eos(i))
                  end do
                  modu=modu+beta_ang
                  modu=1.0_cp/modu
            end select

         case('TRIG','HEXA','TETR')
            select case(ieos)
               case(0)     ! calc V from a and c
                  modu= 1.0_cp/(2.0_cp/Get_K(P,T,cell_eos%eos(1))+1.0_cp/Get_K(P,T,cell_eos%eos(3)))

               case(1)     ! a from V and c
                  modu= 2.0_cp/(1.0_cp/Get_K(P,T,cell_eos%eos(0))-1.0_cp/Get_K(P,T,cell_eos%eos(3)))

               case(3)     ! c from a and V
                  modu= 1.0_cp/(1.0_cp/Get_K(P,T,cell_eos%eos(0))-2.0_cp/Get_K(P,T,cell_eos%eos(1)))
            end select

         case('CUBI','ISOT')
            select case(ieos)
               case(0)     ! calc volume from a
                  modu= Get_K(P,T,cell_eos%eos(1))/3.0_cp

               case(1,2,3)     ! a,b, or c from V
                  modu= Get_K(P,T,cell_eos%eos(0))*3.0_cp
            end select
      end select

   End Function Get_Mod_Third

   !!----
   !!---- FUNCTION GET_MODP_AXIS
   !!----
   !!---- Returns the value of pressure derivative of the modulus of principal axis (ieos)
   !!---- in unit cell in cell_eos at P,T
   !!---- Call this Function directly when the calling routine  knows that the direction is a principal axis
   !!----
   !!---- Date: 09/09/2020
   !!
   Module Function Get_Modp_Axis(P, T, Cell_eos, Ieos) Result(Modp)
      !---- Arguments ----!
      real(kind=cp),      intent(in)  :: p,T
      type(eos_cell_type),intent(in)  :: cell_eos
      integer,            intent(in)  :: ieos      !axis indicator, as in axis_type%ieos
      real(kind=cp)                   :: modp !returned modulus derivative

      !---- Local Variables ----!

      !> init
      modp=0.0_cp

      select case(cell_eos%loaded(ieos))
         case(1)
            modp=get_kp(p,t,cell_eos%eos(ieos))

         case(2) ! sym equiv. Always uses eos(1) for a-axis
            modp=get_kp(p,t,cell_eos%eos(1))

         case(3)
            modp=get_modp_third(p,T,cell_eos,ieos)

         case(4)
            modp=get_kp(p,t,cell_eos%eos(cell_eos%unique))
      end select

   End Function Get_Modp_Axis

   !!----
   !!---- FUNCTION GET_MODP_CELL
   !!----
   !!---- Returns the value of pressure derivative of modulus of any axis in unit
   !!---- cell in cell_eos at P,T
   !!---- Call this Function when the calling routine does not know if the direction
   !!---- is a principal axis or not. If a principal direction is requested, only
   !!---- axis%ieos is required axis%v and axis%atype only used if axis%ieos=-2
   !!----
   !!---- Date: 09/09/2020
   !!
   Module Function Get_Modp_Cell(P, T, Cell_eos, Axis) Result(Modp)
      !---- Arguments ----!
      real(kind=cp),      intent(in)  :: p,T
      type(eos_cell_type),intent(in)  :: cell_eos
      type(axis_type),    intent(in)  :: axis
      real(kind=cp)                   :: modp !returned modulus  derivative

      !---- Local Variables ----!

      !>init
      modp=0.0_cp

      select case(axis%ieos)      !invalid numbers just return
         case(0:6)   !principal direction for which eos exists, or can be calculated
            modp=get_modp_axis(p,t,cell_eos,axis%ieos)

         case(-2)   !general direction
            modp=get_modp_general(p,T,cell_eos,axis)
      end select

   End Function Get_Modp_Cell

   !!--++
   !!--++ FUNCTION GET_MODP_GENERAL
   !!--++
   !!--++ Returns the value of pressure derivative of modulus of any axis in unit cell in cell_eos at P,T
   !!--++
   !!--++ Date: 09/09/2020
   !!
   Module Function Get_Modp_General(P, T, Cell_eos, Axis) Result(Mp)
      !---- Arguments ----!
      real(kind=cp),      intent(in)  :: p,T
      type(eos_cell_type),intent(in)  :: cell_eos
      type(axis_type),    intent(in)  :: axis
      real(kind=cp)                   :: mp

      !---- Local Variables ----!
      integer         :: i
      real(kind=cp)   :: k,kmax,pstep,pcal

      !> for spline
      integer,parameter                 :: NSTEP=11   !must be odd
      integer                           :: imid
      real(kind=cp),dimension(NSTEP)    :: x,y,dy !,d2y

      !> find largest linear modulus of axes
      kmax=tiny(0.0_cp)
      do i=1,3
         k=get_mod_axis(P,T,cell_eos,i)
         if (k > kmax) kmax=k
      end do

      !> Seems good compromise from tests - also depends on nstep
      pstep=kmax/500.
      pcal=p-int(NSTEP/2)*pstep

      do i=1,NSTEP
         x(i)=pcal
         y(i)=get_mod_general(Pcal,T,cell_eos,axis)
         pcal=pcal+pstep
      end do

      !call Second_Derivative(x, y, NSTEP, d2y)
      dy= First_Derivative(x, y, NSTEP)
      imid=int(NSTEP/2) + 1
      Mp=dy(imid)

      return
   End Function Get_Modp_General

   !!--++
   !!--++ FUNCTION GET_MODP_THIRD
   !!--++
   !!--++ Returns the value of pressure derivative of modulus of a principal axis
   !!--++ ieos in unit cell in cell_eos at P,T when it can be calculated from others
   !!--++
   !!--++ Date: 09/09/2020
   !!
   Module Function Get_Modp_Third(P, T, Cell_Eos, Ieos) Result(Modp)
      !---- Arguments ----!
      real(kind=cp),      intent(in) :: p,T
      type(eos_cell_type),intent(in) :: cell_eos
      integer,            intent(in) :: ieos     ! the modulus of the axis (1,2,3) or V (0) to be calculated
      real(kind=cp)                  :: modp

      !---- Local Variables ----!
      real(kind=cp) :: Kp,M1p,M2p,M3p,Mangp,Vf
      integer       :: i

      !> init
      modp=0.0_cp

      !> safety check: if mono or triclinic, should only be called if angle poly used
      if (U_case(cell_eos%system(1:4)) == 'TRIC' .or. U_case(cell_eos%system(1:3)) == 'MONO')then
         if (cell_eos%eosang%iangle == 0) then
            call set_error(1,'Get_Modp_Third called for mono or triclinic, without angle poly set')
         end if
      end if

      select case(U_case(cell_eos%system(1:4)))
         case('TRIC','MONO','ORTH')
            if (U_case(cell_eos%system(1:4)) == 'ORTHO')then
               Mangp=0._cp
            else
               vf=Get_Angle_Volfactor(P,T,cell_eos)
               Mangp=(Get_Angle_Volfactor_Deriv(P,T,cell_eos,'P')**2.0_cp/vf - &
                      Get_Angle_Volfactor_Deriv2(P,T,cell_eos,'P'))/vf
            end if

            select case(ieos)
               case(0)
                  M1p= get_Kp(P,T,cell_eos%eos(1))/Get_K(P,T,cell_eos%eos(1))**2.0_cp
                  M2p= get_Kp(P,T,cell_eos%eos(2))/Get_K(P,T,cell_eos%eos(2))**2.0_cp
                  M3p= get_Kp(P,T,cell_eos%eos(3))/Get_K(P,T,cell_eos%eos(3))**2.0_cp
                  modp=get_mod_third(P,T,cell_eos,0)**2.0_cp*(M1p+M2p+M3p-Mangp)

               case default
                  modp= get_Kp(P,T,cell_eos%eos(0))/Get_K(P,T,cell_eos%eos(0))**2.0_cp
                  do i=1,3
                     if (i == ieos)cycle
                     modp = modp - get_Kp(P,T,cell_eos%eos(i))/Get_K(P,T,cell_eos%eos(i))**2.0_cp
                  end do
                  modp=modp+Mangp
                  modp = get_mod_third(P,T,cell_eos,ieos)**2.0_cp * modp
            end select

         case('TRIG','HEXA','TETR')
            select case(ieos)
               case(0)     ! calc V from a and c
                  M1p= get_Kp(P,T,cell_eos%eos(1))/Get_K(P,T,cell_eos%eos(1))**2.0_cp
                  M3p= get_Kp(P,T,cell_eos%eos(3))/Get_K(P,T,cell_eos%eos(3))**2.0_cp
                  modp=get_mod_third(P,T,cell_eos,0)**2.0_cp*(2.0*M1p+M3p)

               case(1)     ! a from V and c
                  Kp= get_Kp(P,T,cell_eos%eos(0))/Get_K(P,T,cell_eos%eos(0))**2.0_cp
                  M3p= get_Kp(P,T,cell_eos%eos(3))/Get_K(P,T,cell_eos%eos(3))**2.0_cp
                  modp=get_mod_third(P,T,cell_eos,1)**2.0_cp*(Kp-M3p)/2.0

               case(3)     ! c from a and V
                  Kp= get_Kp(P,T,cell_eos%eos(0))/Get_K(P,T,cell_eos%eos(0))**2.0_cp
                  M1p= get_Kp(P,T,cell_eos%eos(1))/Get_K(P,T,cell_eos%eos(1))**2.0_cp
                  modp=get_mod_third(P,T,cell_eos,3)**2.0_cp*(Kp-2.0*M1p)
            end select

         case('CUBI','ISOT')
            select case(ieos)
               case(0)     ! calc volume from a
                  modp=get_Kp(P,T,cell_eos%eos(1))/3.0_cp

               case(1:3)     ! a,b, or c from V
                  modp=get_Kp(P,T,cell_eos%eos(0))*3.0_cp
            end select
      end select

   End Function Get_Modp_Third

End SubModule EoS_ModDir