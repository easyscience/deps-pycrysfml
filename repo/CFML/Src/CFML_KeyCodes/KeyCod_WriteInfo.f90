!!
Submodule (CFML_KeyCodes) KeyCod_WriteInfo
   implicit none

   Contains

   !!----
   !!---- Subroutine WriteInfo_Restraints
   !!----
   !!----    Write the current values of the "observed" and calculated
   !!----    restraints.
   !!----
   !!---- Update: April - 2022
   !!
   Module Subroutine WriteInfo_Restraints(RDis, RAng, RTor, IPhase, AtList, Calc, Iunit)
      !---- Arguments ----!
      type(RestList_Type), intent(in) :: RDis
      type(RestList_Type), intent(in) :: RAng
      type(RestList_Type), intent(in) :: RTor
      integer,             intent(in) :: IPhase
      type(AtList_Type),   intent(in) :: AtList
      logical, optional,   intent(in) :: Calc
      integer, optional,   intent(in) :: iunit

      !---- Local variables ----!
      character(len=14) :: car1,car2,car3,car4
      integer           :: i,i1,i2,i3,i4,lun
      real              :: disto,distc,ango,angc,sigm, cost,w, delta
      logical           :: done

      !> Init
      lun=6
      if (present(iunit)) lun=iunit

      done=.false.
      if (present(calc)) done=calc

      !> -------------------
      !> Distance Restraints
      !> -------------------
      if (RDis%NRel > 0 .and. any(RDis%RT(:)%IPh(1) == Iphase)) then

         write(unit=lun,fmt="(/,a)") " ============================================================"
         write(unit=lun,fmt="(a)")   "   Distance Restraints: Atoms, Dobs, Dcalc, Sigma, delt/Sigma"
         write(unit=lun,fmt="(a,/)") " ============================================================"
         write(unit=lun,fmt="(a)") &
              " Rest#    Atom1         Atom2              Dobs        Dcalc       Sigma   (Do-Dc)/Sigma"

         cost=0.0_cp
         w=1.0_cp
         do i=1,RDis%NRel
            i1=RDis%RT(i)%p(1)
            i2=RDis%RT(i)%p(2)
            car1=trim(Atlist%Atom(i1)%lab)
            car2=trim(Atlist%Atom(i2)%lab)
            if (len_trim(RDis%RT(i)%code(1)) >0) car2=trim(car2)//'_'//trim(RDis%RT(i)%code(1))

            disto=RDis%RT(i)%obs
            distc=RDis%RT(i)%cal
            delta=disto-distc
            sigm=RDis%RT(i)%sig
            if (done) w= 1.0_cp/(sigm*sigm)
            cost= cost+delta*delta*w
            write(unit=lun,fmt="(i6,tr4,2a,4f12.5)") i,car1,car2,disto,distc,sigm,delta/sigm
         end do
         if (done) write(unit=lun,fmt="(/,a,f12.5)") "   Distance Restraints Cost = Sum{[(Dobs-Dcalc)/Sigma]^2} = ",cost
      end if

      !> ----------------
      !> Angle Restraints
      !> ----------------
      if (RAng%NRel > 0 .and. any(RAng%RT(:)%IPh(1) == Iphase)) then
         write(unit=lun,fmt="(/,a)") " ============================================================="
         write(unit=lun,fmt="(a)")   "   Angle Restraints: Atoms, Angobs, Angcalc, Sigma, delt/Sigma"
         write(unit=lun,fmt="(a,/)") " ============================================================="
         write(unit=lun,fmt="(a)") &
              " Rest#    Atom1         Atom2          Atom3            Ang_obs    Ang_calc      Sigma   (Ao-Ac)/Sigma"

         cost=0.0_cp
         w=1.0_cp
         do i=1,RAng%NRel
            i1=RAng%RT(i)%p(1)
            i2=RAng%RT(i)%p(2)
            i3=RAng%RT(i)%p(3)
            car1=trim(Atlist%Atom(i1)%lab)

            car2=trim(Atlist%Atom(i2)%lab)
            if (len_trim(RAng%RT(i)%code(1)) >0) car2=trim(car2)//'_'//trim(RAng%RT(i)%code(1))
            car3=trim(Atlist%Atom(i3)%lab)
            if (len_trim(RAng%RT(i)%code(2)) >0) car3=trim(car3)//'_'//trim(RAng%RT(i)%code(2))

            ango=RAng%RT(i)%obs
            angc=RAng%RT(i)%cal
            delta=ango-angc
            sigm=RAng%RT(i)%sig
            if (done) w= 1.0_cp/(sigm*sigm)
            cost= cost+delta*delta*w
            write(unit=lun,fmt="(i6,tr4,3a,4f12.5)") i,car1,car2,car3,ango,angc,sigm,delta/sigm
         end do
         if (done) write(unit=lun,fmt="(/,a,f12.5)") "   Angle Restraints Cost = Sum{[(Ang_obs-Ang_calc)/Sigma]^2} = ",cost
      end if

      !> -------------------------
      !> Torional Angle Restraints
      !> -------------------------
      if (RTor%NRel > 0 .and. any(RTor%RT(:)%IPh(1) == Iphase)) then
         write(unit=lun,fmt="(/,a)") " ====================================================================="
         write(unit=lun,fmt="(a)")   "   Torsion Angle Restraints: Atoms, Angobs, Angcalc, Sigma, delt/Sigma"
         write(unit=lun,fmt="(a,/)") " ====================================================================="
         write(unit=lun,fmt="(a)") " Rest#    Atom1         Atom2          Atom3          Atom4            "//&
              "Ang_obs    Ang_calc      Sigma   (Ang_obs-Ang_calc)/Sigma"

         cost=0.0_cp
         w=1.0_cp
         do i=1,RTor%NRel
            i1=RTor%RT(i)%p(1)
            i2=RTor%RT(i)%p(2)
            i3=RTor%RT(i)%p(3)
            i4=RTor%RT(i)%p(4)
            car1=trim(Atlist%Atom(i1)%lab)

            car2=trim(Atlist%Atom(i2)%lab)
            if (len_trim(RTor%RT(i)%code(1)) >0) car2=trim(car2)//'_'//trim(RTor%RT(i)%code(1))
            car3=trim(Atlist%Atom(i3)%lab)
            if (len_trim(RTor%RT(i)%code(2)) >0) car3=trim(car3)//'_'//trim(RTor%RT(i)%code(2))
            car4=trim(Atlist%Atom(i4)%lab)
            if (len_trim(RTor%RT(i)%code(3)) >0) car4=trim(car4)//'_'//trim(RTor%RT(i)%code(3))

            ango=RTor%RT(i)%obs
            angc=RTor%RT(i)%cal
            delta=ango-angc
            sigm=RTor%RT(i)%sig
            if (done) w= 1.0_cp/(sigm*sigm)
            cost= cost+delta*delta*w
            write(unit=lun,fmt="(i6,tr4,4a,4f12.5)") i,car1,car2,car3,car4,ango,angc,sigm,delta/sigm
         end do
         if (done) write(unit=lun,fmt="(/,a,f12.5)") "   Torsion Angle Restraints Cost = Sum{[(Ang_obs-Ang_calc)/Sigma]^2} = ",cost
      end if

   End Subroutine WriteInfo_Restraints

   !!----
   !!---- Subroutine WriteInfo_Constraints
   !!----
   !!---- Update: April - 2022
   !!
   Module Subroutine WriteInfo_Constraints(AtList, Iunit)
      !---- Arguments ----!
      type(AtList_Type), intent(in) :: AtList
      integer, optional, intent(in) :: Iunit

      !---- Local Variables ----!
      !integer           :: i,j,k,m,n,np,nmax
      integer           :: lun
      character(len=20) :: car
      character(len=60) :: fmt1

      !> Format Zone
      fmt1="(t10,i3,t21,a,t31,a,t39,f8.4)"

      !> Init
      lun=6
      if (present(iunit)) lun=iunit

      if (NP_Constr <= 0) return

      write(unit=lun, fmt="(a)") " "
      write(unit=lun, fmt="(a,i5)") " => Constraints relations: ",NP_Constr
      write(unit=lun, fmt="(a)") " "
      write(unit=lun, fmt="(a)") "       N.Constr     Name      Father    Factor"
      write(unit=lun, fmt="(a)") "    ============================================="

   End Subroutine WriteInfo_Constraints

End SubModule KeyCod_WriteInfo

