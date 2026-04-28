  Module cost_magfunctions
      use CFML_GlobalDeps,   only: cp,sp
      use CFML_gSpaceGroups, only: spg_type
      use CFML_strings,      only: l_case, file_type
      use CFML_Atoms,        only: mAtom_List_Type
      use CFML_Metrics,      only: Cell_G_Type
      use CFML_KeyCodes,     only: NP_Max,NP_Refi,v_Vec,v_Shift,v_Bounds,v_BCon,v_Name,v_List, &
                                   VState_to_AtomsPar
      use CFML_kvec_Structure_Factors
      use CFML_kvec_Symmetry

      use prep_input

      implicit none
      private

      public::  General_cost_function, Readn_Set_CostFunctPars, Write_CostFunctPars, Write_FinalCost, &
                Write_SOL_mCFL

      logical,                       public :: err_cost=.false.
      character(len=132),            public :: err_mess_cost=" "
      Integer, parameter,            public :: N_costf=4
      Integer,dimension(0:N_costf),  public :: Icost
      real,   dimension(0:N_costf),  public :: Wcost
      real,allocatable,dimension(:), public :: Pcost

      real,            public :: wavel
      character(len=3),public :: diff_mode="NUC"   ! XRA for x-rays, ELE for electrons

    Contains

!******************************************!
    Subroutine Readn_Set_CostFunctPars(file_cfl)
!******************************************!
       !---- Arguments ----!
       Type(file_type),   intent( in)  :: file_cfl
       !---- Local variables ----!
       character(len=132)   :: line
       integer              :: i,ier,j,i1,i2,i3,i4

       ier=0
       Icost=0; wcost=0.0
       i=0; i1=0; i2=0; i3=0; i4=0

       do j=1,file_cfl%nlines
          line=adjustl(file_cfl%line(j)%Str_tmp)
          line=l_case(line)
          if (len_trim(line) == 0) cycle
          if (line(1:1) =="!" .or. line(1:1) =="#") cycle
          i=index(line,"!")
          if( i /= 0) line=trim(line(1:i-1))

          select case (line(1:4))

              case ("opti")    !Optimization

              i=index(line,"f2mag")
              if(i /= 0) then
                i3=index(line,"f2mag_cryopad")
                if(i3 /= 0) then
                  Icost(3)=1
                  read(unit=line(i3+13:),fmt=*,iostat=ier) wcost(0), wcost(3)
                  if(ier /= 0) wcost(0)=1.0; wcost(3)=1.0
                end if
                i4=index(line,"f2mag_mupad")
                if(i4 /= 0) then
                  Icost(4)=1
                  read(unit=line(i4+11:),fmt=*,iostat=ier) wcost(0), wcost(4)
                  if(ier /= 0)  wcost(0)=1.0; wcost(4)=1.0
                end if
                if(i3 == 0 .and. i4 == 0) then !f2mag pure case
                  Icost(0)=1
                  read(unit=line(i+5:),fmt=*,iostat=ier) wcost(0)
                  if(ier /= 0) wcost(0)=1.0
                end if
              else
                Icost(0)=0; wcost(0)=0.0
                i1=index(line,"cryopad")
                if(i1 == 0) then
                   Icost(1)=0; wcost(1)=0.0
                else
                   Icost(1)=1
                   read(unit=line(i1+7:),fmt=*,iostat=ier) wcost(1)
                   if(ier /= 0) wcost(1)=1.0
                end if
                i2=index(line,"mupad")
                if(i2 == 0) then
                   Icost(2)=0; wcost(2)=0.0
                else
                   Icost(2)=1
                   read(unit=line(i2+5:),fmt=*,iostat=ier) wcost(2)
                   if(ier /= 0) wcost(2)=1.0
                end if
              end if

         end select
       end do

       return
    End Subroutine Readn_Set_CostFunctPars

!******************************************!
    Subroutine Write_CostFunctPars(lun)
!******************************************!
       !---- Arguments ----!
       integer,   intent( in)    :: lun
       !---- Local variables ----!
       integer              :: i


       Write(unit=lun,fmt="(/,a)")    "=================================="
       Write(unit=lun,fmt="(a)")      "= Cost functions to be minimized ="
       Write(unit=lun,fmt="(a,/)")    "=================================="

       do i=0,N_Costf

          if(Icost(i) == 0) cycle
          select case (i)

              case (0)    !Optimization "f2mag"
                 Write(unit=lun,fmt="(a,f6.2)") &
                 " => Cost(F2mag): Optimization of Sum(|(Obs-Scale*Calc)/sigma|^2) ) / (Nobs-Nref), with weight: ",wcost(0)
              case (1)    !Optimization "cryopad"
                 Write(unit=lun,fmt="(a,f6.2)") &
                 " => Cost(cryopad): Optimization of Sum(|(Obs-Calc)/sigma|^2) ) / (Nobs-Nref), with weight: ",wcost(1)
              case (2)    !Optimization "mupad"
                 Write(unit=lun,fmt="(a,f6.2)") &
                 " => Cost(mupad): Optimization of Sum(|(Obs-Calc)/sigma|^2) ) / (Nobs-Nref), with weight: ",wcost(2)
              case (3)    !Optimization "f2mag-cryopad"
                 Write(unit=lun,fmt="(a)") &
                 " => Optimization of Sum(|(Obs-Scale*Calc)/sigma|^2) ) / (Nobs-Nref)"
                 Write(unit=lun,fmt="(2(a,f6.2))") &
                 " => Cost(F2mag): with weight: ",wcost(0)," Cost(cryopad): with weight: ",wcost(3)
              case (4)    !Optimization "f2mag-mupad"
                 Write(unit=lun,fmt="(a)") &
                 " => Optimization of Sum(|(Obs-Scale*Calc)/sigma|^2) ) / (Nobs-Nref)"
                 Write(unit=lun,fmt="(2(a,f6.2))") &
                 " =>  Cost(F2mag): with weight: ",wcost(0), " Cost(mupad): with weight: ",wcost(4)

          end select

       end do

       return
    End Subroutine Write_CostFunctPars

!******************************************!
    Subroutine Write_FinalCost(lun)
!******************************************!
       !---- Arguments ----!
       integer,   intent( in)    :: lun
       !---- Local variables ----!
       integer              :: i,iset


       Write(unit=lun,fmt="(/,a)")    "====================================="
       Write(unit=lun,fmt="(a)")      "= Final Cost of minimized functions ="
       Write(unit=lun,fmt="(a,/)")    "====================================="

       do i=0,n_costf

          if(icost(i) == 0) cycle

          select case (i)

              case (0)    !Optimization "F2mag"
                 Write(unit=lun,fmt="(a,f6.2,a,f12.4)") &
                 " => Cost(F2mag): Optimization of Sum(|Obs-Scale*Calc|^2) / Sum(sigma^2)) / (Nobs-Npar), with weight: ",wcost(0),&
                 "  Final Cost: ",cost
              case (1)    !Optimization "cryopad"
                 Write(unit=lun,fmt="(a,f6.2,a,f12.4)") &
                 " => Cost(cryopad): Optimization of Sum(|Obs-Calc|^2) / Sum(sigma^2)) / (Nobs-Npar), with weight: ",wcost(1),&
                 "  Final Cost: ",cost
              case (2)    !Optimization "mupad"
                 Write(unit=lun,fmt="(a,f6.2,a,f12.4)") &
                 " => Cost(mupad): Optimization of Sum(|Obs-Calc|^2) / Sum(sigma^2)) / (Nobs-Npar), with weight: ",wcost(2),&
                 "  Final Cost: ",cost
              case (3)    !Optimization "F2mag_cryopad"
                 Write(unit=lun,fmt="(a,f12.4)") &
                 " => Cost(F2mag_cryopad): Optimization of Sum(|Obs-Scale*Calc|^2) / Sum(sigma^2)) / (Nobs-Npar)",cost
                 do iset=1,Nset
                 if(Multidata%SNP(iset)) then
                  Write(unit=lun,fmt="(a,i2,a,f6.2,a,f12.4)") "Cryopad ",iset,": weight ",wcost(3),"  partial_Cost_NoNpar: ",&
                                                                  Pcost(iset)/(9*Multidata%Nobs(iset))
                 else
                  Write(unit=lun,fmt="(a,f6.2,a,f12.4)") "F2mag: weight ",wcost(0),"  partial_Cost_NoNpar: ", Pcost(iset)/Nf2
                 end if
                 end do
                  Write(unit=lun,fmt="(/)")

              case (4)    !Optimization "F2mag_mupad"
                 Write(unit=lun,fmt="(a,f12.4)") &
                 " => Cost(F2mag_mupad): Optimization of Sum(|Obs-Scale*Calc|^2) / Sum(sigma^2)) / (Nobs-Npar): ",cost
                 do iset=1,Nset
                 if(Multidata%SNP(iset)) then
                  Write(unit=lun,fmt="(a,i2,a,f6.2,a,f12.4)") "Mupad ",iset,": weight ",wcost(4),"  partial_Cost_NoNpar: ",&
                                                                  Pcost(iset)/(9*Multidata%Nobs(iset))
                 else
                  Write(unit=lun,fmt="(a,f6.2,a,f12.4)") "F2mag: weight ",wcost(0),"  partial_Cost_NoNpar: ", Pcost(iset)/Nf2
                 end if
                 end do

          end select

       end do

       return
    End Subroutine Write_FinalCost

!******************************************!
    Subroutine General_Cost_function(v,cost)
!******************************************!
      real,dimension(:),    intent( in):: v
      real,                 intent(out):: cost

       !---- Arguments ----!

      !---- Local variables ----!
      integer               :: i,ic, numv, iset
      integer, dimension(1) :: List

         if(allocated(Pcost)) deallocate(Pcost)
         allocate(Pcost(Nset))
         Pcost=0
         cost=0.0
         costF2=0.0
         costPol=0.0


      v_shift(1:NP_Refi)=v(1:NP_Refi)-v_vec(1:NP_Refi)  !Calculate the shifts w.r.t. old configuration
      v_vec(1:NP_Refi)=v(1:NP_Refi)

      numv=count(abs(v_shift) > 0.00001, dim=1)
      if(numv == 1) then
         i=maxloc(abs(v_shift),dim=1)
         List(1)=v_list(i)
         call VState_to_AtomsPar(mA,mode="S",MGp=MGp,Mag_dom=AllMag_dom) !Update Atomic parameters with the proper constraints

         call MagDom_to_Dataset(AllMag_dom)   !Puts new AllMag_Dom into Multidata%MagDom

         do ic=0,N_costf

            if(Icost(ic) == 0) cycle

            Select Case(ic)

               case(0)      !F2mag
                    do iset=1,Nset
                      if(Multidata%f2mag(iset)) then
                        call Calc_sqMiV_Data(iset)
                        call Cost_sqMiV(iset,Pcost(iset),Scalef)
                        cost=cost+ Pcost(iset)* WCost(0)/(MultiData%Nobs(iset)-NP_Refi)
                      end if
                    end do

               case(1)      !cryopad
                    do iset=1,Nset
                      if(Multidata%SNP(iset)) then
                        call Calc_Polar_Dom_Data(iset)
                        call Cost_Pol(iset,Pcost(iset))
                        cost=cost+ Pcost(iset)* WCost(1)
                      end if
                    end do
                    cost=cost/(9*Nobs-NP_Refi) !normalized cost

!                   cost=cost/(9*sum([(MultiData%Nobs(iset),iset=1,Nset)])-NP_Refi) !normalized cost

               case(2)      !mupad
                    do iset=1,Nset
                      if(Multidata%SNP(iset)) then
                        call Calc_Polar_CrSec_Data(iset)
                        call Cost_Pol_sVs(iset,Pcost(iset))
                        cost=cost+ Pcost(iset)* WCost(2)
                      end if
                    end do
                    cost=cost/(9*Nobs-NP_Refi) !normalized cost

               case(3)      !F2mag+cryopad
                    do iset=1,Nset
                      if(Multidata%SNP(iset)) then
                        call Calc_Polar_Dom_Data(iset)
                        call Cost_Pol(iset,Pcost(iset))
                        costPol=costPol+ Pcost(iset)* WCost(3)
                      end if
                      if(Multidata%f2mag(iset)) then
                        call Calc_sqMiV_Data(iset)
                        call Cost_sqMiV(iset,Pcost(iset),Scalef)
                        costF2=costF2+ Pcost(iset)* WCost(0)
                      end if
                    end do
                    cost=(costPol+costF2)/(9*(Nobs-Nf2)+Nf2-NP_Refi)

               case(4)      !F2mag+mupad
                    do iset=1,Nset
                      if(Multidata%SNP(iset)) then
                        call Calc_Polar_CrSec_Data(iset)
                        call Cost_Pol_sVs(iset,Pcost(iset))
                        costPol=costPol+ Pcost(iset)* WCost(4)
                      end if
                      if(Multidata%f2mag(iset)) then
                        call Calc_sqMiV_Data(iset)
                        call Cost_sqMiV(iset,Pcost(iset),Scalef)
                        costF2=costF2+ Pcost(iset)* WCost(0)
                      end if
                    end do
                    cost=(costPol+costF2)/(9*(Nobs-Nf2)+Nf2-NP_Refi)

            End Select
         end do

      else            !New configuration

         call VState_to_AtomsPar(mA,mode="V",MGp=MGp,Mag_dom=AllMag_dom) !Update Atomic parameters with the proper constraints
         call MagDom_to_Dataset(AllMag_dom) !Puts new AllMag_Dom into Multidata%MagDom

         do ic=0,N_costf

            if(Icost(ic) == 0) cycle

            Select Case(ic)

               case(0)      !F2mag
                    do iset=1,Nset
                      if(Multidata%f2mag(iset)) then
                        call Calc_sqMiV_Data(iset)
                        call Cost_sqMiV(iset,Pcost(iset),Scalef)
                        cost=cost+ Pcost(iset)* WCost(0)/(MultiData%Nobs(1)-NP_Refi)
                      end if
                   end do

               case(1)      !cryopad
                    do iset=1,Nset
                      if(Multidata%SNP(iset)) then
                        call Calc_Polar_Dom_Data(iset)
                        call Cost_Pol(iset,Pcost(iset))
                        cost=cost+ Pcost(iset)* WCost(1)
                       end if
                   end do
                   cost=cost/(9*Nobs-NP_Refi) !normalized cost

               case(2)      !mupad
                    do iset=1,Nset
                      if(Multidata%SNP(iset)) then
                        call Calc_Polar_CrSec_Data(iset)
                        call Cost_Pol_sVs(iset,Pcost(iset))
                        cost=cost+ Pcost(iset)* WCost(2)
                       end if
                    end do
                   cost=cost/(9*Nobs-NP_Refi) !normalized cost

               case(3)      !F2mag+cryopad
                    do iset=1,Nset
                      if(Multidata%SNP(iset)) then
                        call Calc_Polar_Dom_Data(iset)
                        call Cost_Pol(iset,Pcost(iset))
                        costPol=costPol+ Pcost(iset)* WCost(3)
                      end if
                      if(Multidata%f2mag(iset)) then
                        call Calc_sqMiV_Data(iset)
                        call Cost_sqMiV(iset,Pcost(iset),Scalef)
                        costF2=costF2+ Pcost(iset)* WCost(0)
                      end if
                    end do
                    cost=(costPol+costF2)/(9*(Nobs-Nf2)+Nf2-NP_Refi)

               case(4)      !F2mag+mupad
                    do iset=1,Nset
                      if(Multidata%SNP(iset)) then
                        call Calc_Polar_CrSec_Data(iset)
                        call Cost_Pol_sVs(iset,Pcost(iset))
                        costPol=costPol+ Pcost(iset)* WCost(4)
                      end if
                      if(Multidata%f2mag(iset)) then
                        call Calc_sqMiV_Data(iset)
                        call Cost_sqMiV(iset,Pcost(iset),Scalef)
                        costF2=costF2+ Pcost(iset)* WCost(0)
                      end if
                    end do
                    cost=(costPol+costF2)/(9*(Nobs-Nf2)+Nf2-NP_Refi)

            End Select
         end do
      end if

      return
    End Subroutine General_Cost_function

!******************************************!
    Subroutine Cost_sqMiV(iset,cost,Scalef)
!******************************************!
       integer,      intent(in):: iset
       real,        intent(out):: cost
       real,     intent(in out):: Scalef

       !---- Local variables ----!
       integer              :: j,n

       !---- Here Cost is not normalised to Nobs,Npar
       n=Oblist%Nobs
       Scalef=sum(Oblist%Ob(1:n)%Gobs)/max(1.0,sum(MhMultilist%Mhlist(iset)%Mh(1:n)%sqMiV))
       cost=0.0
       do j=1,n
         cost=cost+Oblist%Ob(j)%wGobs* (Oblist%Ob(j)%Gobs-Scalef*MhMultilist%Mhlist(iset)%Mh(j)%sqMiV)**2
       enddo
       return
    End Subroutine Cost_sqMiV

!******************************************!
    Subroutine Cost_Pol(iset,cost)
!******************************************!
       integer,   intent(in):: iset
       real,     intent(out):: cost

       !---- Local variables ----!
       integer              :: iobs

       !---- Here Cost is not normalised to Nobs,Npar
        cost=0.0

        do iobs=1,MultiData%Nobs(iset) !loop over Polar observations
          cost =  cost + sum(PolaroMultilist%Polarolist(iset)%Polaro(iobs)%woPij * &
                 ( (PolariMultilist%Polarilist(iset)%Polari(iobs)%Pij - &
                    PolaroMultilist%Polarolist(iset)%Polaro(iobs)%oPij)**2))
         enddo !end loop over observations
       return
    End Subroutine Cost_Pol

!******************************************!
    Subroutine Cost_Pol_sVs(iset,cost)
!******************************************!
       integer,    intent(in):: iset
       real,      intent(out):: cost

       !---- Local variables ----!
       integer              :: iobs

       !---- Here Cost is not normalised to Nobs,Npar
        cost=0.0

         do iobs=1,MultiData%Nobs(iset) !loop over Polar observations
         cost =  cost + sum(PolaroMultilist%Polarolist(iset)%Polaro(iobs)%woPij * &
                 ( (PolariMultisVslist%PolarisVslist(iset)%PolarisVs(iobs)%Pij - &
                    PolaroMultilist%Polarolist(iset)%Polaro(iobs)%oPij)**2))
         enddo !end loop over Polar observations

       return
    End Subroutine Cost_Pol_sVs

!******************************************!
Subroutine Write_SOL_mCFL(lun,file_cfl,mA,Mag_dom,comment)
!******************************************!
    !!----
    !!---- Subroutine Write_SOL_mCFL(lun,file_cfl,Cel,SpG,mA,Mag_dom,comment)
    !!----    integer,                  intent(in)            :: lun
    !!----    type(file_type),     intent (in out)       :: file_cfl
    !!----    type (atom_list_type),    intent(in)            :: mA
    !!----    type (Magnetic_Domain_type),optional,intent(in) :: Mag_dom
    !!----    character(len=*),optional,intent(in)            :: comment
    !!----
    !!----    Write a file mCFL
    !!----
    !!---- Created: January - 2012
    !!

       !---- Arguments ----!
       integer,                  intent(in)           :: lun
       type(file_type),          intent (in out)      :: file_cfl
       type (mAtom_list_type),   intent(in)           :: mA
       type (Magnetic_Domain_type),optional,intent(in):: Mag_dom
       character(len=*),optional,intent(in)           :: comment

       !----- Local variables -----!
       integer                         :: i,n,ier
       integer                         :: num_matom,num_skp,num_dom,ik,im,ip
       real,dimension(3)               :: Rsk,Isk
       real(kind=cp),dimension(12)     :: coef
       real(kind=cp)                   :: Ph
       character(len=132)              :: lowline
       character(len=30)               :: forma
       logical                         :: skp_begin, bfcoef_begin, magdom_begin=.true.

       if(present(comment)) write(unit=lun,fmt="(a)") "TITLE "//trim(comment)
       write(unit=lun,fmt="(a)") "!  Automatically generated CFL file (Write_SOL_mCFL)"
       write(unit=lun,fmt="(a)") "!  "

      num_matom=0
      num_dom=0
      i=0

      do
      i=i+1
      if(i >= file_cfl%nlines) exit

      lowline=l_case(adjustl(file_cfl%line(i)%Str_tmp))

      if(lowline(1:6) == "magdom".and.magdom_begin) then
        num_dom=num_dom+1
        ip=index(lowline,":")
        write(unit=file_cfl%line(i)%Str_tmp,fmt="(a,2f7.4)") lowline(1:ip),Mag_Dom%Pop(1:2,num_dom)
        write(unit=lun,fmt="(a)") trim(file_cfl%line(i)%Str_tmp)
        do
          i=i+1
          lowline=adjustl(l_case(file_cfl%line(i)%Str_tmp))
          if(lowline(1:6) == "magdom") then
            num_dom=num_dom+1
            ip=index(lowline,":")
            write(unit=file_cfl%line(i)%Str_tmp,fmt="(a,2f7.4)") lowline(1:ip),Mag_Dom%Pop(1:2,num_dom)
            write(unit=lun,fmt="(a)") trim(file_cfl%line(i)%Str_tmp)
          else
            i=i-1
            magdom_begin=.false.
            exit
          end if
        end do
        cycle
      end if! end magdom

       if(lowline(1:5) == "matom") then
         num_matom=num_matom+1 !max NMatom=mA%natoms
         num_skp=0
         skp_begin=.true.
         bfcoef_begin=.true.
         write(unit=file_cfl%line(i)%Str_tmp,fmt="(a)") trim(file_cfl%line(i)%Str_tmp)
         write(unit=lun,fmt="(a)") trim(file_cfl%line(i)%Str_tmp)
         cycle
       end if

       if(lowline(1:3) == "skp".and.skp_begin) then
         num_skp=num_skp+1 !max mA%atom(num_matom)%nvk
         read(unit=lowline(4:),fmt=*,iostat=ier) ik,im,Rsk,Isk,Ph
         if(MGp%Sk_type == "Spherical_Frame") then
           Rsk(:)=mA%atom(num_matom)%Spher_Skr(:,ik)
           Isk(:)=mA%atom(num_matom)%Spher_Ski(:,ik)
           Ph=mA%atom(num_matom)%mphas(ik)
         else
           Rsk(:)=mA%atom(num_matom)%Skr(:,ik)
           Isk(:)=mA%atom(num_matom)%Ski(:,ik)
           Ph=mA%atom(num_matom)%mphas(ik)
         end if

         write(unit=file_cfl%line(i)%Str_tmp,fmt='(a,i8,i3,7f8.3)') 'skp',ik,im,Rsk,Isk,Ph
         do
           i=i+1
           lowline=adjustl(l_case(file_cfl%line(i)%Str_tmp))
           if(lowline(1:3) == "skp") then
             num_skp=num_skp+1
             read(unit=lowline(4:),fmt=*,iostat=ier) ik,im,Rsk,Isk,Ph
             if(MGp%Sk_type == "Spherical_Frame") then
               Rsk(:)=mA%atom(num_matom)%Spher_Skr(:,ik)
               Isk(:)=mA%atom(num_matom)%Spher_Ski(:,ik)
               Ph=mA%atom(num_matom)%mphas(ik)
             else
               Rsk(:)=mA%atom(num_matom)%Skr(:,ik)
               Isk(:)=mA%atom(num_matom)%Ski(:,ik)
               Ph=mA%atom(num_matom)%mphas(ik)
             end if
             write(unit=file_cfl%line(i)%Str_tmp,fmt='(a,i8,i3,7f8.3)') 'skp',ik,im,Rsk,Isk,Ph
           else
             i=i-1
             skp_begin=.false.
             exit
           end if
         end do

       end if! end Rsk,Isk,Ph

       forma="(a6,i8,i3,  f8.3)"

       if(lowline(1:6) == "bfcoef".and.bfcoef_begin) then
         num_skp=num_skp+1 !max mA%atom(num_matom)%nvk
         read(unit=lowline(7:),fmt=*,iostat=ier) ik,im
         n=abs(MGp%nbas(im))

         write(unit=forma(11:12),fmt="(i2)") n+1
         read(unit=lowline(7:),fmt=*,iostat=ier) ik,im,coef(1:n),ph
         coef(1:n)= mA%atom(num_matom)%cbas(1:n,ik)
         Ph=mA%atom(num_matom)%mphas(ik)
         write(unit=file_cfl%line(i)%Str_tmp,fmt=forma) 'bfcoef',ik,im,coef(1:n),Ph

         do
           i=i+1
           lowline=adjustl(l_case(file_cfl%line(i)%Str_tmp))
           if(lowline(1:6) == "bfcoef") then
             num_skp=num_skp+1
             read(unit=lowline(7:),fmt=*,iostat=ier) ik,im
             n=abs(MGp%nbas(im))
             write(unit=forma(11:12),fmt="(i2)") n+1
             read(unit=lowline(7:),fmt=*,iostat=ier) ik,im,coef(1:n),ph
             coef(1:n)= mA%atom(num_matom)%cbas(1:n,ik)
             Ph=mA%atom(num_matom)%mphas(ik)
             write(unit=file_cfl%line(i)%Str_tmp,fmt="(a,i8,i3,4f8.3)") 'bfcoef',ik,im,coef(1:n),Ph
           else
             i=i-1
             bfcoef_begin=.false.
             exit
           endif
         enddo
       endif! end bfcoef

       write(unit=lun,fmt="(a)") trim(file_cfl%line(i)%Str_tmp)

      end do

    End Subroutine Write_SOL_mCFL

  End Module cost_magfunctions