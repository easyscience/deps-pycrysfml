!!----
!!----
!!----
SubModule (CFML_Atoms)  Atm_Write_AtmList
  implicit none
  Contains

   !!----
   !!---- EQUIV_ATM
   !!----
   !!----    Determine whether the atoms of names "nam1" and "nam2" are included in
   !!----    the longer string "name" (constructed by function "wrt_lab").
   !!----
   !!---- Updated: November - 2022
   !!
   Pure Module Function Equiv_Atm(Nam1,Nam2,NameAt) Result(Equiv_Atom)
      !---- Arguments ----!
      character (len=*), intent (in) :: nam1,nam2   ! Atom names
      character (len=*), intent (in) :: NameAt      ! String containing atom names separated by "--"
      logical                        :: equiv_atom

      !---- Local variables ----!
      integer :: i1,i2

      equiv_atom = .false.

      i1=index(nam1,"_")-1
      i2=index(nam2,"_")-1
      if (i1 < 0) i1=len_trim(nam1)
      if (i2 < 0) i2=len_trim(nam2)
      if(index(NameAt,nam1(1:i1)) /= 0  .and.  index(NameAt,nam2(1:i2)) /= 0)  equiv_atom = .true.

   End Function Equiv_Atm

   !!----
   !!---- WRT_LAB
   !!----
   !!----    Character function merging the main part of the labels
   !!----    (before underscore "_") of the atoms "nam1" and "nam2" into
   !!----    the string "bilabel" that is of the form: nam1--nam2
   !!----
   !!----    Updated: November 2022
   !!
   Pure Module Function Wrt_Lab(Nam1,Nam2) Result(Bilabel)
      !---- Arguments ----!
      character (len=*), intent (in) :: nam1,nam2   ! Atoms name
      character (len=:), allocatable :: bilabel     ! Composed string without underscores

      !---- Local variables ----!
      integer :: i1,i2

      bilabel=" "
      i1=index(nam1,"_")-1
      i2=index(nam2,"_")-1
      if (i1 < 0) i1=len_trim(nam1)
      if (i2 < 0) i2=len_trim(nam2)
      bilabel = nam1(1:i1)//"--"//nam2(1:i2)

   End Function Wrt_Lab

   !!----
   !!---- WRITE_ATOM_LIST
   !!----    Write the atoms in the asymmetric unit
   !!----
   !!---- 12/06/2019
   !!
   Module Subroutine Write_Atom_List(A, IPhas, Iunit, SpG)
      !---- Arguments ----!
      type(atlist_type),                    intent(in) :: A        ! Atom list object
      integer, optional,                    intent(in) :: IPhas
      integer, optional,                    intent(in) :: IUnit    ! Logical unit
      type(SuperSpaceGroup_type), optional, intent(in) :: SpG

      !---- Local Variables ----!
      character(len=1)             :: car
      character(len=:),allocatable :: car2
      character(len=4),dimension(A%natoms) :: charge
      character(len=:),allocatable :: fmt1,fmt2,fmt3,fmt4,fmt5,Aux_st
      character(len=:),allocatable :: line
      integer                      :: n, lun, k, j, iph
      logical                      :: Wyck, oxidation_number

      !> Init
      lun=6
      Wyck=.false.; oxidation_number=.false.
      if (present(iunit)) lun=iunit

      !> Header
      write(unit=lun, fmt="(/,a)")    "  ===="
      write(unit=lun, fmt="(a)")      "  ====  Atoms information"
      write(unit=lun, fmt="(a,/)")    "  ===="

      !> Check number of atoms
      if (a%natoms == 0) then
         write(unit=lun,fmt="(/,a,/)") "  => No atoms provided!"
         return
      end if

      if (present(IPhas)) then
         if (IPhas /= A%Iph) then
            call set_error(1,'The Phase selected does not match the actual phase of Atom List!')
            return
         end if
      end if
      iph=A%IPh

      !> Phases

         write(unit=lun, fmt='(a)') " "
         write(unit=lun, fmt='(a,i3)') "   PHASE: ",iph
         write(unit=lun, fmt='(a)') " "


         car2="ISO"
         if (any(A%Atom(:)%Thtype == "ANI") .or. any(A%Atom(:)%Thtype == "ani") ) car2="ANI"

         if (car2 == "ISO") then
            line="  Atom        Scatt / Chem     Mult   x/a       y/b       z/c     B[iso]        Occ"
         else
            car2=trim(u_case(A%Atom(1)%Utype))
            select case (trim(car2))
               case ("BETA")
                  line="  Atom        Scatt / Chem     Mult   x/a       y/b       z/c     B[iso]        Occ"
                  line=line//"   beta_11   beta_22   beta_33   beta_12   beta_13   beta_23"
               case ("U")
                  line="  Atom        Scatt / Chem     Mult   x/a       y/b       z/c     U[iso/eq]     Occ"
                  line=line//"      U_11      U_22      U_33      U_12      U_13      U_23"
               case ("B")
                  line="  Atom        Scatt / Chem     Mult   x/a       y/b       z/c     B[iso]        Occ"
                  line=line//"      B_11      B_22      B_33      B_12      B_13      B_23"
               case default
                  line="  Atom        Scatt / Chem     Mult   x/a       y/b       z/c     B[iso]        Occ"
            end select

         end if
         do j=1,A%natoms
           if(len_trim(A%atom(j)%Wyck) /= 0) then
             Wyck=.true.
             exit
           end if
         end do
         if(any(abs(A%atom(:)%charge) >= 0.0001)) oxidation_number=.true.
         if(Wyck) line=trim(line)//"  Wyckoff"
         charge="        "
         if(oxidation_number) then
           line=trim(line)//"  Ionic_Charge"
           do j=1,A%natoms
              write(unit=charge(j), fmt="(i4)") A%atom(j)%charge
              charge(j)=adjustl(charge(j))
              if(A%atom(j)%charge > 0) charge(j)="+"//charge(j)
           end do
         end if
         write(unit=lun,fmt="(T3,a)") trim(line)

         line=repeat("=", len_trim(line))
         write(unit=lun,fmt="(T3,a)") trim(line)

         fmt1="(T3,a,T6,a,T18,a,T32,i4,5f10.5,tr6,a)"     ! Iso
         fmt2="(T3,a,T6,a,T18,a,T32,i4,11f10.5,tr6,a)"    ! Aniso

         select type (Atm => A%atom)
            type is (atm_type)
               do n=1,A%natoms
                  car2=Atm(n)%ThType
                  car2=u_case(car2)
                  car=car2(1:1)
                  if (.not. A%active(n)) car='-'
                  select case (trim(car2))
                     case ('ISO')
                        write(unit=lun,fmt=fmt1)  car, trim(Atm(n)%Lab),  Atm(n)%SfacSymb//" / "// Atm(n)%chemSymb , &
                             Atm(n)%mult,  Atm(n)%X, Atm(n)%U_iso, Atm(n)%Occ, Atm(n)%Wyck//"     "//charge(n)
                     case ('ANI')
                        write(unit=lun,fmt=fmt2)  car, trim(Atm(n)%Lab), Atm(n)%SfacSymb//" / "// Atm(n)%chemSymb, &
                             Atm(n)%mult,  Atm(n)%X, Atm(n)%U_iso, Atm(n)%Occ, Atm(n)%U, Atm(n)%Wyck//"     "//charge(n)
                  end select
                  if(Atm(n)%magnetic)  then
                       Select Case (trim(u_case(a%mcomp)))
                         Case ("CRYSTAL")
                            write(unit=lun,fmt="(T25,a,3f10.5,a)")  "Moment(uB):", Atm(n)%moment, "  Crystal Components:   Mx(a)  My(b)  Mz(b)"
                         Case ("SPHERICAL")
                            write(unit=lun,fmt="(T25,a,3f10.5,a)")  "Moment(uB):", Atm(n)%moment, "  Spherical Components:  Moment  Phi  Theta"
                         Case ("CARTESIAN")
                            write(unit=lun,fmt="(T25,a,3f10.5,a)")  "Moment(uB):", Atm(n)%moment, "  Cartesian Components:  MxC  MyC  MzC"
                       End Select
                  end if
               end do

            type is (atm_std_type)
               do n=1,A%natoms
                  car=" "
                  if (.not. A%active(n)) car='-'
                  car2=Atm(n)%ThType
                  car2=u_case(car2)
                  select case (trim(car2))
                     case ('ISO')
                        write(unit=lun,fmt=fmt1)  car, trim(Atm(n)%Lab), Atm(n)%SfacSymb//" / "// Atm(n)%chemSymb, &
                             Atm(n)%mult,  Atm(n)%X, Atm(n)%U_iso, Atm(n)%Occ, Atm(n)%Wyck//"     "//charge(n)
                     case ('ANI')
                        write(unit=lun,fmt=fmt2)  car, trim(Atm(n)%Lab), Atm(n)%SfacSymb//" / "// Atm(n)%chemSymb, &
                             Atm(n)%mult,  Atm(n)%X, Atm(n)%U_iso, Atm(n)%Occ, Atm(n)%U, Atm(n)%Wyck//"     "//charge(n)
                  end select
                  if(Atm(n)%magnetic)  then
                       Select Case (trim(u_case(a%mcomp)))
                         Case ("CRYSTAL")
                            write(unit=lun,fmt="(T25,a,3f10.5,a)")  "Moment(uB):", Atm(n)%moment, "  Crystal Components:   Mx(a)  My(b)  Mz(b)"
                         Case ("SPHERICAL")
                            write(unit=lun,fmt="(T25,a,3f10.5,a)")  "Moment(uB):", Atm(n)%moment, "  Spherical Components:  Moment  Phi  Theta"
                         Case ("CARTESIAN")
                            write(unit=lun,fmt="(T25,a,3f10.5,a)")  "Moment(uB):", Atm(n)%moment, "  Cartesian Components:  MxC  MyC  MzC"
                       End Select
                  end if
               end do

            class is (ModAtm_std_Type)
               do n=1,A%natoms
                  car=" "
                  if (.not. A%active(n)) car='-'
                  car2=atm(n)%ThType
                  car2=u_case(car2)
                  select case (trim(car2))
                     case ('ISO')
                        write(unit=lun,fmt=fmt1)  car, trim(Atm(n)%Lab), Atm(n)%SfacSymb//" / "// Atm(n)%chemSymb, &
                             Atm(n)%mult,  Atm(n)%X, Atm(n)%U_iso, Atm(n)%Occ, Atm(n)%Wyck//"     "//charge(n)
                     case ('ANI')
                        write(unit=lun,fmt=fmt2)  car, trim(Atm(n)%Lab), Atm(n)%SfacSymb//" / "// Atm(n)%chemSymb, &
                             Atm(n)%mult,  Atm(n)%X, Atm(n)%U_iso, Atm(n)%Occ, Atm(n)%U, Atm(n)%Wyck//"     "//charge(n)
                  end select
                  if(Atm(n)%magnetic)  then
                       Select Case (trim(u_case(a%mcomp)))
                         Case ("CRYSTAL")
                            write(unit=lun,fmt="(T25,a,3f10.5,a)")  "Moment(uB):", Atm(n)%moment, "  Crystal Components:   Mx(a)  My(b)  Mz(b)"
                         Case ("SPHERICAL")
                            write(unit=lun,fmt="(T25,a,3f10.5,a)")  "Moment(uB):", Atm(n)%moment, "  Spherical Components:  Moment  Phi  Theta"
                         Case ("CARTESIAN")
                            write(unit=lun,fmt="(T25,a,3f10.5,a)")  "Moment(uB):", Atm(n)%moment, "  Cartesian Components:  MxC  MyC  MzC"
                       End Select
                  end if
               end do

               fmt3="(T7,a,t15,a, i3,a,t47,6f10.5,a)"
               fmt4="(T7,a,t15,a, i3,a,t47,f10.5,tr20,f10.5,a)"
               fmt5="(T7,a,t15,a, i3,a,t45,12f10.5,a)"
               Aux_st="Harmonic"
               if(present(SpG)) then
                 write(unit=fmt3(13:13),fmt="(i1)") SpG%nk
                 write(unit=fmt4(13:13),fmt="(i1)") SpG%nk
                 write(unit=fmt5(13:13),fmt="(i1)") SpG%nk
                 Aux_st=" Q_Coeff"
               end if

               if (any(atm(:)%n_mc > 0) .or. any(atm(:)%n_dc > 0) ) then
                  Select Case (trim(u_case(a%mcomp)))
                    Case ("CRYSTAL")
                       line="    Atom        Modulation    "//Aux_st//"           Cos_x     Cos_y     Cos_z     Sin_x     Sin_y     Sin_z"
                    Case ("SPHERICAL")
                       line="    Atom        Modulation    "//Aux_st//"         Cos_Mod   Cos_Phi Cos_Theta   Sin_Mod   Sin_Phi Sin_Theta"
                    Case ("CARTESIAN")
                       line="    Atom        Modulation    "//Aux_st//"          Cos_xC    Cos_yC    Cos_zC    Sin_xC    Sin_yC    Sin_zC"
                  End Select
                  write(unit=lun,fmt="(/,/,T3,a)") line
                  line=repeat("=", len_trim(line))
                  write(unit=lun,fmt="(T3,a)") line
                  if (present(SpG)) then
                    do n=1,A%natoms

                       do j=1,atm(n)%n_mc
                         k=atm(n)%pmc_q(j)
                         write(unit=lun,fmt=fmt3)   trim(Atm(n)%Lab),           "        Moment    [",SpG%Q_coeff(:,k),"]", Atm(n)%Mcs(:,j),"  Constraints: "//trim(Atm(n)%AtmInfo)
                       end do
                       do j=1,atm(n)%n_dc
                         k=atm(n)%pdc_q(j)
                         write(unit=lun,fmt=fmt3)   trim(Atm(n)%Lab),           "  Displacement    [",SpG%Q_coeff(:,k),"]", Atm(n)%Dcs(:,j)
                       end do
                       do j=1,atm(n)%n_oc
                         k=atm(n)%poc_q(j)
                         write(unit=lun,fmt=fmt4)   trim(Atm(n)%Lab), "     Occupancy    [",SpG%Q_coeff(:,k),"]", Atm(n)%Ocs(:,j)
                       end do

                    end do

                  else

                    do n=1,A%natoms
                       do j=1,atm(n)%n_mc
                         k=atm(n)%pmc_q(j)
                         write(unit=lun,fmt=fmt3)   trim(Atm(n)%Lab),           "        Moment    [",k,"]", Atm(n)%Mcs(:,j)
                       end do
                       do j=1,atm(n)%n_dc
                         k=atm(n)%pdc_q(j)
                         write(unit=lun,fmt=fmt3)   trim(Atm(n)%Lab),           "  Displacement    [",k,"]", Atm(n)%Dcs(:,j)
                       end do
                       do j=1,atm(n)%n_oc
                         k=atm(n)%poc_q(j)
                         write(unit=lun,fmt=fmt4)   trim(Atm(n)%Lab), "     Occupancy    [",k,"]", Atm(n)%Ocs(:,j)
                       end do
                    end do
                  end if
               end if

               if(any(atm(:)%n_uc > 0)) then
                  line="    Atom        Modulation       "//Aux_st//"   U_11_Cos  U_22_Cos  U_33_Cos  U_12_Cos  U_13_Cos  U_23_Cos  U_11_Sin  U_22_Sin  U_33_Sin  U_12_Sin  U_13_Sin  U_23_Sin"
                  write(unit=lun,fmt="(/,/,T3,a)") line
                  line=repeat("=", len_trim(line))
                  write(unit=lun,fmt="(T3,a)") line
                  if (present(SpG)) then
                    do n=1,A%natoms
                       do j=1,atm(n)%n_uc
                         k=atm(n)%puc_q(j)
                         write(unit=lun,fmt=fmt5)   trim(Atm(n)%Lab), "Thermal Displacement [",SpG%Q_coeff(:,k),"]", Atm(n)%Ucs(:,j)
                       end do
                    end do
                  else
                    do n=1,A%natoms
                       do j=1,atm(n)%n_uc
                         k=atm(n)%puc_q(j)
                         write(unit=lun,fmt=fmt5)   trim(Atm(n)%Lab), "Thermal Displacement [",k,"]", Atm(n)%Ucs(:,j)
                       end do
                    end do
                  end if
               end if

            type is (Atm_ref_Type)
               do n=1,A%natoms

                  car=" "
                  if (.not. A%active(n)) car='-'
                  car2=atm(n)%ThType
                  car2=u_case(car2)
                  select case (trim(car2))
                     case ('ISO')
                        write(unit=lun,fmt=fmt1)  car, trim(Atm(n)%Lab), trim(Atm(n)%SfacSymb)//" / "//trim(Atm(n)%chemSymb), &
                             Atm(n)%mult,  Atm(n)%X, Atm(n)%U_iso, Atm(n)%Occ, Atm(n)%Wyck//"     "//charge(n)
                     case ('ANI')
                        write(unit=lun,fmt=fmt2)  car, trim(Atm(n)%Lab), trim(Atm(n)%SfacSymb)//" / "//trim(Atm(n)%chemSymb), &
                             Atm(n)%mult,  Atm(n)%X, Atm(n)%U_iso, Atm(n)%Occ, Atm(n)%U, Atm(n)%Wyck//"     "//charge(n)
                  end select
                  if(Atm(n)%magnetic)  then
                       Select Case (trim(u_case(a%mcomp)))
                         Case ("CRYSTAL")
                            write(unit=lun,fmt="(T25,a,3f10.5,a)")  "Moment(uB):", Atm(n)%moment, "  Crystal Components:   Mx(a)  My(b)  Mz(b)"
                         Case ("SPHERICAL")
                            write(unit=lun,fmt="(T25,a,3f10.5,a)")  "Moment(uB):", Atm(n)%moment, "  Spherical Components:  Moment  Phi  Theta"
                         Case ("CARTESIAN")
                            write(unit=lun,fmt="(T25,a,3f10.5,a)")  "Moment(uB):", Atm(n)%moment, "  Cartesian Components:  MxC  MyC  MzC"
                       End Select
                  end if
               end do
         end select

   End Subroutine Write_Atom_List

End SubModule Atm_Write_AtmList