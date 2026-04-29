Submodule (CFML_Molecules) Mol_WriteInfo

   implicit none

 Contains
   !!----
   !!---- SUBROUTINE WRITEINFO_FREE_ATOMS
   !!----
   !!----    Write information about Free Atoms for Molecular Crystal
   !!----
   !!---- Update: April - 2022
   !!
   Module Subroutine WriteInfo_Free_Atoms(AtmF,N,Lun)
      !---- Arguments ----!
      class(Atm_type), dimension(:), intent(in) :: AtmF
      integer,                        intent(in) :: N
      integer, optional,              intent(in) :: Lun

      !---- Local Variables ----!
      integer :: i,uni

      uni=6
      if (present(lun)) uni=lun

      write (unit=uni,fmt="(a)")     " "
      write (unit=uni,fmt="(a,i4)")  " => Number of FREE Atoms: ", N
      write (unit=uni,fmt="(a)")     " "
      write (unit=uni,fmt="(T5,a)") " Atom      Chem        x/a        y/b        z/c       Occ     Uiso"
      write (unit=uni,fmt="(T5,a)") "===================================================================="
      do i=1,N
         write(unit=uni,fmt="(T5,a,T16,a,T21,5f11.4)") atmF(i)%Lab,atmF(i)%chemsymb,atmF(i)%x,atmF(i)%occ,atmF(i)%u_iso
      end do

   End Subroutine WriteInfo_Free_Atoms

   !!----
   !!---- SUBROUTINE WRITEINFO_MOLECULE
   !!----
   !!----    Write information about molecule
   !!----
   !!---- Update: April - 2022
   !!
   Module Subroutine WriteInfo_Molecule(Mol,Lun)
      !---- Arguments ----!
      type (Molecule_type), intent(in):: Mol
      integer,optional,     intent(in):: Lun

      !---- Local variables -----!
      integer            :: i,uni,j
      character(len=4)   :: var
      real(kind=cp), dimension(3  ) :: geom_cent

      uni=6
      if (present(lun)) uni=lun

      write(unit=uni,fmt="(/,/,a,a)")    " =>  MOLECULE of name :  ",trim(Mol%Name_mol)
      select case (Mol%coor_type)
         case ("C","c")
            write(unit=uni,fmt="(a)")     "            Type of Molecular description: CARTESIAN COORDINATES"
         case ("F","f")
            write(unit=uni,fmt="(a)")     "            Type of Molecular description: FRACTIONAL COORDINATES"
         case ("S","s")
            write(unit=uni,fmt="(a)")     "            Type of Molecular description: SPHERICAL COORDINATES"
         case ("Z","z")
            write(unit=uni,fmt="(a)")     "            Type of Molecular description: Z-MATRIX"
         case default
            write(unit=uni,fmt="(a)")     "            Type of Molecular description: UNKNOWN "
      end select ! Mol%coor_type

      write(unit=uni,fmt="(a,i3)")     "                          Number of atoms: ",  Mol%natoms
      if (Mol%in_xtal) then
         write(unit=uni,fmt="(a,3f11.5)")   "         Fractional coordinates of centre: ",  Mol%xcentre
         write(unit=uni,fmt="(a,3f11.5)")   "                         Refinement codes: ",  Mol%mxcentre

         if (Mol%rot_type == "E") then
            write(unit=uni,fmt="(a,3f11.5,a,3f9.5,a)") &
                                "   Orientation EULER angles (alp,bet,gam): ",  Mol%orient,&
                                " (radians:", Mol%orient*to_rad,")"
         else
            write(unit=uni,fmt="(a,3f11.5,a,3f9.5,a)") &
                                "   Orientation POLAR angles (PHI,THE,CHI): ",  Mol%orient,&
                                " (radians:", Mol%orient*to_rad,")"
         end if
         write(unit=uni,fmt="(a,3f11.5)") "                         Refinement codes: ",  Mol%mOrient

         if (Mol%therm_type(1:1) == "T") then
            write(unit=uni,fmt="(a,6f11.5)")"       T-tensor (T11,T22,T33,T12,T13,T23): ", Mol%T_TLS
            write(unit=uni,fmt="(a,6f11.5)")"                         Refinement codes: ", Mol%mT_TLS
         end if

         if (Mol%therm_type(2:2) == "L") then
            write(unit=uni,fmt="(a,6f11.5)")"       L-tensor (L11,L22,L33,L12,L13,L23): ", Mol%L_TLS
            write(unit=uni,fmt="(a,6f11.5)")"                         Refinement codes: ", Mol%mL_TLS
         end if

         if (Mol%therm_type(3:3) == "S") then
            write(unit=uni,fmt="(a,3f11.5,tr5,3f11.5)")"       S-tensor             (S11,S12,S13): ", &
                                             Mol%S_TLS(1,:), Mol%mS_TLS(1,:)
            write(unit=uni,fmt="(a,3f11.5,tr5,3f11.5)")"     + Refinement codes     (S21,S22,S23): ", &
                                             Mol%S_TLS(2,:), Mol%mS_TLS(2,:)
            write(unit=uni,fmt="(a,3f11.5,tr5,3f11.5)")"                            (S31,S32,S33): ", &
                                             Mol%S_TLS(3,:), Mol%mS_TLS(3,:)
         end if

         select case (Mol%coor_type)
            case ("C","c")
               write(unit=uni,fmt="(t29,a)")"Atom    Type        XC          YC          ZC   N1  N2  N3      Uiso        Occ "
            case ("F","f")
               write(unit=uni,fmt="(t29,a)")"Atom    Type        X           Y           Z    N1  N2  N3      Uiso        Occ "
            case ("S","s")
               write(unit=uni,fmt="(t29,a)")"Atom    Type    distance      Theta       Phi    N1  N2  N3      Uiso        Occ "
            case ("Z","z")
               write(unit=uni,fmt="(t29,a)")"Atom    Type    distance  Bond-Angle Torsion-Ang N1  N2  N3      Uiso        Occ "
            case default
               write(unit=uni,fmt="(t29,a)")"Atom    Type      Coor1       Coor2       Coor3  N1  N2  N3      Uiso        Occ "
         end select ! Mol%coor_type

      else  !(Mol%in_xtal)

         select case (Mol%coor_type)
            case ("C","c")
               write(unit=uni,fmt="(t29,a)")"Atom    Type        XC          YC          ZC   N1  N2  N3 "
            case ("F","f")
               write(unit=uni,fmt="(t29,a)")"Atom    Type        X           Y           Z    N1  N2  N3 "
            case ("S","s")
               write(unit=uni,fmt="(t29,a)")"Atom    Type    distance      Theta       Phi    N1  N2  N3 "
            case ("Z","z")
               write(unit=uni,fmt="(t29,a)")"Atom    Type    distance  Bond-Angle Torsion-Ang N1  N2  N3 "
            case default
               write(unit=uni,fmt="(t29,a)")"Atom    Type      Coor1       Coor2       Coor3  N1  N2  N3 "
         end select ! Mol%coor_type

      end if  !(Mol%in_xtal)

      geom_cent=0.0_cp

      if (Mol%in_xtal ) then
         do i=1,Mol%natoms
            if (Mol%AtSymb(i) /= "ZE") geom_cent=geom_cent + Mol%I_Coor(:,i)
            write(unit=uni,fmt="(t29,a,t38,a4,3f12.5,3i4,2f12.5)")  &
                 trim(Mol%AtName(i)), trim(Mol%AtSymb(i)),Mol%I_Coor(:,i),  &
                 Mol%Conn(:,i), Mol%U_iso(i),  Mol%Occ(i)
            var=" "
            do j=1,3
               if (abs(Mol%mI_Coor(j,i)) > EPS) var="VARY"
            end do
            if (abs(Mol%mU_iso(i)) > EPS)      var="VARY"
            if (abs(Mol%mocc(i))  > EPS)      var="VARY"
            if (var == "VARY") then
               write(unit=uni,fmt="(t41,3f12.5,tr12,2f12.5)")  Mol%mI_Coor(:,i), &
                         Mol%mU_iso(i),Mol%mocc(i)
            end if
         end do

      else
         do i=1,Mol%natoms
            if (Mol%AtSymb(i) /= "DU") geom_cent=geom_cent + Mol%I_Coor(:,i)
            write(unit=uni,fmt="(t29,a,t38,a4,3f12.5,3i4       )")  &
                 trim(Mol%Atname(i)), trim(Mol%Atsymb(i)), Mol%I_coor(:,i),  &
                 Mol%conn(:,i)
         end do
      end if

      if (Mol%coor_type == "C" .or. Mol%coor_type == "c" &
          .or. Mol%coor_type == "F" .or. Mol%coor_type == "f") then
         geom_cent=geom_cent/real(Mol%natoms)
         write(unit=uni,fmt="(//,a,3F10.5)")  "  => Geometrical centre of Molecule ( "//trim(Mol%Name_mol)//" ):", geom_cent
      end if

      write(unit=uni,fmt="(/,a)")              "  => Euler Matrix of molecule ( "//trim(Mol%Name_mol)//" ):"
      do i=1,3
         write(unit=uni,fmt="(t29,3f10.5)")  Mol%Euler(i,:)
      end do

   End Subroutine WriteInfo_Molecule

   !!----
   !!---- SUBROUTINE WRITEINFO_MOLECULAR_CRYSTAL
   !!----
   !!----    Write information about Molecular Crystal
   !!----
   !!---- Update: April - 2022
   !!
   Module Subroutine WriteInfo_Molecular_Crystal(MolCrys, Lun)
      !---- Arguments ----!
      type(MolCrystal_Type), intent(in) :: MolCrys
      integer, optional,     intent(in) :: Lun

      !---- Local Variables ----!
      integer :: i,uni

      uni=6
      if (present(lun)) uni=lun

      write(unit=uni,fmt="(/,/,a)") "      MOLECULAR CRYSTAL INFORMATION  "
      write(unit=uni,fmt="(a)")     "   ----------------------------------- "

      write(unit=uni,fmt="(a)")     " "
      call Write_Crystal_Cell(MolCrys%Cell,uni)
      write(unit=uni,fmt="(a)")     " "

      write(unit=uni,fmt="(a)")     " "
      call Write_SpaceGroup_Info(MolCrys%SPG,uni)
      write(unit=uni,fmt="(a)")     " "

      if (MolCrys%N_Free > 0) then
         write(unit=uni,fmt="(a)")     " "
         call WriteInfo_Free_Atoms(MolCrys%Atm,MolCrys%N_Free,uni)
         write(unit=uni,fmt="(a)")     " "
      end if

      if (MolCrys%N_Mol > 0) then
         do i=1,MolCrys%N_Mol
            write(unit=uni,fmt="(a)")     " "
            call WriteInfo_Molecule(MolCrys%Mol(i),uni)
            write(unit=uni,fmt="(a)")     " "
         end do
      end if

   End Subroutine WriteInfo_Molecular_Crystal

End SubModule Mol_WriteInfo