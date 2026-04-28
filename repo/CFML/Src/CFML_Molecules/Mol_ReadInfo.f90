Submodule (CFML_Molecules) Mol_ReadInfo

   implicit none

 Contains
   !!----
   !!---- SUBROUTINE READINFO_FREE_ATOMS
   !!----
   !!--<<    Subroutine to read a set of Free Atoms from a file.
   !!----    The format is:
   !!----        ATOMS N_Atoms
   !!----
   !!----        (Internal Coordinates for Atoms (N_Atoms Lines) )
   !!----        Atom_Name(6)  Atom_Specie(4)  Coordinates(3)  Biso  Occ [VARY]
   !!----
   !!----    if VARY is present as last option on the Internal Coordinates line,
   !!----    then an extra line is readn
   !!----        Codes_Coordinates(3)   Code_BIso  Code_Occ
   !!-->>
   !!---- Update: April - 2022
   !!
   Module Subroutine ReadInfo_Free_Atoms(FileType, AtmF, N, Nl_ini, Nl_end)
      !---- Arguments ----!
      type(File_type),               intent(in)      :: FileType
      class(Atm_Type), dimension(:), intent(in out)  :: AtmF       ! Free atoms
      integer,                       intent(out)     :: N          ! Free atoms read
      integer, optional,             intent(in)      :: Nl_ini
      integer, optional,             intent(in)      :: Nl_end

      !---- Local Variables ----!
      character(len=132)           :: line
      character(len=6)            :: label
      character(len=4)            :: var,symb
      integer                     :: i,k,nlong,iv, n_end
      integer,       dimension(5) :: ivet
      real(kind=cp), dimension(5) :: vet

      !> Init
      call clear_error()

      N=0

      if (FileType%nlines ==0) then
         call set_error(1, " The number of lines is zero reading for Free Atoms!")
         return
      end if

      !> Number of FREE atoms
      i=0
      if (present(Nl_ini)) i=nl_ini-1
      n_end=filetype%nlines
      if (present(nl_end)) n_end=nl_end

      do
         i=i+1
         if (i > n_end) exit

         line=adjustl(filetype%line(i)%str)
         if (line(1:1) == '!' .or. line(1:1) == '#') cycle
         if (u_case(line(1:4)) /= "ATOM") cycle

         call get_num(line(5:),vet,ivet,iv)
         if (iv /= 1) then
            call set_error(1," The number of FREE atoms not found in the directive ATOM!")
            return
         end if
         N=ivet(1)
         exit
      end do
      if (N == 0) return

      k=0
      do
         i=i+1
         if (i > n_end) exit

         line=adjustl(filetype%line(i)%str)
         if (line(1:1) == '!' .or. line(1:1) == '#') cycle

         !> Atom label
         call cut_string(line,nlong,label)

         !> Chemical symbol
         call cut_string(line,nlong,symb)

         !> VARY
         line=u_case(line)
         var=" "
         iv=index(line,"VARY")
         if (iv > 0) then
            line=line(:iv-1)
            var="VARY"
         end if

         !> X's, Uiso, Occ
         call get_num(line,vet,ivet,iv)
         select case (iv)
            case (:2)
               call set_error(1," Wrong numbers to describe the coordinates for the atom!")
               return

            case (3)
               vet(4)=0.0_cp
               vet(5)=1.0_cp

            case (4)
               vet(5)=1.0_cp

            case (6:)
               call set_error(1," Wrong numbers to describe the coordinates, Uiso and Occ for the atom!")
               return
         end select

         k=k+1
         AtmF(k)%Lab =label
         AtmF(k)%ChemSymb=symb
         AtmF(k)%x=vet(1:3)
         AtmF(k)%u_iso=vet(4)
         AtmF(k)%occ =vet(5)

         if (var == "VARY") then
            select type (AtmF)
               type is (Atm_Ref_Type)

               type is (ModAtm_Ref_Type)

               class default
                  call set_error(1,"Defined wrong type for Atom parameters!")
                  return
            end select
         end if

         if (var == "VARY") then
            do
               i=i+1
               if (i > n_end) exit

               line=adjustl(filetype%line(i)%str)
               if (line(1:1) == '!' .or. line(1:1) == '#') cycle

               call get_num(line,vet,ivet,iv)
               select case (iv)
                  case (:2)
                     call set_error(1, 'Wrong numbers for describe the codes of the atom!')
                     return

                  case (3)
                     select type (AtmF)
                        type is (Atm_Ref_Type)
                           AtmF(k)%m_x =vet(1:3)
                           AtmF(k)%m_U_iso =1.0_cp
                           AtmF(k)%m_occ =1.0_cp

                        type is (ModAtm_Ref_Type)
                           AtmF(k)%m_x =vet(1:3)
                           AtmF(k)%m_U_iso =1.0_cp
                           AtmF(k)%m_occ =1.0_cp
                     end select

                  case (4)
                     select type (AtmF)
                        type is (Atm_Ref_Type)
                           AtmF(k)%m_x    =vet(1:3)
                           AtmF(k)%m_U_iso =vet(4)
                           AtmF(k)%m_occ =1.0_cp

                        type is (ModAtm_Ref_Type)
                           AtmF(k)%m_x    =vet(1:3)
                           AtmF(k)%m_U_iso =vet(4)
                           AtmF(k)%m_occ =1.0_cp
                     end select

                  case (5)
                     select type (AtmF)
                        type is (Atm_Ref_Type)
                           AtmF(k)%m_x    =vet(1:3)
                           AtmF(k)%m_U_iso =vet(4)
                           AtmF(k)%m_occ  =vet(5)

                        type is (ModAtm_Ref_Type)
                           AtmF(k)%m_x    =vet(1:3)
                           AtmF(k)%m_U_iso =vet(4)
                           AtmF(k)%m_occ  =vet(5)
                     end select

                  case (6:)
                     call set_error(1, 'Wrong numbers for describe the codes of the atom!')
                     return
               end select
               exit
            end do
         end if   ! if VARY

         if (k == N) exit
      end do

      if (k /= N) then
         call set_error(1, 'The number of FREE atoms readed was not the same that ATOMS directive!')
      end if

   End Subroutine ReadInfo_Free_Atoms

   !!----
   !!---- SUBROUTINE READINFO_MOLECULE
   !!----
   !!----    Subroutine to read a molecule from a file.
   !!----    The format is:
   !!----
   !!----        MOLE[X] N_Atoms Molecule_Name Coordinates_Type
   !!----
   !!----    where:
   !!----        N_atoms             Number of atoms in the molecule definition
   !!----        Molecule_Name       Name for the molecule
   !!----        Coordinates_Type    C: Cartesian coordinates
   !!----                            F: Fractional coordinates
   !!----                            S: Spherical coordinates
   !!----                            Z: Z-Matrix coordinates
   !!----
   !!----    If keyword MOLEX is present, then the next two lines will be read (6 reals, 2 characters)
   !!----        - Molecule_Centre(3), Molecule_Orient(3), Rotational_Angle Type(1), Thermal_Factor Type(1)
   !!----        - Code Molecule_Centre(3), Molecule_Orient(3)
   !!----
   !!----    where:
   !!----        Molecule_Centre     Coordinate of Center of Molecule
   !!----        Molecule_Orient     Angles orientation
   !!----        Rotational Angle    E: Conventional Euler angles (alpha, beta, gamma)
   !!----                            P: Polar Euler angles (Phi, theta, Chi) (default)
   !!----        Thermal Factor    ISO: No collective motion
   !!----                          TLS: Traslational + Librational + Correlation
   !!----                           TL: Traslational + Librational
   !!----                            T: Traslational
   !!----
   !!----
   !!----        According to Thermal Factors, next lines will be read
   !!----                          [T]: 6 Thermal Factors (Line1) + 6 Codes Thermal Factors (Line2)
   !!----
   !!----                         [TL]: 6 Thermal Factors (Line1) + 6 Codes Thermal Factors (Line2)
   !!----                               6 Thermal Factors (Line3) + 6 Codes Thermal Factors (Line4)
   !!----
   !!----                        [TLS]: 6 Thermal Factors (Line1) + 6 Codes Thermal Factors (Line2)
   !!----                               6 Thermal Factors (Line3) + 6 Codes Thermal Factors (Line4)
   !!----                               9 Thermal Factors (Line5) + 9 Codes Thermal Factors (Line6)
   !!----
   !!----    Internal Coordinates for Atoms (N_Atoms Lines)
   !!----        Atom_Name(6)  Atom_Specie(4)  Coordinates(3)  [N1  N2  N3]  Biso  Occ [VARY]
   !!----
   !!----    if VARY is present as last option on the Internal Coordinates line,
   !!----    then an extra line is read
   !!----        Codes_Coordinates(3)   Code_BIso  Code_Occ
   !!----
   !!---- Update: April - 2022
   !!
   Module Subroutine ReadInfo_Molecule(FileType, Mol, Nl_ini, Nl_end)
      !---- Arguments ----!
      type(File_type),      intent(in)   :: FileType
      type (Molecule_type), intent(out)  :: Mol
      integer, optional,    intent(in)   :: Nl_Ini
      integer, optional,    intent(in)   :: Nl_End

      !---- Local variables -----!
      character(len=132)              :: line
      character(len=40)               :: Molname
      character(len=6)                :: Atname
      character(len=4)                :: AtSymb,var
      character(len=40), dimension(11):: dire
      character(len=1)                :: ct
      integer                         :: i,k,ic,iv,ini,npos,na, n_end
      integer,dimension(10)           :: ivet
      real(kind=cp), dimension(10)    :: vet
      real(kind=cp),dimension(3,3)    :: Eu

      logical                         :: in_xtal,err_flag

      !> Init
      call clear_error()

      if (FileType%nlines ==0) then
         call set_error(1, " The number of lines is zero reading for Free Atoms!")
         return
      end if

      in_xtal=.false.

      !> Molecule
      i=0
      if (present(nl_ini)) i=nl_ini-1

      n_end=filetype%nlines
      if (present(nl_end)) n_end=nl_end

      do
         i=i+1
         if (i > n_end) exit

         line=adjustl(filetype%line(i)%str)
         if (line(1:1) == '!' .or. line(1:1) == '#') cycle
         npos=index(line,'!')
         if (npos > 0) line=line(:npos-1)
         if (u_case(line(1:4)) /= "MOLE") cycle
         ini=5

         !> Crystal
         if (u_case(line(5:5)) == 'X') then
            in_xtal=.true.
            ini=6
         end if

         !> Format
         call get_words(line(ini:),dire,ic)
         if (ic /= 3) then
            call set_error(1, "Instruction: MOLE[X] N_Atoms Molecule_Name Coordinates_Type, not found! " )
            return
         end if

         !> N. Atoms in the Molecule
         call get_num(dire(1), vet, ivet, iv)
         if (iv /= 1) then
            call set_error(1, "The number of atoms in the Molecule: "//trim(dire(1)) )
            return
         end if
         Na=ivet(1)
         if (Na <=0) then
            call set_error(1, "The number of atoms in the Molecule is zero!" )
            return
         end if

         !> Name of the Molecule
         Molname='---XXX---'
         Molname=trim(dire(2))

         !> Format of Coordinates
         ct='-'
         ct=adjustl(dire(3))
         ct=u_case(ct)
         select case (ct)
            case ('F','C','S','Z')
            case default
               call set_error(1," The type of the coordinates is unknown: "//ct )
               return
         end select

         exit
      end do

      !> Initialize the Molecule_Type
      call Init_Molecule(Mol, Na)
      Mol%Name_Mol=trim(Molname)
      Mol%coor_type=ct

      if (in_xtal) then
         !> Centre / Orientation,....
         do
            i=i+1
            if (i > n_end) exit

            line=adjustl(filetype%line(i)%str)
            if (line(1:1) == '!' .or. line(1:1) == '#') cycle
            npos=index(line,'!')
            if (npos > 0) line=line(:npos-1)

            !> Format
            call get_words(line, dire, ic)
            if (ic /= 8) then
               call set_error(1, "Molecule_Centre(3R), Molecule_Orient(3R), Rotational_Angle Type(1C), Thermal_Factor Type(1C)" )
               return
            end if

            !> Centre of Molecule
            line=trim(dire(1))//'  '//trim(dire(2))//'  '//trim(dire(3))
            call get_num(line,vet,ivet,iv)
            if (iv /=3) then
               call set_error(1, "Wrong number of parameters describing the Centre position of the molecule!")
               return
            end if
            Mol%xcentre=vet(1:3)

            !> Orientation
            line=trim(dire(4))//'  '//trim(dire(5))//'  '//trim(dire(6))
            call get_num(line,vet,ivet,iv)
            if (iv /=3) then
               call set_error(1, "Wrong number of parameters describing the Orientation of the molecule!")
               return
            end if
            Mol%orient=vet(1:3)

            !> Rotation type
            ct=adjustl(u_case(dire(7)))
            select case (ct)
               case ('E','P')  ! Euler, Polar
               case default
                  call set_error(1, "Wrong description for angle rotation type: "//ct)
                  return
            end select
            Mol%rot_type=ct

            !> Thermal type
            var=adjustl(u_case(dire(8)))
            select case (trim(var))
               case ('ISO')
               case ('TLS')
               case ('TL')
               case ('T')
               case default
                  call set_error(1, "Wrong description for Thermal type: "//trim(var))
                  return
            end select
            Mol%therm_type=trim(var)
            exit
         end do
         Eu=Set_Euler_Matrix(Mol%rot_type, Mol%orient(1), Mol%orient(2), Mol%orient(3))
         Mol%Euler=Eu
         Mol%is_EulerMat=.true.

         !> Codes for Orientation...
         do
            i=i+1
            if (i > n_end) exit

            line=adjustl(filetype%line(i)%str)
            if (line(1:1) == '!' .or. line(1:1) == '#') cycle
            npos=index(line,'!')
            if (npos > 0) line=line(:npos-1)

            call get_num(line,vet,ivet,iv)
            if (iv /= 6) then
               call set_error(1, "Wrong number of parameters for Codes values for Centre and Orientation")
               return
            end if
            Mol%mxcentre=vet(1:3)
            Mol%mOrient =vet(4:6)
            exit
         end do

         !> Thermal factors
         if (var(1:1) == 'T)') then
            !> Read 6 Themal parameters
            do
               i=i+1
               if (i > n_end) exit
               line=adjustl(filetype%line(i)%str)
               if (line(1:1) == '!' .or. line(1:1) == '#') cycle
               npos=index(line,'!')
               if (npos > 0) line=line(:npos-1)

               call get_num(line,vet,ivet,iv)
               if (iv /= 6) then
                  call set_error(1, "Wrong number of parameters for Thermal values for Molecule")
                  return
               end if
               Mol%T_TLS=vet(1:6)
               exit
            end do

            !> ... for codes
            do
               i=i+1
               if (i > n_end) exit
               line=adjustl(filetype%line(i)%str)
               if (line(1:1) == '!' .or. line(1:1) == '#') cycle
               npos=index(line,'!')
               if (npos > 0) line=line(:npos-1)

               call get_num(line,vet,ivet,iv)
               if (iv /= 6) then
                  call set_error(1, "Wrong number of parameters for Code Thermal values for Molecule")
                  return
               end if
               Mol%mT_TLS=vet(1:6)
               exit
            end do
         end if

         if (var(2:2) == 'L') then
            !> Read 6 Themal parameters
            do
               i=i+1
               if (i > n_end) exit
               line=adjustl(filetype%line(i)%str)
               if (line(1:1) == '!' .or. line(1:1) == '#') cycle
               npos=index(line,'!')
               if (npos > 0) line=line(:npos-1)

               call get_num(line,vet,ivet,iv)
               if (iv /= 6) then
                  call set_error(1, "Wrong number of parameters for Thermal values for Molecule")
                  return
               end if
               Mol%L_TLS=vet(1:6)
               exit
            end do

            !> ... for codes
            do
               i=i+1
               if (i > n_end) exit
               line=adjustl(filetype%line(i)%str)
               if (line(1:1) == '!' .or. line(1:1) == '#') cycle
               npos=index(line,'!')
               if (npos > 0) line=line(:npos-1)

               call get_num(line,vet,ivet,iv)
               if (iv /= 6) then
                  call set_error(1, "Wrong number of parameters for Code Thermal values for Molecule")
                  return
               end if
               Mol%mL_TLS=vet(1:6)
               exit
            end do
         end if

         if (var(3:3) == 'S') then
            !> Read 9 Themal parameters
            do
               i=i+1
               if (i > n_end) exit
               line=adjustl(filetype%line(i)%str)
               if (line(1:1) == '!' .or. line(1:1) == '#') cycle
               npos=index(line,'!')
               if (npos > 0) line=line(:npos-1)

               call get_num(line,vet,ivet,iv)
               if (iv /= 9) then
                  call set_error(1, "Wrong number of parameters for Thermal values for Molecule")
                  return
               end if
               Mol%S_TLS(1,:)=vet(1:3)
               Mol%S_TLS(2,:)=vet(4:6)
               Mol%S_TLS(3,:)=vet(7:9)
               exit
            end do

            !> ... for codes
            do
               i=i+1
               if (i > n_end) exit
               line=adjustl(filetype%line(i)%str)
               if (line(1:1) == '!' .or. line(1:1) == '#') cycle
               npos=index(line,'!')
               if (npos > 0) line=line(:npos-1)

               call get_num(line,vet,ivet,iv)
               if (iv /= 9) then
                  call set_error(1, "Wrong number of parameters for Code Thermal values for Molecule")
                  return
               end if
               Mol%mS_TLS(1,:)=vet(1:3)
               Mol%mS_TLS(2,:)=vet(4:6)
               Mol%mS_TLS(3,:)=vet(7:9)
               exit
            end do
         end if

         Mol%in_xtal = .true.

      end if ! xtAL

      !> Read the internal coordinates of the atoms in the Mol
      !> Read the Z-matrix/Cartesian/spherical/Fractional coordinates of the Mol

      k=0
      do
         i=i+1
         if (i > n_end) exit
         line=adjustl(filetype%line(i)%str)
         if (line(1:1) == '!' .or. line(1:1) == '#') cycle
         npos=index(line,'!')
         if (npos > 0) line=line(:npos-1)

         !> Atom Name
         call Cut_string(line, ic, Atname)

         !> Chemical symbol
         call Cut_string(line, ic, AtSymb)

         !> Vary?
         var=' '
         npos=index(u_case(line),'VARY')
         if (npos > 0) then
            var='VARY'
            line=line(:npos-1)
         end if

         !> Get Numbers
         k=k+1
         call get_num(line,vet,ivet,iv)
         select case (Mol%coor_type)
            case ('F','C','S')
               if (iv >= 3 .and. iv <=5) then
                  select case (iv)
                     case (3)
                        Mol%I_Coor(:,k)  =vet(1:3)
                        Mol%U_iso(k)     =0.5_cp
                        Mol%Occ(k)       =1.0_cp
                     case (4)
                        Mol%I_Coor(:,k)  =vet(1:3)
                        Mol%U_iso(k)     =vet(4)
                        Mol%Occ(k)       =1.0_cp
                     case (5)
                        Mol%I_Coor(:,k)  =vet(1:3)
                        Mol%U_iso(k)     =vet(4)
                        Mol%Occ(k)       =vet(5)
                  end select
               else
                  call set_error(1," Wrong number of parameters for Atoms information into Molecule! " //trim(Atname))
                  return
               end if

            case ('Z')
               if (iv >= 6 .and. iv <=8) then
                  select case (iv)
                     case (6)
                        Mol%I_Coor(:,k)  =vet(1:3)
                        Mol%U_iso(k)     =0.5_cp
                        Mol%Occ(k)       =1.0_cp
                        Mol%conn(1:3,k)  =ivet(4:6)
                     case (7)
                        Mol%I_Coor(:,k)  =vet(1:3)
                        Mol%U_iso(k)     =vet(7)
                        Mol%Occ(k)       =1.0_cp
                        Mol%conn(1:3,k)  =ivet(4:6)
                     case (8)
                        Mol%I_Coor(:,k)  =vet(1:3)
                        Mol%U_iso(k)     =vet(7)
                        Mol%Occ(k)       =vet(8)
                        Mol%conn(1:3,k)  =ivet(4:6)
                  end select
               else
                  call set_error(1," Wrong number of parameters for Atoms information into Molecule! " //trim(Atname))
                  return
               end if
         end select

         mol%Atname(k)=trim(atname)
         mol%AtSymb(k)=trim(atsymb)

         !> Codes?
         if (var=='VARY') then
            do
               i=i+1
               if (i > n_end) then
                  call set_error(1," Trying to read a Code line for Atom "//trim(AtName))
                  return
               end if
               line=adjustl(filetype%line(i)%str)
               if (line(1:1) == '!' .or. line(1:1) == '#') cycle
               npos=index(line,'!')
               if (npos > 0) line=line(:npos-1)
               exit
            end do
            call get_num(line,vet,ivet,iv)
            if (iv >= 3 .and. iv <=5) then
               select case (iv)
                  case (3)
                     Mol%mI_Coor(:,k)=vet(1:3)
                     Mol%mU_iso(k)  =0.0
                     Mol%mocc(k)   =0.0
                  case (4)
                     Mol%mI_Coor(:,k)=vet(1:3)
                     Mol%mU_iso(k)  =vet(4)
                     Mol%mocc(k)   =0.0
                  case (5)
                     Mol%mI_Coor(:,k)=vet(1:3)
                     Mol%mU_iso(k)  =vet(4)
                     Mol%mocc(k)   =vet(5)
               end select
            else
               call set_error(1," Wrong number of parameters for Code Atoms information into Molecule! " //trim(Atname))
               return
            end if

         end if  ! Enf Vary

         if (k == Na) exit
      end do

      !> Check
      if (k /= Na) then
         call set_error(1," The number of Atoms readen in the file is different from asked! ")
         return
      end if

      !> Check connectivity if ZMatrix coordinates
      Mol%is_connect=.false.
      err_flag=.false.
      if (Mol%coor_type == "Z") then
         do i=2,Na
            if (i==2 .and. all(Mol%conn(:,2) ==0) ) Mol%conn(1,i)=1
            if (any(Mol%conn(:,i) >= i)) then
               err_flag=.true.
               exit
            end if
            if (i == 3 .and. (Mol%conn(1,i) == 0 .or. Mol%conn(2,i) == 0)) then
               err_flag=.true.
               exit
            end if
            if (i > 3) then
               if (any(Mol%conn(:,i) == 0)) then
                  err_flag=.true.
                  exit
               end if
            end if
         end do
         if (err_flag) then
            call set_error(1,"The Z-matrix connectivity is wrong!" )
            return
         end if
      end if

   End Subroutine ReadInfo_Molecule

End SubModule Mol_ReadInfo