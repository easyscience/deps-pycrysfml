!!-------------------------------------------------------------
!!---- MOLTOPCR Utility
!!
!! @license   Copyright 2019, Juan Rodriguez Carvajal, Institut Laue-Langevin All rights reserved (see LICENSE)
!! @authors   Juan Rodriguez Carvajal (see AUTHORS)
!!
!!-------------------------------------------------------------
 Module Mol_Param
    !---- Use Modules ----!
    Use CFML_GlobalDeps
    Use CFML_Molecules
    Use CFML_Strings
    use CFML_Metrics
    use CFML_Geom

    !---- Variables ----!
    implicit none

    logical                            :: ierror

    character(len=150)                 :: fildat
    character(len=150)                 :: filout,file_cfl
    integer, parameter                 :: jfildat=21
    integer, parameter                 :: jfilout=22
    integer, parameter                 :: i_cfl  =11

    real, dimension(3,10)              :: x1,x2,x3 !Three points defining a frame for each molecule
    logical,dimension(10)              :: fracFrame_read=.false.

    integer                            :: nmol
    type(Molecule_type), dimension(10) :: mol
    type(Molecule_type)                :: moln,molF,molz,molc, molFF,molzz

    type(Cell_G_Type)                  :: celda
    logical                            :: cell_read=.false.

    type(file_type)                    :: fdat

 Contains

    !!----
    !!---- SUBROUTINE GETINFO_COMMAND
    !!----
    Subroutine Getinfo_Command()
       !---- Definition of variables ----!
       integer            :: narg,iargc
       integer            :: npos

       !---- Command Line ----!
       ierror=.false.

       fildat=' '
       filout=' '
       narg=iargc()
          select case (narg)
             case (0)
                write(unit=*,fmt="(a)",advance="no") " =>  Input Data File (.cfl ): "
                read(unit=*,fmt="(a)") fildat
                write(unit=*,fmt="(a)",advance="no") " => Output Data File (.tpcr): "
                read(unit=*,fmt="(a)") filout

             case (1)
                call getarg(1,fildat)

             case (2)
                call getarg(1,fildat)
                call getarg(2,filout)

             case default
                ierror=.true.
                return
          end select

          npos=len_trim(fildat)
          if (npos == 0) then
             ierror=.true.
             return
          end if

          !---- validation of file names ----!
          npos=index(fildat,".")
          if (npos == 0) then
             fildat=trim(fildat)//".cfl"
          end if

          npos=len_trim(filout)
          if (npos == 0) filout=trim(fildat)

          npos=index(filout,".")
          if (npos == 0) then
             filout=trim(filout)//".tpcr"
          else
             filout=filout(1:npos)//"tpcr"
          end if

       return
    End Subroutine Getinfo_Command

    !!----
    !!---- SUBROUTINE LOAD_DATA
    !!----
    Subroutine Load_Data()
       !---- Variables ----!
       character(len=132)     :: line, lline
       real, dimension(3)     :: cell, ang
       integer                :: ier,i,n,n_ini,n_end
       integer, dimension(30) :: pt_mol

       fdat = reading_file(trim(fildat))

       open(unit=jfilout, file=filout, status='replace',action="write")
       i=index(fildat,".",back=.true.)
       file_cfl=fildat(1:i-1)//"_fc.cfl"
       open(unit=i_cfl,file=file_cfl,status="replace",action="write")

       !> Detect number of molecules into file
       pt_mol=0
       nmol=0
       do i=1,fdat%nlines
          line=u_case(fdat%line(i)%str)
          if (line(1:1) =='!') cycle
          if (index(line,'MOLE') > 0) then
             nmol=nmol+1
             pt_mol(nmol)=i
          end if
       end do

       do n=1,nmol
          if (n > 10) exit
          n_ini=pt_mol(n)
          n_end=pt_mol(n+1)-1
          if (n_end <=0) n_end=fdat%nlines


          call ReadInfo_Molecule(Fdat, Mol(n), n_ini, n_end)
          if (err_CFML%IErr /= 0) then
             write(unit=*,fmt="(a)") "  Error in read_molecule: "//trim(ERR_CFML%Msg)
             exit
          end if


          !> Search for three points (fractional coordinates)  defining a Cartesian
          !> frame up to the next molecule
          do i=n_ini, n_end
             line=adjustl(fdat%line(i)%str)
             if (line(1:1) == '!') cycle
             lline=l_case(line)
             if (lline(1:9) /= "xyz_frame") cycle

             read(unit=line(10:),fmt=*,iostat=ier) x1(:,nmol),x2(:,nmol),x3(:,nmol)
             if (ier == 0) then
                fracFrame_read(nmol)=.true.
                exit
             end if
          end do
       end do

       !> Search for a unit cell for all molecules
       do i=1, fdat%nlines
          line=adjustl(fdat%line(i)%str)
          lline=l_case(line)
          if (lline(1:4) /= "cell") cycle

          read(unit=line(5:),fmt=*,iostat=ier) cell,ang
          if (ier == 0) then
             call Set_Crystal_Cell(cell,ang,Celda,"A")
             if (err_CFML%Ierr /=0) exit
             cell_read=.true.
          end if
       end do

    End Subroutine Load_Data

    !!----
    !!---- SUBROUTINE CLOSE_FILES
    !!----
    Subroutine Close_Files()
       !---- Local Variables ----!
       logical :: info

       inquire (unit=jfildat,opened=info)
       if (info) close (jfildat)

       inquire (unit=jfilout,opened=info)
       if (info) close (jfilout)

    End Subroutine Close_Files

    !!----
    !!---- SUBROUTINE WRITE_TPCR
    !!----
    Subroutine write_tpcr(ier)
       !---- Arguments ----!
       integer, intent (in) :: ier

       !---- Local Variables ----!
       integer :: i,j

       if (ier /= 2) then
          WRITE(unit=jfilout,fmt='(/a/)')' => Spherical internal coordinates r,the,phi'
          WRITE(unit=jfilout,fmt='(a)')'!Atom Typ       x        y        z         B      Occ       P6     THETA     PHI  Spc'
          WRITE(unit=jfilout,fmt='(a)')'!  r/xc/rho the/yc/phi phi/zc/z   X0       Y0       Z0      CHI    P16:SAT   DEG  KIND'
          WRITE(unit=jfilout,fmt='(A4,1x,a4,2X,6F9.5,2f9.3,i3/11X,8F9.2/2x,F9.5,2f9.3,3f9.5,2f9.3,2i6/ 2X,7F9.2)') &
          moln%AtName(1), moln%AtSymb(1), molF%I_Coor(:,1), molF%U_iso(1), molF%Occ(1),1.0,moln%Orient(2),moln%Orient(1),0,   &
          0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,  moln%I_Coor(:,1), moln%xCentre(:),moln%Orient(3),0.0,1,0, &
          0.0,0.0,0.0,0.0,0.0,0.0,0.0
          do i=2,moln%natoms
             WRITE(unit=jfilout,fmt='(A4,1x,a4,2X,5F9.5,20x,a,i3)') &
                  moln%AtName(i), moln%AtSymb(i), molF%I_Coor(:,i), moln%U_iso(i), moln%Occ(i)," 0 0 0 ",0
             WRITE(unit=jfilout,fmt='(11X,5F9.2/2x,F9.5,2F9.3/ 2X,3F9.2)') (0.0,j=1,5),  &
                  moln%I_Coor(:,i),(0.0,j=9,11)
          end do
       end if

       if (ier /= 3) then
          WRITE(unit=jfilout,fmt='(/a/)')' => Cartesian internal coordinates xc,yc,zc'
          WRITE(unit=jfilout,fmt='(a)')'!Atom Typ       x        y        z         B      Occ       P6     THETA     PHI  Spc'
          WRITE(unit=jfilout,fmt='(a)')'!  r/xc/rho the/yc/phi phi/zc/z   X0       Y0       Z0      CHI    P16:SAT   DEG  KIND'
          WRITE(unit=jfilout,fmt='(A4,1x,a4,2X,6F9.5,2f9.3,i3/11X,8F9.2/2x,F9.5,2f9.3,3f9.5,2f9.3,2i6/ 2X,7F9.2)') &
               molc%AtName(1), molc%AtSymb(1), molF%I_Coor(:,1), molF%U_iso(1), molF%Occ(1),1.0,moln%Orient(2),moln%Orient(1),0,   &
               0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,  molc%I_Coor(:,1), molc%xCentre(:),molc%Orient(3),0.0,1,1, &
               0.0,0.0,0.0,0.0,0.0,0.0,0.0
           do i=2,moln%natoms
              WRITE(unit=jfilout,fmt='(A4,1x,a4,2X,5F9.5,20x,a,i3)') &
                   molc%AtName(i), molc%AtSymb(i), molF%I_Coor(:,i), molc%U_iso(i), molc%Occ(i)," 0 0 0 ",0
              WRITE(unit=jfilout,fmt='(11X,5F9.2/2x,F9.5,2F9.3/ 2X,3F9.2)') (0.0,j=1,5),  &
                   molc%I_Coor(:,i),(0.0,j=9,11)
           end do
       end if

       if (ier /= 4) then
          WRITE(unit=jfilout,fmt='(/a/)')' => Z-matrix type: distance - bond angle - torsion angle'
          WRITE(unit=jfilout,fmt='(a)')'!Atom Typ       x        y        z         B      Occ       P6     THETA     PHI  Spc'
          WRITE(unit=jfilout,fmt='(a)')'!   dist   Bond-ang  Torsion-ang  X0       Y0       Z0      CHI    Connectiv DEG  KIND'
          WRITE(unit=jfilout,fmt='(A4,1x,a4,2X,6F9.5,2f9.3,i3/11X,8F9.2/2x,F9.5,2f9.3,3f9.5,2f9.3,2i6/ 2X,7F9.2)') &
               molz%AtName(1), molz%AtSymb(1), molF%I_Coor(:,1), molF%U_iso(1), molF%Occ(1),4.0,molz%Orient(2),molz%Orient(1),0,   &
               0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,  molz%I_Coor(:,1), molz%xCentre(:),molz%Orient(3),0.0,1,3, &
               0.0,0.0,0.0,0.0,0.0,0.0,0.0
          do i=2,moln%natoms
             WRITE(unit=jfilout,fmt='(a4,1x,a4,2X,5F9.5,8x,3i4,7x,i3)') &
                  molz%AtName(i), molz%AtSymb(i), molF%I_Coor(:,i), molz%U_iso(i), molz%Occ(i),molz%Conn(:,i),0
             WRITE(unit=jfilout,fmt='(11X,5F9.2/2x,F9.5,2F9.3/ 2X,3F9.2)') (0.0,j=1,5),  &
                  molz%I_Coor(:,i),(0.0,j=9,11)
          end do
       end if

    End Subroutine write_tpcr

    !!----
    !!---- SUBROUTINE WRITE_TCFL
    !!----
    Subroutine write_tcfl(k)
       !---- Arguments ----!
       integer, intent(in) :: k

       !---- Local Variables ----!
       integer           :: i,j
       character(len=10) :: lab,chem_prev

       if (k == 1) then
          write(unit=i_cfl,fmt="(a)")        "TITLE CFL_File generated by Mol2PCR from: "//trim(fildat)
          write(unit=i_cfl,fmt="(a)")        "!"
          write(unit=i_cfl,fmt="(a,6f12.5)") "Cell ",Celda%cell,Celda%ang
          write(unit=i_cfl,fmt="(a)")        "!"
          write(unit=i_cfl,fmt="(a)")        "SpGR   I 1"
          write(unit=i_cfl,fmt="(a)")        "!"
          write(unit=i_cfl,fmt="(a)")        "box  -0.25 1.25 -0.25 1.25  -0.1 1.1"
       end if

       j=1
       i=1
       write(unit=lab,fmt="(a,i3)") trim(molc%AtSymb(i))//mol(k)%Name_mol(1:1), j
       lab=pack_string(lab)
       WRITE(unit=i_cfl,fmt='(a,a4,2X,5F9.5)') &
            "Atom  "//lab, molc%AtSymb(i), molF%I_Coor(:,i), molc%U_iso(i), molc%Occ(i)

       chem_prev=molc%AtSymb(i)
       do i=2,molc%natoms
          if (chem_prev /= molc%AtSymb(i)) then
             j=0
             chem_prev = molc%AtSymb(i)
          end if
          j=j+1
          write(unit=lab,fmt="(a,i3)") trim(molc%AtSymb(i))//mol(k)%Name_mol(1:1),j
          lab=pack_string(lab)
          WRITE(unit=i_cfl,fmt='(a,a4,2X,5F9.5)') &
               "Atom  "//lab, molc%AtSymb(i), molF%I_Coor(:,i), molc%U_iso(i), molc%Occ(i)
       end do

    End Subroutine write_tcfl

    !!----
    !!---- SUBROUTINE CALCULATION
    !!----
    Subroutine calculation()
       !---- Local variables ----!
       integer              :: i,ier
       real                 :: theta,phi,chi
       real, dimension(3)   :: angles,tr
       real, dimension(3,3) :: EuM,Rot

       if (cell_read) call Write_Crystal_Cell(Celda, jfilout)

       do i=1,nmol
          if (fracFrame_read(i)) then

             call get_euler_from_fract(x1(:,i),x2(:,i),x3(:,i),Celda%Cr_Orth_cel,phi,theta,chi,EuM, Code="D")
             write(unit=jfilout,fmt="(/a,i2)") " => INPUT MOLECULE #",i
             write(unit=jfilout,fmt="(a)")  "  Conversion of Fractional coordinates to internal coordinates given P1,P2,P3"
             write(unit=jfilout,fmt="(a/)") "  z//P1-P3, x within the plane (P1-P3,P2-P3), y completes the direct frame "
             write(unit=jfilout,fmt="(a,a)")" => Euler Matrix (get_euler_from_fract)                      ",&
                                            "Input Points                      Fractional to Cartesian"

             write(unit=jfilout,fmt="(3(tr10,3f10.5))")  EuM(1,:),x1(:,i),Celda%Cr_Orth_cel(1,:)
             write(unit=jfilout,fmt="(3(tr10,3f10.5))")  EuM(2,:),x2(:,i),Celda%Cr_Orth_cel(2,:)
             write(unit=jfilout,fmt="(3(tr10,3f10.5))")  EuM(3,:),x3(:,i),Celda%Cr_Orth_cel(3,:)

             !>Save the angles read with the molecule and set the given Euler Matrix
             angles=mol(i)%orient
             tr=mol(i)%xcentre(:)
             Rot = Set_euler_matrix(Mol(i)%rot_type, angles(1),angles(2),angles(3) )
             mol(i)%orient(1)= phi
             mol(i)%orient(2)= theta
             mol(i)%orient(3)= chi
             mol(i)%xcentre(:)= x3(:,i)+tr
             EuM = Set_euler_matrix(Mol(i)%rot_type, phi,theta,chi)
             Mol(i)%Euler=matmul(Rot,EuM)
             Mol(i)%is_EulerMat=.true.

             call WriteInfo_molecule(mol(i),jfilout)

             ier=0
             select case (mol(i)%coor_type)

                case("F")
                   call init_molecule(molF,mol(i)%Natoms)
                   molF=mol(i)

                   call fractional_to_spherical(mol(i),celda,moln)
                   if (err_CFML%Ierr /=0) ier=2

                   call spherical_to_cartesian(moln,molc)
                   if (err_CFML%Ierr /=0) ier=3

                   call fractional_to_Zmatrix(mol(i),celda,molz)
                   if (err_CFML%Ierr /=0) ier=4

               Case("C")
                   call init_molecule(molc,mol(i)%Natoms)
                   molc=mol(i)

                   call cartesian_to_spherical(mol(i),moln)
                   if (err_CFML%Ierr /=0) ier=2

                   call cartesian_to_fractional(mol(i),celda,molF)
                   if (err_CFML%Ierr /=0) ier=1

                   call cartesian_to_Zmatrix(mol(i),molz)
                   if (err_CFML%Ierr /=0) ier=4

               Case("S")
                   call init_molecule(moln,mol(i)%Natoms)
                   moln=mol(i)

                   call spherical_to_fractional(mol(i),celda,molF)
                   if (err_CFML%Ierr /=0) ier=1

                   call spherical_to_cartesian(mol(i),molc)
                   if (err_CFML%Ierr /=0) ier=3

                   call spherical_to_Zmatrix(mol(i),NMol=molz)
                   if (err_CFML%Ierr /=0) ier=4

               Case("Z")
                   call init_molecule(molz,mol(i)%Natoms)
                   molz=mol(i)

                   call Zmatrix_to_spherical(mol(i),moln)
                   if (err_CFML%Ierr /=0) ier=2

                   call Zmatrix_to_fractional(mol(i),celda,molF)
                   if (err_CFML%Ierr /=0) ier=1

                   call Zmatrix_to_cartesian(mol(i),molc)
                   if (err_CFML%Ierr /=0) ier=3

             End Select

             if (err_CFML%Ierr /=0) then
                write(unit=jfilout,fmt="(/,a,i2)") " => ERROR for Molecule #",i
                write(unit=jfilout,fmt="(a,/)")    "   "//trim(Err_CFML%Msg)
                write(unit=*,fmt="(/,a,i2)") " => ERROR for Molecule #",i
                write(unit=*,fmt="(a,/)")    "    "//trim(Err_CFML%Msg)
             end if

             write(unit=jfilout,fmt="(/a,i2)") " => OUTPUT MOLECULE #",i
             if (ier ==0 .or. ier /= 2) call writeinfo_molecule(moln,jfilout)
             if (ier ==0 .or. ier /= 1) call writeinfo_molecule(molF,jfilout)
             if (ier ==0 .or. ier /= 3) call writeinfo_molecule(molc,jfilout)
             if (ier ==0 .or. ier /= 4) call writeinfo_molecule(molz,jfilout)

             call  write_tcfl(i)
             call  write_tpcr(ier)

          else
             write(unit=*,fmt=*) " => No XYZ_Frame instruction has been given for molecule #",i
             if (cell_read)  then
                write(unit=*,fmt=*) " => Transformation to Z-matrix of molecule #",i
                call fractional_to_Cartesian(mol(i),celda,molc)
                call fractional_to_Zmatrix(mol(i),celda,molz)

                call init_molecule(molF,mol(i)%Natoms)
                molF=mol(i)
                call writeinfo_molecule(molF,jfilout)
                call writeinfo_molecule(molc,jfilout)
                call writeinfo_molecule(molz,jfilout)

             else
               write(unit=*,fmt=*) " => Transformation from Z-matrix to cartesian of molecule #",i
               call init_molecule(molz,mol(i)%Natoms)
               molz=mol(i)
               call Zmatrix_to_spherical(mol(i),moln)
               call Zmatrix_to_cartesian(mol(i),molc)
               call writeinfo_molecule(mol(i),jfilout)
               call writeinfo_molecule(moln,jfilout)
               call writeinfo_molecule(molc,jfilout)
             end if

          end if

       end do
       write(unit=i_cfl,fmt="(a)") "conn  Mn O  0 2.3"

    End  Subroutine calculation

 End Module Mol_Param

 !!----
 !!---- Program to test the molecular module
 !!----
 !!
 Program Mol_TPCR
    !---- Use Modules ----!
    Use Mol_Param

    !---- Variables ----!
    implicit none
    integer :: i

    !> Command Line
    call getinfo_command()

    !> Load Data
    if (.not. ierror) call load_data()
    if (nmol==0) then
       write(unit=*,fmt=*) " => No molecule is loaded!... program interrupted!"
       if (err_CFML%Ierr /=0) write(unit=*,fmt=*) trim(Err_CFML%Msg)
       stop
    end if

    if (.not. cell_read) then
       write(unit=*,fmt="(a)") " => No cell given!"
    end if

    write(unit=jfilout,fmt="(a)") "  ----------------------------------------------------------------------"
    write(unit=jfilout,fmt="(a)") "  PROGRAM MOL_TPCR: Conversion of molecules to rigid bodies for FullProf"
    write(unit=jfilout,fmt="(a)") "  ----------------------------------------------------------------------"
    write(unit=jfilout,fmt="(a)") "                  (JRC -- ILL version: April 2022)"
    write(unit=jfilout,fmt="(a)") "                  "
    write(unit=jfilout,fmt="(a)") "  Information about the unit cell and reference frames: "
    write(unit=*,fmt="(a)") "  ----------------------------------------------------------------------"
    write(unit=*,fmt="(a)") "  PROGRAM MOL_TPCR: Conversion of molecules to rigid bodies for FullProf"
    write(unit=*,fmt="(a)") "  ----------------------------------------------------------------------"
    write(unit=*,fmt="(a)") "                  (JRC -- ILL version: April 2022)"
    write(unit=*,fmt="(a)") "                  "
    write(unit=*,fmt="(a,i2)") " => Number of molecules read: ",nmol

    do i=1,nmol
       write(unit=*,fmt="(a,i2,a)") "        Name of molecule #",i,": "//trim(mol(i)%name_mol)
       write(unit=*,fmt="(a,a)")    "        Type of coordinates : ", trim(mol(i)%coor_type)
    end do

    if (.not. ierror) call calculation()
    write(unit=*,fmt="(a)") " => Program finished ... see output file: "//trim(filout)
    write(unit=*,fmt="(a)") " => CFL file with fractional coordinates: "//trim(file_cfl)

    !> Closing Files
    call Close_files()

 End Program  Mol_TPCR

