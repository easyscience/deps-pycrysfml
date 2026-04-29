 SubModule (CFML_SXTAL_Geom) SXTAL_IO

  implicit none

  Contains

    !!----
    !!----    Module Subroutine Read_Twinlaw(Twin,read_ok,lun,fich_cfl)
    !!----      Type(twin_type),   intent (out)          :: Twin
    !!----      Logical,           intent (out)          :: read_ok
    !!----      integer,           intent (in), optional :: lun !logical unit of the file to be read
    !!----      Type (file_type),  intent (in), optional :: fich_cfl
    !!----
    !!----
    !!----    Subroutine to read the necessary items for defining a twin-law and constructing twin_type.
    !!----    Imported from Laue module. If lun is present fich_cfl cannot be present and viceversa.
    !!----
    !!----    Updated September 2018
    !!
    Module Subroutine Read_Twinlaw(Twin,read_ok,lun,fich_cfl)
      Type(twin_type),  intent (out)          :: Twin
      Logical,          intent (out)          :: read_ok
      integer,          intent (in), optional :: lun !logical unit of the file to be read
      Type (file_type), intent (in), optional :: fich_cfl
      ! Local variables
      integer :: ier,i,n, nmat,n_lin
      character(len=132)          :: line
      real(kind=cp), dimension(3) :: angls

      read_ok=.false.
      !The different twins have the identity matrix at starting point
      Twin%Twin_name=" No-name "
      Twin%ityp=0
      Twin%Twin_Mat=0.0_cp
      n_lin=0
      do i=1,48
        do n=1,3
          Twin%Twin_Mat(n,n,i)=1.0_cp
        end do
      end do
      nmat=0
      n=0

      do

        if(present(lun)) then

          n_lin=n_lin+1
          read(unit=lun,fmt="(a)",iostat=ier) line
          if(ier /= 0) then
            if(ier < 0) exit
            write(unit=Err_CFML%Msg,fmt="(a,i5)") " => Error reading in the input file, at line: ",n_lin
            Err_CFML%Ierr=1
            exit
          end if

        else if(present(fich_cfl)) then

          n=n+1
          if(n > fich_cfl%nlines) exit
          line=fich_cfl%line(n)%Str

        end if

        line=adjustl(line)
        if(line(1:1) == "!" .or. line(1:1) == "#") cycle

        if(line(1:8) == "TWIN_nam") then
          Twin%twin_name = line(9:)

        else if(line(1:8) == "TWIN_typ") then

          Read(unit=line(9:),fmt=*,iostat=ier) Twin%ityp
          if(ier /= 0) then
            Err_CFML%Msg=" => Error reading the TWIN law file, item: TWIN_typ"
            Err_CFML%Ierr=1
            return
          end if

          if(Twin%ityp == 4) then
             nmat=1  ! The domain #1 has always rotation matrix equal to identity
          end if
          if(Twin%ityp < 1 .or. Twin%ityp > 4) then
            Err_CFML%Msg=" => Illegal TWIN_typ: It should be 1, 2, 3 or 4!"
            Err_CFML%Ierr=1
           return
          end if

        else if (line(1:8) == "TWIN_rot") then   !This is for Twin%ityp = 4

          nmat=nmat+1
          Read(unit=line(9:),fmt=*,iostat=ier) twin%twin_axis(:,nmat),twin%twin_ang(nmat)
          if(ier /= 0) then
            Err_CFML%Msg=" => Error reading the TWIN law file, item: TWIN_rot"
            Err_CFML%Ierr=1
            return
          end if
          !Calculation of the rotation matrix
          Twin%Twin_Mat(:,:,nmat)=Rot_Gibbs_Matrix(twin%twin_axis(:,nmat),twin%twin_ang(nmat))
          Twin%Twin_Matinv(:,:,nmat)=transpose(Twin%Twin_Mat(:,:,nmat))

        else if (line(1:8) == "TWIN_uma") then !This is another alternative for Twin%ityp = 4

          nmat=nmat+1
          Read(unit=line(9:),fmt=*,iostat=ier) angls
          if(ier /= 0) then
            Err_CFML%Msg=" => Error reading the TWIN law file, item: TWIN_uma"
            Err_CFML%Ierr=1
            return
          end if

          !Calculation of the rotation matrix
          Twin%Twin_Mat(:,:,nmat)= Set_Rotation_Matrix(angls)
          Twin%Twin_Matinv(:,:,nmat)=transpose(Twin%Twin_Mat(:,:,nmat))
          Call Get_Anglen_Axis_From_RotMat(Twin%Twin_Mat(:,:,nmat),Twin%Twin_axis(:,nmat),Twin%Twin_ang(nmat))

        else if (line(1:8) == "TWIN_mat") then   !This is for cases Twin%ityp /= 4

          nmat=nmat+1
          Read(unit=line(9:),fmt=*,iostat=ier) (Twin%Twin_Mat(i,:,nmat),i=1,3)
          if(ier /= 0) then
            Err_CFML%Msg=" => Error reading the TWIN law file, item: TWIN_mat"
            Err_CFML%Ierr=1
            return
          end if
          Twin%Twin_Matinv(:,:,nmat)=Invert(Twin%Twin_Mat(:,:,nmat)) !The twin matrix here is not a rotation matrix

        else if (line(1:8) == "TWIN_end") then
          exit
        end if
      end do

      Twin%N_twins=nmat
      if(Twin%ityp == 0 .and. Twin%N_twins > 1) then
         Err_CFML%Msg=" => Error: The type of twin is unknown! TWIN_typ not given!"
         Err_CFML%Ierr=1
         return
      end if
      read_OK=.true.
    End Subroutine Read_Twinlaw

    !!---- Module Subroutine Write_Twinlaw(Twin,lun,cell)
    !!----   Type(twin_type),             intent(in out) :: Twin
    !!----   integer,                     intent(in)     :: lun !logical unit of the file to be written
    !!----   type(Cell_G_Type), optional, intent(in)     :: cell
    !!----
    !!----   Subroutine imported from Laue modules. Write the available information on twin laws
    !!----
    !!----
    Module Subroutine Write_Twinlaw(Twin,lun,cell)
      Type(twin_type),             intent(in out) :: Twin
      integer,                     intent(in)     :: lun !logical unit of the file to be written
      type(Cell_G_Type), optional, intent(in)     :: cell

      !--- Local variables ---!
      integer :: i,j,l
      real(kind=cp)                 :: rmodul
      real(kind=cp), dimension(3,3) :: tinv,a,b, og
      real(kind=cp), dimension(  3) :: h

      write(unit=lun,fmt="(/,a)") "    INFORMATION GIVEN IN THE TWIN-LAW COMMANDS"
      write(unit=lun,fmt="(a,/)") "    ------------------------------------------"
      write(unit=lun,fmt="(a,a)") " => Name of the twin-law: ", trim(Twin%twin_name)
      Select case (Twin%ityp)
        case(1)
           write(unit=lun,fmt="(a)")  " => Type of the twin-law:  TWIN_typ=1 "
           write(unit=lun,fmt="(a)")  "         The domains contributing to an observation have indices      "
           write(unit=lun,fmt="(a)")  "         obtained from the input (h,k,l) by multiplying the vector    "
           write(unit=lun,fmt="(a)")  "         (hkl) by the matrix P, so (hkl)n = P (hkl) {n=1, ndomains}   "
           write(unit=lun,fmt="(a)")  "         The domains contributing to each observation are those that  "
           write(unit=lun,fmt="(a)")  "         give integer indices (hkl)n after applying the matrix P.     "
           write(unit=lun,fmt="(a)")  "         The new indices are (hkl)n.                                  "
        case(2)
           write(unit=lun,fmt="(a)")  " => Type of the twin-law:  TWIN_typ=2 "
           write(unit=lun,fmt="(a)")  "         The input indices are given w.r.t. the super-lattice. It is   "
           write(unit=lun,fmt="(a)")  "         supposed that a sub-lattice exist and the orientation of the  "
           write(unit=lun,fmt="(a)")  "         superlattice could have different orientational domains.      "
           write(unit=lun,fmt="(a)")  "         Each domain is entered as a matrix relating the superlattice  "
           write(unit=lun,fmt="(a)")  "         direct cell to the parent sub-lattice.                        "
        case(3)
           write(unit=lun,fmt="(a)")  " => Type of the twin-law:  TWIN_typ=3 "
           write(unit=lun,fmt="(a)")  "         The input indices correspond to the first domain. The orientation "
           write(unit=lun,fmt="(a)")  "         of the first domain with respect to a cartesian frame is given    "
           write(unit=lun,fmt="(a)")  "         by the user as well as for the other domains.                     "
           write(unit=lun,fmt="(a)")  "         Each domain is given by the orientation in the Cartesian frame    "
           write(unit=lun,fmt="(a)")  "         of each cell parameter a, b, c. Only the direction is needed.      "
        case(4)
           write(unit=lun,fmt="(a)")  " => Type of the twin-law:  TWIN_typ=4 "
           write(unit=lun,fmt="(a)")  "         The input indices correspond to the first domain. The orientation       "
           write(unit=lun,fmt="(a)")  "         of the n-domain (n/=1) is obtained by a rotation matrix for which       "
           write(unit=lun,fmt="(a)")  "         only the direction in the Busing-Levy cartesian frame attached to the   "
           write(unit=lun,fmt="(a)")  "         crystal and the value of the angle are given. The program calculates    "
           write(unit=lun,fmt="(a)")  "         the Cartesian vectors  z1(n)= U R(n) B h for h indices corresponding to "
           write(unit=lun,fmt="(a)")  "         all equivalent reflections plus those that have a close value of        "
           write(unit=lun,fmt="(a)")  "         d-spacing. If |z1(n)-z1(1)| < delta the corresponding indices may       "
           write(unit=lun,fmt="(a)")  "         contribute to the total intensity.                                      "
           write(unit=lun,fmt="(a)")  "          "
      End Select

      write(unit=lun,fmt="(a,i3)")    " => Number of matrices describing the twin-law: ",Twin%N_twins

      if(Twin%ityp /= 4) then
        do i=1,Twin%N_twins
            write(unit=lun,fmt="(a,i3)")       " => Input matrix for domain Number: ", i
            do j=1,3
             write(unit=lun,fmt="(a,3f10.5)")  "                   ", Twin%Twin_Mat(j,:,i)
            end do
        end do
      else
        do i=2,Twin%N_twins
            write(unit=lun,fmt="(/,a,i3)")        " => Input axis and angle for domain Number: ", i
            write(unit=lun,fmt="(a,3f10.5,a,f10.5)") "            [uvw]=[",Twin%Twin_axis(:,i),"]   ang=",Twin%Twin_ang(i)
            write(unit=lun,fmt="(a)")             "    Rotation matrix: "
            do j=1,3
             write(unit=lun,fmt="(a,3f10.5)")     "                   ", Twin%Twin_Mat(j,:,i)
            end do
        end do
      end if

      !----------------------------------
      If(Twin%ityp == 1) Then    !itwin
      !----------------------------------
        write(unit=lun,fmt="(a)")  " => Final direct and inverse matrices acting on (hkl): "
        Do l=1,Twin%N_twins
          Twin%Twin_Matinv(:,:,l) =  invert(Twin%Twin_Mat(:,:,l))
          write(unit=lun,fmt="(a,i3)")                    " => Matrices for domain Number: ", l
          do j=1,3
           write(unit=lun,fmt="(a,3f10.5,a,3f10.5)")  "         ", Twin%Twin_Mat(j,:,l),"     ",Twin%Twin_Matinv(j,:,l)
          end do
        End Do
      !----------------------------------
      Else If(Twin%ityp == 2) Then    !itwin
      !----------------------------------
        tinv=invert(Twin%Twin_Mat(:,:,1))
        write(unit=lun,fmt="(a)")  " => Final direct and inverse matrices acting on (hkl): "
        Do l=1,Twin%N_twins
          Twin%Twin_Mat(:,:,l)  =  matmul(Twin%Twin_Mat(:,:,l),tinv)
          Twin%Twin_Matinv(:,:,l) =  invert(Twin%Twin_Mat(:,:,l))
          write(unit=lun,fmt="(a,i3)")                    " => Matrices for domain Number: ", l
          do j=1,3
           write(unit=lun,fmt="(a,3f10.5,a,3f10.5)")  "         ", Twin%Twin_Mat(j,:,l),"     ",Twin%Twin_Matinv(j,:,l)
          end do
        End Do
      !---------------------------------------
      Else If(Twin%ityp == 3  ) Then
      !---------------------------------------
        if(present(cell)) then
           write(unit=lun,fmt="(a)")  " => Final direct and inverse matrices acting on (hkl): "
           Do l=1,Twin%N_twins
             Do i=1,3
               h(:)= Twin%Twin_Mat(i,:,l)
               rmodul= Sqrt(h(1)*h(1)+h(2)*h(2)+h(3)*h(3))
               b(i,:)=cell%cell(i)*h(:)/rmodul    !M(L)
             End Do
             a=invert(b)                        !A=M(L)-1
             tinv=transpose(a)                    !TINV= (M(L)-1)t
             If(l == 1) Then
               a=transpose(b)                     !  A=M(1)t
               og=matmul(a,cell%gr)               !  M(1)t * GG
             End If
             b=matmul(cell%gd,tinv)               !GD *  (M(L)-1)t
             Twin%Twin_Mat(:,:,l)=matmul(b,og)    !GD *  (M(L)-1)t  * M(1)t * GG
             Twin%Twin_Matinv(:,:,l) =  invert(Twin%Twin_Mat(:,:,l))  !B=A-1
             write(unit=lun,fmt="(a,i3)")                    " => Matrices for domain Number: ", l
             do j=1,3
              write(unit=lun,fmt="(a,3f10.5,a,3f10.5)")  "         ", Twin%Twin_Mat(j,:,l),"     ",Twin%Twin_Matinv(j,:,l)
             end do
           End Do

        else
           Err_CFML%Msg=" => Error: fow TWIN_typ=3, cell must be given!"
           Err_CFML%Ierr=1
        end if
      !----------------------------------
      End If
    End Subroutine Write_Twinlaw

 End SubModule SXTAL_IO
