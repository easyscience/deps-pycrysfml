!!-------------------------------------------------------------
!!---- FullProf software
!!
!! @license   Copyright 2019, Juan Rodriguez Carvajal, Institut Laue-Langevin All rights reserved (see LICENSE)
!! @authors   Juan Rodriguez Carvajal (see AUTHORS)
!!
!!  Module for Twin calculations
!!
!!
!! ITWIN= 1 The domains contributing to an observation have indices
!!          obtained from the input (h,k,l) by multiplying the vector
!!          (hkl) by a real  matrix P, so that (hkl)n = P (hkl)
!!          The number of the domains contributing to each observation
!!          depends if the resulting indices (hkl)n are integers.
!!
!! ITWIN= 2 The input indices are given w.r.t. the super-lattice. It is
!!          supposed that a sub-lattice exist and the orientation of the
!!          superlattice could have different orientational domains.
!!          Each domain is entered as a matrix relating the superlattice
!!          direct cell to the parent sub-lattice.
!!
!! ITWIN= 3 The input indices correspond to the first domain. The orientation
!!          of the first domain with respect to a cartesian frame is given
!!          by the user as well as for the other domains.
!!          Each domain is given by the orientation in the cartesian frame
!!          of each cell parameter a,b, c. Only the direction is needed
!!          The program calculates internally the director cosinuses.
!!
!! ITWIN= 4 The input indices correspond to the first domain. The orientation
!!          of the n-domain (n/=1) is obtained by a rotation matrix for which
!!          only the direction in the Busing-Levy cartesian frame attached to the
!!          crystal and the value of the angle are given. The program calculates
!!          the Cartesian vectors  z1(n)= U R(n) B h for h indices corresponding to
!!          all equivalent reflections plus those that have a close value of
!!          d-spacing. If |z1(n)-z1(1)| < delta the corresponding indices may
!!          contribute to the total intensity.
!!
!!
!!   Whatever the TWIN model the final matrices act on (hkl) as for ITWIN=1.
!!   For ITWIN=2,3 the angular position of the motors are tested for an eventual
!!   contribution even if the resulting indices are not integer.
!!
!!
! -------------------------------------------------------------
   Module Twin_mod
       Use CFML_GlobalDeps,    only: cp
       Use CFML_Messages,      only: Error_message
       use CFML_Strings,       only: File_Type,u_case
       Use CFML_Maths,         only: Zbelong, epss, inverse_matrix
       Use CFML_Metrics,       only: Cell_G_Type, Rot_Gibbs_Matrix
       Use CFML_gSpaceGroups,  only: SpG_type
       Use CFML_Reflections,   only: h_absent, mh_absent
       implicit none

       Private
       Public :: read_twinlaw, write_twinlaw, get_domain_contrib, Get_Angles, z1frnb

       integer, parameter,public                    :: nmax=24
       logical, public                              :: read_OK =.false.    !> If the twin law has been read => read_OK=.true.
       integer,public                               :: imach               !> Number of the machine type
       real(kind=cp),public                         :: lambda              !> Wavelength
       real(kind=cp),public                         :: tol=0.5             !> Angular tolerance

       Type, public :: Twin_type
          logical                               :: iubm    =.false.    !> The UB-matrix has been given => iubm=.true.
          logical                               :: iSpG    =.false.    !> The Space Group Symbol for domains has been given => iSpG=.true.
          logical                               :: itransf =.false.    !> Transform the UB-matrix => itransf=.true.
          integer                               :: itwin               !> Type of twin
          integer                               :: nmat                !> Number of domain matrices
          character (len=:),allocatable         :: Machine             !> Name of the diffractometer
          character (len=:),allocatable         :: twin_name           !> Name of the Twin Law
          character (len=:),allocatable         :: twin_SpG            !> Symbol of the space Group to treat domains
          real(kind=cp), dimension (3,3,0:nmax) :: twin=0.0, twini=0.0 !> Matrices describing the domains
          real(kind=cp), dimension (3,3,0:nmax) :: rtwin=0.0           !> Rotation Matrices describing the domains
          real(kind=cp), dimension (3,0:nmax)   :: ax_twin=0.0         !> Rotation axes for twin domains
          real(kind=cp), dimension (0:nmax)     :: ang_twin=0.0        !> Rotation angles for twin domains
          real(kind=cp), dimension (3,3)        :: tr                  !> tranformation matrix
          real(kind=cp), dimension (3,3)        :: ubm                 !> UB-matrix
          real(kind=cp), dimension (3,3)        :: ubmi                !> inverse of the UB-matrix
       End Type Twin_type


      contains
         !> @brief
         !!    Subroutine read_twinlaw(fich_cfl,n,Twins)
         !!      Type (file_type), intent(in)     :: fich_cfl ! File_type containing the twin information
         !!      integer,          intent(in out) :: n        ! Number of the line to start reading
         !!      Type (Twin_type), intent(out)    :: Twins    ! Twin object
         !!
         !!    Subroutine to read the necessary items for defining a twin-law.
         !!    The different global variables of the module are loaded
         !!
         !!
         Subroutine read_twinlaw(fich_cfl,n,Twins)
           Type (file_type), intent(in)     :: fich_cfl
           integer,          intent(in out) :: n
           Type (Twin_type), intent(out)    :: Twins

           !--- Local variables ---!
           integer                      :: ier,i,j,nmat
           character(len=:),allocatable :: line,kw
           !logical :: twin_rot_given

           nmat=0

           line="      "
           do
             n=n+1
             if(n > fich_cfl%nlines) exit
             line=adjustl(fich_cfl%line(n)%Str)
             if(len_trim(line) == 0) cycle
             if(line(1:1) == "!" .or. line(1:1) == "#") cycle
             j=index(line," ")
             if (j == 0) cycle
             kw=u_case(line(1:j-1))

             Select Case (kw)
               case("TWIN_NAM")
                 Twins%twin_name = line(j:)

               case("TWIN_TYP")
                 Read(unit=line(j:),fmt=*,iostat=ier) Twins%itwin
                 if(ier /= 0) then
                   call Error_message (" => Error reading the TWIN law file, item: TWIN_typ")
                   return
                 end if
                 if(Twins%itwin == 4) then
                    nmat=1  ! The domain #1 has always rotation matrix equal to identity
                 end if
                 if(Twins%itwin < 1 .or. Twins%itwin > 4) then
                   call Error_message (" => Illegal TWIN_type: It should be 1,2,3 or 4!")
                   return
                 end if

               case("TWIN_TRF")

                 Twins%itransf=.true.
                 Read(unit=line(j:),fmt=*,iostat=ier) (Twins%tr(i,:),i=1,3)
                 if(ier /= 0) then
                   call Error_message (" => Error reading the TWIN law file, item: TWIN_trf")
                   return
                 end if

               case("TWIN_ROT")

                 if(Twins%itwin /= 4) then
                   call Error_message (" => Giving TWIN_rot, implies that ITwin should be equal to 4!" )
                   write(unit=*,fmt="(a)") " => The program is changing ITwin to ITwin=4 automatically for continuing smoothly... "
                   Twins%itwin = 4
                   nmat=1
                 end if

                 nmat=nmat+1
                 Read(unit=line(j:),fmt=*,iostat=ier) Twins%ax_twin(:,nmat),Twins%ang_twin(nmat)
                 if(ier /= 0) then
                   call Error_message (" => Error reading the TWIN law file, item: TWIN_rot")
                   return
                 end if
                 !Calculation of the rotation matrix
                 Twins%rtwin(:,:,nmat)=Rot_Gibbs_Matrix(Twins%ax_twin(:,nmat),Twins%ang_twin(nmat))

               case("TWIN_MAT")

                 nmat=nmat+1
                 Read(unit=line(j:),fmt=*,iostat=ier) ( Twins%twin(i,:,nmat),i=1,3)
                 if(ier /= 0) then
                   call Error_message (" => Error reading the TWIN law file, item: TWIN_mat")
                   return
                 end if

               case("TWIN_UBM")

                 Twins%iubm=.true.
                 Read(unit=line(j:),fmt=*,iostat=ier) (Twins%ubm(i,:),i=1,3)
                 if(ier /= 0) then
                   call Error_message (" => Error reading the TWIN law file, item: TWIN_ubm")
                   return
                 end if

               case("TWIN_MAC")

                 Twins%machine= trim(line(j:))

               case("TWIN_SPG")
                Twins%iSpG=.true.
                Twins%twin_Spg=adjustl(trim(line(j:)))

               case("TWIN_END")
                 exit
             End Select
           end do
           twins%nmat=nmat
           read_OK=.true.

         End Subroutine read_twinlaw

         !> @brief
         !!  Subroutine write_twinlaw(lun,twins,cell)
         !!    integer,                     intent(in)     :: lun   !> logical unit of the file for writing
         !!    type(twin_type),             intent(in out) :: twins !> Twin object
         !!    type(Cell_G_Type), optional, intent(in)     :: cell  !> Unit cell object
         !!----
         !!----
         !!----
         Subroutine write_twinlaw(lun,twins,cell)
           integer,                     intent(in)     :: lun    !> logical unit of the file for writing
           type(twin_type),             intent(in out) :: twins  !> Twin object
           type(Cell_G_Type), optional, intent(in)     :: cell   !> Unit cell object

           integer :: i,j,l
           real(kind=cp)                 :: rmodul
           real(kind=cp), dimension(3,3) :: tinv,a,b, og
           real(kind=cp), dimension(  3) :: h

           if(read_OK) then
             write(unit=lun,fmt="(/,a)") "    INFORMATION GIVEN IN THE TWIN-LAW COMMANDS"
             write(unit=lun,fmt="(a,/)") "    ------------------------------------------"
             write(unit=lun,fmt="(a,a)") " => Name of the twin-law: ", trim(twins%twin_name)
             Select case (twins%itwin)
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


             write(unit=lun,fmt="(a,f6.3)")  " => Epsilon for real/integer comparisons set to: ",epss
             write(unit=lun,fmt="(a,i3)")    " => Number of matrices describing the twin-law: ",twins%nmat

             if(twins%itwin /= 4) then
               do i=1,twins%nmat
                   write(unit=lun,fmt="(a,i3)")          " => Input matrix for domain Number: ", i
                   do j=1,3
                    write(unit=lun,fmt="(a,3f10.5)")  "                   ", twins%twin(j,:,i)
                   end do
               end do
             else
               do i=2,twins%nmat
                   write(unit=lun,fmt="(/,a,i3)")        " => Input axis and angle for domain Number: ", i
                write(unit=lun,fmt="(a,3f10.5,a,f10.5)") "            [uvw]=[",twins%ax_twin(:,i),"]   ang=",twins%ang_twin(i)
                   write(unit=lun,fmt="(a)")             "    Rotation matrix: "
                   do j=1,3
                    write(unit=lun,fmt="(a,3f10.5)")     "                   ", twins%rtwin(j,:,i)
                   end do
               end do
             end if
             if(twins%iubm) then
              write(unit=lun,fmt="(/,/,a)")         " => Input UB-Matrix:   "
              do i=1,3
                write(unit=lun,fmt="(a,3f10.5)")  "                   ",twins%ubm(i,:)
              end do
              If(twins%itransf) then
               write(unit=lun,fmt="(a)")           " => Transformation Matrix:   "
               do i=1,3
                write(unit=lun,fmt="(a,3f10.5)")  "                   ",twins%tr(i,:)
               end do
               twins%ubm=matmul(twins%ubm,twins%tr)
               write(unit=lun,fmt="(a)")           " => Transformed UB-Matrix:   "
               do i=1,3
                write(unit=lun,fmt="(a,3f10.5)")  "                   ",twins%ubm(i,:)
               end do
              End if
              twins%ubmi=inverse_matrix(twins%ubm)   !Inverse Orientation Matrix
              write(unit=lun,fmt="(a)")           " => Inverse of UB-Matrix:   "
              do i=1,3
               write(unit=lun,fmt="(a,3f10.5)")  "                   ",twins%ubmi(i,:)
              end do
             end if
!----------------------------------
             If(twins%itwin == 1) Then    !itwin
!----------------------------------
               write(unit=lun,fmt="(a)")  " => Final direct and inverse matrices acting on (hkl): "
               Do l=1,twins%nmat
                 twins%twini(:,:,l) =  inverse_matrix(twins%twin(:,:,l))
                 write(unit=lun,fmt="(a,i3)")                    " => Matrices for domain Number: ", l
                 do j=1,3
                  write(unit=lun,fmt="(a,3f10.5,a,3f10.5)")  "         ", twins%twin(j,:,l),"     ",twins%twini(j,:,l)
                 end do
               End Do
!----------------------------------
             Else If(twins%itwin == 2) Then    !itwin
!----------------------------------
               tinv=inverse_matrix(twins%twin(:,:,1))
               write(unit=lun,fmt="(a)")  " => Final direct and inverse matrices acting on (hkl): "
               Do l=1,twins%nmat
                 twins%twin(:,:,l)  =  matmul(twins%twin(:,:,l),tinv)
                 twins%twini(:,:,l) =  inverse_matrix(twins%twin(:,:,l))
                 write(unit=lun,fmt="(a,i3)")                    " => Matrices for domain Number: ", l
                 do j=1,3
                  write(unit=lun,fmt="(a,3f10.5,a,3f10.5)")  "         ", twins%twin(j,:,l),"     ",twins%twini(j,:,l)
                 end do
               End Do
!---------------------------------------
             Else If(twins%itwin == 3  ) Then
!---------------------------------------
              if(present(cell)) then
               write(unit=lun,fmt="(a)")  " => Final direct and inverse matrices acting on (hkl): "
               Do l=1,twins%nmat
                 Do i=1,3
                   h(:)= twins%twin(i,:,l)
                   rmodul= Sqrt(h(1)*h(1)+h(2)*h(2)+h(3)*h(3))
                   b(i,:)=cell%cell(i)*h(:)/rmodul    !M(L)
                 End Do
                 a=inverse_matrix(b)                  !A=M(L)-1
                 tinv=transpose(a)                    !TINV= (M(L)-1)t
                 If(l == 1) Then
                   a=transpose(b)                     !  A=M(1)t
                   og=matmul(a,cell%gr)               !  M(1)t * GG
                 End If
                 b=matmul(cell%gd,tinv)               !GD *  (M(L)-1)t
                 twins%twin(:,:,l)=matmul(b,og)       !GD *  (M(L)-1)t  * M(1)t * GG
                 twins%twini(:,:,l) =  inverse_matrix(twins%twin(:,:,l))  !B=A-1
                 write(unit=lun,fmt="(a,i3)")                    " => Matrices for domain Number: ", l
                 do j=1,3
                  write(unit=lun,fmt="(a,3f10.5,a,3f10.5)")  "         ", twins%twin(j,:,l),"     ",twins%twini(j,:,l)
                 end do
               End Do
              else
                call Error_message (" => Error: fow TWIN_typ=3, cell must be given!")
                return
              end if
!----------------------------------
             End If   !itwin == 2, 3,..
!----------------------------------
           end if   !read_OK
           read_OK=.true.
         End Subroutine write_twinlaw



         !> @brief
         !!  Subroutine get_domain_contrib(h,twins,hkl,contr,angles,SpG, Cell)
         !!    real(kind=cp), dimension(3),      intent (in)           :: h
         !!    type(twin_type),                  intent (in)           :: twins
         !!    real(kind=cp), dimension(3,nmax), intent(out)           :: hkl
         !!    integer,       dimension(nmax),   intent(out)           :: contr
         !!    real(kind=cp), dimension(4),      intent (in), optional :: angles
         !!    type(SpG_Type),                   intent (in), optional :: SpG
         !!    type(Cell_G_Type),                intent (in), optional :: cell
         !!
         Subroutine get_domain_contrib(h,twins,hkl,contr,angles,SpG, Cell)
           real(kind=cp), dimension(3),      intent (in)           :: h
           type(twin_type),                  intent (in)           :: twins
           real(kind=cp), dimension(3,nmax), intent(out)           :: hkl
           integer,       dimension(nmax),   intent(out)           :: contr
           real(kind=cp), dimension(4),      intent (in), optional :: angles
           type(SpG_Type),                   intent (in), optional :: SpG
           type(Cell_G_Type),                intent (in), optional :: cell

           integer                     :: i
           real(kind=cp)               :: teta,om,aki,ph
           real(kind=cp), dimension(3) :: xp, xh
           integer, dimension(3)       :: ih

           contr=0
           if(present(Cell)) then  !do nothing ... Cell is for future development
             contr=0
           end if
           Select Case (twins%itwin)
             case(1)
                 Do i=1,twins%nmat
                   hkl(:,i)= matmul( twins%twin(:,:,i), h)
                   if(zbelong(hkl(:,i))) contr(i) = 1
                 End Do
             case(2,3)
                 hkl(:,1) = h(:)
                 contr(1) = 1
                 Do i=twins%nmat,2,-1                  !loop over domains
                   hkl(:,i)= matmul(twins%twin(:,:,i), h)   !coordinates of (hkl) in the frame of domain L
                   if(zbelong(hkl(:,i)))  then
                      contr(i) = 1
                   else
                      xh(:)=Real(Nint(hkl(:,i)))       !nearest reflection of domain L
                      xp=matmul(twins%twini(:,:,i),xh) !coordinates of the nearest refl.
                                                       !of domain L in the frame of 1
                      if(twins%iubm .and. present(angles) ) then
                        Call Get_Angles(twins%ubm,xp,Lambda,teta,om,aki,ph,imach)
                        if(abs(angles(1)-teta) < tol .and. &
                           abs(angles(2)-om  ) < tol .and. &
                           abs(angles(3)-aki ) < tol .and. &
                           abs(angles(3)-ph  ) < tol ) contr(i)= 1
                      end if
                   End If
                 End Do
             case(4)
                 !This has to be corrected (provisional)
                 hkl(:,1) = h(:)
                 contr(1) = 1
                 Do i=twins%nmat,2,-1                     !loop over domains
                   hkl(:,i)= matmul(twins%twin(:,:,i), h) !coordinates of (hkl) in the frame of domain L
                   if(zbelong(hkl(:,i)))  then
                      contr(i) = 1
                   else
                      xh(:)=Real(Nint(hkl(:,i)))       !nearest reflection of domain L
                      xp=matmul(twins%twini(:,:,i),xh) !coordinates of the nearest refl.
                                                       !of domain L in the frame of 1
                      if(twins%iubm .and. present(angles) ) then
                        Call Get_Angles(twins%ubm,xp,Lambda,teta,om,aki,ph,imach)
                        if(abs(angles(1)-teta) < tol .and. &
                           abs(angles(2)-om  ) < tol .and. &
                           abs(angles(3)-aki ) < tol .and. &
                           abs(angles(3)-ph  ) < tol ) contr(i)= 1
                      end if
                   End If
                 End Do

           End Select
           if(present(SpG)) then
             do i=1,twins%nmat
              ih=nint(hkl(:,i))
              if(h_absent(ih,SpG) .and. mh_absent(ih,SpG)) contr(i) = -1
             end do
           end if

         End Subroutine get_domain_contrib

         !> @brief
         !!      Subroutine to get the angular positions of motors corresponding
         !!      to the diffraction condition of reflection h=xp, when the UB-matrix
         !!      and wavelength are known.
         !!      The calculation is performed for bissecting geometry. If im=2 the
         !!      high-Chi condition is applied.
         !!
         Subroutine Get_Angles(ub,xp,wave,teta,om,chi,phi,im)
           Real(kind=cp), Intent(In),Dimension(3,3):: ub
           Real(kind=cp), Intent(In),Dimension(  3):: xp
           Real(kind=cp), Intent(In)               :: wave
           Real(kind=cp), Intent(Out)              :: teta
           Real(kind=cp), Intent(Out)              :: om
           Real(kind=cp), Intent(Out)              :: chi
           Real(kind=cp), Intent(Out)              :: phi
           Integer,       Intent(In)               :: im
           !Local variables
           real(kind=cp), parameter                :: chimax = 200.0, eps=1.E-5
           real(kind=cp)                           :: dst, sinthet, diff
           real(kind=cp), Dimension(3)             :: x

              teta=0.0
              om  =0.0
              chi =0.0
              phi =0.0
              x=matmul(ub,xp)
              dst=Sqrt(x(1)*x(1)+x(2)*x(2)+x(3)*x(3))
              If(Abs(dst) < eps) Return
              x=x/dst
              sinthet=dst*wave/2.0
              If(sinthet <= 1.0) Then
                om=Asind(sinthet)
              Else
                call Error_message (" => Reciprocal vector outside resolution sphere!")
                return
              End If
              teta=2.0*om
             !.....Bissecting geometry high-chi
              phi = atan2d(x(2),x(1))  !getting phi
              dst = Sqrt(x(1)*x(1)+x(2)*x(2))
              chi = atan2d(x(3),dst)    !getting chi
              If (im == 2) Then
                chi=180.0-chi
                phi=phi+180.0
              End If
              If (phi >  180.0) phi=phi-360.0
              If (phi < -180.0) phi=phi+360.0
              If (chi >  180.0) chi=chi-360.0
              If (chi < -180.0) chi=chi+360.0

              If (chimax > 180) Then
                diff=chimax-180
                If (chi < -180+diff .and. chi >= -180)  chi=chi+360.0
              End If

         End Subroutine Get_Angles

        !> @brief
        !!       Function z4frgn(wave,ga,nu,z4)
        !!          Real(kind=cp), Intent(In)  :: wave
        !!          Real(kind=cp), Intent(In)  :: ga,nu
        !!          Real(kind=cp), Dimension(3)  :: z4
        !!
        !!      Calculates diffraction vector in lab system from GA and NU
        !!
         Function z4frgn(wave,ga,nu) Result(z4)
            Real(kind=cp), Intent(In)  :: wave
            Real(kind=cp), Intent(In)  :: ga,nu
            Real(kind=cp), Dimension(3)  :: z4

            z4(1)=( sind(ga)*cosd(nu)     )/wave
            z4(2)=( cosd(ga)*cosd(nu)-1.0 )/wave
            z4(3)=( sind(nu)              )/wave
         End Function z4frgn

         !> @brief
         !!   Function z1frnb(wave,ga,om,nu) result(z1)
         !!      Real(kind=cp), Intent(In)    :: wave,ga,om,nu
         !!      Real(kind=cp), Dimension(3)  :: z1
         !!
         !!    Z1 from Normal Beam diffractometer angles
         !!    Calculate diffraction vector Z1 from GA, OM, NU, assuming CH=PH=0
         !!    This is is the normal beam geometry for a Lifting arm detector or
         !!    for a PSD with a single Omega axis for the sample.
         !!
         !!
         Function z1frnb(wave,ga,om,nu) result(z1)
            Real(kind=cp), Intent(In)    :: wave,ga,om,nu
            Real(kind=cp), Dimension(3)  :: z1
            !--- Local Variables ---!
            Real, Dimension(3,3) ::  omg
            Real, Dimension(3)   ::  z4

            z4 = z4frgn(wave,ga,nu)
            omg= Phi_mat(om)
            z1 = Matmul(Transpose(omg),z4)
         End Function z1frnb

         !> @brief
         !!       Function Phi_mat(phi) result(dum)
         !!         Real(kind=cp), Intent(In)    :: phi
         !!         Real(kind=cp), Dimension(3,3):: dum
         !!
         !!       Calculate the Busing and Levy conventional rotation matrix for PHI
         !!       or OMEGA [eq. 8 & 10]. The PHI/OMEGA angle must be provided in degrees.
         !!
         !!
         Function Phi_mat(phi) result(dum)
            Real(kind=cp), Intent(In)    :: phi
            Real(kind=cp), Dimension(3,3):: dum
            dum=0.0
            dum(1,1)= cosd(phi)
            dum(1,2)= sind(phi)
            dum(2,1)=-dum(1,2)
            dum(2,2)= dum(1,1)
            dum(3,3)= 1.0
         End Function Phi_mat

   End Module Twin_mod

