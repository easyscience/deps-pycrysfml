!!----
!!----
!!----
SubModule (CFML_IOForm) Format_SHX
    implicit none
   Contains

   !!----
   !!---- READ_SHX_ATOM
   !!----    Obtaining Atoms parameters from Shelx file (.ins or .res)
   !!----
   !!---- 26/06/2019
   !!
   Module Subroutine Read_SHX_Atom(shx, n_fvar, fvar, elem_type, cell, AtmList)
      !---- Arguments ----!
      type(File_Type),                intent(in)      :: shx
      integer,                        intent(in)      :: n_fvar
      real(kind=cp), dimension(:),    intent(in)      :: fvar
      character(len=*), dimension(:), intent(in)      :: elem_type
      class(Cell_G_Type),             intent(in)      :: Cell
      type (AtList_type),             intent(out)     :: AtmList

      !---- Local Variables ----!
      logical                         :: flag_atm
      character(len=30),dimension(20) :: label
      character(len=2)                :: el
      integer                         :: i, j, n, nc, iv

      real(kind=cp)                   :: x, p, u

      type(atlist_type)               :: At

      !> Init
      call clear_error()
      if (shx%nlines <=0) then
         err_CFML%Ierr=1
         err_CFML%Msg="Read_SHX_Atom: No lines in the file!"
         return
      end if

      call Allocate_Atom_List(0, Atmlist,'Atm_std_type',0)

      call allocate_atom_list(shx%nlines,At,'Atm_std_type',0)
      n=0
      flag_atm=.false.
      do i=1, shx%nlines
         line=shx%line(i)%str

         !> check
         if (len_trim(line) == 0) cycle
         if (u_case(line(1:3)) == 'REM') cycle
         if (u_case(line(1:3)) == 'END') exit
         if (u_case(line(1:4)) == 'HKLF') exit
         if (u_case(line(1:4)) == 'FVAR') then
            flag_atm=.true.
            cycle
         end if
         if (.not. flag_atm) cycle

         !> Contain symbol =
         iv=index(line,'=')

         call get_words(line,label,nc)
         if (iv > 0) then
            line=shx%line(i+1)%str
            call get_words(line,label(nc:),iv)
            nc=nc+iv-1
         end if

         select case (nc)
            case (5) ! Atomname Sfac X Y Z
               call get_num(label(2),vet,ivet,iv)   ! Is Sfac integer?
               if (iv /= 1) cycle
               call get_num(label(3),vet,ivet,iv)   ! Is X real?
               if (iv /= 1) cycle
               call get_num(label(4),vet,ivet,iv)   ! Is Y real?
               if (iv /= 1) cycle
               call get_num(label(5),vet,ivet,iv)   ! Is Z real?
               if (iv /= 1) cycle

               !> Label
               n=n+1
               at%atom(n)%lab=label(1)(1:4)

               !> Chemical Symbol
               call get_num(label(2),vet,ivet,iv)
               el=elem_type(ivet(1))
               at%atom(n)%chemSymb=u_case(el(1:1))//l_case(el(2:2))

               !> Coordinates
               call get_num(label(3),vet,ivet,iv)
               at%atom(n)%x(1)=vet(1)
               call get_num(label(4),vet,ivet,iv)
               at%atom(n)%x(2)=vet(1)
               call get_num(label(5),vet,ivet,iv)
               at%atom(n)%x(3)=vet(1)

               !> Thermal
               at%atom(n)%utype="u_ij"
               at%atom(n)%thtype="iso"

            case (6) ! Atomname Sfac X Y Z Occ
               call get_num(label(2),vet,ivet,iv)   ! Is Sfac integer?
               if (iv /= 1) cycle
               call get_num(label(3),vet,ivet,iv)   ! Is X real?
               if (iv /= 1) cycle
               call get_num(label(4),vet,ivet,iv)   ! Is Y real?
               if (iv /= 1) cycle
               call get_num(label(5),vet,ivet,iv)   ! Is Z real?
               if (iv /= 1) cycle
               call get_num(label(6),vet,ivet,iv)   ! Is Occ real?
               if (iv /= 1) cycle

               !> Label
               n=n+1
               at%atom(n)%lab=label(1)(1:4)

               !> Chemical Symbol
               call get_num(label(2),vet,ivet,iv)
               el=elem_type(ivet(1))
               at%atom(n)%chemSymb=u_case(el(1:1))//l_case(el(2:2))

               !> Coordinates
               call get_num(label(3),vet,ivet,iv)
               at%atom(n)%x(1)=vet(1)
               call get_num(label(4),vet,ivet,iv)
               at%atom(n)%x(2)=vet(1)
               call get_num(label(5),vet,ivet,iv)
               at%atom(n)%x(3)=vet(1)

               !> Occupancy
               call get_num(label(6),vet,ivet,iv)
               at%atom(n)%occ=vet(1)

               !> Thermal
               at%atom(n)%utype="u_ij"
               at%atom(n)%thtype="iso"

            case (7,8) ! Atomname Sfac X Y Z Occ Uiso
               !> TR: item 8 can be electronic density created by SHELXS
               call get_num(label(2),vet,ivet,iv)   ! Is Sfac integer?
               if (iv /= 1) cycle
               call get_num(label(3),vet,ivet,iv)   ! Is X real?
               if (iv /= 1) cycle
               call get_num(label(4),vet,ivet,iv)   ! Is Y real?
               if (iv /= 1) cycle
               call get_num(label(5),vet,ivet,iv)   ! Is Z real?
               if (iv /= 1) cycle
               call get_num(label(6),vet,ivet,iv)   ! Is Occ real?
               if (iv /= 1) cycle
               call get_num(label(7),vet,ivet,iv)   ! Is Uiso real?
               if (iv /= 1) cycle

               !> Label
               n=n+1
               at%atom(n)%lab=label(1)(1:4)

               !> Chemical Symbol
               call get_num(label(2),vet,ivet,iv)
               el=elem_type(ivet(1))
               at%atom(n)%chemSymb=u_case(el(1:1))//l_case(el(2:2))

               !> Coordinates
               call get_num(label(3),vet,ivet,iv)
               at%atom(n)%x(1)=vet(1)
               call get_num(label(4),vet,ivet,iv)
               at%atom(n)%x(2)=vet(1)
               call get_num(label(5),vet,ivet,iv)
               at%atom(n)%x(3)=vet(1)

               !> Occupancy
               call get_num(label(6),vet,ivet,iv)
               at%atom(n)%occ=vet(1)

               !> U_iso
               call get_num(label(7),vet,ivet,iv)
               at%atom(n)%U_iso=vet(1)

               !> Thermal
               at%atom(n)%utype="u_ij"
               at%atom(n)%thtype="iso"

         case (12) ! Atomname Sfac X Y Z Occ U11 U22 U33 U23 U13 U12
               call get_num(label(2),vet,ivet,iv)   ! Is Sfac integer?
               if (iv /= 1) cycle
               call get_num(label(3),vet,ivet,iv)   ! Is X real?
               if (iv /= 1) cycle
               call get_num(label(4),vet,ivet,iv)   ! Is Y real?
               if (iv /= 1) cycle
               call get_num(label(5),vet,ivet,iv)   ! Is Z real?
               if (iv /= 1) cycle
               call get_num(label(6),vet,ivet,iv)   ! Is Occ real?
               if (iv /= 1) cycle
               call get_num(label(7),vet,ivet,iv)   ! Is U11 real?
               if (iv /= 1) cycle
               call get_num(label(8),vet,ivet,iv)   ! Is U22 real?
               if (iv /= 1) cycle
               call get_num(label(9),vet,ivet,iv)   ! Is U33 real?
               if (iv /= 1) cycle
               call get_num(label(10),vet,ivet,iv)   ! Is U23 real?
               if (iv /= 1) cycle
               call get_num(label(11),vet,ivet,iv)   ! Is U13 real?
               if (iv /= 1) cycle
               call get_num(label(12),vet,ivet,iv)   ! Is U12 real?
               if (iv /= 1) cycle

               !> Label
               n=n+1
               at%atom(n)%lab=label(1)(1:4)

               !> Chemical Symbol
               call get_num(label(2),vet,ivet,iv)
               el=elem_type(ivet(1))
               at%atom(n)%chemSymb=u_case(el(1:1))//l_case(el(2:2))

               !> Coordinates
               call get_num(label(3),vet,ivet,iv)
               at%atom(n)%x(1)=vet(1)
               call get_num(label(4),vet,ivet,iv)
               at%atom(n)%x(2)=vet(1)
               call get_num(label(5),vet,ivet,iv)
               at%atom(n)%x(3)=vet(1)

               !> Occupancy
               call get_num(label(6),vet,ivet,iv)
               at%atom(n)%occ=vet(1)

               !> U's (U11 U22 U33 U12 U13 U23 )
               call get_num(label(7),vet,ivet,iv)
               at%atom(n)%U(1)=vet(1)
               call get_num(label(8),vet,ivet,iv)
               at%atom(n)%U(2)=vet(1)
               call get_num(label(9),vet,ivet,iv)
               at%atom(n)%U(3)=vet(1)
               call get_num(label(12),vet,ivet,iv)
               at%atom(n)%U(4)=vet(1)
               call get_num(label(11),vet,ivet,iv)
               at%atom(n)%U(5)=vet(1)
               call get_num(label(10),vet,ivet,iv)
               at%atom(n)%U(6)=vet(1)

               !> Thermal
               at%atom(n)%utype="u_ij"
               at%atom(n)%thtype="ani"

            case default
               cycle
         end select
      end do
      if (n <= 0) return

      !> Adjusting ...
      call Allocate_Atom_List(n, Atmlist, 'Atm_std_type',0)
      AtmList%atom=At%atom(1:n)
      call Allocate_Atom_List(0, At,'Atm_std_type',0)

      !> Post calculations
      do i=1,n
         !> coordinates
         if (atmlist%atom(i)%x(1) >= 10.0) atmlist%atom(i)%x(1)=atmlist%atom(i)%x(1)-10.0
         if (atmlist%atom(i)%x(2) >= 10.0) atmlist%atom(i)%x(2)=atmlist%atom(i)%x(2)-10.0
         if (atmlist%atom(i)%x(3) >= 10.0) atmlist%atom(i)%x(3)=atmlist%atom(i)%x(3)-10.0

         !> Occ
         if (abs(atmlist%atom(i)%occ)  > 10.0) then
            x=atmlist%atom(i)%occ
            if (x > 10.0) then
               atmlist%atom(i)%occ=x-10.0
            else
               x=abs(atmlist%atom(i)%occ)
               do j=2,n_fvar
                  if (x > 10.0*real(j) .and. x < 10.0*real(j+1)) then
                     p=x-10.0*real(j)
                     if (atmlist%atom(i)%occ > 0.0) then
                        atmlist%atom(i)%occ=p*fvar(j)
                     else
                        atmlist%atom(i)%occ=p*(fvar(j)-1.0)
                     end if
                  end if
               end do
            end if
         end if

         !> Thermal factors
         if (atmlist%atom(i)%thtype == "ani") then
            atmlist%atom(i)%u_iso=U_Equiv(cell,atmlist%atom(i)%u(1:6))  ! Uequi
         else
            if (atmlist%atom(i)%u_iso < 0.0) then
               u=-atmlist%atom(i)%u_iso
               if (u <= 5.0 .and. u >= 0.5) then
                  do j=i-1,1,-1
                     if (u_case(atmlist%atom(j)%ChemSymb) == "H ") cycle
                     atmlist%atom(i)%u_iso=u*U_Equiv(cell,atmlist%atom(j)%u(1:6))  ! Uequi
                  end do
               end if
            end if
         end if

      end do

   End Subroutine Read_SHX_Atom

   !!----
   !!---- READ_SHX_CELL
   !!----    Obtaining Cell Parameter from Shelx format
   !!----
   !!---- 27/06/2019
   !!
   Module Subroutine Read_SHX_Cell(shx, Cell)
      !---- Arguments ----!
      type(File_Type),                 intent(in)     :: shx
      class(Cell_G_Type),allocatable,  intent(out)    :: Cell    ! Cell object

      !---- Local Variables ----!
      integer                      :: i,iv,z_shx
      real(kind=cp)                :: lambda_shx
      real(kind=cp), dimension(6)  :: vcell
      real(kind=cp), dimension(6)  :: std

      !> Init
      call clear_error()
      if (shx%nlines <=0) then
         err_CFML%Ierr=1
         err_CFML%Msg="Read_SHX_CELL: No lines in the file!"
         return
      end if

      !> Wave, CELL
      allocate(Cell_G_Type :: Cell)
      iv=0
      do i=1,shx%nlines
         line=adjustl(shx%line(i)%str)

         !> Check
         if (len_trim(line) == 0) cycle
         if (u_case(line(1:4)) /= 'CELL') cycle
         call Get_Num(line(5:),vet,ivet,iv)
         exit
      end do

      if (iv == 7) then
         lambda_shx = vet(1)
         vcell      = vet(2:7)
      else
         err_CFML%Ierr=1
         err_CFML%Msg="Read_SHX_Cell@CFML_IOForm: Problems reading cell parameters!"
         return
      end if

      !> Z, STD
      iv=0
      do i=1,shx%nlines
         line=adjustl(shx%line(i)%str)

         !> Check
         if (len_trim(line) == 0) cycle
         if (u_case(line(1:4)) /= 'ZERR') cycle
         call Get_Num(line(5:),vet,ivet,iv)
         exit
      end do
      if (iv == 7) then
         z_shx= ivet(1)
         std  = vet(2:7)
      else
         err_CFML%Ierr=1
         err_CFML%Msg="Read_SHX_Cell@CFML_IOForm: Problems reading sigma(cell) parameters!"
         return
      end if

      call Set_Crystal_Cell(vcell(1:3),vcell(4:6), Cell, Vscell=std(1:3), Vsang=std(4:6))

   End Subroutine Read_SHX_Cell

   !!----
   !!---- READ_SHX_FVAR
   !!----    Obtaining Fvar parameters from Shelx file (.ins or .res)
   !!----
   !!---- 27/06/2019
   !!
   Module Subroutine Read_SHX_Fvar(shx, n_fvar, fvar)
      !---- Arguments ----!
      type(File_Type),                intent(in)    :: shx
      integer,                        intent(out)   :: n_fvar
      real(kind=cp), dimension(:),    intent(out)   :: fvar

      !---- Local  variables ----!
      !character(len=80)            :: line
      integer                      :: i,iv

      !> Init
      call clear_error()
      if (shx%nlines <=0) then
         err_CFML%Ierr=1
         err_CFML%Msg="Read_SHX_FVAR: No lines in the file!"
         return
      end if

      n_fvar = 1
      fvar   = 1.0_cp

      iv=0
      do i=1,shx%nlines
         line=adjustl(shx%line(i)%str)

         !> Check
         if (len_trim(line) == 0) cycle
         if (u_case(line(1:4)) /= 'FVAR') cycle
         call Get_Num(line(5:),vet,ivet,iv)
         exit
      end do

      if (iv /= 0) then
         n_fvar=iv
         fvar=vet(1:iv)
      else
         err_CFML%Ierr=1
         err_CFML%Msg="Read_SHX_FVar@CFML_IOForm: Problems reading the FVAR parameters!"
      end if

   End Subroutine Read_SHX_Fvar

   !!----
   !!---- READ_SHX_WAVE
   !!----    Obtaining wavelength Parameter from Shelx format
   !!----
   !!---- 27/06/2019
   !!
   Module Subroutine Read_SHX_Wave(shx, Wave)
      !---- Arguments ----!
      type(File_Type), intent(in)     :: shx
      real(kind=cp),   intent(out)    :: Wave    ! Wavelength

      !---- Local Variables ----!
      integer                      :: i,iv

      !> Init
      Wave=0.0_cp
      call clear_error()

      if (shx%nlines <=0) then
         err_CFML%Ierr=1
         err_CFML%Msg="Read_SHX_Wave: No lines in the file!"
         return
      end if

      do i=1,shx%nlines
         line=adjustl(shx%line(i)%str)

         !> Check
         if (len_trim(line) == 0) cycle
         if (u_case(line(1:4)) /= 'CELL') cycle
         call get_num(line(5:),vet,ivet,iv)
         exit
      end do

      !> Wave, CELL
      if (iv == 7) then
         wave = vet(1)
      else
         err_CFML%Ierr=1
         err_CFML%Msg="Read_SHX_Wave@CFML_IOForm: Problems reading wavelength parameter!"
         return
      end if

   End Subroutine Read_SHX_Wave

   !!----
   !!---- READ_SHX_Z
   !!----    Obtaining Cell Parameter from Shelx format
   !!----
   !!---- 27/06/2019
   !!
   Module Subroutine Read_SHX_Z(shx, Z)
      !---- Arguments ----!
      type(File_Type),    intent(in)     :: shx
      real(kind=cp),      intent(out)    :: Z       ! Z number

      !---- Local Variables ----!
      integer                      :: i,iv

      !> Init
      Z=0.0_cp
      call clear_error()

      if (shx%nlines <=0) then
         err_CFML%Ierr=1
         err_CFML%Msg="Read_SHX_Z: No lines in the file!"
         return
      end if

      do i=1,shx%nlines
         line=adjustl(shx%line(i)%str)

         !> Check
         if (len_trim(line) == 0) cycle
         if (u_case(line(1:4)) /= 'ZERR') cycle
         call get_num(line(5:),vet,ivet,iv)
         exit
      end do

      !> Z
      if (iv == 7) then
         z= vet(1)
      else
         err_CFML%Ierr=1
         err_CFML%Msg="Read_SHX_Z@CFML_IOForm: Problems reading Z value!"
      end if

   End Subroutine Read_SHX_Z

   !!----
   !!---- READ_SHX_TITL
   !!----    Obtaining Title from Shelx file
   !!----
   !!---- 27/06/2019
   !!
   Module Subroutine Read_SHX_Titl(shx,Title)
      !---- Arguments ----!
      type(File_Type),    intent(in)     :: shx
      character(len=*),   intent(out)    :: title

      !---- Local vartiables ----!
      integer           :: i

      !> Init
      Title=" "

      call clear_error()
      if (shx%nlines <=0) then
         err_CFML%Ierr=1
         err_CFML%Msg="Read_SHX_Titl: No lines in the file!"
         return
      end if

      do i=1,shx%nlines
         line=adjustl(shx%line(i)%str)

         !> Check
         if (len_trim(line) == 0) cycle
         if (u_case(line(1:4)) /= 'TITL') cycle

         Title=adjustl(line(5:))
         exit
      end do

   End Subroutine Read_SHX_Titl

   !!----
   !!---- READ_SHX_LATT
   !!----    Obtaining lattice from Shelx file (.ins or .res)
   !!----
   !!----   Lattice
   !!----       1=P                     4=F              7=C
   !!----       2=I                     5=A
   !!----       3=rhombohedral obverse  6=B
   !!----
   !!---- 27/06/2019
   !!
   Module Subroutine Read_SHX_Latt(shx,latt)
      !---- Arguments ----!
      type(File_Type),   intent(in)    :: shx
      integer,           intent(out)   :: latt

      !---- Local Variables ----!
      integer                     :: i,iv


      !> Init
      latt=1

      call clear_error()
      if (shx%nlines <=0) then
         err_CFML%Ierr=1
         err_CFML%Msg="Read_SHX_Latt: No lines in the file!"
         return
      end if

      iv=0
      do i=1,shx%nlines
         line=adjustl(shx%line(i)%str)

         !> Check
         if (len_trim(line) == 0) cycle
         if (u_case(line(1:4)) /= 'LATT') cycle
         call Get_Num(line(5:),vet,ivet,iv)
         exit
      end do

      if (iv == 1) latt = ivet(1)

   End Subroutine Read_Shx_Latt

   !!----
   !!---- READ_SHX_CONT
   !!----    Obtaining Chemical contents from Shelx file (.ins or .res)
   !!----
   !!---- 27/06/2019
   !!
   Module Subroutine Read_SHX_Cont(shx, n_elem_type, elem_type, n_elem)
      !---- Arguments ----!
      type(File_Type),                           intent(in)     :: shx
      integer,                                  intent(out)     :: n_elem_type
      character(len=*), dimension(:),           intent(out)     :: elem_type
      real(kind=cp),    dimension(:), optional, intent(out)     :: n_elem

      !---- Local  variables ----!
      integer                       :: i,iv

      !> Init
      call clear_error()
      if (shx%nlines <=0) then
         err_CFML%Ierr=1
         err_CFML%Msg="Read_SHX_Cont: No lines in the file!"
         return
      end if

      n_elem_type = 0
      elem_type   = " "
      if (present(n_elem)) n_elem = 0.0_cp

      !> Define the elements types: C H N O
      do i=1,shx%nlines
         line=adjustl(shx%line(i)%str)

         !> Check
         if (len_trim(line) == 0) cycle
         if (u_case(line(1:4)) /= 'SFAC') cycle
         call get_words(line(5:),elem_type,n_elem_type)
         exit
      end do
      if (n_elem_type ==0) then
         err_CFML%Ierr=1
         err_CFML%Msg="Read_SHX_CONT@CFML_IOForm: Problems reading the SFAC information!"
         return
      end if

      if (present(n_elem)) then
         iv=0
         do i=1,shx%nlines
            line=adjustl(shx%line(i)%str)

            !> Check
            if (len_trim(line) == 0) cycle
            if (u_case(line(1:4)) /= 'UNIT') cycle
            call get_num(line(5:),vet,ivet,iv)
            exit
         end do
         if (iv > 0) then
            n_elem=vet
         else
            err_CFML%Ierr=1
            err_CFML%Msg="Read_SHX_CONT@CFML_IOForm: Problems reading the UNITS!"
            return
         end if
      end if

   End Subroutine Read_SHX_Cont

   !!----
   !!---- READ_SHX_SYMM
   !!----    Obtaining Symmetry Operators from Shelx file (.ins or .res)
   !!----
   !!---- 27/06/2019
   !!
   Module Subroutine Read_SHX_Symm(shx,n_oper,oper_symm)
      !---- Arguments ----!
      type(File_Type),  intent(in)               :: shx
      integer,          intent(out)              :: n_oper
      character(len=*), dimension(:),intent(out) :: oper_symm

      !---- Local variables ----!
      integer           :: i

      !> Init
      call clear_error()
      n_oper=0
      oper_symm=" "

      if (shx%nlines <=0) then
         err_CFML%Ierr=1
         err_CFML%Msg="Read_SHX_Symm: No lines in the file!"
         return
      end if

      !> SYMM
      do i=1,shx%nlines
         line=adjustl(shx%line(i)%str)

         !> Check
         if (len_trim(line) == 0) cycle
         if (u_case(line(1:4)) /= 'SYMM') cycle
         if (len_trim(line(5:)) > 0) then
            n_oper=n_oper+1
            oper_symm(n_oper)=trim(line(5:))
         end if
      end do

   End Subroutine Read_SHX_Symm

   !!----
   !!---- WRITE_SHX_TEMPLATE
   !!----    Write a Shelx File
   !!----
   !!---- 27/06/2019
   !!
   Module Subroutine Write_SHX_Template(filename,code,title,lambda,z,cell,spg,AtmList)
      !---- Arguments ----!
      character(len=*),        intent(in) :: filename
      integer,                 intent(in) :: code        ! 0: Patterson, 1: DM   2: Refinement
      character(len=*),        intent(in) :: title
      real(kind=cp),           intent(in) :: lambda
      integer,                 intent(in) :: z
      class(Cell_G_Type),      intent(in) :: cell
      class(SpG_Type),         intent(in) :: SpG
      type(atlist_type),       intent(in) :: atmList

      !---- Local Variables ----!
      logical                :: info

      integer                :: i,j,k,nc,iunit
      integer                :: nlat
      integer, dimension(15) :: z_cont

      !> Init
      info=.false.
      z_cont=0
      nc=0  !this depends on scattering factor?

      !> Ask if the file was oppened
      inquire(file=trim(filename),opened=info)
      if (info) then
         inquire(file=trim(filename),number=iunit)
         close(unit=iunit)
      end if

      !> Open
      open(newunit=iunit,file=trim(filename),status="unknown",action="write")
      rewind(unit=iunit)

      !> Title
      write(unit=iunit,fmt="(a)") "TITL "//trim(title)

      !> Lambda, Cell
      write(unit=iunit,fmt="(a,f8.5,3f9.4,3f7.3)") "CELL ",lambda,cell%cell,cell%ang

      !> Z, Std
      write(unit=iunit,fmt="(a,i3,a,3f8.4,3f7.3)") "ZERR ",z,"     ",cell%scell,cell%sang

      !> Latt
      nlat=1
      select case (spg%centred)
         case (0) ! Centric

         case (1) ! Acentric
            nlat=-1

         case (2) ! Not used in Shelx
            err_CFML%IErr=1
            err_CFML%Msg="Write_SHX_Template@CFML_IOFORM: Origin not at -1!"
            close(unit=iunit)
            return
      end select

      select case (spg%spg_lat)
         case ("I")
            nlat=2*nlat
         case ("R")
            nlat=3*nlat
         case ("F")
            nlat=4*nlat
         case ("A")
            nlat=5*nlat
         case ("B")
            nlat=6*nlat
         case ("C")
            nlat=7*nlat
      end select
      write(unit=iunit,fmt="(a,i2)") "LATT ",nlat

      !> Symm
      do i=2,spg%numops
         write(unit=iunit,fmt="(a)") "SYMM "//u_case(trim(spg%Symb_Op(i)))
      end do

      !> Sfac
      j=0
      do i=1,AtmList%natoms
         if (j == 0) then
            j=1
            z_cont(j)=atmlist%atom(i)%z
         else
            do k=1,j
               if (z_cont(k) == atmlist%atom(i)%z) exit
            end do
            if (z_cont(k) /= atmlist%atom(i)%z) then
               j=j+1
               z_cont(j)=atmList%atom(i)%z
            end if
         end if
      end do
      write(unit=iunit,fmt="(a)") "SFAC "

      !> Unit
      write(unit=iunit,fmt="(a)") "UNIT "

      select case (code)
         case (0) ! Shelxs - Patterson
            write(unit=iunit,fmt="(a)") "PATT "

         case (1) ! Shelxs - Direct Methods
            write(unit=iunit,fmt="(a)") "TREF "

         case (2) ! Shelxl - Refinement
            !> L.S.
            write(unit=iunit,fmt="(a)") "L.S. 10"

            !> Fvar
            write(unit=iunit,fmt="(a)") "FVAR 1.0"

            !> Weight
            write(unit=iunit,fmt="(a)") "WGHT 0.2"

            !> Fmap
            write(unit=iunit,fmt="(a)") "FMAP 2"

            !> Atoms
            do i=1,atmlist%natoms
               write(unit=iunit,fmt="(a4,i3,4f11.5)") &
                    atmlist%atom(i)%lab, nc, atmlist%atom(i)%x, atmlist%atom(i)%occ+10.0_cp
            end do
      end select

      !> Format
      write(unit=iunit,fmt="(a)") "HKLF 4"

      !> End
      write(unit=iunit,fmt="(a)") "END "

   End Subroutine Write_SHX_Template

   !!--++
   !!--++ READ_XTAL_SHX
   !!--++
   !!--++ Read and Set Crystal Information in a Shelx File
   !!--++
   !!--++ 09/05/2020
   !!
   Module Subroutine Read_XTal_SHX(shx, Cell, SpG, Atm, database_path)
      !---- Arguments ----!
      type(File_Type),                 intent(in)  :: shx
      class (Cell_G_Type),allocatable, intent(out) :: Cell
      class (SpG_Type),   allocatable, intent(out) :: SpG
      type (AtList_type),              intent(out) :: Atm
      character(len=*),   optional,    intent(in)  :: database_path

      !---- Local Variables ----!
      character(len=60), dimension(192) :: symm_car
      character(len=2),  dimension(15)  :: elem_atm
      integer                           :: i, nl, noper
      integer                           :: n_elem_atm, n_fvar
      real(kind=cp), dimension(10)      :: fvar

      !> Init
      call clear_error()

      if (shx%nlines <=0) then
         err_CFML%Ierr=1
         err_CFML%Msg="Read_XTal_SHX: No lines in the file!"
         return
      end if

      allocate(SpG_Type :: SpG)

      !> CELL / ZERR
      call Read_Shx_Cell(shx, Cell)
      if (err_CFML%Ierr == 1) return

      !> OBTAIN SPACE GROUP (LATT / SYMM) ----!
      call Read_Shx_Latt(shx,nl)
      if (err_CFML%Ierr == 1) return

      call Read_Shx_Symm(shx,noper,symm_car)
      if (err_CFML%Ierr == 1) return

      if (noper ==0) then
         select case (nl)
            case (1)
               if (present(database_path)) then
                  call set_spacegroup("p-1",SpG,database_path=database_path)
               else
                  call set_spacegroup("p-1",SpG)
               end if
            case (-1)
               if (present(database_path)) then
                  call set_spacegroup("p1",SpG,database_path=database_path)
               else
                  call set_spacegroup("p1",SpG)
               end if
         end select
      else

         if (nl > 0) then
            noper=noper+1
            symm_car(noper)="-X,-Y,-Z"
         end if

         select case (abs(nl))
            case (2) ! I
               noper=noper+1
               symm_car(noper)="X+1/2,Y+1/2,Z+1/2"
            case (3) ! Rom, Hex
               noper=noper+1
               symm_car(noper)="X+2/3,Y+1/3,Z+1/3"
               noper=noper+1
               symm_car(noper)="X+1/3,Y+2/3,Z+2/3"
            case (4) ! F
               noper=noper+1
               symm_car(noper)="X,Y+1/2,Z+1/2"
            case (5) ! A
               noper=noper+1
               symm_car(noper)="X,Y+1/2,Z+1/2"
               noper=noper+1
               symm_car(noper)="X+1/2,Y,Z+1/2"
               noper=noper+1
               symm_car(noper)="X+1/2,Y+1/2,Z"
            case (6) ! B
               noper=noper+1
               symm_car(noper)="X+1/2,Y,Z+1/2"
            case (7) ! C
               noper=noper+1
               symm_car(noper)="X+1/2,Y+1/2,Z"
         end select ! nl

         do i=1,noper
            symm_car(i)=l_case(symm_car(i))
         end do
         if (present(database_path)) then
            call set_spacegroup(" ",SpG, noper, symm_car,database_path=database_path)
         else
            call set_spacegroup(" ",SpG, noper, symm_car)
         end if
      end if
      if (err_CFML%Ierr == 1) return

      !> ATOMS
      call Read_Shx_Cont(shx,n_elem_atm,elem_atm)
      if (err_CFML%Ierr == 1) return
      call Read_Shx_Fvar(shx,n_fvar,fvar)
      if (err_CFML%Ierr == 1) return

      call Read_Shx_Atom(shx,n_fvar,fvar,elem_atm, cell, Atm)
      if (err_CFML%Ierr == 1) return

      !> Convert Us to Betas and Uiso to Biso
      do i=1,Atm%natoms
         vet(1:3)=Atm%atom(i)%x
         Atm%atom(i)%Mult=Get_Multip_Pos(vet(1:3),SpG)

         select case (Atm%atom(i)%thtype)
            case ("iso")
               Atm%atom(i)%u_iso= Atm%atom(i)%u_iso*78.95683521
               Atm%atom(i)%Utype="b_ij"

            case ("ani")
               Atm%atom(i)%u_iso=U_Equiv(cell,atm%atom(i)%u(1:6))  ! Uequi
               Atm%atom(i)%u_iso= Atm%atom(i)%u_iso*78.95683521
               select case (Atm%atom(i)%Utype)
                  case ("u_ij")
                     Atm%atom(i)%u(1:6) =  Get_Betas_from_U(Atm%atom(i)%u(1:6),Cell)

                  case ("b_ij")
                     Atm%atom(i)%u(1:6) = Get_Betas_from_B(Atm%atom(i)%u(1:6),Cell)
               end select
               Atm%atom(i)%Utype="beta"

            case default
               Atm%atom(i)%u_iso=0.05
               Atm%atom(i)%u_iso = Atm%atom(i)%u_iso*78.95683521
               Atm%atom(i)%thtype = "iso"
               Atm%atom(i)%Utype="b_ij"
         end select
      end do

   End Subroutine Read_XTal_SHX

End SubModule Format_SHX