!!
Submodule (CFML_KeyCodes) KeyCod_Restraints
   implicit none

   Contains

   !!----
   !!---- Subroutine Allocate_Restraints_List
   !!----
   !!----
   !!---- June 2023
   !!
   Module Subroutine Allocate_Restraints_List(R, NDim)
      !---- Arguments ----!
      Type(RestList_Type), intent(in out) :: R
      integer,             intent(in)     :: NDim   ! Dimension of the Vector RT

      !---- Local Variables ----!
      integer :: i

      !> Deallocating
      if (NDim <=0) then
         if (allocated(R%RT)) deallocate(R%RT)
         R%ND_Max=0
         R%NRel=0

         return
      end if

      !> Allocating
      allocate(R%RT(NDim))
      R%ND_Max=NDim
      R%NRel=0

      R%RT(:)%Obs=0.0_cp
      R%RT(:)%Cal=0.0_cp
      R%RT(:)%Sig=0.0_cp

      do i=1,4
         R%RT(:)%IPh(i)=0
         R%RT(:)%P(i)=0
      end do
      do i=1,3
         R%RT(:)%Code(i)=' '
      end do

   End Subroutine Allocate_Restraints_List

   !!----
   !!---- SUBROUTINE READ_RESTRAINTS_PHAS
   !!----
   !!----
   !!---- Update: June - 2023
   !!
   Module Subroutine Read_Restraints_PHAS(ffile, N_ini, N_end, IPhas, Atlist, RDis, RAng, RTor)
      !---- Arguments ----!
      Type(file_type),     intent( in)    :: ffile
      integer,             intent(in)     :: N_ini
      integer,             intent(in)     :: N_end
      integer,             intent(in)     :: IPhas
      type(AtList_Type),   intent(in)     :: AtList
      Type(RestList_Type), intent(in out) :: RDis
      Type(RestList_Type), intent(in out) :: RAng
      Type(RestList_Type), intent(in out) :: RTor

      !---- Local variables ----!
      integer :: i,k

      !> Init
      call clear_error()

      do i=n_ini, n_end
         !> load information on line variable
         line=adjustl(ffile%line(i)%str)
         if (len_trim(line) == 0) cycle
         if (line(1:1) =="!") cycle

         k=index(line,"!")
         if( k /= 0) line=line(:k-1)

         !> Restraints directives
         select case (u_case(line(1:4)))
            case ("DFIX")   ! DFIX
               call cut_string(line)
               call Get_DFIX_Line(line, IPhas, AtList, RDis)
               if (err_CFML%Flag) return

            case ("AFIX")    ! AFIX
               call cut_string(line)
               call Get_AFIX_Line(line, IPhas, AtList, RAng)
               if (err_CFML%Flag) return

            case ("TFIX")    ! TFIX
               call cut_string(line)
               call Get_TFIX_Line(line, IPhas, AtList, RTor)
               if (err_CFML%Flag) return
         end select
      end do

   End Subroutine Read_Restraints_PHAS

   !!----
   !!---- SUBROUTINE GET_DFIX_LINE
   !!----
   !!----    Get Distance Restraints relations for Free atoms type
   !!----    Line: Dist [sig] At1a At1b At2a At2b......
   !!----
   !!---- Update: April - 2022
   !!
   Module Subroutine Get_DFIX_Line(String, IPhase, AtList, R)
      !---- Arguments ----!
      character(len=*),    intent(in)     :: String
      integer,             intent(in)     :: IPhase
      type(AtList_Type),   intent(in)     :: AtList
      Type(RestList_Type), intent(in out) :: R

      !---- Local variables ----!
      integer, parameter              :: NP=20

      integer,dimension(2,NP)         :: p
      integer                         :: i,j,k,ini, nc,iv,nr,npos
      !integer                         :: iph
      real(kind=cp)                   :: dis,sig
      character(len=8), dimension(NP) :: symtrans=" "
      character(len=8)                :: symcode=" "

      !> Init
      call clear_error()
      if (len_trim(string) == 0  ) return

      !> Words
      call get_words(string, dire, nc)

      !> Value
      dis=0.0_cp
      call get_num(dire(1),vet,ivet,iv)
      if (iv /= 1) then
         call set_error(1," Error in DFIX line: "//trim(string))
         return
      end if
      dis=vet(1)

      !> Sigma
      sig=0.0_cp
      call get_num(dire(2),vet,ivet,iv)
      if (iv /= 1) then
         sig=0.02
         ini=2
      else
         sig=max(vet(1),0.0001_cp)
         ini=3
      end if

      !> Number of relations
      nr=0
      symcode=' '
      symtrans=' '
      do i=ini, nc, 2
         !> Atom name
         npos=index(dire(i),"_")
         if (npos /=0) then
            call set_error(1,"The first atom in DFIX command must belong to the asymmetric unit: "//trim(string))
            return
         end if

         npos=index(dire(i+1),"_")
         if (npos /=0) then
            symcode=dire(i+1)(npos+1:)
            dire(i+1)=dire(i+1)(:npos-1)
         end if

         ivet=0
         associate(A => AtList%Atom)
            do j=1,AtList%natoms
               if (trim(u_case(dire(i))) == trim(u_case(A(j)%Lab))) ivet(1)=j
               if (trim(u_case(dire(i+1))) == trim(u_case(A(j)%Lab))) ivet(2)=j
               if (all(ivet(1:2) > 0) ) exit
            end do
         end associate

         if (any(ivet(1:2) == 0)) then
            call set_error(1,"Some of the atom names in"//trim(string)//" were not found in the asymmetric unit")
            return
         end if

         !> New Relation
         nr=nr+1
         p(:,nr)=ivet(1:2)
         symtrans(nr)=trim(symcode)
      end do

      !> Check
      if (nr <= 0) return

      !> Adding relations

      k=R%NRel
      do i=1,nr
         k=k+1
         R%RT(k)%IPh=iphase
         R%RT(k)%Obs=dis
         R%RT(k)%Cal=0.0_cp
         R%RT(k)%sig=sig
         R%RT(k)%P(1:2)= p(1:2,i)
         R%RT(k)%Code(1)=symtrans(i)
      end do
      R%NRel=R%NRel+nr

   End Subroutine Get_DFIX_Line

   !!----
   !!---- SUBROUTINE GET_AFIX_LINE
   !!----
   !!----     Get Distance Restraints relations for Free atoms type
   !!----     Line: Angle [sig] At1a At1b At1c At2a At2b At2c....
   !!----
   !!---- Update: April - 2022
   !!
   Module Subroutine Get_AFIX_Line(String, IPhase, AtList, R)
      !---- Arguments ----!
      character(len=*),    intent(in)     :: String
      integer,             intent(in)     :: IPhase
      type(AtList_Type),   intent(in)     :: AtList
      Type(RestList_Type), intent(in out) :: R

      !---- Local variables ----!
      integer, parameter                :: NP=30

      integer, dimension(3,NP)          :: p=0
      integer                           :: i,j,k,ini,iv,nc,npos,nr
      character(len=8), dimension(2,NP) :: symtrans=" "
      character(len=8), dimension(2)    :: symcode=" "
      real(kind=cp)                     :: ang,sig

      !> Init
      call clear_error()
      if (len_trim(string) == 0) return

      !> words
      call get_words(string, dire, nc)

      !> Angle
      ang=0.0_cp
      call get_num(dire(1),vet,ivet,iv)
      if (iv /= 1) then
         call set_error(1,"Error in AFIX line: "//trim(string))
         return
      end if
      ang=vet(1)

      !> Sigma
      sig=0.0_cp
      call get_num(dire(2),vet,ivet,iv)
      if (iv /= 1) then
         sig=0.2
         ini=2
      else
         sig=max(vet(1),0.001_cp)
         ini=3
      end if

      nr=0
      symcode=' '
      symtrans=' '
      do i=ini, nc, 3
         npos=index(dire(i),"_")
         if (npos /=0) then
            call set_error(1,"The first atom in AFIX command must belong to the asymmetric unit: "//trim(string))
            return
         end if

         npos=index(dire(i+1),"_")
         if (npos /=0) then
            symcode(1)=dire(i+1)(npos+1:)
            dire(i+1)=dire(i+1)(:npos-1)
         end if

         npos=index(dire(i+2),"_")
         if (npos /=0) then
            symcode(2)=dire(i+2)(npos+1:)
            dire(i+2)=dire(i+2)(:npos-1)
         end if

         ivet=0
         associate(A => AtList%Atom)
            do j=1,AtList%natoms
               if (trim(u_case(dire(i)))   == trim(u_case(A(j)%Lab))) ivet(1)=j
               if (trim(u_case(dire(i+1))) == trim(u_case(A(j)%Lab))) ivet(2)=j
               if (trim(u_case(dire(i+2))) == trim(u_case(A(j)%Lab))) ivet(3)=j
               if (all(ivet(1:3) > 0) ) exit
            end do
         end associate

         if (any(ivet(1:3) == 0)) then
            call set_error(1,"Some of the atom names in "//trim(string)//" were not found in the asymmetric unit")
            return
         end if

         !> New Relation
         nr=nr+1
         p(:,nr)=ivet(1:3)
         symtrans(:,nr)=symcode
      end do

      !> Check
      if (nr <= 0) return

      !> Adding relations
      k=R%NRel
      do i=1,nr
         k=k+1
         R%RT(k)%Iph=iphase
         R%RT(k)%Obs=ang
         R%RT(k)%Cal=0.0_cp
         R%RT(k)%Sig=sig
         R%RT(k)%p(1:3) = p(1:3,i)
         R%RT(k)%Code(1:2)=symtrans(1:2,i)
      end do
      R%NRel=R%NRel + nr

   End Subroutine Get_AFIX_Line

   !!----
   !!---- SUBROUTINE GET_TFIX_LINE
   !!----
   !!----    Get Torsion Restraints relations for Free atoms type
   !!----    Line: Torsion_Angle [sig] At1a At1b At1c At1d ...
   !!----
   !!---- Update: April - 2020
   !!
   Module Subroutine Get_TFIX_Line(String, IPhase, AtList, R)
      !---- Arguments ----!
      character(len=*),    intent(in)     :: String
      integer,             intent(in)     :: IPhase
      type(AtList_Type),   intent(in)     :: AtList
      Type(RestList_Type), intent(in out) :: R

      !---- Local variables ----!
      integer, parameter                :: NP=30

      integer, dimension(4,NP)          :: p=0
      integer                           :: i,j,k,ini, iv,nc,npos,nr
      character(len=8), dimension(3,NP) :: symtrans=" "
      character(len=8), dimension(3)    :: symcode=" "
      real(kind=cp)                     :: tor,sig

      !> Init
      call clear_error()

      if (len_trim(string) == 0 ) return

      !> Words
      call get_words(string,dire,nc)

      !> Torsion
      tor=0.0_cp
      call get_num(dire(1),vet,ivet,iv)
      if (iv /= 1) then
         call set_error(1,"Error in TFIX line: "//trim(string))
         return
      end if
      tor=vet(1)

      !> Sigma
      sig=0.0
      call get_num(dire(2),vet,ivet,iv)
      if (iv /= 1) then
         sig=0.2
         ini=2
      else
         sig=max(vet(1),0.02_cp)
         ini=3
      end if

      nr=0
      symcode=' '
      symtrans=' '
      do i=ini, nc,4
         npos=index(dire(i),"_")
         if (npos /=0) then
            call set_error(1,"The first atom in TFIX command must belong to the asymmetric unit: "//trim(string))
            return
         end if

         npos=index(dire(i+1),"_")
         if (npos /=0) then
            symcode(1)=dire(i+1)(npos+1:)
            dire(i+1)=dire(i+1)(:npos-1)
         end if

         npos=index(dire(i+2),"_")
         if (npos /=0) then
            symcode(2)=dire(i+2)(npos+1:)
            dire(i+2)=dire(i+2)(:npos-1)
         end if

         npos=index(dire(i+3),"_")
         if (npos /=0) then
            symcode(3)=dire(i+3)(npos+1:)
            dire(i+3)=dire(i+3)(:npos-1)
         end if

         ivet=0
         associate (A => AtList%Atom)
            do j=1,AtList%natoms
               if (trim(u_case(dire(i)))   == trim(u_case(A(j)%Lab))) ivet(1)=j
               if (trim(u_case(dire(i+1))) == trim(u_case(A(j)%Lab))) ivet(2)=j
               if (trim(u_case(dire(i+2))) == trim(u_case(A(j)%Lab))) ivet(3)=j
               if (trim(u_case(dire(i+3))) == trim(u_case(A(j)%Lab))) ivet(4)=j
               if (all(ivet(1:4) > 0) ) exit
            end do
         end associate
         if (any(ivet(1:4) == 0)) then
            call set_error(1,"Some of the atom names in "//trim(string)//" were not found in the asymmetric unit")
            return
         end if

         !> New Relation
         nr=nr+1
         p(:,nr)=ivet(1:4)
         symtrans(:,nr)=symcode
      end do

      !> Check
      if (nr <= 0) return

      !> Adding relations
      k=R%NRel
      do i=1,nr
         k=k+1
         R%RT(k)%IPh=iphase
         R%RT(k)%Obs=tor
         R%RT(k)%Cal=0.0_cp
         R%RT(k)%Sig=sig
         R%RT(k)%p = p(1:4,i)
         R%RT(k)%Code=symtrans(1:3,i)
      end do
      R%NRel =R%NRel + nr

   End Subroutine Get_TFIX_Line

End SubModule KeyCod_Restraints