  Submodule (CFML_Geom) Geom_Orbits
    implicit none
    contains

    !!----
    !!---- Module Subroutine Get_Transf_List(Trans,Ox,Pl,Npl)
    !!----   real(kind=cp), dimension(3,3), intent(in)     :: trans   !Matrix transforming the basis
    !!----   real(kind=cp), dimension(3  ), intent(in)     :: ox      !Coordinates of origin of the new basis
    !!----   type(point_list_type),         intent(in)     :: pl      !Input List of points
    !!----   type(point_list_type),         intent(in out) :: npl     !Output list of transformed points
    !!----
    !!----  Subroutine to get the fractional coordinates of the points of the input list "pl" in the
    !!----  new transformed cell ( a'= trans a) displaced to the new origing "ox". The coordinates
    !!----  are generated using only lattice translations. All coordinates are reduced to be
    !!----  between 0.0 and 1.0, so that  0.0 <= x,y,z < 1.0
    !!----
    !!---- Update: February - 2005
    !!
    Module Subroutine Get_Transf_List(trans,ox,pl,npl)
       !---- Arguments ----!
       real(kind=cp),         dimension(3,3), intent(in)     :: trans
       real(kind=cp),         dimension(3  ), intent(in)     :: ox
       type(point_list_type),                 intent(in)     :: pl
       type(point_list_type),                 intent(in out) :: npl

       !---- local variables ----!
       integer                       :: i,j,ia,ib,ic,nat,mm
       integer, dimension(3)         :: mini,maxi
       real(kind=cp), dimension(7,3) :: vecpar
       real(kind=cp), dimension(3,3) :: si
       real(kind=cp), dimension(3  ) :: xx, xxn,v

       si=inverse_matrix(trans)
       if (Err_CFML%Ierr /= 0) return

       !----  Construction of the 7 vertices of the new cell
       !----  1:a, 2:b, 3:c, 4:a+b, 5:a+c, 6:b+c 7:a+b+c
       do j=1,3
          do i=1,3
             vecpar(i,j)=trans(i,j)
          end do
          vecpar(4,j)=trans(1,j)+trans(2,j)
          vecpar(5,j)=trans(1,j)+trans(3,j)
          vecpar(6,j)=trans(2,j)+trans(3,j)
          vecpar(7,j)=trans(1,j)+trans(2,j)+trans(3,j)
       end do

       !---- Exploration of the vertex matrix
       mini(:)=1000
       maxi(:)=-1000
       do j=1,3
          do i=1,7
             if (vecpar(i,j) < mini(j)) mini(j)=nint(min(vecpar(i,j),0.0_cp))
             if (vecpar(i,j) > maxi(j)) maxi(j)=nint(max(vecpar(i,j),1.0_cp))
          end do
       end do

       !
       !   Explore the region  a-> min(1)---max(1)  where atoms will be generated
       !                       b-> min(2)---max(2)
       !                       c-> min(3)---max(3)
       !   and select those belonging to the interior of the new cell before
       !   translation to the new origin.
       !   set the translation to the new origin, put the atoms inside the new
       !   unit cell and, finally, print atoms coordinates
       !
       nat=0
       do mm=1,pl%np
          do ia=mini(1),maxi(1)
             xx(1)=pl%x(1,mm)+real(ia)
             do ib=mini(2),maxi(2)
                xx(2)=pl%x(2,mm)+real(ib)
                do_ic: do ic=mini(3),maxi(3)
                   xx(3)=pl%x(3,mm)+real(ic)
                   xxn=matmul(xx-ox,si)
                   xxn=Modulo_Lat(xxn)
                   do i=nat,1,-1
                      v=npl%x(:,i)-xxn(:)
                      if (Is_Lattice_Vec(v,Prim=.true.)) cycle do_ic
                   end do
                   nat=nat+1
                   npl%x(:,nat)= xxn
                   write(unit=npl%nam(nat),fmt="(a,i5)") trim(pl%nam(mm))//"_",nat
                   npl%nam(nat)=pack_string(npl%nam(nat))
                end do do_ic
             end do
          end do
       end do
       npl%np=nat

    End Subroutine Get_Transf_List

    !!---- Subroutine Set_New_AsymUnit(SpGn,Ate,Mat,orig,A_n,matkind,debug)
    !!----    Class(SpG_Type) ,              intent(in ) :: SpGn    !New space group that has been previously set
    !!----    type (Atom_Equiv_List_Type),   intent(in ) :: Ate     !In old group
    !!----    real(kind=cp), dimension (3,3),intent(in ) :: Mat     !Transformation matrix from the old to the new setting
    !!----    real(kind=cp), dimension (  3),intent(in ) :: orig    !Displacement of the origin in the old setting
    !!----    type (AtList_Type),            intent(out) :: A_n     !New atom list
    !!----    character (len=*), optional,   intent(in ) :: matkind !Kind of transformation matrix
    !!----    character (len=*), optional,   intent(in ) :: debug
    !!----
    !!----    Updated: January 2014 (JRC)
    !!----
    !!----
    Module Subroutine Set_New_AsymUnit(SpGn,Ate,Mat,orig,A_n,matkind,debug)
       Class (SpG_Type) ,             intent(in ) :: SpGn
       type (Atom_Equiv_List_Type),   intent(in ) :: Ate !In old group
       real(kind=cp), dimension (3,3),intent(in ) :: Mat
       real(kind=cp), dimension (  3),intent(in ) :: orig
       type (AtList_Type),            intent(out) :: A_n
       character (len=*), optional,   intent(in ) :: matkind
       character (len=*), optional,   intent(in ) :: debug

       ! Local variables
       integer                                    :: i,j,k,m,ifail,L,n,Ls,ip,L1
       integer                                    :: i1,i2,i3,maxa,maxp,maxm !,mult
       real(kind=cp), dimension(3,3)              :: S,Sinv
       real(kind=cp)                              :: determ
       logical                                    :: newp,fail
       real(kind=cp), dimension(  3)              :: pos
       type(point_list_type)                      :: pl
       type (AtList_Type)                         :: A
       type (Point_Orbit)                         :: orbit
       !real(kind=cp), dimension(:,:),allocatable :: orb
       character(len=*),parameter,dimension(26) :: let=["a","b","c","d","e","f","g","h", &
          "i","j","k","l","m","n","o","p","q","r","s","t","u","v","w","x","y","z"]
       real(kind=cp), allocatable, dimension (:,:) :: vec
       integer,parameter         :: lu=93
       real(kind=cp), parameter  :: epsi = 0.002

       if(present(matkind)) then
        if(matkind(1:2) == "it" .or. matkind(1:2) == "IT" ) then
          S=Mat             !Atoms positions X'=inv(Mat) (X-O)
        else
          S=transpose(Mat)  !Atoms positions X'=inv(MatT)(X-O)
        end if
       else
          S=transpose(Mat)
       end if
       Sinv=inverse_matrix(S) !Atoms positions X'= Sinv X
       if (Err_CFML%Ierr /= 0) then
         Err_CFML%Msg= "Inversion Matrix Failed on: Change_Setting_SG"
         return
       end if
       if(present(debug)) then
         open(unit=lu,file="similar_debug.lis",status="replace",action="write")
         write(unit=lu,fmt="(a)")  "  Debugging SIMILAR calculations  "
         write(unit=lu,fmt="(a/)") "  ============================== "
       end if

       determ=Determ3D(S)
       determ=abs(determ)

       m=0
       if(determ > 1.0001) then  !Generate indices for lattice translations to be applied
         i1=max(nint(maxval(abs(S(:,1))))-1,1)
         i2=max(nint(maxval(abs(S(:,2))))-1,1)
         i3=max(nint(maxval(abs(S(:,3))))-1,1)
         allocate(vec(3,(i1+1)*(i2+1)*(i3+1)))
         do i=0,i1
          do j=0,i2
           do k=0,i3
            if(i==0 .and.j==0 .and. k==0) cycle
            m=m+1
            vec(:,m) = real([i,j,k])
            !write(*,*) "  vect: ",m," :",vec(:,m)
           end do
          end do
         end do
       end if

       maxm=m    !maximum number of translations to be applied before changing the basis
       maxa=maxval(Ate%Atm(:)%mult)  !highest multiplicity of the atom sequence list
       !Factor 2
       maxp=2*maxa*determ    !maximum multiplicity in the new cell of a particular atom type
       !write(*,*) " Allocating atoms_list and Point list for ", maxp*Ate%nauas, " and ", maxp, " values"
       call Allocate_Atom_List(maxp*Ate%nauas, A,"atm_std_type", 0) !Atom list in the new cell, we must use "maxp"*Ate%nauas and not "maxa"
       Call Allocate_Point_List(maxp*Ate%nauas,Pl,Ifail)
       if(ifail /= 0) then
         !write(*,*) " Error allocating PL for ",maxp," values"
          Err_CFML%Ierr=1
          write(unit=Err_CFML%Msg,fmt="(a,i8,a)")  " Error allocating PL for ",maxp," values"
          return
       end if
       Ls=0

       !write(*,*) " Allocating PL and A successful "

       do i=1,Ate%nauas
         Ls=Ls+1
         !Setting pl object
           ip=index(Ate%Atm(i)%Lab(1),"_")
           if(ip /= 0) then
              pl%nam(i)=Ate%Atm(i)%Lab(1)(1:ip-1)
           else
              pl%nam(i)=Ate%Atm(i)%Lab(1)
           end if
           pl%x=0.0
           pl%p=0.0
         !
         n=0
         do j=1,Ate%Atm(i)%mult
           n=n+1
           pos(:)    = Ate%Atm(i)%x(:,j)-orig(:)
           !write(*,*) " n=",n
           pl%x(:,n) = matmul(Sinv,pos)  !Complete list in new coordinate system of atoms of type i
           if(present(debug)) then
             write(unit=lu,fmt="(i4,2(a,3f8.4),a)") n,"  Atom: "//pl%nam(i)//" at (", &
                                         Ate%Atm(i)%x(:,j),") trasform to (",pl%x(:,n),")"
           end if
           pl%x(:,n) = Modulo_Lat(pl%x(:,n))  !Complete list in new coordinate system of atoms of type i
         end do
         if(determ > 1.0) then
          doj:do j=1,Ate%Atm(i)%mult
               do m=1,maxm
                 pos(:) = Ate%Atm(i)%x(:,j)-orig(:)+ vec(:,m)
                 pos(:) = Modulo_Lat(matmul(Sinv,pos))
                 newp=.true.
                 do k=1,n
                    if (sum(abs(pos(:)-pl%x(:,k))) < epsi) then
                       newp=.false.
                       exit
                    end if
                 end do
                 if (newp) then ! new position
                    n=n+1
                    !write(*,*) "  n=",n
                    pl%x(:,n) = pos(:)
                    if(present(debug)) then
                      write(unit=lu,fmt="(i4,2(a,3f8.4),a)") n,"  Atom: "//pl%nam(i)//" at (", &
                                                  Ate%Atm(i)%x(:,j),") trasform to (",pl%x(:,n),")"
                    end if
                    if(n == maxp) exit doj
                 end if
               end do
             end do doj
         end if

         pl%np=n
         A%atom(Ls)%Lab =pl%nam(i)
         A%atom(Ls)%x(:)=pl%x(:,1)
         !write(*,"(2i5,a,i5,a)") i,Ls, "  "//Ate%Atm(i)%Lab(1), Ate%Atm(i)%mult,"   "//A%atom(Ls)%Lab

         !Determine the number of independent orbits for this point
         call Set_Orbits_Inlist(Spgn,pl)
         L=1; L1=1
         do j=2,n
           if(pl%p(j) > L) then
            Ls=Ls+1
            A%atom(Ls)%x(:)=pl%x(:,j)
            !write(unit=let,fmt="(i3.3)") L
            A%atom(Ls)%Lab =trim(pl%nam(i))//let(L1)
           ! write(*,"(2i5,a,i5,a)") i,Ls, "  "//Ate%Atm(i)%Lab(1), Ate%Atm(i)%mult,"   "//A%atom(Ls)%Lab
            L=L+1
            L1=L1+1  !using a different counter for the label
            if(L1 > 26) L1=1 !re-start the labelling with the same letter
           end if
         end do

       end do  !i=1,Ate%nauas

       !write(*,*) "  Orbits correct"
       !write(*,*) "  Allocate_Atom_List for ",Ls," atoms"
       call Allocate_Atom_List(Ls, A_n,"atm_std_type",0)
       fail=Err_CFML%Ierr == 1
       if(fail) then
         if(present(debug)) then
          !write(*,*) "  Error on Allocate_Atom_List for ",A_n%natoms," atoms"
          write(unit=lu,fmt="(a,i4,a)") "  Error on Allocate_Atom_List for ",A_n%natoms," atoms"
         end if
       else
         ! write(*,*) "  Success on Allocate_Atom_List for ",A_n%natoms," atoms"
       end if
       do i=1,A_n%natoms
         A_n%atom(i)%x= A%atom(i)%x
         A_n%atom(i)%Lab= A%atom(i)%Lab
         !call Get_Orbit(A_n%atom(i)%x,Spgn,Mult,orb)
         call Get_Orbit(A_n%atom(i)%x,Spgn,orbit)
         A_n%atom(i)%Mult=orbit%mult
         A_n%atom(i)%occ=real(orbit%mult)/real(Spgn%Multip)
       end do
       if(allocated(A%atom)) deallocate(A%atom)
       if(present(debug)) close(unit=lu)
    End Subroutine Set_New_AsymUnit


    !!----
    !!---- Subroutine Set_Orbits_Inlist(Spg,Pl)
    !!----    Class(SpG_Type),  intent(in)     :: SpG     !  In -> Space group
    !!----    type(point_list_type),  intent(in out) :: pl      !  In -> list of points
    !!----
    !!----    Set up of the integer pointer "pl%p" in the object "pl" of type point_list_type.
    !!----    Each point is associated with the number of an orbit. This pointer is useful
    !!----    to get the asymmetric unit with respect to the input space group of an arbitrary
    !!----    list of points (atom coordinates).
    !!----
    !!---- Update: February - 2005
    !!
    Module Subroutine Set_Orbits_Inlist(Spg,Pl)
       !---- Arguments ----!
       class(SpG_Type),       intent(in)     :: SpG
       type(point_list_type), intent(in out) :: pl

       !--- Local variables ---!
       integer                                  :: i,j,norb,nt,d
       real(kind=cp), dimension(3)              :: x,xx,v

       real(kind=cp), dimension(3,SpG%Multip)   :: tr
       integer,       dimension(3,3,SpG%Multip) :: Mat
       d=SpG%d
       do i=1,SpG%Multip
         Mat(:,:,i)= SpG%Op(i)%Mat(1:3,1:3)
          tr(:,i)  = SpG%Op(i)%Mat(1:3,d)
       End do

       norb=0
       pl%p=0
       do i=1,pl%np
          if (pl%p(i) == 0) then
             norb=norb+1
             pl%p(i)=norb
             x=pl%x(:,i)
             do j=1,Spg%multip
                !xx=Apply_OP(Spg%Op(j),x)
                xx=Matmul(Mat(:,:,j),x)+tr(:,j)
                xx=modulo_lat(xx)
                do nt=1,pl%np
                   if (pl%p(nt) /= 0) cycle
                   v=pl%x(:,nt)-xx(:)
                   if (Is_Lattice_Vec(v,SpG)) pl%p(nt)=norb
                end do
             end do
          end if
       end do

    End Subroutine Set_Orbits_Inlist

  End Submodule Geom_Orbits
