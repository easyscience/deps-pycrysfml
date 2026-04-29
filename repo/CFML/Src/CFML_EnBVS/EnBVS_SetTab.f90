 Submodule (CFML_EnBVS) EnBVS_SetTab
   implicit none
   contains

    !!----
    !!---- Module Function get_soft_covalent_radius(nam) result(radius)
    !!----    character(len=*), intent(in) :: nam    ! In -> Name of the species containing the charge
    !!----    real(kind=cp)                :: radius ! Out-> Covalent radius according to Ap_Table
    !!----
    !!----    Getting the covalent radius according to Ap_Table
    !!----
    !!---- Created: January - 2015, June 2020
    !!
    Module Function get_soft_covalent_radius(nam) result(radius)
      character(len=*), intent(in) :: nam
      real(kind=cp)                :: radius
      !--- Local variables ---!
      integer :: i
      radius=0.0
      do i=1,Ap_species_n
        if(nam == Ap_Table(i)%Symb) then
          radius=Ap_table(i)%Rc
          exit
        end if
      end do
      if(radius < 0.01) then
        radius= Get_Covalent_Radius(nam)
      end if
    End Function get_soft_covalent_radius

    !!----
    !!---- Module Subroutine Allocate_Atoms_Conf_List(N,A)
    !!----    integer, intent(in)                         :: n    !  In -> Atoms in asymmetric unit
    !!----    type (Atoms_Conf_List_Type), intent(in out) :: A    !  In -> Objet to be allocated
    !!----
    !!----    Allocation of objet A of type atom_list_Conf. This subroutine
    !!----    should be called before using an object of type atom_list_Conf.
    !!----
    !!---- Update: March - 2005
    !!
    Module Subroutine Allocate_Atoms_Conf_List(n,A)
       !---- Arguments ----!
       integer,                     intent(in)       :: N  !N. atoms in asymmetric unit
       type (Atoms_Conf_List_Type), intent(in out)   :: A  !Objet to be allocated

       !---- Local variables ----!
       integer :: i

       A%natoms   = n
       A%n_spec   = 0
       A%n_anions = 0
       A%n_cations= 0
       A%tol      = 20.0 ! 20% tolerance by default
       A%totatoms = 0.0
       if(.not. allocated(A%atom)) allocate (A%atom(n))
       do i=1,n
          call init_atom_type(A%atom(i),0)
       end do

    End Subroutine Allocate_Atoms_Conf_List

    !!----
    !!---- Module Subroutine Deallocate_Atoms_Conf_List(A)
    !!----    type (Atoms_Conf_List_Type), intent(in out)   :: A  ! In/ Out -> Objet to be deallocated
    !!----
    !!----    De-allocation of objet A of type Atoms_Conf_List. This subroutine
    !!----    should be after using an object of type Atoms_Conf_List that is no
    !!----    more needed.
    !!----
    !!---- Update: March - 2005
    !!
    Module Subroutine Deallocate_Atoms_Conf_List(A)
       !---- Arguments ----!
       type (Atoms_Conf_List_Type), intent(in out)   :: A  !Objet to be deallocated

       if (allocated(A%atom)) deallocate (A%atom)
       A%natoms   = 0
       A%n_spec   = 0
       A%n_anions = 0
       A%n_cations= 0
    End Subroutine Deallocate_Atoms_Conf_List

    !!----
    !!---- Module Subroutine Complete_Table(A,N_bvsm,bvs_m)
    !!----    type (Atoms_Conf_List_type),   intent(in) :: A      !  In -> Object of Atoms_Conf_List_type
    !!----    Integer,                       intent(in) :: N_bvsm
    !!----    Character(len=*),dimension(:), intent(in) :: bvs_m
    !!----
    !!----    Sets up the table of BV parameters (add provided external values)
    !!----    completing the table when the user gives his/her own BV parameters
    !!----
    !!---- Update: January - 2008
    !!
    Module Subroutine Complete_Table(A,N_bvsm,bvs_m)
       type (Atoms_Conf_List_type),   intent(in) :: A      !  In -> Object of Atoms_Conf_List_type
       Integer,                       intent(in) :: N_bvsm
       Character(len=*),dimension(:), intent(in) :: bvs_m
       !---- Local variables ----!
       Integer       :: i,j,k,ic,icat,ian
       real(kind=cp) :: d0_n, b_n
       character(len=10), dimension(5)        :: dire

          do k=1,N_bvsm
                 dire=" "
                 call Get_words(bvs_m(k),dire,ic)
                 if (ic <= 0) cycle
                 icat=0
                 ian=0
                 if (ic < 3 ) then
                          Err_CFML%Ierr=1
                          Err_CFML%Msg="Cation-Anion D0,b parameters must be provided"
                          return
                 end if

                 do i=1,A%N_Spec
                          if (u_case(dire(1)(1:4)) /= A%Species(i)) cycle
                          icat=i
                 end do
                 do i=1,A%N_Spec
                          if (u_case(dire(2)(1:4)) /= A%Species(i)) cycle
                          ian=i
                 end do
                 if (icat == 0 .or. ian == 0) then
                    Err_CFML%Ierr=1
                    Err_CFML%Msg="The given Cation or Anion cannot be found in the atom list"
                    return
                 end if
                 if (icat > ian) then
                          j=icat
                          icat=ian
                          ian=j
                 end if
                 if (icat > A%N_Cations) then
                    Err_CFML%Ierr=1
                    Err_CFML%Msg="One of the given cations is not found in the atom list"
                    return
                 end if

                 read(unit=dire(3),fmt=*) d0_n
                 if (ic > 3) read(unit=dire(4),fmt=*) b_n

                 Table_d0(icat,ian)=d0_n
                 Table_b(icat,ian)=b_n
                 Table_ref(icat,ian)=0

                 Table_d0(ian,icat)=d0_n
                 Table_b(ian,icat)=b_n
                 Table_ref(ian,icat)=0

          end do

    End Subroutine Complete_Table

    !!----
    !!---- Module Subroutine Complete_Table_BVEL(A,N_bvel,bvel)
    !!----    type (Atoms_Conf_List_type),   intent(in) :: A      !  In -> Object of Atoms_Conf_List_type
    !!----    Integer,                       intent(in) :: N_bvel
    !!----    Character(len=*),dimension(:), intent(in) :: bvel
    !!----
    !!----    Sets up the table of BVEL parameters (add provided external values)
    !!----    Completing the table when the user gives his/her own BVEL parameters
    !!----
    !!---- Created: December - 2014, modified November-2018
    !!
    Module Subroutine Complete_Table_BVEL(A,N_bvel,bvel)
       type (Atoms_Conf_List_type),   intent(in) :: A      !  In -> Object of Atoms_Conf_List_type
       Integer,                       intent(in) :: N_bvel
       Character(len=*),dimension(:), intent(in) :: bvel
       !---- Local variables ----!
       Integer       :: i,j,k,ic,icat,ian
       real(kind=cp) :: Avcoor,Rzero,Rcutoff,Dzero,Rmin,alpha
       character(len=12), dimension(9)  :: dire

          do k=1,N_bvel
                 dire=" "
                 call Get_words(bvel(k),dire,ic)
                 if (ic <= 0) cycle
                 icat=0
                 ian=0
                 if (ic < 8 ) then
                          Err_CFML%Ierr=1
                          Err_CFML%Msg="Cation-Anion {Nc,R0,Cutoff,D0,Rmin,alpha} parameters must be provided"
                          return
                 end if

                 do i=1,A%N_Spec
                          if (u_case(dire(1)(1:4)) /= A%Species(i)) cycle
                          icat=i
                 end do
                 do i=1,A%N_Spec
                          if (u_case(dire(2)(1:4)) /= A%Species(i)) cycle
                          ian=i
                 end do
                 if (icat == 0 .or. ian == 0) then
                    Err_CFML%Ierr=1
                    Err_CFML%Msg="The given Cation or Anion cannot be found in the atom list"
                    return
                 end if
                 if (icat > ian) then
                          j=icat
                          icat=ian
                          ian=j
                 end if
                 if (icat > A%N_Cations) then
                    Err_CFML%Ierr=1
                    Err_CFML%Msg="One of the given cations is not found in the atom list"
                    return
                 end if
                 !write(*,*) " Assigning -> "//trim(bvel(k))
                 read(unit=dire(3),fmt=*) Avcoor
                 read(unit=dire(4),fmt=*) Rzero
                 read(unit=dire(5),fmt=*) Rcutoff
                 read(unit=dire(6),fmt=*) Dzero
                 read(unit=dire(7),fmt=*) Rmin
                 read(unit=dire(8),fmt=*) alpha

                 Table_Avcoor  (icat,ian)=Avcoor
                 Table_Rzero   (icat,ian)=Rzero
                 Table_Rcutoff (icat,ian)=Rcutoff
                 Table_Dzero   (icat,ian)=Dzero
                 Table_Rmin    (icat,ian)=Rmin
                 Table_alpha   (icat,ian)=alpha
                 Table_ref     (icat,ian)=0

                 Table_Avcoor  (ian,icat)=Avcoor
                 Table_Rzero   (ian,icat)=Rzero
                 Table_Rcutoff (ian,icat)=Rcutoff
                 Table_Dzero   (ian,icat)=Dzero
                 Table_Rmin    (ian,icat)=Rmin
                 Table_alpha   (ian,icat)=alpha
                 Table_ref     (ian,icat)=0

          end do

    End Subroutine Complete_Table_BVEL

    !!----
    !!---- Module Subroutine Set_Table_BVEL_Params(A,N_bvel,bvel)
    !!---- type (Atoms_Conf_List_type),            intent(in)  :: A
    !!---- integer,                      optional, intent(in)  :: N_bvel   !Number of bvel strings with externally provided values
    !!---- character(len=*),dimension(:),optional, intent(in)  :: bvel     !bvs strings with externally provided values
    !!---- logical,                      optional, intent(in)  :: soft     !Calculate D0 and Rmin
    !!----
    !!----
    !!---- Created: December - 2014
    !!
    Module Subroutine Set_Table_BVEL_Params(A,N_bvel,bvel,soft,nread)
       type (Atoms_Conf_List_type),            intent(in)  :: A
       integer,                      optional, intent(in)  :: N_bvel
       character(len=*),dimension(:),optional, intent(in)  :: bvel
       logical,                      optional, intent(in)  :: soft
       integer,                      optional, intent(in)  :: nread

       !---- Local Variables ----!
       integer :: i,j,k,L,ia,ic,ac,an,i1,i2
       character(len=80) :: aux
       real(kind=cp) :: rmin,d0,cn,b0,r0
       real(kind=cp),parameter :: f1=0.9185, f2=0.2285 !in eV units
       logical :: soft_true,found

       if (A%N_Spec == 0) then
          Err_CFML%Ierr=1
          Err_CFML%Msg=" The number of different species is zero, tables cannot be set"
          return
       end if

       if (allocated(Table_Avcoor))   deallocate(Table_Avcoor)
       if (allocated(Table_Rzero))    deallocate(Table_Rzero)
       if (allocated(Table_Rcutoff))  deallocate(Table_Rcutoff)
       if (allocated(Table_Dzero))    deallocate(Table_Dzero)
       if (allocated(Table_Rmin))     deallocate(Table_Rmin)
       if (allocated(Table_Alpha))    deallocate(Table_Alpha)
       if (allocated(Table_Ref))      deallocate(Table_Ref)

       allocate(Table_Avcoor(A%N_Spec,A%N_Spec))  ; Table_Avcoor  =0.0
       allocate(Table_Rzero(A%N_Spec,A%N_Spec))   ; Table_Rzero   =0.0
       allocate(Table_Rcutoff(A%N_Spec,A%N_Spec)) ; Table_Rcutoff =0.0
       allocate(Table_Dzero(A%N_Spec,A%N_Spec))   ; Table_Dzero   =0.0
       allocate(Table_Rmin(A%N_Spec,A%N_Spec))    ; Table_Rmin    =0.0
       allocate(Table_Alpha(A%N_Spec,A%N_Spec))   ; Table_Alpha   =0.0
       allocate(Table_ref(A%N_Spec,A%N_Spec))     ; Table_ref     =0

       soft_true=.False.
       if(present(soft)) soft_true=soft
       if(soft_true) then  !Calculate Rmin and D0 from softBVS parameters and softness
          call Set_Atomic_Properties_Table()
          call Set_SBVS_Table()
          do i=1,A%N_Cations
             ic=0; ac=0
             do j=1,sbvs_species_n
                if (trim(A%Species(i)) == trim(sBVS_Table(j)%Symb)) then
                   ic=j
                   do k=1,Ap_species_n
                     if (trim(A%Species(i)) == trim(Ap_Table(k)%Symb)) then
                       ac=k
                       exit
                     end if
                   end do
                   exit
                end if
             end do
             if (ic == 0 .or. ac == 0) then
                if(.not. present(N_bvel)) then
                  Err_CFML%Ierr=1
                  Err_CFML%Msg=" Cation not found on the internal list: "//A%Species(i)
                  return
                else
                   call Complete_Table_BVEL(A,N_bvel,bvel)
                   if(Err_CFML%Ierr /= 0) then
                        return
                   else
                        cycle
                   end if
                end if
             end if

             do k=1,A%N_Anions
                ia=0; an=0
                do j=1,bvs_anions_n
                   if (trim(A%Species(A%N_Cations+k)) == trim(bvs_anions(j))) then
                      ia=j
                      do L=1,Ap_species_n
                        if (A%Species(i) == Ap_Table(L)%Symb) then
                          an=L
                          exit
                        end if
                      end do
                      exit
                   end if
                end do
                if (ia == 0 .or. an == 0) then
                   if(.not. present(N_bvel)) then
                     Err_CFML%Ierr=1
                     Err_CFML%Msg=" Anion not found on the internal list: "//A%Species(i)
                     return
                   else
                      call Complete_Table_BVEL(A,N_bvel,bvel)
                      if(Err_CFML%Ierr /= 0) then
                           return
                      else
                           cycle
                      end if
                   end if
                end if

                b0=sbvs_table(ic)%b_par(ia)
                r0=sbvs_table(ic)%d0(ia)
                cn=sbvs_table(ic)%cn(ia)
                !write(*,"(a,3f12.4)") " => b0,r0,cn: ", b0,r0,cn
                if( b0+r0 < 0.00001) then
                  !Try to use b0,r0,cn from manually given BVS parameters
                   if(present(nread)) Then
                     found=.false.
                     do j=1,nread
                       i1=index(bvel(j),trim(A%Species(i)))
                       i2=index(bvel(j),trim(bvs_anions(ia)))
                       if(i1 /= 0 .and. i2 /= 0) Then
                         found=.true.
                         aux=adjustl(bvel(i)(i2:))
                         i1=index(aux," ")
                         read(unit=aux(i1:),fmt=*) r0,b0,cn
                         exit
                       end if
                     end do
                     if(.not. found) then
                       Err_CFML%Ierr=1
                       Err_CFML%Msg=" In the user-provided bond-Valence parameters there's no pair: "//trim(A%Species(i))//"-"//bvs_anions(ia)
                       return
                     end if
                   else
                     if(.not. present(N_bvel)) then
                       Err_CFML%Ierr=1
                       Err_CFML%Msg=" Bond-Valence parameters not found for pair: "//trim(A%Species(i))//"-"//bvs_anions(ia)
                       return
                     else
                        call Complete_Table_BVEL(A,N_bvel,bvel)
                        if(Err_CFML%Ierr /= 0) then
                             return
                        else
                             cycle
                        end if
                     end if
                   end if
                end if
                if( cn < 0.1 ) cn=6.0
                Table_Avcoor (i,A%N_Cations+k)=cn
                Table_Rzero  (i,A%N_Cations+k)=r0
                Table_Rcutoff(i,A%N_Cations+k)=max(sbvs_table(ic)%ctoff(ia),4.5)
                Table_Alpha  (i,A%N_Cations+k)=1.0/b0
                Table_ref    (i,A%N_Cations+k)=sbvs_table(ic)%refnum(ia)

                !Formula 24 of ref. 34
                Rmin=r0*(f1+f2*abs(Ap_Table(ac)%sigma-Ap_Table(an)%sigma))-b0*log(real(Ap_Table(ac)%oxs)/cn)
                Select Case(Ap_Table(ac)%b)
                   Case(0,1)
                      D0=14.4*real(Ap_Table(ac)%oxs*Ap_Table(an)%oxs)*b0*b0/(2.0*rmin*sqrt(real(Ap_Table(an)%n*Ap_Table(ac)%n)))
                   Case(2:)
                      D0=14.4*sqrt(real(Ap_Table(ac)%oxs*Ap_Table(an)%oxs))*b0*b0/(rmin*sqrt(real(Ap_Table(an)%n*Ap_Table(ac)%n)))
                End Select
                !write(*,"(a,2f12.4)") " => D0 & Rmin: ",D0,Rmin
                Table_Dzero  (i,A%N_Cations+k)=D0
                Table_Rmin   (i,A%N_Cations+k)=Rmin

                Table_Avcoor (A%N_Cations+k,i)=cn
                Table_Rzero  (A%N_Cations+k,i)=r0
                Table_Rcutoff(A%N_Cations+k,i)=sbvs_table(ic)%ctoff(ia)
                Table_Dzero  (A%N_Cations+k,i)=D0
                Table_Rmin   (A%N_Cations+k,i)=Rmin
                Table_Alpha  (A%N_Cations+k,i)=1.0/b0
                Table_ref    (A%N_Cations+k,i)=sbvs_table(ic)%refnum(ia)

             end do  !Anions
          end do  !Cations
          call Remove_SBVS_Table()
          call Remove_Atomic_Properties_Table()

       else  !Just use the tabulated values

          call Set_BVEL_Table()

          do i=1,A%N_Cations
             ic=0
             do j=1,bvel_species_n
                if (trim(A%Species(i)) == trim(BVEL_Table(j)%Symb)) then
                   ic=j
                   exit
                end if
             end do
             if (ic == 0) then
                if(.not. present(N_bvel)) then
                  Err_CFML%Ierr=1
                  Err_CFML%Msg=" Cation not found on the internal list: "//A%Species(i)
                  return
                else
                   call Complete_Table_BVEL(A,N_bvel,bvel)
                   if(Err_CFML%Ierr /= 0) then
                        return
                   else
                        cycle
                   end if
                end if
             end if

             do k=1,A%N_Anions
                ia=0
                do j=1,bvel_anions_n
                   if (A%Species(A%N_Cations+k) == bvel_anions(j)) then
                      ia=j
                      exit
                   end if
                end do
                if (ia == 0) then
                   if(.not. present(N_bvel)) then
                     Err_CFML%Ierr=1
                     Err_CFML%Msg=" Anion not found on the internal list: "//A%Species(A%N_Cations+k)
                     return
                   else
                      call Complete_Table_BVEL(A,N_bvel,bvel)
                      if(Err_CFML%Ierr /= 0) then
                           return
                      else
                           cycle
                      end if
                   end if
                end if
                Table_Avcoor (i,A%N_Cations+k)=bvel_table(ic)%Avcoor(ia)
                Table_Rzero  (i,A%N_Cations+k)=bvel_table(ic)%Rzero(ia)
                Table_Rcutoff(i,A%N_Cations+k)=bvel_table(ic)%Rcutoff(ia)
                Table_Dzero  (i,A%N_Cations+k)=bvel_table(ic)%Dzero(ia)
                Table_Rmin   (i,A%N_Cations+k)=bvel_table(ic)%Rmin(ia)
                Table_Alpha  (i,A%N_Cations+k)=bvel_table(ic)%Alpha(ia)
                Table_ref    (i,A%N_Cations+k)=bvel_table(ic)%refnum(ia)

                Table_Avcoor (A%N_Cations+k,i)=bvel_table(ic)%Avcoor(ia)
                Table_Rzero  (A%N_Cations+k,i)=bvel_table(ic)%Rzero(ia)
                Table_Rcutoff(A%N_Cations+k,i)=bvel_table(ic)%Rcutoff(ia)
                Table_Dzero  (A%N_Cations+k,i)=bvel_table(ic)%Dzero(ia)
                Table_Rmin   (A%N_Cations+k,i)=bvel_table(ic)%Rmin(ia)
                Table_Alpha  (A%N_Cations+k,i)=bvel_table(ic)%Alpha(ia)
                Table_ref    (A%N_Cations+k,i)=bvel_table(ic)%refnum(ia)

             end do
          end do
          call Remove_BVEL_Table()
       end if

    End Subroutine Set_Table_BVEL_Params

    !!----
    !!---- Module Subroutine Set_Table_d0_b(A,N_bvsm,bvs_m)
    !!---- type (Atoms_Conf_List_type),            intent(in)  :: A
    !!---- integer,                      optional, intent(in)  :: N_bvsm   !Number of bvs strings with externally provided values
    !!---- character(len=*),dimension(:),optional, intent(in)  :: bvs_m    !bvs strings with externally provided values
    !!----
    !!----
    !!----
    !!---- Updated: March - 2005, December-2014
    !!
    Module Subroutine Set_Table_d0_b(A,N_bvsm,bvs_m,soft)
       !---- Arguments ----!
       type (Atoms_Conf_List_type),            intent(in)  :: A
       integer,                      optional, intent(in)  :: N_bvsm
       character(len=*),dimension(:),optional, intent(in)  :: bvs_m
       logical,                      optional, intent(in)  :: soft
       !---- Local Variables ----!
       integer :: i,j,k,ia,ic
       logical :: found

       if (A%N_Spec == 0) then
          Err_CFML%Ierr=1
          Err_CFML%Msg=" The number of different species is zero, tables cannot be set"
          return
       end if

       if (allocated(Table_d0))   deallocate(Table_d0)
       if (allocated(Table_b))    deallocate(Table_b)
       if (allocated(Table_ref))  deallocate(Table_ref)

       allocate(Table_d0(A%N_Spec,A%N_Spec))
       allocate(Table_b(A%N_Spec,A%N_Spec))
       allocate(Table_ref(A%N_Spec,A%N_Spec))

       Table_d0=0.0
       Table_b=0.37
       Table_ref = 0

       call Set_BVS_Table()
       if(present(soft)) call set_SBVS_Table()

       do i=1,A%N_Cations
          ic=0; found=.false.
          if(present(soft)) then
             do j=1,sbvs_species_n
                if (trim(A%Species(i)) == trim(sBVS_Table(j)%Symb)) then
                   ic=j
                   found=.true.
                   exit
                end if
             end do
          end if
          if(ic == 0) then
            do j=1,bvs_species_n
               if (trim(A%Species(i)) == trim(BVS_Table(j)%Symb)) then
                  ic=j
                  exit
               end if
            end do
          end if
          if (ic == 0) then
             if(.not. present(N_bvsm)) then
               Err_CFML%Ierr=1
               Err_CFML%Msg=" Cation not found on the internal list: "//A%Species(i)
               return
             else
                call Complete_Table(A,N_bvsm,bvs_m)
                if(Err_CFML%Ierr /= 0) then
                     return
                else
                     cycle
                end if
             end if
          end if

          do k=1,A%N_Anions
             ia=0
             do j=1,bvs_anions_n
                if (trim(A%Species(A%N_Cations+k)) == trim(bvs_anions(j))) then
                   ia=j
                   exit
                end if
             end do
             if (ia == 0) then
                Err_CFML%Ierr=1
                Err_CFML%Msg=" Anion not found on the internal list: "//A%Species(A%N_Cations+k)
                return
             end if
             if(found) then
                Table_d0 (i,A%N_Cations+k)=sbvs_table(ic)%d0(ia)
                Table_b  (i,A%N_Cations+k)=sbvs_table(ic)%b_par(ia)
                Table_ref(i,A%N_Cations+k)=sbvs_table(ic)%refnum(ia)

                Table_d0 (A%N_Cations+k,i)=sbvs_table(ic)%d0(ia)
                Table_b  (A%N_Cations+k,i)=sbvs_table(ic)%b_par(ia)
                Table_ref(A%N_Cations+k,i)=sbvs_table(ic)%refnum(ia)
             else
                Table_d0 (i,A%N_Cations+k)=bvs_table(ic)%d0(ia)
                Table_b  (i,A%N_Cations+k)=bvs_table(ic)%b_par(ia)
                Table_ref(i,A%N_Cations+k)=bvs_table(ic)%refnum(ia)

                Table_d0 (A%N_Cations+k,i)=bvs_table(ic)%d0(ia)
                Table_b  (A%N_Cations+k,i)=bvs_table(ic)%b_par(ia)
                Table_ref(A%N_Cations+k,i)=bvs_table(ic)%refnum(ia)
             end if

          end do
       end do

       call Remove_BVS_Table()
       if(present(soft)) call Remove_BVS_Table()

    End Subroutine Set_Table_d0_b

    !!----
    !!---- Module Subroutine Species_on_List(A,MulG,tol, covalent,softbvs)
    !!----    type (Atoms_Conf_List_Type), intent(in out) :: A
    !!----    Integer, optional,           intent(in)     :: MulG
    !!----    real(kind=cp), optional,     intent(in)     :: tol
    !!----    logical,       optional,     intent(in)     :: covalent
    !!----    logical,       optional,     intent(in)     :: softbvs
    !!----
    !!----    Determines the different species in the List and,
    !!----    optionally, sets the tolerance factor for ionic radii
    !!----    conditions and provides "corrected" occupation factors
    !!----    (mult/MulG) when the user is using a multiplier. The
    !!----    general multiplicity of the space group MulG must be
    !!----    provided in such a case. This first free variable of the
    !!----    Atom-type A%Atom%VFree(1) is set to the corrected
    !!----    occupation. The first atom in the list must completely
    !!----    occupy its site. If covalent is provided the covalent
    !!----    radius is used instead of the ionic radius. If softbvs is
    !!----    present the Set_Atomic_Properties_Table subroutine is called for
    !!----    calculating Morse parameters if needed.
    !!----
    !!---- Update: March - 2005, December 2014, January 2015
    !!
    Module Subroutine Species_on_List(A, MulG, tol, covalent,softbvs)
       !---- Arguments ----!
       type (Atoms_Conf_List_Type), intent(in out) :: A
       Integer, optional,           intent(in)     :: MulG
       real(kind=cp), optional,     intent(in)     :: tol
       logical,       optional,     intent(in)     :: covalent
       logical,       optional,     intent(in)     :: softbvs

       !---- Local variables ----!
       character(len=4), dimension(50) :: cation,anion,spec
       character(len=2)                :: car,cv
       character(len=4)                :: canio
       integer                         :: i,im,j,v,ns,nc,na
       real(kind=cp)                   :: fac1,fact
       logical                         :: soft_true


       if (A%natoms == 0) return
       soft_true=.False.
       if(present(softbvs)) soft_true=softbvs
       ns=0
       spec  = " "
       nc=0
       cation= " "
       na=0
       anion = " "

       if(present(tol)) A%tol=tol

       if(present(MulG)) then

         fac1=A%atom(1)%Occ*real(MulG)/real(A%atom(1)%mult)
         fac1=1.0/fac1
         A%totatoms=0.0
         do i=1,a%natoms
            fact=real(MulG)/real(a%atom(i)%mult)
            A%Atom(i)%VarF(1)=A%atom(i)%occ*fact*fac1      !Site Occupancy (=1, full occupation)
            A%Atom(i)%VarF(2)=0.0 ! A%atom(i)%occ_std*fact*fac1  !standard deviation of Site Occupancy
            A%totatoms=A%totatoms + A%Atom(i)%VarF(1)*real(a%atom(i)%mult) !total number of atoms/conventional unit cell
         end do

       else

         A%totatoms=0.0
         do i=1,a%natoms
            A%Atom(i)%VarF(1)=A%atom(i)%occ  !The user has given site occupancy
            A%Atom(i)%VarF(2)=0.0 ! A%atom(i)%occ_std
            A%totatoms=A%totatoms + A%Atom(i)%VarF(1)*real(a%atom(i)%mult) !total number of atoms/conventional unit cell
         end do

       end if

       if(present(softbvs) .and. .not. allocated(Ap_Table)) call Set_Atomic_Properties_Table()

       loop1:do i=1, A%natoms
          car=u_case(a%atom(i)%ChemSymb)
          v=a%atom(i)%charge
          if (v == 0) then
             Err_CFML%Ierr=1
             Err_CFML%Msg=" The Atom "//a%atom(i)%lab//"has not charge"
             return
          end if
          write(unit=cv,fmt="(i2)") v
          if (v > 0) cv(1:1)="+"
          canio=car//cv
          canio=pack_string(canio)

          if (v > 0) then
             do j=1,nc
                if (canio == cation(j)) cycle loop1
             end do
             nc=nc+1
             cation(nc)=canio
          else
             do j=1,na
                if (canio == anion(j)) cycle loop1
             end do
             na=na+1
             anion(na)=canio
          end if
          if (na+nc == 50) exit
       end do loop1

       ns=nc+na
       A%N_Spec    = ns
       A%N_Anions  = na
       A%N_Cations = nc

       !---- Order the Species vector ----!
       call sort_strings(cation(1:nc))
       call sort_strings(anion(1:na))
       spec(1:nc)=cation(1:nc)
       spec(nc+1:nc+na)=anion(1:na)

       if (allocated(A%Species)) deallocate(A%Species)
       allocate(A%Species(ns))
       A%Species=spec(1:ns)
       !do i=1,ns
       !  write(*,*) i,A%Species(i)
       !end do

       if (allocated(A%Radius)) deallocate(A%Radius)
       allocate(A%Radius(ns))

       do i=1,nc
          im=index(A%Species(i),"+")
          car=A%Species(i)(im+1:im+1)
          read(unit=car,fmt="(i1)") j
          car=A%Species(i)(1:im-1)
          if(present(covalent) .and. soft_true) then
             A%Radius(i)= get_soft_covalent_radius(A%Species(i))
          else if(present(covalent)) then
             A%Radius(i) = get_covalent_radius(car)
          else
             A%Radius(i) = get_ionic_radius(car,j)
          end if
          if (A%Radius(i) < 0.01) A%Radius(i)=0.8
       end do

       do i=1,A%N_Anions
          do j=1,bvs_anions_n
             if (A%Species(nc+i) == bvs_anions(j)) then
                if(present(covalent) .and. present(softbvs)) then
                    A%Radius(nc+i) = get_soft_covalent_radius(A%Species(nc+i))
                else if(present(covalent)) then
                    A%Radius(nc+i) = get_covalent_radius(A%Species(nc+i))
                else
                    A%Radius(nc+i) = bvs_anions_rion(j)
                end if
                exit
             end if
          end do
       end do

       !---- Fix the index on Atm_Type pointing to the Species vector ----!
       do i=1, A%natoms
          do j=1,ns
             im=index(A%Species(j),"+")
             if (im == 0) im=index(A%Species(j),"-")
             car=A%Species(j)(1:im-1)
             cv=A%Species(j)(im:im+1)
             if (cv(1:1)=="+") cv(1:1)=" "
                read(unit=cv,fmt="(i2)") v
                if (u_case(A%Atom(i)%ChemSymb) == car .and. A%Atom(i)%charge == v) then
                A%atom(i)%ind_ff(1)=j
                exit
             end if
          end do
       end do
    End Subroutine Species_on_List

 End Submodule EnBVS_SetTab
