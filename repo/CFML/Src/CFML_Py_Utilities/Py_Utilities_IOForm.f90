submodule (CFML_Py_Utilities) Utilities_IOForm

implicit none

contains

    module function compute_envelope(g,at_id,axis,step) result(env_list)
        !> Compute the envelope for the specified atom along the specified direction

        ! Arguments
        type(graphical_crystal_type), intent(in) :: g        !> atoms, magnetic moments and bonds inside limits
        integer,                      intent(in) :: at_id    !> index of the atom for which the envelope is calculated
        real, dimension(3),           intent(in) :: axis     !> direction along the envelope is calculated
        real,                         intent(in) :: step     !> step used in the calculation of envelope points
        type(envelope_list_type)                 :: env_list !> list of computed envelopes

        ! Local parameters
        integer, parameter :: NENV_MAX = 100
        real,    parameter :: TOL = 1e-4

        ! Local variables
        integer :: i,j,naxis
        integer, dimension(g%natoms) :: at2env ! map every atom to an envelope, if any. Zero otherwise.
        logical :: new_axis
        type(envelope_list_type)     :: env_list_aux !> list of computed envelopes

        env_list_aux%nenv = 0
        allocate(env_list_aux%env(NENV_MAX))

        at2env(:) = 0
        do i = 1 , g%natoms
            if (g%id_atom(1,i) /= at_id) cycle
            new_axis = .true.
            do j = 1 , naxis
            end do
        end do

    end function compute_envelope

    module function inside_limits(x,limits) result(inside)
        !> Check if an atom with coordinates x is inside limits

        ! Arguments
        real, dimension(3),   intent(in) :: x
        real, dimension(2,3), intent(in) :: limits
        logical                          :: inside

        ! Local variables
        integer :: i

        inside = .true.
        do i = 1 , 3
            if (x(i) < limits(1,i) - EPSILON .or. x(i) > limits(2,i) + EPSILON) then
                inside = .false.
                exit
            end if
        end do

    end function inside_limits

    module function read_crystal_structure(filename,database_path) Result(crystal)
        !> Build the object crystal from data given in a cfl, cif or mcif file

        ! Arguments
        character(len=*),           intent(in) :: filename !> file containing the crystal data
        character(len=*), optional, intent(in) :: database_path
        type(crystal_type)                     :: crystal  !> crystal object

        ! Local variable
        integer :: i,j,m,n,ii,jj,kk,nsites,i_masu
        integer :: n_ini,n_end
        integer, dimension(:,:), allocatable :: sites
        real(kind=cp) :: eps
        real(kind=cp), dimension(3,3) :: U
        real, dimension(3) :: t,r,rp ! vector in crystal coordinates
        real, dimension(3) :: x,xp,xpp ! atom coordinates w.r.t crystal axes
        logical :: existe,new_site
        type(file_type) :: f
        type(file_list_type) :: cfl

        ! Reset variable err_cfml
        call clear_error()

        ! Read crystal structure
        crystal%file = adjustl(trim(filename))
        crystal%fourier = .false.
        crystal%nuclear = .false.
        inquire(file=filename,exist=existe)
        if (.not. existe) then
            err_cfml%ierr = 1
            err_cfml%msg  = 'File '//trim(filename)//' does not exist.'
            return
        end if
        if (present(database_path)) then
            call read_xtal_structure(crystal%file,crystal%cell,crystal%spg,crystal%asu,database_path=database_path)
        else
            call read_xtal_structure(crystal%file,crystal%cell,crystal%spg,crystal%asu)
        end if
        if (err_cfml%ierr /= 0) return
        if (crystal%asu%natoms > 0) crystal%nuclear = .true.

        ! Check if the group is a superspace group
        crystal%super = .false.
        select type (G => crystal%spg)
        type is (SuperSpaceGroup_Type)
            crystal%super = .true.
            G%inv = get_inv_op(G%op) !Assure the inverse operator pointer is set
        end select

        if (crystal%nuclear) then
            call set_elements(crystal)
            call set_asu_orbits(crystal)
        end if

        ! Read a magnetic structure description in terms of fourier coefficients, if any.
        f = Reading_File(trim(crystal%file))
        cfl%nlines = f%nlines
        allocate(cfl%line(f%nlines))
        do i = 1 , f%nlines
            cfl%line(i) = f%line(i)%str
        end do
        n_ini = 1
        n_end = cfl%nlines
        if (present(database_path)) then
            call readn_set_magnetic_kv_structure(cfl,n_ini,n_end,crystal%mgp,crystal%am,database_path=database_path)
        else
            call readn_set_magnetic_kv_structure(cfl,n_ini,n_end,crystal%mgp,crystal%am)
        end if
        eps = get_eps_math()
        call set_eps_math(0.001)

        if (err_cfml%ierr == 0 .and. .not. err_cfml%flag) then
            crystal%fourier = .true.
            if (.not. crystal%nuclear) call set_nuclear_from_magnetic(crystal)
            U = U_from_cr_orth_cell(crystal%cell%cr_orth_cel)
            if (.not. allocated(crystal%magat)) then
                allocate(crystal%magat(crystal%maxmul,crystal%asu%natoms,2))
                allocate(crystal%magtr(crystal%maxmul,crystal%asu%natoms,3))
                crystal%magat(:,:,:) = 0
                crystal%magtr(:,:,:) = 0.0
                do i = 1 , crystal%am%natoms
                    x = crystal%am%atom(i)%x
                    do j = 1 , crystal%mgp%Numops
                        xp = ApplySO(crystal%mgp%symop(j),x)
                        do m = 1 , crystal%mgp%num_lat
                            t(:) = crystal%mgp%ltr(:,m)
                            xpp = xp + t(:)
                            do ii = 1 , crystal%asu%natoms
                                do jj = 1 , crystal%orbit(ii)%mult
                                    if (crystal%magat(jj,ii,1) /= 0) cycle
                                    rp(:) = crystal%orbit(ii)%pos(:,jj) - xpp(:)
                                    if (zbelong(rp)) then
                                        crystal%magat(jj,ii,1) = j
                                        crystal%magat(jj,ii,2) = i
                                        crystal%magtr(jj,ii,:) = t
                                        exit
                                    end if
                                end do
                            end do
                        end do
                    end do
                end do
            end if
        else
            call clear_error()
        end if
        call set_eps_math(eps)

        ! Build id array
        allocate(crystal%split(crystal%asu%natoms))
        allocate(crystal%id(crystal%maxmul,crystal%asu%natoms))
        crystal%nat_masu = 0
        if (crystal%fourier) then
            i_masu = 0
            do ii = 1 , crystal%asu%natoms
                allocate(sites(2,crystal%orbit(ii)%mult))
                nsites = 1
                i_masu = i_masu + 1
                sites(1,1) = crystal%magat(1,ii,2)
                sites(2,1) = i_masu
                crystal%id(1,ii) = i_masu
                do jj = 2 , crystal%orbit(ii)%mult
                    new_site = .true.
                    do kk = 1 , nsites
                        if (crystal%magat(jj,ii,2) == sites(1,kk)) then
                            new_site = .false.
                            crystal%id(jj,ii) = sites(2,kk)
                            exit
                        end if
                    end do
                    if (new_site) then
                        nsites = nsites + 1
                        i_masu = i_masu + 1
                        sites(1,nsites) = crystal%magat(jj,ii,2)
                        sites(2,nsites) = i_masu
                        crystal%id(jj,ii) = i_masu
                    end if
                end do
                crystal%split(ii) = nsites
                deallocate(sites)
            end do
            crystal%nat_masu = i_masu
        else
            do ii = 1 , crystal%asu%natoms
                crystal%split(ii) = 1
                do jj = 1 , crystal%orbit(ii)%mult
                    crystal%id(jj,ii) = ii
                end do
            end do
            crystal%nat_masu = crystal%asu%natoms
        end if

        ! Build magnetic array
        allocate(crystal%magnetic(crystal%nat_masu))
        crystal%magnetic(:) = 0
        if (crystal%fourier) then
            do ii = 1 , crystal%asu%natoms
                do jj = 1 , crystal%orbit(ii)%mult
                    i = crystal%id(jj,ii)
                    if (crystal%magat(jj,ii,2) /= 0) crystal%magnetic(i) = 1
                end do
            end do
        else ! asu and masu are the same
            do i = 1 , crystal%asu%natoms
                if (crystal%asu%atom(i)%magnetic) crystal%magnetic(i) = 1
            end do
        end if

    end function read_crystal_structure

    module function set_boundary(crystal,coor,limits,boundary_coordination) result(c)
        !> Returns atoms, magnetic moments and bonds inside limits

        ! Arguments
        type(crystal_type),              intent(inout) :: crystal               !> crystal object
        type(coordination_crystal_type), intent(in)    :: coor                  !> crystal coordination
        real, dimension(2,3),            intent(in)    :: limits                !> cell limits in crystal coordinates (2,3)
        logical,                         intent(in)    :: boundary_coordination !> if True, includes atoms bonded to boundary atoms
        type(graphical_crystal_type)                   :: c                     !> atoms, magnetic moments and bonds inside limits

        ! Local variables
        integer :: i,ii,j,jj,n,nn,m,tx,ty,tz
        integer :: stat                ! variable to test allocations
        integer :: nat                 ! number of atoms inside limits
        integer :: nat_tot             ! total number of atoms (inside limits + extra atoms to represent all bonds)
        integer :: nat_estimation      ! upper limit to the number of atoms inside limits
        integer :: nbonds              ! number of bonds inside the unit cell
        integer :: nbonds_cell         ! number of bonds in the unit cell
        integer :: nbonds_extra        ! number of bonds with one atom outside limits
        integer :: nbonds_tot          ! total number of bonds
        integer :: ncells              ! number of cells to test
        integer, dimension(3)   :: t   ! lattice translation
        integer, dimension(5)   :: id
        integer, dimension(2,3) :: tr  ! translations
        integer, dimension(:,:,:,:,:), allocatable :: map ! map the index of an atom (asu,orbit,tx,ty,tz) to its position in c
        real :: dx
        real, dimension(3) :: disp ! displacement in an incommensurate structure
        real, dimension(3) :: mom ! magnetic moment referred to the unitary crystal basis
        real, dimension(3) :: r,rp ! vector in crystal coordinates
        real, dimension(3) :: x,xp ! atom coordinates w.r.t crystal axes
        real, dimension(3) :: x_c ! atom cartesian coordinates
        real, dimension(3) :: t_x,t_y,t_z ! lattice vectors
        real, dimension(3,3) :: U ! U matrix used to compute moments in cartesian coordinates
        logical :: add_bond,equiv,existe,is_new
        type(graphical_crystal_type) :: c_aux
        integer, dimension(11,1000) :: test_map

        ! Initialization
        nat          = 0
        nat_tot      = 0
        nbonds       = 0
        nbonds_cell  = 0
        nbonds_extra = 0
        nbonds_tot   = 0
        U = U_from_cr_orth_cell(crystal%cell%cr_orth_cel)
        call clear_error()

        ! Limits for translation vectors
        do i = 1 , 3
            do j = 1 , 2
                dx = limits(j,i) - int(limits(j,i))
                tr(j,i) = int(limits(j,i))
                if (j == 1 .and. dx < -EPSILON) then
                    tr(j,i) = tr(j,i) - 1
                else if (j == 2 .and. dx > EPSILON) then
                    tr(j,i) = tr(j,i) + 1
                end if
            end do
        end do

        ! nat_estimation
        ncells = (tr(2,1) - tr(1,1) + 1) * (tr(2,2) - tr(1,2) + 1) * (tr(2,3) - tr(1,3) + 1)
        nat_estimation = (crystal%nat_cell + coor%nextra) * ncells
        allocate(c_aux%id(nat_estimation),stat=stat)
        if (stat == 0) allocate(c_aux%id_atom(5,nat_estimation),stat=stat)
        if (stat == 0) allocate(c_aux%pos_c(3,nat_estimation),stat=stat)
        if (stat /= 0) then
            err_cfml%ierr = 1
            err_cfml%msg = 'set_boundary: insufficient memory for allocating c_aux'
            return
        end if
        allocate(map(crystal%asu%natoms,crystal%maxmul,tr(1,1)-1:tr(2,1)+1,tr(1,2)-1:tr(2,2)+1,tr(1,3)-1:tr(2,3)+1),stat=stat)
        if (stat /= 0) then
            err_cfml%ierr = 1
            err_cfml%msg = 'set_boundary: insufficient memory for allocating map'
            return
        end if

        ! Atoms inside limits
        map = 0
        do tx = tr(1,1), tr(2,1)
            t(1) = tx
            t_x = tx * crystal%cell%Cr_Orth_cel(:,1)
            do ty = tr(1,2), tr(2,2)
                t(2) = ty
                t_y = ty * crystal%cell%Cr_Orth_cel(:,2)
                do tz = tr(1,3), tr(2,3)
                    t(3) = tz
                    t_z = tz * crystal%cell%Cr_Orth_cel(:,3)
                    do i = 1 , crystal%asu%natoms
                        do ii = 1 , crystal%orbit(i)%mult
                            x = crystal%orbit(i)%pos(1:3,ii) + t(:)
                            if (inside_limits(x,limits)) then
                                nat = nat + 1
                                map(i,ii,tx,ty,tz) = nat
                                c_aux%id(nat) = crystal%id(ii,i)
                                c_aux%id_atom(:,nat) = (/i,ii,tx,ty,tz/)
                                c_aux%pos_c(:,nat) = crystal%orbit(i)%pos_c(1:3,ii) + t_x + t_y + t_z
                            end if
                        end do
                    end do
                end do
            end do
        end do

        ! Add extra atoms and count number of bonds
        nat_tot = nat
        do n = 1 , nat
            i = c_aux%id_atom(1,n) ! asu
            ii = c_aux%id_atom(2,n) ! orbit
            do nn = 1 , coor%asu(i)%orbit(ii)%nbonds
                j = coor%asu(i)%orbit(ii)%bond(nn)%asu
                jj = coor%asu(i)%orbit(ii)%bond(nn)%orbit
                t = c_aux%id_atom(3:5,n) + coor%asu(i)%orbit(ii)%bond(nn)%tr(1:3)
                x = crystal%orbit(j)%pos(1:3,jj) + t(:)
                if (.not. inside_limits(x,limits) .and. boundary_coordination) then
                    x_c = c_aux%pos_c(:,n) + coor%asu(i)%orbit(ii)%bond(nn)%rc
                    nbonds_extra = nbonds_extra + 1
                    is_new = .true.
                    do m = nat + 1, nat_tot
                        id(:) = (/j,jj,t(1),t(2),t(3)/)
                        if (all(id(:) == c_aux%id_atom(:,m))) then
                            is_new = .false.
                            exit
                        end if
                    end do
                    if (is_new) then
                        nat_tot                  = nat_tot + 1
                        map(j,jj,t(1),t(2),t(3)) = nat_tot
                        c_aux%id(nat_tot) = crystal%id(jj,j)
                        c_aux%id_atom(:,nat_tot) = (/j,jj,t(1),t(2),t(3)/)
                        c_aux%pos_c(:,nat_tot)   = x_c
                    end if
                else
                    m = map(j,jj,t(1),t(2),t(3))
                    if (n < m) nbonds = nbonds + 1 ! avoid counting twice the same bond
                end if
            end do
        end do
        nbonds_tot = nbonds + nbonds_extra

        ! Build c
        allocate(c%z(nat_tot),stat=stat)
        if (stat == 0) allocate(c%magnetic(nat_tot),stat=stat)
        if (stat == 0) allocate(c%id(nat_tot),stat=stat)
        if (stat == 0) allocate(c%id_atom(5,nat_tot),stat=stat)
        if (stat == 0) allocate(c%ncoor(nat_tot),stat=stat)
        if (stat == 0) allocate(c%pos_c(3,nat_tot),stat=stat)
        if (stat == 0) allocate(c%mom_c(3,nat_tot),stat=stat)
        if (stat == 0) allocate(c%bond_id(2*nbonds_tot),stat=stat)
        if (stat == 0) allocate(c%bond_asu(2*nbonds_tot),stat=stat)
        if (stat == 0) allocate(c%bond_z(2*nbonds_tot),stat=stat)
        if (stat == 0) allocate(c%bond_orig(3,2*nbonds_tot),stat=stat)
        if (stat == 0) allocate(c%bond_c(3,2*nbonds_tot),stat=stat)
        if (stat /= 0) then
            err_cfml%ierr = 1
            err_cfml%msg = 'set_boundary: insufficient memory for allocating c'
            return
        end if
        c%natoms         = nat_tot
        c%nbonds         = nbonds_tot
        c%id(:)          = c_aux%id(1:nat_tot)
        c%id_atom(:,:)   = c_aux%id_atom(:,1:nat_tot)
        c%pos_c(:,:)     = c_aux%pos_c(:,1:nat_tot)
        c%ncoor          = 0
        c%bond_z(:)      = 0
        c%bond_orig(:,:) = 0.0
        c%bond_c(:,:)    = 0.0
        c%magnetic(:)    = 0
        nbonds = 0
        do n = 1 , nat
            i = c_aux%id_atom(1,n) ! asu
            ii = c_aux%id_atom(2,n) ! orbit
            c%z(n) = crystal%asu%atom(i)%z
            c%magnetic(n) = crystal%magnetic(crystal%id(ii,i))
            c%mom_c(:,n) = crystal%orbit(i)%mom_c(1:3,ii)
            ! Compute moments in an incommensurate structure
            !  a. Described by propagation vectors
            if (crystal%fourier) then
                if (crystal%magat(ii,i,1) > 0) then
                    r(1:3) = c_aux%id_atom(3:5,n) + crystal%magtr(ii,i,1:3)
                    call calc_magnetic_moment_kvec(crystal%am,crystal%mgp,crystal%am%atom(crystal%magat(ii,i,2))%lab,crystal%magat(ii,i,1),r,x,mom)
                    !write(*,'(a4,2i4,6f6.2)') adjustl(trim(crystal%asu%atom(i)%lab)),i,crystal%orbit(i)%pts(ii),r,mom
                    c%mom_c(:,n) = matmul(U,mom)
                end if
            else
                ! b. Desribed by a superspace group
                select type (G => crystal%spg)
                    type is (SuperSpaceGroup_Type)
                        r(1:3) = c_aux%id_atom(3:5,n) + crystal%orbit(i)%lat(:,ii)
                        call get_moment_and_displacement_ssg(crystal%asu,G,crystal%asu%atom(i)%lab,crystal%orbit(i)%pts(ii),r,x,disp,mom)
                            ! Testing purposes
                            !write(*,'(a4,5i4,3f6.2)') adjustl(trim(crystal%asu%atom(i)%lab)),i,crystal%orbit(i)%pts(ii),crystal%orbit(i)%lat(:,ii)+c_aux%id_atom(3:5,n),mom
                        c%mom_c(:,n) = matmul(U,mom)
                end select
            end if
            do nn = 1 , coor%asu(i)%orbit(ii)%nbonds
                j = coor%asu(i)%orbit(ii)%bond(nn)%asu
                jj = coor%asu(i)%orbit(ii)%bond(nn)%orbit
                t = c_aux%id_atom(3:5,n) + coor%asu(i)%orbit(ii)%bond(nn)%tr(1:3)
                m = map(j,jj,t(1),t(2),t(3))
                if (boundary_coordination) then
                    if (m == 0) then
                        err_cfml%ierr = 1
                        err_cfml%msg = 'set_boundary: Mapping did not work'
                        return
                    end if
                else
                    if (m == 0) cycle
                end if
                if (n < m) then
                    nbonds = nbonds + 1
                    c%bond_asu(nbonds)    = crystal%id(ii,i)
                    c%bond_z(nbonds)      = crystal%asu%atom(i)%z
                    c%bond_orig(:,nbonds) = c%pos_c(:,n)
                    c%bond_c(:,nbonds)    = coor%asu(i)%orbit(ii)%bond(nn)%rc * 0.5
                    c%bond_id(nbonds)     = coor%asu(i)%orbit(ii)%bond(nn)%id
                    c%ncoor(n)            = c%ncoor(n) + 1
                    nbonds = nbonds + 1
                    c%bond_asu(nbonds)    = crystal%id(jj,j)
                    c%bond_z(nbonds)      = crystal%asu%atom(j)%z
                    c%bond_orig(:,nbonds) = c%pos_c(:,n) + c%bond_c(:,nbonds-1)
                    c%bond_c(:,nbonds)    = c%bond_c(:,nbonds-1)
                    c%bond_id(nbonds)     = c%bond_id(nbonds-1)
                    c%ncoor(m)            = c%ncoor(m) + 1
                end if
            end do
        end do
        nbonds = nbonds / 2

        do n = nat + 1 , nat_tot
            i = c_aux%id_atom(1,n) ! asu
            ii = c_aux%id_atom(2,n) ! orbit
            c%z(n) = crystal%asu%atom(i)%z
            c%magnetic(n) = crystal%magnetic(crystal%id(ii,i))
            c%mom_c(:,n) = crystal%orbit(i)%mom_c(1:3,ii)
            ! Compute moments in an incommensurate structure
            !  a. Described by propagation vectors
            if (crystal%fourier) then
                if (crystal%magat(ii,i,1) > 0) then
                    r(1:3) = c_aux%id_atom(3:5,n) + crystal%magtr(ii,i,1:3)
                    call calc_magnetic_moment_kvec(crystal%am,crystal%mgp,crystal%am%atom(crystal%magat(ii,i,2))%lab,crystal%magat(ii,i,1),r,x,mom)
                    c%mom_c(:,n) = matmul(U,mom)
                end if
            else
                ! b. Desribed by a superspace group
                select type (G => crystal%spg)
                    type is (SuperSpaceGroup_Type)
                        r(1:3) = c_aux%id_atom(3:5,n) + crystal%orbit(i)%lat(:,ii)
                        call get_moment_and_displacement_ssg(crystal%asu,G,crystal%asu%atom(i)%lab,&
                            crystal%orbit(i)%pts(ii),r,x,disp,mom)
                        c%mom_c(:,n) = matmul(U,mom)
                end select
            end if
        end do

    end function set_boundary

    module function set_crystal_coordination(crystal,dmin,dmax) result(coor)

        type(crystal_type),                      intent(inout) :: crystal  !> crystal object
        real(kind=cp), dimension(:,:), optional, intent(in)    :: dmin     !> minimum distance between species i,j of the asymmetric unit (nasu,nasu)
        real(kind=cp), dimension(:,:), optional, intent(in)    :: dmax     !> maximum distance between species i,j of the asymmetric unit (nasu,nasu)
        type(coordination_crystal_type)                        :: coor     !> crystal coordination

        ! Local parameter
        integer, parameter :: MAX_COOR = 20    ! maximum allowed coordination, safe value
        real,    parameter :: ELONGATION = 1.2 ! % of elongation of typical bond-distances for setting dmax

        ! Local variables
        integer       :: i,ii,j,jj,m,n,tx,ty,tz,stat
        integer       :: z_i,z_j   ! atomic numbers
        integer       :: nn        ! Number of neighbors
        integer, dimension(:,:), allocatable :: bond_id
        integer, dimension(:,:,:,:,:), allocatable :: extra_atom ! used for counting atoms with translation != (0,0,0) needed for representing coordination
        real(kind=cp) :: d
        real(kind=cp), dimension(3) :: t_x,t_y,t_z,r_ij
        logical       :: is_dmin,is_dmax
        type(crystal_bond_type), dimension(MAX_COOR) :: orbit_bond

        ! Allocate arrays
        allocate(coor%asu(crystal%asu%natoms))
        coor%nextra = 0
        do i = 1 , crystal%asu%natoms
            allocate(coor%asu(i)%orbit(crystal%orbit(i)%mult),stat=stat)
            if (stat /= 0) then
                err_cfml%ierr = 1
                err_cfml%msg = 'set_crystal_coordination: insufficient memory for allocating coor%asu%orbit'
                return
            end if
        end do

        ! Init dmin and dmax arrays
        is_dmin = .false.
        is_dmax = .false.
        if (present(dmin)) then
            if (size(dmin,1) == crystal%nat_masu .and. size(dmin,2) == crystal%nat_masu) is_dmin = .true.
        end if
        if (present(dmax)) then
            if (size(dmax,1) == crystal%nat_masu .and. size(dmax,2) == crystal%nat_masu) is_dmax = .true.
        end if
        stat = 0
        allocate(coor%dmin(crystal%nat_masu,crystal%nat_masu),stat=stat)
        if (stat == 0) allocate(coor%dmax(crystal%nat_masu,crystal%nat_masu))
        if (stat /= 0) then
            err_cfml%ierr = 1
            err_cfml%msg = 'set_crystal_coordination: insufficient memory for allocating coor%dmin and coor%dmax'
            return
        end if
        if (.not. is_dmin) then
            coor%dmin = 0.1
        else
            coor%dmin(:,:) = dmin(:,:)
            do i = 1 , crystal%nat_masu
                do j = 1 , crystal%nat_masu
                    if (coor%dmin(i,j) < 0.1) coor%dmin(i,j) = 0.1
                end do
            end do
        end if
        if (.not. is_dmax) then
            coor%dmax = 0.0
            call set_chem_info()
            call set_bonds_table()
            m = 0
            do i = 1 , crystal%asu%natoms
                z_i = crystal%asu%atom(i)%Z
                do ii = 1, crystal%split(i)
                    m = m + 1
                    n = 0
                    do j = 1 , crystal%asu%natoms
                        z_j = crystal%asu%atom(j)%Z
                        do jj = 1 , crystal%split(j)
                            n = n + 1
                            if (n < m) cycle
                            if (z_i < 1 .or. z_j < 1 .or. z_i > size(bond_length_table,2) .or. z_j > size(bond_length_table,2)) cycle
                            if (bond_length_table(1,z_i,z_j) > 0.1) coor%dmax(m,n) = bond_length_table(1,z_i,z_j) * ELONGATION
                            coor%dmax(n,m) = coor%dmax(m,n)
                        end do
                    end do
                end do
            end do
            call remove_chem_info()
            call remove_bonds_table()
        else
            coor%dmax(:,:) = dmax(:,:)
        end if

        ! Ids for bond types
        allocate(bond_id(crystal%nelem,crystal%nelem))
        n = 1
        do i = 1 , crystal%nelem
            do j = i , crystal%nelem
                bond_id(i,j) = n
                bond_id(j,i) = n
                n = n + 1
            end do
        end do

        ! Compute bonds for the asymmetric unit
        allocate(extra_atom(crystal%asu%natoms,crystal%maxmul,-1:1,-1:1,-1:1))
        extra_atom = 0
        do i = 1 , crystal%asu%natoms
            do ii = 1 , crystal%orbit(i)%mult
                m = crystal%id(ii,i)
                nn = 0
                do j = 1 , crystal%asu%natoms
                    do jj = 1 , crystal%orbit(j)%mult
                        n = crystal%id(jj,j)
                        do tx = -1 , 1
                            t_x = tx * crystal%cell%Cr_Orth_cel(:,1)
                            do ty = -1 , 1
                                t_y = ty * crystal%cell%Cr_Orth_cel(:,2)
                                do tz = -1 , 1
                                    t_z = tz * crystal%cell%Cr_Orth_cel(:,3)
                                    r_ij = crystal%orbit(j)%pos_c(1:3,jj) + t_x + t_y + t_z - crystal%orbit(i)%pos_c(1:3,ii)
                                    d = sqrt(dot_product(r_ij,r_ij))
                                    if (d > coor%dmax(m,n) .or. d < coor%dmin(m,n)) cycle
                                    nn = nn + 1
                                    if (nn > MAX_COOR) then
                                        nn = nn - 1
                                        cycle
                                    end if
                                    orbit_bond(nn)%asu   = j
                                    orbit_bond(nn)%orbit = jj
                                    orbit_bond(nn)%id    = bond_id(crystal%asu2elem(i),crystal%asu2elem(j))
                                    orbit_bond(nn)%tr    = (/ tx,ty,tz /)
                                    orbit_bond(nn)%rc(:) = r_ij
                                    orbit_bond(nn)%d     = d
                                    if (tx /= 0 .or. ty /= 0 .or. tz /= 0 .and. extra_atom(j,jj,tx,ty,tz) == 0) then
                                        extra_atom(j,jj,tx,ty,tz) = 1
                                        coor%nextra = coor%nextra + 1
                                    end if
                                end do
                            end do
                        end do
                    end do
                end do
                coor%asu(i)%orbit(ii)%nbonds = nn
                allocate(coor%asu(i)%orbit(ii)%bond(nn))
                do n = 1 , nn
                    coor%asu(i)%orbit(ii)%bond(n)%asu   = orbit_bond(n)%asu
                    coor%asu(i)%orbit(ii)%bond(n)%orbit = orbit_bond(n)%orbit
                    coor%asu(i)%orbit(ii)%bond(n)%id    = orbit_bond(n)%id
                    coor%asu(i)%orbit(ii)%bond(n)%tr    = orbit_bond(n)%tr
                    coor%asu(i)%orbit(ii)%bond(n)%rc    = orbit_bond(n)%rc
                    coor%asu(i)%orbit(ii)%bond(n)%d     = orbit_bond(n)%d
                end do
            end do
        end do

    end function set_crystal_coordination

    module function set_mask_atoms(id_atom,id_to_hide) result(mask_array)
        ! Build the mask array for CrystalView

        ! Arguments
        integer, dimension(:), intent(in)            :: id_atom       ! index referring to an atom in the asymmetric unit
        integer, dimension(:), intent(in)            :: id_to_hide    ! index referring to an atom in the asymmetric unit
        integer, dimension(:), allocatable           :: mask_array

        ! Local variables
        integer :: i,j
        integer :: stat ! variable to test allocations

        allocate(mask_array(size(id_atom)),stat=stat)
        if (stat /= 0) then
            err_cfml%ierr = 1
            err_cfml%msg = 'set_mask_atoms: insufficient memory for allocating mask_array'
            return
        end if
        mask_array(:) = 1
        do i = 1 , size(id_atom)
            do j = 1 , size(id_to_hide)
                if (id_atom(i) == id_to_hide(j)) then
                    mask_array(i) = 0
                    exit
                end if
            end do
        end do

    end function set_mask_atoms

    module function U_from_cr_orth_cell(cr_orth_cel) result(U)

        ! Arguments
        real(kind=cp), dimension(3,3), intent(in) :: cr_orth_cel
        real(kind=cp), dimension(3,3)             :: U

        ! Local variables
        integer :: i,j
        real(kind=cp) :: l

        do i = 1 , 3
            l = 0.0
            do j = 1 , 3
                l = l + cr_orth_cel(j,i)**2
            end do
            U(:,i) = cr_orth_cel(:,i) / sqrt(l)
        end do

    end function U_from_cr_orth_cell

    module subroutine set_asu_orbits(crystal)

        ! Arguments
        type(crystal_type), intent(inout) :: crystal       !> crystal object

        ! Local variables
        integer :: i,j
        real(kind=cp), dimension(3,3) :: U
        type(point_orbit) :: orbit

        ! Get orbits
        if (crystal%asu%natoms > 0) then
            U = U_from_cr_orth_cell(crystal%cell%cr_orth_cel)
            allocate(crystal%orbit(crystal%asu%natoms))
            do i = 1 , crystal%asu%natoms
                if (crystal%super) then
                    call get_orbit(crystal%asu%atom(i)%x,crystal%spg,orbit,mom=crystal%asu%atom(i)%moment,orb3D=crystal%orbit(i))
                else
                    call get_orbit(crystal%asu%atom(i)%x,crystal%spg,crystal%orbit(i),mom=crystal%asu%atom(i)%moment)
                end if
                allocate(crystal%orbit(i)%pos_c(3,crystal%orbit(i)%mult))
                allocate(crystal%orbit(i)%mom_c(3,crystal%orbit(i)%mult))
                do j = 1 , crystal%orbit(i)%mult
                    crystal%orbit(i)%pos_c(1:3,j) = matmul(crystal%cell%cr_orth_cel,crystal%orbit(i)%pos(1:3,j)) ! Cartesian coordinates
                    crystal%orbit(i)%mom_c(1:3,j) = matmul(U,crystal%orbit(i)%mom(1:3,j))                        ! Cartesian coordinates
                end do
            end do

            ! Set maxmul and crystal%nat_cell
            crystal%maxmul = 1
            crystal%nat_cell = 0
            do i = 1 , crystal%asu%natoms
                crystal%nat_cell = crystal%nat_cell + crystal%orbit(i)%mult
                if (crystal%orbit(i)%mult > crystal%maxmul) crystal%maxmul = crystal%orbit(i)%mult
            end do
        end if

    end subroutine set_asu_orbits

    module subroutine set_asu_orbits_from_magnetic(crystal)

        ! Arguments
        type(crystal_type), intent(inout) :: crystal       !> crystal object

        ! Local variables
        integer :: i,j
        real(kind=cp), dimension(3) :: r_latt
        real(kind=cp), dimension(3,3) :: U
        type(point_orbit_kv) :: orbit

        ! Get orbits
        r_latt(:) = 0.0
        if (crystal%asu%natoms > 0) then
            U = U_from_cr_orth_cell(crystal%cell%cr_orth_cel)
            allocate(crystal%orbit(crystal%asu%natoms))
            do i = 1 , crystal%am%natoms
                call get_kv_orbit(crystal%am%atom(i),crystal%mgp,r_latt,orbit)
                allocate(crystal%orbit(i)%pts(orbit%mult))
                allocate(crystal%orbit(i)%pos(3,orbit%mult))
                allocate(crystal%orbit(i)%mom(3,orbit%mult))
                allocate(crystal%orbit(i)%pos_c(3,orbit%mult))
                allocate(crystal%orbit(i)%mom_c(3,orbit%mult))
                allocate(crystal%orbit(i)%lat(3,orbit%mult))
                crystal%orbit(i)%mult = orbit%mult
                crystal%orbit(i)%pts(:) = orbit%pts(:)
                crystal%orbit(i)%pos(:,:) = orbit%pos(:,:)
                crystal%orbit(i)%mom(:,:) = orbit%mom(:,:)
                crystal%orbit(i)%lat(:,:) = orbit%latt(:,:)
                do j = 1 , crystal%orbit(i)%mult
                    crystal%orbit(i)%pos_c(1:3,j) = matmul(crystal%cell%cr_orth_cel,crystal%orbit(i)%pos(1:3,j)) ! Cartesian coordinates
                    crystal%orbit(i)%mom_c(1:3,j) = matmul(U,crystal%orbit(i)%mom(1:3,j))                        ! Cartesian coordinates
                end do
            end do

            ! Set maxmul and crystal%nat_cell
            crystal%maxmul = 1
            crystal%nat_cell = 0
            do i = 1 , crystal%am%natoms
                crystal%nat_cell = crystal%nat_cell + crystal%orbit(i)%mult
                if (crystal%orbit(i)%mult > crystal%maxmul) crystal%maxmul = crystal%orbit(i)%mult
            end do
        end if

    end subroutine set_asu_orbits_from_magnetic

    module subroutine set_elements(crystal)

        ! Arguments
        type(crystal_type), intent(inout) :: crystal !> crystal object

        ! Local variables
        integer :: i,j
        logical :: new_elem
        character(len=2), dimension(:), allocatable :: element

        crystal%nelem = 0
        if (crystal%asu%natoms > 0) then
            allocate(crystal%asu2elem(crystal%asu%natoms))
            allocate(element(crystal%asu%natoms))
            do i = 1 , crystal%asu%natoms
                new_elem = .true.
                do j = 1 , crystal%nelem
                    if (element(j) == u_case(crystal%asu%atom(i)%chemsymb)) then
                        new_elem = .false.
                        crystal%asu2elem(i) = j
                        exit
                    end if
                end do
                if (new_elem) then
                    crystal%nelem = crystal%nelem + 1
                    crystal%asu2elem(i) = crystal%nelem
                    element(crystal%nelem) = u_case(crystal%asu%atom(i)%chemsymb)
                end if
            end do
            allocate(crystal%element(crystal%nelem))
            crystal%element(:) = element(1:crystal%nelem)
        end if

    end subroutine set_elements

    module subroutine set_nuclear_from_magnetic(crystal)
        !> Build the nuclear part from the magnetic one when the nuclear
        !> was not specified in the crystal file

        ! Arguments
        type(crystal_type), intent(inout) :: crystal       !> crystal object

        ! Local variables
        integer :: i
        if (.not. allocated(crystal%spg)) call set_spacegroup("P 1",crystal%spg)
        crystal%asu%natoms = crystal%am%natoms
        allocate(crystal%asu%atom(crystal%asu%natoms))
        do i = 1 , crystal%am%natoms
            crystal%asu%atom(i)%lab      = crystal%am%atom(i)%lab
            crystal%asu%atom(i)%chemsymb = get_chem_symb(crystal%asu%atom(i)%lab)
            crystal%asu%atom(i)%z        = get_z_symb(crystal%asu%atom(i)%chemsymb)
            crystal%asu%atom(i)%x        = crystal%am%atom(i)%x
            crystal%asu%atom(i)%magnetic = .True.
        end do
        call set_elements(crystal)
        call set_asu_orbits_from_magnetic(crystal)

    end subroutine set_nuclear_from_magnetic

    module subroutine update_global_phase(global_phase,crystal,c)

        ! Arguments
        real,                            intent(in)    :: global_phase  !> global_phase
        type(crystal_type),              intent(in)    :: crystal       !> crystal object
        type(graphical_crystal_type),    intent(inout) :: c             !> atoms, magnetic moments and bonds inside limits

        ! Local variables
        integer :: i,ii,n
        real, dimension(3) :: dis ! displacement vector referred to the unitary crystal basis
        real, dimension(3) :: mom ! magnetic moment referred to the unitary crystal basis
        real, dimension(3) :: r   ! vector in crystal coordinates
        real, dimension(3) :: x   ! atom coordinates w.r.t crystal axes
        real, dimension(3,3) :: U ! U matrix used to compute moments in cartesian coordinates
        real, dimension(:), allocatable :: p_shift

        U = U_from_cr_orth_cell(crystal%cell%cr_orth_cel)
        select type (G => crystal%spg)
            type is (SuperSpaceGroup_Type)
                if (.not. allocated(p_shift)) allocate(p_shift(G%nk))
                p_shift(:) = global_phase
        end select
        do n = 1 , c%natoms
            i = c%id_atom(1,n) ! asu
            ii = c%id_atom(2,n) ! orbit
            if (crystal%fourier) then
                ! a. Described by propagation vectors
                if (crystal%magat(ii,i,1) > 0) then
                    r(1:3) = c%id_atom(3:5,n) + crystal%magtr(ii,i,1:3)
                    call calc_magnetic_moment_kvec(crystal%am,crystal%mgp, &
                               crystal%am%atom(crystal%magat(ii,i,2))%lab, &
                               crystal%magat(ii,i,1),r, x,mom,glb_phase=global_phase)
                    c%mom_c(1:3,n) = matmul(U,mom)
                end if
            else
                ! b. Desribed by a superspace group
                select type (G => crystal%spg)
                    type is (SuperSpaceGroup_Type)
                        r(1:3) = c%id_atom(3:5,n) + crystal%orbit(i)%lat(:,ii)
                        call get_moment_and_displacement_ssg(crystal%asu,G,crystal%asu%atom(i)%lab,&
                            crystal%orbit(i)%pts(ii),r,x,dis,mom,phase_shift=p_shift)
                        c%mom_c(:,n) = matmul(U,mom)
                end select
            end if
        end do

    end subroutine update_global_phase

end submodule Utilities_IOForm