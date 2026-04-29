 Module Atoms_in_BOX
   use CFML_Globaldeps
   use CFML_Atoms,             only: AtList_Type, ModAtm_Std_Type
   use CFML_gSpaceGroups,      only: Spg_Type, SuperSpaceGroup_Type, Point_Orbit, get_Orbit, Is_Lattice_Vec, &
                                     Get_Orbit3D, Orbit_type, Orbit_List
   use CFML_Strings,           only: pack_string
   use CFML_rational
   use CFML_Maths,             only: equal_vector,Lat_Modulo,EPSS, Zbelong
   Use CFML_Scattering_Tables, only: Get_Chem_Symb

   implicit none
   public

   !!  Type, public :: Point_Orbit
   !!     integer                                      :: Mult=0 ! Multiplicity of the orbit
   !!     real(kind=cp),allocatable,dimension(:,:)     :: pos    ! (d,Mult) Positions of the points
   !!     real(kind=cp),allocatable,dimension(:,:)     :: mom    ! (d,Mult) Associated moments
   !!     integer,      allocatable,dimension(:)       :: pts    ! (  Mult) Pointer to symmetry operator
   !!     integer,      allocatable,dimension(:,:)     :: Lat    ! (d,Mult) lattice translation used to
   !!  End Type Point_Orbit                                      ! put the atom  g(i).pos(:,1) within the cell
   !!                                                            ! pos(:,i)=  g(pts(i)).pos(:,1) + Lat(:,i)

   type, Extends (Point_Orbit) :: orbit_ssp
     character(len=20), dimension(:),   allocatable :: lab         !Atom label
     character(len=2) , dimension(:),   allocatable :: ChemSymb    !Chemical symbol
     integer,           dimension(:),   allocatable :: Ls          !Pointer to a lattice translation
     integer,           dimension(:,:), allocatable :: Latt        !Additional lattice translation to the operator
   end type orbit_ssp

   type :: orbit_list_ssg
     integer :: num_orbs
     type(orbit_ssp), dimension(:), allocatable :: orbit
   end type orbit_list_ssg

   Contains

   !! Module Subroutine Get_Orbita(x,Spg,orbit,mom,orb3D,convl,Tbox)
   !!
   !!---- Subroutine to generate the orbit of a point x in space/superspace
   !!---- If mom is not present then the orbit%mom components remain unallocated
   !!---- If orb3D is present, the restriction to 3D space is also output.
   !!---- If convl is present and convl=.false., only the orbit within a primitive
   !!---- cell is output. If Tbox is present, the orbit is extended to a supercell
   !!---- that is [ Tbox(1) x a, Tbox(2) x b, Tbox(3) x c ]  by adding all possible
   !!---- translations to the Zero-cell orbit
   !!----
   !!---- The orb object is of type Point Orbit defined in the general module
   !!---- and repeated here for the sake of proper documentation.
   !!  Type, public :: Point_Orbit
   !!     integer                                      :: Mult=0 ! Multiplicity of the orbit
   !!     real(kind=cp),allocatable,dimension(:,:)     :: pos    ! (d,Mult) Positions of the points
   !!     real(kind=cp),allocatable,dimension(:,:)     :: mom    ! (d,Mult) Associated moments
   !!     integer,      allocatable,dimension(:)       :: pts    ! (  Mult) Pointer to symmetry operator
   !!     integer,      allocatable,dimension(:,:)     :: Lat    ! (d,Mult) lattice translation used to
   !!  End Type Point_Orbit                                      ! put the atom  g(i).pos(:,1) within the cell
   !!                                                            ! pos(:,i)=  g(pts(i)).pos(:,1) + Lat(:,i)
   !!
   Subroutine Get_Orbita(x,Spg,orbit,tbox)
      !---- Arguments ----!
      real(kind=cp), dimension(:),          intent(in)  :: x
      class(SpG_Type),                      intent(in)  :: spg
      type(Point_Orbit),                    intent(out) :: orbit
      integer,       dimension(3),optional, intent(in)  :: tbox

      !---- Local variables ----!
      integer                                          :: j, nt,d,mult,L,mtot,i1,i2,i3,k
      real(kind=cp), dimension(Spg%d)                  :: xs,xsp
      real(kind=cp), dimension(Spg%d-1)                :: v
      real(kind=cp), dimension(Spg%d,Spg%d,Spg%multip) :: Om
      real(kind=cp), dimension(Spg%d-1,Spg%multip)     :: Orb
      integer,       dimension(Spg%d-1)                :: Latt
      integer,       dimension(Spg%d-1,Spg%multip)     :: Lat
      integer,       dimension(:),   allocatable       :: ptr
      integer,       dimension(:,:), allocatable       :: trn

      d=Spg%d-1
      xs(Spg%d)=1.0_cp
      xs(1:3)=x(1:3)
      orb=0.0_cp

      do j=1,Spg%Multip
        Om(:,:,j)=Spg%Op(j)%Mat
      end do

      allocate(ptr(Spg%multip))
      ptr=0
      mult=1
      orb(:,mult)=xs(1:d)
      Lat(:,mult)=0
      ptr(mult) = 1   !Pointer to symmetry operator
      do_ext: do j=2,Spg%Multip
         xsp=matmul(Om(:,:,j),xs)
         call Lat_Modulo(xsp(1:d),v(1:d),Latt)
         xsp(1:d)=v(1:d)
         do nt=1,mult
            v(1:3)=orb(1:3,nt)-xsp(1:3)
            if(sum(abs(v(1:3))) < 2.0 * EPSS) cycle do_ext
            if (Zbelong(v(1:3))) cycle do_ext
         end do
         mult=mult+1
         orb(:,mult)=xsp(1:d)
         Lat(:,mult)=Latt(1:d)
         ptr(mult) = j   !Pointer to symmetry operator
      end do do_ext

      if(present(tbox)) then
         nt=tbox(1)*tbox(2)*tbox(3)
         allocate(trn(3,nt))
         trn=0
         nt=0
         do i1=0,tbox(1)-1
           do i2=0,tbox(2)-1
             do i3=0,tbox(3)-1
               nt=nt+1
               trn(:,nt)=[i1,i2,i3]
             end do
           end do
         end do

         mtot=mult*nt
         orbit%Mult=mtot
         allocate (orbit%pos(3,mtot),orbit%pts(mtot),orbit%Lat(3,mtot))
         orbit%pos=0.0; orbit%Lat=0
         k=0
         do L=1,nt
           do j=1,Mult
             k=k+1
             orbit%pos(1:3,k) = orb(1:3,j) + trn(:,L)
             orbit%pts(    k) = ptr(j)
             orbit%Lat(1:3,k) = Lat(1:3,j) + trn(:,L)
           end do
         end do
      else
         orbit%Mult=mult
         orbit%pos=orb(:,1:mult)   !Automatic allocation
         orbit%pts=ptr(1:mult)
         orbit%Lat=Lat(:,1:mult)
      end if

    End Subroutine Get_Orbita

    Subroutine Get_moment_and_displacement_Lab(AtL,SpG,atm_lab,num_op,R_latt,pos,Disp,Moment,phase_shift)
      !---- Arguments ----!
      type(AtList_Type),            intent(in) :: AtL
      type(SuperSpaceGroup_type),   intent(in) :: SpG
      character(len=*),             intent(in) :: atm_lab
      Integer,                      intent(in) :: num_op
      real(kind=cp), dimension(3),  intent(in) :: R_latt
      real(kind=cp), dimension(3),  intent(out):: pos     !Final average atom position
      real(kind=cp), dimension(3),  intent(out):: Disp
      real(kind=cp), dimension(3),  intent(out):: moment
      real(kind=cp), dimension(:),  intent(in),optional :: phase_shift
      logical :: found
      integer :: n, i
      !
      moment=0.0; found=.false.; disp=0.0; n=0; pos=0.0

      do i=1,AtL%natoms
        if(trim(atm_lab) == trim(AtL%Atom(i)%Lab)) then
          found=.true.
          n=i
          exit
        end if
      end do
      if(found) then
        Select Type (At => AtL%atom(n))
          Type is (ModAtm_Std_Type)
            if(present(phase_shift)) then
               call Get_moment_and_displacement_ori(At,SpG,num_op,R_latt,disp,moment,pos,phase_shift)
            else
               call Get_moment_and_displacement_ori(At,SpG,num_op,R_latt,disp,moment,pos)
            end if
        End Select
      else
         write(*,"(a)") "  ATOM: "//trim(atm_lab)//"  not found!"
      end if
    End Subroutine Get_moment_and_displacement_Lab

    Subroutine Get_moment_and_displacement_ori(At,SpG,s,Lat,disp,moment,pos,phase_shift,prn)
      type(ModAtm_Std_Type),        intent(in) :: At     !Modulated atom type
      type(SuperSpaceGroup_type),   intent(in) :: SpG
      integer,                      intent(in) :: s      !Pointer to the symmetry operator relating x and x0: x= g(s) x0 + t(s) + Lat
      real(kind=cp), dimension(3),  intent(in) :: Lat    !Additional lattice translation
      real(kind=cp), dimension(3), intent(out) :: disp   !Amplitude of displacement modulation at position x= g(s) At%x + ts + Lat
      real(kind=cp), dimension(3), intent(out) :: moment !Amplitude of magnetic moment at position x= g(s) At%x + ts + Lat
      real(kind=cp), dimension(3), optional,intent(out) :: pos    !Final position x= g(s) At%x + ts + Lat
      real(kind=cp), dimension(:), optional, intent(in) :: phase_shift      !phase_shift   xI= phase_shift + (mE, rI)
      logical, optional, intent(in) :: prn
      !---- Local variables ----!
      real(kind=cp)                      :: xarg
      real(kind=cp), dimension(SpG%nk)   :: rI,tgI,rgI   ! Internal transformed space vector from rI=(x4, x5, ... x3+d) at new position
      real(kind=cp), dimension(SpG%d-1)  :: ts           ! ts=(t,tgI) Total translation in superspace operator
      integer                            :: i, j, k, mod_max, d, ds
                                                                                            !                         [  g    0   t  ]
      integer, dimension(SpG%nk,SpG%nq)     :: n,ng        ![n].Eg  nk-vectors              !   Superspace operator: [  Hg   Eg  tgI  ]   ts=(t,tgI)
      integer, dimension(SpG%nk,3)          :: Hg          !Hg(nk,3)                        !                         [  0    0   1  ]
      integer, dimension(3,3)               :: g, magm     !g, magm= delta * det(g) * g     !                         [  inv_g    0      it  ]
      integer, dimension(SpG%nk,SpG%nk)     :: Eg          !Eg (nk,nk)                      ! Inverse operator:      [    Ng     inv_Eg  itI  ]   inv_ts=(it,itI)
      real(kind=cp), dimension(SpG%nk)      :: p_shift,nEg !rgI= xsi= ki (rmug + L) + p_shift   !                         [   0       0       1  ]
      real(kind=cp), dimension(3)           :: rmu, rmug   ! initial and final positions, homogeneous moment
      real(kind=cp), dimension(3)           :: m0          ! Auxiliary vectors
      real(kind=cp), dimension(3,SpG%nq)    :: kv       ! Auxiliary vectors
      real(kind=cp), dimension(SpG%nk)      :: tI          !tI=Hg.rmu + tgI
      complex(kind=cp)                      :: expi        !exp{2.pi.i [n](Hg.rmu + tgI)}
      complex(kind=cp), dimension(3,SpG%nq) :: Tmu,Dmu     ! 1/2{Mmucos + i Mmusin}
      complex(kind=cp), dimension(3)        :: Tv,Dv       !
      complex(kind=cp), dimension(3,SpG%nq) :: Tmug,Dmug   ! Transformed Fourier coefficients
      character(len=13) :: fm
      logical :: pr
      pr=.false.
      if(present(prn)) pr=prn
      d=Spg%d-1 !Dimension of the superspace
      ds=Spg%d  !Dimension of matrices
      p_shift=0.0
      if(present(phase_shift)) p_shift=phase_shift
      !Submatrices of gS-operator needed for calculations
      g(:,:) = SpG%Op(s)%Mat(1:3,1:3) !Rotation in External space acting on vector positions
      magm(:,:) = g(:,:)*SpG%Op(s)%time_inv*SpG%Op(s)%dt !Rotation in External space acting on axial vectors (moments)
      ts(:) = SpG%Op(s)%Mat(1:d,ds)    !Total translation (t,tgI)
      Eg(:,:) = SpG%Op(s)%Mat(4:d,4:d) !Matrix acting on modulation vectors: Sigma . g = Eg . Sigma + Hg
      Hg(:,:) = SpG%Op(s)%Mat(4:d,1:3) !Matrix of reciprocal lattice vectors resulting of the action of g on modulation vectors
      tgI(:)  = ts(4:d) !Internal translation of the current operator

      rmu=At%x                             !Average position of representant atom mu
      tI=matmul(Hg,rmu)+tgI                !tI=Hg.rmu + tgI
      rmug = matmul(g,rmu)+ts(1:3)+Lat     !Transformed position with translation t+Lat
      if(present(pos)) pos=rmug
      rgI  = matmul(rmug,SpG%kv) + p_shift ! (1,3) x (3,d) ->  d-vector
      if(pr) then
        write(*,"(a,3f12.5)") " Label and Position of the representant: "//trim(At%lab),rmu
        write(*,"(a,3f12.5)") " Final position after applying operator and translation: ",rmug
        write(*,"(a,6f12.5)") " Hg.rmu+tgI: ",tI
        write(*,"(a,6f12.5)") " rgI= rmug .kv =(x4,x5, ...xd): ",rgI
        write(*,"(a)")        " Propagation vectors"
        do i=1,SpG%nk
          write(*,"(a,i2,a,3f12.5,a)") " kv(",i,")=  [",Spg%kv(:,i),"]"
        end do
        write(*,"(a)")  " Representative Atom Modulations:"
        if(At%n_mc > 0) write(*,"(a)")  " ModF      MCos_x      MCos_y      MCos_z          MSin_x      MSin_y      MSin_z"
      end if

      Tmu=0.0_cp; Dmu=0.0_cp; Tmug=0.0_cp; Dmug=0.0_cp
      do j=1,At%n_mc
        k=At%pmc_q(j)  !Pointer to the Q_coeff
        Tmu(:,k)=0.5_cp*cmplx( At%Mcs(1:3,j),At%Mcs(4:6,j) )
        if(pr) write(*,"(i5,3f12.5,tr4,3f12.5)") k,2*real(Tmu(:,k)),2*aimag(Tmu(:,k))
      end do
      if(At%n_dc > 0 .and. pr) write(*,"(a)")  " ModF      DCos_x      DCos_y      DCos_z          DSin_x      DSin_y      DSin_z"
      do j=1,At%n_dc
        k=At%pdc_q(j)  !Pointer to the Q_coeff
        Dmu(:,k)=0.5_cp*cmplx( At%Dcs(1:3,j),At%Dcs(4:6,j) )
        if(pr) write(*,"(i5,3f12.5,tr4,3f12.5)") k,2*real(Dmu(:,k)),2*aimag(Dmu(:,k))
      end do
      n(:,:)= SpG%q_coeff(:,:)
      if(pr)  then
        write(*,"(a)") "  [n] : Q_coeff   kv=Sum{q_coeff*Kv}"
        fm="(  i4,3f12.5)"
        write(fm(2:3),"(i2)") SpG%nk
        kv=0.0
        do i=1,SpG%nq
          do j=1,SpG%nk
            kv(:,i)=kv(:,i)+SpG%q_coeff(j,i)*Spg%kv(:,j)
          end do
          write(unit=*,fmt=fm) n(:,i),kv(:,i)
        end do
        write(*,"(a,i4,a)") "  Matrix Eg for operator:", s, " -> "//trim(SpG%Symb_Op(s))
        do i=1,SpG%nk
          write(*,"(6i4)")  Eg(:,i)
        end do
        write(*,"(a)") "  Product [m]=[n] Eg:"
      end if
       ! Transformations of the Q-coeff under Eg
      do i=1,SpG%nq
        ng(:,i)=matmul(n(:,i),Eg)
        if(pr) write(*,"(6i4)")  ng(:,i)
      end do

      if(pr) then
         write(*,"(a)")  " Tranformed Atom Modulations:"
         if(At%n_mc > 0) write(*,"(a)")  " ModF      MCos_x      MCos_y      MCos_z          MSin_x      MSin_y      MSin_z"
      end if
      do i=1,At%n_mc
        k=At%pmc_q(i)  !Pointer to the Q_coeff
        expi=exp( cmplx(0_cp, 2.0*pi * dot_product(n(:,k),tI) ) )
        Tv=0.0_cp
        do j=1,SpG%nq
          if(equal_vector(ng(:,j),SpG%q_coeff(:,k))) then
            Tv= Tmu(:,k)
            exit
          end if
          if(equal_vector(ng(:,j),-SpG%q_coeff(:,k))) then
            Tv=conjg(Tmu(:,k))
            exit
          end if
        end do
        Tmug(:,k)=matmul(magm,Tv)*expi
        if(pr) write(*,"(i5,3f12.5,tr4,3f12.5)") i,2*real(Tmu(:,k)),2*aimag(Tmu(:,k))
      end do

      if(pr .and. At%n_dc > 0) write(*,"(a)")  " ModF      DCos_x      DCos_y      DCos_z          DSin_x      DSin_y      DSin_z"
      do i=1,At%n_dc
        k=At%pdc_q(i)  !Pointer to the Q_coeff
        expi=exp( cmplx(0_cp, 2.0*pi * dot_product(n(:,k),tI) ) )
        Dv=0.0_cp
        do j=1,SpG%nq
          if(equal_vector(ng(:,j),SpG%q_coeff(:,k))) then
            Dv= Dmu(:,k)
            exit
          end if
          if(equal_vector(ng(:,j),-SpG%q_coeff(:,k))) then
            Dv=conjg(Dmu(:,k))
            exit
          end if
        end do
        Dmug(:,k)=matmul(g,Dv)*expi
        if(pr) write(*,"(i5,3f12.5,tr4,3f12.5)") j,2*real(Dmu(:,k)),2*aimag(Dmu(:,k))
      end do


      !Fourier series for calculating the moments and displacements
      !  m(mug)= Sum(nq){ Tmug(nq) .exp [-2pii [nq]rgI] +   c.c. }
      m0=matmul(magm,At%moment)
      disp=0.0; moment=m0
      do j=1,SpG%nq !Moment and Displacement amplitudes
        xarg=2.0*pi*dot_product(SpG%q_coeff(:,j),rgI)
        expi=exp(cmplx(0.0_cp,-xarg))
        moment=moment + Tmug(:,j) * expi + conjg(Tmug(:,j)*expi)
        disp  =disp   + Dmug(:,j) * expi + conjg(Dmug(:,j)*expi)
      end do
      if(pr) then
        write(*,"(2(a,3f12.5))")  " Magnetic moment:",moment,"       Homogeneous moment:",m0
        write(*,"(2(a,3f12.5))")  "    Displacement:",disp,  "         Average position:",rmug
      end if

    End Subroutine Get_moment_and_displacement_ori

    Subroutine Get_ModAtoms_inBOX(A,SpG,TBOX,Ol)
      !---- Arguments ----!
      type(AtList_Type),       intent(in)  :: A
      class(SpG_Type),         intent(in)  :: spg
      integer, dimension(3),   intent(in)  :: TBOX
      type(orbit_list),        intent(out) :: Ol
      !---- Local variables ----!
      integer                                          :: i, j, k, L, m, s,nt,d, ds, i1,i2,i3,n_mO, &
                                                          s_inv,n_tr, n_orb
      real(kind=cp)                                    :: xarg
      real(kind=cp), dimension(3)                      :: xs,xsp,xst,v
      real(kind=cp), dimension(3)                      :: u_mu,m_mu, u_nu,m_nu
      real(kind=cp), dimension(3)                      :: ms,msp
      integer,      dimension(:,:),   allocatable      :: Lat,Lst !lattice translations
      real(kind=cp), dimension(:,:) ,  allocatable     :: orb,morb,orb3D !Temporal orbits
      real(kind=cp), dimension(:),     allocatable     :: rI, xI,Ls,tI  !rI(nk)   Internal space vector rI=(x4, x5, ... x3+d)
      real(kind=cp), dimension(SpG%d-1)     :: ts            ! ts=(t,tI)  Total translation in superspace
      integer, dimension(3)                 :: Lati          ![m].M  3-components vector      !                         [  g    0   t  ]
      integer, dimension(:),   allocatable  :: mE,pt         ![m].Eg  nk-vector               !   Superspace operator: [   Hg   Eg  tI  ]   ts=(t,tI)
      integer, dimension(:,:), allocatable  :: Hg            !M (nk,3)                        !                         [   0    0   1 ]
      integer, dimension(3,3)               :: g, magm       !g, magm= delta * det(g) * g
      integer, dimension(:,:), allocatable  :: Eg,inv_Eg     !Eg (nk,nk)

      ! Transformation Equation for general vector modulation function
      !
      !   p(nu)[ Hg . rE(mu)+Eg rI(mu) + tI] = g_a p(mu)[rI(mu)]
      !   p(nu)[ rI(nu) ] = g_a p(mu)[inv_Og rI(mu)] = g_a p(mu) [Ng (rE-t) + inv_Eg (rI(mu)-tI) ]
      !
      ! Where g_a= g for polar displacement modulations and g_a=magm for magnetic moment modulations
      !
      !   rI=(x4, x5, ... x3+d)
      !
      !   xj=qj.r(nu)    r(nu,L) = x(nu)+L    xjL= qj ( x(nu)+L )  j=4,...3+d
      !
      !  The general expression of the modulation functions in terms of Fourier series is:
      !
      !  p(nu)[rI(nu)]= g_a . Sum{m} [ Pc(mu)^([m]) cos[2pi [m](Eg^-1).(rI(mu)-Hg rE(m) - tI )] + Ps(mu)^([m]) sin[2pi [m](Eg^-1).(rI(mu)-Hg rE(m) - tI )] ]
      !
      !Calculation of the maximum number of additional lattice translations
      nt=TBOX(1)*TBOX(2)*TBOX(3)
      d=Spg%d-1 !Dimension of the superspace
      ds=Spg%d  !Dimension of matrices

      !Allocation of main arrays
      Select Type(SpG)
        type is(SuperSpaceGroup_Type)
          allocate(mE(SpG%nk),    Hg(SpG%nk,3),    Eg(SpG%nk,SpG%nk), rI(SpG%nk))
          allocate(inv_Eg(SpG%nk,SpG%nk),xI(SpG%nk),tI(SpG%nk))
      End Select
      Ol%num_orbs=A%natoms
      allocate(Ol%orbit(A%natoms))
      !Generate all integer translations (the centring translations are already included within the Spg%Multip operators
      allocate(Lat(3,nt)) !Allocate lattice translations in superspace
      nt=0
      do i1=0,TBOX(1)-1
        do i2=0,TBOX(2)-1
           do i3=0,TBOX(3)-1
              nt=nt+1
              Lat(:,nt) = real([i1,i2,i3],kind=cp)  !External space
           end do
        end do
      end do
      n_mO=2*Spg%multip*nt
      n_tr=nt

      !Generate the orbits of all atoms

      do i=1,A%natoms
        xs=0.0; ms=0.0
        xsp=0.0; msp=0.0
        if(allocated(orb)) deallocate(orb)
        if(allocated(orb3D)) deallocate(orb3D)
        if(allocated(morb)) deallocate(morb)
        if(allocated(pt)) deallocate(pt)
        if(allocated(Ls)) deallocate(Ls)
        if(allocated(Lst)) deallocate(Lst)
        allocate(orb(3,n_mO),morb(3,n_mO),pt(n_mO),Ls(n_mO),orb3D(3,n_mO),Lst(3,n_mO))
        orb=0.0; orb3D=0.0; Ls=0; pt=0
        morb=0.0
        pt=1
        Select Type(at => A%Atom(i))

          class is (ModAtm_Std_Type)

            Select type(SpG)

             Type is (SuperSpaceGroup_Type)
                !Add modulations to positions for the zero cell
                n_orb= 0
                xs   = At%x
                ms=At%Moment
                do_s: do s=1,SpG%Multip
                    s_inv=SpG%inv(s)
                    g(:,:)    = SpG%Op(s)%Mat(1:3,1:3)
                    magm(:,:) = g(:,:)*SpG%Op(s)%time_inv*SpG%Op(s)%dt
                                                     !                         [  g    0   t  ]
                   ts(:) = SpG%Op(s)%Mat(1:d,ds)     !   Superspace operator: [  Hg   Eg   tI  ]    ts=(t,tI)
                   Eg(:,:) = SpG%Op(s)%Mat(4:d,4:d)  !                         [  0    0   1  ]
                   Hg(:,:) = SpG%Op(s)%Mat(4:d,1:3)
                   inv_Eg(:,:) = SpG%Op(s_inv)%Mat(4:d,4:d)

                   xsp = matmul(g,xs)+ts(1:3)
                   call Lat_Modulo(xsp,v,lati)
                   xsp=v
                   msp = matmul(magm,ms)

                   !Applying translations
                   do_L:do L=1,n_tr
                      u_mu= 0.0; m_mu=0.0
                      xarg= 0.0
                      xst = xsp  + Lat(:,L)
                      do m=1,3
                        if(xst(m) < 0.0 .or. xst(m) > tbox(m)) cycle do_L
                      end do
                      do m=n_orb,1,-1
                        if(sum(abs(xst-orb3D(:,m))) < 0.002) cycle do_L
                      end do
                      n_orb = n_orb+1
                      orb3D(:,n_orb) = xst
                      Ls(n_orb) = L
                      Lst(:,n_orb) = Lat(:,L) + Lati
                      pt(n_orb) = s
                      xI  = matmul(xst,SpG%kv)-matmul(Hg,xs)-ts(4:d)
                      rI = matmul(inv_Eg, xI)
                      do j=1,At%n_dc !Displacement amplitudes, calculation of the Fourier series for translated atoms
                         xarg=2.0*pi*dot_product(SpG%q_coeff(:,j),rI)
                         u_mu(1:3)=u_mu(1:3)+at%Dcs(1:3,j)*cos(xarg) + at%Dcs(4:6,j)*sin(xarg)
                      end do
                      u_nu = matmul(g,u_mu)
                      orb(:,n_orb) = orb3D(:,n_orb) + u_nu
                      if(A%Atom(i)%Magnetic) then
                        xarg=0.0; m_mu=0.0
                        do j=1,At%n_mc !Moment amplitudes
                           xarg=2.0*pi*dot_product(SpG%q_coeff(:,j),rI)
                           m_mu(1:3)=m_mu(1:3)+at%Mcs(1:3,j)*cos(xarg) + at%Mcs(4:6,j)*sin(xarg)
                        end do
                        m_nu=matmul(magm,m_mu)
                        !Homogeneous moment + modulation
                        morb(:,n_orb) = msp + m_nu
                      end if
                   end do do_L
                end do do_s
                nt=n_orb
                allocate(Ol%orbit(i)%pos(3,nt),Ol%orbit(i)%mom(3,nt),Ol%orbit(i)%Ls(nt),Ol%orbit(i)%Latt(3,nt))
                allocate(Ol%orbit(i)%Lab(nt),Ol%orbit(i)%pts(nt))
                Ol%orbit(i)%ChemSymb=A%Atom(i)%ChemSymb
                Ol%orbit(i)%mult=nt
                do k=1,n_orb
                  Ol%orbit(i)%pos(:,k)=orb(:,k)
                  Ol%orbit(i)%mom(:,k)=morb(:,k)
                  Ol%orbit(i)%Ls(k)=Ls(k)
                  Ol%orbit(i)%pts(k)=pt(k)
                  Ol%orbit(i)%Latt(:,k)=Lst(:,k) !Lat(:,Ls(k))
                  write(unit=Ol%orbit(i)%lab(k),fmt="(a,i5)")  trim(A%Atom(i)%Lab)//"_",k
                  Ol%orbit(i)%lab(k)=pack_string(Ol%orbit(i)%lab(k))
                end do
            End select

        End Select

      end do !Atoms

    End Subroutine Get_ModAtoms_inBOX

 End Module Atoms_in_BOX
