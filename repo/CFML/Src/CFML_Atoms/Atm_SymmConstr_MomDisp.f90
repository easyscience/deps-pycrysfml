
Submodule (CFML_Atoms) Atm_SymmConstr_MomDisp
  implicit none

  contains
     !!----
     !!---- Check_Symmetry_Constraints
     !!----
     !!---- 26/06/2020
     !!
     Module Subroutine Check_Symmetry_Constraints(SpG,Atm)
       class(SpG_Type),   intent(in)     :: SpG
       type(AtList_Type), intent(in out) :: Atm

       !--- Local variables ---!
       integer :: i,codini
       integer,       dimension(3)   :: Icodes
       real(kind=cp), dimension(3)   :: multip
       real(kind=cp), dimension(6,8) :: codeT

       codini=1
       do i=1,Atm%natoms
          if(Atm%Atom(i)%Magnetic) then
            Icodes=1
            call Get_moment_ctr(Atm%Atom(i)%x,Atm%Atom(i)%moment,SpG,codini,Icodes,Multip)
          end if
       end do

       Select Type (SpG)
          type is (SuperSpaceGroup_Type)
             do i=1,Atm%natoms
                Select Type(at => Atm%Atom(i))
                   class is (ModAtm_Std_Type)
                       if (at%n_mc > 0) then
                          codeT=1.0
                          call Get_TFourier_ctr(at%x,at%Mcs(:,1:at%n_mc),codeT(:,1:at%n_mc),SpG,codini,"M")
                       end if

                       if (at%n_dc > 0) then
                          codeT=1.0
                          call Get_TFourier_ctr(at%x,at%Dcs(:,1:at%n_dc),codeT(:,1:at%n_dc),SpG,codini,"D")
                       end if
                End Select
            end do
       End Select
       Atm%symm_checked=.true.
     End Subroutine Check_Symmetry_Constraints

     Module Subroutine Get_moment_and_displacement_Lab(AtL,SpG,atm_lab,num_op,R_latt,pos,Disp,Moment,phase_shift)
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
                if(CFML_debug) then
                   call Get_moment_and_displacement(At,SpG,num_op,R_latt,disp,moment,pos,phase_shift,.true.)
                else
                   call Get_moment_and_displacement(At,SpG,num_op,R_latt,disp,moment,pos,phase_shift)
                end if
             else
                if(CFML_debug) then
                   call Get_moment_and_displacement(At,SpG,num_op,R_latt,disp,moment,pos,prn=.true.)
                else
                   call Get_moment_and_displacement(At,SpG,num_op,R_latt,disp,moment,pos)
                end if
             end if
         End Select
       else
            Err_CFML%Ierr = 1
            Err_CFML%flag = .true.
            write(unit=Err_CFML%Msg,fmt="(a)") "  Atom: "//trim(atm_lab)//"  not found! (Subroutine:Get_moment_and_displacement_SSG) "
       end if
     End Subroutine Get_moment_and_displacement_Lab

     Module Subroutine Get_moment_and_displacement(At,SpG,s,Lat,disp,moment,pos,phase_shift,prn)
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
       integer                            :: i, j, k, d, ds
                                                                                             !                         [  g    0   t  ]
       integer, dimension(SpG%nk,SpG%nq)     :: n,ng        ![n].Eg  nk-vectors              !   Superspace operator: [  Hg   Eg  tgI  ]   ts=(t,tgI)
       integer, dimension(SpG%nk,3)          :: Hg          !Hg(nk,3)                        !                         [  0    0   1  ]
       integer, dimension(3,3)               :: g, magm     !g, magm= delta * det(g) * g     !                         [  inv_g    0      it  ]
       integer, dimension(SpG%nk,SpG%nk)     :: Eg          !Eg (nk,nk)                      ! Inverse operator:      [    Ng     inv_Eg  itI  ]   inv_ts=(it,itI)
       real(kind=cp), dimension(SpG%nk)      :: p_shift     !rgI= xsi= ki (rmug + L) + p_shift   !                         [   0       0       1  ]
       real(kind=cp), dimension(3)           :: rmu, rmug   ! initial and final positions, homogeneous moment
       real(kind=cp), dimension(3)           :: m0          ! Auxiliary vectors
       real(kind=cp), dimension(3,SpG%nq)    :: kv          ! Auxiliary vectors
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
       ts(:)   = SpG%Op(s)%Mat(1:d,ds)  !Superspace translation ts=(t,tgI) of the operator
       Eg(:,:) = SpG%Op(s)%Mat(4:d,4:d) !Matrix acting on modulation vectors: Sigma . g = Eg . Sigma + Hg
       Hg(:,:) = SpG%Op(s)%Mat(4:d,1:3) !Matrix of reciprocal lattice vectors resulting of the action of g on modulation vectors
       tgI(:)  = ts(4:d)                !Internal translation of the current operator
       rmu=At%x                         !Average position of representant atom mu
       tI=matmul(Hg,rmu)+tgI            !tI=Hg.rmu + tgI
       rmug = matmul(g,rmu)+ts(1:3)+Lat !Transformed position with translation t+Lat
       if(present(pos)) pos=rmug
       rgI  = matmul(rmug,SpG%kv) + p_shift ! (1,3) x (3,d) ->  d-vector
       if(pr) then
         write(*,"(a,3f12.5)") " Label and Position of the representant: "//trim(At%lab),rmu
         write(*,"(a,3f12.5)") " Final position after applying operator and translation: ",rmug
         write(*,"(a,6f12.5)") " Hg.rmu+tgI: ",tI
         write(*,"(a,6f12.5)") " rgI= rmug .kv =(x4,x5, ...xd): ",rgI
         write(*,"(a)")        " Basic Propagation vectors"
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
         do i=1,SpG%nq
           kv=0.0
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
       m0=matmul(magm,At%moment)
       moment=m0
       do i=1,At%n_mc
         k=At%pmc_q(i)  !Pointer to the Q_coeff
         expi=exp( cmplx(0_cp, 2.0*pi * dot_product(n(:,k),tI) ) )
         Tv=0.0_cp
         do j=1,SpG%nq
           if(equal_vector(ng(:,k),SpG%q_coeff(:,j))) then
             Tv= Tmu(:,j)
             exit
           end if
           if(equal_vector(ng(:,k),-SpG%q_coeff(:,j))) then
             Tv=conjg(Tmu(:,j))
             exit
           end if
         end do
         Tmug(:,k)=matmul(magm,Tv)*expi
         if(pr) write(*,"(i5,3f12.5,tr4,3f12.5)") i,2*real(Tmug(:,k)),2*aimag(Tmug(:,k))
         !Fourier series for calculating the moment
         xarg=2.0*pi*dot_product(n(:,k),rgI)
         expi=exp(cmplx(0.0_cp,-xarg))
         moment = moment + Tmug(:,k) * expi + conjg(Tmug(:,k)*expi)
       end do

       if(pr .and. At%n_dc > 0) write(*,"(a)")  " ModF      DCos_x      DCos_y      DCos_z          DSin_x      DSin_y      DSin_z"
       disp=0.0
       do i=1,At%n_dc
         k=At%pdc_q(i)  !Pointer to the Q_coeff
         expi=exp( cmplx(0_cp, 2.0*pi * dot_product(n(:,k),tI) ) )
         Dv=0.0_cp
         do j=1,SpG%nq
           if(equal_vector(ng(:,k),SpG%q_coeff(:,j))) then
             Dv= Dmu(:,j)
             exit
           end if
           if(equal_vector(ng(:,k),-SpG%q_coeff(:,j))) then
             Dv=conjg(Dmu(:,j))
             exit
           end if
         end do
         Dmug(:,k)=matmul(g,Dv)*expi
         if(pr) write(*,"(i5,3f12.5,tr4,3f12.5)") j,2*real(Dmug(:,k)),2*aimag(Dmug(:,k))
         !Fourier series for calculating the displacement
         xarg=2.0*pi*dot_product(n(:,k),rgI)
         expi=exp(cmplx(0.0_cp,-xarg))
         disp = disp + Dmug(:,k) * expi + conjg(Dmug(:,k)*expi)
       end do

       if(pr) then
         write(*,"(2(a,3f12.5))")  " Magnetic moment:",moment,"       Homogeneous moment:",m0
         write(*,"(2(a,3f12.5))")  "    Displacement:",disp,  "         Average position:",rmug
       end if

     End Subroutine Get_moment_and_displacement

     Module Function Get_Transf_Tn(At,SpG,s,mode) result(Tns)
       type(ModAtm_Std_Type),        intent(in) :: At     !Modulated atom type
       type(SuperSpaceGroup_type),   intent(in) :: SpG
       integer,                      intent(in) :: s      !Pointer to the symmetry operator relating x and x0: x= g(s) x0 + t(s) + Lat
       character(len=*),             intent(in) :: mode
       complex(kind=cp),dimension(3,SpG%nq)     :: Tns

       !Local variables
       real(kind=cp), dimension(SpG%nk)   :: tgI    ! Internal transformed space vector from rI=(x4, x5, ... x3+d) at new position
       real(kind=cp), dimension(SpG%d-1)  :: ts     ! ts=(t,tgI) Total translation in superspace operator
       integer                            :: i, j, k, d, ds
                                                                                             !                         [  g    0   t  ]
       integer, dimension(SpG%nk,SpG%nq)     :: n,ng        ![n].Eg  nk-vectors              !   Superspace operator: [  Hg   Eg  tgI  ]   ts=(t,tgI)
       integer, dimension(SpG%nk,3)          :: Hg          !Hg(nk,3)                        !                         [  0    0   1  ]
       integer, dimension(3,3)               :: g, magm     !g, magm= delta * det(g) * g     !                         [  inv_g    0      it  ]
       integer, dimension(SpG%nk,SpG%nk)     :: Eg          !Eg (nk,nk)                      ! Inverse operator:      [    Ng     inv_Eg  itI  ]   inv_ts=(it,itI)
       real(kind=cp), dimension(3)           :: rmu         ! initial and final positions, homogeneous moment
       real(kind=cp), dimension(SpG%nk)      :: tI          !tI=Hg.rmu + tgI
       complex(kind=cp)                      :: expi        !exp{2.pi.i [n](Hg.rmu + tgI)}
       complex(kind=cp), dimension(3)        :: Tv          !
       complex(kind=cp),dimension(3,SpG%nq)  :: Tmu

       d=Spg%d-1 !Dimension of the superspace
       ds=Spg%d  !Dimension of matrices
       !Submatrices of gS-operator needed for calculations
          g(:,:) = SpG%Op(s)%Mat(1:3,1:3) !Rotation in External space acting on vector positions
       magm(:,:) = g(:,:)*SpG%Op(s)%time_inv*SpG%Op(s)%dt !Rotation in External space acting on axial vectors (moments)
         ts(:)   = SpG%Op(s)%Mat(1:d,ds)     !Superspace translation ts=(t,tgI) of the operator
         Eg(:,:) = SpG%Op(s)%Mat(4:d,4:d)    !Matrix acting on modulation vectors: Sigma . g = Eg . Sigma + Hg
         Hg(:,:) = SpG%Op(s)%Mat(4:d,1:3)    !Matrix of reciprocal lattice vectors resulting of the action of g on modulation vectors
         tgI(:)  = ts(4:d)                   !Internal translation of the current operator
           rmu   = At%x                      !Average position of representant atom mu
            tI   = matmul(Hg,rmu)+tgI        !tI=Hg.rmu + tgI
         n(:,:)  = SpG%q_coeff(:,:)
        ! Transformations of the Q-coeff under Eg
       do i=1,SpG%nq
         ng(:,i)=matmul(n(:,i),Eg)
       end do

       Tns=0.0_cp

       Select Case (mode(1:1))

         Case("M","m")  !Magnetic modulation functions

            do j=1,At%n_mc   !Construction of all possible Tmu
              k=At%pmc_q(j)  !Pointer to the Q_coeff
              Tmu(:,k)=0.5_cp*cmplx( At%Mcs(1:3,j),At%Mcs(4:6,j) )
            end do
            do i=1,At%n_mc
              k=At%pmc_q(i)
              expi=exp( cmplx(0_cp, 2.0*pi * dot_product(n(:,k),tI) ) )
              Tv=0.0_cp
              do j=1,SpG%nq
                if(equal_vector(ng(:,k),SpG%q_coeff(:,j))) then
                  Tv= Tmu(:,j)
                  exit
                end if
                if(equal_vector(ng(:,k),-SpG%q_coeff(:,j))) then
                  Tv=conjg(Tmu(:,j))
                  exit
                end if
              end do
              Tns(:,k)=matmul(magm,Tv)*expi
            end do

         Case("D","d")

            do j=1,At%n_dc   !Construction of all possible Tmu
              k=At%pdc_q(j)  !Pointer to the Q_coeff
              Tmu(:,k)=0.5_cp*cmplx( At%Dcs(1:3,j),At%Dcs(4:6,j) )
            end do
            do i=1,At%n_dc
              k=At%pdc_q(i)
              expi=exp( cmplx(0_cp, 2.0*pi * dot_product(n(:,k),tI) ) )
              Tv=0.0_cp
              do j=1,SpG%nq
                if(equal_vector(ng(:,k),SpG%q_coeff(:,j))) then
                  Tv= Tmu(:,j)
                  exit
                end if
                if(equal_vector(ng(:,k),-SpG%q_coeff(:,j))) then
                  Tv=conjg(Tmu(:,j))
                  exit
                end if
              end do
              Tns(:,k)=matmul(g,Tv)*expi
            end do

       End Select

     End Function Get_Transf_Tn

End Submodule Atm_SymmConstr_MomDisp
