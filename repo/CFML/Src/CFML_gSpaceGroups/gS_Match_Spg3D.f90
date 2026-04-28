!!----
!!----
!!----
!!----
SubModule (CFML_gSpaceGroups) gS_Match_SpG3D
   implicit none
   Contains
   !!----
   !!---- MATCH_SPACEGROUP_3D
   !!----    Tries to match the space group G against one of the 230
   !!----    standard crystallographic groups. It returns the space
   !!----    group number G%numspg, the space group symbol G%BNS_symb
   !!----    and the transformation matrix to the standard G%mat2std
   !!----
   !!---- 14/05/2019
   !!
   Module Subroutine Match_SpaceGroup_3D(G,P,M,n,A)
       !---- Arguments ----!
       type(spg_type),                  intent(in out) :: G        ! space group in the original setting
       type(rational), dimension(3,3),   intent(in)    :: P        ! P matrix   -see Get_P_Matrix-
       type(rational), dimension(3,3),   intent(in)    :: M        ! M matrix   -see Get_M_Matrix-
       integer,                          intent(in)    :: n        ! number of A matrices (six as maximum)
       type(rational), dimension(3,3,n), intent(in)    :: A        ! A matrices -see Get_A_Matrices_Crys-

       !---- Local variables ----!
       integer                                         :: s,i,j,ng,ng_,n_gen,d,ier
       character(len=4)                                :: str
       character(len=12)                               :: str_HM
       character(len=256)                              :: symb, glist
       character(len=80),    dimension(:), allocatable :: l_gen
       logical                                         :: shift
       type(rational),       dimension(3)              :: origShift
       type(rational),       dimension(3,3)            :: MA,P_target
       type(rational),       dimension(4,4)            :: C,Cinv,W
       type(rational),       dimension(4,4,n)          :: C_
       type(Symm_Oper_Type), dimension(3)              :: gen_std,gen_x
       type(spg_type)                                  :: G_target
       type(spg_type)                                  :: G_std
       type(spg_type),       dimension(n)              :: G_

       logical :: pout =.false.

       !> Init
       do i=1, 3
          allocate(gen_std(i)%Mat(4,4),gen_x(i)%Mat(4,4))
       end do

       if (pout) then
          print*,' '
          print*," Match-SpaceGroup Procedure..."
          print*," -> N: ",n
          print*,"Num Lat:", G%Num_lat
       end if

       do s=1, n  ! Loop over C matrices
          G_(s)         = G
          G_(s)%numops  = G%multip / (G%num_lat + G%num_alat + 1)
          if (G_(s)%centred == 1) G_(s)%centred = G_(s)%anticentred
          if (G_(s)%centred /= 1) G_(s)%numops  = G_(s)%numops / 2
          MA            = matmul(M,A(:,:,s))
          C_(1:3,1:3,s) = matmul(P,MA)
          C_(1:3,4,s)   = [ 0//1,0//1,0//1 ]
          C_(4,1:4,s)   = [ 0//1,0//1,0//1,1//1 ]
          Cinv= Rational_Inverse_Matrix(C_(:,:,s))

          !> Compute symmetry operations in the new basis
          do j = 1 , G%Multip
             W(1:3,1:3) = G%op(j)%Mat(1:3,1:3)
             W(1:3,4)   = G%op(j)%Mat(1:3,G%d)
             W(4,1:4)   = [ 0//1,0//1,0//1,1//1 ]
             W          = matmul(W,C_(:,:,s))
             W          = matmul(Cinv,W)
             G_(s)%op(j)%Mat(1:3,1:3) = W(1:3,1:3)
             G_(s)%op(j)%Mat(1:3,G%d) = W(1:3,4)
          end do

          !> Compute vectors in the new basis
          do j = 1 , G%num_lat
             G_(s)%lat_tr(1:3,j) = matmul(Cinv(1:3,1:3),G%lat_tr(1:3,j))
             G_(s)%lat_tr(1:3,j) = rational_modulo_lat(G_(s)%lat_tr(1:3,j))
             !call PBC(G_(s)%lat_tr(1:3,j))
          end do
          do j = 1 , G%num_alat
             G_(s)%alat_tr(1:3,j) = matmul(Cinv(1:3,1:3),G%alat_tr(1:3,j))
             G_(s)%alat_tr(1:3,j) = rational_modulo_lat(G_(s)%alat_tr(1:3,j))
             !call PBC(G_(s)%alat_tr(1:3,j))
          end do

          !> Get Lattice Type
          G_(s)%spg_lat=Get_Lattice_Type(MA)
       end do

       if (G%Numspg <=0) then
          Err_CFML%Ierr = 1
          Err_CFML%Msg ="Match_SpaceGroup_3D@GSPACEGROUPS: Zero standard Space group!"
          return
       end if

       write(unit=str, fmt='(i3)', iostat=ier) G%numspg
       if (ier /= 0) then
          Err_CFML%Ierr = 1
          Err_CFML%Msg ="Match_SpaceGroup_3D@GSPACEGROUPS: Error taken standard spacegroup!"
          return
       end if

       gList=get_IT_Generators(str)  ! IT take our default choice for SpaceGroup
       call Get_Generators(gList, d, l_gen, n_gen)
       call Group_Constructor(l_gen,G_std)
       call Identify_PointGroup(G_std)
       call Identify_LaueClass(G_std)
       call Get_SpaceGroup_Symbols(Str, str_HM)
       str_HM=adjustl(str_HM)
       G_std%spg_lat=str_HM(1:1)
       G_std%spg_symb=str_HM(1:1)//l_case(str_HM(2:))
       G%spg_lat=G_std%spg_lat
       if(len_trim(G_std%BNS_symb) == 0) then
         G%BNS_symb=G_std%spg_symb
       else
         G%BNS_symb=G_std%BNS_symb
       end if
       if (Err_CFML%Ierr /= 0) return

       do s=1, n
          if (pout) then
             print*,' '
             print*,'S:',s
             print*,'Numops:', G_std%NumOps, '  ',G_(s)%NumOps
             print*,'Latt', G_std%spg_Lat,'  ', G_(s)%Spg_Lat
             print*,'Latt', G_std%Centred,'  ', G_(s)%Centred
          end if
          if (G_std%NumOps  == G_(s)%NumOps .and. &
              G_std%Spg_Lat == G_(s)%Spg_Lat .and. &
             (G_std%Centred == 1 .and. G_(s)%Centred == 1 .or. &
              G_std%Centred /= 1 .and. G_(s)%Centred /= 1)) then

             !> Get generators from the standard space group
             call Get_Generators_L(G_std%laue,G_std%Multip,G_std%op,gen_std,ng)
             if (Err_CFML%Ierr /= 0) return

             !> Try to get these generators from SGaux(s)
             ng_ = 0
             do i=1, ng
                do j=1, G_(s)%Multip
                   if (Rational_Equal(gen_std(i)%Mat(1:3,1:3),G_(s)%op(j)%Mat(1:3,1:3))) then
                      ng_ = ng_ + 1
                      gen_x(ng_) = G_(s)%op(j)
                      exit
                   end if
                end do
             end do
             if (ng /= ng_) cycle

             G_target%num_alat = 0
             G_target%num_lat  = G_std%Num_Lat
             G_target%Multip   = G_std%Multip


             if (allocated(G_target%lat_tr)) deallocate(G_target%lat_tr)
             if (G_target%num_lat > 0) then
                allocate(G_target%lat_tr(3,G_target%num_lat))
                G_target%lat_tr(:,:) = G_std%lat_tr(:,:)
             end if
             if (allocated(G_target%op)) deallocate(G_target%op)
             allocate(G_target%op(G_std%Multip))
             do i=1, G_std%Multip
                allocate (G_target%op(i)%Mat(3,3))
                G_target%op(i)%Mat = G_std%op(i)%Mat(1:3,1:3)
             end do
             P_target(1:3,1:3)=Get_P_Matrix(G_target)
             if (Err_CFML%Ierr /= 0) return

             !> Try to match the standard by an origin shift
             call Get_Origin_Shift(ng,gen_x(1:ng),gen_std(1:ng),P_target,origShift,shift)
             if (shift) then
                C(1:3,1:3) = C_(1:3,1:3,s)
                C(4,:)     = C_(4,:,s)
                origShift  = matmul(C(1:3,1:3),origShift)
                C(4,1:3)   = rational_modulo_lat(origShift(1:3))
                symb=Get_Symb_from_Mat(transpose(C),"abc")
                G%mat2std  = trim(symb)
                return
             else
                cycle
             end if
          end if
       end do

       Err_CFML%Ierr = 1
       Err_CFML%Msg ="Match_SpaceGroup_3D@GSPACEGROUPS: Imposible to obtain the Shift!"

   End subroutine Match_SpaceGroup_3D

End SubModule gS_Match_SpG3D