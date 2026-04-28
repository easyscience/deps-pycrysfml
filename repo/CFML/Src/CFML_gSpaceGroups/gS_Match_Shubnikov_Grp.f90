!!----
!!----
!!----
!!----
SubModule (CFML_gSpaceGroups) gS_Match_Shubnikov_Grp
   implicit none
   Contains
    !!----
    !!---- MATCH_SHUBNIKOV_GROUP
    !!----
    !!---- Tries to match the space group G against Shubnikov groups. If the
    !!---- matching is successfull, it writes the group number G%numshu, the
    !!---- group symbol G%BNS_symb and the transformation matrix to the
    !!---- standard G%mat2std_shu
    !!----
    !!---- 24/04/2019 15:41:40
    !!
    Module Subroutine Match_Shubnikov_Group(G,P,M,database_path)
        !---- Arguments ----!
        type(spg_type),                   intent(in out):: G
        type(rational), dimension(3,3),   intent(in)    :: P        ! P matrix   -see Get_P_Matrix-
        type(rational), dimension(3,3),   intent(in)    :: M        ! M matrix   -see Get_M_Matrix-
        character(len=*), optional,       intent(in)    :: database_path

        !---- Local variables ----!
        integer                                                    :: i,j,k,n,ngen,ngen_,ni
        integer                                                    :: iniG,endG
        integer                                                    :: nPointOper,nA
        logical                                                    :: hexagonal,shift
        character(len=256)                                         :: symb
        type(symm_oper_type)                                       :: Op
        character(len=1),          dimension(2)                    :: magLat
        type(symm_oper_type),      dimension(3)                    :: Gt
        type(rational),            dimension(3)                    :: origShift
        type(rational),            dimension(4,4)                  :: Caux
        type(symm_oper_type),      dimension(3,6)                  :: Gx
        type(rational),            dimension(3,3,6)                :: A
        logical,                   dimension(:),       allocatable :: doTest
        character(len=80),         dimension(:),       allocatable :: gener
        type(spg_type),            dimension(:),       allocatable :: G_aux
        integer,                   dimension(:,:),     allocatable :: idx,pointerToOper
        type(rational),            dimension(:,:,:),   allocatable :: C,Cinv,pointOper,Paux

        logical :: pout

        !>===== DEBUG =====
        pout=.false.
        pout=(pout .or. CFML_DEBUG)
        !>=================

        !> Initialize variables
        hexagonal = .false.

        !> Check dimension of the group is four
        if (G%d /= 4) then
           Err_CFML%Ierr = 1
           Err_CFML%Msg  = "Match_Shubnikov_Group@SPACEG: Group dimension different from 4."
           return
        end if

        !> Load table for standard magnetic groups
        if(.not. Magnetic_DBase_allocated) then
          if (present(database_path)) then
            call Read_Magnetic_Data(database_path)
          else
            call Read_Magnetic_Data()
          end if
          if (Err_CFML%Ierr /=0) return
        end if

        !> Set the range of possible magnetic groups from the laue class
        select case (trim(G%laue))
           case ("-1")
              iniG  =    1
              endG  =    7
              if (pout) write(*,'(8x,a)') " => Matching representation against standard triclinic magnetic groups..."

           case ("2/m")
              iniG =    8
              endG  =   98
              if (pout) write(*,'(8x,a)') " => Matching representation against standard monoclinic magnetic groups..."

           case ("mmm")
              iniG =   99
              endG  =  660
              if (pout) write(*,'(8x,a)') " => Matching representation against standard orthorhombic magnetic groups..."

           case ("4/m","4/mmm")
              iniG =  661
              endG  = 1230
              if (pout) write(*,'(8x,a)') " => Matching representation against standard tetragonal magnetic groups..."

           case ("-3","-3 R","-3m","-3m R","-3m1","-31m")
              iniG = 1231
              endG  = 1338
              hexagonal = .true.
              if (pout) write(*,'(8x,a)') " => Matching representation against standard trigonal magnetic groups..."

           case ("6/m","6/mmm")
              iniG = 1339
              endG  = 1502
              hexagonal = .true.
              if (pout) write(*,'(8x,a)') " => Matching representation against standard hexagonal magnetic groups..."

           case ("m3","m-3","m3m","m-3m")
              iniG = 1503
              endG  = 1651
              if (pout) write(*,'(8x,a)') " => Matching representation against standard cubic magnetic groups..."

           case default
              Err_CFML%Ierr = 1
              Err_CFML%Msg  = "Match_Shubnikov_Group@SPACEG: Unknown Laue class"
              return
        end select

        !> Set pointers
        if (hexagonal) then
           ! Hexagonal systems
           allocate(pointOper(3,3,24))
           pointOper  = point_op_hex_matrix
           nPointOper = 24

        else
           ! Non hexagonal systems
           allocate(pointOper(3,3,48))
           pointOper  = point_op_matrix
           nPointOper = 48
        end if

        !> Get matrices needed for test different settings
        call Get_A_Matrix_Shub(G%laue,A,nA)

        !> Allocate memory
        allocate(G_aux(nA))
        allocate(doTest(nA))
        allocate(idx(3,nA))
        allocate(C(4,4,nA),Cinv(4,4,nA),Paux(3,3,nA))

        !> Initialize arrays
        do i=1,nA
           G_aux(i) = G
        end do
        doTest(:)          = .true.
        idx(:,:)           = 0

        !> Get transformation matrices for every setting we will test
        Caux(1:3,1:3) = matmul(P,M)
        do n = 1 , nA
           C(1:3,1:3,n) = matmul(Caux(1:3,1:3),A(1:3,1:3,n))
           C(1:3,4,n)   = [ 0,0,0 ]
           C(4,1:4,n)   = [ 0,0,0,1 ]
           Cinv(:,:,n)=Rational_Inverse_Matrix(C(:,:,n))
        end do

        !> Put G in the different settings
        allocate(gener(G%multip))

        !> Build groups from generators
        call allocate_op(G%d,Op)
        do n = 1 , nA
           do i = 1 , G%multip
              Op%Mat      = matmul(G%Op(i)%Mat,C(:,:,n))
              Op%Mat      = matmul(Cinv(:,:,n),Op%Mat)
              Op%time_inv = G%Op(i)%time_inv
              gener(i)=Get_Symb_from_Mat(Op%Mat,"xyz",Op%time_inv)
           end do
           call Group_Constructor(gener,G_aux(n),"xyz")
           G_aux(n)%laue = G%laue
           G_aux(n)%pg   = G%pg
        end do

        ni=maxval(G_aux(:)%multip)           !Allocation of pointerToOper moved here
        allocate(pointerToOper(2*ni,nA))
        pointerToOper(:,:) = 0
        !> Map each symmetry operation in pointerToOper for every possible setting
        do n = 1 , nA
           do i = 1 , G_aux(n)%multip
              j = 1
              do
                 if (Rational_Equal(G_aux(n)%Op(i)%Mat(1:3,1:3),pointOper(:,:,j))) then
                    pointerToOper(i,n) = j
                    exit
                 end if
                 j = j + 1
                 if (j > nPointOper) then
                    Err_CFML%Ierr = 1
                    Err_CFML%Msg  = "Match_Shubnikov_Group@SPACEG: " // &
                                    "A symmetry operation of the group cannot be matched against tabulated symmetry operations"
                    return
                 end if
              end do
           end do
        end do

        !> Get the magnetic lattice type for every setting
        do n = 1 , nA
           if (doTest(n)) then
              call Get_Magnetic_Lattice(G_aux(n))

              !> Correct symbol for triclinic systems with anti-translations
              if (trim(G_aux(n)%laue) == "-1" .and. G_aux(n)%shu_lat(2) /= " ") G_aux(n)%shu_lat(2) = "S"
           end if
        end do

        !> Get generators for every setting
        do n = 1 , nA
           if (doTest(n)) then
              call Get_Generators_L(G_aux(n)%laue,G_aux(n)%Multip,G_aux(n)%op,Gx(:,n),ngen)

              !> Map generators to G_aux(n)%Op
              do i = 1 , ngen
                 idx(i,n) = 0
                 do j = 1 , G_aux(n)%multip
                    if (G_aux(n)%Op(j) == Gx(i,n)) idx(i,n) = j
                 end do
                 if (idx(i,n) == 0) then
                    Err_CFML%Ierr = 1
                    Err_CFML%Msg  = "Match_Shubnikov_Group@SPACEG: Generator cannot be mapped."
                    return
                 end if
              end do
           end if
        end do

        !> Compute the P matrix for every setting
        do n = 1 , nA
           if (doTest(n)) then
              Paux(:,:,n)=Get_P_Matrix(G_aux(n))
              if (Err_CFML%Ierr /= 0) doTest(n) = .false.
           end if
        end do

        !> Try to match G against one of the Shubnikov groups
        do i = iniG , endG
            if (magtype(i) == G%mag_type .and. ops_count(i) == (G%multip/(G%num_lat+1))) then
                !> Extract magnetic lattice type from symbol
                magLat(1) = spacegroup_label_bns(i)(1:1)
                magLat(2) = " "
                if (spacegroup_label_bns(i)(2:2) == "_") magLat(2) = spacegroup_label_bns(i)(3:3)
                do n = 1 , nA
                    if (doTest(n)) then
                        if (G_aux(n)%shu_lat(1) /= magLat(1) .or. &
                            G_aux(n)%shu_lat(2) /= magLat(2)) cycle
                        ! Try to find the same set of generators in the standard magnetic group
                        ngen_ = 0
                        do j = 1 , ngen
                            do k = 1 , ops_count(i)
                                if (ops_bns_point_op(k,i) == pointerToOper(idx(j,n),n) .and. &
                                    ops_bns_timeinv(k,i) == Gx(j,n)%time_inv) then
                                    Gt(j) = Gx(j,n)
                                    Gt(j)%Mat(1:3,4) = (ops_bns_trans(1:3,k,i) // ops_bns_trans_denom(k,i))
                                    ngen_ = ngen_ + 1
                                    exit
                                end if
                            end do
                        end do
                        if (ngen_ == ngen) then
                            if (pout) write(*,'(12x,3a,i1)',advance = 'no') "Trying to match magnetic space group ",&
                                trim(spacegroup_label_bns(i)),", setting ", n
                            call Get_Origin_Shift(nGen,Gx(1:nGen,n),Gt(1:nGen),Paux(1:3,1:3,n),origShift,shift)
                            if (shift) then
                                if (pout) write(*,'(8x,a)') "Done!"
                                origShift(1:3) = matmul(C(1:3,1:3,n),origShift)
                                C(4,1:3,n) = origShift(1:3)
                                symb=Get_Symb_from_Mat(transpose(C(:,:,n)),"abc")
                                if (pout) then
                                    write(*,'(12x,a)',advance='no') "Original setting --> Standard magnetic setting: "
                                    write(*,'(a)') trim(symb)
                                end if
                                G%BNS_symb    = spacegroup_label_bns(i)
                                G%OG_symb     = spacegroup_label_og(i)
                                G%BNS_num     = nlabel_bns(i)
                                G%OG_num      = nlabel_og(i)
                                G%numshu      = i
                                G%mat2std_shu = trim(symb)
                                G%matfrom     = inverse_setting(symb,4)
                                call set_shubnikov_info()
                                j=Litvin2IT(i)
                                G%UNI         = shubnikov_info(j)%STD
                                write(G%UNI_num,"(i4)")  j
                                return
                            else
                                if (pout) write(*,'(8x,a)') "Failed"
                            end if
                        end if
                    end if
                end do
            end if
        end do

        Err_CFML%Ierr = 1
        Err_CFML%Msg  = "Match_Shubnikov_Group@SPACEG: Unable to indentify the magnetic group"

    End Subroutine Match_Shubnikov_Group

End SubModule gS_Match_Shubnikov_Grp