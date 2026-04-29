SubModule (CFML_kvec_Symmetry) ksym_suscept
   implicit none
   Contains

    !!---- Subroutine Calc_Induced_Sk(cell,SpG,MField,dir_MField,Atm)
    !!----    !---- Arguments ----!
    !!----   type(Cell_G_Type),          intent(in)     :: Cell
    !!----   type(SPG_Type),             intent(in)     :: SpG
    !!----   real(kind=cp),              intent(in)     :: MField
    !!----   real(kind=cp),dimension(3), intent(in)     :: dir_MField
    !!----   type(Matom_list_type),    intent(in out)   :: Atm
    !!----
    !!----  This subroutine completes the object Am by calculating the
    !!----  induced magnetic moments of the representant atoms in the asymmetric unit.
    !!----  It modifies also the Chi tensor according to the symmetry constraints of
    !!----  the crystallographic site.
    !!----
    !!----  Created: June 2014 (JRC)
    !!----
    Module Subroutine Calc_Induced_Sk(cell,SpG,MField,dir_MField,Atm,ipr)
       !---- Arguments ----!
       type(Cell_G_Type),          intent(in)     :: Cell
       type(SPG_Type),             intent(in)     :: SpG
       real(kind=cp),              intent(in)     :: MField
       real(kind=cp),dimension(3), intent(in)     :: dir_MField
       type(Matom_list_type),    intent(in out)   :: Atm
       integer, optional,          intent(in)     :: ipr
       !--- Local variables ---!
       integer                          :: i,codini
       integer, dimension(6)            :: icodes
       real(kind=cp),    dimension(6)   :: multip
       real(kind=cp),    dimension(3)   :: u_vect,x
       real(kind=cp),    dimension(3,3) :: chi

       !
       u_vect=MField * dir_MField / Veclength(Cell%Cr_Orth_cel,dir_MField)
       if(present(ipr)) write(unit=ipr,fmt="(a,3f8.4)") " => Applied Magnetic Field: ",u_vect
       icodes=(/1,2,3,4,5,6/); multip=(/1.0,1.0,1.0,1.0,1.0,1.0/)
       codini=1
       do i=1,Atm%natoms
          x=atm%atom(i)%x
          call Get_Atom_2nd_Tensor_Ctr(x,atm%atom(i)%chi,SpG,Codini,Icodes,Multip)
          chi=reshape((/atm%atom(i)%chi(1),atm%atom(i)%chi(4), atm%atom(i)%chi(5), &
                        atm%atom(i)%chi(4),atm%atom(i)%chi(2), atm%atom(i)%chi(6), &
                        atm%atom(i)%chi(6),atm%atom(i)%chi(6), atm%atom(i)%chi(3) /),(/3,3/))
          Atm%atom(i)%SkR(:,1)=matmul(Chi,u_vect)
          Atm%atom(i)%SkI(:,1)=0.0
          if(present(ipr)) then
             write(unit=ipr,fmt="(a,i3,a,6f8.4)")     " Atom # ",i," Chi      values: ",atm%atom(i)%chi
             write(unit=ipr,fmt="(a,6i4,6f6.2)")      "            Chi constraints: ",Icodes,multip
             write(unit=ipr,fmt="(a,3f8.4)")          "            Induced  Moment: ",Atm%atom(i)%SkR(:,1)
          end if
       end do ! Atoms
    End Subroutine Calc_Induced_Sk

    !!----
    !!----  Module Subroutine Get_Atom_2nd_Tensor_Ctr(x,TensVal,Spgr,Codini,Icodes,Multip,Ord,Ss,Ipr)
    !!----     real(kind=cp), dimension(3),             intent(in    ) :: x         !Atom position (fractional coordinates)
    !!----     real(kind=cp), dimension(6),             intent(in out) :: TensVal   !Second order symmetric tensor
    !!----     type(SPG_type),                          intent(ix    ) :: Spgr      !Space Group
    !!----     Integer,                                 intent(in out) :: Codini    !Last attributed parameter
    !!----     Integer, dimension(6),                   intent(in out) :: Icodes    !codewords for TensVal only number
    !!----     real(kind=cp), dimension(6),             intent(in out) :: Multip    !Multipliers
    !!----     integer,                       optional, intent(in    ) :: Ord       !Order of the stabilizer
    !!----     integer, dimension(:),         optional, intent(in    ) :: Ss        !Pointer to SymmOp. of stabilizer
    !!----     integer,                       optional, intent(in    ) :: Ipr       !Printing unit for debug
    !!----
    !!----  Subroutine to get the appropriate constraints in the refinement codes of
    !!----  second order symmetric tensor atomic property parameters.
    !!----  New algorithm based in the Wigner theorem.
    !!----  The matrix Bet = Sum { R Beta RT} displays the symmetry constraints to be
    !!----  applied to the 2nd order symmetric tensor components. The sum runs over all rotational
    !!----  symmetry operators of the stabilizer of the particular atom position in the given
    !!----  space group.
    !!----  There are a total of 29 kind of relations that may appear in the Bet matrix:
    !!----
    !!----     1    A A A 0   0   0  -> m-3m, -43m, 432, m-3,23, 3[111].2[001]
    !!----     2    A A C 0   0   0  -> 4/mmm, -42m, 4mm, 422, 4/m, -4,4, 4[001]
    !!----     3    A B A 0   0   0  -> 4[010]
    !!----     4    A B B 0   0   0  -> 4[100]
    !!----     5    A A A D   D   D  -> -3m, 3m, 32, -3, 3   3[111]
    !!----     6    A A A D  -D  -D  -> 3[11-1]
    !!----     7    A A A D  -D   D  -> 3[1-11]
    !!----     8    A A A D   D  -D  -> 3[-111]
    !!----     9    A A C A/2 0   0  -> 6/mmm, -6m2, 6mm, 622, 6/m, 6,-6,-3m, 32,-3, 3:  h 3[001]
    !!----    10    A B C 0   0   0  -> mmm, mm2, 222  2[001] 2[100]
    !!----    11    A A C D   0   0  -> 2[001], 2[110]    w
    !!----    12    A B A 0   E   0  -> 2[010], 2[101]
    !!----    13    A B B 0   0   F  -> 2[100], 2[011]
    !!----    14    A B C B/2 0   0  -> 2[001], 2[100]    h
    !!----    15    A B C A/2 0   0  -> 2[001], 2[010]    h
    !!----    16    A B C D   0   0  -> 2/m, m, 2: 2[001] w
    !!----    17    A B C 0   E   0  -> 2[010]
    !!----    18    A B C 0   0   F  -> 2[100]
    !!----    19    A A C D   E  -E  -> 2[110]            w
    !!----    20    A A C D   E   E  -> 2[1-10]           w
    !!----    21    A B A D   E  -D  -> 2[101]
    !!----    22    A B A D   E   D  -> 2[10-1]
    !!----    23    A B B D  -D   F  -> 2[011]
    !!----    24    A B B D   D   F  -> 2[01-1]
    !!----    25    A B C B/2 F/2 F  -> 2[100]            h
    !!----    26    A B C A/2 0   F  -> 2[210]            h
    !!----    27    A B C B/2 E   0  -> 2[120]            h
    !!----    28    A B C A/2 E   E/2-> 2[010]            h
    !!----    29    A B C D   E   F  -> 1, -1
    !!----
    !!----   Updated: 27 June 2014 (JRC)
    !!----
    Module Subroutine Get_Atom_2nd_Tensor_Ctr(x,TensVal,Spgr,Codini,Icodes,Multip,Ord,Ss,Ipr)
       real(kind=cp), dimension(3),             intent(in    ) :: x
       real(kind=cp), dimension(6),             intent(in out) :: TensVal
       type(SPG_type),                          intent(in    ) :: Spgr
       Integer,                                 intent(in out) :: Codini
       Integer, dimension(6),                   intent(in out) :: Icodes
       real(kind=cp), dimension(6),             intent(in out) :: Multip
       integer,                       optional, intent(in    ) :: Ord
       integer, dimension(:),         optional, intent(in    ) :: Ss
       integer,                       optional, intent(in    ) :: Ipr

       ! Local variables
       character (len=1), dimension(6)   :: cdd
       integer                           :: i,j,order
       integer,           dimension(48)  :: ss_ptr
       integer,           dimension(6)   :: codd
       integer,           dimension(3,3) :: Rsym
       real(kind=cp),     dimension(3,3) :: bet,bett,Rs
       real(kind=cp),     dimension(6)   :: cod
       real(kind=cp),     dimension(3,48):: atr
       real(kind=cp),     parameter      :: epss=0.01_cp

       cod=real(icodes)

       do j=1,6
          if (cod(j) < 1.0 .and. abs(multip(j)) > epss)  then
             codini=codini+1
             cod(j) = real(codini)
          end if
       end do

       if (present(ord) .and. present(ss)) then
          order=ord
          ss_ptr(1:order) = ss(1:ord)
       else
          call get_stabilizer(x,Spgr,order,ss_ptr,atr)
       end if

       bet=reshape([17.0, 7.0,3.0,  &
                     7.0,13.0,5.0,  &
                     3.0, 5.0,11.0],[3,3])
       bett=bet
       if (order > 1 ) then
          do j=2,order
             Rsym=Spgr%Op(ss_ptr(j))%Mat(1:3,1:3)
             Rs=real(Rsym)
             bett=bett+ matmul(Rs,matmul(bet,transpose(Rs)))
          end do
       end if
       Rsym=nint(1000.0*bett)
       codd=[Rsym(1,1),Rsym(2,2),Rsym(3,3),Rsym(1,2),Rsym(1,3),Rsym(2,3)]
       cdd=['a','b','c','d','e','f']
       multip=1.0
       !Search systematically all the possible constraints

       if(codd(1) == codd(2) .and. codd(1) == codd(3)) then ! a a a
         if(codd(4) == codd(5) .and. codd(4) == codd(6) ) then ! a a a d d d
           if(codd(4) == 0) then
             cdd=['a','a','a','0','0','0']     ! 1 A A A 0   0   0
             multip=[1.0,1.0,1.0,0.0,0.0,0.0]
             TensVal(4:6)=0.0
             TensVal(2:3)=TensVal(1)
             cod(2:3)=cod(1); cod(4:6)=0.0
           else
             cdd=['a','a','a','d','d','d']     ! 5 A A A D   D   D
             multip=[1.0,1.0,1.0,1.0,1.0,1.0]
             TensVal(5:6)=TensVal(4)
             TensVal(2:3)=TensVal(1)
             cod(2:3)=cod(1); cod(5:6)=cod(4)
           end if
         else if(codd(4) == -codd(5) .and. codd(4) == -codd(6) ) then !a a a d -d -d
           cdd=['a','a','a','d','d','d']       ! 6 A A A D  -D  -D
           multip=[1.0,1.0,1.0,1.0,-1.0,-1.0]
           TensVal(5:6)=-TensVal(4)
           TensVal(2:3)=TensVal(1)
           cod(2:3)=cod(1); cod(5:6)=cod(4)
         else if(codd(4) == -codd(5) .and. codd(4) ==  codd(6) ) then !a a a d -d  d
           cdd=['a','a','a','d','d','d']       ! 7 A A A D  -D   D
           multip=[1.0,1.0,1.0,1.0,-1.0, 1.0]
           TensVal(5)=-TensVal(4); TensVal(6)=TensVal(4)
           TensVal(2:3)=TensVal(1)
           cod(2:3)=cod(1); cod(5:6)= cod(4)
         else if(codd(4) ==  codd(5) .and. codd(4) == -codd(6) ) then !a a a d  d -d
           cdd=['a','a','a','d','d','d']       ! 8 A A A D   D  -D
           multip=[1.0,1.0,1.0,1.0, 1.0,-1.0]
           TensVal(6)=-TensVal(4); TensVal(5)=TensVal(4)
           TensVal(2:3)=TensVal(1)
           cod(2:3)=cod(1); cod(5:6)= cod(4)
         end if

       else if(codd(1) == codd(2)) then ! a a c
         if(codd(4) == codd(5) .and. codd(4) == codd(6) .and. codd(4) == 0) then ! a a c 0 0 0
             cdd=['a','a','c','0','0','0']     ! 2 A A C 0   0   0
             multip=[1.0,1.0,1.0,0.0,0.0,0.0]
             TensVal(4:6)=0.0
             TensVal(2)=TensVal(1)
             cod(2)=cod(1); cod(4:6)= 0.0
         else if(codd(5) == codd(6) .and. codd(5) == 0) then ! a a c x 0 0
             if(codd(4) == codd(1)/2) then
               cdd=['a','a','c','a','0','0']     ! 9 A A C A/2 0   0
               multip=[1.0,1.0,1.0,0.5,0.0,0.0]
               TensVal(5:6)=0.0; TensVal(4)=TensVal(1)*0.5
               TensVal(2)=TensVal(1)
               cod(2)=cod(1); cod(4)= cod(1); cod(5:6)=0.0
             else
               cdd=['a','a','c','d','0','0']     !11 A A C D   0   0
               multip=[1.0,1.0,1.0,1.0,0.0,0.0]
               TensVal(5:6)=0.0
               TensVal(2)=TensVal(1)
               cod(2)=cod(1); cod(5:6)=0.0
             end if
         else
             if(codd(5) == codd(6)) then  ! a a c d e e
               cdd=['a','a','c','d','e','e']     !20 A A C D   E   E
               multip=[1.0,1.0,1.0,1.0,1.0,1.0]
               TensVal(6)=TensVal(5)
               TensVal(2)=TensVal(1)
               cod(2)=cod(1); cod(6)=cod(5)
             else if(codd(5) == -codd(6)) then  ! a a c d e -e
               cdd=['a','a','c','d','e','e']     !19 A A C D   E  -E
               multip=[1.0,1.0,1.0,1.0,1.0,-1.0]
               TensVal(6)=-TensVal(5)
               TensVal(2)=TensVal(1)
               cod(2)=cod(1); cod(6)=cod(5)
             end if
         end if

       else if(codd(1) == codd(3)) then ! a b a
         if(codd(4) == codd(6)) then    ! a b a d x d
           if(codd(4) == 0) then  ! a b a 0 x 0
             if(codd(5) == 0) then ! a b a 0 0 0
               cdd=['a','b','a','0','0','0']     ! 3 A B A 0   0   0
               multip=[1.0,1.0,1.0,0.0,0.0,0.0]
               TensVal(4:6)=0.0
               TensVal(3)=TensVal(1)
               cod(3)=cod(1); cod(4:6)=0.0
             else                  ! a b a 0 e 0
               cdd=['a','b','a','0','e','0']     !12 A B A 0   E   0
               multip=[1.0,1.0,1.0,0.0,1.0,0.0]
               TensVal(4)=0.0;  TensVal(6)=0.0
               TensVal(3)=TensVal(1)
               cod(3)=cod(1); cod(4)=0.0;  cod(6)=0.0
             end if
           else  !! a b a d e d
             cdd=['a','b','a','d','e','d']       !22 A B A D   E   D
             multip=[1.0,1.0,1.0,1.0,1.0,1.0]
             TensVal(6)=TensVal(4)
             TensVal(3)=TensVal(1)
             cod(3)=cod(1); cod(6)=cod(4)
          end if

         else if(codd(4) == -codd(6)) then ! a b a d e -d
           cdd=['a','b','a','d','e','d']         !21 A B A D   E  -D
           multip=[1.0,1.0,1.0,1.0,1.0,-1.0]
           TensVal(6)=-TensVal(4)
           TensVal(3)=TensVal(1)
           cod(3)=cod(1); cod(6)=cod(4)
         end if

       else if(codd(2) == codd(3)) then ! a b b
         if(codd(4) == codd(5)) then    ! a b b d d x
           if(codd(4) == 0) then  ! a b b 0 0 x
             if(codd(6) == 0) then ! a b b 0 0 0
               cdd=['a','b','b','0','0','0']     ! 4 A B B 0   0   0
               multip=[1.0,1.0,1.0,0.0,0.0,0.0]
               TensVal(4:6)=0.0
               TensVal(3)=TensVal(2)
               cod(3)=cod(2); cod(4:6)=0.0
             else                  ! a b b 0 0 f
               cdd=['a','b','b','0','0','f']     !13 A B B 0   0   F
               multip=[1.0,1.0,1.0,0.0,0.0,1.0]
               TensVal(4:5)=0.0
               TensVal(3)=TensVal(2)
               cod(3)=cod(2); cod(4:5)=0.0
             end if
           else  !! a b b d d f
             cdd=['a','b','b','d','d','f']       !24 A B B D   D   F
             multip=[1.0,1.0,1.0,1.0,1.0,1.0]
             TensVal(5)=TensVal(4)
             TensVal(3)=TensVal(2)
             cod(3)=cod(2); cod(5)=cod(4)
           end if
         else if(codd(4) == -codd(5)) then ! a b b d -d e
           cdd=['a','b','b','d','d','f']         !23 A B B D  -D   F
           multip=[1.0,1.0,1.0,1.0,-1.0,1.0]
           TensVal(5)=-TensVal(4)
           TensVal(3)=TensVal(2)
           cod(3)=cod(2); cod(5)=cod(4)
         end if

       else !Now a /= b /= c

         if(codd(4) == codd(5) .and. codd(4) == 0) then ! a b c 0 0 x
           if(codd(6) == 0) then ! a b c 0 0 0
             cdd=['a','b','c','0','0','0']          !10 A B C 0   0   0
             multip=[1.0,1.0,1.0,0.0,0.0,0.0]
             TensVal(4:6)=0.0
             cod(4:6)=0.0
           else
             cdd=['a','b','c','0','0','f']          !18 A B C 0   0   F
             multip=[1.0,1.0,1.0,0.0,0.0,1.0]
             TensVal(4:5)=0.0
             cod(4:5)=0.0
           end  if
         else if(codd(5) == codd(6) .and. codd(5) == 0) then  ! a b c x 0 0
           if(codd(4) == codd(1)/2) then ! a b c a/2 0 0
             cdd=['a','b','c','a','0','0']          !15 A B C A/2 0   0
             multip=[1.0,1.0,1.0,0.5,0.0,0.0]
             TensVal(5:6)=0.0; TensVal(4)=TensVal(1)*0.5
             cod(4)=cod(1); cod(5:6)=0.0
           else if(codd(4) == codd(2)/2) then    !a b c b/2 0 0
             cdd=['a','b','c','b','0','0']          !14 A B C B/2 0   0
             multip=[1.0,1.0,1.0,0.5,0.0,0.0]
             TensVal(5:6)=0.0; TensVal(4)=TensVal(2)*0.5
             cod(4)=cod(2); cod(5:6)=0.0
           else
             cdd=['a','b','c','d','0','0']          !16 A B C D   0   0
             multip=[1.0,1.0,1.0,1.0,0.0,0.0]
             TensVal(5:6)=0.0
             cod(5:6)=0.0
           end  if
         else if(codd(4) == codd(6) .and. codd(4) == 0) then !a b c 0 e 0
           cdd=['a','b','c','0','e','0']            !17 A B C 0   E   0
           multip=[1.0,1.0,1.0,0.0,1.0,0.0]
           TensVal(4)=0.0; TensVal(6)=0.0
           cod(4)=0.0; cod(6)=0.0
         else if(codd(4) == codd(1)/2 .and. codd(5) == 0) then !a b c a/2 0 f
           cdd=['a','b','c','a','0','f']            !26 A B C A/2 0   F
           multip=[1.0,1.0,1.0,0.5,0.0,1.0]
           TensVal(4)=TensVal(1)*0.5; TensVal(5)=0.0
           cod(4)=cod(1); cod(5)=0.0
         else if(codd(4) == codd(2)/2 .and. codd(6) == 0) then !a b c b/2 e 0
           cdd=['a','b','c','b','e','0']            !27 A B C B/2 E   0
           multip=[1.0,1.0,1.0,0.5,1.0,0.0]
           TensVal(4)=TensVal(2)*0.5; TensVal(6)=0.0
           cod(4)=cod(2); cod(6)=0.0
         else if(codd(4) == codd(2)/2 .and. codd(5) == codd(6)/2) then !a b c b/2 f/2 f
           cdd=['a','b','c','b','f','f']            !25 A B C B/2 F/2 F
           multip=[1.0,1.0,1.0,0.5,0.5,1.0]
           TensVal(4)=TensVal(2)*0.5; TensVal(5)=TensVal(6)*0.5
           cod(4)=cod(2); cod(5)=cod(6)
         else if(codd(4) == codd(1)/2 .and. codd(6) == codd(5)/2) then !a b c a/2 e e/2
           cdd=['a','b','c','a','e','e']            !28 A B C A/2 E   E/2
           multip=[1.0,1.0,1.0,0.5,1.0,0.5]
           TensVal(4)=TensVal(1)*0.5; TensVal(6)=TensVal(5)*0.5
           cod(4)=cod(1); cod(6)=cod(5)
         else
           cdd=['a','b','c','d','e','f']            !29 A B C D   E   F
           multip=[1.0,1.0,1.0,1.0,1.0,1.0]
         end if
       end if

       do j=1,6
          if (multip(j) < epss .or. cdd(j) == "0" ) then
             icodes(j) = 0
          else
             icodes(j) = nint(cod(j))
          end if
       end do

       if(present(Ipr)) then
         Write(Ipr,'(a,6i5)')           '     Codes on TensVal       :  ',Icodes
         Write(Ipr,'(a,6(a,1x),6f7.3)') '     Codes and multipliers:  ',cdd,multip
         Write(Ipr,'(a)')               '     Tensor_TOT matrix:  '
         Do I=1,3
          Write(Ipr,'(a,3f12.4)')       '                      ',bett(i,:)
         End Do
       end if

    End Subroutine Get_Atom_2nd_Tensor_Ctr

End SubModule ksym_suscept
