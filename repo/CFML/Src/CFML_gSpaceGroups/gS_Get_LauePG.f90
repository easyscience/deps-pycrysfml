!!----
!!----
!!----
SubModule (CFML_gSpaceGroups) gS_Get_LauePG
   implicit none
   Contains

   !!----
   !!---- GET_LAUE_PG
   !!----    Subroutine to get the information of Laue and Point Group
   !!----    Acta Cryst. A55, (1999) 383-395.
   !!----
   !!---- 13/05/2019
   !!
   Module Subroutine Get_Laue_PG(Ops, nops, Centro, Laue, Pg)
      !---- Arguments ----!
      type(Symm_Oper_Type), dimension(:), intent(in) :: Ops    ! Reduced operators (Numops)
      integer,                            intent(in) :: NOps   ! Numops
      logical,                            intent(in) :: Centro ! .True. if centred consideration
      character(len=*),                   intent(out):: Laue   ! Laue string
      character(len=*),                   intent(out):: Pg     ! Point group string

      !---- Local variables ----!
      integer :: nrot_1, nrot_1b
      integer :: nrot_2, nrot_2b
      integer :: nrot_3, nrot_3b
      integer :: nrot_4, nrot_4b
      integer :: nrot_6, nrot_6b
      integer :: i,n_m,nor, ind
      integer, dimension(3,3,48) :: sim
      logical :: hexa

      !> Init
      Laue=" "
      Pg=" "
      if (Nops == 0) then
         err_CFML%Ierr=1
         err_CFML%Msg="Get_Laue_PG@GSPACEGROUPS: The symmetry operators is zero!"
         return
      end if

      nrot_1  = 0
      nrot_2  = 0
      nrot_3  = 0
      nrot_4  = 0
      nrot_6  = 0
      nrot_1b = 0
      nrot_2b = 0
      nrot_3b = 0
      nrot_4b = 0
      nrot_6b = 0
      n_m = 0

      do i=1, Nops
         nor= Get_Rotation_Order(Ops(i)%Mat(1:3,1:3))
         sim(:,:,i)=Ops(i)%Mat(1:3,1:3)
         select case (nor)
            case (-6)
               nrot_6b=nrot_6b +1
            case (-4)
               nrot_4b=nrot_4b +1
            case (-3)
               nrot_3b=nrot_3b +1
            case (-2)
               nrot_2b=nrot_2b +1
            case (-1)
               nrot_1b=nrot_1b +1
            case ( 1)
               nrot_1 =nrot_1  +1
            case ( 2)
               nrot_2 =nrot_2  +1
            case ( 3)
               nrot_3 =nrot_3  +1
            case ( 4)
               nrot_4 =nrot_4  +1
            case ( 6)
               nrot_6 =nrot_6  +1
            case default
               err_CFML%Ierr=1
               err_CFML%Msg="Get_Laue_PG@GSPACEGROUPS: Problems in Rotation order calculation!"
               return
         end select
      end do

      n_m = nrot_1  + nrot_2  + nrot_3  + nrot_4  + nrot_6  + &
            nrot_1b + nrot_2b + nrot_3b + nrot_4b + nrot_6b

      !> Cubic
      if ( (nrot_3 + nrot_3b == 8) ) then
         select case (n_m)
            case (12)
               if (.not. centro) then
                  pg="23"
               else
                  pg="m-3"
               end if
               laue="m-3"

            case (24)
               if (.not. centro) then
                  if (nrot_4  == 6) pg="432"
                  if (nrot_4b == 6) pg="-43m"
               else
                  pg="m-3m"
               end if
               laue="m-3m"
         end select
         return
      end if

      !> Hexagonal
      if ( (nrot_6 + nrot_6b == 2) ) then
         select case (n_m)
            case (6)
               if (.not. centro) then
                  if (nrot_6  == 2) pg="6"
                  if (nrot_6b == 2) pg="-6"
               else
                  pg="6/m"
               end if
               laue="6/m"

            case (12)
               if (.not. centro) then
                  if (nrot_6b == 2) then
                     do i=1, Nops
                        nor= Get_Rotation_Order(Ops(i)%Mat(1:3,1:3))
                        if(nor /= 2) cycle
                         !---- This is only valid for conventional bases ---!
                        sim(:,:,i)=Ops(i)%Mat(1:3,1:3)
                        ind=SearchOp(sim(:,:,i),25,36)
                        if (ind < 0) then
                           ind=-ind-12
                        end if
                        select case (ind)
                           case (31)
                              pg="-62m"
                           case default
                              pg="-6m2"
                        end select
                        exit
                     end do
                  else
                     pg="6/mmm"
                  end if
                  if ( (nrot_6  == 2 .and. nrot_2 == 7) ) pg="622"
                  if ( (nrot_6  == 2 .and. nrot_2b== 6) ) pg="6mm"
               else
                  pg="6/mmm"
               end if
               laue="6/mmm"
         end select
         return
      end if

      !> Trigonal
      if ( (nrot_3 + nrot_3b == 2) ) then
         select case (n_m)
            case (3)
               if (.not. centro) then
                  pg="3"
               else
                  pg="-3"
               end if
               laue="-3"

            case (6)

               hexa=Is_Hexa(Nops,sim) !This works only for conventional bases

               if(hexa) then
                 if(centro) then
                    do i=1,Nops
                       nor= Get_Rotation_Order(Ops(i)%Mat(1:3,1:3))
                       if(nor /= -2) cycle
                        !---- This is only valid for conventional bases ---!
                       ind=SearchOp(sim(:,:,i),25,36)
                       if (ind < 0) then
                          ind=-ind-12
                       end if
                       select case (ind)
                          case (22)
                             pg="-31m"
                             laue="-31m"
                          case default
                             pg="-3m"
                             laue="-3m"
                       end select
                       exit
                    end do

                 else !centro

                    if (nrot_2  == 3 ) then
                       do i=1,Nops
                          nor= Get_Rotation_Order(Ops(i)%Mat(1:3,1:3))
                          if (nor /= 2) cycle
                          !---- This is only valid for conventional bases ---!
                          ind=SearchOp(sim(:,:,i),25,36)
                          if (ind < 0) then
                             ind=-ind-12
                          end if
                          select case (ind)
                             case (34)
                                pg="-312"
                                laue="-31m"
                             case default
                                pg="-32"
                                laue="-3m"
                          end select
                          exit
                       end do

                    end if !nrot_2

                    if (nrot_2b == 3 ) then

                       do i=1,Nops
                          nor= Get_Rotation_Order(Ops(i)%Mat(1:3,1:3))
                          if (nor /= -2) cycle
                          !---- This is only valid for conventional bases ---!
                          ind=SearchOp(sim(:,:,i),25,36)
                          if (ind < 0) then
                             ind=-ind-12
                          end if
                          select case (ind)
                             case (22)
                                pg="31m"
                                laue="-31m"
                             case default
                                pg="3m"
                                laue="-3m"
                          end select
                          exit
                       end do
                    end if    !nrot_2b == 3

                 end if !centro

               else  !Hexa

                 if (.not. centro) then
                    if (nrot_2 == 3) pg="32"
                    if (nrot_2b== 3) pg="3m"
                 else
                    pg="-3m"
                 end if
                 laue="-3m"

               end if !hexa

         end select
         return
      end if

      !> Tetragonal
      if ( (nrot_4 + nrot_4b == 2) ) then
         select case (n_m)
            case (4)
               if (.not. centro) then
                  if (nrot_4  == 2 ) pg="4"
                  if (nrot_4b == 2 ) pg="-4"
               else
                  pg="4/m"
               end if
               laue="4/m"

            case (8)
               if (.not. centro) then
                  if (nrot_4 == 2) then
                     if (nrot_2 == 5)  pg="422"
                     if (nrot_2b == 4) pg="4mmm"

                  else if (nrot_4b == 2) then
                     do i=1,Nops
                        nor= Get_Rotation_Order(Ops(i)%Mat(1:3,1:3))
                        if (nor /= -2) cycle
                        !---- This is only valid for conventional bases ---!
                        ind=SearchOp(sim(:,:,i),1,24)
                        if (ind < 0) then
                           ind=24-ind
                        end if
                        select case (ind)
                           case (28)
                              pg="-4m2"
                           case default
                              pg="-42m"
                        end select
                        exit
                     end do

                  end if !nrot_4b == 2
                  if ( nrot_4  == 2 .and. nrot_2 == 5 ) pg="422"
                  if ( nrot_4  == 2 .and. nrot_2b== 4 ) pg="4mm"
               else
                  pg="4/mmm"
               end if
               laue="4/mmm"

         end select
         return
      end if !nrot_4 + nrot_4b == 2

      !> Orthorhombic
      if ( nrot_2 + nrot_2b == 3 ) then

         if (.not. centro) then
            if (nrot_2  == 3 ) pg="222"
            if (nrot_2b == 2 ) then
              do i=1,Nops
                 nor= Get_Rotation_Order(Ops(i)%Mat(1:3,1:3))
                 if (nor /= 2) cycle
                 !---- This is only valid for conventional bases ---!
                 ind=SearchOp(sim(:,:,i),1,24)
                 select case (ind)
                    case (4)
                       pg="2mm"
                    case (3)
                       pg="m2m"
                    case default
                       pg="mm2"
                 end select
                 exit
              end do
            end if
         else
            pg="mmm"
         end if

         laue="mmm"
         return
      end if

      !> Monoclinic
      if ( nrot_2 + nrot_2b == 1 ) then
         if (.not. centro) then
            if (nrot_2  == 1 ) pg="2"
            if (nrot_2b == 1 ) pg="m"
         else
            pg="2/m"
         end if
         laue="2/m"
         return
      end if

      !> Triclinic
      if (n_m == 1) then
         if (.not. centro) then
            pg="1"
         else
            pg="-1"
         end if
         laue="-1"
      end if
   End Subroutine Get_Laue_PG

   !!----
   !!---- GET_LAUE_NUM
   !!----
   !!----    Obtain the ordinal number corresponding to the Laue-Class
   !!----    symbol according to Laue_Class array. Zero if error is present
   !!----
   !!---- 11/05/2019
   !!
   Module Function Get_Laue_Num(Str_Laue) Result(N)
      !---- Arguments ----!
      character(len=*), intent (in) :: Str_Laue
      integer                       :: N

      !---- Local Variables ----!
      integer                       :: i
      character(len=:), allocatable :: laue

      N=0
      laue=adjustl(Str_laue)

      do i=1,16
         if (laue(1:5) == LAUE_CLASS(i)) then
            N=i
            exit
         end if
      end do

      if (N==15) N=13
      if (N==16) N=14

   End Function Get_Laue_Num

   !!----
   !!---- GET_LAUE_STR
   !!----    Obtain the string for the Laue-Class. Blank if error
   !!----
   !!---- 11/05/2019
   !!
   Module Function Get_Laue_Str(N) Result(Str_Laue)
      !---- Arguments ----!
      integer,          intent( in) :: N
      character(len=:), allocatable :: Str_Laue

      Str_Laue="  "
      if (N < 1 .or. N > 16) return

      str_laue=LAUE_CLASS(N)
   End Function Get_Laue_Str

   !!----
   !!---- GET_POINTGROUP_NUM
   !!----
   !!----    Obtain the ordinal number corresponding to the Point Group
   !!----    symbol according to Point_Group array. Zero if Error is present
   !!----
   !!---- 11/05/2019
   !!
   Module Function Get_PointGroup_Num(Str_PG) Result(N)
      !---- Arguments ----!
      character(len=*), intent (in) :: Str_PG   ! String containing the PG information
      integer                       :: N        ! Return value on the vector POINT_GROUP

      !---- Local Variables ----!
      integer                       :: i
      character(len=:), allocatable :: pg

      N=0
      pg=adjustl(Str_PG)

      do i=1,42
         if (pg(1:5) == POINT_GROUP(i)) then
            N=i
            exit
         end if
      end do

      !> return previous numbers for m3 and m3m
      select case (N)
         case (40) ! m3 -> m-3
            N=36

         case (41) ! m3m -> m-3m
            N=39

         case (42) ! -3m1 -> -3m
            N=23
      end select

   End Function Get_PointGroup_Num

   !!----
   !!---- GET_POINTGROUP_STR
   !!----
   !!----    Obtain the string for the Point Group. Blank if error
   !!----
   !!---- 11/05/2019
   !!
   Module Function Get_PointGroup_Str(N) Result(Str_PG)
      !---- Arguments ----!
      integer,          intent( in) :: N
      character(len=:), allocatable :: Str_PG
      Str_PG="  "
      if (N < 1 .or. N > 42) return

      Str_PG=POINT_GROUP(N)
   End Function Get_PointGroup_Str

   !!----
   !!---- IDENTIFY_CRYSTALLOGRAPHIC_PG
   !!----
   !!----  Determines the crystallographic point group of the group G.
   !!----
   !!---- 24/04/2019
   !!
   Module Subroutine Identify_PointGroup(G)
       !---- Arguments ----!
       type(spg_type), intent(in out) :: G

       !---- Local variables ----!
       integer                                            :: i,j,n,d,t
       integer                                            :: numops,nRepSymOp
       logical                                            :: selected
       type(rational)                                     :: det,tr
       integer,               dimension(6)                :: nRot
       type(Symm_Oper_Type),  dimension(:),   allocatable :: repSymOp ! representative operations
       integer,               dimension(:,:), allocatable :: idd

       !> Init
       call Clear_Error()

       nRot(:)  = 0  ! number of selected rotations of order 1,2,...,6
       G%pg     = ""
       numops = G%multip / (G%num_lat+G%num_alat+1)
       if (G%centred /= 1 .or. G%anticentred /= 1) numops = numops / 2
       allocate(repSymOp(numops))

       !> Get the rotations of the representative matrices
       nRepSymOp = 0
       do i = 1 , G%Multip
          !if (Rational_Equal(G%op(i)%Mat(1:3,1:3),identity)) cycle
          det = rational_determ(G%op(i)%Mat(1:3,1:3))
          if (mod(det%numerator,det%denominator) /= 0) then
             Err_CFML%Ierr = 1
             Err_CFML%Msg = "Identify_Crystallographic_PG@SPACEG: "// &
                            "Determinant is not an integer."
             return
          end if
          d = det%numerator / det%denominator
          selected = .false.
          do j = 1 , nRepSymOp
             if (Rational_Equal(G%op(i)%Mat(1:3,1:3),repSymOp(j)%Mat(1:3,1:3)) .or. &
                Rational_Equal(G%op(i)%Mat(1:3,1:3),-repSymOp(j)%Mat(1:3,1:3))) then
                selected = .true.
                exit
             end if
          end do
          if (.not. selected) then
             nRepSymOp = nRepSymOp + 1
             if (nRepSymOp > numops) then
                Err_CFML%Ierr = 1
                Err_CFML%Msg = "Identify_Crystallographic_PG@SPACEG: nRepSymOp > numops"
                return
             end if
             repSymOp(nRepSymOp) = G%op(i)
             tr  = rational_trace(G%op(i)%Mat(1:3,1:3))
             if (mod(tr%numerator,tr%denominator) /= 0) then
                Err_CFML%Ierr = 1
                Err_CFML%Msg = "Identify_Crystallographic_PG@SPACEG: Trace is not an integer."
                return
             end if
             t = tr%numerator / tr%denominator
             select case (abs(t))
                case (0)
                   nRot(3) = nRot(3) + 1

                case(1)
                   if (d * t ==  1) then
                      nRot(4) = nRot(4) + 1
                   else
                      nRot(2) = nRot(2) + 1
                   end if

                case(2)
                   nRot(6) = nRot(6) + 1

                case(3)
                   nRot(1) = nRot(1) + 1
             end select
          end if
       end do

       !> Get the point group
       !allocate(idd(numops,2))
       allocate(idd(nRepSymOp,2))
       idd=0

       if (nRot(3) == 8) then ! Cubic
          if (nRepSymOp == 12) then
             if (G%Centred == 1 .and. G%anticentred == 1) then
                G%PG = "23"
             else
                G%PG = "m-3"
             end if

          else if (nRepSymOp == 24) then
             if (G%Centred == 1 .and. G%anticentred == 1) then
                call Get_Rotations(nRepSymOp,repSymOp,4,n,idd)
                if (n == 6 .and. idd(1,2) == 1) then
                   G%PG = "432"
                else if (n == 6 .and. idd(1,2) == -1) then
                   G%PG = "-43m"
                end if
             else
                G%PG = "m-3m"
             end if
          end if

       else if (nRot(6) == 2) then ! Hexagonal
          if (nRepSymOp == 6) then
             if (G%Centred == 1 .and. G%anticentred == 1) then
                call Get_Rotations(nRepSymOp,repSymOp,6,n,idd)
                if (idd(1,2) == 1) then
                   G%PG = "6"
                else
                   G%PG = "-6"
                end if
             else
                G%PG = "6/m"
             end if

          else if (nRepSymOp == 12) then
             if (G%Centred == 1 .and. G%anticentred == 1) then
                call Get_Rotations(nRepSymOp,repSymOp,6,n,idd)
                if (idd(1,2) == 1) then
                   call Get_Rotations(nRepSymOp,repSymOp,2,n,idd)
                   if (n == 7 .and. (idd(1,2) == -1 .or. idd(2,2) == -1)) then
                      G%PG = "6mm"
                   else
                      G%PG = "622"
                   end if

                else if (idd(1,2) == -1) then
                   G%PG = "-6m2"
                end if

             else
                G%PG = "6/mmm"
             end if
          end if

       else if (nRot(3) == 2) then ! Trigonal
          if (nRepSymOp == 3) then
             if (G%Centred == 1 .and. G%anticentred == 1) then
                G%PG = "3"
             else
                G%PG = "-3"
             end if

          else if (nRepSymOp == 6) then
             if (G%Centred == 1 .and. G%anticentred == 1) then
                call Get_Rotations(nRepSymOp,repSymOp,2,n,idd)
                if (n == 3 .and. idd(1,2) == 1) then
                   G%PG = "32"
                else if (n == 3 .and. idd(1,2) == -1) then
                   G%PG = "3m"
                end if
             else
                G%PG = "-3m"
             end if
          end if

       else if (nRot(4) == 2) then ! Tetragonal
          if (nRepSymOp == 4) then
             if (G%Centred == 1 .and. G%anticentred == 1) then
                call Get_Rotations(nRepSymOp,repSymOp,4,n,idd)
                if (n == 2 .and. idd(1,2) == 1) then
                   G%PG = "4"
                else if (n == 2 .and. idd(1,2) == -1) then
                   G%PG = "-4"
                end if
             else
                G%PG = "4/m"
             end if

          else if (nRepSymOp == 8) then
             if (G%Centred == 1 .and. G%anticentred == 1) then
                call Get_Rotations(nRepSymOp,repSymOp,4,n,idd)
                if (n == 2 .and. idd(1,2) == 1) then
                   call Get_Rotations(nRepSymOp,repSymOp,2,n,idd)
                   if (n == 5 .and. (idd(1,2) == -1 .or. idd(2,2) == -1)) then
                      G%PG = "4mm"
                   else
                      G%PG = "422"
                   end if

                else if (n == 2 .and. idd(1,2) == -1) then
                   G%PG = "-4m2"
                end if

             else
                G%PG = "4/mmm"
             end if
          end if

       else if (nRot(2) == 3) then ! Orthorhombic
          if (G%Centred == 1 .and. G%anticentred == 1) then
             call Get_Rotations(nRepSymOp,repSymOp,2,n,idd)
             if (n == 3 .and. idd(1,2) == -1 .or. idd(2,2) == -1) then
                G%PG = "mm2"
             else
                G%PG = "222"
             end if

          else
              G%PG = "mmm"
          end if

       else if (nRot(2) == 1) then ! Monoclinic
          if (G%Centred == 1 .and. G%anticentred == 1) then
             call Get_Rotations(nRepSymOp,repSymOp,2,n,idd)
             if (idd(1,2) == 1) then
                G%PG = "2"
             else
                G%PG = "m"
             end if

           else
              G%PG = "2/m"
           end if

       else
          if (G%Centred == 1 .and. G%anticentred == 1) then ! Triclinic
             G%PG = "1"
          else
             G%PG = "-1"
          end if
       end if

       if (G%PG == "") then
          Err_CFML%Ierr = 1
          Err_CFML%Msg = "Identify_Crystallographic_PG@SPACEG: Unable to identify point group"
       end if

   End Subroutine Identify_PointGroup

   !!----
   !!---- Identify_Laue_Class
   !!----
   !!---- Sets the Laue class from the crystallographic point group
   !!----
   !!---- 22/04/2019
   !!
   Module Subroutine Identify_LaueClass(G)
      !---- Arguments ----!
      type(spg_type), intent(inout) :: G

      select case (trim(G%pg))
         case ("1","-1")
            G%laue = "-1"

         case ("2","m","2/m")
            G%laue = "2/m"

         case ("222","mm2","mmm","2mm","m2m")
            G%laue = "mmm"

         case ("4","-4","4/m")
            G%laue = "4/m"

         case ("422","4mm","-4m2","4/mmm","-42m")
            G%laue = "4/mmm"

         case ("3","-3")
            G%laue = "-3"

         case ("32","3m","-3m")
            G%laue = "-3m"

         case ("31m")
            G%laue = "-31m"

         case ("6","-6","6/m")
            G%laue = "6/m"

         case ("622","6mm","-6m2","6/mmm")
            G%laue = "6/mmm"

         case ("23","m-3")
            G%laue = "m-3"

         case ("432","-43m","m-3m")
            G%laue = "m-3m"

         case default
            Err_CFML%Ierr = 1
            Err_CFML%Msg ="Identify_Laue_Class@SPACEG: Inconsistent crystallographic point group."
      end select
   End Subroutine Identify_LaueClass

   !!----
   !!---- IDENTIFY_CRYSTAL_SYSTEM
   !!----
   !!----  Determines the crystal system
   !!----
   !!---- 24/04/2019
   !!
   Module Subroutine Identify_Crystal_System(G)
       !---- Arguments ----!
       type(spg_type), intent(in out) :: G

       !---- Local variables ----!
       integer                                            :: i,j,d,t
       integer                                            :: numops,nRepSymOp
       logical                                            :: selected
       type(rational)                                     :: det,tr
       integer,               dimension(6)                :: nRot
       type(Symm_Oper_Type),  dimension(:),   allocatable :: repSymOp ! representative operations

       !> Init
       call Clear_Error()

       nRot(:)  = 0  ! number of selected rotations of order 1,2,...,6
       numops = G%multip / (G%num_lat+G%num_alat+1)
       if (G%centred /= 1 .or. G%anticentred /= 1) numops = numops / 2
       allocate(repSymOp(numops))

       !> Get the rotations of the representative matrices
       nRepSymOp = 0
       do i = 1 , G%Multip
          !if (Rational_Equal(G%op(i)%Mat(1:3,1:3),identity)) cycle
          det = rational_determ(G%op(i)%Mat(1:3,1:3))
          if (mod(det%numerator,det%denominator) /= 0) then
             Err_CFML%Ierr = 1
             Err_CFML%Msg = "Identify_Crystal_System@SPACEG: "// &
                            "Determinant is not an integer."
             return
          end if
          d = det%numerator / det%denominator
          selected = .false.
          do j = 1 , nRepSymOp
             if (Rational_Equal(G%op(i)%Mat(1:3,1:3),repSymOp(j)%Mat(1:3,1:3)) .or. &
                Rational_Equal(G%op(i)%Mat(1:3,1:3),-repSymOp(j)%Mat(1:3,1:3))) then
                selected = .true.
                exit
             end if
          end do
          if (.not. selected) then
             nRepSymOp = nRepSymOp + 1
             if (nRepSymOp > numops) then
                Err_CFML%Ierr = 1
                Err_CFML%Msg = "Identify_Crystal_System@SPACEG: nRepSymOp > numops"
                return
             end if
             repSymOp(nRepSymOp) = G%op(i)
             tr  = rational_trace(G%op(i)%Mat(1:3,1:3))
             if (mod(tr%numerator,tr%denominator) /= 0) then
                Err_CFML%Ierr = 1
                Err_CFML%Msg = "Identify_Crystal_System@SPACEG: Trace is not an integer."
                return
             end if
             t = tr%numerator / tr%denominator
             select case (abs(t))
                case (0)
                   nRot(3) = nRot(3) + 1

                case(1)
                   if (d * t ==  1) then
                      nRot(4) = nRot(4) + 1
                   else
                      nRot(2) = nRot(2) + 1
                   end if

                case(2)
                   nRot(6) = nRot(6) + 1

                case(3)
                   nRot(1) = nRot(1) + 1
             end select
          end if
       end do

       !> Get the point group
       if (nRot(3) == 8) then ! Cubic
          G%CrystalSys=SYS_CRY(7)

       else if (nRot(6) == 2) then ! Hexagonal
          G%CrystalSys=SYS_CRY(6)

       else if (nRot(3) == 2) then ! Trigonal
          G%CrystalSys=SYS_CRY(5)

       else if (nRot(4) == 2) then ! Tetragonal
          G%CrystalSys=SYS_CRY(4)

       else if (nRot(2) == 3) then ! Orthorhombic
          G%CrystalSys=SYS_CRY(3)

       else if (nRot(2) == 1) then ! Monoclinic
          G%CrystalSys=SYS_CRY(2)

       else
          G%CrystalSys=SYS_CRY(1)
       end if

   End Subroutine Identify_Crystal_System

End SubModule gS_Get_LauePG