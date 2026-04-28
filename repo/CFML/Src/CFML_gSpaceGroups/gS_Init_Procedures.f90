SubModule (CFML_gSpaceGroups) gS_Init_Procedures
   implicit none
   Contains

   !!----
   !!---- SET_IDENTITY_MATRIX
   !!----
   !!---- 19/04/2019
   !!
   Module Subroutine Set_Identity_Matrix(D)
      !---- Arguments ----!
      integer, intent(in) :: D    ! Dimension of the Matrix

      !> Init
      if (allocated(identity_matrix)) deallocate(identity_matrix)
      allocate(identity_matrix(d,d))

      call Rational_Identity_Matrix(identity_matrix)

   End Subroutine Set_Identity_Matrix

   !!----
   !!---- Init_SpaceGroup
   !!----
   !!----    Initializes the components that are not allocatable arrays
   !!----
   !!---- 19/04/2019
   !!
   Module Subroutine Init_SpaceGroup(Grp)
      !---- Arguments ----!
      class(Group_type),  intent(in out) :: Grp

      !> Init
      Grp%multip      = 1
      Grp%d           = 4

      Select Type(mGrp => Grp)

         class is (Spg_Type)
            mGrp%standard_setting=.false.
            mGrp%numspg      = 1
            mGrp%numshu      = 1
            mGrp%Numops      = 1
            mGrp%centred     = 1
            mGrp%anticentred = 0
            mGrp%mag_type    = 1
            mGrp%num_lat     = 0
            mGrp%num_alat    = 0
            mGrp%Parent_num  = 0       ! Number of the parent Group
            mGrp%spg_lat     = "P"
            mGrp%shu_lat(1)  = " "
            mGrp%shu_lat(2)  = " "
            mGrp%init_label  = "       "
            mGrp%Parent_spg  = "       "
            mGrp%tfrom_parent= "       "
            mGrp%setting     = "       "
            mGrp%spg_symb    = "                                        "
            mGrp%BNS_symb    = "                                        "
            mGrp%BNS_num     = "       "
            mGrp%OG_symb     = "       "
            mGrp%OG_num      = "       "
            mGrp%UNI         = "       "
            mGrp%UNI_num     = "       "
            mGrp%Centre      = "       " ! Alphanumeric information about the center of symmetry
            mGrp%crystalsys  = "Triclinic"
            mGrp%pg          = "1      " !Point Group
            mGrp%mag_pg      = "1.1    " !Magnetic Point Group
            mGrp%laue        = "-1     " !laue Group
            mGrp%mat2std     = "       "
            mGrp%mat2std_shu = "       "
            mGrp%matfrom     = "       "
            mGrp%generators_list = "      "
            mGrp%Hall       ="          "
            mGrp%Bravais_num=0      ! Number of the Bravais class
      End Select

      Select Type (mGrp => Grp)   !It seems that this is necessary!!!! stupid Select Type!
         class is (SuperSpaceGroup_Type)
            mGrp%nk=0               !  number of k-vectors
            mGrp%nq=0               !  number of q-coefficients
            mGrp%SSG_symb   ="          "
            mGrp%SSG_Bravais="          "
            mGrp%SSG_nlabel ="          "
      End Select

   End Subroutine Init_SpaceGroup

   !!----
   !!---- SET_CONDITIONS_GROUP
   !!----
   !!---- 19/04/2019
   !!
   Module Subroutine Set_Conditions_NumOP_EPS(maxop,epsg)
      !---- Arguments ----!
      integer,       optional, intent(in) :: maxop
      real(kind=cp), optional, intent(in) :: epsg

      !> Set a  maximum number of operators
      if (present(maxop)) maxnum_op=maxop

      !> Set a new EPS
      if (present(epsg)) then
         call Set_Eps_Math(epsg)
      else
         call Set_Eps_Math(0.001_cp)
      end if

   End Subroutine Set_Conditions_NumOp_EPS

End SubModule gS_Init_Procedures