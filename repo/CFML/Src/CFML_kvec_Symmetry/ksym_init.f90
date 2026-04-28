SubModule (CFML_kvec_Symmetry) ksym_Init
   implicit none
   Contains
    !!----
    !!---- Module subroutine Init_MagSymm_k_Type(MGp)
    !!----   type(MagSymm_k_Type),  intent (in out) :: MGp
    !!----
    !!----   Subroutine to initialize the MagSymm_k_Type variable MGp.
    !!----   It is called inside Readn_set_Magnetic_Structure
    !!----
    !!----  Update: April 2005, January 2014
    !!
    Module Subroutine Init_MagSymm_k_Type(MGp)
       !---- Arguments ----!
       type(MagSymm_k_Type),  intent (in out) :: MGp

       MGp%MagModel="Unnamed Model"
       MGp%Sk_Type="Crystal_Frame" ! "Spherical_Frame"
       MGp%Fcoef_Type="S      "    ! "T" => T_ku=S_ku exp(2pii k.r_u)
       MGp%Latt="P"
       MGp%BNS_number=" "
       MGp%OG_number=" "
       MGp%BNS_symbol=" "
       MGp%OG_symbol=" "
       MGp%MagType=0
       MGp%Parent_num=0
       MGp%Parent_spg=" "
       MGp%nmsym=0
       MGp%nirreps=0
       MGp%irrep_dim=0          !Dimension of the irreps
       MGp%small_irrep_dim=0    !Dimension of the small irrep
       MGp%irrep_modes_number=0 !Number of the mode of the irrep
       MGp%irrep_id=" "         !Labels for the irreps
       MGp%irrep_direction=" "  !Irrep direction in representation space
       MGp%irrep_action=" "     !Irrep character primary or secondary
       MGp%centred=1    !By default the crystal structure is acentric
       MGp%mcentred=1   !By default the magnetic structure is anti-centric (if there is -1 it is combined with time inversion)
       MGp%nkv=0
       MGp%kvec=0.0
       MGp%kv_label=" "
       MGp%k_eqv_minusK=.false.
       MGp%Num_Lat=1
       MGp%Ltr=0.0
       MGp%Numops=0
       MGp%Multip=0
       MGp%nbas=0
       MGp%icomp=0
       MGp%basf=cmplx(0.0,0.0)

    End Subroutine Init_MagSymm_k_Type
End SubModule ksym_Init