!!----
!!----
!!----
!!----
SubModule (CFML_Magnetic_Database) TAB_Allocating_MagneticDBase
   Implicit none
   Contains
   !!----
   !!---- ALLOCATE MAG_DBASE
   !!----
   !!---- 24/04/2019
   !!
   Module Subroutine Allocate_Magnetic_DBase()
      !---- Arguments ----!

      !> Check
      if (Magnetic_DBase_allocated) return

      if (.not. allocated(point_op_label))             Allocate(point_op_label(48))
      if (.not. allocated(point_op_xyz))               Allocate(point_op_xyz(48))
      if (.not. allocated(point_op_matrix))            Allocate(point_op_matrix(3,3,48))
      if (.not. allocated(point_op_hex_label))         Allocate(point_op_hex_label(24))
      if (.not. allocated(point_op_hex_xyz))           Allocate(point_op_hex_xyz(24))
      if (.not. allocated(point_op_hex_matrix))        Allocate(point_op_hex_matrix(3,3,24))
      if (.not. allocated(nlabel_bns))                 Allocate(nlabel_bns(magcount))
      if (.not. allocated(nlabelparts_bns))            Allocate(nlabelparts_bns(2,magcount))
      if (.not. allocated(spacegroup_label_bns))       Allocate(spacegroup_label_bns(magcount))
      if (.not. allocated(spacegroup_label_unified))   Allocate(spacegroup_label_unified(magcount))
      if (.not. allocated(nlabel_og))                  Allocate(nlabel_og(magcount))
      if (.not. allocated(nlabelparts_og))             Allocate(nlabelparts_og(3,magcount))
      if (.not. allocated(spacegroup_label_og))        Allocate(spacegroup_label_og(magcount))
      if (.not. allocated(magtype))                    Allocate(magtype(magcount))
      if (.not. allocated(bnsog_point_op))             Allocate(bnsog_point_op(3,3,magcount))
      if (.not. allocated(bnsog_origin))               Allocate(bnsog_origin(3,magcount))
      if (.not. allocated(bnsog_origin_denom))         Allocate(bnsog_origin_denom(magcount))
      if (.not. allocated(ops_count))                  Allocate(ops_count(magcount))
      if (.not. allocated(wyckoff_site_count))         Allocate(wyckoff_site_count(magcount))
      if (.not. allocated(wyckoff_pos_count))          Allocate(wyckoff_pos_count (27,magcount))
      if (.not. allocated(wyckoff_mult))               Allocate(wyckoff_mult(27,magcount))
      if (.not. allocated(wyckoff_label))              Allocate(wyckoff_label(27,magcount))
      if (.not. allocated(lattice_bns_vectors_count))  Allocate(lattice_bns_vectors_count (magcount))
      if (.not. allocated(lattice_bns_vectors))        Allocate(lattice_bns_vectors(3,6,magcount))
      if (.not. allocated(lattice_bns_vectors_denom))  Allocate(lattice_bns_vectors_denom(6,magcount))
      if (.not. allocated(ops_bns_point_op))           Allocate(ops_bns_point_op(96,magcount))
      if (.not. allocated(ops_bns_trans))              Allocate(ops_bns_trans(3,96,magcount))
      if (.not. allocated(ops_bns_trans_denom))        Allocate(ops_bns_trans_denom(96,magcount))
      if (.not. allocated(ops_bns_timeinv))            Allocate(ops_bns_timeinv(96,magcount))
      if (.not. allocated(wyckoff_bns_fract))          Allocate(wyckoff_bns_fract(3,96,27,magcount))
      if (.not. allocated(wyckoff_bns_fract_denom ))   Allocate(wyckoff_bns_fract_denom(96,27,magcount))
      if (.not. allocated(wyckoff_bns_xyz))            Allocate(wyckoff_bns_xyz(3,3,96,27,magcount))
      if (.not. allocated(wyckoff_bns_mag))            Allocate(wyckoff_bns_mag(3,3,96,27,magcount))
      if (.not. allocated(lattice_og_vectors_count))   Allocate(lattice_og_vectors_count(magcount))
      if (.not. allocated(lattice_og_vectors))         Allocate(lattice_og_vectors(3,6,magcount))
      if (.not. allocated(lattice_og_vectors_denom))   Allocate(lattice_og_vectors_denom(6,magcount))
      if (.not. allocated(ops_og_point_op))            Allocate(ops_og_point_op(96,magcount))
      if (.not. allocated(ops_og_trans))               Allocate(ops_og_trans(3,96,magcount))
      if (.not. allocated(ops_og_trans_denom))         Allocate(ops_og_trans_denom(96,magcount))
      if (.not. allocated(ops_og_timeinv))             Allocate(ops_og_timeinv(96,magcount))
      if (.not. allocated(wyckoff_og_fract))           Allocate(wyckoff_og_fract(3,96,27,magcount))
      if (.not. allocated(wyckoff_og_fract_denom))     Allocate(wyckoff_og_fract_denom (96,27,magcount))
      if (.not. allocated(wyckoff_og_xyz))             Allocate(wyckoff_og_xyz(3,3,96,27,magcount))
      if (.not. allocated(wyckoff_og_mag))             Allocate(wyckoff_og_mag(3,3,96,27,magcount))

      Magnetic_DBase_allocated=.true.
   End Subroutine Allocate_Magnetic_DBase

   !!----
   !!---- DEALLOCATE_MAG_DBASE
   !!----
   !!---- 24/04/2019
   !!
   Module Subroutine Deallocate_Magnetic_DBase()
      !---- Arguments ----!

      if (.not. Magnetic_DBase_allocated) return

      if (allocated(point_op_label))            deAllocate(point_op_label)
      if (allocated(point_op_xyz))              deAllocate(point_op_xyz)
      if (allocated(point_op_matrix))           deAllocate(point_op_matrix)
      if (allocated(point_op_hex_label))        deAllocate(point_op_hex_label)
      if (allocated(point_op_hex_xyz))          deAllocate(point_op_hex_xyz)
      if (allocated(point_op_hex_matrix))       deAllocate(point_op_hex_matrix)
      if (allocated(nlabel_bns))                deAllocate(nlabel_bns)
      if (allocated(nlabelparts_bns))           deAllocate(nlabelparts_bns)
      if (allocated(spacegroup_label_bns))      deAllocate(spacegroup_label_bns)
      if (allocated(spacegroup_label_unified))  deAllocate(spacegroup_label_unified)
      if (allocated(nlabel_og))                 deAllocate(nlabel_og)
      if (allocated(nlabelparts_og))            deAllocate(nlabelparts_og)
      if (allocated(spacegroup_label_og ))      deAllocate(spacegroup_label_og)
      if (allocated(magtype))                   deAllocate(magtype)
      if (allocated(bnsog_point_op))            deAllocate(bnsog_point_op)
      if (allocated(bnsog_origin))              deAllocate(bnsog_origin)
      if (allocated(bnsog_origin_denom))        deAllocate(bnsog_origin_denom )
      if (allocated(ops_count))                 deAllocate(ops_count)
      if (allocated(wyckoff_site_count))        deAllocate(wyckoff_site_count)
      if (allocated(wyckoff_pos_count))         deAllocate(wyckoff_pos_count)
      if (allocated(wyckoff_mult))              deAllocate(wyckoff_mult)
      if (allocated(wyckoff_label))             deAllocate(wyckoff_label)
      if (allocated(lattice_bns_vectors_count)) deAllocate(lattice_bns_vectors_count)
      if (allocated(lattice_bns_vectors))       deAllocate(lattice_bns_vectors)
      if (allocated(lattice_bns_vectors_denom)) deAllocate(lattice_bns_vectors_denom)
      if (allocated(ops_bns_point_op))          deAllocate(ops_bns_point_op)
      if (allocated(ops_bns_trans))             deAllocate(ops_bns_trans)
      if (allocated(ops_bns_trans_denom))       deAllocate(ops_bns_trans_denom)
      if (allocated(ops_bns_timeinv))           deAllocate(ops_bns_timeinv)
      if (allocated(wyckoff_bns_fract))         deAllocate(wyckoff_bns_fract)
      if (allocated(wyckoff_bns_fract_denom))   deAllocate(wyckoff_bns_fract_denom )
      if (allocated(wyckoff_bns_xyz))           deAllocate(wyckoff_bns_xyz)
      if (allocated(wyckoff_bns_mag))           deAllocate(wyckoff_bns_mag)
      if (allocated(lattice_og_vectors_count))  deAllocate(lattice_og_vectors_count)
      if (allocated(lattice_og_vectors))        deAllocate(lattice_og_vectors)
      if (allocated(lattice_og_vectors_denom))  deAllocate(lattice_og_vectors_denom)
      if (allocated(ops_og_point_op))           deAllocate(ops_og_point_op)
      if (allocated(ops_og_trans))              deAllocate(ops_og_trans)
      if (allocated(ops_og_trans_denom))        deAllocate(ops_og_trans_denom)
      if (allocated(ops_og_timeinv))            deAllocate(ops_og_timeinv)
      if (allocated(wyckoff_og_fract))          deAllocate(wyckoff_og_fract)
      if (allocated(wyckoff_og_fract_denom))    deAllocate(wyckoff_og_fract_denom)
      if (allocated(wyckoff_og_xyz))            deAllocate(wyckoff_og_xyz)
      if (allocated(wyckoff_og_mag))            deAllocate(wyckoff_og_mag)

      Magnetic_DBase_allocated=.false.
   End Subroutine Deallocate_Magnetic_DBase

End Submodule TAB_Allocating_MagneticDBase