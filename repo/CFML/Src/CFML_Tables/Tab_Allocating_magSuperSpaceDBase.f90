!!----
!!----
!!----
!!----
SubModule (CFML_magSuperSpace_Database) TAB_Allocating_magSuperSpaceDBase
   Implicit none
   Contains
   !!----
   !!---- ALLOCATE magSSG_DBASE
   !!----
   !!---- 16/06/2022
   !!
   Module Subroutine Allocate_magSSG_DBase()
      if(magSSG_DBase_allocated) return
      if(.not. allocated(mgroup_ssg))            allocate(mgroup_ssg(Mag_NGS))
      if(.not. allocated(mgroup_spacegroup))     allocate(mgroup_spacegroup(Mag_NGS))
      if(.not. allocated(mgroup_nmod))           allocate(mgroup_nmod(Mag_NGS))
      if(.not. allocated(mgroup_mag))            allocate(mgroup_mag(Mag_NGS))
      if(.not. allocated(mgroup_nops))           allocate(mgroup_nops(Mag_NGS))
      if(.not. allocated(mgroup_ops_r))          allocate(mgroup_ops_r(Mag_OPS,Mag_NGS))
      if(.not. allocated(mgroup_nlabel))         allocate(mgroup_nlabel(Mag_NGS))
      if(.not. allocated(mgroup_label))          allocate(mgroup_label(Mag_NGS))
      if(.not. allocated(mgroup_ops))            allocate(mgroup_ops(7,7,Mag_OPS,Mag_NGS))
      if(.not. allocated(mgroup_transmag))       allocate(mgroup_transmag(4,4,Mag_NGS))
      magSSG_DBase_allocated=.true.
   End Subroutine Allocate_magSSG_DBase

   Module Subroutine Deallocate_magSSG_DBase()
      if( .not. magSSG_DBase_allocated) return
      if(allocated(mgroup_ssg))            deallocate(mgroup_ssg)
      if(allocated(mgroup_spacegroup))     deallocate(mgroup_spacegroup)
      if(allocated(mgroup_nmod))           deallocate(mgroup_nmod)
      if(allocated(mgroup_mag))            deallocate(mgroup_mag)
      if(allocated(mgroup_nops))           deallocate(mgroup_nops)
      if(allocated(mgroup_ops_r))          deallocate(mgroup_ops_r)
      if(allocated(mgroup_nlabel))         deallocate(mgroup_nlabel)
      if(allocated(mgroup_label))          deallocate(mgroup_label)
      if(allocated(mgroup_ops))            deallocate(mgroup_ops)
      if(allocated(mgroup_transmag))       deallocate(mgroup_transmag)
      magSSG_DBase_allocated=.false.
   End Subroutine Deallocate_magSSG_DBase

End Submodule TAB_Allocating_magSuperSpaceDBase