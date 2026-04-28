!!----
!!----
!!----
!!----
SubModule (CFML_SuperSpace_Database) TAB_Allocating_SuperSpaceDBase
   Implicit none
   Contains
   !!----
   !!---- ALLOCATE SSG_DBASE
   !!----
   !!----  4/02/2020
   !!
      Module Subroutine Allocate_SSG_DBase()
        if(SSG_DBase_allocated) return
        if(.not. allocated(iclass_nmod))        allocate(iclass_nmod (m_ncl))                         ; iclass_nmod=0
        if(.not. allocated(iclass_number))      allocate(iclass_number(m_ncl))                        ; iclass_number=0
        if(.not. allocated(iclass_spacegroup))  allocate(iclass_spacegroup(m_ncl))                    ; iclass_spacegroup=0
        if(.not. allocated(iclass_nstars))      allocate(iclass_nstars(m_ncl))                        ; iclass_nstars=0
        if(.not. allocated(iclass_nmodstar))    allocate(iclass_nmodstar(3,m_ncl))                    ; iclass_nmodstar=0
        if(.not. allocated(class_nlabel))       allocate(class_nlabel(m_ncl))                         ; class_nlabel=" "
        if(.not. allocated(class_label ))       allocate(class_label (m_ncl))                         ; class_label =" "
        if(.not. allocated(iclass_qvec ))       allocate(iclass_qvec (3,3,m_qv,m_ncl))                ; iclass_qvec =0
        if(.not. allocated(iclass_ncentering))  allocate(iclass_ncentering(m_ncl))                    ; iclass_ncentering=0
        if(.not. allocated(iclass_centering ))  allocate(iclass_centering(m_dim+1,m_cen,m_ncl))       ; iclass_centering =0
        if(.not. allocated(igroup_number    ))  allocate(igroup_number(m_ngs))                        ; igroup_number=0
        if(.not. allocated(igroup_class     ))  allocate(igroup_class(m_ngs))                         ; igroup_class=0
        if(.not. allocated(igroup_spacegroup))  allocate(igroup_spacegroup(m_ngs))                    ; igroup_spacegroup=0
        if(.not. allocated(group_nlabel     ))  allocate(group_nlabel(m_ngs))                         ; group_nlabel=" "
        if(.not. allocated(group_label      ))  allocate(group_label(m_ngs))                          ; group_label=" "
        if(.not. allocated(igroup_nops      ))  allocate(igroup_nops(m_ngs))                          ; igroup_nops=0
        if(.not. allocated(igroup_ops        )) allocate(igroup_ops(m_dim+1,m_dim+1,m_ops,m_ngs))     ; igroup_ops        =0
        if(.not. allocated(igroup_nconditions)) allocate(igroup_nconditions(m_ngs))                   ; igroup_nconditions=0
        if(.not. allocated(igroup_condition1 )) allocate(igroup_condition1(m_dim,m_dim,m_cond,m_ngs)) ; igroup_condition1 =0
        if(.not. allocated(igroup_condition2 )) allocate(igroup_condition2(m_dim+1,m_cond,m_ngs))     ; igroup_condition2 =0
        if(.not. allocated(pos_group ))         allocate(pos_group(m_ngs))                            ; pos_group=0
        if(.not. allocated(pos_class ))         allocate(pos_class(m_ncl))                            ; pos_class=0
        if(.not. allocated(pos_all   ))         allocate(pos_all(m_ncl+m_ngs))                        ; pos_all  =0
        ssg_DBase_allocated=.true.
      End Subroutine Allocate_SSG_DBase

      Module Subroutine Deallocate_SSG_DBase()
        if( .not. ssg_DBase_allocated) return
        if( allocated(iclass_nmod))        deallocate(iclass_nmod)
        if( allocated(iclass_number))      deallocate(iclass_number)
        if( allocated(iclass_spacegroup))  deallocate(iclass_spacegroup)
        if( allocated(iclass_nstars))      deallocate(iclass_nstars)
        if( allocated(iclass_nmodstar))    deallocate(iclass_nmodstar)
        if( allocated(class_nlabel))       deallocate(class_nlabel)
        if( allocated(class_label ))       deallocate(class_label )
        if( allocated(iclass_qvec ))       deallocate(iclass_qvec)
        if( allocated(iclass_ncentering))  deallocate(iclass_ncentering)
        if( allocated(iclass_centering ))  deallocate(iclass_centering)
        if( allocated(igroup_number))      deallocate(igroup_number)
        if( allocated(igroup_class))       deallocate(igroup_class)
        if( allocated(igroup_spacegroup))  deallocate(igroup_spacegroup)
        if( allocated(group_nlabel))       deallocate(group_nlabel)
        if( allocated(group_label ))       deallocate(group_label)
        if( allocated(igroup_nops ))       deallocate(igroup_nops)
        if( allocated(igroup_ops))         deallocate(igroup_ops)
        if( allocated(igroup_nconditions)) deallocate(igroup_nconditions)
        if( allocated(igroup_condition1 )) deallocate(igroup_condition1)
        if( allocated(igroup_condition2 )) deallocate(igroup_condition2)
        if( allocated(pos_group))          deallocate(pos_group)
        if( allocated(pos_class))          deallocate(pos_class)
        if( allocated(pos_all  ))          deallocate(pos_all)
        ssg_DBase_allocated=.false.
      End Subroutine Deallocate_SSG_DBase

End Submodule TAB_Allocating_SuperSpaceDBase