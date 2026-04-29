!!-------------------------------------------------------
!!---- Crystallographic Fortran Modules Library (CrysFML)
!!-------------------------------------------------------
!!---- The CrysFML project is distributed under LGPL. In agreement with the
!!---- Intergovernmental Convention of the ILL, this software cannot be used
!!---- in military applications.
!!----
!!---- Copyright (C) 1999-2022  Institut Laue-Langevin (ILL), Grenoble, FRANCE
!!----                          Universidad de La Laguna (ULL), Tenerife, SPAIN
!!----                          Laboratoire Leon Brillouin(LLB), Saclay, FRANCE
!!----
!!---- Authors: Juan Rodriguez-Carvajal (ILL)
!!----          Javier Gonzalez-Platas  (ULL)
!!----          Nebil Ayape Katcho      (ILL)
!!----
!!---- Contributors: Laurent Chapon     (ILL)
!!----               Marc Janoschek     (Los Alamos National Laboratory, USA)
!!----               Oksana Zaharko     (Paul Scherrer Institute, Switzerland)
!!----               Tierry Roisnel     (CDIFX,Rennes France)
!!----               Eric Pellegrini    (ILL)
!!----               Ross Angel         (University of Pavia)
!!----
!!---- This library is free software; you can redistribute it and/or
!!---- modify it under the terms of the GNU Lesser General Public
!!---- License as published by the Free Software Foundation; either
!!---- version 3.0 of the License, or (at your option) any later version.
!!----
!!---- This library is distributed in the hope that it will be useful,
!!---- but WITHOUT ANY WARRANTY; without even the implied warranty of
!!---- MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
!!---- Lesser General Public License for more details.
!!----
!!---- You should have received a copy of the GNU Lesser General Public
!!---- License along with this library; if not, see <http://www.gnu.org/licenses/>.
!!----
!!---- MODULE: CFML_Wraps_Utils
!!----   INFO: Subroutines related to Python interfacing
!!----
!!---- HISTORY
!!----    Update: 18/02/2024
!!----
!!
Module CFML_Wraps_Utils

   !---- Use Modules ----!
   use Forpy_Mod, str_forpy => str
   use CFML_GlobalDeps
   use CFML_Rational

   !---- Variables ----!
   implicit none

   private

   !---- List of public procedures ----!
   public :: Check_Number_Of_Arguments,Get_Var_From_Item,&
             Ndarray_To_Pointer,Pointer_To_Array,Pointer_To_Alloc_Array,Unwrap_Dict_Item,&
             Unwrap_Dict_Item_String_Alloc

   public :: List_to_Alloc_Array_Primitive,List_to_No_Alloc_Array_Primitive

   !---- Overload ----!

   Interface Get_Var_From_Item
      module procedure Get_Complex32_From_Item
      module procedure Get_Dict_From_Item
      module procedure Get_Int32_From_Item
      module procedure Get_List_From_Item
      module procedure Get_Logical_From_Item
      module procedure Get_Ndarray_From_Item
      module procedure Get_Real32_From_Item
      module procedure Get_Real64_From_Item
      module procedure Get_String_From_Item
   End Interface Get_Var_From_Item

   Interface List_to_Alloc_Array_Primitive
      module procedure List_to_Alloc_Array1d_Character
      module procedure List_to_Alloc_Array2d_Character
      module procedure List_to_Alloc_Array1d_Logical
      module procedure List_to_Alloc_Array2d_Logical
   End Interface List_to_Alloc_Array_Primitive

   Interface List_to_No_Alloc_Array_Primitive
      module procedure List_to_No_Alloc_Array1d_Character
      module procedure List_to_No_Alloc_Array2d_Character
      module procedure List_to_No_Alloc_Array1d_Logical
      module procedure List_to_No_Alloc_Array2d_Logical
   End Interface List_to_No_Alloc_Array_Primitive

   Interface Ndarray_To_Pointer
      module procedure Ndarray_Complex32_1d_To_Pointer
      module procedure Ndarray_Complex32_2d_To_Pointer
      module procedure Ndarray_Complex32_3d_To_Pointer
      module procedure Ndarray_Complex32_4d_To_Pointer
      module procedure Ndarray_Int32_1d_To_Pointer
      module procedure Ndarray_Int32_2d_To_Pointer
      module procedure Ndarray_Int64_2d_To_Pointer
      module procedure Ndarray_Int32_3d_To_Pointer
      module procedure Ndarray_Real32_1d_To_Pointer
      module procedure Ndarray_Real32_2d_To_Pointer
      module procedure Ndarray_Real32_3d_To_Pointer
   End Interface Ndarray_To_Pointer

   Interface Pointer_To_Array
      module procedure Pointer_Complex32_1d_To_Array
      module procedure Pointer_Complex32_2d_To_Array
      module procedure Pointer_Complex32_3d_To_Array
      module procedure Pointer_Complex32_4d_To_Array
      module procedure Pointer_Int32_1d_To_Array
      module procedure Pointer_Int32_2d_To_Array
      module procedure Pointer_Int32_3d_To_Array
      module procedure Pointer_Rational_1d_To_Array
      module procedure Pointer_Rational_2d_To_Array
      module procedure Pointer_Rational_3d_To_Array
      module procedure Pointer_Real32_1d_To_Array
      module procedure Pointer_Real32_2d_To_Array
      module procedure Pointer_Real32_3d_To_Array
   End Interface Pointer_To_Array

   Interface Pointer_To_Alloc_Array
      module procedure Pointer_Complex32_1d_To_Alloc_Array
      module procedure Pointer_Complex32_2d_To_Alloc_Array
      module procedure Pointer_Complex32_3d_To_Alloc_Array
      module procedure Pointer_Complex32_4d_To_Alloc_Array
      module procedure Pointer_Int32_1d_To_Alloc_Array
      module procedure Pointer_Int32_2d_To_Alloc_Array
      module procedure Pointer_Int32_3d_To_Alloc_Array
      module procedure Pointer_Rational_1d_To_Alloc_Array
      module procedure Pointer_Rational_2d_To_Alloc_Array
      module procedure Pointer_Rational_3d_To_Alloc_Array
      module procedure Pointer_Real32_1d_To_Alloc_Array
      module procedure Pointer_Real32_2d_To_Alloc_Array
      module procedure Pointer_Real32_3d_To_Alloc_Array
   End Interface Pointer_To_Alloc_Array

   Interface Unwrap_Dict_Item
      module procedure Unwrap_Dict_Item_Complex32
      module procedure Unwrap_Dict_Item_Dict
      module procedure Unwrap_Dict_Item_Int32
      module procedure Unwrap_Dict_Item_Int64
      module procedure Unwrap_Dict_Item_List
      module procedure Unwrap_Dict_Item_Logical
      module procedure Unwrap_Dict_Item_Ndarray_Complex32_1d
      module procedure Unwrap_Dict_Item_Ndarray_Complex32_2d
      module procedure Unwrap_Dict_Item_Ndarray_Complex32_3d
      module procedure Unwrap_Dict_Item_Ndarray_Complex32_4d
      module procedure Unwrap_Dict_Item_Ndarray_Int32_1d
      module procedure Unwrap_Dict_Item_Ndarray_Int32_2d
      module procedure Unwrap_Dict_Item_Ndarray_Int32_3d
      module procedure Unwrap_Dict_Item_Ndarray_Real32_1d
      module procedure Unwrap_Dict_Item_Ndarray_Real32_2d
      module procedure Unwrap_Dict_Item_Ndarray_Real32_3d
      module procedure Unwrap_Dict_Item_Real32
      module procedure Unwrap_Dict_Item_String
   End Interface Unwrap_Dict_Item

   !---- Interface Zone ----!
   Interface

      Module Subroutine Get_Complex32_from_Item(procedure_name,var_name,item,var,ierror)
         !---- Arguments ----!
         character(len=*), intent(in)    :: procedure_name
         character(len=*), intent(in)    :: var_name
         type(object),     intent(inout) :: item
         complex,          intent(out)   :: var
         integer,          intent(inout) :: ierror
      End Subroutine Get_Complex32_from_Item

      Module Subroutine Get_Dict_from_Item(procedure_name,var_name,item,di,ierror)
         !---- Arguments ----!
         character(len=*), intent(in)    :: procedure_name
         character(len=*), intent(in)    :: var_name
         type(object),     intent(inout) :: item
         type(dict),       intent(inout) :: di
         integer,          intent(inout) :: ierror
      End Subroutine Get_Dict_from_Item

      Module Subroutine Get_Int32_from_Item(procedure_name,var_name,item,var,ierror)
         !---- Arguments ----!
         character(len=*), intent(in)    :: procedure_name
         character(len=*), intent(in)    :: var_name
         type(object),     intent(inout) :: item
         integer,          intent(out)   :: var
         integer,          intent(inout) :: ierror
      End Subroutine Get_Int32_from_Item

      Module Subroutine Get_List_from_Item(procedure_name,var_name,item,li,ierror)
         !---- Arguments ----!
         character(len=*), intent(in)    :: procedure_name
         character(len=*), intent(in)    :: var_name
         type(object),     intent(inout) :: item
         type(list),       intent(inout) :: li
         integer,          intent(inout) :: ierror
      End Subroutine Get_List_from_Item

      Module Subroutine Get_Logical_from_Item(procedure_name,var_name,item,var,ierror)
         !---- Arguments ----!
         character(len=*), intent(in)    :: procedure_name
         character(len=*), intent(in)    :: var_name
         type(object),     intent(inout) :: item
         logical,          intent(out)   :: var
         integer,          intent(inout) :: ierror
      End Subroutine Get_Logical_from_Item

      Module Subroutine Get_Ndarray_from_Item(procedure_name,var_name,item,nd,ierror)
         !---- Arguments ----!
         character(len=*), intent(in)    :: procedure_name
         character(len=*), intent(in)    :: var_name
         type(object),     intent(inout) :: item
         type(ndarray),    intent(inout) :: nd
         integer,          intent(inout) :: ierror
      End Subroutine Get_Ndarray_from_Item

      Module Subroutine Get_Real32_from_Item(procedure_name,var_name,item,var,ierror)
         !---- Arguments ----!
         character(len=*), intent(in)    :: procedure_name
         character(len=*), intent(in)    :: var_name
         type(object),     intent(inout) :: item
         real,             intent(out)   :: var
         integer,          intent(inout) :: ierror
      End Subroutine Get_Real32_from_Item

      Module Subroutine Get_Real64_from_Item(procedure_name,var_name,item,var,ierror)
         !---- Arguments ----!
         character(len=*), intent(in)    :: procedure_name
         character(len=*), intent(in)    :: var_name
         type(object),     intent(inout) :: item
         real(kind=8),     intent(out)   :: var
         integer,          intent(inout) :: ierror
      End Subroutine Get_Real64_from_Item

      Module Subroutine Get_String_from_Item(procedure_name,var_name,item,var,ierror)
         !---- Arguments ----!
         character(len=*),              intent(in)    :: procedure_name
         character(len=*),              intent(in)    :: var_name
         type(object),                  intent(inout) :: item
         character(len=:), allocatable, intent(out)   :: var
         integer,                       intent(inout) :: ierror
      End Subroutine Get_String_from_Item

      Module Subroutine list_to_alloc_array1d_character(my_list,arr,ierror)
         ! Arguments
         type(list),                                  intent(inout) :: my_list
         character(len=*), dimension(:), allocatable, intent(out)   :: arr
         integer,                                     intent(inout) :: ierror
      End Subroutine list_to_alloc_array1d_character

      Module Subroutine list_to_alloc_array2d_character(my_list,arr,ierror)
         ! Arguments
         type(list),                                    intent(inout) :: my_list
         character(len=*), dimension(:,:), allocatable, intent(out)   :: arr
         integer,                                       intent(inout) :: ierror
      End Subroutine list_to_alloc_array2d_character

      Module Subroutine list_to_no_alloc_array1d_character(my_list,arr,ierror)
         ! Arguments
         type(list),                     intent(inout) :: my_list
         character(len=*), dimension(:), intent(out)   :: arr
         integer,                        intent(inout) :: ierror
      End Subroutine list_to_no_alloc_array1d_character

      Module Subroutine list_to_no_alloc_array2d_character(my_list,arr,ierror)
         ! Arguments
         type(list),                       intent(inout) :: my_list
         character(len=*), dimension(:,:), intent(out)   :: arr
         integer,                          intent(inout) :: ierror
      End Subroutine list_to_no_alloc_array2d_character

      Module Subroutine list_to_alloc_array1d_logical(my_list,arr,ierror)
         ! Arguments
         type(list),                         intent(inout) :: my_list
         logical, dimension(:), allocatable, intent(out)   :: arr
         integer,                            intent(inout) :: ierror
      End Subroutine list_to_alloc_array1d_logical

      Module Subroutine list_to_alloc_array2d_logical(my_list,arr,ierror)
         ! Arguments
         type(list),                           intent(inout) :: my_list
         logical, dimension(:,:), allocatable, intent(out)   :: arr
         integer,                              intent(inout) :: ierror
      End Subroutine list_to_alloc_array2d_logical

      Module Subroutine list_to_no_alloc_array1d_logical(my_list,arr,ierror)
         ! Arguments
         type(list),            intent(inout) :: my_list
         logical, dimension(:), intent(out)   :: arr
         integer,               intent(inout) :: ierror
      End Subroutine list_to_no_alloc_array1d_logical

      Module Subroutine list_to_no_alloc_array2d_logical(my_list,arr,ierror)
         ! Arguments
         type(list),              intent(inout) :: my_list
         logical, dimension(:,:), intent(out)   :: arr
         integer,                 intent(inout) :: ierror
      End Subroutine list_to_no_alloc_array2d_logical

      Module Subroutine Ndarray_Complex32_1d_To_Pointer(procedure_name,var_name,nd,p,ierror)
         !---- Arguments ----!
         character(len=*),                   intent(in)    :: procedure_name
         character(len=*),                   intent(in)    :: var_name
         type(ndarray),                      intent(in)    :: nd
         complex, dimension(:), pointer,     intent(out)   :: p
         integer,                            intent(inout) :: ierror
      End Subroutine Ndarray_Complex32_1d_To_Pointer

      Module Subroutine Ndarray_Complex32_2d_To_Pointer(procedure_name,var_name,nd,p,ierror,order)
         !---- Arguments ----!
         character(len=*),                   intent(in)    :: procedure_name
         character(len=*),                   intent(in)    :: var_name
         type(ndarray),                      intent(in)    :: nd
         complex, dimension(:,:), pointer,   intent(out)   :: p
         integer,                            intent(inout) :: ierror
         character(len=1),                   intent(out)   :: order
      End Subroutine Ndarray_Complex32_2d_To_Pointer

      Module Subroutine Ndarray_Complex32_3d_To_Pointer(procedure_name,var_name,nd,p,ierror,order)
         !---- Arguments ----!
         character(len=*),                   intent(in)    :: procedure_name
         character(len=*),                   intent(in)    :: var_name
         type(ndarray),                      intent(in)    :: nd
         complex, dimension(:,:,:), pointer, intent(out)   :: p
         integer,                            intent(inout) :: ierror
         character(len=1),                   intent(out)   :: order
      End Subroutine Ndarray_Complex32_3d_To_Pointer

      Module Subroutine Ndarray_Complex32_4d_To_Pointer(procedure_name,var_name,nd,p,ierror,order)
         !---- Arguments ----!
         character(len=*),                     intent(in)    :: procedure_name
         character(len=*),                     intent(in)    :: var_name
         type(ndarray),                        intent(in)    :: nd
         complex, dimension(:,:,:,:), pointer, intent(out)   :: p
         integer,                              intent(inout) :: ierror
         character(len=1),                     intent(out)   :: order
      End Subroutine Ndarray_Complex32_4d_To_Pointer

      Module Subroutine Ndarray_Int32_1d_To_Pointer(procedure_name,var_name,nd,p,ierror)
         !---- Arguments ----!
         character(len=*),                   intent(in)    :: procedure_name
         character(len=*),                   intent(in)    :: var_name
         type(ndarray),                      intent(in)    :: nd
         integer, dimension(:), pointer,     intent(out)   :: p
         integer,                            intent(inout) :: ierror
      End Subroutine Ndarray_Int32_1d_To_Pointer

      Module Subroutine Ndarray_Int32_2d_To_Pointer(procedure_name,var_name,nd,p,ierror,order)
         !---- Arguments ----!
         character(len=*),                   intent(in)    :: procedure_name
         character(len=*),                   intent(in)    :: var_name
         type(ndarray),                      intent(in)    :: nd
         integer, dimension(:,:), pointer,   intent(out)   :: p
         integer,                            intent(inout) :: ierror
         character(len=1),                   intent(out)   :: order
      End Subroutine Ndarray_Int32_2d_To_Pointer

      Module Subroutine Ndarray_Int64_2d_To_Pointer(procedure_name,var_name,nd,p,ierror,order)
         !---- Arguments ----!
         character(len=*),                         intent(in)    :: procedure_name
         character(len=*),                         intent(in)    :: var_name
         type(ndarray),                            intent(in)    :: nd
         integer(kind=8), dimension(:,:), pointer, intent(out)   :: p
         integer,                                  intent(inout) :: ierror
         character(len=1),                         intent(out)   :: order
      End Subroutine Ndarray_Int64_2d_To_Pointer

      Module Subroutine Ndarray_Int32_3d_To_Pointer(procedure_name,var_name,nd,p,ierror,order)
         !---- Arguments ----!
         character(len=*),                   intent(in)    :: procedure_name
         character(len=*),                   intent(in)    :: var_name
         type(ndarray),                      intent(in)    :: nd
         integer, dimension(:,:,:), pointer, intent(out)   :: p
         integer,                            intent(inout) :: ierror
         character(len=1),                   intent(out)   :: order
      End Subroutine Ndarray_Int32_3d_To_Pointer

      Module Subroutine Ndarray_Real32_1d_To_Pointer(procedure_name,var_name,nd,p,ierror)
         !---- Arguments ----!
         character(len=*),                   intent(in)    :: procedure_name
         character(len=*),                   intent(in)    :: var_name
         type(ndarray),                      intent(in)    :: nd
         real, dimension(:), pointer,        intent(out)   :: p
         integer,                            intent(inout) :: ierror
      End Subroutine Ndarray_Real32_1d_To_Pointer

      Module Subroutine Ndarray_Real32_2d_To_Pointer(procedure_name,var_name,nd,p,ierror,order)
         !---- Arguments ----!
         character(len=*),                   intent(in)    :: procedure_name
         character(len=*),                   intent(in)    :: var_name
         type(ndarray),                      intent(in)    :: nd
         real, dimension(:,:), pointer,      intent(out)   :: p
         integer,                            intent(inout) :: ierror
         character(len=1),                   intent(out)   :: order
      End Subroutine Ndarray_Real32_2d_To_Pointer

      Module Subroutine Ndarray_Real32_3d_To_Pointer(procedure_name,var_name,nd,p,ierror,order)
         !---- Arguments ----!
         character(len=*),                   intent(in)    :: procedure_name
         character(len=*),                   intent(in)    :: var_name
         type(ndarray),                      intent(in)    :: nd
         real, dimension(:,:,:), pointer,    intent(out)   :: p
         integer,                            intent(inout) :: ierror
         character(len=1),                   intent(out)   :: order
      End Subroutine Ndarray_Real32_3d_To_Pointer

      Module Subroutine Pointer_Complex32_1d_To_Array(procedure_name,var_name,p,arr,ierror)
         !---- Arguments ----!
         character(len=*),               intent(in)    :: procedure_name
         character(len=*),               intent(in)    :: var_name
         complex, dimension(:), pointer, intent(in)    :: p
         complex, dimension(:),          intent(out)   :: arr
         integer,                        intent(inout) :: ierror
      End Subroutine Pointer_Complex32_1d_To_Array

      Module Subroutine Pointer_Complex32_1d_To_Alloc_Array(procedure_name,var_name,p,arr,ierror)
         !---- Arguments ----!
         character(len=*),                   intent(in)    :: procedure_name
         character(len=*),                   intent(in)    :: var_name
         complex, dimension(:), pointer,     intent(in)    :: p
         complex, dimension(:), allocatable, intent(out)   :: arr
         integer,                            intent(inout) :: ierror
      End Subroutine Pointer_Complex32_1d_To_Alloc_Array

      Module Subroutine Pointer_Complex32_2d_To_Array(procedure_name,var_name,p,arr,ierror,order)
         !---- Arguments ----!
         character(len=*),                 intent(in)    :: procedure_name
         character(len=*),                 intent(in)    :: var_name
         complex, dimension(:,:), pointer, intent(in)    :: p
         complex, dimension(:,:),          intent(out)   :: arr
         integer,                          intent(inout) :: ierror
         character(len=1),                 intent(in)    :: order
      End Subroutine Pointer_Complex32_2d_To_Array

      Module Subroutine Pointer_Complex32_2d_To_Alloc_Array(procedure_name,var_name,p,arr,ierror,order)
         !---- Arguments ----!
         character(len=*),                     intent(in)    :: procedure_name
         character(len=*),                     intent(in)    :: var_name
         complex, dimension(:,:), pointer,     intent(in)    :: p
         complex, dimension(:,:), allocatable, intent(out)   :: arr
         integer,                              intent(inout) :: ierror
         character(len=1),                     intent(in)    :: order
      End Subroutine Pointer_Complex32_2d_To_Alloc_Array

      Module Subroutine Pointer_Complex32_3d_To_Array(procedure_name,var_name,p,arr,ierror,order)
         !---- Arguments ----!
         character(len=*),                   intent(in)    :: procedure_name
         character(len=*),                   intent(in)    :: var_name
         complex, dimension(:,:,:), pointer, intent(in)    :: p
         complex, dimension(:,:,:),          intent(out)   :: arr
         integer,                            intent(inout) :: ierror
         character(len=1),                   intent(in)    :: order
      End Subroutine Pointer_Complex32_3d_To_Array

      Module Subroutine Pointer_Complex32_3d_To_Alloc_Array(procedure_name,var_name,p,arr,ierror,order)
         !---- Arguments ----!
         character(len=*),                       intent(in)    :: procedure_name
         character(len=*),                       intent(in)    :: var_name
         complex, dimension(:,:,:), pointer,     intent(in)    :: p
         complex, dimension(:,:,:), allocatable, intent(out)   :: arr
         integer,                                intent(inout) :: ierror
         character(len=1),                       intent(in)    :: order
      End Subroutine Pointer_Complex32_3d_To_Alloc_Array

      Module Subroutine Pointer_Complex32_4d_To_Array(procedure_name,var_name,p,arr,ierror,order)
         !---- Arguments ----!
         character(len=*),                     intent(in)    :: procedure_name
         character(len=*),                     intent(in)    :: var_name
         complex, dimension(:,:,:,:), pointer, intent(in)    :: p
         complex, dimension(:,:,:,:),          intent(out)   :: arr
         integer,                              intent(inout) :: ierror
         character(len=1),                     intent(in)    :: order
      End Subroutine Pointer_Complex32_4d_To_Array

      Module Subroutine Pointer_Complex32_4d_To_Alloc_Array(procedure_name,var_name,p,arr,ierror,order)
         !---- Arguments ----!
         character(len=*),                         intent(in)    :: procedure_name
         character(len=*),                         intent(in)    :: var_name
         complex, dimension(:,:,:,:), pointer,     intent(in)    :: p
         complex, dimension(:,:,:,:), allocatable, intent(out)   :: arr
         integer,                                  intent(inout) :: ierror
         character(len=1),                         intent(in)    :: order
      End Subroutine Pointer_Complex32_4d_To_Alloc_Array

      Module Subroutine Pointer_Int32_1d_To_Array(procedure_name,var_name,p,arr,ierror)
         !---- Arguments ----!
         character(len=*),               intent(in)    :: procedure_name
         character(len=*),               intent(in)    :: var_name
         integer, dimension(:), pointer, intent(in)    :: p
         integer, dimension(:),          intent(out)   :: arr
         integer,                        intent(inout) :: ierror
      End Subroutine Pointer_Int32_1d_To_Array

      Module Subroutine Pointer_Int32_1d_To_Alloc_Array(procedure_name,var_name,p,arr,ierror)
         !---- Arguments ----!
         character(len=*),                   intent(in)    :: procedure_name
         character(len=*),                   intent(in)    :: var_name
         integer, dimension(:), pointer,     intent(in)    :: p
         integer, dimension(:), allocatable, intent(out)   :: arr
         integer,                            intent(inout) :: ierror
      End Subroutine Pointer_Int32_1d_To_Alloc_Array

      Module Subroutine Pointer_Int32_2d_To_Array(procedure_name,var_name,p,arr,ierror,order)
         !---- Arguments ----!
         character(len=*),                 intent(in)    :: procedure_name
         character(len=*),                 intent(in)    :: var_name
         integer, dimension(:,:), pointer, intent(in)    :: p
         integer, dimension(:,:),          intent(out)   :: arr
         integer,                          intent(inout) :: ierror
         character(len=1),                 intent(in)    :: order
      End Subroutine Pointer_Int32_2d_To_Array

      Module Subroutine Pointer_Int32_2d_To_Alloc_Array(procedure_name,var_name,p,arr,ierror,order)
         !---- Arguments ----!
         character(len=*),                     intent(in)    :: procedure_name
         character(len=*),                     intent(in)    :: var_name
         integer, dimension(:,:), pointer,     intent(in)    :: p
         integer, dimension(:,:), allocatable, intent(out)   :: arr
         integer,                              intent(inout) :: ierror
         character(len=1),                     intent(in)    :: order
      End Subroutine Pointer_Int32_2d_To_Alloc_Array

      Module Subroutine Pointer_Int32_3d_To_Array(procedure_name,var_name,p,arr,ierror,order)
         !---- Arguments ----!
         character(len=*),                   intent(in)    :: procedure_name
         character(len=*),                   intent(in)    :: var_name
         integer, dimension(:,:,:), pointer, intent(in)    :: p
         integer, dimension(:,:,:),          intent(out)   :: arr
         integer,                            intent(inout) :: ierror
         character(len=1),                   intent(in)    :: order
      End Subroutine Pointer_Int32_3d_To_Array

      Module Subroutine Pointer_Int32_3d_To_Alloc_Array(procedure_name,var_name,p,arr,ierror,order)
         !---- Arguments ----!
         character(len=*),                       intent(in)    :: procedure_name
         character(len=*),                       intent(in)    :: var_name
         integer, dimension(:,:,:), pointer,     intent(in)    :: p
         integer, dimension(:,:,:), allocatable, intent(out)   :: arr
         integer,                                intent(inout) :: ierror
         character(len=1),                       intent(in)    :: order
      End Subroutine Pointer_Int32_3d_To_Alloc_Array

      Module Subroutine Pointer_Rational_1d_To_Array(procedure_name,var_name,p,arr,ierror)
         !---- Arguments ----!
         character(len=*),             intent(in)    :: procedure_name
         character(len=*),             intent(in)    :: var_name
         real, dimension(:), pointer,  intent(in)    :: p
         type(rational), dimension(:), intent(out)   :: arr
         integer,                      intent(inout) :: ierror
      End Subroutine Pointer_Rational_1d_To_Array

      Module Subroutine Pointer_Rational_1d_To_Alloc_Array(procedure_name,var_name,p,arr,ierror)
         !---- Arguments ----!
         character(len=*),                          intent(in)    :: procedure_name
         character(len=*),                          intent(in)    :: var_name
         real, dimension(:), pointer,               intent(in)    :: p
         type(rational), dimension(:), allocatable, intent(out)   :: arr
         integer,                                   intent(inout) :: ierror
      End Subroutine Pointer_Rational_1d_To_Alloc_Array

      Module Subroutine Pointer_Rational_2d_To_Array(procedure_name,var_name,p,arr,ierror,order)
         !---- Arguments ----!
         character(len=*),               intent(in)    :: procedure_name
         character(len=*),               intent(in)    :: var_name
         real, dimension(:,:), pointer,  intent(in)    :: p
         type(rational), dimension(:,:), intent(out)   :: arr
         integer,                        intent(inout) :: ierror
         character(len=1),               intent(in)    :: order
      End Subroutine Pointer_Rational_2d_To_Array

      Module Subroutine Pointer_Rational_2d_To_Alloc_Array(procedure_name,var_name,p,arr,ierror,order)
         !---- Arguments ----!
         character(len=*),                            intent(in)    :: procedure_name
         character(len=*),                            intent(in)    :: var_name
         real, dimension(:,:), pointer,               intent(in)    :: p
         type(rational), dimension(:,:), allocatable, intent(out)   :: arr
         integer,                                     intent(inout) :: ierror
         character(len=1),                            intent(in)    :: order
      End Subroutine Pointer_Rational_2d_To_Alloc_Array

      Module Subroutine Pointer_Rational_3d_To_Array(procedure_name,var_name,p,arr,ierror,order)
         !---- Arguments ----!
         character(len=*),                 intent(in)    :: procedure_name
         character(len=*),                 intent(in)    :: var_name
         real, dimension(:,:,:), pointer,  intent(in)    :: p
         type(rational), dimension(:,:,:), intent(out)   :: arr
         integer,                          intent(inout) :: ierror
         character(len=1),                 intent(in)    :: order
      End Subroutine Pointer_Rational_3d_To_Array

      Module Subroutine Pointer_Rational_3d_To_Alloc_Array(procedure_name,var_name,p,arr,ierror,order)
         !---- Arguments ----!
         character(len=*),                              intent(in)    :: procedure_name
         character(len=*),                              intent(in)    :: var_name
         real, dimension(:,:,:), pointer,               intent(in)    :: p
         type(rational), dimension(:,:,:), allocatable, intent(out)   :: arr
         integer,                                       intent(inout) :: ierror
         character(len=1),                              intent(in)    :: order
      End Subroutine Pointer_Rational_3d_To_Alloc_Array

      Module Subroutine Pointer_Real32_1d_To_Array(procedure_name,var_name,p,arr,ierror)
         !---- Arguments ----!
         character(len=*),            intent(in)    :: procedure_name
         character(len=*),            intent(in)    :: var_name
         real, dimension(:), pointer, intent(in)    :: p
         real, dimension(:),          intent(out)   :: arr
         integer,                     intent(inout) :: ierror
      End Subroutine Pointer_Real32_1d_To_Array

      Module Subroutine Pointer_Real32_1d_To_Alloc_Array(procedure_name,var_name,p,arr,ierror)
         !---- Arguments ----!
         character(len=*),                intent(in)    :: procedure_name
         character(len=*),                intent(in)    :: var_name
         real, dimension(:), pointer,     intent(in)    :: p
         real, dimension(:), allocatable, intent(out)   :: arr
         integer,                         intent(inout) :: ierror
      End Subroutine Pointer_Real32_1d_To_Alloc_Array

      Module Subroutine Pointer_Real32_2d_To_Array(procedure_name,var_name,p,arr,ierror,order)
         !---- Arguments ----!
         character(len=*),              intent(in)    :: procedure_name
         character(len=*),              intent(in)    :: var_name
         real, dimension(:,:), pointer, intent(in)    :: p
         real, dimension(:,:),          intent(out)   :: arr
         integer,                       intent(inout) :: ierror
         character(len=1),              intent(in)    :: order
      End Subroutine Pointer_Real32_2d_To_Array

      Module Subroutine Pointer_Real32_2d_To_Alloc_Array(procedure_name,var_name,p,arr,ierror,order)
         !---- Arguments ----!
         character(len=*),                  intent(in)    :: procedure_name
         character(len=*),                  intent(in)    :: var_name
         real, dimension(:,:), pointer,     intent(in)    :: p
         real, dimension(:,:), allocatable, intent(out)   :: arr
         integer,                           intent(inout) :: ierror
         character(len=1),                  intent(in)    :: order
      End Subroutine Pointer_Real32_2d_To_Alloc_Array

      Module Subroutine Pointer_Real32_3d_To_Array(procedure_name,var_name,p,arr,ierror,order)
         !---- Arguments ----!
         character(len=*),                intent(in)    :: procedure_name
         character(len=*),                intent(in)    :: var_name
         real, dimension(:,:,:), pointer, intent(in)    :: p
         real, dimension(:,:,:),          intent(out)   :: arr
         integer,                         intent(inout) :: ierror
         character(len=1),                intent(in)    :: order
      End Subroutine Pointer_Real32_3d_To_Array

      Module Subroutine Pointer_Real32_3d_To_Alloc_Array(procedure_name,var_name,p,arr,ierror,order)
         !---- Arguments ----!
         character(len=*),                    intent(in)    :: procedure_name
         character(len=*),                    intent(in)    :: var_name
         real, dimension(:,:,:), pointer,     intent(in)    :: p
         real, dimension(:,:,:), allocatable, intent(out)   :: arr
         integer,                             intent(inout) :: ierror
         character(len=1),                    intent(in)    :: order
      End Subroutine Pointer_Real32_3d_To_Alloc_Array

      Module Function Rational_To_Ndarray_1d(rational_arr,ierror) Result(nd_arr)
         !---- Arguments ----!
         type(rational), dimension(:), intent(in)  :: rational_arr
         integer,                      intent(out) :: ierror
         type(ndarray)                             :: nd_arr
      End Function Rational_To_Ndarray_1d

      Module Function Rational_To_Ndarray_2d(rational_arr,ierror) Result(nd_arr)
         !---- Arguments ----!
         type(rational), dimension(:,:), intent(in)  :: rational_arr
         integer,                        intent(out) :: ierror
         type(ndarray)                               :: nd_arr
      End Function Rational_To_Ndarray_2d

      Module Function Rational_To_Ndarray_3d(rational_arr,ierror) Result(nd_arr)
         !---- Arguments ----!
         type(rational), dimension(:,:,:), intent(in)  :: rational_arr
         integer,                          intent(out) :: ierror
         type(ndarray)                                 :: nd_arr
      End Function Rational_To_Ndarray_3d

      Module Subroutine Unwrap_Dict_Item_Complex32(procedure_name,var_name,di,f,ierror)
         !---- Arguments ----!
         character(len=*),            intent(in)    :: procedure_name
         character(len=*),            intent(in)    :: var_name
         type(dict),                  intent(inout) :: di
         complex,                     intent(out)   :: f
         integer,                     intent(inout) :: ierror
      End Subroutine Unwrap_Dict_Item_Complex32

      Module Subroutine Unwrap_Dict_Item_Dict(procedure_name,var_name,di,my_dict,ierror)
         !---- Arguments ----!
         character(len=*),            intent(in)    :: procedure_name
         character(len=*),            intent(in)    :: var_name
         type(dict),                  intent(inout) :: di
         type(dict),                  intent(inout) :: my_dict
         integer,                     intent(inout) :: ierror
      End Subroutine Unwrap_Dict_Item_Dict

      Module Subroutine Unwrap_Dict_Item_Int32(procedure_name,var_name,di,f,ierror)
         !---- Arguments ----!
         character(len=*),            intent(in)    :: procedure_name
         character(len=*),            intent(in)    :: var_name
         type(dict),                  intent(inout) :: di
         integer,                     intent(out)   :: f
         integer,                     intent(inout) :: ierror
      End Subroutine Unwrap_Dict_Item_Int32

      Module Subroutine Unwrap_Dict_Item_Int64(procedure_name,var_name,di,f,ierror)
         !---- Arguments ----!
         character(len=*),            intent(in)    :: procedure_name
         character(len=*),            intent(in)    :: var_name
         type(dict),                  intent(inout) :: di
         integer(kind=8),             intent(out)   :: f
         integer,                     intent(inout) :: ierror
      End Subroutine Unwrap_Dict_Item_Int64

      Module Subroutine Unwrap_Dict_Item_List(procedure_name,var_name,di,my_list,ierror)
         !---- Arguments ----!
         character(len=*),            intent(in)    :: procedure_name
         character(len=*),            intent(in)    :: var_name
         type(dict),                  intent(inout) :: di
         type(list),                  intent(inout) :: my_list
         integer,                     intent(inout) :: ierror
      End Subroutine Unwrap_Dict_Item_List

      Module Subroutine Unwrap_Dict_Item_Logical(procedure_name,var_name,di,l,ierror)
         !---- Arguments ----!
         character(len=*),            intent(in)    :: procedure_name
         character(len=*),            intent(in)    :: var_name
         type(dict),                  intent(inout) :: di
         logical,                     intent(out)   :: l
         integer,                     intent(inout) :: ierror
      End Subroutine Unwrap_Dict_Item_Logical

      Module Subroutine Unwrap_Dict_Item_Ndarray_Complex32_1d(procedure_name,var_name,di,p,ierror)
         !---- Arguments ----!
         character(len=*),               intent(in)    :: procedure_name
         character(len=*),               intent(in)    :: var_name
         type(dict),                     intent(inout) :: di
         complex, dimension(:), pointer, intent(out)   :: p
         integer,                        intent(inout) :: ierror
      End Subroutine Unwrap_Dict_Item_Ndarray_Complex32_1d

      Module Subroutine Unwrap_Dict_Item_Ndarray_Complex32_2d(procedure_name,var_name,di,p,ierror,order)
         !---- Arguments ----!
         character(len=*),                 intent(in)    :: procedure_name
         character(len=*),                 intent(in)    :: var_name
         type(dict),                       intent(inout) :: di
         complex, dimension(:,:), pointer, intent(out)   :: p
         integer,                          intent(inout) :: ierror
         character(len=1),                 intent(out)   :: order
      End Subroutine Unwrap_Dict_Item_Ndarray_Complex32_2d

      Module Subroutine Unwrap_Dict_Item_Ndarray_Complex32_3d(procedure_name,var_name,di,p,ierror,order)
         !---- Arguments ----!
         character(len=*),                   intent(in)    :: procedure_name
         character(len=*),                   intent(in)    :: var_name
         type(dict),                         intent(inout) :: di
         complex, dimension(:,:,:), pointer, intent(out)   :: p
         integer,                            intent(inout) :: ierror
         character(len=1),                   intent(out)   :: order
      End Subroutine Unwrap_Dict_Item_Ndarray_Complex32_3d

      Module Subroutine Unwrap_Dict_Item_Ndarray_Complex32_4d(procedure_name,var_name,di,p,ierror,order)
         !---- Arguments ----!
         character(len=*),                     intent(in)    :: procedure_name
         character(len=*),                     intent(in)    :: var_name
         type(dict),                           intent(inout) :: di
         complex, dimension(:,:,:,:), pointer, intent(out)   :: p
         integer,                              intent(inout) :: ierror
         character(len=1),                     intent(out)   :: order
      End Subroutine Unwrap_Dict_Item_Ndarray_Complex32_4d

      Module Subroutine Unwrap_Dict_Item_Ndarray_Int32_1d(procedure_name,var_name,di,p,ierror)
         !---- Arguments ----!
         character(len=*),               intent(in)    :: procedure_name
         character(len=*),               intent(in)    :: var_name
         type(dict),                     intent(inout) :: di
         integer, dimension(:), pointer, intent(out)   :: p
         integer,                        intent(inout) :: ierror
      End Subroutine Unwrap_Dict_Item_Ndarray_Int32_1d

      Module Subroutine Unwrap_Dict_Item_Ndarray_Int32_2d(procedure_name,var_name,di,p,ierror,order)
         !---- Arguments ----!
         character(len=*),                 intent(in)    :: procedure_name
         character(len=*),                 intent(in)    :: var_name
         type(dict),                       intent(inout) :: di
         integer, dimension(:,:), pointer, intent(out)   :: p
         integer,                          intent(inout) :: ierror
         character(len=1),                 intent(out)   :: order
      End Subroutine Unwrap_Dict_Item_Ndarray_Int32_2d

      Module Subroutine Unwrap_Dict_Item_Ndarray_Int32_3d(procedure_name,var_name,di,p,ierror,order)
         !---- Arguments ----!
         character(len=*),                   intent(in)    :: procedure_name
         character(len=*),                   intent(in)    :: var_name
         type(dict),                         intent(inout) :: di
         integer, dimension(:,:,:), pointer, intent(out)   :: p
         integer,                            intent(inout) :: ierror
         character(len=1),                 intent(out)     :: order
      End Subroutine Unwrap_Dict_Item_Ndarray_Int32_3d

      Module Subroutine Unwrap_Dict_Item_Ndarray_Real32_1d(procedure_name,var_name,di,p,ierror)
         !---- Arguments ----!
         character(len=*),            intent(in)    :: procedure_name
         character(len=*),            intent(in)    :: var_name
         type(dict),                  intent(inout) :: di
         real, dimension(:), pointer, intent(out)   :: p
         integer,                     intent(inout) :: ierror
      End Subroutine Unwrap_Dict_Item_Ndarray_Real32_1d

      Module Subroutine Unwrap_Dict_Item_Ndarray_Real32_2d(procedure_name,var_name,di,p,ierror,order)
         !---- Arguments ----!
         character(len=*),              intent(in)    :: procedure_name
         character(len=*),              intent(in)    :: var_name
         type(dict),                    intent(inout) :: di
         real, dimension(:,:), pointer, intent(out)   :: p
         integer,                       intent(inout) :: ierror
         character(len=1),              intent(out)   :: order
      End Subroutine Unwrap_Dict_Item_Ndarray_Real32_2d

      Module Subroutine Unwrap_Dict_Item_Ndarray_Real32_3d(procedure_name,var_name,di,p,ierror,order)
         !---- Arguments ----!
         character(len=*),                intent(in)    :: procedure_name
         character(len=*),                intent(in)    :: var_name
         type(dict),                      intent(inout) :: di
         real, dimension(:,:,:), pointer, intent(out)   :: p
         integer,                         intent(inout) :: ierror
         character(len=1),                intent(out)   :: order
      End Subroutine Unwrap_Dict_Item_Ndarray_Real32_3d

      Module Subroutine Unwrap_Dict_Item_Real32(procedure_name,var_name,di,f,ierror)
         !---- Arguments ----!
         character(len=*),            intent(in)    :: procedure_name
         character(len=*),            intent(in)    :: var_name
         type(dict),                  intent(inout) :: di
         real,                        intent(out)   :: f
         integer,                     intent(inout) :: ierror
      End Subroutine Unwrap_Dict_Item_Real32

      Module Subroutine Unwrap_Dict_Item_String(procedure_name,var_name,di,s,ierror)
         !---- Arguments ----!
         character(len=*),            intent(in)    :: procedure_name
         character(len=*),            intent(in)    :: var_name
         type(dict),                  intent(inout) :: di
         character(len=*),            intent(inout) :: s
         integer,                     intent(inout) :: ierror
      End Subroutine Unwrap_Dict_Item_String

      Module Subroutine Unwrap_Dict_Item_String_Alloc(procedure_name,var_name,di,my_str,ierror)
         !---- Arguments ----!
         character(len=*),              intent(in)    :: procedure_name
         character(len=*),              intent(in)    :: var_name
         type(dict),                    intent(inout) :: di
         character(len=:), allocatable, intent(inout) :: my_str
         integer,                       intent(inout) :: ierror
      End Subroutine Unwrap_Dict_Item_String_Alloc

   End Interface

   Contains

      Subroutine Check_Number_Of_Arguments(procedure_name,args,nmandatory,narg,ierror)
         !---- Arguments ----!
         character(len=*), intent(in)    :: procedure_name
         type(tuple),      intent(inout) :: args
         integer,          intent(in)    :: nmandatory
         integer,          intent(out)   :: narg
         integer,          intent(out)   :: ierror

         ierror = args%len(narg)
         if (narg < nmandatory) then
            ierror = -1
            err_cfml%ierr = ierror
            err_cfml%msg = procedure_name//': insufficient number of arguments'
        end if
      End Subroutine Check_Number_Of_Arguments

End Module CFML_Wraps_Utils
