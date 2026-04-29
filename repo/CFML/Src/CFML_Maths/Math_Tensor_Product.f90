!!----
!!---- SUBMODULE CFML_Math_3D
!!----
!!----
!!
Submodule (CFML_Maths) Maths_Tensor_Product
 implicit none
 Contains

    !!----
    !!---- TENSOR_PRODUCT_C
    !!----    Calculates the tensor product of vectors Vec1 and Vec2
    !!----
    !!---- 04/04/2019
    !!
    Pure Module Function Tensor_Product_C(Vec1,Vec2) Result(w)
       !---- Argument ----!
       complex(kind=cp), dimension(3), intent( in) :: Vec1,Vec2  ! Vector 1, Vector 2
       complex(kind=cp), dimension(3,3)            :: w          ! Tensor product Vector1 (o) Vector2

       !---- Local Arguments ----!
       complex(kind=cp), dimension(3,3)            :: mu,mv

       mu=0.0_cp;  mv=0.0_cp
       mu(:,1)=Vec1
       mv(1,:)=Vec2
       w=matmul(mu,mv)

    End Function Tensor_Product_C

    !!----
    !!---- TENSOR_PRODUCT_I
    !!----    Calculates the tensor product of vectors Vec1 and Vec2
    !!----
    !!---- 04/04/2019
    !!
    Pure Module Function Tensor_Product_I(Vec1, Vec2) Result(w)
       !---- Argument ----!
       integer, dimension(3), intent( in) :: Vec1,Vec2
       integer, dimension(3,3)            :: w

       !---- Local Variables ----!
       integer, dimension(3,3)            :: mu,mv

       mu=0;  mv=0
       mu(:,1)=Vec1
       mv(1,:)=Vec2
       w=matmul(mu,mv)

    End Function Tensor_Product_I

    !!----
    !!---- TENSOR_PRODUCT_R
    !!----    Calculates the tensor product of vectors Vec1 and Vec2
    !!----
    !!---- 04/04/2019
    !!
    Pure Module Function Tensor_Product_R(Vec1,Vec2) Result(w)
       !---- Argument ----!
       real(kind=cp), dimension(3), intent( in) :: Vec1,Vec2
       real(kind=cp), dimension(3,3)            :: w

       !---- Local Variables ----!
       real(kind=cp), dimension(3,3)            :: mu,mv

       mu=0.0_cp;  mv=0.0_cp
       mu(:,1)=Vec1
       mv(1,:)=Vec2
       w=matmul(mu,mv)

    End Function Tensor_Product_R

End Submodule Maths_Tensor_Product
