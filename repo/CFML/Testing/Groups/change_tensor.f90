 Program change_tensor
   use CFML_Maths, only: Inverse_Matrix
   implicit none
   real, dimension(3,3) :: tensor1,tensor2, matrix
   integer :: i
   matrix = reshape([0.0,0.0,-1.0,  1.0,0.0,-1.0,  0.0,-1.0,0.0 ],[3,3])

   tensor1 = reshape([0.0,2.0,0.0,  3.0,0.0,4.0,  0.0,5.0,0.0 ],[3,3])

   tensor2 = matmul(matmul(matrix,tensor1),inverse_matrix(matrix))

   do i=1,3
      write(*,"(3f8.4,tr5,3f8.4)") tensor1(i,:), tensor2(i,:)
   end do
 End Program change_tensor