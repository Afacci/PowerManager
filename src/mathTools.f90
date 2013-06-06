
!---------------------------------------------------------------------------
!  
!   PowerManager.  
!
!---------------------------------------------------------------------------
!License
!    This file is part of PowerManager.
!
!    PowerManager is free software; you can redistribute it and/or modify it
!    under the terms of the GNU General Public License as published by the
!    Free Software Foundation; either version 2 of the License, or (at your
!    option) any later version.
!
!    PowerManager is distributed in the hope that it will be useful, but WITHOUT
!    ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or
!    FITNESS FOR A PARTICULAR PURPOSE.  See the GNU General Public License
!    for more details.
!
!    You should have received a copy of the GNU General Public License
!    along with OpenFOAM; if not, write to the Free Software Foundation,
!    Inc., 51 Franklin St, Fifth Floor, Boston, MA 02110-1301 USA
!
!>\file mathTools.f90
!>\brief Collection of interfaces for basic mathematical tools. 
!>\author 
!> Andrea Facci.
!
!---------------------------------------------------------------------------

!>\brief Collection of interfaces for basic mathematical tools. 
!>\details Collection of interfaces for basic mathematical tools. 
!>\author 
!> Andrea Facci.

module mathTools

use shared 

interface
    function interpolation(xIn,yIn,n,xOut,m,warn) 
        use shared 
        implicit none
        integer                       , intent(in)  :: n, m
        real(kind = prec), dimension(n), intent(in)  :: xIn, yIn
        real(kind = prec), dimension(m), intent(in)  :: xOut
        integer         , dimension(2), intent(in), optional :: warn
        real(kind = prec), dimension(m)              :: interpolation
    end function interpolation
end interface

contains

   integer function locateRow(row,mat,n,m,error)
     
     use plantVar , only : nm
     use myArithmetic

     implicit none

     real(kind = prec ), dimension(n), intent(in)  :: row
     real(kind = prec), dimension(m,n), intent(in) :: mat
     integer, intent(in) :: n,m
     logical,                         intent(out), optional :: error
     integer :: i,j
     real(kind = prec),dimension(n)              :: test 
     logical                                     :: ok
     real(kind=prec), parameter                  :: err=0.001, aerr=0.01
     real(kind=prec), dimension(n)               :: diff, toll
     real(kind=prec)                             :: delta
    
     !---function body----
     
     if(present(error)) error = .false.

     diff(:) = huge(1.0)
     do i=1,n
        do j=1,m-1
           delta = abs(mat(j,i) - mat(j+1,i))
           if(delta.gt.zero) diff(i) = min(diff(i),delta)
        enddo
     enddo
     toll(:) = err*diff(:)
     
     i = 0
     do 
       ok= .true.
       i = i + 1
       if(i.gt.m) then
         if(present(error)) error = .true.
         locateRow = -huge(1)
         return
       endif
       test = mat(i,:)
       do j=1,n
          delta = abs(test(j) - row(j))
          if(delta.ge.toll(j)) ok = .false.
       enddo
       if(ok) exit
     enddo
     locateRow = i

     return
   end function locateRow

!=================================================================================


   integer function locateElement(val,vec,n,error)
     
     use plantVar , only : nm
     use myArithmetic

     implicit none

     real(kind = prec), intent(in)  :: val
     real(kind = prec), dimension(n), intent(in) :: vec
     integer, intent(in) :: n
     logical,                         intent(out), optional :: error
     integer :: i
     real(kind = prec) :: test 
    
     !---function body----
     
     if(present(error)) error = .false.
     
     i = 0
     do 
       i = i + 1
       if(i.gt.n) then
         if(present(error)) error = .true.
         locateElement = -huge(1)
         return
       endif
       test = vec(i)
       if(abs(val-test).le.1.0) exit
     enddo
     locateElement = i

     return
   end function locateElement

end module mathTools
