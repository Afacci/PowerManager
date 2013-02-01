
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

interface
    function interpolation(xIn,yIn,n,xOut,m,warn) 
        implicit none
        integer                       , intent(in)  :: n, m
        real(kind(1.d0)), dimension(n), intent(in)  :: xIn, yIn
        real(kind(1.d0)), dimension(m), intent(in)  :: xOut
        integer         , dimension(2), intent(in), optional :: warn
        real(kind(1.d0)), dimension(m)              :: interpolation
    end function interpolation
end interface

contains

   integer function locateRow(row,mat,n,m,error)
     
     use plantVar , only : nm
     use myArithmetic

     implicit none

     real(kind(1.d0)), dimension(n), intent(in)   :: row
     real(kind(1.d0)), dimension(m,n), intent(in) :: mat
     integer, intent(in) :: n,m
     logical,                         intent(out), optional :: error
     integer :: i
     real(kind(1.d0)),dimension(n)              :: test 
     real(kind(1.d0)) ::vsmall = 1.0e-5
    
     !---function body----
     
     if(present(error)) error = .false.
     
     i = 0
     do 
       i = i + 1
       if(i.gt.m) then
         if(present(error)) error = .true.
         locateRow = -huge(1)
         return
       endif
       test = mat(i,:)
       if(all(abs(row-test).le.vsmall)) exit
     enddo
     locateRow = i

     return
   end function locateRow

end module mathTools
