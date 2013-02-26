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
!>\file interpolation.f90
!>\brief
!> Remaps a discrete scalar field on a given 1d grid.
!>\author 
!> Andrea Facci.
!
!---------------------------------------------------------------------------

!>\brief
!> Remaps a discrete scalar field on a given 1d grid.
!>\details 
!> This subroutine takes a discrete scalar field defined over a 1d grid and
!> remaps it on another 1d mesh given as input. If any of the values in the new
!> mesh is outside the range defined by the oiginal grid, values are extrapolated
!> and an optional warning code is returned.
!>\param[in]  xIn, yIn grid and values of the 1d field to be mapped
!>\param[in]  n        number of elements of the discrete field (size(xIn))
!>\param[in]  xOut     1d grid where the field is sampled
!>\param[in]  m        number of elements of the interpolation grid (xOut)
!>\param[out] warn     warning code. 
!>\author 
!> Andrea Facci.

function interpolation(xIn,yIn,n,xOut,m,warn) 

!---Declare Module usage---
use shared

!---Declare Local Variables---
implicit none

integer                       , intent(in)  :: n, m
real(kind = prec), dimension(n), intent(in)  :: xIn, yIn
real(kind = prec), dimension(m), intent(in)  :: xOut
integer         , dimension(2), intent(out), optional :: warn
real(kind = prec), dimension(m)              :: interpolation

real(kind = prec) :: x, xL, xU, yL, yU
integer          :: i,j

!---procedure body----

!-- detect if any value needs to be extrapolated, and return a warnign
!-- warn(1) = 1 --> Need to extrapolate higher valuers
!-- warn(1) = 1 --> Need to extrapolate lower values
if(present(warn)) then
    warn(1) = 0
    warn(2) = 0
    if (maxval(xOut).gt.maxval(xIn)) warn(1) = 1
    if (minval(xOUt).lt.minval(xIn)) warn(2) = 1
endif

!-- interpolate points.
do j=1,m
    x = xOut(j)
    do i=2,n-1
       if(xIn(i).gt.x) exit
    enddo
    xL = xIn(i-1)
    xU = xIn(i)
    yL = yIn(i-1)
    yU = yIn(i) 
    interpolation(j) = yL + (x - xL)*(yU - yL)/(xU - xL)
enddo

return

end function interpolation
