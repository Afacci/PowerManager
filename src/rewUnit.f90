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
!>\file rewUnit.f90
!>\brief rewind a unit.
!>\author 
!  Andrea Facci.
!
!---------------------------------------------------------------------------

!>\brief rewind a unit.
!>\details   Rewinds a unit for a specified number of lines. Note that the unit
!> needs to be opened.
!>\param[in] theUnit The Unit to be rewinded.
!>\param[in] n       The number of lines to rewind.
!>\author 
!  Andrea Facci.

subroutine rewUnit(theUnit,n)

!---Declare Mudules usage---

!---Declare Local Variables---
implicit none
integer, intent(in)           :: theUnit, n
integer :: i
logical :: verbose_

!---function body
do i = 1,n
   backspace(theUnit)
enddo

end subroutine rewUnit
