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
!>\file openUnit.f90
!>\brief checks file presence and opens the unit.
!>\author 
!>  Andrea Facci.
!
!---------------------------------------------------------------------------

!>\brief checks file presence and opens the unit.
!>\details checks file presence and opens the unit.
!>\param[in]  fl    The file name
!>\param[in]  unt   The unit number
!>\param[out] prsnt Logical for file presence.
!>\author 
!> Andrea Facci.
subroutine openUnit(fl,unt,prsnt)

!---Declare Module usage---

!---Declare Local Variables---
implicit none
character(len=*), intent(in) :: fl
integer         , intent(in) :: unt
logical         , intent(out):: prsnt

inquire(file = fl, exist = prsnt)
if(prsnt) open(unit = unt, file = fl)

end subroutine openUnit
