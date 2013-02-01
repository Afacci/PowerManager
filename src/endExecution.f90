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
!>\file endExecution.f90
!>\brief normal termination of the execution.
!>\author 
! Andrea Facci.
!
!---------------------------------------------------------------------------

!>\brief normal termination of the execution.
!>\details 
!> This procedure is called for the normal termination of the program execution.
!>\author 
! Andrea Facci.
subroutine endExecution

!---Declare Module usage---

implicit none

!---Declare Local Variables---


print*
print*,'!-----------------------End Execution-----------------------!'
print*

stop

end subroutine endExecution
