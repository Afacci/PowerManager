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
!    along with PowerManager; if not, write to the Free Software Foundation,
!    Inc., 51 Franklin St, Fifth Floor, Boston, MA 02110-1301 USA
!
!>\file 
!>\brief File  prototype. 
!>\details this is the prototype for all the files of the PowerManger project.
!> Copy, rename, and modify this this file to create a new procedure or module.
!>\Author 
!>     Andrea Facci.
!
!---------------------------------------------------------------------------

module euristics

use shared  
use energy
use plantVar
use inputVar

contains
  
  logical function thRedundant(c,t)

  !---Declare Module usage---

  !---Declare Local Variables---
  implicit none

  integer,dimension(nm), intent(in) :: c
  integer              , intent(in) :: t
  integer                           :: i, j
  real(kind = prec)                 :: uTermal

  thRedundant = .false.
  
  uTermal = sum(uTh(t,:)) + thSelfCons(c,t)

  if(cogThProd(c,t).ge.uTermal) then
     do i=is(iB),ie(iB)
        j = c(i)
        if(sp(j,i).gt.0.d0) thRedundant = .true.
     enddo
  endif

  end function thRedundant

!==================================================================0

  logical function chRedundant(c,t)

  !---Declare Module usage---

  !---Declare Local Variables---
  implicit none

  integer,dimension(nm), intent(in) :: c
  integer              , intent(in) :: t
  integer                           :: i, j
  real(kind = prec)                 :: uChilling

  chRedundant = .false.
  
  uChilling = sum(uCh(t,:))

  end function chRedundant

end module euristics
