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

module eolo

contains

function windPower()

!---Declare Module usage---
   use shared
   use inputVar
   use mathTools

!---Declare Local Variables---

   implicit none

   real(kind=prec), dimension(nTime) :: windPower
   real(kind=prec), dimension(nTime) :: eff, vel
   integer                           :: i,t
   real(kind=prec), parameter        :: rho = 1.025  !Air density

   windPower = zero

   vel = wind(:,2)

   do i=1,nwf
      eff = interpolation(cpw(:,1,i),cpw(:,2,i),ncpw(i),vel,nTime) !Power coefficient
      do t=1,nTime
         if(vel(t).gt.minWind(i).and.vel(i).lt.maxWind(i)) then
            windPower(t) = windPower(t) + 0.5*eff(t)*nwt(i)*rho*wSurf(i)*vel(t)**3
         endif
      enddo
   enddo

   return
   
end function windPower

end module eolo
