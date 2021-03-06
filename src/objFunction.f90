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
!>\file objFunction.f90
!>\brief Objective function.
!>\author 
!  Andrea Facci.
!
!---------------------------------------------------------------------------

!>\brief Objective function.
!>\details Returns the value of the objective function for a given plant state,
!> time step and optimization criterion. This procedure accounts only for
!> functions that are local in time, that is, that are function only of the
!> plant state at time t and not at time > t, neither at time < t.
!>\param[in] obj the optimization criterion.
!>\param[in] c   index of the given set-point to be given as input. Defines the state of the plant \f$sp(i) = sp(c\_(i))\f$
!>\param[in] t   time step index. Note t=x meas the x'th time step from the

real(kind = prec) function objFunction(c,t,obj)        

!---Declare Module usage---
use shared
use plantVar, only : nm
use economy
use energy

!---Declare Local Variables---
implicit none
integer, dimension(nm), intent(in)  :: c
integer,                 intent(in) :: t
character(len=100),      intent(in) :: obj 

!---Function body.
select case(obj)
   case('Economic')
       objFunction = currCost(c,t)
   case('PEC')
       objFunction = pec(c,t)
end select

end function objFunction
