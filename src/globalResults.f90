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
!    along with powerManager; if not, write to the Free Software Foundation,
!    Inc., 51 Franklin St, Fifth Floor, Boston, MA 02110-1301 USA
!
!
!>\file economy.f90 
!>\brief  costs and revenues calulation prodedures.
!>\details
!> This module contains the definition of all the procedures that perform
!> economic calculations for a give set-point and time-step, 
!> that are, fuel costs, O&M costs, and the revenues from
!> thermal, electric, and chilling, energy selling. 
!
!>\author 
!     Andrea Facci.
!
!---------------------------------------------------------------------------

!>\details
!> This module contains the definition of all the procedures that perform
!> global economic and energetic calculation for the whole duration of ths simulation
!>\author 
!     Andrea Facci.
module globalResults

use shared
use energy
use economy
use inputVar, only : nTime
use plantVar, only : nm

contains 

!>\brief 
!> Electric energy revenues.
!>\details
!> Calculates the revenues (in euro or any other currency according to the one used in the input)
!> from electric energy selling to the various clients
!> and to the grid.
!> Electric energy revenues are calculated in a different way for each kind of grid
!> connection. Specifically:\n
!> - Stand Alone:
!>\f[
!> R_{el} = \sum_{clients} U_{el}(i)c_{el}(i)dt
!>\f]
!> where \f$ U_{el}(i)\f$ is the power demand (in kW) of the \f$i\f$'th client,
!> \f$c_{el}(i)\f$ is the price in euro/kJ of electric energy for each client
!> and \f$ dt\f$ is the time-step duration.\n
!> - Grid connected with net metering:
!>\f[
!> R_{el} = \sum_{clients} U_{el}(i)c_{el}(i)dt + P_{el}c_{s_{grid}} - U_{el}^{t}c_{b_{grid}}
!>\f]
!> where \f$P_{el}\f$ is the total electric power produced by the power plant,
!>\f$c_{s_{grid}}\f$ is the selling price to the grid, \f$U_{el}^{t} = \sum_{clients} U_{el}(i) + U_{el}^{self} \f$ 
!> is the total electric demand, including the power plant self-consumption \f$U_{el}^{self}\f$\n
!> - Grid Connected without net metering:
!>\f[
!> R_{el} = \sum_{clients} U_{el}(i)c_{el}(i)dt + (P_{el} - U_{el}^{t}) c_{grid}
!>\f]
!>where \f$ c_{grid} = c_{s_{grid}}\f$ if \f$P_{el} \ge U_{el}^{t}\f$ and \f$ c_{grid} = c_{b_{grid}}\f$ otherwise
!>\param[in] c index of the given set-point to be given as input. Defines the state of the plant \f$sp(i) = sp(c\_(i))\f$
!>\param[in] t time step index. Note t=x meas the x'th time step from the
!> simulation start.
!>\author Andrea Facci

real(kind = prec) function globElRev(setPoint)

!---Declare Local Variables---
implicit none
integer, dimension(0:nTime+1, nm), intent(in) :: setPoint !> Vector of set-point indexes
integer          ::  t
integer, dimension(nm) :: c

globElRev = zero

do t=1,nTime
   c = setPoint(t,:)
   globElRev = globElRev + elRev(c,t)
enddo

return

end function globElRev

!===================================================================================

!>\brief 
!> thermal energy revenues.
!>\details
!> Calculates the revenues (in euro or any other currency according to the one used in the input) from thermal energy selling  
!> to the various clients.
!>\f[
!> R_{th} = \sum_{clients} U_{th}(i)c_{th}(i)dt
!>\f]
!> where \f$ U_{th}(i)\f$ is the power demand (in kW) of the \f$i\f$'th client,
!> \f$c_{th}(i)\f$ is the price in euro/kJ of electric energy for each client
!> and \f$ dt\f$ is the time-step duration.\n
!>\param[in] c index of the given set-point to be given as input. Defines the state of the plant \f$sp(i) = sp(c\_(i))\f$
!>\param[in] t time step index. Note t=x meas the x'th time step from the
!> simulation start.
!>\author Andrea Facci

real(kind = prec) function globThRev(setPoint)

!---Declare Local Variables---
implicit none
integer, dimension(0:nTime+1, nm), intent(in) :: setPoint !> Vector of set-point indexes
integer          ::  t
integer, dimension(nm) :: c

globThRev = zero

do t=1,nTime
   c = setPoint(t,:)
   globThRev = globThRev + thRev(t)
enddo

return

end function globThRev


!==============================================================================================================0

!>\brief 
!> Chilling energy revenues.
!>\details
!> Calculates the revenues (in euro or any other currency according to the one used in the input) from chilling energy selling  
!> to the various clients.
!>\f[
!> R_{ch} = \sum_{clients} U_{ch}(i)c_{ch}(i)dt
!>\f]
!> where \f$ U_{ch}(i)\f$ is the power demand (in kW) of the \f$i\f$'th client,
!> \f$c_{ch}(i)\f$ is the price in euro/kJ of electric energy for each client
!> and \f$ dt\f$ is the time-step duration.\n
!>\param[in] c index of the given set-point to be given as input. Defines the state of the plant \f$sp(i) = sp(c\_(i))\f$
!>\param[in] t time step index. Note t=x meas the x'th time step from the
!> simulation start.
!>\author Andrea Facci

real(kind = prec) function globChRev(setPoint)

!---Declare Local Variables---
implicit none
integer, dimension(0:nTime+1, nm), intent(in) :: setPoint !> Vector of set-point indexes
integer          ::  t
integer, dimension(nm) :: c

globChRev = zero

do t=1,nTime
   c = setPoint(t,:)
   globChRev = globChRev + chRev(t)
enddo

return

end function globChRev

!=======================================================================================================

!>\brief 
!> Calculates the costs to buy the fuel.
!>\details
!> Calculates the costs to buy the fuel (in euro or any other currency according to the one used in the input) 
!>\f[
!> C_{f} = \sum_{Trig+Boi} \frac{E_{in}(i)c_{f}(i)}{H_i(i)}dt
!>\f]
!> where \f$c_{f}(i)\f$ is the specific cost (per unit mass or volume) of the
!> fuel for the \f$i\f$'th machine, \f$H_i(i)\f$ is the fuel LHV (kJ per unit mass or volume),
!> \f$E_{in}(i)\f$ is the primary energy input.
!> \f$c_{ch}(i)\f$ is the price in euro/kJ of electric energy for each client
!> and \f$ dt\f$ is the time-step duration.\n
!> Note that even though international units are strongly suggested, any units
!> of mass and/or volume is valid for \f$c_{f}\f$ and \f$H_i\f$ provided that
!> coherence between the units of these two variables is respected. Moreover
!> prices and LVSs may may be expressend in different units for differend
!> machines. 
!>\param[in] c index of the given set-point to be given as input. Defines the state of the plant \f$sp(i) = sp(c\_(i))\f$
!>\param[in] t time step index. Note t=x meas the x'th time step from the
!> simulation start.
!>\author Andrea Facci

real(kind = prec) function globFuelCost(setPoint)

!---Declare Local Variables---
implicit none
integer, dimension(0:nTime+1, nm), intent(in) :: setPoint !> Vector of set-point indexes
integer          ::  t
integer, dimension(nm) :: c

globFuelCost = zero

do t=1,nTime
   c = setPoint(t,:)
   globFuelCost = globFuelCost + fuelCost(c,t)
enddo

return

end function globFuelCost

!================================================================================

!>\brief 
!> Calculates the mintenance costs.
!>\details
!> Calculates the maintenance costs (in euro or any other currency according to the one used in the input) 
!>\f[
!> C_{m} = \sum c_{m}(i)dt \qquad \mbox{if}\qquad sp(i) > 0
!>\f]
!> where \f$c_{m}(i)\f$ is the maintenance cost per unit time
!> and \f$ dt\f$ is the time-step duration.\n
!>\param[in] c index of the given set-point to be given as input. Defines the state of the plant \f$sp(i) = sp(c\_(i))\f$
!>\param[in] t time step index. Note t=x meas the x'th time step from the
!> simulation start.
!>\author Andrea Facci

real(kind = prec) function globMaintCost(setPoint)

!---Declare Local Variables---
implicit none
integer, dimension(0:nTime+1, nm), intent(in) :: setPoint !> Vector of set-point indexes
integer          ::  t
integer, dimension(nm) :: c

globMaintCost = zero

do t=1,nTime
   c = setPoint(t,:)
   globMaintCost = globMaintCost + maintenanceCost(c,t)
enddo

return

end function globMaintCost

!============================================================================================================================

!>\brief 
!> Calculates the lighting cost
!>\details
!> Calculates the cost associated to each lighting of a machinery.
!>\f[
!> C_{l} > 0 \qquad \mbox{if} \qquad sp(t,i) > 0 \;\mbox{and}\; sp(t-1,i) = 0
!>\f]
!> this cost will be added to the operative profit to for the arc profit/cost.
!>\param[in] c index of the given set-point to be given as input. Defines the state of the plant \f$sp(i) = sp(c\_(i))\f$
!>\param[in] t time step index. Note t=x meas the x'th time step from the
!> simulation start.
!>\author Andrea Facci
real(kind = prec) function globFireCost(setPoint)

implicit none

integer, intent (in), dimension(0:nTime+1,nm) :: setPoint
integer :: t
integer, dimension(nm) :: cNew, cOld

globfireCost = zero

do t=1,nTime
   cNew = setPoint(t,:)
   cOld = setPoint(t-1,:)
   globFireCost = globFireCost + fireCost(cNew,cOld)
enddo

end function globFireCost

!================================================================================================

real(kind=prec) function globCost(setPoint)

  implicit none

  integer, intent (in), dimension(0:nTime+1,nm) :: setPoint

  globCost = globFuelCost(setPoint) + globFireCost(setPoint) + globMaintCost(setPoint) 

end function globCost

!================================================================================================

real(kind=prec) function globRevenues(setPoint)

  implicit none

  integer, intent (in), dimension(0:nTime+1,nm) :: setPoint

  globRevenues = globElRev(setPoint)  + globThRev(setPoint)  + globChRev(setPoint) 

end function globRevenues

!================================================================================================

real(kind=prec) function globProfit(setPoint)

  implicit none

  integer, intent (in), dimension(0:nTime+1,nm) :: setPoint

  globProfit = globRevenues(setPoint)  - globCost(setPoint)

end function globProfit

end module globalResults
