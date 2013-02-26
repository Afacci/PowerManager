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
!> economic calculations for a give set-point and time-step, 
!> that are, fuel costs, O&M costs, and the revenues from
!> thermal, electric, and chilling, energy selling. 
!>\author 
!     Andrea Facci.
module economy 

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

real(kind = prec) function elRev(c,t)

!---Declare Module usage---

use shared
use inputVar, only : uEl, cEl, gridBuyCost, gridSellCost, gridConnection
use plantVar, only : nm, dt
use energy

!---Declare Local Variables---
implicit none
integer, dimension(nm), intent(in) :: c !> Vector of set-point indexes
integer,                intent(in) :: t !> Current time-step index
integer          :: n, i
real(kind = prec) :: p, u, rVend

n = size(uEl,2)

if(gridConnection.ne.'StandAlone') then
    p = elProd(c,t)
    u = 0
    do i=1,n
       u = u + uEl(t,i)
    enddo
    u = u + elSelfCons(c,t)
endif
rVend = zero
do i=1,n
    rVend = rVend + uEl(t,i)*cEl(t,i)*dt(t)
enddo

elRev = zero
select case(gridConnection)
    case('StandAlone')
         elRev = rVend
    case('NetMetering')
         if(p.gt.u) then
            elRev = rVend + (p - u)*gridSellCost(t)*dt(t)
         else
            elRev = rVend + (p - u)*gridBuyCost(t)*dt(t)
         endif
    case('DedicatedRetire')
         elRev = rVend + (p*gridSellCost(t) - u*gridBuyCost(t))*dt(t)
end select

return
end function elRev

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


real(kind = prec) function thRev(t)

!---Declare Module usage---

use shared
use inputVar, only : uTh, cTh
use plantVar, only : dt

!---Declare Local Variables---
implicit none
integer, intent(in) :: t
integer :: nTh,i

nTh = size(uTh,2)

thRev = 0
do i=1,nTh
   thRev = thRev + uTh(t,i)*cTh(t,i)*dt(t)
enddo

end function thRev

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

real(kind = prec) function chRev(t)

!---Declare Module usage---

use shared
use inputVar, only : uCh, cCh
use plantVar, only : dt

!---Declare Local Variables---
implicit none
integer, intent(in) :: t
integer :: nCh,i

nCh = size(uCh,2)

chRev = 0
do i=1,nCh
   chRev = chRev + uCh(t,i)*cCh(t,i)*dt(t)
enddo

end function chRev

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

real(kind = prec) function fuelCost(c,t)

!--Declare Module usage---
use shared
use plantVar
use inputVar
use energy

implicit none

!---Declare Local Variables---
integer,         dimension(nm), intent(in) :: c
integer,                        intent(in) :: t
integer                                    :: i, n
real(kind = prec),dimension(nTrig+nBoi)     :: ein

!---Function Body

ein  = fuelCons(c,t)
fuelCost = zero
n = nTrig + nBoi
do i=1,n
   fuelCost = fuelCost + dt(t)*cf(i)*ein(i)/lhv(i)
enddo

end function fuelCost

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

real(kind = prec) function maintenanceCost(c,t)

use shared
use plantVar
use inputVar

implicit none

integer, dimension(nm), intent(in) :: c
integer,                intent(in) :: t
integer                            :: i,j

maintenanceCost = zero
do j=1,nm
   i = c(j)
   if(sp(i,j).gt.0) maintenanceCost = maintenanceCost + OeMCost(j)*dt(t)
enddo

end function maintenanceCost

!================================================================================

!>\brief 
!> Calculates the profit during a time step
!>\details
!> Calculates the profit of a time-step(in euro or any other currency according to the one used in the input) 
!> for a given set-point,
!> all the costs, except for the costs associated to equipment ignition.
!>\f[
!> G = R_{el} + R_{th} + R_{ch} + C_{f} + C_{m}
!>\f]
!> where \n
!> - \f$R_{el}\f$ are the electrical revenues;
!> - \f$R_{th}\f$ are the thermal revenues;
!> - \f$R_{ch}\f$ are the chilling revenues
!> - \f$C_{f}\f$  are the fuel costs;
!> - \f$C_{m}\f$  are the maintenance costs;
!> - \f$ dt\f$ is the time-step duration.\n
!> Having discarded lighting costs, this profit depends only on the state of the plant at a 
!> determined time-step and not on the state at pre previous or subsequent
!> time-step and will be associated to a vertex of the graph.
!>\param[in] c index of the given set-point to be given as input. Defines the state of the plant \f$sp(i) = sp(c\_(i))\f$
!>\param[in] t time step index. Note t=x meas the x'th time step from the
!> simulation start.
!>\author Andrea Facci

real(kind = prec) function currCost(c,t)

!---Declare Module usage---

use shared
use plantVar
use inputVar

implicit none

integer, dimension(nm), intent(in) :: c
integer,                intent(in) :: t

!---Declare Local Variables---

currCost = fuelCost(c,t) - elRev(c,t) - thRev(t) - chRev(t) + maintenanceCost(c,t)

end function currCost

!==============================================================================================================

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
real(kind = prec) function fireCost(cNew, cOld)

use shared
use plantVar
use inputVar

implicit none

integer, intent (in), dimension(nm) :: cNew, cOld
integer :: i,j,k
real(kind = prec), allocatable, dimension(:) :: spNew, spOld

allocate(spNew(nm), spOld(nm))
fireCost = zero
do i=1,nm
   spNew = sp(cNew(i),i)
   spOld = sp(cOld(i),i)
   if(spNew(i).eq.0.and.spOld(i).ne.0) fireCost = fireCost + onOffCost(i)
enddo
deallocate(spNew,spOld)

end function fireCost

!================================================================================================

function gridEconomy(c,t)

use shared
use inputVar, only : uEl, cEl, gridBuyCost, gridSellCost, gridConnection
use plantVar, only : nm, dt
use energy

!---Declare Local Variables---
implicit none
real(kind = prec), dimension(2) :: gridEconomy
integer, dimension(nm), intent(in) :: c !> Vector of set-point indexes
integer,                intent(in) :: t !> Current time-step index
integer          :: n, i
real(kind = prec) :: p, u

gridEconomy(:) = zero

if(gridConnection.ne.'StandAlone') then
    p = elProd(c,t)
    u = 0
    n = size(uEl,2)
    do i=1,n
       u = u + uEl(t,i)
    enddo
    u = u + elSelfCons(c,t)
endif

select case(gridConnection)
    case('StandAlone')
         gridEconomy(1) = zero        
         gridEconomy(2) = zero
    case('NetMetering')
         if(p.gt.u) then
            gridEconomy(1) = (p - u)*gridSellCost(t)
         else
            gridEconomy(2) = (p - u)*gridBuyCost(t)
         endif
    case('DedicatedRetire')
         gridEconomy(1) = p*gridSellCost(t) 
         gridEconomy(2) = u*gridBuyCost(t)
end select

return

end function gridEconomy

end module economy
