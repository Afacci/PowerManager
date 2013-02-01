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
!>\file energy.f90
!>\brief Collection of function for energy flow calculation
!>\details 
!>This file contains a module that collects all the procedure for energy
!>calculations
!>\author 
!>     Andrea Facci.
!
!---------------------------------------------------------------------------

!>\brief module for energy calculations.
!>\details This module contains all procedures tu calculate the enrgy fluxes
!>inside the power plant and bertween the power plant and the clients.
!>\author Andrea Facci

module energy

contains

!>\brief Electrical production
!>\details Calculates the electrical production in kW of the whole power plant,
!>for a given set-point
!> Note that only trigeneration machines produce electrical power so far. Thus
!> electrical power is:
!>\f[ 
!> P_{el} = \sum_{Trig} sp(i)\cdot P_{max}(i)
!>\f]
!>where \f$sp(i)\f$ is the set point of the \f$i\f$'th trigenerative machine and
!>\f$P_{max}(i)\f$ is its rated power.
!>\param[in] c_  index of the given set-point to be given as input. Defines the state of the plant \f$sp(i) = sp(c\_(i))\f$
!>\author Andrea Facci

real(kind(1.d0)) function elProd(c_)

!--Declare Module usage---
use plantVar
use inputVar, only : nTrig

implicit none

!---Declare Local Variables---
integer,dimension(nm), intent(in) :: c_
integer                                    :: i,j 

!---Function Body

!--note that trigeneration machines are the only one that produce electricity
!(so far) and that they occupy the first positions of the plant load vector
elProd = 0
if(nTrig.gt.0) then
   do i=is(iT),ie(iT)
      j = c_(i)
      elProd = elProd + sp(j,i)*Pmax(i)
   enddo
endif
return
end function elProd

!==============================================================================

!>\brief Thermal production
!>\details Calculates the Thermal production in kW of the whole power plant, for
!> a given set-point
!> Note that only trigeneration machines and Boilers produce thermal power so far. Thus
!> Thermal power is:
!>\f[ 
!> P_{th} = \sum_{Trig} \frac{sp(i)\cdot P_{max}(i)}{\eta_{el}(i,sp(i))}\eta_{th}(i,sp(i)) + \sum_{Boi} sp(i)\cdot P_{max}(i)
!>\f]
!>where \f$sp(i)\f$ is the set point of the \f$i\f$'th  machine,
!>\f$\eta_{th}\f$ and \f$\eta_{el}\f$ are the thermal and electrical efficiencies,
!>respectively, and \f$P_{max}(i)\f$ is its rated power.
!>\param[in] c_  index of the given set-point to be given as input. Defines the state of the plant \f$sp(i) = sp(c\_(i))\f$
!>\author Andrea Facci

real(kind(1.d0)) function thProd(c_)

!--Declare Module usage---
use plantVar
use inputVar

implicit none

!---Declare Local Variables---
integer,dimension(nm), intent(in) :: c_
integer                                    :: i,j

!---Function Body

thProd = 0
if(nTrig.gt.0) then
   do i=is(iT),ie(iT)
      j = c_(i)
      thProd = thProd + sp(j,i)*etaTh(j,i)*Pmax(i)/etaEl(j,i)
   enddo
endif
if(nBoi.gt.0) then
   do i=is(iB),ie(iB)
      j = c_(i)
      thProd = thProd + sp(j,i)*Pmax(i)
   enddo
endif
return

end function thProd

!=======================================================================

!>\brief Chilling production
!>\details Calculates the chilling production in kW of the whole power plant,
!>for a given set-point
!> Note that only trigeneration machines and Chillers produce chilling power so far. Thus
!> chilling power is:
!>\f[ 
!> P_{ch} = \sum_{Trig} \frac{sp(i)\cdot P_{max}(i)}{\eta_{el}(i,sp(i))}\eta_{ch}(i,sp(i)) + \sum_{Chi} sp(i)\cdot P_{max}(i)
!>\f]
!>where \f$sp(i)\f$ is the set point of the \f$i\f$'th  machine,
!>\f$\eta_{ch}\f$ and \f$\eta_{el}\f$ are the chilling and electrical efficiencies,
!>respectively, and \f$P_{max}(i)\f$ is its rated power.
!>\param[in] c_  index of the given set-point to be given as input. Defines the state of the plant \f$sp(i) = sp(c\_(i))\f$
!>\author Andrea Facci

real(kind(1.d0)) function chProd(c_)

!--Declare Module usage---
use plantVar
use inputVar

implicit none

!---Declare Local Variables---
integer,dimension(nm), intent(in) :: c_
integer                                    :: i,j

!---Function Body

chProd = 0
if(nTrig.gt.0) then
   do i=is(iT),ie(iT)
      j = c_(i)
      chProd = chProd + sp(j,i)*etaCh(j,i)*Pmax(i)/etaEl(j,i)
   enddo
endif
if(nChi.gt.0) then
   do i=is(iC),ie(iC)
      j = c_(i)
      chProd = chProd + sp(j,i)*Pmax(i)
   enddo
endif
return

end function chProd

!=======================================================================================================

!>\brief Internal thermal consumption of the power plant.
!>\details Calculates the thermal self-consumption of the trigeneration plant,
!>that is, hte thermal power needed by the absorbtion chillers.
!>\f[ 
!> U_{th}^{self} = \sum_{AbsChi} \frac{sp(i)\cdot P_{max}(i)}{\eta_{ch}(i,sp(i))}
!>\f]
!>where \f$sp(i)\f$ is the set point of the \f$i\f$'th  machine,
!>\f$\eta_{ch}\f$ is  the chilling efficiency, and \f$P_{max}(i)\f$ is its rated power.
!>\param[in] c_  index of the given set-point to be given as input. Defines the state of the plant \f$sp(i) = sp(c\_(i))\f$
!>\author Andrea Facci

real(kind(1.d0)) function thSelfCons(c_)

!--Declare Module usage---
use plantVar
use inputVar

implicit none

!---Declare Local Variables---
integer,dimension(nm), intent(in) :: c_
integer                                    :: i,j 

!---Function Body

!--note that only absorption refigerators are heat consumer so far.
thSelfCons = 0
if(nChi.gt.0) then
   do i=is(3),ie(3)
      if(pes(i).eq.'heat') then
         j = c_(i)
         thSelfCons = thSelfCons + sp(j,i)*Pmax(i)/etaCh(j,i)
      endif
   enddo
endif
return

end function thSelfCons

!==============================================================================

!>\brief Internal electrical consumption of the power plant.
!>\details Calculates the electrical self-consumption of the trigeneration plant,
!>that is, the electrical power needed by the mechanical chillers, for a given
!>set-point
!>\f[ 
!> U_{el}^{self} = \sum_{MecChi} \frac{sp(i)\cdot P_{max}(i)}{\eta_{ch}(i,sp(i))}
!>\f]
!>where \f$sp(i)\f$ is the set point of the \f$i\f$'th  machine,
!>\f$\eta_{ch}\f$ is  the chilling efficiency, and \f$P_{max}(i)\f$ is its rated power. The summation is extended 
!>over the nmber of mechanical chillers.
!>\param[in] c_  index of the given set-point to be given as input. Defines the state of the plant \f$sp(i) = sp(c\_(i))\f$
!>\author Andrea Facci

real(kind(1.d0)) function elSelfCons(c_)

!--Declare Module usage---
use plantVar
use inputVar

implicit none

!---Declare Local Variables---
integer,dimension(nm), intent(in) :: c_
integer                                    :: i,j 

!---Function Body

!--note that only electrical refigerators are electrcity consumer so far.
elSelfCons = 0
if(nChi.gt.0) then
   do i=is(3),ie(3)
      if(pes(i).eq.'elec') then
         j = c_(i)
         elSelfCons = elSelfCons + sp(j,i)*Pmax(i)/etaCh(j,i)
      endif
   enddo
endif
return
end function elSelfCons

!======================================================================================

!>\brief Primary energy input
!>\details Calculates the eprimary energy input of the trigeneration plant,
!>for a given set-point
!>\f[ 
!> E_{in}(i) = \frac{sp(i)\cdot P_{max}(i)}{\eta(i,sp(i))}
!>\f]
!>where \f$sp(i)\f$ is the set point of the \f$i\f$'th  machine,
!>\f$\eta(i,sp(i)) = \eta_{el}(i,sp(i))\f$ for trigenerative equipment and 
!>\f$\eta(i,sp(i))=\eta_{th}(i,sp(i))\f$ for boilers
!>and, and \f$P_{max}(i)\f$ is their rated power.
!>\param[in] c_  index of the given set-point to be given as input. Defines the state of the plant \f$sp(i) = sp(c\_(i))\f$
!>\author Andrea Facci

function fuelCons(c_)

!--Declare Module usage---
use plantVar
use inputVar

implicit none

!---Declare Local Variables---
real(kind(1.d0)),dimension(nm)             :: fuelCons
integer         ,dimension(nm), intent(in) :: c_
integer                                    :: i,j 
real(kind(1.d0))              , parameter  :: vsmall = 1.0e-20

!---Function Body

if(nTrig.gt.0) then
   do i=is(iT),ie(iT)
      j = c_(i)
      fuelCons(i) = sp(j,i)*Pmax(i)/etaEl(j,i)
   enddo
endif

if(nBoi.gt.0) then
   do i=is(iB),ie(iB)
      j = c_(i)
      fuelCons(i) = sp(j,i)*Pmax(i)/etaTh(j,i)
      if(fuelCons(i).lt.vsmall) fuelCons(i) = 0.d0
   enddo
endif

end function fuelCons

!real(kind(1.d0)) function efficiency(c_)
!
!   use plantVar
!   use inputVar
!
!end function efficiency(c_)

end module energy
