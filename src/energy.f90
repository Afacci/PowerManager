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

use shared

contains

!>\brief Electrical production
!>\details Calculates the electrical production in kW of the whole power plant,
!>for a given set-point
!> Note that only trigeneration machines produce electrical power so far. Thus
!> electrical power is:
!>\f[ 
!> P_{el} = \sum_{Trig} sp(i)\cdot P_{max}(i)\cdot k_{env} 
!>\f]
!>where \f$sp(i)\f$ is the set point of the \f$i\f$'th trigenerative machine and
!>\f$P_{max}(i)\f$ is its rated power. \f$ k_{env} = k_a\cdot k_T \cdot k_p \f$ is the environmental power
!> correction; and \f$ k_a\f$,  \f$ k_t\f$, and \f$ k_p\f$ are the altitude, temperature, and pressure corrections respectively.
!>\param[in] c_  index of the given set-point to be given as input. Defines the state of the plant \f$sp(i) = sp(c\_(i))\f$
!>\param[in] t   time index
!>\author Andrea Facci

real(kind = prec) function elProd(c_, t)

!--Declare Module usage---
use plantVar
use inputVar, only : nTrig

implicit none

!---Declare Local Variables---
integer,dimension(nm), intent(in) :: c_
integer,               intent(in) :: t
integer                           :: i,j 

!---Function Body

!--note that trigeneration machines are the only one that produce electricity
!(so far) and that they occupy the first positions of the plant load vector
elProd = 0
if(nTrig.gt.0) then
   do i=is(iT),ie(iT)
      j = c_(i)
      elProd = elProd + sp(j,i)*Pmax(i)*envCorr(t,i,4)
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
!> P_{th} = \sum_{Trig} \frac{sp(i)\cdot P_{max}(i)\cdot k_{env}}{\eta_{el}(i,sp(i))\cdot \alpha_{env}}\eta_{th}(i,sp(i))\cdot\gamma_{env}+ \sum_{Boi} sp(i)\cdot P_{max}(i)\cdot \lambda_{env}
!>\f]
!>where \f$sp(i)\f$ is the set point of the \f$i\f$'th  machine,
!>\f$\eta_{th}\f$ and \f$\eta_{el}\f$ are the thermal and electrical efficiencies,
!>respectively, and \f$P_{max}(i)\f$ is its rated power; \f$ k_{env} \f$, \f$ \alpha_{env} \f$, \f$ \gamma_{env} \f$, and \f$ \lambda_{env} \f$ are the environmental corrections for electrical power
!> and efficiency, thermal efficiency, and thermal power, respectively.
!>\param[in] c_  index of the given set-point to be given as input. Defines the state of the plant \f$sp(i) = sp(c\_(i))\f$
!>\param[in] t   time index
!>\author Andrea Facci

real(kind = prec) function thProd(c_,t)

!--Declare Module usage---
use plantVar
use inputVar

implicit none

!---Declare Local Variables---
integer,dimension(nm), intent(in) :: c_
integer              , intent(in) :: t
integer                           :: i,j,k,l
real(kind = prec)                  :: calore,eDisp,eEff,tEff,pow 

!---Function Body

thProd = 0
if(nTrig.gt.0) then
   do i=is(iT),ie(iT)
      j = c_(i)
      eEff = etaEl(j,i)*envCorr(t,i,1)
      tEff = etaTh(j,i)*envCorr(t,i,2)
      pow  = sp(j,i)*Pmax(i)*envCorr(t,i,4)
      eEff = max(eEff,vsmall)
      thProd = thProd + tEff*pow/eEff
   enddo
endif
if(nBoi.gt.0) then
   do i=is(iB),ie(iB)
      j = c_(i)
      calore = sp(j,i)*Pmax(i)*envCorr(t,i,4)
      if(pes(i).eq.'heat') then
         k     = eSource(i)
         l     = c_(k)
         pow   = sp(l,k)*Pmax(k)*envCorr(t,k,4)
         eEff  = etaEl(l,k)*envCorr(t,k,1)
         eEff  = max(eEff,vsmall)
         eDisp = pow*(1.0/eEff - 1.0)
         if(calore.gt.eDisp) calore = limitRecovery(i,eDisp,t)
      endif
      thProd = thProd + calore
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
!> P_{ch} = \sum_{Trig} \frac{sp(i)\cdot P_{max}(i)\cdot k_{env}}{\eta_{el}(i,sp(i))\cdot\alpha_{env}}\eta_{ch}(i,sp(i))\cdot\beta_{env} + \sum_{Chi} sp(i)\cdot P_{max}(i)\cdot\vartheta_{env}
!>\f]
!>where \f$sp(i)\f$ is the set point of the \f$i\f$'th  machine,
!>\f$\eta_{ch}\f$ and \f$\eta_{el}\f$ are the chilling and electrical efficiencies,
!>respectively, and \f$P_{max}(i)\f$ is its rated power; f$ k_{env} \f$, \f$ \alpha_{env} \f$, \f$ \beta_{env} \f$, and \f$ \vartheta_{env} \f$ are the environmental corrections for electrical power
!> and efficiency, chilling efficiency, and chilling power, respectively.
!>\param[in] c_  index of the given set-point to be given as input. Defines the state of the plant \f$sp(i) = sp(c\_(i))\f$
!>\param[in] t   time index
!>\author Andrea Facci

real(kind = prec) function chProd(c_,t)

!--Declare Module usage---
use plantVar
use inputVar

implicit none

!---Declare Local Variables---
integer,dimension(nm), intent(in) :: c_
integer              , intent(in) :: t
integer                           :: i,j
real(kind = prec)                  :: cEff, eEff, pow

!---Function Body

chProd = 0
if(nTrig.gt.0) then
   do i=is(iT),ie(iT)
      j = c_(i)
      cEff = etaCh(j,i)*envCorr(t,i,3)
      pow  = Pmax(i)*sp(j,i)*envCorr(t,i,4)
      eEff = etaEl(j,i)*envCorr(t,i,1)
      eEff = max(eEff,vsmall)
      chProd = chProd + pow*cEff/eEff
   enddo
endif
if(nChi.gt.0) then
   do i=is(iC),ie(iC)
      j = c_(i)
      chProd = chProd + sp(j,i)*Pmax(i)*envCorr(t,i,4)
   enddo
endif
return

end function chProd

!=======================================================================================================

!>\brief Internal thermal consumption of the power plant.
!>\details Calculates the thermal self-consumption of the trigeneration plant,
!>that is, hte thermal power needed by the absorbtion chillers.
!>\f[ 
!> U_{th}^{self} = \sum_{AbsChi} \frac{sp(i)\cdot P_{max}(i)\cdot \vartheta_{env}}{\eta_{ch}(i,sp(i))\cdot \beta_{env}}
!>\f]
!>where \f$sp(i)\f$ is the set point of the \f$i\f$'th  machine,
!>\f$\eta_{ch}\f$ is  the chilling efficiency, and \f$P_{max}(i)\f$ is its rated power; \f$\beta_{env}\f$ and \f$\vartheta_{env}\f$ are the environmental corrections for 
!> chilling efficiency, and chilling power, respectively.

!>\param[in] c_  index of the given set-point to be given as input. Defines the state of the plant \f$sp(i) = sp(c\_(i))\f$
!>\param[in] t   time index
!>\author Andrea Facci

real(kind = prec) function thSelfCons(c_,t)

!--Declare Module usage---
use plantVar
use inputVar

implicit none

!---Declare Local Variables---
integer,dimension(nm), intent(in) :: c_
integer,               intent(in) :: t
integer                           :: i,j 
real(kind = prec)                  :: pow, cEff

!---Function Body

!--note that only absorption refigerators are heat consumer so far.
thSelfCons = 0
if(nChi.gt.0) then
   do i=is(3),ie(3)
      if(pes(i).eq.'heat') then
         j = c_(i)
         pow  = sp(j,i)*Pmax(i)*envCorr(t,i,4)
         cEff = etaCh(j,i)*envCorr(t,i,3)
         cEff = max(cEff,vSmall)
         thSelfCons = thSelfCons + pow/cEff
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
!> U_{el}^{self} = \sum_{MecChi} \frac{sp(i)\cdot P_{max}(i)\cdot \vartheta_{env}}{\eta_{ch}(i,sp(i))\cdot \beta_{env}}
!>\f]
!>where \f$sp(i)\f$ is the set point of the \f$i\f$'th  machine,
!>\f$\eta_{ch}\f$ is  the chilling efficiency, and \f$P_{max}(i)\f$ is its rated power; \f$\beta_{env}\f$ and \f$\vartheta_{env}\f$ are the environmental corrections for 
!> chilling efficiency, and chilling power, respectively.
!>The summation is extended over the nmber of mechanical chillers.
!>\param[in] c_  index of the given set-point to be given as input. Defines the state of the plant \f$sp(i) = sp(c\_(i))\f$
!>\param[in] t   time index
!>\author Andrea Facci

real(kind = prec) function elSelfCons(c_,t)

!--Declare Module usage---
use plantVar
use inputVar

implicit none

!---Declare Local Variables---
integer,dimension(nm), intent(in) :: c_
integer                           :: t
integer                           :: i,j 
real(kind = prec)                  :: pow, cEff

!---Function Body

!--note that only electrical refigerators are electrcity consumer so far.
elSelfCons = 0
if(nChi.gt.0) then
   do i=is(3),ie(3)
      if(pes(i).eq.'elec') then
         j = c_(i)
         pow  = sp(j,i)*Pmax(i)*envCorr(t,i,4)
         cEff = etaCh(j,i)*envCorr(t,i,3)
         cEff = max(cEff,vSmall)
         elSelfCons = elSelfCons + pow/cEff
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
!> E_{in}(i) = \frac{sp(i)\cdot P_{max}(i)\cdot k_{env}}{\eta(i,sp(i))\cdot \alpha_{env}}
!>\f]
!>where \f$sp(i)\f$ is the set point of the \f$i\f$'th  machine,
!>\f$\eta(i,sp(i)) = \eta_{el}(i,sp(i))\f$ for trigenerative equipment and 
!>\f$\eta(i,sp(i))=\eta_{th}(i,sp(i))\f$ for boilers
!>and, and \f$P_{max}(i)\f$ is their rated power.
!> \f$k_{env}\f$ and \f$\alpha_{env}\f$ represent the environmental corrections
!> for power and efficiency respectively.
!>\param[in] c_  index of the given set-point to be given as input. Defines the state of the plant \f$sp(i) = sp(c\_(i))\f$
!>\param[in] t   time index
!>\author Andrea Facci

function fuelCons(c_,t)

!--Declare Module usage---
use plantVar
use inputVar

implicit none

!---Declare Local Variables---
real(kind = prec),dimension(nBoi+nTrig)     :: fuelCons
integer,                        intent(in) :: t
integer         ,dimension(nm), intent(in) :: c_
integer                                    :: i,j 
real(kind = prec)                           :: pow, eff

!---Function Body

fuelCons = zero

if(nTrig.gt.0) then
   do i=is(iT),ie(iT)
      j    = c_(i)
      pow  = sp(j,i)*Pmax(i)*envCorr(t,i,4)
      eff = etaEl(j,i)*envCorr(t,i,1)
      eff = max(eff,vsmall)
      fuelCons(i) = pow/eff
   enddo
endif

if(nBoi.gt.0) then
   do i=is(iB),ie(iB)
      j = c_(i)
      pow  = sp(j,i)*Pmax(i)*envCorr(t,i,4)
      eff = etaTh(j,i)*envCorr(t,i,2)
      eff = max(eff,vsmall)
      if(pes(i).eq.'fuel') fuelCons(i) = pow/eff
   enddo
endif

end function fuelCons

!===================================================================================

!>\brief Primary energy input
!>\details Calculates the primary energy input of the trigeneration plant,
!>for a given set-point
!>\f[ 
!> E_{in}(i) = \frac{sp(i)\cdot P_{max}(i)\cdot k_{env} }{\eta(i,sp(i))\cdot \alpha_{env}}
!>\f]
!>where \f$sp(i)\f$ is the set point of the \f$i\f$'th  machine,
!>\f$\eta(i,sp(i)) = \eta_{el}(i,sp(i))\f$ for trigenerative equipment and 
!>\f$\eta(i,sp(i))=\eta_{th}(i,sp(i))\f$ for boilers
!>and, and \f$P_{max}(i)\f$ is their rated power.
!> \f$k_{env}\f$ and \f$\alpha_{env}\f$ represent the environmental corrections
!>\param[in] c_  index of the given set-point to be given as input. Defines the state of the plant \f$sp(i) = sp(c\_(i))\f$
!>\param[in] t   time index
!>\author Andrea Facci

function energyInput(c_,t)

!--Declare Module usage---
use plantVar
use inputVar

implicit none

!---Declare Local Variables---
real(kind = prec),dimension(nm)             :: energyInput
integer,                        intent(in) :: t
integer         ,dimension(nm), intent(in) :: c_
integer                                    :: i,j 
real(kind = prec)                           :: pow, eff

!---Function Body

energyInput = zero

if(nTrig.gt.0) then
   do i=is(iT),ie(iT)
      j    = c_(i)
      pow  = sp(j,i)*Pmax(i)*envCorr(t,i,4)
      eff  = etaEl(j,i)*envCorr(t,i,1)
      eff = max(eff,vsmall)
      energyInput(i) = pow/eff
   enddo
endif

if(nBoi.gt.0) then
   do i=is(iB),ie(iB)
      j = c_(i)
      pow  = sp(j,i)*Pmax(i)*envCorr(t,i,4)
      eff  = etaTh(j,i)*envCorr(t,i,2)
      eff = max(eff,vsmall)
      energyInput(i) = pow/eff
   enddo
endif

if(nChi.gt.0) then
   do i=is(iC),ie(iC)
      j = c_(i)
      pow  = sp(j,i)*Pmax(i)*envCorr(t,i,4)
      eff  = etaCh(j,i)*envCorr(t,i,3)
      eff = max(eff,vsmall)
      energyInput(i) = pow/eff
   enddo
endif

end function energyInput

!===========================================================================
!>\brief Energy exhausted by combustions equipment.
!>\details Calculates the energy exhausted by trigenerative equipment and fuel boilers.
!>\f[ 
!> E_{exh}(i) = sp(i)\cdot P_{max}(i)\cdot k_{env} \left[\frac{1}{\eta(i,sp(i))\cdot \alpha_{env}} - 1 \right]
!>\f]
!>where \f$sp(i)\f$ is the set point of the \f$i\f$'th  machine,
!>\f$\eta(i,sp(i)) = \eta_{el}(i,sp(i))\f$ for trigenerative equipment and 
!>\f$\eta(i,sp(i))=\eta_{th}(i,sp(i))\f$ for boilers
!>and, and \f$P_{max}(i)\f$ is their rated power.
!> \f$k_{env}\f$ and \f$\alpha_{env}\f$ represent the environmental corrections
!>\param[in] c_  index of the given set-point to be given as input. Defines the state of the plant \f$sp(i) = sp(c\_(i))\f$
!>\param[in] t   time index
!>\author Andrea Facci
function energyExhaust(c_,t)

!--Declare Module usage---
use plantVar
use inputVar

implicit none

!---Declare Local Variables---
real(kind = prec),dimension(nTrig+nBoi)             :: energyExhaust
integer         ,dimension(nTrig+nBoi), intent(in) :: c_
integer                               , intent(in) :: t
integer                                    :: i,j 
real(kind = prec)                           :: pow, eff

!---Function Body

energyExhaust = zero

if(nTrig.gt.0) then
   do i=is(iT),ie(iT)
      j = c_(i)
      pow  = sp(i,j)*Pmax(i)*envCorr(t,i,4)
      eff  = etaEl(j,i)*envCorr(t,i,1)
      eff = max(eff,vsmall)
      energyExhaust(i) = pow*(1.0/eff - 1.0)
   enddo
endif

if(nBoi.gt.0) then
   do i=is(iB),ie(iB)
      j = c_(i)
      pow  = sp(i,j)*Pmax(i)*envCorr(t,i,4)
      eff  = etaTh(j,i)*envCorr(t,i,2)
      eff = max(eff,vsmall)
      energyExhaust(i) = pow*(1.0/eff - 1.0)
   enddo
endif

end function energyExhaust

!=====================================================================

!>\brief Limit the power of an HRSG.
!>\details Limit the power of an HRSG  according to the maximum input power coming
!> from the topping plant.
!>\param[in] i  index of the HRSG being considered
!>\param[in] t  time index
!>\param[in] valore maximum value of the input energy.
!>\author Andrea Facci

real(kind = prec) function limitRecovery(i,valore,t)


!--Declare Module usage---
use plantVar
use inputVar

implicit none

!---Declare Local Variables---
real(kind = prec), intent(in) :: valore
integer         , intent(in) :: i,t

real(kind = prec),allocatable, dimension(:) :: eIn, eOUt
real(kind = prec)              :: c, a, b, eTh
integer                       :: j,n,iBoi

!---Function Body

iBoi = i - is(iB) + 1
n = nSp(i)
allocate(eIn(n), eOut(n))
eIn = zero
eOut = zero

do j=1,n
   c = sp(j,i)
   eOut(j)= Pmax(i)*c*envCorr(t,i,4)         
   eTh = etaTh(j,i)*envCorr(t,i,2)
   eTh = max(eTh,vsmall)
   eIn(j) = eOut(j)/eTh
   if(eIn(j).ge.valore) exit
enddo

a = envCorr(t,i,2)*(etaTh(j,i) - etaTh(j-1,i))/(eOut(j) - eOut(j-1))
b = envCorr(t,i,2)*etaTh(j-1,i) - a*eOut(j-1)
limitRecovery = -b*valore/(a*Valore - 1.0)

deallocate(eIn, eOut)

end function limitRecovery

!==============================================================================

!>\brief Cogenerative Thermal production
!>\details Calculates the cogenerative Thermal production in kW of the whole power plant, for
!> a given set-point
!> Note that only trigeneration machines and Boilers produce thermal power so far. Thus
!> Thermal power is:
!>\f[ 
!> P_{th} = \sum_{Trig} \frac{sp(i)\cdot P_{max}(i)}{\eta_{el}(i,sp(i))}\eta_{th}(i,sp(i)) + \sum_{HRSG} sp(i)\cdot P_{max}(i)
!>\f]
!>where \f$sp(i)\f$ is the set point of the \f$i\f$'th  machine,
!>\f$\eta_{th}\f$ and \f$\eta_{el}\f$ are the thermal and electrical efficiencies,
!>respectively, and \f$P_{max}(i)\f$ is its rated power.
!>\param[in] c_  index of the given set-point to be given as input. Defines the state of the plant \f$sp(i) = sp(c\_(i))\f$
!>\param[in] t   time index
!>\author Andrea Facci

real(kind = prec) function cogThProd(c_,t)

!--Declare Module usage---
use plantVar
use inputVar

implicit none

!---Declare Local Variables---
integer,dimension(nm), intent(in) :: c_
integer              , intent(in) :: t
integer                           :: i,j,k,l
real(kind = prec)                 :: calore,eDisp,eEff,tEff,pow 

!---Function Body

cogthProd = zero
if(nTrig.gt.0) then
   do i=is(iT),ie(iT)
      j = c_(i)
      eEff = etaEl(j,i)*envCorr(t,i,1)
      tEff = etaTh(j,i)*envCorr(t,i,2)
      eEff = max(eEff,vsmall)
      pow  = sp(j,i)*Pmax(i)*envCorr(t,i,4)
      cogthProd = cogthProd + tEff*pow/eEff
   enddo
endif
if(nBoi.gt.0) then
   do i=is(iB),ie(iB)
      if(pes(i).eq.'heat') then
         j = c_(i)
         calore = sp(j,i)*Pmax(i)*envCorr(t,i,4)
         k     = eSource(i)
         l     = c_(k)
         pow   = sp(l,k)*Pmax(k)*envCorr(t,k,4)
         eEff  = etaEl(l,k)*envCorr(t,k,1)
         eEff  = max(eEff,vsmall)
         eDisp = pow*(1.0/eEff - 1.0)
         if(calore.gt.eDisp) calore = limitRecovery(i,eDisp,t)
         cogThProd = cogThProd + calore
      endif
   enddo
endif
return

end function cogThProd

!=======================================================================

real(kind=prec) function pec(c,t)

  use plantVar
  use inputVar , only : pefGrid, uEl

  implicit none

  integer, dimension(nm), intent(in) :: c
  integer,                intent(in) :: t
  integer                            :: i
  real(kind=prec), dimension(nm)     :: pin
  real(kind=prec)                    :: pGrid

  pec = zero

  pin = energyInput(c,t)

  do i=is(iT),ie(iT)
     pec = pec + pin(i)*pef(i)*dt(t)
  enddo

  do i=is(iB),ie(iB)
     pec = pec + pin(i)*pef(i)*dt(t)
  enddo
   
  pGrid = sum(uEl(t,:)) + elSelfCons(c,t) - elProd(c,t)
  
  pec = pec + pGrid*pefGrid*dt(t)

end function pec
!=======================================================================

real(kind=prec) function pecPenalty(cNew,cOld,t)

  use plantVar
  use inputVar , only : pefGrid, uEl

  implicit none

  integer, dimension(nm), intent(in) :: cNew, cOld
  integer,                intent(in) :: t
  integer                            :: i, j
  real(kind=prec), dimension(nm)     :: pin
  real(kind=prec)                    :: spNew, spOld

  pecPenalty = zero

  return 

  pin = energyInput(cNew,t)

  do i=is(iT),ie(iT)
     j = cNew(i)
     spNew = sp(j,i)
     j = cOld(i)
     spOld = sp(j,i)
     if(spNew.gt.zero.and.spOld.eq.zero) pecPenalty = pecPenalty +  pin(i)*pef(i)*dt(t)*pecOn(i)
  enddo

  do i=is(iB),ie(iB)
     j = cNew(i)
     spNew = sp(j,i)
     j = cOld(i)
     spOld = sp(j,i)
     if(spNew.gt.zero.and.spOld.eq.zero) pecPenalty = pecPenalty + pin(i)*pef(i)*dt(t)*pecOn(i)
  enddo

end function pecPenalty

end module energy
