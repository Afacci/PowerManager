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
!>\brief Rule-based strategies.
!>\details This module contains all procedures to define the plant behavior
!> under rule-based regulation strategies. Thermal tracking, electrical tracking
!> and full load are considered.

!>\author Andrea Facci

module strategies

use shared
use inputvar
use plantvar
use energy
use graphTools
use getSetPoint
use mathTools

contains
  
   !============================================================
   function thermalTracking()

      !------------------------------------------------------------- 
      implicit none

      integer, dimension(0:nTime+1,nm) :: thermalTracking
      integer, dimension(nChi)         :: kc
      integer, dimension(nBoi)         :: kb
      integer, dimension(ntrig)        :: kt
      real(kind=prec)                  :: power, heat, cold, tSelf, eSelf, qmax
      integer                          :: t, i, j, istart
      integer, dimension(nm)           :: kk
      logical                          :: error
      !-------------------------------------------------------------

      iStart = locateRow(startPoint,spVal,nm,nComb,error)
      if(error) then
         call abortExecution(14)
      else
         thermalTracking(0,:)       = comb(istart,:)
         thermalTracking(nTime+1,:) = comb(istart,:)
      endif
      
      do t=1,nTime
         kk    = nSp
         qmax  = cogThProd(kk, t) - sum(uTh(t,:))! + pmax Accumuli.
         cold  = sum(uCh(t,:))
         kc    = getSpChi(cold,t=t,hlimit_=qmax)
         kk(:) = 1
         kk(is(ic):ie(ic)) = kc
         tSelf = thSelfCons(kk,t)
         eSelf = elSelfCons(kk,t)
         heat  = sum(uTh(t,:)) + tSelf
         kt    = getSpTrig(Heat_=heat,t=t)
         kk(:) = 1
         kk(is(iT):ie(iT)) = kt
         heat  = heat - thProd(kk,t)
         kb    = getSpBoi(heat,t)
         thermalTracking(t,is(iT):ie(iT)) = kt(:)
         thermalTracking(t,is(iB):ie(iB)) = kb(:)
         thermalTracking(t,is(iC):ie(iC)) = kc(:)
      enddo
   end function thermalTracking

   !============================================================
   
   function electricalTracking()

      !------------------------------------------------------------- 
      implicit none

      integer, dimension(0:nTime+1,nm) :: electricalTracking
      integer, dimension(nChi)         :: kc
      integer, dimension(nBoi)         :: kb
      integer, dimension(ntrig)        :: kt
      real(kind=prec)                  :: power, heat, cold, tSelf, eSelf, qmax
      integer                          :: t, i, j, istart
      integer, dimension(nm)           :: kk
      logical                          :: error
      !-------------------------------------------------------------

      iStart = locateRow(startPoint,spVal,nm,nComb,error)
      if(error) then
         call abortExecution(14)
      else
         electricalTracking(0,:)       = comb(istart,:)
         electricalTracking(nTime+1,:) = comb(istart,:)
      endif
      
      do t=1,nTime
         kk    = nSp
         qmax  = cogThProd(kk, t) - sum(uTh(t,:))
         cold  = sum(uCh(t,:))
         kc    = getSpChi(cold,t=t,hlimit_=qmax)
         kk(:) = 1
         kk(is(ic):ie(ic)) = kc
         tSelf = thSelfCons(kk,t)
         eSelf = elSelfCons(kk,t)
         power =  sum(uEl(t,:)) + eSelf
         kt    = getSpTrig(Power_=power,t=t)
         kk(:) = 1
         kk(is(iT):ie(iT)) = kt
         heat  = sum(uTh(t,:)) + tself - thProd(kk,t)
         kb    = getSpBoi(heat,t)
         electricalTracking(t,is(iT):ie(iT)) = kt(:)
         electricalTracking(t,is(iB):ie(iB)) = kb(:)
         electricalTracking(t,is(iC):ie(iC)) = kc(:)
      enddo

   end function ElectricalTracking


   !============================================================
   function fullLoad()

      !------------------------------------------------------------- 
      implicit none

      integer, dimension(0:nTime+1,nm) :: fullLoad
      integer, dimension(nChi)         :: kc
      integer, dimension(nBoi)         :: kb
      integer, dimension(ntrig)        :: kt
      real(kind=prec)                  :: power, heat, cold, tSelf, eSelf, qmax, heatSto
      integer                          :: t, i, j, istart
      integer, dimension(nm)           :: kk, kk0
      logical                          :: error
      real(kind=prec), dimension(0:nTime+1) :: thLevel
      real(kind=prec), dimension(2) :: maxTes
      !-------------------------------------------------------------

      iStart = locateRow(startPoint,spVal,nm,nComb,error)
      if(error) then
         call abortExecution(14)
      else
         fullLoad(0,:)       = comb(istart,:)
         fullLoad(nTime+1,:) = comb(istart,:)
      endif
      thLevel(0) = iSocTh

      kk0 = 1
      kk0(is(iTs)) = nSpTs + 1

      do t=1,nTime
         kk = kk0
         kk(is(iT):ie(iT)) = preset(t,:)! cPred(t,:) !nSp(is(iT):ie(iT))
         maxTes = thStoragePmax(thLevel(t-1),t,.true.)
         qmax  = cogThProd(kk, t) - sum(uTh(t,:)) + maxTes(1)
         qmax = max(qmax,zero)
         cold  = sum(uCh(t,:))
         kc = getSpChi(cold,t=t,hlimit_=qmax)
         kk(is(ic):ie(ic)) = kc
         tSelf = thSelfCons(kk,t)
         heat  = sum(uTh(t,:)) + tSelf - cogThProd(kk,t)
         heatSto = min(heat, maxTes(1))
         heatSto = max(heatSto, maxTes(2))
         kk(is(iTS)) = getSpTes(heatSto,t)
         heat =  heat - tesPower(kk,t)
         if(heat.gt.zero) kk(is(iB):ie(iB)) = getSpBoi(heat,t)
         fullLoad(t,:) = kk
         thLevel(t) = thStorageLevelUpdate(thLevel(t-1),kk,t)
      enddo
   end function fullLoad

   !============================================================

end module strategies
