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

module predStrategies

  use shared
  use inputVar
  use plantVar
  
contains

  function thermalGuide(dummy)

  !---Declare Module usage---

   implicit none

   !---Declare Local Variables---
   real(kind=prec), dimension(0:nTime+1,nm) :: thermalGuide
   logical, optional, intent(in) :: dummy
   real(kind=prec) :: tload, cload, eff, utc
   integer  :: t,i, nac, j, ptm
   integer, dimension(nChi) :: iac
   real(kind=prec), allocatable, dimension(:,:) :: val, kc, kb, kt, kcm
   
   nac = 0
   do i=is(iC),ie(ic)
      if(tecC(i).eq.'Absorption') then
         nac = nac + 1
         iac(nac) = i
      endif
   enddo
   allocate(kc(nca,1), kb(nBoi), kt(nTrig), kcm(nChi))
   kb = zero
   kt = zero
   
   do t=1,nTime
      tload = zero
      cload = zero
      do i=1,nTh
         tload = Uth(t,i) + tload
      enddo
      do i=1,nch
         cload = Uch(t,i) + cload
      enddo
      utc = zero
      do i=1,nac
         j = iac(i)
         jj = iac(i) - is(ic) + 1
         pcm = Pmax(j)*envCorr(t,j,1)
         if(cload.le.pcm) then
            kc(i,1) = cload/pcm
            eff = interpolation(etaC(:,1,jj), etaC(:,2,jj), nEtaC(jj), kc(i,:),1)
            utc = utc + cload/eff
            cload = 0.0
            exit
         else
            kc(i) = 1.0
            cload = cload - pcm
            eff = interpolation(etaC(:,1,jj), etaC(:,2,jj), nEtaC(jj), kc(i,:),1)
            utc = utc + pcm/eff
         endif
      enddo
      tload = tload + utc
      if (cload.gt.0.0) then
         j = 0
         do i=is(iC),ie(iC)
            j = j + 1 
            pcm = Pmax(i)
            if(ccload.le.ptb) then
               kcm(j,1) = tload/pcm
               cload = 0.0
               exit
            else
               kcm(j,1) = 1.0
               cload = cload - pcm
           endif
         enddo
      endif
      do i=1, nTrig
         ptm = pMax(i)
         if(tload.le.ptm) then
            kt(i) = trigThSp(tload, i) 
            tload = 0.0
            exit
         else
           kt(i) = 1.0
           tload = tload - ptm
         endif
      enddo
      if(tload.gt.0.0) then
         j = 0
         do i=is(iB), ie(iB)
            j = j + 1 
            ptb = Pmax(i)
            if(tload.le.ptb) then
               kb(j,1) = tload/ptb
               tload = 0.0
               exit
            else
               kb(j,1) = 1.0
               tload = tload - ptb
            endif
         enddo
      endif
   enddo

   deallocate(kc)

end function thermalGuide

!======================================================================0

real(kind=prec)  function trigThSp(heat,i)

  implicit none
  real(kind=prec), intent(in) :: heat
  integer, intent(in)         :: i
  real(kind=prec), allocatable, dimension(:) :: ein, eth, eff, effT
  real(kind=prec) :: c, pm, err, rhs, ee, et, eOut
  real(kind=prec), dimension(1) :: val
  real(kind=prec), parameter :: toll = 1.0e-2

  n = nEtaElT(i)
  allocate(eIn(n), eff(n), effT(n))
  eIn = zero
  eOut = zero

  pm = pMax(i)*envCorr(t,i,4)         
  do j=1,n
     c = etaElT(j,1,i)
     eOut = pm*c
     eff(j) = etaElT(j,2,i)*envCorr(t,i,2)
     eff(j) = max(eTh,vsmall)
     eIn(j) = eOut/eff(j)
     val(1) = c
     effT(j) = interpolation(etaThT(:,1,i), etaThT(:,2,i), nEtaThT(i), val ,1) 
     eTh(j) = eIn(j)*effT
     if(eTh(j).ge.heat) exit
  enddo

  eEl(1) = eff(j-1)
  eEl(2) = eff(j)
  eTh(1) = effT(j-1)
  eTh(2) = effT(j)
  cc(1) = etaElT(j-1,1,i)
  cc(2) = etaElT(j,1,i)

  k(1) = 0.5*(c1 + c2)

  do
     ee = interpolation(cc, eEl, 2, k, 1)
     et = interpolation(cc, eTh, 2, k, 1)
     rhs = et*k*pm/ee
     err = abs(heat - rhs)/heat
     if(err.le.toll) exit
     if(rhs.gt.heat) then
        k = 0.5*(k + cc(1))
     else
        k = 0.5*(k + cc(2))
     endif
  enddo

  trigThSp = k

  deallocate(eIn, eTh)
end module predStrategies
