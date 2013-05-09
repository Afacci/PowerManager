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

function correction()

!---Declare Module usage---

implicit none

!---Declare Local Variables---

!---Estimate environmental condition corrections for each time-step---
call allocateVar(32)

allocate(pCorr(nTime,nm,4), tCorr(nTime,nm,4), aCorr(nm,4))
pCorr = rNan(rVal)
tCorr = rNan(rVal)
aCorr = rNan(rVal)
do i=1,nTime
   k = 0
   do j=is(iT),ie(iT)
      k = k + 1
      rdummy2= interpolation(tempCorrT(:,1,k), tempCorrT(:,2,k), ntcT(k), tAmb(i),1)
      tCorr(i,j,1) = rdummy2(1)
      rdummy2= interpolation(tempCorrT(:,1,k), tempCorrT(:,3,k), ntcT(k), tAmb(i),1)
      tCorr(i,j,2) = rdummy2(1)
      rdummy2= interpolation(tempCorrT(:,1,k), tempCorrT(:,4,k), ntcT(k), tAmb(i),1)
      tCorr(i,j,3) = rdummy2(1)
      rdummy2= interpolation(tempCorrT(:,1,k), tempCorrT(:,5,k), ntcT(k), tAmb(i),1)
      tCorr(i,j,4) = rdummy2(1)

      rdummy2= interpolation(presCorrT(:,1,k), presCorrT(:,2,k), npcT(k), pAmb(i),1)
      pCorr(i,j,1) = rdummy2(1)
      rdummy2= interpolation(presCorrT(:,1,k), presCorrT(:,3,k), npcT(k), pAmb(i),1)
      pCorr(i,j,2) = rdummy2(1)
      rdummy2= interpolation(presCorrT(:,1,k), presCorrT(:,4,k), npcT(k), pAmb(i),1)
      pCorr(i,j,3) = rdummy2(1)
      rdummy2= interpolation(presCorrT(:,1,k), presCorrT(:,5,k), npcT(k), pAmb(i),1)
      pCorr(i,j,4) = rdummy2(1)
   enddo
   k = 0
   do j=is(iB),ie(iB)
      k = k + 1
      rdummy2= interpolation(tempCorrB(:,1,k), tempCorrB(:,2,k), ntcB(k), tAmb(i),1)
      tCorr(i,j,2) = rdummy2(1)
      rdummy2= interpolation(tempCorrB(:,1,k), tempCorrB(:,3,k), ntcB(k), tAmb(i),1)
      tCorr(i,j,4) = rdummy2(1)

      rdummy2= interpolation(presCorrB(:,1,k), presCorrB(:,2,k), npcB(k), pAmb(i),1)
      pCorr(i,j,2) = rdummy2(1)
      rdummy2= interpolation(presCorrB(:,1,k), presCorrB(:,3,k), npcB(k), pAmb(i),1)
      pCorr(i,j,4) = rdummy2(1)
   enddo
   k = 0
   do j=is(iC),ie(iC)
      k = k + 1
      rdummy2= interpolation(tempCorrC(:,1,k), tempCorrC(:,2,k), ntcC(k), tAmb(i),1)
      tCorr(i,j,3) = rdummy2(1)
      rdummy2= interpolation(tempCorrC(:,1,k), tempCorrC(:,3,k), ntcC(k), tAmb(i),1)
      tCorr(i,j,4) = rdummy2(1)

      rdummy2= interpolation(presCorrC(:,1,k), presCorrC(:,2,k), npcC(k), pAmb(i),1)
      pCorr(i,j,3) = rdummy2(1)
      rdummy2= interpolation(presCorrC(:,1,k), presCorrC(:,3,k), npcC(k), pAmb(i),1)
      pCorr(i,j,4) = rdummy2(1)
   enddo
enddo

k = 0
do j=is(iT),ie(iT)
   k = k + 1
   rdummy2= interpolation(altCorrT(:,1,k), altCorrT(:,2,k), nacT(k), altitude,1)
   aCorr(j,1) = rdummy2(1)
   rdummy2= interpolation(altCorrT(:,1,k), altCorrT(:,3,k), nacT(k), altitude,1)
   aCorr(j,2) = rdummy2(1)
   rdummy2= interpolation(altCorrT(:,1,k), altCorrT(:,4,k), nacT(k), altitude,1)
   aCorr(j,3) = rdummy2(1)
   rdummy2= interpolation(altCorrT(:,1,k), altCorrT(:,5,k), nacT(k), altitude,1)
   aCorr(j,4) = rdummy2(1)
enddo
k = 0
do j=is(iB),ie(iB)
   k = k + 1
   rdummy2= interpolation(altCorrB(:,1,k), altCorrB(:,2,k), nacB(k), altitude,1)
   aCorr(j,2) = rdummy2(1)
   rdummy2= interpolation(altCorrB(:,1,k), altCorrB(:,3,k), nacB(k), altitude,1)
   aCorr(j,4) = rdummy2(1)
enddo
k = 0
do j=is(iC),ie(iC)
   k = k + 1
   rdummy2= interpolation(altCorrC(:,1,k), altCorrC(:,2,k), nacC(k), altitude,1)
   aCorr(j,3) = rdummy2(1)
   rdummy2= interpolation(altCorrC(:,1,k), altCorrC(:,3,k), nacC(k), altitude,1)
   aCorr(j,4) = rdummy2(1)
enddo

envCorr = rNaN(rVal)
do i=1,nTime
   do j = 1,nm
      do k = 1,4 
         envCorr(i,j,k) = tCorr(i,j,k)*pCorr(i,j,k)*aCorr(j,k)
      enddo
   enddo
enddo
deallocate(aCorr,pCorr,tCorr)!---Estimate environmental condition corrections for each time-step---
call allocateVar(32)

allocate(pCorr(nTime,nm,4), tCorr(nTime,nm,4), aCorr(nm,4))
pCorr = rNan(rVal)
tCorr = rNan(rVal)
aCorr = rNan(rVal)
do i=1,nTime
   k = 0
   do j=is(iT),ie(iT)
      k = k + 1
      rdummy2= interpolation(tempCorrT(:,1,k), tempCorrT(:,2,k), ntcT(k), tAmb(i),1)
      tCorr(i,j,1) = rdummy2(1)
      rdummy2= interpolation(tempCorrT(:,1,k), tempCorrT(:,3,k), ntcT(k), tAmb(i),1)
      tCorr(i,j,2) = rdummy2(1)
      rdummy2= interpolation(tempCorrT(:,1,k), tempCorrT(:,4,k), ntcT(k), tAmb(i),1)
      tCorr(i,j,3) = rdummy2(1)
      rdummy2= interpolation(tempCorrT(:,1,k), tempCorrT(:,5,k), ntcT(k), tAmb(i),1)
      tCorr(i,j,4) = rdummy2(1)

      rdummy2= interpolation(presCorrT(:,1,k), presCorrT(:,2,k), npcT(k), pAmb(i),1)
      pCorr(i,j,1) = rdummy2(1)
      rdummy2= interpolation(presCorrT(:,1,k), presCorrT(:,3,k), npcT(k), pAmb(i),1)
      pCorr(i,j,2) = rdummy2(1)
      rdummy2= interpolation(presCorrT(:,1,k), presCorrT(:,4,k), npcT(k), pAmb(i),1)
      pCorr(i,j,3) = rdummy2(1)
      rdummy2= interpolation(presCorrT(:,1,k), presCorrT(:,5,k), npcT(k), pAmb(i),1)
      pCorr(i,j,4) = rdummy2(1)
   enddo
   k = 0
   do j=is(iB),ie(iB)
      k = k + 1
      rdummy2= interpolation(tempCorrB(:,1,k), tempCorrB(:,2,k), ntcB(k), tAmb(i),1)
      tCorr(i,j,2) = rdummy2(1)
      rdummy2= interpolation(tempCorrB(:,1,k), tempCorrB(:,3,k), ntcB(k), tAmb(i),1)
      tCorr(i,j,4) = rdummy2(1)

      rdummy2= interpolation(presCorrB(:,1,k), presCorrB(:,2,k), npcB(k), pAmb(i),1)
      pCorr(i,j,2) = rdummy2(1)
      rdummy2= interpolation(presCorrB(:,1,k), presCorrB(:,3,k), npcB(k), pAmb(i),1)
      pCorr(i,j,4) = rdummy2(1)
   enddo
   k = 0
   do j=is(iC),ie(iC)
      k = k + 1
      rdummy2= interpolation(tempCorrC(:,1,k), tempCorrC(:,2,k), ntcC(k), tAmb(i),1)
      tCorr(i,j,3) = rdummy2(1)
      rdummy2= interpolation(tempCorrC(:,1,k), tempCorrC(:,3,k), ntcC(k), tAmb(i),1)
      tCorr(i,j,4) = rdummy2(1)

      rdummy2= interpolation(presCorrC(:,1,k), presCorrC(:,2,k), npcC(k), pAmb(i),1)
      pCorr(i,j,3) = rdummy2(1)
      rdummy2= interpolation(presCorrC(:,1,k), presCorrC(:,3,k), npcC(k), pAmb(i),1)
      pCorr(i,j,4) = rdummy2(1)
   enddo
enddo

k = 0
do j=is(iT),ie(iT)
   k = k + 1
   rdummy2= interpolation(altCorrT(:,1,k), altCorrT(:,2,k), nacT(k), altitude,1)
   aCorr(j,1) = rdummy2(1)
   rdummy2= interpolation(altCorrT(:,1,k), altCorrT(:,3,k), nacT(k), altitude,1)
   aCorr(j,2) = rdummy2(1)
   rdummy2= interpolation(altCorrT(:,1,k), altCorrT(:,4,k), nacT(k), altitude,1)
   aCorr(j,3) = rdummy2(1)
   rdummy2= interpolation(altCorrT(:,1,k), altCorrT(:,5,k), nacT(k), altitude,1)
   aCorr(j,4) = rdummy2(1)
enddo
k = 0
do j=is(iB),ie(iB)
   k = k + 1
   rdummy2= interpolation(altCorrB(:,1,k), altCorrB(:,2,k), nacB(k), altitude,1)
   aCorr(j,2) = rdummy2(1)
   rdummy2= interpolation(altCorrB(:,1,k), altCorrB(:,3,k), nacB(k), altitude,1)
   aCorr(j,4) = rdummy2(1)
enddo
k = 0
do j=is(iC),ie(iC)
   k = k + 1
   rdummy2= interpolation(altCorrC(:,1,k), altCorrC(:,2,k), nacC(k), altitude,1)
   aCorr(j,3) = rdummy2(1)
   rdummy2= interpolation(altCorrC(:,1,k), altCorrC(:,3,k), nacC(k), altitude,1)
   aCorr(j,4) = rdummy2(1)
enddo

envCorr = rNaN(rVal)
do i=1,nTime
   do j = 1,nm
      do k = 1,4 
         envCorr(i,j,k) = tCorr(i,j,k)*pCorr(i,j,k)*aCorr(j,k)
      enddo
   enddo
enddo
deallocate(aCorr,pCorr,tCorr)

end fucntion correction 
