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

module getSetPoint

use shared
use inputVar
use plantVar

contains

!============================================================        

   function getSpBoi(Heat_, t)

      !--------------------------------------------
      implicit none
      
      integer, dimension(nBoi)    :: getSpBoi
      real(kind=prec), intent(in) :: Heat_
      integer,         intent(in) :: t
      integer                     :: i, j, k, full, ii
      real(kind=prec)             :: c, p, maxp, Heat

      !--------------------------------------------
      
      Heat = Heat_
      getSpBoi(:) = 1

      do i=1,nBoi
         j    = BoiPriority(i)
         ii   = j - is(iB) + 1
         maxp = pMax(j)*envCorr(t,j,4)
!         full = maxval(sp(:,j))
         full = nSp(j)
         if(maxp.ge.Heat) then
            k = 0
            do 
               k = k + 1
               c = sp(k,j)
               p = c*maxp
               if (p.ge.heat) then
                   getSpBoi(ii) = k
                   exit
               endif
            enddo
            exit
         else
            getSpBoi(ii) = full
            heat = heat - maxp
         endif
      enddo

   end function getSpBoi


!============================================================        

   function getSpChi(Cold_,hlimit_,t)

      !--------------------------------------------
      implicit none
      
      integer, dimension(nChi)    :: getSpChi
      real(kind=prec), intent(in) :: Cold_
      real(kind=prec), intent(in), optional :: hlimit_
      integer,         intent(in) :: t
      integer                     :: i, j, k, full, ii
      real(kind=prec)             :: c, p, maxp, Cold, ech, hmax, echf, &
                                     pin, hlimit, cmax
      integer, dimension(nChi)    :: spmax

      !--------------------------------------------
      
      Cold = Cold_
      getSpChi(:) = 1

      do i=1,nChi
         j = i + is(iC) - 1
         spmax(i) = nSp(j)
      enddo


      if(present(hlimit_)) then
         hlimit  = hlimit_
         do i=1,nChi
            j    = ChiPriority(i)
            ii   = j - is(iC) + 1
            if(pes(j).eq.'heat') then
               full = nSp(j)
               echf = etaCh(full,j)*envCorr(t,j,3)
               echf = max(echf,vsmall)
               hmax = pMax(j)*envCorr(t,j,4)/echf
               if(hmax.lt.hlimit) then
                  spmax(i) = full
                  hlimit = hlimit - hmax
               else 
                  k = 0
                  do 
                     k = k + 1
                     c = sp(k,j)
                     ech  = etaCh(k,j)*envCorr(t,j,3)
                     ech  = max(ech,vsmall)
                     pin  = c*pMax(j)*envCorr(t,j,4)/ech
                     if (pin.ge.hlimit) then
                         spmax(ii) = k - 1
                         exit
                     endif
                  enddo
               endif
            endif
         enddo
      endif

      do i=1,nChi
         j    = ChiPriority(i)
         ii   = j - is(iC) + 1
         full = spmax(ii)
         cmax = sp(full,j)
         maxp = cmax*pMax(j)*envCorr(t,j,4)
!         full = maxval(sp(:,j))
         if(maxp.gt.Cold) then
            k = 0
            do 
               k = k + 1
               c = sp(k,j)
               p = c*pMax(j)*envCorr(t,j,4)
               if (p.ge.Cold) then
                   getSpChi(ii) = k
                   exit
               endif
            enddo
            exit
         else
            getSpChi(ii) = full
            Cold = Cold - maxp
         endif
      enddo

   end function getSpChi

!============================================================        

   function getSpTrig(Power_,Heat_,Cold_,t)

      !--------------------------------------------
      implicit none
      
      integer, dimension(nTrig)             :: getSpTrig
      real(kind=prec), intent(in), optional :: Power_, Heat_, Cold_
      integer,         intent(in)           :: t
      integer                               :: i, j, k, full, nin, request, ii, ifull
      real(kind=prec)                       :: c, p, maxp, Power, Heat, Cold &
                                               , eth, ech, eel

      !--------------------------------------------
      
      getSpTrig(:) = 1

      nin = 0
      if(present(Power_)) then
           nin     = nin + 1
           request = 1
           Power = Power_
      endif
      if(present(Heat_))  then 
           nin     = nin + 1
           request = 2
           Heat = Heat_
      endif
      if(present(Cold_))  then 
           nin     = nin + 1
           request = 3
           Cold = Cold_
      endif

      if(nin.le.0) then
         print*,'***********FATAL ERROR IN getSpTrig CALL********'
         print*,'          one input energy paramete needed    '
         print*,'***********FATAL ERROR IN getSpTrig CALL********'
         stop
      endif
      if(nin.gt.1) then
         print*,'***********FATAL ERROR IN getSpTrig CALL********'
         print*,'              too many inputs provided    '
         print*,'***********FATAL ERROR IN getSpTrig CALL********'
         stop
      endif

      select case(request)
      
      case(1)

            do i=1,nTrig
               j    = TrigPriority(i)
               ii   = j - is(iT) + 1
               maxp = pMax(j)*envCorr(t,j,4)
               !full = maxval(sp(:,j))
               full = nSp(j)
               if(maxp.ge.Power) then
                  k = 0
                  do 
                     k = k + 1
                     c = sp(k,j)
                     p = c*maxp
                     if (p.ge.Power) then
                         getSpTrig(ii) = k
                         exit
                     endif
                  enddo
                  exit
               else
                  getSpTrig(ii) = full
                  Power = Power - maxp
               endif
            enddo

      case(2)

            do i=1,nTrig
               j    = TrigPriority(i)
               ii   = j - is(iT) + 1
               !full = maxval(sp(:,j))
               full = nSp(j)
               eth  = etaTh(full,j)*envCorr(t,j,2)
               eel  = etaEl(full,j)*envCorr(t,j,1)
               eel  = max(eel, vsmall) 
               maxp = eth*pMax(j)*envCorr(t,j,4)/(eel)
               if(maxp.gt.Heat) then
                  k = 0
                  do 
                     k = k + 1
                     c = sp(k,j)
                     eth  = etaTh(k,j)*envCorr(t,j,2)
                     eel  = etaEl(k,j)*envCorr(t,j,1)
                     eel  = max(eel, vsmall) 
                     p = c*eth*envCorr(t,j,4)*pMax(j)/(eel)
                     if (p.ge.Heat) then
                         getSpTrig(ii) = k
                         exit
                     endif
                  enddo
                  exit
               else
                  getSpTrig(ii) = full
                  Power = Power - maxp
               endif
            enddo

      case(3)

            do i=1,nTrig
               j    = TrigPriority(i)
               ii   = j - is(iT) + 1
               full= nSp(j)
               !full = maxval(sp(:,j))
               ech  = etach(full,j)*envCorr(t,j,3)
               eel  = etaEl(full,j)*envCorr(t,j,1)
               eel  = max(eel, vsmall) 
               maxp = ech*pMax(j)/eel
               if(maxp.ge.Cold) then
                  k = 0
                  do 
                     k = k + 1
                     c = sp(k,j)
                     ech  = etaCh(k,j)*envCorr(t,j,3)
                     eel  = etaEl(k,j)*envCorr(t,j,1)
                     eel  = max(eel, vsmall) 
                     p = c*ech*envCorr(t,j,4)*pMax(j)/(eel)
                     if (p.ge.Cold) then
                         getSpTrig(ii) = k
                         exit
                     endif
                  enddo
                  exit
               else
                  getSpTrig(ii) = full
                  Power = Power - maxp
               endif
            enddo

      end select

   end function getSpTrig

end module getSetPoint
