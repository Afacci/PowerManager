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
      integer                     :: i, j, k, full
      real(kind=prec)             :: c, p, maxp, Heat

      !--------------------------------------------
      
      Heat = Heat_
      getSpBoi(:) = 1

      do i=1,nBoi
         j    = BoiPriority(i)
         maxp = pMax(j)*envCorr(t,j,2)
         full = maxval(sp(:,j))
         if(maxp.ge.Heat) then
            k = 0
            do 
               k = k + 1
               c = sp(k,j)
               p = c*maxp
               if (p.ge.heat) then
                   getSpBoi(i) = k
                   exit
               endif
            enddo
            exit
         else
            getSpBoi(i) = full
            heat = heat - maxp
         endif
      enddo

   end function getSpBoi


!============================================================        

   function getSpChi(Cold_,t)

      !--------------------------------------------
      implicit none
      
      integer, dimension(nChi)    :: getSpChi
      real(kind=prec), intent(in) :: Cold_
      integer,         intent(in) :: t
      integer                     :: i, j, k, full
      real(kind=prec)             :: c, p, maxp, Cold

      !--------------------------------------------
      
      Cold = Cold_
      getSpChi(:) = 1

      do i=1,nChi
         j    = ChiPriority(i)
         maxp = pMax(j)*envCorr(t,j,2)
         full = maxval(sp(:,j))
         if(maxp.ge.Cold) then
            k = 0
            do 
               k = k + 1
               c = sp(k,j)
               p = c*maxp
               if (p.ge.Cold) then
                   getSpChi(i) = k
                   exit
               endif
            enddo
            exit
         else
            getSpChi(i) = full
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
      integer                               :: i, j, k, full, nin, request
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
               maxp = pMax(j)*envCorr(t,j,4)
               full = maxval(sp(:,j))
               if(maxp.ge.Power) then
                  k = 0
                  do 
                     k = k + 1
                     c = sp(k,j)
                     p = c*maxp
                     if (p.ge.Power) then
                         getSpTrig(i) = k
                         exit
                     endif
                  enddo
                  exit
               else
                  getSpTrig(i) = full
                  Power = Power - maxp
               endif
            enddo

      case(2)

            do i=1,nTrig
               j    = TrigPriority(i)
               full = maxval(sp(:,j))
               eth  = etaTh(full,j)*envCorr(t,j,2)
               eel  = etaEl(full,j)*envCorr(t,j,1)
               eel  = max(eel, 0.0) 
               maxp = eth*pMax(j)*envCorr(t,j,4)/(eel)
               if(maxp.ge.Heat) then
                  k = 0
                  do 
                     k = k + 1
                     c = sp(k,j)
                     eth  = etaTh(k,j)*envCorr(t,j,2)
                     eel  = etaEl(k,j)*envCorr(t,j,1)
                     eel  = max(eel, 0.0) 
                     p = c*eth*envCorr(t,j,4)*pMax(j)/(eel)
                     if (p.ge.Heat) then
                         getSpTrig(i) = k
                         exit
                     endif
                  enddo
                  exit
               else
                  getSpTrig(i) = full
                  Power = Power - maxp
               endif
            enddo

      case(3)

            do i=1,nTrig
               j    = TrigPriority(i)
               full = maxval(sp(:,j))
               ech  = etach(full,j)*envCorr(t,j,3)
               eel  = etaEl(full,j)*envCorr(t,j,1)
               eel  = max(eel, 0.0) 
               maxp = ech*pMax(j)/eel
               if(maxp.ge.Cold) then
                  k = 0
                  do 
                     k = k + 1
                     c = sp(k,j)
                     ech  = etaCh(k,j)*envCorr(t,j,3)
                     eel  = etaEl(k,j)*envCorr(t,j,1)
                     eel  = max(eel, 0.0) 
                     p = c*ech*envCorr(t,j,4)*pMax(j)/(eel)
                     if (p.ge.Cold) then
                         getSpTrig(i) = k
                         exit
                     endif
                  enddo
                  exit
               else
                  getSpTrig(i) = full
                  Power = Power - maxp
               endif
            enddo

      end select

   end function getSpTrig

end module getSetPoint
