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
!> \file
!> \brief graph construction and minumum path.
!> 
!>  this file implements a module (graphTools) that contains all the routines
!>  necessary to build the graph representing the problem and find the minumum
!>  path across the graph. The graph is acyclic (no closed paths) and represented
!>  in topological ordering using a predecessor list.
!> \author 
!>  Andrea Facci.
!
!---------------------------------------------------------------------------
module graphTools

integer,                                       private :: nComb     !> number of combination of the given set point vectors
integer,                                       private :: nTvComb   !> number of combination of the time-constraints vectors.
real(kind(1.d0)), allocatable, dimension(:,:), private :: comb      !> array of all the possible set point indexes combinations (one combination per row)
real(kind(1.d0)), allocatable, dimension(:,:), private :: spVal      !> array of all the possible set point valiues combinations (one combination per row)
real(kind(1.d0)), allocatable, dimension(:,:), private :: tState    !> array of all the possible stime-constraints combinations (one combination per row)
integer         , allocatable, dimension(:,:), private :: pointLoad !> set point of each vertex of the graph
real(kind(1.d0)), allocatable, dimension(:)  , private :: pointCost !> weight associated to each vertex of the graph
integer         , allocatable, dimension(:)  , private :: nt        !> number of vertices for each time-step
real(kind(1.d0)), allocatable, dimension(:)  , private :: pointTime !> time-step index of each vertex
integer,          allocatable, dimension(:,:), private :: predList  !> Predecessor List
integer,          allocatable, dimension(:,:), private :: succList  !> Successor List
real(kind(1.d0)), allocatable, dimension(:,:), private :: predCost  !> weight of each arc in the predecessor list 
real(kind(1.d0)), allocatable, dimension(:,:), private :: succCost  !> weight of each arc in the successor list 
integer,          allocatable, dimension(:)  , private :: nPre      !> number of predecessors for each graph vertex
integer,          allocatable, dimension(:)  , private :: nSuc      !> number of successors for each graph vertex
integer                                      , private :: nPoint    !> vertex number
integer,          allocatable, dimension(:,:), private :: timePoint !> vertexes list for each time step

!===================================================================================================================

interface
    real(kind(1.d0)) function objFunction(c,t,obj)
        use plantVar, only : nm
        implicit none
        integer, dimension(nm), intent(in)  :: c
        integer,                 intent(in) :: t
        character(len=100),      intent(in) :: obj 
    end function objFunction
end interface

interface
    logical function constraints(c,t)
        use plantVar
        use interfaces
        use inputVar
        implicit none
        integer, dimension(nm), intent(in) :: c
        integer,                intent(in) :: t
    end function constraints
end interface

interface 
    logical function timeConstr(cindex,tState)
        use inputVar
        use plantVar
        implicit none
        integer, dimension(nm), intent(in) :: cindex
        real(kind(1.d0)), dimension(nm), intent(in) :: tState
    end function timeConstr
end interface

contains 
   !>\brief
   !> Generates all the combinations of the set points, staring from the
   !> set-point  vectors that are stored in the columns of "cm". 
   !>\details
   !> Generates all the combinations of the set points, staring from the
   !> set-point  vectors that are stored in the columns of "cm". Each row of
   !> the array "comb" represents a set-point of the plant. The total number of
   !> states of the plant is also returned in the variable "nComb"
   !> \author Andrea Facci
   subroutine allCombin(icm,dcm,imax,m,targ)
  
   !---Declare Module usage---
  
   implicit none
   integer,                            intent(in)                   :: m         !> number of vectors (input)
   integer,          dimension(m)             , intent(in)          :: imax      !> lenght of each set-point vector (input)
   integer,          dimension(maxval(imax),m), intent(in),optional :: icm       !> set-point vectors (input)
   real(kind(1.d0)), dimension(maxval(imax),m), intent(in),optional :: dcm       !> set-point vectors (input)
   character(len=100),                          intent(in),optional :: targ
   integer,          dimension(m)                                   :: j
   integer                                                          :: k, i, h, n
   logical                                                          :: fine
   character(len=7)                                                 :: tipo
   integer,         dimension(:,:), allocatable                     :: iComb_
   real(kind(1.d0)),dimension(:,:), allocatable                     :: dComb_

   if(present(dcm).and.present(icm)) then
      print*, '----Fatal error in "allCombin" call----'
      print*, ' olny one between dcm and icm optional '
      print*, '        variables can be present       '
      call abortexecution(-1)
   endif
   if(.not.present(dcm).and.(.not.present(icm))) then
      print*, '----Fatal error in "allCombin" call----'
      print*, '   one between dcm and icm optional    '
      print*, '     variables must be present         '
      call abortexecution(-1)
   endif

   n=maxval(imax)
   !---Declare Local Variables---
   k = n**m        ! maximum number of combinations. The actual number
                   ! will be lower, because m is the maximum number of 
                   ! set points.
   if(present(icm)) then 
      tipo = 'integer'
      allocate(icomb_(k,m))   
      icomb_(:,:) = -1.d0
   elseif(present(dcm)) then
      tipo = 'double '
      allocate(dcomb_(k,m))   
      dcomb_(:,:) = -1.d0
   endif
   !---generate the combinations.
   i = 0
   j(:) = 1
   do 
       fine = .true.
       i = i + 1
       do h = 1,m
          if(tipo.eq.'double ') dComb_(i,h) = dcm(j(h),h)
          if(tipo.eq.'integer') iComb_(i,h) = icm(j(h),h)
       enddo
       do h =m,1,-1
           if(j(h).lt.imax(h)) then
               j(h) = j(h) + 1
               fine = .false.
               exit
           else
               j(h) = 1
           endif
       enddo
       if(fine) exit
   enddo
   
   !---associate the generated combinations to the proper array.
   select case(targ)
      case('set-point')
         ncomb = i
         allocate(comb(nComb,m))
         comb(:,:)  = iComb_(1:nComb,:)
         deallocate(iComb_)
      case('time-constraints')
         nTvComb = i
         allocate(tState(nTvComb,m))
         tState(:,:) = dComb_(1:nTvComb,:)
         deallocate(dComb_)
      case('state')
         allocate(spVal(i,m))
         spVal(:,:) = dComb_(1:i,:)
         deallocate(dComb_)
   end select

   end subroutine allCombin
  
!================================================================================
   
   !>\brief
   !> Generates the graph veteces, starting from the array of the set-point
   !> combinations.
   !>\details
   !> Generates the graph veteces, starting from the array of the set-point
   !> combinations. For each time-step determines which plant state respects the
   !> energy (staitc) constraints, and associates them to a graph vertex. A
   !> weight, that accounts for the costs/revenues of operating the power plant
   !> from time-step t to t+1 at the vertex state, is also calculated for each vertex.
   !> The time-step relative to each vertex and the number of verteces for each
   !> time step are associated to "pointTime" and "nt" vectors respectively.
   !> Vertex 0 will be the starting point of the graph and vertex nPoint + 1 the
   !> arriving point
   !> \author Andrea Facci.
   subroutine graphPoints
  
   !---Declare Module usage---
  
   use interfaces
   use inputVar
   use plantVar
   use mathTools
  
   !---Declare Local Variables---
   implicit none
  
   real(kind(1.d0)), allocatable, dimension(:,:) :: cl_
   real(kind(1.d0)), allocatable, dimension(:)   :: cost_, time_
   integer         , allocatable, dimension(:)   :: startLoad 
   integer :: i,j,n, iStart
   integer , allocatable, dimension(:) :: load
   logical :: v, error
  
   allocate(load(nm),nt(0:nTime+1))
   n = nComb*nTime
   allocate(cost_(n),cl_(n,nm),time_(n))
   allocate(timePoint(0:nTime+1,nComb))
   timePoint(:,:) = 0
  
   nt(0)   = 1
   n = 0
   timePoint(0,1) = 0
   do i=1,nTime
      nt(i) = 0
      do j=1,nComb
         load = comb(j,:)
         v    = constraints(load,i)
         if(v) then
            n        = n + 1
            time_(n) = i
            timePoint(i,j) = n
            nt(i)    = nt(i) + 1
            cl_(n,:) = load
            cost_(n) = objFunction(load,i,obj)
         endif
      enddo
   enddo
   nt(nTime+1) = 1
   timePoint(nTime+1,1) = nPoint + 1 
   do i=1,nTime
      if(nt(i).eq.0) call abortExecution(18,i)
   enddo
  
   allocate(pointCost(0:n+1),pointLoad(0:n+1,nm),pointTime(0:n+1))
   allocate(startLoad(nm))
  
   iStart = locateRow(startPoint,spVal,nm,nComb,error)
   if(error) then
      call abortExecution(14)
   else
     startLoad = comb(iStart,:)
   endif
   nPoint           = n
   pointCost(0)     = 0.d0
   pointCost(1:n)   = cost_(1:n)
   pointCost(n+1)   = 0.d0 
   pointLoad(0,:)   = startLoad(:)
   pointLoad(1:n,:) = cl_(1:n,:)
   pointLoad(n+1,:) = startLoad(:)
   pointTime(0)     = 0
   pointTime(1:n)   = time_(1:n)
   pointTime(n+1)   = nTime + 1
  
   deallocate(load)
   deallocate(cost_,cl_,time_)
   deallocate(startLoad)
  
   return
  
   end subroutine graphPoints
  
!==============================================================================
   
   !>\brief
   !> Generates the grapsh arcs.
   !>\details
   !> Generates the grapsh arcs. Note that only vertex relative to consecutive
   !> time-steps are connected and that arcs are oriented in the direction of
   !> increasing time. Arcs are stored in the form of a predecessor list
   !> "predList", that associates to each node all its predecessors. A weight
   !> is assiciated to each element of the predecessor list, equal to the weight
   !> of the predecessor vertex plus a cost connected to the variation of state
   !> between the actual and predecessor state. 
   !> \f[
   !> arcCost(i,j) = pointCost(c(i)) + fireCost(i,j)
   !> \f]
   !> The number of predecessors for each vertex is asle stored in the "nPre(i)"
   !> array.
   subroutine graphArcs
  
   !---Declare Module usage---
  
   use inputVar
   use plantVar
   use economy
   use myArithmetic
   use cmdVar
  
   !---Declare Local Variables---
   implicit none
  
   integer :: i,j,t,n1,n2,ni,nf,k
   integer, parameter :: zero = 0.d0
   integer, allocatable, dimension(:) :: cNew, cOld
  
   allocate(nPre(nPoint+1), predCost(nPoint+1,nComb))
   allocate(nSuc(0:nPoint), succCost(0:nPoint,nComb))
   allocate(predList(0:nPoint+1,nComb))
   allocate(succList(0:nPoint+1,nComb))
  
   predList(:,:) = -1!inan(1)
   succList(:,:) = -1!inan(1)
   predCost(:,:) = rnan(1.d0)
   succCost(:,:) = rnan(1.d0)
   
   select case(method)
      case('Forward')
   !---detect the predecessors for each node---
         n1 = 1
         ni  = 0
         do t=1,nTime+1
            n2 = n1 + nt(t)   - 1
            nf = ni + nt(t-1) - 1 
            do i=n1,n2
               nPre(i) = nt(t-1)
               predList(i,1:nPre(i)) = (/ (k, k=ni,nf) /)
            enddo
            ni = nf + 1
            n1 = n2 + 1
         enddo
         !---associate the cost to each arc in the predecessor list---
         allocate(cNew(nm), cOld(nm))
         do i=1,nPoint+1
            cNew = pointLoad(i,:)
            do j=1,nPre(i)
               k = predList(i,j)
               cOld = pointLoad(k,:)
               predCost(i,j) = pointCost(k) + fireCost(cNew,cOld)
            enddo
         enddo

      case('Backward')
         !---detect the successors for each node---
         n1 = 0
         ni  = 1
         do t=0,nTime
            n2 = n1 + nt(t)   - 1
            nf = ni + nt(t+1) - 1 
            do i=n1,n2
               nSuc(i) = nt(t+1)
               succList(i,1:nSuc(i)) = (/ (k, k=ni,nf) /)
            enddo
            ni = nf + 1
            n1 = n2 + 1
         enddo
         !---associate the cost to each arc in the successor list---
         do i=0,nPoint
            cOld = pointLoad(i,:)
            do j=1,nSuc(i)
!               k = sucList(i,j)
               cNew = pointLoad(k,:)
               succCost(i,j) = pointCost(i) + fireCost(cNew,cOld)
            enddo
         enddo
       end select
  
   deallocate(cNew,cOld)

   if(.not.silent) then
      print*
      print*, '              ------------Graph report------------'
      print*, '              | Plant State number: ', nComb,   '|'
      print*, '              | Time-steps        : ', nTime,   '|'
      print*, '              | Verteces number   : ', nPoint,  '|'
      if(method.eq.'Forward') print*, '              | Arcs number       : ', sum(nPre(:)), '|'
      if(method.eq.'Backward') print*, '              | Arcs number       : ', sum(nSuc(:)), '|'
      print*, '              ------------------------------------'
      print*
   endif

   return
  
   end subroutine graphArcs

   !===================================================================================

   !>\brief 
   !> Miniumum path detemination.
   !>\details
   !> This function determies the minumum path that connects the start point (0)
   !> to the arrival point of the graph (nPoint + 1), using dynamic programming.
   !> Specifically the oprimizazion the algorithm is tailored to sort acyclic
   !> graphs with topolgical ordering.
   !>\author Andrea Facci.

   subroutine minPathTopoFw(ottLoad, minCost)
   
   use inputVar, only : nTime
   use plantVar, only : nm

   implicit none
    
   integer, dimension(0:nTime+1,nm), intent(out):: ottLoad
   real(kind(1.d0)),              intent(out)   :: minCost
   integer                                      :: orig, dest, i, j, p
   real(kind(1.d0))                             :: ci
   real(kind(1.d0)), allocatable, dimension(:)  :: pathCost
   integer         , allocatable, dimension(: ) :: minPred
   integer, dimension(0:nTime+1)                :: minPath

   orig = 0              
   dest = nPoint + 1
   allocate(pathCost(orig:dest), minPred(orig:dest))
   pathCost(orig+1:dest) = huge(1.d0)
   pathCost(orig)        = 0.d0
   minPred(orig:dest)    = 0

   do i=1,dest
      do j=1,nPre(i)
         p  = predList(i,j)
         ci = pathCost(p) + predCost(i,j)
         if(ci < pathCost(i)) then
            pathCost(i) = ci
            minPred(i)  = p
         endif
      enddo
   enddo
  
   minCost    = pathCost(dest)
   minPath(nTime+1) = dest
   ottLoad(nTime+1,:) = pointLoad(dest,:)
   p = dest
   j = nTime
   do while(p > orig)
      p = minPred(p)
      minPath(j) = p
      ottLoad(j,:) = pointLoad(p,:)
      j = j - 1
   enddo

   end subroutine minPathTopoFw

!=========================================================================================

   !>\brief 
   !> Miniumum path detemination.
   !>\details
   !> This function determies the minumum path that connects the start point (0)
   !> to the arrival point of the graph (nPoint + 1), using backward dynamic programming.
   !> Specifically the oprimizazion the algorithm is tailored to sort acyclic
   !> graphs with topolgical ordering. Constraints on the duration of operative
   !> intervals (on and off states) are considered.
   !>\author Andrea Facci.

   subroutine minPathTopoBw(ottLoad, minCost,upTime, minPath)
   
   use inputVar, only : nTime, upTime0, downTime0
   use plantVar, only : nm, minUpTime, minDownTime
   use mathTools

   implicit none
    
   integer, dimension(0:nTime+1,nm), intent(out):: ottLoad
   real(kind(1.d0)),              intent(out)   :: minCost
   integer                                      :: orig, dest, i, j, p, suc,t,k,ki
   real(kind(1.d0))                             :: ci
   real(kind(1.d0)), allocatable, dimension(:,:)  :: pathCost
   integer         , allocatable, dimension(:,:)  :: minSucc
   integer, dimension(0:nTime+1)                :: minPath
   logical                                      :: tv
   integer, dimension(nm)                       :: cn, co
   real(kind(1.d0)), dimension(0:nTime+1,2*nm), intent(out)       :: upTime 
   real(kind(1.d0)), dimension(2*nm)              :: upTimeIn, upTimeOut

   !----------function body-----------------------------------------------------

   orig = 0              
   dest = nPoint + 1
   allocate(pathCost(orig:dest,nTvComb), minSucc(orig:dest,nTvComb))
   pathCost(orig:dest,:)   = huge(1.d0)
   pathCost(dest,:)        = 0.d0
   minSucc(orig:dest,:)    = 0

   do i=nPoint,0,-1
      do k=1,nTvComb
         t         = pointTime(i)
         co        = pointLoad(i,:)
         upTimeIn  = tState(k,:)
         tv = timeConstr(co,upTimeIn)
         if(tv) then
            upTimeOut = upTimeCalc(upTimeIn,co,t)
            ki = locateRow(upTimeOut,tState,2*nm,nTvComb)
            do j=1,nSuc(i)
               suc = succList(i,j)
               ci  = pathCost(suc,ki) + succCost(i,j)
               cn  = pointLoad(suc,:)
               if(ci.lt.pathCost(i,k)) then
                   pathCost(i,k) = ci
                   minSucc(i,k)  = suc
               endif
            enddo
         endif
      enddo
   enddo

   minCost            = pathCost(orig,1)
   minPath(nTime+1)   = dest
   minPath(0)         = orig
   ottLoad(nTime+1,:) = pointLoad(dest,:)
   ottLoad(0,      :) = pointLoad(orig,:)
   p = orig 
   t = 0
   do i=1,nm
      j = i + nm
      upTime(0,i) = min(upTime0(i), minUpTime(i))
      upTime(0,j) = min(downTime0(i), minDownTime(i))
   enddo
   do while (p < dest)
      t = t + 1
      ki = locateRow(upTime(t-1,:),tState,2*nm,nTvComb)
      upTime(t,:) = upTimeCalc(upTime(t-1,:),pointLoad(p,:),t-1)
      p = minSucc(p,ki)
      minPath(t)  = p
      ottLoad(t,:)= pointLoad(p,:)
   enddo
   

   end subroutine minPathTopoBw

!========================================================================

  function upTimeCalc(upT,c,t)

     use plantVar, only : nm, sp, dt, minUpTime, minDownTime

     implicit none

     real(kind(1.d0)), dimension(2*nm)    :: upTimeCalc
     real(kind(1.d0)), dimension(2*nm)    :: upT
     integer,                intent(in) :: t
     integer, dimension(nm), intent(in) :: c
     integer                            :: i,j,k
     real(kind(1.d0))                   :: load

     !---function body----
     do i=1,nm
        j = c(i)
        k = i + nm
        load = sp(j,i)
        if(load.gt.0.d0) then 
           upTimeCalc(i) = upT(i) + dt(t)
           upTimeCalc(k) = 0.d0
        else   
           upTimeCalc(i) = 0.d0
           upTimeCalc(k) = upT(k) + dt(t)
        endif
        upTimeCalc(i) = min(upTimeCalc(i),minUpTime(i))       !limit the value of upTime
        upTimeCalc(k) = min(upTimeCalc(k),minDownTime(i))     !limit the value of downTime
     enddo
   
   return
  
   end function upTimeCalc

!========================================================================

end module graphTools
