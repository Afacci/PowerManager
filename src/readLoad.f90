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
!>\file readLoad.f90
!>\brief Reads Load.inp file
!>\author 
!>     Andrea Facci.
!
!---------------------------------------------------------------------------

!>\brief Reads Load.inp file
!>\details 
!> This subroutine reads the file "Load.inp". The procedure looks for each specific entry
!> in the "keyword field", and associates the value in the "value" field, to the
!> corresponding variable. If the desired entry is not present returns an error
!> message and aborts the execution. The structure of the input file is
!> clarified in the following example along with the meaning of "keyword" and
!> "value" field.\n
!> \verbatim
!> A Lot of usless stuff because only text between begin and end is read.
!>    begin 
!>       !Commented line
!>       KeywordField  | ValueField | !Comment two
!>
!>       KeywordField  | ValueField |
!>       ScalarValue   | 1          |
!>       Vector        | 1 2 n      |
!>       VectorSeries  |(a b c ) (d e f)|
!>       Matrix        |(11 12 13  |
!>                     | 21 22 23  |
!>                     | 31 32 33) |
!>    end
!> A lof really usless text
!>\endverbatim
!> Note that only the text between "begin" and "end" is read. Blank lines are
!> automatically discarded, while any unrecognized entry is discarded returning
!> a warning code. Line beginning with "!" are considered comments and
!> discarded.
!>\author 
!>     Andrea Facci.

subroutine readLoads

!---Declare Unit usage---
use inputVar
use fileTools
use interfaces

implicit none

!---Declare Local Variables---
integer              :: genUnit = 109, idummy
character(len=20)    :: inputFile = './Input/Loads.inp'
logical              :: filePresent
character(len=100)   :: buffer, keyword, value
integer              :: firstLine, i, n2, j, ntest, k, nmax, n3, n4, nPrice &
                        , iBuy, iSell
logical,dimension(4) :: isPresent = .false.
character(len=100)   :: line
character(len=100), allocatable, dimension(:)     :: matrix
character(len=100), allocatable, dimension(:,:)   :: theLoads, thePrices
character(len=100),              dimension(1,3)   :: grids
real(kind(1.d0)),   allocatable, dimension(:,:,:) :: loads_, prices_
real(kind(1.d0)),   allocatable, dimension(:,:) :: gridPrices

!---Check File Presence---
call openUnit(inputFile,genUnit,filePresent)
if(.not.filePresent) call abortExecution(1,5)
!---Skip all the lines before the keyword "begin".
firstLine = 0;
do 
    read(genUnit,100) buffer
    buffer = adjustl(buffer)
    firstLine = firstLine + 1
    if(buffer(1:5).eq.'begin') exit
enddo

!---Reading "Load" entry
buffer   = 'Loads'
call findEntry(buffer,genUnit,.false.,buffer,isPresent(1),idummy)
nLoad    = hCount(buffer)
allocate(theLoads(1,nLoad))
call rewUnit(genUnit,1)
theLoads = cMatrixRead(theUnit=genUnit, nline=1, nCol = nLoad)
nTime    = vCount(theUnit=genUnit,rew_=.true.,first_='[',last_=']')
do i=1,nLoad
   buffer = theLoads(1,i)
   select case(buffer)
       case('Time')
           iTime = i
           if(iTime.ne.1) call abortExecution(8)
       case('Electricity')
           iEl   = i - 1
       case('Thermal')
           iTh   = i - 1 
       case('Chilling')
           iCh   = i - 1
   end select
end do
allocate(matrix(nTime))
allocate(nld(nLoad-1))
matrix = matrixRead(genUnit,nTime, '[', ']')
buffer = adjustl(matrix(1))
i      = index(buffer, ' ') + 1
buffer = adjustl(buffer(i:))
do i=1, nLoad-1
   nld(i) = hCount(buffer)
   n2     = index(buffer,')') + 1
   buffer =  buffer(n2 + 2:)
enddo

nmax = maxval(nld)
allocate(loads_(nTime,nLoad,nmax))
loads_(:,:,:)  = -1
call allocateVar(15)

do i=1,nTime
   buffer = adjustl(matrix(i))
   read(buffer,*) time(i)
   n2     = index(buffer,' ') + 1
   buffer =  buffer(n2:)
   do j=1,nLoad-1
      ntest  = hCount(buffer)
      if(ntest.ne.nld(j)) call abortExecution(9,j,r1=time(i))
      n3 = index(buffer,'(')
      n4 = index(buffer,')')
      value  = buffer(n3+1:n4-1) 
      read(value,*) (loads_(i,j,k), k=1,nld(j))
      buffer = buffer(n4+1:)
   enddo
enddo
uEl(:,:) = Loads_(:,iEl,1:nld(iEl))
uTh(:,:) = Loads_(:,iTh,1:nld(iTh))
uCh(:,:) = Loads_(:,iCh,1:nld(iCh))


!Read Prices entry

buffer   = 'Prices'
call findEntry(buffer,genUnit,.false.,buffer,isPresent(1),idummy)
nPrice    = hCount(buffer)
if(nPrice.ne.nLoad) call abortExecution(10)
allocate(thePrices(1,nLoad))
call rewUnit(genUnit,1)
thePrices = cMatrixRead(theUnit=genUnit, nline=1, nCol = nLoad)
nTime    = vCount(theUnit=genUnit,rew_=.true.,first_='[',last_=']')
do i=1,nLoad
   buffer = thePrices(1,i)
   select case(buffer)
       case('Time')
           if(i.ne.1) call abortExecution(8)
       case('Electricity')
           iElp = i - 1
       case('Thermal')
           iThp = i - 1 
       case('Chilling')
           iChp = i - 1
   end select
end do

matrix = matrixRead(genUnit,nTime, '[', ']')

allocate(nlp(nLoad-1))

buffer = adjustl(matrix(1))
i      = index(buffer, ' ') + 1
buffer = adjustl(buffer(i:))
do i=1, nLoad-1
   nlp(i)  = hCount(buffer)
   n2     = index(buffer,')') + 1
   buffer =  buffer(n2 + 2:)
enddo

if(nlp(iElp).ne.nld(iEl)) call abortExecution(11,1)
if(nlp(iThp).ne.nld(iTh)) call abortExecution(11,2)
if(nlp(iChp).ne.nld(iCh)) call abortExecution(11,3)

nmax = maxval(nlp)
allocate(prices_(nTime,nPrice,nmax))
prices_(:,:,:)  = -1
call allocateVar(20)

do i=1,nTime
   buffer = adjustl(matrix(i))
   read(buffer,*) time(i)
   n2     = index(buffer,' ') + 1
   buffer =  buffer(n2:)
   do j=1,nPrice-1
      ntest  = hCount(buffer)
      if(ntest.ne.nld(j)) call abortExecution(12,j,r1=time(i))
      n3 = index(buffer,'(')
      n4 = index(buffer,')')
      value  = buffer(n3+1:n4-1) 
      read(value,*) (prices_(i,j,k), k=1,nlp(j))
      buffer = buffer(n4+1:)
   enddo
enddo

cEl(:,:) = prices_(:,iEl,1:nld(iEl))
cTh(:,:) = prices_(:,iTh,1:nld(iTh))
cCh(:,:) = prices_(:,iCh,1:nld(iCh))

!---Read the grid buy and sell prices
buffer   = 'GridPrice'
call findEntry(buffer,genUnit,.false.,buffer,isPresent(1),idummy)
call rewUnit(genUnit,1)
grids    = cMatrixRead(theUnit=genUnit, nline=1, nCol = 3)
nTime    = vCount(theUnit=genUnit,rew_=.true.,first_='(',last_=')')
do i=1,3
   buffer = grids(1,i)
   select case(buffer)
       case('Time')
           if(i.ne.1) call abortExecution(8)
       case('Buy')
           iBuy  = i
       case('Sell')
           iSell = i
   end select
end do
deallocate(matrix)
allocate(gridPrices(nTime,3))
gridPrices = dMatrixRead(genUnit,nTime,3)
gridBuyCost(:)  = gridPrices(:,iBuy)
gridSellCost(:) = gridPrices(:,iSell) 

deallocate(theLoads, thePrices)
deallocate(Loads_, prices_, gridPrices)

close(genUnit)
100 format(A100)

end subroutine readLoads
