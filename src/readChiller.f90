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
!    ; if not, write to the Free Software Foundation,
!    Inc., 51 Franklin St, Fifth Floor, Boston, MA 02110-1301 USA
!
!>\file readChiller.f90
!>\brief Reads Chiller.inp file
!>\author 
!>     Andrea Facci.
!
!---------------------------------------------------------------------------

!>\brief Reads Chiller.inp file
!>\details 
!> This subroutine reads the file "Chiller.inp". The procedure looks for each specific entry
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

subroutine readChillers

!---Declare Unit usage---
use shared
use inputVar
use fileTools
use interfaces
use cmdVar
use myArithmetic

implicit none

!---Declare Local Variables---
integer              :: genUnit = 108, dinte
character(len=50)    :: inputFile = './Input/Chillers.inp'
logical              :: filePresent
character(len=500)   :: buffer, keyword, value,vector,elements,value_
integer              :: firstLine, line, nl, i, nInp, nRow, n1, n2, x, j, il
logical,dimension(18):: isPresent = .false.
character(len=100)   :: chKind(2)
integer, dimension(10) :: dummy
integer              :: error
character(len=20), dimension(3) :: param
integer :: n, nt, nee, net, nec, np
real(kind = prec), allocatable, dimension(:,:) :: matrix

!---Check File Presence---
inquire(file = inputFile, exist = filePresent)
if(.not.filePresent) then
   call abortExecution(1,4)
else
   open(unit = genUnit, file = inputFile)
endif

isPresent(4) = .true.
isPresent(5) = .true.
isPresent(6) = .true.
isPresent(10) = .true.

!---Skip all the lines before the keyword "begin".
firstLine = 0;
do 
    read(genUnit,100) buffer
    buffer = adjustl(buffer)
    firstLine = firstLine + 1
    if(buffer(1:5).eq.'begin') exit
enddo

!---Look for the the "Number" entry that is neede for most of the other entries---
buffer = 'Number'
call iFindEntry(buffer,1,genUnit,.true.,dummy(1),isPresent(1))
nChi = dummy(1)
if(nChi.eq.0) return
if(.not.isPresent(1)) call abortExecution(6,1)
call allocateVar(11)

buffer = 'Technology'
call cFindEntry(buffer,nChi,genUnit,.true.,chKind,isPresent(2),dinte)
if(.not.isPresent(2)) call abortExecution(6,2)

!---read the input list---
line = firstLine
do 
    call readKeyword(genUnit,.false., keyword,value,error,nl)
    if(error.eq.1) call abortExecution(0,4)
    line = line + nl
    if(debug) print*,'file: Chillers.inp; Keyword: ', trim(keyword)
    select case(keyword)
       case('end')
          exit
       case('Power')
          read(value,*) (pMaxC(i), i=1,nChi)
          isPresent(3) = .true.
       case('Priority')
          read(value,*) (ChiPriority(i), i=1,nChi)
          iPrio(3) = .true.
       case('OnOffCost')
          read(value,*) (fireCostC(i), i=1,nChi)
          isPresent(7) = .true.
       case('OeMCost')
          read(value,*) (maintCostC(i), i=1,nChi)
          isPresent(8) = .true.
       case('SetPoint')
          isPresent(9) = .true.
          value_ = value
          do i=1,nChi
              nSpC(i) = hCount(value_)
              n2 = index(value_,')') + 1
              value_ =  value_(n2 + 1:)
          enddo
          call allocateVar(12)
          do i = 1, nChi
             n1 = index(value,'(') + 1
             n2 = index(value,')') - 1
             vector = trim(value(n1:n2))
             read(vector,*) (spC(j,i), j=1,nSpC(i))
             value =  value(n2 + 2:)
          enddo
       case('Efficiency')
              isPresent(11) = .true.
              backspace(genUnit)
              line = line - 1
              do i = 1,nChi
                  nEtaC(i) = vCount(genUnit,.false.)
              enddo
              j = sum(nEtaC)
              call allocateVar(14)
              call rewUnit(genUnit,j)
              do i = 1, nChi
                 etaC(:,:,i) =  dmatrixRead(genUnit,nEtaC(i),2)
              enddo
              line = line + j
       case('Technology')
             read(value,*) (tecC(i),i=1,nChi)
       case('MinUpTime')
             isPresent(12) = .true.
             read(value,*) (minUpTimeC(i), i=1,nChi)
       case('MinDownTime')
             isPresent(13) = .true.
             read(value,*) (minDownTimeC(i), i=1,nChi)
!       case('OutletTemp')
!          isPresent(14) = .true.
!          allocate(nOutTc(nChi))
!          value_ = value
!          do i=1,nChi
!              nOutTc(i) = hCount(value_)
!              n2 = index(value_,')') + 1
!              value_ =  value_(n2 + 1:)
!          enddo
!          allocate(outTempChi(maxval(nOutTc), nChi))
!          do i = 1, nChi
!             n1 = index(value,'(') + 1
!             n2 = index(value,')') - 1
!             vector = trim(value(n1:n2))
!             read(vector,*) (outTempChi(j,i), j=1,nOutTc(i))
!             value =  value(n2 + 2:)
!          enddo
!       case('EnvTemp')
!          allocate(nTenvC(nChi))
!          isPresent(15) = .true.
!          value_ = value
!          do i=1,nChi
!              nTenvC(i) = hCount(value_)
!              n2 = index(value_,')') + 1
!              value_ =  value_(n2 + 1:)
!          enddo
!          allocate(envTempChi(maxval(nTenvC), nChi))
!          do i = 1, nChi
!             n1 = index(value,'(') + 1
!             n2 = index(value,')') - 1
!             vector = trim(value(n1:n2))
!             read(vector,*) (envTempChi(j,i), j=1,nTenvC(i))
!             value =  value(n2 + 2:)
!          enddo
!      case('PowerCorrection')
!            isPresent(16) = .true.
!            do i=1,nChi
!               n1 = maxval(nOutTc)
!               n2 = maxval(nTenvC)
!               allocate(ChiCorrectionP(n1,n2,nChi))
!               call rewUnit(genUnit,1)
!               chiCorrectionP(:,:,i) = dmatrixRead(genUnit,nOutTc(i),nTenvC(i))
!            enddo
!      case('CopCorrection')
!            isPresent(17) = .true.
!            do i=1,nChi
!               n1 = maxval(nOutTc)
!               n2 = maxval(nTenvC)
!               allocate(ChiCorrectionE(n1,n2,nChi))
!               call rewUnit(genUnit,1)
!               chiCorrectionE(:,:,i) = dmatrixRead(genUnit,nOutTc(i),nTenvC(i))
!            enddo
!      case('Tmandata')
!            isPresent(18) = .true.
!            read(value,*) (TutileChi (i),i=1,nChi)
       case('Number','EnvTemp', 'OutletTemp','PowerCorrection','CopCorrection','Tmandata')
             continue
       case(' ')
            if(verb) call warning(4,4,line=line)
       case default
            if(.not.silent)  call warning(1,4,line=line,word=keyword)
    end select
enddo

call rewUnit(genUnit,line-firstLine)
line = firstLine
do 
    call readKeyword(genUnit,.false., keyword,value,error,nl)
    if(error.eq.1) call abortExecution(0,11)
    line = line + nl
    if(debug) print*,'file: Chillers.inp; Keyword: ', trim(keyword)
    select case(keyword)
       case('end')
          exit
       case('OutletTemp')
          isPresent(14) = .true.
          allocate(nOutTc(nChi))
          value_ = value
          do i=1,nChi
              nOutTc(i) = hCount(value_)
              n2 = index(value_,')') + 1
              value_ =  value_(n2 + 1:)
          enddo
          allocate(outTempChi(maxval(nOutTc), nChi))
          do i = 1, nChi
             n1 = index(value,'(') + 1
             n2 = index(value,')') - 1
             vector = trim(value(n1:n2))
             read(vector,*) (outTempChi(j,i), j=1,nOutTc(i))
             value =  value(n2 + 2:)
          enddo
       case('EnvTemp')
          allocate(nTenvC(nChi))
          isPresent(15) = .true.
          value_ = value
          do i=1,nChi
              nTenvC(i) = hCount(value_)
              n2 = index(value_,')') + 1
              value_ =  value_(n2 + 1:)
          enddo
          allocate(envTempChi(maxval(nTenvC), nChi))
          do i = 1, nChi
             n1 = index(value,'(') + 1
             n2 = index(value,')') - 1
             vector = trim(value(n1:n2))
             read(vector,*) (envTempChi(j,i), j=1,nTenvC(i))
             value =  value(n2 + 2:)
          enddo
      case(' ')
            if(verb) call warning(4,4,line=line)
    end select
enddo

call rewUnit(genUnit,line-firstLine)
line = firstLine
if(.not.isPresent(14)) call abortExecution(6,14)
if(.not.isPresent(15)) call abortExecution(6,15)
do 
    call readKeyword(genUnit,.false., keyword,value,error,nl)
    if(error.eq.1) call abortExecution(0,11)
    line = line + nl
    select case(keyword)
      case('end')
          exit
      case('PowerCorrection')
            isPresent(16) = .true.
            do i=1,nChi
               n1 = maxval(nOutTc)
               n2 = maxval(nTenvC)
               allocate(ChiCorrectionP(n1,n2,nChi))
               call rewUnit(genUnit,1)
               chiCorrectionP(:,:,i) = dmatrixRead(genUnit,nOutTc(i),nTenvC(i))
            enddo
      case('CopCorrection')
            isPresent(17) = .true.
            do i=1,nChi
               n1 = maxval(nOutTc)
               n2 = maxval(nTenvC)
               allocate(ChiCorrectionE(n1,n2,nChi))
               call rewUnit(genUnit,1)
               chiCorrectionE(:,:,i) = dmatrixRead(genUnit,nOutTc(i),nTenvC(i))
            enddo
      case('Tmandata')
            isPresent(18) = .true.
            read(value,*) (TutileChi (i),i=1,nChi)
      case(' ')
            if(verb) call warning(4,4,line=line)
    end select
enddo

!---check if all the variablea were read---
nInp = size(isPresent)
do i = 1,nInp
    if(.not.isPresent(i)) call abortExecution(6,i)
enddo

close(genUnit)
100 format(A500)

end subroutine readChillers
