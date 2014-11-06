!!  Project
!!  drainage analysis
!!  Author
!!  Danyang Su
!!  Email: 
!!  danyang.su@gmail.com
!!  License:
!!  This program is free software: you can redistribute it and/or modify
!!  it under the terms of the GNU General Public License as published by
!!  the Free Software Foundation, either version 3 of the License, or
!!  (at your option) any later version.
!!
!!  This program is distributed in the hope that it will be useful,
!!  but WITHOUT ANY WARRANTY; without even the implied warranty of
!!  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
!!  GNU General Public License for more details.
!!
!!  You should have received a copy of the GNU General Public License
!!  along with this program.  If not, see <http://www.gnu.org/licenses/>.
!!
!!���μ���ģ��
!!

module Geometry

!!use TinSturcture

implicit none

contains

    !!
    !!���������������αߵĹ�ϵ��val(i)>0 ��ˮ���뿪�ñߣ�val(i)<0 ��ˮ������ñߣ�val(i)=0 ��ˮ��ƽ�иñ�
    !!
    subroutine calculateFlowToLineValue(xA,yA,zA,xB,yB,zB,xC,yC,zC,valA,valB,valC)
    
        implicit none
        
        real,intent(in)::xA,yA,zA,xB,yB,zB,xC,yC,zC
        
        real,intent(out)::valA,valB,valC
        
        real::a,b,tempA,tempB
        
        tempA = (yA - yC) * (zA - zB) - (yA - yB) * (zA - zC)
        
        tempB = (xA - xB) * (yA - yC) - (xA - xC) * (yA - yB)
        
        a = tempA / tempB
        
        tempA = (xA - xB) * (zA - zC) - (xA - xC) * (zA - zB)
        
        b = tempA / tempB
        
        valA =  b * (xB - xC) - a * (yB - yC)
        
        valB =  b * (xC - xA) - a * (yC - yA)
       
        valC =  b * (xA - xB) - a * (yA - yB)
           
    end subroutine
    
    !!
    !!ˮ������s=ai+bj
    !!
    subroutine calculateFlowDirection(xA,yA,zA,xB,yB,zB,xC,yC,zC,a,b)
    
        implicit none
        
        real,intent(in)::xA,yA,zA,xB,yB,zB,xC,yC,zC
        
        real,intent(out)::a,b
        
        real::tempA,tempB
        
        tempA = (yA - yC) * (zA - zB) - (yA - yB) * (zA - zC)
        
        tempB = (xA - xB) * (yA - yC) - (xA - xC) * (yA - yB)
        
        a = -tempA / tempB
        
        tempA =(xA - xB) * (zA - zC) - (xA - xC) * (zA - zB)
        
        b = -tempA / tempB
           
    end subroutine
    
    
    !!
    !!����ƽ�����ε���������������±��1��ʼ����
    !!
    function polygonArea2D(n, xArray,yArray) result(area)
    
        implicit none
        
        integer,intent(in):: n
        
        real,dimension(*) :: xArray , yArray
        
        integer :: i 
        
        real :: area
        
        area = 0
        
        if(n < 3) then
            
            return
        
        end if
        
        do i = 2 , n - 1 , 1
        
            area = area + xArray(i) * (yArray(i + 1) - yArray(i - 1))
        
        end do
        
        area = area + xArray(n) * (yArray(1) - yArray(n - 1)) + xArray(1) * (yArray(2) - yArray(n))
        
        area = abs(area) * 0.5
        
        return
        
    end function
    
    !!
    !!ƽ����������������㷨��Ч
    !!
    function triangleArea2D(x1,y1,x2,y2,x3,y3) result(area)
    
        implicit none
        
        real,intent(in) :: x1,x2,x3,y1,y2,y3
        
        real :: area
        
        area = abs((x2 - x1) * (y3 - y1) - (x3 - x1) * (y2 - y1)) * 0.5
        
        return
        
    end function
    
    !!
    !!�ռ�ֱ��A->B�¶�
    !!
    function spacialLineGradient(xA,yA,zA,xB,yB,zB) result(gradient)
    
        implicit none
        
        real,intent(in)::xA,yA,zA,xB,yB,zB
        
        real :: gradient, spacialLength
        
        spacialLength = sqrt((xA - xB) * (xA - xB) + (yA - yB) * (yA - yB) + (zA - zB) * (zA - zB))
        
        gradient = (zA - zB) / spacialLength
        
        return
            
    end function

end module