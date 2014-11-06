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
!!��2DM�ļ��ж�ȡ����������
!!


module InputMeshFile

    use TinSturcture

    implicit none
    
    integer::TrianglesAmount = 0,NodesAmount = 0
    
    logical::ScanStatus = .false. , InputStatus = .false.
    
    public TrianglesAmount,NodesAmount,ScanStatus,InputStatus
    
    public ScanFile,GetTinData
    
    contains
    
        !!
        !!ɨ�������ļ�,�ó������θ����ͽڵ����
        !!
        subroutine ScanFile(fileId,fileName)
        
            implicit none
            
            integer , intent(in)::fileId
            
            character(len=*) , intent(in)::fileName
            
            character(len=120)::cardType
            
            integer::error
            
            TrianglesAmount = 0
            
            NodesAmount = 0
            
            open(unit=fileId,file=fileName)
            
            do while(.true.)
            
                read(fileId,"(A120)",iostat=error) cardType
                
                if(error > 0) then
                
                    ScanStatus = .false.
                    
                    TrianglesAmount = 0
                
                    NodesAmount = 0
                    
                    exit
                    
                else if(error < 0) then
                
                    ScanStatus = .true.
                    
                    exit
                
                end if
                
                cardType = adjustl(cardType)
                 
                 if(index(cardType,"E3T") == 1) then
                        
                    TrianglesAmount = TrianglesAmount + 1
                    
                else if(index(cardType,"ND") == 1) then
                    
                    NodesAmount = NodesAmount + 1
                  
                end if
                    
            end do
            
            !!���ص��ļ�ͷ�����ļ�
            
            close(fileId)
           
            !!write(*,*) "�����θ�����",TrianglesAmount,"���ڵ������",NodesAmount
            
        end subroutine
        
        
        
        subroutine GetTinData(fileId,fileName,triangles,nodes)
        
            integer , intent(in)::fileId
            
            character(len=*) , intent(in)::fileName
            
            type(Triangle), allocatable::triangles(:)
            
            type(Node), allocatable::nodes(:)
            
            character(len=120)::cardType   !!�ٶ�һ���ַ�������120��
            
            integer::error
            
            integer::i = 0 , j = 0
            
            call ScanFile(fileId,fileName)
            
            if(.not.ScanStatus) then
            
               InputStatus = .false.
            
               stop
               
            end if
            
            !!��������
            if(allocated(triangles)) then
                
                deallocate(triangles)
            
            end if
            
            if(allocated(nodes)) then
            
                deallocate(nodes)
            
            end if
            
            allocate(triangles(TrianglesAmount))
            
            allocate(nodes(NodesAmount))
            
            !!���ݸ�ֵ
            open(unit=fileId,file=fileName)
            
            i = 0
            
            j = 0
            
            do while(.true.)
            
                read(fileId,"(A120)",iostat=error) cardType
                
                if(error > 0) then
                
                    InputStatus = .false.
                    
                    deallocate(triangles)
                
                    deallocate(nodes)
                    
                    exit
                    
                else if(error < 0) then
                
                    InputStatus = .true.
                    
                    exit
                
                end if
                
                cardType = adjustl(cardType)
                 
                 if(index(cardType,"E3T") == 1) then
                        
                    i = i + 1
                        
                    cardType = cardType(4:)
                    
                    read(cardType,*) triangles(i)%ID,Triangles(i)%NodesID(1),Triangles(i)%NodesID(2),Triangles(i)%NodesID(3),triangles(i)%MaterialID
                    
                else if(index(cardType,"ND") == 1) then
                    
                    j = j + 1
                    
                    cardType =cardType(3:)
                    
                    read(cardType,*) nodes(j)%ID,nodes(j)%Location%X,nodes(j)%Location%Y,nodes(j)%Location%Z
                    
                    !!write(*,"(I4,3ES20.8E3)") nodes(j)%ID,nodes(j)%Location%X,nodes(j)%Location%Y,nodes(j)%Location%Z
                 
                end if
                
            end do
            
            !!���ص��ļ�ͷ�����ļ�
            
            close(fileId)            
            
            !InputStatus = .true.
       
        end subroutine
       
end module