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
!!Tin�ṹ
!!

module TinSturcture

    implicit none
    
    !!����ṹ
    type Point3D
    
        real::X = -999999 , Y = -999999 , Z = -999999
    
    end type
    
    !!�����νṹ
    type Triangle
    
        integer::ID = -1
        
        integer::NodesID(3) = -1        !!�ڵ�ID
        
        integer::MaterialID = -1       !!����ID
        
        integer::LinesID(3) = -1        !!��ID��Line(1)��ӦNodeID(1)����Ӧ�ıߣ�����
        
        real::FlowToLineValue(3)        !!һ����ˮ������Ĺ�ϵ,�������0����ˮ���뿪�ñߣ�
                                         !!���С��0����ˮ������ñߣ��������0����ˮ��������ñ�ʸ��������ͬ
        
        integer::FlowToLine(3)   !!����������ߵĹ�ϵ��+1��ʾˮ���뿪�ñߣ�-1��ʾˮ������ñߣ�
                                  !!0��ʾˮ��������ñ�ʸ��ƽ�У�FlowToLine��Ҫ���ڴ������������߿��ܳ��������
                                  !!�ֹ涨������ֻ�ܴ�һ����������������������С��0������ȡ����ֵ�ϴ��һ����
                                  !!��Ϊˮ�������ı߽磬��Ϊ-1����һ���߼�Ϊ0,������ֵ��ȣ�����ȡ����һ����Ϊ�����߽�
        
        integer::ToDrainageID = -1      !!���������ID
        
        real::Area = 0             !!���������
        
        type(Point3D)::CenterLocation  !!���ĵ����꣬��������ĸ߳�ֵ
        
        logical :: IsFlowFacet = .false.
                                         
    
    end type
    
    
    !!�ڵ�ṹ
    type Node
    
        integer::ID = -1   
    
        type(Point3D):: Location    !!�ڵ�λ��
        
        integer::LinkedTriAmount = 0
        
        integer::LinkedNodesAmount = 0
        
        integer,allocatable:: LinkedNodesID(:)       !!�ڵ������������ڵ�
        
        integer,allocatable::LinkedTriID(:)          !!��ڵ�������������ID
        
        integer,allocatable::LinkedLinesID(:)         !!��ڵ������ı�ID
        
        real::DZ = 0                                    !!�߳�У����ֵ������Ҫ����ԭ�и߳��ϵ�΢С����
        
        integer :: NodeType = 0    !!�ڵ����ʣ�����ڵ�Ϊ0���ݵعȵ׵�Ϊ1������������ڵ�Ϊ-1,α�ݵص�Ϊ2, �ݵر߽���ڵ�Ϊ-2
                                    !!α�ݵص㶨��Ϊ���õ������Ļ���������ڸõ㣬�����������ĳ����㣬�Ҳ�λ�ڱ߽���        
        
        
        integer,allocatable::FlowFromLineID(:)   !!����õ�Ļ�ˮ����һ�ڵ�ID
        
        integer::FlowToLineID = -1               !!�ýڵ�������ڵ�
        
    end type
    
    type Line
    
        integer::ID = -1
        
        integer::LeftTriID = -1 , RightTriID = -1      !!���������Σ������������ABC����AB�ĵ��������λ��BA����������Ϊ�䱾��
        
        integer::StartNodeID = -1 , EndNodeID = -1     !!�ߵ������յ�ID
        
        integer::LeftFlowToLine = 0 , RightFlowToLine = 0     !!������������ñߵ������ϵ��ͬTriangle�ṹ�е�FlowToLine���ʣ�
        
        integer::FlowCharacter = 9999                 !!�ߵĻ�ˮ���ʣ���ˮ��> 0 ��1��2������ˮ��< 0 ����1����2������ˮ�ߣ�0
                                                       !!�ݵصĳ����� �� ԭ��ˮ���� + ����5����������7~��3��
                                                       !!������ˮ����ƽ�иñߵ��������ˮ�ߴ�����
        
    end type
    
    
    type Drainage
    
        integer::ID                                    !!����ID
    
        integer::OutLetNodeID = -1                     !!������ڵ�ID�������ϲ���������ڵ��仯������������
        
        integer,allocatable::JoinLinesID(:)          !!������ID����
        
        integer,allocatable::PassTrianglesID(:)      !!����������ID��
        
        integer::DrainageType = 0                      !!��������Ϊ-1���ݵ�����Ϊ1��α�ݵ�����Ϊ2��ͬNodeType
        
        integer,allocatable::OuterBoundaryNodesID(:) !!��߽�ڵ�ID
        
        integer,allocatable::SpanTypicalLineNodesID(:) !!��Խ���͵ȸ��ߵĽڵ�ID
        
        logical :: IsValid = .true.
        
        real :: Area
        
    end type
    
    
    type LinesStatistics
    
        integer::SeparateLinesAmount = 0               !!��ˮ������
        
        integer::JoinLinesAmount = 0                   !!��ˮ������
        
        integer::PassLinesAmount = 0                   !!��ˮ������
        
        integer::LinesAmount = 0                       !!�ܱ���
    
    end type
    
end module