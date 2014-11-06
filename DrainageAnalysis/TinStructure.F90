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
!!Tin结构
!!

module TinSturcture

    implicit none
    
    !!坐标结构
    type Point3D
    
        real::X = -999999 , Y = -999999 , Z = -999999
    
    end type
    
    !!三角形结构
    type Triangle
    
        integer::ID = -1
        
        integer::NodesID(3) = -1        !!节点ID
        
        integer::MaterialID = -1       !!材料ID
        
        integer::LinesID(3) = -1        !!边ID，Line(1)对应NodeID(1)所对应的边，类推
        
        real::FlowToLineValue(3)        !!一边与水流方向的关系,如果大于0，则水流离开该边；
                                         !!如果小于0，则水流流向该边；如果等于0，则水流方向与该边矢量方向相同
        
        integer::FlowToLine(3)   !!最终流向与边的关系，+1表示水流离开该边，-1表示水流流向该边，
                                  !!0表示水流方向与该边矢量平行，FlowToLine主要用于处理存在两条边可能出流的情况
                                  !!现规定三角形只能从一条边流出，即若存在两个小于0的数，取绝对值较大的一条边
                                  !!作为水流流出的边界，记为-1，另一条边记为0,若两个值相等，任意取其中一条作为流出边界
        
        integer::ToDrainageID = -1      !!所属流域的ID
        
        real::Area = 0             !!三角形面积
        
        type(Point3D)::CenterLocation  !!中心点坐标，含修正后的高程值
        
        logical :: IsFlowFacet = .false.
                                         
    
    end type
    
    
    !!节点结构
    type Node
    
        integer::ID = -1   
    
        type(Point3D):: Location    !!节点位置
        
        integer::LinkedTriAmount = 0
        
        integer::LinkedNodesAmount = 0
        
        integer,allocatable:: LinkedNodesID(:)       !!节点相连的其它节点
        
        integer,allocatable::LinkedTriID(:)          !!与节点相连的三角形ID
        
        integer,allocatable::LinkedLinesID(:)         !!与节点相连的边ID
        
        real::DZ = 0                                    !!高程校正的值，即需要加在原有高程上的微小增量
        
        integer :: NodeType = 0    !!节点性质，常规节点为0，洼地谷底点为1，正常流域出口点为-1,伪洼地点为2, 洼地边界出口点为-2
                                    !!伪洼地点定义为：该点相连的汇流点均高于该点，不存在正常的出流点，且不位于边界上        
        
        
        integer,allocatable::FlowFromLineID(:)   !!流向该点的汇水边另一节点ID
        
        integer::FlowToLineID = -1               !!该节点的流出节点
        
    end type
    
    type Line
    
        integer::ID = -1
        
        integer::LeftTriID = -1 , RightTriID = -1      !!左右三角形，如对于三角形ABC，边AB的的左三角形或边BA的右三角形为其本身
        
        integer::StartNodeID = -1 , EndNodeID = -1     !!边的起点和终点ID
        
        integer::LeftFlowToLine = 0 , RightFlowToLine = 0     !!左右三角形与该边的流向关系，同Triangle结构中的FlowToLine性质，
        
        integer::FlowCharacter = 9999                 !!边的汇水性质，分水边> 0 （1，2），汇水边< 0 （－1，－2），过水边＝0
                                                       !!洼地的出流边 ＝ 原汇水性质 + （－5），即（－7~－3）
                                                       !!（两边水流均平行该边的情况按分水边处理）
        
    end type
    
    
    type Drainage
    
        integer::ID                                    !!流域ID
    
        integer::OutLetNodeID = -1                     !!流域出口点ID，经过合并的流域出口点会变化，不可再引用
        
        integer,allocatable::JoinLinesID(:)          !!汇流线ID集，
        
        integer,allocatable::PassTrianglesID(:)      !!汇流三角形ID集
        
        integer::DrainageType = 0                      !!正常流域为-1，洼地流域为1，伪洼地流域为2，同NodeType
        
        integer,allocatable::OuterBoundaryNodesID(:) !!外边界节点ID
        
        integer,allocatable::SpanTypicalLineNodesID(:) !!跨越典型等高线的节点ID
        
        logical :: IsValid = .true.
        
        real :: Area
        
    end type
    
    
    type LinesStatistics
    
        integer::SeparateLinesAmount = 0               !!分水边条数
        
        integer::JoinLinesAmount = 0                   !!汇水边条数
        
        integer::PassLinesAmount = 0                   !!过水边条数
        
        integer::LinesAmount = 0                       !!总边数
    
    end type
    
end module