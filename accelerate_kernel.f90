!Crown Copyright 2012 AWE.
!
! This file is part of CloverLeaf.
!
! CloverLeaf is free software: you can redistribute it and/or modify it under 
! the terms of the GNU General Public License as published by the 
! Free Software Foundation, either version 3 of the License, or (at your option) 
! any later version.
!
! CloverLeaf is distributed in the hope that it will be useful, but 
! WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or 
! FITNESS FOR A PARTICULAR PURPOSE. See the GNU General Public License for more 
! details.
!
! You should have received a copy of the GNU General Public License along with 
! CloverLeaf. If not, see http://www.gnu.org/licenses/.

!>  @brief Fortran acceleration kernel
!>  @author Wayne Gaudin
!>  @details The pressure and viscosity gradients are used to update the 
!>  velocity field.

MODULE accelerate_kernel_module

CONTAINS

SUBROUTINE accelerate_kernel(THREE_D,                                    &
                             x_min,x_max,y_min,y_max,z_min,z_max,dt,     &
                             xarea,yarea,zarea,                          &
                             volume,                                     &
                             density0,                                   &
                             pressure,                                   &
                             viscosity,                                  &
                             xvel0,                                      &
                             yvel0,                                      &
                             zvel0,                                      &
                             xvel1,                                      &
                             yvel1,                                      &
                             zvel1,                                      &
                             stepbymass                                  )

  IMPLICIT NONE

  LOGICAL               :: THREE_D
  INTEGER               :: x_min,x_max,y_min,y_max,z_min,z_max
  REAL(KIND=8)          :: dt,dim_factor

  REAL(KIND=8), DIMENSION(x_min-2:x_max+2 ,y_min-2:y_max+2 ,z_min-2:z_max+2) :: density0
  REAL(KIND=8), DIMENSION(x_min-2:x_max+2 ,y_min-2:y_max+2 ,z_min-2:z_max+2) :: volume
  REAL(KIND=8), DIMENSION(x_min-2:x_max+3 ,y_min-2:y_max+2 ,z_min-2:z_max+2) :: xarea
  REAL(KIND=8), DIMENSION(x_min-2:x_max+2 ,y_min-2:y_max+3 ,z_min-2:z_max+2) :: yarea
  REAL(KIND=8), DIMENSION(x_min-2:x_max+2 ,y_min-2:y_max+2 ,z_min-2:z_max+3) :: zarea
  REAL(KIND=8), DIMENSION(x_min-2:x_max+2 ,y_min-2:y_max+2 ,z_min-2:z_max+2) :: pressure
  REAL(KIND=8), DIMENSION(x_min-2:x_max+2 ,y_min-2:y_max+2 ,z_min-2:z_max+2) :: viscosity
  REAL(KIND=8), DIMENSION(x_min-2:x_max+3 ,y_min-2:y_max+3 ,z_min-2:z_max+3) :: xvel0,yvel0,zvel0
  REAL(KIND=8), DIMENSION(x_min-2:x_max+3 ,y_min-2:y_max+3 ,z_min-2:z_max+3) :: xvel1,yvel1,zvel1
  REAL(KIND=8), DIMENSION(x_min-2:x_max+3 ,y_min-2:y_max+3 ,z_min-2:z_max+3) :: stepbymass

  INTEGER               :: j,k,l
  REAL(KIND=8)          :: nodal_mass

  IF(THREE_D) THEN
    dim_factor=0.125_8
  ELSE
    dim_factor=0.25_8
  ENDIF


!$OMP PARALLEL

!This is not correct for 3d yet
!$OMP DO PRIVATE(nodal_mass)
  DO l=z_min,z_max+1
    DO k=y_min,y_max+1
      DO j=x_min,x_max+1

        nodal_mass=(density0(j-1,k-1,l)*volume(j-1,k-1,l)  &
                   +density0(j  ,k-1,l)*volume(j  ,k-1,l)  &
                   +density0(j  ,k  ,l)*volume(j  ,k  ,l)  &
                   +density0(j-1,k  ,l)*volume(j-1,k  ,l)) &
                   *dim_factor

        stepbymass(j,k,l)=0.5_8*dt/nodal_mass

      ENDDO
    ENDDO
  ENDDO
!$OMP END DO

!$OMP DO
  DO l=z_min,z_max+1
    DO k=y_min,y_max+1
      DO j=x_min,x_max+1

        xvel1(j,k,l)=xvel0(j,k,l)-stepbymass(j,k,l)*(xarea(j  ,k  ,l)*(pressure(j  ,k  ,l)-pressure(j-1,k  ,l))    &
                                              +xarea(j  ,k-1,l)*(pressure(j  ,k-1,l)-pressure(j-1,k-1,l)))
      ENDDO
    ENDDO
  ENDDO
!$OMP END DO

!$OMP DO
  DO l=z_min,z_max+1
    DO k=y_min,y_max+1
      DO j=x_min,x_max+1

        yvel1(j,k,l)=yvel0(j,k,l)-stepbymass(j,k,l)*(yarea(j  ,k  ,l)*(pressure(j  ,k  ,l)-pressure(j  ,k-1,l))    &
                                              +yarea(j-1,k  ,l)*(pressure(j-1,k  ,l)-pressure(j-1,k-1,l)))

      ENDDO
    ENDDO
  ENDDO
!$OMP END DO

!$OMP DO
  DO l=z_min,z_max+1
    DO k=y_min,y_max+1
      DO j=x_min,x_max+1

        zvel1(j,k,l)=zvel0(j,k,l)-stepbymass(j,k,l)*(zarea(j  ,k  ,l)*(pressure(j  ,k  ,l)-pressure(j  ,k,l-1))    &
                                              +zarea(j-1,k-1,l)*(pressure(j,k  ,l-1)-pressure(j-1,k-1,l)))

      ENDDO
    ENDDO
  ENDDO
!$OMP END DO

!$OMP DO
  DO l=z_min,z_max+1
    DO k=y_min,y_max+1
      DO j=x_min,x_max+1

        xvel1(j,k,l)=xvel1(j,k,l)-stepbymass(j,k,l)*(xarea(j  ,k  ,l)*(viscosity(j  ,k  ,l)-viscosity(j-1,k  ,l)) &
                                            +xarea(j  ,k-1,l)*(viscosity(j  ,k-1,l)-viscosity(j-1,k-1,l)))

      ENDDO
    ENDDO
  ENDDO
!$OMP END DO

!$OMP DO
  DO l=z_min,z_max+1
    DO k=y_min,y_max+1
      DO j=x_min,x_max+1

        yvel1(j,k,l)=yvel1(j,k,l)-stepbymass(j,k,l)*(yarea(j  ,k  ,l)*(viscosity(j  ,k  ,l)-viscosity(j  ,k-1,l)) &
                                              +yarea(j-1,k  ,l)*(viscosity(j-1,k  ,l)-viscosity(j-1,k-1,l)))

      ENDDO
    ENDDO
  ENDDO
!$OMP END DO

!$OMP DO
  DO l=z_min,z_max+1
    DO k=y_min,y_max+1
      DO j=x_min,x_max+1

        zvel1(j,k,l)=zvel1(j,k,l)-stepbymass(j,k,l)*(zarea(j  ,k  ,l)*(viscosity(j  ,k  ,l)-viscosity(j  ,k-1,l)) &
                                              +zarea(j-1,k  ,l)*(viscosity(j-1,k  ,l)-viscosity(j-1,k-1,l)))

      ENDDO
    ENDDO
  ENDDO
!$OMP END DO

!$OMP END PARALLEL

END SUBROUTINE accelerate_kernel

END MODULE accelerate_kernel_module
