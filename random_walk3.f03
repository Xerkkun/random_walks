PROGRAM CAM_ALEATORIA
!------------------------------------------------------------------------------
!PROGRAMA PARA EL PROBLEMA DE LAS CAMINATAS ALEATORIAS EN DOS DIMENSIONES
!CON DESPLAZAMIENTO VARIABLE
!------------------------------------------------------------------------------
IMPLICIT NONE
!DECLARACION DE VARIABLES
CHARACTER(len=20)     :: ARCHIVO
REAL,ALLOCATABLE      :: x(:,:), num(:,:)
REAL                  :: DRMAX
INTEGER :: i,j,N,d
!==============================================================================
PRINT*, "NOMBRE DEL ARCHIVO PARA GUARDAR LOS DATOS:"
READ*, ARCHIVO

!ABRIR ARCHIVO PARA GUARDAR EL HISTOGRAMA Y EL NUMERO DE PASOS
OPEN(10,FILE=ARCHIVO,STATUS="REPLACE",ACTION="WRITE")
!==============================================================================

WRITE(*,*) "INGRESE EL NUMERO DE PASOS:"
READ*, N

d = 2

PRINT*, "INGRESE DESPLAZAMIENTO MAXIMO"
READ*, DRMAX
!==============================================================================
ALLOCATE(x(N+1,d),num(N,d))
!==============================================================================
!OBTENER SEMILLA DE NUMEROS ALEATORIOS
CALL INIT_RANDOM_SEED()
!==============================================================================
!MOVER A LAS PARTICULAS
x(0,:) = 0
    DO i=1,N !PASOS
      CALL RANDOM_NUMBER(num(i,:))
        x(i,:) = x(i-1,:) + ((2.0*num(i,:)-1.0)*DRMAX)
    END DO

    DO j=1,N
      WRITE(10,*) x(j,1), x(j,2)
    END DO
!==============================================================================
CLOSE(10)

DEALLOCATE(x,num)
END PROGRAM CAM_ALEATORIA
!%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
!NOTA:CORREGIR PARA UNA SEMILLA DADA POR EL USUARIO
SUBROUTINE init_random_seed()
INTEGER :: i, n, clock
INTEGER,DIMENSION(:),ALLOCATABLE :: seed

CALL RANDOM_SEED(size = n)
ALLOCATE(seed(n))

CALL SYSTEM_CLOCK(COUNT=clock)

seed = clock + 37 * (/ (i - 1, i = 1, n) /)
CALL RANDOM_SEED(PUT = seed)

DEALLOCATE(seed)
END SUBROUTINE init_random_seed
!%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
