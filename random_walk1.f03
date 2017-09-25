PROGRAM CAM_ALEATORIA
!------------------------------------------------------------------------------
!PROGRAMA PARA EL PROBLEMA DE LAS CAMINATAS ALEATORIAS BIDIMENSIONALES
!CON LONGITUD DE PASO UNITARIA
!------------------------------------------------------------------------------
IMPLICIT NONE
!DECLARACION DE VARIABLES
CHARACTER(len=20) :: ARCHIVO
INTEGER           :: i,DRMAX,M,d,x,y
REAL              :: numx,numy,p
!==============================================================================
PRINT*, "NOMBRE DEL ARCHIVO PARA GUARDAR LOS DATOS:"
READ*, ARCHIVO

!ABRIR ARCHIVO PARA GUARDAR EL HISTOGRAMA Y EL NUMERO DE PASOS
OPEN(10,FILE=ARCHIVO,STATUS="REPLACE",ACTION="WRITE")
!==============================================================================
WRITE(*,*) "INGRESE EL NUMERO DE PASOS:"
READ*, M

PRINT*, "PROBABILIDAD DE DAR PASOS A LA DERECHA"
READ*, p
!==============================================================================
!OBTENER NUMEROS ALEATORIOS
CALL INIT_RANDOM_SEED()
!==============================================================================
!MOVER A LAS PARTICULAS
x = 0
y = 0
DO i=1,M !PASOS
    CALL RANDOM_NUMBER(numx)
    CALL RANDOM_NUMBER(numy)
    IF (numx < p)THEN
      x = x + 1
    ELSE
      x = x - 1
    END IF

    IF (numy < p)THEN
      y = y + 1
    ELSE
      y = y - 1
    END IF
    WRITE(10,*) x,y
END DO

!==============================================================================
CLOSE(10)
END PROGRAM CAM_ALEATORIA
!%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
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
