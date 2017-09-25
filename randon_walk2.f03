PROGRAM CAM_ALEATORIA
!------------------------------------------------------------------------------
!PROGRAMA PARA EL PROBLEMA DE LAS CAMINATAS ALEATORIAS UNIDIMENSIONALES
!------------------------------------------------------------------------------
IMPLICIT NONE
!DECLARACION DE VARIABLES
CHARACTER(len=20)     :: ARCHIVO
!REAL,ALLOCATABLE      :: x(:)
REAL                  :: p,num
INTEGER,ALLOCATABLE  :: c(:),b(:),n1(:),n2(:),x(:)
INTEGER :: j,N,l,M,k,t,i,DRMAX
!==============================================================================
PRINT*, "NOMBRE DEL ARCHIVO PARA GUARDAR LOS DATOS:"
READ*, ARCHIVO

!ABRIR ARCHIVO PARA GUARDAR EL HISTOGRAMA Y EL NUMERO DE PASOS
OPEN(10,FILE=ARCHIVO,STATUS="REPLACE",ACTION="WRITE")
!==============================================================================
PRINT*, "INGRESE EL NUMERO DE CAMINANTES"
READ*, N

WRITE(*,*) "INGRESE EL NUMERO DE PASOS:"
READ*, M

PRINT*, "INGRESE LA PROBABILIDAD DE QUE DE UN PASO A LA DERECHA"
READ*, p

!DESPLAZAMIENTO MAXIMO
DRMAX = 1

ALLOCATE(x(N),n1(N),n2(N),c(M+1),b(M+1))
!==============================================================================
!OBTENER SEMILLA ALEATORIA
CALL INIT_RANDOM_SEED()
!==============================================================================
!MOVER A LOS CAMINANTES
x = 0 !PARTE DEL ORIGEN
n1 = 0
n2 = 0
DO k=1,N    !CAMINANTES
  DO j=1,M  !PASOS
    CALL RANDOM_NUMBER(num)
    IF (num < p)THEN
!      x(k) = x(k) + DRMAX !POSICION DEL CAMINANTE k
      !CUENTA EL NUMERO DE PASOS A LA DERECHA QUE HA REALIZADO CADA CAMINANTE
      n1(k) = n1(k) + 1
    ELSE
!      x(k) = x(k) - DRMAX !POSICION DEL CAMINANTE k
      !CUENTA EL NUMERO DE PASOS A LA IZQUIERDA QUE HA REALIZADO CADA CAMINANTE
      n2(k) = n2(k) + 1
    END IF
  END DO
END DO

!HISTOGRAMA
!SE CUENTAN LOS CAMINANTES QUE REALIZARON EL MISMO NUMERO DE PASOS A LA DERECHA
!ACUMULADOS AL FINAL DE LA CAMINATA.
c=0         !CONTADOR DE FRECUENCIAS
b(0)=0      !CONTADOR DE PASOS TOTALES A LA DERECHA
DO l=1,N    !CAMINANTES
  DO t=0,M  !PASOS

    b(t) = b(0) + DRMAX*t !BARRIDO DE PASOS POSIBLES TOTALES A LA DERECHA AUMENTANDO EN t
    IF (n1(l) == b(t))THEN

      c(t) = c(t) + 1     !SI EL DESPLAZAMIENTO TOTAL A LA DERECHA(n1) DE
                          !ALGUNOS CAMINANTES COINCIDE, SE INCREMENTA EL
                          !CONTADOR DE FRECUENCIAS PARA ESE DESPLAZAMIENTO
    END IF

  END DO
END DO

DO i=0,M
    WRITE(10,*) i,c(i)
END DO

!==============================================================================
CLOSE(10)
DEALLOCATE(x,n1,n2,c,b)
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
