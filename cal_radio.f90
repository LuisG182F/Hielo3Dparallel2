module cal_radio

use presicion

contains

!*********************************************************************
!                Function radiopp0 (FORTRAN)
!
!  Esta rutina calcula si la particula esta al lado de otra particula. 
!  Arroja una variable pp0=1 si lo esta y pp0=0 si no lo esta. 
!
!*********************************************************************

Integer function radiopp0(c,ii,jj,kk,filas,columnas,ancho)

IMPLICIT NONE
integer                                   :: filas,columnas,ancho
integer                                   :: ii,jj,kk,jfl,jfr,jcl,jcr,jkl,jkr,s
integer,dimension(filas,columnas,ancho)   :: c
integer, dimension(26)                    :: vvecinos

radiopp0=0
                 jfl = ii-1 ;  if (jfl < 1)        jfl = filas
                 jcl = jj-1 ;  if (jcl < 1)        jcl = columnas
                 jkl = kk-1 ;  if (jkl < 1)        jkl = ancho
                 jfr = ii+1 ;  if (jfr > filas)    jfr = 1  
                 jcr = jj+1 ;  if (jcr > columnas) jcr = 1
                 jkr = kk+1 ;  if (jkr > ancho)    jkr = 1
                 
                 Vvecinos(1) = c(jfl, jcl,jkl)
                 Vvecinos(2) = c(jfl, jj,jkl) 
                 Vvecinos(3) = c(jfl, jcr,jkl)
                 Vvecinos(4) = c(ii, jcl,jkl)
                 Vvecinos(5) = c(ii, jcr,jkl)
                 Vvecinos(6) = c(jfr, jcl,jkl)
                 Vvecinos(7) = c(jfr, jj,jkl)
                 Vvecinos(8) = c(jfr, jcr,jkl)
                 Vvecinos(9) = c(jfl, jcl,kk)
                 Vvecinos(10) = c(jfl, jj,kk)
                 Vvecinos(11) = c(jfl, jcr,kk)
                 Vvecinos(12) = c(ii, jcl,kk)
                 Vvecinos(13) = c(ii, jcr,kk)
                 Vvecinos(14) = c(jfr, jcl,kk)
                 Vvecinos(15) = c(jfr, jj,kk)
                 Vvecinos(16) = c(jfr, jcr,kk)
                 Vvecinos(17) = c(jfl, jcl,jkr)
                 Vvecinos(18) = c(jfl, jj,jkr)
                 Vvecinos(19) = c(jfl, jcr,jkr)
                 Vvecinos(20) = c(ii, jcl,jkr)
                 Vvecinos(21) = c(ii, jcr,jkr)
                 Vvecinos(22) = c(jfr, jcl,jkr)
                 Vvecinos(23) = c(jfr, jj,jkr)
                 Vvecinos(24) = c(jfr, jcr,jkr)
                 Vvecinos(25) = c(ii, jj,jkl)
                 Vvecinos(26) = c(ii, jj,jkr)

do s=1,26
if (vvecinos(s)/=0) radiopp0=1
end do 

end function

!*********************************************************************
!                Function radiopp00 (FORTRAN)
!
!  Esta rutina calcula si la particula esta al lado de otra particula. 
!  Arroja una variable pp0=1 si lo esta y pp0=0 si no lo esta. 
!
!*********************************************************************

Integer function radiopp00(c,ii,jj,kk,filas,columnas,ancho)

IMPLICIT NONE
integer                                   :: filas,columnas,ancho,luis
integer                                   :: ii,jj,kk,jfl,jfr,jcl,jcr,jkl,jkr,s
integer,dimension(filas,columnas,ancho)   :: c
integer, dimension(26)                    :: vvecinos

luis=0
radiopp00=0
                 jfl = ii-1 ;  if (jfl < 1)        jfl = filas
                 jcl = jj-1 ;  if (jcl < 1)        jcl = columnas
                 jkl = kk-1 ;  if (jkl < 1)        jkl = ancho
                 jfr = ii+1 ;  if (jfr > filas)    jfr = 1  
                 jcr = jj+1 ;  if (jcr > columnas) jcr = 1
                 jkr = kk+1 ;  if (jkr > ancho)    jkr = 1
                 
                 Vvecinos(1) = c(jfl, jcl,jkl)
                 Vvecinos(2) = c(jfl, jj,jkl) 
                 Vvecinos(3) = c(jfl, jcr,jkl)
                 Vvecinos(4) = c(ii, jcl,jkl)
                 Vvecinos(5) = c(ii, jcr,jkl)
                 Vvecinos(6) = c(jfr, jcl,jkl)
                 Vvecinos(7) = c(jfr, jj,jkl)
                 Vvecinos(8) = c(jfr, jcr,jkl)
                 Vvecinos(9) = c(jfl, jcl,kk)
                 Vvecinos(10) = c(jfl, jj,kk)
                 Vvecinos(11) = c(jfl, jcr,kk)
                 Vvecinos(12) = c(ii, jcl,kk)
                 Vvecinos(13) = c(ii, jcr,kk)
                 Vvecinos(14) = c(jfr, jcl,kk)
                 Vvecinos(15) = c(jfr, jj,kk)
                 Vvecinos(16) = c(jfr, jcr,kk)
                 Vvecinos(17) = c(jfl, jcl,jkr)
                 Vvecinos(18) = c(jfl, jj,jkr)
                 Vvecinos(19) = c(jfl, jcr,jkr)
                 Vvecinos(20) = c(ii, jcl,jkr)
                 Vvecinos(21) = c(ii, jcr,jkr)
                 Vvecinos(22) = c(jfr, jcl,jkr)
                 Vvecinos(23) = c(jfr, jj,jkr)
                 Vvecinos(24) = c(jfr, jcr,jkr)
                 Vvecinos(25) = c(ii, jj,jkl)
                 Vvecinos(26) = c(ii, jj,jkr)

do s=1,26
if (vvecinos(s)/=0) luis=luis+1
end do 
if (luis==26) radiopp00=1

end function





!*********************************************************************
!                Function radiopp1 (FORTRAN)
!
!  Esta rutina calcula si la particula esta al lado de otra particula. 
!  Arroja una variable pp1=1 si lo esta y pp1=0 si no lo esta. 
!
!*********************************************************************

Integer function radiopp1(c,ii,jj,kk,filas,columnas,ancho)
IMPLICIT NONE
integer                                   :: filas,columnas,ancho
integer                                   :: ii,jj,kk,jfl,jfr,jcl,jcr,jkl,jkr,mfl,mfr,mcl,mcr,mkl,mkr,s
integer,dimension(filas,columnas,ancho)   :: c
integer, dimension(98)                    :: vvecinos

jfl = ii-1 ;  if (jfl < 1)        jfl = filas
jcl = jj-1 ;  if (jcl < 1)        jcl = columnas
jkl = kk-1 ;  if (jkl < 1)        jkl = ancho
jfr = ii+1 ;  if (jfr > filas)    jfr = 1
jcr = jj+1 ;  if (jcr > columnas) jcr = 1
jkr = kk+1 ;  if (jkr > ancho)    jkr = 1

radiopp1=0

 mfl=ii-2
 if (mfl==-1) then 
 mfl = filas-1
 else
 if (mfl==0) then
 mfl = filas
 end if
 end if
mcl=jj-2
 if (mcl==-1) then 
 mcl = columnas-1
 else
 if (mcl==0) then
 mcl = columnas
 end if
 end if
 mkl=kk-2
 if (mkl==-1) then 
 mkl = ancho-1
 else
 if (mkl==0) then
 mkl = ancho
else
    if (mkl==-2) then
    mkl = ancho-2
 end if
 end if
 end if
 mfr=ii+2
 if (mfr==filas+2) then 
 mfr = 2
 else
 if (mfr==filas+1) then
 mfr = 1
 end if
 end if
 mcr=jj+2
 if (mcr==columnas+2) then 
 mcr = 2
 else
 if (mcr==columnas+1) then
 mcr = 1
 end if
 end if
 mkr=kk+2
 if (mkr==ancho+2) then 
 mkr = 2
 else
 if (mkr==ancho+1) then
 mkr = 1
 end if
 end if
 

!********tapa1********************** 
 Vvecinos(1) = c(jfl, jcl,mkr) 
 Vvecinos(2) = c(jfl, jj,mkr) 
 Vvecinos(3) = c(jfl, jcr,mkr) 
 Vvecinos(4) = c(ii, jcl,mkr) 
 Vvecinos(5) = c(ii, jcr,mkr) 
 Vvecinos(6) = c(jfr, jcl,mkr) 
 Vvecinos(7) = c(jfr, jj,mkr) 
 Vvecinos(8) = c(jfr, jcr,mkr) 
  
!********tapa2********************** 
 
 Vvecinos(9) = c(jfl, jcl,mkl) 
 Vvecinos(10) = c(jfl, jj,mkl) 
 Vvecinos(11) = c(jfl, jcr,mkl) 
 Vvecinos(12) = c(ii, jcl,mkl) 
 Vvecinos(13) = c(ii, jcr,mkl) 
 Vvecinos(14) = c(jfr, jcl,mkl) 
 Vvecinos(15) = c(jfr, jj,mkl) 
 Vvecinos(16) = c(jfr, jcr,mkl) 
 
!********tapa3********************** 
Vvecinos(17) = c(mfl, jcl,jkr) 
Vvecinos(18) = c(mfl, jcl,jkl)  
Vvecinos(19) = c(mfl, jcl,kk) 
Vvecinos(20) = c(mfl, jcr,jkr) 
Vvecinos(21) = c(mfl, jcr,jkl) 
Vvecinos(22) = c(mfl, jcr,kk) 
Vvecinos(23) = c(mfl, jj,kk) 
Vvecinos(24) = c(mfl, jj,jkr) 
Vvecinos(25) = c(mfl, jj,jkl) 
 
!********tapa4********************** 
 
Vvecinos(26) = c(mfr, jcl,jkr) 
Vvecinos(27) = c(mfr, jcl,jkl)  
Vvecinos(28) = c(mfr, jcl,kk) 
 Vvecinos(29) = c(mfr, jcr,jkr) 
 Vvecinos(30) = c(mfr, jcr,jkl) 
 Vvecinos(31) = c(mfr, jcr,kk) 
 Vvecinos(32) = c(mfr, jj,kk) 
Vvecinos(33) = c(mfr, jj,jkr) 
Vvecinos(34) = c(mfr, jj,jkl) 
 
!********tapa5********************** 
 
Vvecinos(35) = c(jfl, mcl,jkr) 
Vvecinos(36) = c(jfl, mcl,jkl) 
Vvecinos(37) = c(jfl, mcl,kk) 
Vvecinos(38) = c(ii, mcl,jkr) 
Vvecinos(39) = c(ii, mcl,jkl)  
Vvecinos(40) = c(ii, mcl,kk)  
Vvecinos(41) = c(jfr, mcl,jkr) 
Vvecinos(42) = c(jfr, mcl,jkl) 
Vvecinos(43) = c(jfr, mcl,kk) 
 
!********tapa6********************** 
 
Vvecinos(44) = c(jfl, mcr,jkr) 
Vvecinos(45) = c(jfl, mcr,jkl) 
Vvecinos(46) = c(jfl, mcr,kk) 
Vvecinos(47) = c(ii, mcr,jkr) 
Vvecinos(48) = c(ii, mcr,jkl)  
Vvecinos(49) = c(ii, mcr,kk)  
Vvecinos(50) = c(jfr, mcr,jkr) 
Vvecinos(51) = c(jfr, mcr,jkl) 
Vvecinos(52) = c(jfr, mcr,kk) 
 
!********arista1********************** 
 
 Vvecinos(53) = c(mfl, jcl,mkr) 
 Vvecinos(54) = c(mfl, jj,mkr) 
 Vvecinos(55) = c(mfl, jcr,mkr) 
 
Vvecinos(56) = c(jfl, mcl,mkr) 
 Vvecinos(57) = c(jfr, mcl,mkr) 
 Vvecinos(58) = c(ii, mcl,mkr) 
 
 Vvecinos(59) = c(ii, mcr,mkr) 
Vvecinos(60) = c(jfr, mcr,mkr) 
Vvecinos(61) = c(jfl, mcr,mkr) 
 
 Vvecinos(62) = c(mfr, jcl,mkr) 
 Vvecinos(63) = c(mfr, jj,mkr) 
 Vvecinos(64) = c(mfr, jcr,mkr) 
 
  
 Vvecinos(65) = c(mfl, mcr,mkr) 
 Vvecinos(66) = c(mfl, mcl,mkr) 
 
 Vvecinos(67) = c(mfr, mcl,mkr) 
 Vvecinos(68) = c(mfr, mcr,mkr) 
  
  
!********arista2********************** 
 
 Vvecinos(69) = c(mfl, jcl,mkl) 
 Vvecinos(70) = c(mfl, jj,mkl) 
 Vvecinos(71) = c(mfl, jcr,mkl) 
 
Vvecinos(72) = c(jfl, mcl,mkl) 
 Vvecinos(73) = c(jfr, mcl,mkl) 
 Vvecinos(74) = c(ii, mcl,mkl) 
 
 Vvecinos(75) = c(ii, mcr,mkl) 
Vvecinos(76) = c(jfr, mcr,mkl) 
Vvecinos(77) = c(jfl, mcr,mkl) 
 
 Vvecinos(78) = c(mfr, jcl,mkl) 
 Vvecinos(79) = c(mfr, jj,mkl) 
 Vvecinos(80) = c(mfr, jcr,mkl) 
 
Vvecinos(81) = c(mfl, mcr,mkl) 
 
 Vvecinos(82) = c(mfl, mcl,mkl) 
 Vvecinos(83) = c(mfr, mcl,mkl) 
 Vvecinos(84) = c(mfr, mcr,mkl) 
  
 
!********arista3********************** 
 
Vvecinos(85) = c(mfl, mcl,jkr) 
Vvecinos(86) = c(mfl, mcl,jkl)  
Vvecinos(87) = c(mfl, mcl,kk) 
Vvecinos(88) = c(mfl, mcr,jkr) 
Vvecinos(89) = c(mfl, mcr,jkl) 
Vvecinos(90) = c(mfl, mcr,kk) 
 
 
!********arista4********************** 
 
Vvecinos(91) = c(mfr, mcl,jkr) 
Vvecinos(92) = c(mfr, mcl,jkl)  
Vvecinos(93) = c(mfr, mcl,kk) 
 Vvecinos(94) = c(mfr, mcr,jkr) 
 Vvecinos(95) = c(mfr, mcr,jkl) 
 Vvecinos(96) = c(mfr, mcr,kk) 
 
 VVecinos(97)=c(ii,jj,mkr)
 vvecinos(98)=c(ii,jj,mkl)
 
 
 Vvecinos(65) = c(mfl, mcr,mkr)

 Vvecinos(66) = c(mfl, mcl,mkr)



 Vvecinos(67) = c(mfr, mcl,mkr)

 Vvecinos(68) = c(mfr, mcr,mkr)
 
 Vvecinos(81) = c(mfl, mcr,mkl)



 Vvecinos(82) = c(mfl, mcl,mkl)

 Vvecinos(83) = c(mfr, mcl,mkl)

 Vvecinos(84) = c(mfr, mcr,mkl)

 

do s=1,98
if (vvecinos(s)/=0) radiopp1=1
end do



end function



!*********************************************************************
!                Function radiopp2 (FORTRAN)
!
!  Esta rutina calcula si la particula esta al lado de otra particula. 
!  Arroja una variable pp1=1 si lo esta y pp1=0 si no lo esta. 
!
!*********************************************************************

Integer function radiopp2(c,ii,jj,kk,filas,columnas,ancho)
IMPLICIT NONE
integer                                   :: filas,columnas,ancho
integer                                   :: ii,jj,kk,jfl,jfr,jcl,jcr,jkl,jkr,mfl,mfr,mcl,mcr,mkl,mkr,s
integer,dimension(filas,columnas,ancho)   :: c
integer, dimension(98)                    :: vvecinos

jfl = ii-1 ;  if (jfl < 1)        jfl = filas
jcl = jj-1 ;  if (jcl < 1)        jcl = columnas
jkl = kk-1 ;  if (jkl < 1)        jkl = ancho
jfr = ii+1 ;  if (jfr > filas)    jfr = 1
jcr = jj+1 ;  if (jcr > columnas) jcr = 1
jkr = kk+1 ;  if (jkr > ancho)    jkr = 1

radiopp2=0

 mfl=ii-2
 if (mfl==-1) then 
 mfl = filas-1
 else
 if (mfl==0) then
 mfl = filas
 end if
 end if
mcl=jj-2
 if (mcl==-1) then 
 mcl = columnas-1
 else
 if (mcl==0) then
 mcl = columnas
 end if
 end if
 mkl=kk-2
 if (mkl==-1) then 
 mkl = ancho-1
 else
 if (mkl==0) then
 mkl = ancho
 end if
 end if
 mfr=ii+2
 if (mfr==filas+2) then 
 mfr = 2
 else
 if (mfr==filas+1) then
 mfr = 1
 end if
 end if
 mcr=jj+2
 if (mcr==columnas+2) then 
 mcr = 2
 else
 if (mcr==columnas+1) then
 mcr = 1
 end if
 end if
 mkr=kk+2
 if (mkr==ancho+2) then 
 mkr = 2
 else
 if (mkr==ancho+1) then
 mkr = 1
 end if
 end if
 

!********tapa1********************** 
 Vvecinos(1) = c(jfl, jcl,mkr) 
 Vvecinos(2) = c(jfl, jj,mkr) 
 Vvecinos(3) = c(jfl, jcr,mkr) 
 Vvecinos(4) = c(ii, jcl,mkr) 
 Vvecinos(5) = c(ii, jcr,mkr) 
 Vvecinos(6) = c(jfr, jcl,mkr) 
 Vvecinos(7) = c(jfr, jj,mkr) 
 Vvecinos(8) = c(jfr, jcr,mkr) 
  
!********tapa2********************** 
 
 Vvecinos(9) = c(jfl, jcl,mkl) 
 Vvecinos(10) = c(jfl, jj,mkl) 
 Vvecinos(11) = c(jfl, jcr,mkl) 
 Vvecinos(12) = c(ii, jcl,mkl) 
 Vvecinos(13) = c(ii, jcr,mkl) 
 Vvecinos(14) = c(jfr, jcl,mkl) 
 Vvecinos(15) = c(jfr, jj,mkl) 
 Vvecinos(16) = c(jfr, jcr,mkl) 
 
!********tapa3********************** 
Vvecinos(17) = c(mfl, jcl,jkr) 
Vvecinos(18) = c(mfl, jcl,jkl)  
Vvecinos(19) = c(mfl, jcl,kk) 
Vvecinos(20) = c(mfl, jcr,jkr) 
Vvecinos(21) = c(mfl, jcr,jkl) 
Vvecinos(22) = c(mfl, jcr,kk) 
Vvecinos(23) = c(mfl, jj,kk) 
Vvecinos(24) = c(mfl, jj,jkr) 
Vvecinos(25) = c(mfl, jj,jkl) 
 
!********tapa4********************** 
 
Vvecinos(26) = c(mfr, jcl,jkr) 
Vvecinos(27) = c(mfr, jcl,jkl)  
Vvecinos(28) = c(mfr, jcl,kk) 
 Vvecinos(29) = c(mfr, jcr,jkr) 
 Vvecinos(30) = c(mfr, jcr,jkl) 
 Vvecinos(31) = c(mfr, jcr,kk) 
 Vvecinos(32) = c(mfr, jj,kk) 
Vvecinos(33) = c(mfr, jj,jkr) 
Vvecinos(34) = c(mfr, jj,jkl) 
 
!********tapa5********************** 
 
Vvecinos(35) = c(jfl, mcl,jkr) 
Vvecinos(36) = c(jfl, mcl,jkl) 
Vvecinos(37) = c(jfl, mcl,kk) 
Vvecinos(38) = c(ii, mcl,jkr) 
Vvecinos(39) = c(ii, mcl,jkl)  
Vvecinos(40) = c(ii, mcl,kk)  
Vvecinos(41) = c(jfr, mcl,jkr) 
Vvecinos(42) = c(jfr, mcl,jkl) 
Vvecinos(43) = c(jfr, mcl,kk) 
 
!********tapa6********************** 
 
Vvecinos(44) = c(jfl, mcr,jkr) 
Vvecinos(45) = c(jfl, mcr,jkl) 
Vvecinos(46) = c(jfl, mcr,kk) 
Vvecinos(47) = c(ii, mcr,jkr) 
Vvecinos(48) = c(ii, mcr,jkl)  
Vvecinos(49) = c(ii, mcr,kk)  
Vvecinos(50) = c(jfr, mcr,jkr) 
Vvecinos(51) = c(jfr, mcr,jkl) 
Vvecinos(52) = c(jfr, mcr,kk) 
 
!********arista1********************** 
 
 Vvecinos(53) = c(mfl, jcl,mkr) 
 Vvecinos(54) = c(mfl, jj,mkr) 
 Vvecinos(55) = c(mfl, jcr,mkr) 
 
Vvecinos(56) = c(jfl, mcl,mkr) 
 Vvecinos(57) = c(jfr, mcl,mkr) 
 Vvecinos(58) = c(ii, mcl,mkr) 
 
 Vvecinos(59) = c(ii, mcr,mkr) 
Vvecinos(60) = c(jfr, mcr,mkr) 
Vvecinos(61) = c(jfl, mcr,mkr) 
 
 Vvecinos(62) = c(mfr, jcl,mkr) 
 Vvecinos(63) = c(mfr, jj,mkr) 
 Vvecinos(64) = c(mfr, jcr,mkr) 
 
  
 Vvecinos(65) = c(mfl, mcr,mkr) 
 Vvecinos(66) = c(mfl, mcl,mkr) 
 
 Vvecinos(67) = c(mfr, mcl,mkr) 
 Vvecinos(68) = c(mfr, mcr,mkr) 
  
  
!********arista2********************** 
 
 Vvecinos(69) = c(mfl, jcl,mkl) 
 Vvecinos(70) = c(mfl, jj,mkl) 
 Vvecinos(71) = c(mfl, jcr,mkl) 
 
Vvecinos(72) = c(jfl, mcl,mkl) 
 Vvecinos(73) = c(jfr, mcl,mkl) 
 Vvecinos(74) = c(ii, mcl,mkl) 
 
 Vvecinos(75) = c(ii, mcr,mkl) 
Vvecinos(76) = c(jfr, mcr,mkl) 
Vvecinos(77) = c(jfl, mcr,mkl) 
 
 Vvecinos(78) = c(mfr, jcl,mkl) 
 Vvecinos(79) = c(mfr, jj,mkl) 
 Vvecinos(80) = c(mfr, jcr,mkl) 
 
Vvecinos(81) = c(mfl, mcr,mkl) 
 
 Vvecinos(82) = c(mfl, mcl,mkl) 
 Vvecinos(83) = c(mfr, mcl,mkl) 
 Vvecinos(84) = c(mfr, mcr,mkl) 
  
 
!********arista3********************** 
 
Vvecinos(85) = c(mfl, mcl,jkr) 
Vvecinos(86) = c(mfl, mcl,jkl)  
Vvecinos(87) = c(mfl, mcl,kk) 
Vvecinos(88) = c(mfl, mcr,jkr) 
Vvecinos(89) = c(mfl, mcr,jkl) 
Vvecinos(90) = c(mfl, mcr,kk) 
 
 
!********arista4********************** 
 
Vvecinos(91) = c(mfr, mcl,jkr) 
Vvecinos(92) = c(mfr, mcl,jkl)  
Vvecinos(93) = c(mfr, mcl,kk) 
 Vvecinos(94) = c(mfr, mcr,jkr) 
 Vvecinos(95) = c(mfr, mcr,jkl) 
 Vvecinos(96) = c(mfr, mcr,kk) 
 
 VVecinos(97)=c(ii,jj,mkr)
 vvecinos(98)=c(ii,jj,mkl)
 
 
 Vvecinos(65) = c(mfl, mcr,mkr)

 Vvecinos(66) = c(mfl, mcl,mkr)



 Vvecinos(67) = c(mfr, mcl,mkr)

 Vvecinos(68) = c(mfr, mcr,mkr)
 
 Vvecinos(81) = c(mfl, mcr,mkl)



 Vvecinos(82) = c(mfl, mcl,mkl)

 Vvecinos(83) = c(mfr, mcl,mkl)

 Vvecinos(84) = c(mfr, mcr,mkl)

 



do s=1,98
if (vvecinos(s)==0) radiopp2=radiopp2+1
end do

if(radiopp2<27) radiopp2=1

end function


end module cal_radio