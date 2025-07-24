module radio2

use presicion

 contains
!*********************************************************************
!                Function bordeCT (FORTRAN)
!
!  Esta rutina calcula si la particula esta en el borde de grano (BG) y centrada 
!  Arroja una variable vec1=1 si esta en el BG y centrada y vec1=0 si no esta en el BG centrada.
!
!*********************************************************************

Integer function Vect2(c,ii,jj,kk,filas,columnas,ancho) 
IMPLICIT NONE  
integer                                        :: filas,columnas,ancho,numero_gauss1,numero_gauss2,rrrr,rrrrr
integer                                        :: ii,jj,kk,jfl,jfr,jcl,jcr,jkl,jkr,mfl,mfr,mcl,mcr,mkl,mkr,s,sfl,sfr,skl,skr,scl,scr
integer,dimension(filas,columnas,ancho)        :: c 
integer, dimension(218)                        :: vvecinos 

vect2=0


jfl = ii-1 ;  if (jfl < 1)        jfl = filas
 jcl = jj-1 ;  if (jcl < 1)        jcl = columnas
 jkl = kk-1 ;  if (jkl < 1)        jkl = ancho
 
 jfr = ii+1 ;  if (jfr > filas)    jfr = 1
 jcr = jj+1 ;  if (jcr > columnas) jcr = 1
 jkr = kk+1 ;  if (jkr > ancho)    jkr = 1


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


!&&&&&&&&&&&&&&&&&&&&&&&&&&&&&

sfl=ii-3
 if (sfl==-2) then 
 sfl = filas-2
 else
 if (sfl==0) then
 sfl = filas
 end if
 end if

 
scl=jj-3
 if (scl==-2) then 
 scl = columnas-2
 else
 if (scl==0) then
 scl = columnas
 end if
 end if

 skl=kk-3
 if (skl==-2) then 
 skl = ancho-2
 else
 if (skl==0) then
 skl = ancho
 end if
 end if
 
 sfr=ii+3
 if (sfr==filas+3) then 
 sfr = 3
 else
 if (sfr==filas+2) then
 sfr = 2
 end if
 end if
 
 scr=jj+3
 if (scr==columnas+3) then 
 scr = 3
 else
 if (scr==columnas+2) then
 scr = 2
 end if
 end if

 
 skr=kk+3
 if (skr==ancho+3) then 
 skr = 3
 else
 if (skr==ancho+2) then
 skr = 2
 end if
 end if


!********anillo 1**********************
 vvecinos(1) = c(sfl, scr,mkr)
 vvecinos(2) = c(sfl, mcr,mkr)
 vvecinos(3) = c(sfl, jcr,mkr)
 vvecinos(4) = c(sfl, jj,mkr)
 vvecinos(5) = c(sfl, jcl,mkr)
 vvecinos(6) = c(sfl, mcl,mkr)
 vvecinos(7) = c(sfl, scl,mkr)
 
 vvecinos(8) = c(sfr, scr,mkr)
 vvecinos(9) = c(sfr, mcr,mkr)
 vvecinos(10) = c(sfr, jcr,mkr)
 vvecinos(11) = c(sfr, jj,mkr)
 vvecinos(12) = c(sfr, jcl,mkr)
 vvecinos(13) = c(sfr, mcl,mkr)
 vvecinos(14) = c(sfr, scl,mkr)
 
 vvecinos(15) = c(mfl,scr,mkr)
 vvecinos(16) = c(jfl,scr,mkr)
 vvecinos(17) = c(ii,scr,mkr)
 vvecinos(18) = c(jfr,scr,mkr)
 vvecinos(19) = c(mfl,scr,mkr)
 
 vvecinos(20) = c(mfl,scl,mkr)
 vvecinos(21) = c(jfl,scl,mkr)
 vvecinos(22) = c(ii,scl,mkr)
 vvecinos(23) = c(jfr,scl,mkr)
 vvecinos(24) = c(mfl,scl,mkr)

 !********anillo 2**********************
 
 vvecinos(25) = c(sfl, scr,jkr)
 vvecinos(26) = c(sfl, mcr,jkr)
 vvecinos(27) = c(sfl, jcr,jkr)
 vvecinos(28) = c(sfl, jj,jkr)
 vvecinos(29) = c(sfl, jcl,jkr)
 vvecinos(30) = c(sfl, mcl,jkr)
 vvecinos(31) = c(sfl, scl,jkr)
 
 vvecinos(32) = c(sfr, scr,jkr)
 vvecinos(33) = c(sfr, mcr,jkr)
 vvecinos(34) = c(sfr, jcr,jkr)
 vvecinos(35) = c(sfr, jj,jkr)
 vvecinos(36) = c(sfr, jcl,jkr)
 vvecinos(37) = c(sfr, mcl,jkr)
 vvecinos(38) = c(sfr, scl,jkr)
 
 vvecinos(39) = c(mfl,scr,jkr)
 vvecinos(40) = c(jfl,scr,jkr)
 vvecinos(41) = c(ii,scr,jkr)
 vvecinos(42) = c(jfr,scr,jkr)
 vvecinos(43) = c(mfl,scr,jkr)
 
 vvecinos(44) = c(mfl,scl,jkr)
 vvecinos(45) = c(jfl,scl,jkr)
 vvecinos(46) = c(ii,scl,jkr)
 vvecinos(47) = c(jfr,scl,jkr)
 vvecinos(48) = c(mfl,scl,jkr)
 
!********anillo 3**********************

 vvecinos(49) = c(sfl, scr,kk)
 vvecinos(50) = c(sfl, mcr,kk)
 vvecinos(51) = c(sfl, jcr,kk)
 vvecinos(52) = c(sfl, jj,kk)
 vvecinos(53) = c(sfl, jcl,kk)
 vvecinos(54) = c(sfl, mcl,kk)
 vvecinos(55) = c(sfl, scl,kk)
 
 vvecinos(56) = c(sfr, scr,kk)
 vvecinos(57) = c(sfr, mcr,kk)
 vvecinos(58) = c(sfr, jcr,kk)
 vvecinos(59) = c(sfr, jj,kk)
 vvecinos(60) = c(sfr, jcl,kk)
 vvecinos(61) = c(sfr, mcl,kk)
 vvecinos(62) = c(sfr, scl,kk)
 
 vvecinos(63) = c(mfl,scr,kk)
 vvecinos(64) = c(jfl,scr,kk)
 vvecinos(65) = c(ii,scr,kk)
 vvecinos(66) = c(jfr,scr,kk)
 vvecinos(67) = c(mfl,scr,kk)
 
 vvecinos(68) = c(mfl,scl,kk)
 vvecinos(69) = c(jfl,scl,kk)
 vvecinos(70) = c(ii,scl,kk)
 vvecinos(71) = c(jfr,scl,kk)
 vvecinos(72) = c(mfl,scl,kk)
 	 
!********anillo 4**********************
 vvecinos(73) = c(sfl, scr,jkl)
 vvecinos(74) = c(sfl, mcr,jkl)
 vvecinos(75) = c(sfl, jcr,jkl)
 vvecinos(76) = c(sfl, jj,jkl)
 vvecinos(77) = c(sfl, jcl,jkl)
 vvecinos(78) = c(sfl, mcl,jkl)
 vvecinos(79) = c(sfl, scl,jkl)
 
 vvecinos(80) = c(sfr, scr,jkl)
 vvecinos(81) = c(sfr, mcr,jkl)
 vvecinos(82) = c(sfr, jcr,jkl)
 vvecinos(83) = c(sfr, jj,jkl)
 vvecinos(84) = c(sfr, jcl,jkl)
 vvecinos(85) = c(sfr, mcl,jkl)
 vvecinos(86) = c(sfr, scl,jkl)
 
 vvecinos(87) = c(mfl,scr,jkl)
 vvecinos(88) = c(jfl,scr,jkl)
 vvecinos(89) = c(ii,scr,jkl)
 vvecinos(90) = c(jfr,scr,jkl)
 vvecinos(91) = c(mfl,scr,jkl)
 
 vvecinos(92) = c(mfl,scl,jkl)
 vvecinos(93) = c(jfl,scl,jkl)
 vvecinos(94) = c(ii,scl,jkl)
 vvecinos(95) = c(jfr,scl,jkl)
 vvecinos(96) = c(mfl,scl,jkl)
 
 !********anillo 5**********************
 vvecinos(97) = c(sfl, scr,mkl)
 vvecinos(98) = c(sfl, mcr,mkl)
 vvecinos(99) = c(sfl, jcr,mkl)
 vvecinos(100) = c(sfl, jj,mkl)
 vvecinos(101) = c(sfl, jcl,mkl)
 vvecinos(102) = c(sfl, mcl,mkl)
 vvecinos(103) = c(sfl, scl,mkl)
 
 vvecinos(104) = c(sfr, scr,mkl)
 vvecinos(105) = c(sfr, mcr,mkl)
 vvecinos(106) = c(sfr, jcr,mkl)
 vvecinos(107) = c(sfr, jj,mkl)
 vvecinos(108) = c(sfr, jcl,mkl)
 vvecinos(109) = c(sfr, mcl,mkl)
 vvecinos(110) = c(sfr, scl,mkl)
 
 vvecinos(111) = c(mfl,scr,mkl)
 vvecinos(112) = c(jfl,scr,mkl)
 vvecinos(113) = c(ii,scr,mkl)
 vvecinos(114) = c(jfr,scr,mkl)
 vvecinos(115) = c(mfl,scr,mkl)
 
 vvecinos(116) = c(mfl,scl,mkl)
 vvecinos(117) = c(jfl,scl,mkl)
 vvecinos(118) = c(ii,scl,mkl)
 vvecinos(119) = c(jfr,scl,mkl)
 vvecinos(120) = c(mfl,scl,mkl)
 
!************tapa1*******************

 vvecinos(121) = c(sfl, scr,skl)
 vvecinos(122) = c(sfl, mcr,skl)
 vvecinos(123) = c(sfl, jcr,skl)
 vvecinos(124) = c(sfl, jj,skl)
 vvecinos(125) = c(sfl, jcl,skl)
 vvecinos(126) = c(sfl, mcl,skl)
 vvecinos(127) = c(sfl, scl,skl)
 
 vvecinos(128) = c(mfl, scr,skl)
 vvecinos(129) = c(mfl, mcr,skl)
 vvecinos(130) = c(mfl, jcr,skl)
 vvecinos(131) = c(mfl, jj,skl)
 vvecinos(132) = c(mfl, jcl,skl)
 vvecinos(133) = c(mfl, mcl,skl)
 vvecinos(134) = c(mfl, scl,skl)
 
 vvecinos(135) = c(jfl, scr,skl)
 vvecinos(136) = c(jfl, mcr,skl)
 vvecinos(137) = c(jfl, jcr,skl)
 vvecinos(138) = c(jfl, jj,skl)
 vvecinos(139) = c(jfl, jcl,skl)
 vvecinos(140) = c(jfl, mcl,skl)
 vvecinos(141) = c(jfl, scl,skl)
 
 vvecinos(142) = c(ii, scr,skl)
 vvecinos(143) = c(ii, mcr,skl)
 vvecinos(144) = c(ii, jcr,skl)
 vvecinos(145) = c(ii, jj,skl)
 vvecinos(146) = c(ii, jcl,skl)
 vvecinos(147) = c(ii, mcl,skl)
 vvecinos(148) = c(ii, scl,skl)
 
 vvecinos(149) = c(jfr, scr,skl)
 vvecinos(150) = c(jfr, mcr,skl)
 vvecinos(151) = c(jfr, jcr,skl)
 vvecinos(152) = c(jfr, jj,skl)
 vvecinos(153) = c(jfr, jcl,skl)
 vvecinos(154) = c(jfr, mcl,skl)
 vvecinos(155) = c(jfr, scl,skl)
 
 vvecinos(156) = c(mfr, scr,skl)
 vvecinos(157) = c(mfr, mcr,skl)
 vvecinos(158) = c(mfr, jcr,skl)
 vvecinos(159) = c(mfr, jj,skl)
 vvecinos(160) = c(mfr, jcl,skl)
 vvecinos(161) = c(mfr, mcl,skl)
 vvecinos(162) = c(mfr, scl,skl)
 
 vvecinos(163) = c(sfr, scr,skl)
 vvecinos(164) = c(sfr, mcr,skl)
 vvecinos(165) = c(sfr, jcr,skl)
 vvecinos(166) = c(sfr, jj,skl)
 vvecinos(167) = c(sfr, jcl,skl)
 vvecinos(168) = c(sfr, mcl,skl)
 vvecinos(169) = c(sfr, scl,skl)
 
! ************tapa2*******************

 vvecinos(170) = c(sfl, scr,skr)
 vvecinos(171) = c(sfl, mcr,skr)
 vvecinos(172) = c(sfl, jcr,skr)
 vvecinos(173) = c(sfl, jj,skr)
 vvecinos(174) = c(sfl, jcl,skr)
 vvecinos(175) = c(sfl, mcl,skr)
 vvecinos(176) = c(sfl, scl,skr)
 
 vvecinos(177) = c(mfl, scr,skr)
 vvecinos(178) = c(mfl, mcr,skr)
 vvecinos(179) = c(mfl, jcr,skr)
 vvecinos(180) = c(mfl, jj,skr)
 vvecinos(181) = c(mfl, jcl,skr)
 vvecinos(182) = c(mfl, mcl,skr)
 vvecinos(183) = c(mfl, scl,skr)
 
 vvecinos(184) = c(jfl, scr,skr)
 vvecinos(185) = c(jfl, mcr,skr)
 vvecinos(186) = c(jfl, jcr,skr)
 vvecinos(187) = c(jfl, jj,skr)
 vvecinos(188) = c(jfl, jcl,skr)
 vvecinos(189) = c(jfl, mcl,skr)
 vvecinos(190) = c(jfl, scl,skr)
 
 vvecinos(191) = c(ii, scr,skr)
 vvecinos(192) = c(ii, mcr,skr)
 vvecinos(193) = c(ii, jcr,skr)
 vvecinos(194) = c(ii, jj,skr)
 vvecinos(195) = c(ii, jcl,skr)
 vvecinos(196) = c(ii, mcl,skr)
 vvecinos(197) = c(ii, scl,skr)
 
 vvecinos(198) = c(jfr, scr,skr)
 vvecinos(199) = c(jfr, mcr,skr)
 vvecinos(200) = c(jfr, jcr,skr)
 vvecinos(201) = c(jfr, jj,skr)
 vvecinos(202) = c(jfr, jcl,skr)
 vvecinos(203) = c(jfr, mcl,skr)
 vvecinos(204) = c(jfr, scl,skr)
 
 vvecinos(205) = c(mfr, scr,skr)
 vvecinos(206) = c(mfr, mcr,skr)
 vvecinos(207) = c(mfr, jcr,skr)
 vvecinos(208) = c(mfr, jj,skr)
 vvecinos(209) = c(mfr, jcl,skr)
 vvecinos(210) = c(mfr, mcl,skr)
 vvecinos(211) = c(mfr, scl,skr)
 
 vvecinos(212) = c(sfr, scr,skr)
 vvecinos(213) = c(sfr, mcr,skr)
 vvecinos(214) = c(sfr, jcr,skr)
 vvecinos(215) = c(sfr, jj,skr)
 vvecinos(216) = c(sfr, jcl,skr)
 vvecinos(217) = c(sfr, mcl,skr)
 vvecinos(218) = c(sfr, scl,skr)

 	  


numero_gauss1=0
numero_gauss2=0
rrrr=1
rrrrr=1

do s=1,218
if (vvecinos(s)/=0) rrrr=s 
end do 

do s=1,218
if ((vvecinos(s)/=0) .and. (vvecinos(s)/=vvecinos(rrrr))) rrrrr=s 
end do

do s=1,218
if (vvecinos(s)==vvecinos(rrrr)) numero_gauss1=numero_gauss1+1 
end do 

do s=1,218 
if (vvecinos(s)==vvecinos(rrrrr)) numero_gauss2=numero_gauss2+1 
end do 


!esto quiere decir que la particula esta centrada en el borde de grano GB
if (abs(numero_gauss2-numero_gauss1)<=10) vect2=1

!esto quiere decir que la particula esta tocando el borde de grano GB y se movera al volumen chico
!if ((gauss>=numero_gauss2)) vect1=1


end function



! *********************************************************************
!                Function veci (FORTRAN)
!
!  This subroutine calculates the neighbour of a particle 
!
!								Version 1.0
!								7/06/16
!
! *********************************************************************



Integer Function Ve2(c,ii,jj,kk,filas,columnas,ancho)
 
    IMPLICIT NONE

integer::filas,columnas,ancho
integer:: ii,jj,kk,jfl,jfr,jcl,jcr,jkl,jkr,mfl,mfr,mcl,mcr,mkl,mkr,s,sfl,sfr,skl,skr,scl,scr
		  


integer,dimension(filas,columnas,ancho):: c
integer, dimension(218) :: vvecinos

ve2=0

 jfl = ii-1 ;  if (jfl < 1)        jfl = filas
 jcl = jj-1 ;  if (jcl < 1)        jcl = columnas
 jkl = kk-1 ;  if (jkl < 1)        jkl = ancho
 
 jfr = ii+1 ;  if (jfr > filas)    jfr = 1
 jcr = jj+1 ;  if (jcr > columnas) jcr = 1
 jkr = kk+1 ;  if (jkr > ancho)    jkr = 1

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


!&&&&&&&&&&&&&&&&&&&&&&&&&&&&&

sfl=ii-3
 if (sfl==-2) then 
 sfl = filas-2
 else
 if (sfl==0) then
 sfl = filas
 end if
 end if

 
scl=jj-3
 if (scl==-2) then 
 scl = columnas-2
 else
 if (scl==0) then
 scl = columnas
 end if
 end if

 skl=kk-3
 if (skl==-2) then 
 skl = ancho-2
 else
 if (skl==0) then
 skl = ancho
 end if
 end if
 
 sfr=ii+3
 if (sfr==filas+3) then 
 sfr = 3
 else
 if (sfr==filas+2) then
 sfr = 2
 end if
 end if
 
 scr=jj+3
 if (scr==columnas+3) then 
 scr = 3
 else
 if (scr==columnas+2) then
 scr = 2
 end if
 end if

 
 skr=kk+3
 if (skr==ancho+3) then 
 skr = 3
 else
 if (skr==ancho+2) then
 skr = 2
 end if
 end if


!********anillo 1**********************
 vvecinos(1) = c(sfl, scr,mkr)
 vvecinos(2) = c(sfl, mcr,mkr)
 vvecinos(3) = c(sfl, jcr,mkr)
 vvecinos(4) = c(sfl, jj,mkr)
 vvecinos(5) = c(sfl, jcl,mkr)
 vvecinos(6) = c(sfl, mcl,mkr)
 vvecinos(7) = c(sfl, scl,mkr)
 
 vvecinos(8) = c(sfr, scr,mkr)
 vvecinos(9) = c(sfr, mcr,mkr)
 vvecinos(10) = c(sfr, jcr,mkr)
 vvecinos(11) = c(sfr, jj,mkr)
 vvecinos(12) = c(sfr, jcl,mkr)
 vvecinos(13) = c(sfr, mcl,mkr)
 vvecinos(14) = c(sfr, scl,mkr)
 
 vvecinos(15) = c(mfl,scr,mkr)
 vvecinos(16) = c(jfl,scr,mkr)
 vvecinos(17) = c(ii,scr,mkr)
 vvecinos(18) = c(jfr,scr,mkr)
 vvecinos(19) = c(mfl,scr,mkr)
 
 vvecinos(20) = c(mfl,scl,mkr)
 vvecinos(21) = c(jfl,scl,mkr)
 vvecinos(22) = c(ii,scl,mkr)
 vvecinos(23) = c(jfr,scl,mkr)
 vvecinos(24) = c(mfl,scl,mkr)

 !********anillo 2**********************
 
 vvecinos(25) = c(sfl, scr,jkr)
 vvecinos(26) = c(sfl, mcr,jkr)
 vvecinos(27) = c(sfl, jcr,jkr)
 vvecinos(28) = c(sfl, jj,jkr)
 vvecinos(29) = c(sfl, jcl,jkr)
 vvecinos(30) = c(sfl, mcl,jkr)
 vvecinos(31) = c(sfl, scl,jkr)
 
 vvecinos(32) = c(sfr, scr,jkr)
 vvecinos(33) = c(sfr, mcr,jkr)
 vvecinos(34) = c(sfr, jcr,jkr)
 vvecinos(35) = c(sfr, jj,jkr)
 vvecinos(36) = c(sfr, jcl,jkr)
 vvecinos(37) = c(sfr, mcl,jkr)
 vvecinos(38) = c(sfr, scl,jkr)
 
 vvecinos(39) = c(mfl,scr,jkr)
 vvecinos(40) = c(jfl,scr,jkr)
 vvecinos(41) = c(ii,scr,jkr)
 vvecinos(42) = c(jfr,scr,jkr)
 vvecinos(43) = c(mfl,scr,jkr)
 
 vvecinos(44) = c(mfl,scl,jkr)
 vvecinos(45) = c(jfl,scl,jkr)
 vvecinos(46) = c(ii,scl,jkr)
 vvecinos(47) = c(jfr,scl,jkr)
 vvecinos(48) = c(mfl,scl,jkr)
 
!********anillo 3**********************

 vvecinos(49) = c(sfl, scr,kk)
 vvecinos(50) = c(sfl, mcr,kk)
 vvecinos(51) = c(sfl, jcr,kk)
 vvecinos(52) = c(sfl, jj,kk)
 vvecinos(53) = c(sfl, jcl,kk)
 vvecinos(54) = c(sfl, mcl,kk)
 vvecinos(55) = c(sfl, scl,kk)
 
 vvecinos(56) = c(sfr, scr,kk)
 vvecinos(57) = c(sfr, mcr,kk)
 vvecinos(58) = c(sfr, jcr,kk)
 vvecinos(59) = c(sfr, jj,kk)
 vvecinos(60) = c(sfr, jcl,kk)
 vvecinos(61) = c(sfr, mcl,kk)
 vvecinos(62) = c(sfr, scl,kk)
 
 vvecinos(63) = c(mfl,scr,kk)
 vvecinos(64) = c(jfl,scr,kk)
 vvecinos(65) = c(ii,scr,kk)
 vvecinos(66) = c(jfr,scr,kk)
 vvecinos(67) = c(mfl,scr,kk)
 
 vvecinos(68) = c(mfl,scl,kk)
 vvecinos(69) = c(jfl,scl,kk)
 vvecinos(70) = c(ii,scl,kk)
 vvecinos(71) = c(jfr,scl,kk)
 vvecinos(72) = c(mfl,scl,kk)
 	 
!********anillo 4**********************
 vvecinos(73) = c(sfl, scr,jkl)
 vvecinos(74) = c(sfl, mcr,jkl)
 vvecinos(75) = c(sfl, jcr,jkl)
 vvecinos(76) = c(sfl, jj,jkl)
 vvecinos(77) = c(sfl, jcl,jkl)
 vvecinos(78) = c(sfl, mcl,jkl)
 vvecinos(79) = c(sfl, scl,jkl)
 
 vvecinos(80) = c(sfr, scr,jkl)
 vvecinos(81) = c(sfr, mcr,jkl)
 vvecinos(82) = c(sfr, jcr,jkl)
 vvecinos(83) = c(sfr, jj,jkl)
 vvecinos(84) = c(sfr, jcl,jkl)
 vvecinos(85) = c(sfr, mcl,jkl)
 vvecinos(86) = c(sfr, scl,jkl)
 
 vvecinos(87) = c(mfl,scr,jkl)
 vvecinos(88) = c(jfl,scr,jkl)
 vvecinos(89) = c(ii,scr,jkl)
 vvecinos(90) = c(jfr,scr,jkl)
 vvecinos(91) = c(mfl,scr,jkl)
 
 vvecinos(92) = c(mfl,scl,jkl)
 vvecinos(93) = c(jfl,scl,jkl)
 vvecinos(94) = c(ii,scl,jkl)
 vvecinos(95) = c(jfr,scl,jkl)
 vvecinos(96) = c(mfl,scl,jkl)
 
 !********anillo 5**********************
 vvecinos(97) = c(sfl, scr,mkl)
 vvecinos(98) = c(sfl, mcr,mkl)
 vvecinos(99) = c(sfl, jcr,mkl)
 vvecinos(100) = c(sfl, jj,mkl)
 vvecinos(101) = c(sfl, jcl,mkl)
 vvecinos(102) = c(sfl, mcl,mkl)
 vvecinos(103) = c(sfl, scl,mkl)
 
 vvecinos(104) = c(sfr, scr,mkl)
 vvecinos(105) = c(sfr, mcr,mkl)
 vvecinos(106) = c(sfr, jcr,mkl)
 vvecinos(107) = c(sfr, jj,mkl)
 vvecinos(108) = c(sfr, jcl,mkl)
 vvecinos(109) = c(sfr, mcl,mkl)
 vvecinos(110) = c(sfr, scl,mkl)
 
 vvecinos(111) = c(mfl,scr,mkl)
 vvecinos(112) = c(jfl,scr,mkl)
 vvecinos(113) = c(ii,scr,mkl)
 vvecinos(114) = c(jfr,scr,mkl)
 vvecinos(115) = c(mfl,scr,mkl)
 
 vvecinos(116) = c(mfl,scl,mkl)
 vvecinos(117) = c(jfl,scl,mkl)
 vvecinos(118) = c(ii,scl,mkl)
 vvecinos(119) = c(jfr,scl,mkl)
 vvecinos(120) = c(mfl,scl,mkl)
 
!************tapa1*******************

 vvecinos(121) = c(sfl, scr,skl)
 vvecinos(122) = c(sfl, mcr,skl)
 vvecinos(123) = c(sfl, jcr,skl)
 vvecinos(124) = c(sfl, jj,skl)
 vvecinos(125) = c(sfl, jcl,skl)
 vvecinos(126) = c(sfl, mcl,skl)
 vvecinos(127) = c(sfl, scl,skl)
 
 vvecinos(128) = c(mfl, scr,skl)
 vvecinos(129) = c(mfl, mcr,skl)
 vvecinos(130) = c(mfl, jcr,skl)
 vvecinos(131) = c(mfl, jj,skl)
 vvecinos(132) = c(mfl, jcl,skl)
 vvecinos(133) = c(mfl, mcl,skl)
 vvecinos(134) = c(mfl, scl,skl)
 
 vvecinos(135) = c(jfl, scr,skl)
 vvecinos(136) = c(jfl, mcr,skl)
 vvecinos(137) = c(jfl, jcr,skl)
 vvecinos(138) = c(jfl, jj,skl)
 vvecinos(139) = c(jfl, jcl,skl)
 vvecinos(140) = c(jfl, mcl,skl)
 vvecinos(141) = c(jfl, scl,skl)
 
 vvecinos(142) = c(ii, scr,skl)
 vvecinos(143) = c(ii, mcr,skl)
 vvecinos(144) = c(ii, jcr,skl)
 vvecinos(145) = c(ii, jj,skl)
 vvecinos(146) = c(ii, jcl,skl)
 vvecinos(147) = c(ii, mcl,skl)
 vvecinos(148) = c(ii, scl,skl)
 
 vvecinos(149) = c(jfr, scr,skl)
 vvecinos(150) = c(jfr, mcr,skl)
 vvecinos(151) = c(jfr, jcr,skl)
 vvecinos(152) = c(jfr, jj,skl)
 vvecinos(153) = c(jfr, jcl,skl)
 vvecinos(154) = c(jfr, mcl,skl)
 vvecinos(155) = c(jfr, scl,skl)
 
 vvecinos(156) = c(mfr, scr,skl)
 vvecinos(157) = c(mfr, mcr,skl)
 vvecinos(158) = c(mfr, jcr,skl)
 vvecinos(159) = c(mfr, jj,skl)
 vvecinos(160) = c(mfr, jcl,skl)
 vvecinos(161) = c(mfr, mcl,skl)
 vvecinos(162) = c(mfr, scl,skl)
 
 vvecinos(163) = c(sfr, scr,skl)
 vvecinos(164) = c(sfr, mcr,skl)
 vvecinos(165) = c(sfr, jcr,skl)
 vvecinos(166) = c(sfr, jj,skl)
 vvecinos(167) = c(sfr, jcl,skl)
 vvecinos(168) = c(sfr, mcl,skl)
 vvecinos(169) = c(sfr, scl,skl)
 
! ************tapa2*******************

 vvecinos(170) = c(sfl, scr,skr)
 vvecinos(171) = c(sfl, mcr,skr)
 vvecinos(172) = c(sfl, jcr,skr)
 vvecinos(173) = c(sfl, jj,skr)
 vvecinos(174) = c(sfl, jcl,skr)
 vvecinos(175) = c(sfl, mcl,skr)
 vvecinos(176) = c(sfl, scl,skr)
 
 vvecinos(177) = c(mfl, scr,skr)
 vvecinos(178) = c(mfl, mcr,skr)
 vvecinos(179) = c(mfl, jcr,skr)
 vvecinos(180) = c(mfl, jj,skr)
 vvecinos(181) = c(mfl, jcl,skr)
 vvecinos(182) = c(mfl, mcl,skr)
 vvecinos(183) = c(mfl, scl,skr)
 
 vvecinos(184) = c(jfl, scr,skr)
 vvecinos(185) = c(jfl, mcr,skr)
 vvecinos(186) = c(jfl, jcr,skr)
 vvecinos(187) = c(jfl, jj,skr)
 vvecinos(188) = c(jfl, jcl,skr)
 vvecinos(189) = c(jfl, mcl,skr)
 vvecinos(190) = c(jfl, scl,skr)
 
 vvecinos(191) = c(ii, scr,skr)
 vvecinos(192) = c(ii, mcr,skr)
 vvecinos(193) = c(ii, jcr,skr)
 vvecinos(194) = c(ii, jj,skr)
 vvecinos(195) = c(ii, jcl,skr)
 vvecinos(196) = c(ii, mcl,skr)
 vvecinos(197) = c(ii, scl,skr)
 
 vvecinos(198) = c(jfr, scr,skr)
 vvecinos(199) = c(jfr, mcr,skr)
 vvecinos(200) = c(jfr, jcr,skr)
 vvecinos(201) = c(jfr, jj,skr)
 vvecinos(202) = c(jfr, jcl,skr)
 vvecinos(203) = c(jfr, mcl,skr)
 vvecinos(204) = c(jfr, scl,skr)
 
 vvecinos(205) = c(mfr, scr,skr)
 vvecinos(206) = c(mfr, mcr,skr)
 vvecinos(207) = c(mfr, jcr,skr)
 vvecinos(208) = c(mfr, jj,skr)
 vvecinos(209) = c(mfr, jcl,skr)
 vvecinos(210) = c(mfr, mcl,skr)
 vvecinos(211) = c(mfr, scl,skr)
 
 vvecinos(212) = c(sfr, scr,skr)
 vvecinos(213) = c(sfr, mcr,skr)
 vvecinos(214) = c(sfr, jcr,skr)
 vvecinos(215) = c(sfr, jj,skr)
 vvecinos(216) = c(sfr, jcl,skr)
 vvecinos(217) = c(sfr, mcl,skr)
 vvecinos(218) = c(sfr, scl,skr)

do s=1,218
if (vvecinos(s)==0) ve2=1
end do


end function


! *********************************************************************
!                Function borde (FORTRAN)
!
!  This subroutine calculates the neighbour of a particle 
!
!								Version 1.0
!								7/06/16
!
! *********************************************************************



Integer Function Vec2(c,ii,jj,kk,filas,columnas,ancho)
 
    IMPLICIT NONE

integer::filas,columnas,ancho,s,rrrr
integer:: ii,jj,kk,jfl,jfr,jcl,jcr,jkl,jkr,mfl,mfr,mcl,mcr,mkl,mkr,sfl,sfr,skl,skr,scl,scr

integer,dimension(filas,columnas,ancho):: c
integer, dimension(218) :: vvecinos

rrrr=1


jfl = ii-1 ;  if (jfl < 1)        jfl = filas
 jcl = jj-1 ;  if (jcl < 1)        jcl = columnas
 jkl = kk-1 ;  if (jkl < 1)        jkl = ancho
 
 jfr = ii+1 ;  if (jfr > filas)    jfr = 1
 jcr = jj+1 ;  if (jcr > columnas) jcr = 1
 jkr = kk+1 ;  if (jkr > ancho)    jkr = 1

vec2=0

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
!&&&&&&&&&&&&&&&&&&&&&&&&&&&&&

sfl=ii-3
 if (sfl==-2) then 
 sfl = filas-2
 else
 if (sfl==0) then
 sfl = filas
 end if
 end if

 
scl=jj-3
 if (scl==-2) then 
 scl = columnas-2
 else
 if (scl==0) then
 scl = columnas
 end if
 end if

 skl=kk-3
 if (skl==-2) then 
 skl = ancho-2
 else
 if (skl==0) then
 skl = ancho
 end if
 end if
 
 sfr=ii+3
 if (sfr==filas+3) then 
 sfr = 3
 else
 if (sfr==filas+2) then
 sfr = 2
 end if
 end if
 
 scr=jj+3
 if (scr==columnas+3) then 
 scr = 3
 else
 if (scr==columnas+2) then
 scr = 2
 end if
 end if

 
 skr=kk+3
 if (skr==ancho+3) then 
 skr = 3
 else
 if (skr==ancho+2) then
 skr = 2
 end if
 end if


!********anillo 1**********************
 vvecinos(1) = c(sfl, scr,mkr)
 vvecinos(2) = c(sfl, mcr,mkr)
 vvecinos(3) = c(sfl, jcr,mkr)
 vvecinos(4) = c(sfl, jj,mkr)
 vvecinos(5) = c(sfl, jcl,mkr)
 vvecinos(6) = c(sfl, mcl,mkr)
 vvecinos(7) = c(sfl, scl,mkr)
 
 vvecinos(8) = c(sfr, scr,mkr)
 vvecinos(9) = c(sfr, mcr,mkr)
 vvecinos(10) = c(sfr, jcr,mkr)
 vvecinos(11) = c(sfr, jj,mkr)
 vvecinos(12) = c(sfr, jcl,mkr)
 vvecinos(13) = c(sfr, mcl,mkr)
 vvecinos(14) = c(sfr, scl,mkr)
 
 vvecinos(15) = c(mfl,scr,mkr)
 vvecinos(16) = c(jfl,scr,mkr)
 vvecinos(17) = c(ii,scr,mkr)
 vvecinos(18) = c(jfr,scr,mkr)
 vvecinos(19) = c(mfl,scr,mkr)
 
 vvecinos(20) = c(mfl,scl,mkr)
 vvecinos(21) = c(jfl,scl,mkr)
 vvecinos(22) = c(ii,scl,mkr)
 vvecinos(23) = c(jfr,scl,mkr)
 vvecinos(24) = c(mfl,scl,mkr)

 !********anillo 2**********************
 
 vvecinos(25) = c(sfl, scr,jkr)
 vvecinos(26) = c(sfl, mcr,jkr)
 vvecinos(27) = c(sfl, jcr,jkr)
 vvecinos(28) = c(sfl, jj,jkr)
 vvecinos(29) = c(sfl, jcl,jkr)
 vvecinos(30) = c(sfl, mcl,jkr)
 vvecinos(31) = c(sfl, scl,jkr)
 
 vvecinos(32) = c(sfr, scr,jkr)
 vvecinos(33) = c(sfr, mcr,jkr)
 vvecinos(34) = c(sfr, jcr,jkr)
 vvecinos(35) = c(sfr, jj,jkr)
 vvecinos(36) = c(sfr, jcl,jkr)
 vvecinos(37) = c(sfr, mcl,jkr)
 vvecinos(38) = c(sfr, scl,jkr)
 
 vvecinos(39) = c(mfl,scr,jkr)
 vvecinos(40) = c(jfl,scr,jkr)
 vvecinos(41) = c(ii,scr,jkr)
 vvecinos(42) = c(jfr,scr,jkr)
 vvecinos(43) = c(mfl,scr,jkr)
 
 vvecinos(44) = c(mfl,scl,jkr)
 vvecinos(45) = c(jfl,scl,jkr)
 vvecinos(46) = c(ii,scl,jkr)
 vvecinos(47) = c(jfr,scl,jkr)
 vvecinos(48) = c(mfl,scl,jkr)
 
!********anillo 3**********************

 vvecinos(49) = c(sfl, scr,kk)
 vvecinos(50) = c(sfl, mcr,kk)
 vvecinos(51) = c(sfl, jcr,kk)
 vvecinos(52) = c(sfl, jj,kk)
 vvecinos(53) = c(sfl, jcl,kk)
 vvecinos(54) = c(sfl, mcl,kk)
 vvecinos(55) = c(sfl, scl,kk)
 
 vvecinos(56) = c(sfr, scr,kk)
 vvecinos(57) = c(sfr, mcr,kk)
 vvecinos(58) = c(sfr, jcr,kk)
 vvecinos(59) = c(sfr, jj,kk)
 vvecinos(60) = c(sfr, jcl,kk)
 vvecinos(61) = c(sfr, mcl,kk)
 vvecinos(62) = c(sfr, scl,kk)
 
 vvecinos(63) = c(mfl,scr,kk)
 vvecinos(64) = c(jfl,scr,kk)
 vvecinos(65) = c(ii,scr,kk)
 vvecinos(66) = c(jfr,scr,kk)
 vvecinos(67) = c(mfl,scr,kk)
 
 vvecinos(68) = c(mfl,scl,kk)
 vvecinos(69) = c(jfl,scl,kk)
 vvecinos(70) = c(ii,scl,kk)
 vvecinos(71) = c(jfr,scl,kk)
 vvecinos(72) = c(mfl,scl,kk)
 	 
!********anillo 4**********************
 vvecinos(73) = c(sfl, scr,jkl)
 vvecinos(74) = c(sfl, mcr,jkl)
 vvecinos(75) = c(sfl, jcr,jkl)
 vvecinos(76) = c(sfl, jj,jkl)
 vvecinos(77) = c(sfl, jcl,jkl)
 vvecinos(78) = c(sfl, mcl,jkl)
 vvecinos(79) = c(sfl, scl,jkl)
 
 vvecinos(80) = c(sfr, scr,jkl)
 vvecinos(81) = c(sfr, mcr,jkl)
 vvecinos(82) = c(sfr, jcr,jkl)
 vvecinos(83) = c(sfr, jj,jkl)
 vvecinos(84) = c(sfr, jcl,jkl)
 vvecinos(85) = c(sfr, mcl,jkl)
 vvecinos(86) = c(sfr, scl,jkl)
 
 vvecinos(87) = c(mfl,scr,jkl)
 vvecinos(88) = c(jfl,scr,jkl)
 vvecinos(89) = c(ii,scr,jkl)
 vvecinos(90) = c(jfr,scr,jkl)
 vvecinos(91) = c(mfl,scr,jkl)
 
 vvecinos(92) = c(mfl,scl,jkl)
 vvecinos(93) = c(jfl,scl,jkl)
 vvecinos(94) = c(ii,scl,jkl)
 vvecinos(95) = c(jfr,scl,jkl)
 vvecinos(96) = c(mfl,scl,jkl)
 
 !********anillo 5**********************
 vvecinos(97) = c(sfl, scr,mkl)
 vvecinos(98) = c(sfl, mcr,mkl)
 vvecinos(99) = c(sfl, jcr,mkl)
 vvecinos(100) = c(sfl, jj,mkl)
 vvecinos(101) = c(sfl, jcl,mkl)
 vvecinos(102) = c(sfl, mcl,mkl)
 vvecinos(103) = c(sfl, scl,mkl)
 
 vvecinos(104) = c(sfr, scr,mkl)
 vvecinos(105) = c(sfr, mcr,mkl)
 vvecinos(106) = c(sfr, jcr,mkl)
 vvecinos(107) = c(sfr, jj,mkl)
 vvecinos(108) = c(sfr, jcl,mkl)
 vvecinos(109) = c(sfr, mcl,mkl)
 vvecinos(110) = c(sfr, scl,mkl)
 
 vvecinos(111) = c(mfl,scr,mkl)
 vvecinos(112) = c(jfl,scr,mkl)
 vvecinos(113) = c(ii,scr,mkl)
 vvecinos(114) = c(jfr,scr,mkl)
 vvecinos(115) = c(mfl,scr,mkl)
 
 vvecinos(116) = c(mfl,scl,mkl)
 vvecinos(117) = c(jfl,scl,mkl)
 vvecinos(118) = c(ii,scl,mkl)
 vvecinos(119) = c(jfr,scl,mkl)
 vvecinos(120) = c(mfl,scl,mkl)
 
!************tapa1*******************

 vvecinos(121) = c(sfl, scr,skl)
 vvecinos(122) = c(sfl, mcr,skl)
 vvecinos(123) = c(sfl, jcr,skl)
 vvecinos(124) = c(sfl, jj,skl)
 vvecinos(125) = c(sfl, jcl,skl)
 vvecinos(126) = c(sfl, mcl,skl)
 vvecinos(127) = c(sfl, scl,skl)
 
 vvecinos(128) = c(mfl, scr,skl)
 vvecinos(129) = c(mfl, mcr,skl)
 vvecinos(130) = c(mfl, jcr,skl)
 vvecinos(131) = c(mfl, jj,skl)
 vvecinos(132) = c(mfl, jcl,skl)
 vvecinos(133) = c(mfl, mcl,skl)
 vvecinos(134) = c(mfl, scl,skl)
 
 vvecinos(135) = c(jfl, scr,skl)
 vvecinos(136) = c(jfl, mcr,skl)
 vvecinos(137) = c(jfl, jcr,skl)
 vvecinos(138) = c(jfl, jj,skl)
 vvecinos(139) = c(jfl, jcl,skl)
 vvecinos(140) = c(jfl, mcl,skl)
 vvecinos(141) = c(jfl, scl,skl)
 
 vvecinos(142) = c(ii, scr,skl)
 vvecinos(143) = c(ii, mcr,skl)
 vvecinos(144) = c(ii, jcr,skl)
 vvecinos(145) = c(ii, jj,skl)
 vvecinos(146) = c(ii, jcl,skl)
 vvecinos(147) = c(ii, mcl,skl)
 vvecinos(148) = c(ii, scl,skl)
 
 vvecinos(149) = c(jfr, scr,skl)
 vvecinos(150) = c(jfr, mcr,skl)
 vvecinos(151) = c(jfr, jcr,skl)
 vvecinos(152) = c(jfr, jj,skl)
 vvecinos(153) = c(jfr, jcl,skl)
 vvecinos(154) = c(jfr, mcl,skl)
 vvecinos(155) = c(jfr, scl,skl)
 
 vvecinos(156) = c(mfr, scr,skl)
 vvecinos(157) = c(mfr, mcr,skl)
 vvecinos(158) = c(mfr, jcr,skl)
 vvecinos(159) = c(mfr, jj,skl)
 vvecinos(160) = c(mfr, jcl,skl)
 vvecinos(161) = c(mfr, mcl,skl)
 vvecinos(162) = c(mfr, scl,skl)
 
 vvecinos(163) = c(sfr, scr,skl)
 vvecinos(164) = c(sfr, mcr,skl)
 vvecinos(165) = c(sfr, jcr,skl)
 vvecinos(166) = c(sfr, jj,skl)
 vvecinos(167) = c(sfr, jcl,skl)
 vvecinos(168) = c(sfr, mcl,skl)
 vvecinos(169) = c(sfr, scl,skl)
 
! ************tapa2*******************

 vvecinos(170) = c(sfl, scr,skr)
 vvecinos(171) = c(sfl, mcr,skr)
 vvecinos(172) = c(sfl, jcr,skr)
 vvecinos(173) = c(sfl, jj,skr)
 vvecinos(174) = c(sfl, jcl,skr)
 vvecinos(175) = c(sfl, mcl,skr)
 vvecinos(176) = c(sfl, scl,skr)
 
 vvecinos(177) = c(mfl, scr,skr)
 vvecinos(178) = c(mfl, mcr,skr)
 vvecinos(179) = c(mfl, jcr,skr)
 vvecinos(180) = c(mfl, jj,skr)
 vvecinos(181) = c(mfl, jcl,skr)
 vvecinos(182) = c(mfl, mcl,skr)
 vvecinos(183) = c(mfl, scl,skr)
 
 vvecinos(184) = c(jfl, scr,skr)
 vvecinos(185) = c(jfl, mcr,skr)
 vvecinos(186) = c(jfl, jcr,skr)
 vvecinos(187) = c(jfl, jj,skr)
 vvecinos(188) = c(jfl, jcl,skr)
 vvecinos(189) = c(jfl, mcl,skr)
 vvecinos(190) = c(jfl, scl,skr)
 
 vvecinos(191) = c(ii, scr,skr)
 vvecinos(192) = c(ii, mcr,skr)
 vvecinos(193) = c(ii, jcr,skr)
 vvecinos(194) = c(ii, jj,skr)
 vvecinos(195) = c(ii, jcl,skr)
 vvecinos(196) = c(ii, mcl,skr)
 vvecinos(197) = c(ii, scl,skr)
 
 vvecinos(198) = c(jfr, scr,skr)
 vvecinos(199) = c(jfr, mcr,skr)
 vvecinos(200) = c(jfr, jcr,skr)
 vvecinos(201) = c(jfr, jj,skr)
 vvecinos(202) = c(jfr, jcl,skr)
 vvecinos(203) = c(jfr, mcl,skr)
 vvecinos(204) = c(jfr, scl,skr)
 
 vvecinos(205) = c(mfr, scr,skr)
 vvecinos(206) = c(mfr, mcr,skr)
 vvecinos(207) = c(mfr, jcr,skr)
 vvecinos(208) = c(mfr, jj,skr)
 vvecinos(209) = c(mfr, jcl,skr)
 vvecinos(210) = c(mfr, mcl,skr)
 vvecinos(211) = c(mfr, scl,skr)
 
 vvecinos(212) = c(sfr, scr,skr)
 vvecinos(213) = c(sfr, mcr,skr)
 vvecinos(214) = c(sfr, jcr,skr)
 vvecinos(215) = c(sfr, jj,skr)
 vvecinos(216) = c(sfr, jcl,skr)
 vvecinos(217) = c(sfr, mcl,skr)
 vvecinos(218) = c(sfr, scl,skr)
 
 
 
do s=1,218 
if (vvecinos(s)/=0) rrrr=s 
end do 

do s=1,218
if ((vvecinos(s)/=vvecinos(rrrr)) .and. (vvecinos(s)/=0)) vec2=1 
end do 

end function


! *********************************************************************
!                Function centro (FORTRAN)
!
!  This subroutine calculates the neighbour of a particle 
!
!								Version 1.0
!								7/06/16
!
! *********************************************************************



Integer Function Vet2(c,ii,jj,kk,filas,columnas,ancho)
 
    IMPLICIT NONE

integer::filas,columnas,ancho
integer:: ii,jj,kk,jfl,jfr,jcl,jcr,jkl,jkr,mfl,mfr,mcl,mcr,mkl,mkr,s
		  


integer,dimension(filas,columnas,ancho):: c
integer, dimension(98) :: vvecinos

vet2=1

 jfl = ii-1 ;  if (jfl < 1)        jfl = filas 
jcl = jj-1 ;  if (jcl < 1)        jcl = columnas 
jkl = kk-1 ;  if (jkl < 1)        jkl = ancho 
jfr = ii+1 ;  if (jfr > filas)    jfr = 1 
jcr = jj+1 ;  if (jcr > columnas) jcr = 1 
jkr = kk+1 ;  if (jkr > ancho)    jkr = 1  

 
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
 
do s=1,98
if (vvecinos(s)==0) vet2=vet2+1
end do 
if (vet2==99) then
vet2=1
else 
vet2=0
end if
 	 
end function


!*********************************************************************
!                Function matrix(FORTRAN)
!
!  esta rutina calcula si la particula esta  de la region de calculo) 
!  arroja una variable vecm=0  esta y vecm=1 si no esta  
!
!*********************************************************************

Integer function Vecm2(radius,ii,jj,kk,filas,columnas,ancho)
IMPLICIT NONE
integer     :: filas,columnas,ancho
integer     :: ii,jj,kk,radius
vecm2=0
if ((ii-radius-1<=1) .or.(ii+radius+1>=filas))  vecm2=1
if ((jj-radius-1<=1) .or.(jj+radius+1>=columnas))  vecm2=1
if ((kk-radius-1<=1) .or.(kk+radius+1>=ancho))  vecm2=1
end function

end module radio2
