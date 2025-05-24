module hielonn

use presicion

 contains


!****************************************************************
!                         FUNCTION Rand0
!
!    Esta subrotine es para inicializar la función Rand()
!
!****************************************************************


subroutine Rand0

implicit None
integer                 :: Iseed1, Iseed2, Count
integer, dimension(33)  :: Seed
real(pr), dimension(2)  :: R
real(pr)                :: Rand
COMMON/RSEED/ISEED1,ISEED2
Call System_Clock( Count )
Seed = Count
Call Random_Seed( PUT = Seed )
Call Random_Number( R )
Iseed1 = IdnInt( R(1) * 2.0D0**31 )
Iseed2 = IDNINT( R(2)    * 2.0D0**31 )
end Subroutine Rand0



!******************************************************************
!                         Function Rand
!    
!
!     This is an adapted version of Subroutine RANECU written by 
!  F. JAMES (COMPUT. PHYS. COMMUN. 60 (1990) 329-344), which has 
!  been modified to give a single Random number at each call.
!  The 'SEEDS' ISEED1 AND ISEED2 must be initialized in the main 
!  program by call the subroutine Rand0 and transferred through the 
!  named COMMON BLOCK/RSEED/.
!                            
!******************************************************************
   
   
real(pr) function Rand()
implicit none
real(pr)     ::  Uscale
integer      ::  Iseed1, Iseed2, I1, I2,Iz
PARAMETER (USCALE=1.0D0/2.0D0**31)
COMMON/RSEED/ISEED1,ISEED2
I1=ISEED1/53668
ISEED1=40014*(ISEED1-I1*53668)-I1*12211
IF(ISEED1.LT.0) ISEED1=ISEED1+2147483563
      I2=ISEED2/52774
      ISEED2=40692*(ISEED2-I2*52774)-I2*3791
      IF(ISEED2.LT.0) ISEED2=ISEED2+2147483399
           IZ=ISEED1-ISEED2
           IF(IZ.LT.1) IZ=IZ+2147483562
              Rand = Iz * Uscale
end function Rand


!  *********************************************************************
!                Subroutine Timer0 (for ANSI FORTRAN 77)
!
!  This subroutine gives the execution time in seconds. It calls the
!  function DATE_AND_TIME (of ANSI FORTRAN) to find the current time.
!  The output value of the variable SEC is the time (in seconds) 
!  elapsed since the last call to TEMPO.
!
!                            Version 1.0
!                            23/11/08
!  *********************************************************************


Subroutine Timer0()


    Integer*4 :: Values(8), Mes(13), Anio(20)
    Character*10 :: Date, Times, Zone
    
    COMMON/SUBTEMPO/Anio, Mes
      
    Mes(1)  = 0
    Mes(2)  = 31 * 24 * 3600
    Mes(3)  = 59 * 24 * 3600
    Mes(4)  = 90 * 24 * 3600
    Mes(5)  = 120 * 24 * 3600
    Mes(6)  = 151 * 24 * 3600
    Mes(7)  = 181 * 24 * 3600
    Mes(8)  = 212 * 24 * 3600
    Mes(9)  = 243 * 24 * 3600
    Mes(10) = 273 * 24 * 3600
    Mes(11) = 304 * 24 * 3600
    Mes(12) = 334 * 24 * 3600
    Mes(13) = 365 * 24 * 3600

    Anio(1)  = 365 * 24 * 3600                !Seconds from Year 2000 to 2001
    Anio(2)  = Anio(1)  + 365 * 24 * 3600    !Seconds from Year 2000 to 2002
    Anio(3)  = Anio(2)  + 365 * 24 * 3600    !Seconds from Year 2000 to 2003
    Anio(4)  = Anio(3)  + 366 * 24 * 3600    !Seconds from Year 2000 to 2004
    Anio(5)  = Anio(4)  + 365 * 24 * 3600    !...2005
    Anio(6)  = Anio(5)  + 365 * 24 * 3600    !...2006
    Anio(7)  = Anio(6)  + 365 * 24 * 3600    !...2007
    Anio(8)  = Anio(7)  + 366 * 24 * 3600    !...2008  Biciesto
    Anio(9)  = Anio(8)  + 365 * 24 * 3600    !...2009
    Anio(10) = Anio(9)  + 365 * 24 * 3600    !...2010
    Anio(11) = Anio(10) + 365 * 24 * 3600    !...2011
    Anio(12) = Anio(11) + 366 * 24 * 3600    !Seconds from Year 2000 to 2012    Biciesto
    Anio(13) = Anio(12) + 365 * 24 * 3600    !...2009
    Anio(14) = Anio(13) + 365 * 24 * 3600    !...2010
    Anio(15) = Anio(14) + 365 * 24 * 3600    !...2011
    Anio(16) = Anio(15) + 366 * 24 * 3600    !Seconds from Year 2000 to 2016...Biciesto
    Anio(17) = Anio(16) + 365 * 24 * 3600    !...2017
    Anio(18) = Anio(17) + 365 * 24 * 3600    !...2018
    Anio(19) = Anio(18) + 365 * 24 * 3600    !...2019
    Anio(20) = Anio(19) + 366 * 24 * 3600    !Seconds from Year 2000 to 2020...Biciesto
        
    
    CALL DATE_AND_TIME(Date,Times,Zone,Values)
    
             If (Values(1) <= 2001) Then
                Print *,'This Computer has The Year Wrong'
                Print *,'Year : ', Values(1)
                Stop
            End If

             If (Values(1) > 2020) Then
                Print *,'I am sorry but The Tempo Subroutine is Old.'
                Print *,'Please change it!'
                Stop
            End If

End Subroutine timer0




!**************************************************************************
!                Function TIMER (for ANSI FORTRAN)
!
!  This subroutine gives the execution time in seconds. It calls the
!  function DATE_AND_TIME (of ANSI FORTRAN) to find the current time.
!  The output value of the variable SEC is the time (in seconds) elapsed
!  since the last call to TIME0.
!
!**************************************************************************

function Timer()
implicit none
integer :: Values(8), Mes(13), Anio(16), Ysec, Dsec, Msec, iii, Timer
character(10) :: Date, Times, Zone
COMMON/SUBTIME/Anio, Mes
CALL DATE_AND_TIME(Date,Times,Zone,Values)
Ysec = Anio(Values(1)-2001)     
!Seconds from Year 2000 to the past Year
Msec = Mes(Values(2)) 
!Seconds from Juanuary to the past months
if (MOD(Values(1),4).EQ.0.AND.Values(2).GT.2) Msec=Msec+86400
! Chek when February has 29 days
Dsec = (Values(3)-1) * 86400
!Seconds as a consecuences of days of this month to yesterday
iii = Ysec+Msec+Dsec+Values(7)
Timer = DBLE(iii)+DBLE(Values(5))*3600.00+DBLE(Values(6))*60.00
end function 


!********************************************************************* 
!                Function Graph (FORTRAN) 
! 
!  This subroutine plots the faces of the crystal 
!
!********************************************************************* 
 
subroutine Graph(c,time,directorio,conce,filas,columnas,ancho,delta,distri_b) 
implicit none

integer                                  :: filas,columnas,ancho,delta
integer                                  :: i,j,k,s,color 
integer                                  :: cara1,cara2,tapa,qq,gris,time,Q           
real(pr)                                 :: factor1,factor2,grises 
integer,dimension(filas,columnas,ancho)  :: c 
character(100)                           :: archivo 
character(60)                            :: timew,conce,distri_b 
character(100)                           :: directorio 



if (distri_b=='uniforme') then

  
 cara1=int(real(filas-2)/real(delta)*real(int(delta/2.0)))
 cara2=int(real(columnas-2)/real(delta)*real(int(delta/2.0)))
 tapa=int(real(ancho-2)/real(delta)*real(int(delta/2.0)))
  
 else
 
 cara2=int(filas/2.0)
 cara1=int(columnas/2.0)
 tapa=int(ancho/2.0)+1
 
 

 end if
 
!colores RGB

!blanco (255,255,255)
!negro (0,0,0)
!azul (0,0,255)
!verde (0,255,0)
!rojo (255,0,0)
 
 
 grises=255.0 
 gris=int(grises) 
 color=255
 

Q=filas*columnas*ancho 
  
factor1=1.0/(Q)*255.0 
factor2=1.0/(Q)*255.0
  

 
Write( timew,'(i10)' ) time 
archivo=trim(adjustl(directorio)) //'grains' // '_' // trim(adjustl(timew))//' '// trim(adjustl(conce)) // ' '  // '.ppm'  
open(unit=100,file=archivo) 

!encabezado del archivo

write(100,1010)'P3' 
1010 format(A2) 
write(100,*) (ancho+columnas+15), (filas+ancho+15) 
write(100,*) gris 

do qq=1,5  
   do s=1,ancho+columnas+15 
   write(100,*) 1,0,0 
   end do 
end do 

!hago la cara de la tapa y la cara lateral 1

do i=1,filas 

    do qq=1,5 
       write(100,*) 1,0,0 
    end do 

    do j=1,columnas 
   
           if (c(i,j,tapa)==0) then 
       write(100,*) color,0,0 
       else 
         write(100,*) int(c(i,j,tapa)*factor1),int(factor1*c(i,j,tapa)),int(factor2*c(i,j,tapa)) 
       end if 
    end do 

    do qq=1,5 
          write(100,*) 1,0,0 
    end do 

    do k=1,ancho 
          if (c(i,cara1,k)==0) then 
          write(100,*) color,0,0 
      else 
          write(100,*) int(c(i,cara1,k)*factor1),int(c(i,cara1,k)*factor1),int(c(i,cara1,k)*factor2) 
      end if 
    end do 

    do qq=1,5 
       write(100,*) 1,0,0 
    end do 
end do  

!ahora hago la cara lateral 2

do qq=1,5 
    do s=1,ancho+columnas+15 
       write(100,*) 1,0,0 
    end do 
end do 

do k=1,ancho 

    do qq=1,5 
       write(100,*) 1,0,0 
    end do 
    
    do j=1,columnas 
      
      if (c(cara2,j,k)==0) then  
         write(100,*) color,0,0 
      else 
         write(100,*) int(c(cara2,j,k)*factor1),int(c(cara2,j,k)*factor1),int(c(cara2,j,k)*factor2)  
      end if 
    end do 

    do qq=1,5 
        write(100,*) 1,0,0 
    end do 

    do i=1,ancho 
        write(100,*) 1,0,0 
    end do 
    do qq=1,5 
        write(100,*) 1,0,0 
    end do 

    end do 



do qq=1,5 
    do s=1,ancho+columnas+15 
        write(100,*) 1,0,0 
    end do 
end do 
close(100) 
end subroutine Graph 

 
 
!********************************************************************* 
!                Function Mask (FORTRAN) 
! 
!  This subroutine plots the faces of the crystal with mask 
!
!********************************************************************* 
 
 
 
subroutine mask(c,time,directorio,conce,filas,columnas,ancho, delta,distri_b) 
implicit none
integer                                  :: filas,columnas,ancho,delta
integer                                  :: i,j,s,k,color 
integer                                  :: cara1,cara2,tapa,qq,gris,time,salto1,salto2            
real(pr)                                 :: grises 
integer,dimension(filas,columnas,ancho)  :: c 
character(100)                           :: archivo 
character(60)                            :: timew,conce,distri_b 
character(100)                           :: directorio 




grises =  255.0 
gris =  int(grises) 
color=255
!+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++ 
!+ 
!+     generaci?n del ARCHIVO del policristal inicial    MASK 
!+ 
!+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++ 

 if (distri_b=='uniforme') then

 cara1=int(real(filas-2)/real(delta)*real(int(delta/2.0)))
 cara2=int(real(columnas-2)/real(delta)*real(int(delta/2.0)))
 tapa=int(real(ancho-2)/real(delta)*real(int(delta/2.0)))
  
 else
 
 cara1=int(filas/2.0)
 cara2=int(columnas/2.0)
 tapa=int(ancho/2.0)+1
 
 end if
 
!colores RGB

!blanco (255,255,255)
!negro (0,0,0)
!azul (0,0,255)
!verde (0,255,0)
!rojo (255,0,0)


 
  
 
Write( timew,'(i10)' )  time 
archivo=trim(adjustl(directorio)) //'mask' // ' ' // trim(adjustl(timew)) //' '// trim(adjustl(conce)) // ' ' // '.ppm'  
open(unit=81,file=archivo) 


write(81,1004)'P3' 
1004 format(A2) 
write(81,*) (columnas+ancho+15), (filas+ancho+15) 
write(81,*) gris 

do qq=1,5 
     do s=1,columnas+ancho+15 
        write(81,*) 255,255,255 
     end do 
end do 

do i=1,filas-1 
    
    
    do qq=1,5 
       write(81,*) 255,255,255 
    end do 

    salto1 = c(i,1,tapa) 
    do j=1,columnas 
     if ((salto1/=c(i,j,tapa)) .or. (salto1/=c(i+1,j,tapa)))  then 
            if (c(i,j,tapa)/=0)    then  
           write(81,*) 0,0,0  
           salto1 = c(i,j,tapa) 
            else 
        write(81,*) 255,0,0
        end if 
     else 
        if (salto1/=0) then  
           write(81,*) 255,255,255 
        else 
          write(81,*) 255,0,0 
            end if 
    end if 
    end do
  
  do qq=1,5 
    write(81,*) 255,255,255 
    end do 
 
    salto2 = c(i,cara2,1) 
    do k=1,ancho 
    if ((salto2/=c(i,cara2,k)) .or. (salto2/=c(i+1,cara2,k))) then 
         if (c(i,cara2,k)/=0) then 
              write(81,*) 0,0,0    !COLOR DEL CIRCULO SUPERIOR DERECHO
                  salto2=c(i,cara2,k) 
             else 
                  write(81,*) 255,0,0 
         end if 
     else 
         write(81,*) 255,255,255 
    end if 
    end do 
   
   
   do qq=1,5 
    write(81,*) 255,255,255 
    end do
end do 

!hago tapa lateral 2

do qq=1,5 
    do s=1,ancho+columnas+15 
       write(81,*) 255,255,255 
    end do 
end do 

do k=1,ancho-1

    do qq=1,5 
       write(81,*) 255,255,255  
    end do 
    
    salto1=c(cara1,1,k) 
    do j=1,columnas 
           if ((salto1/=c(cara1,j,k)) .or. (salto1/=c(cara1,j,k+1))) then      
             if (c(cara1,j,k)/=0) then 
                 write(81,*) 0,0,0  !COLOR DEL CIRCULO INFERIOR IZQUIERDO
                 salto1=c(cara1,j,k) 
             else 
                 write(81,*) 255,0,0 
                 end if 
          else 
             write(81,*) 255,255,255 
         end if 
    end do
    
    do qq=1,5 
        write(81,*) 255,255,255 
    end do 

    do qq=1,ancho+5
        write(81,*) 255,255,255 
    end do 
end do

do qq=1,7
    do s=1,ancho+columnas+15 
       write(81,*) 255,255,255 
    end do 
end do 

close(81) 
end subroutine mask 
 
 
 
!********************************************************************** 
!                Function tresD (FORTRAN) 
!  This subroutine plots the 3D crystal  
! 
!********************************************************************** 
 
subroutine tresD(c,time,directorio,conce,filas,columnas,ancho) 
implicit none
integer                                  :: filas,columnas,ancho 
integer                                  :: i,j,ii,jj,kk,s,l,x_largo,y_largo,mm,qq 
integer                                  :: tapa,gris,time,punto 
real(pr)                                 :: grises,alpha,pi,aa,bb,tt,angulo,factor1,factor2
integer,dimension(filas,columnas,ancho)  :: c 
character(100)                           :: archivo,archivo2 
character(60)                            :: timew,conce 
character(100)                           :: directorio 

grises=255.0 
pi=3.14159 
gris=int(grises) 
tapa=1 



!factor1=1.0/(ancho*columnas*filas)*255.0 
!factor2=1.0/(ancho*columnas*filas)*0.0 

factor1=1.0/(90)*255.0 
factor2=1.0/(90)*0

!+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++ 
!     generacion del ARCHIVO del policristal inicial    3D 
!+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++ 


!blanco (255,255,255)
!negro (0,0,0)
!azul (0,0,255)
!verde (0,255,0)
!rojo (255,0,0)




Write( timew,'(i10)' )  time 
archivo=trim(adjustl(directorio)) //'3D' // ' ' // ' '// trim(adjustl(conce)) // ' ' // '.ppm'  
open(unit=82,file=archivo) 

write(82,1006)'P3' 
1006 format(A2) 


alpha=pi*20.0/180.0 
x_largo=int((columnas+filas)*cos(alpha)) 
y_largo=int((filas+columnas)*sin(alpha))+ancho 

write(82,*) x_largo,y_largo 
write(82,*) gris 

do j=1,y_largo   
    do i=1,x_largo 
         if ((i<cos(alpha)*filas) .and.  (j<=int(-tan(alpha)*i+filas*sin(alpha)))) then 
                   write(82,*) 0,0,0 
         else 
            if ((i<cos(alpha)*filas) .and.  (j>=int(tan(alpha)*i+filas*sin(alpha)+ancho))) then  
                   write(82,*) 0,0,0 
            else 
                  if ((i>=cos(alpha)*filas) .and.  (j<=int(tan(alpha)*i-filas*sin(alpha)))) then 
                       write(82,*) 0,0,0 
                  else 
                      if ((i>cos(alpha)*filas) .and.  (j>int(-tan(alpha)*i+2*columnas*sin(alpha)+filas*sin(alpha)+ancho))) then        
                         write(82,*) 0,0,0 
                      else 
                         write(82,*) 150,150,150 
                      end if                
                  end if 
            end if 
         end if 
     end do 
end do 
close(82) 

open(unit=83,file=archivo) 
archivo2=trim(adjustl(directorio)) //'3D_1' // ' ' // trim(adjustl(timew)) //' '// trim(adjustl(conce)) // ' ' // '.ppm'  
open(unit=84,file=archivo2) 
read(83,*)  
read(83,*) x_largo,y_largo 
read(83,*)  
write(84,1007)'P3' 
1007 format(A2) 
write(84,*) x_largo,y_largo 
write(84,*) gris 
do j=1,y_largo 
    do i=1,x_largo 
       read(83,*) ii,jj,kk     
       if (ii==150) then 
           if ((i<=cos(alpha)*filas) .and.  (j<=int(tan(alpha)*i+filas*sin(alpha)))) then 
           write(84,*) 60,0,0 
       else 
          if ((i>cos(alpha)*filas) .and.  (j<int(-tan(alpha)*i+2*columnas*sin(alpha)+filas*sin(alpha)))) then 
                 write(84,*) 60,0,0     !TAPA 
              else 
                 write(84,*) ii,jj,kk 
              end if 
           end if 
       else 
           write(84,*) ii,jj,kk 
       end if 
    end do
end do 
close(83) 
close(84) 
open(unit=85,file=archivo2) 
open(unit=86,file=archivo) 
read(85,*)  
read(85,*) x_largo,y_largo 
read(85,*)  
write(86,1008)'P3' 
1008 format(A2) 
write(86,*) x_largo,y_largo 
write(86,*) gris 
do j=1,y_largo 
     do i=1,x_largo 
         read(85,*) ii,jj,kk     
         if (ii==150) then 
           if (i<=cos(alpha)*filas)  then 
                   write(86,*) 30,0,30      !CARA IZQUIERDA 
               else 
               write(86,*) 90,0,90       !CARA DERECHA 
               end if    
         else 
              write(86,*) ii,jj,kk 
         end if 
     end do
end do 
close(85) 
close(86) 
open(unit=87,file=archivo) 
open(unit=88,file=archivo2) 
read(87,*)  
read(87,*) x_largo,y_largo 
read(87,*)  
write(88,1009)'P3' 
1009 format(A2) 
write(88,*) x_largo,y_largo 
write(88,*) gris 
do j=1,y_largo 
     do i=1,x_largo 
         read(87,*) ii,jj,kk     
              if (ii==60) then 
              aa=i
              bb=j-filas*sin(alpha)
              tt=(aa**2+bb**2)**(0.5)
              angulo=acos((aa*cos(alpha)+bb*sin(alpha))/tt)
              qq=int(tt*sin(angulo)/sin(2*alpha)) !teorema del seno
              mm=int(tt*sin(2*alpha-angulo)/sin(2*alpha)) 
                 if (mm==0) mm=1 
                     if (qq==0) qq=1 
                         tapa=1 
                         punto=0
                         !TAPA 
                            do s=1,filas 
                                do l=1,columnas 
                                    if ((mm==l) .and. (qq==s)) then  
                                            write (88,*) int(factor1*c(s,l,tapa)),int(factor1*c(s,l,tapa)),int(factor1*c(s,l,tapa)) 
                                            !write (88,*) int(factor2),int(factor2),int(factor1*c(s,l,tapa)) 
                                            punto=1 
                                            
                                        else 
                                        end if 
                                    end do 
                                end do 
                                    if (punto==0) write (88,*) ii,jj,kk 
                                        else 
                                           if (ii==30) then 
                                              aa=i 
                                              bb=j-filas*sin(alpha) 
                                              tt=(aa**2+bb**2)**(0.5) 
                                              angulo=acos((bb)/tt) 
                                              qq=int(tt*sin(angulo)/sin(pi/2.0+alpha))    !largo 
                                              mm=int(tt*sin(pi/2.0-alpha-angulo)/sin(pi/2.0+alpha))       !alto 
                                                  if (mm==0) mm=1 
                                                      if (qq==0) qq=1 
                                                          punto=0 
                                                              do l=1,columnas 
                                                                  do s=1,ancho 
                                                                     !CARA IZQUIERDA 
                                                                      if ((mm==s) .and. (qq==l)) then 
                                                                          write (88,*) int(factor1*c(1,l,s)),int(factor1*c(1,l,s)),&
                                                                          &int(factor1*c(1,l,s)) 
                                                                          !write (88,*) int(factor2),int(factor2),&
                                                                          !&int(factor1*c(1,l,s))
                                                                          punto=1 
                                                                      else 
                                                                      end if 
                                                                  end do 
                                                              end do 
                                                                  if (punto==0) write (88,*) ii,jj,kk 
                                                                  else 
                                                                    if (ii==90) then 
                                                                       aa=i-filas*cos(alpha) 
                                                                       bb=j-2*filas*sin(alpha) 
                                                                       tt=(aa**2+bb**2)**(0.5) 
                                                                       angulo=acos((bb)/tt) 
                                                                       qq=int(tt*sin(angulo)/sin(pi/2.0-alpha))       !largo
                                                                       mm=int(tt*sin(pi/2.0+alpha-angulo)/sin(pi/2.0-alpha)) 
                                                                       if (mm==0) mm=1 
                                                                           if (qq==0) qq=1 
                                                                               punto=0 
                                                                               !CARA DERECHA 
                                                                               do s=1,filas 
                                                                                   do l=1,ancho 
                                                                                       if ((mm==l) .and. (qq==s)) then  
                                                                                           write (88,*) &
                                                                                           &int(factor1*c(s,columnas,l))&
                                                                                           &,int(factor1*c(s,columnas,l)),&
                                                                                           &int(factor1*c(s,columnas,l)) 
                                                                                           !write (88,*) &
                                                                                           !&int(factor2)&
                                                                                           !&,int(factor2),&
                                                                                           !&int(factor1*c(s,columnas,l))
                                                                                           punto=1 
                                                                                       else 
                                                                                       end if 
                                                                                   end do 
                                                                               end do 
                                                                               if (punto==0) write (88,*) ii,jj,kk 
                                                                               else 
                                                                               write (88,*) ii,jj,kk 
                                                                               end if 
                                                                          end if 
                                                                      end if 
     end do 
end do 
close(87) 
close(88) 
end subroutine 


!*********************************************************************
!                Function distribution (FORTRAN)
!
!  This subroutine plots the grain size distribution  
!
!*********************************************************************

subroutine distri(orient1,time,directorio,conce,filas,columnas,ancho)
integer                                   :: filas,columnas,ancho,s,Q,time
integer,dimension(filas*columnas*ancho)   :: orient1
character(100)                            :: archivo
character(60)                             :: timew,conce
character(100)                            :: directorio
!+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
!Generaci?n de los distribuci?n de tamano de granos y orientaci?n inicial
!!+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
Write( timew,'(i10)' )  time
archivo= trim(adjustl(directorio)) // 'distri' // ' ' // trim(adjustl(timew)) // ' '// trim(adjustl(conce)) // ' ' // '.dat' 
open(unit=70,file=archivo)
Q=filas*columnas*ancho
do s=1,Q
    if (orient1(s)>0) write(70,*) s, orient1(s)
end do
close(70)
end subroutine




!*********************************************************************
!                Function ENERGY (for ANSI FORTRAN)
!
!  This subroutine gives the grain boundary energy. 
!
!*********************************************************************

real(pr) function Lif(difa3,indice,ang_l1)
implicit none
real(pr), parameter   :: pi=3.14159
real(pr)              :: difa3,ang_l1, baja
integer               :: indice 


Select case (indice) 
 
  case(1)
    Lif=1.0
    baja=0
       if (difa3==0) Lif=0
            if ((difa3<=ang_l1) .and. (difa3/=0)) Lif=(1-baja)*difa3/ang_l1*(1-log(difa3/ang_l1))+baja
  case(2)
    Lif=sin(2*difa3*pi/180)
  
  case (3)
    if (difa3==0) Lif=0.0
        if (difa3/=0) Lif=1.0
  
  case (4)
    Lif=1.0
    if (difa3<=ang_l1)  Lif=difa3/ang_l1
  
  case (5)
    if (ang_l1==90.0) stop
    Lif=1.0
    if (difa3<=ang_l1)  Lif=difa3/ang_l1
      if (difa3>=90.0-ang_l1) Lif=(90.0-difa3)/ang_l1
end select
end function


!*********************************************************************
!                Function distribution desorintacion(FORTRAN)
!
!  This subroutine calculate the grain misorientation distribution  
!
!*********************************************************************

subroutine misori(c,time,directorio,conce,filas,columnas,ancho)
integer                                    :: filas,columnas,ancho,s1,s2,s3,s4,time,Q,difa
!integer,dimension(filas*columnas*ancho)   :: orient1
integer,dimension (26)                     :: vveci
character(100)                             :: archivo
character(60)                              :: timew,conce
character(100)                             :: directorio
integer,dimension(filas,columnas,ancho)    :: c
!+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
!
!Generaci?n de los distribuci?n de tamano de granos y orientaci?n inicial
!
!+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
Write( timew,'(i10)' )  time
archivo= trim(adjustl(directorio)) // 'misori' // ' ' // trim(adjustl(timew)) // ' '// trim(adjustl(conce)) // ' ' // '.dat' 
open(unit=99,file=archivo)
Q=filas*columnas*ancho
do s1=1,filas
do s2=1,columnas
do s3=1,ancho
!+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
!
!     calculo de vecinos!!!!
!
!+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
 jfl = s1-1 ;  if (jfl < 1)        jfl = filas
 jcl = s2-1 ;  if (jcl < 1)        jcl = columnas
 jkl = s3-1 ;  if (jkl < 1)        jkl = ancho
 jfr = s1+1 ;  if (jfr > filas)    jfr = 1
 jcr = s2+1 ;  if (jcr > columnas) jcr = 1
 jkr = s3+1 ;  if (jkr > ancho)    jkr = 1
 vveci(1) = c(jfl, jcl,jkl)
 vveci(2) = c(jfl, s2,jkl)
 vveci(3) = c(jfl, jcr,jkl)
 vveci(4) = c(s1, jcl,jkl)
 vveci(5) = c(s1, jcr,jkl)
 vveci(6) = c(jfr, jcl,jkl)
 vveci(7) = c(jfr, s2,jkl)
 vveci(8) = c(jfr, jcr,jkl) 
 vveci(9) = c(jfl, jcl,s3)
 vveci(10) = c(jfl, s2,s3)
 vveci(11) = c(jfl, jcr,s3)
 vveci(12) = c(s1, jcl,s3)
 vveci(13) = c(s1, jcr,s3)
 vveci(14) = c(jfr, jcl,s3)
 vveci(15) = c(jfr, s2,s3)
 vveci(16) = c(jfr, jcr,s3)
 vveci(17) = c(jfl, jcl,jkr)
 vveci(18) = c(jfl, s2,jkr)
 vveci(19) = c(jfl, jcr,jkr)
 vveci(20) = c(s1, jcl,jkr)
 vveci(21) = c(s1, jcr,jkr)
 vveci(22) = c(jfr, jcl,jkr)
 vveci(23) = c(jfr, s2,jkr)
 vveci(24) = c(jfr, jcr,jkr)
 vveci(25) = c(s1, s2,jkl)
 vveci(26) = c(s1, s2,jkr)
do s4=1,26
    difa=abs((vveci(s4)-c(s1,s2,s3))*90.0/Q)
    if (difa/=0) then 
       write(99,*) difa
    end if
end do
end do
end do
end do
close(99)
end subroutine


!********************************************************************* 
!                Function archivo_xyz (FORTRAN) 
! 
!  escribe la matriz x(i,j,k) como un archivo .xyz
!
!********************************************************************* 

 subroutine archivo_xyz(c, filas, columnas, ancho, time, directorio)
    implicit none
    integer, intent(in) :: filas, columnas, ancho, time
    integer, intent(in) :: c(filas, columnas, ancho)
    character(len=*), intent(in) :: directorio
    integer :: i, j, k, total, unit
    character(len=20) :: etiqueta
    character(100) :: archivo
    character(20) :: time_str

    ! Convertir time a string sin espacios
    write(time_str,'(I0)') time

    ! Construir nombre del archivo similar a Graph
    archivo = trim(adjustl(directorio)) // 'output_' // trim(time_str) // '.xyz'

    ! Contar elementos válidos (>=0)
    total = 0
    do i = 1, filas
        do j = 1, columnas
            do k = 1, ancho
                if (c(i,j,k) >= 0) then
                    total = total + 1
                end if
            end do
        end do
    end do

    ! Abrir archivo nuevo con nombre dinámico
    open(newunit=unit, file=archivo, status='replace')

    ! Escribir encabezado XYZ
    write(unit,*) total
    write(unit,*) 'XYZ output del sistema c(i,j,k) time=' // trim(time_str)

    ! Escribir datos: etiqueta y posición (i,j,k)
    do i = 1, filas
        do j = 1, columnas
            do k = 1, ancho
                if (c(i,j,k) == 0) then
                    etiqueta = 'P' !particle
                else
                    write(etiqueta,'(A,I0)') 'G', c(i,j,k) !grain con el valor en c(i,j,k)
                end if
                write(unit,'(A,1X,I0,1X,I0,1X,I0)') trim(etiqueta), i, j, k
            end do
        end do
    end do

    close(unit)

end subroutine 


!********************************************************************* 
!                Function archivo_lammpstrj (FORTRAN) 
! 
!  escribe la matriz x(i,j,k) como un archivo LAMMPS
!
!********************************************************************* 

 

subroutine archivo_lammpstrj(c, filas, columnas, ancho, time, directorio)
    implicit none
    integer, intent(in) :: filas, columnas, ancho, time
    integer, intent(in) :: c(filas, columnas, ancho)
    character(len=*), intent(in) :: directorio
    integer :: i, j, k, total, unit, id, tipo
    character(100) :: archivo
    character(20) :: time_str

    ! Convertir time a string sin ceros a la izquierda
    write(time_str,'(I0)') time

    ! Construir nombre del archivo
    archivo = trim(adjustl(directorio)) // 'output_' // trim(time_str) // '.lammpstrj'

    ! Contar elementos válidos
    total = 0
    do i = 1, filas
        do j = 1, columnas
            do k = 1, ancho
                if (c(i,j,k) >= 0) then
                    total = total + 1
                end if
            end do
        end do
    end do

    ! Abrir archivo
    open(newunit=unit, file=archivo, status='replace')

    ! Escribir encabezado LAMMPS
    write(unit,*) 'ITEM: TIMESTEP'
    write(unit,*) time
    write(unit,*) 'ITEM: NUMBER OF ATOMS'
    write(unit,*) total
    write(unit,*) 'ITEM: BOX BOUNDS pp pp pp'
    write(unit,*) 0, filas
    write(unit,*) 0, columnas
    write(unit,*) 0, ancho
    write(unit,*) 'ITEM: ATOMS id type x y z'

    ! Escribir partículas (id, tipo, coordenadas)
    id = 0
do i = 1, filas
    do j = 1, columnas
        do k = 1, ancho
            if (c(i,j,k) >= 0) then
                id = id + 1
                if (c(i,j,k) == 0) then
                    tipo = 1
                else
                    tipo = c(i,j,k) + 1
                end if
                write(unit,'(I8,1X,I4,1X,F8.3,1X,F8.3,1X,F8.3)') id, tipo, real(i)-0.5, real(j)-0.5, real(k)-0.5
            end if
        end do
    end do
end do


    close(unit)
end subroutine


  

end module hielonn
