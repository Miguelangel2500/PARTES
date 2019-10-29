
*READ events 
CD C:\vispartes    
SET DATE ITALIAN
set deleted on
*SET SYSMENU OFF
SET TALK OFF
****************************************
***************************************
************FICHERO DETALLES SOLO CORRECTO A PARTIR DE MES
************** 7 DE 1999
*********************************************
************************************************
*el programa graba la media de gasoil en el fichero gasoil.dbf en el procedimiento altas, posteriormete se puede 
*recalcular y grabar en el procedimiento impresion sea imprimiendo o en salida por pantalla

_SCREEN.FontSize=16
_SCREEN.FontName="COURIER NEW"
_SCREEN.WINDOWSTATE= 2
_SCREEN.Caption="Programa Partes"
*_screen.Visible=.f.

*_screen.Icon="C:\Archivos de programa\Microsoft Visual FoxPro 9\Samples\Tastrade\Bitmaps\catgry.icon"

SET PRINT FONT "COURIER new",9
set century on
SET DECIMALS TO 2
*SET STEP OFF
*SET ECHO OFF
SET POINT TO ","
SET SEPARATOR TO "."


WMAYUS=CAPSLOCK()

CAPSLOCK(.T.)
PCONDUCTOR="          "
PNUMERO=0
*set procedure to PARTES
*set talk oFF
respuesta="a"
*READ events
DO WHILE .T.
        *SET DECIMALS TO 2
        *SET FIXED OFF
        a=.f.
       IF a
        CLEAR
       
       
        @ 1,1 TO 30,90 DOUBLE
        @ 3,10 SAY "PARTES   DE   CONDUCTORES"
        @ 2,9 TO 5,60
        @ 7,9 to 28,60
        @ 8,10 SAY "(Q)uit---------------------"
        @ 10,10 SAY "(S)alir--------------------"
        @ 12,10 say "(A)ltas--------------------"
        @ 14,10 say "(I)mpresion----------------"
        @ 16,10 say "(B)ajas--------------------"
        @ 18,10 SAY "(H)ojas mensuales----------"
        @ 20,10 SAY "(R)UEDA--------------------" 
        @ 22,10 SAY "Sa(l)ida en archivo--------"
        @ 24,10 SAY "Resu(m)en de partes--------"
        @ 26,10 SAY "Reinde(x)ar----------------" GET RESPUESTA
        read
       endif 
        *respuesta="a"
        *READ events
        DO FORM vispartes_menuinicio
        *CLEAR events
        respuesta=lower(respuesta)
        CLOSE INDEXES
        CLOSE DATABASES
        
        do case
	         case respuesta="a"
	               do altas
	         case respuesta="i"
	               do impresion
	         case respuesta="r"
	               do rueda
	         case respuesta="b"
	               do borrar
	         case respuesta="m"
	              do resupar

	              ********linea  776
	         case respuesta="l"
	               do salarch

	         case respuesta="x"
		         CLEAR
		         *SET TALK ON
		          CLOSE INDEXES
		          CLOSE DATABASES 
		          SET EXCLUSIVE ON
		               use partes index innomnum,incamnum
		               REINDEX
		               use detalles index innomdet,incamdet
		               REINDEX
		               use jornada index innomjor
		               REINDEX
		               
		          SET EXCLUSIVE OFF
		          CLOSE INDEXES 
		          CLOSE DATABASES 
		          *     SET TALK OFF
		          *     use
	               
	         case respuesta="s"
	               CAPSLOCK(WMAYUS)
	               
	               *CLOSE INDEXES
	               *CLOSE DATABASES
	               
	               CLOSE ALTERNATE
	               CLOSE INDEX
	               CLOSE DATABASES
	               _SCREEN.Caption="Microsoft Visual FoxPro"
	               CLEAR
	               *quit
	               *CLEAR events
	               cancel
	         CASE RESPUESTA="q"
	               CAPSLOCK(WMAYUS)
	               *CLEAR EVENTS
	               
	               QUIT
	         case respuesta="h"
	               do hoja
        
              OTHERWISE
              
        endcase
enddo


**********ALTAS************
***************************

procedure altas
CLOSE INDEXES
CLOSE DATABASES

use partes  index innomnum,INCAMNUM 









***inicializacion de var
************************
wfechaini=ctod("01-01-2006")
wfechafin=ctod("01-01-2006")
store space(10) to wcamion,wconductor
store space(50) to wnotas
wdiscos=.F.
WDINENT=0
WGASJUS=0
WDIFERENCIA=0

wnumero=0
WRESP=" "

a=.f.
IF a
clear
@ 1,1 TO 29,90 DOUBLE
@ 3,10 SAY "DATOS GENERALES DEL PARTE"
@ 2,9 TO 4,50
@ 8,9 to 19,80

@ 9,10  say "numero de parte " get wnumero
@ 11,10 say "fecha de inicio " get wfechaini
@ 13,10 say "fecha fin       " get wfechafin
@ 15,10 say "camion          " get wcamion
@ 17,10 say "conductor       " get wconductor


@ 20,4 to 27,80

@ 21,5  say "notas           " get wnotas
@ 23,5  say "discos          " get wdiscos
@ 25,5  SAY "volver s/n      " get wresp

READ
ENDIF
DO FORM vispartes_datosgenerales
pconductor=wconductor
WCONDUCTOR=TRIM(WCONDUCTOR)
wresp=upper(wresp)
if wresp="S"
     return
endif


SEEK wconductor


PEPE=FOUND()
if PEPE


  do while conductor=wconductor.and.(.not.eof())
       wnumero=numero
       SKIP
  enddo
 ELSE
  WNUMERO=0
endif



WNUMERO=WNUMERO+1
pnumero=wnumero
CLOSE INDEXES 
CLOSE DATABASES 

use partes index incamnum,INNOMNUM
*REINDEX
SEEK wcamion
IF FOUND()


   do while camion=wcamion.and.(.not.eof())
       wnumcam=numcam
       skip
   enddo
   wnumcam=wnumcam+1
   ELSE
   WNUMCAM=1
ENDIF

CLOSE INDEXES
CLOSE DATABASES

use partes index innomnum,INCAMNUM


append blank
replace fechaini with wfechaini
replace fechafin with wfechafin
replace camion with wcamion
replace conductor with wconductor
replace notas with wnotas
replace numero with wnumero
REPLACE NUMCAM WITH WNUMCAM
replace discos with wdiscos
REPLACE DINENT WITH WDINENT
REPLACE GASJUS WITH WGASJUS
REPLACE DIFERENCIA WITH WDIFERENCIA


**********introduccion de datos
terminado="n"
wjornada=wfechaini
do while terminado<>"s"
      anterior=wjornada
      
      con=" "
      a=.f.
      IF a
      clear
      @ 1,5 SAY "Conductor: "+wconductor+"     Nº de camion: "+STR(wnumcam,4)+"     Nº de parte: "+STR(wnumero,4)
      @ 2,5 say "jornada " +dtoc(wjornada)+ "  "+Cdow(wjornada)+" (v) visual foxpro 9.0"
      @ 3,5 SAY "            MENU   DE     GASTOS"

      @ 9,1 TO 32,90 DOUBLE
      
      @ 10,5 say "jornada -------------- A."
      @ 12,5 say "pistas italianas ----- B."
      @ 14,5 say "pistas francesas (DKV) C."
      @ 16,5 say "otras pistas --------- D."
      @ 18,5 say "pistas españolas ----- E."
      @ 20,5 say "gasoil DKV español---- F."
      @ 22,5 say "gasoil DKV frances --- G."
      @ 24,5 say "AS24 español --------- H."
      @ 26,5 say "AS24 extranjero ------ I."
      
      @ 10,50 say "CIDY ----------------  J."
      @ 12,50 say "NAVE ----------------  K."
      @ 14,50 say "otros español--------  L."
      @ 16,50 say "otros extranjero-----  M."
      @ 18,50 say "dinero entregado-----  N."
      @ 20,50 SAY "gasoil dkv italiano--  O."
      @ 22,50 SAY "PISTAS PORTUGUESAS---  P."
      @ 24,50 SAY "GASOIL PORTUGUES-----  R."
      @ 26,50 say "siguiente jornada ---  Q." 
      @ 28,50 SAY "SALIR ---------------  S." get con

      READ
      ENDIF
      DO FORM vispartes_menugastos
      CON=UPPER(CON)
      do case
         case con="A"

                   *********jornada
                   cuestion="JORNADA"
                   CLOSE INDEXES
                   CLOSE DATABASES
                   
                   use jornada index innomjor
                   store space(15) to worigen,wdestino,wmercancia
                   store space(15) to wcliente
                   wkilos=0
                   clear
                   @ 1,5 SAY "          " +CUESTION


                   @ 5,5 say  "          " +dtoc(wjornada)
                   @ 7,5 say  "origen    " get worigen
                   @ 9,5 say  "destino   " get wdestino
                   @ 11,5 say  "mercancia " get wmercancia
                   @ 13,5 say  "cliente   " get wcliente
                   @ 15,5 say "kilos     " get wkilos
                   read
                   append blank
                   replace camion with wcamion
                   replace conductor with wconductor
                   replace fecha with wjornada
                   replace origen with worigen
                   replace destino with wdestino
                   replace mercancia with wmercancia
                   replace cliente with wcliente
                   replace kilos with wkilos
                   replace numero with wnumero
                   replace numcam with wnumcam
       case con  ="B"



                *******pistas italianas
                ****************************
                CUESTION="PISTAS ITALIANAS"
                use detalles  index innomdet,incamdet
                seg="s"
                do while seg<>"n"
	                 wimporte=0
	                 clear

	                 @ 1,5 SAY  "          " +CUESTION
	                 @ 5,5 say  " " +dtoc(wjornada)
	                 @ 7,5 say  "importe " get wimporte  PICTURE "99,999.99"
	                 read
	                 @ 11,5 say "continuar  s/n" get seg
	                 read

	                 seg=lower(seg)
	                 append blank
	                 replace camion with wcamion
	                 replace conductor with wconductor
	                 replace fecha with wjornada
	                 replace pais with "ITALIA"
	                 replace detalle with "AUTOPISTA"
	                 replace moneda with "EUR"
	                 replace importe with wimporte

	                 replace numero with wnumero

	                 replace numcam with wnumcam
                enddo

       CASE con=="DIETAS"
       
       
                    cuestion="DIETAS"
                    use detalles  index innomdet,incamdet
                seg="s"
                do while seg<>"n"
	                 wimporte=0
	                 clear

	                 @ 1,5 SAY  "          " +CUESTION
	                 @ 5,5 say  " " +dtoc(wjornada)
	                 @ 7,5 say  "importe " get wimporte  PICTURE "99,999.99"
	                 read
	                 @ 11,5 say "continuar  s/n" get seg
	                 read

	                 seg=lower(seg)
	                 append blank
	                 replace camion with wcamion
	                 replace conductor with wconductor
	                 replace fecha with wjornada
	                 replace pais with "ESPAÑA"
	                 replace detalle with "DIETA"
	                 replace moneda with "EUR"
	                 replace importe with wimporte

	                 replace numero with wnumero

	                 replace numcam with wnumcam
                enddo
       
               
       case con=="P"



                *******pistas PORTUGUESAS
                ****************************
                CUESTION="PISTAS PORTUGUESAS"
                use detalles  index innomdet,incamdet
                seg="s"
                do while seg<>"n"
	                 wimporte=0
	                 clear

	                 @ 1,5 SAY  "          " +CUESTION
	                 @ 5,5 say  " " +dtoc(wjornada)
	                 @ 7,5 say  "importe " get wimporte  PICTURE "99,999.99"
	                 read
	                 @ 11,5 say "continuar  s/n" get seg
	                 read

	                 seg=lower(seg)
	                 append blank
	                 replace camion with wcamion
	                 replace conductor with wconductor
	                 replace fecha with wjornada
	                 replace pais with "PORTUGAL"
	                 replace detalle with "AUTOPISTA"
	                 replace moneda with "EUR"
	                 replace importe with wimporte

	                 replace numero with wnumero

	                 replace numcam with wnumcam
                enddo
      case con="C"

                ******pistas francesas dkv
                ***************************
                CUESTION="PISTAS FRANCESAS DKV"
                use detalles index innomdet,incamdet
                seg="s"
                do while seg<>"n"
	                 wimporte=0
	                 clear

	                 @ 1,5 SAY "          " +CUESTION
	                 @ 5,5 say " " +dtoc(wjornada)
	                 @ 7,5 say "importe " get wimporte PICTURE "99,999.99"
	                 read
	                 @ 11,5 say "continuar  s/n" get seg
	                 read

	                 seg=lower(seg)
	                 append blank
	                 replace camion with wcamion
	                 replace conductor with wconductor
	                 replace fecha with wjornada
	                 replace pais with "FRANCIA"
	                 replace detalle with "AUTOPISTA"
	                 replace moneda with "EUR"
	                 replace proveedor with "DKV"
	                 replace importe with wimporte

	                 replace numero with wnumero

	                 replace numcam with wnumcam
                enddo


     case con=="PISTASSERVISAESPAÑOLAS"

                ******pistas servisa españolas
                ***************************
                CUESTION="PISTAS SERVISA ESPAÑOLAS"
                use detalles index innomdet,incamdet
                seg="s"
                do while seg<>"n"
	                 wimporte=0
	                 clear

	                 @ 1,5 SAY "          " +CUESTION
	                 @ 5,5 say " " +dtoc(wjornada)
	                 @ 7,5 say "importe " get wimporte PICTURE "99,999.99"
	                 read
	                 @ 11,5 say "continuar  s/n" get seg
	                 read

	                 seg=lower(seg)
	                 append blank
	                 replace camion with wcamion
	                 replace conductor with wconductor
	                 replace fecha with wjornada
	                 replace pais with "ESPAÑA"
	                 replace detalle with "AUTOPISTA"
	                 replace moneda with "EUR"
	                 replace proveedor with "SERVISA"
	                 replace importe with wimporte

	                 replace numero with wnumero

	                 replace numcam with wnumcam
                enddo

     case con=="PISTASCONTADO"

                ******pistas servisa españolas
                ***************************
                CUESTION="PISTAS CONTADO"
                use detalles index innomdet,incamdet
                seg="s"
                do while seg<>"n"
	                 wimporte=0
	                 clear

	                 @ 1,5 SAY "          " +CUESTION
	                 @ 5,5 say " " +dtoc(wjornada)
	                 @ 7,5 say "importe " get wimporte PICTURE "99,999.99"
	                 read
	                 @ 11,5 say "continuar  s/n" get seg
	                 read

	                 seg=lower(seg)
	                 append blank
	                 replace camion with wcamion
	                 replace conductor with wconductor
	                 replace fecha with wjornada
	                 replace pais with "ESPAÑA"
	                 replace detalle with "AUTOPISTA"
	                 replace moneda with "EUR"
	                 replace proveedor with "CONTADO"
	                 replace importe with wimporte

	                 replace numero with wnumero

	                 replace numcam with wnumcam
                enddo
    
    
    
     case con=="PISTASSERVISAPORTUGUESAS"

                ******pistas servisa portuguesas
                ***************************
                CUESTION="PISTAS SERVISA PORTUGUESAS"
                use detalles index innomdet,incamdet
                seg="s"
                do while seg<>"n"
	                 wimporte=0
	                 clear

	                 @ 1,5 SAY "          " +CUESTION
	                 @ 5,5 say " " +dtoc(wjornada)
	                 @ 7,5 say "importe " get wimporte PICTURE "99,999.99"
	                 read
	                 @ 11,5 say "continuar  s/n" get seg
	                 read

	                 seg=lower(seg)
	                 append blank
	                 replace camion with wcamion
	                 replace conductor with wconductor
	                 replace fecha with wjornada
	                 replace pais with "PORTUGAL"
	                 replace detalle with "AUTOPISTA"
	                 replace moneda with "EUR"
	                 replace proveedor with "SERVISA"
	                 replace importe with wimporte

	                 replace numero with wnumero

	                 replace numcam with wnumcam
                enddo
   


    case con=="D"
                **********otras pistas
                ************************+
                CUESTION="OTRAS PISTAS"
                use detalles index innomdet,incamdet
                seg="s"
                do while seg<>"n"
	                 wimporte=0
	                 store space(10) to wpais
	                 store space(3) to wmoneda
	                 clear

	                 @ 1,5 SAY "          " +CUESTION
	                 @ 5,5 say " " +dtoc(wjornada)
	                 @ 7,5 say "importe " get wimporte PICTURE "99,999.99"
	                 @ 9,5 say "pais   " get wpais
	                 @ 11,5 say "moneda " get wmoneda
	                 read
	                 @ 13,5 say "continuar  s/n" get seg
	                 read

	                 seg=lower(seg)
	                 append blank
	                 replace camion with wcamion
	                 replace conductor with wconductor
	                 replace fecha with wjornada
	                 replace pais with wpais
	                 replace detalle with "AUTOPISTA"
	                 replace moneda with wmoneda

	                 replace importe with wimporte

	                 replace numero with wnumero

	                 replace numcam with wnumcam
                enddo


   case con ="E"
                *********pistas españolas
                **************************
                CUESTION="PISTAS ESPAÑOLAS"
                use detalles index innomdet,incamdet
                seg="s"
                do while seg<>"n"
	                 wimporte=0
	                 clear

	                 @ 1,5 SAY "          " +CUESTION
	                 @ 5,5 say " " +dtoc(wjornada)
	                 @ 7,5 say "importe " get wimporte PICTURE "99,999.99"
	                 read
	                 @ 11,5 say "continuar  s/n" get seg
	                 read

	                 seg=lower(seg)
	                 append blank
	                 replace camion with wcamion
	                 replace conductor with wconductor
	                 replace fecha with wjornada
	                 replace pais with "ESPAÑA"
	                 replace detalle with "AUTOPISTA"
	                 replace moneda with "EUR"
	                 replace proveedor with "DKV"
	                 replace importe with wimporte

	                 replace numero with wnumero

	                 replace numcam with wnumcam
                enddo



   case con="F"
                *******gasoil dkv español
                *************************
                CUESTION="GASOIL DKV ESPAÑOL"
                use detalles index innomdet,incamdet
                seg="s"
                do while seg<>"n"
	                 wimporte=0
	                 wlitros=0
	                 wkilometros=0
	                 wlleno=.f.
	                 clear


	                 @ 1,5 SAY "          " +CUESTION
	                 @ 5,5 say " " +dtoc(wjornada)
	                 @ 7,5 say "importe     " get wimporte PICTURE "99,999.99"
	                 @ 9,5 say "litros     " get wlitros PICTURE "9,999.99"
	                 @ 11,5 say "kilometros " get wkilometros
	                 @ 13,5 say "lleno      " get wlleno
	                 read
	                 @ 19,5 say "continuar  s/n" get seg
	                 read

	                 seg=lower(seg)
	                 append blank
	                 replace camion with wcamion
	                 replace conductor with wconductor
	                 replace fecha with wjornada
	                 replace pais with "ESPAÑA"
	                 replace detalle with "GASOIL"
	                 replace moneda with "EUR"

	                 replace proveedor with "DKV"
	                 replace importe with wimporte
	                 replace lleno with wlleno
	                 replace litros with wlitros
	                 replace kilometros with wkilometros
	                 replace numero with wnumero

	                 replace numcam with wnumcam
                enddo


  case con=="GASOILTEJEDORESPAÑOL"
                *******gasoil TEJEDOR español
                *************************
                CUESTION="GASOIL TEJEDOR ESPAÑOL"
                use detalles index innomdet,incamdet
                seg="s"
                do while seg<>"n"
	                 wimporte=0
	                 wlitros=0
	                 wkilometros=0
	                 wlleno=.f.
	                 clear


	                 @ 1,5 SAY "          " +CUESTION
	                 @ 5,5 say " " +dtoc(wjornada)
	                 @ 7,5 say "importe     " get wimporte PICTURE "99,999.99"
	                 @ 9,5 say "litros     " get wlitros PICTURE "9,999.99"
	                 @ 11,5 say "kilometros " get wkilometros
	                 @ 13,5 say "lleno      " get wlleno
	                 read
	                 @ 19,5 say "continuar  s/n" get seg
	                 read

	                 seg=lower(seg)
	                 append blank
	                 replace camion with wcamion
	                 replace conductor with wconductor
	                 replace fecha with wjornada
	                 replace pais with "ESPAÑA"
	                 replace detalle with "GASOIL"
	                 replace moneda with "EUR"

	                 replace proveedor with "TEJEDOR"
	                 replace importe with wimporte
	                 replace lleno with wlleno
	                 replace litros with wlitros
	                 replace kilometros with wkilometros
	                 replace numero with wnumero

	                 replace numcam with wnumcam
                enddo


 case con=="GASOILCONTADO"
                *******gasoil TEJEDOR español
                *************************
                CUESTION="GASOIL CONTADO"
                use detalles index innomdet,incamdet
                seg="s"
                do while seg<>"n"
	                 wimporte=0
	                 wlitros=0
	                 wkilometros=0
	                 wlleno=.f.
	                 clear


	                 @ 1,5 SAY "          " +CUESTION
	                 @ 5,5 say " " +dtoc(wjornada)
	                 @ 7,5 say "importe     " get wimporte PICTURE "99,999.99"
	                 @ 9,5 say "litros     " get wlitros PICTURE "9,999.99"
	                 @ 11,5 say "kilometros " get wkilometros
	                 @ 13,5 say "lleno      " get wlleno
	                 read
	                 @ 19,5 say "continuar  s/n" get seg
	                 read

	                 seg=lower(seg)
	                 append blank
	                 replace camion with wcamion
	                 replace conductor with wconductor
	                 replace fecha with wjornada
	                 replace pais with "ESPAÑA"
	                 replace detalle with "GASOIL"
	                 replace moneda with "EUR"

	                 replace proveedor with "CONTADO"
	                 replace importe with wimporte
	                 replace lleno with wlleno
	                 replace litros with wlitros
	                 replace kilometros with wkilometros
	                 replace numero with wnumero

	                 replace numcam with wnumcam
                enddo







     case con="G"

               *****gasoil dkv FRANCES
               *******************
               CUESTION="GASOIL DKV FRANCES"
                use detalles index innomdet,incamdet
                seg="s"
                do while seg<>"n"
	                 wimporte=0
	                 wlitros=0
	                 wkilometros=0
	                 wlleno=.f.
	                 clear




	                 @ 1,5 SAY "          " +CUESTION
	                 @ 5,5 say " " +dtoc(wjornada)
	                 @ 7,5 say "importe     " get wimporte PICTURE "99,999.99"
	                 @ 9,5 say "litros     " get wlitros  PICTURE "9,999.99"
	                 @ 11,5 say "kilometros " get wkilometros
	                 @ 13,5 say "lleno      " get wlleno
	                 read
	                 @ 19,5 say "continuar  s/n" get seg
	                 read

	                 seg=lower(seg)
	                 append blank
	                 replace camion with wcamion
	                 replace conductor with wconductor
	                 replace fecha with wjornada
	                 replace pais with "FRANCIA"
	                 replace detalle with "GASOIL"
	                 replace moneda with "EUR"

	                 replace proveedor with "DKV"
	                 replace importe with wimporte
	                 replace lleno with wlleno
	                 replace litros with wlitros
	                 replace kilometros with wkilometros
                     replace numero with wnumero

                     replace numcam with wnumcam
                enddo

     case con="H"


               *****gasoil as24 español
               *******************
               CUESTION="GASOIL AS24 ESPAÑOL"
                use detalles  index innomdet,incamdet
                seg="s"
                do while seg<>"n"


	                 wimporte=0
	                 wlitros=0
	                 wkilometros=0
	                 wlleno=.f.
	                 clear

	                 @ 1,5 SAY "          " +CUESTION
	                 @ 5,5 say " " +dtoc(wjornada)
	                 @ 7,5 say "importe     " get wimporte PICTURE "99,999.99"
	                 @ 9,5 say "litros     " get wlitros PICTURE "9,999.99"
	                 @ 11,5 say "kilometros " get wkilometros
	                 @ 13,5 say "lleno      " get wlleno
	                 read
	                 @ 19,5 say "continuar  s/n" get seg
	                 read

	                 seg=lower(seg)
	                 append blank
	                 replace camion with wcamion
	                 replace conductor with wconductor
	                 replace fecha with wjornada
	                 replace pais with "ESPAÑA"
	                 replace detalle with "GASOIL"
	                 replace moneda with "EUR"

	                 replace proveedor with "AS24"
	                 replace importe with wimporte
	                 replace lleno with wlleno
	                 replace litros with wlitros
	                 replace kilometros with wkilometros
	                 replace numero with wnumero

	                 replace numcam with wnumcam
                enddo



 case con="V"


               *****gasoil as24 español
               *******************
               CUESTION="VALCARCE"
                use detalles  index innomdet,incamdet
                seg="s"
                do while seg<>"n"


	                 wimporte=0
	                 wlitros=0
	                 wkilometros=0
	                 wlleno=.f.
	                 clear

	                 @ 1,5 SAY "          " +CUESTION
	                 @ 5,5 say " " +dtoc(wjornada)
	                 @ 7,5 say "importe     " get wimporte PICTURE "99,999.99"
	                 @ 9,5 say "litros     " get wlitros PICTURE "9,999.99"
	                 @ 11,5 say "kilometros " get wkilometros
	                 @ 13,5 say "lleno      " get wlleno
	                 read
	                 @ 19,5 say "continuar  s/n" get seg
	                 read

	                 seg=lower(seg)
	                 append blank
	                 replace camion with wcamion
	                 replace conductor with wconductor
	                 replace fecha with wjornada
	                 replace pais with "ESPAÑA"
	                 replace detalle with "GASOIL"
	                 replace moneda with "EUR"

	                 replace proveedor with "VALCARCE"
	                 replace importe with wimporte
	                 replace lleno with wlleno
	                 replace litros with wlitros
	                 replace kilometros with wkilometros
	                 replace numero with wnumero

	                 replace numcam with wnumcam
                enddo





case con=="CD"


               *****gasoil as24 español
               *******************
               CUESTION="CENTROLID"
                use detalles  index innomdet,incamdet
                seg="s"
                do while seg<>"n"


	                 wimporte=0
	                 wlitros=0
	                 wkilometros=0
	                 wlleno=.f.
	                 clear

	                 @ 1,5 SAY "          " +CUESTION
	                 @ 5,5 say " " +dtoc(wjornada)
	                 @ 7,5 say "importe     " get wimporte PICTURE "99,999.99"
	                 @ 9,5 say "litros     " get wlitros PICTURE "9,999.99"
	                 @ 11,5 say "kilometros " get wkilometros
	                 @ 13,5 say "lleno      " get wlleno
	                 read
	                 @ 19,5 say "continuar  s/n" get seg
	                 read

	                 seg=lower(seg)
	                 append blank
	                 replace camion with wcamion
	                 replace conductor with wconductor
	                 replace fecha with wjornada
	                 replace pais with "ESPAÑA"
	                 replace detalle with "GASOIL"
	                 replace moneda with "EUR"

	                 replace proveedor with "CENTROLID"
	                 replace importe with wimporte
	                 replace lleno with wlleno
	                 replace litros with wlitros
	                 replace kilometros with wkilometros
	                 replace numero with wnumero

	                 replace numcam with wnumcam
                enddo









        case con="I"


               *****gasoil as24 extrangero
               *******************
               CUESTION="GASOIL AS24 EXTRANJERO"
                use detalles index innomdet,incamdet
                seg="s"
                do while seg<>"n"

	                 wimporte=0
	                 wlitros=0
	                 wkilometros=0
	                 wlleno=.f.
	                 clear


	                 @ 1,5 SAY "          " +CUESTION

	                 @ 4,1 TO 24,79 DOUBLE
	                 @ 5,5 say " " +dtoc(wjornada)
	                 @ 7,5 say "importe     " get wimporte PICTURE "99,999.99"
	                 @ 9,5 say "litros     " get wlitros PICTURE "9,999.99"
	                 @ 11,5 say "kilometros " get wkilometros
	                 @ 13,5 say "lleno      " get wlleno
	                 read
	                 @ 19,5 say "continuar  s/n" get seg
	                 read

	                 seg=lower(seg)
	                 append blank
	                 replace camion with wcamion
	                 replace conductor with wconductor
	                 replace fecha with wjornada
	                 replace pais with "FRANCIA"
	                 replace detalle with "GASOIL"
	                 replace moneda with "EUR"

	                 replace proveedor with "AS24"
	                 replace importe with wimporte
	                 replace lleno with wlleno
	                 replace litros with wlitros
	                 replace kilometros with wkilometros

	                 replace numero with wnumero

	                 replace numcam with wnumcam
                enddo

     case con="J"

               *****gasoil cidy
               *******************
               CUESTION="GASOIL CIDY"
                use detalles  index innomdet,incamdet
                seg="s"
                do while seg<>"n"

	                 wimporte=0
	                 wlitros=0
	                 wkilometros=0
	                 wlleno=.T.
	                 clear

	                 @ 1,5 SAY "          " +CUESTION


	                 @ 4,1 TO 24,79 DOUBLE
	                 @ 5,5 say " " +dtoc(wjornada)


	                 @ 7,5 say "importe     " get wimporte PICTURE "99,999.99"
	                 @ 9,5 say "litros     " get wlitros PICTURE "9,999.99"
	                 @ 11,5 say "kilometros " get wkilometros
	                 @ 13,5 say "lleno      " get wlleno
	                 read
	                 @ 19,5 say "continuar  s/n" get seg
	                 read

	                 seg=lower(seg)
	                 append blank
	                 replace camion with wcamion
	                 replace conductor with wconductor
	                 replace fecha with wjornada
	                 replace pais with "ESPAÑA"
	                 replace detalle with "GASOIL"
	                 replace moneda with "EUR"

	                 replace proveedor with "CIDY"
	                 replace importe with wimporte
	                 replace lleno with wlleno
	                 replace litros with wlitros
	                 replace kilometros with wkilometros
	                 replace numero with wnumero

	                 replace numcam with wnumcam
                enddo

       case con="K"


               *****gasoil nave
               *******************
               CUESTION="GASOIL NAVE"
                use detalles index innomdet,incamdet
                seg="s"
                do while seg<>"n"

	                 wimporte=0
	                 wlitros=0
	                 wkilometros=0
	                 wlleno=.T.
	                 clear

	                 @ 1,5 SAY "          " +CUESTION
	                 @ 5,5 say " " +dtoc(wjornada)
	                 @ 7,5 say "precio/LT  " get wimporte PICTURE "99,999.99"
	                 @ 9,5 say "litros     " get wlitros  PICTURE "9,999.99"
	                 @ 11,5 say "kilometros " get wkilometros
	                 @ 13,5 say "lleno      " get wlleno
	                 read
	                 @ 19,5 say "continuar  s/n" get seg
	                 read

	                 seg=lower(seg)
	                 append blank
	                 replace camion with wcamion
	                 replace conductor with wconductor
	                 replace fecha with wjornada
	                 replace pais with "ESPAÑA"
	                 replace detalle with "GASOIL"
	                 replace moneda with "EUR"

	                 replace proveedor with "NAVE"
	                 WIMPORTE=WIMPORTE*WLITROS
	                 replace importe with wimporte
	                 replace lleno with wlleno
	                 replace litros with wlitros
	                 replace kilometros with wkilometros
	                 replace numero with wnumero

	                 replace numcam with wnumcam
                enddo

        case con="L"

               *******otros español
               *******************
               CUESTION="OTROS ESPAÑOL"

                use detalles index innomdet,incamdet
                seg="s"
                do while seg<>"n"
	                 wimporte=0
	                 wproveedor=SPACE(10)
	                 wlitros=0
	                 store space(10) to wdetalle
	                 clear

	                 @ 1,5 SAY "          " +CUESTION
	                 @ 5,5 say " " +dtoc(wjornada)
	                 @ 7,5 say "importe      " get wimporte PICTURE "99,999.99"
	                 @ 8,5 say "litros       " get wlitros picture "999.99"
	                 
	                 @ 9,5 say "detalle    " get wdetalle
                     @ 10,5 say "proveedor  " get wproveedor

	                 read
	                 @ 15,5 say "continuar  s/n" get seg
	                 read

	                 seg=lower(seg)
	                 append blank
	                 replace camion with wcamion
	                 replace conductor with wconductor
	                 replace fecha with wjornada
	                 replace pais with "ESPAÑA"
	                 replace detalle with wdetalle
	                 replace moneda with "EUR"

	                 replace proveedor with wproveedor
	                 replace importe with wimporte
                     replace litros WITH wlitros
                     
	                 replace numero with wnumero

	                 replace numcam with wnumcam
                enddo

         case con="M"

                *******otros extranjero
                ************************
                CUESTION="OTROS EXTRANJERO"
                use detalles index innomdet,incamdet
                seg="s"
                do while seg<>"n"

	                 wimporte=0
	                 store space(10) to wdetalle
	                 clear

	                 @ 1,5 SAY "          " +CUESTION
	                 @ 5,5 say " " +dtoc(wjornada)
	                 @ 7,5 say "importe     " get wimporte PICTURE "99,999.99"
	                 @ 9,5 say "detalle    " get wdetalle


	                 read
	                 @ 17,5 say "continuar  s/n" get seg
	                 read

	                 seg=lower(seg)
	                 append blank
	                 replace camion with wcamion
	                 replace conductor with wconductor
	                 replace fecha with wjornada
	                 replace pais with "FRANCIA"
	                 replace detalle with wdetalle
	                 replace moneda with "EUR"

	                 replace proveedor with "DKV"
	                 replace importe with wimporte

	                 replace numero with wnumero

	                 replace numcam with wnumcam
                enddo

       case con="N"
                 *********entrega de dinero
                 ************************
                 CUESTION="ENTREGA DE DINERO"
                use detalles index innomdet,incamdet
                seg="s"
                do while seg<>"n"
	                 wimporte=0
	                 clear

	                 @ 1,5 SAY "          " +CUESTION
	                 @ 5,5 say " " +dtoc(wjornada)
	                 @ 7,5 say "importe    " get wimporte PICTURE "99,999.99"



	                 read
	                 @ 16,5 say "continuar  s/n" get seg
	                 read
	                 seg=lower(seg)
	                 append blank
	                 replace camion with wcamion
	                 replace conductor with wconductor
	                 replace fecha with wjornada

	                 replace detalle with "ENTREGA"
	                 replace moneda with "EUR"


	                 replace importe with wimporte

	                 replace numero with wnumero

	                 replace numcam with wnumcam
                enddo
       case con="Q"

       

               wjornada=wjornada+1
       CASE con=="DISMINUIRFECHA"
               wjornada=wjornada-1
       
       case con="S"
               terminado="s"


       case con="O"

               *****gasoil dkv ITALIANO
               *******************
               CUESTION="GASOIL DKV ITALIANO"
                use detalles index innomdet,incamdet
                seg="s"
                do while seg<>"n"
	                 wimporte=0
	                 wlitros=0
	                 wkilometros=0
	                 wlleno=.f.
	                 clear



	                 @ 1,5 SAY "          " +CUESTION

	                 @ 5,5 say " " +dtoc(wjornada)
	                 @ 7,5 say "importe     " get wimporte PICTURE "99,999.99"
	                 @ 9,5 say "litros     " get wlitros  PICTURE "9,999.99"
	                 @ 11,5 say "kilometros " get wkilometros
	                 @ 13,5 say "lleno      " get wlleno
	                 read
	                 @ 19,5 say "continuar  s/n" get seg
	                 read

	                 seg=lower(seg)
	                 append blank
	                 replace camion with wcamion
	                 replace conductor with wconductor
	                 replace fecha with wjornada
	                 replace pais with "ITALIA"
	                 replace detalle with "GASOIL"
	                 replace moneda with "EUR"

	                 replace proveedor with "DKV"
	                 replace importe with wimporte
	                 replace lleno with wlleno
	                 replace litros with wlitros
	                 replace kilometros with wkilometros
	                 replace numero with wnumero

	                 replace numcam with wnumcam
                enddo


       case con="R"

               *****gasoil dkv PORTUGUES
               *******************
               CUESTION="GASOIL DKV PORTUGUES"
                use detalles index innomdet,incamdet
                seg="s"
                do while seg<>"n"
	                 wimporte=0
	                 wlitros=0
	                 wkilometros=0
	                 wlleno=.f.
	                 clear



	                 @ 1,5 SAY "          " +CUESTION

	                 @ 5,5 say " " +dtoc(wjornada)
	                 @ 7,5 say "importe     " get wimporte PICTURE "99,999.99"
	                 @ 9,5 say "litros     " get wlitros  PICTURE "9,999.99"
	                 @ 11,5 say "kilometros " get wkilometros
	                 @ 13,5 say "lleno      " get wlleno
	                 read
	                 @ 19,5 say "continuar  s/n" get seg
	                 read

	                 seg=lower(seg)
	                 append blank
	                 replace camion with wcamion
	                 replace conductor with wconductor
	                 replace fecha with wjornada
	                 replace pais with "PORTUGAL"
	                 replace detalle with "GASOIL"
	                 replace moneda with "EUR"

	                 replace proveedor with " "
	                 replace importe with wimporte
	                 replace lleno with wlleno
	                 replace litros with wlitros
	                 replace kilometros with wkilometros
	                 replace numero with wnumero

	                 replace numcam with wnumcam
                enddo






    endcase
       
  enddo

**************************
*************** INTRODUCCION DE MEDIAS DE GASOIL EN EL ARCHIVO GASOIL.DBF
***************************************************


IF (WCAMION<>"VA-1178-AD").AND.(WCAMION<>"VA-5510-AF").AND.WCAMION<>"8263BYG".AND.WCAMION<>"814BFC".AND.WCAMION<>"1852BFR"

*********************************************************
************************************************************

**************************************************************
SELECT 1
use detalles index incamdet,innomdet
SELECT 2
USE GASOIL INDEX INGAS
SELECT 1
gclave=wcamion+str(wnumcam,6)
SEEK gclave
****caso 1§ detalle del parte lleno
if lleno.AND.KILOMETROS<>0.AND.WNUMCAM<>1
     skip -1
endif
do while .not.lleno.OR.KILOMETROS=0
        skip -1
enddo
inikim=kilometros
TOTLIT=0
SKIP
N1=NUMCAM
C1=CAMION
do while WCAMION=C1.AND.(.NOT.EOF()).AND.WNUMCAM>=N1


      if detalle="GASOIL"

             TOTLIT=TOTLIT+LITROS

      endif
      if lleno.AND.KILOMETROS<>0

             auxfecha=fecha
             finkim=kilometros
             WCONDUCTOR=CONDUCTOR
             if finkim<inikim
                 ********caso cambio de cuentakilometros
                 INIKIM=INIKIM-1000000
                 *inikim=0
             endif


             distancia=finkim-inikim
             SELECT 2
                 ******PARECE QUE PARA QUE FUNCIONE EL APPEND BLANK,
                 ********EL FICHERO TIENE QUE ESTAR INDEXADO
               
               primeravez=.T.
               IF primeravez
                APPEND BLANK
                replace litros with totlit
                replace kilometros with distancia
                replace km_llenado with finkim
                replace numcam with wnumcam
                replace camion with wcamion
                replace numero WITH wnumero
                REPLACE FECHA WITH  AUXFECHA
                REPLACE CONDUCTOR WITH WCONDUCTOR
                 media=totlit/distancia*100
                 replace promedio with media
               ENDIF  
                 
                 
                  
             
             
             SELECT 1
             *?" Fecha de llenado=",auxfecha,"    Km. iniciales=",inikim,"   Km. finales=",finkim
             *?" Distancia= ",distancia,"Km."
             *?" Litros=    ",totlit,"Lt."

             
             SELECT 2
             SUM LITROS FOR CAMION=WCAMION.AND.YEAR(FECHA)=2006 TO A
             SUM KILOMETROS FOR CAMION=WCAMION.AND.YEAR(FECHA)=2006 TO B
             MED06=A/B*100

             SUM LITROS FOR CAMION=WCAMION.AND.YEAR(FECHA)=2007 TO C
             SUM KILOMETROS FOR CAMION=WCAMION.AND.YEAR(FECHA)=2007 TO D
             MED07=C/D*100
             SELECT 1
             *?" Gasto=     ",media,"Lt./100Km."," ","MED05",MED05,"MED06",MED06
             inikim=finkim
             totlit=0
      endif
    
      SKIP
      N1=NUMCAM
      C1=CAMION

enddo
ENDIF


return

*****************************
********RESUMEN DE PARTES
******************************


procedure RESUPAR



R=1
DO WHILE R<6
DO CASE
     *CASE R=9.OR.R=10
       *   R=3
     *CASE R=10
       *   R=10
ENDCASE
store space(10) to wconductor
wnumero=0

DO CASE
       CASE R=1
           wconductor ="VALENTIN  "
           wnumero=265
     CASE R=2
          WCONDUCTOR  ="CARLOS    "
          WNUMERO=245
       CASE R=3
            WCONDUCTOR="ALFONSO   "
            WNUMERO=180
      CASE R=4
           WCONDUCTOR ="JUAN JOSE "
           WNUMERO=192
      CASE R=5
           WCONDUCTOR ="NACHO     "
           WNUMERO=157
     CASE R=6
          WCONDUCTOR  ="ALFONSO   "
          WNUMERO=173
       CASE R=7
            WCONDUCTOR="NACHO     "
            WNUMERO=147
      CASE R=8
           WCONDUCTOR ="LUIS      "
           WNUMERO=1
      CASE R=9
           WCONDUCTOR ="CARLOS    "
           WNUMERO=214
      CASE R=10
           WCONDUCTOR ="CARLOS    "
           WNUMERO=215
      CASE R=11
           WCONDUCTOR ="JUAN JOSE "
           WNUMERO=178
      CASE R=12
           WCONDUCTOR ="JUAN JOSE "
           WNUMERO=179
      CASE R=13
           WCONDUCTOR ="CARLOS    "
           WNUMERO=126
      CASE R=14
           WCONDUCTOR ="CARLOS    "
           WNUMERO=127
      CASE R=15
           WCONDUCTOR ="JAVIER    "
           WNUMERO=115
      
ENDCASE
use partes index innomnum,INCAMNUM

clear



clave=wconductor+str(wnumero,6)
SEEK clave

wfecha=fechaini
wfechaFIN=fechaFIN

WCAMION=CAMION
wnumcam=numcam
WDISCOS=DISCOS



SET PRINT ON PROMPT
SET PRINT FONT "COURIER new",9
set century off
?"Nombre: ",conductor,"Matricula: ",camion,"Fecha: ",Fechaini,"al",Fechafin," Nº Parte: ",NUMERO,NUMCAM,CHR(27)+CHR(72)
?"================================================================================"

USE DETALLES
MANUEL=.T.
IF MANUEL
sum ALL IMPORTE for (detalle="AUTOPISTA").AND.(PAIS="FRANCIA").AND.(NUMERO=WNUMERO).and.(conductor=wconductor) TO AUTOFRAN

sum ALL IMPORTE for (detalle="AUTOPISTA").AND.(PAIS="ITALIA").AND.(NUMERO=WNUMERO).and.(conductor=wconductor) TO AUTOITA

sum ALL IMPORTE for (detalle="AUTOPISTA").AND.(PAIS="PORTUGAL").AND.(NUMERO=WNUMERO).and.(conductor=wconductor) TO AUTOPOR

sum ALL IMPORTE for (detalle="AUTOPISTA").AND.(PAIS="ESPAÑA").AND.(NUMERO=WNUMERO).and.(conductor=wconductor) TO AUTOES

sum ALL LITROS for (detalle="GASOIL").AND.(NUMERO=WNUMERO).and.(conductor=wconductor) TO TGAS

sum ALL IMPORTE for ((detalle="LAMPARA").OR.(detalle="ACEITE").OR.(DETALLE="TELEFONO").or.(DETALLE="TAXI")).AND.(PAIS="FRANCIA").AND.(NUMERO=WNUMERO).and.(conductor=wconductor) TO TFVAR

sum ALL IMPORTE for ((detalle="LAMPARA").OR.(detalle="ACEITE").OR.(DETALLE="TELEFONO").OR.(DETALLE="TAXI")).AND.(PAIS="ESPAÑA").AND.(NUMERO=WNUMERO).and.(conductor=wconductor) TO TEVAR

sum ALL IMPORTE for (detalle="ENTREGA").AND.(NUMERO=WNUMERO).and.(conductor=wconductor) TO TENTREGA
?
?
?"Autopistas españolas:   ",STR(autoes,10,2),"EUR"
?"Autopistas francesas:   ",STR(autofran,10,2),"EUR"
?"Autopistas italianas:   ",STR(autoita,10,2),"EUR"
?"Autopistas portuguesas: ",STR(autopor,10,2),"EUR"
?
pistas=autoes+autofran+autopor+autoita
?"Total autopistas      : ",STR(pistas,10,2),"EUR"
?
?
?"Total litros de gasoil: ",tgas,"lt."
?
?"Dinero entregado:        ",STR(tentrega,10,2),"EUR","Discos entregados:  ",wdiscos
?
WTVAR=TFVAR+TEVAR
?"Total varios          : ",STR(WTVAR,10,2),"EUR"
ENDIF    && DE MANUEL


IF (WCAMION<>"VA-1178-AD").AND.(WCAMION<>"VA-5510-AF")

*********************************************************
************************************************************

**************************************************************
SELECT 1
use detalles index incamdet,innomdet
SELECT 2
USE GASOIL INDEX INGAS
SELECT 1
gclave=wcamion+str(wnumcam,6)
SEEK gclave
****caso 1§ detalle del parte lleno
if lleno.AND.KILOMETROS<>0
     skip -1
endif
do while .not.lleno.OR.KILOMETROS=0
        skip -1
enddo
inikim=kilometros
TOTLIT=0
SKIP
N1=NUMCAM
C1=CAMION
do while WCAMION=C1.AND.(.NOT.EOF()).AND.WNUMCAM>=N1


      if detalle="GASOIL"

             TOTLIT=TOTLIT+LITROS

      endif
      if lleno.AND.KILOMETROS<>0

             auxfecha=fecha
             finkim=kilometros
             WCONDUCTOR=CONDUCTOR
             if finkim<inikim
                 ********caso cambio de cuentakilometros
                 inikim=0
             endif


             distancia=finkim-inikim
                 media=totlit/distancia*100

             SELECT 1
             ?" Fecha de llenado=",auxfecha,"    Km. iniciales=",inikim,"   Km. finales=",finkim
             ?" Distancia= ",distancia,"Km."
             ?" Litros=    ",totlit,"Lt."
             SELECT 2
             SUM LITROS FOR CAMION=WCAMION.AND.YEAR(FECHA)=2006 TO A
             SUM KILOMETROS FOR CAMION=WCAMION.AND.YEAR(FECHA)=2006 TO B
             MED06=A/B*100

             SUM LITROS FOR CAMION=WCAMION.AND.YEAR(FECHA)=2007 TO C
             SUM KILOMETROS FOR CAMION=WCAMION.AND.YEAR(FECHA)=2007 TO D
             MED07=C/D*100
             SELECT 1
             ?" Gasto=     ",STR(media,5,2),"Lt./100Km."," ","MED06",STR(MED06,5,2),"       MED07",STR(MED07,5,2)
             inikim=finkim
             totlit=0
      endif
      SKIP
      N1=NUMCAM
      C1=CAMION

enddo
ENDIF
************************************************************
*************************************************************
*************************************************************




TESTIGO=(WFECHA<=wFECHAFIN)
***********************CONDENADO

do while TESTIGO
  use jornada index innomjor
  wclave =clave +str(year(wfecha),4)+str(month(wfecha),2)+str(day(wfecha),2)
  SEEK wclave
  TFECHA=fecha
  if found()
   do while tfecha=fecha
     A=CDOW(FECHA)
     A=LEFT(A,8)
     I=LEN(A)
     N=8-I
     B=A+SPACE(N)

     ?B,fecha,origen,destino,mercancia,kilos,"Kg.",cliente
     skip
    enddo
  else

     A=CDOW(WFECHA)
     A=LEFT(A,8)
     I=LEN(A)
     N=8-I
     B=A+SPACE(N)
   
  ENDIF
  use detalles  index innomdet,INCAMDET
  SEEK wclave

    do while (wclave=conductor+str(numero,6)+str(year(fecha),4)+str(month(fecha),2)+str(day(fecha),2)).and.(.not.eof())

    if detalle="GASOIL"
     A=CDOW(FECHA)
     A=LEFT(A,8)
     I=LEN(A)
     N=8-I
     B=A+SPACE(N)
      DO CASE
         CASE LLENO
               WLLENO="LLENO"
         CASE .NOT.LLENO
               WLLENO="-----"
      ENDCASE
    ELSE

     A=CDOW(FECHA)
     A=LEFT(A,8)
     I=LEN(A)
     N=8-I
     B=A+SPACE(N)
    ENDIF
      skip
    enddo



  wfecha=wfecha+1

  TESTIGO=(WFECHA<=wFECHAFIN)


enddo






SET PRINT OFF
SET PRINTER TO
set century on
R=R+1
ENDDO
WAIT " "


return


*****************************
********IMPRESION
******************************


procedure impresion

use partes index innomnum,INCAMNUM





store space(10) to wconductor,wcamion

wCONDUCTOR=pCONDUCTOR
 


wnumero = pnumero
wnumcam= 0
WRESP="p"
wgrab="n"
clear
@ 9,10 SAY  "camion          " get wcamion
@ 11,10 SAY "numero camion   " get wnumcam
@ 13,10 say "conductor       " get wconductor
@ 15,10 say "numero de parte " get wnumero
@ 17,10 SAY "SALIDA (P)ANTALLA O (I)MPRESORA"  GET WRESP
@ 19,10 say "(G)rabar gasoil " get wgrab
read

WRESP=LOWER(WRESP)
wgrab=LOWER(wgrab)

clave=wconductor+str(wnumero,6)
SEEK clave

wfecha=fechaini
wfechaFIN=fechaFIN

WCAMION=CAMION
wnumcam=numcam
DO CASE
 CASE WRESP="i"
 

     SET PRINT ON PROMPT
 CASE WRESP="p"
      SET ALTERNATE TO aparte.txt
      SET ALTERNATE ON         

 
      
ENDCASE
*****************************
*IF IMPRESORA<>""
*****************************

SET PRINT FONT "COURIER new",9
set century off



?
?
?
?
?


?"Nombre: "+conductor+"  Matricula: "+camion+"  Fecha: "+DTOC(Fechaini)+" al "+DTOC(Fechafin)+" Nº Parte: ",NUMERO,NUMCAM
?"================================================================================"

USE DETALLES INDEX INNOMDET,INCAMDET
MANUEL=.T.
IF MANUEL
sum ALL IMPORTE for (detalle="AUTOPISTA").AND.(PAIS="FRANCIA").AND.(NUMERO=WNUMERO).and.(conductor=wconductor) TO AUTOFRAN

sum ALL IMPORTE for (detalle="AUTOPISTA").AND.(PAIS="ITALIA").AND.(NUMERO=WNUMERO).and.(conductor=wconductor) TO AUTOITA

sum ALL IMPORTE for (detalle="AUTOPISTA").AND.(PAIS="PORTUGAL").AND.(NUMERO=WNUMERO).and.(conductor=wconductor) TO AUTOPOR

sum ALL IMPORTE for (detalle="AUTOPISTA").AND.(PAIS="ESPAÑA").AND.(NUMERO=WNUMERO).and.(conductor=wconductor) TO AUTOES

sum ALL LITROS for (detalle="GASOIL").AND.(NUMERO=WNUMERO).and.(conductor=wconductor) TO TGAS

sum ALL IMPORTE for ((detalle="LAMPARA").OR.(detalle="ACEITE").OR.(DETALLE="TELEFONO").or.(DETALLE="TAXI")).AND.(PAIS="FRANCIA").AND.(NUMERO=WNUMERO).and.(conductor=wconductor) TO TFVAR

sum ALL IMPORTE for ((detalle="LAMPARA").OR.(detalle="ACEITE").OR.(DETALLE="TELEFONO").OR.(DETALLE="TAXI")).AND.(PAIS="ESPAÑA").AND.(NUMERO=WNUMERO).and.(conductor=wconductor) TO TEVAR

sum ALL IMPORTE for (detalle="ENTREGA").AND.(NUMERO=WNUMERO).and.(conductor=wconductor) TO TENTREGA
?
?
?"Autopistas españolas:   ",STR(autoes,10,2),"EUR"
?"Autopistas francesas:   ",STR(autofran,10,2),"EUR"
?"Autopistas italianas:   ",STR(autoita,10,2),"EUR"
?"Autopistas portuguesas: ",STR(autopor,10,2),"EUR"
?
pistas=autoes+autofraN+autopor+autoita
?"Total autopistas      : ",STR(pistas,10,2),"EUR"
?
?
?"Total litros de gasoil: ",tgas,"lt."
?
?"Dinero entregado:        ",STR(tentrega,10,2),"EUR"
?
WTVAR=TFVAR+TEVAR
?"Total varios          : ",WTVAR,"EUR"
ENDIF    && DE MANUEL

IF (WCAMION<>"VA-1178-AD").AND.(WCAMION<>"VA-5510-AF").AND.WCAMION<>"8263BYG".AND.WCAMION<>"814BFC".AND.WCAMION<>"1852BFR"

*********************************************************
************************************************************

**************************************************************
SELECT 1
use detalles index incamdet,innomdet
SELECT 2
USE GASOIL INDEX INGAS
SELECT 1
gclave=wcamion+str(wnumcam,6)
SEEK gclave
****caso 1§ detalle del parte lleno
if lleno.AND.KILOMETROS<>0.AND.WNUMCAM<>1
     skip -1
endif
do while .not.lleno.OR.KILOMETROS=0
        skip -1
enddo
inikim=kilometros
TOTLIT=0
SKIP
N1=NUMCAM
C1=CAMION
do while WCAMION=C1.AND.(.NOT.EOF()).AND.WNUMCAM>=N1


      if detalle="GASOIL"

             TOTLIT=TOTLIT+LITROS

      endif
      if lleno.AND.KILOMETROS<>0

             auxfecha=fecha
             finkim=kilometros
             WCONDUCTOR=CONDUCTOR
             if finkim<inikim
                 ********caso cambio de cuentakilometros
                 INIKIM=INIKIM-1000000
                 *inikim=0
             endif


             distancia=finkim-inikim
             SELECT 2
                 ******PARECE QUE PARA QUE FUNCIONE EL APPEND BLANK,
                 ********EL FICHERO TIENE QUE ESTAR INDEXADO
               IF wgrab="g"
                  grabar=.t.
               ELSE
                  
                  grabar=.f.
               ENDIF
               
               IF grabar
               
                APPEND BLANK
                replace litros with totlit
                replace kilometros with distancia
                replace km_llenado with finkim
                replace numcam with wnumcam
                replace camion with wcamion
                replace numero WITH wnumero
                REPLACE FECHA WITH  AUXFECHA
                REPLACE CONDUCTOR WITH WCONDUCTOR
                media=totlit/distancia*100
                replace promedio with media
               ELSE 
                 media=totlit/distancia*100
               ENDIF
                 
                  
             
             
             SELECT 1
             ?" Fecha de llenado=",auxfecha,"    Km. iniciales=",inikim,"   Km. finales=",finkim
             ?" Distancia= ",distancia,"Km."
             ?" Litros=    ",totlit,"Lt."

             
             SELECT 2
             SUM LITROS FOR CAMION=WCAMION.AND.YEAR(FECHA)=2007 TO A
             SUM KILOMETROS FOR CAMION=WCAMION.AND.YEAR(FECHA)=2007 TO B
             MED07=A/B*100

             SUM LITROS FOR CAMION=WCAMION.AND.YEAR(FECHA)=2008 TO C
             SUM KILOMETROS FOR CAMION=WCAMION.AND.YEAR(FECHA)=2008 TO D
             MED08=C/D*100
             SELECT 1
             ?" Gasto=     ",STR(media,5,2),"Lt./100Km."," ","MED07",STR(MED07,5,2),"       MED08",STR(MED08,5,2)
             inikim=finkim
             totlit=0
      endif
    
      SKIP
      N1=NUMCAM
      C1=CAMION

enddo
ENDIF
************************************************************
*************************************************************
*************************************************************


WAIT " "
?
?
TESTIGO=(WFECHA<=wFECHAFIN)


do while TESTIGO
  use jornada index innomjor
  wclave =clave +str(year(wfecha),4)+str(month(wfecha),2)+str(day(wfecha),2)
  SEEK wclave
  TFECHA=fecha
  if found()
   do while tfecha=fecha
     A=CDOW(FECHA)
     A=LEFT(A,8)
     I=LEN(A)
     N=8-I
     B=A+SPACE(N)

     ?B,fecha,origen,destino,mercancia,kilos,"Kg.",cliente
     ?"--------------------------------------------------------------------------------"
     skip
    enddo
  else

     A=CDOW(WFECHA)
     A=LEFT(A,8)
     I=LEN(A)
     N=8-I
     B=A+SPACE(N)
     ?B,wfecha
     ?"-----------------------"
     IF CDOW(WFECHA)="Domingo"
          ?"-----------------------"
     ENDIF
  
  ENDIF
  use detalles  index innomdet
  SEEK wclave

    do while (wclave=conductor+str(numero,6)+str(year(fecha),4)+str(month(fecha),2)+str(day(fecha),2)).and.(.not.eof())

    if detalle="GASOIL"
     A=CDOW(FECHA)
     A=LEFT(A,8)
     I=LEN(A)
     N=8-I
     B=A+SPACE(N)
      DO CASE
         CASE LLENO
               WLLENO="LLENO"
         CASE .NOT.LLENO
               WLLENO="-----"
      ENDCASE
      ?detalle,proveedor,pais,STR(importe,10,2),moneda,litros,"Lt.",kilometros,"Km.",Wlleno
    ELSE

     A=CDOW(FECHA)
     A=LEFT(A,8)
     I=LEN(A)
     N=8-I
     B=A+SPACE(N)
     ?detalle,proveedor,pais,STR(importe,10,2),moneda,CHR(27)+CHR(70)
    ENDIF
      skip
    enddo



  wfecha=wfecha+1

  TESTIGO=(WFECHA<=wFECHAFIN)


enddo



DO CASE
   CASE WRESP="i"


      SET PRINT OFF
      SET PRINTER TO
   CASE WRESP="p"
      SET ALTERNATE off
      SET ALTERNATE TO
      MODIFY FILE APARTE.txt NOEDIT
      DELETE FILE APARTE.txt
ENDCASE
*************************************
*ENDIF && DE SELECCION DE IMPRESORA
*************************************

set century on
WAIT


return



**************************
******* SALIDA DE PARTES A FICHERO
*************************


*****************************
********
******************************


procedure SALARCH

use partes index innomnum,INCAMNUM
store space(10) to wconductor,wcamion
wnumero = 0
wnumcam= 0
clear
@ 9,10 SAY  "camion          " get wcamion
@ 11,10 SAY "numero camion   " get wnumcam
@ 13,10 say "conductor       " get wconductor
@ 15,10 say "numero de parte " get wnumero
read



clave=wconductor+str(wnumero,6)
SEEK clave

wfecha=fechaini
wfechaFIN=fechaFIN

WCAMION=CAMION
wnumcam=numcam
SET ALTERNATE TO PUESSI
SET ALTERNATE ON
set century off
?
?"Nombre: ",conductor,"Matricula: ",camion,"Fecha: ",Fechaini,"al",Fechafin," Nº Parte: ",NUMERO,NUMCAM
?"================================================================================"

USE DETALLES INDEX INNOMDET,INCAMDET
MANUEL=.T.
IF MANUEL
sum ALL IMPORTE for (detalle="AUTOPISTA").AND.(PAIS="FRANCIA").AND.(NUMERO=WNUMERO).and.(conductor=wconductor) TO AUTOFRAN

sum ALL IMPORTE for (detalle="AUTOPISTA").AND.(PAIS="ITALIA").AND.(NUMERO=WNUMERO).and.(conductor=wconductor) TO AUTOITA

sum ALL IMPORTE for (detalle="AUTOPISTA").AND.(PAIS="PORTUGAL").AND.(NUMERO=WNUMERO).and.(conductor=wconductor) TO AUTOPOR

sum ALL IMPORTE for (detalle="AUTOPISTA").AND.(PAIS="ESPAÑA").AND.(NUMERO=WNUMERO).and.(conductor=wconductor) TO AUTOES

sum ALL LITROS for (detalle="GASOIL").AND.(NUMERO=WNUMERO).and.(conductor=wconductor) TO TGAS

sum ALL IMPORTE for ((detalle="LAMPARA").OR.(detalle="ACEITE").OR.(DETALLE="TELEFONO").or.(DETALLE="TAXI")).AND.(PAIS="FRANCIA").AND.(NUMERO=WNUMERO).and.(conductor=wconductor) TO TFVAR

sum ALL IMPORTE for ((detalle="LAMPARA").OR.(detalle="ACEITE").OR.(DETALLE="TELEFONO").OR.(DETALLE="TAXI")).AND.(PAIS="ESPAÑA").AND.(NUMERO=WNUMERO).and.(conductor=wconductor) TO TEVAR

sum ALL IMPORTE for (detalle="ENTREGA").AND.(NUMERO=WNUMERO).and.(conductor=wconductor) TO TENTREGA
?
?
?"Autopistas españolas:   ",autoes,"EUR"
?"Autopistas francesas:   ",autofran,"EUR"
?"Autopistas italianas:   ",autoita,"EUR"
?"Autopistas portuguesas: ",autopor,"EUR"
?
pistas=autoes+autofran+autopor+autoita
?"Total autopistas      : ",STR(pistas,10,2),"EUR"
?
?
?"Total litros de gasoil: ",tgas,"lt."
?
?"Dinero entregado:        ",STR(tentrega,10,2),"EUR"
?
WTVAR=TFVAR+TEVAR
?"Total varios          : ",STR(WTVAR,10,2),"EUR"
ENDIF    && DE MANUEL

IF (WCAMION<>"VA-1178-AD").AND.(WCAMION<>"VA-5510-AF")

*********************************************************
************************************************************

**************************************************************
SELECT 1
use detalles index incamdet,innomdet
SELECT 2
USE GASOIL INDEX INGAS
SELECT 1
gclave=wcamion+str(wnumcam,6)
SEEK gclave
****caso 1§ detalle del parte lleno
if lleno.AND.KILOMETROS<>0.AND.WNUMCAM<>1
     skip -1
endif
do while .not.lleno.OR.KILOMETROS=0
        skip -1
enddo
inikim=kilometros
TOTLIT=0
SKIP
N1=NUMCAM
C1=CAMION
do while WCAMION=C1.AND.(.NOT.EOF()).AND.WNUMCAM>=N1


      if detalle="GASOIL"

             TOTLIT=TOTLIT+LITROS

      endif
      if lleno.AND.KILOMETROS<>0

             auxfecha=fecha
             finkim=kilometros
             WCONDUCTOR=CONDUCTOR
             if finkim<inikim
                 ********caso cambio de cuentakilometros
                 INIKIM=INIKIM-1000000
                 
                 
             endif


             distancia=finkim-inikim
             SELECT 2
                 ******PARECE QUE PARA QUE FUNCIONE EL APPEND BLANK,
                 ********EL FICHERO TIENE QUE ESTAR INDEXADO
                APPEND BLANK
                replace litros with totlit
                replace kilometros with distancia
                replace km_llenado with finkim
                replace numcam with wnumcam
                replace camion with wcamion
                REPLACE FECHA WITH  AUXFECHA
                REPLACE CONDUCTOR WITH WCONDUCTOR
                 media=totlit/distancia*100
                 replace promedio with media

             SELECT 1
             ?" Fecha de llenado=",auxfecha,"    Km. iniciales=",inikim,"   Km. finales=",finkim
             ?" Distancia= ",distancia,"Km."
             ?" Litros=    ",totlit,"Lt."
             SELECT 2
             SUM LITROS FOR CAMION=WCAMION.AND.YEAR(FECHA)=2006 TO A
             SUM KILOMETROS FOR CAMION=WCAMION.AND.YEAR(FECHA)=2006 TO B
             MED06=A/B*100

             SUM LITROS FOR CAMION=WCAMION.AND.YEAR(FECHA)=2007 TO C
             SUM KILOMETROS FOR CAMION=WCAMION.AND.YEAR(FECHA)=2007 TO D
             MED07=C/D*100
             SELECT 1
             ?" Gasto=     ",STR(media,5,2),"Lt./100Km."," ","MED06",STR(MED06,5,2),"       MED07",STR(MED07,5,2)
             inikim=finkim
             totlit=0
      endif
      SKIP
      N1=NUMCAM
      C1=CAMION

enddo
ENDIF
************************************************************
*************************************************************
*************************************************************


WAIT " "
?
?
TESTIGO=(WFECHA<=wFECHAFIN)


do while TESTIGO
  use jornada index innomjor
  wclave =clave +str(year(wfecha),4)+str(month(wfecha),2)+str(day(wfecha),2)
  SEEK wclave
  TFECHA=fecha
  if found()
   do while tfecha=fecha
     A=CDOW(FECHA)
     A=LEFT(A,8)
     I=LEN(A)
     N=8-I
     B=A+SPACE(N)

     ?B,fecha,origen,destino,mercancia,kilos,"Kg.",cliente
     ?"--------------------------------------------------------------------------------"
     skip
    enddo
  else

     A=CDOW(WFECHA)
     A=LEFT(A,8)
     I=LEN(A)
     N=8-I
     B=A+SPACE(N)
     ?B,wfecha
     ?"-----------------------"
  ENDIF
  use detalles  index innomdet
  SEEK wclave

    do while (wclave=conductor+str(numero,6)+str(year(fecha),4)+str(month(fecha),2)+str(day(fecha),2)).and.(.not.eof())

    if detalle="GASOIL"
     A=CDOW(FECHA)
     A=LEFT(A,8)
     I=LEN(A)
     N=8-I
     B=A+SPACE(N)
      DO CASE
         CASE LLENO
               WLLENO="LLENO"
         CASE .NOT.LLENO
               WLLENO="-----"
      ENDCASE
      ?detalle,proveedor,pais,STR(importe,10,2),moneda,litros,"Lt.",kilometros,"Km.",Wlleno
    ELSE

     A=CDOW(FECHA)
     A=LEFT(A,8)
     I=LEN(A)
     N=8-I
     B=A+SPACE(N)
     ?detalle,proveedor,pais,STR(importe,10,2),moneda
    ENDIF
      skip
    enddo



  wfecha=wfecha+1

  TESTIGO=(WFECHA<=wFECHAFIN)


enddo






?
SET ALTERNATE OFF
set century on
WAIT


return




************************
******BORRAR
**********************
PROCEDURE BORRAR

wnumero=0
store space(10) to wconductor
CLEAR
@ 5,15 SAY "BAJAS"
@ 6,15 SAY "-----"
@ 10,10 SAY "conductor " get wconductor
@ 12,10 say "numero    " get wnumero
read
use partes index innomnum,incamnum
delete all for (conductor=wconductor).and.(numero=wnumero)
use detalles index innomdet,incamdet

delete all for (conductor=wconductor).and.(numero=wnumero)
use jornada index innomjor

delete all for (conductor=wconductor).and.(numero=wnumero)
return


**********************************
********procedimiento hoja mensual
***********************************
procedure hoja
private fechaini,clave
set century off
set decimals to 0
set fixed on
use detalles
store space(10) to wcamion
mes=0
totpis=0
sumpis=0
anno=0
clear
@ 10,10 say "Camion: " get wcamion
@ 12,10 say "Mes:    " get mes
@ 14,10 SAY "Año:    " get anno
read
fechaini=ctod("01-"+str(mes,2)+"-"+str(anno,4))


SET PRINT FONT "COURIER new",6
SET PRINT ON PROMPT

?"                    ",wcamion,"         ",cmonth(FECHAINI),str(year(fechaini),4)


?"                Gas. extr.                   Gas. esp.             Gas. nave             varios     p. esp.     p. fran.     p. ital.      p. port"
?"--------------------------------------------------------------------------------------------------------------------------------------------------------------"
do while month(fechaINI)=mes

     A=CDOW(FECHAini)
     A=LEFT(A,3)
     I=LEN(A)
     N=3-I
     B=A+SPACE(N)
     sum litros for camion=wcamion.and.fecha=fechaini.and.detalle="GASOIL".AND.pais<>"ESPAÑA" TO gLITEXT

     sum importe for camion=wcamion.and.fecha=fechaini.and.detalle="GASOIL".AND.pais<>"ESPAÑA" TO gimpeXT

     sum litros for camion=wcamion.and.fecha=fechaini.and.detalle="GASOIL".AND.pais="ESPAÑA".and.(proveedor="DKV".OR.PROVEEDOR="AS24") TO gdLITEsp

     sum importe for camion=wcamion.and.fecha=fechaini.and.detalle="GASOIL".AND.pais="ESPAÑA".and.(proveedor="DKV".OR.PROVEEDOR="AS24") TO gdimpesp



     sum litros for camion=wcamion.and.fecha=fechaini.and.proveedor="NAVE" TO gnLIT

     sum importe for camion=wcamion.and.fecha=fechaini.and.proveedor="NAVE" TO gNimp

     sum importe for camion=wcamion.and.fecha=fechaini.and.(Detalle="TELEFONO".or.detalle="ACEITE".or.detalle="LAMPARA".or.detalle="TAXI") to varios

     sum importe for camion=wcamion.and.fecha=fechaini.and.detalle="AUTOPISTA".and.pais="ESPAÑA" to autoes

     sum importe for camion=wcamion.and.fecha=fechaini.and.detalle="AUTOPISTA".and.pais="FRANCIA" to autoFR

     sum importe for camion=wcamion.and.fecha=fechaini.and.detalle="AUTOPISTA".and.pais="ITALIA" to autoIT

     sum importe for camion=wcamion.and.fecha=fechaini.and.detalle="AUTOPISTA".and.pais="PORTUGAL" to autoPOR
     autofr=autofr
     autoit=autoit
     sumpis=autoes+autofr+autoit+autopor
     ?B,STR(DAY(FECHAINI),2),GLITEXT,GIMPEXT,GDLITESP,GDIMPESP,GNLIT,GNIMP,VARIOS,AUTOES,AUTOFR,AUTOIT,AUTOPOR
     IF DOW(FECHAINI)=1
       ?"---------------------------------------------------------------------------------------------------------------------------------------------------------------------"
     ENDIF
     FECHAINI=FECHAINI+1
     totpis=totpis+sumpis
ENDDO
sum litros for camion=wcamion.and.month(fecha)=mes.AND.YEAR(FECHA)=ANNO.and.detalle="GASOIL" to totlit

sum importe for camion=wcamion.and.month(fecha)=mes.AND.YEAR(FECHA)=ANNO.and.detalle="GASOIL".AND.PAIS="ESPAÑA".AND.(PROVEEDOR="DKV".OR.PROVEEDOR="AS24") to DETIVA
DETIVA=DETIVA/100*16

sum importe for camion=wcamion.and.month(fecha)=mes.AND.YEAR(FECHA)=ANNO.and.detalle="GASOIL" to totimp
sum importe for camion=wcamion.and.month(fecha)=mes.AND.YEAR(FECHA)=ANNO.and.(Detalle="TELEFONO".or.detalle="ACEITE".or.detalle="LAMPARA".or.detalle="TAXI") to totvar
?"Total litros: ",STR(totlit,10,2)
?"Total gasoil: ",STR(totimp-DETIVA,10,2)
?"Total varios: ",STR(totvar,10,2)
?"Total pistas: ",STR(totpis,10,2)
SET PRINT OFF
SET PRINTER TO
SET PRINT FONT "COURIER new",9
set century on
WAIT
return

**************************************+
**************************************
******PROCEDURE RUEDA
*****************************************
PROCEDURE RUEDA

USE DETALLES INDEX INNOMDET,INCAMDET
WMES=0
WANNO=2000
WPRECGN=0
COMDKV=0.01
COMCAPLIS=0.01
WSERVISA=0
WATRAITA=0
WMATRICULA="          "
CLEAR
@ 10,10 SAY "RESUMEN MENSUAL JULIO RUEDA"

@ 14,10 SAY "MES                            " GET WMES
@ 15,10 SAY "AÑO                            " GET WANNO
@ 16,10 SAY "PRECIO GASOIL NAVE             " GET WPRECGN PICTURE "99,999.999"
@ 17,10 SAY "COMISION DKV PISTAS FRANCESAS  " GET COMDKV
@ 19,10 SAY "MATRICULA                      " GET WMATRICULA
@ 20,10 SAY "PISTAS SERVISA                 " GET WSERVISA PICTURE "99,999.99"
@ 21,10 SAY "PISTAS ITALIANAS               " GET WATRAITA PICTURE "99,999.99"


READ

RESP="S"

CLEAR
@ 1,1 SAY "¿IMPRIMIR?" GET RESP
READ
IF RESP<>"N"

SET PRINT ON PROMPT
SET PRINT FONT "COURIER new",9
ENDIF

*********AUTOPISTAS
*****************
********************

**** SUMA AUTOPISTAS ESPAÑOLAS


SUM IMPORTE FOR DETALLE="AUTOPISTA".AND.PAIS="ESPAÑA".AND.PROVEEDOR="DKV".AND.MONTH(FECHA)=WMES.AND.YEAR(FECHA)=WANNO.AND.CAMION=WMATRICULA TO AESP

SUM IMPORTE FOR DETALLE="AUTOPISTA".AND.PAIS="ESPAÑA".AND.PROVEEDOR="SERVISA".AND.MONTH(FECHA)=WMES.AND.YEAR(FECHA)=WANNO.AND.CAMION=WMATRICULA TO ASERESP

**** SUMA AUTOPISTAS FRANCESAS

SUM IMPORTE FOR DETALLE="AUTOPISTA".AND.PAIS="FRANCIA".AND.PROVEEDOR="DKV".AND.MONTH(FECHA)=WMES.AND.YEAR(FECHA)=WANNO.AND.CAMION=WMATRICULA TO AFR
TAFR=AFR+AFR/100*COMDKV
*TAFR=AFR
ETAFR=TAFR

SUM IMPORTE FOR DETALLE="AUTOPISTA".AND.PAIS="FRANCIA".AND.PROVEEDOR="CAPLIS".AND.MONTH(FECHA)=WMES.AND.YEAR(FECHA)=WANNO.AND.CAMION=WMATRICULA TO ACFR
TACFR=ACFR+ACFR/100*COMCAPLIS
ETACFR=TACFR


*** SUMA AUTOPISTAS ITALIANAS

SUM IMPORTE FOR DETALLE="AUTOPISTA".AND.PAIS="ITALIA".AND.MONTH(FECHA)=WMES.AND.YEAR(FECHA)=WANNO.AND.CAMION=WMATRICULA TO AIT
EAIT=AIT


*** GASOIL
******************
*****************


****** SUMA GASOIL NAVE

SUM LITROS FOR DETALLE="GASOIL".AND.PROVEEDOR="NAVE".AND.MONTH(FECHA)=WMES.AND.YEAR(FECHA)=WANNO.AND.CAMION=WMATRICULA TO GNAV
TGNAV=GNAV*WPRECGN


***** SUMA GASOIL DKV

SUM LITROS FOR DETALLE="GASOIL".AND.PROVEEDOR="DKV".AND.MONTH(FECHA)=WMES.AND.YEAR(FECHA)=WANNO.AND.CAMION=WMATRICULA TO GDKV
SUM IMPORTE FOR DETALLE="GASOIL".AND.PROVEEDOR="DKV".AND.MONTH(FECHA)=WMES.AND.YEAR(FECHA)=WANNO.AND.CAMION=WMATRICULA TO PGDKV


******* SUMA GASOIL AS24

SUM LITROS FOR DETALLE="GASOIL".AND.PROVEEDOR="AS24".AND.MONTH(FECHA)=WMES.AND.YEAR(FECHA)=WANNO.AND.CAMION=WMATRICULA TO GAS24
SUM IMPORTE FOR DETALLE="GASOIL".AND.PROVEEDOR="AS24".AND.MONTH(FECHA)=WMES.AND.YEAR(FECHA)=WANNO.AND.CAMION=WMATRICULA TO PGAS24


*******IMPRESION
SET DECIMALS TO 2
SET FIXED ON
?"               GASTOS JULIO RUEDA  ",WMATRICULA,CMONTH(CTOD("01-"+STR(WMES,2)+"-"+STR(WANNO,4))),STR(WANNO,4)
?"              =============================================================="
?

?
AESP=AESP+ASERESP
?"SUMA AUTOPISTAS ESPAÑOLAS     ",STR(AESP,10,2),"EUR"
?
AFR=AFR+ACFR
?"SUMA AUTOPISTAS FRANCESAS     ",STR(AFR,10,2),"EUR"
ETAFR=ETACFR+ETAFR
?
?"AUTOPISTAS ITALIANAS          ",STR(WATRAITA,10,2),"EUR"
?


?"SUMA AUTOPISTAS SERVISA       ",STR(WSERVISA,10,2),"EUR"
?

?"LISTADO GASOIL NAVE"
LIST LITROS,FECHA FOR DETALLE="GASOIL".AND.PROVEEDOR="NAVE".AND.MONTH(FECHA)=WMES.AND.YEAR(FECHA)=WANNO.AND.CAMION=WMATRICULA OFF
?"SUMA LITROS GASOIL NAVE       ",GNAV
?"PRECIO/LITRO                  ",STR(WPRECGN,10,3),"EUR"
?"TOTAL EUR                     ",STR(TGNAV,10,2),"EUR"
?




?"LISTADO GASOIL DKV"



LIST LITROS,PAIS,IMPORTE,FECHA FOR DETALLE="GASOIL".AND.PROVEEDOR="DKV".AND.MONTH(FECHA)=WMES.AND.YEAR(FECHA)=WANNO.AND.CAMION=WMATRICULA OFF


?"  TOTAL LITROS                ",GDKV
?"  TOTAL EUR                   ",STR(PGDKV,10,2),"EUR"
?

?

?"LISTADO GASOIL AS24"
list litros,pais,IMPORTE,FECHA FOR DETALLE="GASOIL".AND.PROVEEDOR="AS24".AND.MONTH(FECHA)=WMES.AND.YEAR(FECHA)=WANNO.AND.CAMION=WMATRICULA OFF


?"  TOTAL LITROS                ",GAS24
?"  TOTAL EUR                   ",STR(PGAS24,10,2),"EUR"
?
?


FA=AESP+ETAFR+EAIT+WATRAITA+WSERVISA
?"TOTAL AUTOPISTAS              ",STR(FA,10,2),"EUR"
FG=TGNAV+PGDKV+PGAS24
?"TOTAL GASOIL                  ",STR(FG,10,2),"EUR"
?
?"GASTO TOTAL------------------ ",STR(FA+FG,10,2),"EUR"
SET FIXED OFF
SET DECIMALS TO 2
SET PRINT OFF
set printer to
close ALL
USE
WAIT
RETURN
