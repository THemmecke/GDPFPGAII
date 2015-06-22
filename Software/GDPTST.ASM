
**************************** MACRO-BIBLIOTHEK ******************************
* aufruf der Macros im Programm mit .<NAME> p1 p2 ...

MACRO gotoxy            * springt an X,Y Position
 movem.l a0-a6/d0-d7,-(a7)
 move.b |0,d1           * d1=X (0..79)
 move.b |1,d2           * d2=Y (0..23)
 moveq #!SETCURXY,D7
 trap #1
 movem.l (a7)+,a0-a6/d0-d7
 rts
ENDMACRO

MACRO writexy                   * gibt einen text an x/y position aus
                                * TextGrüsse,X,Y,TextAddr (Nullterminiert)
        movem.l a0-a6/d0-d7,-(a7)
        move.b #|0,d0
        move.w #|1,d1
        move.w #|2,d2
        lea |3,a0
        moveq #!WRITE,d7
        trap #1
        movem.l (a7)+,a0-a6/d0-d7
ENDMACRO


MACRO writeln                   * TextAddr (Nullterminiert)
        movem.l a0-a6/d0-d7,-(a7)
        lea |0,a0
LOOP||:
        move.b (a0)+,d0
        beq.s LOOPE||
        moveq #!CO2,d7
        trap #1
        bra.s LOOP||
LOOPE||:
        movem.l (a7)+,a0-a6/d0-d7
ENDMACRO

MACRO clrscr                    * loescht den Bildschirm
        movem.l a0-a6/d0-d7,-(a7)
        moveq #!CLRSCREEN,d7
        trap #1
        movem.l (a7)+,a0-a6/d0-d7
ENDMACRO

MACRO crlf                      * gibt CR-LF aus
        movem.l a0-a6/d0-d7,-(a7)
        moveq #!CRLF,d7
        trap #1
        movem.l (a7)+,a0-a6/d0-d7
ENDMACRO


MACRO prthex2                   * gibt 2stellige HEX Zahl aus
                                * IN: WERT
        movem.l a0-a6/d0-d7,-(a7)
        move.l |0,d0
        lea buffer(pc),a0
        moveq #!PRINT2X,d7
        trap #1
        movem.l (a7)+,a0-a6/d0-d7
ENDMACRO


MACRO prthex8                   * gibt 8stellige Hex Zahl aus
                                * In: WERT
        movem.l a0-a6/d0-d7,-(a7)
        move.l |0,d0
        lea buffer(pc),a0
        moveq #!PRINT8X,d7
        trap #1
        movem.l (a7)+,a0-a6/d0-d7
ENDMACRO

MACRO waitcr                    * Wartet auf CR (Enter)
        movem.l a0-a6/d0-d7,-(a7)
wait02||:
        moveq #!CI,d7
        trap #1
        cmp.b #$d,d0
        bne.s wait02||
        movem.l (a7)+,a0-a6/d0-d7
ENDMACRO

MACRO setflip       * fliprate setzen
                    * In: 2pageflip,4pageflip
     movem.l d0-d1/d7,-(a7)
     move.b #|0,d0
     move.b #|1,d1
     moveq #!SETFLIP,d7
     trap #1
     movem.l (a7)+,d0-d1/d7
ENDMACRO

MACRO gdpwait        * Warte auf GDP Ready
wait00||:
        btst #2,gdpstatus.w
        beq wait00||
ENDMACRO

MACRO gdpcmd    * Kommando an GDP schicken
                * In: cmd
        .gdpwait
        move.b #|0,gdpcmd.w
ENDMACRO

MACRO gdpclr    * Seite der GDP Lüschen
                * In: Seite (0..3)
        movem.l d0,-(a7)
        moveq #|0,d0
        lsl.b #6,d0
        and #$f0,d0
        move.b d0,gdppage.w
        .gdpcmd 6
        movem.l (a7)+,d0
ENDMACRO

MACRO gdppage       * Schreib-/Lese-Seite wûhlen
                    * In: schreib,lese,xor-mode
        movem.l d0-d1,-(a7)
        clr.l d0
        move.b #|0,d0
        lsl #6,d0
        move.b #|1,d1
        lsl #4,d1
        or d1,d0
        or #|2,d0
        move.b d0,gdppage.w
        movem.l (a7)+,d0-d1
ENDMACRO


MACRO newpage       * Schreib-/Lese-Seite wûhlen
                    * In: schreib,lese
        movem.l d0-d1,-(a7)
        clr.l d0
        clr.l d1
        move.b #|0,d0
        move.b #|1,d1
        moveq #!NEWPAGE,d7
        trap #1
        movem.l (a7)+,d0-d1
ENDMACRO

MACRO gdpcolor    * Vor-/Hintergrundfarbe der GDP setzen
                  * In: foreground,background
        .gdpwait
        move.b #|0,gdpfgc.w
        move.b #|1,gdpbgc.w

ENDMACRO

MACRO gdpputch    * character an aktueller position ausgeben
        .gdpwait
        move.b #|0,gdpcmd.w
ENDMACRO



******************** Konstanten ***********

cpu equ 4

membase         equ $00800000*cpu
memsize         equ $3FFFF
pagesize        equ $ffff

color0          equ $77  * Zyan
color1          equ $33  * Grýn
color2          equ $44  * Rot
color3          equ $55  * Blau

gdpbase         equ $ffffff70*cpu
gdpstatus       equ $ffffff70*cpu
gdpcmd          equ $ffffff70*cpu
gdpctrl1        equ $ffffff71*cpu
gdpctrl2        equ $ffffff72*cpu
gdpcsize        equ $ffffff73*cpu
gdpdx           equ $ffffff75*cpu

gdppage         equ $ffffff60*cpu

gdpfgc          equ $ffffffa0*cpu
gdpbgc          equ $ffffffa1*cpu

tctrl           equ $fffffff4*cpu


******************** START ***************


        jmp start(pc)

start:

        .setflip 0,0
        moveq #!CUROFF,d7
        trap #1

        **** PAGE 0 *******
        movea.l #membase,a0           * Start bei page 0
        move.l  #pagesize,d1           * Anzahl Speicherzellen
        move.l d1,d2                    * doppelte Schleife aufbauen
        swap d2

loop00:
        move.b #color0,(a0)+                 * Byte schreiben
        dbra d1,loop00
        dbra d2,loop00

        **** PAGE 1 *******
        movea.l #membase+pagesize+1,a0           * Start bei page1

        move.l  #pagesize,d1           * Anzahl Speicherzellen
        move.l d1,d2                    * doppelte Schleife aufbauen
        swap d2

loop01:
        move.b #color1,(a0)+                 * Byte schreiben
        dbra d1,loop01
        dbra d2,loop01


        **** PAGE 2 *******
        movea.l #membase+pagesize*2+2,a0           * Start bei Page 2

        move.l  #pagesize,d1           * Anzahl Speicherzellen
        move.l d1,d2                    * doppelte Schleife aufbauen
        swap d2

loop02:
        move.b #color2,(a0)+                 * Byte schreiben
        dbra d1,loop02
        dbra d2,loop02

        **** PAGE 3 *******
        movea.l #membase+pagesize*3+3,a0           * Start bei Page 3

        move.l  #pagesize,d1           * Anzahl Speicherzellen
        move.l d1,d2                    * doppelte Schleife aufbauen
        swap d2

loop03:
        move.b #color3,(a0)+                 * Byte schreiben
        dbra d1,loop03
        dbra d2,loop03


        .setflip 0,20
        .waitcr
        .setflip 20,0
        .clrscr
        moveq #!CURON,d7
        trap #1
end:
        rts
