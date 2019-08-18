/*
 * Hbedit Diff view.
 *
 * Copyright 2019 Alexander S.Kresin <alex@kresin.ru>
 * www - http://www.kresin.ru
 */

#include "inkey.ch"
#include "hbclass.ch"

#define ALT_PRESSED   0x040000

#define HILIGHT_KEYW1   1
#define HILIGHT_KEYW2   2
#define HILIGHT_KEYW3   3
#define HILIGHT_KEYW4   4
#define HILIGHT_QUOTE   5
#define HILIGHT_SCOMM   6
#define HILIGHT_SLEFT   7
#define HILIGHT_MCOMM   8
#define HILIGHT_BLOCK   9

STATIC lCurr, cParent, nDiffs
STATIC nDiffMode                   // 1 - diff, 2 - full, 3 - prev. file version
STATIC aShort, aDiff, aFull, aFrom

STATIC FUNCTION _DiffTool()

   LOCAL cFileRes := hb_DirTemp() + "hbedit.out", cBuff
   STATIC nDiffTool := 0

   IF nDiffTool == 0
#ifndef __PLATFORM__UNIX
      IF File( hb_dirBase() + "diff.exe" )
         RETURN (nDiffTool := 1)
      ENDIF
#endif
      cedi_RunConsoleApp( 'diff -v', cFileRes )
      IF !Empty( cBuff := MemoRead( cFileRes ) ) .AND. Lower( Left( cBuff,4 ) ) == "diff"
         RETURN (nDiffTool := 2)
      ENDIF

      cedi_RunConsoleApp( 'git --version', cFileRes )
      IF !Empty( cBuff := MemoRead( cFileRes ) ) .AND. Lower( Left( cBuff,3 ) ) == "git"
         RETURN (nDiffTool := 3)
      ENDIF

   ENDIF

   RETURN 0

FUNCTION edi_MakeDiff( oEdit, cFileName )

   LOCAL nDiffTool := _DiffTool()
   LOCAL cFileRes := hb_DirTemp() + "hbedit.out"

   IF nDiffTool > 0
      IF nDiffTool == 1
         cedi_RunConsoleApp( hb_dirBase() + 'diff -u ' + cFileName + " " + oEdit:cFileName, cFileRes )
      ELSEIF nDiffTool == 2
         cedi_RunConsoleApp( 'diff -u ' + cFileName + " " + oEdit:cFileName, cFileRes )
      ELSEIF nDiffTool == 3
         cedi_RunConsoleApp( 'git diff ' + cFileName + " " + oEdit:cFileName, cFileRes )
      ENDIF
      RETURN MemoRead( cFileRes )
   ENDIF

   RETURN Nil

FUNCTION edi_AddDiff( oEdit, cText, lCurrEd )

   LOCAL cAddw := "$Diff", nPos, o, i, nPos2, c, nminus := 0, n
   LOCAL bStartEdit := {|o|
      _diff_About( o )
      o:bStartEdit := Nil
      RETURN Nil
   }

   cParent := oEdit:cFileName
   lCurr   := lCurrEd
   nDiffs  := 0
   aFull := aDiff := aFrom := Nil

   IF ( nPos := Ascan( oEdit:aWindows, {|o|o:cFileName==cAddW} ) ) > 0
      o := oEdit:aWindows[nPos]
      o:SetText( cBuff, cAddW )
      mnu_ToBuf( oEdit, nPos )
   ELSE
      o := edi_AddWindow( oEdit, cText, cAddW, 3, Int(MaxCol()/2), oEdit:cp )
      o:lReadOnly := .T.
      o:nMode := 1
      o:bOnKey := {|o,n|_diff_onkey( o,n )}
      o:Highlighter( HiliDiff():New() )
      o:oHili:hHili := hb_Hash()
      o:oHili:hHili['colors'] := TEdit():aHiliAttrs
      o:oHili:hHili['bra'] := .F.
      o:bStartEdit := bStartEdit
   ENDIF
   aShort := {}
   FOR i := 1 TO Len( o:aText )
      IF Right( o:aText[i],1 ) == Chr(13 )
         o:aText[i] := Left( o:aText[i], Len(o:aText[i])-1 )
      ENDIF
      IF ( c := Left( o:aText[i], 1 ) ) == '@'
         nDiffs ++
         IF ( nPos := At( '+', o:aText[i] ) ) > 0 .AND. ( nPos2 := At( '-', o:aText[i] ) ) > 0
            n := Val( Substr( o:aText[i], nPos+1 ) )
            Aadd( aShort, { i, n + nminus, Val( Substr( o:aText[i], nPos2+1 ) ), n } )
         ENDIF
      ELSEIF c == '-'
         nminus ++
      ENDIF
   NEXT
   nDiffMode := 1

   RETURN Nil

FUNCTION _diff_onkey( oEdit, nKeyExt )

   LOCAL nKey := hb_keyStd(nKeyExt), n, nminus, c, nPos, i

   IF nKey == 78 .OR. nKey == 110     // n,N
      n := oEdit:nLine
      IF ( i := Ascan( aShort, {|a|a[nDiffMode]>n} ) ) > 0
         oEdit:GoTo( aShort[i,nDiffMode]+1, 1 )
      ENDIF
      RETURN -1

   ELSEIF nKey == 66 .OR. nKey == 98  // b,B
      n := oEdit:nLine - 1
      IF ( i := Ascan( aShort, {|a|a[nDiffMode]>n} ) ) > 2
         oEdit:GoTo( aShort[i-2,nDiffMode]+1, 1 )
      ELSEIF i == 0
         IF ( i := aShort[Len(aShort),nDiffMode] ) < n - 3
            oEdit:GoTo( i+1, 1 )
         ELSE
            oEdit:GoTo( aShort[Len(aShort)-1,nDiffMode]+1, 1 )
         ENDIF
      ENDIF
      RETURN -1

   ELSEIF nKey == 83 .OR. nKey == 115 // s,S
      _diff_Switch( oEdit )
      RETURN -1
   ELSEIF nKey == K_ENTER
      IF lCurr
         n := oEdit:nLine
         IF nDiffMode == 1
            nminus := 0
            DO WHILE n >= 1
              IF ( c := Left( oEdit:aText[n],1 ) ) == '-'
                 nMinus ++
              ELSEIF c == '@' .AND. Left( oEdit:aText[n],2 ) == '@@'
                 nPos := 1
                 IF ( nPos := At( '+', oEdit:aText[n] ) ) > 0
                    nPos := Val( Substr( oEdit:aText[n], nPos+1 ) )
                    nPos := oEdit:nLine - n - nminus + nPos - 1
                 ENDIF
                 EXIT
              ENDIF
              n --
            ENDDO
         ELSEIF nDiffMode == 2
            i := Ascan( aShort, {|a|a[4]>n} )
            nPos := Iif( i == 1, 1, Iif( i == 0, ATail(aShort)[4], aShort[i-1,4] ) )
            n := Iif( i == 1, 1, Iif( i == 0, ATail(aShort)[2], aShort[i-1,2] ) ) - 1
            edi_writelog( str(oEdit:nLine) + " " + str(nPos) + " " + str(n) )
            DO WHILE ++n < oEdit:nLine
               IF Left( oEdit:aText[n],1 ) != '-'
                  nPos ++
               ENDIF
            ENDDO
         ELSEIF nDiffMode == 3
            i := Ascan( aShort, {|a|a[4]>n} )
            nPos := Iif( i == 1, 1, Iif( i == 0, ATail(aShort)[4], aShort[i-1,4] ) )
            n := Iif( i == 1, 1, Iif( i == 0, ATail(aShort)[3], aShort[i-1,3] ) ) - 1
            edi_writelog( str(oEdit:nLine) + " " + str(nPos) + " " + str(n) )
            DO WHILE ++n < oEdit:nLine
               IF Left( oEdit:aText[n],1 ) != '-'
                  nPos ++
               ENDIF
            ENDDO
         ENDIF
         IF ( i := Ascan( oEdit:aWindows, {|o|o:cFileName==cParent} ) ) > 0
            oEdit:aWindows[i]:GoTo( nPos, 1 )
            mnu_ToBuf( oEdit, oEdit:aWindows[i] )
         ENDIF
      ENDIF
      RETURN -1
   ELSEIF nKey == K_F1
      _diff_About( oEdit )
      RETURN -1
   ENDIF

   RETURN 0

STATIC FUNCTION _diff_Switch( oEdit )

   LOCAL aMenu := { "Diff only", "Full", "Version 'From'" }, i
   LOCAL i1, i2, nminus := 0, nplus := 0, c
   LOCAL nPos, nLine1, nLineNew, aTextBase

   IF ( i := FMenu( oEdit, aMenu, oEdit:y1+3, oEdit:x1+6 ) ) > 0 .AND. nDiffMode != i
      IF nDiffMode == 1
         aDiff := oEdit:aText
      ENDIF
      IF i == 1
         oEdit:aText := aDiff
         oEdit:Goto( 1, 1 )

      ELSEIF i == 2
         IF Empty( aFull )
            i1 := 0
            DO WHILE Left( aDiff[++i1], 1 ) != '@';  ENDDO
            DO WHILE ++i1 <= Len( aDiff )
               IF Left( aDiff[i1], 1 ) == '-'
                  nminus ++
               ENDIF
            ENDDO
            aTextBase := oEdit:oParent:aText
            aFull := Array( Len(aTextBase) + nminus )
            nLine1 := nLineNew := 1
            i1 := 0
            DO WHILE Left( aDiff[++i1], 1 ) != '@';  ENDDO
            DO WHILE i1 < Len( aDiff ) .AND. nLineNew < Len( aFull )
               IF ( c := Left( aDiff[i1], 1 ) ) == '@'
                  IF ( nPos := At( '+', aDiff[i1] ) ) > 0
                    nPos := Val( Substr( aDiff[i1], nPos+1 ) )
                    FOR i2 := nLine1 TO nPos-1
                       aFull[nLineNew] := aTextBase[i2]
                       nLineNew ++
                    NEXT
                    nLine1 := nPos
                  ENDIF
               ELSE
                  aFull[nLineNew] := aDiff[i1]
                  nLineNew ++
                  IF c != '-'
                     nLine1 ++
                  ENDIF
               ENDIF
               i1 ++
            ENDDO
            i1 := nLine1
            DO WHILE nLine1 <= Len( aTextBase ) .AND. nLineNew <= Len( aFull )
               aFull[nLineNew++] := aTextBase[nLine1++]
            ENDDO
            AFill( aFull, "", nLineNew )
         ENDIF
         oEdit:aText := aFull

      ELSEIF i == 3
         IF Empty( aFrom )
            i1 := 0
            DO WHILE Left( aDiff[++i1], 1 ) != '@';  ENDDO
            DO WHILE ++i1 <= Len( aDiff )
               IF Left( aDiff[i1], 1 ) == '-'
                  nminus ++
               ELSEIF c == '+'
                  nplus ++
               ENDIF
            ENDDO
            aTextBase := oEdit:oParent:aText
            aFrom := Array( Len(aTextBase) + nminus - nplus )
            nLine1 := nLineNew := 1
            i1 := 0
            DO WHILE Left( aDiff[++i1], 1 ) != '@';  ENDDO
            DO WHILE i1 < Len( aDiff ) .AND. nLineNew < Len( aFrom )
               IF ( c := Left( aDiff[i1], 1 ) ) == '@'
                  IF ( nPos := At( '+', aDiff[i1] ) ) > 0
                    nPos := Val( Substr( aDiff[i1], nPos+1 ) )
                    FOR i2 := nLine1 TO nPos-1
                       aFrom[nLineNew] := aTextBase[i2]
                       nLineNew ++
                    NEXT
                    nLine1 := nPos
                  ENDIF
               ELSEIF c == '+'
                  //nLine1 ++
               ELSE
                  aFrom[nLineNew] := aDiff[i1]
                  nLineNew ++
                  //IF c != '-'
                     nLine1 ++
                  //ENDIF
               ENDIF
               i1 ++
            ENDDO
            i1 := nLine1
            DO WHILE nLine1 <= Len( aTextBase ) .AND. nLineNew <= Len( aFrom )
               aFrom[nLineNew++] := aTextBase[nLine1++]
            ENDDO
            AFill( aFrom, "", nLineNew )
         ENDIF
         oEdit:aText := aFrom

      ENDIF
      nDiffMode := i
      oEdit:TextOut()
   ENDIF

   RETURN Nil

STATIC FUNCTION _diff_About( oEdit )

   LOCAL nw := 28, x1 := oEdit:x1 + Int( (oEdit:x2 - oEdit:x1 - nw)/2 ), y2 := 18
   LOCAL oldc := SetColor( oEdit:cColorSel )
   LOCAL cBufScr := Savescreen( 09, x1, y2, x1+nw )
   LOCAL nRow := Row(), nCol := Col()

   @ 09, x1, y2, x1+nw BOX "         "
   @ 10, x1+2 SAY "Differences: " + Ltrim( Str( nDiffs ) )
   @ 11, x1+4 SAY "N - Next"
   @ 12, x1+4 SAY "B - Previous"
   @ 13, x1+4 SAY "S - Switch view mode"
   @ 14, x1+4 SAY "Enter - GoTo text"
   @ 15, x1+4 SAY "F1 - this help screen"
   @ y2-1, x1+2 SAY "Press any key..."
   Inkey( 0 )
   Restscreen( 09, x1, y2, x1+nw, cBufScr )
   SetColor( oldc )
   DevPos( nRow, nCol )

   RETURN Nil

CLASS HiliDiff INHERIT Hili

   METHOD DO( nLine )

ENDCLASS

METHOD DO( nLine ) CLASS HiliDiff

   LOCAL c, nHili := 0

   ::nItems := 0
   IF ( c := Left( ::oEdit:aText[nLine], 1 ) ) == '@'
      nHili := HILIGHT_SCOMM
   ELSEIF c == '-'
      nHili := HILIGHT_SLEFT
   ELSEIF c == '+'
      nHili := HILIGHT_KEYW1
   ENDIF

   IF nHili > 0
      ::AddItem( 1, cp_Len( ::oEdit:lUtf8, ::oEdit:aText[nLine] ), nHili )
   ENDIF

   RETURN Nil
