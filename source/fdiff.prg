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

STATIC lCurr, cParent

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

   LOCAL cAddw := "$Diff", nPos, o, i, id := 0

   cParent := oEdit:cFileName
   lCurr   := lCurrEd

   IF ( nPos := Ascan( oEdit:aWindows, {|o|o:cFileName==cAddW} ) ) > 0
      o := oEdit:aWindows[nPos]
      o:SetText( cBuff, cAddW )
      mnu_ToBuf( oEdit, nPos )
   ELSE
      o := edi_AddWindow( oEdit, cText, cAddW, 3, Int(MaxCol()/2) )
      o:lReadOnly := .T.
      o:bOnKey := {|o,n|_diff_onkey( o,n )}
      o:Highlighter( HiliDiff():New() )
      o:oHili:hHili := hb_Hash()
      o:oHili:hHili['colors'] := TEdit():aHiliAttrs
      o:oHili:hHili['bra'] := .F.
   ENDIF
   FOR i := Len( o:aText ) TO 1 STEP -1
      IF Right( o:aText[i],1 ) == Chr(13 )
         o:aText[i] := Left( o:aText[i], Len(o:aText[i])-1 )
      ENDIF
      IF Left( o:aText[i], 1 ) == '@'
         id ++
      ENDIF
   NEXT
   //edi_Alert( "Differences: " + Ltrim(Str(id)) + ";" + " Alt-N - Next;Alt-B - Previous;Enter - GoTo text" )

   RETURN Nil

FUNCTION _diff_onkey( oEdit, nKeyExt )

   LOCAL nKey := hb_keyStd(nKeyExt), n, nminus, c, nPos, i

   IF hb_BitAnd( nKeyExt, ALT_PRESSED ) != 0
      IF nKey == K_ALT_N
         n := oEdit:nLine
         DO WHILE ++n < Len( oEdit:aText )
           IF Left( oEdit:aText[n],1 ) == '@'
              oEdit:GoTo( n+1, 1 )
              EXIT
           ENDIF
         ENDDO
         RETURN -1
      ELSEIF nKey == K_ALT_B
         n := oEdit:nLine - 1
         DO WHILE --n > 1
           IF Left( oEdit:aText[n],1 ) == '@'
              oEdit:GoTo( n+1, 1 )
              EXIT
           ENDIF
         ENDDO
         RETURN -1
      ENDIF
   ELSEIF nKey == K_ENTER
      IF lCurr
         n := oEdit:nLine
         nminus := 0
         DO WHILE n >= 1
           IF ( c := Left( oEdit:aText[n],1 ) ) == '-'
              nMinus ++
           ELSEIF c == '@' .AND. Left( oEdit:aText[n],2 ) == '@@'
              IF ( nPos := At( '+', oEdit:aText[n] ) ) > 0
                 nPos := Val( Substr( oEdit:aText[n], nPos+1 ) )
                 IF ( i := Ascan( oEdit:aWindows, {|o|o:cFileName==cParent} ) ) > 0
                    oEdit:aWindows[i]:GoTo( oEdit:nLine - n - nminus + nPos - 1, 1 )
                    mnu_ToBuf( oEdit, oEdit:aWindows[i] )
                 ENDIF
              ENDIF
              EXIT
           ENDIF
           n --
         ENDDO
      ENDIF
      RETURN -1
   ENDIF

   RETURN 0

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
