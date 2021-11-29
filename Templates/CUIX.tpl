#TEMPLATE(ClarionUIX,'Clarion UIX'),FAMILY('ABC'),FAMILY('CW20')
#!Copyright RAD
#!MIT License
#EXTENSION(ClarionUIX,'Clarion UIX Global Template'),APPLICATION,LAST
#!
#SHEET
#TAB('Clarion UIX Global Template B0.9')
#DISPLAY('This template will transform your Clarion App')
#DISPLAY('And will add theme packs support')
#DISPLAY('In Clarion 10.x.')
#PROMPT('H5 is present',CHECK),%H5Present,at(10),DEFAULT(%False)
#ENDTAB
#ENDSHEET
#! ----------------------------------------------------------------
#AT(%AfterGlobalIncludes)
  INCLUDE('EntryFields.inc'),ONCE
  INCLUDE('CoolButtonz.inc'),ONCE
#ENDAT
#! ----------------------------------------------------------------
#AT(%ProgramProcedures)
#ENDAT
#!----------------------------------------------------------------
#EXTENSION(ClarionUIXCreator,'UIX Theme Support'),PROCEDURE, LAST
#PROMPT('Theme should be EVENT:Accepted aware',CHECK),%EventAcceptedAware,at(10),DEFAULT(%True)
#PROMPT('Theme should be EVENT:Selected aware',CHECK),%EventSelectedAware,at(11),DEFAULT(%False)
#SHEET
#TAB('Wallpapers')
#BOXED('')
#PROMPT('Use Theme Defined Wallpaper',OPTION),%ThemeDefinedWallpaper,REQ,DEFAULT('Yes')
#PROMPT('Yes',RADIO)
#PROMPT('Nope I want to use my own wallpaper',RADIO)
#PROMPT('Position',OPTION),%PositionWallpaper,REQ,DEFAULT('Centered')
#PROMPT('Centered',RADIO)
#PROMPT('Tiled',RADIO)
#ENDBOXED
#ENDTAB
#TAB('Entry fields')
#DISPLAY('Entry Fields UIX Support')
#BOXED('')
#PROMPT('Transform',OPTION),%WhatToTransform,REQ,DEFAULT('All Entry Fields')
#PROMPT('All Entry Fields',RADIO)
#PROMPT('Selected Fields',RADIO)
#BUTTON ('Entry Fields'), MULTI(%EntresControls, %IMGControl), INLINE
#PROMPT('Entry Field:',CONTROL),%IMGControl
#ENDBUTTON
#ENDBOXED
#ENDTAB
#TAB('Buttons')
#DISPLAY('Button UIX Support')
#BOXED('')
#PROMPT('Transform',OPTION),%WhatButtonsToTransform,REQ,DEFAULT('All Buttons')
#PROMPT('All Buttons',RADIO)
#PROMPT('Selected Buttons',RADIO)
#BUTTON ('Buttons'), MULTI(%TilesControls, %BTNControl), INLINE
#PROMPT('Buttons:',CONTROL),%BTNControl
#ENDBUTTON
#ENDBOXED
#ENDTAB
#ENDSHEET
#! ----------------------------------------------------------------
#ATSTART
#DECLARE(%EntryIMGControlNumber)
#DECLARE(%EntryIMGVar)
#DECLARE(%IMGControlNumber)
#DECLARE(%IMGVar)
#ENDAT
#! ----------------------------------------------------------------
#AT(%WindowManagerMethodCodeSection,'Init','(),BYTE'),PRIORITY(9000)
	CoolEntryFields.Init(SELF)
	DO InitEntries_ROUTINE
	Buttonz.Init(SELF)
	DO InitButtonz_ROUTINE
#CASE(%ThemeDefinedWallpaper)
#OF('Yes')
	DO SetThemeWallpaper_ROUTINE
#ENDCASE
#ENDAT
#! ----------------------------------------------------------------
#AT(%WindowManagerMethodCodeSection,'TakeEvent','(),BYTE'),PRIORITY(6300)
	!Something was triggered on the screen, Either EVENT:Accepted or EVENT:Selected
	CASE EVENT()
#IF(%EventAcceptedAware)
	OF EVENT:Accepted
		DO EntriesPosition_ROUTINE
		DO RebuildButtonz_ROUTINE
#ENDIF
#IF(%EventSelectedAware)
	OF EVENT:Selected
		DO EntriesPosition_ROUTINE
		DO RebuildButtonz_ROUTINE
#ENDIF
	END
#ENDAT
#! ----------------------------------------------------------------
#AT (%DataSection)
CoolEntryFields EntryFields
Buttonz CoolButtonz
#ENDAT
#! ----------------------------------------------------------------
#AT(%ResizerMethodCodeSection,, 'WindowResizeEnd',),PRIORITY(7500)
	DO EntriesPosition_ROUTINE
	DO ResizeButtons_ROUTINE
#ENDAT
#! ----------------------------------------------------------------
#AT(%ProcedureRoutines)
InitEntries_ROUTINE    ROUTINE                !Pass Entry Fields to the Entries Class
#CASE(%WhatToTransform)
#OF('All Entry Fields')
	LOOP i#  = FirstField() TO LastField()
  		IF i#{Prop:Type} = Create:Entry OR i#{Prop:Type} = Create:TEXT
    		CoolEntryFields.AddEntryBG(i#,i#{PROP:Color},,GLO:2in1)
  		END
	END !LOOP
#OF('Selected Fields')
#FOR(%EntresControls)
	    	CoolEntryFields.AddEntryBG(%IMGControl,%IMGControl{PROP:Color},,GLO:2in1)
#ENDFOR
#ENDCASE
EntriesPosition_ROUTINE     ROUTINE                !Re-draw the entries if the window changed
#CASE(%WhatToTransform)
#OF('All Entry Fields')
	LOOP i#  = FirstField() TO LastField()
  		IF i#{Prop:Type} = Create:Entry OR i#{Prop:Type} = Create:TEXT
    		CoolEntryFields.RefreshEntry(i#,GLO:2in1)
  		END
	END !LOOP
#OF('Selected Fields')
#FOR(%EntresControls)
		CoolEntryFields.RefreshEntry(%IMGControl,GLO:2in1)
#ENDFOR
#ENDCASE
ResizeButtons_ROUTINE ROUTINE
#CASE(%WhatButtonsToTransform)
#OF('All Buttons')
	LOOP ThisBTN#  = FirstField() TO LastField()
  		IF ThisBTN#{Prop:Type} = Create:Button
    		Buttonz.ResizeButtons(ThisBTN#,GLO:2in1)
  		END
	END !LOOP
#OF('Selected Buttons')
#FOR(%TilesControls)
#IF(%BTNControl)
	    	Buttonz.ResizeButtons(%BTNControl,GLO:2in1)
#ENDIF
#ENDFOR
#ENDCASE
InitButtonz_ROUTINE    ROUTINE                !Pass buttons to the Cool Buttons Class
#CASE(%WhatButtonsToTransform)
#OF('All Buttons')
	LOOP ThisBTN#  = FirstField() TO LastField()
  		IF ThisBTN#{Prop:Type} = Create:Button
    		Buttonz.AddButtonMimic(ThisBTN#,ThisBTN#{PROP:Color},,GLO:2in1)
  		END
	END !LOOP
#OF('Selected Buttons')
#FOR(%TilesControls)
#IF(%BTNControl)
	    	Buttonz.AddButtonMimic(%BTNControl,%BTNControl{PROP:Color},,GLO:2in1)
#ENDIF
#ENDFOR
#ENDCASE
RebuildButtonz_ROUTINE     ROUTINE                !Re-draw the buttons if the window changed
#CASE(%WhatButtonsToTransform)
#OF('All Buttons')
	LOOP ThisBTN#  = FirstField() TO LastField()
  		IF ThisBTN#{Prop:Type} = Create:Button
    		Buttonz.RebuildButtons(ThisBTN#,GLO:2in1)
  		END
	END !LOOP
#OF('Selected Buttons')
#FOR(%TilesControls)
#IF(%BTNControl)
		Buttonz.RebuildButtons(%BTNControl,GLO:2in1)
#ENDIF
#ENDFOR
#ENDCASE
ButtonzPosition_ROUTINE     ROUTINE     	!Re-draw the buttons if the window changed in 2 in 1 mode, Legacy, it should dissapear
	DO Enable2in1Buttons_ROUTINE
Enable2in1Buttons_ROUTINE ROUTINE     	!Re-draw the buttons if the window changed in 2 in 1 mode
#CASE(%WhatButtonsToTransform)
#OF('All Buttons')
	LOOP ThisBTN#  = FirstField() TO LastField()
  		IF ThisBTN#{Prop:Type} = Create:Button
    		Buttonz.Buttons2in1(ThisBTN#,GLO:2in1)
  		END
	END !LOOP
#OF('Selected Buttons')
#FOR(%TilesControls)
#IF(%BTNControl)
		Buttonz.Buttons2in1(%BTNControl,GLO:2in1)
#ENDIF
#ENDFOR
#ENDCASE
SetThemeWallpaper_ROUTINE ROUTINE
#CASE(%ThemeDefinedWallpaper)
#OF('Yes')
	0{PROP:Wallpaper} = 'Wallpaper.jpg'
#CASE(%PositionWallpaper)
#OF('Tiled')
	0{PROP:Tiled} = TRUE
#OF('Centered')
	0{PROP:Centered} = TRUE
#ENDCASE
#ENDCASE
#ENDAT
