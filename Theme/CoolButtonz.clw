					MEMBER()

					MAP
						INCLUDE('CWUTIL.INC'),ONCE
					END

	INCLUDE('CoolButtonz.inc'),ONCE

CoolButtonz.Construct       PROCEDURE()
	CODE

		SELF.ButtonQ &= New(ButtonQ_Type)
		SELF.imageMarginLeft     = 6
		SELF.imageMarginTop      = 2
		SELF.imageWidth          = 22
		SELF.imageHeight         = 22
		SELF.promptMarginLeft    = 6
		SELF.promptMarginTop     = 4
		SELF.DefaultColor 		= 00937032h


CoolButtonz.Destruct        PROCEDURE()
	CODE

		FREE(SELF.ButtonQ)
		DISPOSE(SELF.ButtonQ)

CoolButtonz.Init    PROCEDURE(WindowManager pWM, <STRING pFont>)
	CODE

		pWM.AddItem(SELF.WindowComponent)
		IF Omitted(pFont) = FALSE
			SELF.fontName = pFont
		END

		0{PROP:Buffer} = 1 					! More memory consumption but worth it...
	! 0{PROP:LazyDisplay} = 1 			!LazyDisplay creates flicker...

CoolButtonz.SetButtonProperties     PROCEDURE(SIGNED pButtonFEQ)

	CODE

		

		SELF.ButtonQ.BTNimageFEQ{PROP:Disable} 	= pButtonFEQ{PROP:Disable}
		!SELF.ButtonQ.BTNBorderFEQ{PROP:Disable} 	= pButtonFEQ{PROP:Disable}
	!SELF.ButtonQ.LedFEQ{PROP:Disable} 		= pButtonFEQ{PROP:Disable}
		SELF.ButtonQ.imageFEQ{PROP:Disable} 		= pButtonFEQ{PROP:Disable}
		SELF.ButtonQ.promptFEQ{PROP:Disable} 	= pButtonFEQ{PROP:Disable}
		SELF.ButtonQ.regionFEQ{PROP:Hide} 	= pButtonFEQ{PROP:Disable}

		
	
CoolButtonz.AddButtonMimic  PROCEDURE(SIGNED pButtonFEQ, LONG pFillNormal, BYTE pActionFactor=77, BYTE Enable2in1)
ParentFeq                       SIGNED
	CODE

		SELF.ButtonQ.ButtonFEQ  = pButtonFEQ ! Original Button FEQ
		
		SELF.ButtonQ.actionFactor = pActionFactor
		ParentFeq = SELF.ButtonQ.ButtonFEQ{PROP:Parent}

		IF pFillNormal < 255 ! If an invalid color is passed
			SELF.ButtonQ.fillNormal = SELF.DefaultColor	
		ELSE
			SELF.ButtonQ.fillNormal = pFillNormal
		END
	
  	! Add an image as the Button BG

		SELF.ButtonQ.BTNimageFEQ = Create(0, CREATE:Image, ParentFeq)

  	! Make a border
!
		SELF.ButtonQ.BTNBorderFEQ = Create(0, CREATE:Box, ParentFeq)
		SELF.ButtonQ.BTNBorderFEQ{PROP:Color} = 00000000h !'' ! 00000000h !Black... If table mode used, silver...
		SELF.ButtonQ.BTNBorderFEQ{PROP:Fill} = 00413723h ! pFillNormal! 00000000h !'' ! 00000000h !Black... If table mode used, silver...
		SELF.ButtonQ.BTNBorderFEQ{PROP:LineWidth} = 0
		SELF.ButtonQ.BTNBorderFEQ{PROP:Round} = TRUE
		SELF.ButtonQ.BTNBorderFEQ{PROP:Hide} = TRUE

  	! Make a box
	!SELF.ButtonQ.LedFEQ = Create(0, CREATE:Box, ParentFeq)


  	! Add an icon

		SELF.ButtonQ.imageFEQ = Create(0, CREATE:Image, ParentFeq)

  	! Original Text from the button itself
		SELF.ButtonQ.promptFEQ = Create(0, CREATE:Prompt, ParentFeq)
		SELF.ButtonQ.promptFEQ{PROP:FontColor} = 00CCCCCCh
		SELF.ButtonQ.promptFEQ{PROP:Font} = 'Arial Narrow'
		SELF.ButtonQ.promptFEQ{PROP:FontSize} = 9
		SELF.ButtonQ.promptFEQ{PROP:FontStyle}   = FONT:bold
		SELF.ButtonQ.promptFEQ{PROP:Trn} = TRUE
		IF SELF.fontName
			SELF.ButtonQ.promptFEQ{PROP:FontName} = SELF.fontName
		END

  	! A Region with IMM to receive the event...

		SELF.ButtonQ.regionFEQ = Create(0, CREATE:Region, ParentFeq)
		SELF.ButtonQ.regionFEQ{PROP:IMM} = TRUE

	!Filename of the original Icon?
		SELF.ButtonQ.IconText = SELF.ButtonQ.ButtonFEQ{PROP:Icon}


		! We are algmost ready... Let's add the info to the Queue



		Add(SELF.ButtonQ)
		SELF.RebuildButtons(pButtonFEQ, Enable2in1)
		SELF.ResizeButtons(pButtonFEQ, Enable2in1)


CoolButtonz.IsHidden        PROCEDURE()

	CODE
		

		! If the button is hidden or disabled, we will inherit the property


		SELF.ButtonQ.BTNBorderFEQ{PROP:Hide} = SELF.ButtonQ.ButtonFEQ{PROP:Hide}
		SELF.ButtonQ.imageFEQ{PROP:Hide} = SELF.ButtonQ.ButtonFEQ{PROP:Hide}
		SELF.ButtonQ.promptFEQ{PROP:Hide} = SELF.ButtonQ.ButtonFEQ{PROP:Hide}
		SELF.ButtonQ.regionFEQ{PROP:Hide} = SELF.ButtonQ.ButtonFEQ{PROP:Hide}
		!SELF.buttonQ.BTNimageFEQ{PROP:Hide} = SELF.ButtonQ.ButtonFEQ{PROP:Hide}
		
		IF SELF.ButtonQ.ButtonFEQ{PROP:Disable} = TRUE
			
		!	SELF.ButtonQ.BTNBorderFEQ{PROP:Disable} 	= TRUE
			SELF.ButtonQ.imageFEQ{PROP:Disable} 	= TRUE
			SELF.ButtonQ.promptFEQ{PROP:Disable} 	= TRUE
			SELF.ButtonQ.regionFEQ{PROP:Disable} 	= TRUE
			!SELF.buttonQ.BTNimageFEQ{PROP:Hide} = TRUE
			
		END
		
		SELF.buttonQ.BTNimageFEQ{PROP:Hide} = SELF.ButtonQ.ButtonFEQ{PROP:Hide}
		SELF.ButtonQ.BTNBorderFEQ{PROP:Hide} = TRUE
		!END

CoolButtonz.RebuildButtons  PROCEDURE(SIGNED pButtonFEQ,BYTE Enable2in1)
SavePixels                      BYTE

	CODE

		SavePixels = 0{PROP:Pixels}

		SELF.ButtonQ.ButtonFEQ = pButtonFEQ
		Get(SELF.ButtonQ, SELF.ButtonQ.ButtonFEQ)
		IF ErrorCode()
			RETURN
		END
	
		 !Image for the Button BG
		SELF.ButtonQ.BTNimageFEQ{PROP:Text} = 'blue75.png'
		!SELF.ButtonQ.BTNimageFEQ{PROP:Centered} = FALSE
		SELF.ButtonQ.BTNimageFEQ{PROP:Hide} = TRUE !This theme will be minimalist, but I might be able to use this in the future

		! Border Color

		SELF.ButtonQ.BTNBorderFEQ{PROP:FillColor} = 00CCCCCCh !'' ! 00000000h !Black... If table mode used, silver...
		SELF.ButtonQ.BTNBorderFEQ{PROP:LineWidth} = 1
		SELF.ButtonQ.BTNBorderFEQ{PROP:TRN} = TRUE
		SELF.ButtonQ.BTNBorderFEQ{PROP:Hide} = FALSE

		! LED color

	!SELF.ButtonQ.LedFEQ{PROP:Fill} = SELF.ButtonQ.fillNormal
	!SELF.ButtonQ.LedFEQ{PROP:Hide} = TRUE

		! Prompt

		SELF.ButtonQ.promptFEQ{PROP:Text} = SELF.ButtonQ.ButtonFEQ{PROP:Text}

		! Icon
		
		IF pButtonFEQ{PROP:Icon} <> ''
			SELF.ButtonQ.imageFEQ{PROP:Text} = SELF.ButtonQ.ButtonFEQ{PROP:Icon}		
		ELSE
			SELF.ButtonQ.imageFEQ{PROP:Text} = 'morpho.png'
		END
	


		SELF.IsHidden()


		! Restore the Pixels Property of the Window

		0{PROP:Pixels} = SavePixels


CoolButtonz.ResizeButtons   PROCEDURE(SIGNED pButtonFEQ,BYTE Enable2in1)
pos                             GROUP
XPos                                SIGNED           !Horizontal coordinate
YPos                                SIGNED           !Vertical coordinate
Width                               UNSIGNED         !Width
Height                              UNSIGNED         !Height
LEDHeight                           UNSIGNED
								END
SavePixels                      BYTE
	CODE
		SavePixels = 0{PROP:Pixels}
		0{PROP:Pixels} = TRUE
		pos.LEDHeight = 0 ! The small colored underline of the button

		SELF.ButtonQ.ButtonFEQ = pButtonFEQ
		Get(SELF.ButtonQ, SELF.ButtonQ.ButtonFEQ)
		IF ErrorCode()
			RETURN
		END
	
		GetPosition(pButtonFEQ, pos.XPos, pos.YPos, pos.Width, pos.Height) ! Where is the original button?

		IF Enable2in1 = FALSE ! Is it a Laptop or Slate?
			SetPosition(SELF.ButtonQ.BTNimageFEQ, pos.XPos, pos.YPos, pos.Width, pos.Height)
			SetPosition(SELF.ButtonQ.BTNBorderFEQ, pos.XPos, pos.YPos, pos.Width, pos.Height)
		!SetPosition(SELF.ButtonQ.LedFEQ, pos.XPos, pos.YPos + pos.Height - pos.LEDHeight + 1, pos.Width, pos.LEDHeight)
			SetPosition(SELF.ButtonQ.imageFEQ, pos.XPos+SELF.imageMarginLeft, pos.YPos + ((pos.Height - SELF.ButtonQ.imageFEQ{PROP:Height}) / 2) , SELF.imageWidth, SELF.imageHeight)
			SELF.ButtonQ.imageFEQ{PROP:NoHeight} = TRUE
			SELF.ButtonQ.imageFEQ{PROP:NoWidth} = TRUE
		
		
		!SELF.ButtonQ.imageFEQ{PROP:Xpos} = !SELF.ButtonQ.LedFEQ{PROP:Xpos} + ((!SELF.ButtonQ.LedFEQ{PROP:Width} - SELF.ButtonQ.imageFEQ{PROP:Width}) / 2 )
			SetPosition(SELF.ButtonQ.promptFEQ, SELF.ButtonQ.imageFEQ{PROP:Xpos} + SELF.ButtonQ.imageFEQ{PROP:Width} + SELF.imageMarginLeft, SELF.ButtonQ.imageFEQ{PROP:Ypos} + (SELF.ButtonQ.imageFEQ{PROP:Height} / 2) - (SELF.ButtonQ.promptFEQ{PROP:Height} / 2))
			SetPosition(SELF.ButtonQ.regionFEQ, pos.XPos, pos.YPos, pos.Width, pos.Height)
		ELSE
			SetPosition(SELF.ButtonQ.BTNimageFEQ, pos.XPos, pos.YPos, pos.Width, pos.Height)
			SetPosition(SELF.ButtonQ.BTNBorderFEQ, pos.XPos, pos.YPos, pos.Width, pos.Height)
		!SetPosition(SELF.ButtonQ.LedFEQ, pos.XPos, pos.YPos + pos.Height - pos.LEDHeight + 1, pos.Width, pos.LEDHeight)
		!SELF.ButtonQ.LedFEQ{PROP:Round} = FALSE
			SetPosition(SELF.ButtonQ.imageFEQ, pos.XPos+SELF.imageMarginLeft, pos.YPos+SELF.imageMarginTop, SELF.imageWidth, SELF.imageHeight)
			SELF.ButtonQ.imageFEQ{PROP:NoHeight} = TRUE
			SELF.ButtonQ.imageFEQ{PROP:NoWidth} = TRUE
			SetPosition(SELF.ButtonQ.promptFEQ, pos.XPos+SELF.promptMarginLeft, SELF.ButtonQ.imageFEQ{PROP:Ypos} + SELF.ButtonQ.imageFEQ{PROP:Height} + SELF.promptMarginTop )
			SetPosition(SELF.ButtonQ.regionFEQ, pos.XPos, pos.YPos, pos.Width, pos.Height)
		END

	! Let's "hide" the button... No, not really, but we will send it away so the end user cannot see it or press it...

		pButtonFEQ{PROP:Height} = 0
		pButtonFEQ{PROP:Width} = 0
		pButtonFEQ{PROP:Xpos} = -10000
		pButtonFEQ{PROP:Ypos} = -10000

		SELF.IsHidden()

		! Restore the Pixels Property of the Window

		0{PROP:Pixels} = SavePixels

CoolButtonz.Buttons2in1     PROCEDURE(SIGNED pButtonFEQ,BYTE Enable2in1)
pos                             GROUP
XPos                                SIGNED           !Horizontal coordinate
YPos                                SIGNED           !Vertical coordinate
Width                               UNSIGNED         !Width
Height                              UNSIGNED         !Height
LEDHeight                           UNSIGNED
								END
SavePixels                      BYTE
	CODE
		SavePixels = 0{PROP:Pixels}
		0{PROP:Pixels} = TRUE
		pos.LEDHeight = 0 ! The small colored underline of the button

		SELF.ButtonQ.ButtonFEQ = pButtonFEQ
		Get(SELF.ButtonQ, SELF.ButtonQ.ButtonFEQ)
		IF ErrorCode()
			RETURN
		END
	
		GetPosition(SELF.ButtonQ.BTNimageFEQ, pos.XPos, pos.YPos, pos.Width, pos.Height) ! Where is the original button?

		IF Enable2in1 = FALSE ! Is it a Laptop or Slate?
			SetPosition(SELF.ButtonQ.BTNimageFEQ, pos.XPos, pos.YPos, pos.Width, pos.Height)
			SetPosition(SELF.ButtonQ.BTNBorderFEQ, pos.XPos, pos.YPos, pos.Width, pos.Height)
		!SetPosition(SELF.ButtonQ.LedFEQ, pos.XPos, pos.YPos + pos.Height - pos.LEDHeight + 1, pos.Width, pos.LEDHeight)
			SetPosition(SELF.ButtonQ.imageFEQ, pos.XPos+SELF.imageMarginLeft, pos.YPos + ((pos.Height - SELF.ButtonQ.imageFEQ{PROP:Height}) / 2) , SELF.imageWidth, SELF.imageHeight)
			SELF.ButtonQ.imageFEQ{PROP:NoHeight} = TRUE
			SELF.ButtonQ.imageFEQ{PROP:NoWidth} = TRUE
		
		!SELF.ButtonQ.imageFEQ{PROP:Xpos} = !SELF.ButtonQ.LedFEQ{PROP:Xpos} + ((!SELF.ButtonQ.LedFEQ{PROP:Width} - SELF.ButtonQ.imageFEQ{PROP:Width}) / 2 )
		!SetPosition(SELF.ButtonQ.promptFEQ, pos.XPos + (SELF.ButtonQ.BTNimageFEQ{PROP:Width} / 2) - (SELF.ButtonQ.promptFEQ{PROP:Width} / 2), SELF.ButtonQ.imageFEQ{PROP:Ypos} + SELF.ButtonQ.imageFEQ{PROP:Height} + SELF.promptMarginTop )
			SetPosition(SELF.ButtonQ.promptFEQ, SELF.ButtonQ.imageFEQ{PROP:Xpos} + SELF.ButtonQ.imageFEQ{PROP:Width} + SELF.imageMarginLeft, SELF.ButtonQ.imageFEQ{PROP:Ypos} + (SELF.ButtonQ.imageFEQ{PROP:Height} / 2) - (SELF.ButtonQ.promptFEQ{PROP:Height} / 2))
			SetPosition(SELF.ButtonQ.regionFEQ, pos.XPos, pos.YPos, pos.Width, pos.Height)
		ELSE
			SetPosition(SELF.ButtonQ.BTNimageFEQ, pos.XPos, pos.YPos, pos.Width, pos.Height)
			SetPosition(SELF.ButtonQ.BTNBorderFEQ, pos.XPos, pos.YPos, pos.Width, pos.Height)
		!SetPosition(SELF.ButtonQ.LedFEQ, pos.XPos, pos.YPos + pos.Height - pos.LEDHeight + 1, pos.Width, pos.LEDHeight)
		!SELF.ButtonQ.LedFEQ{PROP:Round} = FALSE
			SetPosition(SELF.ButtonQ.imageFEQ, pos.XPos+SELF.imageMarginLeft, pos.YPos+SELF.imageMarginTop, SELF.imageWidth, SELF.imageHeight)
			SELF.ButtonQ.imageFEQ{PROP:NoHeight} = TRUE
			SELF.ButtonQ.imageFEQ{PROP:NoWidth} = TRUE
			SetPosition(SELF.ButtonQ.promptFEQ, pos.XPos+SELF.promptMarginLeft, SELF.ButtonQ.imageFEQ{PROP:Ypos} + SELF.ButtonQ.imageFEQ{PROP:Height} + SELF.promptMarginTop )
			SetPosition(SELF.ButtonQ.regionFEQ, pos.XPos, pos.YPos, pos.Width, pos.Height)
		END

	! Restore the Pixels Property of the Window

		0{PROP:Pixels} = SavePixels

CoolButtonz.RefreshTile     PROCEDURE(SIGNED pButtonFEQ, BYTE Enable2in1)

	CODE

		SELF.ButtonQ.ButtonFEQ = pButtonFEQ
		Get(SELF.ButtonQ, SELF.ButtonQ.ButtonFEQ)
		IF NOT ERRORCODE()
			SELF.RebuildButtons(pButtonFEQ, Enable2in1)
		ELSE
			RETURN
		END


CoolButtonz.WindowComponent.TakeEvent       PROCEDURE() ! ,BYTE ! Declare Procedure
RetVal                                          BYTE
	CODE
		RetVal = PARENT.WindowComponent.TakeEvent()
		IF 0{Prop:AcceptAll} = TRUE
			RETURN RetVal
		END

		IF FIELD()

			SELF.ButtonQ.regionFEQ = Field()
			Get(SELF.ButtonQ, SELF.ButtonQ.regionFEQ)

			IF ErrorCode() = FALSE

				CASE EVENT()
				
				OF EVENT:MouseDown
					
				!SELF.ButtonQ.LedFEQ{PROP:Fill} = MixColors(COLOR:Black, SELF.ButtonQ.fillNormal, SELF.ButtonQ.actionFactor)

				OF EVENT:MouseUp
				!SELF.ButtonQ.LedFEQ{PROP:Fill} = SELF.ButtonQ.fillNormal
					IF SELF.ButtonQ.ButtonFEQ{PROP:Disable} = FALSE
						POST(Event:Accepted, SELF.ButtonQ.ButtonFEQ)
						SELECT(SELF.ButtonQ.ButtonFEQ)
						SELF.ButtonQ.BTNimageFEQ{PROP:Text} = 'w50.png'
					END
					

				OF EVENT:MouseIn

				!IF  !SELF.ButtonQ.LedFEQ{PROP:Fill} <> MixColors(COLOR:White, SELF.ButtonQ.fillNormal, SELF.ButtonQ.actionFactor)
					!SELF.ButtonQ.LedFEQ{PROP:Fill} = MixColors(COLOR:White, SELF.ButtonQ.fillNormal, SELF.ButtonQ.actionFactor)
					!SELF.ButtonQ.BTNimageFEQ{PROP:Text} = 'rbg100.png'
					IF SELF.ButtonQ.BTNBorderFEQ{PROP:FillColor} = 00CCCCCCh
						SELF.ButtonQ.BTNBorderFEQ{PROP:FillColor} = 00FFFFFFh
						SELF.ButtonQ.BTNimageFEQ{PROP:Text} = 'bluehot75.png'
					END
					
				!ELSE

				!END
					SELF.ButtonQ.promptFEQ{PROP:FontColor} = 00FFFFFFh				
					SELF.ButtonQ.imageFEQ{PROP:Text} = 'Hot_' & SELF.ButtonQ.IconText


				OF EVENT:MouseOut
				!SELF.ButtonQ.LedFEQ{PROP:Fill} = SELF.ButtonQ.fillNormal
					IF SELF.ButtonQ.BTNBorderFEQ{PROP:FillColor} = 00FFFFFFh
						SELF.ButtonQ.BTNimageFEQ{PROP:Text} = 'blue75.png'
						SELF.ButtonQ.promptFEQ{PROP:FontColor} = 00CCCCCCh
						SELF.ButtonQ.imageFEQ{PROP:Text} = SELF.ButtonQ.IconText
						SELF.ButtonQ.BTNBorderFEQ{PROP:FillColor} = 00CCCCCCh
					END
					
				END
			END
		END

		RETURN RetVal