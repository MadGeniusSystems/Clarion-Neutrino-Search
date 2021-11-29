					MEMBER()

					MAP
						INCLUDE('CWUTIL.INC'),ONCE
					END

INCLUDE('EntryFields.inc'),ONCE

EntryFields.Init    PROCEDURE(WindowManager pWM, <STRING pFont>)
CODE
	pWM.AddItem(SELF.WindowComponent)
	IF Omitted(pFont) = FALSE
		SELF.fontName = pFont
	END

EntryFields.AddEntryBG      PROCEDURE(SIGNED pEntryFEQ, LONG pFillNormal, BYTE pActionFactor=77, BYTE Enable2in1)
parentFeq                       SIGNED
CODE

	SELF.EntryQ.EntryFEQ  = pEntryFEQ
	SELF.EntryQ.fillNormal = pFillNormal
	SELF.EntryQ.actionFactor = pActionFactor
	parentFeq = SELF.EntryQ.EntryFEQ{PROP:Parent}		
  
  ! Add an image as the Entry BG
	SELF.EntryQ.EntryimageFEQ = Create(0, CREATE:Image, parentFeq)		
	SELF.EntryQ.EntryimageFEQ{PROP:Text} = 'MasterEBG2.png'
	SELF.EntryQ.EntryimageFEQ{PROP:Stretch} = TRUE
	SELF.EntryQ.EntryimageFEQ{PROP:Hide} = SELF.EntryQ.EntryFEQ{PROP:Hide}
	SELF.EntryQ.EntryimageFEQ{PROP:NoHeight} = TRUE

  ! Add an image as the Entry Underline
	SELF.EntryQ.imageFEQ = Create(0, CREATE:Image, parentFeq)
	SELF.EntryQ.imageFEQ{PROP:Tiled} = TRUE
	SELF.EntryQ.imageFEQ{PROP:Hide} = SELF.EntryQ.EntryFEQ{PROP:Hide}


! Add an image as the Entry Start of Text
	SELF.EntryQ.StartEntryimageFEQ = Create(0, CREATE:Image, parentFeq)		
	SELF.EntryQ.StartEntryimageFEQ{PROP:NoHeight} = TRUE
	SELF.EntryQ.StartEntryimageFEQ{PROP:Tiled} = TRUE
	SELF.EntryQ.StartEntryimageFEQ{PROP:Hide} = SELF.EntryQ.EntryFEQ{PROP:Hide}


! Add an image as the Entry End of Text
	SELF.EntryQ.EndEntryimageFEQ = Create(0, CREATE:Image, parentFeq)
	SELF.EntryQ.EndEntryimageFEQ{PROP:NoHeight} = TRUE
	SELF.EntryQ.EndEntryimageFEQ{PROP:Tiled} = TRUE
	SELF.EntryQ.EndEntryimageFEQ{PROP:Hide} = SELF.EntryQ.EntryFEQ{PROP:Hide}

	SELF.EntryQ.Status = 777 	!let's give it an impossible combination... 
								!We need to detect if something changed with this particular Entry

	Add(SELF.EntryQ)
	

	SELF.SetEntryElements(Enable2in1)
	
EntryFields.SetEntryElements        PROCEDURE(BYTE Enable2in1)
pos                                     GROUP
XPos                                        SIGNED           !Horizontal coordinate
YPos                                        SIGNED           !Vertical coordinate
Width                                       UNSIGNED         !Width
Height                                      UNSIGNED         !Height
LEDHeight                                   UNSIGNED
StartEntryHeight                            UNSIGNED
StartEntryWidth                             UNSIGNED
UnderlineHeight                             UNSIGNED
PosChecksum                                 SIGNED
Status                                      SIGNED
										END
savePixels                              BYTE
CODE
	savePixels = 0{PROP:Pixels}
	0{PROP:Pixels} = TRUE
	pos.LEDHeight = 5

	pos.StartEntryHeight  	= 3
	pos.StartEntryWidth   	= 1	
	pos.UnderlineHeight 	= 1



	pos.Status = SELF.EntryQ.EntryFEQ{PROP:Hide} & SELF.EntryQ.EntryFEQ{PROP:Disable} & SELF.EntryQ.EntryFEQ{PROP:ReadOnly} & SELF.EntryQ.EntryFEQ{PROP:Req} 

	! IF something changed with this field...
	! Is it now hidden? Or something else? Shall we re-draw something? Resize Something?

	IF SELF.EntryQ.Status <> pos.Status


		! let's make the original field Transparent, but only if it is not already transparent

		IF SELF.EntryQ.EntryFEQ{PROP:TRN} = FALSE
			SELF.EntryQ.EntryFEQ{PROP:TRN} =  TRUE
		END
		IF SELF.EntryQ.EntryFEQ{PROP:Flat} =  FALSE
			SELF.EntryQ.EntryFEQ{PROP:Flat} =  TRUE
		END
		
		! Hidden now?

		SELF.EntryQ.imageFEQ{PROP:Hide} = SELF.EntryQ.EntryFEQ{PROP:Hide}
		SELF.EntryQ.EntryimageFEQ{PROP:Hide} = SELF.EntryQ.EntryFEQ{PROP:Hide}
		SELF.EntryQ.imageFEQ{PROP:Hide} = SELF.EntryQ.EntryFEQ{PROP:Hide}
		SELF.EntryQ.StartEntryimageFEQ{PROP:Hide} = SELF.EntryQ.EntryFEQ{PROP:Hide}
		SELF.EntryQ.EndEntryimageFEQ{PROP:Hide} = SELF.EntryQ.EntryFEQ{PROP:Hide}

		! Different status?

		IF SELF.EntryQ.EntryFEQ{PROP:Req} = TRUE
			SELF.EntryQ.imageFEQ{PROP:Text} = 'Master_Required.png'
		END

		IF SELF.EntryQ.EntryFEQ{PROP:ReadOnly} = TRUE
			SELF.EntryQ.imageFEQ{PROP:Text} = 'Master_ReadOnly.png'
		END

		IF SELF.EntryQ.EntryFEQ{PROP:Disable} = TRUE
			SELF.EntryQ.imageFEQ{PROP:Text} = 'Master_Disabled.png'
		END	

		IF SELF.EntryQ.EntryFEQ{PROP:Disable} <> TRUE AND SELF.EntryQ.EntryFEQ{PROP:ReadOnly} <> TRUE AND SELF.EntryQ.EntryFEQ{PROP:Req} <> TRUE 
			SELF.EntryQ.imageFEQ{PROP:Text} = 'Master_Normal.png'
		END	

		! Start and end of the Entry Fields

		SELF.EntryQ.EndEntryimageFEQ{PROP:Text} = SELF.EntryQ.imageFEQ{PROP:Text}
		SELF.EntryQ.StartEntryimageFEQ{PROP:Text} = SELF.EntryQ.imageFEQ{PROP:Text}

	END
	
		!Should we resize the fields?

	GetPosition(SELF.EntryQ.EntryFEQ, pos.XPos, pos.YPos, pos.Width, pos.Height)
	pos.PosChecksum = pos.XPos * pos.YPos * pos.Width * pos.Height

		!Let's resize the images only if the original field has been changed

	IF SELF.EntryQ.PosChecksum <> pos.PosChecksum
		
		SetPosition(SELF.EntryQ.EntryimageFEQ, pos.XPos, pos.YPos, pos.Width, pos.Height )
		SetPosition(SELF.EntryQ.imageFEQ, pos.XPos, pos.YPos + SELF.EntryQ.EntryFEQ{PROP:Height}, pos.Width, pos.UnderlineHeight )
		SetPosition(SELF.EntryQ.StartEntryimageFEQ, SELF.EntryQ.imageFEQ{PROP:Xpos} , SELF.EntryQ.imageFEQ{PROP:Ypos} - pos.StartEntryHeight, pos.StartEntryWidth, pos.StartEntryHeight )
		SetPosition(SELF.EntryQ.EndEntryimageFEQ, SELF.EntryQ.imageFEQ{PROP:Xpos} + SELF.EntryQ.imageFEQ{PROP:Width} - pos.StartEntryWidth, SELF.EntryQ.imageFEQ{PROP:Ypos} - pos.StartEntryHeight, pos.StartEntryWidth, pos.StartEntryHeight )

	END

		!Let's save the new checksums for future reference

	SELF.EntryQ.PosChecksum = pos.XPos * pos.YPos * pos.Width * pos.Height
	SELF.EntryQ.Status = SELF.EntryQ.EntryFEQ{PROP:Hide} & SELF.EntryQ.EntryFEQ{PROP:Disable} & SELF.EntryQ.EntryFEQ{PROP:ReadOnly} & SELF.EntryQ.EntryFEQ{PROP:Req}
	PUT(SELF.EntryQ) 

	0{PROP:Pixels} = savePixels

EntryFields.RefreshEntry    PROCEDURE(SIGNED pEntryFEQ, BYTE Enable2in1)

CODE

	SELF.EntryQ.EntryFEQ = pEntryFEQ
	Get(SELF.EntryQ, SELF.EntryQ.EntryFEQ)
	IF ErrorCode()
		RETURN
	END

	SELF.SetEntryElements(Enable2in1)

EntryFields.Construct       PROCEDURE()

CODE

	SELF.EntryQ &= New(EntryQ_Type)
	SELF.imageMarginLeft     = 6
	SELF.imageMarginTop      = 2
	SELF.imageWidth          = 22
	SELF.imageHeight         = 22
	SELF.promptMarginLeft    = 6
	SELF.promptMarginTop     = 4


EntryFields.Destruct        PROCEDURE()

CODE
	Free(SELF.EntryQ)
	Dispose(SELF.EntryQ)

EntryFields.WindowComponent.TakeEvent       PROCEDURE() ! ,BYTE ! Declare Procedure

rv                                              BYTE

CODE
	rv = PARENT.WindowComponent.TakeEvent()
	IF 0{Prop:AcceptAll} = TRUE
		RETURN rv
	END

!	IF FIELD()
!		SELF.EntryQ.regionFEQ = Field()
!		Get(SELF.EntryQ, SELF.EntryQ.regionFEQ)
!
!		IF ErrorCode() = FALSE
!			CASE EVENT()
!				
!			OF EVENT:MouseDown
!				SELF.EntryQ.boxFEQ{PROP:Fill} = MixColors(COLOR:Black, SELF.EntryQ.fillNormal, SELF.EntryQ.actionFactor)
!					!SELF.EntryQ.promptFEQ{PROP:Background} = MixColors(COLOR:Black, SELF.EntryQ.fillNormal, SELF.EntryQ.actionFactor)
!
!			OF EVENT:MouseUp
!				SELF.EntryQ.boxFEQ{PROP:Fill} = SELF.EntryQ.fillNormal
!				Post(Event:Accepted, SELF.EntryQ.EntryFEQ)
!				SELECT(SELF.EntryQ.EntryFEQ)
!
!			OF EVENT:MouseIn
!
!				IF  SELF.EntryQ.boxFEQ{PROP:Fill} <> MixColors(COLOR:White, SELF.EntryQ.fillNormal, SELF.EntryQ.actionFactor)
!					SELF.EntryQ.boxFEQ{PROP:Fill} = MixColors(COLOR:White, SELF.EntryQ.fillNormal, SELF.EntryQ.actionFactor)
!					SELF.EntryQ.EntryimageFEQ{PROP:Text} = 'BTN_BG_Act.png'
!				ELSE
!
!				END
!					!SELF.EntryQ.promptFEQ{PROP:Background} = MixColors(COLOR:White, SELF.EntryQ.fillNormal, SELF.EntryQ.actionFactor)
!
!			OF EVENT:MouseOut
!				SELF.EntryQ.boxFEQ{PROP:Fill} = SELF.EntryQ.fillNormal
!				SELF.EntryQ.EntryimageFEQ{PROP:Text} = 'BTN_BG.png'
!					!SELF.EntryQ.promptFEQ{PROP:Background} = SELF.EntryQ.fillNormal
!			END
!		END
!	END
!
	RETURN rv