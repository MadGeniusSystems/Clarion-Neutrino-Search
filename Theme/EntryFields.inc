INCLUDE('BaseWindowComponent.inc'),ONCE

EntryQ_Type         QUEUE,TYPE
EntryimageFEQ           SIGNED
BTNBorderFEQ            SIGNED
EntryFEQ                SIGNED
regionFEQ               SIGNED
boxFEQ                  SIGNED
promptFEQ               SIGNED
imageFEQ                SIGNED
StartEntryimageFEQ      SIGNED
EndEntryimageFEQ        SIGNED
PosChecksum				SIGNED
Status					SIGNED
fillNormal              LONG
actionFactor            BYTE
					END

EntryFields         CLASS(BaseWindowComponent),IMPLEMENTS(WindowComponent),TYPE,MODULE('EntryFields.Clw'),LINK('EntryFields.Clw',_ABCLinkMode_),DLL(_ABCDllMode_)
EntryQ                  &EntryQ_Type
fontName                CSTRING(255)
imageMarginLeft         BYTE
imageMarginTop          BYTE
imageWidth              BYTE
imageHeight             BYTE
promptMarginLeft        BYTE
promptMarginTop         BYTE
Construct               PROCEDURE()
Destruct                PROCEDURE()
Init                    PROCEDURE(WindowManager pWM, <STRING pFont>)
AddEntryBG              PROCEDURE(SIGNED pEntryFEQ, LONG pFillNormal, BYTE pHoverLightenFactor=66, BYTE Enable2in1 = 0 )
SetEntryElements        PROCEDURE(BYTE Enable2in1)
RefreshEntry            PROCEDURE(SIGNED pEntryFEQ, BYTE Enable2in1)
					END
