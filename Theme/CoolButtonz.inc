INCLUDE('BaseWindowComponent.inc'),ONCE

buttonQ_Type        QUEUE,TYPE
BTNimageFEQ             SIGNED
IconText				CSTRING(256)
BTNBorderFEQ            SIGNED
buttonFEQ               SIGNED
regionFEQ               SIGNED
LedFEQ                  SIGNED
promptFEQ               SIGNED
imageFEQ                SIGNED
fillNormal              LONG
actionFactor            BYTE
Status                  SIGNED
IHidIt                  BYTE
WasHidden               BYTE
Hidden                  BYTE
Disabled                BYTE
ReadOnly                BYTE
PositionChecksum        SIGNED
					END

CoolButtonz         CLASS(BaseWindowComponent),IMPLEMENTS(WindowComponent),TYPE,MODULE('CoolButtonz.Clw'),LINK('CoolButtonz.Clw',_ABCLinkMode_),DLL(_ABCDllMode_)
buttonQ                 &buttonQ_Type
controlsQ               &ControlQ_Type
fontName                CSTRING(255)
imageMarginLeft         BYTE
imageMarginTop          BYTE
imageWidth              BYTE
imageHeight             BYTE
promptMarginLeft        BYTE
promptMarginTop         BYTE
DefaultColor			LONG
Construct               PROCEDURE()
Destruct                PROCEDURE()
Init                    PROCEDURE(WindowManager pWM, <STRING pFont>)
AddButtonMimic          PROCEDURE(SIGNED pButtonFEQ, LONG pFillNormal, BYTE pHoverLightenFactor=66, BYTE Enable2in1 = 0 )
IsHidden                PROCEDURE()
RefreshTile             PROCEDURE(SIGNED pButtonFEQ,BYTE Enable2in1)
RebuildButtons          PROCEDURE(SIGNED pButtonFEQ,BYTE Enable2in1)
ResizeButtons           PROCEDURE(SIGNED pButtonFEQ,BYTE Enable2in1)
Buttons2in1             PROCEDURE(SIGNED pButtonFEQ,BYTE Enable2in1)
SetButtonProperties     PROCEDURE(SIGNED pButtonFEQ)
					END
