

   MEMBER('Neutrino_Search.clw')                           ! This is a MEMBER module


   INCLUDE('ABBROWSE.INC'),ONCE
   INCLUDE('ABPOPUP.INC'),ONCE
   INCLUDE('ABRESIZE.INC'),ONCE
   INCLUDE('ABTOOLBA.INC'),ONCE
   INCLUDE('ABWINDOW.INC'),ONCE
   INCLUDE('BRWEXT.INC'),ONCE

                     MAP
                       INCLUDE('NEUTRINO_SEARCH001.INC'),ONCE        !Local module procedure declarations
                     END


!!! <summary>
!!! Generated from procedure template - Frame
!!! Neutrino Search Demo
!!! </summary>
Main PROCEDURE 

LOC:CommitCounter    LONG                                  ! 
LocEnableEnterByTab  BYTE(1)                               !Used by the ENTER Instead of Tab template
EnterByTabManager    EnterByTabClass
AppFrame             APPLICATION('Neutrino Search Demo'),AT(,,716,437),FONT('Microsoft Sans Serif',8,,FONT:regular, |
  CHARSET:DEFAULT),RESIZE,CENTERED,CENTER,ICON('WAFRAME.ICO'),MAX,STATUS(-1,80,120,45),SYSTEM, |
  WALLPAPER('.\resources\wallpaper.jpg'),IMM
                       MENUBAR,USE(?Menubar)
                         MENU('&File'),USE(?FileMenu)
                           ITEM('&Print Setup ...'),USE(?PrintSetup),MSG('Setup printer'),STD(STD:PrintSetup)
                           ITEM,USE(?SEPARATOR1),SEPARATOR
                           ITEM('E&xit'),USE(?Exit),MSG('Exit this application'),STD(STD:Close)
                         END
                         MENU('&Edit'),USE(?EditMenu)
                           ITEM('Cu&t'),USE(?Cut),MSG('Cut Selection To Clipboard'),STD(STD:Cut)
                           ITEM('&Copy'),USE(?Copy),MSG('Copy Selection To Clipboard'),STD(STD:Copy)
                           ITEM('&Paste'),USE(?Paste),MSG('Paste From Clipboard'),STD(STD:Paste)
                         END
                         MENU('&Browse'),USE(?BrowseMenu)
                           ITEM('Browse the CSV_US_DB file'),USE(?BrowseCSV_US_DB),MSG('Browse CSV_US_DB')
                           ITEM('Browse the SV_US_DB file'),USE(?BrowseSV_US_DB),MSG('Browse SV_US_DB')
                         END
                         MENU('Data'),USE(?Data_Menu)
                           ITEM('Convert CSV to SQLite and TPS'),USE(?Menu_Conversion_SQLite_TPS)
                         END
                         MENU('&Window'),USE(?WindowMenu),STD(STD:WindowList)
                           ITEM('T&ile'),USE(?Tile),MSG('Arrange multiple opened windows'),STD(STD:TileWindow)
                           ITEM('&Cascade'),USE(?Cascade),MSG('Arrange multiple opened windows'),STD(STD:CascadeWindow)
                           ITEM('&Arrange Icons'),USE(?Arrange),MSG('Arrange the icons for minimized windows'),STD(STD:ArrangeIcons)
                         END
                         MENU('&Help'),USE(?HelpMenu)
                           ITEM('&Contents'),USE(?Helpindex),MSG('View the contents of the help file'),STD(STD:HelpIndex)
                           ITEM('&Search for Help On...'),USE(?HelpSearch),MSG('Search for help on a subject'),STD(STD:HelpSearch)
                           ITEM('&How to Use Help'),USE(?HelpOnHelp),MSG('Provides general instructions on using help'), |
  STD(STD:HelpOnHelp)
                         END
                       END
                       TOOLBAR,AT(0,0,716,36),USE(?TOOLBAR1),FONT(,,COLOR:White),COLOR(00312421h)
                         BUTTON('Search CSV'),AT(8,4,103,27),USE(?Search_CSV_BTN),LEFT,ICON('Customs-WF_48x48.png'), |
  FLAT,TRN
                         BUTTON('Search TPS'),AT(120,4,103,27),USE(?Search_TPS_BTN),LEFT,ICON('Customs-WF_48x48.png'), |
  FLAT,TRN
                         BUTTON('Search SQLite'),AT(232,4,103,27),USE(?Search_SQLite_BTN),LEFT,ICON('Customs-WF_48x48.png'), |
  FLAT,TRN
                       END
                     END

CoolEntryFields EntryFields
Buttonz CoolButtonz
ThisWindow           CLASS(WindowManager)
Init                   PROCEDURE(),BYTE,PROC,DERIVED
Kill                   PROCEDURE(),BYTE,PROC,DERIVED
TakeAccepted           PROCEDURE(),BYTE,PROC,DERIVED
TakeEvent              PROCEDURE(),BYTE,PROC,DERIVED
                     END

Toolbar              ToolbarClass

  CODE
  GlobalResponse = ThisWindow.Run()                        ! Opens the window and starts an Accept Loop

!---------------------------------------------------------------------------
DefineListboxStyle ROUTINE
!|
!| This routine create all the styles to be shared in this window
!| It`s called after the window open
!|
!---------------------------------------------------------------------------
Menu::Menubar ROUTINE                                      ! Code for menu items on ?Menubar
Menu::FileMenu ROUTINE                                     ! Code for menu items on ?FileMenu
Menu::EditMenu ROUTINE                                     ! Code for menu items on ?EditMenu
Menu::BrowseMenu ROUTINE                                   ! Code for menu items on ?BrowseMenu
  CASE ACCEPTED()
  OF ?BrowseCSV_US_DB
    START(BrowseCSV_US_DB, 050000)
  OF ?BrowseSV_US_DB
    START(BrowseSV_US_DB, 050000)
  END
Menu::Data_Menu ROUTINE                                    ! Code for menu items on ?Data_Menu
  CASE ACCEPTED()
  OF ?Menu_Conversion_SQLite_TPS
    START(Conversion_Window, 25000)
  END
Menu::WindowMenu ROUTINE                                   ! Code for menu items on ?WindowMenu
Menu::HelpMenu ROUTINE                                     ! Code for menu items on ?HelpMenu
InitEntries_ROUTINE    ROUTINE                !Pass Entry Fields to the Entries Class
 LOOP i#  = FirstField() TO LastField()
    IF i#{Prop:Type} = Create:Entry OR i#{Prop:Type} = Create:TEXT
      CoolEntryFields.AddEntryBG(i#,i#{PROP:Color},,GLO:2in1)
    END
 END !LOOP
EntriesPosition_ROUTINE     ROUTINE                !Re-draw the entries if the window changed
 LOOP i#  = FirstField() TO LastField()
    IF i#{Prop:Type} = Create:Entry OR i#{Prop:Type} = Create:TEXT
      CoolEntryFields.RefreshEntry(i#,GLO:2in1)
    END
 END !LOOP
ResizeButtons_ROUTINE ROUTINE
      Buttonz.ResizeButtons(?Search_CSV_BTN,GLO:2in1)
      Buttonz.ResizeButtons(?Search_TPS_BTN,GLO:2in1)
      Buttonz.ResizeButtons(?Search_SQLite_BTN,GLO:2in1)
InitButtonz_ROUTINE    ROUTINE                !Pass buttons to the Cool Buttons Class
      Buttonz.AddButtonMimic(?Search_CSV_BTN,?Search_CSV_BTN{PROP:Color},,GLO:2in1)
      Buttonz.AddButtonMimic(?Search_TPS_BTN,?Search_TPS_BTN{PROP:Color},,GLO:2in1)
      Buttonz.AddButtonMimic(?Search_SQLite_BTN,?Search_SQLite_BTN{PROP:Color},,GLO:2in1)
RebuildButtonz_ROUTINE     ROUTINE                !Re-draw the buttons if the window changed
  Buttonz.RebuildButtons(?Search_CSV_BTN,GLO:2in1)
  Buttonz.RebuildButtons(?Search_TPS_BTN,GLO:2in1)
  Buttonz.RebuildButtons(?Search_SQLite_BTN,GLO:2in1)
ButtonzPosition_ROUTINE     ROUTINE     	!Re-draw the buttons if the window changed in 2 in 1 mode, Legacy, it should dissapear										
 DO Enable2in1Buttons_ROUTINE
Enable2in1Buttons_ROUTINE ROUTINE     	!Re-draw the buttons if the window changed in 2 in 1 mode
  Buttonz.Buttons2in1(?Search_CSV_BTN,GLO:2in1)
  Buttonz.Buttons2in1(?Search_TPS_BTN,GLO:2in1)
  Buttonz.Buttons2in1(?Search_SQLite_BTN,GLO:2in1)
SetThemeWallpaper_ROUTINE ROUTINE

ThisWindow.Init PROCEDURE

ReturnValue          BYTE,AUTO

  CODE
  GlobalErrors.SetProcedureName('Main')
  SELF.Request = GlobalRequest                             ! Store the incoming request
  ReturnValue = PARENT.Init()
  IF ReturnValue THEN RETURN ReturnValue.
  SELF.FirstField = 1
  SELF.VCRRequest &= VCRRequest
  SELF.Errors &= GlobalErrors                              ! Set this windows ErrorManager to the global ErrorManager
  CLEAR(GlobalRequest)                                     ! Clear GlobalRequest after storing locally
  CLEAR(GlobalResponse)
  SELF.AddItem(Toolbar)
  SELF.Open(AppFrame)                                      ! Open window
  !Setting the LineHeight for every control of type LIST/DROP or COMBO in the window using the global setting.
  Do DefineListboxStyle
  SELF.SetAlerts()
   CoolEntryFields.Init(SELF)
   DO InitEntries_ROUTINE
   Buttonz.Init(SELF)
   DO InitButtonz_ROUTINE
      AppFrame{PROP:TabBarVisible}  = False
  EnterByTabManager.Init(False)
  RETURN ReturnValue


ThisWindow.Kill PROCEDURE

ReturnValue          BYTE,AUTO

  CODE
  ReturnValue = PARENT.Kill()
  IF ReturnValue THEN RETURN ReturnValue.
  GlobalErrors.SetProcedureName
  RETURN ReturnValue


ThisWindow.TakeAccepted PROCEDURE

ReturnValue          BYTE,AUTO

Looped BYTE
  CODE
  LOOP                                                     ! This method receive all EVENT:Accepted's
    IF Looped
      RETURN Level:Notify
    ELSE
      Looped = 1
    END
    CASE ACCEPTED()
    ELSE
      DO Menu::Menubar                                     ! Process menu items on ?Menubar menu
      DO Menu::FileMenu                                    ! Process menu items on ?FileMenu menu
      DO Menu::EditMenu                                    ! Process menu items on ?EditMenu menu
      DO Menu::BrowseMenu                                  ! Process menu items on ?BrowseMenu menu
      DO Menu::Data_Menu                                   ! Process menu items on ?Data_Menu menu
      DO Menu::WindowMenu                                  ! Process menu items on ?WindowMenu menu
      DO Menu::HelpMenu                                    ! Process menu items on ?HelpMenu menu
    END
  ReturnValue = PARENT.TakeAccepted()
    CASE ACCEPTED()
    OF ?Search_CSV_BTN
      START(BrowseCSV_US_DB, 25000)
    OF ?Search_TPS_BTN
      START(BrowseSV_US_DB, 25000)
    OF ?Search_SQLite_BTN
      START(Search_SQLite, 25000)
    END
    RETURN ReturnValue
  END
  ReturnValue = Level:Fatal
  RETURN ReturnValue


ThisWindow.TakeEvent PROCEDURE

ReturnValue          BYTE,AUTO

Looped BYTE
  CODE
  LOOP                                                     ! This method receives all events
    IF Looped
      RETURN Level:Notify
    ELSE
      Looped = 1
    END
  IF EnterByTabManager.TakeEvent()
     RETURN(Level:Notify)
  END
  ReturnValue = PARENT.TakeEvent()
   !Something was triggered on the screen, Either EVENT:Accepted or EVENT:Selected
   CASE EVENT()
   OF EVENT:Accepted
    DO EntriesPosition_ROUTINE
    DO RebuildButtonz_ROUTINE
   END
    RETURN ReturnValue
  END
  ReturnValue = Level:Fatal
  RETURN ReturnValue

!!! <summary>
!!! Generated from procedure template - Window
!!! Browse the CSV_US_DB file
!!! </summary>
BrowseCSV_US_DB PROCEDURE 

CurrentTab           STRING(80)                            ! 
LOC:ResetBRW         LONG                                  ! 
LOC:SearchString     CSTRING(32)                           ! 
LOC:Match            BYTE                                  ! 
LOC:FoundIt          BYTE                                  ! 
BRW1::View:Browse    VIEW(CSV_US_DB)
                       PROJECT(CSV:RecordNumber)
                       PROJECT(CSV:Zipcode)
                       PROJECT(CSV:ZipCodeType)
                       PROJECT(CSV:City)
                       PROJECT(CSV:State)
                       PROJECT(CSV:Lat)
                       PROJECT(CSV:Long)
                       PROJECT(CSV:Country)
                       PROJECT(CSV:LocationText)
                     END
Queue:Browse:1       QUEUE                            !Queue declaration for browse/combo box using ?Browse:1
FuzzyResult7           LONG                           !List box control field - Fuzzy matcher result item
CSV:RecordNumber       LIKE(CSV:RecordNumber)         !List box control field - type derived from field
CSV:Zipcode            LIKE(CSV:Zipcode)              !List box control field - type derived from field
CSV:ZipCodeType        LIKE(CSV:ZipCodeType)          !List box control field - type derived from field
CSV:City               LIKE(CSV:City)                 !List box control field - type derived from field
CSV:State              LIKE(CSV:State)                !List box control field - type derived from field
CSV:Lat                LIKE(CSV:Lat)                  !List box control field - type derived from field
CSV:Long               LIKE(CSV:Long)                 !List box control field - type derived from field
CSV:Country            LIKE(CSV:Country)              !List box control field - type derived from field
CSV:LocationText       LIKE(CSV:LocationText)         !List box control field - type derived from field
Mark                   BYTE                           !Entry's marked status
ViewPosition           STRING(1024)                   !Entry's view position
                     END
LocEnableEnterByTab  BYTE(1)                               !Used by the ENTER Instead of Tab template
EnterByTabManager    EnterByTabClass
QuickWindow          WINDOW('Browse the CSV_US_DB file'),AT(,,670,369),FONT('Microsoft Sans Serif',8,00F5F5F5h, |
  FONT:regular,CHARSET:DEFAULT),RESIZE,MAXIMIZE,CENTER,COLOR(005E442Ch),GRAY,IMM,MDI,HLP('BrowseCSV_US_DB'), |
  SYSTEM,WALLPAPER('Noisea8.jpg')
                       LIST,AT(7,66,661,274),USE(?Browse:1),HVSCROLL,FORMAT('64R(2)|M~Record Number~C(0)@n-14@' & |
  '80L(2)|M~Zipcode~L(2)@s20@80L(2)|M~Zip Code Type~L(2)@s20@80L(2)|M~City~L(2)@s64@80L' & |
  '(2)|M~State~L(2)@s20@64R(2)|M~Lat~C(0)@n-14@64R(2)|M~Long~C(0)@n-14@80L(2)|M~Country' & |
  '~L(2)@s64@80L(2)|M~Location Text~L(2)@s64@'),FROM(Queue:Browse:1),IMM,MSG('Browsing t' & |
  'he CSV_US_DB file')
                       BUTTON('&Close'),AT(593,345,75,22),USE(?Close),LEFT,ICON('close_32x32.png'),FLAT,MSG('Close Window'), |
  TIP('Close Window')
                       GROUP('Neutrino Search'),AT(7,2,332),USE(?GROUP1),BOXED,TRN
                         BUTTON('Search'),AT(183,17),USE(?BUTTON1),LEFT,ICON('Search_32x32.png')
                         BUTTON('Reset Search'),AT(246,17),USE(?BUTTON2),LEFT,ICON('Command-Reset_32x32.png')
                         ENTRY(@s31),AT(49,23,129,10),USE(LOC:SearchString)
                         PROMPT('Search:'),AT(19,23),USE(?SearchString:Prompt),TRN
                       END
                       GROUP('Fu&zzy Search Options'),AT(343,2,325,50),USE(?FuzzyGroup),BOXED,TRN
                         ENTRY(@S255),AT(360,23,129,10),USE(?FuzzyQuery)
                         BUTTON('&Search'),AT(501,17,59,23),USE(?FuzzyGo),LEFT,ICON('Search_32x32.png')
                         BUTTON('&Clear'),AT(564,17,80,23),USE(?FuzzyClear),LEFT,ICON('Command-Reset_32x32.png')
                       END
                       BUTTON('&Insert'),AT(7,345,75,22),USE(?Insert),LEFT,ICON('insert_32x32.png')
                       BUTTON('&Change'),AT(85,345,75,22),USE(?Change),LEFT,ICON('edit_32x32.png')
                       BUTTON('&Delete'),AT(164,345,75,22),USE(?Delete),LEFT,ICON('delete_32x32.png')
                     END
FuzzyOrder7          BYTE,AUTO
FuzzyQuery7          STRING(255)
FuzzyResult7         LONG,AUTO

BRW1::AutoSizeColumn CLASS(AutoSizeColumnClassType)
               END
CoolEntryFields EntryFields
Buttonz CoolButtonz
ThisWindow           CLASS(WindowManager)
Init                   PROCEDURE(),BYTE,PROC,DERIVED
Kill                   PROCEDURE(),BYTE,PROC,DERIVED
Run                    PROCEDURE(USHORT Number,BYTE Request),BYTE,PROC,DERIVED
TakeAccepted           PROCEDURE(),BYTE,PROC,DERIVED
TakeEvent              PROCEDURE(),BYTE,PROC,DERIVED
                     END

Toolbar              ToolbarClass
BRW1                 CLASS(BrowseClass)                    ! Browse using ?Browse:1
Q                      &Queue:Browse:1                !Reference to browse queue
Init                   PROCEDURE(SIGNED ListBox,*STRING Posit,VIEW V,QUEUE Q,RelationManager RM,WindowManager WM)
ResetSort              PROCEDURE(BYTE Force),BYTE,PROC,DERIVED
SetQueueRecord         PROCEDURE(),DERIVED
ValidateRecord         PROCEDURE(),BYTE,DERIVED
                     END

BRW1::Sort0:StepClass StepClass                            ! Default Step Manager
Resizer              CLASS(WindowResizeClass)
Init                   PROCEDURE(BYTE AppStrategy=AppStrategy:Resize,BYTE SetWindowMinSize=False,BYTE SetWindowMaxSize=False)
WindowResizeEnd        PROCEDURE(),DERIVED
                     END


  CODE
  GlobalResponse = ThisWindow.Run()                        ! Opens the window and starts an Accept Loop

!---------------------------------------------------------------------------
DefineListboxStyle ROUTINE
!|
!| This routine create all the styles to be shared in this window
!| It`s called after the window open
!|
!---------------------------------------------------------------------------
InitEntries_ROUTINE    ROUTINE                !Pass Entry Fields to the Entries Class
 LOOP i#  = FirstField() TO LastField()
    IF i#{Prop:Type} = Create:Entry OR i#{Prop:Type} = Create:TEXT
      CoolEntryFields.AddEntryBG(i#,i#{PROP:Color},,GLO:2in1)
    END
 END !LOOP
EntriesPosition_ROUTINE     ROUTINE                !Re-draw the entries if the window changed
 LOOP i#  = FirstField() TO LastField()
    IF i#{Prop:Type} = Create:Entry OR i#{Prop:Type} = Create:TEXT
      CoolEntryFields.RefreshEntry(i#,GLO:2in1)
    END
 END !LOOP
ResizeButtons_ROUTINE ROUTINE
 LOOP ThisBTN#  = FirstField() TO LastField()
    IF ThisBTN#{Prop:Type} = Create:Button
      Buttonz.ResizeButtons(ThisBTN#,GLO:2in1)
    END
 END !LOOP
InitButtonz_ROUTINE    ROUTINE                !Pass buttons to the Cool Buttons Class
 LOOP ThisBTN#  = FirstField() TO LastField()
    IF ThisBTN#{Prop:Type} = Create:Button
      Buttonz.AddButtonMimic(ThisBTN#,ThisBTN#{PROP:Color},,GLO:2in1)
    END
 END !LOOP
RebuildButtonz_ROUTINE     ROUTINE                !Re-draw the buttons if the window changed
 LOOP ThisBTN#  = FirstField() TO LastField()
    IF ThisBTN#{Prop:Type} = Create:Button
      Buttonz.RebuildButtons(ThisBTN#,GLO:2in1)
    END
 END !LOOP
ButtonzPosition_ROUTINE     ROUTINE     	!Re-draw the buttons if the window changed in 2 in 1 mode, Legacy, it should dissapear										
 DO Enable2in1Buttons_ROUTINE
Enable2in1Buttons_ROUTINE ROUTINE     	!Re-draw the buttons if the window changed in 2 in 1 mode
 LOOP ThisBTN#  = FirstField() TO LastField()
    IF ThisBTN#{Prop:Type} = Create:Button
      Buttonz.Buttons2in1(ThisBTN#,GLO:2in1)
    END
 END !LOOP
SetThemeWallpaper_ROUTINE ROUTINE

ThisWindow.Init PROCEDURE

ReturnValue          BYTE,AUTO

  CODE
  GlobalErrors.SetProcedureName('BrowseCSV_US_DB')
  SELF.Request = GlobalRequest                             ! Store the incoming request
  ReturnValue = PARENT.Init()
  IF ReturnValue THEN RETURN ReturnValue.
  SELF.FirstField = ?Browse:1
  SELF.VCRRequest &= VCRRequest
  SELF.Errors &= GlobalErrors                              ! Set this windows ErrorManager to the global ErrorManager
  BIND('FuzzyMatch',FuzzyMatch)                            ! Added by: BrowseFuzzyMatching(ABC)
  BIND('FuzzyResult7',FuzzyResult7)                        ! Added by: BrowseBox(ABC)
  BIND('FuzzyQuery7',FuzzyQuery7)                          ! Added by: BrowseFuzzyMatching(ABC)
  CLEAR(GlobalRequest)                                     ! Clear GlobalRequest after storing locally
  CLEAR(GlobalResponse)
  SELF.AddItem(Toolbar)
  IF SELF.Request = SelectRecord
     SELF.AddItem(?Close,RequestCancelled)                 ! Add the close control to the window manger
  ELSE
     SELF.AddItem(?Close,RequestCompleted)                 ! Add the close control to the window manger
  END
  Relate:CSV_US_DB.Open()                                  ! File CSV_US_DB used by this procedure, so make sure it's RelationManager is open
  SELF.FilesOpened = True
  BRW1.Init(?Browse:1,Queue:Browse:1.ViewPosition,BRW1::View:Browse,Queue:Browse:1,Relate:CSV_US_DB,SELF) ! Initialize the browse manager
  SELF.Open(QuickWindow)                                   ! Open window
  !Setting the LineHeight for every control of type LIST/DROP or COMBO in the window using the global setting.
  ?Browse:1{PROP:LineHeight} = 15
  Do DefineListboxStyle
  BRW1.Q &= Queue:Browse:1
  BRW1.AddSortOrder(,)                                     ! Add the sort order for  for sort order 1
  BRW1.AddResetField(LOC:ResetBRW)                         ! Apply the reset field
  BRW1.AddField(FuzzyResult7, BRW1.Q.FuzzyResult7)
  BRW1.AddField(CSV:RecordNumber,BRW1.Q.CSV:RecordNumber)  ! Field CSV:RecordNumber is a hot field or requires assignment from browse
  BRW1.AddField(CSV:Zipcode,BRW1.Q.CSV:Zipcode)            ! Field CSV:Zipcode is a hot field or requires assignment from browse
  BRW1.AddField(CSV:ZipCodeType,BRW1.Q.CSV:ZipCodeType)    ! Field CSV:ZipCodeType is a hot field or requires assignment from browse
  BRW1.AddField(CSV:City,BRW1.Q.CSV:City)                  ! Field CSV:City is a hot field or requires assignment from browse
  BRW1.AddField(CSV:State,BRW1.Q.CSV:State)                ! Field CSV:State is a hot field or requires assignment from browse
  BRW1.AddField(CSV:Lat,BRW1.Q.CSV:Lat)                    ! Field CSV:Lat is a hot field or requires assignment from browse
  BRW1.AddField(CSV:Long,BRW1.Q.CSV:Long)                  ! Field CSV:Long is a hot field or requires assignment from browse
  BRW1.AddField(CSV:Country,BRW1.Q.CSV:Country)            ! Field CSV:Country is a hot field or requires assignment from browse
  BRW1.AddField(CSV:LocationText,BRW1.Q.CSV:LocationText)  ! Field CSV:LocationText is a hot field or requires assignment from browse
  Resizer.Init(AppStrategy:Resize,Resize:SetMinSize)       ! Controls will change size as the window gets bigger
  SELF.AddItem(Resizer)                                    ! Add resizer to window manager
  ?FuzzyQuery{PROP:Use} = FuzzyQuery7
  FuzzyOrder7 = BRW1.AddSortOrder()
  BRW1.AppendOrder('200-FuzzyMatch(FuzzyQuery7,CSV:RecordNumber&'' ''&CSV:Zipcode&'' ''&CSV:ZipCodeType&'' ''&CSV:City&'' ''&CSV:State&'' ''&CSV:Lat&'' ''&CSV:Long&'' ''&CSV:Country&'' ''&CSV:LocationText)')
  BRW1.SetFilter('FuzzyMatch(FuzzyQuery7,CSV:RecordNumber&'' ''&CSV:Zipcode&'' ''&CSV:ZipCodeType&'' ''&CSV:City&'' ''&CSV:State&'' ''&CSV:Lat&'' ''&CSV:Long&'' ''&CSV:Country&'' ''&CSV:LocationText)>=50', 'FuzzyFilter')
  ?Browse:1{PROP:Format} = '48C(2)|M~Match Result~L(1)@n_3@' & ?Browse:1{PROP:Format}
  BRW1.AskProcedure = 1                                    ! Will call: Update_CSV
  BRW1.AddToolbarTarget(Toolbar)                           ! Browse accepts toolbar control
  SELF.SetAlerts()
   CoolEntryFields.Init(SELF)
   DO InitEntries_ROUTINE
   Buttonz.Init(SELF)
   DO InitButtonz_ROUTINE
  BRW1::AutoSizeColumn.Init()
  BRW1::AutoSizeColumn.AddListBox(?Browse:1,Queue:Browse:1)
  EnterByTabManager.Init(False)
  RETURN ReturnValue


ThisWindow.Kill PROCEDURE

ReturnValue          BYTE,AUTO

  CODE
  ReturnValue = PARENT.Kill()
  IF ReturnValue THEN RETURN ReturnValue.
  IF SELF.FilesOpened
    Relate:CSV_US_DB.Close()
  END
  BRW1::AutoSizeColumn.Kill()
  GlobalErrors.SetProcedureName
  RETURN ReturnValue


ThisWindow.Run PROCEDURE(USHORT Number,BYTE Request)

ReturnValue          BYTE,AUTO

  CODE
  ReturnValue = PARENT.Run(Number,Request)
  IF SELF.Request = ViewRecord
    ReturnValue = RequestCancelled                         ! Always return RequestCancelled if the form was opened in ViewRecord mode
  ELSE
    GlobalRequest = Request
    Update_CSV
    ReturnValue = GlobalResponse
  END
  RETURN ReturnValue


ThisWindow.TakeAccepted PROCEDURE

ReturnValue          BYTE,AUTO

Looped BYTE
  CODE
  LOOP                                                     ! This method receive all EVENT:Accepted's
    IF Looped
      RETURN Level:Notify
    ELSE
      Looped = 1
    END
  ReturnValue = PARENT.TakeAccepted()
    CASE ACCEPTED()
    OF ?BUTTON1
      ThisWindow.Update()
      LOC:ResetBRW += 1
    OF ?BUTTON2
      ThisWindow.Update()
      LOC:SearchString = ''
      
      ThisWindow.RESET(1)
      
      POST(EVENT:ScrollTop,?Browse:1)
    OF ?FuzzyGo
      ThisWindow.Update()
      BRW1.ResetSort(1)
    OF ?FuzzyClear
      ThisWindow.Update()
      BRW1.ResetSort(1)
    END
    RETURN ReturnValue
  END
  ReturnValue = Level:Fatal
  RETURN ReturnValue


ThisWindow.TakeEvent PROCEDURE

ReturnValue          BYTE,AUTO

Looped BYTE
  CODE
  LOOP                                                     ! This method receives all events
    IF Looped
      RETURN Level:Notify
    ELSE
      Looped = 1
    END
  IF EnterByTabManager.TakeEvent()
     RETURN(Level:Notify)
  END
  IF BRW1::AutoSizeColumn.TakeEvents()
     RETURN Level:Notify
  END
  ReturnValue = PARENT.TakeEvent()
   !Something was triggered on the screen, Either EVENT:Accepted or EVENT:Selected
   CASE EVENT()
   OF EVENT:Accepted
    DO EntriesPosition_ROUTINE
    DO RebuildButtonz_ROUTINE
   END
    RETURN ReturnValue
  END
  ReturnValue = Level:Fatal
  RETURN ReturnValue


BRW1.Init PROCEDURE(SIGNED ListBox,*STRING Posit,VIEW V,QUEUE Q,RelationManager RM,WindowManager WM)

  CODE
  PARENT.Init(ListBox,Posit,V,Q,RM,WM)
  IF WM.Request <> ViewRecord                              ! If called for anything other than ViewMode, make the insert, change & delete controls available
    SELF.InsertControl=?Insert
    SELF.ChangeControl=?Change
    SELF.DeleteControl=?Delete
  END


BRW1.ResetSort PROCEDURE(BYTE Force)

ReturnValue          BYTE,AUTO

FuzzyMatched         BYTE(False),STATIC
  CODE
  IF EVENT() = EVENT:Accepted
    CASE ACCEPTED()
    OF ?FuzzyGo
      IF FuzzyQuery7
        FuzzyMatched = True
        RETURN SELF.SetSort(FuzzyOrder7, Force)
      END
    OF ?FuzzyClear
      FuzzyMatched = False
      CLEAR(FuzzyQuery7)
    END
  END
  IF FuzzyMatched = True THEN RETURN PARENT.ResetSort(Force).
  RETURN SELF.SetSort(1,Force)
  ReturnValue = PARENT.ResetSort(Force)
  RETURN ReturnValue


BRW1.SetQueueRecord PROCEDURE

  CODE
  FuzzyResult7 = FuzzyMatch(FuzzyQuery7,CSV:RecordNumber&' '&CSV:Zipcode&' '&CSV:ZipCodeType&' '&CSV:City&' '&CSV:State&' '&CSV:Lat&' '&CSV:Long&' '&CSV:Country&' '&CSV:LocationText)
  PARENT.SetQueueRecord


BRW1.ValidateRecord PROCEDURE

ReturnValue          BYTE,AUTO

BRW1::RecordStatus   BYTE,AUTO
  CODE
  ReturnValue = PARENT.ValidateRecord()
  LOC:Match = FALSE
  
  IF LOC:SearchString <> ''
  	
  	ReturnValue=Record:Filtered
  
  	IF LOC:Match = FALSE
  		LOC:Match = Fast.Find(LOC:SearchString,CSV:RecordNumber)
  		IF LOC:Match
  			ReturnValue=Record:Ok
  
  		END			
  	END			
  
  	IF LOC:Match = FALSE
  				
  		LOC:Match = Fast.Find(LOC:SearchString,CSV:Zipcode)
  		IF LOC:Match
  			ReturnValue=Record:Ok
  
  		END			
  	END			
  			
  	IF LOC:Match = FALSE
  		LOC:Match = Fast.Find(LOC:SearchString,CSV:ZipCodeType)
  		IF LOC:Match
  			ReturnValue=Record:Ok
  
  		END			
  	END			
  			
  	IF LOC:Match = FALSE
  		LOC:Match = Fast.Find(LOC:SearchString,CSV:City)
  		IF LOC:Match
  			ReturnValue=Record:Ok
  
  		END			
  	END		
  	
  	
  	IF LOC:Match = FALSE
  		LOC:Match = Fast.Find(LOC:SearchString,CSV:State)
  		IF LOC:Match
  			ReturnValue=Record:Ok
  
  		END			
  	END	
  	
  	IF LOC:Match = FALSE
  		LOC:Match = Fast.Find(LOC:SearchString,CSV:Lat)
  		IF LOC:Match
  			ReturnValue=Record:Ok
  
  		END			
  	END	
  		
  	IF LOC:Match = FALSE
  		LOC:Match = Fast.Find(LOC:SearchString,CSV:Location)
  		IF LOC:Match
  			ReturnValue=Record:Ok
  
  		END			
  	END	
  		
  	IF LOC:Match = FALSE
  		LOC:Match = Fast.Find(LOC:SearchString,CSV:Long)
  		IF LOC:Match
  			ReturnValue=Record:Ok
  
  		END			
  	END	
  		
  	IF LOC:Match = FALSE
  		LOC:Match = Fast.Find(LOC:SearchString,CSV:Country)
  		IF LOC:Match
  			ReturnValue=Record:Ok
  
  		END			
  	END	
  		
  	IF LOC:Match = FALSE
  		LOC:Match = Fast.Find(LOC:SearchString,CSV:LocationText)
  		IF LOC:Match
  			ReturnValue=Record:Ok
  
  		END			
  	END	
  !============================== For Bullet Path ==============================
  !	LOC:Increment += 1
  !
  !	IF LOC:Increment  => LOC:Step
  !		LOC:Increment = 0
  !		?BOX1{PROP:Width} = ?BOX1{PROP:Width} + (LOC:GaugeSize / 200)
  !		DISPLAY(?BOX1)
  !	END
  
  	
  END	
  !END	
  
  	
  BRW1::RecordStatus=ReturnValue
  RETURN ReturnValue


Resizer.Init PROCEDURE(BYTE AppStrategy=AppStrategy:Resize,BYTE SetWindowMinSize=False,BYTE SetWindowMaxSize=False)


  CODE
  PARENT.Init(AppStrategy,SetWindowMinSize,SetWindowMaxSize)
  SELF.SetParentDefaults()                                 ! Calculate default control parent-child relationships based upon their positions on the window
  SELF.SetStrategy(?FuzzyGroup, Resize:FixNearestX + Resize:FixNearestY, Resize:LockSize)


Resizer.WindowResizeEnd PROCEDURE


  CODE
  PARENT.WindowResizeEnd
   DO EntriesPosition_ROUTINE
   DO ResizeButtons_ROUTINE

!!! <summary>
!!! Generated from procedure template - Window
!!! Browse the SV_US_DB file
!!! </summary>
BrowseSV_US_DB PROCEDURE 

CurrentTab           STRING(80)                            ! 
LOC:FileRecords      LONG                                  ! 
LOC:Step             LONG                                  ! 
LOC:SearchMSG        CSTRING(64)                           ! 
LOC:Match            BYTE                                  ! 
LOC:ResetBRW         LONG                                  ! 
LOC:SearchString     CSTRING(32)                           ! 
LOC:FoundIt          BYTE                                  ! 
LOC:Increment        LONG                                  ! 
LOC:GaugeSize        LONG                                  ! 
BRW1::View:Browse    VIEW(SV_US_DB)
                       PROJECT(SV_:RecordNumber)
                       PROJECT(SV_:Zipcode)
                       PROJECT(SV_:ZipCodeType)
                       PROJECT(SV_:City)
                       PROJECT(SV_:State)
                       PROJECT(SV_:Lat)
                       PROJECT(SV_:Long)
                       PROJECT(SV_:Country)
                       PROJECT(SV_:LocationText)
                     END
Queue:Browse:1       QUEUE                            !Queue declaration for browse/combo box using ?Browse:1
FuzzyResult7           LONG                           !List box control field - Fuzzy matcher result item
SV_:RecordNumber       LIKE(SV_:RecordNumber)         !List box control field - type derived from field
SV_:Zipcode            LIKE(SV_:Zipcode)              !List box control field - type derived from field
SV_:ZipCodeType        LIKE(SV_:ZipCodeType)          !List box control field - type derived from field
SV_:City               LIKE(SV_:City)                 !List box control field - type derived from field
SV_:State              LIKE(SV_:State)                !List box control field - type derived from field
SV_:Lat                LIKE(SV_:Lat)                  !List box control field - type derived from field
SV_:Long               LIKE(SV_:Long)                 !List box control field - type derived from field
SV_:Country            LIKE(SV_:Country)              !List box control field - type derived from field
SV_:LocationText       LIKE(SV_:LocationText)         !List box control field - type derived from field
Mark                   BYTE                           !Entry's marked status
ViewPosition           STRING(1024)                   !Entry's view position
                     END
LocEnableEnterByTab  BYTE(1)                               !Used by the ENTER Instead of Tab template
EnterByTabManager    EnterByTabClass
QuickWindow          WINDOW('US ZIP Codes, TPS'),AT(,,669,346),FONT('IBM Plex Sans',8,,FONT:regular,CHARSET:DEFAULT), |
  RESIZE,MAXIMIZE,CENTER,COLOR(00291C18h),GRAY,IMM,MDI,HLP('BrowseSV_US_DB'),SYSTEM
                       LIST,AT(6,39,657,278),USE(?Browse:1),FONT(,,00BBA98Bh),VSCROLL,COLOR(0030201Ch,COLOR:White), |
  GRID(00413528h),FLAT,FORMAT('64C(2)|M~Record Number~C(0)@n-14@80C(2)|M~Zipcode~@s20@8' & |
  '0C(2)|M~Zip Code Type~@s20@80L(2)|M~City~C(2)@s64@80C(2)|M~State~@s20@64C(2)|M~Lat~C' & |
  '(0)@n-14@64C(2)|M~Long~C(0)@n-14@80C(2)|M~Country~@s64@80C(2)|M~Location Text~@s64@'),FROM(Queue:Browse:1), |
  IMM,MSG('Browsing the SV_US_DB file')
                       BUTTON('&Close'),AT(601,321,67,19),USE(?Close),LEFT,FLAT,MSG('Close Window'),TIP('Close Window'), |
  TRN
                       GROUP('Neutrino Search'),AT(7,4,317,6),USE(?GROUP1)
                         BUTTON,AT(203,16,62,18),USE(?Neutrino_Search),ICON('.\resources\search.png'),FLAT,TRN
                         BUTTON,AT(269,16,67,19),USE(?Clear_NSearch),ICON('.\resources\clear_search.png'),FLAT,TRN
                         ENTRY(@s31),AT(46,21,153,10),USE(LOC:SearchString),FONT(,,00DBD5D2h),TRN
                         PROMPT('Search:'),AT(16,21),USE(?SearchString:Prompt)
                         BOX,AT(46,35,231,3),USE(?BOX1),FILL(0000C000h),HIDE,LINEWIDTH(1)
                         BOX,AT(45,29,154,1),USE(?BOX_Neutrino_Search),FILL(COLOR:Lime),LINEWIDTH(1)
                       END
                       GROUP('Fu&zzy Search Options'),AT(350,4,317,6),USE(?FuzzyGroup)
                         ENTRY(@S255),AT(381,21,153,10),USE(?FuzzyQuery),FONT(,,00DBD5D2h)
                         BUTTON('&Search'),AT(540,16,67,19),USE(?FuzzyGo)
                         BUTTON('&Clear'),AT(604,16,67,19),USE(?FuzzyClear)
                       END
                     END
FuzzyOrder7          BYTE,AUTO
FuzzyQuery7          STRING(255)
FuzzyResult7         LONG,AUTO

BRW1::AutoSizeColumn CLASS(AutoSizeColumnClassType)
               END
ThisWindow           CLASS(WindowManager)
Init                   PROCEDURE(),BYTE,PROC,DERIVED
Kill                   PROCEDURE(),BYTE,PROC,DERIVED
TakeAccepted           PROCEDURE(),BYTE,PROC,DERIVED
TakeEvent              PROCEDURE(),BYTE,PROC,DERIVED
TakeWindowEvent        PROCEDURE(),BYTE,PROC,DERIVED
                     END

Toolbar              ToolbarClass
BRW1                 CLASS(BrowseClass)                    ! Browse using ?Browse:1
Q                      &Queue:Browse:1                !Reference to browse queue
ResetSort              PROCEDURE(BYTE Force),BYTE,PROC,DERIVED
SetQueueRecord         PROCEDURE(),DERIVED
ValidateRecord         PROCEDURE(),BYTE,DERIVED
                     END

BRW1::Sort0:Locator  StepLocatorClass                      ! Default Locator
BRW1::Sort0:StepClass StepLongClass                        ! Default Step Manager
Resizer              CLASS(WindowResizeClass)
Init                   PROCEDURE(BYTE AppStrategy=AppStrategy:Resize,BYTE SetWindowMinSize=False,BYTE SetWindowMaxSize=False)
WindowResizeEnd        PROCEDURE(),DERIVED
                     END


  CODE
  GlobalResponse = ThisWindow.Run()                        ! Opens the window and starts an Accept Loop

!---------------------------------------------------------------------------
DefineListboxStyle ROUTINE
!|
!| This routine create all the styles to be shared in this window
!| It`s called after the window open
!|
!---------------------------------------------------------------------------
Custom_Resize_ROUTINE       ROUTINE
	
	?BOX_Neutrino_Search{PROP:Xpos} = ?LOC:SearchString{PROP:Xpos}
	?BOX_Neutrino_Search{PROP:Ypos} = ?LOC:SearchString{PROP:Ypos} + ?LOC:SearchString{PROP:Height}
	?BOX_Neutrino_Search{PROP:Width} = ?LOC:SearchString{PROP:Width}
	

	
	BRW_Width# = ?Browse:1{PROP:Width}
	?Browse:1{PROPLIST:width,1} = BRW_Width# * 0.1
	?Browse:1{PROPLIST:width,2} = BRW_Width# * 0.08
	?Browse:1{PROPLIST:width,3} = BRW_Width# * 0.10
	?Browse:1{PROPLIST:width,4} = BRW_Width# * 0.11
	?Browse:1{PROPLIST:width,5} = BRW_Width# * 0.20
	?Browse:1{PROPLIST:width,6} = BRW_Width# * 0.08
	?Browse:1{PROPLIST:width,7} = BRW_Width# * 0.08
	?Browse:1{PROPLIST:width,8} = BRW_Width# * 0.08
	?Browse:1{PROPLIST:width,9} = BRW_Width# * 0.05
	?Browse:1{PROPLIST:width,10} = BRW_Width# * 0.05
	
	EXIT

ThisWindow.Init PROCEDURE

ReturnValue          BYTE,AUTO

  CODE
  GlobalErrors.SetProcedureName('BrowseSV_US_DB')
  SELF.Request = GlobalRequest                             ! Store the incoming request
  ReturnValue = PARENT.Init()
  IF ReturnValue THEN RETURN ReturnValue.
  SELF.FirstField = ?Browse:1
  SELF.VCRRequest &= VCRRequest
  SELF.Errors &= GlobalErrors                              ! Set this windows ErrorManager to the global ErrorManager
  BIND('FuzzyMatch',FuzzyMatch)                            ! Added by: BrowseFuzzyMatching(ABC)
  BIND('FuzzyResult7',FuzzyResult7)                        ! Added by: BrowseBox(ABC)
  BIND('FuzzyQuery7',FuzzyQuery7)                          ! Added by: BrowseFuzzyMatching(ABC)
  CLEAR(GlobalRequest)                                     ! Clear GlobalRequest after storing locally
  CLEAR(GlobalResponse)
  SELF.AddItem(Toolbar)
  IF SELF.Request = SelectRecord
     SELF.AddItem(?Close,RequestCancelled)                 ! Add the close control to the window manger
  ELSE
     SELF.AddItem(?Close,RequestCompleted)                 ! Add the close control to the window manger
  END
  Relate:SV_US_DB.Open()                                   ! File SV_US_DB used by this procedure, so make sure it's RelationManager is open
  SELF.FilesOpened = True
  BRW1.Init(?Browse:1,Queue:Browse:1.ViewPosition,BRW1::View:Browse,Queue:Browse:1,Relate:SV_US_DB,SELF) ! Initialize the browse manager
  SELF.Open(QuickWindow)                                   ! Open window
  !Setting the LineHeight for every control of type LIST/DROP or COMBO in the window using the global setting.
  ?Browse:1{PROP:LineHeight} = 15
  Do DefineListboxStyle
  BRW1.Q &= Queue:Browse:1
  BRW1::Sort0:StepClass.Init(+ScrollSort:AllowAlpha)       ! Moveable thumb based upon SV_:RecordNumber for sort order 1
  BRW1.AddSortOrder(BRW1::Sort0:StepClass,SV_:Record_Number_Key) ! Add the sort order for SV_:Record_Number_Key for sort order 1
  BRW1.AddLocator(BRW1::Sort0:Locator)                     ! Browse has a locator for sort order 1
  BRW1::Sort0:Locator.Init(,SV_:RecordNumber,1,BRW1)       ! Initialize the browse locator using  using key: SV_:Record_Number_Key , SV_:RecordNumber
  BRW1.AddResetField(LOC:ResetBRW)                         ! Apply the reset field
  BRW1.AddField(FuzzyResult7, BRW1.Q.FuzzyResult7)
  BRW1.AddField(SV_:RecordNumber,BRW1.Q.SV_:RecordNumber)  ! Field SV_:RecordNumber is a hot field or requires assignment from browse
  BRW1.AddField(SV_:Zipcode,BRW1.Q.SV_:Zipcode)            ! Field SV_:Zipcode is a hot field or requires assignment from browse
  BRW1.AddField(SV_:ZipCodeType,BRW1.Q.SV_:ZipCodeType)    ! Field SV_:ZipCodeType is a hot field or requires assignment from browse
  BRW1.AddField(SV_:City,BRW1.Q.SV_:City)                  ! Field SV_:City is a hot field or requires assignment from browse
  BRW1.AddField(SV_:State,BRW1.Q.SV_:State)                ! Field SV_:State is a hot field or requires assignment from browse
  BRW1.AddField(SV_:Lat,BRW1.Q.SV_:Lat)                    ! Field SV_:Lat is a hot field or requires assignment from browse
  BRW1.AddField(SV_:Long,BRW1.Q.SV_:Long)                  ! Field SV_:Long is a hot field or requires assignment from browse
  BRW1.AddField(SV_:Country,BRW1.Q.SV_:Country)            ! Field SV_:Country is a hot field or requires assignment from browse
  BRW1.AddField(SV_:LocationText,BRW1.Q.SV_:LocationText)  ! Field SV_:LocationText is a hot field or requires assignment from browse
  Resizer.Init(AppStrategy:Resize,Resize:SetMinSize)       ! Controls will change size as the window gets bigger
  SELF.AddItem(Resizer)                                    ! Add resizer to window manager
  ?FuzzyQuery{PROP:Use} = FuzzyQuery7
  FuzzyOrder7 = BRW1.AddSortOrder()
  BRW1.AppendOrder('200-FuzzyMatch(FuzzyQuery7,SV_:RecordNumber&'' ''&SV_:Zipcode&'' ''&SV_:ZipCodeType&'' ''&SV_:City&'' ''&SV_:State&'' ''&SV_:Lat&'' ''&SV_:Long&'' ''&SV_:Country&'' ''&SV_:LocationText)')
  BRW1.SetFilter('FuzzyMatch(FuzzyQuery7,SV_:RecordNumber&'' ''&SV_:Zipcode&'' ''&SV_:ZipCodeType&'' ''&SV_:City&'' ''&SV_:State&'' ''&SV_:Lat&'' ''&SV_:Long&'' ''&SV_:Country&'' ''&SV_:LocationText)>=50', 'FuzzyFilter')
  ?Browse:1{PROP:Format} = '48C(2)|M~Match Result~L(1)@n_3@' & ?Browse:1{PROP:Format}
  SELF.SetAlerts()
  BRW1::AutoSizeColumn.Init()
  BRW1::AutoSizeColumn.AddListBox(?Browse:1,Queue:Browse:1)
  EnterByTabManager.Init(False)
  RETURN ReturnValue


ThisWindow.Kill PROCEDURE

ReturnValue          BYTE,AUTO

  CODE
  ReturnValue = PARENT.Kill()
  IF ReturnValue THEN RETURN ReturnValue.
  IF SELF.FilesOpened
    Relate:SV_US_DB.Close()
  END
  BRW1::AutoSizeColumn.Kill()
  GlobalErrors.SetProcedureName
  RETURN ReturnValue


ThisWindow.TakeAccepted PROCEDURE

ReturnValue          BYTE,AUTO

Looped BYTE
  CODE
  LOOP                                                     ! This method receive all EVENT:Accepted's
    IF Looped
      RETURN Level:Notify
    ELSE
      Looped = 1
    END
  ReturnValue = PARENT.TakeAccepted()
    CASE ACCEPTED()
    OF ?Neutrino_Search
      ThisWindow.Update()
      LOC:FileRecords = RECORDS(SV_US_DB)
      LOC:Step = INT( LOC:FileRecords / 100 )
      
      LOC:ResetBRW += 1
    OF ?Clear_NSearch
      ThisWindow.Update()
      LOC:SearchString = ''
      
      ThisWindow.RESET(1)
      
      POST(EVENT:ScrollTop,?Browse:1)
    OF ?LOC:SearchString
      POST(EVENT:Accepted,?Neutrino_Search)
    OF ?FuzzyGo
      ThisWindow.Update()
      BRW1.ResetSort(1)
    OF ?FuzzyClear
      ThisWindow.Update()
      BRW1.ResetSort(1)
    END
    RETURN ReturnValue
  END
  ReturnValue = Level:Fatal
  RETURN ReturnValue


ThisWindow.TakeEvent PROCEDURE

ReturnValue          BYTE,AUTO

Looped BYTE
  CODE
  LOOP                                                     ! This method receives all events
    IF Looped
      RETURN Level:Notify
    ELSE
      Looped = 1
    END
  IF EnterByTabManager.TakeEvent()
     RETURN(Level:Notify)
  END
  IF BRW1::AutoSizeColumn.TakeEvents()
     RETURN Level:Notify
  END
  ReturnValue = PARENT.TakeEvent()
    RETURN ReturnValue
  END
  ReturnValue = Level:Fatal
  RETURN ReturnValue


ThisWindow.TakeWindowEvent PROCEDURE

ReturnValue          BYTE,AUTO

Looped BYTE
  CODE
  LOOP                                                     ! This method receives all window specific events
    IF Looped
      RETURN Level:Notify
    ELSE
      Looped = 1
    END
    CASE EVENT()
    OF EVENT:OpenWindow
      LOC:GaugeSize = ?BOX1{PROP:Width}
      ?BOX1{PROP:Width} = 0
    END
  ReturnValue = PARENT.TakeWindowEvent()
    RETURN ReturnValue
  END
  ReturnValue = Level:Fatal
  RETURN ReturnValue


BRW1.ResetSort PROCEDURE(BYTE Force)

ReturnValue          BYTE,AUTO

FuzzyMatched         BYTE(False),STATIC
  CODE
  IF EVENT() = EVENT:Accepted
    CASE ACCEPTED()
    OF ?FuzzyGo
      IF FuzzyQuery7
        FuzzyMatched = True
        RETURN SELF.SetSort(FuzzyOrder7, Force)
      END
    OF ?FuzzyClear
      FuzzyMatched = False
      CLEAR(FuzzyQuery7)
    END
  END
  IF FuzzyMatched = True THEN RETURN PARENT.ResetSort(Force).
  RETURN SELF.SetSort(1,Force)
  ReturnValue = PARENT.ResetSort(Force)
  RETURN ReturnValue


BRW1.SetQueueRecord PROCEDURE

  CODE
  FuzzyResult7 = FuzzyMatch(FuzzyQuery7,SV_:RecordNumber&' '&SV_:Zipcode&' '&SV_:ZipCodeType&' '&SV_:City&' '&SV_:State&' '&SV_:Lat&' '&SV_:Long&' '&SV_:Country&' '&SV_:LocationText)
  PARENT.SetQueueRecord


BRW1.ValidateRecord PROCEDURE

ReturnValue          BYTE,AUTO

BRW1::RecordStatus   BYTE,AUTO
  CODE
  ReturnValue = PARENT.ValidateRecord()
  LOC:Match = FALSE
  
  IF LOC:SearchString <> ''
  	
  	ReturnValue=Record:Filtered
  
  	IF LOC:Match = FALSE
  		LOC:Match = Fast.Find(LOC:SearchString,SV_:RecordNumber)
  		IF LOC:Match
  			ReturnValue=Record:Ok
  
  		END			
  	END			
  
  	IF LOC:Match = FALSE
  				
  		LOC:Match = Fast.Find(LOC:SearchString,SV_:Zipcode)
  		IF LOC:Match
  			ReturnValue=Record:Ok
  
  		END			
  	END			
  			
  	IF LOC:Match = FALSE
  		LOC:Match = Fast.Find(LOC:SearchString,SV_:ZipCodeType)
  		IF LOC:Match
  			ReturnValue=Record:Ok
  
  		END			
  	END			
  			
  	IF LOC:Match = FALSE
  		LOC:Match = Fast.Find(LOC:SearchString,SV_:City)
  		IF LOC:Match
  			ReturnValue=Record:Ok
  
  		END			
  	END		
  	
  	
  	IF LOC:Match = FALSE
  		LOC:Match = Fast.Find(LOC:SearchString,SV_:State)
  		IF LOC:Match
  			ReturnValue=Record:Ok
  
  		END			
  	END	
  	
  	IF LOC:Match = FALSE
  		LOC:Match = Fast.Find(LOC:SearchString,SV_:Lat)
  		IF LOC:Match
  			ReturnValue=Record:Ok
  
  		END			
  	END	
  		
  	IF LOC:Match = FALSE
  		LOC:Match = Fast.Find(LOC:SearchString,SV_:Location)
  		IF LOC:Match
  			ReturnValue=Record:Ok
  
  		END			
  	END	
  		
  	IF LOC:Match = FALSE
  		LOC:Match = Fast.Find(LOC:SearchString,SV_:Long)
  		IF LOC:Match
  			ReturnValue=Record:Ok
  
  		END			
  	END	
  		
  	IF LOC:Match = FALSE
  		LOC:Match = Fast.Find(LOC:SearchString,SV_:Country)
  		IF LOC:Match
  			ReturnValue=Record:Ok
  
  		END			
  	END	
  		
  	IF LOC:Match = FALSE
  		LOC:Match = Fast.Find(LOC:SearchString,SV_:LocationText)
  		IF LOC:Match
  			ReturnValue=Record:Ok
  
  		END			
  	END	
  !============================== For Bullet Path ==============================
  !	LOC:Increment += 1
  !
  !	IF LOC:Increment  => LOC:Step
  !		LOC:Increment = 0
  !		?BOX1{PROP:Width} = ?BOX1{PROP:Width} + (LOC:GaugeSize / 200)
  !		DISPLAY(?BOX1)
  !	END
  
  	
  END	
  !END	
  
  	
  BRW1::RecordStatus=ReturnValue
  RETURN ReturnValue


Resizer.Init PROCEDURE(BYTE AppStrategy=AppStrategy:Resize,BYTE SetWindowMinSize=False,BYTE SetWindowMaxSize=False)


  CODE
  PARENT.Init(AppStrategy,SetWindowMinSize,SetWindowMaxSize)
  SELF.SetParentDefaults()                                 ! Calculate default control parent-child relationships based upon their positions on the window
  SELF.SetStrategy(?FuzzyGroup, Resize:FixNearestX + Resize:FixNearestY, Resize:LockSize)


Resizer.WindowResizeEnd PROCEDURE


  CODE
  PARENT.WindowResizeEnd
  DO Custom_Resize_ROUTINE 

!!! <summary>
!!! Generated from procedure template - Window
!!! Browse the SQLite_US_DB file
!!! </summary>
Search_SQLite PROCEDURE 

CurrentTab           STRING(80)                            ! 
LOC:SearchMode       CSTRING('''verywhere''<0>{20}')       ! 
SearchGRP            GROUP,PRE(SRCH)                       ! 
RecordNumber         CSTRING(64)                           ! 
ZIPCode              CSTRING(64)                           ! 
ZipCodeType          CSTRING(64)                           ! 
City                 CSTRING(64)                           ! 
State                CSTRING(64)                           ! 
Lat                  CSTRING(64)                           ! 
Long                 CSTRING(64)                           ! 
Country              CSTRING(64)                           ! 
LocationText         CSTRING(64)                           ! 
                     END                                   ! 
LOC:Match            BYTE                                  ! 
LOC:SearchGRPByte    BYTE                                  ! 
LOC:ResetBRW         LONG                                  ! 
LOC:SearchString     CSTRING(32)                           ! 
LOC:FoundIt          BYTE                                  ! 
LOC:MatchCount       LONG                                  ! 
BRW1::View:Browse    VIEW(SQLiteUSDB)
                       PROJECT(SQLT:RecordNumber)
                       PROJECT(SQLT:Zipcode)
                       PROJECT(SQLT:ZipCodeType)
                       PROJECT(SQLT:City)
                       PROJECT(SQLT:State)
                       PROJECT(SQLT:Lat)
                       PROJECT(SQLT:Long)
                       PROJECT(SQLT:Country)
                       PROJECT(SQLT:LocationText)
                     END
Queue:Browse:1       QUEUE                            !Queue declaration for browse/combo box using ?Browse:1
SQLT:RecordNumber      LIKE(SQLT:RecordNumber)        !List box control field - type derived from field
SQLT:Zipcode           LIKE(SQLT:Zipcode)             !List box control field - type derived from field
SQLT:ZipCodeType       LIKE(SQLT:ZipCodeType)         !List box control field - type derived from field
SQLT:City              LIKE(SQLT:City)                !List box control field - type derived from field
SQLT:State             LIKE(SQLT:State)               !List box control field - type derived from field
SQLT:Lat               LIKE(SQLT:Lat)                 !List box control field - type derived from field
SQLT:Long              LIKE(SQLT:Long)                !List box control field - type derived from field
SQLT:Country           LIKE(SQLT:Country)             !List box control field - type derived from field
SQLT:LocationText      LIKE(SQLT:LocationText)        !List box control field - type derived from field
Mark                   BYTE                           !Entry's marked status
ViewPosition           STRING(1024)                   !Entry's view position
                     END
LocEnableEnterByTab  BYTE(1)                               !Used by the ENTER Instead of Tab template
EnterByTabManager    EnterByTabClass
QuickWindow          WINDOW('Browse the SQLite_US_DB file'),AT(,,668,404),FONT('Microsoft Sans Serif',8,00AFA29Ch, |
  FONT:regular,CHARSET:DEFAULT),RESIZE,MAXIMIZE,CENTER,COLOR(00291C18h),GRAY,IMM,MDI,HLP('Search_SQLite'), |
  SYSTEM
                       TOOLBAR,AT(0,0,668,36),USE(?TOOLBAR1),COLOR(00291C18h),NOMERGE
                         OPTION,AT(2,26,79,6),USE(LOC:SearchMode,,?LOC:SearchMode:2),TRN
                           RADIO('Everywhere'),AT(245,4,68,16),USE(?LOC:SearchMode:Radio1),FLAT,TRN,VALUE('Everywhere')
                           RADIO('Granular Search'),AT(245,22,,16),USE(?LOC:SearchMode:Radio2),FLAT,TRN,VALUE('Specific')
                         END
                         GROUP,AT(2,6,231,20),USE(?Search_Everywhere_GRP),TRN
                           PROMPT('Search everywhere:'),AT(7,11),USE(?LOC:SearchString:Prompt),TRN
                           ENTRY(@s31),AT(75,10,145,12),USE(LOC:SearchString),FLAT,TRN
                           BOX,AT(95,10,34,9),USE(?Underline:10),COLOR(00D46F00h),FILL(004D382Ah),LINEWIDTH(1)
                         END
                         BUTTON('Search'),AT(325,5,80,22),USE(?Neutrino_Search),FONT(,,00DFF0DDh),LEFT,ICON('.\resource' & |
  's\Hot_Magnifying Glass-WF_32x32.png'),FLAT,TRN
                         BOX,AT(419,5,80,22),USE(?BTN:Box:2),COLOR(00B847AAh),FILL(00712C69h),HIDE,LINEWIDTH(1),ROUND
                         BUTTON('Clear Search'),AT(419,5,80,22),USE(?Clear_NSearch),FONT(,,00DDD8D6h),LEFT,ICON('.\resource' & |
  's\Hot_Close_32x32.png'),FLAT,TRN
                         BOX,AT(325,5,80,22),USE(?BTN:Box),COLOR(0047D239h),FILL(0030702Eh),HIDE,LINEWIDTH(1),ROUND
                         BUTTON('&Close'),AT(586,5,80,22),USE(?Close),FONT(,,00C1B6B2h),LEFT,ICON('.\resources\H' & |
  'ot_Close_32x32.png'),FLAT,MSG('Close Window'),TIP('Close Window'),TRN
                         BOX,AT(586,5,80,22),USE(?BTN:Box:3),COLOR(004643BBh),FILL(002C2B73h),HIDE,LINEWIDTH(1),ROUND
                       END
                       LIST,AT(3,25,663,377),USE(?Browse:1),FONT(,,00BBA98Bh),HVSCROLL,COLOR(0030201Ch),GRID(00413528h), |
  FORMAT('64C(2)|M~Record Number~C(0)@n-14@80C(2)|M~Zipcode~@s20@80C(3)|M~Zip Code Type' & |
  '~C(2)@s20@80L(2)|M~City~C(2)@s64@80C(2)|M~State~@s20@64C(2)|M~Lat~C(0)@n-14@64C(2)|M' & |
  '~Long~C(0)@n-14@80C(2)|M~Country~@s64@80L(2)|M~Location Text~C(2)@s64@'),FROM(Queue:Browse:1), |
  IMM,MSG('Browsing the SQLite_US_DB file'),VCR
                       GROUP,AT(7,15,665,35),USE(?Search_Group)
                         ENTRY(@n10B),AT(18,39,60,12),USE(SRCH:RecordNumber),FONT(,,00C1B6B2h),CENTER,FLAT,TRN
                         ENTRY(@s63),AT(82,39,77,12),USE(SRCH:ZIPCode),FONT(,,00C1B6B2h),CENTER,FLAT,TRN
                         ENTRY(@s63),AT(163,39,60,12),USE(SRCH:ZipCodeType),FONT(,,00C1B6B2h),CENTER,FLAT,TRN
                         ENTRY(@s63),AT(237,39,83,12),USE(SRCH:City),FONT(,,00C1B6B2h),CENTER,FLAT,TRN
                         ENTRY(@s63),AT(323,39,75,12),USE(SRCH:State),FONT(,,00C1B6B2h),CENTER,FLAT,TRN
                         ENTRY(@s63),AT(402,39,60,12),USE(SRCH:Lat),FONT(,,00C1B6B2h),CENTER,FLAT,TRN
                         ENTRY(@s63),AT(466,39,60,12),USE(SRCH:Long),FONT(,,00C1B6B2h),CENTER,FLAT,TRN
                         ENTRY(@s63),AT(530,39,60,12),USE(SRCH:Country),FONT(,,00C1B6B2h),CENTER,FLAT,TRN
                         ENTRY(@s63),AT(594,39,60,12),USE(SRCH:LocationText),FONT(,,00C1B6B2h),CENTER,FLAT,TRN
                         IMAGE('.\resources\arrowDown.png'),AT(43,25),USE(?Arrow),CENTERED
                         IMAGE('.\resources\arrowDown.png'),AT(307,25,7,6),USE(?Arrow:5),CENTERED
                         IMAGE('.\resources\arrowDown.png'),AT(375,25,7,6),USE(?Arrow:8),CENTERED
                         IMAGE('.\resources\arrowDown.png'),AT(347,25,7,6),USE(?Arrow:7),CENTERED
                         IMAGE('.\resources\arrowDown.png'),AT(330,25,7,6),USE(?Arrow:6),CENTERED
                         IMAGE('.\resources\arrowDown.png'),AT(395,25,7,6),USE(?Arrow:9),CENTERED
                         IMAGE('.\resources\arrowDown.png'),AT(286,25,7,6),USE(?Arrow:4),CENTERED
                         IMAGE('.\resources\arrowDown.png'),AT(271,25,7,6),USE(?Arrow:3),CENTERED
                         IMAGE('.\resources\arrowDown.png'),AT(254,25,7,6),USE(?Arrow:2),CENTERED
                         BOX,AT(135,17,34,9),USE(?Underline:9),COLOR(00D46F00h),FILL(004D382Ah),LINEWIDTH(1)
                         BOX,AT(135,17,34,9),USE(?Underline:8),COLOR(00D46F00h),FILL(004D382Ah),LINEWIDTH(1)
                         BOX,AT(135,17,34,9),USE(?Underline:7),COLOR(00D46F00h),FILL(004D382Ah),LINEWIDTH(1)
                         BOX,AT(135,17,34,9),USE(?Underline:6),COLOR(00D46F00h),FILL(004D382Ah),LINEWIDTH(1)
                         BOX,AT(135,17,34,9),USE(?Underline:5),COLOR(00D46F00h),FILL(004D382Ah),LINEWIDTH(1)
                         BOX,AT(135,17,34,9),USE(?Underline:4),COLOR(00D46F00h),FILL(004D382Ah),LINEWIDTH(1)
                         BOX,AT(135,17,34,9),USE(?Underline:3),COLOR(00D46F00h),FILL(004D382Ah),LINEWIDTH(1)
                         BOX,AT(135,17,34,9),USE(?Underline:2),COLOR(00D46F00h),FILL(004D382Ah),LINEWIDTH(1)
                         BOX,AT(135,17,34,9),USE(?Underline),COLOR(00D46F00h),FILL(004D382Ah),LINEWIDTH(1)
                       END
                     END

BRW1::LastSortOrder       BYTE
BRW1::SortHeader  CLASS(SortHeaderClassType) !Declare SortHeader Class
QueueResorted          PROCEDURE(STRING pString),VIRTUAL
                  END
BRW1::AutoSizeColumn CLASS(AutoSizeColumnClassType)
               END
ThisWindow           CLASS(WindowManager)
Init                   PROCEDURE(),BYTE,PROC,DERIVED
Kill                   PROCEDURE(),BYTE,PROC,DERIVED
SetAlerts              PROCEDURE(),DERIVED
TakeAccepted           PROCEDURE(),BYTE,PROC,DERIVED
TakeEvent              PROCEDURE(),BYTE,PROC,DERIVED
TakeWindowEvent        PROCEDURE(),BYTE,PROC,DERIVED
                     END

Toolbar              ToolbarClass
BRW1                 CLASS(BrowseClass)                    ! Browse using ?Browse:1
Q                      &Queue:Browse:1                !Reference to browse queue
SetSort                PROCEDURE(BYTE NewOrder,BYTE Force),BYTE,PROC,DERIVED
ValidateRecord         PROCEDURE(),BYTE,DERIVED
                     END

Resizer              CLASS(WindowResizeClass)
Init                   PROCEDURE(BYTE AppStrategy=AppStrategy:Resize,BYTE SetWindowMinSize=False,BYTE SetWindowMaxSize=False)
WindowResizeEnd        PROCEDURE(),DERIVED
                     END


  CODE
  GlobalResponse = ThisWindow.Run()                        ! Opens the window and starts an Accept Loop

!---------------------------------------------------------------------------
DefineListboxStyle ROUTINE
!|
!| This routine create all the styles to be shared in this window
!| It`s called after the window open
!|
!---------------------------------------------------------------------------
Custom_Resize_ROUTINE       ROUTINE
	
!	?BOX_Neutrino_Search{PROP:Xpos} = ?LOC:SearchString{PROP:Xpos}
!	?BOX_Neutrino_Search{PROP:Ypos} = ?LOC:SearchString{PROP:Ypos} + ?LOC:SearchString{PROP:Height}
!	?BOX_Neutrino_Search{PROP:Width} = ?LOC:SearchString{PROP:Width}
	

	!List width
	BRW_Width# = ?Browse:1{PROP:Width}
	?Browse:1{PROPLIST:width,1} = BRW_Width# * 0.1
	?Browse:1{PROPLIST:width,2} = BRW_Width# * 0.10
	?Browse:1{PROPLIST:width,3} = BRW_Width# * 0.11
	?Browse:1{PROPLIST:width,4} = BRW_Width# * 0.25
	?Browse:1{PROPLIST:width,5} = BRW_Width# * 0.08
	?Browse:1{PROPLIST:width,6} = BRW_Width# * 0.08
	?Browse:1{PROPLIST:width,7} = BRW_Width# * 0.08
	?Browse:1{PROPLIST:width,8} = BRW_Width# * 0.08
	!?Browse:1{PROPLIST:width,9} = BRW_Width# * 0.05
	
	!Super Search Fields
	SETPOSITION(?SRCH:RecordNumber,?Browse:1{PROP:Xpos},?Browse:1{PROP:Ypos} - ?SRCH:RecordNumber{PROP:Height} - 8,?Browse:1{PROPLIST:width,1})
	SETPOSITION(?SRCH:ZipCode,?SRCH:RecordNumber{PROP:Xpos} + ?SRCH:RecordNumber{PROP:Width} + 1,?SRCH:RecordNumber{PROP:Ypos},?Browse:1{PROPLIST:width,2} )
	SETPOSITION(?SRCH:ZipCodeType,?SRCH:ZipCode{PROP:Xpos} + ?SRCH:ZipCode{PROP:Width} + 1,?SRCH:RecordNumber{PROP:Ypos},?Browse:1{PROPLIST:width,3} )
	SETPOSITION(?SRCH:City,?SRCH:ZipCodeType{PROP:Xpos} + ?SRCH:ZipCodeType{PROP:Width} + 1,?SRCH:RecordNumber{PROP:Ypos},?Browse:1{PROPLIST:width,4})
	SETPOSITION(?SRCH:State,?SRCH:City{PROP:Xpos} + ?SRCH:City{PROP:Width} + 1,?SRCH:RecordNumber{PROP:Ypos},?Browse:1{PROPLIST:width,5})
	SETPOSITION(?SRCH:Lat,?SRCH:State{PROP:XPos} + ?SRCH:State{PROP:Width} + 1,?SRCH:RecordNumber{PROP:Ypos},?Browse:1{PROPLIST:width,6})
	SETPOSITION(?SRCH:Long,?SRCH:Lat{PROP:Xpos} + ?SRCH:Lat{PROP:Width} + 1,?SRCH:RecordNumber{PROP:Ypos},?Browse:1{PROPLIST:width,7})
	SETPOSITION(?SRCH:Country,?SRCH:Long{PROP:Xpos} + ?SRCH:Long{PROP:Width} + 1,?SRCH:RecordNumber{PROP:Ypos},?Browse:1{PROPLIST:width,8})
	SETPOSITION(?SRCH:LocationText,?SRCH:Country{PROP:Xpos} + ?SRCH:Country{PROP:Width} + 1,?SRCH:RecordNumber{PROP:Ypos},?Browse:1{PROPLIST:width,9})
	
	!Place the arrows
	SETPOSITION(?Arrow,?SRCH:RecordNumber{PROP:Xpos} + (?SRCH:RecordNumber{PROP:Width} / 2),?SRCH:RecordNumber{PROP:Ypos} + ?SRCH:RecordNumber{PROP:Height})
	SETPOSITION(?Arrow:2,?SRCH:ZipCode{PROP:Xpos} + (?SRCH:ZipCode{PROP:Width} / 2),?SRCH:ZipCode{PROP:Ypos} + ?SRCH:ZipCode{PROP:Height})
	SETPOSITION(?Arrow:3,?SRCH:ZipCodeType{PROP:Xpos} + (?SRCH:ZipCodeType{PROP:Width} / 2),?SRCH:ZipCodeType{PROP:Ypos} + ?SRCH:ZipCodeType{PROP:Height})
	SETPOSITION(?Arrow:4,?SRCH:City{PROP:Xpos} + (?SRCH:City{PROP:Width} / 2),?SRCH:City{PROP:Ypos} + ?SRCH:City{PROP:Height})
	SETPOSITION(?Arrow:5,?SRCH:State{PROP:Xpos} + (?SRCH:State{PROP:Width} / 2),?SRCH:State{PROP:Ypos} + ?SRCH:State{PROP:Height})
	SETPOSITION(?Arrow:6,?SRCH:Lat{PROP:Xpos} + (?SRCH:Lat{PROP:Width} / 2),?SRCH:Lat{PROP:Ypos} + ?SRCH:Lat{PROP:Height})
	SETPOSITION(?Arrow:7,?SRCH:Long{PROP:Xpos} + (?SRCH:Long{PROP:Width} / 2),?SRCH:Long{PROP:Ypos} + ?SRCH:Long{PROP:Height})
	SETPOSITION(?Arrow:8,?SRCH:Country{PROP:Xpos} + (?SRCH:Country{PROP:Width} / 2),?SRCH:Country{PROP:Ypos} + ?SRCH:Country{PROP:Height})
	SETPOSITION(?Arrow:9,?SRCH:LocationText{PROP:Xpos} + (?SRCH:LocationText{PROP:Width} / 2),?SRCH:LocationText{PROP:Ypos} + ?SRCH:LocationText{PROP:Height})
	!Place the super search boxes
	
	SETPOSITION(?Underline,?SRCH:RecordNumber{PROP:Xpos},?SRCH:RecordNumber{PROP:Ypos},?SRCH:RecordNumber{PROP:Width},?SRCH:RecordNumber{PROP:Height})
	SETPOSITION(?Underline:2,?SRCH:ZipCode{PROP:Xpos},?SRCH:ZipCode{PROP:Ypos},?SRCH:ZipCode{PROP:Width},?SRCH:ZipCode{PROP:Height})
	SETPOSITION(?Underline:3,?SRCH:ZipCodeType{PROP:Xpos},?SRCH:ZipCodeType{PROP:Ypos},?SRCH:ZipCodeType{PROP:Width},?SRCH:ZipCodeType{PROP:Height})
	SETPOSITION(?Underline:4,?SRCH:City{PROP:Xpos},?SRCH:City{PROP:Ypos},?SRCH:City{PROP:Width},?SRCH:City{PROP:Height})
	SETPOSITION(?Underline:5,?SRCH:State{PROP:Xpos},?SRCH:State{PROP:Ypos},?SRCH:State{PROP:Width},?SRCH:State{PROP:Height})
	SETPOSITION(?Underline:6,?SRCH:Lat{PROP:Xpos},?SRCH:Lat{PROP:Ypos},?SRCH:Lat{PROP:Width},?SRCH:Lat{PROP:Height})
	SETPOSITION(?Underline:7,?SRCH:Long{PROP:Xpos},?SRCH:Long{PROP:Ypos},?SRCH:Long{PROP:Width},?SRCH:Long{PROP:Height})
	SETPOSITION(?Underline:8,?SRCH:Country{PROP:Xpos},?SRCH:Country{PROP:Ypos},?SRCH:Country{PROP:Width},?SRCH:Country{PROP:Height})
	SETPOSITION(?Underline:9,?SRCH:LocationText{PROP:Xpos},?SRCH:LocationText{PROP:Ypos},?SRCH:LocationText{PROP:Width},?SRCH:LocationText{PROP:Height})
	
	!Search Everywhere box
	SETPOSITION(?Underline:10,?LOC:SearchString{PROP:Xpos},?LOC:SearchString{PROP:Ypos},?LOC:SearchString{PROP:Width},?LOC:SearchString{PROP:Height})
	
	
	EXIT

ThisWindow.Init PROCEDURE

ReturnValue          BYTE,AUTO

  CODE
  GlobalErrors.SetProcedureName('Search_SQLite')
  SELF.Request = GlobalRequest                             ! Store the incoming request
  ReturnValue = PARENT.Init()
  IF ReturnValue THEN RETURN ReturnValue.
  SELF.FirstField = ?Browse:1
  SELF.VCRRequest &= VCRRequest
  SELF.Errors &= GlobalErrors                              ! Set this windows ErrorManager to the global ErrorManager
  CLEAR(GlobalRequest)                                     ! Clear GlobalRequest after storing locally
  CLEAR(GlobalResponse)
  SELF.AddItem(Toolbar)
  IF SELF.Request = SelectRecord
     SELF.AddItem(?Close,RequestCancelled)                 ! Add the close control to the window manger
  ELSE
     SELF.AddItem(?Close,RequestCompleted)                 ! Add the close control to the window manger
  END
  Relate:SQLiteUSDB.Open()                                 ! File SQLiteUSDB used by this procedure, so make sure it's RelationManager is open
  SELF.FilesOpened = True
  BRW1.Init(?Browse:1,Queue:Browse:1.ViewPosition,BRW1::View:Browse,Queue:Browse:1,Relate:SQLiteUSDB,SELF) ! Initialize the browse manager
  SELF.Open(QuickWindow)                                   ! Open window
  !Setting the LineHeight for every control of type LIST/DROP or COMBO in the window using the global setting.
  ?Browse:1{PROP:LineHeight} = 15
  Do DefineListboxStyle
  BRW1.Q &= Queue:Browse:1
  BRW1.AddSortOrder(,SQLT:SQLRecordNumberKey)              ! Add the sort order for SQLT:SQLRecordNumberKey for sort order 1
  BRW1.AddResetField(LOC:ResetBRW)                         ! Apply the reset field
  BRW1.AddField(SQLT:RecordNumber,BRW1.Q.SQLT:RecordNumber) ! Field SQLT:RecordNumber is a hot field or requires assignment from browse
  BRW1.AddField(SQLT:Zipcode,BRW1.Q.SQLT:Zipcode)          ! Field SQLT:Zipcode is a hot field or requires assignment from browse
  BRW1.AddField(SQLT:ZipCodeType,BRW1.Q.SQLT:ZipCodeType)  ! Field SQLT:ZipCodeType is a hot field or requires assignment from browse
  BRW1.AddField(SQLT:City,BRW1.Q.SQLT:City)                ! Field SQLT:City is a hot field or requires assignment from browse
  BRW1.AddField(SQLT:State,BRW1.Q.SQLT:State)              ! Field SQLT:State is a hot field or requires assignment from browse
  BRW1.AddField(SQLT:Lat,BRW1.Q.SQLT:Lat)                  ! Field SQLT:Lat is a hot field or requires assignment from browse
  BRW1.AddField(SQLT:Long,BRW1.Q.SQLT:Long)                ! Field SQLT:Long is a hot field or requires assignment from browse
  BRW1.AddField(SQLT:Country,BRW1.Q.SQLT:Country)          ! Field SQLT:Country is a hot field or requires assignment from browse
  BRW1.AddField(SQLT:LocationText,BRW1.Q.SQLT:LocationText) ! Field SQLT:LocationText is a hot field or requires assignment from browse
  Resizer.Init(AppStrategy:Resize,Resize:SetMinSize)       ! Controls will change size as the window gets bigger
  SELF.AddItem(Resizer)                                    ! Add resizer to window manager
  IF LOC:SearchMode = 'Specific'
     LOC:SearchString = ''
     UNHIDE(?Search_Group)
     HIDE(?Search_Everywhere_GRP)
  END
  IF LOC:SearchMode <> 'Specific'
     HIDE(?Search_Group)
     UNHIDE(?Search_Everywhere_GRP)
  END
  SELF.SetAlerts()
  BRW1::AutoSizeColumn.Init()
  BRW1::AutoSizeColumn.AddListBox(?Browse:1,Queue:Browse:1)
  !Initialize the Sort Header using the Browse Queue and Browse Control
  BRW1::SortHeader.Init(Queue:Browse:1,?Browse:1,'','',BRW1::View:Browse,SQLT:SQLRecordNumberKey)
  BRW1::SortHeader.UseSortColors = False
  EnterByTabManager.Init(False)
  RETURN ReturnValue


ThisWindow.Kill PROCEDURE

ReturnValue          BYTE,AUTO

  CODE
  ReturnValue = PARENT.Kill()
  IF ReturnValue THEN RETURN ReturnValue.
  IF SELF.FilesOpened
    Relate:SQLiteUSDB.Close()
  !Kill the Sort Header
  BRW1::SortHeader.Kill()
  END
  BRW1::AutoSizeColumn.Kill()
  GlobalErrors.SetProcedureName
  RETURN ReturnValue


ThisWindow.SetAlerts PROCEDURE

  CODE
  PARENT.SetAlerts
  !Initialize the Sort Header using the Browse Queue and Browse Control
  BRW1::SortHeader.SetAlerts()


ThisWindow.TakeAccepted PROCEDURE

ReturnValue          BYTE,AUTO

Looped BYTE
  CODE
  LOOP                                                     ! This method receive all EVENT:Accepted's
    IF Looped
      RETURN Level:Notify
    ELSE
      Looped = 1
    END
  ReturnValue = PARENT.TakeAccepted()
    CASE ACCEPTED()
    OF ?LOC:SearchMode:2
      IF LOC:SearchMode = 'Specific'
         LOC:SearchString = ''
         UNHIDE(?Search_Group)
         HIDE(?Search_Everywhere_GRP)
      END
      IF LOC:SearchMode <> 'Specific'
         HIDE(?Search_Group)
         UNHIDE(?Search_Everywhere_GRP)
      END
      ThisWindow.Reset()
    OF ?LOC:SearchString
      POST(EVENT:Accepted,?Neutrino_Search)
    OF ?Neutrino_Search
      ThisWindow.Update()
      LOC:SearchGRPByte = 0
      
      IF SRCH:RecordNumber <> ''
      	LOC:SearchGRPByte += 1
      END
      IF SRCH:ZIPCode <> ''
      	LOC:SearchGRPByte += 1
      END
      IF SRCH:ZipCodeType <> ''
      	LOC:SearchGRPByte += 1
      END
      IF SRCH:City <> ''
      	LOC:SearchGRPByte += 1
      END
      IF SRCH:State <> ''
      	LOC:SearchGRPByte += 1
      END
      IF SRCH:Lat <> ''
      	LOC:SearchGRPByte += 1
      END
      IF SRCH:Long <> ''
      	LOC:SearchGRPByte += 1
      END
      IF SRCH:Country <> ''
      	LOC:SearchGRPByte += 1
      END
      IF SRCH:LocationText <> ''
      	LOC:SearchGRPByte += 1
      END
      
      
      LOC:ResetBRW += 1
    OF ?Clear_NSearch
      ThisWindow.Update()
      CLEAR(LOC:SearchString)
      CLEAR(SearchGRP)
      CLEAR(LOC:SearchGRPByte)
      
      ThisWindow.RESET(1)
      
      POST(EVENT:ScrollTop,?Browse:1)
    OF ?SRCH:RecordNumber
      POST(EVENT:Accepted,?Neutrino_Search)
    OF ?SRCH:ZIPCode
      POST(EVENT:Accepted,?Neutrino_Search)
    OF ?SRCH:ZipCodeType
      POST(EVENT:Accepted,?Neutrino_Search)
    OF ?SRCH:City
      POST(EVENT:Accepted,?Neutrino_Search)
    OF ?SRCH:State
      POST(EVENT:Accepted,?Neutrino_Search)
    OF ?SRCH:Lat
      POST(EVENT:Accepted,?Neutrino_Search)
    OF ?SRCH:Long
      POST(EVENT:Accepted,?Neutrino_Search)
    OF ?SRCH:Country
      POST(EVENT:Accepted,?Neutrino_Search)
    OF ?SRCH:LocationText
      POST(EVENT:Accepted,?Neutrino_Search)
    END
    RETURN ReturnValue
  END
  ReturnValue = Level:Fatal
  RETURN ReturnValue


ThisWindow.TakeEvent PROCEDURE

ReturnValue          BYTE,AUTO

Looped BYTE
  CODE
  LOOP                                                     ! This method receives all events
    IF Looped
      RETURN Level:Notify
    ELSE
      Looped = 1
    END
  !Take Sort Headers Events
  IF BRW1::SortHeader.TakeEvents()
     RETURN Level:Notify
  END
  IF EnterByTabManager.TakeEvent()
     RETURN(Level:Notify)
  END
  IF BRW1::AutoSizeColumn.TakeEvents()
     RETURN Level:Notify
  END
  ReturnValue = PARENT.TakeEvent()
    RETURN ReturnValue
  END
  ReturnValue = Level:Fatal
  RETURN ReturnValue


ThisWindow.TakeWindowEvent PROCEDURE

ReturnValue          BYTE,AUTO

Looped BYTE
  CODE
  LOOP                                                     ! This method receives all window specific events
    IF Looped
      RETURN Level:Notify
    ELSE
      Looped = 1
    END
  ReturnValue = PARENT.TakeWindowEvent()
    CASE EVENT()
    OF EVENT:OpenWindow
      0{PROP:Buffer} = 1
      LOC:SearchMode = 'Everywhere'
      DISPLAY(?LOC:SearchMode:Radio1)
    END
    RETURN ReturnValue
  END
  ReturnValue = Level:Fatal
  RETURN ReturnValue


BRW1.SetSort PROCEDURE(BYTE NewOrder,BYTE Force)

ReturnValue          BYTE,AUTO

  CODE
  ReturnValue = PARENT.SetSort(NewOrder,Force)
  IF BRW1::LastSortOrder<>NewOrder THEN
     BRW1::SortHeader.ClearSort()
  END
  BRW1::LastSortOrder=NewOrder
  RETURN ReturnValue


BRW1.ValidateRecord PROCEDURE

ReturnValue          BYTE,AUTO

BRW1::RecordStatus   BYTE,AUTO
  CODE
  ReturnValue = PARENT.ValidateRecord()
  
  
  IF LOC:SearchString <> '' AND LOC:SearchMode = 'Everywhere'
  
  	LOC:Match = FALSE
  	ReturnValue=Record:Filtered
  	
  	IF LOC:SearchString <> ''
  
  		IF LOC:Match = FALSE
  			LOC:Match = Fast.Find(LOC:SearchString,SQLT:RecordNumber)
  			IF LOC:Match
  				ReturnValue=Record:Ok
  
  			END			
  		END			
  
  		IF LOC:Match = FALSE
  				
  			LOC:Match = Fast.Find(LOC:SearchString,SQLT:Zipcode)
  			IF LOC:Match
  				ReturnValue=Record:Ok
  
  			END			
  		END			
  			
  		IF LOC:Match = FALSE
  			LOC:Match = Fast.Find(LOC:SearchString,SQLT:ZipCodeType)
  			IF LOC:Match
  				ReturnValue=Record:Ok
  
  			END			
  		END			
  			
  		IF LOC:Match = FALSE
  			LOC:Match = Fast.Find(LOC:SearchString,SQLT:City)
  			IF LOC:Match
  				ReturnValue=Record:Ok
  
  			END			
  		END		
  	
  	
  		IF LOC:Match = FALSE
  			LOC:Match = Fast.Find(LOC:SearchString,SQLT:State)
  			IF LOC:Match
  				ReturnValue=Record:Ok
  
  			END			
  		END	
  	
  		IF LOC:Match = FALSE
  			LOC:Match = Fast.Find(LOC:SearchString,SQLT:Lat)
  			IF LOC:Match
  				ReturnValue=Record:Ok
  
  			END			
  		END	
  		
  		IF LOC:Match = FALSE
  			LOC:Match = Fast.Find(LOC:SearchString,SQLT:Location)
  			IF LOC:Match
  				ReturnValue=Record:Ok
  
  			END			
  		END	
  		
  		IF LOC:Match = FALSE
  			LOC:Match = Fast.Find(LOC:SearchString,SQLT:Long)
  			IF LOC:Match
  				ReturnValue=Record:Ok
  
  			END			
  		END	
  		
  		IF LOC:Match = FALSE
  			LOC:Match = Fast.Find(LOC:SearchString,SQLT:Country)
  			IF LOC:Match
  				ReturnValue=Record:Ok
  
  			END			
  		END	
  		
  		IF LOC:Match = FALSE
  			LOC:Match = Fast.Find(LOC:SearchString,SQLT:LocationText)
  			IF LOC:Match
  				ReturnValue=Record:Ok
  
  			END			
  		END	
  	END
  END
  
  
  
  IF LOC:SearchGRPByte => 1 AND LOC:SearchMode = 'Specific'
  	
  	LOC:MatchCount = 0
  	ReturnValue=Record:Filtered
  		
  	IF 	SRCH:RecordNumber <> ''
  		LOC:MatchCount += Fast.Find(SRCH:RecordNumber,SQLT:RecordNumber)
  	END
  		
  	IF SRCH:ZIPCode <> ''
  		LOC:MatchCount += Fast.Find(SRCH:ZIPCode,SQLT:Zipcode)
  	END
  		
  	IF SRCH:ZipCodeType
  		LOC:MatchCount += Fast.Find(SRCH:ZipCodeType,SQLT:ZipCodeType)
  	END
  		
  	IF SRCH:City <> ''
  		LOC:MatchCount += Fast.Find(SRCH:City,SQLT:City)
  	END
  	IF SRCH:State <> ''
  		LOC:MatchCount += Fast.Find(SRCH:State,SQLT:State)
  	END
  		
  	IF SRCH:Lat <> ''
  		LOC:MatchCount += Fast.Find(SRCH:Lat,SQLT:Lat)
  	END
  		
  	IF SRCH:Long <> ''
  		LOC:MatchCount += Fast.Find(SRCH:Long,SQLT:Long)
  	END
  		
  	IF SRCH:Country <> ''
  		LOC:MatchCount += Fast.Find(SRCH:Country,SQLT:Country)
  	END
  
  	IF SRCH:LocationText <> ''
  		LOC:MatchCount += Fast.Find(SRCH:LocationText,SQLT:LocationText)
  	END
  		
  	IF LOC:SearchGRPByte = LOC:MatchCount
  		ReturnValue=Record:Ok
  	END
  		
  
  END
  	
  
  	
  BRW1::RecordStatus=ReturnValue
  RETURN ReturnValue


Resizer.Init PROCEDURE(BYTE AppStrategy=AppStrategy:Resize,BYTE SetWindowMinSize=False,BYTE SetWindowMaxSize=False)


  CODE
  PARENT.Init(AppStrategy,SetWindowMinSize,SetWindowMaxSize)
  SELF.SetParentDefaults()                                 ! Calculate default control parent-child relationships based upon their positions on the window
  SELF.RemoveControl(?Arrow)                               ! Remove ?Arrow from the resizer, it will not be moved or sized
  SELF.RemoveControl(?Arrow:5)                             ! Remove ?Arrow:5 from the resizer, it will not be moved or sized
  SELF.RemoveControl(?Arrow:8)                             ! Remove ?Arrow:8 from the resizer, it will not be moved or sized
  SELF.RemoveControl(?Arrow:7)                             ! Remove ?Arrow:7 from the resizer, it will not be moved or sized
  SELF.RemoveControl(?Arrow:6)                             ! Remove ?Arrow:6 from the resizer, it will not be moved or sized
  SELF.RemoveControl(?Arrow:9)                             ! Remove ?Arrow:9 from the resizer, it will not be moved or sized
  SELF.RemoveControl(?Arrow:4)                             ! Remove ?Arrow:4 from the resizer, it will not be moved or sized
  SELF.RemoveControl(?Arrow:3)                             ! Remove ?Arrow:3 from the resizer, it will not be moved or sized
  SELF.RemoveControl(?Arrow:2)                             ! Remove ?Arrow:2 from the resizer, it will not be moved or sized
  SELF.RemoveControl(?Underline:9)                         ! Remove ?Underline:9 from the resizer, it will not be moved or sized
  SELF.RemoveControl(?Underline:8)                         ! Remove ?Underline:8 from the resizer, it will not be moved or sized
  SELF.RemoveControl(?Underline:7)                         ! Remove ?Underline:7 from the resizer, it will not be moved or sized
  SELF.RemoveControl(?Underline:6)                         ! Remove ?Underline:6 from the resizer, it will not be moved or sized
  SELF.RemoveControl(?Underline:5)                         ! Remove ?Underline:5 from the resizer, it will not be moved or sized
  SELF.RemoveControl(?Underline:4)                         ! Remove ?Underline:4 from the resizer, it will not be moved or sized
  SELF.RemoveControl(?Underline:3)                         ! Remove ?Underline:3 from the resizer, it will not be moved or sized
  SELF.RemoveControl(?Underline:2)                         ! Remove ?Underline:2 from the resizer, it will not be moved or sized
  SELF.RemoveControl(?Underline)                           ! Remove ?Underline from the resizer, it will not be moved or sized


Resizer.WindowResizeEnd PROCEDURE


  CODE
  PARENT.WindowResizeEnd
  DO Custom_Resize_ROUTINE

BRW1::SortHeader.QueueResorted       PROCEDURE(STRING pString)
  CODE
    IF pString = ''
       BRW1.RestoreSort()
       BRW1.ResetSort(True)
    ELSE
       BRW1.ReplaceSort(pString)
    END
!!! <summary>
!!! Generated from procedure template - Window
!!! Window
!!! </summary>
Conversion_Window PROCEDURE 

LOC:Records_To_Convert LONG                                ! 
LOC:RecordProgress   LONG                                  ! 
LocEnableEnterByTab  BYTE(1)                               !Used by the ENTER Instead of Tab template
EnterByTabManager    EnterByTabClass
QuickWindow          WINDOW('Conversion...'),AT(,,489,74),FONT('Microsoft Sans Serif',8,COLOR:White,FONT:regular, |
  CHARSET:DEFAULT),CENTER,COLOR(00543A2Ch),GRAY,IMM,HLP('Conversion_Window'),SYSTEM
                       BUTTON('&Close Window'),AT(387,49,85,14),USE(?Cancel),LEFT,ICON('WACANCEL.ICO'),FLAT,MSG('Cancel Operation'), |
  TIP('Cancel Operation')
                       PROGRESS,AT(13,30,459),USE(?Conversion_Progress),RANGE(0,100)
                       STRING(''),AT(13,14,459),USE(?Conversion_Text),CENTER
                       BUTTON('Begin Conversion'),AT(141,42,133,21),USE(?Begin_Conversion),HIDE
                     END

ThisWindow           CLASS(WindowManager)
Init                   PROCEDURE(),BYTE,PROC,DERIVED
Kill                   PROCEDURE(),BYTE,PROC,DERIVED
TakeAccepted           PROCEDURE(),BYTE,PROC,DERIVED
TakeEvent              PROCEDURE(),BYTE,PROC,DERIVED
TakeWindowEvent        PROCEDURE(),BYTE,PROC,DERIVED
                     END

Toolbar              ToolbarClass
Resizer              CLASS(WindowResizeClass)
Init                   PROCEDURE(BYTE AppStrategy=AppStrategy:Resize,BYTE SetWindowMinSize=False,BYTE SetWindowMaxSize=False)
                     END


  CODE
  GlobalResponse = ThisWindow.Run()                        ! Opens the window and starts an Accept Loop

!---------------------------------------------------------------------------
DefineListboxStyle ROUTINE
!|
!| This routine create all the styles to be shared in this window
!| It`s called after the window open
!|
!---------------------------------------------------------------------------

ThisWindow.Init PROCEDURE

ReturnValue          BYTE,AUTO

  CODE
  GlobalErrors.SetProcedureName('Conversion_Window')
  SELF.Request = GlobalRequest                             ! Store the incoming request
  ReturnValue = PARENT.Init()
  IF ReturnValue THEN RETURN ReturnValue.
  SELF.FirstField = ?Cancel
  SELF.VCRRequest &= VCRRequest
  SELF.Errors &= GlobalErrors                              ! Set this windows ErrorManager to the global ErrorManager
  CLEAR(GlobalRequest)                                     ! Clear GlobalRequest after storing locally
  CLEAR(GlobalResponse)
  SELF.AddItem(Toolbar)
  SELF.AddItem(?Cancel,RequestCancelled)                   ! Add the cancel control to the window manager
  Relate:CSV_US_DB.Open()                                  ! File CSV_US_DB used by this procedure, so make sure it's RelationManager is open
  Relate:SQLiteUSDB.Open()                                 ! File SQLiteUSDB used by this procedure, so make sure it's RelationManager is open
  Relate:SV_US_DB.Open()                                   ! File SV_US_DB used by this procedure, so make sure it's RelationManager is open
  SELF.FilesOpened = True
  SELF.Open(QuickWindow)                                   ! Open window
  !Setting the LineHeight for every control of type LIST/DROP or COMBO in the window using the global setting.
  Do DefineListboxStyle
  Resizer.Init(AppStrategy:Surface,Resize:SetMinSize)      ! Controls like list boxes will resize, whilst controls like buttons will move
  SELF.AddItem(Resizer)                                    ! Add resizer to window manager
  SELF.SetAlerts()
  EnterByTabManager.Init(False)
  RETURN ReturnValue


ThisWindow.Kill PROCEDURE

ReturnValue          BYTE,AUTO

  CODE
  ReturnValue = PARENT.Kill()
  IF ReturnValue THEN RETURN ReturnValue.
  IF SELF.FilesOpened
    Relate:CSV_US_DB.Close()
    Relate:SQLiteUSDB.Close()
    Relate:SV_US_DB.Close()
  END
  GlobalErrors.SetProcedureName
  RETURN ReturnValue


ThisWindow.TakeAccepted PROCEDURE

ReturnValue          BYTE,AUTO

Looped BYTE
  CODE
  LOOP                                                     ! This method receive all EVENT:Accepted's
    IF Looped
      RETURN Level:Notify
    ELSE
      Looped = 1
    END
  ReturnValue = PARENT.TakeAccepted()
    CASE ACCEPTED()
    OF ?Begin_Conversion
      ThisWindow.Update()
      ?Cancel{PROP:Hide} = TRUE
      ?Begin_Conversion{PROP:Hide} = TRUE
      
      CLOSE(SQLiteUSDB) !Let's close the file...
      CLOSE(SV_US_DB)
      
      REMOVE('USDB.sqlite') !Let's remove the file itself... Faster than EMPTY
      REMOVE('SV_US_DB.TPS')
      
      CREATE(SQLiteUSDB) !Creation of new file...
      CREATE(SV_US_DB)
      
      OPEN(SQLiteUSDB) !let's open the newborn file...
      OPEN(SV_US_DB)
      
      STREAM(SV_US_DB)	!We want to use STREAM to make everything faster... 
      STREAM(SQLiteUSDB)
      SET(CSV_US_DB)	!Let's being on record order...
      
      LOOP 
      	IF Access:CSV_US_DB.NEXT() <> Level:Benign THEN BREAK. !Reached enf of file? read error? Break the LOOP
      	CLEAR(SV_:Record)
      	CLEAR(SQLT:Record)
      	SV_:Record = CSV:Record
      	SQLT:Record = CSV:Record
      	ADD(SV_US_DB)
      	ADD(SQLiteUSDB)
      	LOC:RecordProgress += 1
      	?Conversion_Text{PROP:Text} = ' Converting record ' & LOC:RecordProgress & ' of ' & LOC:Records_To_Convert
      	?Conversion_Progress{PROP:progress} = ?Conversion_Progress{PROP:progress} + 1
      	
      	DISPLAY(?Conversion_Text{PROP:Text})
      	DISPLAY(?Conversion_Progress)
      END !LOOP
      
      ?Conversion_Text{PROP:Text} = 'Flushing... This will take a while...'
      DISPLAY(?Conversion_Text{PROP:Text})
      FLUSH(SV_US_DB)
      FLUSH(SQLiteUSDB)
      ?Conversion_Text{PROP:Text} = 'All done...'
      DISPLAY(?Conversion_Text{PROP:Text})
      
      ?Cancel{PROP:Hide} = FALSE
    END
    RETURN ReturnValue
  END
  ReturnValue = Level:Fatal
  RETURN ReturnValue


ThisWindow.TakeEvent PROCEDURE

ReturnValue          BYTE,AUTO

Looped BYTE
  CODE
  LOOP                                                     ! This method receives all events
    IF Looped
      RETURN Level:Notify
    ELSE
      Looped = 1
    END
  IF EnterByTabManager.TakeEvent()
     RETURN(Level:Notify)
  END
  ReturnValue = PARENT.TakeEvent()
    RETURN ReturnValue
  END
  ReturnValue = Level:Fatal
  RETURN ReturnValue


ThisWindow.TakeWindowEvent PROCEDURE

ReturnValue          BYTE,AUTO

Looped BYTE
  CODE
  LOOP                                                     ! This method receives all window specific events
    IF Looped
      RETURN Level:Notify
    ELSE
      Looped = 1
    END
  ReturnValue = PARENT.TakeWindowEvent()
    CASE EVENT()
    OF EVENT:OpenWindow
      !This is a simple loop over the CSV file.
      !We will count them... one by one...
      
      CLEAR(CSV:Record)
      LOC:Records_To_Convert = 0
      SET(CSV_US_DB)
      LOOP
      	IF Access:CSV_US_DB.Next() <> Level:Benign THEN BREAK.	
      	LOC:Records_To_Convert += 1
      	?Conversion_Text{PROP:Text} = LOC:Records_To_Convert & ' Records to convert'
      	DISPLAY(?Conversion_Text)
      END
      
      LOC:RecordProgress = 0
      ?Conversion_Progress{PROP:RangeHigh} = LOC:Records_To_Convert 
      ?Conversion_Text{PROP:Text} = LOC:Records_To_Convert & ' Records to convert'
      ?Begin_Conversion{PROP:Hide} = FALSE
    END
    RETURN ReturnValue
  END
  ReturnValue = Level:Fatal
  RETURN ReturnValue


Resizer.Init PROCEDURE(BYTE AppStrategy=AppStrategy:Resize,BYTE SetWindowMinSize=False,BYTE SetWindowMaxSize=False)


  CODE
  PARENT.Init(AppStrategy,SetWindowMinSize,SetWindowMaxSize)
  SELF.SetParentDefaults()                                 ! Calculate default control parent-child relationships based upon their positions on the window

!!! <summary>
!!! Generated from procedure template - Window
!!! Form CSV_US_DB
!!! </summary>
Update_CSV PROCEDURE 

CurrentTab           STRING(80)                            ! 
ActionMessage        CSTRING(40)                           ! 
LocEnableEnterByTab  BYTE(1)                               !Used by the ENTER Instead of Tab template
EnterByTabManager    EnterByTabClass
History::CSV:Record  LIKE(CSV:RECORD),THREAD
QuickWindow          WINDOW('Form CSV_US_DB'),AT(,,352,189),FONT('Microsoft Sans Serif',8,00F0FFF0h,FONT:regular, |
  CHARSET:DEFAULT),RESIZE,CENTER,COLOR(0048382Bh),GRAY,IMM,MDI,HLP('Update_CSV'),SYSTEM
                       BUTTON('&OK'),AT(185,161,71,22),USE(?OK),LEFT,ICON('ok_32x32.png'),DEFAULT,FLAT,MSG('Accept dat' & |
  'a and close the window'),TIP('Accept data and close the window')
                       BUTTON('&Cancel'),AT(269,161,71,22),USE(?Cancel),LEFT,ICON('cancel_32x32.png'),FLAT,MSG('Cancel operation'), |
  TIP('Cancel operation')
                       ENTRY(@n-14),AT(79,12,64,10),USE(CSV:RecordNumber),RIGHT(1)
                       PROMPT('Record Number:'),AT(19,12),USE(?CSV:RecordNumber:Prompt),TRN
                       PROMPT('Zipcode:'),AT(19,26),USE(?CSV:Zipcode:Prompt),TRN
                       ENTRY(@s20),AT(79,26,84,10),USE(CSV:Zipcode),READONLY
                       PROMPT('Zip Code Type:'),AT(19,40),USE(?CSV:ZipCodeType:Prompt),TRN
                       ENTRY(@s20),AT(79,40,84,10),USE(CSV:ZipCodeType)
                       PROMPT('City:'),AT(19,54),USE(?CSV:City:Prompt),TRN
                       ENTRY(@s64),AT(79,54,260,10),USE(CSV:City),READONLY
                       PROMPT('State:'),AT(19,68),USE(?CSV:State:Prompt),TRN
                       ENTRY(@s20),AT(79,68,84,10),USE(CSV:State),REQ
                       PROMPT('Lat:'),AT(19,82),USE(?CSV:Lat:Prompt),TRN
                       ENTRY(@n-14),AT(79,82,64,10),USE(CSV:Lat),RIGHT(1)
                       PROMPT('Long:'),AT(19,96),USE(?CSV:Long:Prompt),TRN
                       ENTRY(@n-14),AT(79,96,64,10),USE(CSV:Long),RIGHT(1)
                       PROMPT('Country:'),AT(19,110),USE(?CSV:Country:Prompt),TRN
                       ENTRY(@s64),AT(79,110,260,10),USE(CSV:Country)
                       PROMPT('Location Text:'),AT(19,124),USE(?CSV:LocationText:Prompt),TRN
                       ENTRY(@s64),AT(79,124,260,10),USE(CSV:LocationText)
                       PROMPT('Location:'),AT(19,138),USE(?CSV:Location:Prompt),TRN
                       ENTRY(@s64),AT(79,138,260,10),USE(CSV:Location)
                     END

CoolEntryFields EntryFields
Buttonz CoolButtonz
ThisWindow           CLASS(WindowManager)
Ask                    PROCEDURE(),DERIVED
Init                   PROCEDURE(),BYTE,PROC,DERIVED
Kill                   PROCEDURE(),BYTE,PROC,DERIVED
Run                    PROCEDURE(),BYTE,PROC,DERIVED
TakeAccepted           PROCEDURE(),BYTE,PROC,DERIVED
TakeEvent              PROCEDURE(),BYTE,PROC,DERIVED
                     END

Toolbar              ToolbarClass
Resizer              CLASS(WindowResizeClass)
Init                   PROCEDURE(BYTE AppStrategy=AppStrategy:Resize,BYTE SetWindowMinSize=False,BYTE SetWindowMaxSize=False)
WindowResizeEnd        PROCEDURE(),DERIVED
                     END

CurCtrlFeq          LONG
FieldColorQueue     QUEUE
Feq                   LONG
OldColor              LONG
                    END

  CODE
  GlobalResponse = ThisWindow.Run()                        ! Opens the window and starts an Accept Loop

!---------------------------------------------------------------------------
DefineListboxStyle ROUTINE
!|
!| This routine create all the styles to be shared in this window
!| It`s called after the window open
!|
!---------------------------------------------------------------------------
InitEntries_ROUTINE    ROUTINE                !Pass Entry Fields to the Entries Class
 LOOP i#  = FirstField() TO LastField()
    IF i#{Prop:Type} = Create:Entry OR i#{Prop:Type} = Create:TEXT
      CoolEntryFields.AddEntryBG(i#,i#{PROP:Color},,GLO:2in1)
    END
 END !LOOP
EntriesPosition_ROUTINE     ROUTINE                !Re-draw the entries if the window changed
 LOOP i#  = FirstField() TO LastField()
    IF i#{Prop:Type} = Create:Entry OR i#{Prop:Type} = Create:TEXT
      CoolEntryFields.RefreshEntry(i#,GLO:2in1)
    END
 END !LOOP
ResizeButtons_ROUTINE ROUTINE
 LOOP ThisBTN#  = FirstField() TO LastField()
    IF ThisBTN#{Prop:Type} = Create:Button
      Buttonz.ResizeButtons(ThisBTN#,GLO:2in1)
    END
 END !LOOP
InitButtonz_ROUTINE    ROUTINE                !Pass buttons to the Cool Buttons Class
 LOOP ThisBTN#  = FirstField() TO LastField()
    IF ThisBTN#{Prop:Type} = Create:Button
      Buttonz.AddButtonMimic(ThisBTN#,ThisBTN#{PROP:Color},,GLO:2in1)
    END
 END !LOOP
RebuildButtonz_ROUTINE     ROUTINE                !Re-draw the buttons if the window changed
 LOOP ThisBTN#  = FirstField() TO LastField()
    IF ThisBTN#{Prop:Type} = Create:Button
      Buttonz.RebuildButtons(ThisBTN#,GLO:2in1)
    END
 END !LOOP
ButtonzPosition_ROUTINE     ROUTINE     	!Re-draw the buttons if the window changed in 2 in 1 mode, Legacy, it should dissapear										
 DO Enable2in1Buttons_ROUTINE
Enable2in1Buttons_ROUTINE ROUTINE     	!Re-draw the buttons if the window changed in 2 in 1 mode
 LOOP ThisBTN#  = FirstField() TO LastField()
    IF ThisBTN#{Prop:Type} = Create:Button
      Buttonz.Buttons2in1(ThisBTN#,GLO:2in1)
    END
 END !LOOP
SetThemeWallpaper_ROUTINE ROUTINE

ThisWindow.Ask PROCEDURE

  CODE
  CASE SELF.Request                                        ! Configure the action message text
  OF ViewRecord
    ActionMessage = 'View Record'
  OF InsertRecord
    ActionMessage = 'Record Will Be Added'
  OF ChangeRecord
    ActionMessage = 'Record Will Be Changed'
  END
  QuickWindow{PROP:Text} = ActionMessage                   ! Display status message in title bar
  PARENT.Ask


ThisWindow.Init PROCEDURE

ReturnValue          BYTE,AUTO

  CODE
  GlobalErrors.SetProcedureName('Update_CSV')
  SELF.Request = GlobalRequest                             ! Store the incoming request
  ReturnValue = PARENT.Init()
  IF ReturnValue THEN RETURN ReturnValue.
  SELF.FirstField = ?OK
  SELF.VCRRequest &= VCRRequest
  SELF.Errors &= GlobalErrors                              ! Set this windows ErrorManager to the global ErrorManager
  CLEAR(GlobalRequest)                                     ! Clear GlobalRequest after storing locally
  CLEAR(GlobalResponse)
  SELF.AddItem(Toolbar)
  SELF.HistoryKey = CtrlH
  SELF.AddHistoryFile(CSV:Record,History::CSV:Record)
  SELF.AddHistoryField(?CSV:RecordNumber,1)
  SELF.AddHistoryField(?CSV:Zipcode,2)
  SELF.AddHistoryField(?CSV:ZipCodeType,3)
  SELF.AddHistoryField(?CSV:City,4)
  SELF.AddHistoryField(?CSV:State,5)
  SELF.AddHistoryField(?CSV:Lat,6)
  SELF.AddHistoryField(?CSV:Long,7)
  SELF.AddHistoryField(?CSV:Country,8)
  SELF.AddHistoryField(?CSV:LocationText,9)
  SELF.AddHistoryField(?CSV:Location,10)
  SELF.AddUpdateFile(Access:CSV_US_DB)
  SELF.AddItem(?Cancel,RequestCancelled)                   ! Add the cancel control to the window manager
  Relate:CSV_US_DB.Open()                                  ! File CSV_US_DB used by this procedure, so make sure it's RelationManager is open
  SELF.FilesOpened = True
  SELF.Primary &= Relate:CSV_US_DB
  IF SELF.Request = ViewRecord AND NOT SELF.BatchProcessing ! Setup actions for ViewOnly Mode
    SELF.InsertAction = Insert:None
    SELF.DeleteAction = Delete:None
    SELF.ChangeAction = Change:None
    SELF.CancelAction = Cancel:Cancel
    SELF.OkControl = 0
  ELSE
    SELF.ChangeAction = Change:Caller                      ! Changes allowed
    SELF.CancelAction = Cancel:Cancel+Cancel:Query         ! Confirm cancel
    SELF.OkControl = ?OK
    IF SELF.PrimeUpdate() THEN RETURN Level:Notify.
  END
  SELF.Open(QuickWindow)                                   ! Open window
  !Setting the LineHeight for every control of type LIST/DROP or COMBO in the window using the global setting.
  Do DefineListboxStyle
  IF SELF.Request = ViewRecord                             ! Configure controls for View Only mode
    ?CSV:RecordNumber{PROP:ReadOnly} = True
    ?CSV:Zipcode{PROP:ReadOnly} = True
    ?CSV:ZipCodeType{PROP:ReadOnly} = True
    ?CSV:City{PROP:ReadOnly} = True
    ?CSV:State{PROP:ReadOnly} = True
    ?CSV:Lat{PROP:ReadOnly} = True
    ?CSV:Long{PROP:ReadOnly} = True
    ?CSV:Country{PROP:ReadOnly} = True
    ?CSV:LocationText{PROP:ReadOnly} = True
    ?CSV:Location{PROP:ReadOnly} = True
  END
  Resizer.Init(AppStrategy:Surface,Resize:SetMinSize)      ! Controls like list boxes will resize, whilst controls like buttons will move
  SELF.AddItem(Resizer)                                    ! Add resizer to window manager
  SELF.SetAlerts()
   CoolEntryFields.Init(SELF)
   DO InitEntries_ROUTINE
   Buttonz.Init(SELF)
   DO InitButtonz_ROUTINE
  EnterByTabManager.Init(False)
  RETURN ReturnValue


ThisWindow.Kill PROCEDURE

ReturnValue          BYTE,AUTO

  CODE
  ReturnValue = PARENT.Kill()
  IF ReturnValue THEN RETURN ReturnValue.
  IF SELF.FilesOpened
    Relate:CSV_US_DB.Close()
  END
  GlobalErrors.SetProcedureName
  RETURN ReturnValue


ThisWindow.Run PROCEDURE

ReturnValue          BYTE,AUTO

  CODE
  ReturnValue = PARENT.Run()
  IF SELF.Request = ViewRecord                             ! In View Only mode always signal RequestCancelled
    ReturnValue = RequestCancelled
  END
  RETURN ReturnValue


ThisWindow.TakeAccepted PROCEDURE

ReturnValue          BYTE,AUTO

Looped BYTE
  CODE
  LOOP                                                     ! This method receive all EVENT:Accepted's
    IF Looped
      RETURN Level:Notify
    ELSE
      Looped = 1
    END
  ReturnValue = PARENT.TakeAccepted()
    CASE ACCEPTED()
    OF ?OK
      ThisWindow.Update()
      IF SELF.Request = ViewRecord AND NOT SELF.BatchProcessing THEN
         POST(EVENT:CloseWindow)
      END
    END
    RETURN ReturnValue
  END
  ReturnValue = Level:Fatal
  RETURN ReturnValue


ThisWindow.TakeEvent PROCEDURE

ReturnValue          BYTE,AUTO

Looped BYTE
  CODE
  LOOP                                                     ! This method receives all events
    IF Looped
      RETURN Level:Notify
    ELSE
      Looped = 1
    END
  IF EnterByTabManager.TakeEvent()
     RETURN(Level:Notify)
  END
  ReturnValue = PARENT.TakeEvent()
   !Something was triggered on the screen, Either EVENT:Accepted or EVENT:Selected
   CASE EVENT()
   OF EVENT:Accepted
    DO EntriesPosition_ROUTINE
    DO RebuildButtonz_ROUTINE
   END
    RETURN ReturnValue
  END
  ReturnValue = Level:Fatal
  RETURN ReturnValue


Resizer.Init PROCEDURE(BYTE AppStrategy=AppStrategy:Resize,BYTE SetWindowMinSize=False,BYTE SetWindowMaxSize=False)


  CODE
  PARENT.Init(AppStrategy,SetWindowMinSize,SetWindowMaxSize)
  SELF.SetParentDefaults()                                 ! Calculate default control parent-child relationships based upon their positions on the window


Resizer.WindowResizeEnd PROCEDURE


  CODE
  PARENT.WindowResizeEnd
   DO EntriesPosition_ROUTINE
   DO ResizeButtons_ROUTINE

