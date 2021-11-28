   PROGRAM



   INCLUDE('ABERROR.INC'),ONCE
   INCLUDE('ABFILE.INC'),ONCE
   INCLUDE('ABUTIL.INC'),ONCE
   INCLUDE('EFOCUS.INC'),ONCE
   INCLUDE('ERRORS.CLW'),ONCE
   INCLUDE('KEYCODES.CLW'),ONCE
   INCLUDE('ABFUZZY.INC'),ONCE
Fast             CLASS
Find                    PROCEDURE(STRING SearchSTR,STRING OnThisSTR,BYTE DebugMSG=0), BYTE
					END
  INCLUDE('EntryFields.inc'),ONCE
  INCLUDE('CoolButtonz.inc'),ONCE

   MAP
     MODULE('NEUTRINO_SEARCH_BC.CLW')
DctInit     PROCEDURE                                      ! Initializes the dictionary definition module
DctKill     PROCEDURE                                      ! Kills the dictionary definition module
     END
!--- Application Global and Exported Procedure Definitions --------------------------------------------
     MODULE('NEUTRINO_SEARCH001.CLW')
Main                   PROCEDURE   !Neutrino Search Demo
     END
   END

GLO:2IN1             BYTE(FALSE)
SilentRunning        BYTE(0)                               ! Set true when application is running in 'silent mode'

!region File Declaration
CSV_US_DB            FILE,DRIVER('BASIC','/ALWAYSQUOTE=OFF /FILEBUFFERS=128 /FIRSTROWHEADER=ON /QUICKSCAN=ON'),NAME('zip_db_us.csv'),PRE(CSV),BINDABLE,THREAD !                     
Record                   RECORD,PRE()
RecordNumber                LONG                           !                     
Zipcode                     STRING(20)                     !                     
ZipCodeType                 STRING(20)                     !                     
City                        STRING(64)                     !                     
State                       STRING(20)                     !                     
Lat                         LONG                           !                     
Long                        LONG                           !                     
Country                     STRING(64)                     !                     
LocationText                STRING(64)                     !                     
Location                    STRING(64)                     !                     
                         END
                     END                       

SV_US_DB             FILE,DRIVER('TOPSPEED'),RECLAIM,OEM,NAME('US_ZIP_DB'),PRE(SV_),BINDABLE,CREATE,THREAD !                     
Record_Number_Key        KEY(SV_:RecordNumber),NOCASE      !                     
Record                   RECORD,PRE()
RecordNumber                LONG                           !                     
Zipcode                     STRING(20)                     !                     
ZipCodeType                 STRING(20)                     !                     
City                        STRING(64)                     !                     
State                       STRING(20)                     !                     
Lat                         LONG                           !                     
Long                        LONG                           !                     
Country                     STRING(64)                     !                     
LocationText                STRING(64)                     !                     
Location                    STRING(64)                     !                     
                         END
                     END                       

SQLiteUSDB           FILE,DRIVER('SQLite','/AUTOINC=FALSE'),RECLAIM,OEM,OWNER('USDB.sqlite'),PRE(SQLT),BINDABLE,CREATE,THREAD !                     
SQLRecordNumberKey       KEY(SQLT:RecordNumber),NOCASE,PRIMARY !                     
Record                   RECORD,PRE()
RecordNumber                LONG                           !                     
Zipcode                     STRING(20)                     !                     
ZipCodeType                 STRING(20)                     !                     
City                        STRING(64)                     !                     
State                       STRING(20)                     !                     
Lat                         LONG                           !                     
Long                        LONG                           !                     
Country                     STRING(64)                     !                     
LocationText                STRING(64)                     !                     
Location                    STRING(64)                     !                     
                         END
                     END                       

!endregion

Access:CSV_US_DB     &FileManager,THREAD                   ! FileManager for CSV_US_DB
Relate:CSV_US_DB     &RelationManager,THREAD               ! RelationManager for CSV_US_DB
Access:SV_US_DB      &FileManager,THREAD                   ! FileManager for SV_US_DB
Relate:SV_US_DB      &RelationManager,THREAD               ! RelationManager for SV_US_DB
Access:SQLiteUSDB    &FileManager,THREAD                   ! FileManager for SQLiteUSDB
Relate:SQLiteUSDB    &RelationManager,THREAD               ! RelationManager for SQLiteUSDB

FuzzyMatcher         FuzzyClass                            ! Global fuzzy matcher
GlobalErrorStatus    ErrorStatusClass,THREAD
GlobalErrors         ErrorClass                            ! Global error manager
INIMgr               INIClass                              ! Global non-volatile storage manager
GlobalRequest        BYTE(0),THREAD                        ! Set when a browse calls a form, to let it know action to perform
GlobalResponse       BYTE(0),THREAD                        ! Set to the response from the form
VCRRequest           LONG(0),THREAD                        ! Set to the request from the VCR buttons

Dictionary           CLASS,THREAD
Construct              PROCEDURE
Destruct               PROCEDURE
                     END


  CODE
  GlobalErrors.Init(GlobalErrorStatus)
  FuzzyMatcher.Init                                        ! Initilaize the browse 'fuzzy matcher'
  FuzzyMatcher.SetOption(MatchOption:NoCase, 1)            ! Configure case matching
  FuzzyMatcher.SetOption(MatchOption:WordOnly, 0)          ! Configure 'word only' matching
  INIMgr.Init('.\Neutrino_Search.INI', NVD_INI)            ! Configure INIManager to use INI file
  DctInit()
  Main
  INIMgr.Update
  INIMgr.Kill                                              ! Destroy INI manager
  FuzzyMatcher.Kill                                        ! Destroy fuzzy matcher
    
Fast.Find           PROCEDURE(STRING SearchSTR,STRING OnThisSTR,BYTE DebugMSG)

Found_STR               BYTE()
SearchSTR_C             &STRING
OnThisSTR_C             &STRING
SlicedSTR_C             &STRING
XXN                     LONG
LenSTRSearch            LONG


CODE
	
	Found_STR = FALSE
	
	
		
	IF LEN(CLIP(LEFT(OnThisSTR))) => 1 !Only if there is something, otherwise we will cause an error accesing something that does not exist
		
	
		
		SearchSTR_C &= NEW STRING(LEN(CLIP(LEFT(SearchSTR))))
		OnThisSTR_C &= NEW STRING(LEN(CLIP(LEFT(OnThisSTR)))+1) !+1 in case the passed string is a CSTRING, otherwise it will truncate the CSTRING
		

		SearchSTR_C = UPPER(CLIP(LEFT(SearchSTR)))
		OnThisSTR_C = UPPER(CLIP(LEFT(OnThisSTR)))
	
		LenSTRSearch = LEN(CLIP(LEFT(SearchSTR)))
		SlicedSTR_C &= NEW STRING(LenSTRSearch)
		
		
	
		IF LEN(SearchSTR_C) > LEN(OnThisSTR_C) !no point on comparing on something smaller than the search string itself... Counting is faster than comparing anyway...
			DO DISPOSE_ROUTINE
		END
		
			
		IF SearchSTR_C = OnThisSTR_C !If by luck, we are looking at an exact match.... :-)
			DO Return_Found_ROUTINE
		END	
	
		!We are using string slicing... 
		
		LOOP XXN = 1 TO (LEN(OnThisSTR_C) - LenSTRSearch)
			
			SlicedSTR_C = OnThisSTR_C[XXN : LenSTRSearch + XXN]
			
			IF SearchSTR_C = SlicedSTR_C
				
				DO Return_Found_ROUTINE
			END		
	
		END

	!IF Still here
	
		Found_STR = FALSE
	END
	
	
	DO DISPOSE_ROUTINE
	
	


Return_Found_ROUTINE        ROUTINE
	
	Found_STR = TRUE
	
	DO DISPOSE_ROUTINE
	
	RETURN(Found_STR)
	
	EXIT

DISPOSE_ROUTINE     ROUTINE
	
	
	DISPOSE(SearchSTR_C)
	DISPOSE(OnThisSTR_C)
	DISPOSE(SlicedSTR_C)

	RETURN(Found_STR)
	
	EXIT


Dictionary.Construct PROCEDURE

  CODE
  IF THREAD()<>1
     DctInit()
  END


Dictionary.Destruct PROCEDURE

  CODE
  DctKill()

