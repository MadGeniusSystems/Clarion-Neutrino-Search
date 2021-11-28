  MEMBER('Neutrino_Search.clw')

  INCLUDE('ABFILE.INC'),ONCE

  MAP
Neutrino_Search_BC0:DctInit    PROCEDURE()
Neutrino_Search_BC0:DctKill    PROCEDURE()
Neutrino_Search_BC0:FilesInit  PROCEDURE()
  END

Hide:Access:CSV_US_DB CLASS(FileManager),TYPE              ! FileManager for CSV_US_DB
Init                   PROCEDURE(),DERIVED
Kill                   PROCEDURE(),DERIVED
UseFile                PROCEDURE(BYTE UseType = UseType:Uses),BYTE,PROC,DERIVED
                     END


Hide:Relate:CSV_US_DB CLASS(RelationManager),TYPE          ! RelationManager for CSV_US_DB
Init                   PROCEDURE
Kill                   PROCEDURE(),DERIVED
                     END

Hide:Access:SV_US_DB CLASS(FileManager),TYPE               ! FileManager for SV_US_DB
Init                   PROCEDURE(),DERIVED
Kill                   PROCEDURE(),DERIVED
UseFile                PROCEDURE(BYTE UseType = UseType:Uses),BYTE,PROC,DERIVED
                     END


Hide:Relate:SV_US_DB CLASS(RelationManager),TYPE           ! RelationManager for SV_US_DB
Init                   PROCEDURE
Kill                   PROCEDURE(),DERIVED
                     END

Hide:Access:SQLiteUSDB CLASS(FileManager),TYPE             ! FileManager for SQLiteUSDB
Init                   PROCEDURE(),DERIVED
Kill                   PROCEDURE(),DERIVED
UseFile                PROCEDURE(BYTE UseType = UseType:Uses),BYTE,PROC,DERIVED
                     END


Hide:Relate:SQLiteUSDB CLASS(RelationManager),TYPE         ! RelationManager for SQLiteUSDB
Init                   PROCEDURE
Kill                   PROCEDURE(),DERIVED
                     END


_Hide:Access:CSV_US_DB &Hide:Access:CSV_US_DB,AUTO,THREAD
_Hide:Relate:CSV_US_DB &Hide:Relate:CSV_US_DB,AUTO,THREAD
_Hide:Access:SV_US_DB &Hide:Access:SV_US_DB,AUTO,THREAD
_Hide:Relate:SV_US_DB &Hide:Relate:SV_US_DB,AUTO,THREAD
_Hide:Access:SQLiteUSDB &Hide:Access:SQLiteUSDB,AUTO,THREAD
_Hide:Relate:SQLiteUSDB &Hide:Relate:SQLiteUSDB,AUTO,THREAD


Neutrino_Search_BC0:DctInit PROCEDURE()
  CODE
  _Hide:Access:CSV_US_DB &= NEW(Hide:Access:CSV_US_DB)
  _Hide:Relate:CSV_US_DB &= NEW(Hide:Relate:CSV_US_DB)
  _Hide:Access:SV_US_DB &= NEW(Hide:Access:SV_US_DB)
  _Hide:Relate:SV_US_DB &= NEW(Hide:Relate:SV_US_DB)
  _Hide:Access:SQLiteUSDB &= NEW(Hide:Access:SQLiteUSDB)
  _Hide:Relate:SQLiteUSDB &= NEW(Hide:Relate:SQLiteUSDB)
  Relate:CSV_US_DB &= _Hide:Relate:CSV_US_DB
  Relate:SV_US_DB &= _Hide:Relate:SV_US_DB
  Relate:SQLiteUSDB &= _Hide:Relate:SQLiteUSDB


Neutrino_Search_BC0:FilesInit PROCEDURE()
  CODE
  _Hide:Relate:CSV_US_DB.Init
  _Hide:Relate:SV_US_DB.Init
  _Hide:Relate:SQLiteUSDB.Init


Neutrino_Search_BC0:DctKill PROCEDURE()
  CODE
  _Hide:Relate:CSV_US_DB.Kill
  DISPOSE(_Hide:Relate:CSV_US_DB)
  _Hide:Relate:SV_US_DB.Kill
  DISPOSE(_Hide:Relate:SV_US_DB)
  _Hide:Relate:SQLiteUSDB.Kill
  DISPOSE(_Hide:Relate:SQLiteUSDB)


Hide:Relate:CSV_US_DB.Init PROCEDURE()
  CODE
  _Hide:Access:CSV_US_DB.Init
  SELF.Init(Access:CSV_US_DB,1)


Hide:Access:CSV_US_DB.Init PROCEDURE

  CODE
  SELF.Initialized = False
  SELF.Buffer &= CSV:Record
  SELF.FileNameValue = 'CSV_US_DB'
  SELF.SetErrors(GlobalErrors)
  SELF.File &= CSV_US_DB
  PARENT.Init
  Access:CSV_US_DB &= SELF


Hide:Access:CSV_US_DB.Kill PROCEDURE

  CODE
  PARENT.Kill
  Access:CSV_US_DB &= NULL                                 ! File manager has been destroyed to assign null, it is an error to reference this after this point


Hide:Access:CSV_US_DB.UseFile PROCEDURE(BYTE UseType = UseType:Uses)

ReturnValue          BYTE,AUTO

  CODE
  IF UseType ~= UseType:Initialize
    SELF.UseFile(UseType:Initialize)                       !Recursive call to ensure initialization takes place
  END
  IF UseType = UseType:Initialize AND ~SELF.Initialized
    SELF.InUseFile = True
    SELF.Init(CSV_US_DB,GlobalErrors)
    SELF.Create = 0
    SELF.LockRecover = 10
  END
                                                           !SELF.InUseFile will be set to False in PARENT
  ReturnValue = PARENT.UseFile(UseType)
  RETURN ReturnValue


Hide:Relate:CSV_US_DB.Kill PROCEDURE

  CODE
  _Hide:Access:CSV_US_DB.Kill                              ! Kill the file manager
  PARENT.Kill
  Relate:CSV_US_DB &= NULL                                 ! Assign NULL to the RelationManager's FileManager reference, it is an error to reference this after this point
  DISPOSE(_Hide:Access:CSV_US_DB)                          ! destroy the file manager


Hide:Relate:SV_US_DB.Init PROCEDURE()
  CODE
  _Hide:Access:SV_US_DB.Init
  SELF.Init(Access:SV_US_DB,1)


Hide:Access:SV_US_DB.Init PROCEDURE

  CODE
  SELF.Initialized = False
  SELF.Buffer &= SV_:Record
  SELF.FileNameValue = 'SV_US_DB'
  SELF.SetErrors(GlobalErrors)
  SELF.File &= SV_US_DB
  PARENT.Init
  Access:SV_US_DB &= SELF


Hide:Access:SV_US_DB.Kill PROCEDURE

  CODE
  PARENT.Kill
  Access:SV_US_DB &= NULL                                  ! File manager has been destroyed to assign null, it is an error to reference this after this point


Hide:Access:SV_US_DB.UseFile PROCEDURE(BYTE UseType = UseType:Uses)

ReturnValue          BYTE,AUTO

  CODE
  IF UseType ~= UseType:Initialize
    SELF.UseFile(UseType:Initialize)                       !Recursive call to ensure initialization takes place
  END
  IF UseType = UseType:Initialize AND ~SELF.Initialized
    SELF.InUseFile = True
    SELF.Init(SV_US_DB,GlobalErrors)
    SELF.Create = 1
    SELF.LockRecover = 10
    SELF.AddKey(SV_:Record_Number_Key,'SV_:Record_Number_Key',1)
  END
                                                           !SELF.InUseFile will be set to False in PARENT
  ReturnValue = PARENT.UseFile(UseType)
  RETURN ReturnValue


Hide:Relate:SV_US_DB.Kill PROCEDURE

  CODE
  _Hide:Access:SV_US_DB.Kill                               ! Kill the file manager
  PARENT.Kill
  Relate:SV_US_DB &= NULL                                  ! Assign NULL to the RelationManager's FileManager reference, it is an error to reference this after this point
  DISPOSE(_Hide:Access:SV_US_DB)                           ! destroy the file manager


Hide:Relate:SQLiteUSDB.Init PROCEDURE()
  CODE
  _Hide:Access:SQLiteUSDB.Init
  SELF.Init(Access:SQLiteUSDB,1)


Hide:Access:SQLiteUSDB.Init PROCEDURE

  CODE
  SELF.Initialized = False
  SELF.Buffer &= SQLT:Record
  SELF.FileNameValue = 'SQLiteUSDB'
  SELF.SetErrors(GlobalErrors)
  SELF.File &= SQLiteUSDB
  PARENT.Init
  Access:SQLiteUSDB &= SELF


Hide:Access:SQLiteUSDB.Kill PROCEDURE

  CODE
  PARENT.Kill
  Access:SQLiteUSDB &= NULL                                ! File manager has been destroyed to assign null, it is an error to reference this after this point


Hide:Access:SQLiteUSDB.UseFile PROCEDURE(BYTE UseType = UseType:Uses)

ReturnValue          BYTE,AUTO

  CODE
  IF UseType ~= UseType:Initialize
    SELF.UseFile(UseType:Initialize)                       !Recursive call to ensure initialization takes place
  END
  IF UseType = UseType:Initialize AND ~SELF.Initialized
    SELF.InUseFile = True
    SELF.Init(SQLiteUSDB,GlobalErrors)
    SELF.Create = 1
    SELF.LockRecover = 10
    SELF.AddKey(SQLT:SQLRecordNumberKey,'SQLT:SQLRecordNumberKey',1)
  END
                                                           !SELF.InUseFile will be set to False in PARENT
  ReturnValue = PARENT.UseFile(UseType)
  RETURN ReturnValue


Hide:Relate:SQLiteUSDB.Kill PROCEDURE

  CODE
  _Hide:Access:SQLiteUSDB.Kill                             ! Kill the file manager
  PARENT.Kill
  Relate:SQLiteUSDB &= NULL                                ! Assign NULL to the RelationManager's FileManager reference, it is an error to reference this after this point
  DISPOSE(_Hide:Access:SQLiteUSDB)                         ! destroy the file manager

