  MEMBER('Neutrino_Search.clw')

  PRAGMA('define(init_priority=>3)')

  MAP
    MODULE('Neutrino_Search_BC0.CLW')
Neutrino_Search_BC0:DctInit             PROCEDURE()
Neutrino_Search_BC0:DctKill             PROCEDURE()
Neutrino_Search_BC0:FilesInit           PROCEDURE()
    END
  END

DctInit PROCEDURE()
  CODE
  Neutrino_Search_BC0:DctInit
  Neutrino_Search_BC0:FilesInit


DctKill PROCEDURE()
  CODE
  Neutrino_Search_BC0:DctKill

