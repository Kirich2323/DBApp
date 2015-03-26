unit DataUnit;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, IBConnection, sqldb, FileUtil, Menus;

type

  { TDataBaseConnectionUnit }

  TDataBaseConnectionUnit = class(TDataModule)
    IBConnection: TIBConnection;
    SQLTransaction: TSQLTransaction;
  end;

var
  DataBaseConnectionUnit: TDataBaseConnectionUnit;

implementation

{$R *.lfm}

end.

