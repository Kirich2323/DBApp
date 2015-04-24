program project1;

{$mode objfpc}{$H+}

uses {$IFDEF UNIX} {$IFDEF UseCThreads}
  cthreads, {$ENDIF} {$ENDIF}
  Interfaces,
  Forms,
  MainUnit,
  DataUnit,
  Listview,
  MetaData,
  AboutApp,
  SQLQueryCreation,
  UMyPanel, UEditCard;

{$R *.res}

begin
  RequireDerivedFormResource := True;
  Application.Initialize;
  Application.CreateForm(TMainForm, MainForm);
  Application.CreateForm(TDataBaseConnectionUnit, DataBaseConnectionUnit);
  Application.CreateForm(TAppInfo, AppInfo);
  //Application.CreateForm(TEditCard, EditCard);
  Application.Run;
end.
