unit AboutApp;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, Forms, Controls, Graphics, Dialogs, StdCtrls, MetaData;

type

  { TAppInfo }

  TAppInfo = class(TForm)
    Close_btn: TButton;
    Developer_txt: TLabel;
    procedure Close_btnClick(Sender: TObject);
    procedure FormClose(Sender: TObject; var CloseAction: TCloseAction);
  end;

var
  AppInfo: TAppInfo;

implementation

{$R *.lfm}

{ TAppInfo }

procedure TAppInfo.Close_btnClick(Sender: TObject);
begin
  isAboutAppCreated := False;
  AppInfo.Close;
end;

procedure TAppInfo.FormClose(Sender: TObject; var CloseAction: TCloseAction);
begin
  CloseAction := caFree;
end;

end.

