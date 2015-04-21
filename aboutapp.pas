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
    Professor_txt: TLabel;
    Year_txt: TLabel;
    procedure Close_btnClick(Sender: TObject);
  end;

var
  AppInfo: TAppInfo;

implementation

{$R *.lfm}

{ TAppInfo }

procedure TAppInfo.Close_btnClick(Sender: TObject);
begin
  AppInfo.Close;
end;

end.
