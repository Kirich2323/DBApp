unit MainUnit;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, Forms, Controls, Graphics, Dialogs, Menus,
  StdCtrls, MetaData, DataUnit, ListView, AboutApp;

type

  { TMainForm }

  TMainForm = class(TForm)
    MainMenu: TMainMenu;
    List: TMenuItem;
    MainFile: TMenuItem;
    AboutApp: TMenuItem;
    Browse: TMenuItem;
    OpenDialog: TOpenDialog;
    Quit: TMenuItem;
    procedure AboutAppClick(Sender: TObject);
    procedure BrowseClick(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure ListClick(Sender: TObject);
    procedure MenuItemClick(Sender: TObject);
    procedure QuitClick(Sender: TObject);
  private
    Reference: TMenuItem;
    FormArray: array of TListForm;
    procedure CreateListReference();
  end;

var
  MainForm: TMainForm;

implementation

procedure FormClose(Sender: TObject; var CloseAction: TCloseAction);
begin
  CloseAction := caFree;
end;

{$R *.lfm}

{ TMainForm }
procedure TMainForm.CreateListReference();
var
  i: integer;
begin
  for i := 0 to High(TableArray) do
  begin
    Reference := TMenuItem.Create(Reference);
    List.Add(Reference);
    Reference.Visible := True;
    Reference.Caption := TableArray[i].Caption;
    Reference.Tag := i;
    Reference.OnClick := @MenuItemClick;
    Setlength(FormArray, Length(FormArray) + 1);
    SetLength(IsFormCreated, length(IsFormCreated) + 1);
  end;
end;

procedure TMainForm.MenuItemClick(Sender: TObject);
var
  Index: integer;
begin
  Index := TMenuItem(Sender).Tag;
  if not TMenuItem(Sender).Checked then
  begin
    Application.CreateForm(TListForm, FormArray[Index]);
    FormArray[Index].Caption :=
      TableArray[Index].Caption;
    FormArray[Index].TableTag := TMenuItem(Sender).Tag;
    FormArray[Index].Show;
    isFormCreated[TMenuItem(Sender).Tag] := True;
  end
  else
  begin
    FormArray[TmenuItem(Sender).Tag].ShowOnTop;
  end;
end;

procedure TMainForm.QuitClick(Sender: TObject);
begin
  Application.Terminate;
end;

procedure TMainForm.FormCreate(Sender: TObject);
begin
  CreateListReference;
end;

procedure TMainForm.ListClick(Sender: TObject);
var
  i: integer;
begin
  for i := 0 to high(isFormCreated) do
    if isFormCreated[i] then
      List.Items[i].Checked := True
    else
      list.Items[i].Checked := False;
end;

procedure TMainForm.BrowseClick(Sender: TObject);
begin
  if OpenDialog.Execute then
    DataBaseConnectionUnit.IBConnection.DatabaseName := OpenDialog.FileName;

end;

procedure TMainForm.AboutAppClick(Sender: TObject);
begin
  if not (isAboutAppCreated) then
  begin
    Application.CreateForm(TAppInfo, AppInfo);
    AppInfo.Show;
    isAboutAppCreated := True;
  end
  else
    Appinfo.ShowOnTop;
end;

initialization

end.
