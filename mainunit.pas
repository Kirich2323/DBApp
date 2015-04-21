unit MainUnit;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, Forms, Controls, Graphics, Dialogs, Menus,
  MetaData, DataUnit, ListView, AboutApp;

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
    procedure AppInfoFormClose(Sender: TObject; var CloseAction: TCloseAction);
    procedure ListFormClose(Sender: TObject; var CloseAction: TCloseAction);
    procedure FormCreate(Sender: TObject);
    procedure MenuItemClick(Sender: TObject);
    procedure QuitClick(Sender: TObject);
  private
    Reference: TMenuItem;
    FormsArray: array of TListForm;
    IsFormCreated: array of boolean;
    IsAboutAppCreated: boolean;
    procedure CreateListReference();
  end;

var
  MainForm: TMainForm;

implementation

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
    Setlength(FormsArray, Length(FormsArray) + 1);
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
    Application.CreateForm(TListForm, FormsArray[Index]);
    with FormsArray[Index] do
    begin
      Caption := TableArray[Index].Caption;
      TableTag := Index;
      Table := TableArray[Index];
      Show;
      OnClose := @ListFormClose;
      IsFormCreated[Index] := True;
    end;
    List.Items[Index].Checked := True;
  end
  else
    FormsArray[TmenuItem(Sender).Tag].ShowOnTop;
end;

procedure TMainForm.QuitClick(Sender: TObject);
begin
  Application.Terminate;
end;

procedure TMainForm.FormCreate(Sender: TObject);
begin
  CreateListReference;
end;

procedure TMainForm.BrowseClick(Sender: TObject);
begin
  if OpenDialog.Execute then
    DataBaseConnectionUnit.IBConnection.DatabaseName := OpenDialog.FileName;
end;

procedure TMainForm.AppInfoFormClose(Sender: TObject; var CloseAction: TCloseAction);
begin
  AboutApp.Checked := False;
  CloseAction := caFree;
end;

procedure TMainForm.ListFormClose(Sender: TObject; var CloseAction: TCloseAction);
begin
  List.Items[TListForm(Sender).TableTag].Checked := False;
  CloseAction := caFree;
end;

procedure TMainForm.AboutAppClick(Sender: TObject);
begin
  if not (AboutApp.Checked) then
  begin
    Application.CreateForm(TAppInfo, AppInfo);
    with AppInfo do
    begin
      AppInfo.Show;
      OnClose := @AppInfoFormClose;
    end;
    IsAboutAppCreated := True;
    AboutApp.Checked := True;
  end
  else
    Appinfo.ShowOnTop;
end;

end.
