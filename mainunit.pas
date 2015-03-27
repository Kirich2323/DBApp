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
    procedure MenuItem1Click(Sender: TObject);
    procedure QuitClick(Sender: TObject);
  private
    reference: TMenuItem;
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
    reference := TMenuItem.Create(Reference);
    List.Add(reference);
    reference.Visible := True;
    reference.Caption := TableArray[i].Caption;
    reference.Tag := i;
    reference.OnClick := @MenuItem1Click;
    setlength(FormArray, Length(FormArray) + 1);
    setLength(isFormCreated, length(isFormCreated) + 1);
  end;
end;

procedure TMainForm.MenuItem1Click(Sender: TObject);
var
  index: integer;
begin
  index := TMenuItem(Sender).Tag;
  if not TMenuItem(Sender).Checked then
  begin
    Application.CreateForm(TListForm, FormArray[index]);
    //FormArray[index].OnClose := @FormClose;
    FormArray[index].Caption :=
      TableArray[index].Caption;
    FormArray[index].TableTag := TMenuItem(Sender).Tag;
    FormArray[index].Show;
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
