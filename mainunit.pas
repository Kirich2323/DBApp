unit MainUnit;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, Forms, Controls, Graphics, Dialogs, Menus,
  MetaData, DataUnit, ListView, AboutApp, UEditCard, USchedule;

type

  { TMainForm }

  TMainForm = class(TForm)
    MainMenu: TMainMenu;
    List: TMenuItem;
    MainFile: TMenuItem;
    AboutApp: TMenuItem;
    Browse: TMenuItem;
    ISchedule: TMenuItem;
    OpenDialog: TOpenDialog;
    Quit: TMenuItem;
    procedure AboutAppClick(Sender: TObject);
    procedure BrowseClick(Sender: TObject);
    procedure AppInfoFormClose(Sender: TObject; var CloseAction: TCloseAction);
    procedure IScheduleClick(Sender: TObject);
    procedure ListFormClose(Sender: TObject; var CloseAction: TCloseAction);
    procedure FormCreate(Sender: TObject);
    procedure MenuItemClick(Sender: TObject);
    procedure QuitClick(Sender: TObject);
    procedure UpdateForms();
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
procedure TMainForm.CreateListReference();
var
  i: integer;
begin
  for i := 0 to High(TableArray) do
  begin
    Reference := TMenuItem.Create(Reference);
    List.Add(Reference);
    with Reference do
    begin
      Visible := True;
      Caption := TableArray[i].Caption;
      Tag := i;
      OnClick := @MenuItemClick;
    end;
    Setlength(FormsArray, Length(FormsArray) + 1);
    SetLength(IsFormCreated, length(IsFormCreated) + 1);
  end;
end;

procedure TMainForm.UpdateForms();
var
  i: integer;
begin
  with Screen do
    for i := 0 to (FormCount - 1) do
    begin
      if Forms[i] is TListForm then
        TListForm(Forms[i]).CustomizeColumns()
      else
      if Forms[i] is TEditCard then
        TEditCard(Forms[i]).RefreshEditCard()
      else
      if Forms[i] is TSchedule then
        TSchedule(Forms[i]).ApplyClick(TSchedule(Forms[i]).Apply);
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
  RefreshForms := @UpdateForms;
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

procedure TMainForm.IScheduleClick(Sender: TObject);
var
  IsCreated: boolean;
  i: integer;
begin
  IsCreated := False;
  with Screen do
    for i := 0 to FormCount - 1 do
      if Forms[i] is TSchedule then
      begin
        Forms[i].ShowOnTop;
        IsCreated := True;
        break;
      end;
  if not IsCreated then
  begin
    Application.CreateForm(TSchedule, Schedule);
    with Schedule do
    begin
      Show;
    end;
  end;
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
