unit UMyPanel;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Forms, Controls, StdCtrls, ExtCtrls, DataUnit, MetaData, Dialogs;

type

  TEvent = procedure of object;

  TMyPanel = class(TPanel)
  public
    FieldType: string;
    AndOrBox: TComboBox;
    FieldNamesBox: TComboBox;
    NativeName: TStringList;
    ConditionsBox: TCombobox;
    DeleteButton: TButton;
    Edit: TEdit;
    constructor Create(TheOwner: TComponent);
  end;

  TMainFilter = class
    Filters: array of TMyPanel;
    BackGround: TPanel;
    AcceptBtn: TButton;
    DeleteBtn: TButton;
    AddNewBtn: TButton;
    PreviousControl: TWinControl;
    Parent: TWinControl;
    VisibleFields: array of TField;
    AcceptEvent: TEvent;
    constructor Create(P: TWinControl; Fields: array of TField; CurEvent: Tevent);
    procedure AddClick(Sender: TObject);
    procedure DeleteAllFilters(Sender: TObject);
    procedure RemovePanel(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: integer);
    procedure AcceptClick(Sender: TObject);
    procedure PanelItemChange(Sender: TObject);
  end;

implementation

constructor TMainFilter.Create(P: TWinControl; Fields: array of TField;
  CurEvent: Tevent);
var
  i: integer;
begin
  Parent := P;
  BackGround := TPanel.Create(Parent);
  AcceptBtn := TButton.Create(BackGround);
  DeleteBtn := TButton.Create(BackGround);
  AddNewBtn := TButton.Create(BackGround);
  PreviousControl := AddNewBtn;
  AcceptEvent := CurEvent;

  for i := 0 to High(Fields) do
    if Fields[i].Visible then
    begin
      SetLength(VisibleFields, Length(VisibleFields) + 1);
      VisibleFields[High(VisibleFields)] := Fields[i];
    end;
  with BackGround do
  begin
    Parent := P;
    Width := 800;
    Height := 30;
    Visible := True;
    BevelOuter := bvNone;
  end;

  with AddNewBtn do
  begin
    Parent := BackGround;
    Visible := True;
    Caption := 'Добавить фильтр';
    left := 10;
    Width := 140;
    OnClick := @AddClick;
  end;

  with AcceptBtn do
  begin
    Parent := BackGround;
    Visible := True;
    Width := 140;
    Caption := 'Приминить';
    Left := 150;
    OnClick := @AcceptClick;
  end;

  with DeleteBtn do
  begin
    Parent := BackGround;
    Visible := True;
    Caption := 'Удалить все фильтры';
    Width := 140;
    Left := 290;
    OnClick := @DeleteAllFilters;
  end;
end;

procedure TMainfilter.AddClick(Sender: TObject);
var
  i: integer;
begin
  SetLength(Filters, Length(Filters) + 1);
  Filters[High(Filters)] := TMyPanel.Create(Parent);
  AcceptBtn.Enabled := True;
  Filters[High(FIlters)].NativeName := TStringList.Create();

  BackGround.Height := BackGround.Height + 30;
  with Filters[High(Filters)] do
  begin
    Parent := BackGround;
    AnchorToNeighbour(akTop, 0, PreviousControl);
    Left := 10;
    with AndOrBox do
    begin
      if Length(Filters) = 1 then
        Visible := False;
      for i := 0 to 1 do
        Items[i] := LogicOperatorsArray[i];
      ItemIndex := 0;
      OnChange := @PanelItemChange;
    end;

    with FieldNamesBox do
    begin
      for i := 0 to High(VisibleFields) do
        Items[i] := VisibleFields[i].Caption;
      ItemIndex := 0;
      OnChange := @PanelItemChange;
    end;

    with NativeName do
    begin
      for i := 0 to High(VisibleFields) do
        Add(VisibleFields[i].NativeName);
    end;

    with ConditionsBox do
    begin
      for i := 0 to High(Conditions) do
        Items[i] := Conditions[i].Caption;
      ItemIndex := 0;
      OnChange := @PanelItemChange;
    end;

    with DeleteButton do
    begin
      OnMouseDown := @RemovePanel;
      Tag := High(Filters);
    end;

    with Edit do
      OnChange := @PanelItemChange;
  end;
  PreviousControl := Filters[High(Filters)];
end;

procedure TMainFilter.AcceptClick(Sender: TObject);
begin
  AcceptEvent;
  AcceptBtn.Enabled := False;
end;

procedure TMainFilter.PanelItemChange(Sender: TObject);
begin
  AcceptBtn.Enabled := True;
end;

procedure TMainFilter.DeleteAllFilters(Sender: TObject);
var
  i: integer;
begin
  AcceptBtn.Enabled := True;
  if Length(Filters) > 0 then
    if Filters[0].Enabled = False then
    begin
      for i := 2 to High(Filters) do
        Filters[i].Free;
      SetLength(Filters, 2);
      BackGround.Height := 90;
      PreviousControl := Filters[1];
    end
    else
    begin
      for i := 0 to High(Filters) do
        Filters[i].Free;
      SetLength(Filters, 0);
      BackGround.Height := 30;
      PreviousControl := AddNewBtn;
    end;
end;

constructor TMyPanel.Create(TheOwner: TComponent);

begin
  inherited Create(TheOwner);
  Width := 800;
  Height := 30;
  Visible := True;
  BevelOuter := bvNone;

  AndOrBox := TComboBox.Create(self);
  FieldNamesBox := TComboBox.Create(self);
  ConditionsBox := TComboBox.Create(self);
  DeleteButton := TButton.Create(self);
  Edit := TEdit.Create(self);

  with AndOrBox do
  begin
    Parent := Self;
    ReadOnly := True;
    Visible := True;
    Left := 0;
    Width := 50;
  end;
  with FieldNamesBox do
  begin
    Parent := Self;
    ReadOnly := True;
    Visible := True;
    Width := 200;
    Left := 60;
  end;
  with ConditionsBox do
  begin
    Parent := Self;
    ReadOnly := True;
    Visible := True;
    Width := 150;
    left := 280;
  end;
  with DeleteButton do
  begin
    Parent := Self;
    Visible := True;
    Caption := 'Удалить';
    Left := 660;
  end;
  with Edit do
  begin
    Parent := Self;
    Left := 450;
    Width := 200;
  end;
end;

procedure TMainFilter.RemovePanel(Sender: TObject; Button: TMouseButton;
  Shift: TShiftState; X, Y: integer);
var
  j: integer;
begin
  AcceptBtn.Enabled := True;
  j := TButton(Sender).Tag;
  if j <> High(Filters) then
  begin
    Filters[j].Free;
    for j := j to (High(Filters) - 1) do
      Filters[j] := Filters[j + 1];
    SetLength(Filters, Length(Filters) - 1);
    Filters[0].AndOrBox.Visible := False;
    Filters[0].AnchorToNeighbour(akTop, 0, AddNewBtn);
    Filters[0].DeleteButton.Tag := 0;
    for j := 1 to High(Filters) do
    begin
      Filters[j].DeleteButton.Tag := j;
      Filters[j].AnchorToNeighbour(akTop, 0, Filters[j - 1]);
    end;
  end
  else
  begin
    Filters[j].Free;
    SetLength(Filters, Length(Filters) - 1);
  end;
  if Length(Filters) > 0 then
    PreviousControl := Filters[High(Filters)]
  else
    PreviousControl := AddNewBtn;
  BackGround.Height := BackGround.Height - 30;
end;

end.
