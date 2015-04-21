unit UMyPanel;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Forms, Controls, StdCtrls, ExtCtrls;

type

  TMyPanel = class(TPanel)
  public
    FieldType: string;
    AndOrBox: TComboBox;
    FieldNamesBox: TComboBox;
    ConditionsBox: TCombobox;
    DeleteButton: TButton;
    Edit: TEdit;
    constructor Create(TheOwner: TComponent);
  end;

implementation

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

end.
