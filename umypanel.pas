unit UMyPanel;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Forms, Controls, StdCtrls, ExtCtrls;

type

  TMyParentPanel = class(TPanel)
  public
    AndOrBox: TComboBox;
    FieldNames: TComboBox;
    Conditions: TCombobox;
    DeleteButton: TButton;
    Edit: TEdit;
    constructor Create(TheOwner: TComponent);
  end;

implementation

constructor TMyParentPanel.Create(TheOwner: TComponent);
begin
  inherited Create(TheOwner);
  Width := 800;
  Height := 30;
  Visible := True;
  AndOrBox := TComboBox.Create(self);
  FieldNames := TComboBox.Create(self);
  Conditions := TComboBox.Create(self);
  DeleteButton := TButton.Create(self);
  Edit := TEdit.Create(self);
  with AndOrBox do
  begin
    Visible := True;
    Left := 0;
    Width := 50;
  end;
  with FieldNames do
  begin
    Visible := True;
    Width := 200;
    Left := 60;
    BevelOuter := bvNone;
  end;
  with Conditions do
  begin
    Visible := True;
    Width := 150;
    left := 280;
  end;
  with DeleteButton do
  begin
    Visible := True;
    Caption := 'Удалить';
    Left := 660;
  end;
  with Edit do
  begin
    Left := 450;
    Width := 200;
    Caption := 'Введите значение';
  end;
end;

end.
