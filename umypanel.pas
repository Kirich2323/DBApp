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
  AndOrBox := TComboBox.Create(self);
  FieldNamesBox := TComboBox.Create(self);
  ConditionsBox := TComboBox.Create(self);
  DeleteButton := TButton.Create(self);
  Edit := TEdit.Create(self);
  with AndOrBox do
  begin
    Visible := True;
    Left := 0;
    Width := 50;
  end;
  with FieldNamesBox do
  begin
    Visible := True;
    Width := 200;
    Left := 60;
    BevelOuter := bvNone;
  end;
  with ConditionsBox do
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
    //Caption := 'Введите значение';
  end;
end;

end.
