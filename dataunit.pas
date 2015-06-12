unit DataUnit;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, IBConnection, sqldb, FileUtil, Menus;

type

  { TDataBaseConnectionUnit }

  TDataBaseConnectionUnit = class(TDataModule)
    IBConnection: TIBConnection;
    SQLTransaction: TSQLTransaction;
  end;

  Condition = record
    Caption, Name: string;
  end;

const
  LogicOperatorsArray: array [0..1] of string = ('И', 'Или');
  LogicOperatorsArrayName: array [0..1] of string = ('And', 'Or');

var
  DataBaseConnectionUnit: TDataBaseConnectionUnit;
  Conditions: array of Condition;

implementation

procedure AddCondition(ConCap, ConName: string);
begin
  SetLength(Conditions, Length(Conditions) + 1);
  with Conditions[High(Conditions)] do
  begin
    Caption := ConCap;
    Name := ConName;
  end;
end;

{$R *.lfm}

initialization
  AddCondition('Равно', '=');
  AddCondition('Больше', '>');
  AddCondition('Меньше', '<');
  AddCondition('Содержит', 'CONTAINING');
  AddCondition('Не содержит', 'NOT CONTAINING');
  AddCondition('Начинается с', 'STARTING WITH');

end.
