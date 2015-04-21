unit SQLQueryCreation;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, MetaData, Dialogs, UMyPanel;

type
  SortField = record
    Name: string;
    State, Index: integer;
  end;

  Condition = record
    Caption, Name: string;
  end;

const
  LogicOperatorsArray: array [0..1] of string = ('И', 'Или');
  LogicOperatorsArrayName: array [0..1] of string = ('And', 'Or');

function MainSQLQueryCreate(Table: TTable): string;
function SortSQLQueryCreate(SortArray: array of SortField): string;
function FilterSQLQueryCreate(FilterArray: array of TMyPanel;
  FieldsArray: array of string): string;

var
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

function MainSqlQueryCreate(Table: TTable): string;
var
  i: integer;
  CurTable: TTable;
begin
  Result := Format('Select %s.%s', [Table.Fields[0].Table, Table.Fields[0].Name]);
  for i := 1 to High(Table.Fields) do
    Result += Format(', %s.%s ', [Table.Fields[i].Table, Table.Fields[i].Name]);
  Result += Format(' From %s ', [Table.Name]);
  if Length(CurTable.RefefenceFields) > 0 then
    for i := 0 to high(Table.RefefenceFields) do
      Result += Format(' inner join %s on %s.%s = %s.%s',
        [Table.RefefenceFields[i].FromTable, Table.Name,
        Table.RefefenceFields[i].LeftTablesField,
        Table.RefefenceFields[i].FromTable,
        Table.RefefenceFields[i].RightTablesField]);
end;

function SortSQLQueryCreate(SortArray: array of SortField): string;
var
  i: integer;
begin
  if Length(SortArray) > 0 then
  begin
    Result := Format(' Order by %s ', [SortArray[0].Name]);
    if SortArray[0].State = 1 then
      Result += ' desc';
    for i := 1 to high(SortArray) do
    begin
      Result += Format(', %s', [SortArray[i].Name]);
      if SortArray[i].State = 1 then
        Result += ' desc';
    end;
  end;
end;

function FilterSQLQueryCreate(FilterArray: array of TMyPanel;
  FieldsArray: array of string): string;
var
  i: integer;
begin
  if Length(FilterArray) > 0 then
  begin
    Result := Format('Where %s %s :param%d',
      [FieldsArray[FilterArray[0].FieldNamesBox.ItemIndex],
      Conditions[FilterArray[0].ConditionsBox.ItemIndex].Name, 0]);
    for i := 1 to High(FilterArray) do
    begin
      Result += Format(' %s %s %s :param%d ',
        [LogicOperatorsArrayName[FilterArray[i].AndOrBox.ItemIndex],
        FieldsArray[FilterArray[i].FieldNamesBox.ItemIndex],
        Conditions[FilterArray[i].ConditionsBox.ItemIndex].Name, i]);
    end;
  end;
end;

initialization
  AddCondition('Равно', '=');
  AddCondition('Больше', '>');
  AddCondition('Меньше', '<');
  AddCondition('Содержит', 'CONTAINING');
  AddCondition('Не содержит', 'NOT CONTAINING');
  AddCondition('Начинается с', 'STARTING WITH');
end.
