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

function MainSQLQueryCreate(TableTag: integer): string;
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

function MainSqlQueryCreate(TableTag: integer): string;
var
  i: integer;
  s, Query: string;
  CurTable: TTable;
begin
  CurTable := TableArray[TableTag];
  Query := 'Select ';
  for i := 0 to High(CurTable.Fields) do
  begin
    Query += Format('%s.%s, ', [CurTable.Fields[i].Table, CurTable.Fields[i].Name]);
    if not (i <= (High(CurTable.Fields) - 1)) then
    begin
      s := Query;
      Delete(s, Length(s) - 1, 1);
      Query := s;
    end;
  end;
  Query += ('From ');
  Query += (CurTable.Name);
  if Length(CurTable.RefefenceFields) > 0 then
    for i := 0 to high(CurTable.RefefenceFields) do
      Query += Format(' inner join %s on %s.%s = %s.%s',
        [CurTable.RefefenceFields[i].FromTable, CurTable.Name,
        CurTable.RefefenceFields[i].LeftTablesField,
        CurTable.RefefenceFields[i].FromTable,
        CurTable.RefefenceFields[i].RightTablesField]);
  Result := Query;
end;

function SortSQLQueryCreate(SortArray: array of SortField): string;
var
  i: integer;
  s, Query: string;
begin
  if Length(SortArray) > 0 then
  begin
    Query := (' Order by ');
    for i := 0 to high(SortArray) do
    begin
      Query += (SortArray[i].Name);
      if SortArray[i].State = 1 then
        Query += (' desc');
      Query += (', ');
      if not (i <= (High(SortArray) - 1)) then
      begin
        s := Query;
        Delete(s, Length(s) - 1, 1);
        Query := s;
      end;
    end;
  end;
  Result := Query;
end;

function FilterSQLQueryCreate(FilterArray: array of TMyPanel;
  FieldsArray: array of string): string;
var
  i: integer;
  Query: string;
begin
  if Length(FilterArray) > 0 then
  begin
    Query := Format('Where %s %s :param%d',
      [FieldsArray[FilterArray[0].FieldNamesBox.ItemIndex],
      Conditions[FilterArray[0].ConditionsBox.ItemIndex].Name, 0]);
    for i := 1 to High(FilterArray) do
    begin
      Query += Format(' %s %s %s :param%d ',
        [LogicOperatorsArrayName[FilterArray[i].AndOrBox.ItemIndex],
        FieldsArray[FilterArray[i].FieldNamesBox.ItemIndex],
        Conditions[FilterArray[i].ConditionsBox.ItemIndex].Name, i]);
    end;
  end;
  Result := Query;
end;

initialization
  AddCondition('Равно', '=');
  AddCondition('Больше', '>');
  AddCondition('Меньше', '<');
  AddCondition('Содержит', 'CONTAINING');
  AddCondition('Не содержит', 'NOT CONTAINING');
  AddCondition('Начинается с', 'STARTING WITH');


end.
