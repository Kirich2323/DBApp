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

const
  ConditionsArray: array [0..4] of
    string = ('Равно', 'Больше', 'Меньше', 'Содержит', 'Начинается на');
  LogicOperatorsArray: array [0..1] of string = ('И', 'Или');
  ConditionsArrayName: array [0..4] of string = ('=', '>', '<', 'CONTAINING', 'STARTING WITH');
  LogicOperatorsArrayName: array [0..1] of string = ('And', 'Or');

function MainSQLQueryCreate(TableTag: integer): string;
function SortSQLQueryCreate(SortArray: array of SortField): string;
function FilterSQLQueryCreate(FilterArray: array of TMyParentPanel;
  FieldsArray: array of string): string;

implementation

function MainSqlQueryCreate(TableTag: integer): string;
var
  i: integer;
  s, Query: string;
begin
  Query := 'Select ';
  for i := 0 to High(TableArray[TableTag].Fields) do
  begin
    Query += (TableArray[TableTag].Fields[i].Table + '.' +
      TableArray[TableTag].Fields[i].Name + ', ');
    if not (i <= (High(TableArray[TableTag].Fields) - 1)) then
    begin
      s := Query;
      Delete(s, Length(s) - 1, 1);
      Query := s;
    end;
  end;

  Query += ('From ');
  Query += (TableArray[TableTag].Name);
  if Length(TableArray[TableTag].RefefenceFields) > 0 then
    for i := 0 to high(TableArray[TableTag].RefefenceFields) do
      Query += ((' inner join ' +
        TableArray[TableTag].RefefenceFields[i].FromTable + ' on ' +
        TableArray[TableTag].Name + '.' +
        TableArray[TableTag].RefefenceFields[i].LeftTablesField +
        ' = ' + TableArray[TableTag].RefefenceFields[i].FromTable +
        '.' + TableArray[TableTag].RefefenceFields[i].RightTablesField));
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

function FilterSQLQueryCreate(FilterArray: array of TMyParentPanel;
  FieldsArray: array of string): string;
var
  i: integer;
  Query: string;
begin
  if Length(FilterArray) > 0 then
  begin
    Query := Format('Where %s %s :param%d',
      [FieldsArray[FilterArray[0].FieldNames.ItemIndex],
      ConditionsArrayName[FilterArray[0].Conditions.ItemIndex], 0]);

    for i := 1 to High(FilterArray) do
    begin
      Query += Format(' %s %s %s :param%d ',
        [LogicOperatorsArrayName[FilterArray[i].AndOrBox.ItemIndex],
        FieldsArray[FilterArray[i].FieldNames.ItemIndex],
        ConditionsArrayName[FilterArray[i].Conditions.ItemIndex], i]);
    end;
  end;
  Result := Query;
end;

end.
