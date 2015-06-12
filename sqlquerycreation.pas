unit SQLQueryCreation;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, MetaData, Dialogs, UMyPanel, DataUnit;

type
  SortField = record
    Name: string;
    State, Index: integer;
  end;

function MainSQLQueryCreate(Table: TTable): string;
function SortSQLQueryCreate(SortArray: array of SortField): string;
function FilterSQLQueryCreate(FilterArray: array of TMyPanel;
  FieldsArray: array of TField): string;
function CreateUpdateSQL(Table: TTable; id: integer): string;
function CreateInsertSQL(Table: TTable): string;
function CreateDeleteSQL(TableName, IdField, id: string): string;

implementation

function MainSqlQueryCreate(Table: TTable): string;
var
  i: integer;
  //CurTable: TTable;
begin
  Result := Format('Select %s.%s', [Table.Fields[0].Table, Table.Fields[0].Name]);
  for i := 1 to High(Table.Fields) do
    Result += Format(', %s.%s ', [Table.Fields[i].Table, Table.Fields[i].Name]);
  Result += Format(' From %s ', [Table.Name]);
  if Length(Table.RefefenceFields) > 0 then
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
    for i := 1 to High(SortArray) do
    begin
      Result += Format(', %s', [SortArray[i].Name]);
      if SortArray[i].State = 1 then
        Result += ' desc';
    end;
  end;
end;

function FilterSQLQueryCreate(FilterArray: array of TMyPanel;
  FieldsArray: array of TField): string;
var
  i: integer;
begin
  if Length(FilterArray) > 0 then
  begin
    Result := Format('Where %s %s :param%d',
      [FieldsArray[FilterArray[0].FieldNamesBox.ItemIndex].Name,
      Conditions[FilterArray[0].ConditionsBox.ItemIndex].Name, 0]);
    for i := 1 to High(FilterArray) do
      Result += Format(' %s %s %s :param%d ',
        [LogicOperatorsArrayName[FilterArray[i].AndOrBox.ItemIndex],
        FieldsArray[FilterArray[i].FieldNamesBox.ItemIndex].Name,
        Conditions[FilterArray[i].ConditionsBox.ItemIndex].Name, i]);
  end;
end;

function CreateUpdateSQL(Table: TTable; id: integer): string;
var
  i: integer;
  VisibleFields: array of TField;
begin
  for i := 0 to High(Table.Fields) do
    if Table.Fields[i].Visible then
    begin
      SetLength(VisibleFields, Length(VisibleFields) + 1);
      VisibleFields[High(VisibleFields)] := Table.Fields[i];
    end;
  Result := Format('Update %s Set %s = :Param0', [Table.Name, Table.Fields[0].Name]);
  for i := 0 to high(VisibleFields) do
    if VisibleFields[i].InnerJoin then
      Result += Format(', %s = :Param%d', [VisibleFields[i].NativeName, i + 1])
    else
      Result += Format(', %s = :Param%d', [VisibleFields[i].Name, i + 1]);
  Result += Format(' Where %s = :Param0', [Table.Fields[0].Name]);
end;

function CreateInsertSQL(Table: TTable): string;
var
  i, k: integer;
begin
  k := 1;
  Result := Format('Insert into %s VALUES(:Param0', [Table.Name]);
  for i := 1 to High(Table.Fields) do
  begin
    if Table.Fields[i].Visible then
    begin
      Result += Format(', :Param%d', [k]);
      Inc(k);
    end;
  end;
  Result += ')';
end;

function CreateDeleteSQL(TableName, IdField, id: string): string;
begin
  Result := Format('Delete from %s Where %s = %s', [TableName, IdField, id]);
end;

end.
