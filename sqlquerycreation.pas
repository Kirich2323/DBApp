unit SQLQueryCreation;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, MetaData, Dialogs;

type
  SortField = record
    Name: string;
    IsDescend: boolean;
  end;

function SQLQueryCreate(TableTag: integer; SortArray: array of SortField;
  FilterArray: array of string): string;

implementation

function SqlQueryCreate(TableTag: integer; SortArray: array of SortField;
  FilterArray: array of string): string;
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


  if Length(FilterArray) > 0 then
  begin
    //filtering incoming;
  end;

  if Length(SortArray) > 0 then
  begin
    Query += (' Order by ');
    for i := 0 to high(SortArray) do
    begin
      Query += (SortArray[i].Name);
      if SortArray[i].IsDescend then
        Query += (' desc');
      Query += (', ');
      if not (i <= (High(SortArray) - 1)) then
      begin
        s := Query;
        Delete(s, Length(s) - 1, 1);
        Query := s;
      end;
      //sortirovka incoming;
    end;
  end;
  Result := Query;

end;

end.
