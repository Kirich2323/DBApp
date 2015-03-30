unit ListView;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, Forms, Controls, Graphics, Dialogs, DBGrids,
  DBCtrls, StdCtrls, Menus, sqldb, DB, MetaData;

type

  { TListForm }

  TListForm = class(TForm)
    DataSource: TDataSource;
    DBGrid: TDBGrid;
    DBNavigator1: TDBNavigator;
    SQLQuery: TSQLQuery;
    procedure FormClose(Sender: TObject; var CloseAction: TCloseAction);
    procedure FormShow(Sender: TObject);
    procedure CreateSql();
  public
    TableTag: integer;
  end;

var
  ListForm: TListForm;

implementation

procedure TListForm.CreateSql;
var
  i: integer;
  s: string;
begin
  SQLQuery.SQL.Text := 'SELECT ';
  for i := 0 to High(TableArray[TableTag].Fields) do
  begin
    if i <= (High(TableArray[TableTag].Fields) - 1) then
      SQLQuery.SQL.Add(TableArray[TableTag].Fields[i].Table + '.' +
        TableArray[TableTag].Fields[i].Name + ', ')
    else
      SQLQuery.SQL.Add(TableArray[TableTag].Fields[i].Table +
        '.' + TableArray[TableTag].Fields[i].Name);
  end;
  SQLQuery.SQl.Add('From ');
  SQLQuery.SQL.Add(TableArray[TableTag].Name);
  if Length(TableArray[TableTag].RefefenceFields) > 0 then
    for i := 0 to high(TableArray[TableTag].RefefenceFields) do
      SQLQuery.SQL.AddText(('inner join ' +
        TableArray[TableTag].RefefenceFields[i].FromTable + ' on ' +
        TableArray[TableTag].Name + '.' +
        TableArray[TableTag].RefefenceFields[i].LeftTablesField +
        ' = ' + TableArray[TableTag].RefefenceFields[i].FromTable +
        '.' + TableArray[TableTag].RefefenceFields[i].RightTablesField));
end;

{$R *.lfm}

{ TListForm }
procedure TListForm.FormShow(Sender: TObject);
var
  i: integer;
begin
  CreateSql;
  SQLQuery.Open;
  for i := 0 to high(TableArray[TableTag].Fields) do
  begin
    DBGrid.Columns.Items[i].FieldName := TableArray[TableTag].Fields[i].Name;
    DBGrid.Columns.Items[i].Title.Caption := TableArray[TableTag].Fields[i].Caption;
    DBGrid.Columns.Items[i].Width := TableArray[TableTag].Fields[i].Width;
    DBGrid.Columns.Items[i].Visible := TableArray[TableTag].Fields[i].Visible;
  end;
end;

procedure TListForm.FormClose(Sender: TObject; var CloseAction: TCloseAction);
begin
  isFormCreated[TableTag] := False;
  CloseAction := caFree;
end;

initialization

end.
