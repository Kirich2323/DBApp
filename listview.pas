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
  public
    TableTag: integer;
  end;

var
  ListForm: TListForm;

implementation

{$R *.lfm}

{ TListForm }
procedure TListForm.FormShow(Sender: TObject);
var
  i: integer;
begin
  SQLQuery.SQL.Add(TableArray[TableTag].Name);
  SQLQuery.Open;
  for i := 0 to (DBGrid.Columns.Count - 1) do
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

