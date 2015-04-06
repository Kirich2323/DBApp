unit ListView;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, Forms, Controls, Graphics, Dialogs, DBGrids,
  DBCtrls, StdCtrls, Menus, ExtCtrls, sqldb, DB, MetaData, SQLQueryCreation;

type

  { TListForm }

  TListForm = class(TForm)
    AddFilter_btn: TButton;
    DataSource: TDataSource;
    DBGrid: TDBGrid;
    DBNavigator1: TDBNavigator;
    Filter_panel: TPanel;
    Panel: TPanel;
    SQLQuery: TSQLQuery;
    procedure DBGridTitleClick(Column: TColumn);
    procedure FormClose(Sender: TObject; var CloseAction: TCloseAction);
    procedure FormShow(Sender: TObject);
  public
    TableTag: integer;
    FilterArray: array of string;
    SortArray: array of SortField;
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
  SQLQuery.SQL.Text := SQLQueryCreate(TableTag, SortArray, FilterArray);
  SQLQuery.Open;
  for i := 0 to high(TableArray[TableTag].Fields) do
  begin
    DBGrid.Columns.Items[i].FieldName := TableArray[TableTag].Fields[i].Name;
    DBGrid.Columns.Items[i].Title.Caption := TableArray[TableTag].Fields[i].Caption;
    //вынести
    DBGrid.Columns.Items[i].Width := TableArray[TableTag].Fields[i].Width;
    DBGrid.Columns.Items[i].Visible := TableArray[TableTag].Fields[i].Visible;
  end;
end;

procedure TListForm.FormClose(Sender: TObject; var CloseAction: TCloseAction);
begin
  isFormCreated[TableTag] := False;
  CloseAction := caFree;
end;

procedure TListForm.DBGridTitleClick(Column: TColumn);
var
  i: integer;
  IsFieldFiltered: boolean;
begin
  IsFieldFiltered := False;
  for i := 0 to high(SortArray) do
    if SortArray[i].Name = Column.FieldName then
    begin
      SortArray[i].IsDescend := not SortArray[i].IsDescend;
      IsFieldFiltered := True;
      break;
    end;
  if not (IsFieldFiltered) then
  begin
    SetLength(SortArray, Length(SortArray) + 1);
    SortArray[High(SortArray)].Name := Column.FieldName;
  end;
  SQLQuery.SQL.Text := SQLQueryCreate(TableTag, SortArray, FilterArray);
  SQLQuery.Close;                                                 //вынести
  SQLQuery.Open;
  for i := 0 to high(TableArray[TableTag].Fields) do
  begin
    DBGrid.Columns.Items[i].FieldName := TableArray[TableTag].Fields[i].Name;
    DBGrid.Columns.Items[i].Title.Caption := TableArray[TableTag].Fields[i].Caption;
    DBGrid.Columns.Items[i].Width := TableArray[TableTag].Fields[i].Width;
    DBGrid.Columns.Items[i].Visible := TableArray[TableTag].Fields[i].Visible;
  end;
end;

initialization

end.
