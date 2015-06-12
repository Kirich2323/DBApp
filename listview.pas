unit ListView;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, Forms, Controls, Graphics, Dialogs, DBGrids,
  DBCtrls, StdCtrls, Menus, ExtCtrls, PairSplitter, Buttons, sqldb, DB,
  MetaData, SQLQueryCreation, UMyPanel, UEditCard, DataUnit, Windows;

type

  { TListForm }

  TListForm = class(TForm)
    DeleteField_btn: TButton;
    CreateNew_btn: TButton;
    DataSource: TDataSource;
    DBGrid: TDBGrid;
    DBNavigator1: TDBNavigator;
    ImageList: TImageList;
    PairSplitter: TPairSplitter;
    PairSplitterTop: TPairSplitterSide;
    PairSplitterBottom: TPairSplitterSide;
    ScrollBox: TScrollBox;
    SQLQuery: TSQLQuery;
    procedure CreateNew_btnClick(Sender: TObject);
    procedure DBGridDblClick(Sender: TObject);
    procedure DeleteField_btnClick(Sender: TObject);
    procedure DBGridTitleClick(Column: TColumn);
    procedure FormShow(Sender: TObject);
    procedure CustomizeColumns();
    procedure AcceptFilters();
  private
    BaseSQLText: string;
    FilteredSQLText: string;
    FilterArray: array of TMyPanel;
    SortArray: array of SortField;
    VisibleFields: array of TField;
    id: integer;
    procedure MakeParametrs();
  public
    Filter: TMainFilter;
    TableTag: integer;
    Table: TTable;
  end;

  TEvent = procedure of object;

var
  ListForm: TListForm;

implementation

{$R *.lfm}

procedure TListForm.CustomizeColumns();
var
  i: integer;
begin
  SQLQuery.Open;
  SQLQuery.Locate(Table.Fields[0].Name, id, []);
  for i := 0 to High(Table.Fields) do
  begin
    with DBGrid.Columns.Items[i] do
    begin
      FieldName := Table.Fields[i].Name;
      Title.Caption := Table.Fields[i].Caption;
      Width := Table.Fields[i].Width;
      Visible := Table.Fields[i].Visible;
    end;
  end;
end;

procedure TListForm.MakeParametrs();
var
  i: integer;
  Params: array of string;
begin
  SetLength(Params, Length(Filter.Filters));
  for i := 0 to High(Params) do
  begin
    Params[i] := Format('Param%d', [i]);
    SQLQuery.Params.CreateParam(
      VisibleFields[Filter.Filters[i].FieldNamesBox.ItemIndex].TypeOfData,
      Params[i], ptInput);
    case VisibleFields[Filter.Filters[i].FieldNamesBox.ItemIndex].TypeOfData of
      ftstring:
        SQLQuery.ParamByName(Params[i]).AsString := Filter.Filters[i].Edit.Text;
      ftinteger:
        SQLQuery.ParamByName(Params[i]).AsInteger :=
          StrToInt(Filter.Filters[i].Edit.Text);
    end;
  end;
end;

procedure TListForm.FormShow(Sender: TObject);
var
  i: integer;
begin
  BaseSQLText := MainSQLQueryCreate(Table);
  SQLQuery.SQL.Text := BaseSQLText;
  FilteredSQLText := BaseSQLText;
  CustomizeColumns();
  Filter := TMainFilter.Create(ScrollBox, Table.Fields, @AcceptFilters);
  for i := 0 to High(Table.Fields) do
    if Table.Fields[i].Visible then
    begin
      SetLength(VisibleFields, Length(VisibleFields) + 1);
      VisibleFields[High(VisibleFields)] := Table.Fields[i];
    end;
end;

procedure TListForm.AcceptFilters();
begin
  SQLQuery.SQL.Text := BaseSQLText;
  SQLQuery.SQL.Add(FilterSQLQueryCreate(Filter.Filters, Filter.VisibleFields));
  FilteredSQLText := SQLQuery.SQL.Text;
  MakeParametrs();
  SQLQuery.Close;
  CustomizeColumns();
  SetLength(SortArray, 0);
end;

procedure TListForm.DBGridTitleClick(Column: TColumn);
var
  i, j: integer;
  IsFieldFiltered: boolean;
begin
  IsFieldFiltered := False;
  for i := 0 to High(SortArray) do
    if SortArray[i].Index = Column.Index then
    begin
      SortArray[i].State += 1;
      if SortArray[i].State = 2 then
      begin
        for j := i to High(SortArray) - 1 do
          SortArray[j] := SortArray[j + 1];
        SetLength(SortArray, Length(SortArray) - 1);
      end;
      IsFieldFiltered := True;
      break;
    end;
  if not (IsFieldFiltered) then
  begin
    SetLength(SortArray, Length(SortArray) + 1);
    with SortArray[High(SortArray)] do
    begin
      State := 0;
      Index := Column.Index;
      Name := Table.Fields[Index].Name;
    end;
  end;

  SQLQuery.SQL.Text := FilteredSQLText + SortSQLQueryCreate(SortArray);
  SQLQuery.Close;
  CustomizeColumns();

  with DBGrid.Columns do
  begin
    for j := 0 to High(SortArray) do
    begin
      case SortArray[j].State of
        0: Items[SortArray[j].Index].Title.ImageIndex := 1;
        1: Items[SortArray[j].Index].Title.ImageIndex := 0;
        2: Items[SortArray[j].Index].Title.ImageIndex := -1;
      end;
    end;
  end;
end;

procedure TListForm.CreateNew_btnClick(Sender: TObject);
begin
  Application.CreateForm(TEditCard, EditCard);
  with EditCard do
  begin
    id := -1;
    CurrentTable := Table;
    Show;
  end;
end;

procedure TListForm.DBGridDblClick(Sender: TObject);
var
  i: integer;
  CurrId: integer;
  IsExist: boolean;
begin
  IsExist := False;
  CurrId := SQLQuery.Fields[0].Value;
  with Screen do
  begin
    for i := 0 to FormCount - 1 do
      if Forms[i] is TEditCard then
        if (TEditCard(Forms[i]).Id > 0) and (TEditCard(Forms[i]).ID = CurrId) and
          (TEditCard(Forms[i]).CurrentTable = Table) then
        begin
          Forms[i].ShowOnTop;
          IsExist := True;
          break;
        end;
  end;

  if not IsExist then
  begin
    Application.CreateForm(TEditCard, EditCard);
    with EditCard do
    begin
      Id := CurrId;
      CurrentTable := Table;
      Show;
    end;
    id := CurrID;
  end;
end;

procedure TListForm.DeleteField_btnClick(Sender: TObject);
var
  TempSQL: string;
begin
  if MessageBox(Handle, PChar(
    Utf8ToAnsi('Вы действительно хотите удалить этот элемент?')),
    '', MB_YESNO) = mrYes then
  begin
    TempSQL := SQLQuery.SQL.Text;
    SQLQuery.SQL.Text := CreateDeleteSQL(Table.Name,
      DBGrid.Columns.Items[0].FieldName, DataSource.DataSet.Fields[0].Value);
    SQLQuery.Close;
    try
      begin
        SQLQuery.ExecSQL;
        DataUnit.DataBaseConnectionUnit.SQLTransaction.Commit;
        SQLQuery.SQL.Text := TempSQL;
        RefreshForms();
      end;
    except
      begin
        SQLQuery.SQL.Text := TempSQL;
        RefreshForms();
        MessageBox(Handle, PChar(
          Utf8ToAnsi('Удаление невозможно, так как на данный элемент ссылается другой элемент')),
          '', MB_OK);
      end;
    end;
  end;
end;

end.
