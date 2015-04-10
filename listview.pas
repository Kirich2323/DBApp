unit ListView;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, Forms, Controls, Graphics, Dialogs, DBGrids,
  DBCtrls, StdCtrls, Menus, ExtCtrls, PairSplitter, sqldb, DB, MetaData,
  SQLQueryCreation, UMyPanel;

type

  { TListForm }

  TListForm = class(TForm)
    AddFilter_btn: TButton;
    AcceptFilters_btn: TButton;
    DeleteAllFilters_btn: TButton;
    DataSource: TDataSource;
    DBGrid: TDBGrid;
    DBNavigator1: TDBNavigator;
    ImageList: TImageList;
    PairSplitter: TPairSplitter;
    PairSplitterTop: TPairSplitterSide;
    PairSplitterBottom: TPairSplitterSide;
    ScrollBox: TScrollBox;
    SQLQuery: TSQLQuery;
    procedure AcceptFilters_btnClick(Sender: TObject);
    procedure AddFilter_btnClick(Sender: TObject);
    procedure DBGridTitleClick(Column: TColumn);
    procedure DeleteAllFilters_btnClick(Sender: TObject);
    procedure FormClose(Sender: TObject; var CloseAction: TCloseAction);
    procedure FormResize(Sender: TObject);
    procedure FormShow(Sender: TObject);
    procedure RemovePanel(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: integer);
  private
    BaseSQLText: string;
    FilteredSQLText: string;
  public
    TableTag: integer;
    FilterArray: array of TMyParentPanel;
    FilterArray1: array of string;
    SortArray: array of SortField;
    FieldsArray: array of string;
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
  BaseSQLText := MainSQLQueryCreate(TableTag);
  SQLQuery.SQL.Text := BaseSQLText;
  FilteredSQLText := BaseSQLText;
  SQLQuery.Open;
  for i := 0 to high(TableArray[TableTag].Fields) do
    with DBGrid.Columns.Items[i] do
    begin
      FieldName := TableArray[TableTag].Fields[i].Name;
      Title.Caption := TableArray[TableTag].Fields[i].Caption;
      Width := TableArray[TableTag].Fields[i].Width;
      Visible := TableArray[TableTag].Fields[i].Visible;
      SetLength(FieldsArray, Length(FieldsArray) + 1);
      FieldsArray[High(FieldsArray)] := DBGrid.Columns.Items[i].FieldName;
    end;
end;

procedure TListForm.FormClose(Sender: TObject; var CloseAction: TCloseAction);
begin
  isFormCreated[TableTag] := False;
  CloseAction := caFree;
end;

procedure TListForm.FormResize(Sender: TObject);
begin

end;

procedure TListForm.DBGridTitleClick(Column: TColumn);
var
  i, j, Index: integer;
  IsFieldFiltered: boolean;
begin
  IsFieldFiltered := False;
  for i := 0 to High(SortArray) do
    if SortArray[i].Name = Column.FieldName then
    begin
      SortArray[i].State := (SortArray[i].State + 1);
      if SortArray[i].State = 2 then
      begin
        for j := i to (High(SortArray) - 1) do
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
      Name := Column.FieldName;
      State := 0;
      Index := Column.Index;
    end;
  end;
  SQLQuery.SQL.Text := FilteredSQLText + SortSQLQueryCreate(SortArray);
  SQLQuery.Close;                                                 //вынести
  SQLQuery.Open;
  with DBGrid.Columns do
  begin
    for i := 0 to High(TableArray[TableTag].Fields) do
    begin
      with Items[i] do
      begin
        FieldName := TableArray[TableTag].Fields[i].Name;
        Title.Caption := TableArray[TableTag].Fields[i].Caption;
        Width := TableArray[TableTag].Fields[i].Width;
        Visible := TableArray[TableTag].Fields[i].Visible;
      end;
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
end;

procedure TListForm.DeleteAllFilters_btnClick(Sender: TObject);
var
  i: integer;
begin
  for i := 0 to High(FilterArray) do
    FilterArray[i].Free;
  SetLength(FilterArray, 0);
end;


procedure TListForm.AddFilter_btnClick(Sender: TObject);
var
  i: integer;
begin
  SetLength(FilterArray, Length(FilterArray) + 1);
  FilterArray[High(FilterArray)] := TMyParentPanel.Create(self);

  with FilterArray[High(FilterArray)] do
  begin
    Parent := ScrollBox;
    Top := 50 + 30 * High(FilterArray);
    Left := 10;

    with AndOrBox do
    begin
      Parent := FilterArray[High(FilterArray)];
      if Length(FilterArray) = 1 then
        Visible := False;
      for i := 0 to 1 do
        Items[i] := LogicOperatorsArray[i];
      ItemIndex := 0;
    end;

    with FieldNames do
    begin
      Parent := FilterArray[High(FilterArray)];
      for i := 0 to (DBGrid.Columns.Count - 1) do
        Items[i] := DBGrid.Columns.Items[i].Title.Caption;
      ItemIndex := 0;
    end;

    with Conditions do
    begin
      Conditions.Parent := FilterArray[High(FilterArray)];
      for i := 0 to 4 do
        Items[i] := ConditionsArray[i];
      ItemIndex := 0;
    end;

    with DeleteButton do
    begin
      Parent := FilterArray[High(FilterArray)];
      OnMouseDown := @RemovePanel;
      Tag := High(FilterArray);
    end;

    Edit.Parent := FilterArray[High(FilterArray)];
  end;
end;

procedure TListForm.AcceptFilters_btnClick(Sender: TObject);
var
  i: integer;
begin
  SQLQuery.SQL.Text := BaseSQLText;
  SQLQuery.SQL.Add(FilterSQLQueryCreate(FilterArray, FieldsArray));
  FilteredSQLText := SQLQuery.SQL.Text;
  ShowMessage(SQLQuery.SQL.Text);
  SQLQuery.Close;
  SQLQuery.Open;
  for i := 0 to high(TableArray[TableTag].Fields) do
  begin
    with DBGrid.Columns.Items[i] do
    begin
      FieldName := TableArray[TableTag].Fields[i].Name;
      Title.Caption := TableArray[TableTag].Fields[i].Caption;
      Width := TableArray[TableTag].Fields[i].Width;
      Visible := TableArray[TableTag].Fields[i].Visible;
    end;
  end;
  SetLength(SortArray, 0);
end;

procedure TListForm.RemovePanel(Sender: TObject; Button: TMouseButton;
  Shift: TShiftState; X, Y: integer);
var
  j: integer;
begin
  j := TButton(Sender).Tag;
  if j <> High(FilterArray) then
  begin
    FilterArray[j].Free;
    for j := j to (High(FilterArray) - 1) do
    begin
      FilterArray[j] := FilterArray[j + 1];
    end;
    SetLength(FilterArray, Length(FilterArray) - 1);
    for j := 0 to High(FilterArray) do
    begin
      if j = 0 then
        FilterArray[j].AndOrBox.Visible := False;
      FilterArray[j].DeleteButton.Tag := j;
      FilterArray[j].Top := 50 + 30 * j;
    end;
  end
  else
  begin
    FilterArray[j].Free;
    SetLength(FilterArray, Length(FilterArray) - 1);
  end;
end;


initialization

end.
