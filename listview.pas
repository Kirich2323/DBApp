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
    PairSplitter: TPairSplitter;
    PairSplitterSide1: TPairSplitterSide;
    PairSplitterSide2: TPairSplitterSide;
    ScrollBox: TScrollBox;
    SQLQuery: TSQLQuery;
    procedure AcceptFilters_btnClick(Sender: TObject);
    procedure AddFilter_btnClick(Sender: TObject);
    procedure DBGridTitleClick(Column: TColumn);
    procedure DeleteAllFilters_btnClick(Sender: TObject);
    procedure FormClose(Sender: TObject; var CloseAction: TCloseAction);
    procedure FormShow(Sender: TObject);
    procedure RemovePanel(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: integer);
  private
    SQLText: string;
  public
    TableTag: integer;
    FilterArray: array of TMyParentPanel;
    FilterArray1: array of string;
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
  SQLText := SQLQueryCreate(TableTag, SortArray, FilterArray1);
  SQLQuery.SQL.Text := SQLText;
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
  SQLQuery.SQL.Text := SQLQueryCreate(TableTag, SortArray, FilterArray1);
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

procedure TListForm.DeleteAllFilters_btnClick(Sender: TObject);
begin

end;

procedure TListForm.AddFilter_btnClick(Sender: TObject);
var
  i: integer;
begin
  SetLength(FilterArray, Length(FilterArray) + 1);
  if Length(FilterArray) = 1 then
  begin
    FilterArray[High(FilterArray)] := TMyFirstPanel.Create(self);
  end
  else
  begin
    FilterArray[High(FilterArray)] := TMyOtherPanel.Create(self);
    FilterArray[High(FilterArray)].AndOrBox.Parent := FilterArray[High(FilterArray)];
  end;
  with FilterArray[High(FilterArray)] do
  begin
    Parent := ScrollBox;
    Top := 50 + 30 * High(FilterArray);
    Left := 10;
    with FieldNames do
    begin
      Parent := FilterArray[High(FilterArray)];
      for i := 0 to (DBGrid.Columns.Count - 1) do
        Items[i] := DBGrid.Columns.Items[i].Title.Caption;
    end;

    Conditions.Parent := FilterArray[High(FilterArray)];

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
begin

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
