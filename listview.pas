unit ListView;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, Forms, Controls, Graphics, Dialogs, DBGrids,
  DBCtrls, StdCtrls, Menus, ExtCtrls, PairSplitter, Buttons, sqldb, DB,
  MetaData, SQLQueryCreation, UMyPanel;

type

  { TListForm }

  TListForm = class(TForm)
    AddFilter_btn: TButton;
    DeleteAllFilters_btn: TButton;
    DataSource: TDataSource;
    DBGrid: TDBGrid;
    DBNavigator1: TDBNavigator;
    ImageList: TImageList;
    PairSplitter: TPairSplitter;
    PairSplitterTop: TPairSplitterSide;
    PairSplitterBottom: TPairSplitterSide;
    ScrollBox: TScrollBox;
    AcceptFilters_spdbtn: TSpeedButton;
    SQLQuery: TSQLQuery;
    procedure AcceptFilters_btnClick(Sender: TObject);
    procedure AddFilter_btnClick(Sender: TObject);
    procedure PanelItemChange(Sender: TObject);
    procedure DBGridTitleClick(Column: TColumn);
    procedure DeleteAllFilters_btnClick(Sender: TObject);
    procedure FormClose(Sender: TObject; var CloseAction: TCloseAction);
    procedure FormShow(Sender: TObject);
    procedure RemovePanel(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: integer);
    procedure EditFields();
  private
    BaseSQLText: string;
    FilteredSQLText: string;
  public
    TableTag: integer;
    FilterArray: array of TMyPanel;
    SortArray: array of SortField;
    FieldsArray: array of string;
    DataTypeArray: array of TFieldType;
  end;

var
  ListForm: TListForm;

implementation

{$R *.lfm}
{ TListForm }

procedure TListForm.EditFields();
var
  i: integer;
begin
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
end;

procedure TListForm.FormShow(Sender: TObject);
var
  i: integer;
begin
  BaseSQLText := MainSQLQueryCreate(TableTag);
  SQLQuery.SQL.Text := BaseSQLText;
  FilteredSQLText := BaseSQLText;
  EditFields();
  SetLength(DataTypeArray, Length(TableArray[TableTag].Fields));
  for i := 0 to high(TableArray[TableTag].Fields) do
  begin
    SetLength(FieldsArray, Length(FieldsArray) + 1);
    FieldsArray[High(FieldsArray)] := DBGrid.Columns.Items[i].FieldName;
    DataTypeArray[i] := TableArray[TableTag].Fields[i].TypeOfData;
  end;
end;

procedure TListForm.FormClose(Sender: TObject; var CloseAction: TCloseAction);
begin
  isFormCreated[TableTag] := False;
  CloseAction := caFree;
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
  SQLQuery.Close;
  EditFields();
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

procedure TListForm.DeleteAllFilters_btnClick(Sender: TObject);
var
  i: integer;
begin
  AcceptFilters_spdbtn.Enabled := True;
  for i := 0 to High(FilterArray) do
    FilterArray[i].Free;
  SetLength(FilterArray, 0);
end;


procedure TListForm.AddFilter_btnClick(Sender: TObject);
var
  i: integer;
begin
  SetLength(FilterArray, Length(FilterArray) + 1);
  FilterArray[High(FilterArray)] := TMyPanel.Create(self);
  AcceptFilters_spdbtn.Enabled := True;

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
      OnChange := @PanelItemChange;
    end;

    with FieldNamesBox do
    begin
      Parent := FilterArray[High(FilterArray)];
      for i := 0 to (DBGrid.Columns.Count - 1) do
        Items[i] := DBGrid.Columns.Items[i].Title.Caption;
      ItemIndex := 0;
      OnChange := @PanelItemChange;
    end;

    with ConditionsBox do
    begin
      ConditionsBox.Parent := FilterArray[High(FilterArray)];
      for i := 0 to High(Conditions) do
        Items[i] := Conditions[i].Caption;
      ItemIndex := 0;
      OnChange := @PanelItemChange;
    end;

    with DeleteButton do
    begin
      Parent := FilterArray[High(FilterArray)];
      OnMouseDown := @RemovePanel;
      Tag := High(FilterArray);
    end;

    with Edit do
    begin
      Parent := FilterArray[High(FilterArray)];
      OnChange := @PanelItemChange;
    end;
  end;
end;

procedure TListForm.PanelItemChange(Sender: TObject);
begin
  AcceptFilters_spdbtn.Enabled := True;
end;

procedure TListForm.AcceptFilters_btnClick(Sender: TObject);
var
  i: integer;
  Params: array of string;
begin
  SQLQuery.SQL.Text := BaseSQLText;
  SQLQuery.SQL.Add(FilterSQLQueryCreate(FilterArray, FieldsArray));
  FilteredSQLText := SQLQuery.SQL.Text;
  SetLength(Params, Length(FilterArray));
  AcceptFilters_spdbtn.Enabled := False;

  for i := 0 to High(FilterArray) do
  begin
    Params[i] := Format('Param%d', [i]);
    SQLQuery.Params.CreateParam(DataTypeArray[FilterArray[i].FieldNamesBox.ItemIndex],
      Params[i], ptInput);
    case DataTypeArray[FilterArray[i].FieldNamesBox.ItemIndex] of
      ftstring:
        SQLQuery.ParamByName(Params[i]).AsString := FilterArray[i].Edit.Text;
      ftinteger:
        SQLQuery.ParamByName(Params[i]).AsInteger :=
          StrToInt(FilterArray[i].Edit.Text);
    end;
  end;
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
  AcceptFilters_spdbtn.Enabled := True;
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

end.
