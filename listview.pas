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
    AddFilter_btn: TButton;
    DeleteField_btn: TButton;
    CreateNew_btn: TButton;
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
    procedure CreateNew_btnClick(Sender: TObject);
    procedure DBGridDblClick(Sender: TObject);
    procedure DeleteField_btnClick(Sender: TObject);
    procedure PanelItemChange(Sender: TObject);
    procedure DBGridTitleClick(Column: TColumn);
    procedure DeleteAllFilters_btnClick(Sender: TObject);
    procedure FormShow(Sender: TObject);
    procedure RemovePanel(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: integer);
    procedure CustomizeColumns();
  private
    BaseSQLText: string;
    FilteredSQLText: string;
    FilterArray: array of TMyPanel;
    SortArray: array of SortField;
    VisibleFields: array of TField;
    FormIndex: integer;
    procedure MakeParametrs();
  public
    TableTag: integer;
    Table: TTable;
  end;

var
  ListForm: TListForm;

implementation

{$R *.lfm}

procedure TListForm.CustomizeColumns();
var
  i: integer;
begin
  SQLQuery.Open;
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

procedure RefreshListForms();
var
  i: integer;
begin
  with Screen do
    for i := 0 to (FormCount - 2) do
    begin
      if Forms[i].ClassName = 'TListForm' then
        TListForm(Forms[i]).CustomizeColumns();
    end;
end;

procedure TListForm.MakeParametrs();
var
  i: integer;
  Params: array of string;
begin
  SetLength(Params, Length(FilterArray));
  for i := 0 to High(Params) do
  begin
    Params[i] := Format('Param%d', [i]);
    SQLQuery.Params.CreateParam(
      VisibleFields[FilterArray[i].FieldNamesBox.ItemIndex].TypeOfData,
      Params[i], ptInput);
    case VisibleFields[FilterArray[i].FieldNamesBox.ItemIndex].TypeOfData of
      ftstring:
        SQLQuery.ParamByName(Params[i]).AsString := FilterArray[i].Edit.Text;
      ftinteger:
        SQLQuery.ParamByName(Params[i]).AsInteger :=
          StrToInt(FilterArray[i].Edit.Text);
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
  for i := 0 to High(Table.Fields) do
    if Table.Fields[i].Visible then
    begin
      SetLength(VisibleFields, Length(VisibleFields) + 1);
      VisibleFields[High(VisibleFields)] := Table.Fields[i];
    end;
end;

procedure TListForm.DBGridTitleClick(Column: TColumn);
var
  i, j, Index: integer;
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
  FilterArray[High(FilterArray)] := TMyPanel.Create(Self);
  AcceptFilters_spdbtn.Enabled := True;

  with FilterArray[High(FilterArray)] do
  begin
    Parent := ScrollBox;
    Top := 50 + 30 * High(FilterArray);
    Left := 10;
    with AndOrBox do
    begin
      if Length(FilterArray) = 1 then
        Visible := False;
      for i := 0 to 1 do
        Items[i] := LogicOperatorsArray[i];
      ItemIndex := 0;
      OnChange := @PanelItemChange;
    end;

    with FieldNamesBox do
    begin
      for i := 0 to High(VisibleFields) do
        Items[i] := VisibleFields[i].Caption;
      ItemIndex := 0;
      OnChange := @PanelItemChange;
    end;

    with ConditionsBox do
    begin
      for i := 0 to High(Conditions) do
        Items[i] := Conditions[i].Caption;
      ItemIndex := 0;
      OnChange := @PanelItemChange;
    end;

    with DeleteButton do
    begin
      OnMouseDown := @RemovePanel;
      Tag := High(FilterArray);
    end;

    with Edit do
      OnChange := @PanelItemChange;
  end;
end;

procedure TListForm.CreateNew_btnClick(Sender: TObject);
var
  i: integer;
begin
  Application.CreateForm(TEditCard, EditCard);
  with EditCard do
  begin
    Fields := VisibleFields;
    IsCreateNew := True;
    CurrentTable := Table;
    ListDataSource := DataSource;
    ShowModal;
    RefreshListForms();
  end;
end;

procedure TListForm.DBGridDblClick(Sender: TObject);
var
  i: integer;
begin
  Application.CreateForm(TEditCard, EditCard);
  with EditCard do
  begin
    Fields := VisibleFields;
    IsCreateNew := False;
    CurrentTable := Table;
    ListDataSource := DataSource;
    ShowModal;
    RefreshListForms();
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
    SQLQuery.SQL.Text := CreateDeleteSQL(Table.Name, DBGrid.Columns.Items[0].FieldName,
      DataSource.DataSet.Fields[0].Value);
    SQLQuery.Close;
    SQLQuery.ExecSQL;
    DataUnit.DataBaseConnectionUnit.SQLTransaction.Commit;
    SQLQuery.SQL.Text := TempSQL;
    RefreshListForms();
  end;
end;


procedure TListForm.PanelItemChange(Sender: TObject);
begin
  AcceptFilters_spdbtn.Enabled := True;
end;

procedure TListForm.AcceptFilters_btnClick(Sender: TObject);
begin
  SQLQuery.SQL.Text := BaseSQLText;
  SQLQuery.SQL.Add(FilterSQLQueryCreate(FilterArray, VisibleFields));
  FilteredSQLText := SQLQuery.SQL.Text;
  AcceptFilters_spdbtn.Enabled := False;
  MakeParametrs();
  SQLQuery.Close;
  CustomizeColumns();
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
      FilterArray[j] := FilterArray[j + 1];
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
