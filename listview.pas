unit ListView;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, Forms, Controls, Graphics, Dialogs, DBGrids,
  DBCtrls, StdCtrls, Menus, ExtCtrls, PairSplitter, Buttons, sqldb, DB,
  MetaData, SQLQueryCreation, UMyPanel, UEditCard;

type

  { TListForm }

  TListForm = class(TForm)
    AddFilter_btn: TButton;
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
    procedure AcceptEditCardButtonClick(Sender: TObject);
    procedure CreateNew_btnClick(Sender: TObject);
    procedure DBGridDblClick(Sender: TObject);
    procedure EditCardFormClose(Sender: TObject; var CloseAction: TCloseAction);
    procedure PanelItemChange(Sender: TObject);
    procedure DBGridTitleClick(Column: TColumn);
    procedure DeleteAllFilters_btnClick(Sender: TObject);
    procedure FormShow(Sender: TObject);
    procedure RemovePanel(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: integer);
    procedure EditFields();
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
{ TListForm }

procedure TListForm.EditFields();
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
  //ShowMessage(SQLQuery.SQL.Text);
  EditFields();
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

procedure TListForm.AcceptEditCardButtonClick(Sender: TObject);
begin
  //пофиксить
end;

procedure TListForm.CreateNew_btnClick(Sender: TObject);
var
  i: integer;
  PreviousControl: TControl;
begin
  Application.CreateForm(TEditCard, EditCard);
  EditCard.Fields := VisibleFields;
  with EditCard do
  begin
    IsCreateNew := True;
    CurrentTable := Table;
    TestDataSource := DataSource;
    //  SetLength(FieldsCaptions, Length(VisibleFields));      //Оптимизировать это!!!
    //  SetLength(EditControls, Length(VisibleFields));
    //  FieldsCaptions[0] := TLabel.Create(Self);
    //  if VisibleFields[0].InnerJoin then
    //  begin
    //    TempSQLQuery.SQL.Text :=
    //      Format('Select %s from %s', [VisibleFields[0].Name, VisibleFields[0].Table]);
    //    TempSQLQuery.Open;
    //    EditControls[0] := TDBLookupComboBox.Create(Self);
    //    TDBLookUpComboBox(EditControls[0]).ListSource := TempDataSource;
    //    TDBLookUpComboBox(EditControls[0]).KeyField := VisibleFields[i].Name;
    //    TempSQLQuery.Close;
    //    TDBLookUpComboBox(EditControls[0]).ListSource := nil;
    //  end
    //  else
    //  begin
    //    EditControls[0] := TDBEdit.Create(Self);
    //    TDBEdit(EditControls[0]).DataSource := DataSource;
    //    TDBEdit(EditControls[0]).DataField := VisibleFields[0].Name;
    //    EditControls[0].AutoSize := True;
    //    //TDBEdit(EditControls[0]).DataSource := nil;
    //    //TDBEdit(EditControls[0]).Text := '';
    //  end;
    //  with FieldsCaptions[0] do
    //  begin
    //    Parent := EditCard;
    //    Caption := VisibleFields[0].Caption;
    //    Top := 30;
    //    Left := 15;
    //    AutoSize := True;
    //  end;
    //  with EditControls[0] do
    //  begin
    //    Parent := EditCard;
    //    Top := 30;
    //    Left := 200;
    //    Width := 180;
    //    //AutoSize := True;
    //  end;
    //  //--
    //  PreviousControl := EditControls[0];
    //  //CreateOtherControls
    //  for i := 1 to High(VisibleFields) do
    //  begin
    //    FieldsCaptions[i] := TLabel.Create(Self);
    //    if VisibleFields[i].InnerJoin then
    //    begin
    //      EditControls[i] := TDBLookupComboBox.Create(Self);
    //      //TDBLookUpComboBox(EditControls[i]).DataSource := DataSource;       //пофиксить
    //      //TDBLookUpComboBox(EditControls[i]).ListSource := DataSource;
    //      TempSQLQuery.SQL.Text :=
    //        Format('Select %s from %s', [VisibleFields[i].Name,
    //        VisibleFields[i].Table]);
    //      TempSQLQuery.Open;
    //      ShowMessage(TempSQLQuery.SQL.Text); //showmessage
    //      TDBLookUpComboBox(EditControls[i]).ListSource := TempDataSource;
    //      //TDBLookUpComboBox(EditControls[i]).DataSource := EditCard.TempDataSource;
    //      //TDBLookUpComboBox(EditControls[i]).DataField := VisibleFields[i].Name;
    //      TDBLookUpComboBox(EditControls[i]).KeyField := VisibleFields[i].Name;
    //      TDBLookUpComboBox(EditControls[i]).ListSource := nil;
    //      TempSQLQuery.Close;
    //    end
    //    else
    //    begin
    //      EditControls[i] := TDBEdit.Create(Self);
    //      TDBEdit(EditControls[i]).DataSource := DataSource;
    //      TDBEdit(EditControls[i]).DataField := VisibleFields[i].Name;
    //      EditControls[i].AutoSize := True;
    //      //TDBEdit(EditControls[i]).DataSource := nil;
    //      //TDBEdit(EditControls[i]).Text := '';
    //    end;
    //    with FieldsCaptions[i] do
    //    begin
    //      Parent := EditCard;
    //      Visible := True;
    //      Caption := VisibleFields[i].Caption;
    //      AnchorToNeighbour(akTop, 10, PreviousControl);
    //      Left := 15;
    //      AutoSize := True;
    //    end;
    //    with EditControls[i] do
    //    begin
    //      //AutoSize := True;
    //      AnchorSide[akTop].Side := asrTop;
    //      Parent := EditCard;
    //      AnchorToNeighbour(akTop, 10, PreviousControl);
    //      Visible := True;
    //      Top := 30 + 50 * i;
    //      Left := 200;
    //      Width := 180;
    //    end;
    //    PreviousControl := EditControls[i];
    //    //Accept_btn.OnClick := @AcceptEditCardButtonClick;
    //  end;
    //-----
    OnClose := @EditCardFormClose;
    ShowModal;
  end;
end;

procedure TListForm.DBGridDblClick(Sender: TObject);
var
  i: integer;
  PreviousControl: TControl;
begin
  Application.CreateForm(TEditCard, EditCard);
  EditCard.Fields := VisibleFields;
  with EditCard do
  begin
    IsCreateNew := False;
    CurrentTable := Table;
    TestDataSource := DataSource;
    //  SetLength(FieldsCaptions, Length(VisibleFields)); //Оптимизировать это!!!
    //  SetLength(EditControls, Length(VisibleFields));
    //  //CreateFirstControl();
    //  FieldsCaptions[0] := TLabel.Create(Self);
    //  if VisibleFields[0].InnerJoin then
    //  begin
    //    EditCard.TempSQLQuery.SQL.Text :=
    //      Format('Select %s from %s', [VisibleFields[0].Name, VisibleFields[0].Table]);
    //    EditCard.TempSQLQuery.Open;
    //    EditControls[0] := TDBLookupComboBox.Create(Self);
    //    TDBLookUpComboBox(EditControls[0]).ListSource := EditCard.TempDataSource;
    //    TDBLookUpComboBox(EditControls[0]).KeyField := VisibleFields[i].Name;
    //    EditCard.TempSQLQuery.Close;
    //    TDBLookUpComboBox(EditControls[0]).ListSource := nil;
    //  end
    //  else
    //  begin
    //    EditControls[0] := TDBEdit.Create(Self);
    //    TDBEdit(EditControls[0]).DataSource := DataSource;
    //    TDBEdit(EditControls[0]).DataField := VisibleFields[0].Name;
    //    EditControls[0].AutoSize := True;
    //  end;
    //  with FieldsCaptions[0] do
    //  begin
    //    Parent := EditCard;
    //    Caption := VisibleFields[0].Caption;
    //    Top := 30;
    //    Left := 15;
    //    AutoSize := True;
    //  end;
    //  with EditControls[0] do
    //  begin
    //    Parent := EditCard;
    //    Top := 30;
    //    Left := 200;
    //    Width := 180;
    //    //AutoSize := True;
    //  end;
    //  //--
    //  PreviousControl := EditControls[0];
    //  //CreateOtherControls
    //  for i := 1 to High(VisibleFields) do
    //  begin
    //    FieldsCaptions[i] := TLabel.Create(Self);
    //    if VisibleFields[i].InnerJoin then
    //    begin
    //      EditControls[i] := TDBLookupComboBox.Create(Self);
    //      //TDBLookUpComboBox(EditControls[i]).DataSource := DataSource;       //пофиксить
    //      //TDBLookUpComboBox(EditControls[i]).ListSource := DataSource;
    //      EditCard.TempSQLQuery.SQL.Text :=
    //        Format('Select %s from %s', [VisibleFields[i].Name,
    //        VisibleFields[i].Table]);
    //      EditCard.TempSQLQuery.Open;
    //      ShowMessage(EditCard.TempSQLQuery.SQL.Text);
    //      TDBLookUpComboBox(EditControls[i]).ListSource := EditCard.TempDataSource;
    //      //TDBLookUpComboBox(EditControls[i]).DataSource := EditCard.TempDataSource;
    //      //TDBLookUpComboBox(EditControls[i]).DataField := VisibleFields[i].Name;
    //      TDBLookUpComboBox(EditControls[i]).KeyField := VisibleFields[i].Name;
    //      TDBLookUpComboBox(EditControls[i]).ListSource := nil;
    //      EditCard.TempSQLQuery.Close;
    //      //TDBLookUpComboBox(EditControls[i]).DataSource := nil;
    //      //EditControls[i] := TDBComboBox.Create(Self);
    //      //TDBComboBox(EditControls[i]).DataSource := EditCard.TempDataSource;
    //      //EditCard.TempSQLQuery.SQL.Text :=
    //      //  Format('Select %s from %s', [VisibleFields[i].Name,
    //      //  VisibleFields[i].Table]);
    //      //EditCard.TempSQLQuery.Open;
    //      //TDBComboBox(EditControls[i]).DataField := VisibleFields[i].Name;
    //      //EditCard.TempSQLQuery.Close;
    //      //TDBComboBox(EditControls[i]).DataSource := nil;
    //    end
    //    else
    //    begin
    //      EditControls[i] := TDBEdit.Create(Self);
    //      TDBEdit(EditControls[i]).DataSource := DataSource;
    //      TDBEdit(EditControls[i]).DataField := VisibleFields[i].Name;
    //      EditControls[i].AutoSize := True;
    //    end;
    //    with FieldsCaptions[i] do
    //    begin
    //      Parent := EditCard;
    //      Visible := True;
    //      Caption := VisibleFields[i].Caption;
    //      AnchorToNeighbour(akTop, 10, PreviousControl);
    //      Left := 15;
    //      AutoSize := True;
    //    end;
    //    with EditControls[i] do
    //    begin
    //      //AutoSize := True;
    //      AnchorSide[akTop].Side := asrTop;
    //      Parent := EditCard;
    //      AnchorToNeighbour(akTop, 10, PreviousControl);
    //      Visible := True;
    //      Top := 30 + 50 * i;
    //      Left := 200;
    //      Width := 180;
    //    end;
    //    PreviousControl := EditControls[i];
    //    //Accept_btn.OnClick := @AcceptEditCardButtonClick;
    //  end;
    //  //DBLookupComboBox1.DataField := VisibleFields[1].Name;
    //  //DBLookupComboBox1.KeyField := VisibleFields[1].Name;
    //  //-----
    OnClose := @EditCardFormClose;
    ShowModal;
  end;
end;

procedure TListForm.EditCardFormClose(Sender: TObject; var CloseAction: TCloseAction);
begin

  CloseAction := caFree;
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
  EditFields();
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
