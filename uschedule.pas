unit USchedule;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, Forms, Controls, Graphics, Dialogs, Grids,
  StdCtrls, PairSplitter, CheckLst, Menus, MetaData, sqldb, DB,
  SQLQueryCreation, UMyPanel, UEditCard, ListView, Windows, DataUnit,
  fpspreadsheet;

type

  { TSchedule }

  CellData = class
    Values: TStringList;
    Id: integer;
    constructor Create();
  end;

  MyCell = class
    Count: integer;
    IsFullSize: boolean;
    Data: array of CellData;
    constructor Create();
  end;

  TSchedule = class(TForm)
    Apply: TButton;
    CheckListBox: TCheckListBox;
    MainMenu: TMainMenu;
    Export: TMenuItem;
    HTML: TMenuItem;
    Ecxel: TMenuItem;
    ThisFile: TMenuItem;
    OrderBox: TComboBox;
    SortTxt: TLabel;
    SortBox: TComboBox;
    ImageList: TImageList;
    HorizontalTxt: TLabel;
    VerticalTxt: TLabel;
    PairSplitter: TPairSplitter;
    PairSplitterSide1: TPairSplitterSide;
    PairSplitterSide2: TPairSplitterSide;
    SchedulesDataSource: TDataSource;
    SchedulesSQLQuery: TSQLQuery;
    ScrollBox: TScrollBox;
    Vertical_cbox: TComboBox;
    Horizontal_cbox: TComboBox;
    DrawGrid: TDrawGrid;
    procedure ApplyClick(Sender: TObject);
    procedure CheckListBoxClick(Sender: TObject);
    procedure DrawGridDblClick(Sender: TObject);
    procedure DrawGridDrawCell(Sender: TObject; aCol, aRow: integer;
      aRect: TRect; aState: TGridDrawState);
    procedure DrawGridMouseDown(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: integer);
    procedure DrawGridMouseMove(Sender: TObject; Shift: TShiftState; X, Y: integer);
    procedure DrawGridMouseUp(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: integer);
    procedure EcxelClick(Sender: TObject);
    procedure FormClose(Sender: TObject; var CloseAction: TCloseAction);
    procedure FormCreate(Sender: TObject);
    procedure Edit(CurrId: integer);
    procedure Horizontal_cboxChange(Sender: TObject);
    procedure HTMLClick(Sender: TObject);
    procedure OrderBoxChange(Sender: TObject);
    procedure ShowListView(Arow, ACol: integer);
    procedure CreateNew(aRow, aCol: integer);
    procedure SetChekBoxes();
    procedure DeleteItem(CurrId: integer);
    procedure SetSortArray();
    procedure SetCheckedArray();
    procedure SetDrawColRowCount();
    procedure CreateStringLists();
    procedure SetCells();
    procedure SetComboboxesAndStringLists();
    procedure ClearCells();
    procedure SetRowHeights();
    procedure SetVisibleFields();
    procedure SetParams();
    procedure EnableApplyBtn();
    procedure ApplyFilters();
    procedure SetWSpace();
    function ChekArea(X, Y, LeftBound, Right, TopBound, Bottom: integer): boolean;
    function CreateDragNDropUpdateSQL(Id, ToCol, ToRow: integer): string;
    procedure SetAxis(AxisBox: TCombobox; Values: TStringList; Ids: TStringList);
    procedure SortBoxChange(Sender: TObject);
    procedure Vertical_cboxChange(Sender: TObject);
  private
    VisibleFields: array of METADATA.TField;
    VValues: TStringList;
    HValues: TStringList;
    VIds: TStringList;
    HIds: TStringList;
    Filter: TMainFilter;
    CheckedArray: array of boolean;
    Space: integer;
    WSpace: integer;
    IsDragNDrop: boolean;
    DragId: integer;
  public
    Cells: array of array of MyCell;
    SortArray: array of SortField;
    Table: TTable;
  end;

  TEvent = procedure of object;

var
  Schedule: TSchedule;

implementation

{$R *.lfm}
constructor CellData.Create();
begin
  Values := TStringList.Create();
  Id := -1;
end;

constructor MyCell.Create();
begin
  Count := 0;
end;

{ TSchedule }

procedure TSchedule.FormClose(Sender: TObject; var CloseAction: TCloseAction);
begin
  CloseAction := caFree;
end;

procedure TSchedule.SetVisibleFields();
var
  i: integer;
begin
  for i := 0 to High(Table.Fields) do
    if Table.Fields[i].Visible then
    begin
      SetLength(VisibleFields, Length(VisibleFields) + 1);
      VisibleFields[High(VisibleFields)] := Table.Fields[i];
    end;
end;

procedure TSchedule.SetParams();
var
  i: integer;
  Params: array of string;
begin
  SetLength(Params, Length(Filter.Filters));
  for i := 0 to High(Params) do
  begin
    Params[i] := Format('Param%d', [i]);
    SchedulesSQLQuery.Params.CreateParam(
      VisibleFields[Filter.Filters[i].FieldNamesBox.ItemIndex].TypeOfData,
      Params[i], ptInput);
    case VisibleFields[Filter.Filters[i].FieldNamesBox.ItemIndex].TypeOfData of
      ftstring:
        SchedulesSQLQuery.ParamByName(Params[i]).AsString := Filter.Filters[i].Edit.Text;
      ftinteger:
        SchedulesSQLQuery.ParamByName(Params[i]).AsInteger :=
          StrToInt(Filter.Filters[i].Edit.Text);
    end;
    ShowMessage(Filter.Filters[i].Edit.Text);
    ShowMessage(SchedulesSQLQuery.Params.ParamValues[Params[i]]);
  end;
end;

procedure TSchedule.EnableApplyBtn();
begin
  Apply.Enabled := True;
end;

function TSchedule.CreateDragNDropUpdateSQL(Id, ToCol, ToRow: integer): string;
begin
  Result := Format('Update %s Set %s = :Param0, %s = :Param1 where %s = :Param2',
    [Table.Name, VisibleFields[Vertical_cbox.ItemIndex].NativeName,
    VisibleFields[Horizontal_cbox.ItemIndex].NativeName, Table.Fields[0].Name]);
end;

function TSchedule.ChekArea(X, Y, LeftBound, Right, TopBound, Bottom: integer): boolean;
begin
  if (X <= Right) and (X >= LeftBound) and (Y >= TopBound) and (Y <= Bottom) then
    Result := True
  else
    Result := False;
end;

procedure TSchedule.ApplyFilters();
begin
  SetParams();
  SchedulesSQLQuery.SQL.Text :=
    Format('%s %s %s', [MainSQLQueryCreate(Table),
    FilterSQLQueryCreate(Filter.Filters, VisibleFields), SortSQLQueryCreate(SortArray)]);
  SchedulesSQLQuery.close;
  SchedulesSQLQuery.open;

  ClearCells();
  SetCells();
  DrawGrid.invalidate;
end;

procedure TSchedule.SetWSpace();
var
  i: integer;
begin
  WSpace := 0;
  for i := 0 to VValues.Count - 1 do
    if Length(VValues.Strings[i]) > WSpace then
      WSpace := Length(VValues.Strings[i]) * DrawGrid.Canvas.TextWidth('A') div 2;
end;

procedure TSchedule.SetRowHeights();
var
  i, j: integer;
  IsEmpty: boolean;
begin
  for i := 1 to DrawGrid.RowCount - 1 do
  begin
    j := 0;
    IsEmpty := False;
    if Length(Cells) > 0 then
    begin
      while (Length(MyCell(Cells[i - 1, j]).Data) = 0) do
      begin
        if j = DrawGrid.ColCount - 2 then
        begin
          IsEmpty := True;
          break;
        end;
        j += 1;
      end;
      if not IsEmpty then
      begin
        Space := 15 * (MyCell(Cells[i - 1, j]).Data[0].Values.Count + 1);
        DrawGrid.RowHeights[i] := Space;
      end
      else
        DrawGrid.RowHeights[i] := 0;
    end;
  end;
end;

procedure TSchedule.ClearCells();
var
  i, j: integer;
begin
  for i := 0 to High(Cells) do
  begin
    for j := 0 to High(Cells[i]) do
      MyCell(Cells[i, j]).Free;
  end;
  SetLength(Cells, 0);
end;

procedure TSchedule.SetComboboxesAndStringLists();
var
  i: integer;
  IsValueExist: boolean;
begin
  for i := 0 to High(Table.Fields) do
    if Table.Fields[i].Visible then
    begin
      SchedulesSQLQuery.First;
      IsValueExist := False;
      SetLength(CheckedArray, Length(CheckedArray) + 1);
      CheckedArray[High(CheckedArray)] := True;
      Vertical_cbox.Items.Add(Table.Fields[i].Caption);
      Horizontal_cbox.Items.Add(Table.Fields[i].Caption);
      SortBox.Items.Add(Table.Fields[i].Caption);
    end;
end;

procedure TSchedule.CreateStringLists();
begin
  HValues := TStringList.Create();
  VValues := TStringList.Create();
  HIds := TStringList.Create();
  VIds := TStringList.Create();
end;

procedure TSchedule.SetCheckedArray();
var
  i, t: integer;
begin
  for i := 0 to High(CheckedArray) do
  begin
    t := CheckListBox.Items.IndexOf(VisibleFields[i].Caption);
    if t >= 0 then
    begin
      if CheckListBox.Checked[t] = False then
        CheckedArray[i] := False
      else
        CheckedArray[i] := True;
    end;
  end;
end;

procedure TSchedule.SetAxis(AxisBox: TComboBox; Values: TStringList; Ids: TStringList);
begin
  //SchedulesSQLQuery.Close;
  SchedulesSQLQuery.SQL.Text :=
    Format('Select * from %s', [VisibleFields[AxisBox.ItemIndex].Table]);
  SchedulesSQLQuery.Close;
  SchedulesSQLQuery.Open;
  Values.Clear;
  Ids.Clear;
  SchedulesSQLQuery.First;
  while not SchedulesSQLQuery.EOF do
  begin
    Values.Add(SchedulesSQLQuery.FieldByName(
      VisibleFields[AxisBox.ItemIndex].Name).Value);
    Ids.Add(SchedulesSQLQuery.Fields.FieldByNumber(1).Value);
    SchedulesSQLQuery.Next;
  end;
  //SchedulesSQLQuery.Close;
end;

procedure TSchedule.SortBoxChange(Sender: TObject);
begin
  EnableApplyBtn();
end;

procedure TSchedule.Vertical_cboxChange(Sender: TObject);
begin
  EnableApplyBtn();
end;

procedure TSchedule.SetCells();
var
  i, j, k: integer;
  WasItemAdded: boolean;
begin
  SetLength(Cells, VValues.Count);
  for i := 0 to VValues.Count - 1 do
  begin
    SetLength(Cells[i], HValues.Count);
    for j := 0 to HValues.Count - 1 do
    begin
      Cells[i, j] := MyCell.Create();
      while ((SchedulesSQLQuery.FieldByName(
          VisibleFields[Horizontal_cbox.ItemIndex].NativeName).Value =
          HIds.Strings[j]) and (SchedulesSQLQuery.FieldByName(
          VisibleFields[Vertical_cbox.ItemIndex].NativeName).Value =
          VIds.Strings[i])) and (not SchedulesSQLQuery.EOF) do
      begin
        with MyCell(Cells[i, j]) do
        begin
          WasItemAdded := False;
          SetLength(Data, Length(Data) + 1);
          Data[High(Data)] := CellData.Create;
          Data[High(Data)].Id := SchedulesSQLQuery.Fields.FieldByNumber(1).Value;
          for k := 0 to High(VisibleFields) do
            if (k <> Horizontal_cbox.ItemIndex) and (k <> Vertical_cbox.ItemIndex) and
              ((CheckListBox.Checked[CheckListBox.items.IndexOf(
              VisibleFields[k].Caption)])) then
            begin
              Data[High(Data)].Values.Add(Format('%s: %s',
                [VisibleFields[k].Caption,
                SchedulesSQLQuery.FieldByName(VisibleFields[k].Name).Value]));
              WasItemAdded := True;
            end;
          if WasItemAdded then
            Inc(Count);
        end;
        SchedulesSQLQuery.Next;
      end;
    end;
  end;
end;

procedure TSchedule.DeleteItem(CurrId: integer);
var
  TempSQL: string;
begin
  if MessageBox(Handle, PChar(
    Utf8ToAnsi('Вы действительно хотите удалить этот элемент?')),
    '', MB_YESNO) = mrYes then
  begin
    TempSQL := SchedulesSQLQuery.SQL.Text;
    SchedulesSQLQuery.SQl.Text :=
      CreateDeleteSQL(Table.Name, Table.Fields[0].Name, IntToStr(CurrId));
    SchedulesSQLQuery.Close;
    try
      begin
        SchedulesSQLQuery.ExecSQL;
        DataUnit.DataBaseConnectionUnit.SQLTransaction.Commit;
        SchedulesSQLQuery.SQL.Text := TempSQL;
        ApplyClick(Apply);
      end;
    except
      begin
        SchedulesSQLQuery.SQL.Text := TempSQL;
        MessageBox(Handle, PChar(
          Utf8ToAnsi('Удаление невозможно, так как на данный элемент ссылается другой элемент')),
          '', MB_OK);
      end;
    end;
  end;
end;

procedure TSchedule.SetDrawColRowCount();
begin
  DrawGrid.ColCount := HValues.Count + 1;
  DrawGrid.RowCount := VValues.Count + 1;
end;

procedure TSchedule.ShowListView(Arow, ACol: integer);
begin
  Application.CreateForm(TListForm, ListForm);
  ListForm.Table := Table;
  ListForm.Show;
  with ListForm.Filter do
  begin
    AddClick(AddNewBtn);
    AddClick(AddNewBtn);
    Filters[0].Enabled := False;
    Filters[1].Enabled := False;
    Filters[0].FieldNamesBox.Text := Vertical_cbox.Items[Vertical_cbox.ItemIndex];
    Filters[0].Edit.Text := VValues.ValueFromIndex[ARow - 1];
    Filters[1].FieldNamesBox.Text := Horizontal_cbox.Items[Horizontal_cbox.ItemIndex];
    Filters[1].Edit.Text := HValues.ValueFromIndex[ACol - 1];
    AcceptClick(AcceptBtn);
  end;
end;

procedure TSchedule.SetSortArray();
begin
  SetLength(SortArray, 3);
  SortArray[0].Name := VisibleFields[Vertical_cbox.ItemIndex].NativeName;
  SortArray[1].Name := VisibleFields[Horizontal_cbox.ItemIndex].NativeName;
  SortArray[2].Name := VisibleFields[SortBox.ItemIndex].NativeName;
  if OrderBox.ItemIndex = 1 then
    SortArray[2].state := 1
  else
    SortArray[2].state := 0;
end;

procedure TSchedule.SetChekBoxes();
var
  i: integer;
  t: integer;
begin
  CheckListBox.Clear;
  for i := 0 to High(VisibleFields) do
    if (i <> Vertical_cbox.ItemIndex) and (i <> Horizontal_cbox.ItemIndex) then
    begin
      t := CheckListBox.Items.Add(VisibleFields[i].Caption);
      if CheckedArray[i] then
        CheckListBox.Checked[t] := True
      else
        CheckListBox.Checked[t] := False;
    end;
end;

procedure TSchedule.Edit(CurrId: integer);
var
  IsExist: boolean;
  i: integer;
begin
  IsExist := False;
  with Screen do
    for i := 0 to FormCount - 1 do
      if Forms[i] is TEditCard then
        if (TEditCard(Forms[i]).Id > 0) and (TEditCard(Forms[i]).Id = CurrId) and
          (TEditCard(Forms[i]).CurrentTable = Table) then
        begin
          Forms[i].ShowOnTop;
          IsExist := True;
          break;
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
  end;
end;

procedure TSchedule.Horizontal_cboxChange(Sender: TObject);
begin
  EnableApplyBtn();
end;

procedure TSchedule.HTMLClick(Sender: TObject);
begin

end;

procedure TSchedule.OrderBoxChange(Sender: TObject);
begin
  EnableApplyBtn();
end;

procedure TSchedule.CreateNew(aRow, aCol: integer);
begin
  Application.CreateForm(TEditCard, EditCard);
  with EditCard do
  begin
    id := -1;
    CurrentTable := Table;
    Show;
    TCombobox(EditCard.EditControls[Vertical_cbox.ItemIndex]).Text :=
      VValues.ValueFromIndex[aRow - 1];
    TCombobox(EditCard.EditControls[Horizontal_cbox.ItemIndex]).Text :=
      HValues.ValueFromIndex[aCol - 1];
  end;
end;

procedure TSchedule.DrawGridDrawCell(Sender: TObject; aCol, aRow: integer;
  aRect: TRect; aState: TGridDrawState);
var
  i, k: integer;
begin
  if gdFixed in aState then
  begin
    DrawGrid.Canvas.Brush.Color := DrawGrid.FixedColor;
    DrawGrid.Canvas.FillRect(aRect);
    if (ACol > 0) and (ACol <= HValues.Count) then
    begin
      DrawGrid.Canvas.TextOut(aRect.Left, aRect.Top, HValues.ValueFromIndex[ACol - 1]);
      DrawGrid.RowHeights[0] := 30;
    end;
    if (ARow > 0) and (ARow <= VValues.Count) then
    begin
      DrawGrid.Canvas.TextOut(aRect.Left, aRect.Top, VValues.ValueFromIndex[ARow - 1]);
      if WSpace > 20 then
        DrawGrid.ColWidths[0] := WSpace + 15
      else
        DrawGrid.ColWidths[0] := 10;
    end;
  end
  else
  begin
    DrawGrid.Canvas.Brush.Color := $FFFFFF;
    DrawGrid.Canvas.FillRect(aRect);
    with MyCell(Cells[Arow - 1, ACol - 1]) do
    begin
      if Length(Data) > 0 then
        Space := 15 * (Data[0].Values.Count + 1);
      for i := 0 to High(Data) do
      begin
        DrawGrid.Canvas.TextOut(aRect.Right - 15, aRect.Bottom - 15,
          IntToStr(Count));
        for k := 0 to Data[i].Values.Count - 1 do
          DrawGrid.Canvas.TextOut(aRect.Left, aRect.Top + k * 15 +
            Space * i, Data[i].Values.Strings[k]);
        ImageList.Draw(DrawGrid.Canvas, aRect.Right - 16, aRect.Top + space * i, 0);
        //ImageList.Draw(DrawGrid.Canvas, aRect.Right - 16, aRect.Bottom - 16, 1);
        ImageList.Draw(DrawGrid.Canvas, aRect.Right - 56, aRect.Top, 2);
        ImageList.Draw(DrawGrid.Canvas, aRect.Right - 16, aRect.Top + space * i + 18, 4);
      end;
      if Vertical_cbox.ItemIndex = Horizontal_cbox.ItemIndex then
      begin
        if aRow = aCol then
          ImageList.Draw(DrawGrid.Canvas, aRect.Right - 36, aRect.Top, 3);
      end
      else
        ImageList.Draw(DrawGrid.Canvas, aRect.Right - 36, aRect.Top, 3);
    end;
  end;
end;

procedure TSchedule.DrawGridMouseDown(Sender: TObject; Button: TMouseButton;
  Shift: TShiftState; X, Y: integer);
var
  Col, Row: integer;
  i, j: integer;
  IsButton: boolean;
  RightBound: integer;
  TopBound: integer;
begin
  DrawGrid.MouseToCell(X, Y, Col, Row);
  IsButton := False;
  if (Col > 0) and (Row > 0) then
  begin
    if MyCell(Cells[Row - 1, Col - 1]).Count > 0 then
    begin
      RightBound := DrawGrid.CellRect(Col, Row).Right;
      TopBound := DrawGrid.CellRect(Col, Row).Top;
      for i := 0 to MyCell(Cells[Row - 1, Col - 1]).Count do
        if ChekArea(X, Y, RightBound - 16, RightBound, TopBound + i *
          space, TopBound + 16 + i * space) then
          IsButton := True;

      for i := 0 to MyCell(Cells[Row - 1, Col - 1]).Count do
        if ChekArea(X, Y, RightBound - 16, RightBound, TopBound + i *
          space + 18, TopBound + 16 + i * space + 18) then
          IsButton := True;

      if ChekArea(X, Y, RightBound - 56, RightBound - 40, TopBound,
        TopBound + 16) then
        IsButton := True;
    end;
    if Vertical_cbox.ItemIndex = Horizontal_cbox.ItemIndex then
    begin
      if Col = Row then
        if ChekArea(X, Y, RightBound - 36, RightBound - 20, TopBound, TopBound + 16) then
          IsButton := True;
    end
    else
    if ChekArea(X, Y, RightBound - 36, RightBound - 20, TopBound, TopBound + 16) then
      IsButton := True;
  end;

  if (Col > 0) and (Row > 0) and (not IsButton) and
    (length(MyCell(Cells[Row - 1, Col - 1]).Data) > 0) then
  begin
    IsDragNDrop := True;
    DragId := MyCell(Cells[Row - 1, Col - 1]).Data[
      (Y - DrawGrid.CellRect(Col, Row).Top) div Space].Id;
  end;
end;

procedure TSchedule.DrawGridMouseMove(Sender: TObject; Shift: TShiftState;
  X, Y: integer);
var
  Col, Row: integer;
begin
  DrawGrid.MouseToCell(X, Y, Col, Row);
end;

procedure TSchedule.DrawGridMouseUp(Sender: TObject; Button: TMouseButton;
  Shift: TShiftState; X, Y: integer);
var
  Col, Row: integer;
  i: integer;
  CurrentId: integer;
  Params: array of string;
  RightBound, TopBound: integer;
begin
  DrawGrid.MouseToCell(X, Y, Col, Row);
  if IsDragNDrop then
  begin
    if Vertical_cbox.ItemIndex <> Horizontal_cbox.ItemIndex then
    begin
      CurrentId := -1;
      if (Length(MyCell(Cells[Row - 1, Col - 1]).Data) > 0) then
        CurrentId := MyCell(Cells[Row - 1, Col - 1]).Data[
          (Y - DrawGrid.CellRect(Col, Row).Top) div Space].Id;
      if CurrentId <> DragId then
      begin
        SetLength(Params, 3);
        for i := 0 to 2 do
        begin
          Params[i] := Format('Param%d', [i]);
          SchedulesSQLQuery.Params.CreateParam(ftinteger, Params[i], ptinput);
        end;
        SchedulesSQLQuery.ParamByName(Params[0]).AsInteger := StrToInt(VIds[Row - 1]);
        SchedulesSQLQuery.ParamByName(Params[1]).AsInteger := StrToInt(HIds[Col - 1]);
        SchedulesSQLQuery.ParamByName(Params[2]).AsInteger := DragId;
        SchedulesSQLQuery.SQL.Text := CreateDragNDropUpdateSQL(DragId, Col, Row);
        SchedulesSQLQuery.ExecSQL;
        DataBaseConnectionUnit.SQLTransaction.Commit;
        ApplyClick(Apply);
      end;
      IsDragNDrop := False;
    end;
  end
  else
  begin
    if (Col > 0) and (Row > 0) then
    begin
      RightBound := DrawGrid.CellRect(Col, Row).Right;
      TopBound := DrawGrid.CellRect(Col, Row).Top;
      if MyCell(Cells[Row - 1, Col - 1]).Count > 0 then
      begin
        for i := 0 to MyCell(Cells[Row - 1, Col - 1]).Count do
          if ChekArea(X, Y, RightBound - 16, RightBound, TopBound +
            i * space, TopBound + 16 + i * space) then
            Edit(MyCell(cells[Row - 1, Col - 1]).Data[i].Id);

        for i := 0 to MyCell(Cells[Row - 1, Col - 1]).Count do
          if ChekArea(X, Y, RightBound - 16, RightBound, TopBound +
            i * space + 18, TopBound + 16 + i * space + 18) then
            DeleteItem(MyCell(cells[Row - 1, Col - 1]).Data[i].Id);

        if ChekArea(X, Y, RightBound - 56, RightBound - 40, TopBound,
          TopBound + 16) then
          ShowListView(Row, Col);
      end;
      if Vertical_cbox.ItemIndex = Horizontal_cbox.ItemIndex then
      begin
        if Col = Row then
          if ChekArea(X, Y, RightBound - 36, RightBound - 20, TopBound,
            TopBound + 16) then
            CreateNew(Row, Col);
      end
      else
      if ChekArea(X, Y, RightBound - 36, RightBound - 20, TopBound, TopBound + 16) then
        CreateNew(Row, Col);
    end;
  end;
end;

procedure TSchedule.EcxelClick(Sender: TObject);
begin

end;

procedure TSchedule.ApplyClick(Sender: TObject);
var
  i, j, k: integer;
begin
  SchedulesSQLQuery.SQL.Text :=
    MainSQLQueryCreate(Table) + ' ' + FilterSQLQueryCreate(Filter.Filters,
    VisibleFields) + ' ' + SortSQLQueryCreate(SortArray);
  //ShowMessage(SchedulesSQLQuery.SQL.Text);
  SchedulesSQLQuery.Close;
  SchedulesSQLQuery.Open;
  ClearCells();
  SetCheckedArray();
  SetChekBoxes();
  SetSortArray();
  SetParams();

  SetAxis(Vertical_cbox, VValues, VIds);
  SetAxis(Horizontal_cbox, HValues, HIds);

  SetWSpace();

  SchedulesSQLQuery.SQL.Text :=
    MainSQLQueryCreate(Table) + ' ' + FilterSQLQueryCreate(Filter.Filters,
    VisibleFields) + ' ' + SortSQLQueryCreate(SortArray);
//  ShowMessage(SchedulesSQLQuery.SQL.Text);
  SchedulesSQLQuery.Close;
  SchedulesSQLQuery.Open;
  SetDrawColRowCount();
  SetCells();
  SetRowHeights();

  for i := 0 to High(Cells) do
  begin
    k := 0;
    for j := 0 to high(Cells[i]) do
      k += MyCell(Cells[i, j]).Count;
    if k = 0 then
      DrawGrid.RowHeights[i + 1] := 0;
  end;

  TButton(Sender).Enabled := False;
  DrawGrid.Invalidate;
end;

procedure TSchedule.CheckListBoxClick(Sender: TObject);
begin
  EnableApplyBtn();
end;

procedure TSchedule.DrawGridDblClick(Sender: TObject);
var
  MaxWidth: integer;
  i, j: integer;
  aRow, aCol: integer;
begin
  aRow := TDrawGrid(Sender).Row;
  aCol := TDrawGrid(Sender).Col;
  MaxWidth := 0;
  with MyCell(Cells[aRow - 1, aCol - 1]) do
  begin
    if (TDrawGrid(Sender).Row > 0) and (Length(Data) > 0) then
      if not IsFullSize then
      begin
        for i := 0 to High(Data) do
          for j := 0 to Data[i].Values.Count - 1 do
            if Length(Data[i].Values.Strings[j]) > MaxWidth then
              MaxWidth := Length(Data[i].Values.Strings[j]);
        DrawGrid.RowHeights[TDrawGrid(Sender).Row] := Count * Space;
        DrawGrid.ColWidths[TDrawGrid(Sender).Col] :=
          MaxWidth div 2 * DrawGrid.Canvas.TextWidth('A');
        IsFullSize := True;
      end
      else
      begin
        DrawGrid.RowHeights[TDrawGrid(Sender).Row] := Space;
        DrawGrid.ColWidths[TDrawGrid(Sender).Col] := 300;
        IsFullSize := False;
      end;
  end;
end;

procedure TSchedule.FormCreate(Sender: TObject);
begin
  Table := TableArray[7];
  Filter := TMainFilter.Create(ScrollBox, Table.Fields, @ApplyFilters);
  //Filter.AcceptBtn.Visible := False;
  //Filter.DeleteBtn.left := Filter.AcceptBtn.Left;
  //Filter.AcceptBtn := Apply;

  SetVisibleFields();
  SchedulesSQLQuery.SQL.Text :=
    MainSQLQueryCreate(Table) + SortSQLQueryCreate(SortArray);
  SchedulesSQLQuery.Open;
  CreateStringLists();
  SetComboboxesAndStringLists();
  Vertical_cbox.ItemIndex := 1;
  Horizontal_cbox.ItemIndex := 0;

  SortBox.ItemIndex := 0;
  SetSortArray();
  SetAxis(Vertical_cbox, VValues, VIds);
  SetAxis(Horizontal_cbox, HValues, HIds);

  SchedulesSQLQuery.SQL.Text :=
    MainSQLQueryCreate(Table) + SortSQLQueryCreate(SortArray);
  SchedulesSQLQuery.Close;
  SchedulesSQLQuery.Open;

  SetDrawColRowCount();

  SetWSpace();

  DrawGrid.DefaultColWidth := 300;
  SetChekBoxes();
  SetCells();
  SetRowHeights;
end;

end.
