unit UEditCard;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, DB, BufDataset, sqldb, FileUtil, Forms, Controls, Graphics,
  Dialogs, StdCtrls, DBCtrls, DBGrids, MetaData, SQLQueryCreation, DataUnit, Windows;

type
  TEvent = procedure of object;

  { TEditCard }

  TEditCard = class(TForm)
    Accept_btn: TButton;
    Cancel_btn: TButton;
    CardSQLQuery: TSQLQuery;
    CardDataSource: TDataSource;
    procedure Accept_btnClick(Sender: TObject);
    procedure Button1Click(Sender: TObject);
    procedure Cancel_btnClick(Sender: TObject);
    procedure FormClose(Sender: TObject; var CloseAction: TCloseAction);
    procedure FormShow(Sender: TObject);
    function ChekErrors(): boolean;
    procedure RefreshEditCard();
  private
    FieldsCaptions: array of TLabel;
    CurrentId: string;
    Ids: array of TStringList;
    Fields: array of TField;
  public
    Id: integer;
    EditControls: array of TControl;
    CurrentTable: TTable;
    IsCreateNew: boolean;
  end;

var
  EditCard: TEditCard;
  RefreshForms: TEvent;

implementation

{$R *.lfm}

procedure TEditCard.Cancel_btnClick(Sender: TObject);
begin
  Close;
end;

function TEditCard.ChekErrors(): boolean;
var
  i: integer;
begin
  Result := False;
  for i := 0 to High(Fields) do
    if Fields[i].InnerJoin then
      if TCombobox(EditControls[i]).Text = '' then
      begin
        MessageBox(Handle, PChar(Utf8ToAnsi('Заполните пожалуйста все поля!')),
          PChar(Utf8ToAnsi('Ошибка')), MB_OK);
        Result := True;
        break;
      end
      else
      if TEdit(EditCOntrols[i]).Text = '' then
      begin
        MessageBox(Handle, PChar(Utf8ToAnsi('Заполните пожалуйста все поля!')),
          PChar(Utf8ToAnsi('Ошибка')), MB_OK);
        Result := True;
        break;
      end;
end;

procedure TEditCard.FormClose(Sender: TObject; var CloseAction: TCloseAction);
begin
  CloseAction := caFree;
end;

procedure TEditCard.Accept_btnClick(Sender: TObject);
var
  i: integer;
  NewId: string;
  Values: array of string;
  Params: array of string;
  FieldsNames: array of string;
begin
  SetLength(Params, Length(Fields) + 1);

  if not ChekErrors() then
  begin
    Params[0] := 'Param0';
    CardSQLQuery.Params.CreateParam(ftInteger, Params[0], ptInput);
    CardSQLQuery.ParamByName(Params[0]).AsInteger := id;
    for i := 1 to Length(EditControls) do
    begin
      Params[i] := Format('Param%d', [i]);
      CardSQLQuery.Params.CreateParam(Fields[i - 1].TypeOfData, Params[i], ptInput);
      if Fields[i - 1].InnerJoin then
      begin
        CardSQLQuery.ParamByName(Params[i]).AsInteger :=
          StrToInt(Ids[i - 1].Strings[TCombobox(EditControls[i - 1]).ItemIndex]);
      end
      else
        case Fields[i - 1].TypeOfData of
          ftinteger: CardSQLQuery.ParamByName(Params[i]).AsInteger :=
              StrToInt(TEdit(EditControls[i - 1]).Text);
          ftstring: CardSQLQuery.ParamByName(Params[i]).AsString :=
              TEdit(EditControls[i - 1]).Text;
        end;
    end;

    if id < 0 then
      CardSQLQuery.SQl.Text := CreateInsertSQL(CurrentTable)
    else
      CardSQLQuery.SQl.Text := CreateUpdateSQL(CurrentTable, id);
    //ShowMessage(CardSQLQuery.SQL.Text);
    CardSQLQuery.ExecSQL;
    DataBaseConnectionUnit.SQLTransaction.Commit;
    RefreshForms();
    Close;
  end;
end;

procedure TEditCard.Button1Click(Sender: TObject);
begin
  ShowMessage(Ids[1].Strings[TCombobox(EditControls[1]).ItemIndex]);
end;

procedure TEditCard.FormShow(Sender: TObject);
var
  PreviousControl: TControl;
  i: integer;
begin
  CardSQLQuery.SQL.Text := MainSQLQueryCreate(CurrentTable);
  CardSQLQuery.Open;

  for i := 0 to High(CurrentTable.Fields) do
    if CurrentTable.Fields[i].Visible then
    begin
      SetLength(Fields, Length(Fields) + 1);
      Fields[High(Fields)] := CurrentTable.Fields[i];
    end;

  SetLength(FieldsCaptions, Length(Fields));
  SetLength(EditControls, Length(Fields));
  SetLength(Ids, Length(Fields));
  PreviousControl := EditCard;
  CardSQLQuery.Close;

  for i := 0 to High(Fields) do
  begin
    if Fields[i].InnerJoin then
    begin
      EditControls[i] := TComboBox.Create(Self);
      Ids[i] := TStringList.Create();
      CardSQLQuery.SQL.Text :=
        Format('Select * from %s', [Fields[i].Table]);
      CardSQLQuery.Open;
      with TComboBox(EditControls[i]) do
      begin
        while not CardSQLQuery.EOF do
        begin
          Items.Add(CardSQLQuery.FieldByName(Fields[i].Name).Value);
          Ids[i].Add(CardSQLQuery.Fields.FieldByNumber(1).Value);
          CardSQLQuery.Next;
        end;
        Style := csDropDownList;
      end;
      CardSQLQuery.Close;
    end
    else
      EditControls[i] := TEdit.Create(Self);

    with EditControls[i] do
    begin
      Parent := Self;
      AnchorToNeighbour(akTop, 10, PreviousControl);
      Left := 200;
      Width := 280;
    end;

    FieldsCaptions[i] := TLabel.Create(Self);
    with FieldsCaptions[i] do
    begin
      Parent := Self;
      Caption := Fields[i].Caption;
      AnchorToNeighbour(akTop, 10, PreviousControl);
      Left := 15;
    end;
    PreviousControl := EditControls[i];
  end;

  with EditControls[0] do
  begin
    Anchors := [];
    Top := 30;
  end;

  with FieldsCaptions[0] do
  begin
    Anchors := [];
    Top := 30;
  end;

  CardSQLQuery.SQL.Text := MainSQLQueryCreate(CurrentTable) +
    Format(' Where %s = %s', [CurrentTable.Fields[0].Name, IntToStr(id)]);
  CardSQLQuery.Open;

  if id >= 0 then
    for i := 0 to High(Fields) do
      if Fields[i].InnerJoin then
      begin
        TComboBox(EditControls[i]).Text :=
          CardSQLQuery.FieldByName(Fields[i].Name).Text;
      end
      else
        TEdit(EditControls[i]).Text :=
          CardSQLQuery.FieldByName(Fields[i].Name).Value;
end;

procedure TEditCard.RefreshEditCard();
var
  i: integer;
begin
  for i := 0 to High(Fields) do
  begin
    if Fields[i].InnerJoin then
    begin
      TCombobox(EditControls[i]).Items.Clear;
      Ids[i].Clear;
      CardSQLQuery.SQL.Text :=
        Format('Select * from %s', [Fields[i].Table]);
      CardSQLQuery.Open;
      with TComboBox(EditControls[i]) do
      begin
        while not CardSQLQuery.EOF do
        begin
          Items.Add(CardSQLQuery.FieldByName(Fields[i].Name).Value);
          Ids[i].Add(CardSQLQuery.Fields.FieldByNumber(1).Value);
          CardSQLQuery.Next;
        end;
        Style := csDropDownList;
      end;
      CardSQLQuery.Close;
    end;
  end;

  CardSQLQuery.SQL.Text := MainSQLQueryCreate(CurrentTable) +
    Format(' Where %s = %s', [CurrentTable.Fields[0].Name, IntToStr(id)]);
  CardSQLQuery.Open;

  if id >= 0 then
    for i := 0 to High(Fields) do
      if Fields[i].InnerJoin then
      begin
        TComboBox(EditControls[i]).Text :=
          CardSQLQuery.FieldByName(Fields[i].Name).Text;
      end;
end;

end.
