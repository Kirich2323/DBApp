unit UEditCard;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, DB, BufDataset, sqldb, FileUtil, Forms, Controls, Graphics,
  Dialogs, StdCtrls, DBCtrls, DBGrids, MetaData, SQLQueryCreation, DataUnit, Windows;

type

  { TEditCard }

  TEditCard = class(TForm)
    Accept_btn: TButton;
    Cancel_btn: TButton;
    CardSQLQuery: TSQLQuery;
    CardDataSource: TDataSource;
    procedure Accept_btnClick(Sender: TObject);
    procedure Cancel_btnClick(Sender: TObject);
    procedure FormClose(Sender: TObject; var CloseAction: TCloseAction);
    procedure FormShow(Sender: TObject);
    function ChekErrors(): boolean;
  private
    FieldsCaptions: array of TLabel;
    EditControls: array of TControl;
    CurrentId: string;
  public
    Fields: array of TField;
    CurrentTable: TTable;
    ListDataSource: TDataSource;
    IsCreateNew: boolean;
  end;

var
  EditCard: TEditCard;

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
  for i := 0 to High(EditControls) do
    if TEdit(EditControls[i]).Text = '' then
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
  FieldsNames: array of string;
  IsError: boolean;
begin
  IsError := ChekErrors();

  if not IsError then
  begin
    IsError := False;
    SetLength(Values, Length(EditControls));
    for i := 0 to High(EditControls) do
      if Fields[i].InnerJoin then
      begin
        CardSQLQuery.SQL.Text :=
          Format('Select first 1 skip %s * from %s',
          [IntToStr(TDBLookupComboBox(EditControls[i]).ItemIndex),
          Fields[i].Table]);
        CardSQLQuery.Open;
        Values[i] := CardDataSource.DataSet.Fields.FieldByNumber(1).Value;
        CardSQLQuery.Close;
      end
      else
        Values[i] := Format('''%s''', [TEdit(EditControls[i]).Text]);

    //CardSQLQuery.Close;

    if IsCreateNew then
    begin
      with CardSQLQuery do
      begin
        SQL.Text :=
          Format('Select Max(%s) From %s', [CurrentTable.Fields[0].Name,
          CurrentTable.Name]);
        Open;
        NewId := CardDataSource.DataSet.FieldByName('Max').Value + 1;
        Close;
        SQL.Text := CreateInsertSQL(NewId, CurrentTable.Name, Values);
      end;
    end
    else
    begin
      with CardSQLQuery do
      begin
        SQL.Text := Format('Select * from %s', [CurrentTable.Name]);
        Open;
        SetLength(FieldsNames, CardDataSource.DataSet.Fields.Count);
        for i := 0 to High(FieldsNames) do
          FieldsNames[i] := CardDataSource.DataSet.Fields[i].FieldName;
        Close;
        SQl.Text :=
          CreateUpdateSQL(CurrentTable.Name, FieldsNames, Values,
          CurrentTable.Fields[0].Name, CurrentId);
      end;
    end;

    CardSQLQuery.ExecSQL;
    DataBaseConnectionUnit.SQLTransaction.Commit;
    Close;
  end;
end;

procedure TEditCard.FormShow(Sender: TObject);
var
  PreviousControl: TControl;
  i: integer;
begin
  SetLength(FieldsCaptions, Length(Fields));
  SetLength(EditControls, Length(Fields));
  PreviousControl := EditCard;
  CurrentId := ListDataSource.DataSet.Fields.FieldByNumber(1).Value;

  for i := 0 to High(Fields) do
  begin
    if Fields[i].InnerJoin then
    begin
      EditControls[i] := TDBLookupComboBox.Create(Self);
      CardSQLQuery.SQL.Text :=
        Format('Select %s from %s', [Fields[i].Name, Fields[i].Table]);
      CardSQLQuery.Open;
      with TDBLookUpComboBox(EditControls[i]) do
      begin
        ListSource := CardDataSource;
        KeyField := Fields[i].Name;
        Style := csDropDownList;
        ListSource := nil;
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

  if not IsCreateNew then
    for i := 0 to High(Fields) do
      TEdit(EditControls[i]).Text :=
        ListDataSource.DataSet.FieldByName(Fields[i].Name).Value;
end;

end.
