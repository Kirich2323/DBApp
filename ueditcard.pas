unit UEditCard;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, DB, BufDataset, sqldb, FileUtil, Forms, Controls, Graphics,
  Dialogs, StdCtrls, DBCtrls, DBGrids, MetaData, SQLQueryCreation, DataUnit;

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
    procedure FormCreate(Sender: TObject);
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
begin
  SetLength(Values, Length(EditControls));
  for i := 0 to High(EditControls) do
  begin
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
  end;

  CardSQLQuery.Close;

  if IsCreateNew then
  begin
    CardSQLQuery.SQL.Text :=
      Format('Select Max(%s) From %s', [CurrentTable.Fields[0].Name, CurrentTable.Name]);
    CardSQLQuery.Open;
    NewId := CardDataSource.DataSet.FieldByName('Max').Value + 1;
    CardSQLQuery.Close;
    CardSQLQuery.SQL.Text := CreateInsertSQL(NewId, CurrentTable.Name, Values);
  end
  else
  begin
    CardSQLQuery.SQL.Text := Format('Select * from %s', [CurrentTable.Name]);
    CardSQLQuery.Open;
    SetLength(FieldsNames, CardDataSource.DataSet.Fields.Count);
    for i := 0 to High(FieldsNames) do
      FieldsNames[i] := CardDataSource.DataSet.Fields[i].FieldName;
    CardSQLQuery.Close;
    CardSQLQuery.SQl.Text := CreateUpdateSQL(CurrentTable.Name,
      FieldsNames, Values, CurrentTable.Fields[0].Name, CurrentId);
  end;

  CardSQLQuery.ExecSQL;
  DataBaseConnectionUnit.SQLTransaction.Commit;
  Close;
end;

procedure TEditCard.FormCreate(Sender: TObject);
var
  PreviousControl: TControl;
  i, j: integer;
begin
  SetLength(FieldsCaptions, Length(Fields));      //Оптимизировать это!!!
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
      Width := 180;
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
      try
        TEdit(EditControls[i]).Text :=
          ListDataSource.DataSet.FieldByName(Fields[i].Name).Value;
      finally
      end;
end;

end.
