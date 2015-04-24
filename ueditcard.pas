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
    TempSQLQuery: TSQLQuery;
    TempDataSource: TDataSource;
    procedure Accept_btnClick(Sender: TObject);
    procedure Cancel_btnClick(Sender: TObject);
    procedure FormCreate(Sender: TObject);
  private
  public
    FieldsCaptions: array of TLabel;
    EditControls: array of TControl;
    Fields: array of Tfield;
    CurrentTable: TTable;
    TestDataSource: TDataSource;
    CurrentId: string;
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

procedure TEditCard.Accept_btnClick(Sender: TObject);
var
  i, j: integer;
  NewId: string;
  Values: array of string;
  FieldsNames: array of string;
begin
  SetLength(Values, Length(EditControls));
  for i := 0 to High(EditControls) do
  begin
    if Fields[i].InnerJoin then
    begin
      TempSQLQuery.SQL.Text :=
        Format('Select first 1 skip %s * from %s',
        [IntToStr(TDBLookupComboBox(EditControls[i]).ItemIndex),
        Fields[i].Table]);
      TempSQLQuery.Open;
      Values[i] := TempDataSOurce.DataSet.Fields.FieldByNumber(1).Value;
      TempSQLQuery.Close;
    end
    else
      Values[i] := Format('''%s''', [TEdit(EditControls[i]).Text]);
  end;

  TempSQLQuery.Close;

  if IsCreateNew then
  begin
    TempSQLQuery.SQL.Text :=
      Format('Select Max(%s) From %s', [CurrentTable.Fields[0].Name, CurrentTable.Name]);
    TempSQLQuery.Open;
    NewId := TempDataSource.DataSet.FieldByName('Max').Value + 1;
    TempSQLQuery.Close;
    TempSQLQuery.SQL.Text := CreateInsertSQL(NewId, CurrentTable.Name, Values);
  end
  else
  begin
    TempSQLQuery.SQL.Text := Format('Select * from %s', [CurrentTable.Name]);
    TempSQLQuery.Open;
    SetLength(FieldsNames, TempDataSource.DataSet.Fields.Count);
    for i := 0 to High(FieldsNames) do
      FieldsNames[i] := TempDataSource.DataSet.Fields[i].FieldName;
    TempSQLQuery.Close;
    TempSQLQuery.SQl.Text := CreateUpdateSQL(CurrentTable.Name,
      FieldsNames, Values, CurrentTable.Fields[0].Name, CurrentId);
  end;
  TempSQLQuery.ExecSQL;
  DataUnit.DataBaseConnectionUnit.SQLTransaction.Commit;
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
  CurrentId := TestDataSource.DataSet.Fields.FieldByNumber(1).Value;

  for i := 0 to High(Fields) do
  begin
    if Fields[i].InnerJoin then
    begin
      EditControls[i] := TDBLookupComboBox.Create(Self);
      TempSQLQuery.SQL.Text :=
        Format('Select %s from %s', [Fields[i].Name, Fields[i].Table]);
      TempSQLQuery.Open;
      with TDBLookUpComboBox(EditControls[i]) do
      begin
        ListSource := TempDataSource;
        KeyField := Fields[i].Name;
        Style := csDropDownList;
        ListSource := nil;
      end;
      TempSQLQuery.Close;
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
          TestDataSource.DataSet.FieldByName(Fields[i].Name).Value;
      finally
      end;
end;

end.
