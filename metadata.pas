unit MetaData;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Dialogs, DB;

type

  TField = class
  public
    Name: string;
    NativeName: string;
    Caption: string;
    Width: integer;
    Visible: boolean;
    Table: string;
    TypeOfData: TFieldType;
    InnerJoin: boolean;
  end;

  TReferenceField = class(Tfield)
  public
    FromTable, FiledName: string;
    LeftTablesField, RightTablesField: string;
  end;

  TTable = class
  public
    Name: string;
    Caption: string;
    Fields: array of Tfield;
    RefefenceFields: array of TReferenceField;
    class function RegisterTable(TableName, TableCaption: string): TTable; static;
  end;

  TTableClass = class of TTable;

var
  TableArray: array of TTable;

implementation

class function TTable.RegisterTable(TableName, TableCaption: string): Ttable; static;
begin
  SetLength(TableArray, Length(TableArray) + 1);
  TableArray[high(TableArray)] := TTable.Create;
  TableArray[High(TableArray)].Name := TableName;
  TableArray[high(TableArray)].Caption := TableCaption;
  Result := TableArray[high(TableArray)];
end;

procedure AddField(FieldName, FieldCaption: string; DataType: TFieldType;
  TableName: string; FieldWidth: integer; IsVisible: boolean;
  NeedToJoin: boolean = False; JoinFieldLeft: string = '';
  JoiningFieldRight: string = '');
var
  CurrentTable: TTable;
  i: integer;
  IsTableAlreadyExists: boolean = False;
begin
  CurrentTable := TableArray[high(TableArray)];
  SetLength(CurrentTable.Fields, length(CurrentTable.Fields) + 1);
  CurrentTable.Fields[High(CurrentTable.Fields)] := TField.Create;
  with CurrentTable.Fields[High(CurrentTable.Fields)] do
  begin
    Name := FieldName;
    Caption := FieldCaption;
    Width := FieldWidth;
    TypeOfData := DataType;
    Visible := IsVisible;
    Table := TableName;
    InnerJoin := NeedToJoin;
  end;

  if NeedToJoin then
  begin
    for i := 0 to High(CurrentTable.RefefenceFields) do
      if CurrentTable.RefefenceFields[i].FromTable = TableName then
      begin
        IsTableAlreadyExists := True;
        break;
      end;
    CurrentTable.Fields[High(CurrentTable.Fields)].NativeName := JoinFieldLeft;

    if not IsTableAlreadyExists then
    begin
      SetLength(CurrentTable.RefefenceFields, Length(CurrentTable.RefefenceFields) + 1);
      CurrentTable.RefefenceFields[High(CurrentTable.RefefenceFields)] :=
        TReferenceField.Create;

      with CurrentTable.RefefenceFields[high(CurrentTable.RefefenceFields)] do
      begin
        FromTable := TableName;
        //FiledName := FieldName;
        LeftTablesField := JoinFieldLeft;
        RightTablesField := JoiningFieldRight;
      end;
    end;
  end;
end;

initialization
  with TTable.RegisterTable('Students', 'Студенты') do
  begin
    AddField('StudentID', 'Ид', ftinteger, 'Students', 100, False);
    AddField('StudentInitials', 'Фамилия Имя Отчество',
      ftstring, 'Students', 220, True);
    AddField('GroupNumber', 'Номер группы', ftstring, 'Groups',
      130, True, True, 'Groupid', 'Groupid');
    //AddField('GroupName', 'Специальность', ftstring, 'Groups',
    //  250, True, True, 'Groupid', 'Groupid');
  end;

  with Ttable.RegisterTable('Groups', 'Группы') do
  begin
    AddField('Groupid', 'id', ftinteger, 'Groups', 30, False);
    AddField('GroupNumber', 'Номер группы', ftstring, 'Groups', 130, True);
    AddField('GroupName', 'Специальность', ftstring, 'Groups', 250, True);
  end;

  with Ttable.RegisterTable('Teachers', 'Преподаватели') do
  begin
    AddField('Teacherid', 'id', ftinteger, 'Teachers', 30, False);
    AddField('TeacherInitials', 'Фамилия Имя Отчество',
      ftstring, 'Teachers', 230, True);
  end;

  with TTable.RegisterTable('Subjects', 'Предметы') do
  begin
    AddField('Subjectid', 'id', ftinteger, 'Subjects', 30, False);
    AddField('SubjectName', 'Наименование предмета', ftstring, 'Subjects',
      215, True);
  end;

  with Ttable.RegisterTable('Audiences', 'Аудитории') do
  begin
    AddField('Audienceid', 'id', ftinteger, 'Audiences', 30, False);
    AddField('AudienceNumber', 'Номер Аудитории', ftstring, 'Audiences',
      150, True);
  end;

  with Ttable.RegisterTable('Pairs', 'Занятия') do
  begin
    AddField('Pairid', 'id', ftinteger, 'Pairs', 30, False);
    AddField('PairBegin', 'Начало занятия', ftstring, 'Pairs', 120, True);
    AddField('PairEnd', 'Окончание занятия', ftstring, 'Pairs', 150, True);
    AddField('PairNumber', 'Номер занятия', ftinteger, 'Pairs', 130, True);
  end;

  with Ttable.RegisterTable('WeekDays', 'Дни недели') do
  begin
    AddField('WeekDayid', 'id', ftinteger, 'WeekDays', 30, False);
    AddField('WeekDayName', 'День недели', ftstring, 'WeekDays', 130, True);
    AddField('WeekDayNumber', 'Номер дня недели', ftinteger, 'WeekDays', 150, True);
  end;

  with Ttable.RegisterTable('Schedules', 'Расписание') do
  begin
    AddField('RecordId', 'id', ftinteger, 'Schedules', 30, False);
    AddField('GroupId', 'GroupId', ftinteger, 'Schedules', 30, False);
    AddField('GroupNumber', 'Номер группы', ftstring, 'Groups',
      130, True, True, 'GroupId', 'GroupId');

    AddField('WeekDayId', 'WeekDayId', ftinteger, 'Schedules', 30, False);
    AddField('WeekDayName', 'День недели', ftstring, 'WeekDays', 130,
      True, True, 'WeekDayId', 'WeekDayId');

    AddField('PairId', 'PairId', ftinteger, 'Schedules', 30, False);
    AddField('PairNumber', 'Номер занятия', ftinteger, 'Pairs', 150,
      True, True, 'PairId', 'PairId');

    AddField('SubjectId', 'SubjectId', ftinteger, 'Schedules', 30, False);
    AddField('SubjectName', 'Наименование предмета', ftstring, 'Subjects',
      215, True, True, 'SubjectId', 'SubjectId');

    AddField('EducId', 'EducId', ftinteger, 'Schedules', 30, False);
    AddField('EducName', 'Вид занятия', ftstring, 'EducActivities', 130,
      True, True, 'EducId', 'EducId');

    AddField('TeacherId', 'TeacherId', ftinteger, 'Schedules', 30, False);
    AddField('TeacherInitials', 'Фамилия Имя Отчество',
      ftstring, 'Teachers', 230, True, True, 'TeacherId', 'TeacherId');

    AddField('AudienceId', 'AudienceId', ftinteger, 'Schedules', 30, False);
    AddField('AudienceNumber', 'Номер Аудитории', ftstring, 'Audiences',
      150, True, True, 'AudienceId', 'AudienceId');
  end;

  with Ttable.RegisterTable('Teachers_Subjects', 'Предметы преподавателей') do
  begin
    AddField('RecordId', 'id', ftinteger, 'Teachers_subjects', 30, False);
    AddField('TeacherInitials', 'Фамилия Имя Отчество',
      ftstring, 'Teachers', 230, True, True, 'TeacherId', 'TeacherId');
    AddField('SubjectName', 'Наименование предмета', ftstring, 'Subjects',
      215, True, True, 'SubjectId', 'SubjectId');
  end;

  with Ttable.RegisterTable('Group_Subjects', 'Предметы групп') do
  begin
    AddField('RecordId', 'id', ftinteger, 'Group_Subjects', 30, False);
    AddField('GroupNumber', 'Номер группы', ftstring, 'Groups',
      130, True, True, 'GroupId', 'GroupId');
    AddField('SubjectName', 'Наименование предмета', ftstring, 'Subjects',
      215, True, True, 'SubjectId', 'SubjectId');
  end;

  with Ttable.RegisterTable('EducActivities', 'Виды занятий') do
  begin
    AddField('EducId', 'id', ftinteger, 'EducActivities', 60, False);
    AddField('EducName', 'Вид занятия', ftstring, 'EducActivities', 120, True);
  end;
end.
