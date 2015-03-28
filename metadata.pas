unit MetaData;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils;

type

  TField = class
  public
    Name: string;
    Caption: string;
    Width: integer;
    Visible: boolean;
    Table: string;
    TypeOfData: string;
  end;

  TReferenceField = class(Tfield)
  public
    FromTable, FiledName: string;
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
  IsFormCreated: array of boolean;
  IsAboutAppCreated: boolean;

implementation

class function TTable.RegisterTable(TableName, TableCaption: string): Ttable; static;
begin
  SetLength(TableArray, Length(TableArray) + 1);
  TableArray[high(TableArray)] := TTable.Create;
  TableArray[High(TableArray)].Name := TableName;
  TableArray[high(TableArray)].Caption := TableCaption;
  Result := TableArray[high(TableArray)];
end;

procedure AddField(FieldName, FieldCaption, DataType, TableName: string;
  FieldWidth: integer; IsVisible: boolean);
var
  CurrentTable: TTable;
begin
  CurrentTable := TableArray[high(TableArray)];
  SetLength(CurrentTable.Fields, length(CurrentTable.Fields) + 1);
  CurrentTable.Fields[High(CurrentTable.Fields)] := TField.Create;
  CurrentTable.Fields[High(CurrentTable.Fields)].Name := FieldName;
  CurrentTable.Fields[High(CurrentTable.Fields)].Caption := FieldCaption;
  CurrentTable.Fields[High(CurrentTable.Fields)].Width := FieldWidth;
  CurrentTable.Fields[High(CurrentTable.Fields)].TypeOfData := DataType;
  CurrentTable.Fields[High(CurrentTable.Fields)].Visible := IsVisible;
  CurrentTable.Fields[High(CurrentTable.Fields)].Table := TableName;
end;

procedure AddReferenceField(From, Name: string);
var
  CurrentTable: TTable;
begin
  CurrentTable := TableArray[high(TableArray)];
  SetLength(CurrentTable.RefefenceFields, length(CurrentTable.RefefenceFields) + 1);
  CurrentTable.RefefenceFields[High(CurrentTable.RefefenceFields)] :=
    TReferenceField.Create;
  CurrentTable.RefefenceFields[high(CurrentTable.RefefenceFields)].FromTable := From;
  CurrentTable.RefefenceFields[high(CurrentTable.RefefenceFields)].FiledName := Name;
end;

initialization
  with TTable.RegisterTable('Students', 'Студенты') do
  begin
    addField('StudentID', 'id', 'integer', 'Students', 30, False);
    addField('StudentInitials', 'Фамилия Имя Отчество',
      'varchar (100)', 'Students', 220, True);
    addField('Groupid', 'id Группы', 'integer', 'Students', 60, False);
    addField('GroupNumber', 'Номер группы', 'varchar (100)', 'Groups', 100, True);
    addField('GroupName', 'Специальность', 'varchar (100)', 'Groups', 250, True);
    AddReferenceField('Groups', 'Groupid');
  end;

  with Ttable.RegisterTable('Groups', 'Группы') do
  begin
    addField('Groupid', 'id', 'integer', 'Groups', 30, False);
    addField('GroupNumber', 'Номер группы', 'varchar (100)', 'Groups', 100, True);
    addField('GroupName', 'Специальность', 'varchar (100)', 'Groups', 250, True);
  end;

  with Ttable.RegisterTable('Teachers', 'Преподаватели') do
  begin
    addField('Teacherid', 'id', 'integer', 'Teachers', 30, False);
    AddField('TeacherInitials', 'Фамилия Имя Отчество',
      'varchar (100)', 'Teachers', 230, True);
  end;

  with TTable.RegisterTable('Subjects', 'Предметы') do
  begin
    AddField('Subjectid', 'id', 'integer', 'Subjects', 30, False);
    AddField('SubjectName', 'Наименование предмета', 'varchar (100)', 'Subjects',
      215, True);
  end;

  with Ttable.RegisterTable('Audiences', 'Аудитории') do
  begin
    AddField('Audienceid', 'id', 'integer', 'Audiences', 30, False);
    AddField('AudienceNumber', 'Номер Аудитории', 'varchar (100)', 'Audiences',
      150, True);
  end;

  with Ttable.RegisterTable('Pairs', 'Занятия') do
  begin
    AddField('Pairid', 'id', 'integer', 'Pairs', 30, False);
    AddField('PairNumber', 'Номер занятия', 'integer', 'Pairs', 100, True);
    AddField('PairBegin', 'Начало занятия', 'varchar (100)', 'Pairs', 120, True);
    AddField('PairEnd', 'Окончание занятия', 'varchar (100)', 'Pairs', 120, True);
  end;

  with Ttable.RegisterTable('WeekDays', 'Дни недели') do
  begin
    AddField('WeekDayid', 'id', 'integer', 'WeekDays', 30, False);
    AddField('WeekDayName', 'День недели', 'varchar (100)', 'WeekDays', 80, True);
    AddField('WeekDayNumber', 'Номер дня недели', 'integer', 'WeekDays', 110, True);
  end;

  with Ttable.RegisterTable('Schedules', 'Расписание') do
  begin
    AddField('groupid', 'id Группы', 'integer', 'Schedules', 65, False);
    AddField('GroupNumber', 'Номер группы', 'varchar (100)', 'Groups', 100, True);
    AddReferenceField('Groups', 'Groupid');
    AddField('WeekDayId', 'id Дня недели', 'integer', 'Schedules', 85, False);
    AddReferenceField('WeekDays', 'WeekDayId');
    AddField('WeekDayName', 'День недели', 'varchar (100)', 'WeekDays', 80, True);
    AddField('PairID', 'id Занятия', 'integer', 'Schedules', 65, False);
    AddReferenceField('Pairs', 'Pairid');
    AddField('PairNumber', 'Номер занятия', 'integer', 'Pairs', 120, True);
    AddField('PairBegin', 'Начало занятия', 'varchar (100)', 'Pairs', 120, True);
    AddField('PairEnd', 'Окончание занятия', 'varchar (100)', 'Pairs', 120, True);
    AddField('SubjectId', 'id Предмета', 'integer', 'Schedules', 80, False);
    AddReferenceField('Subjects', 'SubjectId');
    AddField('SubjectName', 'Наименование предмета', 'varchar (100)', 'Subjects',
      215, True);
    AddField('EducId', 'id Вида занятия', 'integer', 'Schedules', 100, False);
    AddReferenceField('EducActivities', 'EducId');
    AddField('EducName', 'Вид занятия', 'varchar (100)', 'EducActivities', 90, True);
    AddField('TeacherId', 'id Преподавателя', 'integer', 'Schedules', 105, False);
    AddReferenceField('Teachers', 'TeacherId');
    AddField('TeacherInitials', 'Фамилия Имя Отчество',
      'varchar (100)', 'Teachers', 230, True);
    AddField('AudienceId', 'id Аудитории', 'integer', 'Schedules', 80, False);
    AddReferenceField('Audiences', 'AudienceId');
    AddField('AudienceNumber', 'Номер Аудитории', 'varchar (100)', 'Audiences',
      150, True);
  end;

  with Ttable.RegisterTable('Teachers_Subjects', 'Предметы преподавателей') do
  begin
    AddField('TeacherId', 'id Преподавателя', 'integer', 'Teachers_Subjects',
      105, False);
    AddReferenceField('Teachers', 'TeacherID');
    AddField('TeacherInitials', 'Фамилия Имя Отчество',
      'varchar (100)', 'Teachers', 230, True);
    AddField('SubjectId', 'id Предмета', 'integer', 'Teachers_Subjects', 80, False);
    AddReferenceField('Subjects', 'SubjectId');
    AddField('SubjectName', 'Наименование предмета', 'varchar (100)', 'Subjects',
      215, True);
  end;

  with Ttable.RegisterTable('Group_Subjects', 'Предметы групп') do
  begin
    AddField('groupid', 'id Группы', 'integer', 'Group_Subjects', 65, False);
    AddReferenceField('Groups', 'GroupId');
    AddField('GroupNumber', 'Номер группы', 'varchar (100)', 'Groups', 100, True);
    AddField('SubjectId', 'id Предмета', 'integer', 'Group_Subjects', 80, False);
    AddReferenceField('Subjects', 'SubjectId');
    AddField('SubjectName', 'Наименование предмета', 'varchar (100)', 'Subjects',
      215, True);
  end;

  with Ttable.RegisterTable('EducActivities', 'Виды занятий') do
  begin
    AddField('EducId', 'id', 'integer', 'EducActivities', 30, False);
    AddField('EducName', 'Вид занятия', 'varchar (100)', 'EducActivities', 90, True);
  end;
end.
