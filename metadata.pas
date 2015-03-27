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
    TypeOfData: string;
  end;

  TTable = class
  public
    Name: string;
    Caption: string;
    Fields: array of Tfield;
    class function RegisterTable(TableName, TableCaption: string): TTable; static;
    //class procedure AddField(FieldName, FieldCaption: string; FieldWidth: integer); static;
  end;

  TTableClass = class of TTable;

var
  TableArray: array of TTable;
  isFormCreated: array of boolean;
  isAboutAppCreated: boolean;

implementation

class function TTable.RegisterTable(TableName, TableCaption: string): Ttable; static;
begin
  SetLength(TableArray, Length(TableArray) + 1);
  TableArray[high(TableArray)] := TTable.Create;
  TableArray[High(TableArray)].Name := TableName;
  TableArray[high(TableArray)].Caption := TableCaption;
  Result := TableArray[high(TableArray)];
end;

procedure AddField(FieldName, FieldCaption, DataType: string;
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
end;

//class procedure TTable.AddField(FieldName, FieldCaption: string; FieldWidth: integer);
//begin
//  //CurrentTable := TableArray[high(TableArray)];
//  SetLength(Fields, length(Fields) + 1);
//  Fields[High(Fields)] := TField.Create;
//  Fields[High(Fields)].Name := FieldName;
//  Fields[High(Fields)].Caption := FieldCaption;
//  Fields[High(.Fields)].Width := FieldWidth;
//end;

initialization
  with TTable.RegisterTable('Students', 'Студенты') do
  begin
    addField('StudentID', 'id', 'integer', 30, False);
    addField('StudentInitials', 'Фамилия Имя Отчество', 'varchar (100)', 220, True);
    addField('Groupid', 'id Группы', 'integer', 60, False);
  end;

  with Ttable.RegisterTable('Groups', 'Группы') do
  begin
    addField('Groupid', 'id', 'integer', 30, False);
    addField('GroupNumber', 'Номер группы', 'varchar (100)', 100, True);
    addField('GroupName', 'Специальность', 'varchar (100)', 250, True);
  end;

  with Ttable.RegisterTable('Teachers', 'Преподаватели') do
  begin
    addField('Teacherid', 'id', 'integer', 30, False);
    AddField('TeacherInitials', 'Фамилия Имя Отчество',
      'varchar (100)', 230, True);
  end;

  with TTable.RegisterTable('Subjects', 'Предметы') do
  begin
    AddField('Subjectid', 'id', 'integer', 30, False);
    AddField('SubjectName', 'Наименование предмета', 'varchar (100)',
      215, True);
  end;

  with Ttable.RegisterTable('Audiences', 'Аудитории') do
  begin
    AddField('Audienceid', 'id', 'integer', 30, False);
    AddField('AudienceNumber', 'Номер Аудитории', 'varchar (100)',
      150, True);
  end;

  with Ttable.RegisterTable('Pairs', 'Занятия') do
  begin
    AddField('Pairid', 'id', 'integer', 30, False);
    AddField('PairBegin', 'Начало занятия', 'varchar (100)', 120, True);
    AddField('PairEnd', 'Окончание занятия', 'varchar (100)', 120, True);
    AddField('PairNumber', 'Номер занятия', 'integer', 120, True);
  end;

  with Ttable.RegisterTable('WeekDays', 'Дни недели') do
  begin
    AddField('WeekDayid', 'id', 'integer', 30, False);
    AddField('WeekDayName', 'День недели', 'varchar (100)', 80, True);
    AddField('WeekDayNumber', 'Номер дня недели', 'integer', 110, True);
  end;

  with Ttable.RegisterTable('Schedules', 'Расписание') do
  begin
    AddField('groupid', 'id Группы', 'integer', 65, False);
    AddField('WeekDayId', 'id Дня недели', 'integer', 85, False);
    AddField('PairID', 'id Занятия', 'integer', 65, False);
    AddField('SubjectId', 'id Предмета', 'integer', 80, False);
    AddField('EducId', 'id Вида занятия', 'integer', 100, False);
    AddField('TeacherId', 'id Преподавателя', 'integer', 105, False);
    AddField('AudienceId', 'id Аудитории', 'integer', 80, False);
  end;

  with Ttable.RegisterTable('Teachers_Subjects', 'Предметы преподавателей') do
  begin
    AddField('TeacherId', 'id Преподавателя', 'integer', 105, False);
    AddField('SubjectId', 'id Предмета', 'integer', 80, False);
  end;

  with Ttable.RegisterTable('Group_Subjects', 'Предметы групп') do
  begin
    AddField('groupid', 'id Группы', 'integer', 65, False);
    AddField('SubjectId', 'id Предмета', 'integer', 80, False);
  end;

  with Ttable.RegisterTable('EducActivities', 'Виды занятий') do
  begin
    AddField('EducId', 'id', 'integer', 30, False);
    AddField('EducName', 'Вид занятия', 'varchar (100)', 90, True);
  end;
end.
