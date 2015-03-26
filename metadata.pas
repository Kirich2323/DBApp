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
  end;

  TTable = class
  public
    Name: string;
    Caption: string;
    Fields: array of Tfield;
  end;

  TTableClass = class of TTable;

var
  TableArray: array of TTable;
  isFormCreated: array of boolean;
  isAboutAppCreated: boolean;

implementation

procedure RegisterTable(TableName, TableCaption: string);
begin
  SetLength(TableArray, Length(TableArray) + 1);
  TableArray[high(TableArray)] := TTable.Create;
  TableArray[High(TableArray)].Name := TableName;
  TableArray[high(TableArray)].Caption := TableCaption;
end;

procedure AddField(FieldName, FieldCaption: string; FieldWidth: integer);
var
  CurrentTable: TTable;
begin
  CurrentTable := TableArray[high(TableArray)];
  SetLength(CurrentTable.Fields, length(CurrentTable.Fields) + 1);
  CurrentTable.Fields[High(CurrentTable.Fields)] := TField.Create;
  CurrentTable.Fields[High(CurrentTable.Fields)].Name := FieldName;
  CurrentTable.Fields[High(CurrentTable.Fields)].Caption := FieldCaption;
  CurrentTable.Fields[High(CurrentTable.Fields)].Width := FieldWidth;
end;

initialization
  RegisterTable('Students', 'Студенты');
  addField('id', 'id', 30);
  addField('Name', 'Фамилия Имя Отчество', 220);
  addField('Group id', 'id Группы', 60);
  RegisterTable('Groups', 'Группы');
  addField('id', 'id', 30);
  addField('Name', 'Номер группы', 100);
  addField('GroupNumber', 'Специальность', 250);
  RegisterTable('Teachers', 'Преподаватели');
  addField('id', 'id', 30);
  AddField('Initials', 'Фамилия Имя Отчество', 230);
  RegisterTable('Subjects', 'Предметы');
  AddField('id', 'id', 30);
  AddField('Name', 'Наименование предмета', 215);
  RegisterTable('Audiences', 'Аудитории');
  AddField('id', 'id', 30);
  AddField('Number', 'Номер Аудитории', 150);
  RegisterTable('Pairs', 'Занятия');
  AddField('id', 'id', 30);
  AddField('Begin', 'Начало занятия', 120);
  AddField('End', 'Окончание занятия', 120);
  AddField('Number', 'Номер занятия', 120);
  RegisterTable('WeekDays', 'Дни недели');
  AddField('id', 'id', 30);
  AddField('Name', 'День недели', 80);
  AddField('Number', 'Номер дня недели', 110);
  RegisterTable('Schedules', 'Расписание');
  AddField('groupid', 'id Группы', 65);
  AddField('WeekDayId', 'id Дня недели', 85);
  AddField('PairID', 'id Занятия', 65);
  AddField('SubjectId', 'id Предмета', 80);
  AddField('EducId', 'id Вида занятия', 100);
  AddField('TeacherId', 'id Преподавателя', 105);
  AddField('AudienceId', 'id Аудитории', 80);
  RegisterTable('Teachers_Subjects', 'Предметы преподавателей');
  AddField('TeacherId', 'id Преподавателя', 105);
  AddField('SubjectId', 'id Предмета', 80);
  RegisterTable('Group_Subjects', 'Предметы групп');
  AddField('groupid', 'id Группы', 65);
  AddField('SubjectId', 'id Предмета', 80);
  RegisterTable('EducActivities', 'Виды занятий');
  AddField('EducId', 'id', 30);
  AddField('EducName', 'Вид занятия', 90);
end.
