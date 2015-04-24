CREATE DATABASE 'D:\Databases\test.fdb' user 'SYSDBA' password 'masterkey' DEFAULT CHARACTER SET WIN1251;
CREATE TABLE EducActivities
(
    EducID INTEGER not null primary key, 
    EducName VARCHAR (100) 
);
CREATE TABLE Teachers
( 
	TeacherID INTEGER  not null primary key,
	TeacherInitials VARCHAR (100)
);
CREATE TABLE Groups
(
	GroupID     INTEGER  not null primary key,
	GroupNumber VARCHAR (100) ,
	GroupName   VARCHAR (100)  
);
CREATE TABLE Students
( 
	StudentID       INTEGER  not null primary key,
	StudentInitials VARCHAR (100) ,
	GroupID         INTEGER 
);
CREATE TABLE Subjects
( 
	SubjectID   INTEGER  not null primary key,
    SubjectName VARCHAR (100) 
);
CREATE TABLE Audiences
(
    AudienceID     INTEGER  not null primary key,
	AudienceNumber VARCHAR (100)  
);
CREATE TABLE Pairs
(
	PairID     INTEGER  not null primary key,
	PairBegin  VARCHAR (100) ,
	PairEnd    VARCHAR (100) ,
	PairNumber INTEGER  
);
CREATE TABLE WeekDays
(
	WeekDayID     INTEGER  not null primary key,
	WeekDayName   VARCHAR (100) , 
	WeekDayNumber INTEGER  
);
CREATE TABLE Schedules
(
	GroupID    INTEGER ,
	WeekDayID  INTEGER ,
	PairID     INTEGER ,
	SubjectID  INTEGER ,
	EducID     INTEGER ,
	TeacherID  INTEGER ,
	AudienceID INTEGER 
);
CREATE TABLE Teachers_Subjects
(
	TeacherID INTEGER,
	SubjectID INTEGER
);
CREATE TABLE Group_Subjects
(
	GroupID INTEGER,
	SubjectID INTEGER
);

INSERT INTO EducActivities VALUES (1, 'Лекция');
INSERT INTO EducActivities VALUES (2, 'Практика');

INSERT INTO Teachers VALUES (101, 'Кленин Александр Сергеевич');
INSERT INTO Teachers VALUES (102, 'Туфанов Игорь Евгеньевич');
INSERT INTO Teachers VALUES (103, 'Клевчихин Юрий Александрович');
INSERT INTO Teachers VALUES (104, 'Лудов Игорь Юрьевич ');
INSERT INTO Teachers VALUES (105, 'Жуплев Антон Сергеевич');
INSERT INTO Teachers VALUES (106, 'Пак Геннадий Константинович');
INSERT INTO Teachers VALUES (107, 'Машенцев Владимир Юрьевич');
INSERT INTO Teachers VALUES (108, 'Преподаватель физкультуры');
INSERT INTO Teachers VALUES (109, 'Спорышев Максим Сергеевич');
INSERT INTO Teachers VALUES (110, 'Никольская  Татьяна Владимировна');
INSERT INTO Teachers VALUES (111, 'Крикунова Юлия Анатольевна');
INSERT INTO Teachers VALUES (112, 'Пак С.Б.');
INSERT INTO Teachers VALUES (113, 'Сторожок Е.А.');
INSERT INTO Teachers VALUES (114, 'Давыдов Денис Витальевич');
INSERT INTO Teachers VALUES (115, 'Доставалов Валерий Николаевич');
INSERT INTO Teachers VALUES (116, 'Шепелева Риорита Петровна');
INSERT INTO Teachers VALUES (117, 'Романюк Мария Александровна');
INSERT INTO Teachers VALUES (118, 'Бризицкий Роман Викторович');
INSERT INTO Teachers VALUES (119, 'Пынько Ирина Викторовна');
INSERT INTO Teachers VALUES (120, 'Кравцов Дмитрий Сергеевич');
INSERT INTO Teachers VALUES (121, 'Пак Татьяна Владимировна');

INSERT INTO Groups VALUES (701, 'Б8103a', 'Прикладная математика и информатика');
INSERT INTO Groups VALUES (702, 'Б8103б', 'Прикладная математика и информатика');
INSERT INTO Groups VALUES (703, 'Б8203а', 'Прикладная математика и информатика');
INSERT INTO Groups VALUES (704, 'Б8203б', 'Прикладная математика и информатика');

INSERT INTO Students VALUES (201,  'Акмухамедова Наталья Алексеевна', 701);
INSERT INTO Students VALUES (202,  'Батраков Артем Глебович', 701);
INSERT INTO Students VALUES (203,  'Бедина Полина Артемовна', 701);
INSERT INTO Students VALUES (204,  'Варламов Кирилл Евгеньевич', 701);
INSERT INTO Students VALUES (205,  'Ватутин Сергей Федорович', 701);
INSERT INTO Students VALUES (206,  'Виноградов Иван Владимирович', 701);
INSERT INTO Students VALUES (207,  'Возный Роман Сергеевич', 701);
INSERT INTO Students VALUES (208,  'Глух Валерия Игоревна', 701);
INSERT INTO Students VALUES (209,  'Глущенко Николай Владимирович', 701);
INSERT INTO Students VALUES (210, 'Гнездилова Алина Игоревна', 701);
INSERT INTO Students VALUES (211, 'Гусаров Владислав Евгеньевич', 701);
INSERT INTO Students VALUES (212, 'Гутник Павел Евгеньевич', 701);
INSERT INTO Students VALUES (213, 'Жихарева Александра Руслановна', 701);
INSERT INTO Students VALUES (214, 'Зуев Илья Андреевич', 701);
INSERT INTO Students VALUES (215, 'Карандаев Тимур Александрович', 701);
INSERT INTO Students VALUES (216, 'Кирпа Вадим Дмитриевич', 701);
INSERT INTO Students VALUES (217, 'Козицкая Ирина Сергеевна', 701);
INSERT INTO Students VALUES (218, 'Кулинченко Евгений Марсельевич', 701);
INSERT INTO Students VALUES (219, 'Липов Илья Максимович', 701);
INSERT INTO Students VALUES (220, 'Манякин Алексей Сергеевич', 701);
INSERT INTO Students VALUES (221, 'Мартыненко Лев Константинович', 701);
INSERT INTO Students VALUES (222, 'Махлярчук Андрей Александрович', 701);
INSERT INTO Students VALUES (223, 'Михайлова Евгения Александровна', 701);
INSERT INTO Students VALUES (224, 'Недалюк Игорь Андреевич', 701);
INSERT INTO Students VALUES (225, 'Нестеров Михаил Васильевич', 701);
INSERT INTO Students VALUES (226, 'Николенко Михаил Юрьевич', 701);
INSERT INTO Students VALUES (227, 'Рассказова Дарья Романовна', 701);
INSERT INTO Students VALUES (228, 'Сбоев Константин Павлович', 701);
INSERT INTO Students VALUES (229, 'Стоцкий Олег Владиславович', 701);
INSERT INTO Students VALUES (230, 'Томак Дмитрий Геннадьевич', 701);
INSERT INTO Students VALUES (231, 'Трофимова Ольга Николаевна', 701);
INSERT INTO Students VALUES (232, 'Тышковский Артур Дмитриевич', 701);
INSERT INTO Students VALUES (233, 'Утин Никита Сергеевич', 701);
INSERT INTO Students VALUES (234, 'Чернов Дмитрий Дмитриевич', 701);
INSERT INTO Students VALUES (235, 'Щуров Алексей Андреевич', 701);

INSERT INTO Students VALUES (236, 'Аветисян Богдан Робертович', 702);
INSERT INTO Students VALUES (237, 'Арефьев Андрей Михайлович', 702);
INSERT INTO Students VALUES (238, 'Бельчиков Владимир Геннадьевич', 702);
INSERT INTO Students VALUES (239, 'Богданов Дмитрий Игоревич', 702);
INSERT INTO Students VALUES (240, 'Бычков Кирилл Олегович', 702);
INSERT INTO Students VALUES (241, 'Васильева Анастасия Андреевна', 702);
INSERT INTO Students VALUES (242, 'Глухова Кристина Александровна', 702);
INSERT INTO Students VALUES (243, 'Глущенко Даниил Александрович', 702);
INSERT INTO Students VALUES (244, 'Гутник Марина Леонидовна', 702);
INSERT INTO Students VALUES (245, 'Демидовский Игорь Вадимович', 702);
INSERT INTO Students VALUES (246, 'Джумаев Никита Рустамович', 702);
INSERT INTO Students VALUES (247, 'Завершинский Иван Владимирович', 702);
INSERT INTO Students VALUES (248, 'Ильницкий Сергей Васильевич', 702);
INSERT INTO Students VALUES (249, 'Коренев Андрей Борисович', 702);
INSERT INTO Students VALUES (250, 'Коротких Марк Александрович', 702);
INSERT INTO Students VALUES (251, 'Красников Алексей Викторович', 702);
INSERT INTO Students VALUES (252, 'Кудрявцев Никита Андреевич', 702);
INSERT INTO Students VALUES (253, 'Кузнецов Кирилл Леонидович', 702);
INSERT INTO Students VALUES (254, 'Лешневский Захар Юрьевич', 702);
INSERT INTO Students VALUES (255, 'Лю Елизавета Робертовна', 702);
INSERT INTO Students VALUES (256, 'Маринченко Иван Александрович', 702);
INSERT INTO Students VALUES (257, 'Медведев Максим Дмитриевич', 702);
INSERT INTO Students VALUES (258, 'Павленко Дмитрий Русланович', 702);
INSERT INTO Students VALUES (259, 'Пискун Илья Владимирович', 702);
INSERT INTO Students VALUES (260, 'Потапова Валерия Евгеньевна', 702);
INSERT INTO Students VALUES (261, 'Пьянкова Алена Федоровна', 702);
INSERT INTO Students VALUES (262, 'Разломова Анастасия Константиновна', 702);
INSERT INTO Students VALUES (263, 'Стулева Светлана Руслановна', 702);
INSERT INTO Students VALUES (264, 'Сыромятникова Дарина Олеговна', 702);
INSERT INTO Students VALUES (265, 'Федоров Константин Сергеевич', 702);
INSERT INTO Students VALUES (266, 'Харченко Наталия Владимировна', 702);
INSERT INTO Students VALUES (267, 'Шкрябина Ирина Владимировна', 702);


INSERT INTO Subjects VALUES (301, 'Математический анализ');
INSERT INTO Subjects VALUES (302, 'Алгебра и геометрия');
INSERT INTO Subjects VALUES (303, 'Практикум на ЭВМ');
INSERT INTO Subjects VALUES (304, 'Физическая культура');
INSERT INTO Subjects VALUES (305, 'Иностранный язык');
INSERT INTO Subjects VALUES (306, 'Базы данных');
INSERT INTO Subjects VALUES (307, 'Дискретная математика');
INSERT INTO Subjects VALUES (308, 'Языки и методы программирования');
INSERT INTO Subjects VALUES (309, 'Экономика');
INSERT INTO Subjects VALUES (310, 'Объектно-ориентированный анализ');
INSERT INTO Subjects VALUES (311, 'Физика');
INSERT INTO Subjects VALUES (312, 'Дифференциальные уравнения');
INSERT INTO Subjects VALUES (313, 'Комплексный анализ');
INSERT INTO Subjects VALUES (314, 'Безопасность жизнедеятельности');
INSERT INTO Subjects VALUES (315, 'Численные методы');
INSERT INTO Subjects VALUES (316, 'Экономическая теория');

INSERT INTO Audiences VALUES (401,  'D734');
INSERT INTO Audiences VALUES (402,  'D549a');
INSERT INTO Audiences VALUES (403,  'D547');
INSERT INTO Audiences VALUES (404,  'D743');
INSERT INTO Audiences VALUES (405,  'D547');
INSERT INTO Audiences VALUES (406,  'D732');
INSERT INTO Audiences VALUES (407,  'D542');
INSERT INTO Audiences VALUES (408, 'D747');
INSERT INTO Audiences VALUES (409, 'физкультурный зал');
INSERT INTO Audiences VALUES (410, 'D738');
INSERT INTO Audiences VALUES (411, 'D820');
INSERT INTO Audiences VALUES (412, 'D548');
INSERT INTO Audiences VALUES (413, 'D549'); 
INSERT INTO Audiences VALUES (414, 'D818');
INSERT INTO Audiences VALUES (415, 'D820');
INSERT INTO Audiences VALUES (416, 'D734a'); 

INSERT INTO Pairs VALUES (501, '8.30',  '10.10', 1);
INSERT INTO Pairs VALUES (502, '10.10', '11.40', 2);
INSERT INTO Pairs VALUES (503, '11.50', '13.20', 3);
INSERT INTO Pairs VALUES (504, '13.30', '15.00', 4);
INSERT INTO Pairs VALUES (505, '15.10', '16.40', 5);
INSERT INTO Pairs VALUES (506, '16.50', '18.20', 6);

INSERT INTO WeekDays VALUES (601, 'Понедельник', 1);
INSERT INTO WeekDays VALUES (602, 'Вторник', 2);
INSERT INTO WeekDays VALUES (603, 'Среда', 3);
INSERT INTO WeekDays VALUES (604, 'Четверг', 4);
INSERT INTO WeekDays VALUES (605, 'Пятница', 5);
INSERT INTO WeekDays VALUES (606, 'Суббота', 6);
INSERT INTO WeekDays VALUES (607, 'Воскресенье', 7);

INSERT INTO Schedules VALUES (701, 601, 501, 303, 2, 107, 401);
INSERT INTO Schedules VALUES (701, 601, 502, 303, 2, 107, 401);

INSERT INTO Schedules VALUES (701, 602, 501, 302, 2, 106, 403);
INSERT INTO Schedules VALUES (701, 602, 502, 307, 2, 106, 407);
INSERT INTO Schedules VALUES (701, 602, 503, 301, 2, 103, 408);
INSERT INTO Schedules VALUES (701, 602, 504, 307, 2, 106, 403);

INSERT INTO Schedules VALUES (701, 603, 502, 307, 2, 106, 408);
INSERT INTO Schedules VALUES (701, 603, 503, 302, 1, 106, 408);
INSERT INTO Schedules VALUES (701, 603, 504, 302, 1, 106, 406);
INSERT INTO Schedules VALUES (701, 603, 505, 304, 2, 106, 409);

INSERT INTO Schedules VALUES (701, 604, 501, 308, 2, 109, 401);
INSERT INTO Schedules VALUES (701, 604, 502, 308, 2, 109, 401);
INSERT INTO Schedules VALUES (701, 604, 503, 308, 1, 104, 410);

INSERT INTO Schedules VALUES (701, 605, 502, 301, 1, 103, 407);
INSERT INTO Schedules VALUES (701, 605, 503, 301, 2, 103, 407);

INSERT INTO Schedules VALUES (701, 606, 502, 306, 2, 101, 401);
INSERT INTO Schedules VALUES (701, 606, 503, 306, 1, 101, 410);
INSERT INTO Schedules VALUES (701, 606, 504, 305, 2, 110, 406);
INSERT INTO Schedules VALUES (701, 606, 505, 304, 2, 108, 409);


INSERT INTO Schedules VALUES (702, 601, 501, 307, 2, 106, 411);
INSERT INTO Schedules VALUES (702, 601, 502, 307, 2, 106, 411);
INSERT INTO Schedules VALUES (702, 601, 503, 305, 2, 111, 404);
INSERT INTO Schedules VALUES (702, 601, 504, 302, 2, 106, 412);

INSERT INTO Schedules VALUES (702, 602, 502, 307, 1, 106, 407);
INSERT INTO Schedules VALUES (702, 602, 503, 302, 2, 106, 403);

INSERT INTO Schedules VALUES (702, 603, 503, 301, 2, 103, 405);
INSERT INTO Schedules VALUES (702, 603, 504, 302, 1, 106, 406);
INSERT INTO Schedules VALUES (702, 603, 505, 304, 2, 108, 409);

INSERT INTO Schedules VALUES (702, 604, 502, 301, 2, 103, 405);
INSERT INTO Schedules VALUES (702, 604, 503, 308, 1, 104, 410);
INSERT INTO Schedules VALUES (702, 604, 504, 303, 2, 112, 401);
INSERT INTO Schedules VALUES (702, 604, 505, 303, 2, 112, 401);

INSERT INTO Schedules VALUES (702, 605, 501, 301, 1, 103, 407);
INSERT INTO Schedules VALUES (702, 605, 502, 308, 2, 102, 401);
INSERT INTO Schedules VALUES (702, 605, 503, 308, 2, 102, 401);
INSERT INTO Schedules VALUES (702, 605, 502, 306, 2, 113, 401);

INSERT INTO Schedules VALUES (702, 606, 503, 306, 1, 101, 410);
INSERT INTO Schedules VALUES (702, 606, 505, 304, 2, 108, 409);


INSERT INTO Schedules VALUES (703, 601, 501, 309, 1, 114, 410);
INSERT INTO Schedules VALUES (703, 601, 502, 309, 1, 114, 410);
INSERT INTO Schedules VALUES (703, 601, 503, 310, 1, 105, 406);
INSERT INTO Schedules VALUES (703, 601, 504, 303, 2, 101, 413);
INSERT INTO Schedules VALUES (703, 601, 505, 303, 2, 101, 413);

INSERT INTO Schedules VALUES (703, 602, 505, 304, 2, 108, 409);

INSERT INTO Schedules VALUES (703, 603, 501, 311, 1, 115, 414);
INSERT INTO Schedules VALUES (703, 603, 502, 312, 1, 116, 406);
INSERT INTO Schedules VALUES (703, 603, 503, 312, 2, 116, 406);
INSERT INTO Schedules VALUES (703, 603, 504, 312, 2, 116, 411);
INSERT INTO Schedules VALUES (703, 603, 505, 305, 2, 117, 404);

INSERT INTO Schedules VALUES (703, 604, 501, 313, 1, 103, 407);
INSERT INTO Schedules VALUES (703, 604, 502, 315, 1, 121, 410);
INSERT INTO Schedules VALUES (703, 604, 503, 313, 2, 103, 405);
INSERT INTO Schedules VALUES (703, 604, 504, 315, 2, 118, 401);
INSERT INTO Schedules VALUES (703, 604, 505, 314, 2, 119, 406);

INSERT INTO Schedules VALUES (703, 605, 501, 310, 2, 105, 401);
INSERT INTO Schedules VALUES (703, 605, 502, 310, 2, 105, 401);
INSERT INTO Schedules VALUES (703, 605, 503, 316, 2, 120, 406);


INSERT INTO Schedules VALUES (704, 601, 501, 309, 1, 114, 410);
INSERT INTO Schedules VALUES (704, 601, 502, 309, 1, 114, 410);
INSERT INTO Schedules VALUES (704, 601, 503, 310, 1, 105, 406);
INSERT INTO Schedules VALUES (704, 601, 504, 310, 2, 107, 406);
INSERT INTO Schedules VALUES (704, 601, 505, 310, 2, 107, 406);

INSERT INTO Schedules VALUES (704, 602, 503, 312, 2, 116, 406);
INSERT INTO Schedules VALUES (704, 602, 504, 312, 2, 116, 406);
INSERT INTO Schedules VALUES (704, 602, 505, 304, 2, 108, 409);

INSERT INTO Schedules VALUES (704, 603, 501, 311, 1, 115, 414);
INSERT INTO Schedules VALUES (704, 603, 502, 312, 1, 116, 406);
INSERT INTO Schedules VALUES (704, 603, 504, 305, 2, 117, 412);

INSERT INTO Schedules VALUES (704, 604, 502, 315, 1, 121, 410);
INSERT INTO Schedules VALUES (704, 604, 503, 315, 2, 118, 401);
INSERT INTO Schedules VALUES (704, 604, 504, 313, 2, 103, 405);
INSERT INTO Schedules VALUES (704, 604, 505, 314, 2, 119, 406);

INSERT INTO Schedules VALUES (704, 605, 502, 303, 2, 112, 402);
INSERT INTO Schedules VALUES (704, 605, 503, 316, 2, 120, 406);
INSERT INTO Schedules VALUES (704, 605, 504, 303, 2, 112, 402);
INSERT INTO Schedules VALUES (704, 605, 505, 304, 2, 108, 409);


INSERT INTO Teachers_Subjects VALUES (101, 306);
INSERT INTO Teachers_Subjects VALUES (103, 301);
INSERT INTO Teachers_Subjects VALUES (104, 308);
INSERT INTO Teachers_Subjects VALUES (106, 302);
INSERT INTO Teachers_Subjects VALUES (106, 307);
INSERT INTO Teachers_Subjects VALUES (107, 303);
INSERT INTO Teachers_Subjects VALUES (108, 304);
INSERT INTO Teachers_Subjects VALUES (109, 308);
INSERT INTO Teachers_Subjects VALUES (110, 305);
INSERT INTO Teachers_Subjects VALUES (111, 305);
INSERT INTO Teachers_Subjects VALUES (112, 303);
INSERT INTO Teachers_Subjects VALUES (113, 306);
INSERT INTO Teachers_Subjects VALUES (114, 309);
INSERT INTO Teachers_Subjects VALUES (105, 310);
INSERT INTO Teachers_Subjects VALUES (115, 311);
INSERT INTO Teachers_Subjects VALUES (117, 305);
INSERT INTO Teachers_Subjects VALUES (101, 303);
INSERT INTO Teachers_Subjects VALUES (103, 313);
INSERT INTO Teachers_Subjects VALUES (116, 312);
INSERT INTO Teachers_Subjects VALUES (121, 315);
INSERT INTO Teachers_Subjects VALUES (119, 314);
INSERT INTO Teachers_Subjects VALUES (118, 315);
INSERT INTO Teachers_Subjects VALUES (120, 316);

INSERT INTO Group_Subjects VALUES (701, 301);
INSERT INTO Group_Subjects VALUES (701, 302);
INSERT INTO Group_Subjects VALUES (701, 303);
INSERT INTO Group_Subjects VALUES (701, 304);
INSERT INTO Group_Subjects VALUES (701, 305);
INSERT INTO Group_Subjects VALUES (701, 306);
INSERT INTO Group_Subjects VALUES (701, 307);
INSERT INTO Group_Subjects VALUES (701, 308);
INSERT INTO Group_Subjects VALUES (702, 301);
INSERT INTO Group_Subjects VALUES (702, 302);
INSERT INTO Group_Subjects VALUES (702, 303);
INSERT INTO Group_Subjects VALUES (702, 304);
INSERT INTO Group_Subjects VALUES (702, 305);
INSERT INTO Group_Subjects VALUES (702, 306);
INSERT INTO Group_Subjects VALUES (702, 307);
INSERT INTO Group_Subjects VALUES (702, 308);
INSERT INTO Group_Subjects VALUES (703, 309);
INSERT INTO Group_Subjects VALUES (703, 310);
INSERT INTO Group_Subjects VALUES (703, 304);
INSERT INTO Group_Subjects VALUES (703, 311);
INSERT INTO Group_Subjects VALUES (703, 312);
INSERT INTO Group_Subjects VALUES (703, 305);
INSERT INTO Group_Subjects VALUES (703, 313);
INSERT INTO Group_Subjects VALUES (703, 314);
INSERT INTO Group_Subjects VALUES (703, 315);
INSERT INTO Group_Subjects VALUES (703, 303);
INSERT INTO Group_Subjects VALUES (703, 316);
INSERT INTO Group_Subjects VALUES (704, 309);
INSERT INTO Group_Subjects VALUES (704, 310);
INSERT INTO Group_Subjects VALUES (704, 304);
INSERT INTO Group_Subjects VALUES (704, 311);
INSERT INTO Group_Subjects VALUES (704, 312);
INSERT INTO Group_Subjects VALUES (704, 305);
INSERT INTO Group_Subjects VALUES (704, 313);
INSERT INTO Group_Subjects VALUES (704, 314);
INSERT INTO Group_Subjects VALUES (704, 315);
INSERT INTO Group_Subjects VALUES (704, 303);
INSERT INTO Group_Subjects VALUES (704, 316);


