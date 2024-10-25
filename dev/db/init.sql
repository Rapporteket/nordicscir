CREATE DATABASE IF NOT EXISTS db_data
  DEFAULT
  CHARACTER SET = 'utf8'
  COLLATE = 'utf8_danish_ci';

CREATE DATABASE IF NOT EXISTS db_log
  CHARACTER SET = 'utf8'
  COLLATE = 'utf8_danish_ci';


CREATE DATABASE IF NOT EXISTS db_autoreport
  CHARACTER SET = 'utf8'
  COLLATE = 'utf8_danish_ci';

GRANT ALL PRIVILEGES ON `db`.* TO 'user'@'%' IDENTIFIED BY 'user';
