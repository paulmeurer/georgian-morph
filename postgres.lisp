
# lokokina: psql is /usr/local/pgsql/bin/psql

psql -U postgres
create database gnc;
create user gnc with password 'kartuli';
grant all privileges on database gnc to gnc;
create schema morph;
grant all privileges on schema morph to gnc;
