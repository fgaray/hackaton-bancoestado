from flask import Flask
from flask.ext.sqlalchemy import SQLAlchemy
from flask.ext.script import Manager
from flask.ext.migrate import Migrate, MigrateCommand

#from sqlalchemy import Table, Column, Integer, ForeignKey
from sqlalchemy.orm import relationship, backref
from sqlalchemy.ext.declarative import declarative_base

# Database Configurations
app = Flask(__name__)
DATABASE = 'database-name'
PASSWORD = 'password'
USER = 'root'
HOSTNAME = 'mysqlserver'

app.config['SQLALCHEMY_DATABASE_URI'] = 'mysql://%s:%s@%s/%s' % (USER,
                                                                 PASSWORD,
                                                                 HOSTNAME,
                                                                 DATABASE)
db = SQLAlchemy(app)

# Database migration command line
migrate = Migrate(app, db)
manager = Manager(app)
manager.add_command('db', MigrateCommand)

class Person(db.Model):
    __tablename__ = 'person'
    id = db.Column(db.Integer, primary_key=True, nullable=False)
    name = db.Column(db.String(80))
    
    def __init__(self, name):
        self.name = name

    def __repr__(self):
        return '<Name %r>' % (self.name)


class CreateDB():
    def __init__(self, hostname=None):
        if hostname != None:
            HOSTNAME = hostname
        import sqlalchemy
        engine = sqlalchemy.create_engine('mysql://%s:%s@%s' %
                                          (USER, PASSWORD,
                                           HOSTNAME))  # connect to server
        engine.execute("CREATE DATABASE IF NOT EXISTS %s " %
                       (DATABASE))  # create db
        # TODO: Automatize create tables
        #db.create_all()


if __name__ == '__main__':
    manager.run()
