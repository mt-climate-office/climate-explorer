import os

from simplejson import dumps
from sqlalchemy import create_engine
from sqlalchemy.ext.declarative import declarative_base
from sqlalchemy.orm import sessionmaker

user = os.getenv("POSTGRES_USER")
password = os.getenv("POSTGRES_PASSWORD")
dbname = os.getenv("POSTGRES_DBNAME")
hostname = os.getenv("POSTGRES_HOSTNAME")

SQLALCHEMY_DATABASE_URL = f"postgresql://{user}:{password}@{hostname}:5432/{dbname}"

engine = create_engine(SQLALCHEMY_DATABASE_URL, json_serializer=dumps)
SessionLocal = sessionmaker(autocommit=False, autoflush=False, bind=engine)

Base = declarative_base()