from sqlalchemy import Column, Date, Numeric, String

from database import Base


class County(Base):
    __tablename__ = "county"
    __table_args__ = {"schema": "future", "extend_existing": True}

    name = Column(String)
    id = Column(String, primary_key=True, index = True)
    model = Column(String, primary_key=True, index = True)
    scenario = Column(String, primary_key=True, index = True)
    variable = Column(String, primary_key=True, index = True)
    date = Column(Date, primary_key=True, index=True)
    value = Column(Numeric)


