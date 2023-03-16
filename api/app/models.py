from sqlalchemy import Column, Integer, String
from database import Base

class Product(Base):
    __tablename__ = 'products'

    id = Column(Integer, primary_key=True, index=True)
    name = Column(String(50), nullable=False)
    description = Column(String(255))
    price = Column(Integer, nullable=False)
    quantity = Column(Integer, nullable=False)
