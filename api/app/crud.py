import polars as pl
import pint
from fastapi.encoders import jsonable_encoder
from sqlalchemy import extract, Numeric
from sqlalchemy.orm import Session, aliased
from sqlalchemy.sql import func, cast, select
from sqlalchemy.sql.expression import case

import database as db
import models
from database import SessionLocal

# Set the unit system and definitions
ureg = pint.UnitRegistry(system="SI")
Q_ = ureg.Quantity
ureg.define("percent = 0.01 = %")

# A function to get the latest raw data
# as a Pandas data frame
def get_timeseries(session: Session, county: str, variable: str):

    from sqlalchemy import case, literal

    session = SessionLocal()
    county = "30001"
    variable = "tas"
    query = (
        session.query(
            extract('year', models.County.date).label('year'),
            models.County.scenario,
            models.County.model,
            case((models.County.variable.in_(['pr', 'penman', 'hargreaves']), func.sum(models.County.value)),
                else_=func.avg(models.County.value)).label('value')
        )
        .filter(
            models.County.id == '30001',
            models.County.variable == 'pr'
        )
        .group_by(
            extract('year', models.County.date),
            models.County.scenario,
            models.County.model
        )
    )


    session= SessionLocal()
    q1 = select(
            func.extract('year', models.County.date).label('year'),
            models.County.scenario,
            models.County.model,
            case((models.County.variable.in_(['pr', 'penman', 'hargreaves']), func.sum(models.County.value)),
                else_=func.avg(models.County.value)).label('value')
        ).where(
            (models.County.id == county) & (models.County.variable == variable)
        ).group_by(
            func.extract('year', models.County.date),
            models.County.scenario,
            models.County.model
        ).subquery()

    q2 = select(
            q1.c.year,
            q1.c.scenario,
            cast(func.percentile_cont(0.9).within_group(q1.c.value), Numeric).label('upper'),
            cast(func.percentile_cont(0.1).within_group(q1.c.value), Numeric).label('lower'),
            func.percentile_cont(0.5).within_group(q1.c.value).label('value')
        ).group_by(
            q1.c.year,
            q1.c.scenario
        )

    result = session.execute(q2).fetchall()
    dat = pl.from_dicts(
        jsonable_encoder(q.all())
    )
