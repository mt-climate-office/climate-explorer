import logging


from fastapi import (
    Depends,
    FastAPI,
    Request,
    Response,
)
from sqlalchemy.orm import Session
from sqlalchemy.dialects import postgresql

import models
import database as db
import schemas

models.Base.metadata.create_all(bind=db.engine)


app = FastAPI(
    title="BLM Climate Report APIv1",
    description="An API to gather historical and projected climate data for BLM parcels in Montana",
    version="0.0.1",
    terms_of_service="http://example.com/terms/",
    contact={
        "name": "Colin Brust",
        "url": "https://climate.umt.edu",
        "email": "colin.brust@mso.umt.edu",
    },
    # TODO: This has to be commented out when running locally.
    root_path="/api/",
)

logger = logging.getLogger("uvicorn")


# Dependency
def get_db():
    d = db.SessionLocal()
    try:
        yield d
    finally:
        d.close()


def return_type_table(data, request, mime_type):

    if mime_type == "text/csv":
        return Response(content=data.to_csv(index=False), media_type=mime_type)

    elif mime_type == "application/json":
        return Response(content=data.to_json(orient="records"), media_type=mime_type)



@app.get("/{county}/")
async def hello(
    request: Request,
    session: Session = Depends(get_db),
    q: schemas.Timeseries = Depends(),
):

    return {"testing": "123"}

    # return return_type_table(
    #     data=crud.get_latest_raw(session), request=request, mime_type=parse_type(q.type)
    # )

