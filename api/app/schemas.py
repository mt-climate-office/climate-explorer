from dataclasses import dataclass
from typing import Optional

from fastapi import Query, Path


@dataclass
class Timeseries:
    county: str = Path(
        ..., 
        description="The county FIPS code to gather data for.",
        example="30001"
    )


    type: Optional[str] = Query(
        "html",
        description="The output format of data. Can be 'html', 'json', or 'csv'.",
    )

