FROM python:3.11-slim

RUN apt-get update && apt-get -y install curl

# Install Poetry
RUN curl -sSL https://install.python-poetry.org | POETRY_HOME=/opt/poetry python3 && \
    cd /usr/local/bin && \
    ln -s /opt/poetry/bin/poetry && \
    poetry config virtualenvs.create false

# Copy using poetry.lock* in case it doesn't exist yet
COPY ./pyproject.toml ./poetry.lock* /app/

WORKDIR /app

RUN poetry install --no-root --no-dev

COPY ./app/ /app/

CMD ["uvicorn", "main:app", "--host", "0.0.0.0", "--port", "80"]