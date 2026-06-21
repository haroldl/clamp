import sys
from copy import copy, deepcopy
from dataclasses import asdict, dataclass as std_dataclass, field as dataclass_field, is_dataclass
from os import environ
from pathlib import Path

PYDANTIC_MAIN = Path(environ.get("PYDANTIC_MAIN", "~/local/pydantic-main")).expanduser()
sys.path.insert(0, str(PYDANTIC_MAIN / "pydantic-core" / "python"))
sys.path.insert(0, str(PYDANTIC_MAIN))

from pydantic import BaseModel, ConfigDict, Field, PrivateAttr, RootModel, TypeAdapter, computed_field, field_serializer, field_validator, model_validator, validate_call
from pydantic_core import SchemaValidator, ValidationError
from pydantic.dataclasses import dataclass as pydantic_dataclass


class Child(BaseModel):
    value: int


class Demo(BaseModel):
    id: int
    child: Child
    tags: list[int]
    name: str = Field(alias="Name")

    @computed_field
    @property
    def label(self) -> str:
        return f"{self.name}:{self.id}"

    @field_serializer("tags")
    def serialize_tags(self, tags: list[int]):
        return ",".join(str(tag) for tag in tags)

    @model_validator(mode="after")
    def validate_demo(self):
        if self.id <= 0:
            raise ValueError("id must be positive")
        return self


class Numbers(RootModel[list[int]]):
    pass


demo = Demo(id="7", child={"value": "11"}, tags=["1", 2], Name="clamp")
print(demo.id, demo.child.value, demo.name, demo.label)
print(demo.model_dump())
print(demo.model_dump_json())
print(Demo.model_json_schema()["properties"]["child"]["$ref"])

numbers = Numbers(["3", 4])
print(numbers.root)
print(numbers.model_dump())

validator = SchemaValidator({"type": "int"})
print(validator.validate_python("123"))

try:
    Demo(id=0, child={"value": 1}, tags=[], Name="bad")
except ValidationError as exc:
    print(type(exc).__name__)
    print("id must be positive" in str(exc))


class Configured(BaseModel):
    model_config = ConfigDict(extra="forbid", populate_by_name=True)
    id: int = Field(alias="ID")
    name: str

    @field_validator("name", mode="before")
    @classmethod
    def strip_name(cls, value):
        return value.strip()

configured = Configured(id="5", name=" clamp ")
print(configured.id, configured.name)

adapter = TypeAdapter(list[Configured])
values = adapter.validate_python([{"ID": "7", "name": " ada "}])
print(type(values[0]).__name__, values[0].id, values[0].name)
print(adapter.dump_python(values))
schema = adapter.json_schema()
print(sorted(schema.keys()))
print("items" in schema or "$defs" in schema)

try:
    Configured(ID=1, name="x", extra=2)
except ValidationError as exc:
    print(type(exc).__name__)
    print("Extra inputs" in str(exc))


@validate_call
def add(left: int, right: int = 1) -> int:
    return left + right


print(add("4", right="5"))
try:
    add("bad")
except ValidationError as exc:
    print(type(exc).__name__)
    print("int_parsing" in str(exc))

@std_dataclass
class PlainData:
    count: int
    tags: list[int] = dataclass_field(default_factory=list)


plain_data = PlainData("3", tags=[1])
print(is_dataclass(PlainData), is_dataclass(plain_data), plain_data.count, plain_data.tags)
print(asdict(plain_data))


@pydantic_dataclass
class DataPoint:
    x: int
    y: int = 2


data_point = DataPoint("4", y="5")
print(is_dataclass(DataPoint), is_dataclass(data_point), data_point.x, data_point.y)
print(TypeAdapter(DataPoint).validate_python({"x": "6", "y": "7"}).x)
try:
    DataPoint("bad")
except ValidationError as exc:
    print(type(exc).__name__)
    print("int_parsing" in str(exc))

class CopyChild(BaseModel):
    value: int


class CopyDemo(BaseModel):
    id: int
    tags: list[int] = Field(default_factory=list)
    child: CopyChild
    _secret: str = PrivateAttr(default="token")


copy_model = CopyDemo(id="1", child={"value": "2"})
copy_model.tags.append(3)
print(copy_model.id, copy_model.child.value, copy_model.tags, copy_model._secret)
shallow_copy = copy_model.model_copy(update={"id": 4})
print(type(shallow_copy).__name__, shallow_copy.id, shallow_copy.child.value, shallow_copy.tags, shallow_copy._secret)
shallow_copy.tags.append(5)
print(copy_model.tags, shallow_copy.tags)
deep_copy = copy_model.model_copy(deep=True)
deep_copy.tags.append(7)
print(copy_model.tags, deep_copy.tags)
plain_copy = copy(copy_model)
print(type(plain_copy).__name__, plain_copy.id, plain_copy.tags, plain_copy._secret)
plain_deepcopy = deepcopy(copy_model)
plain_deepcopy.tags.append(9)
print(copy_model.tags, plain_deepcopy.tags)

