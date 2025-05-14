#!/usr/bin/env python3

import csv
from dataclasses import dataclass
import dataclasses
from datetime import datetime, timedelta
import json
import sys
from typing import Final, Self, override

FRIDAY: Final = datetime(day=23, month=5, year=2025)
THURSDAY: Final = datetime(day=24, month=5, year=2025)


@dataclass
class Event:
    name: str
    venue: str
    starttime: datetime
    endtime: datetime

    @classmethod
    def from_row(
        cls, startdate: datetime, name: str, venue: str, starttime: str, endtime: str
    ) -> Self:
        return cls(
            name=name.upper(),
            venue=venue.upper(),
            starttime=date_with_time(startdate, starttime),
            endtime=date_with_time(startdate, endtime),
        )


def parse_hour(s: str) -> tuple[int, int]:
    hour, minute = s.split(":")
    return int(hour), int(minute)


def date_with_time(base: datetime, time: str) -> datetime:
    h, m = parse_hour(time)
    if h < 12:
        base += timedelta(days=1)

    return base.replace(hour=h, minute=m)


def parse_row(row: dict[str, str], startdate: datetime) -> list[Event]:
    venue: str = row.pop("venue")

    name = None
    starttime = None
    endtime = None

    events: list[Event] = []

    for timeslot, eventname in row.items():
        match name, eventname:
            case None, "":
                continue
            case None, new_name:
                name = new_name
                starttime = timeslot
            case _, _ if name != eventname:
                assert starttime is not None
                endtime = timeslot
                ev = Event.from_row(
                    startdate,
                    name=name,
                    venue=venue,
                    starttime=starttime,
                    endtime=endtime,
                )
                events.append(ev)
                name = eventname if eventname else None
            case _, _:
                pass

    return events


class EventEncoder(json.JSONEncoder):
    @override
    def default(self, o: object) -> object:
        if dataclasses.is_dataclass(o) and not isinstance(o, type):
            return dataclasses.asdict(o)

        if isinstance(o, datetime):
            return o.isoformat()

        return o


def main():
    events: list[Event] = []
    with sys.stdin as f:
        reader = csv.DictReader(f)
        for row in reader:
            events.extend(parse_row(row, FRIDAY))

    with sys.stdout as f:
        js = EventEncoder().encode(events)
        print(js)


if __name__ == "__main__":
    main()
