import json
import sys
from dataclasses import dataclass
from datetime import datetime
from typing import Any


@dataclass
class Event:
    band: str
    venue: str
    starttime: datetime
    endtime: datetime


def timestamp(timestr: str, day: int) -> datetime:
    day = day if timestr >= "1300" else day + 1
    return datetime.fromisoformat(f"2024-05-{day}T{formattimestr(timestr)}+02:00")


def parse_venue(s: str) -> str:
    return s.removesuffix(":")


def parse_event(s: str, venue: str, day: int) -> Event:
    band, timeslot = s.split(" @ ")
    start, end = timeslot.split("-")
    return Event(
        band=band,
        venue=venue,
        starttime=timestamp(start, day=day),
        endtime=timestamp(end, day=day),
    )


def parse_file(filename: str) -> list[Event]:
    events = []
    with open(filename, "r") as f:
        day = next(f).strip()
        day = int(day)
        venue = "?"
        for line in f:
            line = line.strip()
            if line == "":
                continue
            if line.endswith(":"):
                venue = parse_venue(line)
                continue
            event = parse_event(line, day=day, venue=venue)
            events.append(event)

    return events


def formattimestr(timestr: str) -> str:
    return f"{timestr[:2]}:{timestr[2:]}"


class EventEncoder(json.JSONEncoder):
    def default(self, o: Any) -> Any:
        if isinstance(o, Event):
            return {
                "name": o.band,
                "venue": o.venue,
                "starttime": o.starttime.isoformat(),
                "endtime": o.endtime.isoformat(),
            }

        return super().default(o)


if __name__ == "__main__":
    for infile in sys.argv[1:]:
        outfile = f"{infile[:-4]}.json"
        print("Parsing", infile, "to", outfile)
        events = parse_file(infile)
        with open(outfile, "w+") as f:
            print(json.dump(events, fp=f, cls=EventEncoder))
