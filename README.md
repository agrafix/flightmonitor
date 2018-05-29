# flightmonitor (WIP)

A Haskell tool to monitor flight prices given a trip specification. An example specification file is included at `config.yaml`:

```yaml
amadeusKey: YOUR_KEY_HERE
weekLookAhead: 5
trips:
  - origin: SFO
    destination: HNL
    departure:
      dayOfWeek: friday
      time: "16:00"
    return:
      returnday:
        - monday
        - "07:00"
    nonStop: true
    maxPriceUSD: 700
  - origin: SFO
    destination: FRA
    departure:
      dayOfWeek: friday
    return:
      length: 14
    nonStop: true
    maxPriceUSD: 3000
```

The output currently looks like this:

```
# Price: 677 USD
Outbound:
UA 1509: 2018-06-08 16:35 -> 2018-06-08 18:51
Inbound:
UA 396: 2018-06-10 21:00 -> 2018-06-11 04:53
Outbound:
UA 1509: 2018-06-08 16:35 -> 2018-06-08 18:51
Inbound:
UA 1575: 2018-06-10 21:30 -> 2018-06-11 05:35
Outbound:
UA 1670: 2018-06-08 19:11 -> 2018-06-08 21:40
Inbound:
UA 396: 2018-06-10 21:00 -> 2018-06-11 04:53
Outbound:
UA 1670: 2018-06-08 19:11 -> 2018-06-08 21:40
Inbound:
UA 1575: 2018-06-10 21:30 -> 2018-06-11 05:35
# Price: 2667 USD
Outbound:
UA 926: 2018-06-15 19:10 -> 2018-06-16 15:15
Inbound:
LH 454: 2018-06-29 10:30 -> 2018-06-29 12:55
Outbound:
UA 58: 2018-06-15 13:45 -> 2018-06-16 09:55
Inbound:
LH 454: 2018-06-29 10:30 -> 2018-06-29 12:55

...
```
