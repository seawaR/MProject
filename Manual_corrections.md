# Manual corrections to data

## Duplicates

We consider duplicates based on the ID. We found one individual with three entries and 13 individuals with two entries each.

- GN10U: The sequences are the same lenght but the content differ. 
Decision: Take the most recent entry (StartDate = 03/02/2017 21:17:24).
- AB30A: The sequences and content differ. The later sequence is more complete.
Decision: Take the most recent entry (StartDate = 08/29/2017 13:50:01).
- AG13U: The sequences lengths and content differ. The first sequence is more complete.
Decision: Take the first entry (09/14/2017 09:04:39).
- AV38A: There are three sequences with different lengths. The first sequence is more complete.
Decision: Take the first entry (08/04/2017 21:51:23).
- DN08U: The sequences lengths and content differ. The last sequence is more complete.
Decision: Take the last entry (08/15/2017 21:13:48).
- GL14U: The sequences lengths and content differ. The last sequence is more complete. The first entry was not finished.
Decision: Take the last entry (07/12/2017 20:28:17).
- KV13E: The sequences lengths and content differ. The last sequence is more complete. The first entry was not finished.
Decision: Take the last entry (01/12/2018 15:37:42).
- lg24o/Lg24o: There are three sequences (one was registered with the Id Lg24o). Two of the sequences are the same.
Decision: Take the last entry (06/16/2017 16:48:18).
- MR17E: The sequences lengths and content differ. The first sequence is more complete. 
Decision: Take the first entry (06/03/2017 19:58:26).
- MR28E: The sequences lengths and content differ. The last sequence is more complete. The first entry was not finished.
Decision: Take the last entry (11/03/2017 21:55:17).
- PI27H: The sequences lengths and content differ. The last sequence is more complete. The first entry was not finished.
Decision: Take the last entry (01/10/2018 17:19:57).
- Pp18r: The Sequences are the same length. The first entry was not finished.
Decision: Take the last entry (11/02/2017 10:24:37).
- SP38A: The sequences lengths and content differ. The last sequence is more complete. 
Decision: Take the last entry (11/30/2017 20:35:24).
- VG37T: The sequences lengths and content differ. The last sequence is more complete. The first entry was not finished.
Decision: Take the last entry (11/27/2017 11:19:58).

## Individuals data in example fields

- The individual data included in the entries reserved for the example will be ignored if the sequence of personal data seems to be complete.

- Mt15o: Data for the last phase (23-45) was included in the example but not in the fields of the individual's sequence. 
Action: Manually include data for the last phase in the corresponding fields.

- DN15U: All the data was registered in the example fields.
Action: Manually include data for the all phases in the corresponding fields.

- BRO9C: All the data was registered in the example fields.
Action: Manually include data for the all phases in the corresponding fields.

- UV104O: All the data was registered in the example fields. Only two phases registered but the data seems incomplete.
Action: Remove individual.

- AI32A: Only one phase repeatedly registered in the example fields.
Action: Manually include date in the corresponding fields.

- B25T0: Overlapping phases included in example fields and no data in the individual's fields.
Action: Remove individual.

- hro31: Data for the last phase (31-70) was included in the example but not in the fields of the individual's sequence. 
Action: Manually include data for the last phase in the corresponding fields.

## Other cases

- Entry without identifier is deleted (StartDate = 01/26/2018 21:43:53 and EndDate = 01/26/2018 21:44:08)

- DN15U: The start age of the first phase is 17 but in this phase the individual was single so the start age was changed to 15 to coincide with the starting point of the other individuals.
