Debug if multiple dog agents are sharing the same Hostkey.

1. SSH tunnel to dog_trainer's RethinkDB instance:
   `ssh -L 15672:localhost:15672 -L 9443:localhost:9443 dog-pro-awsoh01.phonebooth.net`

2. https://localhost:9443/#dataexplorer

3. run:

```r.db('dog').table('host').changes().filter(
    r.row('new_val')('interfaces').ne(r.row('old_val')('interfaces'))
    ).pluck(['new_val','old_val'])
```

    This will start a long running query that filters a changefeed for instance updates that share a hostkey but have different intefaces (IP addresses).  Compare the 'instances' of the new_val and old_val.  The dog agents at these IPs are sharing the same 'hostkey'.  Watch until you start seeing repeats, then abort the ReQL query.  Compile a list of all IPs of these systems.

4. SSH to these systems

5. Change 'hostkey' in /etc/dog/config.json on each to a unique value. Can use different output of `uuid` for each.

6. Restart dog agents on these systems.
